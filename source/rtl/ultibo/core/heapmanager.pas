{
Ultibo Heap Manager interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


Heap Manager
============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HeapManager; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes;
                     
//To Do //Look for:

//Critical
             
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
             
{==============================================================================}
const
 {Heap specific constants}
 HEAP_MIN_BLOCK     = 33;   {SizeOf(THeapBlock) + 1} 
 HEAP_MIN_ALIGN     = 64;   {SizeOf(THeapBlock) * 2} {Must be greater than or equal to HEAP_MIN_BLOCK, must be a power of 2}
 
 {Heap Signature}
 HEAP_SIGNATURE      = $E84DF600;
 HEAP_SIGNATURE_MASK = $FFFFFF00;
 
 {Heap Block States}
 HEAP_STATE_FREE = 0;
 HEAP_STATE_USED = 1;
 
 HEAP_STATE_MASK = $000000FF;
 HEAP_STATE_ALL  = 2; {Only for use by GetHeapBlockCount/GetHeapBlockMin/GetHeapBlockMax}

 {Heap Block Flags}
 HEAP_FLAG_NORMAL     = $00000000; {A normal memory block}
 HEAP_FLAG_SHARED     = $00000001; {A sharable memory block, usually marked as sharable in the page tables of the memory management unit}
 HEAP_FLAG_LOCAL      = $00000002; {A local memory block with an affinity to a specific processor, usually marked as non global in the page tables of the memory management unit}
 HEAP_FLAG_CODE       = $00000004; {A code memory block (with an optional affinity to a specific processor), usually marked as executable in the page tables of the memory management unit}
 HEAP_FLAG_DEVICE     = $00000008; {A device memory block, usually marked as device memory in the page tables of the memory management unit}
 HEAP_FLAG_NOCACHE    = $00000010; {A non cached memory block, usually marked as not cacheable in the page tables of the memory management unit}
 HEAP_FLAG_NONSHARED  = $00000020; {A non shared memory block, usually marked as not shareable in the page tables of the memory management unit}
 HEAP_FLAG_LOCKED     = $00000040; {A locked memory block (Not currently implemented in Ultibo)}
 HEAP_FLAG_IRQ        = $00000080; {An IRQ allocatable memory block}
 HEAP_FLAG_FIQ        = $00000100; {An FIQ allocatable memory block}
 HEAP_FLAG_RECLAIM    = $00000200; {A reclaimable memory block (eg Disk Cache)(with a registered callback to reclaim as required for normal memory)}
 
 HEAP_FLAG_CUSTOM     = $08000000; {A custom flag reserved for non standard uses}
 
 HEAP_FLAG_ALL        = $FFFFFFFF; {Only for use by GetHeapBlockCount/GetHeapBlockMin/GetHeapBlockMax}
 HEAP_FLAG_INVALID    = $FFFFFFFF; {Return value from MemFlags/IRQ/FIQ on invalid}
 
 {Heap Small Blocks}
 HEAP_SMALL_MIN   = 32; {SizeOf(THeapBlock)} {Minimum size of a small heap block}
 HEAP_SMALL_MAX   = SIZE_4K;                 {Maximum size of a small heap block}
 HEAP_SMALL_ALIGN = 4;  {SizeOf(LongWord);}  {Alignment for small heap blocks}
 HEAP_SMALL_SHIFT = 2;  {Size to Index conversion (Divide by 4)}
 
 HEAP_SMALL_LOW  = (HEAP_SMALL_MIN div HEAP_SMALL_ALIGN); {8}
 HEAP_SMALL_HIGH = (HEAP_SMALL_MAX div HEAP_SMALL_ALIGN); {1024}
 
{==============================================================================}
type
 {Heap specific types}
 THeapCallback = function(Size:PtrUInt):LongWord;
 
 PHeapBlock = ^THeapBlock;
 THeapBlock = record
  Size:PtrUInt;           {Size of the Heap Block (including the size of this structure)}
  State:LongWord;         {State of the Heap Block (eg HEAP_STATE_FREE)}
  Flags:LongWord;         {Flags of the Heap Block (eg HEAP_FLAG_SHARED)}
  Affinity:LongWord;      {CPU Affinity of the Heap Block (eg CPU_AFFINITY_0)}
  Prev:PHeapBlock;        {Previous Heap Block in list}
  Next:PHeapBlock;        {Next Heap Block in list}
  PrevLink:PHeapBlock;    {Previous Free/Used Block in list}
  NextLink:PHeapBlock;    {Next Free/Used Block in list}
 end;
 
 PSmallBlocks = ^TSmallBlocks;
 TSmallBlocks = array[HEAP_SMALL_LOW..HEAP_SMALL_HIGH] of PHeapBlock; {8..1024}
 
 PHeapLock = ^THeapLock;
 THeapLock = record 
  Lock:THandle;
  IRQLock:THandle;
  FIQLock:THandle;
  AcquireLock:function(Handle:THandle):LongWord;
  ReleaseLock:function(Handle:THandle):LongWord;
  AcquireIRQLock:function(Handle:THandle):LongWord;
  ReleaseIRQLock:function(Handle:THandle):LongWord;
  AcquireFIQLock:function(Handle:THandle):LongWord;
  ReleaseFIQLock:function(Handle:THandle):LongWord;
 end;
 
 {$IFDEF HEAP_STATISTICS}
 PHeapStatistics = ^THeapStatistics;
 THeapStatistics = record 
  {Get/Alloc/Realloc}
  GetCount:LongWord;  
  AllocCount:LongWord; 
  ReallocCount:LongWord;  
  GetAlignedCount:LongWord;
  AllocAlignedCount:LongWord;
  ReallocAlignedCount:LongWord;
  GetSharedCount:LongWord;
  AllocSharedCount:LongWord;
  ReallocSharedCount:LongWord;
  GetLocalCount:LongWord;
  AllocLocalCount:LongWord;
  ReallocLocalCount:LongWord;
  GetCodeCount:LongWord;
  AllocCodeCount:LongWord;
  ReallocCodeCount:LongWord;
  GetDeviceCount:LongWord;
  AllocDeviceCount:LongWord;
  ReallocDeviceCount:LongWord;
  GetNoCacheCount:LongWord;
  AllocNoCacheCount:LongWord;
  ReallocNoCacheCount:LongWord;
  GetNonSharedCount:LongWord;
  AllocNonSharedCount:LongWord;
  ReallocNonSharedCount:LongWord;
  GetIRQCount:LongWord;
  AllocIRQCount:LongWord;
  ReallocIRQCount:LongWord;
  GetFIQCount:LongWord;
  AllocFIQCount:LongWord;
  ReallocFIQCount:LongWord;
  {Free}
  FreeCount:LongWord;  
  FreeIRQCount:LongWord;  
  FreeFIQCount:LongWord;  
  FreeSizeCount:LongWord; 
  {Size}
  SizeCount:LongWord;   
  SizeIRQCount:LongWord;   
  SizeFIQCount:LongWord;   
  {Flags}
  FlagsCount:LongWord;   
  FlagsIRQCount:LongWord;   
  FlagsFIQCount:LongWord;   
  {Register}
  RegisterCount:LongWord;
  {Request}
  RequestCount:LongWord;
  RequestSharedCount:LongWord;
  RequestLocalCount:LongWord;
  RequestCodeCount:LongWord;
  RequestDeviceCount:LongWord;
  RequestNoCacheCount:LongWord;
  RequestNonSharedCount:LongWord;
  RequestIRQCount:LongWord;
  RequestFIQCount:LongWord;
  {Get Internal}
  GetZeroCount:LongWord;
  GetRemainCount:LongWord; 
  GetInvalidCount:LongWord; 
  GetUnavailableCount:LongWord; 
  GetAddFailCount:LongWord; 
  GetSplitFailCount:LongWord; 
  GetRemoveFailCount:LongWord; 
  {Realloc Internal}
  ReallocZeroCount:LongWord;
  ReallocSmallerCount:LongWord; 
  ReallocLargerCount:LongWord; 
  ReallocReleaseCount:LongWord; 
  ReallocReleaseBytes:LongWord; 
  ReallocAddFailCount:LongWord;
  ReallocSplitFailCount:LongWord;
  ReallocRemoveFailCount:LongWord;
  {GetAligned Internal}
  GetAlignedRemainCount:LongWord;
  GetAlignedInvalidCount:LongWord; 
  GetAlignedUndersizeCount:LongWord;
  GetAlignedUnavailableCount:LongWord; 
  GetAlignedAddFailCount:LongWord; 
  GetAlignedSplitFailCount:LongWord; 
  GetAlignedRemoveFailCount:LongWord; 
  GetAlignedOrphanCount:LongWord;
  GetAlignedOrphanBytes:LongWord;
  GetAlignedReleaseCount:LongWord;
  GetAlignedReleaseBytes:LongWord; 
  {Free Internal}
  FreeInvalidCount:LongWord;
  FreeAddFailCount:LongWord; 
  FreeMergeFailCount:LongWord;
  FreeRemoveFailCount:LongWord;
  {Size Internal}
  SizeInvalidCount:LongWord;
  {Flags Internal}
  FlagsInvalidCount:LongWord;
  {Register Internal}
  RegisterInvalidCount:LongWord;
  RegisterAddFailCount:LongWord;
  {Request Internal}
  RequestInvalidCount:LongWord;
  RequestAddFailCount:LongWord;
  RequestSplitFailCount:LongWord;
  RequestRemoveFailCount:LongWord;
  RequestUnavailableCount:LongWord;
  {Split Internal}
  SplitCount:LongWord;
  {Merge Internal}
  MergePrevCount:LongWord;
  MergeNextCount:LongWord;
  {Block Internal}
  GetSmallCount:LongWord;
  GetLargeCount:LongWord;
  AddSmallCount:LongWord;
  AddLargeCount:LongWord;
  RemoveSmallCount:LongWord;
  RemoveLargeCount:LongWord;
  SmallUnavailableCount:LongWord;
 end;
 {$ENDIF}
 
 PHeapSnapshot = ^THeapSnapshot;
 THeapSnapshot = record 
  {Snapshot Properties}
  Adddress:PtrUInt;       {Address of the Heap Block}
  Size:PtrUInt;           {Size of the Heap Block (including the size of the THeapBlock structure)}
  State:LongWord;         {State of the Heap Block (eg HEAP_STATE_FREE)}
  Flags:LongWord;         {Flags of the Heap Block (eg HEAP_FLAG_SHARED)}
  Affinity:LongWord;      {CPU Affinity of the Heap Block (eg CPU_AFFINITY_0)}
  {Internal Properties}
  Next:PHeapSnapshot;     {Next entry in Heap snapshot}
 end;
 
{==============================================================================}
{var}
 {Heap specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure RegisterMemoryManager;

procedure RegisterHeapBlock(Address:Pointer;Size:PtrUInt);
function RequestHeapBlock(Hint:Pointer;Size:PtrUInt;Flags,Affinity:LongWord):Pointer;

function RequestSharedHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
function RequestLocalHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
function RequestCodeHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
function RequestDeviceHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
function RequestNoCacheHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
function RequestNonSharedHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
function RequestIRQHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
function RequestFIQHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;

//To Do //ReleaseSharedHeapBlock/ReleaseLocalHeapBlock etc

//To Do //RequestReclaimHeapBlock( etc etc (with Callback for reclaim)

//function RegisterReclaimCallback    //To Do
//function DeregisterReclaimCallback  //To Do

{==============================================================================}
{Heap Functions}
{GetMem see SysGetMem}
function GetMemEx(Size:PtrUInt;Flags,Affinity:LongWord):Pointer;

function GetAlignedMem(Size,Alignment:PtrUInt):Pointer;
function GetAlignedMemEx(Size,Alignment:PtrUInt;Flags,Affinity:LongWord):Pointer;

function GetSharedMem(Size:PtrUInt):Pointer;
function GetSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;

function GetLocalMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function GetLocalAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

function GetCodeMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function GetCodeAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

function GetDeviceMem(Size:PtrUInt):Pointer;
function GetDeviceAlignedMem(Size,Alignment:PtrUInt):Pointer;

function GetNoCacheMem(Size:PtrUInt):Pointer;
function GetNoCacheAlignedMem(Size,Alignment:PtrUInt):Pointer;

function GetNonSharedMem(Size:PtrUInt):Pointer;
function GetNonSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;

function GetIRQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function GetIRQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

function GetFIQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function GetFIQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

//To Do //GetReclaimMem/GetReclaimAlignedMem etc

{FreeMem see SysFreeMem}
function FreeIRQMem(Addr:Pointer):PtrUInt;
function FreeFIQMem(Addr:Pointer):PtrUInt;

{AllocMem see SysAllocMem}
function AllocMemEx(Size:PtrUInt;Flags,Affinity:LongWord):Pointer;
{ReAllocMem see SysReAllocMem}
function ReAllocMemEx(var Addr:Pointer;Size:PtrUInt;Flags,Affinity:LongWord):Pointer;

function AllocAlignedMem(Size,Alignment:PtrUInt):Pointer;
function AllocAlignedMemEx(Size,Alignment:PtrUInt;Flags,Affinity:LongWord):Pointer;
function ReAllocAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;
function ReAllocAlignedMemEx(var Addr:Pointer;Size,Alignment:PtrUInt;Flags,Affinity:LongWord):Pointer;

function AllocSharedMem(Size:PtrUInt):Pointer;
function AllocSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;
function ReAllocSharedMem(var Addr:Pointer;Size:PtrUInt):Pointer;
function ReAllocSharedAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;

function AllocLocalMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function AllocLocalAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocLocalMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocLocalAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

function AllocCodeMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function AllocCodeAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocCodeMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocCodeAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

function AllocDeviceMem(Size:PtrUInt):Pointer;
function AllocDeviceAlignedMem(Size,Alignment:PtrUInt):Pointer;
function ReAllocDeviceMem(var Addr:Pointer;Size:PtrUInt):Pointer;
function ReAllocDeviceAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;

function AllocNoCacheMem(Size:PtrUInt):Pointer;
function AllocNoCacheAlignedMem(Size,Alignment:PtrUInt):Pointer;
function ReAllocNoCacheMem(var Addr:Pointer;Size:PtrUInt):Pointer;
function ReAllocNoCacheAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;

function AllocNonSharedMem(Size:PtrUInt):Pointer;
function AllocNonSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;
function ReAllocNonSharedMem(var Addr:Pointer;Size:PtrUInt):Pointer;
function ReAllocNonSharedAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;

function AllocIRQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function AllocIRQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocIRQMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocIRQAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

function AllocFIQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
function AllocFIQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocFIQMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
function ReAllocFIQAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;

//To Do //AllocReclaimMem/AllocReclaimAlignedMem etc
//To Do //ReAllocReclaimMem/ReAllocReclaimAlignedMem etc

{SizeMem see SysSizeMem}
function SizeIRQMem(Addr:Pointer):PtrUInt;
function SizeFIQMem(Addr:Pointer):PtrUInt;

function MemFlags(Addr:Pointer):LongWord;
function MemFlagsIRQ(Addr:Pointer):LongWord;
function MemFlagsFIQ(Addr:Pointer):LongWord;
{$IFDEF HEAP_STATISTICS}
function GetHeapStatistics:THeapStatistics;
{$ENDIF}
function GetHeapBlockCount(State:LongWord):LongWord;
function GetHeapBlockCountEx(State,Flags,Affinity:LongWord):LongWord;

function GetHeapBlockMin(State:LongWord):LongWord;
function GetHeapBlockMinEx(State,Flags,Affinity:LongWord):LongWord;

function GetHeapBlockMax(State:LongWord):LongWord;
function GetHeapBlockMaxEx(State,Flags,Affinity:LongWord):LongWord;

function CreateHeapSnapshot(State:LongWord):PHeapSnapshot;
function CreateHeapSnapshotEx(State,Flags,Affinity:LongWord):PHeapSnapshot;
function DestroyHeapSnapshot(Snapshot:PHeapSnapshot):LongWord;

{==============================================================================}
{Internal Functions}
function GetHeapBlock(Address:Pointer):PHeapBlock;
function FindHeapBlock(Address:Pointer;Size:PtrUInt):PHeapBlock;
function AddHeapBlock(Block:PHeapBlock):Boolean;
function SplitHeapBlock(Block:PHeapBlock;Size:PtrUInt):PHeapBlock;
function MergeHeapBlock(Block:PHeapBlock):PHeapBlock;

function GetFreeBlock(Size:PtrUInt):PHeapBlock;
function GetFreeBlockEx(Size:PtrUInt;Flags,Affinity:LongWord):PHeapBlock;
function FindFreeBlock(Size:PtrUInt):PHeapBlock;
function AddFreeBlock(Block:PHeapBlock):Boolean;
function RemoveFreeBlock(Block:PHeapBlock):Boolean;

function GetUsedBlock(Address:Pointer):PHeapBlock;
function CheckUsedBlock(Address:Pointer):Boolean; inline;
function AddUsedBlock(Block:PHeapBlock):Boolean;
function RemoveUsedBlock(Block:PHeapBlock):Boolean;

function GetIRQBlock(Address:Pointer):PHeapBlock;
function CheckIRQBlock(Address:Pointer):Boolean;
function AddIRQBlock(Block:PHeapBlock):Boolean;
function SplitIRQBlock(Block:PHeapBlock;Size:PtrUInt):PHeapBlock;
function MergeIRQBlock(Block:PHeapBlock):PHeapBlock;

function GetFreeIRQBlock(Size:PtrUInt;Affinity:LongWord):PHeapBlock;
function AddFreeIRQBlock(Block:PHeapBlock):Boolean;
function RemoveFreeIRQBlock(Block:PHeapBlock):Boolean;

function GetFIQBlock(Address:Pointer):PHeapBlock;
function CheckFIQBlock(Address:Pointer):Boolean;
function AddFIQBlock(Block:PHeapBlock):Boolean;
function SplitFIQBlock(Block:PHeapBlock;Size:PtrUInt):PHeapBlock;
function MergeFIQBlock(Block:PHeapBlock):PHeapBlock;

function GetFreeFIQBlock(Size:PtrUInt;Affinity:LongWord):PHeapBlock;
function AddFreeFIQBlock(Block:PHeapBlock):Boolean;
function RemoveFreeFIQBlock(Block:PHeapBlock):Boolean;

{==============================================================================}
{RTL Heap Manager Functions}
function SysGetMem(Size:PtrUInt):Pointer;
  
function SysFreeMem(Addr:Pointer):PtrUInt;
function SysFreeMemSize(Addr:Pointer;Size:PtrUInt):PtrUInt;

function SysAllocMem(Size:PtrUInt):Pointer;
function SysReAllocMem(var Addr:Pointer;Size:PtrUInt):Pointer;

function SysSizeMem(Addr:Pointer):PtrUInt;
  
procedure SysInitThread;
procedure SysDoneThread;

procedure SysRelocateHeap;
  
function SysGetHeapStatus:THeapStatus;
function SysGetFPCHeapStatus:TFPCHeapStatus;

{==============================================================================}
{Helper Functions}
procedure AcquireHeapLock; inline; //To Do //Change to function:Boolean
procedure ReleaseHeapLock; inline;

procedure AcquireHeapIRQLock; inline;
procedure ReleaseHeapIRQLock; inline;

procedure AcquireHeapFIQLock; inline;
procedure ReleaseHeapFIQLock; inline;

procedure RegisterHeapLock(const Lock:THeapLock);
  
function HeapStateToString(State:LongWord):String;

{==============================================================================}
const
 {RTL Memory Manager Structure}
 MyMemoryManager: TMemoryManager = (
  NeedLock:False;  // Obsolete
  GetMem:@SysGetMem;
  FreeMem:@SysFreeMem;
  FreeMemSize:@SysFreeMemSize;
  AllocMem:@SysAllocMem;
  ReAllocMem:@SysReAllocMem;
  MemSize:@SysSizeMem;
  InitThread:@SysInitThread; 
  DoneThread:@SysDoneThread; 
  RelocateHeap:nil; // Nothing 
  GetHeapStatus:@SysGetHeapStatus;
  GetFPCHeapStatus:@SysGetFPCHeapStatus;
 );
     
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Heap specific variables}
 HeapInitialized:Boolean;
 
 HeapBlocks:PHeapBlock = nil;
 FreeBlocks:PHeapBlock = nil;
 UsedBlocks:PHeapBlock = nil;
 SmallBlocks:TSmallBlocks;
 
 IRQBlocks:PHeapBlock = nil;
 FreeIRQBlocks:PHeapBlock = nil;
 SmallIRQBlocks:TSmallBlocks;
 
 FIQBlocks:PHeapBlock = nil;
 FreeFIQBlocks:PHeapBlock = nil;
 SmallFIQBlocks:TSmallBlocks;
 
 HeapLock:THeapLock;
 
 HeapStatus:THeapStatus;
 FPCHeapStatus:TFPCHeapStatus;
 {$IFDEF HEAP_STATISTICS}
 HeapStatistics:THeapStatistics;
 {$ENDIF}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RegisterMemoryManager;
begin
 {}
 if HeapInitialized then Exit;
 
 {Initialize Heap Lock}
 FillChar(HeapLock,SizeOf(THeapLock),0);
 HeapLock.Lock:=INVALID_HANDLE_VALUE;
 HeapLock.IRQLock:=INVALID_HANDLE_VALUE;
 HeapLock.FIQLock:=INVALID_HANDLE_VALUE;
 HeapLock.AcquireLock:=nil;
 HeapLock.ReleaseLock:=nil;
 HeapLock.AcquireIRQLock:=nil;
 HeapLock.ReleaseIRQLock:=nil;
 HeapLock.AcquireFIQLock:=nil;
 HeapLock.ReleaseFIQLock:=nil;
 
 {Initialize Status and Statistics}
 FillChar(HeapStatus,SizeOf(THeapStatus),0);
 FillChar(FPCHeapStatus,SizeOf(TFPCHeapStatus),0);
 {$IFDEF HEAP_STATISTICS}
 FillChar(HeapStatistics,SizeOf(THeapStatistics),0);
 {$ENDIF}
 
 {Register the RTL Memory Manager}
 SetMemoryManager(MyMemoryManager);
 
 {Register the RTL Heap Block}
 RegisterHeapBlock(@RtlHeapAddr,RtlHeapSize);
 
 //To Do //Should we set ReturnNilIfGrowHeapFails to True so that failed allocations do not generate a Runtime error ?
                       //Is it relevant to us at all ?
                       //See other notes in HeapManager.pas regarding memory manager etc
 
 HeapInitialized:=True;
end;     

{==============================================================================}

procedure RegisterHeapBlock(Address:Pointer;Size:PtrUInt);
var
 Block:PHeapBlock;
begin
 {}
 AcquireHeapLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.RegisterCount);
  {$ENDIF}
  
  {Check Size}
  if Size < HEAP_MIN_BLOCK then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RegisterInvalidCount);
    {$ENDIF}
    Exit;
   end;
  
  {Create Block} 
  Block:=PHeapBlock(Address);
  Block^.Size:=Size;
  Block^.State:=HEAP_SIGNATURE + HEAP_STATE_FREE;
  Block^.Flags:=HEAP_FLAG_NORMAL;
  Block^.Affinity:=CPU_AFFINITY_NONE;
  Block^.PrevLink:=nil;
  Block^.NextLink:=nil;
  
  {Get Block}
  if GetHeapBlock(Block) <> nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RegisterInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Add Block}
  if not AddHeapBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RegisterAddFailCount);
    {$ENDIF}
    Exit;
   end;

  {Add Free Block}
  if not AddFreeBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RegisterAddFailCount);
    {$ENDIF}
    Exit;
   end;
   
  {Update Heap Status}
  Inc(HeapStatus.TotalAddrSpace,Size);
  Inc(HeapStatus.TotalUncommitted,Size);
  Inc(HeapStatus.TotalFree,Size);
  Inc(HeapStatus.Unused,Size);
 
  {Update FPC Heap Status}
  Inc(FPCHeapStatus.CurrHeapSize,Size);
  Inc(FPCHeapStatus.CurrHeapFree,Size);
  if FPCHeapStatus.CurrHeapSize > FPCHeapStatus.MaxHeapSize then FPCHeapStatus.MaxHeapSize:=FPCHeapStatus.CurrHeapSize;
 finally
  ReleaseHeapLock; 
 end;      
end;

{==============================================================================}

function RequestHeapBlock(Hint:Pointer;Size:PtrUInt;Flags,Affinity:LongWord):Pointer;
{Request registration a Heap Block with specified flags and affinity within an existing block.

 Hint provides the requested base address of the heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.

 Flags provides the heap block flags such as shared, local, code, device, nocache etc.
 
 Affinity provides the processor affinity mask, a mask of 0 indicates no specific processor.
 
 The return is the heap block that has been registered or nil if the request failed.}
{Note: To allocate this memory use GetMemEx / AllocMemEx etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
var
 Count:Integer;
 Offset:PtrUInt;
 Block:PHeapBlock;
 Split:PHeapBlock;
 
 Mask:PtrUInt;
 Value:PtrUInt;
 Aligned:PtrUInt;
begin
 {}
 Result:=nil;

 AcquireHeapLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.RequestCount);
  {$ENDIF}

  {Check Flags}
  if Flags = HEAP_FLAG_NORMAL then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RequestInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Check Affinity}
  {Done by Caller}
  
  {Check Size}
  if Size < HEAP_MIN_BLOCK then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RequestInvalidCount);
    {$ENDIF}
    Exit;
   end;

  {Check Size}
  Count:=0; 
  while Count < 32 do
   begin
    Value:=1 shl Count;
    if Value = Size then Break;
    Inc(Count);
   end;   
  if Value <> Size then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RequestInvalidCount);
    {$ENDIF}
    Exit;
   end;
  
  {Check Alignment}
  if HEAP_REQUEST_ALIGNMENT = 0 then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RequestInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Check Hint}
  Mask:=(HEAP_REQUEST_ALIGNMENT - 1); 
  if (PtrUInt(Hint) and Mask) <> 0 then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.RequestInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Check Hint}
  Block:=nil;
  if Hint <> nil then
   begin
    {Find Block}
    Block:=FindHeapBlock(Hint,Size);
    if Block <> nil then
     begin
      {Check State/Flags/Affinity}
      if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_FREE) and (Block^.Flags = HEAP_FLAG_NORMAL) and (Block^.Affinity = CPU_AFFINITY_NONE) then
       begin
        {Find Start}
        Offset:=(PtrUInt(Hint) - PtrUInt(Block));
        if Offset > 0 then
         begin
          if Offset >= HEAP_MIN_BLOCK then
           begin
            {Remove Free Block}
            if not RemoveFreeBlock(Block) then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestRemoveFailCount);
              {$ENDIF}
              Exit;
             end;
             
            {Split Block}
            Split:=SplitHeapBlock(Block,Offset);
            if Split = nil then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestSplitFailCount);
              {$ENDIF}
              Exit;
             end;
            
            {Add Free Block}
            if not AddFreeBlock(Block) then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestAddFailCount);
              {$ENDIF}
              Exit;
             end;
             
            {Add Free Block}
            if not AddFreeBlock(Split) then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestAddFailCount);
              {$ENDIF}
              Exit;
             end;
             
            {Get Block}
            Block:=Split;            
           end
          else
           begin
            {Find a Free Block}
            Block:=nil;
           end;           
         end;
         
        {Find End}
        if Block <> nil then
         begin
          Offset:=(Block^.Size - Size);
          if Offset > 0 then
           begin
            if Offset >= HEAP_MIN_BLOCK then
             begin
              {Remove Free Block}
              if not RemoveFreeBlock(Block) then
               begin
                {$IFDEF HEAP_STATISTICS}
                {Update Heap Statistics}
                Inc(HeapStatistics.RequestRemoveFailCount);
                {$ENDIF}
                Exit;
               end;
               
              {Split Block}
              Split:=SplitHeapBlock(Block,Size);
              if Split = nil then
               begin
                {$IFDEF HEAP_STATISTICS}
                {Update Heap Statistics}
                Inc(HeapStatistics.RequestSplitFailCount);
                {$ENDIF}
                Exit;
               end;
              
              {Add Free Block}
              if not AddFreeBlock(Block) then
               begin
                {$IFDEF HEAP_STATISTICS}
                {Update Heap Statistics}
                Inc(HeapStatistics.RequestAddFailCount);
                {$ENDIF}
                Exit;
               end;
             
              {Add Free Block}
              if not AddFreeBlock(Split) then
               begin
                {$IFDEF HEAP_STATISTICS}
                {Update Heap Statistics}
                Inc(HeapStatistics.RequestAddFailCount);
                {$ENDIF}
                Exit;
               end;
             end
            else
             begin
              {Find a Free Block}
              Block:=nil;
             end;           
           end;
        
          {Update Block}
          if Block <> nil then
           begin
            Block^.Flags:=Flags;
            Block^.Affinity:=Affinity;
            
            {Return Result}
            Result:=Block;
           end; 
         end; 
       end
      else
       begin
        {Find a Free Block}
        Block:=nil;
       end;       
     end
    else
     begin
      {Find a Free Block}
      Block:=nil;
     end;     
   end;
  
  {Check Block}
  if Block = nil then
   begin  
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    if Hint <> nil then Inc(HeapStatistics.RequestUnavailableCount);
    {$ENDIF}
    
    {Find Free (Also checks Flags and Affinity)}
    Block:=FindFreeBlock(Size);
    if Block <> nil then
     begin
      {Get Aligned}
      Aligned:=Align(PtrUInt(Block),HEAP_REQUEST_ALIGNMENT); 
      
      {Find Start}
      Offset:=(PtrUInt(Aligned) - PtrUInt(Block));
      if Offset > 0 then
       begin
        if Offset >= HEAP_MIN_BLOCK then
         begin
          {Remove Free Block}
          if not RemoveFreeBlock(Block) then
           begin
            {$IFDEF HEAP_STATISTICS}
            {Update Heap Statistics}
            Inc(HeapStatistics.RequestRemoveFailCount);
            {$ENDIF}
            Exit;
           end;
      
          {Split Block}
          Split:=SplitHeapBlock(Block,Offset);
          if Split = nil then
           begin
            {$IFDEF HEAP_STATISTICS}
            {Update Heap Statistics}
            Inc(HeapStatistics.RequestSplitFailCount);
            {$ENDIF}
            Exit;
           end;
            
          {Add Free Block}
          if not AddFreeBlock(Block) then
           begin
            {$IFDEF HEAP_STATISTICS}
            {Update Heap Statistics}
            Inc(HeapStatistics.RequestAddFailCount);
            {$ENDIF}
            Exit;
           end;
             
          {Add Free Block}
          if not AddFreeBlock(Split) then
           begin
            {$IFDEF HEAP_STATISTICS}
            {Update Heap Statistics}
            Inc(HeapStatistics.RequestAddFailCount);
            {$ENDIF}
            Exit;
           end;
          
          {Get Block}
          Block:=Split;            
         end
        else
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.RequestUnavailableCount);
          {$ENDIF}
          Exit;
         end;           
       end;
      
      {Find End}
      if Block <> nil then
       begin
        Offset:=(Block^.Size - Size);
        if Offset > 0 then
         begin
          if Offset >= HEAP_MIN_BLOCK then
           begin
            {Remove Free Block}
            if not RemoveFreeBlock(Block) then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestRemoveFailCount);
              {$ENDIF}
              Exit;
             end;
             
            {Split Block}
            Split:=SplitHeapBlock(Block,Size);
            if Split = nil then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestSplitFailCount);
              {$ENDIF}
              Exit;
             end;
             
            {Add Free Block}
            if not AddFreeBlock(Block) then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestAddFailCount);
              {$ENDIF}
              Exit;
             end;
             
            {Add Free Block}
            if not AddFreeBlock(Split) then
             begin
              {$IFDEF HEAP_STATISTICS}
              {Update Heap Statistics}
              Inc(HeapStatistics.RequestAddFailCount);
              {$ENDIF}
              Exit;
             end;
           end
          else
           begin
            {$IFDEF HEAP_STATISTICS}
            {Update Heap Statistics}
            Inc(HeapStatistics.RequestUnavailableCount);
            {$ENDIF}
            Exit;
           end;           
         end;
        
        {Update Block}
        if Block <> nil then
         begin
          Block^.Flags:=Flags;
          Block^.Affinity:=Affinity;
            
          {Return Result}
          Result:=Block;
         end; 
       end; 
     end
    else
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.RequestUnavailableCount);
      {$ENDIF}
     end;     
   end;   
 finally
  ReleaseHeapLock; 
 end;      
end;

{==============================================================================}

function RequestSharedHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
{Request registration of a Shared Heap Block within an existing block.
 
 Hint provides the requested base address of the shared heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 The return is the heap block that has been marked as shared or nil if the request failed.
 (The memory management unit should mark this region of memory as sharable)}
{Note: To allocate shared memory use GetSharedMem / AllocSharedMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
var
 Flags:LongWord;       
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestSharedCount);
 {$ENDIF}
 
 {Get Flags}
 Flags:=HEAP_FLAG_SHARED;
 //if HEAP_NORMAL_SHARED then Flags:=HEAP_FLAG_NORMAL; //To Do //Critical //Need to modify RequestHeapBlock //Should also do HEAP_NORMAL_DEVICE/HEAP_NORMAL_NOCACHE/HEAP_NORMAL_NONSHARED etc
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,Flags,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function RequestLocalHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Request registration of a Local Heap Block within an existing block.
 
 Hint provides a requested base address of the local heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 Affinity provides the processor affinity mask, a mask of 0 indicates no specific processor.
 
 The return is the heap block that has been marked as local or nil if the request failed
 (The memory management unit should mark this region of memory as local or non global)}
{Note: To allocate local memory use GetLocalMem / AllocLocalMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestLocalCount);
 {$ENDIF}
 
 {Check Affinity}
 if (Affinity = CPU_AFFINITY_ALL) or (Affinity = CPU_AFFINITY_NONE) or ((Affinity and not(CPU_MASK)) <> 0) then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.RequestInvalidCount);
   {$ENDIF}
   Exit;
  end;
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,HEAP_FLAG_LOCAL,Affinity);
end;

{==============================================================================}

function RequestCodeHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Request registration of a Code Heap Block within an existing block.
 
 Hint provides a requested base address of the code heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 Affinity provides the processor affinity mask, a mask of 0 indicates no specific processor.
 
 The return is the heap block that has been marked as code or nil if the request failed
 (The memory management unit should mark this region of memory as executable)}
{Note: To allocate code memory use GetCodeMem / AllocCodeMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestCodeCount);
 {$ENDIF}

 {Check Affinity}
 if (Affinity = CPU_AFFINITY_ALL) or (Affinity = CPU_AFFINITY_NONE) or ((Affinity and not(CPU_MASK)) <> 0) then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.RequestInvalidCount);
   {$ENDIF}
   Exit;
  end;
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,HEAP_FLAG_LOCAL,Affinity);
end;

{==============================================================================}

function RequestDeviceHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
{Request registration of a Device Heap Block within an existing block.
 
 Hint provides the requested base address of the device heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 The return is the heap block that has been marked as device or nil if the request failed.
 (The memory management unit should mark this region of memory as sharable)}
{Note: To allocate device memory use GetDeviceMem / AllocDeviceMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestDeviceCount);
 {$ENDIF}
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,HEAP_FLAG_DEVICE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function RequestNoCacheHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
{Request registration of a Non Cached Heap Block within an existing block.
 
 Hint provides the requested base address of the non cached heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 The return is the heap block that has been marked as non cached or nil if the request failed.
 (The memory management unit should mark this region of memory as sharable)}
{Note: To allocate non cached memory use GetNoCacheMem / AllocNoCacheMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestNoCacheCount);
 {$ENDIF}
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,HEAP_FLAG_NOCACHE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function RequestNonSharedHeapBlock(Hint:Pointer;Size:PtrUInt):Pointer;
{Request registration of a Non Shared Heap Block within an existing block.
 
 Hint provides the requested base address of the non shared heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 The return is the heap block that has been marked as non shared or nil if the request failed.
 (The memory management unit should mark this region of memory as sharable)}
{Note: To allocate non shared memory use GetNonSharedMem / AllocNonSharedMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestNonSharedCount);
 {$ENDIF}
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,HEAP_FLAG_NONSHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function RequestIRQHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Request registration of an IRQ Heap Block within an existing block.
 
 Hint provides a requested base address of the IRQ heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 Affinity provides the processor affinity mask, a mask of 0 indicates no specific processor.
 
 The return is the heap block that has been marked as IRQ or nil if the request failed}
{Note: To allocate IRQ memory use GetIRQMem / AllocIRQMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
var
 Block:PHeapBlock;       
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestIRQCount);
 {$ENDIF}

 {Check Affinity}
 if (Affinity = CPU_AFFINITY_ALL) or (Affinity = CPU_AFFINITY_NONE) or ((Affinity and not(CPU_MASK)) <> 0) then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.RequestInvalidCount);
   {$ENDIF}
   Exit;
  end;
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,HEAP_FLAG_IRQ,Affinity);
 
 {Check Result}
 if Result <> nil then
  begin
   AcquireHeapIRQLock;
   try
    {Create Block} 
    Block:=PHeapBlock(PtrUInt(PtrUInt(Result) + SizeOf(THeapBlock)));
    Block^.Size:=Size - SizeOf(THeapBlock);
    Block^.State:=HEAP_SIGNATURE + HEAP_STATE_FREE;
    Block^.Flags:=HEAP_FLAG_IRQ;
    Block^.Affinity:=Affinity;
    Block^.PrevLink:=nil;
    Block^.NextLink:=nil;
   
    {Get Block}
    if GetIRQBlock(Block) <> nil then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.RequestInvalidCount);
      {$ENDIF}
      Exit;
     end;
   
    {Add Block}
    if not AddIRQBlock(Block) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.RequestAddFailCount);
      {$ENDIF}
      Exit;
     end;
   
    {Add Free Block}
    if not AddFreeIRQBlock(Block) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.RequestAddFailCount);
      {$ENDIF}
      Exit;
     end;
   
    {Update Heap Status}
    {Free}
    Dec(HeapStatus.TotalUncommitted,SizeOf(THeapBlock));
    Dec(HeapStatus.TotalFree,SizeOf(THeapBlock));
    Dec(HeapStatus.Unused,SizeOf(THeapBlock));
    {Used}  
    Inc(HeapStatus.TotalCommitted,SizeOf(THeapBlock));
    Inc(HeapStatus.TotalAllocated,SizeOf(THeapBlock));
   
    {Update FPC Heap Status}
    {Free}
    Dec(FPCHeapStatus.CurrHeapFree,SizeOf(THeapBlock));
    {Used}
    Inc(FPCHeapStatus.CurrHeapUsed,SizeOf(THeapBlock));
    {Max}
    if FPCHeapStatus.CurrHeapUsed > FPCHeapStatus.MaxHeapUsed then FPCHeapStatus.MaxHeapUsed:=FPCHeapStatus.CurrHeapUsed;
   finally 
    ReleaseHeapIRQLock;
   end;
  end; 
end;

{==============================================================================}

function RequestFIQHeapBlock(Hint:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Request registration of an FIQ Heap Block within an existing block.
 
 Hint provides a requested base address of the FIQ heap block but may be overridden
 by the memory manager if the block is already partially or fully allocated, pass nil 
 if any heap block is suitable.
 
 Size provides the requested size of the heap block. Size must be a power of 2
 and both Hint and the returned pointer must be aligned on a multiple of HEAP_REQUEST_ALIGNMENT.
 
 Affinity provides the processor affinity mask, a mask of 0 indicates no specific processor.
 
 The return is the heap block that has been marked as FIQ or nil if the request failed}
{Note: To allocate FIQ memory use GetFIQMem / AllocFIQMem etc}
{Note: The return value points directly to the heap block, to access the memory referenced
       by the heap block you must add SizeOf(THeapBlock) to this value}
var
 Block:PHeapBlock;       
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.RequestFIQCount);
 {$ENDIF}

 {Check Affinity}
 if (Affinity = CPU_AFFINITY_ALL) or (Affinity = CPU_AFFINITY_NONE) or ((Affinity and not(CPU_MASK)) <> 0) then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.RequestInvalidCount);
   {$ENDIF}
   Exit;
  end;
 
 {Request Heap Block}
 Result:=RequestHeapBlock(Hint,Size,HEAP_FLAG_FIQ,Affinity);

 {Check Result}
 if Result <> nil then
  begin
   AcquireHeapFIQLock;
   try
    {Create Block} 
    Block:=PHeapBlock(PtrUInt(PtrUInt(Result) + SizeOf(THeapBlock)));
    Block^.Size:=Size - SizeOf(THeapBlock);
    Block^.State:=HEAP_SIGNATURE + HEAP_STATE_FREE;
    Block^.Flags:=HEAP_FLAG_FIQ;
    Block^.Affinity:=Affinity;
    Block^.PrevLink:=nil;
    Block^.NextLink:=nil;
   
    {Get Block}
    if GetFIQBlock(Block) <> nil then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.RequestInvalidCount);
      {$ENDIF}
      Exit;
     end;

    {Add Block}
    if not AddFIQBlock(Block) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.RequestAddFailCount);
      {$ENDIF}
      Exit;
     end;
   
    {Add Free Block}
    if not AddFreeFIQBlock(Block) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.RequestAddFailCount);
      {$ENDIF}
      Exit;
     end;
   
    {Update Heap Status}
    {Free}
    Dec(HeapStatus.TotalUncommitted,SizeOf(THeapBlock));
    Dec(HeapStatus.TotalFree,SizeOf(THeapBlock));
    Dec(HeapStatus.Unused,SizeOf(THeapBlock));
    {Used}  
    Inc(HeapStatus.TotalCommitted,SizeOf(THeapBlock));
    Inc(HeapStatus.TotalAllocated,SizeOf(THeapBlock));
   
    {Update FPC Heap Status}
    {Free}
    Dec(FPCHeapStatus.CurrHeapFree,SizeOf(THeapBlock));
    {Used}
    Inc(FPCHeapStatus.CurrHeapUsed,SizeOf(THeapBlock));
    {Max}
    if FPCHeapStatus.CurrHeapUsed > FPCHeapStatus.MaxHeapUsed then FPCHeapStatus.MaxHeapUsed:=FPCHeapStatus.CurrHeapUsed;
   finally 
    ReleaseHeapFIQLock;
   end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{Heap Functions}
function GetMemEx(Size:PtrUInt;Flags,Affinity:LongWord):Pointer;
{Allocate a block of memory with the flags and affinity requested}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocSize:PtrUInt;
 RemainSize:PtrUInt;
begin
 {}
 Result:=nil;
 
 AcquireHeapLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.GetCount);
  {$ENDIF}

  {Check Size}
  if Size = 0 then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetZeroCount);
    {$ENDIF}
    Size:=4;
   end;
   
  {Check Flags}
  if (Flags and (HEAP_FLAG_IRQ or HEAP_FLAG_FIQ)) <> 0 then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetInvalidCount);
    {$ENDIF}
    Exit;
   end;
  
  {Determine Size}
  AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
 
  {Get Free Block}
  Block:=GetFreeBlockEx(AllocSize,Flags,Affinity);
  if Block = nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetUnavailableCount);
    {$ENDIF}
    Exit;
   end;
 
  {Remove Free Block}
  if not RemoveFreeBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemoveFailCount);
    {$ENDIF}
    Exit;
   end;
    
  {Determine Remain Size}
  if (Block^.Size - AllocSize) >= HEAP_MIN_BLOCK then
   begin
    RemainSize:=(Block^.Size - AllocSize);
   end
  else
   begin
    AllocSize:=Block^.Size;
    RemainSize:=0;
   end;
   
  {Check Remain Size}
  if RemainSize > 0 then 
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemainCount); 
    {$ENDIF}
    
    {Split Block}
    Split:=SplitHeapBlock(Block,AllocSize);
    if Split = nil then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetSplitFailCount);
      {$ENDIF}
      Exit;
     end; 
    
    {Add Free Block}
    if not AddFreeBlock(Split) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAddFailCount);
      {$ENDIF}
      Exit;
     end;
   end; 

  {Add Used Block}
  if not AddUsedBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetAddFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Update Heap Status}
  {Free}
  Dec(HeapStatus.TotalUncommitted,AllocSize);
  Dec(HeapStatus.TotalFree,AllocSize);
  Dec(HeapStatus.Unused,AllocSize);
  {Used}  
  Inc(HeapStatus.TotalCommitted,AllocSize);
  Inc(HeapStatus.TotalAllocated,AllocSize);
   
  {Update FPC Heap Status}
  {Free}
  Dec(FPCHeapStatus.CurrHeapFree,AllocSize);
  {Used}
  Inc(FPCHeapStatus.CurrHeapUsed,AllocSize);
  {Max}
  if FPCHeapStatus.CurrHeapUsed > FPCHeapStatus.MaxHeapUsed then FPCHeapStatus.MaxHeapUsed:=FPCHeapStatus.CurrHeapUsed;
     
  {Return Result}
  Result:=Pointer(PtrUInt(Block) + SizeOf(THeapBlock));
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

function GetAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate a block of normal memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocMemory:Pointer;
 AlignedMemory:PtrUInt;
begin
 {}
 Result:=nil;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetAlignedCount);
 {$ENDIF}
 
 {Check Alignment}
 if (Alignment and (HEAP_MIN_ALIGNMENT - 1)) <> 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedInvalidCount);
   {$ENDIF}
   Exit;
  end;
  
 if Alignment <= HEAP_MIN_ALIGNMENT then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedUndersizeCount);
   {$ENDIF}
   {Get Memory}
   Result:=SysGetMem(Size);
  end 
 else
  begin
   {Prevent orphan blocks}
   if Alignment < HEAP_MIN_ALIGN then Alignment:=HEAP_MIN_ALIGN;
    
   {Get Memory}    
   AllocMemory:=SysGetMem(Size + Alignment + (Alignment - 1));
   if AllocMemory = nil then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.GetAlignedUnavailableCount);
     {$ENDIF}
     Exit;
    end;
    
   {Get Aligned Memory}
   AlignedMemory:=Align(PtrUInt(AllocMemory),Alignment) + Alignment;
   if PtrUInt(AllocMemory) <> AlignedMemory then
    begin
     AcquireHeapLock;
     try
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedRemainCount);
      {$ENDIF}
     
      {Check Size}
      if (PtrUInt(AllocMemory) + SizeOf(THeapBlock)) > AlignedMemory then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedOrphanCount); 
        Inc(HeapStatistics.GetAlignedOrphanBytes,AlignedMemory - PtrUInt(AllocMemory));
        {$ENDIF}
        Exit;
       end;

      {Get Block} 
      Block:=PHeapBlock(PtrUInt(PtrUInt(AllocMemory) - SizeOf(THeapBlock)));
      
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedReleaseCount); 
      Inc(HeapStatistics.GetAlignedReleaseBytes,AlignedMemory - PtrUInt(AllocMemory)); 
      {$ENDIF}
      
      {Remove Used Block}
      if not RemoveUsedBlock(Block) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedRemoveFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Split Block}
      Split:=SplitHeapBlock(Block,AlignedMemory - PtrUInt(AllocMemory));
      if Split = nil then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedSplitFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Add Free Block}
      if not AddFreeBlock(Block) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedAddFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Add Used Block}
      if not AddUsedBlock(Split) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedAddFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Update Heap Status}
      {Free}
      Inc(HeapStatus.TotalUncommitted,Block^.Size);
      Inc(HeapStatus.TotalFree,Block^.Size);
      Inc(HeapStatus.Unused,Block^.Size);
      {Used}  
      Dec(HeapStatus.TotalCommitted,Block^.Size);
      Dec(HeapStatus.TotalAllocated,Block^.Size);
      
      {Update FPC Heap Status}
      {Free}
      Inc(FPCHeapStatus.CurrHeapFree,Block^.Size);
      {Used}
      Dec(FPCHeapStatus.CurrHeapUsed,Block^.Size);
      
      {Get Aligned Memory}
      AlignedMemory:=PtrUInt(Split) + SizeOf(THeapBlock);
     finally
      ReleaseHeapLock; 
     end;      
    end;
   {Return Result}
   Result:=Pointer(AlignedMemory);
  end;
end;

{==============================================================================}

function GetAlignedMemEx(Size,Alignment:PtrUInt;Flags,Affinity:LongWord):Pointer;
{Allocate a block of memory aligned on a multiple of the alignment value with the
 flags and affinity requested
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocMemory:Pointer;
 AlignedMemory:PtrUInt;
begin
 {}
 Result:=nil;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetAlignedCount);
 {$ENDIF}

 {Check Alignment}
 if (Alignment and (HEAP_MIN_ALIGNMENT - 1)) <> 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedInvalidCount);
   {$ENDIF}
   Exit;
  end;
 
 if Alignment <= HEAP_MIN_ALIGNMENT then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedUndersizeCount);
   {$ENDIF}
   {Get Memory}
   Result:=GetMemEx(Size,Flags,Affinity);
  end 
 else
  begin
   {Prevent orphan blocks}
   if Alignment < HEAP_MIN_ALIGN then Alignment:=HEAP_MIN_ALIGN;
 
   {Get Memory}    
   AllocMemory:=GetMemEx(Size + Alignment + (Alignment - 1),Flags,Affinity);
   if AllocMemory = nil then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.GetAlignedUnavailableCount);
     {$ENDIF}
     Exit;
    end;
 
   {Get Aligned Memory}
   AlignedMemory:=Align(PtrUInt(AllocMemory),Alignment) + Alignment;
   if PtrUInt(AllocMemory) <> AlignedMemory then
    begin
     AcquireHeapLock;
     try
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedRemainCount);
      {$ENDIF}
     
      {Check Size}
      if (PtrUInt(AllocMemory) + SizeOf(THeapBlock)) > AlignedMemory then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedOrphanCount); 
        Inc(HeapStatistics.GetAlignedOrphanBytes,AlignedMemory - PtrUInt(AllocMemory));
        {$ENDIF}
        Exit;
       end;

      {Get Block} 
      Block:=PHeapBlock(PtrUInt(PtrUInt(AllocMemory) - SizeOf(THeapBlock)));
      
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedReleaseCount); 
      Inc(HeapStatistics.GetAlignedReleaseBytes,AlignedMemory - PtrUInt(AllocMemory)); 
      {$ENDIF}
      
      {Remove Used Block}
      if not RemoveUsedBlock(Block) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedRemoveFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Split Block}
      Split:=SplitHeapBlock(Block,AlignedMemory - PtrUInt(AllocMemory));
      if Split = nil then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedSplitFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Add Free Block}
      if not AddFreeBlock(Block) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedAddFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Add Used Block}
      if not AddUsedBlock(Split) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedAddFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Update Heap Status}
      {Free}
      Inc(HeapStatus.TotalUncommitted,Block^.Size);
      Inc(HeapStatus.TotalFree,Block^.Size);
      Inc(HeapStatus.Unused,Block^.Size);
      {Used}  
      Dec(HeapStatus.TotalCommitted,Block^.Size);
      Dec(HeapStatus.TotalAllocated,Block^.Size);
      
      {Update FPC Heap Status}
      {Free}
      Inc(FPCHeapStatus.CurrHeapFree,Block^.Size);
      {Used}
      Dec(FPCHeapStatus.CurrHeapUsed,Block^.Size);
      
      {Get Aligned Memory}
      AlignedMemory:=PtrUInt(Split) + SizeOf(THeapBlock);
     finally
      ReleaseHeapLock; 
     end;      
    end;
   {Return Result}
   Result:=Pointer(AlignedMemory);
  end;
end;

{==============================================================================}

function GetSharedMem(Size:PtrUInt):Pointer;
{Allocate a block of shared memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetSharedCount);
 {$ENDIF}
 
 {Get Memory}
 Result:=GetMemEx(Size,HEAP_FLAG_SHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate a block of shared memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetSharedCount);
 {$ENDIF}

 {Get Aligned Memory}
 Result:=GetAlignedMemEx(Size,Alignment,HEAP_FLAG_SHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetLocalMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of local memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetLocalCount);
 {$ENDIF}

 {Get Memory}
 Result:=GetMemEx(Size,HEAP_FLAG_LOCAL,Affinity);
end;

{==============================================================================}

function GetLocalAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of local memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetLocalCount);
 {$ENDIF}

 {Get Aligned Memory}
 Result:=GetAlignedMemEx(Size,Alignment,HEAP_FLAG_LOCAL,Affinity);
end;

{==============================================================================}

function GetCodeMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of code memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetCodeCount);
 {$ENDIF}

 {Get Memory}
 Result:=GetMemEx(Size,HEAP_FLAG_CODE,Affinity);
end;

{==============================================================================}

function GetCodeAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of code memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 Result:=nil;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetCodeCount);
 {$ENDIF}

 {Get Aligned Memory}
 Result:=GetAlignedMemEx(Size,Alignment,HEAP_FLAG_CODE,Affinity);
end;

{==============================================================================}

function GetDeviceMem(Size:PtrUInt):Pointer;
{Allocate a block of device memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetDeviceCount);
 {$ENDIF}
 
 {Get Memory}
 Result:=GetMemEx(Size,HEAP_FLAG_DEVICE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetDeviceAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate a block of device memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetDeviceCount);
 {$ENDIF}

 {Get Aligned Memory}
 Result:=GetAlignedMemEx(Size,Alignment,HEAP_FLAG_DEVICE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetNoCacheMem(Size:PtrUInt):Pointer;
{Allocate a block of non cached memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetNoCacheCount);
 {$ENDIF}
 
 {Get Memory}
 Result:=GetMemEx(Size,HEAP_FLAG_NOCACHE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetNoCacheAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate a block of non cached memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetNoCacheCount);
 {$ENDIF}

 {Get Aligned Memory}
 Result:=GetAlignedMemEx(Size,Alignment,HEAP_FLAG_NOCACHE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetNonSharedMem(Size:PtrUInt):Pointer;
{Allocate a block of non shared memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetNonSharedCount);
 {$ENDIF}
 
 {Get Memory}
 Result:=GetMemEx(Size,HEAP_FLAG_NONSHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetNonSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate a block of non shared memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetNonSharedCount);
 {$ENDIF}

 {Get Aligned Memory}
 Result:=GetAlignedMemEx(Size,Alignment,HEAP_FLAG_NONSHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function GetIRQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of IRQ memory}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocSize:PtrUInt;
 RemainSize:PtrUInt;
begin
 {}
 Result:=nil;
 
 AcquireHeapIRQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.GetIRQCount);
  {$ENDIF}

  {Check Size}
  if Size = 0 then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetZeroCount);
    {$ENDIF}
    Size:=4;
   end;
  
  {Determine Size}
  AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
 
  {Get Free Block}
  Block:=GetFreeIRQBlock(AllocSize,Affinity);
  if Block = nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetUnavailableCount);
    {$ENDIF}
    Exit;
   end;
  
  {Remove Free Block}
  if not RemoveFreeIRQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemoveFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Determine Remain Size}
  if (Block^.Size - AllocSize) >= HEAP_MIN_BLOCK then
   begin
    RemainSize:=(Block^.Size - AllocSize);
   end
  else
   begin
    AllocSize:=Block^.Size;
    RemainSize:=0;
   end;
  
  {Check Remain Size}
  if RemainSize > 0 then 
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemainCount); 
    {$ENDIF}
    
    {Split Block}
    Split:=SplitIRQBlock(Block,AllocSize);
    if Split = nil then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetSplitFailCount);
      {$ENDIF}
      Exit;
     end; 
    
    {Add Free Block}
    if not AddFreeIRQBlock(Split) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAddFailCount);
      {$ENDIF}
      Exit;
     end;
   end; 
  
  {Update Heap Status}
  {Free}
  Dec(HeapStatus.TotalUncommitted,AllocSize);
  Dec(HeapStatus.TotalFree,AllocSize);
  Dec(HeapStatus.Unused,AllocSize);
  {Used}  
  Inc(HeapStatus.TotalCommitted,AllocSize);
  Inc(HeapStatus.TotalAllocated,AllocSize);
   
  {Update FPC Heap Status}
  {Free}
  Dec(FPCHeapStatus.CurrHeapFree,AllocSize);
  {Used}
  Inc(FPCHeapStatus.CurrHeapUsed,AllocSize);
  {Max}
  if FPCHeapStatus.CurrHeapUsed > FPCHeapStatus.MaxHeapUsed then FPCHeapStatus.MaxHeapUsed:=FPCHeapStatus.CurrHeapUsed;
     
  {Return Result}
  Result:=Pointer(PtrUInt(Block) + SizeOf(THeapBlock));
 finally
  ReleaseHeapIRQLock;
 end; 
end;

{==============================================================================}

function GetIRQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of IRQ memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocMemory:Pointer;
 AlignedMemory:PtrUInt;
begin
 {}
 Result:=nil;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetIRQCount);
 {$ENDIF}
 
 {Check Alignment}
 if (Alignment and (HEAP_MIN_ALIGNMENT - 1)) <> 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedInvalidCount);
   {$ENDIF}
   Exit;
  end;
 
 if Alignment <= HEAP_MIN_ALIGNMENT then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedUndersizeCount);
   {$ENDIF}
   {Get Memory}
   Result:=GetIRQMem(Size,Affinity);
  end 
 else
  begin
   {Prevent orphan blocks}
   if Alignment < HEAP_MIN_ALIGN then Alignment:=HEAP_MIN_ALIGN;
 
   {Get Memory}    
   AllocMemory:=GetIRQMem(Size + Alignment + (Alignment - 1),Affinity);
   if AllocMemory = nil then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.GetAlignedUnavailableCount);
     {$ENDIF}
     Exit;
    end;

   {Get Aligned Memory}
   AlignedMemory:=Align(PtrUInt(AllocMemory),Alignment) + Alignment;
   if PtrUInt(AllocMemory) <> AlignedMemory then
    begin
     AcquireHeapIRQLock;
     try
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedRemainCount);
      {$ENDIF}
     
      {Check Size}
      if (PtrUInt(AllocMemory) + SizeOf(THeapBlock)) > AlignedMemory then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedOrphanCount); 
        Inc(HeapStatistics.GetAlignedOrphanBytes,AlignedMemory - PtrUInt(AllocMemory));
        {$ENDIF}
        Exit;
       end;

      {Get Block} 
      Block:=PHeapBlock(PtrUInt(PtrUInt(AllocMemory) - SizeOf(THeapBlock)));
      
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedReleaseCount); 
      Inc(HeapStatistics.GetAlignedReleaseBytes,AlignedMemory - PtrUInt(AllocMemory)); 
      {$ENDIF}
      
      {Split Block}
      Split:=SplitIRQBlock(Block,AlignedMemory - PtrUInt(AllocMemory));
      if Split = nil then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedSplitFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Add Free Block}
      if not AddFreeIRQBlock(Block) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedAddFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Update Heap Status}
      {Free}
      Inc(HeapStatus.TotalUncommitted,Block^.Size);
      Inc(HeapStatus.TotalFree,Block^.Size);
      Inc(HeapStatus.Unused,Block^.Size);
      {Used}  
      Dec(HeapStatus.TotalCommitted,Block^.Size);
      Dec(HeapStatus.TotalAllocated,Block^.Size);
      
      {Update FPC Heap Status}
      {Free}
      Inc(FPCHeapStatus.CurrHeapFree,Block^.Size);
      {Used}
      Dec(FPCHeapStatus.CurrHeapUsed,Block^.Size);
      
      {Get Aligned Memory}
      AlignedMemory:=PtrUInt(Split) + SizeOf(THeapBlock);
     finally
      ReleaseHeapIRQLock; 
     end;      
    end;
   {Return Result}
   Result:=Pointer(AlignedMemory);
  end;
end;

{==============================================================================}

function GetFIQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of FIQ memory}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocSize:PtrUInt;
 RemainSize:PtrUInt;
begin
 {}
 Result:=nil;
 
 AcquireHeapFIQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.GetFIQCount);
  {$ENDIF}

  {Check Size}
  if Size = 0 then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetZeroCount);
    {$ENDIF}
    Size:=4;
   end;
  
  {Determine Size}
  AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
  
  {Get Free Block}
  Block:=GetFreeFIQBlock(AllocSize,Affinity);
  if Block = nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetUnavailableCount);
    {$ENDIF}
    Exit;
   end;
 
  {Remove Free Block}
  if not RemoveFreeFIQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemoveFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Determine Remain Size}
  if (Block^.Size - AllocSize) >= HEAP_MIN_BLOCK then
   begin
    RemainSize:=(Block^.Size - AllocSize);
   end
  else
   begin
    AllocSize:=Block^.Size;
    RemainSize:=0;
   end;
  
  {Check Remain Size}
  if RemainSize > 0 then 
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemainCount); 
    {$ENDIF}
    
    {Split Block}
    Split:=SplitFIQBlock(Block,AllocSize);
    if Split = nil then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetSplitFailCount);
      {$ENDIF}
      Exit;
     end; 
    
    {Add Free Block}
    if not AddFreeFIQBlock(Split) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAddFailCount);
      {$ENDIF}
      Exit;
     end;
   end; 
  
  {Update Heap Status}
  {Free}
  Dec(HeapStatus.TotalUncommitted,AllocSize);
  Dec(HeapStatus.TotalFree,AllocSize);
  Dec(HeapStatus.Unused,AllocSize);
  {Used}  
  Inc(HeapStatus.TotalCommitted,AllocSize);
  Inc(HeapStatus.TotalAllocated,AllocSize);
   
  {Update FPC Heap Status}
  {Free}
  Dec(FPCHeapStatus.CurrHeapFree,AllocSize);
  {Used}
  Inc(FPCHeapStatus.CurrHeapUsed,AllocSize);
  {Max}
  if FPCHeapStatus.CurrHeapUsed > FPCHeapStatus.MaxHeapUsed then FPCHeapStatus.MaxHeapUsed:=FPCHeapStatus.CurrHeapUsed;
     
  {Return Result}
  Result:=Pointer(PtrUInt(Block) + SizeOf(THeapBlock));
 finally
  ReleaseHeapFIQLock;
 end; 
end;

{==============================================================================}

function GetFIQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate a block of FIQ memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocMemory:Pointer;
 AlignedMemory:PtrUInt;
begin
 {}
 Result:=nil;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.GetFIQCount);
 {$ENDIF}
 
 {Check Alignment}
 if (Alignment and (HEAP_MIN_ALIGNMENT - 1)) <> 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedInvalidCount);
   {$ENDIF}
   Exit;
  end;
 
 if Alignment <= HEAP_MIN_ALIGNMENT then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.GetAlignedUndersizeCount);
   {$ENDIF}
   {Get Memory}
   Result:=GetFIQMem(Size,Affinity);
  end 
 else
  begin
   {Prevent orphan blocks}
   if Alignment < HEAP_MIN_ALIGN then Alignment:=HEAP_MIN_ALIGN;
 
   {Get Memory}    
   AllocMemory:=GetFIQMem(Size + Alignment + (Alignment - 1),Affinity);
   if AllocMemory = nil then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.GetAlignedUnavailableCount);
     {$ENDIF}
     Exit;
    end;
 
   {Get Aligned Memory}
   AlignedMemory:=Align(PtrUInt(AllocMemory),Alignment) + Alignment;
   if PtrUInt(AllocMemory) <> AlignedMemory then
    begin
     AcquireHeapFIQLock;
     try
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedRemainCount);
      {$ENDIF}
     
      {Check Size}
      if (PtrUInt(AllocMemory) + SizeOf(THeapBlock)) > AlignedMemory then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedOrphanCount); 
        Inc(HeapStatistics.GetAlignedOrphanBytes,AlignedMemory - PtrUInt(AllocMemory));
        {$ENDIF}
        Exit;
       end;

      {Get Block} 
      Block:=PHeapBlock(PtrUInt(PtrUInt(AllocMemory) - SizeOf(THeapBlock)));
      
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAlignedReleaseCount); 
      Inc(HeapStatistics.GetAlignedReleaseBytes,AlignedMemory - PtrUInt(AllocMemory)); 
      {$ENDIF}
      
      {Split Block}
      Split:=SplitFIQBlock(Block,AlignedMemory - PtrUInt(AllocMemory));
      if Split = nil then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedSplitFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Add Free Block}
      if not AddFreeFIQBlock(Block) then
       begin
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.GetAlignedAddFailCount);
        {$ENDIF}
        Exit;
       end;
      
      {Update Heap Status}
      {Free}
      Inc(HeapStatus.TotalUncommitted,Block^.Size);
      Inc(HeapStatus.TotalFree,Block^.Size);
      Inc(HeapStatus.Unused,Block^.Size);
      {Used}  
      Dec(HeapStatus.TotalCommitted,Block^.Size);
      Dec(HeapStatus.TotalAllocated,Block^.Size);
      
      {Update FPC Heap Status}
      {Free}
      Inc(FPCHeapStatus.CurrHeapFree,Block^.Size);
      {Used}
      Dec(FPCHeapStatus.CurrHeapUsed,Block^.Size);
      
      {Get Aligned Memory}
      AlignedMemory:=PtrUInt(Split) + SizeOf(THeapBlock);
     finally
      ReleaseHeapFIQLock; 
     end;      
    end;
   {Return Result}
   Result:=Pointer(AlignedMemory);
  end;
end;

{==============================================================================}

function FreeIRQMem(Addr:Pointer):PtrUInt;
{Free a block of IRQ memory}
var
 Block:PHeapBlock;
 Merge:PHeapBlock;
 BlockSize:PtrUInt;
begin
 {}
 Result:=0;
 
 AcquireHeapIRQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.FreeIRQCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
  
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
 
  {Check Block}
  {if GetIRQBlock(Block) <> Block then}
  if not CheckIRQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeInvalidCount);
    {$ENDIF}
    Exit;
   end;
 
  {Get Size}
  BlockSize:=Block^.Size;
 
  {Merge Block}
  Merge:=MergeIRQBlock(Block);
  if Merge = nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeMergeFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Add Free Block}
  if not AddFreeIRQBlock(Merge) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeAddFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Update Heap Status}
  {Free}
  Inc(HeapStatus.TotalUncommitted,BlockSize);
  Inc(HeapStatus.TotalFree,BlockSize);
  Inc(HeapStatus.Unused,BlockSize);
  {Used}  
  Dec(HeapStatus.TotalCommitted,BlockSize);
  Dec(HeapStatus.TotalAllocated,BlockSize);
  
  {Update FPC Heap Status}
  {Free}
  Inc(FPCHeapStatus.CurrHeapFree,BlockSize);
  {Used}
  Dec(FPCHeapStatus.CurrHeapUsed,BlockSize);
  
  {Return Result}
  Result:=BlockSize - SizeOf(THeapBlock);
 finally 
  ReleaseHeapIRQLock;
 end;
end;

{==============================================================================}

function FreeFIQMem(Addr:Pointer):PtrUInt;
{Free a block of FIQ memory}
var
 Block:PHeapBlock;
 Merge:PHeapBlock;
 BlockSize:PtrUInt;
begin
 {}
 Result:=0;
 
 AcquireHeapFIQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.FreeFIQCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
  
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
 
  {Check Block}
  {if GetFIQBlock(Block) <> Block then}
  if not CheckFIQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeInvalidCount);
    {$ENDIF}
    Exit;
   end;
 
  {Get Size}
  BlockSize:=Block^.Size;
  
  {Merge Block}
  Merge:=MergeFIQBlock(Block);
  if Merge = nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeMergeFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Add Free Block}
  if not AddFreeFIQBlock(Merge) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeAddFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Update Heap Status}
  {Free}
  Inc(HeapStatus.TotalUncommitted,BlockSize);
  Inc(HeapStatus.TotalFree,BlockSize);
  Inc(HeapStatus.Unused,BlockSize);
  {Used}  
  Dec(HeapStatus.TotalCommitted,BlockSize);
  Dec(HeapStatus.TotalAllocated,BlockSize);
  
  {Update FPC Heap Status}
  {Free}
  Inc(FPCHeapStatus.CurrHeapFree,BlockSize);
  {Used}
  Dec(FPCHeapStatus.CurrHeapUsed,BlockSize);
  
  {Return Result}
  Result:=BlockSize - SizeOf(THeapBlock);
 finally 
  ReleaseHeapFIQLock;
 end;
end;

{==============================================================================}

function AllocMemEx(Size:PtrUInt;Flags,Affinity:LongWord):Pointer;
{Allocate and clear a block of memory with the flags and affinity requested}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocCount); 
 {$ENDIF}
 
 {Get Memory}
 Result:=GetMemEx(Size,Flags,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocMemEx(var Addr:Pointer;Size:PtrUInt;Flags,Affinity:LongWord):Pointer;
{Reallocate a block of memory with the flags and affinity requested}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocCount);
 {$ENDIF}
 
 {Check Size}
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     SysFreeMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin 
   {Get Size}
   CurrentSize:=SysSizeMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize); 
        {$ENDIF}
        
        {Remove Used Block}
        if not RemoveUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocRemoveFailCount);
          {$ENDIF}
          Exit;
         end;
  
        {Split Block}
        Split:=SplitHeapBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Used Block}
        if not AddUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Memory}
     Result:=AllocMemEx(Size,Flags,Affinity); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SysSizeMem(Result);
         {CurrentSize:=SysSizeMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then SysFreeMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end; 
  end;
end;

{==============================================================================}

function AllocAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate and clear a block of normal memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocAlignedCount); 
 {$ENDIF}
 
 {Get Aligned Memory}
 Result:=GetAlignedMem(Size,Alignment);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocAlignedMemEx(Size,Alignment:PtrUInt;Flags,Affinity:LongWord):Pointer;
{Allocate and clear a block of normal memory aligned on a multiple of the
 alignment value with the flags and affinity requested
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocAlignedCount); 
 {$ENDIF}
 
 {Get Aligned Memory}
 Result:=GetAlignedMemEx(Size,Alignment,Flags,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;
{Reallocate a block of normal memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocAlignedCount);
 {$ENDIF}
 
 {Check Size}
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     SysFreeMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin 
   {Get Size}
   CurrentSize:=SysSizeMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize); 
        {$ENDIF}
        
        {Remove Used Block}
        if not RemoveUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocRemoveFailCount);
          {$ENDIF}
          Exit;
         end;
  
        {Split Block}
        Split:=SplitHeapBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Used Block}
        if not AddUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Aligned Memory}
     Result:=AllocAlignedMem(Size,Alignment); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SysSizeMem(Result);
         {CurrentSize:=SysSizeMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then SysFreeMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end; 
  end;
end;

{==============================================================================}

function ReAllocAlignedMemEx(var Addr:Pointer;Size,Alignment:PtrUInt;Flags,Affinity:LongWord):Pointer;
{Reallocate a block of memory aligned on a multiple of the alignment value with the
 flags and affinity requested
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocAlignedCount);
 {$ENDIF}

 {Check Size}
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     SysFreeMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin 
   {Get Size}
   CurrentSize:=SysSizeMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize);
        {$ENDIF}
        
        {Remove Used Block}
        if not RemoveUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocRemoveFailCount);
          {$ENDIF}
          Exit;
         end;
  
        {Split Block}
        Split:=SplitHeapBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Used Block}
        if not AddUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Aligned Memory}
     Result:=AllocAlignedMemEx(Size,Alignment,Flags,Affinity); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SysSizeMem(Result);
         {CurrentSize:=SysSizeMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then SysFreeMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end;
  end;  
end;

{==============================================================================}

function AllocSharedMem(Size:PtrUInt):Pointer;
{Allocate and clear a block of shared memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocSharedCount); 
 {$ENDIF}
 
 {Get Shared Memory}
 Result:=GetSharedMem(Size);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate and clear a block of shared memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocSharedCount); 
 {$ENDIF}
 
 {Get Shared Memory}
 Result:=GetSharedAlignedMem(Size,Alignment);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocSharedMem(var Addr:Pointer;Size:PtrUInt):Pointer;
{Reallocate a block of shared memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocSharedCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocMemEx(Addr,Size,HEAP_FLAG_SHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function ReAllocSharedAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;
{Reallocate a block of shared memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocSharedCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocAlignedMemEx(Addr,Size,Alignment,HEAP_FLAG_SHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function AllocLocalMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of local memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocLocalCount); 
 {$ENDIF}
 
 {Get Local Memory}
 Result:=GetLocalMem(Size,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocLocalAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of local memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocLocalCount); 
 {$ENDIF}
 
 {Get Local Memory}
 Result:=GetLocalAlignedMem(Size,Alignment,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocLocalMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of local memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocLocalCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocMemEx(Addr,Size,HEAP_FLAG_LOCAL,Affinity);
end;

{==============================================================================}

function ReAllocLocalAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of local memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocLocalCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocAlignedMemEx(Addr,Size,Alignment,HEAP_FLAG_LOCAL,Affinity);
end;

{==============================================================================}

function AllocCodeMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of code memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocCodeCount); 
 {$ENDIF}
 
 {Get Code Memory}
 Result:=GetCodeMem(Size,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocCodeAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of code memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocCodeCount); 
 {$ENDIF}
 
 {Get Code Memory}
 Result:=GetCodeAlignedMem(Size,Alignment,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocCodeMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of code memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocCodeCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocMemEx(Addr,Size,HEAP_FLAG_CODE,Affinity);
end;

{==============================================================================}

function ReAllocCodeAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of code memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocCodeCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocAlignedMemEx(Addr,Size,Alignment,HEAP_FLAG_CODE,Affinity);
end;

{==============================================================================}

function AllocDeviceMem(Size:PtrUInt):Pointer;
{Allocate and clear a block of device memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocDeviceCount); 
 {$ENDIF}
 
 {Get Device Memory}
 Result:=GetDeviceMem(Size);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocDeviceAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate and clear a block of device memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocDeviceCount); 
 {$ENDIF}
 
 {Get Device Memory}
 Result:=GetDeviceAlignedMem(Size,Alignment);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocDeviceMem(var Addr:Pointer;Size:PtrUInt):Pointer;
{Reallocate a block of device memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocDeviceCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocMemEx(Addr,Size,HEAP_FLAG_DEVICE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function ReAllocDeviceAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;
{Reallocate a block of device memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocDeviceCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocAlignedMemEx(Addr,Size,Alignment,HEAP_FLAG_DEVICE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function AllocNoCacheMem(Size:PtrUInt):Pointer;
{Allocate and clear a block of non cached memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocNoCacheCount); 
 {$ENDIF}
 
 {Get Non Cached Memory}
 Result:=GetNoCacheMem(Size);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocNoCacheAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate and clear a block of non cached memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocNoCacheCount); 
 {$ENDIF}
 
 {Get Non Cached Memory}
 Result:=GetNoCacheAlignedMem(Size,Alignment);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocNoCacheMem(var Addr:Pointer;Size:PtrUInt):Pointer;
{Reallocate a block of non cached memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocNoCacheCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocMemEx(Addr,Size,HEAP_FLAG_NOCACHE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function ReAllocNoCacheAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;
{Reallocate a block of non cached memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocNoCacheCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocAlignedMemEx(Addr,Size,Alignment,HEAP_FLAG_NOCACHE,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function AllocNonSharedMem(Size:PtrUInt):Pointer;
{Allocate and clear a block of non shared memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocNonSharedCount); 
 {$ENDIF}
 
 {Get Non Shared Memory}
 Result:=GetNonSharedMem(Size);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocNonSharedAlignedMem(Size,Alignment:PtrUInt):Pointer;
{Allocate and clear a block of non shared memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocNonSharedCount); 
 {$ENDIF}
 
 {Get Non Shared Memory}
 Result:=GetNonSharedAlignedMem(Size,Alignment);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocNonSharedMem(var Addr:Pointer;Size:PtrUInt):Pointer;
{Reallocate a block of non shared memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocNonSharedCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocMemEx(Addr,Size,HEAP_FLAG_NONSHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function ReAllocNonSharedAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt):Pointer;
{Reallocate a block of non shared memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocNonSharedCount); 
 {$ENDIF}

 {Realloc Memory} 
 Result:=ReAllocAlignedMemEx(Addr,Size,Alignment,HEAP_FLAG_NONSHARED,CPU_AFFINITY_NONE);
end;

{==============================================================================}

function AllocIRQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of IRQ memory}
{Note: The memory must be freed using FreeIRQMem}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocIRQCount); 
 {$ENDIF}
 
 {Get IRQ Memory}
 Result:=GetIRQMem(Size,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SizeIRQMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocIRQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of IRQ memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
{Note: The memory must be freed using FreeIRQMem}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocIRQCount); 
 {$ENDIF}
 
 {Get IRQ Memory}
 Result:=GetIRQAlignedMem(Size,Alignment,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SizeIRQMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocIRQMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of IRQ memory}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocIRQCount);
 {$ENDIF}

 {Check Size}
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     FreeIRQMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin 
   {Get Size}
   CurrentSize:=SizeIRQMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapIRQLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize); 
        {$ENDIF}
  
        {Split Block}
        Split:=SplitIRQBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeIRQBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
        
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapIRQLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Memory}
     Result:=AllocIRQMem(Size,Affinity); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SizeIRQMem(Result);
         {CurrentSize:=SizeIRQMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then FreeIRQMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end; 
  end;
end;

{==============================================================================}

function ReAllocIRQAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of IRQ memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocIRQCount);
 {$ENDIF}

 {Check Size}
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     FreeIRQMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin
   {Get Size}
   CurrentSize:=SizeIRQMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapIRQLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize);
        {$ENDIF}
  
        {Split Block}
        Split:=SplitIRQBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeIRQBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapIRQLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Aligned Memory}
     Result:=AllocIRQAlignedMem(Size,Alignment,Affinity); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SizeIRQMem(Result);
         {CurrentSize:=SizeIRQMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then FreeIRQMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end; 
  end;
end;

{==============================================================================}

function AllocFIQMem(Size:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of FIQ memory}
{Note: The memory must be freed using FreeFIQMem}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocFIQCount); 
 {$ENDIF}
 
 {Get FIQ Memory}
 Result:=GetFIQMem(Size,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SizeFIQMem(Result),0); 
  end; 
end;

{==============================================================================}

function AllocFIQAlignedMem(Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Allocate and clear a block of FIQ memory aligned on a multiple of the
 alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
{Note: The memory must be freed using FreeFIQMem}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocFIQCount); 
 {$ENDIF}
 
 {Get FIQ Memory}
 Result:=GetFIQAlignedMem(Size,Alignment,Affinity);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SizeFIQMem(Result),0); 
  end; 
end;

{==============================================================================}

function ReAllocFIQMem(var Addr:Pointer;Size:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of FIQ memory}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocFIQCount);
 {$ENDIF}

 {Check Size} 
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     FreeFIQMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin 
   {Get Size}
   CurrentSize:=SizeFIQMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapFIQLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize); 
        {$ENDIF}
  
        {Split Block}
        Split:=SplitFIQBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeFIQBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapFIQLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Memory}
     Result:=AllocFIQMem(Size,Affinity); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SizeFIQMem(Result);
         {CurrentSize:=SizeFIQMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then FreeFIQMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end; 
  end;
end;

{==============================================================================}

function ReAllocFIQAlignedMem(var Addr:Pointer;Size,Alignment:PtrUInt;Affinity:LongWord):Pointer;
{Reallocate a block of FIQ memory aligned on a multiple of the alignment value
 (Alignment must be a multiple of the minimum alignment configuration)}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocFIQCount);
 {$ENDIF}

 {Check Size} 
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     FreeFIQMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin
   {Get Size}
   CurrentSize:=SizeFIQMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapFIQLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize);
        {$ENDIF}
        
        {Split Block}
        Split:=SplitFIQBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeFIQBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapFIQLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Aligned Memory}
     Result:=AllocFIQAlignedMem(Size,Alignment,Affinity); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SizeFIQMem(Result);
         {CurrentSize:=SizeFIQMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then FreeFIQMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end; 
  end;
end;

{==============================================================================}

function SizeIRQMem(Addr:Pointer):PtrUInt;
{Return the size of an allocated block of IRQ memory}
var
 Block:PHeapBlock;
begin
 {}
 Result:=0;
 
 AcquireHeapIRQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.SizeIRQCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
 
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
  
  {Check Block}
  {if GetIRQBlock(Block) <> Block then}
  if not CheckIRQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.SizeInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Return Result}
  Result:=Block^.Size - SizeOf(THeapBlock);
 finally
  ReleaseHeapIRQLock;
 end; 
end;

{==============================================================================}

function SizeFIQMem(Addr:Pointer):PtrUInt;
{Return the size of an allocated block of FIQ memory}
var
 Block:PHeapBlock;
begin
 {}
 Result:=0;
 
 AcquireHeapFIQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.SizeFIQCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
 
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
  
  {Check Block}
  {if GetFIQBlock(Block) <> Block then}
  if not CheckFIQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.SizeInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Return Result}
  Result:=Block^.Size - SizeOf(THeapBlock);
 finally
  ReleaseHeapFIQLock;
 end; 
end;

{==============================================================================}

function MemFlags(Addr:Pointer):LongWord;
{Return the flags of an allocated block of memory}
var
 Block:PHeapBlock;
begin
 {}
 Result:=HEAP_FLAG_INVALID;
 
 AcquireHeapLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.FlagsCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
 
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
  
  {Check Block}
  {if GetUsedBlock(Block) <> Block then}
  if not CheckUsedBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FlagsInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Return Result}
  Result:=Block^.Flags;
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

function MemFlagsIRQ(Addr:Pointer):LongWord;
{Return the flags of an allocated block of IRQ memory}
var
 Block:PHeapBlock;
begin
 {}
 Result:=HEAP_FLAG_INVALID;
 
 AcquireHeapIRQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.FlagsIRQCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
 
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
  
  {Check Block}
  {if GetIRQBlock(Block) <> Block then}
  if not CheckIRQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FlagsInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Return Result}
  Result:=Block^.Flags;
 finally
  ReleaseHeapIRQLock;
 end; 
end;

{==============================================================================}

function MemFlagsFIQ(Addr:Pointer):LongWord;
{Return the flags of an allocated block of FIQ memory}
var
 Block:PHeapBlock;
begin
 {}
 Result:=HEAP_FLAG_INVALID;
 
 AcquireHeapFIQLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.FlagsFIQCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
 
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
  
  {Check Block}
  {if GetFIQBlock(Block) <> Block then}
  if not CheckFIQBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FlagsInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Return Result}
  Result:=Block^.Flags;
 finally
  ReleaseHeapFIQLock;
 end; 
end;

{==============================================================================}
{$IFDEF HEAP_STATISTICS}
function GetHeapStatistics:THeapStatistics; 
{Return detailed statistics for the heap manager}
begin
 {}
 FillChar(Result,SizeOf(THeapStatistics),0);
 
 AcquireHeapLock;
 try
  {Copy Statistics}
  System.Move(HeapStatistics,Result,SizeOf(THeapStatistics));
 finally
  ReleaseHeapLock;
 end; 
end;
{$ENDIF}
{==============================================================================}

function GetHeapBlockCount(State:LongWord):LongWord;
{Get the total number of current heap blocks based on state}
begin
 {}
 Result:=GetHeapBlockCountEx(State,HEAP_FLAG_ALL,CPU_AFFINITY_ALL);
end;

{==============================================================================}

function GetHeapBlockCountEx(State,Flags,Affinity:LongWord):LongWord;
{Get the number of current heap blocks based on state, flags and affinity}
{Note: This uses the block list (not the Free/Used/Small lists) in order to account for all blocks}
var 
 Block:PHeapBlock;
begin
 {}
 Result:=0;
 
 AcquireHeapLock;
 try
  Block:=HeapBlocks;
  while (Block <> nil) do
   begin
    case State of
     HEAP_STATE_FREE:begin
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_FREE) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         Inc(Result);
        end; 
      end;
     HEAP_STATE_USED:begin
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_USED) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         Inc(Result);
        end; 
      end;
     HEAP_STATE_ALL:begin
       if ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         Inc(Result);
        end; 
      end;
    end;
    Block:=Block^.Next;
   end;
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

function GetHeapBlockMin(State:LongWord):LongWord;
{Get the minimum size of current heap blocks based on state}
begin
 {}
 Result:=GetHeapBlockMinEx(State,HEAP_FLAG_ALL,CPU_AFFINITY_ALL);
end;

{==============================================================================}

function GetHeapBlockMinEx(State,Flags,Affinity:LongWord):LongWord;
{Get the minimum size of current heap blocks based on state, flags and affinity}
{Note: This uses the block list (not the Free/Used/Small lists) in order to account for all blocks}
var 
 Block:PHeapBlock;
begin
 {}
 Result:=$FFFFFFFF;
 
 AcquireHeapLock;
 try
  Block:=HeapBlocks;
  while (Block <> nil) do
   begin
    case State of
     HEAP_STATE_FREE:begin
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_FREE) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         if Block^.Size < Result then Result:=Block^.Size;
        end; 
      end;
     HEAP_STATE_USED:begin
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_USED) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         if Block^.Size < Result then Result:=Block^.Size;
        end; 
      end;
     HEAP_STATE_ALL:begin
       if ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         if Block^.Size < Result then Result:=Block^.Size;
        end; 
      end;
    end;
    Block:=Block^.Next;
   end;
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

function GetHeapBlockMax(State:LongWord):LongWord;
{Get the maximum size of current heap blocks based on state}
begin
 {}
 Result:=GetHeapBlockMaxEx(State,HEAP_FLAG_ALL,CPU_AFFINITY_ALL);
end;

{==============================================================================}

function GetHeapBlockMaxEx(State,Flags,Affinity:LongWord):LongWord;
{Get the maximum size of current heap blocks based on state, flags and affinity}
{Note: This uses the block list (not the Free/Used/Small lists) in order to account for all blocks}
var 
 Block:PHeapBlock;
begin
 {}
 Result:=0;
 
 AcquireHeapLock;
 try
  Block:=HeapBlocks;
  while (Block <> nil) do
   begin
    case State of
     HEAP_STATE_FREE:begin
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_FREE) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         if Block^.Size > Result then Result:=Block^.Size;
        end; 
      end;
     HEAP_STATE_USED:begin
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_USED) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         if Block^.Size > Result then Result:=Block^.Size;
        end; 
      end;
     HEAP_STATE_ALL:begin
       if ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         if Block^.Size > Result then Result:=Block^.Size;
        end; 
      end;
    end;
    Block:=Block^.Next;
   end;
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

function CreateHeapSnapshot(State:LongWord):PHeapSnapshot;
begin
 {}
 Result:=CreateHeapSnapshotEx(State,HEAP_FLAG_ALL,CPU_AFFINITY_ALL);
end;

{==============================================================================}

function CreateHeapSnapshotEx(State,Flags,Affinity:LongWord):PHeapSnapshot;
var
 Count:LongWord;
 Total:LongWord;
 Block:PHeapBlock;
 Current:PHeapSnapshot;
 Previous:PHeapSnapshot;
 Snapshot:PHeapSnapshot;
begin
 {}
 Result:=nil;
 
 {Get Total}
 Total:=GetHeapBlockCountEx(State,Flags,Affinity);
 
 {Check Total}
 if Total = 0 then Exit;

 {Allocate Snapshot}
 Snapshot:=AllocMem(SizeOf(THeapSnapshot) * Total);
 
 AcquireHeapLock;
 try
  {Setup Start}
  Count:=0;
  Current:=Snapshot;
  Previous:=nil;
  
  {Get First Block}
  Block:=HeapBlocks;
  while (Block <> nil) do
   begin
    {Check State}
    case State of
     HEAP_STATE_FREE:begin
       {Check Flags and Affinity}
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_FREE) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         {Add Block}
         Current.Adddress:=PtrUInt(Block);
         Current.Size:=Block^.Size;
         Current.State:=(Block^.State and HEAP_STATE_MASK);  
         Current.Flags:=Block^.Flags;  
         Current.Affinity:=Block^.Affinity;
         
         {Add Next}
         if Previous <> nil then Previous.Next:=Current;
         Previous:=Current;
         Current:=PHeapSnapshot(LongWord(Previous) + SizeOf(THeapSnapshot));

         {Update Count}
         Inc(Count);
         if Count >= Total then Break;
        end; 
      end;
     HEAP_STATE_USED:begin
       {Check Flags and Affinity}
       if (Block^.State = HEAP_SIGNATURE + HEAP_STATE_USED) and ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         {Add Block}
         Current.Adddress:=PtrUInt(Block);
         Current.Size:=Block^.Size;
         Current.State:=(Block^.State and not(HEAP_SIGNATURE));  
         Current.Flags:=Block^.Flags;  
         Current.Affinity:=Block^.Affinity;
         
         {Add Next}
         if Previous <> nil then Previous.Next:=Current;
         Previous:=Current;
         Current:=PHeapSnapshot(LongWord(Previous) + SizeOf(THeapSnapshot));

         {Update Count}
         Inc(Count);
         if Count >= Total then Break;
        end; 
      end;
     HEAP_STATE_ALL:begin
       {Check Flags and Affinity}
       if ((Flags = HEAP_FLAG_ALL) or ((Block^.Flags and Flags) = Flags)) and ((Affinity = CPU_AFFINITY_ALL) or ((Block^.Affinity and Affinity) = Affinity)) then
        begin
         {Add Block}
         Current.Adddress:=PtrUInt(Block);
         Current.Size:=Block^.Size;
         Current.State:=(Block^.State and not(HEAP_SIGNATURE));  
         Current.Flags:=Block^.Flags;  
         Current.Affinity:=Block^.Affinity;
         
         {Add Next}
         if Previous <> nil then Previous.Next:=Current;
         Previous:=Current;
         Current:=PHeapSnapshot(LongWord(Previous) + SizeOf(THeapSnapshot));
         
         {Update Count}
         Inc(Count);
         if Count >= Total then Break;
        end; 
      end;
    end;
    
    {Get Next Block}
    Block:=Block^.Next;
   end;
   
  {Return Result}
  Result:=Snapshot;
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

function DestroyHeapSnapshot(Snapshot:PHeapSnapshot):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Snapshot}
 if Snapshot = nil then Exit;
 
 {Free Snapshot}
 FreeMem(Snapshot);
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{Internal Functions}
function GetHeapBlock(Address:Pointer):PHeapBlock;
{Get the Heap Block referenced by Address}
{Address has already been normalized to include the Heap Block}
{Caller must hold the heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Address}
 if Address = nil then Exit;
 
 Block:=HeapBlocks;
 while (Block <> nil) do
  begin
   if Block = PHeapBlock(Address) then
    begin
     Result:=Block;
     Exit;
    end;
   Block:=Block^.Next;
  end;
end;

{==============================================================================}

function FindHeapBlock(Address:Pointer;Size:PtrUInt):PHeapBlock;
{Find the Heap Block containing Address up to the Size specified}
{Address has already been normalized to include the Heap Block}
{Caller must hold the heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Address}
 if Address = nil then Exit;
 
 Block:=HeapBlocks;
 while (Block <> nil) do
  begin
   if (PtrUInt(Address) >= PtrUInt(Block)) and (PtrUInt(Address) < (PtrUInt(Block) + Block^.Size)) then
    begin
     if ((PtrUInt(Address) + Size) > PtrUInt(Block)) and ((PtrUInt(Address) + Size) <= (PtrUInt(Block) + Block^.Size)) then
      begin
       Result:=Block;
       Exit;
      end; 
    end;
   Block:=Block^.Next;
  end;
end;

{==============================================================================}

function AddHeapBlock(Block:PHeapBlock):Boolean;
{Add a heap block, sorted by ascending Address order}
{Caller must hold the heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;
 
 {Find Next Block}
 Prev:=nil;
 Next:=HeapBlocks;
 while (Next <> nil) and (PtrUInt(Next) < PtrUInt(Block)) do
  begin
   Prev:=Next;
   Next:=Next^.Next;
  end;
  
 if Next <> nil then
  begin
   {Add before Next Block}
   if Prev <> nil then
    begin
     {Add after Previous Block}
     Prev^.Next:=Block;
     Block^.Prev:=Prev;
    end
   else
    begin
     {Add as First Block}
     HeapBlocks:=Block;
     Block^.Prev:=nil;
    end;
   Next^.Prev:=Block;
   Block^.Next:=Next;
  end
 else
  begin
   {Add as Last Block}
   if Prev <> nil then
    begin
     {Add after Previous Block}
     Prev^.Next:=Block;
     Block^.Prev:=Prev;
    end
   else
    begin
     {Add as First Block}
     HeapBlocks:=Block;
     Block^.Prev:=nil;
    end;
   Block^.Next:=nil; 
  end;
 
 Result:=True; 
end;

{==============================================================================}

function SplitHeapBlock(Block:PHeapBlock;Size:PtrUInt):PHeapBlock;
{Split a heap block at the size indicated}
{Return is the split portion of the block}
{Caller must remove block from the free list}
{Caller must add split block to the free list}
{Caller must hold the heap lock}
var
 Next:PHeapBlock;
 Split:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Block}
 if Block = nil then Exit;
 
 {Check Size}
 if Size < HEAP_MIN_BLOCK then Exit;
 
 {Create Block}
 Split:=PHeapBlock(PtrUInt(Block) + Size);
 Split^.Size:=Block^.Size - Size;
 Split^.State:=Block^.State;
 Split^.Flags:=Block^.Flags;
 Split^.Affinity:=Block^.Affinity;
 Split^.PrevLink:=nil;
 Split^.NextLink:=nil;
 
 {Update Size}
 Block^.Size:=Size;
 
 {Link Split (After Block)}
 Split^.Prev:=Block;
 Split^.Next:=Block^.Next;

 {Link Next (After Split)}
 Next:=Block^.Next;
 if Next <> nil then
  begin
   Next^.Prev:=Split;
  end;

 {Link Block (Before Split)}
 Block^.Next:=Split;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.SplitCount);
 {$ENDIF}
 
 {Return Result}
 Result:=Split;
end;

{==============================================================================}

function MergeHeapBlock(Block:PHeapBlock):PHeapBlock;
{Merge a heap block with Prev or Next blocks if free}
{Return is the merged result of the blocks}
{Caller must remove block from the free list}
{Caller must add merged block to the free list}
{Caller must hold the heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
 Merge:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Block}
 if Block = nil then Exit;

 {Get Merge}
 Merge:=Block;
 
 {Get Previous}
 Prev:=Merge^.Prev;
 if Prev <> nil then
  begin
   {Check Free}
   if Prev^.State = HEAP_SIGNATURE + HEAP_STATE_FREE then
    begin
     {Check Flags and Affinity}
     if (Prev^.Flags = Merge^.Flags) and (Prev^.Affinity = Merge^.Affinity) then
      begin
       {Check Contiguous}
       if (PtrUInt(Prev) + Prev^.Size) = PtrUInt(Merge) then
        begin
         {Remove Free}
         if RemoveFreeBlock(Prev) then
          begin
           {Get Size}
           Prev^.Size:=Prev^.Size + Merge^.Size;
           {Link Prev}
           Prev^.Next:=Merge^.Next;
           {Link Next}
           Next:=Prev^.Next;
           if Next <> nil then
            begin
             Next^.Prev:=Prev;
            end;
           {Get Merge}
           Merge:=Prev;
           
           {$IFDEF HEAP_STATISTICS}
           {Update Heap Statistics}
           Inc(HeapStatistics.MergePrevCount);
           {$ENDIF}
          end; 
        end; 
      end;
    end;  
  end;
 
 {Get Next}
 Next:=Merge^.Next;
 if Next <> nil then
  begin
   {Check Free}
   if Next^.State = HEAP_SIGNATURE + HEAP_STATE_FREE then
    begin
     {Check Flags and Affinity}
     if (Merge^.Flags = Next^.Flags) and (Merge^.Affinity = Next^.Affinity) then
      begin
       {Check Contiguous}
       if (PtrUInt(Merge) + Merge^.Size) = PtrUInt(Next) then
        begin
         {Remove Free}
         if RemoveFreeBlock(Next) then
          begin
           {Get Size}
           Merge^.Size:=Merge^.Size + Next^.Size;
           {Link Merge}
           Merge^.Next:=Next^.Next;
           {Link Next}
           Next:=Merge^.Next;
           if Next <> nil then
            begin
             Next^.Prev:=Merge;
            end;
            
           {$IFDEF HEAP_STATISTICS}
           {Update Heap Statistics}
           Inc(HeapStatistics.MergeNextCount);
           {$ENDIF}
          end;
        end;
      end;
    end;
  end;

 {Return Result}
 Result:=Merge;
end;

{==============================================================================}

function GetFreeBlock(Size:PtrUInt):PHeapBlock;
{Get a free block of at least the size indicated}
{Size has already been normalized to alignment and includes the size of the Heap Block}
{Caller must hold the heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size <= 0 then Exit;
 
 {Check Size}
 if Size < HEAP_SMALL_MAX then
  begin
   {Small Block}
   Block:=SmallBlocks[(Size shr HEAP_SMALL_SHIFT)];
   while (Block <> nil) do
    begin
     if Block^.Flags = HEAP_FLAG_NORMAL then
      begin
       {$IFDEF HEAP_STATISTICS}
       {Update Heap Statistics}
       Inc(HeapStatistics.GetSmallCount);
       {$ENDIF}
       
       {Return Block}
       Result:=Block;
       Exit;
      end;
      
     {Get Next}
     Block:=Block^.NextLink; 
    end;
    
   {$IFDEF HEAP_STATISTICS} 
   {Update Heap Statistics}
   Inc(HeapStatistics.SmallUnavailableCount);
   {$ENDIF}
  end;
  
 {Large Block}
 Block:=FreeBlocks;
 while (Block <> nil) do
  begin
   if (Block^.Size >= Size) and (Block^.Flags = HEAP_FLAG_NORMAL) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.GetLargeCount);
     {$ENDIF}
     
     {Return Block}
     Result:=Block;
     Exit;
    end; 

   {Get Next}    
   Block:=Block^.NextLink;
  end;
end;

{==============================================================================}

function GetFreeBlockEx(Size:PtrUInt;Flags,Affinity:LongWord):PHeapBlock;
{Get a free block of at least the size indicated with the flags and affinity requested}
{Size has already been normalized to alignment and includes the size of the Heap Block}
{Caller must hold the heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size <= 0 then Exit;
 
 {Check Size}
 if Size < HEAP_SMALL_MAX then
  begin
   {Small Block}
   Block:=SmallBlocks[(Size shr HEAP_SMALL_SHIFT)];
   while (Block <> nil) do
    begin
     if Affinity = CPU_AFFINITY_NONE then
      begin
       if Block^.Flags = Flags then
        begin
         {$IFDEF HEAP_STATISTICS}
         {Update Heap Statistics}
         Inc(HeapStatistics.GetSmallCount);
         {$ENDIF}
         
         {Return Block}
         Result:=Block;
         Exit;
        end;
      end
     else
      begin
       if (Block^.Flags = Flags) and (Block^.Affinity = Affinity) then
        begin
         {$IFDEF HEAP_STATISTICS}
         {Update Heap Statistics}
         Inc(HeapStatistics.GetSmallCount);
         {$ENDIF}
         
         {Return Block}
         Result:=Block;
         Exit;
        end;
      end;
      
     {Get Next}
     Block:=Block^.NextLink; 
    end;
    
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.SmallUnavailableCount);
   {$ENDIF}
  end;
 
 {Large Block}
 Block:=FreeBlocks;
 if Affinity = CPU_AFFINITY_NONE then
  begin
   while (Block <> nil) do
    begin
     if (Block^.Size >= Size) and (Block^.Flags = Flags) then
      begin
       {$IFDEF HEAP_STATISTICS}
       {Update Heap Statistics}
       Inc(HeapStatistics.GetLargeCount);
       {$ENDIF}
       
       {Return Block}
       Result:=Block;
       Exit;
      end; 

     {Get Next} 
     Block:=Block^.NextLink;
    end;
  end
 else
  begin
   while (Block <> nil) do
    begin
     if (Block^.Size >= Size) and (Block^.Flags = Flags) and (Block^.Affinity = Affinity) then
      begin
       {$IFDEF HEAP_STATISTICS}
       {Update Heap Statistics}
       Inc(HeapStatistics.GetLargeCount);
       {$ENDIF}
       
       {Return Block}
       Result:=Block;
       Exit;
      end; 

     {Get Next}
     Block:=Block^.NextLink;
    end;
  end;
end;

{==============================================================================}

function FindFreeBlock(Size:PtrUInt):PHeapBlock;
{Find a free block of at least the size indicated with no flags or affinity which
 is alignable to the HEAP_REQUEST_ALIGNMENT value}
{Size is the exact size and has not been normalized}
{Caller must hold the heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size <= 0 then Exit;
 
 {Large Block (No check for Small)}
 Block:=FreeBlocks;
 while (Block <> nil) do
  begin
   if (Block^.Size >= (Size + HEAP_REQUEST_ALIGNMENT)) and (Block^.Flags = HEAP_FLAG_NORMAL) and (Block^.Affinity = CPU_AFFINITY_NONE) then
    begin
     //To Do //Statistics
     
     {Return Block}
     Result:=Block;
     Exit;
    end;    
    
   {Get Next} 
   Block:=Block^.NextLink;
  end;
end; 

{==============================================================================}

function AddFreeBlock(Block:PHeapBlock):Boolean;
{Add a free block, sorted by ascending Size order}
{Caller must hold the heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;

 {Check Size}
 if Block^.Size < HEAP_SMALL_MAX then
  begin
   {Small Block}
   {Get Next}
   Next:=SmallBlocks[(Block^.Size shr HEAP_SMALL_SHIFT)];
   
   {Check Next}
   if Next <> nil then
    begin
     {Add as First Free}
     SmallBlocks[(Block^.Size shr HEAP_SMALL_SHIFT)]:=Block;
     Block^.PrevLink:=nil;
     Next^.PrevLink:=Block;
     Block^.NextLink:=Next;
    end
   else
    begin
     {Add as First Free}
     SmallBlocks[(Block^.Size shr HEAP_SMALL_SHIFT)]:=Block;
     Block^.PrevLink:=nil;
     Block^.NextLink:=nil;
    end;  
    
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.AddSmallCount);
   {$ENDIF}
  end
 else
  begin 
   {Large Block}
   {Find Next Free}
   Prev:=nil;
   Next:=FreeBlocks;
   while (Next <> nil) and (Next^.Size < Block^.Size) do
    begin
     Prev:=Next;
     Next:=Next^.NextLink;
    end;
   
   {Check Next}
   if Next <> nil then
    begin
     {Add before Next Free}
     if Prev <> nil then
      begin
       {Add after Previous Free}
       Prev^.NextLink:=Block;
       Block^.PrevLink:=Prev;
      end
     else
      begin
       {Add as First Free}
       FreeBlocks:=Block;
       Block^.PrevLink:=nil;
      end;
     Next^.PrevLink:=Block;
     Block^.NextLink:=Next;
    end
   else
    begin
     {Add as Last Free}
     if Prev <> nil then
      begin
       {Add after Previous Free}
       Prev^.NextLink:=Block;
       Block^.PrevLink:=Prev;
      end
     else
      begin
       {Add as First Free}
       FreeBlocks:=Block;
       Block^.PrevLink:=nil;
      end;
     Block^.NextLink:=nil; 
    end;
    
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.AddLargeCount);
   {$ENDIF}
  end;  
 
 {Mark as Free}
 Block^.State:=HEAP_SIGNATURE + HEAP_STATE_FREE;
 
 Result:=True; 
end;

{==============================================================================}

function RemoveFreeBlock(Block:PHeapBlock):Boolean;
{Remove a free block}
{Caller must hold the heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;
 
 {Check Size}
 if Block^.Size < HEAP_SMALL_MAX then
  begin
   {Small Block}
   {Get Prev/Next}
   Prev:=Block^.PrevLink;
   Next:=Block^.NextLink;
   
   {Check Next}
   if Next <> nil then
    begin
     {Remove from Next Free}
     if Prev <> nil then
      begin
       {Remove from Previous Free}
       Prev^.NextLink:=Next;
      end
     else
      begin
       {Remove First Free}
       SmallBlocks[(Block^.Size shr HEAP_SMALL_SHIFT)]:=Next;
      end;    
     Next^.PrevLink:=Prev; 
    end
   else
    begin   
     {Remove Last Free}
     if Prev <> nil then
      begin
       {Remove from Previous Free}
       Prev^.NextLink:=nil;
      end
     else
      begin
       {Remove First Free}
       SmallBlocks[(Block^.Size shr HEAP_SMALL_SHIFT)]:=nil;
      end;    
    end;  
   Block^.PrevLink:=nil;
   Block^.NextLink:=nil;
   
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.RemoveSmallCount);
   {$ENDIF}
  end
 else
  begin 
   {Large Block}
   {Get Prev/Next}
   Prev:=Block^.PrevLink;
   Next:=Block^.NextLink;
   
   {Check Next}
   if Next <> nil then
    begin
     {Remove from Next Free}
     if Prev <> nil then
      begin
       {Remove from Previous Free}
       Prev^.NextLink:=Next;
      end
     else
      begin
       {Remove First Free}
       FreeBlocks:=Next;
      end;    
     Next^.PrevLink:=Prev; 
    end
   else
    begin   
     {Remove Last Free}
     if Prev <> nil then
      begin
       {Remove from Previous Free}
       Prev^.NextLink:=nil;
      end
     else
      begin
       {Remove First Free}
       FreeBlocks:=nil;
      end;    
    end;  
   Block^.PrevLink:=nil;
   Block^.NextLink:=nil;
   
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.RemoveLargeCount);
   {$ENDIF}
  end;
  
 {Mark as Used}
 Block^.State:=HEAP_SIGNATURE + HEAP_STATE_USED;
   
 Result:=True; 
end;

{==============================================================================}

function GetUsedBlock(Address:Pointer):PHeapBlock;
{Get a used block by address}
{Address has already been normalized to include the Heap Block}
{Caller must hold the heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Address}
 if Address = nil then Exit;
 
 Block:=UsedBlocks;
 while (Block <> nil) do
  begin
   if Block = PHeapBlock(Address) then
    begin
     Result:=Block;
     Exit;
    end;
   Block:=Block^.NextLink;
  end;
end;

{==============================================================================}

function CheckUsedBlock(Address:Pointer):Boolean; inline;
{Check a used block by address}
{Address has already been normalized to include the Heap Block}
{Caller must hold the heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Address}
 if Address = nil then Exit;

 {Get Block}
 Block:=PHeapBlock(Address);
 
 {Check State}
 if Block^.State <> HEAP_SIGNATURE + HEAP_STATE_USED then Exit;
 
 {Check Flags}
 if (Block^.Flags and (HEAP_FLAG_IRQ or HEAP_FLAG_FIQ)) <> 0 then Exit;
 
 Result:=True;
end;
 
{==============================================================================}

function AddUsedBlock(Block:PHeapBlock):Boolean;
{Add a used block}
{Caller must hold the heap lock}
var
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;

 {Get Next}
 Next:=UsedBlocks;
 
 {Check Next}
 if Next <> nil then
  begin
   {Add as First Used}
   UsedBlocks:=Block;
   Block^.PrevLink:=nil;
   Next^.PrevLink:=Block;
   Block^.NextLink:=Next;
  end
 else
  begin
   {Add as First Used}
   UsedBlocks:=Block;
   Block^.PrevLink:=nil;
   Block^.NextLink:=nil;
  end;  
 
 {Mark as Used}
 Block^.State:=HEAP_SIGNATURE + HEAP_STATE_USED;
 
 Result:=True; 
end;
 
{==============================================================================}

function RemoveUsedBlock(Block:PHeapBlock):Boolean;
{Remove a used block}
{Caller must hold the heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;

 {Get Prev/Next}
 Prev:=Block^.PrevLink;
 Next:=Block^.NextLink;
 
 {Check Next}
 if Next <> nil then
  begin
   {Remove from Next Used}
   if Prev <> nil then
    begin
     {Remove from Previous Used}
     Prev^.NextLink:=Next;
    end
   else
    begin
     {Remove First Used}
     UsedBlocks:=Next;
    end;    
   Next^.PrevLink:=Prev;  
  end
 else
  begin   
   {Remove Last Used}
   if Prev <> nil then
    begin
     {Remove from Previous Used}
     Prev^.NextLink:=nil;
    end
   else
    begin
     {Remove First Used}
     UsedBlocks:=nil;
    end;    
  end;  
 Block^.PrevLink:=nil;
 Block^.NextLink:=nil;

 {Mark as Free}
 {Block^.State:=HEAP_SIGNATURE + HEAP_STATE_FREE;} {Do not mark on removal from Used, will be marked when added to Free}
   
 Result:=True; 
end;
 
{==============================================================================}
 
function GetIRQBlock(Address:Pointer):PHeapBlock;
{Get the IRQ Heap Block referenced by Address}
{Address has already been normalized to include the IRQ Heap Block}
{Caller must hold the IRQ heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Address}
 if Address = nil then Exit;
 
 Block:=IRQBlocks;
 while (Block <> nil) do
  begin
   if Block = PHeapBlock(Address) then
    begin
     Result:=Block;
     Exit;
    end;
   Block:=Block^.Next;
  end;
end;

{==============================================================================}

function CheckIRQBlock(Address:Pointer):Boolean;
{Check the IRQ Heap Block referenced by Address}
{Address has already been normalized to include the IRQ Heap Block}
{Caller must hold the IRQ heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Address}
 if Address = nil then Exit;

 {Get Block}
 Block:=PHeapBlock(Address);
 
 {Check State}
 if Block^.State <> HEAP_SIGNATURE + HEAP_STATE_USED then Exit;
 
 {Check Flags}
 if (Block^.Flags and HEAP_FLAG_IRQ) = 0 then Exit;
 
 Result:=True;
end;

{==============================================================================}

function AddIRQBlock(Block:PHeapBlock):Boolean;
{Add an IRQ heap block, sorted by ascending Address order}
{Caller must hold the IRQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;
 
 {Find Next Block}
 Prev:=nil;
 Next:=IRQBlocks;
 while (Next <> nil) and (PtrUInt(Next) < PtrUInt(Block)) do
  begin
   Prev:=Next;
   Next:=Next^.Next;
  end;
  
 if Next <> nil then
  begin
   {Add before Next Block}
   if Prev <> nil then
    begin
     {Add after Previous Block}
     Prev^.Next:=Block;
     Block^.Prev:=Prev;
    end
   else
    begin
     {Add as First Block}
     IRQBlocks:=Block;
     Block^.Prev:=nil;
    end;
   Next^.Prev:=Block;
   Block^.Next:=Next;
  end
 else
  begin
   {Add as Last Block}
   if Prev <> nil then
    begin
     {Add after Previous Block}
     Prev^.Next:=Block;
     Block^.Prev:=Prev;
    end
   else
    begin
     {Add as First Block}
     IRQBlocks:=Block;
     Block^.Prev:=nil;
    end;
   Block^.Next:=nil; 
  end;
 Result:=True; 
end;

{==============================================================================}

function SplitIRQBlock(Block:PHeapBlock;Size:PtrUInt):PHeapBlock;
{Split an IRQ heap block at the size indicated}
{Return is the split portion of the block}
{Caller must remove block from the IRQ free list}
{Caller must add split block to the IRQ free list}
{Caller must hold the IRQ heap lock}
var
 Next:PHeapBlock;
 Split:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Block}
 if Block = nil then Exit;
 
 {Check Size}
 if Size < HEAP_MIN_BLOCK then Exit;
 
 {Create Block}
 Split:=PHeapBlock(PtrUInt(Block) + Size);
 Split^.Size:=Block^.Size - Size;
 Split^.State:=Block^.State;
 Split^.Flags:=Block^.Flags;
 Split^.Affinity:=Block^.Affinity;
 Split^.PrevLink:=nil;
 Split^.NextLink:=nil;
 
 {Update Size}
 Block^.Size:=Size;
 
 {Link Split (After Block)}
 Split^.Prev:=Block;
 Split^.Next:=Block^.Next;

 {Link Next (After Split)}
 Next:=Block^.Next;
 if Next <> nil then
  begin
   Next^.Prev:=Split;
  end;

 {Link Block (Before Split)}
 Block^.Next:=Split;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.SplitCount);     
 {$ENDIF}
 
 {Return Result}
 Result:=Split;
end;

{==============================================================================}

function MergeIRQBlock(Block:PHeapBlock):PHeapBlock;
{Merge an IRQ heap block with Prev or Next blocks if free}
{Return is the merged result of the blocks}
{Caller must remove block from the IRQ free list}
{Caller must add merged block to the IRQ free list}
{Caller must hold the IRQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
 Merge:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Block}
 if Block = nil then Exit;

 {Get Merge}
 Merge:=Block;
 
 {Get Previous}
 Prev:=Merge^.Prev;
 if Prev <> nil then
  begin
   {Check Free}
   if Prev^.State = HEAP_SIGNATURE + HEAP_STATE_FREE then
    begin
     {Check Flags and Affinity}
     if (Prev^.Flags = Merge^.Flags) and (Prev^.Affinity = Merge^.Affinity) then
      begin
       {Check Contiguous}
       if (PtrUInt(Prev) + Prev^.Size) = PtrUInt(Merge) then
        begin
         {Remove Free}
         if RemoveFreeIRQBlock(Prev) then
          begin
           {Get Size}
           Prev^.Size:=Prev^.Size + Merge^.Size;
           {Link Prev}
           Prev^.Next:=Merge^.Next;
           {Link Next}
           Next:=Prev^.Next;
           if Next <> nil then
            begin
             Next^.Prev:=Prev;
            end;
           {Get Merge}
           Merge:=Prev;
           
           {$IFDEF HEAP_STATISTICS}
           {Update Heap Statistics}
           Inc(HeapStatistics.MergePrevCount);               
           {$ENDIF}
          end; 
        end; 
      end;
    end;  
  end;
 
 {Get Next}
 Next:=Merge^.Next;
 if Next <> nil then
  begin
   {Check Free}
   if Next^.State = HEAP_SIGNATURE + HEAP_STATE_FREE then
    begin
     {Check Flags and Affinity}
     if (Merge^.Flags = Next^.Flags) and (Merge^.Affinity = Next^.Affinity) then
      begin
       {Check Contiguous}
       if (PtrUInt(Merge) + Merge^.Size) = PtrUInt(Next) then
        begin
         {Remove Free}
         if RemoveFreeIRQBlock(Next) then
          begin
           {Get Size}
           Merge^.Size:=Merge^.Size + Next^.Size;
           {Link Merge}
           Merge^.Next:=Next^.Next;
           {Link Next}
           Next:=Merge^.Next;
           if Next <> nil then
            begin
             Next^.Prev:=Merge;
            end;
            
           {$IFDEF HEAP_STATISTICS}
           {Update Heap Statistics}
           Inc(HeapStatistics.MergeNextCount);               
           {$ENDIF}
          end;
        end;
      end;
    end;
  end;

 {Return Result}
 Result:=Merge;
end;

{==============================================================================}

function GetFreeIRQBlock(Size:PtrUInt;Affinity:LongWord):PHeapBlock;
{Get a free IRQ block of at least the size indicated}
{Size has already been normalized to alignment and includes the size of the IRQ Heap Block}
{Caller must hold the IRQ heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size <= 0 then Exit;
 
 Block:=FreeIRQBlocks;
 if Affinity = CPU_AFFINITY_NONE then
  begin
   while (Block <> nil) do
    begin
     if (Block^.Size >= Size) then
      begin
       Result:=Block;
       Exit;
      end;    
     Block:=Block^.NextLink;
    end;
  end
 else
  begin
   while (Block <> nil) do
    begin
     if (Block^.Size >= Size) and (Block^.Affinity = Affinity) then
      begin
       Result:=Block;
       Exit;
      end;    
     Block:=Block^.NextLink;
    end;
  end;  
end;

{==============================================================================}

function AddFreeIRQBlock(Block:PHeapBlock):Boolean;
{Add a free IRQ block, sorted by ascending Size order}
{Caller must hold the IRQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;

 {Find Next Free}
 Prev:=nil;
 Next:=FreeIRQBlocks;
 while (Next <> nil) and (Next^.Size < Block^.Size) do
  begin
   Prev:=Next;
   Next:=Next^.NextLink;
  end;
 
 {Check Next}
 if Next <> nil then
  begin
   {Add before Next Free}
   if Prev <> nil then
    begin
     {Add after Previous Free}
     Prev^.NextLink:=Block;
     Block^.PrevLink:=Prev;
    end
   else
    begin
     {Add as First Free}
     FreeIRQBlocks:=Block;
     Block^.PrevLink:=nil;
    end;
   Next^.PrevLink:=Block;
   Block^.NextLink:=Next;
  end
 else
  begin
   {Add as Last Free}
   if Prev <> nil then
    begin
     {Add after Previous Free}
     Prev^.NextLink:=Block;
     Block^.PrevLink:=Prev;
    end
   else
    begin
     {Add as First Free}
     FreeIRQBlocks:=Block;
     Block^.PrevLink:=nil;
    end;
   Block^.NextLink:=nil; 
  end;
 
 {Mark as Free}
 Block^.State:=HEAP_SIGNATURE + HEAP_STATE_FREE;
 
 Result:=True; 
end;
 
{==============================================================================}

function RemoveFreeIRQBlock(Block:PHeapBlock):Boolean;
{Remove a free IRQ block}
{Caller must hold the IRQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;

 {Get Prev/Next}
 Prev:=Block^.PrevLink;
 Next:=Block^.NextLink;
 
 {Check Next}
 if Next <> nil then
  begin
   {Remove from Next Free}
   if Prev <> nil then
    begin
     {Remove from Previous Free}
     Prev^.NextLink:=Next;
    end
   else
    begin
     {Remove First Free}
     FreeIRQBlocks:=Next;
    end;    
   Next^.PrevLink:=Prev; 
  end
 else
  begin   
   {Remove Last Free}
   if Prev <> nil then
    begin
     {Remove from Previous Free}
     Prev^.NextLink:=nil;
    end
   else
    begin
     {Remove First Free}
     FreeIRQBlocks:=nil;
    end;    
  end;  
 Block^.PrevLink:=nil;
 Block^.NextLink:=nil;

 {Mark as Used}
 Block^.State:=HEAP_SIGNATURE + HEAP_STATE_USED;
   
 Result:=True; 
end;

{==============================================================================}

function GetFIQBlock(Address:Pointer):PHeapBlock;
{Get the FIQ Heap Block referenced by Address}
{Address has already been normalized to include the FIQ Heap Block}
{Caller must hold the FIQ heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Address}
 if Address = nil then Exit;
 
 Block:=FIQBlocks;
 while (Block <> nil) do
  begin
   if Block = PHeapBlock(Address) then
    begin
     Result:=Block;
     Exit;
    end;
   Block:=Block^.Next;
  end;
end;

{==============================================================================}

function CheckFIQBlock(Address:Pointer):Boolean;
{Check the FIQ Heap Block referenced by Address}
{Address has already been normalized to include the FIQ Heap Block}
{Caller must hold the FIQ heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Address}
 if Address = nil then Exit;

 {Get Block}
 Block:=PHeapBlock(Address);
 
 {Check State}
 if Block^.State <> HEAP_SIGNATURE + HEAP_STATE_USED then Exit;
 
 {Check Flags}
 if (Block^.Flags and HEAP_FLAG_FIQ) = 0 then Exit;
 
 Result:=True;
end;

{==============================================================================}

function AddFIQBlock(Block:PHeapBlock):Boolean;
{Add an FIQ heap block, sorted by ascending Address order}
{Caller must hold the FIQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;
 
 {Find Next Block}
 Prev:=nil;
 Next:=FIQBlocks;
 while (Next <> nil) and (PtrUInt(Next) < PtrUInt(Block)) do
  begin
   Prev:=Next;
   Next:=Next^.Next;
  end;
  
 if Next <> nil then
  begin
   {Add before Next Block}
   if Prev <> nil then
    begin
     {Add after Previous Block}
     Prev^.Next:=Block;
     Block^.Prev:=Prev;
    end
   else
    begin
     {Add as First Block}
     FIQBlocks:=Block;
     Block^.Prev:=nil;
    end;
   Next^.Prev:=Block;
   Block^.Next:=Next;
  end
 else
  begin
   {Add as Last Block}
   if Prev <> nil then
    begin
     {Add after Previous Block}
     Prev^.Next:=Block;
     Block^.Prev:=Prev;
    end
   else
    begin
     {Add as First Block}
     FIQBlocks:=Block;
     Block^.Prev:=nil;
    end;
   Block^.Next:=nil; 
  end;
 Result:=True; 
end;

{==============================================================================}

function SplitFIQBlock(Block:PHeapBlock;Size:PtrUInt):PHeapBlock;
{Split an FIQ heap block at the size indicated}
{Return is the split portion of the block}
{Caller must remove block from the FIQ free list}
{Caller must add split block to the FIQ free list}
{Caller must hold the FIQ heap lock}
var
 Next:PHeapBlock;
 Split:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Block}
 if Block = nil then Exit;
 
 {Check Size}
 if Size < HEAP_MIN_BLOCK then Exit;
 
 {Create Block}
 Split:=PHeapBlock(PtrUInt(Block) + Size);
 Split^.Size:=Block^.Size - Size;
 Split^.State:=Block^.State;
 Split^.Flags:=Block^.Flags;
 Split^.Affinity:=Block^.Affinity;
 Split^.PrevLink:=nil;
 Split^.NextLink:=nil;
 
 {Update Size}
 Block^.Size:=Size;
 
 {Link Split (After Block)}
 Split^.Prev:=Block;
 Split^.Next:=Block^.Next;

 {Link Next (After Split)}
 Next:=Block^.Next;
 if Next <> nil then
  begin
   Next^.Prev:=Split;
  end;

 {Link Block (Before Split)}
 Block^.Next:=Split;
 
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.SplitCount);     
 {$ENDIF}
 
 {Return Result}
 Result:=Split;
end;

{==============================================================================}

function MergeFIQBlock(Block:PHeapBlock):PHeapBlock;
{Merge an FIQ heap block with Prev or Next blocks if free}
{Return is the merged result of the blocks}
{Caller must remove block from the FIQ free list}
{Caller must add merged block to the FIQ free list}
{Caller must hold the FIQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
 Merge:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Block}
 if Block = nil then Exit;

 {Get Merge}
 Merge:=Block;
 
 {Get Previous}
 Prev:=Merge^.Prev;
 if Prev <> nil then
  begin
   {Check Free}
   if Prev^.State = HEAP_SIGNATURE + HEAP_STATE_FREE then
    begin
     {Check Flags and Affinity}
     if (Prev^.Flags = Merge^.Flags) and (Prev^.Affinity = Merge^.Affinity) then
      begin
       {Check Contiguous}
       if (PtrUInt(Prev) + Prev^.Size) = PtrUInt(Merge) then
        begin
         {Remove Free}
         if RemoveFreeFIQBlock(Prev) then
          begin
           {Get Size}
           Prev^.Size:=Prev^.Size + Merge^.Size;
           {Link Prev}
           Prev^.Next:=Merge^.Next;
           {Link Next}
           Next:=Prev^.Next;
           if Next <> nil then
            begin
             Next^.Prev:=Prev;
            end;
           {Get Merge}
           Merge:=Prev;
           
           {$IFDEF HEAP_STATISTICS}
           {Update Heap Statistics}
           Inc(HeapStatistics.MergePrevCount);               
           {$ENDIF}
          end; 
        end; 
      end;
    end;  
  end;
 
 {Get Next}
 Next:=Merge^.Next;
 if Next <> nil then
  begin
   {Check Free}
   if Next^.State = HEAP_SIGNATURE + HEAP_STATE_FREE then
    begin
     {Check Flags and Affinity}
     if (Merge^.Flags = Next^.Flags) and (Merge^.Affinity = Next^.Affinity) then
      begin
       {Check Contiguous}
       if (PtrUInt(Merge) + Merge^.Size) = PtrUInt(Next) then
        begin
         {Remove Free}
         if RemoveFreeFIQBlock(Next) then
          begin
           {Get Size}
           Merge^.Size:=Merge^.Size + Next^.Size;
           {Link Merge}
           Merge^.Next:=Next^.Next;
           {Link Next}
           Next:=Merge^.Next;
           if Next <> nil then
            begin
             Next^.Prev:=Merge;
            end;
            
           {$IFDEF HEAP_STATISTICS}
           {Update Heap Statistics}
           Inc(HeapStatistics.MergeNextCount);               
           {$ENDIF}
          end;
        end;
      end;
    end;
  end;

 {Return Result}
 Result:=Merge;
end;

{==============================================================================}

function GetFreeFIQBlock(Size:PtrUInt;Affinity:LongWord):PHeapBlock;
{Get a free FIQ block of at least the size indicated}
{Size has already been normalized to alignment and includes the size of the FIQ Heap Block}
{Caller must hold the FIQ heap lock}
var
 Block:PHeapBlock;
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size <= 0 then Exit;
 
 Block:=FreeFIQBlocks;
 if Affinity = CPU_AFFINITY_NONE then
  begin
   while (Block <> nil) do
    begin
     if (Block^.Size >= Size) then
      begin
       Result:=Block;
       Exit;
      end;    
     Block:=Block^.NextLink;
    end;
  end
 else
  begin
   while (Block <> nil) do
    begin
     if (Block^.Size >= Size) and (Block^.Affinity = Affinity) then
      begin
       Result:=Block;
       Exit;
      end;    
     Block:=Block^.NextLink;
    end;
  end;
end;

{==============================================================================}

function AddFreeFIQBlock(Block:PHeapBlock):Boolean;
{Add a free FIQ block, sorted by ascending Size order}
{Caller must hold the FIQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;

 {Find Next Free}
 Prev:=nil;
 Next:=FreeFIQBlocks;
 while (Next <> nil) and (Next^.Size < Block^.Size) do
  begin
   Prev:=Next;
   Next:=Next^.NextLink;
  end;
 
 {Check Next}
 if Next <> nil then
  begin
   {Add before Next Free}
   if Prev <> nil then
    begin
     {Add after Previous Free}
     Prev^.NextLink:=Block;
     Block^.PrevLink:=Prev;
    end
   else
    begin
     {Add as First Free}
     FreeFIQBlocks:=Block;
     Block^.PrevLink:=nil;
    end;
   Next^.PrevLink:=Block;
   Block^.NextLink:=Next;
  end
 else
  begin
   {Add as Last Free}
   if Prev <> nil then
    begin
     {Add after Previous Free}
     Prev^.NextLink:=Block;
     Block^.PrevLink:=Prev;
    end
   else
    begin
     {Add as First Free}
     FreeFIQBlocks:=Block;
     Block^.PrevLink:=nil;
    end;
   Block^.NextLink:=nil; 
  end;
 
 {Mark as Free}
 Block^.State:=HEAP_SIGNATURE + HEAP_STATE_FREE;
 
 Result:=True; 
end;

{==============================================================================}

function RemoveFreeFIQBlock(Block:PHeapBlock):Boolean;
{Remove a free FIQ block}
{Caller must hold the FIQ heap lock}
var
 Prev:PHeapBlock;
 Next:PHeapBlock;
begin
 {}
 Result:=False;
 
 {Check Block}
 if Block = nil then Exit;

 {Get Prev/Next}
 Prev:=Block^.PrevLink;
 Next:=Block^.NextLink;
 
 {Check Next}
 if Next <> nil then
  begin
   {Remove from Next Free}
   if Prev <> nil then
    begin
     {Remove from Previous Free}
     Prev^.NextLink:=Next;
    end
   else
    begin
     {Remove First Free}
     FreeFIQBlocks:=Next;
    end;    
   Next^.PrevLink:=Prev; 
  end
 else
  begin   
   {Remove Last Free}
   if Prev <> nil then
    begin
     {Remove from Previous Free}
     Prev^.NextLink:=nil;
    end
   else
    begin
     {Remove First Free}
     FreeFIQBlocks:=nil;
    end;    
  end;  
 Block^.PrevLink:=nil;
 Block^.NextLink:=nil;

 {Mark as Used}
 Block^.State:=HEAP_SIGNATURE + HEAP_STATE_USED;
   
 Result:=True; 
end;

{==============================================================================}
{==============================================================================}
{RTL Heap Manager Functions}
function SysGetMem(Size:PtrUInt):Pointer;
{Allocate a block of normal memory}
var
 Block:PHeapBlock;
 Split:PHeapBlock;
 AllocSize:PtrUInt;
 RemainSize:PtrUInt;
begin
 {}
 Result:=nil;
 
 AcquireHeapLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.GetCount);
  {$ENDIF}

  {Check Size}
  if Size = 0 then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetZeroCount);
    {$ENDIF}
    Size:=4;
   end;
  
  {Determine Size}
  AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
 
  {Get Free Block}
  Block:=GetFreeBlock(AllocSize);
  if Block = nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetUnavailableCount);
    {$ENDIF}
    Exit;
   end;

  {Remove Free Block}
  if not RemoveFreeBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemoveFailCount);
    {$ENDIF}
    Exit;
   end;
    
  {Determine Remain Size}
  if (Block^.Size - AllocSize) >= HEAP_MIN_BLOCK then
   begin
    RemainSize:=(Block^.Size - AllocSize);
   end
  else
   begin
    AllocSize:=Block^.Size;
    RemainSize:=0;
   end;
   
  {Check Remain Size}
  if RemainSize > 0 then 
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetRemainCount); 
    {$ENDIF}
    
    {Split Block}
    Split:=SplitHeapBlock(Block,AllocSize);
    if Split = nil then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetSplitFailCount);
      {$ENDIF}
      Exit;
     end; 
    
    {Add Free Block}
    if not AddFreeBlock(Split) then
     begin
      {$IFDEF HEAP_STATISTICS}
      {Update Heap Statistics}
      Inc(HeapStatistics.GetAddFailCount);
      {$ENDIF}
      Exit;
     end;
   end; 

  {Add Used Block}
  if not AddUsedBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.GetAddFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Update Heap Status}
  {Free}
  Dec(HeapStatus.TotalUncommitted,AllocSize);
  Dec(HeapStatus.TotalFree,AllocSize);
  Dec(HeapStatus.Unused,AllocSize);
  {Used}  
  Inc(HeapStatus.TotalCommitted,AllocSize);
  Inc(HeapStatus.TotalAllocated,AllocSize);
   
  {Update FPC Heap Status}
  {Free}
  Dec(FPCHeapStatus.CurrHeapFree,AllocSize);
  {Used}
  Inc(FPCHeapStatus.CurrHeapUsed,AllocSize);
  {Max}
  if FPCHeapStatus.CurrHeapUsed > FPCHeapStatus.MaxHeapUsed then FPCHeapStatus.MaxHeapUsed:=FPCHeapStatus.CurrHeapUsed;
     
  {Return Result}
  Result:=Pointer(PtrUInt(Block) + SizeOf(THeapBlock));
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}
  
function SysFreeMem(Addr:Pointer):PtrUInt;
{Free a block of memory}
var
 Block:PHeapBlock;
 Merge:PHeapBlock;
 BlockSize:PtrUInt;
begin
 {}
 Result:=0; 
 
 AcquireHeapLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.FreeCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
  
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));

  {Check Block}
  {if GetUsedBlock(Block) <> Block then}
  if not CheckUsedBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeInvalidCount);
    {$ENDIF}
    Exit;
   end;
  
  {Get Size}
  BlockSize:=Block^.Size;
  
  {Remove Used Block}
  if not RemoveUsedBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeRemoveFailCount);
    {$ENDIF}
    Exit;
   end; 
  
  {Merge Block}
  Merge:=MergeHeapBlock(Block);
  if Merge = nil then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeMergeFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Add Free Block}
  if not AddFreeBlock(Merge) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.FreeAddFailCount);
    {$ENDIF}
    Exit;
   end;
  
  {Update Heap Status}
  {Free}
  Inc(HeapStatus.TotalUncommitted,BlockSize);
  Inc(HeapStatus.TotalFree,BlockSize);
  Inc(HeapStatus.Unused,BlockSize);
  {Used}  
  Dec(HeapStatus.TotalCommitted,BlockSize);
  Dec(HeapStatus.TotalAllocated,BlockSize);
  
  {Update FPC Heap Status}
  {Free}
  Inc(FPCHeapStatus.CurrHeapFree,BlockSize);
  {Used}
  Dec(FPCHeapStatus.CurrHeapUsed,BlockSize);
  
  {Return Result}
  Result:=BlockSize - SizeOf(THeapBlock);
 finally 
  ReleaseHeapLock;
 end;
end;

{==============================================================================}
  
function SysFreeMemSize(Addr:Pointer;Size:PtrUInt):PtrUInt;
{Free a block of memory}
{Note: Size is not currently used}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.FreeSizeCount);  
 {$ENDIF}
 
 {Return Result}
 Result:=SysFreeMem(Addr);
end;

{==============================================================================}
  
function SysAllocMem(Size:PtrUInt):Pointer;
{Allocate and clear a block of normal memory}
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.AllocCount); 
 {$ENDIF}
 
 {Get Memory}
 Result:=SysGetMem(Size);
 if Result <> nil then
  begin
   {Zero Memory}
   FillChar(Result^,SysSizeMem(Result),0); 
  end; 
end;

{==============================================================================}
  
function SysReAllocMem(var Addr:Pointer;Size:PtrUInt):Pointer;
{Reallocate a block of normal memory}
var
 Block:PHeapBlock;
 Split:PHeapBlock;

 NewSize:PtrUInt;
 AllocSize:PtrUInt;
 CurrentSize:PtrUInt;
begin
 {}
 {$IFDEF HEAP_STATISTICS}
 {Update Heap Statistics}
 Inc(HeapStatistics.ReallocCount);
 {$ENDIF}
 
 {Check Size}
 if Size = 0 then
  begin
   {$IFDEF HEAP_STATISTICS}
   {Update Heap Statistics}
   Inc(HeapStatistics.ReallocZeroCount);
   {$ENDIF}
   
   {Free Memory}
   if Addr <> nil then
    begin
     SysFreeMem(Addr);  
     Addr:=nil;
    end; 
   
   {Return Result}
   Result:=Addr;
  end
 else
  begin 
   {Get Size}
   CurrentSize:=SysSizeMem(Addr);
   if (CurrentSize > 0) and (CurrentSize >= Size) then
    begin
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocSmallerCount); 
     {$ENDIF}
  
     {Determine Size}
     AllocSize:=Align(Size + SizeOf(THeapBlock),HEAP_MIN_ALIGNMENT);
     
     {Get Block} 
     Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
     
     {Check Size}
     if Block^.Size >= (AllocSize shl 1) then
      begin
       AcquireHeapLock;
       try
        {$IFDEF HEAP_STATISTICS}
        {Update Heap Statistics}
        Inc(HeapStatistics.ReallocReleaseCount); 
        Inc(HeapStatistics.ReallocReleaseBytes,Block^.Size - AllocSize); 
        {$ENDIF}
        
        {Remove Used Block}
        if not RemoveUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocRemoveFailCount);
          {$ENDIF}
          Exit;
         end;
  
        {Split Block}
        Split:=SplitHeapBlock(Block,AllocSize);
        if Split = nil then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocSplitFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Free Block}
        if not AddFreeBlock(Split) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Add Used Block}
        if not AddUsedBlock(Block) then
         begin
          {$IFDEF HEAP_STATISTICS}
          {Update Heap Statistics}
          Inc(HeapStatistics.ReallocAddFailCount);
          {$ENDIF}
          Exit;
         end;
         
        {Update Heap Status}
        {Free}
        Inc(HeapStatus.TotalUncommitted,Split^.Size);
        Inc(HeapStatus.TotalFree,Split^.Size);
        Inc(HeapStatus.Unused,Split^.Size);
        {Used}  
        Dec(HeapStatus.TotalCommitted,Split^.Size);
        Dec(HeapStatus.TotalAllocated,Split^.Size);
        
        {Update FPC Heap Status}
        {Free}
        Inc(FPCHeapStatus.CurrHeapFree,Split^.Size);
        {Used}
        Dec(FPCHeapStatus.CurrHeapUsed,Split^.Size);
       finally
        ReleaseHeapLock; 
       end;      
      end;
     
     {Return Result}
     Result:=Addr;
    end
   else
    begin 
     {$IFDEF HEAP_STATISTICS}
     {Update Heap Statistics}
     Inc(HeapStatistics.ReallocLargerCount); 
     {$ENDIF}
  
     {Alloc Memory}
     Result:=SysAllocMem(Size); 
     if Result <> nil then
      begin
       if Addr <> nil then
        begin
         {Get Size}
         NewSize:=SysSizeMem(Result);
         {CurrentSize:=SysSizeMem(Addr);} {Done above} 
         if CurrentSize > NewSize then CurrentSize:=NewSize; 
  
         {Copy Memory}
         System.Move(Addr^,Result^,CurrentSize);
        end;
      end;
  
     {Free Memory}
     if Addr <> nil then SysFreeMem(Addr);  
  
     {Return Result}
     Addr:=Result;
    end; 
  end;
end;

{==============================================================================}

function SysSizeMem(Addr:Pointer):PtrUInt;
{Return the size of an allocated block of memory}
var
 Block:PHeapBlock;
begin
 {}
 Result:=0;
 
 AcquireHeapLock;
 try
  {$IFDEF HEAP_STATISTICS}
  {Update Heap Statistics}
  Inc(HeapStatistics.SizeCount); 
  {$ENDIF}
 
  {Check Addr}
  if Addr = nil then Exit;
 
  {Get Block}
  Block:=PHeapBlock(PtrUInt(PtrUInt(Addr) - SizeOf(THeapBlock)));
  
  {Check Block}
  {if GetUsedBlock(Block) <> Block then}
  if not CheckUsedBlock(Block) then
   begin
    {$IFDEF HEAP_STATISTICS}
    {Update Heap Statistics}
    Inc(HeapStatistics.SizeInvalidCount);
    {$ENDIF}
    Exit;
   end;
   
  {Return Result}
  Result:=Block^.Size - SizeOf(THeapBlock);
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

procedure SysInitThread;
{Initialize thread specific heap information}
begin
 {}
 {Nothing}
end;

{==============================================================================}

procedure SysDoneThread;
{Finalize thread specific heap information}
begin
 {}
 {Nothing}
end;

{==============================================================================}

procedure SysRelocateHeap;
{Relocate heap data}
begin
 {}
 {Nothing}
end;

{==============================================================================}

function SysGetHeapStatus:THeapStatus;
{Return status information for the heap manager}
begin
 {}
 FillChar(Result,SizeOf(THeapStatus),0);
 
 AcquireHeapLock;
 try
  {Copy Status}
  System.Move(HeapStatus,Result,SizeOf(THeapStatus));
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}

function SysGetFPCHeapStatus:TFPCHeapStatus;
{Return status information for the heap manager}
begin
 {}
 FillChar(Result,SizeOf(TFPCHeapStatus),0);
 
 AcquireHeapLock;
 try
  {Copy Status}
  System.Move(FPCHeapStatus,Result,SizeOf(TFPCHeapStatus));
 finally
  ReleaseHeapLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{Helper Functions}
procedure AcquireHeapLock; inline;
begin
 {}
 if (HeapLock.Lock <> INVALID_HANDLE_VALUE) and Assigned(HeapLock.AcquireLock) then
  begin
   HeapLock.AcquireLock(HeapLock.Lock);
  end;
end;

{==============================================================================}

procedure ReleaseHeapLock; inline;
begin
 {}
 if (HeapLock.Lock <> INVALID_HANDLE_VALUE) and Assigned(HeapLock.ReleaseLock) then
  begin
   HeapLock.ReleaseLock(HeapLock.Lock);
  end;
end;

{==============================================================================}

procedure AcquireHeapIRQLock; inline;
begin
 {}
 if (HeapLock.IRQLock <> INVALID_HANDLE_VALUE) and Assigned(HeapLock.AcquireIRQLock) then
  begin
   HeapLock.AcquireIRQLock(HeapLock.IRQLock);
  end;
end;

{==============================================================================}

procedure ReleaseHeapIRQLock; inline;
begin
 {}
 if (HeapLock.IRQLock <> INVALID_HANDLE_VALUE) and Assigned(HeapLock.ReleaseIRQLock) then
  begin
   HeapLock.ReleaseIRQLock(HeapLock.IRQLock);
  end;
end;

{==============================================================================}

procedure AcquireHeapFIQLock; inline;
begin
 {}
 if (HeapLock.FIQLock <> INVALID_HANDLE_VALUE) and Assigned(HeapLock.AcquireFIQLock) then
  begin
   HeapLock.AcquireFIQLock(HeapLock.FIQLock);
  end;
end;

{==============================================================================}

procedure ReleaseHeapFIQLock; inline;
begin
 {}
 if (HeapLock.FIQLock <> INVALID_HANDLE_VALUE) and Assigned(HeapLock.ReleaseFIQLock) then
  begin
   HeapLock.ReleaseFIQLock(HeapLock.FIQLock);
  end;
end;

{==============================================================================}

procedure RegisterHeapLock(const Lock:THeapLock);
begin
 {}
 HeapLock:=Lock;
end;

{==============================================================================}

function HeapStateToString(State:LongWord):String;
begin
 {}
 Result:='';
 
 case State of 
  HEAP_STATE_FREE:Result:='HEAP_STATE_FREE';
  HEAP_STATE_USED:Result:='HEAP_STATE_USED';
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 RegisterMemoryManager;
 
{==============================================================================}
 
finalization
 {Nothing}
  
{==============================================================================}
{==============================================================================}
  
end.
