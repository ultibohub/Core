{
Ultibo Platform interface unit for AARCH64 (ARM64).

Copyright (C) 2021 - SoftOz Pty Ltd.

Arch
====

 ARMv8 (Cortex A53/A57/A72)
 
Boards
======

 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3/CM3+
 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4
 QEMU VersatilePB
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


Platform AARCH64
================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformAARCH64; 

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,HeapManager,Threads{$IFDEF DEVICE_TREE_ENABLE},DeviceTree{$ENDIF DEVICE_TREE_ENABLE},SysUtils; 

{==============================================================================}
//const
 {AARCH64 specific constants}
 //To Do
 
{==============================================================================}
const
 {Definitions of ARM Boot Tags}  
 ATAG_NONE       = $00000000;
 ATAG_CORE       = $54410001;
 ATAG_MEM        = $54410002;
 ATAG_VIDEOTEXT  = $54410003;
 ATAG_RAMDISK    = $54410004;
 ATAG_INITRD     = $54410005; {Deprecated}
 ATAG_INITRD2    = $54420005;
 ATAG_SERIAL     = $54410006;
 ATAG_REVISION   = $54410007;
 ATAG_VIDEOLFB   = $54410008;
 ATAG_CMDLINE    = $54410009;
 
 ARMTAGS_INITIAL = $FFFFFFFF;
 
{==============================================================================}
const
 {Definitions of Device Tree Blob}  
 DTB_SIGNATURE = $d00dfeed; {See: https://github.com/devicetree-org/devicetree-specification/releases/download/v0.3/devicetree-specification-v0.3.pdf}
 
{==============================================================================}
const
 {Definitions of ARM Machine Types}
 ARM_MACHINE_VERSATILE_PB = $00000183;
 ARM_MACHINE_BCM2708      = $00000C42;
 ARM_MACHINE_BCM2709      = $00000C42; {BCM2709 uses the same Machine Type as BCM2708}
 ARM_MACHINE_BCM2710      = $00000C42; {BCM2710 uses the same Machine Type as BCM2708}
 ARM_MACHINE_BCM2711      = $00000C42; {BCM2711 uses the same Machine Type as BCM2708}
 
{==============================================================================}
type
 {AARCH64 specific types}
 
 {ARM Boot Tag Structure Definitions} 
 {ARM Boot Tag header}
 PARMTagHeader = ^TARMTagHeader;
 TARMTagHeader = record
  Size:LongWord;  {Size of tag, in words (32bit), including the header.}
  Tag:LongWord;   {One of the ATAG_* values from above.}
 end;

 {Core parameters (ATAG_CORE)}
 PARMTagCore = ^TARMTagCore;
 TARMTagCore = record
  Flags:LongWord;              {Bit 0 = read-only}
  PageSize:LongWord;           {Systems page size (usually 4k)}
  RootDev:LongWord;            {Root device number}
 end;
 
 {Description of memory region (ATAG_MEM)}
 PARMTagMemory = ^TARMTagMemory;
 TARMTagMemory = record
  Size:LongWord;
  Start:LongWord;
 end;

 {Description of VGA text type displays (ATAG_VIDEOTEXT)}
 PARMTagVideoText = ^TARMTagVideoText;
 TARMTagVideoText = record
  X:Byte;           {Width of display}
  Y:Byte;           {Height of display}
  Video_page:Word;
  Video_mode:Byte;
  Video_cols:Byte;
  Video_ega_bx:Word;
  Video_lines:Byte;
  Video_isvga:Byte;
  Video_points:Word; 
 end;
 
 {Description of how the ramdisk will be used by the kernel (ATAG_RAMDISK)}
 PARMTagRamdisk = ^TARMTagRamdisk;
 TARMTagRamdisk = record
  Flags:LongWord;      {Bit 0 = load, Bit 1 = prompt}
  Size:LongWord;       {Decompressed ramdisk size in _kilo_ bytes}
  Start:LongWord;      {Starting block of floppy-based RAM disk image}
 end;
 
 {Description of the physical location of the compressed ramdisk image (ATAG_INITRD2)}
 PARMTagInitRd2 = ^TARMTagInitRd2;
 TARMTagInitRd2 = record
  Start:LongWord;      {Physical start address}
  Size:LongWord;       {Size of compressed ramdisk image in bytes}
 end;
 
 {Board serial number (ATAG_SERIAL)}
 PARMTagSerial = ^TARMTagSerial;
 TARMTagSerial = record
  Low:LongWord;
  High:LongWord;
 end; 

 {Board revision (ATAG_REVISION)}
 PARMTagRevision = ^TARMTagRevision;
 TARMTagRevision = record
  Revision:LongWord;
 end;
 
 {Description of the parameters for a linear framebuffer type display (ATAG_VIDEOLFB)}
 PARMTagVideoFB = ^TARMTagVideoFB;
 TARMTagVideoFB = record
  Lfb_width:Word;
  Lfb_height:Word;
  Lfb_depth:Word;
  Lfb_linelength:Word;
  Lfb_base:LongWord;
  Lfb_size:LongWord;
  Red_size:Byte;
  Red_pos:Byte;
  Green_size:Byte;
  Green_pos:Byte;
  Blue_size:Byte;
  Blue_pos:Byte;
  Rsvd_size:Byte;
  Rsvd_pos:Byte; 
 end;
 
 {Commandline for the kernel (ATAG_CMDLINE)}
 PARMTagCommand = ^TARMTagCommand;
 TARMTagCommand = record
  Cmdline:array[0..0] of Char;     {This is the minimum size} 
 end;
 
 {Format of ARM Boot Tag}
 PARMTag = ^TARMTag;
 TARMTag = record
  Header:TARMTagHeader;
  case Integer of
   0:(Core:TARMTagCore);
   1:(Memory:TARMTagMemory);
   2:(VideoText:TARMTagVideoText);
   3:(Ramdisk:TARMTagRamdisk);
   4:(InitRd2:TARMTagInitRd2);
   5:(Serial:TARMTagSerial);
   6:(Revision:TARMTagRevision);
   7:(VideoFB:TARMTagVideoFB);
   8:(Command:TARMTagCommand)
 end; 
 
{$IFNDEF DEVICE_TREE_ENABLE} 
type
 {Device Tree Blob header}
 PDTBHeader = ^TDTBHeader;
 TDTBHeader = packed record
  Magic:LongWord;               {The value 0xd00dfeed (big-endian)}
  TotalSize:LongWord;           {The total size in bytes of the devicetree data structure (big-endian)}
  StructureOffset:LongWord;     {The offset in bytes of the structure block from the beginning of the header (big-endian)}
  StringsOffset:LongWord;       {The offset in bytes of the strings block from the beginning of the header (big-endian)}
  ReservationOffset:LongWord;   {The offset in bytes of the memory reservation block from the beginning of the header (big-endian)}
  Version:LongWord;             {The version of the devicetree data structure (big-endian)}
  CompatibleVersion:LongWord;   {The lowest version of the devicetree data structure with which the version used is backwards compatible (big-endian)}
  BootCPUID:LongWord;           {The physical ID of the system’s boot CPU (big-endian)}
  StringsSize:LongWord;         {The length in bytes of the strings block section of the devicetree blob (big-endian)}
  StructureSize:LongWord;       {The length in bytes of the structure block section of the devicetree blob (big-endian)}
 end;
{$ENDIF DEVICE_TREE_ENABLE}
 
type 
 {Prototypes for Wait Handlers}
 TAARCH64Wait = procedure;
 TAARCH64LongWait = procedure;
 TAARCH64ShortWait = procedure;
 
type 
 {Prototypes for Blink Handlers}
 TAARCH64SlowBlink = procedure;
 TAARCH64FastBlink = procedure;
 
{==============================================================================}
var
 {AARCH64 specific variables}
 AARCH64Initialized:Boolean;
 
 AARCH64BootMode:LongWord = 0;                 {The ARM Mode that the processor was in at boot time (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 AARCH64BootVectors:LongWord = 0;              {The Vector Base Address that was current at boot time (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 AARCH64TagsAddress:PtrUInt = ARMTAGS_INITIAL; {Pointer to the ARM TAGS provided by the bootloader at startup (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 AARCH64MachineType:LongWord = 0;              {ARM Machine Type provided by the bootloader at startup (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 AARCH64SecureBoot:LongWord = 1;               {If 1 then startup will attempt to switch back to secure world during boot process (If supported by the AARCH64 boot stub)} 
 
var
 {ARM Tags Variables}
 ARMTagsCount:LongWord;     {Number of ARM Tags found during parse}
 
 {Tag None Variables}
 TagNoneCount:LongWord;     {Number of ARM NONE Tags found during parse}
 
 {Tag Core Variables}
 TagCoreCount:LongWord;     {Number of ARM CORE Tags found during parse}
 TagCoreFlags:LongWord;
 TagCorePageSize:LongWord;
 TagCoreRootDevice:LongWord;

 {Tag Memory Variables}
 TagMemoryCount:LongWord;   {Number of ARM MEM Tags found during parse}
 TagMemorySize:LongWord;    {Size of the last block reported by ARM Tags}
 TagMemoryStart:PtrUInt;    {Start of the last block reported by ARM Tags}
 TagMemoryLength:LongWord;  {Adjusted Size of the last block reported by ARM Tags}
 TagMemoryAddress:PtrUInt;  {Adjusted Address of the last block reported by ARM Tags}

 {Tag Video Text Variables}
 TagVideoTextCount:LongWord;{Number of ARM VIDEOTEXT Tags found during parse}
 
 {Tag Ramdisk Variables}
 TagRamdiskCount:LongWord;  {Number of ARM RAMDISK Tags found during parse}
 
 {Tag Init RD Variables}
 TagInitRdCount:LongWord;   {Number of ARM INITRD Tags found during parse (Deprecated)}
 
 {Tag Init RD2 Variables}
 TagInitRd2Count:LongWord;  {Number of ARM INITRD2 Tags found during parse}
 TagInitRd2Start:LongWord;
 TagInitRd2Size:LongWord;
 
 {Tag Serial Variables}
 TagSerialCount:LongWord;   {Number of ARM SERIAL Tags found during parse}
 TagSerialNoLow:LongWord;
 TagSerialNoHigh:LongWord;

 {Tag Revision Variables}
 TagRevisionCount:LongWord; {Number of ARM REVISION Tags found during parse}
 TagRevisionNo:LongWord;

 {Tag Video FB Variables}
 TagVideoFBCount:LongWord;  {Number of ARM VIDEOLFB Tags found during parse}

 {Tag Command Variables}
 TagCmdCount:LongWord;      {Number of ARM CMDLINE Tags found during parse}
 TagCommandSize:LongWord;   {Length of the command line in characters (Including null terminator)}
 TagCommandCount:LongInt;   {Count of parameters (space delimited) in the command line}
 TagCommandAddress:PChar;   {Pointer to the start of the command line}

var
 {Wait Handlers}
 AARCH64WaitHandler:TAARCH64Wait;
 AARCH64LongWaitHandler:TAARCH64LongWait;
 AARCH64ShortWaitHandler:TAARCH64ShortWait;
 
var
 {Blink Handlers}
 AARCH64SlowBlinkHandler:TAARCH64SlowBlink;
 AARCH64FastBlinkHandler:TAARCH64FastBlink;
 
{==============================================================================}
{Initialization Functions}
procedure AARCH64Init;
 
{==============================================================================}
{AARCH64 Platform Functions}
procedure AARCH64ParseBootTags;
procedure AARCH64ParseCommandLine;
procedure AARCH64ParseEnvironment;

function AARCH64GetSP:PtrUInt;
function AARCH64GetPC:PtrUInt;

function AARCH64GetIRQ:Boolean;
procedure AARCH64EnableIRQ; 
procedure AARCH64DisableIRQ; 
function AARCH64SaveIRQ:TIRQMask;
function AARCH64RestoreIRQ(IRQMask:TIRQMask):TIRQMask; 

function AARCH64GetFIQ:Boolean;
procedure AARCH64EnableFIQ; 
procedure AARCH64DisableFIQ; 
function AARCH64SaveFIQ:TFIQMask;
function AARCH64RestoreFIQ(FIQMask:TFIQMask):TFIQMask; 

procedure AARCH64EnableIRQFIQ;
procedure AARCH64DisableIRQFIQ;
function AARCH64SaveIRQFIQ:TIRQFIQMask;
function AARCH64RestoreIRQFIQ(IRQFIQMask:TIRQFIQMask):TIRQFIQMask;

function AARCH64GetAbort:Boolean;
procedure AARCH64EnableAbort;
procedure AARCH64DisableAbort;
function AARCH64SaveAbort:TAbortMask;
function AARCH64RestoreAbort(AbortMask:TAbortMask):TAbortMask;
 
{==============================================================================}
{AARCH64 Helper Functions}
procedure AARCH64Wait; inline;
procedure AARCH64LongWait; inline;
procedure AARCH64ShortWait; inline;

procedure AARCH64SlowBlink; inline;
procedure AARCH64FastBlink; inline;

function AARCH64ModeToString(AARCH64Mode:LongWord):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure AARCH64Init;
begin
 {}
 if AARCH64Initialized then Exit;
 
 {Setup STACK_MIN_ALIGNMENT} 
 STACK_MIN_ALIGNMENT:=SIZE_16;
 
 {Register Platform ParseBootTags Handler}
 ParseBootTagsHandler:=AARCH64ParseBootTags;

 {Register Platform ParseCommandLine Handler}
 ParseCommandLineHandler:=AARCH64ParseCommandLine;

 {Register Platform ParseEnvironment Handler}
 ParseEnvironmentHandler:=AARCH64ParseEnvironment;
 
 {Register Platform GetSP/PC Handlers}
 GetSPHandler:=AARCH64GetSP;
 GetPCHandler:=AARCH64GetPC;
 
 {Register Platform Enable/Disable/Save/RestoreIRQ Handlers}
 GetIRQHandler:=AARCH64GetIRQ;
 EnableIRQHandler:=AARCH64EnableIRQ;
 DisableIRQHandler:=AARCH64DisableIRQ;
 SaveIRQHandler:=AARCH64SaveIRQ;
 RestoreIRQHandler:=AARCH64RestoreIRQ;

 {Register Platform Enable/Disable/Save/RestoreFIQ Handlers}
 GetFIQHandler:=AARCH64GetFIQ;
 EnableFIQHandler:=AARCH64EnableFIQ;
 DisableFIQHandler:=AARCH64DisableFIQ;
 SaveFIQHandler:=AARCH64SaveFIQ;
 RestoreFIQHandler:=AARCH64RestoreFIQ;
 
 {Register Platform Enable/Disable/Save/RestoreIRQFIQ Handlers}
 EnableIRQFIQHandler:=AARCH64EnableIRQFIQ;
 DisableIRQFIQHandler:=AARCH64DisableIRQFIQ;
 SaveIRQFIQHandler:=AARCH64SaveIRQFIQ;
 RestoreIRQFIQHandler:=AARCH64RestoreIRQFIQ;
 
 {Register Platform Enable/Disable/Save/RestoreAbort Handlers}
 GetAbortHandler:=AARCH64GetAbort;
 EnableAbortHandler:=AARCH64EnableAbort;
 DisableAbortHandler:=AARCH64DisableAbort;
 SaveAbortHandler:=AARCH64SaveAbort;
 RestoreAbortHandler:=AARCH64RestoreAbort;
 
 AARCH64Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{AARCH64 Platform Functions}
procedure AARCH64ParseBootTags;
{Extract some information from the ARM boot tag list and use it to load the
 memory manager, some other information is stored in variables for future use}
 
 function ExtractCommandLine(Value:PChar):Boolean;
 var
  CommandOffset:PChar;
  CommandLength:LongWord;
 begin
  {}
  Result:=False;
  
  if Value = nil then Exit;
  
  {Save Address}
  TagCommandAddress:=Value;
  
  {Count the command line parameters}
  TagCommandSize:=SizeOf(Char); {Must be at least the null terminator}
  TagCommandCount:=0;
  CommandOffset:=TagCommandAddress;
  CommandLength:=0;
  while CommandOffset^ <> #0 do
   begin
    if CommandOffset^ = #32 then
     begin
      if CommandLength > 0 then
       begin
        Inc(TagCommandCount);
       end;
       
      CommandLength:=0;
     end
    else
     begin
      Inc(CommandLength);
     end;             
    Inc(TagCommandSize,SizeOf(Char));
    Inc(CommandOffset,SizeOf(Char)); 
   end; 
  
  {Check last paramter}
  if CommandLength > 0 then
   begin
    Inc(TagCommandCount);
   end;
   
  Result:=True; 
 end;
 
 function ExtractMemoryBlock(Address,Size:LongWord):Boolean;
 var
  BlockSize:LongWord;
  BlockAddress:PtrUInt;
  StartAddress:PtrUInt;
 begin
  {}
  Result:=False;
  
  if Size = 0 then Exit;

  BlockAddress:=Address;
  BlockSize:=Size;
  
  {Save Size and Start}
  TagMemorySize:=BlockSize;
  TagMemoryStart:=BlockAddress;
  if BlockSize > 0 then
   begin
    StartAddress:=BlockAddress;
    if BlockAddress < (INITIAL_HEAP_BASE + INITIAL_HEAP_SIZE) then
     begin
      StartAddress:=INITIAL_HEAP_BASE + INITIAL_HEAP_SIZE;
      if (StartAddress - BlockAddress) < BlockSize then
       begin
        BlockSize:=BlockSize - (StartAddress - BlockAddress);
       end
      else
       begin
        BlockSize:=0; 
       end;
     end;
    
    {Save Address and Length}
    TagMemoryAddress:=StartAddress;
    TagMemoryLength:=BlockSize;
    if BlockSize > 0 then
     begin
      {Register Block}
      RegisterHeapBlock(Pointer(StartAddress),BlockSize);
     end;           
   end;
  
  Result:=True; 
 end;
 
 function ExtractInitialRamdisk(Address,Size:LongWord):Boolean;
 begin
  {}
  Result:=False;
  
  if (Address = 0) or (Size = 0) then Exit;
  
  {Save Address and Size}
  TagInitRd2Start:=Address;
  TagInitRd2Size:=Size;
  
  {Reserve Ramdisk memory}
  if ReserveHeapBlock(Pointer(TagInitRd2Start),TagInitRd2Size) <> nil then
   begin
    {Update Configuration}
    INITIAL_RAMDISK_BASE:=Address;
    INITIAL_RAMDISK_SIZE:=Size;
   end; 
  
  Result:=True; 
 end;
 
var
 ARMTag:PARMTag;

 Size:UInt64;
 Address:PtrUInt;
begin
 {}
 {Check Tags Count}
 if ARMTagsCount = 0 then
  begin
   {Initialize Counts}
   ARMTagsCount:=0;
   TagNoneCount:=0;
   TagCoreCount:=0;
   TagMemoryCount:=0;
   TagVideoTextCount:=0;
   TagRamdiskCount:=0;
   TagInitRdCount:=0;
   TagInitRd2Count:=0;
   TagSerialCount:=0;
   TagRevisionCount:=0;
   TagVideoFBCount:=0;
   TagCmdCount:=0;
   
   {$IFDEF DEVICE_TREE_ENABLE}
   {Check for valid Device Tree Blob}
   if (AARCH64TagsAddress <> ARMTAGS_INITIAL) then
    begin
     DEVICE_TREE_VALID:=DeviceTreeValidate(AARCH64TagsAddress,DEVICE_TREE_SIZE);
     if DEVICE_TREE_VALID then DEVICE_TREE_BASE:=AARCH64TagsAddress;
    end;
    
   {Check for default Tag Address value or for Device Tree Blob available}
   if (AARCH64TagsAddress = ARMTAGS_INITIAL) or DEVICE_TREE_VALID then
    begin
     {Device Tree Blob supplied or ARM tags not present}
     {Check Device Tree}
     if DEVICE_TREE_VALID then
      begin
       {Get Memory Block (First)}
       if not DeviceTreeGetMemory(0,Address,Size) then
        begin
         {Get Default Block}
         Address:=CPU_MEMORY_BASE;
         Size:=CPU_MEMORY_SIZE;
        end;
       if ExtractMemoryBlock(Address,Size) then
        begin
         Inc(ARMTagsCount);
         Inc(TagMemoryCount);
        end;
       
       {Get Command Line}
       if ExtractCommandLine(DeviceTreeGetBootArgs) then
        begin
         Inc(ARMTagsCount);
         Inc(TagCmdCount);
        end;
       
       {Get Ramdisk}
       if DeviceTreeGetRamdisk(Address,Size) then
        begin
         if ExtractInitialRamdisk(Address,Size) then
          begin
           Inc(ARMTagsCount);
           Inc(TagInitRd2Count);
          end;
        end;

       {Reserve Device Tree memory}
       ReserveHeapBlock(Pointer(DEVICE_TREE_BASE),DEVICE_TREE_SIZE);
      end
     else
      begin
       {Get Memory Block}
       if ExtractMemoryBlock(CPU_MEMORY_BASE,CPU_MEMORY_SIZE) then
        begin
         Inc(ARMTagsCount);
         Inc(TagMemoryCount);
        end;
      end;  
    end
   {$ELSE DEVICE_TREE_ENABLE}
   {Check for default Tag Address value and for Device Tree Blob signature}
   if (AARCH64TagsAddress = ARMTAGS_INITIAL) or (LongWordBEtoN(PLongWord(AARCH64TagsAddress)^) = DTB_SIGNATURE) then
    begin
     {Device Tree Blob supplied or ARM tags not present}
     {Check Device Tree}
     if LongWordBEtoN(PLongWord(AARCH64TagsAddress)^) = DTB_SIGNATURE then
      begin
       DEVICE_TREE_VALID:=True;
       DEVICE_TREE_BASE:=AARCH64TagsAddress;
       DEVICE_TREE_SIZE:=LongWordBEtoN(PDTBHeader(AARCH64TagsAddress).TotalSize);
      end;

     {Get Memory Block}
     if ExtractMemoryBlock(CPU_MEMORY_BASE,CPU_MEMORY_SIZE) then
      begin
       Inc(ARMTagsCount);
       Inc(TagMemoryCount);
      end;
    end
   {$ENDIF DEVICE_TREE_ENABLE}
   else
    begin
     {ARM Tags address supplied}
     {Get First Tag}
     ARMTag:=PARMTag(AARCH64TagsAddress);
     while (ARMTag.Header.Size >= 2) and (ARMTag.Header.Tag <> ATAG_NONE) do
      begin
       Inc(ARMTagsCount);
       {Check Tag Type}
       case ARMTag.Header.Tag of
        ATAG_NONE:begin
          {NONE}
          Inc(TagNoneCount);
          {Must be last in the list, will have a size of 0}
         end;    
        ATAG_CORE:begin
          {CORE}
          Inc(TagCoreCount);
          {Must be the first in the list, size may be 2 to indicate no data}
          if ARMTag.Header.Size > 2 then
           begin
            TagCoreFlags:=ARMTag.Core.Flags;
            TagCorePageSize:=ARMTag.Core.PageSize;
            TagCoreRootDevice:=ARMTag.Core.RootDev;
           end;
         end;    
        ATAG_MEM:begin      
          {MEM}
          Inc(TagMemoryCount);
          if ARMTag.Header.Size > 2 then
           begin
            ExtractMemoryBlock(ARMTag.Memory.Start,ARMTag.Memory.Size);
           end; 
         end;    
        ATAG_VIDEOTEXT:begin
          {VIDEOTEXT}
          Inc(TagVideoTextCount);
          if ARMTag.Header.Size > 2 then
           begin
            {Not relevant to Ultibo}
           end;
         end;    
        ATAG_RAMDISK:begin 
          {RAMDISK}
          Inc(TagRamdiskCount);
          {Not relevant to Ultibo}
         end;    
        ATAG_INITRD:begin
          {INITRD}
          Inc(TagInitRdCount);
          {Not relevant to Ultibo (Deprecated)}
         end;    
        ATAG_INITRD2:begin
          {INITRD2}
          Inc(TagInitRd2Count);
          if ARMTag.Header.Size > 2 then
           begin
            ExtractInitialRamdisk(ARMTag.InitRd2.Start,ARMTag.InitRd2.Size);
           end;
         end;    
        ATAG_SERIAL:begin   
          {SERIAL}
          Inc(TagSerialCount);
          if ARMTag.Header.Size > 2 then
           begin
            TagSerialNoLow:=ARMTag.Serial.Low;
            TagSerialNoHigh:=ARMTag.Serial.High;
           end;
         end;    
        ATAG_REVISION:begin
          {REVISION}
          Inc(TagRevisionCount);
          if ARMTag.Header.Size > 2 then
           begin
            TagRevisionNo:=ARMTag.Revision.Revision;
           end;
         end;    
        ATAG_VIDEOLFB:begin
          {VIDEOLFB}
          Inc(TagVideoFBCount);
          if ARMTag.Header.Size > 2 then
           begin
            {Not relevant to Ultibo}
           end;
         end;    
        ATAG_CMDLINE:begin
          {CMDLINE}
          Inc(TagCmdCount);
          if ARMTag.Header.Size > 2 then
           begin
            ExtractCommandLine(@ARMTag.Command.Cmdline[0]);
           end;
         end;    
       end;
       
       {Get Next Tag}
       ARMTag:=PARMTag(PtrUInt(ARMTag) + (ARMTag.Header.Size * SizeOf(LongWord)));
      end;
    end;
  end;
end;

{==============================================================================}

procedure AARCH64ParseCommandLine;
{Setup argc, argv and cmdline and process known command line options}
var
 CommandStart:PChar;
 CommandOffset:PChar;
 CommandCount:LongWord;
 CommandLength:LongWord;
begin
 {}
 {Check argv}
 if argv = nil then
  begin
   {Update argc}
   argc:=TagCommandCount + 1; {Add one for ParamStr(0)}
     
   {Update argv}
   argv:=AllocMem(SizeOf(PChar) * (TagCommandCount + 1)); {Add one for ParamStr(0)}
   
   {Get Kernal Name}
   CommandCount:=0;
   argv[CommandCount]:=KERNEL_NAME;
     
   {Check Command Line}
   if TagCommandAddress <> nil then
    begin
     {Check Command Count}
     if TagCommandCount > 0 then
      begin
       {Get First Command}
       CommandOffset:=TagCommandAddress;
       CommandStart:=CommandOffset;
       CommandLength:=0;
       while CommandOffset^ <> #0 do
        begin
         if CommandOffset^ = #32 then
          begin
           {Check Length}
           if CommandLength > 0 then
            begin
             {Allocate Command}
             Inc(CommandCount);
             argv[CommandCount]:=AllocMem(SizeOf(Char) * (CommandLength + 1)); {Add one for null terminator}
             
             {Copy Command}
             System.Move(CommandStart^,argv[CommandCount]^,CommandLength);
            end;
            
           {Update Offset}
           Inc(CommandOffset,SizeOf(Char));  
           
           {Get Next Command}
           CommandStart:=CommandOffset;
           CommandLength:=0;
          end
         else
          begin
           {Update Offset}
           Inc(CommandOffset,SizeOf(Char));  
           
           {Update Length}
           Inc(CommandLength);
          end;      
        end;  
       
       {Check Last Command}     
       if CommandLength > 0 then
        begin
         {Allocate Command}
         Inc(CommandCount);
         argv[CommandCount]:=AllocMem(SizeOf(Char) * (CommandLength + 1)); {Add one for null terminator}
         
         {Copy Command}
         System.Move(CommandStart^,argv[CommandCount]^,CommandLength);
        end;
      end;  
     
     {Check Command Size}
     if TagCommandSize > 0 then
      begin
       {Update cmdline}
       cmdline:=AllocMem(TagCommandSize);
       
       {Copy Command Line}
       System.Move(TagCommandAddress^,cmdline^,TagCommandSize);
      end; 
     
     {Process Command Line}
      {No currently supported ARM command line options}
    end; 
  end; 
end;

{==============================================================================}

procedure AARCH64ParseEnvironment;
{Setup envp and process known environment options}
var
 CommandStart:PChar;
 CommandOffset:PChar;
 CommandCount:LongWord;
 CommandLength:LongWord;
begin
 {}
 {Check envp}
 if envp = nil then
  begin
   {Check String Count}
   if ENVIRONMENT_STRING_COUNT < TagCommandCount then
    begin
     ENVIRONMENT_STRING_COUNT:=TagCommandCount;
    end;
    
   {Update envp}
   envp:=AllocMem(SizeOf(PChar) * (ENVIRONMENT_STRING_COUNT + 1)); {Add one for terminating null}
   
   {Check Command Line}
   if TagCommandAddress <> nil then
    begin
     {Check Command Count}
     if TagCommandCount > 0 then
      begin
       {Get First Command}
       CommandOffset:=TagCommandAddress;
       CommandStart:=CommandOffset;
       CommandLength:=0;
       CommandCount:=0;
       while CommandOffset^ <> #0 do
        begin
         if CommandOffset^ = #32 then
          begin
           {Check Length}
           if CommandLength > 0 then
            begin
             {Allocate Command}
             envp[CommandCount]:=AllocMem(SizeOf(Char) * (CommandLength + 1)); {Add one for null terminator}
             
             {Copy Command}
             System.Move(CommandStart^,envp[CommandCount]^,CommandLength);
             
             {Update Count}
             Inc(CommandCount);
            end;
            
           {Update Offset}
           Inc(CommandOffset,SizeOf(Char));  
           
           {Get Next Command}
           CommandStart:=CommandOffset;
           CommandLength:=0;
          end
         else
          begin
           {Update Offset}
           Inc(CommandOffset,SizeOf(Char));  
           
           {Update Length}
           Inc(CommandLength);
          end;      
        end;  
       
       {Check Last Command}     
       if CommandLength > 0 then
        begin
         {Allocate Command}
         envp[CommandCount]:=AllocMem(SizeOf(Char) * (CommandLength + 1)); {Add one for null terminator}
         
         {Copy Command}
         System.Move(CommandStart^,envp[CommandCount]^,CommandLength);
        end;
      end;  
     
     {Process Environment}
      {No currently supported ARM environment options}
    end; 
  end;
end;

{==============================================================================}

function AARCH64GetSP:PtrUInt; assembler; nostackframe; 
{Get the current stack pointer (SP)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64GetPC:PtrUInt; assembler; nostackframe; 
{Get the current program counter (PC)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64GetIRQ:Boolean; assembler; nostackframe; 
{Get Interrupts (IRQ) state}
{Return: True is enabled, False if disabled (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64EnableIRQ; assembler; nostackframe; 
{Enable Interrupts (IRQ) unconditionally}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64DisableIRQ; assembler; nostackframe;  
{Disable Interrupts (IRQ) unconditionally}
asm
 //To Do
end;

{==============================================================================}

function AARCH64SaveIRQ:TIRQMask; assembler; nostackframe; 
{Disable Interrupts (IRQ) and return the previous state}
{Return: IRQ state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64RestoreIRQ(IRQMask:TIRQMask):TIRQMask; assembler; nostackframe;  
{Restore Interrupts (IRQ) to a previous state}
{IRQMask: IRQ state to restore (Passed in R0)}
{Return: IRQ state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64GetFIQ:Boolean; assembler; nostackframe; 
{Get Fast Interrupts (FIQ) state}
{Return: True is enabled, False if disabled (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64EnableFIQ; assembler; nostackframe; 
{Enable Fast Interrupts (FIQ) unconditionally}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64DisableFIQ; assembler; nostackframe; 
{Disable Fast Interrupts (FIQ) unconditionally}
asm
 //To Do
end;

{==============================================================================}

function AARCH64SaveFIQ:TFIQMask; assembler; nostackframe; 
{Disable Fast Interrupts (FIQ) and return the previous state}
{Return: FIQ state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64RestoreFIQ(FIQMask:TFIQMask):TFIQMask; assembler; nostackframe; 
{Restore Fast Interrupts (FIQ) to a previous state}
{FIQMask: FIQ state to restore (Passed in R0)}
{Return: FIQ state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64EnableIRQFIQ; assembler; nostackframe; 
{Enable Interrupts and Fast Interrupts (IRQ/FIQ) unconditionally}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64DisableIRQFIQ; assembler; nostackframe; 
{Disable Interrupts and Fast Interrupts (IRQ/FIQ) unconditionally}
asm
 //To Do
end;

{==============================================================================}

function AARCH64SaveIRQFIQ:TIRQFIQMask; assembler; nostackframe; 
{Disable Interrupts and Fast Interrupts (IRQ/FIQ) and return the previous state}
{Return: IRQ/FIQ state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64RestoreIRQFIQ(IRQFIQMask:TIRQFIQMask):TIRQFIQMask; assembler; nostackframe; 
{Restore Interrupts and Fast Interrupts (IRQ/FIQ) to a previous state}
{IRQFIQMask: IRQ/FIQ state to restore (Passed in R0)}
{Return: IRQ/FIQ state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64GetAbort:Boolean; assembler; nostackframe; 
{Get Abort state}
{Return: True is enabled, False if disabled (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64EnableAbort; assembler; nostackframe; 
{Enable Aborts unconditionally}
asm
 //To Do
end;

{==============================================================================}

procedure AARCH64DisableAbort; assembler; nostackframe; 
{Disable Aborts unconditionally}
asm
 //To Do
end;

{==============================================================================}

function AARCH64SaveAbort:TAbortMask; assembler; nostackframe; 
{Disable Aborts and return the previous state}
{Return: Abort state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}

function AARCH64RestoreAbort(AbortMask:TAbortMask):TAbortMask; assembler; nostackframe; 
{Restore Aborts to a previous state}
{AbortMask: Abort state to restore (Passed in R0)}
{Return: Abort state when called (Returned in R0)}
asm
 //To Do
end;

{==============================================================================}
{==============================================================================}
{AARCH64 Helper Functions}
procedure AARCH64Wait; inline;
begin
 {}
 if Assigned(AARCH64WaitHandler) then
  begin
   AARCH64WaitHandler;
  end;
end;

{==============================================================================}

procedure AARCH64LongWait; inline;
begin
 {}
 if Assigned(AARCH64LongWaitHandler) then
  begin
   AARCH64LongWaitHandler;
  end;
end;

{==============================================================================}

procedure AARCH64ShortWait; inline;
begin
 {}
 if Assigned(AARCH64ShortWaitHandler) then
  begin
   AARCH64ShortWaitHandler;
  end;
end;

{==============================================================================}

procedure AARCH64SlowBlink; inline;
begin
 {}
 if Assigned(AARCH64SlowBlinkHandler) then
  begin
   AARCH64SlowBlinkHandler;
  end;
end;

{==============================================================================}

procedure AARCH64FastBlink; inline;
begin
 {}
 if Assigned(AARCH64FastBlinkHandler) then
  begin
   AARCH64FastBlinkHandler;
  end;
end;

{==============================================================================}

function AARCH64ModeToString(AARCH64Mode:LongWord):String;
begin
 {}
 Result:='AARCH64_MODE_INVALID';
 
 //To Do
end;

{==============================================================================}
{==============================================================================}

end.
 