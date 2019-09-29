{
Ultibo Platform interface unit for ARM.

Copyright (C) 2019 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)
 ARMv7 (Cortex A5/A7/A8/A9/A15/A17)
 ARMv8 (Cortex A53/A57/A72)
 
Boards
======

 Raspberry Pi - Model A/B/A+/B+
 Raspberry Pi - Model Zero/ZeroW
 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B/B+/A+
 QEMU VersatilePB
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


Platform ARM
============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformARM; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,HeapManager,Threads,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {ARM specific constants}
 
 {Program Status Register (CPSR and SPSR) bit definitions}
 {Definitions of Mode bits (Bits 4..0) in the ARM program status register.  See: A2.2 Processor Modes of the ARM Architecture Reference Manual and also A2.5.7 The mode bits}
 {                                                                     See also: B1.3.1 ARM processor modes of the ARM Architecture Reference Manual (ARMv7-A and ARMv7-R edition)}
 ARM_MODE_USR = $10;    {Normal User Mode}                                       
 ARM_MODE_FIQ = $11;    {FIQ Processing Fast Interrupts Mode}
 ARM_MODE_IRQ = $12;    {IRQ Processing Standard Interrupts Mode}                
 ARM_MODE_SVC = $13;    {Supervisor Processing Software Interrupts Mode}
 ARM_MODE_MON = $16;    {Secure Monitor Mode (For Secure / Non Secure Switching)} 
 ARM_MODE_ABT = $17;    {Abort Processing memory Faults Mode}                    
 ARM_MODE_HYP = $1A;    {Hypervisor Mode}                                    
 ARM_MODE_UND = $1B;    {Undefined Processing Undefined Instructions Mode}       
 ARM_MODE_SYS = $1F;    {System Running Priviledged Operating System Tasks Mode} 
 
 ARM_MODE_BITS = $0000001F; {Mask of the mode bits in the program status register}
  
 {Definitions of Interrupt disable bits (Bits 7 and 6) in the ARM program status register. See: A2.5.6 "The interrupt disable bits" of the ARM Architecture Reference Manual}
 ARM_I_BIT = $00000080;    {IRQs disabled when set to 1}
 ARM_F_BIT = $00000040;    {FIQs disabled when set to 1}

 {Definitions of Thumb and Jazelle bits (Bits 24 and 5) in the ARM program status register. See: A2.5.8 "The T and J bits" of the ARM Architecture Reference Manual}
 ARM_T_BIT = $00000020;    {Thumb mode enabled when set to 1}
 ARM_J_BIT = $01000000;    {Jazelle mode enabled when set to 1}
 
 {Definition of Abort bit (Bit 8) in the ARM program status register}
 ARM_A_BIT = $00000100;    {Data Abort masked when set to 1}
 
{==============================================================================}
const
 {Definitions of ARM Boot Tags}  
 ATAG_NONE       = $00000000;
 ATAG_CORE       = $54410001;
 ATAG_MEM        = $54410002;
 ATAG_VIDEOTEXT  = $54410003;
 ATAG_RAMDISK    = $54410004;
 ATAG_INITRD2    = $54410005;
 ATAG_SERIAL     = $54410006;
 ATAG_REVISION   = $54410007;
 ATAG_VIDEOLFB   = $54410008;
 ATAG_CMDLINE    = $54410009;
 
 ARMTAGS_INITIAL = $FFFFFFFF;
 
{==============================================================================}
const
 {Definitions of Device Tree Blob}  
 DTB_SIGNATURE = $d00dfeed; {See: https://www.kernel.org/doc/Documentation/arm/Booting}
 
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
 {ARM specific types}
 
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
 
type 
 {Prototypes for Wait Handlers}
 TARMWait = procedure;
 TARMLongWait = procedure;
 TARMShortWait = procedure;
 
type 
 {Prototypes for Blink Handlers}
 TARMSlowBlink = procedure;
 TARMFastBlink = procedure;
 
{==============================================================================}
var
 {ARM specific variables}
 ARMInitialized:Boolean;
 
 ARMBootMode:LongWord = 0;                  {The ARM Mode that the processor was in at boot time (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 ARMBootVectors:LongWord = 0;               {The Vector Base Address that was current at boot time (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 ARMTagsAddress:PtrUInt = ARMTAGS_INITIAL;  {Pointer to the ARM TAGS provided by the bootloader at startup (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 ARMMachineType:LongWord = 0;               {ARM Machine Type provided by the bootloader at startup (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 
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
 
 {Tag Init RD2 Variables}
 TagInitRd2Count:LongWord;  {Number of ARM INITRD2 Tags found during parse}
 
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
 ARMWaitHandler:TARMWait;
 ARMLongWaitHandler:TARMLongWait;
 ARMShortWaitHandler:TARMShortWait;
 
var
 {Blink Handlers}
 ARMSlowBlinkHandler:TARMSlowBlink;
 ARMFastBlinkHandler:TARMFastBlink;
 
{==============================================================================}
{Initialization Functions}
procedure ARMInit;
 
{==============================================================================}
{ARM Platform Functions}
procedure ARMParseBootTags;
procedure ARMParseCommandLine;
procedure ARMParseEnvironment;

function ARMGetSP:PtrUInt;
function ARMGetPC:PtrUInt;

function ARMGetIRQ:Boolean;
procedure ARMEnableIRQ; 
procedure ARMDisableIRQ; 
function ARMSaveIRQ:TIRQMask;
function ARMRestoreIRQ(IRQMask:TIRQMask):TIRQMask; 

function ARMGetFIQ:Boolean;
procedure ARMEnableFIQ; 
procedure ARMDisableFIQ; 
function ARMSaveFIQ:TFIQMask;
function ARMRestoreFIQ(FIQMask:TFIQMask):TFIQMask; 

procedure ARMEnableIRQFIQ;
procedure ARMDisableIRQFIQ;
function ARMSaveIRQFIQ:TIRQFIQMask;
function ARMRestoreIRQFIQ(IRQFIQMask:TIRQFIQMask):TIRQFIQMask;

function ARMGetAbort:Boolean;
procedure ARMEnableAbort;
procedure ARMDisableAbort;
function ARMSaveAbort:TAbortMask;
function ARMRestoreAbort(AbortMask:TAbortMask):TAbortMask;
 
{==============================================================================}
{ARM Helper Functions}
procedure ARMWait; inline;
procedure ARMLongWait; inline;
procedure ARMShortWait; inline;

procedure ARMSlowBlink; inline;
procedure ARMFastBlink; inline;

function ARMModeToString(ARMMode:LongWord):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ARMInit;
begin
 {}
 if ARMInitialized then Exit;
 
 {Setup STACK_MIN_ALIGNMENT} 
 STACK_MIN_ALIGNMENT:=SIZE_8;
 
 {Register Platform ParseBootTags Handler}
 ParseBootTagsHandler:=ARMParseBootTags;

 {Register Platform ParseCommandLine Handler}
 ParseCommandLineHandler:=ARMParseCommandLine;

 {Register Platform ParseEnvironment Handler}
 ParseEnvironmentHandler:=ARMParseEnvironment;
 
 {Register Platform GetSP/PC Handlers}
 GetSPHandler:=ARMGetSP;
 GetPCHandler:=ARMGetPC;
 
 {Register Platform Enable/Disable/Save/RestoreIRQ Handlers}
 GetIRQHandler:=ARMGetIRQ;
 EnableIRQHandler:=ARMEnableIRQ;
 DisableIRQHandler:=ARMDisableIRQ;
 SaveIRQHandler:=ARMSaveIRQ;
 RestoreIRQHandler:=ARMRestoreIRQ;

 {Register Platform Enable/Disable/Save/RestoreFIQ Handlers}
 GetFIQHandler:=ARMGetFIQ;
 EnableFIQHandler:=ARMEnableFIQ;
 DisableFIQHandler:=ARMDisableFIQ;
 SaveFIQHandler:=ARMSaveFIQ;
 RestoreFIQHandler:=ARMRestoreFIQ;
 
 {Register Platform Enable/Disable/Save/RestoreIRQFIQ Handlers}
 EnableIRQFIQHandler:=ARMEnableIRQFIQ;
 DisableIRQFIQHandler:=ARMDisableIRQFIQ;
 SaveIRQFIQHandler:=ARMSaveIRQFIQ;
 RestoreIRQFIQHandler:=ARMRestoreIRQFIQ;
 
 {Register Platform Enable/Disable/Save/RestoreAbort Handlers}
 GetAbortHandler:=ARMGetAbort;
 EnableAbortHandler:=ARMEnableAbort;
 DisableAbortHandler:=ARMDisableAbort;
 SaveAbortHandler:=ARMSaveAbort;
 RestoreAbortHandler:=ARMRestoreAbort;
 
 ARMInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ARM Platform Functions}
procedure ARMParseBootTags;
{Extract some information from the ARM boot tag list and use it to load the
 memory manager, some other information is stored in variables for future use}
var
 ARMTag:PARMTag;

 CommandOffset:PChar;
 CommandLength:LongWord;
 
 BlockSize:LongWord;
 BlockAddress:PtrUInt;
 StartAddress:PtrUInt;
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
   TagInitRd2Count:=0;
   TagSerialCount:=0;
   TagRevisionCount:=0;
   TagVideoFBCount:=0;
   TagCmdCount:=0;
   
   {Check for default Tag Address value and for Device Tree Blob signature}
   if (ARMTagsAddress = ARMTAGS_INITIAL) or (LongWordBEtoN(PLongWord(ARMTagsAddress)^) = DTB_SIGNATURE) then
    begin
     {Device Tree Blob supplied or ARM tags not present}
     {Check Memory Size}
     if CPU_MEMORY_SIZE > 0 then
      begin
       {Registry Memory}
       Inc(ARMTagsCount);
       Inc(TagMemoryCount);
        
       BlockAddress:=CPU_MEMORY_BASE;
       BlockSize:=CPU_MEMORY_SIZE;
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
         TagMemoryAddress:=StartAddress;
         TagMemoryLength:=BlockSize;
         if BlockSize > 0 then
          begin
           RegisterHeapBlock(Pointer(StartAddress),BlockSize);
          end;           
        end;
      end;
    end
   else
    begin
     {ARM Tags address supplied}
     {Get First Tag}
     ARMTag:=PARMTag(ARMTagsAddress);
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
            BlockAddress:=ARMTag.Memory.Start;
            BlockSize:=ARMTag.Memory.Size;
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
              TagMemoryAddress:=StartAddress;
              TagMemoryLength:=BlockSize;
              if BlockSize > 0 then
               begin
                RegisterHeapBlock(Pointer(StartAddress),BlockSize);
               end;           
             end;
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
        ATAG_INITRD2:begin
          {INITRD2}
          Inc(TagInitRd2Count);
          {Not relevant to Ultibo}
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
            TagCommandAddress:=@ARMTag.Command.Cmdline[0];
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

procedure ARMParseCommandLine;
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

procedure ARMParseEnvironment;
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

function ARMGetSP:PtrUInt; assembler; nostackframe; 
{Get the current stack pointer (SP)}
asm
 //Copy stack pointer to R0
 mov r0, sp
end;

{==============================================================================}

function ARMGetPC:PtrUInt; assembler; nostackframe; 
{Get the current program counter (PC)}
asm
 //Copy link register (Return program counter) to R0
 mov r0, lr
end;

{==============================================================================}

function ARMGetIRQ:Boolean; assembler; nostackframe; 
{Get Interrupts (IRQ) state}
{Return: True is enabled, False if disabled (Returned in R0)}
asm
 //Get Current program status register
 mrs r1, cpsr
 //Mask off everything except IRQ bit
 and r1, r1, #ARM_I_BIT
 //Default return to True
 mov r0, #1
 //Check if IRQ bit is currently set
 cmp r1, #ARM_I_BIT
 //If not set then return True
 bne .LCompleted
 //Otherwise return False
 mov r0, #0

.LCompleted:
end;

{==============================================================================}

procedure ARMEnableIRQ; assembler; nostackframe; 
{Enable Interrupts (IRQ) unconditionally}
asm
 //Change program status interrupt enable
 cpsie i 
end;

{==============================================================================}

procedure ARMDisableIRQ; assembler; nostackframe;  
{Disable Interrupts (IRQ) unconditionally}
asm
 //Change program status interrupt disable
 cpsid i 
end;

{==============================================================================}

function ARMSaveIRQ:TIRQMask; assembler; nostackframe; 
{Disable Interrupts (IRQ) and return the previous state}
{Return: IRQ state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except IRQ bit
 and r0, r0, #ARM_I_BIT
 //Change program status interrupt disable
 cpsid i
end;

{==============================================================================}

function ARMRestoreIRQ(IRQMask:TIRQMask):TIRQMask; assembler; nostackframe;  
{Restore Interrupts (IRQ) to a previous state}
{IRQMask: IRQ state to restore (Passed in R0)}
{Return: IRQ state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r1, cpsr
 //Mask off everything except IRQ bit
 and r1, r1, #ARM_I_BIT
 //Check if the supplied IRQ bit is set
 cmp r0, #ARM_I_BIT
 //If not set then enable IRQ
 bne .LEnable
 //Otherwise disable IRQ
 //Change program status interrupt disable
 cpsid i
 b   .LCompleted

.LEnable:
 //Change program status interrupt enable
 cpsie i 

.LCompleted:
 //Return the previous state
 mov r0, r1
end;

{==============================================================================}

function ARMGetFIQ:Boolean; assembler; nostackframe; 
{Get Fast Interrupts (FIQ) state}
{Return: True is enabled, False if disabled (Returned in R0)}
asm
 //Get Current program status register
 mrs r1, cpsr
 //Mask off everything except FIQ bit
 and r1, r1, #ARM_F_BIT
 //Default return to True
 mov r0, #1
 //Check if FIQ bit is currently set
 cmp r1, #ARM_F_BIT
 //If not set then return True
 bne .LCompleted
 //Otherwise return False
 mov r0, #0

.LCompleted:
end;

{==============================================================================}

procedure ARMEnableFIQ; assembler; nostackframe; 
{Enable Fast Interrupts (FIQ) unconditionally}
asm
 //Change program status interrupt enable
 cpsie f 
end;

{==============================================================================}

procedure ARMDisableFIQ; assembler; nostackframe; 
{Disable Fast Interrupts (FIQ) unconditionally}
asm
 //Change program status interrupt disable
 cpsid f 
end;

{==============================================================================}

function ARMSaveFIQ:TFIQMask; assembler; nostackframe; 
{Disable Fast Interrupts (FIQ) and return the previous state}
{Return: FIQ state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except FIQ bit
 and r0, r0, #ARM_F_BIT
 //Change program status interrupt disable
 cpsid f
end;

{==============================================================================}

function ARMRestoreFIQ(FIQMask:TFIQMask):TFIQMask; assembler; nostackframe; 
{Restore Fast Interrupts (FIQ) to a previous state}
{FIQMask: FIQ state to restore (Passed in R0)}
{Return: FIQ state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r1, cpsr
 //Mask off everything except FIQ bit
 and r1, r1, #ARM_F_BIT
 //Check if the supplied FIQ bit is set
 cmp r0, #ARM_F_BIT
 //If not set then enable FIQ
 bne .LEnable
 //Otherwise disable FIQ
 //Change program status interrupt disable
 cpsid f
 b   .LCompleted

.LEnable:
 //Change program status interrupt enable
 cpsie f 

.LCompleted:
 //Return the previous state
 mov r0, r1
end;

{==============================================================================}

procedure ARMEnableIRQFIQ; assembler; nostackframe; 
{Enable Interrupts and Fast Interrupts (IRQ/FIQ) unconditionally}
asm
 //Change program status interrupt enable
 cpsie if 
end;

{==============================================================================}

procedure ARMDisableIRQFIQ; assembler; nostackframe; 
{Disable Interrupts and Fast Interrupts (IRQ/FIQ) unconditionally}
asm
 //Change program status interrupt disable
 cpsid if 
end;

{==============================================================================}

function ARMSaveIRQFIQ:TIRQFIQMask; assembler; nostackframe; 
{Disable Interrupts and Fast Interrupts (IRQ/FIQ) and return the previous state}
{Return: IRQ/FIQ state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except IRQ/FIQ bits
 and r0, r0, #ARM_I_BIT | ARM_F_BIT
 //Change program status interrupt disable
 cpsid if
end;

{==============================================================================}

function ARMRestoreIRQFIQ(IRQFIQMask:TIRQFIQMask):TIRQFIQMask; assembler; nostackframe; 
{Restore Interrupts and Fast Interrupts (IRQ/FIQ) to a previous state}
{IRQFIQMask: IRQ/FIQ state to restore (Passed in R0)}
{Return: IRQ/FIQ state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r1, cpsr
 //Mask off everything except IRQ/FIQ bits
 and r1, r1, #ARM_I_BIT | ARM_F_BIT
 
.LCheckIRQ:
 //Extract the supplied IRQ bit
 and r2, r0, #ARM_I_BIT
 //Check if the supplied IRQ bit is set
 cmp r2, #ARM_I_BIT
 //If not set then enable IRQ
 bne .LEnableIRQ
 //Otherwise disable IRQ
 //Change program status interrupt disable
 cpsid i
 b   .LCheckFIQ

.LEnableIRQ:
 //Change program status interrupt enable
 cpsie i 
 
.LCheckFIQ:
 //Extract the supplied FIQ bit
 and r2, r0, #ARM_F_BIT
 //Check if the supplied FIQ bit is set
 cmp r2, #ARM_F_BIT
 //If not set then enable FIQ
 bne .LEnableFIQ
 //Otherwise disable FIQ
 //Change program status interrupt disable
 cpsid f
 b   .LCompleted
 
.LEnableFIQ:
 //Change program status interrupt enable
 cpsie f 
 
.LCompleted:
 //Return the previous state
 mov r0, r1
end;

{==============================================================================}

function ARMGetAbort:Boolean; assembler; nostackframe; 
{Get Abort state}
{Return: True is enabled, False if disabled (Returned in R0)}
asm
 //Get Current program status register
 mrs r1, cpsr
 //Mask off everything except Abort bit
 and r1, r1, #ARM_A_BIT
 //Default return to True
 mov r0, #1
 //Check if Abort bit is currently set
 cmp r1, #ARM_A_BIT
 //If not set then return True
 bne .LCompleted
 //Otherwise return False
 mov r0, #0

.LCompleted:
end;

{==============================================================================}

procedure ARMEnableAbort; assembler; nostackframe; 
{Enable Aborts unconditionally}
asm
 //Change program status interrupt enable
 cpsie a 
end;

{==============================================================================}

procedure ARMDisableAbort; assembler; nostackframe; 
{Disable Aborts unconditionally}
asm
 //Change program status interrupt disable
 cpsid a 
end;

{==============================================================================}

function ARMSaveAbort:TAbortMask; assembler; nostackframe; 
{Disable Aborts and return the previous state}
{Return: Abort state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except Abort bit
 and r0, r0, #ARM_A_BIT
 //Change program status interrupt disable
 cpsid a
end;

{==============================================================================}

function ARMRestoreAbort(AbortMask:TAbortMask):TAbortMask; assembler; nostackframe; 
{Restore Aborts to a previous state}
{AbortMask: Abort state to restore (Passed in R0)}
{Return: Abort state when called (Returned in R0)}
asm
 //Get Current program status register
 mrs r1, cpsr
 //Mask off everything except Abort bit
 and r1, r1, #ARM_A_BIT
 //Check if the supplied Abort bit is set
 cmp r0, #ARM_A_BIT
 //If not set then enable Abort
 bne .LEnable
 //Otherwise disable Abort
 //Change program status interrupt disable
 cpsid a
 b   .LCompleted

.LEnable:
 //Change program status interrupt enable
 cpsie a 

.LCompleted:
 //Return the previous state
 mov r0, r1
end;

{==============================================================================}
{==============================================================================}
{ARM Helper Functions}
procedure ARMWait; inline;
begin
 {}
 if Assigned(ARMWaitHandler) then
  begin
   ARMWaitHandler;
  end;
end;

{==============================================================================}

procedure ARMLongWait; inline;
begin
 {}
 if Assigned(ARMLongWaitHandler) then
  begin
   ARMLongWaitHandler;
  end;
end;

{==============================================================================}

procedure ARMShortWait; inline;
begin
 {}
 if Assigned(ARMShortWaitHandler) then
  begin
   ARMShortWaitHandler;
  end;
end;

{==============================================================================}

procedure ARMSlowBlink; inline;
begin
 {}
 if Assigned(ARMSlowBlinkHandler) then
  begin
   ARMSlowBlinkHandler;
  end;
end;

{==============================================================================}

procedure ARMFastBlink; inline;
begin
 {}
 if Assigned(ARMFastBlinkHandler) then
  begin
   ARMFastBlinkHandler;
  end;
end;

{==============================================================================}

function ARMModeToString(ARMMode:LongWord):String;
begin
 {}
 Result:='ARM_MODE_INVALID';
 
 case ARMMode of
  ARM_MODE_USR:Result:='ARM_MODE_USR';
  ARM_MODE_FIQ:Result:='ARM_MODE_FIQ';
  ARM_MODE_IRQ:Result:='ARM_MODE_IRQ';
  ARM_MODE_SVC:Result:='ARM_MODE_SVC';
  ARM_MODE_MON:Result:='ARM_MODE_MON';
  ARM_MODE_ABT:Result:='ARM_MODE_ABT';
  ARM_MODE_HYP:Result:='ARM_MODE_HYP';
  ARM_MODE_UND:Result:='ARM_MODE_UND';
  ARM_MODE_SYS:Result:='ARM_MODE_SYS';
 end;
end;

{==============================================================================}
{==============================================================================}

end.
