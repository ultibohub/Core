{
Ultibo Platform interface unit for QEMU VersatilePB.

Copyright (C) 2018 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A8)
 ARMv8 (Cortex A53)
 
Boards
======

 QEMU - VersatilePB 
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  Linux - \arch\arm\mach-versatile\*
 
  Linux - \drivers\clocksource\timer-sp804.c - Copyright (C) 1999 - 2003 ARM Limited
  
  Linux - \drivers\irqchip\irq-vic.c - Copyright (C) 1999 - 2003 ARM Limited
 
References
==========
 
 QEMU System ARM - http://wiki.qemu.org/download/qemu-doc.html#ARM-System-emulator
 
 RealView Versatile PB - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0224i/index.html
 
 SP804 Dual Timer - http://infocenter.arm.com/help/topic/com.arm.doc.ddi0271d/DDI0271.pdf
 
 PL190 Vectored Interrupt Controller - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0181e/index.html
 
Platform QEMUVPB
================
 
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformQEMUVPB; 

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,VersatilePB,Platform,{$IFDEF CPUARM}PlatformARM,PlatformARMv7,{$ENDIF CPUARM}{$IFDEF CPUAARCH64}PlatformAARCH64,PlatformARMv8,{$ENDIF CPUAARCH64}HeapManager,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF},SysUtils;

{==============================================================================}
const
 {QEMUVPB specific constants}
 
 {Address of StartupHandler on Reset}
 QEMUVPB_STARTUP_ADDRESS = $00010000;

const
 {Page Table Address and Size}
 QEMUVPB_PAGE_TABLE_BASE = $00004000;     {Place the first level Page Table after the interrupt vectors at 0x00001000 and before the code start at 0x00010000}
 QEMUVPB_PAGE_TABLE_SIZE = SIZE_16K;      {ARMv7 first level Page Table is exactly 16KB in size (4096 32 bit (4 byte) entries)}
 
const
 {Vector Table Address and Size} 
 QEMUVPB_VECTOR_TABLE_BASE  = $00001000;  {Place the Interrupt Vector Table at 0x00001000 before the code start at 0x00010000} 
 QEMUVPB_VECTOR_TABLE_SIZE  = SIZE_64;    {The Interrupt Vector Table is exactly 64 bytes (16 32 bit (4 byte) entries)}
 QEMUVPB_VECTOR_TABLE_COUNT = 8;          {The Interrupt Vector Table contains 8 entries on an ARMv7 device}
 
const
 {CPU Count, Boot and Mask} 
 QEMUVPB_CPU_COUNT = VERSATILEPB_CPU_COUNT;
 QEMUVPB_CPU_BOOT = CPU_ID_0;
 QEMUVPB_CPU_MASK = CPU_AFFINITY_0;
 
const
 {SWI}
 QEMUVPB_SWI_COUNT = 256;                 {Number of available SWI entries}
 
const
 {Kernel Image Name}
 {$IFDEF CPUARM}
 QEMUVPB_KERNEL_NAME = 'kernel.bin';
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 QEMUVPB_KERNEL_NAME = 'kernel64.bin';
 {$ENDIF CPUAARCH64}
 QEMUVPB_KERNEL_CONFIG = '';  {Not available as a file}
 QEMUVPB_KERNEL_COMMAND = ''; {Not available as a file}
 QEMUVPB_FIRMWARE_FILES = ''; {Not available as a file}
 
{$IFDEF CONSOLE_EARLY_INIT}
const
 {PL110 specific constants}
 PL110_FRAMEBUFFER_DESCRIPTION = 'ARM PrimeCell PL110 Color LCD';  {Description of PL110 device}

 {PL110 mode constants}
 PL110_MODE_UNKNOWN = 0;
 PL110_MODE_VGA     = 1; {Connected to a VGA display}
 PL110_MODE_SVGA    = 2; {Connected to a SVGA display}
 PL110_MODE_TFT     = 3; {Connected to a TFT display}
 PL110_MODE_STN     = 4; {Connected to an STN display}

 {PL110 register offsets (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING0 = $00000000; {Horizontal Axis Panel Control Register}
 PL110_CLCD_TIMING1 = $00000004; {Vertical Axis Panel Control Register}
 PL110_CLCD_TIMING2 = $00000008; {Clock and Signal Polarity Control Register}
 PL110_CLCD_TIMING3 = $0000000c; {Line End Control Register}
 PL110_CLCD_UPBASE  = $00000010; {Upper Panel Frame Base Address Registers}
 PL110_CLCD_LPBASE  = $00000014; {Lower Panel Frame Base Address Registers}
 PL110_CLCD_CONTROL = $00000018; {Control Register}                           {Note: Reversed in VersatilePB implementation, 0x0000001c in PL110 TRM}
 PL110_CLCD_IMSC    = $0000001c; {Interrupt Mask Set/Clear Register}          {Note: Reversed in VersatilePB implementation, 0x00000018 in PL110 TRM}
 PL110_CLCD_RIS     = $00000020; {Raw Interrupt Status Register}
 PL110_CLCD_MIS     = $00000024; {Masked Interrupt Status Register}
 PL110_CLCD_ICR     = $00000028; {Interrupt Clear Register}
 PL110_CLCD_UPCURR  = $0000002C; {Upper Panel Current Address Value Registers}
 PL110_CLCD_LPCURR  = $00000030; {Lower Panel Current Address Value Registers}
 PL110_CLCD_PALETTE = $00000200; {Color Palette Register}

 {PL110 Timing0 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING0_HBP = ($FF shl 24); {Horizontal back porch}
 PL110_CLCD_TIMING0_HFP = ($FF shl 16); {Horizontal front porch}
 PL110_CLCD_TIMING0_HSW = ($FF shl 8);  {Horizontal synchronization pulse width} 
 PL110_CLCD_TIMING0_PPL = ($FC shl 2);  {Pixels-per-line (Actual pixels-per-line = 16 * (PPL + 1))}

 {PL110 Timing1 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING1_VBP = ($FF shl 24); {Vertical back porch}
 PL110_CLCD_TIMING1_VFP = ($FF shl 16); {Vertical front porch} 
 PL110_CLCD_TIMING1_VSW = ($FC shl 10); {Vertical synchronization pulse width}
 PL110_CLCD_TIMING1_LPP = ($3FF shl 0); {Lines per panel is the number of active lines per screen (Program to number of lines required minus 1)} 
 
 {PL110 Timing2 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING2_PCD_HI = ($1F shl 27);  {Upper five bits of Panel Clock Divisor}
 PL110_CLCD_TIMING2_BCD    = (1 shl 26);    {Bypass pixel clock divider}
 PL110_CLCD_TIMING2_CPL    = ($3FF shl 16); {Clocks per line}
 PL110_CLCD_TIMING2_IOE    = (1 shl 14);    {Invert output enable}
 PL110_CLCD_TIMING2_IPC    = (1 shl 13);    {Invert panel clock}
 PL110_CLCD_TIMING2_IHS    = (1 shl 12);    {Invert horizontal synchron}
 PL110_CLCD_TIMING2_IVS    = (1 shl 11);    {Invert vertical synchronization}
 PL110_CLCD_TIMING2_ACB    = ($1F shl 6);   {AC bias pin frequency}
 PL110_CLCD_TIMING2_CLKSEL = (1 shl 5);     {This bit drives the CLCDCLKSEL signal which is used as the select signal for the external LCD clock multiplexor}
 PL110_CLCD_TIMING2_PCD_LO = ($1F shl 0);   {Lower five bits of Panel Clock Divisor}
 
 {PL110 Timing3 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING3_LEE = (1 shl 16);  {LCD Line end enable: 0 = CLLE disabled (held LOW) / 1 = CLLE signal active}
 PL110_CLCD_TIMING3_LED = ($3F shl 0); {Line-end signal delay from the rising-edge of the last panel clock}
 
 {PL110 Control register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_CONTROL_LCDEN           = (1 shl 0);  {}
 PL110_CLCD_CONTROL_LCDBPP1         = (0 shl 1);  {LCD bits per pixel: 000 = 1 bpp}
 PL110_CLCD_CONTROL_LCDBPP2         = (1 shl 1);  {                    001 = 2 bpp}
 PL110_CLCD_CONTROL_LCDBPP4         = (2 shl 1);  {                    010 = 4 bpp}
 PL110_CLCD_CONTROL_LCDBPP8         = (3 shl 1);  {                    011 = 8 bpp}
 PL110_CLCD_CONTROL_LCDBPP16        = (4 shl 1);  {                    100 = 16 bpp}
 PL110_CLCD_CONTROL_LCDBPP16_565    = (6 shl 1);  {                    110 = 16 bpp 565 (PL111 only)}
 PL110_CLCD_CONTROL_LCDBPP16_444    = (7 shl 1);  {                    111 = 16 bpp 444 (PL111 only)}
 PL110_CLCD_CONTROL_LCDBPP24        = (5 shl 1);  {                    101 = 24 bpp}
 PL110_CLCD_CONTROL_LCDBW           = (1 shl 4);  {STN LCD is monochrome (black and white) (0 = STN LCD is color / 1 = STN LCD is monochrome)}
 PL110_CLCD_CONTROL_LCDTFT          = (1 shl 5);  {LCD is TFT (0 = LCD is an STN display, use gray scaler / 1 = LCD is TFT, do not use gray scaler)}
 PL110_CLCD_CONTROL_LCDMONO8        = (1 shl 6);  {Monochrome LCD has an 8-bit interface (0 = mono LCD uses 4-bit interface / 1 = mono LCD uses 8-bit interface)}
 PL110_CLCD_CONTROL_LCDDUAL         = (1 shl 7);  {LCD interface is dual panel STN (0 = single panel LCD is in use / 1 = dual panel LCD is in use)}
 PL110_CLCD_CONTROL_BGR             = (1 shl 8);  {RGB of BGR format selection (0 = RGB normal output / 1 = BGR red and blue swapped.)}
 PL110_CLCD_CONTROL_BEBO            = (1 shl 9);  {Big-endian byte order (0 = little-endian byte order / 1 = big-endian byte order)}
 PL110_CLCD_CONTROL_BEPO            = (1 shl 10); {Big-endian pixel ordering within a byte (0 = little-endian pixel ordering within a byte / 1= big-endian pixel ordering within a byte)}
 PL110_CLCD_CONTROL_LCDPWR          = (1 shl 11); {LCD power enable}
 PL110_CLCD_CONTROL_LCDVCOMP_VSYNC  = (0 shl 12); {Generate interrupt at: 00 = start of vertical synchronization}
 PL110_CLCD_CONTROL_LCDVCOMP_BPORCH = (1 shl 12); {                       01 = start of back porch}
 PL110_CLCD_CONTROL_LCDVCOMP_VIDEO  = (2 shl 12); {                       10 = start of active video}
 PL110_CLCD_CONTROL_LCDVCOMP_FPORCH = (3 shl 12); {                       11 = start of front porch}
 PL110_CLCD_CONTROL_LDMAFIFOTIME    = (1 shl 15); {Unknown}
 PL110_CLCD_CONTROL_WATERMARK       = (1 shl 16); {LCD DMA FIFO Watermark level}

 {PL110 control constants}
 PL110_CONTROL_VGA = PL110_CLCD_CONTROL_LCDTFT or PL110_CLCD_CONTROL_LCDVCOMP_BPORCH;
 PL110_CONTROL_SVGA = PL110_CLCD_CONTROL_LCDTFT or PL110_CLCD_CONTROL_LCDVCOMP_BPORCH;
 
 {PL110 timing0 constants}
 PL110_TIMING0_VGA = $3F1F3F9C;
 PL110_TIMING0_SVGA = $1313A4C4;

 {PL110 timing1 constants}
 PL110_TIMING1_VGA = $090B61DF;
 PL110_TIMING1_SVGA = $0505F657;
  
 {PL110 timing2 constants}
 PL110_TIMING2_VGA = $067F1800;
 PL110_TIMING2_SVGA = $071F1800;
{$ENDIF}
{==============================================================================}
{type}
 {QEMUVPB specific types}
 
{$IFDEF CONSOLE_EARLY_INIT}
type
 {PL110 specific types}
 {Layout of the PL110 registers (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PPL110CLCDRegisters = ^TPL110CLCDRegisters;
 TPL110CLCDRegisters = record
  TIMING0:LongWord; {Horizontal Axis Panel Control Register}
  TIMING1:LongWord; {Vertical Axis Panel Control Register}
  TIMING2:LongWord; {Clock and Signal Polarity Control Register}
  TIMING3:LongWord; {Line End Control Register}
  UPBASE:LongWord;  {Upper Panel Frame Base Address Registers}
  LPBASE:LongWord;  {Lower Panel Frame Base Address Registers}
  CONTROL:LongWord; {Control Register}                           {Note: Reversed in VersatilePB implementation, 0x0000001c in PL110 TRM}
  IMSC:LongWord;    {Interrupt Mask Set/Clear Register}          {Note: Reversed in VersatilePB implementation, 0x00000018 in PL110 TRM}
  RIS:LongWord;     {Raw Interrupt Status Register}
  MIS:LongWord;     {Masked Interrupt Status Register}
  ICR:LongWord;     {Interrupt Clear Register}
  UPCURR:LongWord;  {Upper Panel Current Address Value Registers}
  LPCURR:LongWord;  {Lower Panel Current Address Value Registers}
 end; 
 
 PPL110Framebuffer = ^TPL110Framebuffer;
 TPL110Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {PL110 Properties}
  Mode:LongWord;                  {PL110 framebuffer mode (eg PL110_MODE_TFT)}
  Depth:LongWord;                 {Framebuffer color depth (eg FRAMEBUFFER_DEPTH_16)}
  Width:LongWord;                 {Framebuffer width in pixels}
  Height:LongWord;                {Framebuffer height in pixels}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  {Driver Properties}
  Control:LongWord;               {Preset Control register value}
  Timing0:LongWord;               {Preset Timing0 register value}
  Timing1:LongWord;               {Preset Timing1 register value}
  Timing2:LongWord;               {Preset Timing2 register value}
  Timing3:LongWord;               {Preset Timing2 register value}
  Registers:PPL110CLCDRegisters;  {PL110 registers}
 end; 
{$ENDIF} 
{==============================================================================}
var
 {QEMUVPB specific Ultibo variables}
 QEMUVPBInitialized:Boolean;
 
var
 {Clock Variables}
 ClockGetLast:LongWord;                        {Value of 24MHz Counter on last ClockGetCount or ClockGetTotal call}
 ClockGetBase:Int64;                           {Base value for 64-bit clock, incremented each time the 24MHz Counter rolls over (Only accurate if ClockGetCount/ClockGetTotal is called at least once per 178 seconds)}
 ClockGetLock:THandle = INVALID_HANDLE_VALUE;  {Lock handle for creating 64-bit clock from a 32-bit register}
 ClockGetTimer:THandle = INVALID_HANDLE_VALUE; {Timer handle for ensuring clock is read periodically to maintain accurracy}
  
var
 {Timer Variables}
 Timer0Registers:PSP804TimerRegisters; {Use Timer0 for Clock}
 Timer2Registers:PSP804TimerRegisters; {Use Timer2 for Scheduler}
 
var
 {Interrupt Variables}
 PrimaryInterruptRegisters:PPL190InterruptRegisters;
 SecondaryInterruptRegisters:PVersatilePBInterruptRegisters;
 
 InterruptEntries:array[0..(VERSATILEPB_IRQ_COUNT - 1)] of TInterruptEntry;

var
 {System Call Variables}
 SystemCallEntries:array[0..QEMUVPB_SWI_COUNT - 1] of TSystemCallEntry;
 
var
 {IRQ/FIQ Variables}
 IRQEnabled:array[0..1] of LongWord; {2 groups of IRQs to Enable/Disable (See: TPL190InterruptRegisters)}
 FIQEnabled:LongWord;                {The single IRQ number to Enable as FIQ instead (See: TPL190InterruptRegisters)}
 
{==============================================================================}
{Initialization Functions}
procedure QEMUVPBInit;

{==============================================================================}
{QEMUVPB Platform Functions}
procedure QEMUVPBBoardInit;
procedure QEMUVPBMemoryInit;
procedure QEMUVPBClockInit;
procedure QEMUVPBPowerInit;
procedure QEMUVPBInterruptInit;
procedure QEMUVPBPeripheralInit;
{$IFDEF CONSOLE_EARLY_INIT}
procedure QEMUVPBFramebufferInit;
{$ENDIF}
procedure QEMUVPBPageTableInit;

function QEMUVPBRequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function QEMUVPBReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function QEMUVPBRequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
function QEMUVPBReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 

function QEMUVPBRegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
function QEMUVPBDeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;

function QEMUVPBGetInterruptEntry(Number:LongWord):TInterruptEntry; 
function QEMUVPBGetSystemCallEntry(Number:LongWord):TSystemCallEntry; 

function QEMUVPBSystemRestart(Delay:LongWord):LongWord; 
function QEMUVPBSystemShutdown(Delay:LongWord):LongWord;

function QEMUVPBClockGetCount:LongWord;
function QEMUVPBClockGetTotal:Int64; 
procedure QEMUVPBClockGetTimer(Data:Pointer);

{==============================================================================}
{QEMUVPB Thread Functions}
procedure QEMUVPBSchedulerInit;

{==============================================================================}
{QEMUVPB IRQ Functions}
function QEMUVPBDispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

function QEMUVPBHandleIRQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

{==============================================================================}
{QEMUVPB FIQ Functions}
function QEMUVPBDispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

function QEMUVPBHandleFIQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

{==============================================================================}
{QEMUVPB SWI Functions}
function QEMUVPBDispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 

{==============================================================================}
{QEMUVPB Clock Functions}
procedure QEMUVPBClockInterrupt(Parameter:Pointer);
procedure QEMUVPBClockUpdate(Cycles:LongWord;var Last:LongWord);

{==============================================================================}
{QEMUVPB Scheduler Functions}
function QEMUVPBSchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
procedure QEMUVPBSchedulerUpdate(Cycles:LongWord;var Last:LongWord);

procedure QEMUVPBSchedulerSystemCall(Request:PSystemCallRequest);

{==============================================================================}
{QEMUVPB Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function QEMUVPBFramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function QEMUVPBFramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;

function QEMUVPBFramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function QEMUVPBFramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;

function QEMUVPBFramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{$ENDIF}
{==============================================================================}
{QEMUVPB Helper Functions}
procedure QEMUVPBBootBlink;
procedure QEMUVPBBootOutput(Value:LongWord);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure QEMUVPBInit;
var
 Value:LongWord;
begin
 {}
 if QEMUVPBInitialized then Exit;

 {Setup IO_BASE/IO_ALIAS}
 IO_BASE:=VERSATILEPB_PERIPHERALS_BASE;
 IO_ALIAS:=$00000000;

 {Setup BUS_ALIAS}
 BUS_ALIAS:=$00000000;
 
 {Setup SECURE_BOOT}
 SECURE_BOOT:=False; {Versatile PB does not support PL3 (TrustZone)}
 
 {Setup STARTUP_ADDRESS}
 STARTUP_ADDRESS:=PtrUInt(@_text_start); {QEMUVPB_STARTUP_ADDRESS} {Obtain from linker}
 
 {Setup PERIPHERALS_BASE and SIZE}
 PERIPHERALS_BASE:=VERSATILEPB_PERIPHERALS_BASE;
 PERIPHERALS_SIZE:=VERSATILEPB_PERIPHERALS_SIZE;

 {Setup LOCAL_PERIPHERALS_BASE and SIZE}
 LOCAL_PERIPHERALS_BASE:=$00000000;
 LOCAL_PERIPHERALS_SIZE:=0;
 
 {Setup MEMORY_BASE and SIZE}
 MEMORY_BASE:=$00000000;
 MEMORY_SIZE:=SIZE_256M;
 
 {Setup MEMORY_PAGE_SIZE}
 MEMORY_PAGE_SIZE:=SIZE_4K;
 MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
 
 {Setup MEMORY_IRQ/FIQ/LOCAL/SHARED/DEVICE/NOCACHE/NONSHARED_SIZE}
 MEMORY_IRQ_SIZE:=SIZE_2M;
 MEMORY_FIQ_SIZE:=SIZE_2M;
 MEMORY_LOCAL_SIZE:=SIZE_0;
 MEMORY_SHARED_SIZE:=SIZE_16M;
 MEMORY_DEVICE_SIZE:=SIZE_0;
 MEMORY_NOCACHE_SIZE:=SIZE_16M;
 MEMORY_NONSHARED_SIZE:=SIZE_0;
 
 {Setup PAGE_TABLE_BASE and SIZE}
 PAGE_TABLE_BASE:=QEMUVPB_PAGE_TABLE_BASE;
 PAGE_TABLE_SIZE:=QEMUVPB_PAGE_TABLE_SIZE;
 
 {Setup VECTOR_TABLE_BASE, SIZE and COUNT}
 VECTOR_TABLE_BASE:=QEMUVPB_VECTOR_TABLE_BASE;
 VECTOR_TABLE_SIZE:=QEMUVPB_VECTOR_TABLE_SIZE;
 VECTOR_TABLE_COUNT:=QEMUVPB_VECTOR_TABLE_COUNT;
 
 {Setup MACHINE_TYPE} 
 MACHINE_TYPE:=MACHINE_TYPE_UNKNOWN;
 case ARMMachineType of 
  ARM_MACHINE_VERSATILE_PB:MACHINE_TYPE:=MACHINE_TYPE_VERSATILEPB;
 end;
 
 {Setup BOARD_TYPE}
 BOARD_TYPE:=BOARD_TYPE_QEMUVPB;
 
 {Setup CPU_ARCH, TYPE and COUNT}
 {$IFDEF CPUARM}
 CPU_ARCH:=CPU_ARCH_ARM32;
 CPU_TYPE:=CPU_TYPE_ARMV7;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 CPU_ARCH:=CPU_ARCH_ARM64;
 CPU_TYPE:=CPU_TYPE_ARMV8;
 {$ENDIF CPUAARCH64}
 CPU_COUNT:=QEMUVPB_CPU_COUNT;
 CPU_BOOT:=QEMUVPB_CPU_BOOT;
 CPU_MASK:=QEMUVPB_CPU_MASK;
 CPU_MAX_COUNT:=QEMUVPB_CPU_COUNT;
 
 {Setup CPU_MEMORY_BASE and SIZE}
 CPU_MEMORY_BASE:=$00000000;
 CPU_MEMORY_SIZE:=SIZE_256M;
 
 {Setup CPU_MEMORY_RESTRICTED}
 CPU_MEMORY_RESTRICTED:=True;
 
 {Setup FPU_TYPE}
 FPU_TYPE:=FPU_TYPE_VFPV3;
 
 {Setup GPU_TYPE}
 GPU_TYPE:=GPU_TYPE_UNKNOWN;
 
 {Setup GPU_MEMORY_BASE and SIZE}
 GPU_MEMORY_BASE:=$00000000;
 GPU_MEMORY_SIZE:=0;
 
 {Setup GPU_MEMORY_CACHED}
 GPU_MEMORY_CACHED:=False;
 
 {Setup IRQ/FIQ/SWI_COUNT/START/ROUTING}
 IRQ_COUNT:=VERSATILEPB_IRQ_COUNT;
 FIQ_COUNT:=VERSATILEPB_FIQ_COUNT;
 
 IRQ_START:=0;                           {System wide IRQs start at zero}

 IRQ_ROUTING:=CPU_ID_0;                  {Route system wide IRQs to CPU0}
 FIQ_ROUTING:=CPU_ID_0;                  {Route system wide FIQs to CPU0}
 
 IRQ_LOCAL_COUNT:=0;                     {There are no Local IRQs}
 FIQ_LOCAL_COUNT:=0;                     {There are no Local FIQs}
 
 IRQ_LOCAL_START:=VERSATILEPB_IRQ_COUNT; {There are no Local IRQs}
 
 SWI_COUNT:=QEMUVPB_SWI_COUNT;
 
 {Setup IRQ/FIQ/SWI/UNDEF/ABORT_ENABLED}
 IRQ_ENABLED:=True;
 FIQ_ENABLED:=True;
 SWI_ENABLED:=True;
 ABORT_ENABLED:=True;
 UNDEFINED_ENABLED:=True;
 
 {Setup IRQ/FIQ/SWI/UNDEF/ABORT_STACK_ENABLED}
 IRQ_STACK_ENABLED:=True;
 FIQ_STACK_ENABLED:=True;
 SWI_STACK_ENABLED:=True;
 ABORT_STACK_ENABLED:=True;
 UNDEFINED_STACK_ENABLED:=True;
 
 {Setup CLOCK_FREQUENCY/TICKS/CYCLES}
 CLOCK_FREQUENCY:=VERSATILEPB_TIMER_FREQUENCY;
 CLOCK_TICKS_PER_SECOND:=1000;   {Note: QEMU uses the timeGetDevCaps() function on Windows which returns wPeriodMin as 1 millisecond}
 CLOCK_TICKS_PER_MILLISECOND:=1; {      That means that any timer interval less then 1ms will not be honoured, the result will be 1ms}
 CLOCK_CYCLES_PER_TICK:=CLOCK_FREQUENCY div CLOCK_TICKS_PER_SECOND;
 CLOCK_CYCLES_PER_MILLISECOND:=CLOCK_FREQUENCY div MILLISECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_MICROSECOND:=CLOCK_FREQUENCY div MICROSECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_NANOSECOND:=CLOCK_FREQUENCY div NANOSECONDS_PER_SECOND;
 CLOCK_CYCLES_TOLERANCE:=CLOCK_CYCLES_PER_TICK div 10;
 TIME_TICKS_PER_CLOCK_INTERRUPT:=CLOCK_TICKS_PER_MILLISECOND * TIME_TICKS_PER_MILLISECOND;
 
 {Setup HEAP Behaviour}
 HEAP_NORMAL_NONSHARED:=True;
 HEAP_IRQ_CACHE_COHERENT:=True;
 HEAP_FIQ_CACHE_COHERENT:=True;
 
 {Setup SCHEDULER_INTERRUPTS/CLOCKS}
 SCHEDULER_INTERRUPTS_PER_SECOND:=1000;   {Note: QEMU uses the timeGetDevCaps() function on Windows which returns wPeriodMin as 1 millisecond}
 SCHEDULER_INTERRUPTS_PER_MILLISECOND:=1; {      That means that any timer interval less then 1ms will not be honoured, the result will be 1ms}
 SCHEDULER_CLOCKS_PER_INTERRUPT:=CLOCK_FREQUENCY div SCHEDULER_INTERRUPTS_PER_SECOND;
 SCHEDULER_CLOCKS_TOLERANCE:=SCHEDULER_CLOCKS_PER_INTERRUPT div 10;
 TIME_TICKS_PER_SCHEDULER_INTERRUPT:=SCHEDULER_INTERRUPTS_PER_MILLISECOND * TIME_TICKS_PER_MILLISECOND;
 
 {Setup SCHEDULER_IDLE}
 SCHEDULER_IDLE_WAIT:=False;
 SCHEDULER_IDLE_OFFSET:=1;
 SCHEDULER_IDLE_PER_SECOND:=SCHEDULER_INTERRUPTS_PER_SECOND;
 
 {Setup KERNEL_NAME/CONFIG/COMMAND}
 KERNEL_NAME:=QEMUVPB_KERNEL_NAME;
 KERNEL_CONFIG:=QEMUVPB_KERNEL_CONFIG;
 KERNEL_COMMAND:=QEMUVPB_KERNEL_COMMAND;
 FIRMWARE_FILES:=QEMUVPB_FIRMWARE_FILES;
 
 {Register Platform BoardInit Handler}
 BoardInitHandler:=QEMUVPBBoardInit;
 
 {Register Platform MemoryInit Handler}
 MemoryInitHandler:=QEMUVPBMemoryInit;
 
 {Register Platform ClockInit Handler}
 ClockInitHandler:=QEMUVPBClockInit;
 
 {Register Platform PowerInit Handler}
 PowerInitHandler:=QEMUVPBPowerInit;
 
 {Register Platform InterruptInit Handler}
 InterruptInitHandler:=QEMUVPBInterruptInit;
 
 {Register Platform PeripheralInit Handler}
 PeripheralInitHandler:=QEMUVPBPeripheralInit;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Framebuffer FramebufferInit Handler}
 FramebufferInitHandler:=QEMUVPBFramebufferInit;
 {$ENDIF}
 {$IFDEF CPUARM}
 {Register PlatformARMv7 PageTableInit Handler}
 ARMv7PageTableInitHandler:=QEMUVPBPageTableInit;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformARMv8 PageTableInit Handler}
 ARMv8PageTableInitHandler:=QEMUVPBPageTableInit;
 {$ENDIF CPUAARCH64}
 
 {Register Platform Blink/Output Handlers}
 BootBlinkHandler:=QEMUVPBBootBlink;
 BootOutputHandler:=QEMUVPBBootOutput;
 
 {Register Platform IRQ Handlers}
 RequestExIRQHandler:=QEMUVPBRequestExIRQ;
 ReleaseExIRQHandler:=QEMUVPBReleaseExIRQ;

 {Register Platform FIQ Handlers}
 RequestExFIQHandler:=QEMUVPBRequestExFIQ;
 ReleaseExFIQHandler:=QEMUVPBReleaseExFIQ;

 {Register Platform System Call Handlers}
 RegisterSystemCallExHandler:=QEMUVPBRegisterSystemCallEx;
 DeregisterSystemCallExHandler:=QEMUVPBDeregisterSystemCallEx;

 {Register Platform Interrupt Handlers}
 GetInterruptEntryHandler:=QEMUVPBGetInterruptEntry;
 
 {Register Platform System Call Handlers}
 GetSystemCallEntryHandler:=QEMUVPBGetSystemCallEntry;
 
 {Register Platform System Handlers}
 SystemRestartHandler:=QEMUVPBSystemRestart;
 SystemShutdownHandler:=QEMUVPBSystemShutdown;
 
 {Register Platform Clock Handlers}
 ClockGetCountHandler:=QEMUVPBClockGetCount;
 ClockGetTotalHandler:=QEMUVPBClockGetTotal;
 
 {Register Threads SchedulerInit Handler}
 SchedulerInitHandler:=QEMUVPBSchedulerInit;
 {No SchedulerStart, QEMUVPB is Uniprocessor}
 
 {Register Threads SecondaryBoot Handler}
 {Nothing, QEMUVPB is Uniprocessor}
 
 {$IFDEF CPUARM}
 {Register PlatformARMv7 IRQ Handlers}
 ARMv7DispatchIRQHandler:=QEMUVPBDispatchIRQ;

 {Register PlatformARMv7 FIQ Handlers}
 ARMv7DispatchFIQHandler:=QEMUVPBDispatchFIQ;
 
 {Register PlatformARMv7 SWI Handlers}
 ARMv7DispatchSWIHandler:=QEMUVPBDispatchSWI;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformARMv8 IRQ Handlers}
 ARMv8DispatchIRQHandler:=QEMUVPBDispatchIRQ;

 {Register PlatformARMv8 FIQ Handlers}
 ARMv8DispatchFIQHandler:=QEMUVPBDispatchFIQ;
 
 {Register PlatformARMv8 SWI Handlers}
 ARMv8DispatchSWIHandler:=QEMUVPBDispatchSWI;
 {$ENDIF CPUAARCH64}
 
 {Set All Timers to Reference Clock (1MHz)}
 Value:=PLongWord(VERSATILEPB_SYSCTRL_REGS_BASE)^;
 Value:=Value or (VERSATILEPB_SYSCTRL_TIMCLK shl VERSATILEPB_SYSCTRL_TIMER0_ENSEL);
 Value:=Value or (VERSATILEPB_SYSCTRL_TIMCLK shl VERSATILEPB_SYSCTRL_TIMER1_ENSEL);
 Value:=Value or (VERSATILEPB_SYSCTRL_TIMCLK shl VERSATILEPB_SYSCTRL_TIMER2_ENSEL);
 Value:=Value or (VERSATILEPB_SYSCTRL_TIMCLK shl VERSATILEPB_SYSCTRL_TIMER3_ENSEL);
 PLongWord(VERSATILEPB_SYSCTRL_REGS_BASE)^:=Value;
 
 {Disable All Timers}
 PLongWord(VERSATILEPB_TIMER0_REGS_BASE + SP804_TIMER_CONTROL)^:=0;
 PLongWord(VERSATILEPB_TIMER1_REGS_BASE + SP804_TIMER_CONTROL)^:=0;
 PLongWord(VERSATILEPB_TIMER2_REGS_BASE + SP804_TIMER_CONTROL)^:=0;
 PLongWord(VERSATILEPB_TIMER3_REGS_BASE + SP804_TIMER_CONTROL)^:=0;
 
 QEMUVPBInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{QEMUVPB Platform Functions}
procedure QEMUVPBBoardInit;
begin
 {}
 {Nothing}
end;

{==============================================================================}

procedure QEMUVPBMemoryInit;
begin
 {}
 {Nothing}
end;

{==============================================================================}

procedure QEMUVPBClockInit;
begin
 {}
 {Setup Timer Registers}
 Timer0Registers:=PSP804TimerRegisters(VERSATILEPB_TIMER0_REGS_BASE); 
 
 {Setup Clock Variables}
 ClockBase:=TIME_TICKS_TO_1899;
 ClockLast:=0; 
 {$IFDEF CLOCK_TICK_MANUAL}
 ClockTicks:=0;
 ClockSeconds:=0;
 {$ENDIF}
 
 {Request the Clock IRQ/FIQ}
 if CLOCK_FIQ_ENABLED then
  begin
   RequestFIQ(QEMUVPB_CPU_BOOT,VERSATILEPB_IRQ_TIMER0_1,QEMUVPBClockInterrupt,nil); 
  end
 else
  begin
   RequestIRQ(QEMUVPB_CPU_BOOT,VERSATILEPB_IRQ_TIMER0_1,QEMUVPBClockInterrupt,nil); 
  end;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Setup the Timer}
 Timer0Registers.Load:=CLOCK_CYCLES_PER_TICK;
 Timer0Registers.Control:=SP804_TIMER_CONTROL_32BIT or SP804_TIMER_CONTROL_PRESCALE1 or SP804_TIMER_CONTROL_INT_ENABLED or SP804_TIMER_CONTROL_PERIODIC or SP804_TIMER_CONTROL_TIMER_ENABLED;
 
 {Setup the first Clock Interrupt}
 QEMUVPBClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
end;

{==============================================================================}

procedure QEMUVPBPowerInit;
begin
 {}
 {Setup Watchdog Registers}
 //To Do //Continuing
end;

{==============================================================================}

procedure QEMUVPBInterruptInit;
var
 Count:Integer;
begin
 {}
 {Setup Interrupt Registers}
 PrimaryInterruptRegisters:=PPL190InterruptRegisters(VERSATILEPB_VIC_REGS_BASE);
 SecondaryInterruptRegisters:=PVersatilePBInterruptRegisters(VERSATILEPB_SIC_REGS_BASE);
 
 {Setup Interrupt Entries}
 for Count:=0 to VERSATILEPB_IRQ_COUNT - 1 do
  begin
   FillChar(InterruptEntries[Count],SizeOf(TInterruptEntry),0);
   
   InterruptEntries[Count].Number:=Count;
   InterruptEntries[Count].CPUID:=CPU_ID_ALL;
  end; 
 
 {Setup System Call Entries}
 for Count:=0 to QEMUVPB_SWI_COUNT - 1 do
  begin
   FillChar(SystemCallEntries[Count],SizeOf(TSystemCallEntry),0);
   
   SystemCallEntries[Count].Number:=Count; 
   SystemCallEntries[Count].CPUID:=CPU_ID_ALL;
  end;
 
 {Setup Enabled IRQs}
 for Count:=0 to 1 do {Number of elements in IRQEnabled}
  begin
   IRQEnabled[Count]:=0;
  end; 
 
 {Setup Enabled FIQ}
 FIQEnabled:=LongWord(-1);
 
 {Clear Primary Interrupt Enable}
 PrimaryInterruptRegisters.INTENCLEAR:=$FFFFFFFF;
 
 {Setup Primary Interrupt Sic Source (Secondary)}
 PrimaryInterruptRegisters.INTENABLE:=(1 shl VERSATILEPB_IRQ_SICSOURCE);
 
 {Clear Secondary Interrupt Enable}
 SecondaryInterruptRegisters.SIC_ENCLR:=$FFFFFFFF;
 
 {Setup Secondary Interrupt Pass Through}
 SecondaryInterruptRegisters.SIC_PICENSET:=VERSATILEPB_SIC_PIC_MASK;
end;

{==============================================================================}

procedure QEMUVPBPeripheralInit;
var
 CacheLineSize:LongWord;
begin
 {}
 {Get Cache Line Size}
 CacheLineSize:=Max(L1DataCacheGetLineSize,L2CacheGetLineSize);
 
 {Setup Peripherals}
 INTERRUPT_REGS_BASE:=VERSATILEPB_VIC_REGS_BASE;
 SYSTEMTIMER_REGS_BASE:=VERSATILEPB_SYS_24MHZ;
 TIMER_REGS_BASE:=VERSATILEPB_TIMER0_REGS_BASE;
 GPIO_REGS_BASE:=VERSATILEPB_GPIO0_REGS_BASE;
 UART_REGS_BASE:=VERSATILEPB_UART0_REGS_BASE;
 
 {Setup Interrupts}
 //To Do 
 
 {Setup GPIO}
 //To Do //Continuing
 
 {Setup LEDs}
 //To Do //Continuing
 
 {Setup DMA}
 DMA_ALIGNMENT:=SizeOf(LongWord); 
 DMA_MULTIPLIER:=SizeOf(LongWord);
 DMA_SHARED_MEMORY:=True;
 DMA_NOCACHE_MEMORY:=False;
 DMA_BUS_ADDRESSES:=False;
 DMA_CACHE_COHERENT:=False; {True;} {L1 Cache is not coherent for normal memory}
 if CacheLineSize > DMA_ALIGNMENT then DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > DMA_MULTIPLIER then DMA_MULTIPLIER:=CacheLineSize;
 
 {Setup USB}
 //To Do //Continuing
 
 {Setup MMC}
 //To Do //Continuing
 
 {Setup VersatilePB}
 //To Do //Continuing //Done by QEMUVersatilePBInit
end;

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
procedure QEMUVPBFramebufferInit;
var
 Status:LongWord;
 PL110Framebuffer:PPL110Framebuffer;
begin
 {}
 {Create Framebuffer}
 PL110Framebuffer:=PPL110Framebuffer(FramebufferDeviceCreateEx(SizeOf(TPL110Framebuffer)));
 if PL110Framebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   PL110Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL110Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   PL110Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED{$IFDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};
   PL110Framebuffer.Framebuffer.Device.DeviceData:=nil;
   PL110Framebuffer.Framebuffer.Device.DeviceDescription:=PL110_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   PL110Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   PL110Framebuffer.Framebuffer.DeviceAllocate:=QEMUVPBFramebufferDeviceAllocate;
   PL110Framebuffer.Framebuffer.DeviceRelease:=QEMUVPBFramebufferDeviceRelease;
   PL110Framebuffer.Framebuffer.DeviceBlank:=QEMUVPBFramebufferDeviceBlank;
   PL110Framebuffer.Framebuffer.DeviceCommit:=QEMUVPBFramebufferDeviceCommit;
   PL110Framebuffer.Framebuffer.DeviceSetProperties:=QEMUVPBFramebufferDeviceSetProperties;
   {PL110}
   PL110Framebuffer.Mode:=PL110_MODE_SVGA;
   PL110Framebuffer.Depth:=FRAMEBUFFER_DEFAULT_DEPTH;
   PL110Framebuffer.Width:=FRAMEBUFFER_DEFAULT_WIDTH;
   PL110Framebuffer.Height:=FRAMEBUFFER_DEFAULT_HEIGHT;
   PL110Framebuffer.Rotation:=FRAMEBUFFER_DEFAULT_ROTATION;
   if (FRAMEBUFFER_DEFAULT_ROTATION = FRAMEBUFFER_ROTATION_90) or (FRAMEBUFFER_DEFAULT_ROTATION = FRAMEBUFFER_ROTATION_270) then
    begin
     PL110Framebuffer.Width:=FRAMEBUFFER_DEFAULT_HEIGHT;
     PL110Framebuffer.Height:=FRAMEBUFFER_DEFAULT_WIDTH;
    end;
   {Driver}
   PL110Framebuffer.Control:=PL110_CONTROL_SVGA;
   PL110Framebuffer.Timing0:=PL110_TIMING0_SVGA;
   PL110Framebuffer.Timing1:=PL110_TIMING1_SVGA;
   PL110Framebuffer.Timing2:=PL110_TIMING2_SVGA;
   PL110Framebuffer.Timing3:=0;
   PL110Framebuffer.Registers:=PPL110CLCDRegisters(VERSATILEPB_CLCD_REGS_BASE);
   
   {Setup Flags}
   {Nothing}
   
   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@PL110Framebuffer.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@PL110Framebuffer.Framebuffer,nil);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'QEMUVPB: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else
    begin     
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'QEMUVPB: Failed to register new framebuffer device: ' + ErrorToString(Status));
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'QEMUVPB: Failed to create new framebuffer device');
  end;
end;
{$ENDIF}
{==============================================================================}

procedure QEMUVPBPageTableInit;
{Initialize the Hardware Page Tables before enabling the MMU
 See ??????}
var
 Count:Integer;
 Table:PtrUInt;
 Address:PtrUInt;
 ActualAddress:PtrUInt;
 RequestAddress:PtrUInt;
begin
 {}
 {Parse Boot Tags (Register all memory with Heap manager)}
 if not(ParseBootTagsCompleted) then {$IFDEF CPUARM}ARMParseBootTags{$ENDIF CPUARM}{$IFDEF CPUAARCH64}AARCH64ParseBootTags{$ENDIF CPUAARCH64};

 {Parse Command Line (Copy command line from zero page)}
 if not(ParseCommandLineCompleted) then {$IFDEF CPUARM}ARMParseCommandLine{$ENDIF CPUARM}{$IFDEF CPUAARCH64}AARCH64ParseCommandLine{$ENDIF CPUAARCH64};

 {Parse Environment (Copy environment from zero page)}
 if not(ParseEnvironmentCompleted) then {$IFDEF CPUARM}ARMParseEnvironment{$ENDIF CPUARM}{$IFDEF CPUAARCH64}AARCH64ParseEnvironment{$ENDIF CPUAARCH64};
 
 {$IFDEF CPUARM}
 {Create the first level page table}
 {Setup 1MB sections covering the entire 4GB address space with a default layout}
 {Set the 1MB sections in the first 1GB as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
 Address:=$00000000;
 for Count:=0 to 1023 do
  begin
   ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
   Inc(Address,SIZE_1M);
  end;
  
 {Set the 1MB sections in the second 1GB as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH (Non Shared)(Non Executable)(Read Write)}
 for Count:=1024 to 2047 do
  begin
   if CPU_MEMORY_RESTRICTED then
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_NONE);
    end 
   else
    begin
      ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
    end; 
   Inc(Address,SIZE_1M);
  end;
  
 {Set the 1MB sections in the remaining 2GB as ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED (Non Shared)(Non Executable)(Read Write)}
 for Count:=2048 to 4095 do
  begin
   if CPU_MEMORY_RESTRICTED then
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_NONE);
    end
   else
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
    end; 
   Inc(Address,SIZE_1M);
  end;
 
 {Set the 1MB sections containing the PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Non Shared)(Non Executable)(Read Write)} 
 if PERIPHERALS_SIZE > 0 then
  begin
   Address:=(PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   while Address < (PERIPHERALS_BASE + PERIPHERALS_SIZE) do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
  end;  
 
 {Set the 1MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Non Shared)(Non Executable)(Read Write)} 
 if LOCAL_PERIPHERALS_SIZE > 0 then
  begin
   Address:=(LOCAL_PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   while Address < (LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE) do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE); 
     Inc(Address,SIZE_1M);
    end;
  end;  

 {Create the second level (Coarse) page tables}
 Table:=(PAGE_TABLES_ADDRESS and ARMV7_L1D_COARSE_BASE_MASK);
 Address:=$00000000;
 for Count:=0 to PAGE_TABLES_USED - 1 do
  begin
   ARMv7SetPageTableCoarse(Address,Table,0);
   Inc(Table,SIZE_1K);
   Inc(Address,SIZE_1M);
  end;
 PAGE_TABLES_NEXT:=Table;
 
 {Set the 4KB zero page to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED (Non Shared)(Non Executable)(No Access)}
 Address:=$00000000;
 ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_NONE); 

 {Set the 4KB pages containing the VECTOR_TABLE_BASE to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)} 
 Address:=(VECTOR_TABLE_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (VECTOR_TABLE_BASE + VECTOR_TABLE_SIZE) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end; 
 
 {Set the 4KB pages containing the first level page table to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)} 
 Address:=(PAGE_TABLE_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (PAGE_TABLE_BASE + PAGE_TABLE_SIZE) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;
 
 {Set the 4KB pages containing the TEXT (Code) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)} 
 Address:=(LongWord(@_text_start) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (LongWord(@_data)) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the DATA (Initialized) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
 Address:=(LongWord(@_data) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (LongWord(@_bss_start)) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE); 
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the BSS (Uninitialized) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
 Address:=(LongWord(@_bss_start) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (LongWord(@_bss_end)) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the second level page tables to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_TABLES_ADDRESS and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (PAGE_TABLES_ADDRESS + PAGE_TABLES_LENGTH) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;
 
 {Set the 4KB pages containing the initial stack to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_STACK_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (INITIAL_STACK_BASE + INITIAL_STACK_SIZE) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the initial heap to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_HEAP_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (INITIAL_HEAP_BASE + INITIAL_HEAP_SIZE) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;
 
 {Set the starting address for NoCache/Device/Shared/Local/IRQ/FIQ Blocks}
 if CPU_MEMORY_SIZE > 0 then
  begin
   {Get the top of CPU memory}
   RequestAddress:=CPU_MEMORY_BASE + CPU_MEMORY_SIZE;
   
   {Round CPU memory to a 1MB multiple (Divide by 1MB / Multiply by 1MB)}
   RequestAddress:=(RequestAddress shr 20) shl 20;
   if RequestAddress > 0 then
    begin
     {Round NoCache/Device/Shared/Local/IRQ/FIQ sizes to a 1MB multiple}   
     MEMORY_NONSHARED_SIZE:=(MEMORY_NONSHARED_SIZE shr 20) shl 20;
     MEMORY_NOCACHE_SIZE:=(MEMORY_NOCACHE_SIZE shr 20) shl 20;
     MEMORY_DEVICE_SIZE:=(MEMORY_DEVICE_SIZE shr 20) shl 20;
     MEMORY_SHARED_SIZE:=(MEMORY_SHARED_SIZE shr 20) shl 20;
     MEMORY_LOCAL_SIZE:=(MEMORY_LOCAL_SIZE shr 20) shl 20;
     MEMORY_IRQ_SIZE:=(MEMORY_IRQ_SIZE shr 20) shl 20;
     MEMORY_FIQ_SIZE:=(MEMORY_FIQ_SIZE shr 20) shl 20;
     
     {Subtract from top of CPU memory}
     Dec(RequestAddress,MEMORY_NONSHARED_SIZE);
     Dec(RequestAddress,MEMORY_NOCACHE_SIZE);
     Dec(RequestAddress,MEMORY_DEVICE_SIZE);
     Dec(RequestAddress,MEMORY_SHARED_SIZE);
     Dec(RequestAddress,MEMORY_LOCAL_SIZE * QEMUVPB_CPU_COUNT); {Local memory is per CPU}
     if IRQ_ENABLED then Dec(RequestAddress,MEMORY_IRQ_SIZE * QEMUVPB_CPU_COUNT); {IRQ memory is per CPU}
     if FIQ_ENABLED then Dec(RequestAddress,MEMORY_FIQ_SIZE * QEMUVPB_CPU_COUNT); {FIQ memory is per CPU}
     
     {Register 1MB Non Shared Memory Blocks as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_NONSHARED_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestNonSharedHeapBlock(Pointer(RequestAddress),MEMORY_NONSHARED_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         while Address < (ActualAddress + MEMORY_NONSHARED_SIZE) do
          begin
           ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
           Inc(Address,SIZE_1M);
          end;
         Inc(RequestAddress,MEMORY_NONSHARED_SIZE);
        end;
      end;
     
     {Register 1MB Non Cached Memory Blocks as ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_NOCACHE_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestNoCacheHeapBlock(Pointer(RequestAddress),MEMORY_NOCACHE_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         while Address < (ActualAddress + MEMORY_NOCACHE_SIZE) do
          begin
           ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
           Inc(Address,SIZE_1M);
          end;
         Inc(RequestAddress,MEMORY_NOCACHE_SIZE);
        end;
      end;
  
     {Register 1MB Device Memory Blocks as ARMV7_L1D_CACHE_REMAP_DEVICE (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_DEVICE_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestDeviceHeapBlock(Pointer(RequestAddress),MEMORY_DEVICE_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         while Address < (ActualAddress + MEMORY_DEVICE_SIZE) do
          begin
           ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
           Inc(Address,SIZE_1M);
          end;
         Inc(RequestAddress,MEMORY_DEVICE_SIZE);
        end;
      end;
     
     {Register 1MB Shared Memory Blocks as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
     if MEMORY_SHARED_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestSharedHeapBlock(Pointer(RequestAddress),MEMORY_SHARED_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         while Address < (ActualAddress + MEMORY_SHARED_SIZE) do
          begin
           ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
           Inc(Address,SIZE_1M);
          end;
         Inc(RequestAddress,MEMORY_SHARED_SIZE);
        end;
      end;
   
     {Register 1MB Local Memory Blocks as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_LOCAL_SIZE > 0 then
      begin
       for Count:=0 to (QEMUVPB_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestLocalHeapBlock(Pointer(RequestAddress),MEMORY_LOCAL_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           while Address < (ActualAddress + MEMORY_LOCAL_SIZE) do
            begin
             ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
             Inc(Address,SIZE_1M);
            end;
           Inc(RequestAddress,MEMORY_LOCAL_SIZE);
          end;
        end;
      end;
   
     {Register 1MB IRQ Memory Blocks as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if IRQ_ENABLED and (MEMORY_IRQ_SIZE > 0) then
      begin
       for Count:=0 to (QEMUVPB_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestIRQHeapBlock(Pointer(RequestAddress),MEMORY_IRQ_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           while Address < (ActualAddress + MEMORY_IRQ_SIZE) do
            begin
             ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
             Inc(Address,SIZE_1M);
            end;
           Inc(RequestAddress,MEMORY_IRQ_SIZE);
          end;
        end;  
      end; 
   
     {Register 1MB FIQ Memory Blocks as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if FIQ_ENABLED and (MEMORY_FIQ_SIZE > 0) then
      begin
       for Count:=0 to (QEMUVPB_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestFIQHeapBlock(Pointer(RequestAddress),MEMORY_FIQ_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           while Address < (ActualAddress + MEMORY_FIQ_SIZE) do
            begin
             ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
             Inc(Address,SIZE_1M);
            end;
           Inc(RequestAddress,MEMORY_FIQ_SIZE);
          end;
        end;  
      end; 
    end; 
  end;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 //To Do
 {$ENDIF CPUAARCH64}
 
 {Synchronization Barrier}
 DataSynchronizationBarrier;
end;

{==============================================================================}

function QEMUVPBRequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified IRQ number}
{CPUID: CPU to route IRQ to}
{Number: IRQ number to register}
{Handler: Interrupt handler function to register}
{HandlerEx: Extended Interrupt handler function to register}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  Result:=ERROR_ALREADY_ASSIGNED;
  if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
  if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
 
  {Find Group}
  if Number < 32 then
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}
 
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Enable IRQ}
    PrimaryInterruptRegisters.INTENABLE:=(1 shl Number);
    IRQEnabled[0]:=IRQEnabled[0] or (1 shl Number);
    
    {Register Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=Handler;
    InterruptEntries[Number].HandlerEx:=HandlerEx;
    InterruptEntries[Number].Parameter:=Parameter;
   end
  else if Number < 64 then
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}
 
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Enable IRQ}
    SecondaryInterruptRegisters.SIC_ENSET:=(1 shl (Number - 32));
    IRQEnabled[1]:=IRQEnabled[1] or (1 shl (Number - 32));
    
    {Register Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=Handler;
    InterruptEntries[Number].HandlerEx:=HandlerEx;
    InterruptEntries[Number].Parameter:=Parameter;
   end
  else 
   begin
    {Nothing under QEMU}
    Exit;
   end;
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified IRQ number}
{CPUID: CPU to route IRQ to}
{Number: IRQ number to deregister}
{Handler: Interrupt handler function to deregister}
{HandlerEx: Extended Interrupt handler function to deregister}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  Result:=ERROR_NOT_ASSIGNED;
  if not(Assigned(InterruptEntries[Number].Handler)) and not(Assigned(InterruptEntries[Number].HandlerEx)) then Exit;
 
  {Check Handlers}
  Result:=ERROR_ALREADY_ASSIGNED;
  if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
  if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
 
  {Find Group}
  if Number < 32 then
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Disable IRQ}
    PrimaryInterruptRegisters.INTENCLEAR:=(1 shl Number);
    IRQEnabled[0]:=IRQEnabled[0] and not(1 shl Number); 
    
    {Deregister Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=nil;
    InterruptEntries[Number].HandlerEx:=nil;
    InterruptEntries[Number].Parameter:=nil;
   end
  else if Number < 64 then
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Disable IRQ}
    SecondaryInterruptRegisters.SIC_ENCLR:=(1 shl (Number - 32));
    IRQEnabled[1]:=IRQEnabled[1] and not(1 shl (Number - 32));
    
    {Deregister Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=nil;
    InterruptEntries[Number].HandlerEx:=nil;
    InterruptEntries[Number].Parameter:=nil;
   end
  else 
   begin
    {Nothing under QEMU}
    Exit;
   end;
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBRequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
{Request registration of the supplied handler to the specified FIQ number}
{CPUID: CPU to route FIQ to}
{Number: FIQ number to register}
{Handler: Interrupt handler function to register}
{HandlerEx: Extended Interrupt handler function to register}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit; {IRQ Count not FIQ Count}

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  Result:=ERROR_ALREADY_ASSIGNED;
  if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
  if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
 
  {Find Group}
  if Number < 32 then
   begin
    {Check FIQ}
    if FIQEnabled <> LongWord(-1) then Exit; {FIQEnabled will be -1 when nothing enabled}
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Enable FIQ}
    PrimaryInterruptRegisters.INTENABLE:=(1 shl Number);
    PrimaryInterruptRegisters.INTSELECT:=PrimaryInterruptRegisters.INTSELECT or (1 shl Number);
    FIQEnabled:=Number;
    
    {Register Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=Handler;
    InterruptEntries[Number].HandlerEx:=HandlerEx;
    InterruptEntries[Number].Parameter:=Parameter;
   end
  else if Number < 64 then
   begin
    {Not supported on Secondary Interrupt Controller}
    Exit;
   end
  else 
   begin
    {Nothing under QEMU}
    Exit;
   end;
    
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
{Request deregistration of the supplied handler from the specified FIQ number}
{CPUID: CPU to route FIQ to}
{Number: FIQ number to deregister}
{Handler: Interrupt handler function to deregister}
{HandlerEx: Extended Interrupt handler function to deregister}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit; {IRQ Count not FIQ Count}

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  Result:=ERROR_NOT_ASSIGNED;
  if not(Assigned(InterruptEntries[Number].Handler)) and not(Assigned(InterruptEntries[Number].HandlerEx)) then Exit;
 
  {Check Handlers}
  Result:=ERROR_ALREADY_ASSIGNED;
  if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
  if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
 
  {Find Group}
  if Number < 32 then
   begin
    {Check FIQ}
    if FIQEnabled <> Number then Exit; {FIQEnabled will be -1 when nothing enabled}
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Disable FIQ}
    PrimaryInterruptRegisters.INTENCLEAR:=(1 shl Number);
    PrimaryInterruptRegisters.INTSELECT:=PrimaryInterruptRegisters.INTSELECT and not(1 shl Number);
    FIQEnabled:=LongWord(-1);
    
    {Deregister Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=nil;
    InterruptEntries[Number].HandlerEx:=nil;
    InterruptEntries[Number].Parameter:=nil;
   end
  else if Number < 64 then
   begin
    {Not supported on Secondary Interrupt Controller}
    Exit;
   end
  else 
   begin
    {Nothing under QEMU}
    Exit;
   end;
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBRegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request registration of the supplied extended handler to the specified System Call number}
{CPUID: The CPU ID to register the System Call against (Ignored on QEMUVPB)}
{Number: The System Call number to be registered}
{Handler: The handler function to be registered}
{HandlerEx: The extended handler function to be registered}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Number}
 if Number > (SWI_COUNT - 1) then Exit;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  Result:=ERROR_ALREADY_ASSIGNED;
  if Assigned(SystemCallEntries[Number].Handler) and (@SystemCallEntries[Number].Handler <> @Handler) then Exit;
  if Assigned(SystemCallEntries[Number].HandlerEx) and (@SystemCallEntries[Number].HandlerEx <> @HandlerEx) then Exit;
 
  {Register Entry}
  SystemCallEntries[Number].CPUID:=CPU_ID_ALL;
  SystemCallEntries[Number].Handler:=Handler;
  SystemCallEntries[Number].HandlerEx:=HandlerEx;
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBDeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request deregistration of the supplied extended handler from the specified System Call number}
{CPUID: The CPU ID to deregister the System Call from (Ignored on QEMUVPB)}
{Number: The System Call number to be deregistered}
{Handler: The handler function to be deregistered}
{HandlerEx: The extended handler function to be deregistered}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Number}
 if Number > (SWI_COUNT - 1) then Exit;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  Result:=ERROR_NOT_ASSIGNED;
  if not(Assigned(SystemCallEntries[Number].Handler)) and not(Assigned(SystemCallEntries[Number].HandlerEx)) then Exit;
 
  {Check Handlers}
  Result:=ERROR_ALREADY_ASSIGNED;
  if Assigned(SystemCallEntries[Number].Handler) and (@SystemCallEntries[Number].Handler <> @Handler) then Exit;
  if Assigned(SystemCallEntries[Number].HandlerEx) and (@SystemCallEntries[Number].HandlerEx <> @HandlerEx) then Exit;
 
  {Deregister Entry}
  SystemCallEntries[Number].CPUID:=CPU_ID_ALL;
  SystemCallEntries[Number].Handler:=nil;
  SystemCallEntries[Number].HandlerEx:=nil;
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBGetInterruptEntry(Number:LongWord):TInterruptEntry; 
{Get the interrupt entry for the specified interrupt number}
begin
 {}
 FillChar(Result,SizeOf(TInterruptEntry),0);
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Return Entry}
  Result:=InterruptEntries[Number];
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBGetSystemCallEntry(Number:LongWord):TSystemCallEntry; 
{Get the system call entry for the specified system call number}
begin
 {}
 FillChar(Result,SizeOf(TSystemCallEntry),0);
 
 {Check Number}
 if Number > (SWI_COUNT - 1) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Return Entry}
  Result:=SystemCallEntries[Number];
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function QEMUVPBSystemRestart(Delay:LongWord):LongWord; 
var
 Value:LongWord;
 Mask:TIRQFIQMask;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Delay}
 Sleep(Delay);

 {Disable IRQ/FIQ}
 Mask:=SaveIRQFIQ;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Register}
 Value:=PLongWord(VERSATILEPB_SYS_RESETCTL)^ or VERSATILEPB_SYS_RESETCTL_RESET or VERSATILEPB_SYS_RESETCTL_PORRESET;
 
 {Unlock}
 PLongWord(VERSATILEPB_SYS_LOCK)^:=VERSATILEPB_SYS_LOCK_LOCKVAL;
 
 {Reset}
 PLongWord(VERSATILEPB_SYS_RESETCTL)^:=Value;
 
 {Lock}
 PLongWord(VERSATILEPB_SYS_LOCK)^:=0;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Delay (Non Sleep)}
 MillisecondDelay(1000);
 
 {Restore IRQ/FIQ}
 RestoreIRQFIQ(Mask);
end;

{==============================================================================}

function QEMUVPBSystemShutdown(Delay:LongWord):LongWord;
begin
 {}
 Result:=ERROR_CALL_NOT_IMPLEMENTED;
end;

{==============================================================================}

function QEMUVPBClockGetCount:LongWord;
{Gets the current system clock count (32 least significant bits of total)}
{Note: On the VersatilePB this comes from the 24MHz counter which will 
 overflow every 178 seconds and increment the rollover value. Because we
 return the lower 32 bits then the value returned by this function will
 rollover to zero every 4295 seconds or about every 71 minutes}
begin
 {}
 Result:=QEMUVPBClockGetTotal;
end;

{==============================================================================}

function QEMUVPBClockGetTotal:Int64; 
{Gets the total system clock count}
{Note: On the VersatilePB this comes from the 24MHz counter which will 
 overflow every 178 seconds and increment the rollover value. This is 
 only accurate if either ClockGetCount or ClockGetTotal is called at
 least once per 178 seconds in order to increment the rollover}
var
 Value:LongWord;
begin
 {}
 {Acquire Lock}
 if ClockGetLock <> INVALID_HANDLE_VALUE then
  begin
   if SCHEDULER_FIQ_ENABLED then
    begin
     SpinLockIRQFIQ(ClockGetLock);
    end
   else
    begin
     SpinLockIRQ(ClockGetLock);
    end;    
  end; 

 {Get 24MHz Counter}
 Value:=PLongWord(VERSATILEPB_SYS_24MHZ)^ div 24;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Check for Rollover}
 if Value < ClockGetLast then
  begin
   {Increment Base}
   Inc(ClockGetBase,178956970); {0xFFFFFFFF div 24}
  end;
 
 {Save Last Value} 
 ClockGetLast:=Value;
 
 {Get Result}
 Result:=ClockGetBase + Value; 
 
 {Release Lock}
 if ClockGetLock <> INVALID_HANDLE_VALUE then
  begin
   if SCHEDULER_FIQ_ENABLED then
    begin
     SpinUnlockIRQFIQ(ClockGetLock);
    end
   else
    begin
     SpinUnlockIRQ(ClockGetLock);
    end;    
  end; 
end;

{==============================================================================}

procedure QEMUVPBClockGetTimer(Data:Pointer);
{Timer procedure to ensure ClockGetTotal is called at least once per rollover interval}
{Note: Not intended to be called directly by applications}
var
 Value:Int64;
begin
 {}
 Value:=QEMUVPBClockGetTotal;
end;

{==============================================================================}
{==============================================================================}
{QEMUVPB Thread Functions}
procedure QEMUVPBSchedulerInit;
{Initialize the scheduler interrupt on the boot CPU}
begin
 {}
 {Setup Timer Registers}
 Timer2Registers:=PSP804TimerRegisters(VERSATILEPB_TIMER2_REGS_BASE); 
 
 {Request the Scheduler IRQ/FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   RequestExFIQ(QEMUVPB_CPU_BOOT,VERSATILEPB_IRQ_TIMER2_3,nil,QEMUVPBSchedulerInterrupt,nil);
  end
 else
  begin
   RequestExIRQ(QEMUVPB_CPU_BOOT,VERSATILEPB_IRQ_TIMER2_3,nil,QEMUVPBSchedulerInterrupt,nil);
  end;

 {Register the Scheduler SWI}
 RegisterSystemCall(SYSTEM_CALL_CONTEXT_SWITCH,QEMUVPBSchedulerSystemCall);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Setup the Timer} 
 Timer2Registers.Load:=SCHEDULER_CLOCKS_PER_INTERRUPT;
 Timer2Registers.Control:=SP804_TIMER_CONTROL_32BIT or SP804_TIMER_CONTROL_PRESCALE1 or SP804_TIMER_CONTROL_INT_ENABLED or SP804_TIMER_CONTROL_PERIODIC or SP804_TIMER_CONTROL_TIMER_ENABLED;
 
 {Setup the first Clock Interrupt}
 QEMUVPBSchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[SCHEDULER_CPU_BOOT]);
 
 {Create the Clock Lock (Here instead of ClockInit to ensure that locking is initialized)}
 ClockGetLock:=SpinCreate;
end;

{==============================================================================}
{==============================================================================}
{QEMUVPB IRQ Functions}
function QEMUVPBDispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending IRQ requests}
{Called by ARMv7/8IRQHandler in PlatformARMv7/8}
{Note: A DataMemoryBarrier is executed before and after calling this function} 
var
 Group:LongWord;
 IRQBit:LongWord;
 IRQMatch:LongWord;
begin
 {}
 Result:=Thread;
 
 {$IFDEF INTERRUPT_DEBUG}
 Inc(DispatchInterruptCounter[CPUID]);
 {$ENDIF}
 
 {Check IRQ Groups}
 for Group:=0 to 1 do
  begin
   {Check IRQ Enabled}
   if IRQEnabled[Group] <> 0 then
    begin
     case Group of
      {Check Primary Controller IRQ}
      0:IRQMatch:=(IRQEnabled[Group] and PrimaryInterruptRegisters.IRQSTATUS);
      {Check Secondary Controller IRQ}
      1:IRQMatch:=(IRQEnabled[Group] and SecondaryInterruptRegisters.SIC_STATUS);
     end; 
     {Check IRQ Match}
     while IRQMatch <> 0 do
      begin
       {Find first set bit}
       IRQBit:=FirstBitSet(IRQMatch); 
         
       {Clear set bit}
       IRQMatch:=IRQMatch xor (1 shl IRQBit);
         
       {Call IRQ Handler}
       Result:=QEMUVPBHandleIRQ(IRQBit + (Group shl 5),CPUID,Result); {Pass Result as Thread to allow for multiple calls}
      end; 
    end;
  end;  
end;

{==============================================================================}

function QEMUVPBHandleIRQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for an IRQ that was received, or halt if it doesn't exist}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;
 
 {Get Entry}
 Entry:=@InterruptEntries[Number];
 
 {Check Interrupt Handler}
 if Assigned(Entry.Handler) then
  begin
   Entry.Handler(Entry.Parameter); 
  end
 else
  begin
   if Assigned(Entry.HandlerEx) then
    begin
     Result:=Entry.HandlerEx(CPUID,Thread,Entry.Parameter);  
    end
   else
    begin 
     {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}    
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('No handler registered for interrupt ' + IntToStr(Number));
     {$ENDIF} 
     
     Halt;   
    end; 
  end;  
end;

{==============================================================================}
{==============================================================================}
{QEMUVPB FIQ Functions}
function QEMUVPBDispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending FIQ requests}
{Called by ARMv7/8FIQHandler in PlatformARMv7/8}
{Note: A DataMemoryBarrier is executed before and after calling this function} 
begin
 {}
 Result:=Thread;
 
 {$IFDEF INTERRUPT_DEBUG}
 Inc(DispatchFastInterruptCounter[CPUID]);
 {$ENDIF}
 
 {Check FIQ Enabled}
 if FIQEnabled <> LongWord(-1) then
  begin
   {Call FIQ Handler}
   Result:=QEMUVPBHandleFIQ(FIQEnabled,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
  end;
end;

{==============================================================================}

function QEMUVPBHandleFIQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for an FIQ that was received, or halt if it doesn't exist}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;
 
 {Get Entry}
 Entry:=@InterruptEntries[Number];
 
 {Check Interrupt Handler}
 if Assigned(Entry.Handler) then
  begin
   Entry.Handler(Entry.Parameter); 
  end
 else
  begin
   if Assigned(Entry.HandlerEx) then
    begin
     Result:=Entry.HandlerEx(CPUID,Thread,Entry.Parameter);  
    end
   else
    begin 
     {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}    
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('No handler registered for interrupt ' + IntToStr(Number));
     {$ENDIF} 
     
     Halt;   
    end; 
  end;  
end;

{==============================================================================}
{==============================================================================}
{QEMUVPB SWI Functions}
function QEMUVPBDispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 
{Process an SWI request}
{Called by ARMv7/8SoftwareInterruptHandler in PlatformARMv7/8}
{Note: A DataMemoryBarrier is executed before and after calling this function} 
var
 Entry:PSystemCallEntry;
begin
 {}
 Result:=Thread;
 
 {$IFDEF INTERRUPT_DEBUG}
 Inc(DispatchSystemCallCounter[CPUID]);
 {$ENDIF}

 {Check Request}
 if Request = nil then Exit;
 
 {Check Number}
 if Request.Number > (SWI_COUNT - 1) then Exit;
 
 {Get Entry}
 Entry:=@SystemCallEntries[Request.Number];
 
 {Check System Call Handler}
 if Assigned(Entry.Handler) then
  begin
   Entry.Handler(Request); 
  end
 else if Assigned(Entry.HandlerEx) then
  begin
   Result:=Entry.HandlerEx(CPUID,Thread,Request);  
  end;  
end;

{==============================================================================}
{==============================================================================}
{QEMUVPB Clock Functions}
procedure QEMUVPBClockInterrupt(Parameter:Pointer);
{Interrupt handler function for the clock interrupt. This schedules another clock
 interrupt to occur CLOCK_CYCLES_PER_TICK in the future, then updates ClockTicks
 and ClockSeconds and checks for timers to trigger}
begin
 {}
 {$IFDEF CLOCK_DEBUG}
 Inc(ClockInterruptCounter);
 {$ENDIF}

 {$IFDEF CLOCK_TICK_MANUAL}
 {Add another Clock Tick}
 Inc(ClockTicks);
 
 {Update Clock Seconds}
 if ClockTicks = CLOCK_TICKS_PER_SECOND then
  begin
   Inc(ClockSeconds);
   ClockTicks:=0;
  end;
 {$ENDIF}

 {Schedule the next Clock Interrupt}
 QEMUVPBClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
  
 {Check Timer Queue}
 if TimerCheck = ERROR_SUCCESS then
  begin
   {Trigger Timer Events}
   TimerTrigger;
  end;
  
 {Check Tasker List}
 if TaskerCheck = ERROR_SUCCESS then
  begin
   {Trigger Tasks}
   TaskerTrigger;
  end;
end;

{==============================================================================}

procedure QEMUVPBClockUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a clock interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the timer interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by CLOCK_FREQUENCY}
var
 Current:LongWord;
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Get Timer Value}
 Current:=Timer0Registers.Value;
 
 {Get Last}
 if Current > 0 then
  begin
   Last:=Cycles - (Cycles - Current);
  end
 else
  begin
   Last:=Cycles;
  end;  
 
 {Clear Interrupt}
 Timer0Registers.IRQClear:=1;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}
{==============================================================================}
{QEMUVPB Scheduler Functions}
function QEMUVPBSchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
{Interrupt handler function for the scheduler interrupt. This schedules another 
 scheduler interrupt to occur SCHEDULER_CLOCKS_PER_INTERRUPT in the future, then
 checks for threads to wakeup or timeout and the next thread to schedule}
begin
 {}
 Result:=Thread;
 
 {$IFDEF SCHEDULER_DEBUG}
 Inc(SchedulerInterruptCounter[CPUID]);
 {$ENDIF}
 
 {Add another Scheduler Interrupt}
 Inc(SchedulerInterrupts[CPUID]);
 
 {Update Utilization}
 if SchedulerInterrupts[CPUID] = SCHEDULER_INTERRUPTS_PER_SECOND then
  begin
   UtilizationLast[CPUID]:=UtilizationCurrent[CPUID];
   UtilizationCurrent[CPUID]:=SCHEDULER_IDLE_PER_SECOND;
   SchedulerInterrupts[CPUID]:=0;
   if UtilizationLast[CPUID] > SCHEDULER_IDLE_PER_SECOND then
    begin
     UtilizationLast[CPUID]:=0;
    end;
  end;
 
 {Schedule the next Scheduler Interrupt}
 QEMUVPBSchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);
 
 {Check Sleep and Timeout Queues}
 if SchedulerCheck(CPUID) = ERROR_SUCCESS then
  begin
   {Wakeup Sleep Threads}
   SchedulerWakeup(CPUID);
   
   {Expire Timeout Threads}
   SchedulerExpire(CPUID);
  end;
 {Check Preemption}
 if SchedulerThreadPreempt[CPUID] = SCHEDULER_PREEMPT_ENABLED then
  begin
   {Switch Threads}
   Result:=SchedulerSwitch(CPUID,Thread);
  end; 
end;

{==============================================================================}

procedure QEMUVPBSchedulerUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a scheduler interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the scheduler interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by VERSATILEPB_TIMER_FREQUENCY}
var
 Current:LongWord;
 {$IFDEF SCHEDULER_DEBUG}
 CurrentCPU:LongWord;
 {$ENDIF}
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Timer Value}
 Current:=Timer2Registers.Value;
 
 {Get Last}
 if Current > 0 then
  begin
   Last:=Cycles - (Cycles - Current);
  end
 else
  begin
   Last:=Cycles;
  end;  
 
 {Clear Interrupt}
 Timer2Registers.IRQClear:=1;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {$IFDEF SCHEDULER_DEBUG}
 CurrentCPU:=CPUGetCurrent;
 SchedulerInterruptOffset[CurrentCPU]:=Last;
 if SchedulerInterruptMinOffset[CurrentCPU] = 0 then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU]; 
 if SchedulerInterruptOffset[CurrentCPU] < SchedulerInterruptMinOffset[CurrentCPU] then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
 if SchedulerInterruptOffset[CurrentCPU] > SchedulerInterruptMaxOffset[CurrentCPU] then SchedulerInterruptMaxOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
 {$ENDIF}
end;

{==============================================================================}

procedure QEMUVPBSchedulerSystemCall(Request:PSystemCallRequest);
{System Call handler for the scheduler. This is registered to receive requests for
 the SYSTEM_CALL_CONTEXT_SWITCH and will perform a context switch from within an SWI}
begin
 {}
 {$IFDEF CPUARM}
 ARMv7ContextSwitchSWI(Pointer(Request.Param1),Pointer(Request.Param2),Request.Param3);
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 //To Do
 {$ENDIF CPUAARCH64}
end;

{==============================================================================}
{==============================================================================}
{QEMUVPB Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function QEMUVPBFramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceAllocate API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceAllocate instead}
var
 Value:LongWord;
 Bytes:LongWord;
 Buffer:Pointer;
 Defaults:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'QEMUVPB: Framebuffer Allocate');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Defaults}
    Defaults.Depth:=PPL110Framebuffer(Framebuffer).Depth;
    Defaults.Order:=FRAMEBUFFER_ORDER_RGB;
    Defaults.Mode:=FRAMEBUFFER_MODE_IGNORED;
    Defaults.PhysicalWidth:=PPL110Framebuffer(Framebuffer).Width;
    Defaults.PhysicalHeight:=PPL110Framebuffer(Framebuffer).Height;
    Defaults.VirtualWidth:=Defaults.PhysicalWidth; 
    Defaults.VirtualHeight:=Defaults.PhysicalHeight;
    Defaults.OffsetX:=0;                           
    Defaults.OffsetY:=0;                            
    Defaults.OverscanTop:=0;                         
    Defaults.OverscanBottom:=0;                      
    Defaults.OverscanLeft:=0;                        
    Defaults.OverscanRight:=0;                       
    Defaults.Rotation:=PPL110Framebuffer(Framebuffer).Rotation;
 
    {Check Properties}
    if Properties <> nil then
     begin
      {Adjust Depth}
      if (Properties.Depth = FRAMEBUFFER_DEPTH_16) or (Properties.Depth = FRAMEBUFFER_DEPTH_32) then Defaults.Depth:=Properties.Depth;
      {Adjust Order} {Do not allow}
      {if Properties.Order <= FRAMEBUFFER_ORDER_RGB then Defaults.Order:=Properties.Order;}
      {Adjust Rotation}
      if Properties.Rotation <= FRAMEBUFFER_ROTATION_270 then Defaults.Rotation:=Properties.Rotation;
      {Check Rotation}
      if Properties.Rotation <> PPL110Framebuffer(Framebuffer).Rotation then
       begin
        if (Properties.Rotation = FRAMEBUFFER_ROTATION_90) or (Properties.Rotation = FRAMEBUFFER_ROTATION_270) then 
         begin
          if (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_90) and (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_270) then
           begin
            Defaults.PhysicalWidth:=PPL110Framebuffer(Framebuffer).Height;
            Defaults.PhysicalHeight:=PPL110Framebuffer(Framebuffer).Width;
           end;
         end
        else
         begin
          if (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_0) and (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_180) then
           begin
            Defaults.PhysicalWidth:=PPL110Framebuffer(Framebuffer).Height;
            Defaults.PhysicalHeight:=PPL110Framebuffer(Framebuffer).Width;
           end;
         end;      
         
        Defaults.VirtualWidth:=Defaults.PhysicalWidth; 
        Defaults.VirtualHeight:=Defaults.PhysicalHeight;
       end;
     end;
 
    {Get Format}  
    case Defaults.Depth of
     FRAMEBUFFER_DEPTH_16:begin
       Defaults.Format:=COLOR_FORMAT_RGB16;
      end;
     FRAMEBUFFER_DEPTH_32:begin
       Defaults.Format:=COLOR_FORMAT_UBGR32; {Note: This is reversed in the hardware}
      end;
    end;
 
    {Get Bytes}
    Bytes:=ColorFormatToBytes(Defaults.Format);
    if Bytes = 0 then Exit;
    
    {Get Size}
    Defaults.Size:=(Defaults.PhysicalWidth * Defaults.PhysicalHeight) * Bytes;
    
    {Get Pitch}
    Defaults.Pitch:=Defaults.PhysicalWidth * Bytes;
 
    {Allocate Framebuffer}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) and DMAAvailable then
     begin
      {Allocate DMA Buffer}
      Buffer:=DMAAllocateBuffer(Defaults.Size);
     end
    else
     begin
      {Allocate Normal Buffer (No DMA)}
      {Use DMA Alignment and Multiplier if available}
      if (DMA_ALIGNMENT <> 0) and (DMA_MULTIPLIER <> 0) then
       begin
        Buffer:=GetAlignedMem(RoundUp(Defaults.Size,DMA_MULTIPLIER),DMA_ALIGNMENT);
       end
      else
       begin      
        Buffer:=GetMem(Defaults.Size);
       end; 
     end;
    if Buffer = nil then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit; 
     end; 
    
    {Check Cache}
    if not(DMA_CACHE_COHERENT) then
     begin
      {Clean Cache (Dest)}
      CleanDataCacheRange(LongWord(Buffer),Defaults.Size);
     end;
 
    {Update Framebuffer}
    Framebuffer.Address:=LongWord(Buffer);
    Framebuffer.Size:=Defaults.Size;
    Framebuffer.Pitch:=Defaults.Pitch;
    Framebuffer.Depth:=Defaults.Depth;
    Framebuffer.Order:=Defaults.Order;
    Framebuffer.Mode:=Defaults.Mode;
    Framebuffer.Format:=Defaults.Format;
    Framebuffer.PhysicalWidth:=Defaults.PhysicalWidth;
    Framebuffer.PhysicalHeight:=Defaults.PhysicalHeight;
    Framebuffer.VirtualWidth:=Defaults.VirtualWidth;
    Framebuffer.VirtualHeight:=Defaults.VirtualHeight;
    Framebuffer.OffsetX:=Defaults.OffsetX;
    Framebuffer.OffsetY:=Defaults.OffsetY;
    Framebuffer.OverscanTop:=Defaults.OverscanTop;
    Framebuffer.OverscanBottom:=Defaults.OverscanBottom;
    Framebuffer.OverscanLeft:=Defaults.OverscanLeft;
    Framebuffer.OverscanRight:=Defaults.OverscanRight;
    Framebuffer.Rotation:=Defaults.Rotation;
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Setup PL110}
    Value:=PPL110Framebuffer(Framebuffer).Control;
    if Framebuffer.Depth = FRAMEBUFFER_DEPTH_16 then Value:=Value or PL110_CLCD_CONTROL_LCDBPP16;
    if Framebuffer.Depth = FRAMEBUFFER_DEPTH_32 then Value:=Value or PL110_CLCD_CONTROL_LCDBPP24;
    if Framebuffer.Order = FRAMEBUFFER_ORDER_BGR then Value:=Value or PL110_CLCD_CONTROL_BGR;
    PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value;
    PPL110Framebuffer(Framebuffer).Registers.TIMING0:=(PPL110Framebuffer(Framebuffer).Timing0 and not(PL110_CLCD_TIMING0_PPL)) or (((Framebuffer.PhysicalWidth - 1) div 16) shl 2);
    PPL110Framebuffer(Framebuffer).Registers.TIMING1:=(PPL110Framebuffer(Framebuffer).Timing1 and not(PL110_CLCD_TIMING1_LPP)) or (Framebuffer.PhysicalHeight - 1);
    PPL110Framebuffer(Framebuffer).Registers.TIMING2:=PPL110Framebuffer(Framebuffer).Timing2;
    PPL110Framebuffer(Framebuffer).Registers.TIMING3:=PPL110Framebuffer(Framebuffer).Timing3;
    PPL110Framebuffer(Framebuffer).Registers.UPBASE:=LongWord(Buffer);
    PPL110Framebuffer(Framebuffer).Registers.LPBASE:=LongWord(Buffer) + ((Framebuffer.PhysicalHeight * Framebuffer.Pitch) div 2);
    
    {Enable PL110}
    Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
    Value:=Value or PL110_CLCD_CONTROL_LCDEN;
    PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
    MillisecondDelay(20);
    
    Value:=Value or PL110_CLCD_CONTROL_LCDPWR;
    PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    {Update Statistics}
    Inc(Framebuffer.AllocateCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function QEMUVPBFramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceRelease API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceRelease instead}
var
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'QEMUVPB: Framebuffer Release');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Disable PL110}
    Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
    if (Value and PL110_CLCD_CONTROL_LCDPWR) <> 0 then
     begin
      Value:=Value and not(PL110_CLCD_CONTROL_LCDPWR);
      PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value;
      MillisecondDelay(20);
     end;
     
    if (Value and PL110_CLCD_CONTROL_LCDEN) <> 0 then
     begin
      Value:=Value and not(PL110_CLCD_CONTROL_LCDEN);
      PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
     end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    {Release Framebuffer}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) and DMAAvailable then
     begin
      {Release DMA Buffer}
      Result:=DMAReleaseBuffer(Pointer(Framebuffer.Address));
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      {Release Normal Buffer (No DMA)}
      FreeMem(Pointer(Framebuffer.Address));
     end;
     
    {Update Framebuffer}
    Framebuffer.Address:=0;
    Framebuffer.Size:=0;
    Framebuffer.Pitch:=0;
    Framebuffer.Depth:=FRAMEBUFFER_DEPTH_32;
    Framebuffer.Order:=FRAMEBUFFER_ORDER_RGB;
    Framebuffer.Mode:=FRAMEBUFFER_MODE_ENABLED;
    Framebuffer.Format:=COLOR_FORMAT_DEFAULT;
    Framebuffer.PhysicalWidth:=0;
    Framebuffer.PhysicalHeight:=0;
    Framebuffer.VirtualWidth:=0;
    Framebuffer.VirtualHeight:=0;
    Framebuffer.OffsetX:=0;
    Framebuffer.OffsetY:=0;
    Framebuffer.OverscanTop:=0;
    Framebuffer.OverscanBottom:=0;
    Framebuffer.OverscanLeft:=0;
    Framebuffer.OverscanRight:=0;
    Framebuffer.Rotation:=FRAMEBUFFER_ROTATION_0;
    
    {Update Statistics}
    Inc(Framebuffer.ReleaseCount);
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function QEMUVPBFramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDevicBlank API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDevicBlank instead}
var
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'QEMUVPB: Framebuffer Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Blank}
 if Blank then
  begin
   {Disable PL110}
   Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
   if (Value and PL110_CLCD_CONTROL_LCDPWR) <> 0 then
    begin
     Value:=Value and not(PL110_CLCD_CONTROL_LCDPWR);
     PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value;
     MillisecondDelay(20);
    end;
    
   if (Value and PL110_CLCD_CONTROL_LCDEN) <> 0 then
    begin
     Value:=Value and not(PL110_CLCD_CONTROL_LCDEN);
     PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
    end;
  end
 else
  begin
   {Enable PL110}
   Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
   Value:=Value or PL110_CLCD_CONTROL_LCDEN;
   PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
   MillisecondDelay(20);
   
   Value:=Value or PL110_CLCD_CONTROL_LCDPWR;
   PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
  end;  
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
  
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function QEMUVPBFramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceCommit API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceCommit instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'QEMUVPB: Framebuffer Commit (Address=' + IntToHex(Address,8) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Flags}
 if (Flags and FRAMEBUFFER_TRANSFER_DMA) = 0 then
  begin
   {Clean Cache}
   //To Do //Continuing //Check for DMA Cache Coherent ?
  end
 else
  begin
   {Invalidate Cache}
   //To Do //Continuing //Check for DMA Cache Coherent ?
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function QEMUVPBFramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceSetProperties API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetProperties instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'QEMUVPB: Framebuffer Set Properties');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
 
    //To Do //Check Properties against current, modify if possible, otherwise reallocate ? (and Notify Resize)
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
{$ENDIF}
{==============================================================================}
{==============================================================================}
{QEMUVPB Helper Functions}
procedure QEMUVPBBootBlink; assembler; nostackframe;
{Output characters to UART0 without dependency on any other RTL setup}
{$IFDEF CPUARM}
asm
 ldr r1, =VERSATILEPB_UART0_REGS_BASE
 
.Loop: 
 mov r0, #65
 str r0, [r1]
 
 b .Loop
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure QEMUVPBBootOutput(Value:LongWord);
{Output characters to UART0 without dependency on any other RTL setup}
{Based on hexstrings() function by dwelch67 (https://github.com/dwelch67)}
var
 Bits:LongWord;
 Character:LongWord;
begin
 {}
 Bits:=32;
 while True do
  begin
   Dec(Bits,4);
   
   Character:=(Value shr Bits) and $0F;
   if Character > 9 then
    begin
     Character:=Character + $37;
    end
   else
    begin
     Character:=Character + $30;
    end;
    
   PLongWord(VERSATILEPB_UART0_REGS_BASE)^:=Character;
   
   if Bits = 0 then Break;
  end;
 
 {Line End}
 PLongWord(VERSATILEPB_UART0_REGS_BASE)^:=$0D;
 PLongWord(VERSATILEPB_UART0_REGS_BASE)^:=$0A;
end;

{==============================================================================}
{==============================================================================}

end.
