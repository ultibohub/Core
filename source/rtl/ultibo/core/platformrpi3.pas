{
Ultibo Platform interface unit for Raspberry Pi 3.

Copyright (C) 2021 - SoftOz Pty Ltd.

Arch
====

 ARMv8 (Cortex A53)

Boards
======

 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3/CM3+
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  rsta2 (circle) - https://github.com/rsta2/circle
  
  dwelch67 (raspberrypi) - https://github.com/dwelch67/raspberrypi
  
  PeterLemon (RaspberryPi) - https://github.com/PeterLemon/RaspberryPi
  
  brianwiddas (pi-baremetal) - https://github.com/brianwiddas/pi-baremetal
  
  OSDev - http://wiki.osdev.org/Raspberry_Pi_Bare_Bones
          http://wiki.osdev.org/ARM_RaspberryPi_Tutorial_C
 
  U-Boot - \arch\arm\cpu\arm1176\bcm2835\mbox.c
           \arch\arm\include\asm\arch-bcm2835\mbox.h
 
  Linux - \drivers\gpio\gpio-bcm-virt.c
  
References
==========

 BCM2835 ARM Peripherals
 
 QA7 Rev3.4

 Cortex-A8 MPCore Technical Reference Manual (Revision: r0p4)
 
 ARM v8 Architecture Reference Manual
 
 ARM Architecture Reference Manual (ARMv8-A)
 
 Raspberry Pi Mailboxes
 
  https://github.com/raspberrypi/firmware/wiki/Mailboxes

 RPi Framebuffer
 
  http://elinux.org/RPi_Framebuffer
 
Platform RPi3
=============

 Notes: The RPi3 B has the Activity LED connected to the GPU, access is via a Virtual GPIO (Power LED is only accessible via the GPIO expander driver)
        The RPi3 B+ has the Activity LED connected to GPIO Pin 29 (Power LED is only accessible via the GPIO expander driver)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformRPi3; 

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2837,Platform,{$IFDEF CPUARM}PlatformARM,PlatformARMv7,{$ENDIF CPUARM}{$IFDEF CPUAARCH64}PlatformAARCH64,PlatformARMv8,{$ENDIF CPUAARCH64}HeapManager,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF},SysUtils;

{==============================================================================}
const
 {RPi3 specific constants}

 {ARM Physical to VC IO Mapping}
 RPI3_VCIO_ALIAS = BCM2837_VCIO_ALIAS;     {The VCIO Alias (For ARM Physcial to VC IO translation)}
 
 {ARM Physical to VC Bus Mapping}
 RPI3_VCBUS_ALIAS = BCM2837_VCBUS_C_ALIAS; {The currently selected VCBUS Alias (For ARM Physcial to VC Bus translation)}

const
 {Secure World Boot} 
 {RPI3_SECURE_BOOT = $00000001;}           {If 1 then startup will attempt to switch back to secure world during boot process} {Moved to ARMSecureBoot}
 RPI3_SECURE_BOOT_OFFSET = $000000D4;      {The address of the Secure Boot marker in the ARM boot stub}
 RPI3_SECURE_BOOT_MARKER = $58495052;      {The Secure Boot marker (ASCII "RPIX")}
 
{const}
 {Address of StartupHandler on Reset}
 {RPI3_STARTUP_ADDRESS = $00008000;} {Obtain from linker}
 
const
 {Page Table Address and Size}
 RPI3_PAGE_TABLE_BASE = $00004000;     {Place the first level Page Table after the interrupt vectors at 0x00001000 and before the code start at 0x00008000}
 RPI3_PAGE_TABLE_SIZE = SIZE_16K;      {ARM Cortex A7 first level Page Table is exactly 16KB in size (4096 32 bit (4 byte) entries)}
 
const
 {Vector Table Address and Size} 
 RPI3_VECTOR_TABLE_BASE  = $00001000;  {Place the Interrupt Vector Table at 0x00001000 before the code start at 0x00008000}
 RPI3_VECTOR_TABLE_SIZE  = SIZE_64;    {The Interrupt Vector Table is exactly 64 bytes (16 32 bit (4 byte) entries)}
 RPI3_VECTOR_TABLE_COUNT = 8;          {The Interrupt Vector Table contains 8 entries on an ARMv7 device}
 
const
 {CPU Count}
 RPI3_CPU_COUNT = BCM2837_CPU_COUNT;
 RPI3_CPU_BOOT = CPU_ID_0;
 RPI3_CPU_MASK = CPU_AFFINITY_0 or CPU_AFFINITY_1 or CPU_AFFINITY_2 or CPU_AFFINITY_3;

const
 {IRQ/FIQ Start/Routing}
 RPI3_IRQ_START = 0;                   {System wide IRQs start at zero}
 
 RPI3_IRQ_ROUTING = CPU_ID_0;          {Route system wide IRQs to CPU0}
 RPI3_FIQ_ROUTING = CPU_ID_0;          {Route system wide FIQs to CPU0}

 RPI3_IRQ_LOCAL_START = BCM2837_GPU_IRQ_COUNT + BCM2837_ARM_IRQ_COUNT; {Local IRQs start after GPU and ARM IRQs}

const
 {SWI}
 RPI3_SWI_COUNT = 256;                 {Number of available SWI entries}
 
const
 {Core Timer Prescaler}
 {$IFNDEF RPI3_MAX_CLOCK_RATE}
 RPI3_CORE_TIMER_PRESCALER    = $06AAAAAB; {Divide the Crystal Clock by 19.2 to give a 1MHz Core Timer}
 RPI3_CORE_TIMER_FREQUENCY    = 1000000;   {The Core Timer frequency from the prescaler setting above}
 RPI3_GENERIC_TIMER_FREQUENCY = 1000000;   {The ARM Generic Timer frequency from the prescaler setting above}
 {$ELSE}
 RPI3_CORE_TIMER_PRESCALER    = $80000000; {Divide the Crystal Clock by 1 to give a 19.2MHz Core Timer}
 RPI3_CORE_TIMER_FREQUENCY    = 19200000;  {The Core Timer frequency from the prescaler setting above}
 RPI3_GENERIC_TIMER_FREQUENCY = 19200000;  {The ARM Generic Timer frequency from the prescaler setting above}
 {$ENDIF}
 
const
 {Kernel Image Name}
 {$IFDEF CPUARM}
 RPI3_KERNEL_NAME = 'kernel7.img';
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 RPI3_KERNEL_NAME = 'kernel8.img';
 {$ENDIF CPUAARCH64}
 RPI3_KERNEL_CONFIG = 'config.txt';
 RPI3_KERNEL_COMMAND = 'cmdline.txt';
 RPI3_FIRMWARE_FILES = 'bootcode.bin,start.elf,fixup.dat'{$IFDEF CPUARM} + ',armstub32-rpi3.bin'{$ENDIF CPUARM}{$IFDEF CPUAARCH64} + ',armstub64-rpi3.bin'{$ENDIF CPUAARCH64};
 
const
 {Mailbox constants}
 RPI3_MAILBOX_TIMEOUT = 100;                         {Default timeout to wait for mailbox calls to complete (Milliseconds)}
 RPI3_MAILBOX_TIMEOUT_EX = 1000;                     {Extended timeout to wait for mailbox calls to complete (Milliseconds)}
 
const
 {Mailbox constants}
 RPI3_LOCAL_MAILBOX_TIMEOUT = 100;                   {Default timeout to wait for local mailbox calls to complete (Milliseconds)}
 
const
 {Framebuffer constants}
 RPI3_FRAMEBUFFER_DESCRIPTION = 'BCM2837 Framebuffer';
 
{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
type
 {RPi3 specific types}
 PRPi3Framebuffer = ^TRPi3Framebuffer;
 TRPi3Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {RPi3 Properties}
  MultiDisplay:LongBool;
  DisplayNum:LongWord;
  DisplaySettings:TDisplaySettings;
 end;  
{$ENDIF} 
{==============================================================================}
var
 {RPi3 specific Ultibo variables}
 RPi3Initialized:Boolean;

 RPi3CNTVOFFLow:LongWord = 0;               {The low 32 bits of the Virtual Counter Offset register at boot time (CPU0 only) (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 RPi3CNTVOFFHigh:LongWord = 0;              {The high 32 bits of the Virtual Counter Offset register at boot time (CPU0 only) (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 
var
 {Timer Variables}
 TimerRegisters:PBCM2837SystemTimerRegisters;
 
var
 {Mailbox Variables}
 Mailbox0Registers:PBCM2837Mailbox0Registers;
 Mailbox1Registers:PBCM2837Mailbox1Registers;
 
var
 {Interrupt Variables}
 InterruptRegisters:PBCM2837InterruptRegisters;
 
 InterruptEntries:array[0..(BCM2837_GPU_IRQ_COUNT + BCM2837_ARM_IRQ_COUNT - 1)] of PInterruptEntry;
 LocalInterruptEntries:array[RPI3_IRQ_LOCAL_START..(BCM2837_IRQ_COUNT - 1),0..(RPI3_CPU_COUNT - 1)] of PInterruptEntry;
 
var
 {System Call Variables}
 SystemCallEntries:array[0..RPI3_SWI_COUNT - 1] of TSystemCallEntry;
 
var
 {IRQ/FIQ Variables}
 IRQEnabled:array[0..2] of LongWord; {3 groups of IRQs to Enable/Disable (See: TBCM2837InterruptRegisters)}
 FIQEnabled:LongWord;                {The single IRQ number to Enable as FIQ instead (See: TBCM2837InterruptRegisters)}
 
 LocalIRQEnabled:array[0..(RPI3_CPU_COUNT - 1)] of LongWord; {1 group of local IRQs to Enable/Disable per CPU (See: TBCM2837ARMLocalRegisters)}
 LocalFIQEnabled:array[0..(RPI3_CPU_COUNT - 1)] of LongWord; {1 group of local FIQs to Enable/Disable per CPU (See: TBCM2837ARMLocalRegisters)}
 
var
 {Watchdog Variables}
 WatchdogRegisters:PBCM2837PMWatchdogRegisters;
 
var
 {ARM Local Variables}
 ARMLocalRegisters:PBCM2837ARMLocalRegisters;
 
var
 {Virtual GPIO Variables}
 VirtualGPIOBuffer:TBCM2837VirtualGPIOBuffer;
 
{==============================================================================}
{Initialization Functions}
procedure RPi3Init;

procedure RPi3SecondarySwitch;
procedure RPi3SecondarySecure;
procedure RPi3SecondaryHandler;

{==============================================================================}
{RPi3 Platform Functions}
procedure RPi3SMPInit;
procedure RPi3BoardInit;
procedure RPi3MemoryInit;
procedure RPi3ClockInit;
procedure RPi3PowerInit;
procedure RPi3MailboxInit;
procedure RPi3InterruptInit;
procedure RPi3PeripheralInit;
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi3FramebufferInit;
{$ENDIF}
procedure RPi3PageTableInit;

procedure RPi3PowerLEDEnable;
procedure RPi3PowerLEDOn;
procedure RPi3PowerLEDOff;

procedure RPi3ActivityLEDEnable;
procedure RPi3ActivityLEDOn;
procedure RPi3ActivityLEDOff;

function RPi3MailboxReceive(Mailbox,Channel:LongWord):LongWord;
procedure RPi3MailboxSend(Mailbox,Channel,Data:LongWord);

function RPi3MailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
function RPi3MailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord;
function RPi3MailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
function RPi3MailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord;

function RPi3MailboxPropertyTag(Tag:LongWord;Data:Pointer;Size:LongWord):LongWord;

function RPi3RequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function RPi3ReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function RPi3RequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function RPi3ReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function RPi3RegisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
function RPi3DeregisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;

function RPi3RegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
function RPi3DeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;

function RPi3GetInterruptEntry(Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
function RPi3GetLocalInterruptEntry(CPUID,Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
function RPi3GetSystemCallEntry(Number:LongWord):TSystemCallEntry; 

function RPi3SystemRestart(Delay:LongWord):LongWord; 
function RPi3SystemShutdown(Delay:LongWord):LongWord;
function RPi3SystemGetCommandLine:String;

function RPi3CPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord; 

function RPi3GPUGetState:LongWord;
function RPi3GPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord; 

function RPi3BoardGetModel:LongWord;
function RPi3BoardGetSerial:Int64;
function RPi3BoardGetRevision:LongWord;
function RPi3BoardGetMACAddress:String;

function RPi3FirmwareGetRevision:LongWord;
function RPi3FirmwareGetThrottled:LongWord;

function RPi3PowerGetWait(PowerId:LongWord):LongWord;
function RPi3PowerGetState(PowerId:LongWord):LongWord;
function RPi3PowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;

function RPi3ClockGetCount:LongWord;
function RPi3ClockGetTotal:Int64; 

function RPi3ClockGetRate(ClockId:LongWord):LongWord;
function RPi3ClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;

function RPi3ClockGetState(ClockId:LongWord):LongWord;
function RPi3ClockSetState(ClockId,State:LongWord):LongWord;

function RPi3ClockGetMinRate(ClockId:LongWord):LongWord;
function RPi3ClockGetMaxRate(ClockId:LongWord):LongWord;

function RPi3TurboGetState(TurboId:LongWord):LongWord;
function RPi3TurboSetState(TurboId,State:LongWord):LongWord;

function RPi3VoltageGetValue(VoltageId:LongWord):LongWord;
function RPi3VoltageSetValue(VoltageId,Value:LongWord):LongWord;

function RPi3VoltageGetMinValue(VoltageId:LongWord):LongWord;
function RPi3VoltageGetMaxValue(VoltageId:LongWord):LongWord;

function RPi3TemperatureGetCurrent(TemperatureId:LongWord):LongWord;
function RPi3TemperatureGetMaximum(TemperatureId:LongWord):LongWord;

function RPi3GPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
function RPi3GPUMemoryRelease(Handle:THandle):LongWord;
function RPi3GPUMemoryLock(Handle:THandle):LongWord;
function RPi3GPUMemoryUnlock(Handle:THandle):LongWord;

function RPi3GPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;

function RPi3DispmanxHandleGet(Resource:THandle):THandle;
function RPi3EDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPi3FramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
function RPi3FramebufferRelease:LongWord;
function RPi3FramebufferSetState(State:LongWord):LongWord;

function RPi3FramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord; 

function RPi3FramebufferGetPhysical(var Width,Height:LongWord):LongWord;
function RPi3FramebufferSetPhysical(var Width,Height:LongWord):LongWord;
function RPi3FramebufferTestPhysical(var Width,Height:LongWord):LongWord;

function RPi3FramebufferGetVirtual(var Width,Height:LongWord):LongWord;
function RPi3FramebufferSetVirtual(var Width,Height:LongWord):LongWord;
function RPi3FramebufferTestVirtual(var Width,Height:LongWord):LongWord;

function RPi3FramebufferGetDepth(var Depth:LongWord):LongWord;
function RPi3FramebufferSetDepth(var Depth:LongWord):LongWord;
function RPi3FramebufferTestDepth(var Depth:LongWord):LongWord;

function RPi3FramebufferGetPixelOrder(var Order:LongWord):LongWord;
function RPi3FramebufferSetPixelOrder(var Order:LongWord):LongWord;
function RPi3FramebufferTestPixelOrder(var Order:LongWord):LongWord;

function RPi3FramebufferGetAlphaMode(var Mode:LongWord):LongWord;
function RPi3FramebufferSetAlphaMode(var Mode:LongWord):LongWord;
function RPi3FramebufferTestAlphaMode(var Mode:LongWord):LongWord;

function RPi3FramebufferGetPitch:LongWord;

function RPi3FramebufferGetOffset(var X,Y:LongWord):LongWord;
function RPi3FramebufferSetOffset(var X,Y:LongWord):LongWord;
function RPi3FramebufferTestOffset(var X,Y:LongWord):LongWord;

function RPi3FramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPi3FramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPi3FramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;

function RPi3FramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
function RPi3FramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
function RPi3FramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPi3FramebufferTestVsync:LongWord;
function RPi3FramebufferSetVsync:LongWord;

function RPi3FramebufferSetBacklight(Brightness:LongWord):LongWord;

function RPi3FramebufferGetNumDisplays(var NumDisplays:LongWord):LongWord;
function RPi3FramebufferGetDisplayId(DisplayNum:LongWord):LongWord;
function RPi3FramebufferSetDisplayNum(DisplayNum:LongWord):LongWord;
function RPi3FramebufferGetDisplaySettings(DisplayNum:LongWord;var DisplaySettings:TDisplaySettings):LongWord;
function RPi3FramebufferDisplayIdToName(DisplayId:LongWord):String;

function RPi3TouchGetBuffer(var Address:PtrUInt):LongWord;
function RPi3TouchSetBuffer(Address:PtrUInt):LongWord;

function RPi3VirtualGPIOGetBuffer(var Address:PtrUInt):LongWord;
function RPi3VirtualGPIOSetBuffer(Address:PtrUInt):LongWord;

function RPi3CursorSetDefault:LongWord;
function RPi3CursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
function RPi3CursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;

function RPi3DMAGetChannels:LongWord;

function RPi3VirtualGPIOInputGet(Pin:LongWord):LongWord; 
function RPi3VirtualGPIOOutputSet(Pin,Level:LongWord):LongWord; 
function RPi3VirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord; 

{==============================================================================}
{RPi3 Thread Functions}
procedure RPi3SchedulerInit;
procedure RPi3SchedulerStart(CPUID:LongWord);

procedure RPi3SecondaryBoot(CPUID:LongWord);

{==============================================================================}
{RPi3 SWI Functions}
function RPi3DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 

{==============================================================================}
{RPi3 Clock Functions}
procedure RPi3ClockInterrupt(Parameter:Pointer);
procedure RPi3ClockUpdate(Cycles:LongWord;var Last:LongWord);

{==============================================================================}
{RPi3 Scheduler Functions}
function RPi3SchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
procedure RPi3SchedulerUpdate(Cycles:LongWord;var Last:LongWord);

procedure RPi3SchedulerSystemCall(Request:PSystemCallRequest);

{==============================================================================}
{RPi3 Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPi3FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPi3FramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPi3FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;

function RPi3FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function RPi3FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;

function RPi3FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;

function RPi3FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{$ENDIF}
{==============================================================================}
{RPi3 Helper Functions}
procedure RPi3Wait;
procedure RPi3LongWait;
procedure RPi3ShortWait;

procedure RPi3SlowBlink;
procedure RPi3FastBlink;

procedure RPi3BootBlink;

procedure RPi3BootOutput(Value:LongWord);
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi3BootConsoleStart;
procedure RPi3BootConsoleWrite(const Value:String);
procedure RPi3BootConsoleWriteEx(const Value:String;X,Y:LongWord);
function RPi3BootConsoleGetX:LongWord;
function RPi3BootConsoleGetY:LongWord;
{$ENDIF}
function RPi3ConvertPowerIdRequest(PowerId:LongWord):LongWord;
function RPi3ConvertPowerStateRequest(PowerState:LongWord):LongWord;
function RPi3ConvertPowerStateResponse(PowerState:LongWord):LongWord;

function RPi3ConvertClockIdRequest(ClockId:LongWord):LongWord;
function RPi3ConvertClockStateRequest(ClockState:LongWord):LongWord;
function RPi3ConvertClockStateResponse(ClockState:LongWord):LongWord;

function RPi3ConvertVoltageIdRequest(VoltageId:LongWord):LongWord;

function RPi3ConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{RPi3 Forward Declarations}
function RPi3InterruptIsValid(Number:LongWord):Boolean; forward;
function RPi3InterruptIsLocal(Number:LongWord):Boolean; forward;
function RPi3InterruptIsGlobal(Number:LongWord):Boolean; forward;

function RPi3InterruptCheckValid(const Entry:TInterruptEntry):Boolean; forward;
function RPi3InterruptCheckHandlers(const Entry:TInterruptEntry):Boolean; forward;
function RPi3InterruptCompareHandlers(const Entry,Current:TInterruptEntry):Boolean; forward;

function RPi3InterruptEnable(const Entry:TInterruptEntry):Boolean; forward;
function RPi3InterruptDisable(const Entry:TInterruptEntry):Boolean; forward;

function RPi3InterruptGetCurrentCount(CPUID,Number:LongWord):LongWord; forward;
function RPi3InterruptGetCurrentEntry(CPUID,Number:LongWord;Index:LongWord):PInterruptEntry; forward;

function RPi3InterruptAddCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;
function RPi3InterruptDeleteCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;

function RPi3InterruptFindMatchingEntry(const Entry:TInterruptEntry):PInterruptEntry; forward;

function RPi3InterruptGetEntry(CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord; forward;
function RPi3InterruptRegisterEntry(const Entry:TInterruptEntry):LongWord; forward;
function RPi3InterruptDeregisterEntry(const Entry:TInterruptEntry):LongWord; forward;

function RPi3DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;
function RPi3DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;

function RPi3HandleInterrupt(Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RPi3Init;
var
 SchedulerFrequency:LongWord;
begin
 {}
 if RPi3Initialized then Exit;

 {Check for Emulator}
 {$IFDEF CPUARM}
 if PLongWord(BCM2837_GPIO_REGS_BASE + BCM2837_GPSET0)^ <> BCM2837_GPIO_SIGNATURE then ARMEmulatorMode:=1;
 {if PBCM2837ARMLocalRegisters(BCM2837_ARM_LOCAL_REGS_BASE).CoreTimerPrescaler = 0 then ARMEmulatorMode:=1;} {Alternate detection option for RPi3}
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 if PLongWord(BCM2837_GPIO_REGS_BASE + BCM2837_GPSET0)^ <> BCM2837_GPIO_SIGNATURE then AARCH64EmulatorMode:=1;
 {if PBCM2837ARMLocalRegisters(BCM2837_ARM_LOCAL_REGS_BASE).CoreTimerPrescaler = 0 then AARCH64EmulatorMode:=1;} {Alternate detection option for RPi3}
 {$ENDIF CPUAARCH64}

 {Setup IO_BASE/IO_ALIAS}
 IO_BASE:=BCM2837_PERIPHERALS_BASE;
 IO_ALIAS:=RPI3_VCIO_ALIAS;

 {Setup BUS_ALIAS}
 BUS_ALIAS:=RPI3_VCBUS_ALIAS;
 
 {Setup SECURE_BOOT}
 SECURE_BOOT:={$IFDEF CPUARM}(ARMSecureBoot <> 0){$ENDIF CPUARM}{$IFDEF CPUAARCH64}(AARCH64SecureBoot <> 0){$ENDIF CPUAARCH64};
 
 {Setup EMULATOR_MODE}
 EMULATOR_MODE:={$IFDEF CPUARM}(ARMEmulatorMode <> 0){$ENDIF CPUARM}{$IFDEF CPUAARCH64}(AARCH64EmulatorMode <> 0){$ENDIF CPUAARCH64};
 
 {Setup STARTUP_ADDRESS}
 STARTUP_ADDRESS:=PtrUInt(@_text_start); {RPI3_STARTUP_ADDRESS} {Obtain from linker}
 
 {Setup PERIPHERALS_BASE and SIZE}
 PERIPHERALS_BASE:=BCM2837_PERIPHERALS_BASE;
 PERIPHERALS_SIZE:=BCM2837_PERIPHERALS_SIZE;

 {Setup LOCAL_PERIPHERALS_BASE and SIZE}
 LOCAL_PERIPHERALS_BASE:=BCM2837_ARM_LOCAL_BASE;
 LOCAL_PERIPHERALS_SIZE:=BCM2837_ARM_LOCAL_SIZE;
 
 {Setup MEMORY_BASE and SIZE}
 {Done by RPi3MemoryInit}
 
 {Setup MEMORY_IRQ/FIQ/LOCAL/SHARED/DEVICE/NOCACHE/NONSHARED_SIZE}
 {Done by RPi3MemoryInit}
 
 {Setup PAGE_TABLE_BASE and SIZE}
 PAGE_TABLE_BASE:=RPI3_PAGE_TABLE_BASE;
 PAGE_TABLE_SIZE:=RPI3_PAGE_TABLE_SIZE;
 
 {Setup VECTOR_TABLE_BASE, SIZE and COUNT}
 VECTOR_TABLE_BASE:=RPI3_VECTOR_TABLE_BASE;
 VECTOR_TABLE_SIZE:=RPI3_VECTOR_TABLE_SIZE;
 VECTOR_TABLE_COUNT:=RPI3_VECTOR_TABLE_COUNT;
 
 {Setup MACHINE_TYPE} 
 MACHINE_TYPE:=MACHINE_TYPE_UNKNOWN;
 {$IFDEF CPUARM}
 case ARMMachineType of
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 case AARCH64MachineType of
 {$ENDIF CPUAARCH64}
  ARM_MACHINE_BCM2710:MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
 end;
 
 {Setup BOARD_TYPE}
 {Done by RPi3BoardInit}
 
 {Setup CPU_ARCH, TYPE and COUNT}
 CPU_ARCH:=CPU_ARCH_ARM32;
 CPU_TYPE:=CPU_TYPE_ARMV8;
 CPU_COUNT:=RPI3_CPU_COUNT;
 CPU_BOOT:=RPI3_CPU_BOOT;
 CPU_MASK:=RPI3_CPU_MASK;
 CPU_MAX_COUNT:=RPI3_CPU_COUNT;
 
 {Setup CPU_MEMORY_BASE and SIZE}
 {Done by RPi3MemoryInit}
 
 {Setup CPU_MEMORY_RESTRICTED}
 CPU_MEMORY_RESTRICTED:=True;
 
 {Setup FPU_TYPE}
 FPU_TYPE:=FPU_TYPE_VFPV3;
 
 {Setup GPU_TYPE}
 GPU_TYPE:=GPU_TYPE_VC4;
 
 {Setup GPU_MEMORY_BASE and SIZE}
 {Done by RPi3MemoryInit}

 {Setup GPU_MEMORY_CACHED}
 GPU_MEMORY_CACHED:=True;
 
 {Setup IRQ/FIQ_COUNT/START/ROUTING}
 IRQ_COUNT:=BCM2837_IRQ_COUNT;
 FIQ_COUNT:=BCM2837_FIQ_COUNT;
 
 IRQ_START:=RPI3_IRQ_START; 

 IRQ_ROUTING:=RPI3_IRQ_ROUTING;
 FIQ_ROUTING:=RPI3_FIQ_ROUTING; 

 IRQ_LOCAL_COUNT:=BCM2837_ARM_LOCAL_IRQ_COUNT;
 FIQ_LOCAL_COUNT:=BCM2837_ARM_LOCAL_IRQ_COUNT;
 
 IRQ_LOCAL_START:=RPI3_IRQ_LOCAL_START;
 
 SWI_COUNT:=RPI3_SWI_COUNT;
 
 {Setup IRQ/FIQ/IPI/SWI/UNDEF/ABORT_ENABLED}
 IRQ_ENABLED:=True;
 FIQ_ENABLED:=True;
 IPI_ENABLED:=False;
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
 {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
 CLOCK_FREQUENCY:=RPI3_GENERIC_TIMER_FREQUENCY;
 if EMULATOR_MODE then CLOCK_FREQUENCY:={$IFDEF CPUARM}ARMv7GetTimerFrequency{$ENDIF CPUARM}{$IFDEF CPUAARCH64}ARMv8GetTimerFrequency{$ENDIF CPUAARCH64};
 {$ELSE}
 CLOCK_FREQUENCY:=BCM2837_SYSTEM_TIMER_FREQUENCY;
 {$ENDIF}
 CLOCK_TICKS_PER_SECOND:=1000;
 CLOCK_TICKS_PER_MILLISECOND:=1;
 CLOCK_CYCLES_PER_TICK:=CLOCK_FREQUENCY div CLOCK_TICKS_PER_SECOND;
 CLOCK_CYCLES_PER_MILLISECOND:=CLOCK_FREQUENCY div MILLISECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_MICROSECOND:=CLOCK_FREQUENCY div MICROSECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_NANOSECOND:=CLOCK_FREQUENCY div NANOSECONDS_PER_SECOND;
 CLOCK_CYCLES_TOLERANCE:=CLOCK_CYCLES_PER_TICK div 10;
 TIME_TICKS_PER_CLOCK_INTERRUPT:=CLOCK_TICKS_PER_MILLISECOND * TIME_TICKS_PER_MILLISECOND;
 
 {Setup HEAP Behaviour}
 HEAP_NORMAL_SHARED:=True;
 
 {Setup SCHEDULER_INTERRUPTS/CLOCKS}
 SCHEDULER_INTERRUPTS_PER_SECOND:=2000;
 SCHEDULER_INTERRUPTS_PER_MILLISECOND:=2;
 SchedulerFrequency:=RPI3_GENERIC_TIMER_FREQUENCY;
 if EMULATOR_MODE then
  begin
   SCHEDULER_INTERRUPTS_PER_SECOND:=1000;   {Note: QEMU uses the timeGetDevCaps() function on Windows which returns wPeriodMin as 1 millisecond}
   SCHEDULER_INTERRUPTS_PER_MILLISECOND:=1; {      That means that any timer interval less then 1ms will not be honoured, the result will be 1ms}
   SchedulerFrequency:={$IFDEF CPUARM}ARMv7GetTimerFrequency{$ENDIF CPUARM}{$IFDEF CPUAARCH64}ARMv8GetTimerFrequency{$ENDIF CPUAARCH64};
  end; 
 SCHEDULER_CLOCKS_PER_INTERRUPT:=SchedulerFrequency div SCHEDULER_INTERRUPTS_PER_SECOND;
 SCHEDULER_CLOCKS_TOLERANCE:=SCHEDULER_CLOCKS_PER_INTERRUPT div 10;
 TIME_TICKS_PER_SCHEDULER_INTERRUPT:=SCHEDULER_INTERRUPTS_PER_MILLISECOND * TIME_TICKS_PER_MILLISECOND;
 
 {Setup SCHEDULER_IDLE}
 SCHEDULER_IDLE_WAIT:=True;
 SCHEDULER_IDLE_OFFSET:=1;
 SCHEDULER_IDLE_PER_SECOND:=SCHEDULER_INTERRUPTS_PER_SECOND;
 
 {Setup KERNEL_NAME/CONFIG/COMMAND}
 KERNEL_NAME:=RPI3_KERNEL_NAME;
 KERNEL_CONFIG:=RPI3_KERNEL_CONFIG;
 KERNEL_COMMAND:=RPI3_KERNEL_COMMAND;
 FIRMWARE_FILES:=RPI3_FIRMWARE_FILES;

 {Setup GPIO (Set early to support activity LED)}
 GPIO_REGS_BASE:=BCM2837_GPIO_REGS_BASE;

 {Check for Emulator}
 if EMULATOR_MODE then
  begin
   {QEMU DMA device is very slow}
   CONSOLE_DMA_BOX:=False; 
   CONSOLE_DMA_LINE:=False; 
   CONSOLE_DMA_FILL:=False;
   CONSOLE_DMA_CLEAR:=False; 
   CONSOLE_DMA_SCROLL:=False; 
   
   {Framebuffer has no default settings}
   FRAMEBUFFER_DEFAULT_WIDTH:=800;
   FRAMEBUFFER_DEFAULT_HEIGHT:=600;
  end; 

 {Register Platform SMPInit Handler}
 SMPInitHandler:=RPi3SMPInit;
 
 {Register Platform BoardInit Handler}
 BoardInitHandler:=RPi3BoardInit;
 
 {Register Platform MemoryInit Handler}
 MemoryInitHandler:=RPi3MemoryInit;
 
 {Register Platform ClockInit Handler}
 ClockInitHandler:=RPi3ClockInit;
 
 {Register Platform PowerInit Handler}
 PowerInitHandler:=RPi3PowerInit;
 
 {Register Platform MailboxInit Handler}
 MailboxInitHandler:=RPi3MailboxInit;
 
 {Register Platform InterruptInit Handler}
 InterruptInitHandler:=RPi3InterruptInit;
 
 {Register Platform PeripheralInit Handler}
 PeripheralInitHandler:=RPi3PeripheralInit;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Framebuffer FramebufferInit Handler}
 FramebufferInitHandler:=RPi3FramebufferInit;
 {$ENDIF}
 
 {$IFDEF CPUARM}
 {Register PlatformARMv7 PageTableInit Handler}
 ARMv7PageTableInitHandler:=RPi3PageTableInit;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformARMv8 PageTableInit Handler}
 ARMv8PageTableInitHandler:=RPi3PageTableInit;
 {$ENDIF CPUAARCH64}

 {Register Platform Boot Blink Handlers}
 BootBlinkHandler:=RPi3BootBlink;
 BootOutputHandler:=RPi3BootOutput;

 {Register Platform Boot Console Handlers}
 {$IFDEF CONSOLE_EARLY_INIT}
 BootConsoleStartHandler:=RPi3BootConsoleStart;
 BootConsoleWriteHandler:=RPi3BootConsoleWrite;
 BootConsoleWriteExHandler:=RPi3BootConsoleWriteEx;
 BootConsoleGetXHandler:=RPi3BootConsoleGetX;
 BootConsoleGetYHandler:=RPi3BootConsoleGetY;
 {$ENDIF}
 
 {Register Platform LED Handlers}
 PowerLEDEnableHandler:=RPi3PowerLEDEnable;
 PowerLEDOnHandler:=RPi3PowerLEDOn;
 PowerLEDOffHandler:=RPi3PowerLEDOff;
 ActivityLEDEnableHandler:=RPi3ActivityLEDEnable;
 ActivityLEDOnHandler:=RPi3ActivityLEDOn;
 ActivityLEDOffHandler:=RPi3ActivityLEDOff;
 
 {Register Platform Mailbox Handlers}
 MailboxReceiveHandler:=RPi3MailboxReceive;
 MailboxSendHandler:=RPi3MailboxSend;
 MailboxCallHandler:=RPi3MailboxCall;
 MailboxCallExHandler:=RPi3MailboxCallEx;
 MailboxPropertyCallHandler:=RPi3MailboxPropertyCall;
 MailboxPropertyCallExHandler:=RPi3MailboxPropertyCallEx;
 MailboxPropertyTagHandler:=RPi3MailboxPropertyTag;

 {Register Platform IRQ Handlers}
 RequestExIRQHandler:=RPi3RequestExIRQ;
 ReleaseExIRQHandler:=RPi3ReleaseExIRQ;

 {Register Platform FIQ Handlers}
 RequestExFIQHandler:=RPi3RequestExFIQ;
 ReleaseExFIQHandler:=RPi3ReleaseExFIQ;

 {Register Platform Interrupt Handlers}
 RegisterInterruptHandler:=RPi3RegisterInterrupt;
 DeregisterInterruptHandler:=RPi3DeregisterInterrupt;

 {Register Platform System Call Handlers}
 RegisterSystemCallExHandler:=RPi3RegisterSystemCallEx;
 DeregisterSystemCallExHandler:=RPi3DeregisterSystemCallEx;

 {Register Platform Interrupt Handlers}
 GetInterruptEntryHandler:=RPi3GetInterruptEntry;
 
 {Register Platform Local Interrupt Handlers}
 GetLocalInterruptEntryHandler:=RPi3GetLocalInterruptEntry;
 
 {Register Platform System Call Handlers}
 GetSystemCallEntryHandler:=RPi3GetSystemCallEntry;
 
 {Register Platform System Handlers}
 SystemRestartHandler:=RPi3SystemRestart;
 SystemShutdownHandler:=RPi3SystemShutdown;
 SystemGetCommandLineHandler:=RPi3SystemGetCommandLine;

 {Register Platform CPU Handlers}
 CPUGetMemoryHandler:=RPi3CPUGetMemory;

 {Register Platform GPU Handlers}
 GPUGetStateHandler:=RPi3GPUGetState;
 GPUGetMemoryHandler:=RPi3GPUGetMemory;

 {Register Platform Board Handlers}
 BoardGetModelHandler:=RPi3BoardGetModel;
 BoardGetSerialHandler:=RPi3BoardGetSerial;
 BoardGetRevisionHandler:=RPi3BoardGetRevision;
 BoardGetMACAddressHandler:=RPi3BoardGetMACAddress;

 {Register Platform Firmware Handlers}
 FirmwareGetRevisionHandler:=RPi3FirmwareGetRevision;
 FirmwareGetThrottledHandler:=RPi3FirmwareGetThrottled;

 {Register Platform Power Handlers}
 PowerGetWaitHandler:=RPi3PowerGetWait;
 PowerGetStateHandler:=RPi3PowerGetState;
 PowerSetStateHandler:=RPi3PowerSetState;

 {Register Platform Clock Handlers}
 ClockGetCountHandler:=RPi3ClockGetCount;
 ClockGetTotalHandler:=RPi3ClockGetTotal;

 ClockGetRateHandler:=RPi3ClockGetRate;
 ClockSetRateHandler:=RPi3ClockSetRate;

 ClockGetStateHandler:=RPi3ClockGetState;
 ClockSetStateHandler:=RPi3ClockSetState;

 ClockGetMinRateHandler:=RPi3ClockGetMinRate;
 ClockGetMaxRateHandler:=RPi3ClockGetMaxRate;

 {Register Platform Turbo Handlers}
 TurboGetStateHandler:=RPi3TurboGetState;
 TurboSetStateHandler:=RPi3TurboSetState;

 {Register Platform Voltage Handlers}
 VoltageGetValueHandler:=RPi3VoltageGetValue;
 VoltageSetValueHandler:=RPi3VoltageSetValue;
 VoltageGetMinValueHandler:=RPi3VoltageGetMinValue;
 VoltageGetMaxValueHandler:=RPi3VoltageGetMaxValue;
 
 {Register Platform Temperature Handlers}
 TemperatureGetCurrentHandler:=RPi3TemperatureGetCurrent;
 TemperatureGetMaximumHandler:=RPi3TemperatureGetMaximum;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPi3GPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPi3GPUMemoryRelease;
 GPUMemoryLockHandler:=RPi3GPUMemoryLock;
 GPUMemoryUnlockHandler:=RPi3GPUMemoryUnlock;
 
 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPi3GPUExecuteCode;
 DispmanxHandleGetHandler:=RPi3DispmanxHandleGet;
 EDIDBlockGetHandler:=RPi3EDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPi3FramebufferAllocate;
 FramebufferReleaseHandler:=RPi3FramebufferRelease;
 FramebufferSetStateHandler:=RPi3FramebufferSetState;

 FramebufferGetDimensionsHandler:=RPi3FramebufferGetDimensions;
 
 FramebufferGetPhysicalHandler:=RPi3FramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPi3FramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPi3FramebufferTestPhysical;
 
 FramebufferGetVirtualHandler:=RPi3FramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPi3FramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPi3FramebufferTestVirtual;
 
 FramebufferGetDepthHandler:=RPi3FramebufferGetDepth;
 FramebufferSetDepthHandler:=RPi3FramebufferSetDepth;
 FramebufferTestDepthHandler:=RPi3FramebufferTestDepth;
 
 FramebufferGetPixelOrderHandler:=RPi3FramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPi3FramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPi3FramebufferTestPixelOrder;
 
 FramebufferGetAlphaModeHandler:=RPi3FramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPi3FramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPi3FramebufferTestAlphaMode;
 
 FramebufferGetPitchHandler:=RPi3FramebufferGetPitch;
 
 FramebufferGetOffsetHandler:=RPi3FramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPi3FramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPi3FramebufferTestOffset;
 
 FramebufferGetOverscanHandler:=RPi3FramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPi3FramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPi3FramebufferTestOverscan;
 
 FramebufferGetPaletteHandler:=RPi3FramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPi3FramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPi3FramebufferTestPalette;

 FramebufferTestVsyncHandler:=RPi3FramebufferTestVsync;
 FramebufferSetVsyncHandler:=RPi3FramebufferSetVsync;
 
 FramebufferSetBacklightHandler:=RPi3FramebufferSetBacklight;
 
 FramebufferGetNumDisplaysHandler:=RPi3FramebufferGetNumDisplays;
 FramebufferGetDisplayIdHandler:=RPi3FramebufferGetDisplayId;
 FramebufferSetDisplayNumHandler:=RPi3FramebufferSetDisplayNum;
 FramebufferGetDisplaySettingsHandler:=RPi3FramebufferGetDisplaySettings;
 FramebufferDisplayIdToNameHandler:=RPi3FramebufferDisplayIdToName;
 
 {Register Platform Touch Handlers}
 TouchGetBufferHandler:=RPi3TouchGetBuffer;
 TouchSetBufferHandler:=RPi3TouchSetBuffer;
 
 {Register Platform Cursor Handlers}
 CursorSetDefaultHandler:=RPi3CursorSetDefault;
 CursorSetInfoHandler:=RPi3CursorSetInfo;
 CursorSetStateHandler:=RPi3CursorSetState;
 {$ENDIF}
 {Register Platform DMA Handlers}
 DMAGetChannelsHandler:=RPi3DMAGetChannels;
 
 {Register Platform Virtual GPIO Handlers}
 VirtualGPIOInputGetHandler:=RPi3VirtualGPIOInputGet;
 VirtualGPIOOutputSetHandler:=RPi3VirtualGPIOOutputSet;
 VirtualGPIOFunctionSelectHandler:=RPi3VirtualGPIOFunctionSelect;
 
 {Register Threads SchedulerInit Handler}
 SchedulerInitHandler:=RPi3SchedulerInit;
 SchedulerStartHandler:=RPi3SchedulerStart;
 
 {Register Threads SecondaryBoot Handler}
 SecondaryBootHandler:=RPi3SecondaryBoot;
 
 {$IFDEF CPUARM}
 {Register PlatformARMv7 IRQ Handlers}
 ARMv7DispatchIRQHandler:=RPi3DispatchIRQ;

 {Register PlatformARMv7 FIQ Handlers}
 ARMv7DispatchFIQHandler:=RPi3DispatchFIQ;
 
 {Register PlatformARMv7 SWI Handlers}
 ARMv7DispatchSWIHandler:=RPi3DispatchSWI;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformARMv8 IRQ Handlers}
 ARMv8DispatchIRQHandler:=RPi3DispatchIRQ;

 {Register PlatformARMv8 FIQ Handlers}
 ARMv8DispatchFIQHandler:=RPi3DispatchFIQ;
 
 {Register PlatformARMv8 SWI Handlers}
 ARMv8DispatchSWIHandler:=RPi3DispatchSWI;
 {$ENDIF CPUAARCH64}
 
 {$IFDEF CPUARM}
 {Register PlatformARM Helper Handlers}
 ARMWaitHandler:=RPi3Wait;
 ARMLongWaitHandler:=RPi3LongWait;
 ARMShortWaitHandler:=RPi3ShortWait;
 ARMSlowBlinkHandler:=RPi3SlowBlink;
 ARMFastBlinkHandler:=RPi3FastBlink;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformAARCH64 Helper Handlers}
 AARCH64WaitHandler:=RPi3Wait;
 AARCH64LongWaitHandler:=RPi3LongWait;
 AARCH64ShortWaitHandler:=RPi3ShortWait;
 AARCH64SlowBlinkHandler:=RPi3SlowBlink;
 AARCH64FastBlinkHandler:=RPi3FastBlink;
 {$ENDIF CPUAARCH64}
 
 RPi3Initialized:=True;
end;

{==============================================================================}

procedure RPi3SecondarySwitch; assembler; nostackframe; 
{Secondary CPU switch from HYP mode handler}
{$IFDEF CPUARM}
asm
 //Get the CPSR
 mrs r0, cpsr  
 //Test for HYP mode
 eor r0, r0, #ARM_MODE_HYP                           
 tst r0, #ARM_MODE_BITS
 //Clear the mode bits
 bic r0, r0, #ARM_MODE_BITS		
 //Mask IRQ/FIQ bits and set SVC mode 
 orr r0, r0, #ARM_I_BIT | ARM_F_BIT | ARM_MODE_SVC
 
 //Return if not in HYP mode 
 bne .LNoSwitch             

 //Reset CNTVOFF to 0 while in HYP mode
 mov r1, #0
 mcrr p15, #4, r1, r1, cr14
 
 //Mask the Abort bit 
 orr r0, r0, #ARM_A_BIT                              
 //Load the SPSR
 msr spsr_cxsf, r0
 //Return to SVC mode
.long 0xE12EF30E  //msr ELR_hyp, lr  (Not supported by the FPC compiler)
.long 0xE160006E  //eret             (Not supported by the FPC compiler)

.LNoSwitch:
 //Set the CPSR (C fields)
 msr cpsr_c, r0 
 //Return to startup 
 bx lr
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi3SecondarySecure; assembler; nostackframe; 
{Secondary CPU switch to secure mode handler}
{$IFDEF CPUARM}
asm
 //Check the secure boot configuration
 ldr r0, .LARMSecureBoot
 ldr r0, [r0]
 cmp r0, #0
 beq .LNoSecure
  
 //Attempt to switch back to secure world by performing a
 //secure monitor call to the Secure Monitor handler.
 //No need to copy the secure vectors again
 
 //Clean Data Cache MVA
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #1
 
 //Perform a data synchronisation barrier
 dsb
 
 //Invalidate Instruction Cache
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #0

 //Flush Branch Target Cache 
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #6

 //Perform a data synchronisation barrier
 dsb
 
 //Perform an instruction synchronisation barrier
 isb
 
 //Perform a secure monitor call (Not supported by the FPC compiler)
 .long 0xE1600070  //smc #0
  
.LNoSecure:
 //Return to startup
 bx lr
 
.LARMSecureBoot:
  .long ARMSecureBoot
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi3SecondaryHandler; assembler; nostackframe; 
{Secondary CPU startup handler routine}
{$IFDEF CPUARM}
asm
 //Call the HYP mode switch handler in case the CPU is in HYP mode
 bl RPi3SecondarySwitch
 
 //Call the secure mode switch handler to return to secure mode
 bl RPi3SecondarySecure
 
 //Invalidate Instruction Cache before starting the boot process
 bl ARMv7InvalidateInstructionCache
 
 //Invalidate L1 Data Cache before starting the boot process
 bl ARMv7InvalidateL1DataCache
 
 //Invalidate the TLB before starting the boot process
 bl ARMv7InvalidateTLB
 
 //Change to SYS mode and ensure all interrupts are disabled
 //so the ARM processor is in a known state.
 cpsid if, #ARM_MODE_SYS
 
 //Set the Vector Base Address register in the System Control
 //register to the address of the vector table base above.
 mov r0, #RPI3_VECTOR_TABLE_BASE
 mcr p15, #0, r0, cr12, cr0, #0
 
 //Enable Unaligned Memory Accesses (U Bit) in the System Control
 //Register to simplify memory access routines from Pascal code.
 //
 //This would normally occur in CPUInit but is done here to allow
 //calls to Pascal code during initialization. (Always enabled in ARMv7)
 //mrc p15, #0, r0, cr1, cr0, #0
 //orr r0, #ARMV7_CP15_C1_U_BIT
 //mcr p15, #0, r0, cr1, cr0, #0
 
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r1, cr0, cr0, #5;
 //Mask off the CPUID value
 and r1, #ARMV7_CP15_C0_MPID_CPUID_MASK
 //Multiply by 4 to get the offset in the array
 lsl r1, #2
 
 //Get the Boot stack base
 ldr r0, .LBOOT_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the Boot stack pointer
 mov sp, r0
 
 //Change to IRQ mode
 cpsid if, #ARM_MODE_IRQ
 //Get the IRQ mode stack base
 ldr r0, .LIRQ_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the IRQ mode stack pointer
 mov sp, r0
 
 //Change to FIQ mode
 cpsid if, #ARM_MODE_FIQ
 //Get the FIQ mode stack base
 ldr r0, .LFIQ_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the FIQ mode stack pointer
 mov sp, r0

 //Change to SWI mode
 cpsid if, #ARM_MODE_SVC
 //Get the SWI mode stack base
 ldr r0, .LSWI_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the SWI mode stack pointer
 mov sp, r0

 //Change to ABORT mode
 cpsid if, #ARM_MODE_ABT
 //Get the ABORT mode stack base
 ldr r0, .LABORT_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the ABORT mode stack pointer
 mov sp, r0
 
 //Change to UNDEFINED mode
 cpsid if, #ARM_MODE_UND
 //Get the UNDEFINED mode stack base
 ldr r0, .LUNDEFINED_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the UNDEFINED mode stack pointer
 mov sp, r0
 
 //Return to SYS mode (Note: Interrupts remain disabled during initialization)
 cpsid if, #ARM_MODE_SYS
 
 //Initialize the CPU
 bl ARMv7CPUInit
 
 //Initialize the FPU
 bl ARMv7FPUInit
 
 //Start the MMU
 bl ARMv7StartMMU
 
 //Initialize the Caches
 bl ARMv7CacheInit
 
 //Check the secure boot configuration
 ldr r0, .LARMSecureBoot
 ldr r0, [r0]
 cmp r0, #0
 beq .LNoTimer
 
 //Set the ARM Generic Timer Frequency
 ldr r0, =RPI3_GENERIC_TIMER_FREQUENCY
 bl ARMv7TimerInit
 
.LNoTimer:
 
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15 CP15
 mrc p15, #0, r1, cr0, cr0, #5;
 //Mask off the CPUID value
 and r1, #ARMV7_CP15_C0_MPID_CPUID_MASK
 //Multiply by 4 to get the offset in the array
 lsl r1, #2
 
 //Get the Boot thread handle
 ldr r0, .LBOOT_THREAD_HANDLE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 
 //Set the current thread id in c13 (Thread and process ID) register of system control coprocessor CP15
 mcr p15, #0, r0, cr13, cr0, #4
 
 //Get the Boot stack size
 ldr r0, .LBOOT_STACK_SIZE
 ldr r0, [r0]
 
 //Setup Boot Thread Tls Memory (Stack size in R0)
 bl InitThread
 
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5;
 //Mask off the CPUID value
 and r0, #ARMV7_CP15_C0_MPID_CPUID_MASK

 //Branch to the CPU Start function (Current CPU in R0)
 bl SecondaryStart
 
 //If startup fails halt the CPU
 b ARMv7Halt
 
.LBOOT_STACK_SIZE:
  .long BOOT_STACK_SIZE 
.LBOOT_STACK_BASE:
  .long BOOT_STACK_BASE  
.LBOOT_THREAD_HANDLE:
  .long BOOT_THREAD_HANDLE  
  
.LIRQ_STACK_BASE:
  .long IRQ_STACK_BASE  
.LFIQ_STACK_BASE:
  .long FIQ_STACK_BASE  
.LSWI_STACK_BASE:
  .long SWI_STACK_BASE  
.LABORT_STACK_BASE:
  .long ABORT_STACK_BASE  
.LUNDEFINED_STACK_BASE:
  .long UNDEFINED_STACK_BASE  
  
.LARMSecureBoot:
  .long ARMSecureBoot
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}
{==============================================================================}
{RPi3 Platform Functions}
procedure RPi3SMPInit;
var
 Control:LongWord;
 GPUInterruptRouting:LongWord;
begin
 {}
 {Setup ARM Local Registers}
 ARMLocalRegisters:=PBCM2837ARMLocalRegisters(BCM2837_ARM_LOCAL_REGS_BASE);

 {Setup Core Timer Clock}
 Control:=ARMLocalRegisters.Control;
 Control:=Control and not(BCM2837_ARM_LOCAL_CONTROL_APB_CLOCK or BCM2837_ARM_LOCAL_CONTROL_INCREMENT_2); {Disable APB Clock and Increment 2}
 Control:=Control or BCM2837_ARM_LOCAL_CONTROL_CRYSTAL_CLOCK or BCM2837_ARM_LOCAL_CONTROL_INCREMENT_1;   {Enable Crystal Clock and Increment 1}
 ARMLocalRegisters.Control:=Control;
 
 {Setup Core Timer Prescaler}
 ARMLocalRegisters.CoreTimerPrescaler:=RPI3_CORE_TIMER_PRESCALER;
 
 {Setup GPU IRQ/FIQ Routing}
 GPUInterruptRouting:=ARMLocalRegisters.GPUInterruptRouting;
 GPUInterruptRouting:=GPUInterruptRouting and not(BCM2837_ARM_LOCAL_GPU_INT_ROUTING_IRQ_MASK or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_FIQ_MASK); {Clear all routing}
 case IRQ_ROUTING of {Setup IRQ Routing}
  CPU_ID_0:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_IRQ0;
  CPU_ID_1:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_IRQ1;
  CPU_ID_2:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_IRQ2;
  CPU_ID_3:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_IRQ3;
 end;
 case FIQ_ROUTING of {Setup FIQ Routing}
  CPU_ID_0:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_FIQ0;
  CPU_ID_1:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_FIQ1;
  CPU_ID_2:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_FIQ2;
  CPU_ID_3:GPUInterruptRouting:=GPUInterruptRouting or BCM2837_ARM_LOCAL_GPU_INT_ROUTING_FIQ3;
 end;
 ARMLocalRegisters.GPUInterruptRouting:=GPUInterruptRouting;
 
 {Setup ARM Generic Timer}
 {$IFDEF CPUARM}
 if SECURE_BOOT then ARMv7TimerInit(RPI3_GENERIC_TIMER_FREQUENCY);
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 if SECURE_BOOT then ARMv8TimerInit(RPI3_GENERIC_TIMER_FREQUENCY);
 {$ENDIF CPUAARCH64}
end;

{==============================================================================}

procedure RPi3BoardInit;
var
 Revision:LongWord;
 ClockRateMax:LongWord;
begin
 {}
 {$IFDEF RPI3_CLOCK_SYSTEM_TIMER}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then InterruptInit;
 
 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then ClockInit;
 {$ENDIF}
 
 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then MailboxInit;
 
 {Get Board Revision}
 Revision:=RPi3BoardGetRevision;
 
 {Get Board Type}
 if (Revision and BCM2837_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   case (Revision and BCM2837_BOARD_REVISION_MODEL_MASK) of
    BCM2837_BOARD_REVISION_MODEL_2B:begin
      BOARD_TYPE:=BOARD_TYPE_RPI2B;
      
      {Check for 2B Revision 1 (BCM2836 with Cortex A7)}
      case (Revision and BCM2837_BOARD_REVISION_PROCESSOR_MASK) of 
       BCM2837_BOARD_REVISION_PROCESSOR_BCM2836:begin
         {Adjust CPU Type}
         CPU_TYPE:=CPU_TYPE_ARMV7;
       
         {Adjust Machine Type}
         MACHINE_TYPE:=MACHINE_TYPE_BCM2709;
        end;
      end;  
     end;
    BCM2837_BOARD_REVISION_MODEL_3B:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3B;
     end;
    BCM2837_BOARD_REVISION_MODEL_3BPLUS:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3B_PLUS;
     end;
    BCM2837_BOARD_REVISION_MODEL_3APLUS:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3A_PLUS;
     end;
    BCM2837_BOARD_REVISION_MODEL_COMPUTE3:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE3;
     end;
    BCM2837_BOARD_REVISION_MODEL_COMPUTE3PLUS:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE3_PLUS;
     end;
   end;
  end
 else
  begin 
   {Old Style Revision}
   case (Revision and BCM2837_BOARD_REV_MASK) of
    BCM2837_BOARD_REV_3B_1,BCM2837_BOARD_REV_3B_2,BCM2837_BOARD_REV_3B_3:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3B;
     end; 
    BCM2837_BOARD_REV_CM3_1,BCM2837_BOARD_REV_CM3_2:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE3;
     end; 
   end;
  end; 
 
 {Get CPU Clock Maximum}
 ClockRateMax:=RPi3ClockGetMaxRate(CLOCK_ID_CPU);
 if ClockRateMax > 0 then
  begin
   {Set CPU Clock}
   RPi3ClockSetRate(CLOCK_ID_CPU,ClockRateMax,True); 
  end;
  
 {Note: As of firmware dated 19 March 2021 the clock rates reported by the
        firmware may not be accurate unless the ARM has set the rate first} 
 {Get Core Clock Maximum}
 ClockRateMax:=RPi3ClockGetMaxRate(CLOCK_ID_CORE);
 if ClockRateMax > 0 then
  begin
   {Set Core Clock}
   RPi3ClockSetRate(CLOCK_ID_CORE,ClockRateMax,True); 
  end;
  
 {Get V3D Clock Maximum}
 ClockRateMax:=RPi3ClockGetMaxRate(CLOCK_ID_V3D);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPi3ClockSetRate(CLOCK_ID_V3D,ClockRateMax,True); 
  end;
  
 {Get H264 Clock Maximum}
 ClockRateMax:=RPi3ClockGetMaxRate(CLOCK_ID_H264);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPi3ClockSetRate(CLOCK_ID_H264,ClockRateMax,True); 
  end;
  
 {Get ISP Clock Maximum}
 ClockRateMax:=RPi3ClockGetMaxRate(CLOCK_ID_ISP);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPi3ClockSetRate(CLOCK_ID_ISP,ClockRateMax,True); 
  end;
  
 {Get SDRAM Clock Maximum}
 ClockRateMax:=RPi3ClockGetMaxRate(CLOCK_ID_SDRAM);
 if ClockRateMax > 0 then
  begin
   {Set SDRAM Clock}
   RPi3ClockSetRate(CLOCK_ID_SDRAM,ClockRateMax,True); 
  end;
end;

{==============================================================================}

procedure RPi3MemoryInit;
var
 Address:PtrUInt;
 Length:UInt64;
 Revision:LongWord;
begin
 {}
 {$IFDEF RPI3_CLOCK_SYSTEM_TIMER}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then InterruptInit;
 
 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then ClockInit;
 {$ENDIF}
 
 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then MailboxInit;
 
 {Get Board Revision}
 Revision:=RPi3BoardGetRevision;
 
 {Check Board Revision}
 if (Revision and BCM2837_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   case (Revision and BCM2837_BOARD_REVISION_MODEL_MASK) of
    BCM2837_BOARD_REVISION_MODEL_2B,
    BCM2837_BOARD_REVISION_MODEL_3B,
    BCM2837_BOARD_REVISION_MODEL_3BPLUS,
    BCM2837_BOARD_REVISION_MODEL_COMPUTE3,
    BCM2837_BOARD_REVISION_MODEL_COMPUTE3PLUS:begin
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_1G;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
      {Get Memory Section Size}
      MEMORY_SECTION_SIZE:=SIZE_1M;
      {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
      MEMORY_IRQ_SIZE:=SIZE_8M;
      MEMORY_FIQ_SIZE:=SIZE_8M;
      MEMORY_LOCAL_SIZE:=SIZE_8M;
      MEMORY_SHARED_SIZE:=SIZE_32M;
      MEMORY_DEVICE_SIZE:=SIZE_0; {was SIZE_8M}
      MEMORY_NOCACHE_SIZE:=SIZE_16M;
      MEMORY_NONSHARED_SIZE:=SIZE_8M;
     end;
    BCM2837_BOARD_REVISION_MODEL_3APLUS:begin
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_512M;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
      {Get Memory Section Size}
      MEMORY_SECTION_SIZE:=SIZE_1M;
      {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
      MEMORY_IRQ_SIZE:=SIZE_8M;
      MEMORY_FIQ_SIZE:=SIZE_8M;
      MEMORY_LOCAL_SIZE:=SIZE_8M;
      MEMORY_SHARED_SIZE:=SIZE_32M;
      MEMORY_DEVICE_SIZE:=SIZE_0; {was SIZE_8M}
      MEMORY_NOCACHE_SIZE:=SIZE_16M;
      MEMORY_NONSHARED_SIZE:=SIZE_8M;
     end;    
   end;
  end
 else
  begin 
   {Old Style Revision}
   case (Revision and BCM2837_BOARD_REV_MASK) of
    BCM2837_BOARD_REV_3B_1,BCM2837_BOARD_REV_3B_2,BCM2837_BOARD_REV_3B_3,
    BCM2837_BOARD_REV_CM3_1,BCM2837_BOARD_REV_CM3_2:begin 
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_1G;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
      {Get Memory Section Size}
      MEMORY_SECTION_SIZE:=SIZE_1M;
      {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
      MEMORY_IRQ_SIZE:=SIZE_8M;
      MEMORY_FIQ_SIZE:=SIZE_8M;
      MEMORY_LOCAL_SIZE:=SIZE_8M;
      MEMORY_SHARED_SIZE:=SIZE_32M;
      MEMORY_DEVICE_SIZE:=SIZE_0; {was SIZE_8M}
      MEMORY_NOCACHE_SIZE:=SIZE_16M;
      MEMORY_NONSHARED_SIZE:=SIZE_8M;
     end;
   end; 
  end;
 
 {Get CPU Memory}
 if RPi3CPUGetMemory(Address,Length) = ERROR_SUCCESS then
  begin
   CPU_MEMORY_BASE:=Address;
   CPU_MEMORY_SIZE:=Length;
   
   {Handle 256MB or less Memory (Missing fixup.dat)}
   if CPU_MEMORY_SIZE < SIZE_256M then
    begin
     {Get Memory Base/Size (Assume 256MB default)}
     MEMORY_BASE:=$00000000;
     MEMORY_SIZE:=SIZE_256M;
     {Get Memory Page Size}
     MEMORY_PAGE_SIZE:=SIZE_4K;
     MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
     {Get Memory Section Size}
     MEMORY_SECTION_SIZE:=SIZE_1M;
     {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
     MEMORY_IRQ_SIZE:=SIZE_2M;
     MEMORY_FIQ_SIZE:=SIZE_2M;
     MEMORY_LOCAL_SIZE:=SIZE_2M;
     MEMORY_SHARED_SIZE:=SIZE_8M;
     MEMORY_DEVICE_SIZE:=SIZE_0;
     MEMORY_NOCACHE_SIZE:=SIZE_4M;
     MEMORY_NONSHARED_SIZE:=SIZE_2M;
    end;
  end;
  
 {Get GPU Memory}
 if RPi3GPUGetMemory(Address,Length) = ERROR_SUCCESS then
  begin
   GPU_MEMORY_BASE:=Address;
   GPU_MEMORY_SIZE:=Length;
  end;
end;

{==============================================================================}

procedure RPi3ClockInit;
{$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
var
 State:LongWord;
{$ENDIF}
begin
 {}
 {Setup Timer Registers}
 TimerRegisters:=PBCM2837SystemTimerRegisters(BCM2837_SYSTEM_TIMER_REGS_BASE);
 
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
   {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
   RequestFIQ(RPI3_CPU_BOOT,BCM2837_IRQ_LOCAL_ARM_CNTVIRQ,RPi3ClockInterrupt,nil); 
   {$ELSE}
   RequestFIQ(RPI3_CPU_BOOT,BCM2837_IRQ_SYSTEM_TIMER_3,RPi3ClockInterrupt,nil); 
   {$ENDIF}
  end
 else
  begin
   {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
   RequestIRQ(RPI3_CPU_BOOT,BCM2837_IRQ_LOCAL_ARM_CNTVIRQ,RPi3ClockInterrupt,nil); 
   {$ELSE}
   RequestIRQ(RPI3_CPU_BOOT,BCM2837_IRQ_SYSTEM_TIMER_3,RPi3ClockInterrupt,nil); 
   {$ENDIF}
  end;

 {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
 {Setup the Generic Timer}
 {$IFDEF CPUARM}
 State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTV);
 State:=State and not(ARMV7_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV7_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv7SetTimerState(ARMV7_CP15_C14_CNTV,State); 
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 State:=ARMv8GetTimerState(ARMV8_CP15_C14_CNTV);
 State:=State and not(ARMV8_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV8_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv8SetTimerState(ARMV8_CP15_C14_CNTV,State); 
 {$ENDIF CPUAARCH64}
 {$ENDIF}
 
 {Setup the first Clock Interrupt}
 RPi3ClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
end;

{==============================================================================}

procedure RPi3PowerInit;
begin
 {}
 {Setup Watchdog Registers}
 WatchdogRegisters:=PBCM2837PMWatchdogRegisters(BCM2837_PM_REGS_BASE);
end;

{==============================================================================}

procedure RPi3MailboxInit;
begin
 {}
 {Setup Mailbox0/1 Registers}
 Mailbox0Registers:=PBCM2837Mailbox0Registers(BCM2837_MAILBOX0_REGS_BASE);
 Mailbox1Registers:=PBCM2837Mailbox1Registers(BCM2837_MAILBOX1_REGS_BASE);
end;

{==============================================================================}

procedure RPi3InterruptInit;
var
 Count:LongWord;
 Counter:LongWord;
begin
 {}
 {Setup Interrupt Registers}
 InterruptRegisters:=PBCM2837InterruptRegisters(BCM2837_INTERRUPT_REGS_BASE);
 
 {Setup Interrupt Entries}
 for Count:=0 to BCM2837_GPU_IRQ_COUNT + BCM2837_ARM_IRQ_COUNT - 1 do
  begin
   InterruptEntries[Count]:=nil;
  end; 
 
 {Setup Local Interrupt Entries}
 for Count:=RPI3_IRQ_LOCAL_START to BCM2837_IRQ_COUNT - 1 do
  begin
   for Counter:=0 to RPI3_CPU_COUNT - 1 do
    begin
     LocalInterruptEntries[Count,Counter]:=nil;
    end; 
  end; 
 
 {Setup System Call Entries}
 for Count:=0 to RPI3_SWI_COUNT - 1 do
  begin
   FillChar(SystemCallEntries[Count],SizeOf(TSystemCallEntry),0);
   
   SystemCallEntries[Count].Number:=Count; 
   SystemCallEntries[Count].CPUID:=CPU_ID_ALL;
  end;
  
 {Setup Enabled IRQs}
 for Count:=0 to 2 do {Number of elements in IRQEnabled}
  begin
   IRQEnabled[Count]:=0;
  end; 

 {Setup Enabled FIQ}
 FIQEnabled:=LongWord(-1);
 
 {Setup Local Enabled IRQs}
 for Count:=0 to RPI3_CPU_COUNT - 1 do
  begin
   LocalIRQEnabled[Count]:=0;
  end;
  
 {Setup Local Enabled FIQs}
 for Count:=0 to RPI3_CPU_COUNT - 1 do
  begin
   LocalFIQEnabled[Count]:=0;
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Clear Interrupt Enabled}
 InterruptRegisters.FIQ_control:=0;
 InterruptRegisters.Disable_IRQs_1:=$FFFFFFFF;
 InterruptRegisters.Disable_IRQs_2:=$FFFFFFFF;
 InterruptRegisters.Disable_Basic_IRQs:=$FFFFFFFF;
end;

{==============================================================================}

procedure RPi3PeripheralInit;
var
 CacheLineSize:LongWord;
begin
 {}
 {Get Cache Line Size}
 CacheLineSize:=Max(L1DataCacheGetLineSize,L2CacheGetLineSize);
 
 {Setup Peripherals}
 INTERRUPT_REGS_BASE:=BCM2837_INTERRUPT_REGS_BASE;
 SYSTEMTIMER_REGS_BASE:=BCM2837_SYSTEM_TIMER_REGS_BASE;
 TIMER_REGS_BASE:=BCM2837_TIMER_REGS_BASE;
 GPIO_REGS_BASE:=BCM2837_GPIO_REGS_BASE;
 UART_REGS_BASE:=BCM2837_PL011_REGS_BASE;

 {Setup GPIO}
 GPIO_PIN_COUNT:=BCM2837_GPIO_PIN_COUNT;
 
 {Setup Virtual GPIO}
 case BOARD_TYPE of 
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    VIRTUAL_GPIO_PIN_COUNT:=BCM2837_VIRTUAL_GPIO_PIN_COUNT;
   end; 
 end;  
 
 {Setup LEDs}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {Power LED}
    POWER_LED_PIN:=GPIO_PIN_35;
    POWER_LED_PULL:=GPIO_PULL_NONE;
    POWER_LED_FUNCTION:=GPIO_FUNCTION_OUT;
    POWER_LED_ACTIVE_LOW:=False;
    
    {Activity LED}
    ACTIVITY_LED_PIN:=GPIO_PIN_47;
    ACTIVITY_LED_PULL:=GPIO_PULL_NONE;
    ACTIVITY_LED_FUNCTION:=GPIO_FUNCTION_OUT;
    ACTIVITY_LED_ACTIVE_LOW:=False;
   end;
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI_COMPUTE3:begin
    {Activity LED}
    ACTIVITY_LED_PIN:=VIRTUAL_GPIO_PIN_0;
    ACTIVITY_LED_FUNCTION:=VIRTUAL_GPIO_FUNCTION_OUT;
    ACTIVITY_LED_ACTIVE_LOW:=False;
   end;
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    {Activity LED}
    ACTIVITY_LED_PIN:=GPIO_PIN_29;
    ACTIVITY_LED_PULL:=GPIO_PULL_NONE;
    ACTIVITY_LED_FUNCTION:=GPIO_FUNCTION_OUT;
    ACTIVITY_LED_ACTIVE_LOW:=False;
   end;  
 end;
 
 {Setup DMA}
 DMA_ALIGNMENT:=SizeOf(LongWord);
 DMA_MULTIPLIER:=SizeOf(LongWord);
 DMA_SHARED_MEMORY:=False;
 DMA_NOCACHE_MEMORY:=False;
 DMA_BUS_ADDRESSES:=True;
 DMA_CACHE_COHERENT:=False;
 if CacheLineSize > DMA_ALIGNMENT then DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > DMA_MULTIPLIER then DMA_MULTIPLIER:=CacheLineSize;
 
 {Setup USB}
 USB_DMA_ALIGNMENT:=SizeOf(LongWord);
 USB_DMA_MULTIPLIER:=SizeOf(LongWord);
 USB_DMA_SHARED_MEMORY:=False;
 USB_DMA_NOCACHE_MEMORY:=False;
 USB_DMA_BUS_ADDRESSES:=True;
 USB_DMA_CACHE_COHERENT:=False;
 if CacheLineSize > USB_DMA_ALIGNMENT then USB_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > USB_DMA_MULTIPLIER then USB_DMA_MULTIPLIER:=CacheLineSize;
 
 {Setup MMC}
 MMC_DMA_ALIGNMENT:=SizeOf(LongWord);
 MMC_DMA_MULTIPLIER:=SizeOf(LongWord);
 MMC_DMA_SHARED_MEMORY:=False;
 MMC_DMA_NOCACHE_MEMORY:=False;
 MMC_DMA_BUS_ADDRESSES:=True;
 MMC_DMA_CACHE_COHERENT:=False;
 if CacheLineSize > MMC_DMA_ALIGNMENT then MMC_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > MMC_DMA_MULTIPLIER then MMC_DMA_MULTIPLIER:=CacheLineSize;
 
 {Setup BCM2710}
 BCM2710DMA_ALIGNMENT:=SizeOf(LongWord);
 BCM2710DMA_MULTIPLIER:=SizeOf(LongWord);
 BCM2710DMA_SHARED_MEMORY:=False;
 BCM2710DMA_NOCACHE_MEMORY:=False;
 BCM2710DMA_BUS_ADDRESSES:=True;
 BCM2710DMA_CACHE_COHERENT:=False;
 if CacheLineSize > BCM2710DMA_ALIGNMENT then BCM2710DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > BCM2710DMA_MULTIPLIER then BCM2710DMA_MULTIPLIER:=CacheLineSize;
 
 BCM2710FRAMEBUFFER_ALIGNMENT:=SIZE_256;
 BCM2710FRAMEBUFFER_CACHED:=GPU_MEMORY_CACHED;
 
 {Setup DWCOTG}
 DWCOTG_IRQ:=BCM2837_IRQ_USB;
 DWCOTG_POWER_ID:=POWER_ID_USB0;
 DWCOTG_REGS_BASE:=BCM2837_USB_REGS_BASE;
 DWCOTG_DMA_ALIGNMENT:=SizeOf(LongWord);
 DWCOTG_DMA_MULTIPLIER:=SizeOf(LongWord);
 DWCOTG_DMA_SHARED_MEMORY:=False;
 DWCOTG_DMA_NOCACHE_MEMORY:=False;
 DWCOTG_DMA_BUS_ADDRESSES:=True; 
 DWCOTG_DMA_CACHE_COHERENT:=False;
 DWCOTG_HOST_FRAME_INTERVAL:=True;
 if CacheLineSize > DWCOTG_DMA_ALIGNMENT then DWCOTG_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > DWCOTG_DMA_MULTIPLIER then DWCOTG_DMA_MULTIPLIER:=CacheLineSize;
 
 {Setup LAN}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B,
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI_COMPUTE3:begin
    SMSC95XX_MAC_ADDRESS:=BoardGetMACAddress;
   end;
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    LAN78XX_MAC_ADDRESS:=BoardGetMACAddress; 
   end;
 end;
end;

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi3FramebufferInit;
var
 Status:LongWord;
 
 DisplayId:LongWord;
 DisplayNum:LongWord;
 DisplayCount:LongWord;
 MultiDisplay:Boolean;
 
 RPi3Framebuffer:PRPi3Framebuffer;
begin
 {}
 {Get Display Count and Check Multi-Display support}
 if FramebufferGetNumDisplays(DisplayCount) = ERROR_SUCCESS then
  begin
   MultiDisplay:=(DisplayCount > 0);
  end
 else
  begin
   MultiDisplay:=False;
   DisplayCount:=1;
  end;
  
 {Create Framebuffer for first Display}
 if DisplayCount > 0 then
  begin
   {Set Display Num}
   DisplayNum:=0;
   
   {Get Display Id}
   DisplayId:=FramebufferGetDisplayId(DisplayNum);
   
   {Create Framebuffer}
   RPi3Framebuffer:=PRPi3Framebuffer(FramebufferDeviceCreateEx(SizeOf(TRPi3Framebuffer)));
   if RPi3Framebuffer <> nil then
    begin
     {Device}
     RPi3Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     RPi3Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
     RPi3Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_BACKLIGHT;
     RPi3Framebuffer.Framebuffer.Device.DeviceData:=nil;
     RPi3Framebuffer.Framebuffer.Device.DeviceDescription:=RPI3_FRAMEBUFFER_DESCRIPTION + ' (' + FramebufferDisplayIdToName(DisplayId) + ')';
     {Framebuffer}
     RPi3Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
     RPi3Framebuffer.Framebuffer.DeviceAllocate:=RPi3FramebufferDeviceAllocate;
     RPi3Framebuffer.Framebuffer.DeviceRelease:=RPi3FramebufferDeviceRelease;
     RPi3Framebuffer.Framebuffer.DeviceBlank:=RPi3FramebufferDeviceBlank;
     RPi3Framebuffer.Framebuffer.DeviceCommit:=RPi3FramebufferDeviceCommit;
     RPi3Framebuffer.Framebuffer.DeviceSetBacklight:=RPi3FramebufferDeviceSetBacklight;
     RPi3Framebuffer.Framebuffer.DeviceSetProperties:=RPi3FramebufferDeviceSetProperties;
     {Driver}
     RPi3Framebuffer.MultiDisplay:=MultiDisplay;
     RPi3Framebuffer.DisplayNum:=DisplayNum;
     FramebufferGetDisplaySettings(DisplayNum,RPi3Framebuffer.DisplaySettings);
   
     {Setup Flags}
     if BCM2710FRAMEBUFFER_CACHED then RPi3Framebuffer.Framebuffer.Device.DeviceFlags:=RPi3Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_COMMIT;
     if BCM2710FRAMEBUFFER_CACHED then RPi3Framebuffer.Framebuffer.Device.DeviceFlags:=RPi3Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
     {if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then RPi3Framebuffer.Framebuffer.Device.DeviceFlags:=RPi3Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;} {Handled by FramebufferAllocate}
     
     {Register Framebuffer}
     Status:=FramebufferDeviceRegister(@RPi3Framebuffer.Framebuffer);
     if Status = ERROR_SUCCESS then
      begin
       {Allocate Framebuffer}
       Status:=FramebufferDeviceAllocate(@RPi3Framebuffer.Framebuffer,nil);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
        end;
      end
     else
      begin     
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: Failed to register new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: Failed to create new framebuffer device');
    end;
  end;
end;
{$ENDIF}
{==============================================================================}

procedure RPi3PageTableInit;
{Initialize the Hardware Page Tables before enabling the MMU}
var
 Count:Integer;
 Table:PtrUInt;
 Address:PtrUInt;
 ActualAddress:PtrUInt;
 RequestAddress:PtrUInt;
begin
 {}
 {Initialize Memory (Get values for CPU_MEMORY_BASE/SIZE)}
 if not(MemoryInitialized) then MemoryInit;
 
 {Parse Boot Tags (Register all memory with Heap manager)}
 if not(ParseBootTagsCompleted) then ParseBootTags;
 
 {Parse Command Line (Copy command line from zero page)}
 if not(ParseCommandLineCompleted) then ParseCommandLine;

 {Parse Environment (Copy environment from zero page)}
 if not(ParseEnvironmentCompleted) then ParseEnvironment;
 
 {$IFDEF CPUARM}
 {Create the first level page table}
 {Setup 1MB sections covering the entire 4GB address space with a default layout}
 {Set the 1MB sections in the first 1GB as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=$00000000;
 for Count:=0 to 1023 do
  begin
   ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
   Inc(Address,SIZE_1M);
  end;

 {Set the 1MB sections in the second 1GB as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH (Shared)(Non Executable)(Read Write)}
 for Count:=1024 to 2047 do
  begin
   if CPU_MEMORY_RESTRICTED then
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_NONE);
    end 
   else
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
    end; 
   Inc(Address,SIZE_1M);
  end;
  
 {Set the 1MB sections in the remaining 2GB as ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED (Shared)(Non Executable)(Read Write)}
 for Count:=2048 to 4095 do
  begin
   if CPU_MEMORY_RESTRICTED then
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_NONE);
    end 
   else
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
    end; 
   Inc(Address,SIZE_1M);
  end;
   
 {Set the 1MB sections containing the PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Shared)(Non Executable)(Read Write)} 
 if PERIPHERALS_SIZE > 0 then
  begin
   Address:=(PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   while Address < (PERIPHERALS_BASE + PERIPHERALS_SIZE) do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
  end;  
 
 {Set the 1MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Shared)(Non Executable)(Read Write)} 
 if LOCAL_PERIPHERALS_SIZE > 0 then
  begin
   Address:=(LOCAL_PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   while Address < (LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE) do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE); 
     Inc(Address,SIZE_1M);
    end;
  end;  

 {Create the currently used second level (Coarse) page tables}
 Table:=(PAGE_TABLES_ADDRESS and ARMV7_L1D_COARSE_BASE_MASK);
 Address:=$00000000;
 for Count:=0 to PAGE_TABLES_USED - 1 do
  begin
   ARMv7SetPageTableCoarse(Address,Table,0);
   Inc(Table,SIZE_1K);
   Inc(Address,SIZE_1M);
  end;
 PAGE_TABLES_NEXT:=Table;
 
 {Set the 4KB zero page to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED (Shared)(Non Executable)(No Access)}
 Address:=$00000000;
 ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_NONE); 
 
 {Set the 4KB pages containing the VECTOR_TABLE_BASE to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)} 
 {Changed to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK (Shared)(Executable)(Read Only) due to no write through support for data caching on ARMv7 or later}
 {See 5.2.1 Memory types and attributes in the Cortex A7 Technical Reference Manual (http://infocenter.arm.com/help/topic/com.arm.doc.ddi0464f/CIHJCAAG.html)}
 Address:=(VECTOR_TABLE_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (VECTOR_TABLE_BASE + VECTOR_TABLE_SIZE) do
  begin
   {ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L2D_ACCESS_READONLY);}
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end; 
 
 {Set the 4KB pages containing the first level page table to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)} 
 Address:=(PAGE_TABLE_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (PAGE_TABLE_BASE + PAGE_TABLE_SIZE) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;
 
 {Set the 4KB pages containing the TEXT (Code) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)} 
 {Changed to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK (Shared)(Executable)(Read Only) due to no write through support for data caching on ARMv7 or later}
 {See 5.2.1 Memory types and attributes in the Cortex A7 Technical Reference Manual (http://infocenter.arm.com/help/topic/com.arm.doc.ddi0464f/CIHJCAAG.html)}
 Address:=(PtrUInt(@_text_start) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (PtrUInt(@_data)) do
  begin
   {ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L2D_ACCESS_READONLY);}
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the DATA (Initialized) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PtrUInt(@_data) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (PtrUInt(@_bss_start)) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE); 
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the BSS (Uninitialized) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PtrUInt(@_bss_start) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (PtrUInt(@_bss_end)) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the second level page tables to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_TABLES_ADDRESS and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (PAGE_TABLES_ADDRESS + PAGE_TABLES_LENGTH) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;
 
 {Set the 4KB pages containing the initial stack to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_STACK_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (INITIAL_STACK_BASE + INITIAL_STACK_SIZE) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the initial heap to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_HEAP_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (INITIAL_HEAP_BASE + INITIAL_HEAP_SIZE) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
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
     Dec(RequestAddress,MEMORY_LOCAL_SIZE * RPI3_CPU_COUNT); {Local memory is per CPU}
     if IRQ_ENABLED then Dec(RequestAddress,MEMORY_IRQ_SIZE * RPI3_CPU_COUNT); {IRQ memory is per CPU}
     if FIQ_ENABLED then Dec(RequestAddress,MEMORY_FIQ_SIZE * RPI3_CPU_COUNT); {FIQ memory is per CPU}
     
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
       for Count:=0 to (RPI3_CPU_COUNT - 1) do
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
       for Count:=0 to (RPI3_CPU_COUNT - 1) do
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
       for Count:=0 to (RPI3_CPU_COUNT - 1) do
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
  
 {Set the 1MB sections containing the GPU_MEMORY to ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 if GPU_MEMORY_SIZE > 0 then
  begin
   Address:=(GPU_MEMORY_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   while (Address < (GPU_MEMORY_BASE + GPU_MEMORY_SIZE)) and (Address < (PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK)) do
    begin
     if GPU_MEMORY_CACHED then
      begin
       ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
      end
     else
      begin
       ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
      end;
     Inc(Address,SIZE_1M);
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

procedure RPi3PowerLEDEnable;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {Disable Pull Up/Down}
    GPIOPullSelect(GPIO_PIN_35,GPIO_PULL_NONE);
    {Enable Output}
    GPIOFunctionSelect(GPIO_PIN_35,GPIO_FUNCTION_OUT);
   end;
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    {Virtual GPIO}
    VirtualGPIOFunctionSelect(POWER_LED_PIN,POWER_LED_FUNCTION);
   end;  
 end;
end;

{==============================================================================}

procedure RPi3PowerLEDOn;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {LED On}
    GPIOOutputSet(GPIO_PIN_35,GPIO_LEVEL_HIGH);
   end;
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    {LED On}
    if POWER_LED_ACTIVE_LOW then
     begin
      VirtualGPIOOutputSet(POWER_LED_PIN,GPIO_LEVEL_LOW);
     end
    else
     begin
      VirtualGPIOOutputSet(POWER_LED_PIN,GPIO_LEVEL_HIGH);
     end;
   end;  
 end;
end;

{==============================================================================}

procedure RPi3PowerLEDOff;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {LED Off}
    GPIOOutputSet(GPIO_PIN_35,GPIO_LEVEL_LOW);
   end;
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin 
    {LED Off}
    if POWER_LED_ACTIVE_LOW then
     begin
      VirtualGPIOOutputSet(POWER_LED_PIN,GPIO_LEVEL_HIGH);
     end
    else
     begin
      VirtualGPIOOutputSet(POWER_LED_PIN,GPIO_LEVEL_LOW);
     end;
   end;
 end;
end;

{==============================================================================}

procedure RPi3ActivityLEDEnable;
var
 Value:LongWord;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {Disable Pull Up/Down}
    GPIOPullSelect(GPIO_PIN_47,GPIO_PULL_NONE);
    {Enable Output}
    GPIOFunctionSelect(GPIO_PIN_47,GPIO_FUNCTION_OUT);
   end;
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI_COMPUTE3:begin 
    {Virtual GPIO}
    VirtualGPIOFunctionSelect(ACTIVITY_LED_PIN,ACTIVITY_LED_FUNCTION);
   end;
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    {Setup Pull Up/Down}
    GPIOPullSelect(ACTIVITY_LED_PIN,ACTIVITY_LED_PULL);
    {Enable Function}
    GPIOFunctionSelect(ACTIVITY_LED_PIN,ACTIVITY_LED_FUNCTION);
   end;
 end;
end;

{==============================================================================}

procedure RPi3ActivityLEDOn;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {LED On}
    GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_HIGH);
   end;
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI_COMPUTE3:begin 
    {LED On}
    VirtualGPIOOutputSet(ACTIVITY_LED_PIN,GPIO_LEVEL_HIGH);
   end;
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    {LED On}
    GPIOOutputSet(ACTIVITY_LED_PIN,GPIO_LEVEL_HIGH);
   end;  
 end;
end;

{==============================================================================}

procedure RPi3ActivityLEDOff;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {LED Off}
    GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_LOW);
   end;
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI_COMPUTE3:begin 
    {LED Off}
    VirtualGPIOOutputSet(ACTIVITY_LED_PIN,GPIO_LEVEL_LOW);
   end;
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    {LED Off}
    GPIOOutputSet(ACTIVITY_LED_PIN,GPIO_LEVEL_LOW);
   end;  
 end;
end;

{==============================================================================}

function RPi3MailboxReceive(Mailbox,Channel:LongWord):LongWord;
{Receive from specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 ResultCode:LongWord;
begin
 {}
 Result:=0;
 {Check Mailbox}
 if Mailbox = BCM2837_MAILBOX_0 then
  begin 
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try 
    {Setup Timeout}
    Timeout:=RPI3_MAILBOX_TIMEOUT;
   
    {Setup Result}
    ResultCode:=BCM2837_MAILBOX_CHANNEL_MASK; {Start with all channel bits set}
   
    {Check Channel}
    while ((ResultCode and BCM2837_MAILBOX_CHANNEL_MASK) <> Channel) do
     begin
      {Check Status}
      while (Mailbox0Registers.Status and BCM2837_MAILBOX_STATUS_EMPTY) = BCM2837_MAILBOX_STATUS_EMPTY do
       begin
        {Memory Barrier}
        DataMemoryBarrier; {After the Last Read (MicrosecondDelay also Reads)}
        
	    {Wait for data to arrive in the mailbox}
        if Timeout = 0 then
         begin
          Exit;
         end;
        Dec(Timeout);
        MicrosecondDelay(1000);
       end; 
     
      {Read Data}
      ResultCode:=Mailbox0Registers.Read;
     end;
   
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
   
    {Return Result}
    Result:=ResultCode and BCM2837_MAILBOX_DATA_MASK; {Account for channel offset}
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end; 
end;

{==============================================================================}

procedure RPi3MailboxSend(Mailbox,Channel,Data:LongWord);
{Send to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 WriteData:LongWord;
begin
 {}
 {Check Mailbox}
 if Mailbox = BCM2837_MAILBOX_0 then
  begin 
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try 
    {Setup Timeout}
    Timeout:=RPI3_MAILBOX_TIMEOUT;
   
    {Setup Data}
    WriteData:=Channel or (Data and BCM2837_MAILBOX_DATA_MASK);
   
    {Check Status}
    while (Mailbox1Registers.Status and BCM2837_MAILBOX_STATUS_FULL) = BCM2837_MAILBOX_STATUS_FULL do
     begin
      {Memory Barrier}
      DataMemoryBarrier; {After the Last Read (MicrosecondDelay also Reads)}
      
      {Wait for space available in the mailbox}
      if Timeout = 0 then
       begin
        Exit;
       end;
      Dec(Timeout);
      MicrosecondDelay(1000);
     end;
   
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read / Before the First Write}
   
    {Write Data}
    Mailbox1Registers.Write:=WriteData;
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end; 
end;

{==============================================================================}

function RPi3MailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
{Perform a transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPi3MailboxCallEx(Mailbox,Channel,Data,Response,RPI3_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPi3MailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord; 
{Perform a transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
var
 Retries:LongWord;
 WriteData:LongWord;
 ResultCode:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mailbox}
 if Mailbox = BCM2837_MAILBOX_0 then
  begin 
   {Check the Data (Must not use the lowest 4 bits)}
   if (Data and BCM2837_MAILBOX_CHANNEL_MASK) <> 0 then Exit;

   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try 
    {Setup Timeout}
    Retries:=Timeout;
    
    {Wait for Mailbox 0 Empty} 
    while (Mailbox0Registers.Status and BCM2837_MAILBOX_STATUS_EMPTY) <> BCM2837_MAILBOX_STATUS_EMPTY do
     begin
      {Read Data from Mailbox 0}
      ResultCode:=Mailbox0Registers.Read;
      
      {Memory Barrier}
      DataMemoryBarrier; {After the Last Read (MicrosecondDelay also Reads)}
      
      {Wait for no data available in the mailbox}
      if Retries = 0 then
       begin
        Result:=ERROR_TIMEOUT;
        Exit;
       end;
      Dec(Retries);
      MicrosecondDelay(1000);
     end;
   
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
 
    {Setup Timeout}
    Retries:=Timeout;
    
    {Wait for Mailbox 1 not Full}
    while (Mailbox1Registers.Status and BCM2837_MAILBOX_STATUS_FULL) = BCM2837_MAILBOX_STATUS_FULL do
     begin
      {Memory Barrier}
      DataMemoryBarrier; {After the Last Read (MicrosecondDelay also Reads)}
      
      {Wait for space available in the mailbox}
      if Retries = 0 then
       begin
        Result:=ERROR_TIMEOUT;
        Exit;
       end;
      Dec(Retries);
      MicrosecondDelay(1000);
     end;
   
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read / Before the First Write}
 
    {Write Data to Mailbox 1}
    WriteData:=Channel or (Data and BCM2837_MAILBOX_DATA_MASK);
    Mailbox1Registers.Write:=WriteData; 
 
    {Setup Timeout}
    Retries:=Timeout;
    
    {Wait for Mailbox 0 not Empty}
    while (Mailbox0Registers.Status and BCM2837_MAILBOX_STATUS_EMPTY) = BCM2837_MAILBOX_STATUS_EMPTY do
     begin
      {Memory Barrier}
      DataMemoryBarrier; {After the Last Read (MicrosecondDelay also Reads)}
      
      {Wait for data to arrive in the mailbox}
      if Retries = 0 then
       begin
        Result:=ERROR_TIMEOUT;
        Exit;
       end;
      Dec(Retries);
      MicrosecondDelay(1000);
     end;
   
    {Memory Barrier}
    {DataMemoryBarrier;} {After the Last Read}
  
    {Read Data from Mailbox 0}
    ResultCode:=Mailbox0Registers.Read;
   
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
  
    {Check the Response}
    if (ResultCode and BCM2837_MAILBOX_CHANNEL_MASK) <> Channel then
     begin
      Result:=ERROR_INVALID_DATA;
      Exit;
     end; 
  
    {Return the Response}
    Response:=ResultCode and BCM2837_MAILBOX_DATA_MASK; {Account for channel offset}
    
    Result:=ERROR_SUCCESS;
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end; 
end;

{==============================================================================}

function RPi3MailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPi3MailboxPropertyCallEx(Mailbox,Channel,Data,Response,RPI3_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPi3MailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord; 
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
var
 Tag:PBCM2837MailboxTagHeader;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF PLATFORM_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - (Mailbox=' + IntToHex(Mailbox,8) + ' Channel=' + IntToHex(Channel,8) + ' Data=' + IntToHex(PtrUInt(Data),8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Mailbox}
 if Mailbox = BCM2837_MAILBOX_0 then
  begin 
   {Check the Data}
   if Data = nil then Exit;
 
   {Call Mailbox}
   Result:=MailboxCallEx(Mailbox,Channel,PhysicalToBusAddress(Data),Response,Timeout); 
   if Result <> ERROR_SUCCESS then
    begin
     if PLATFORM_LOG_ENABLED then PlatformLogError('MailboxPropertyCallEx - MailboxCallEx Failed');
     Exit;
    end;

   {Check the Response}
   if Response <> PhysicalToBusAddress(Data) then
    begin
     Result:=ERROR_FUNCTION_FAILED;
     if PLATFORM_LOG_ENABLED then PlatformLogError('MailboxPropertyCallEx - Response Check Failed: (Response=' + IntToHex(Response,8) + ' Data=' + IntToHex(PhysicalToBusAddress(Data),8) + ')');
     Exit;
    end;
 
   {Check the Response Code}
   if PBCM2837MailboxHeader(Data).Code <> BCM2837_MBOX_RESPONSE_CODE_SUCCESS then
    begin
     Result:=ERROR_FUNCTION_FAILED;
     if PLATFORM_LOG_ENABLED then PlatformLogError('MailboxPropertyCallEx - Response Code Failed: (Code=' + IntToHex(PBCM2837MailboxHeader(Data).Code,8) + ')');
     Exit;
    end;
 
   {Check each tags Response Code}
   Tag:=PBCM2837MailboxTagHeader(PtrUInt(Data) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
   while Tag.Tag <> BCM2837_MBOX_TAG_END do
    begin
     if (Tag.Length and BCM2837_MBOX_TAG_RESPONSE_CODE) = 0 then
      begin
       {$IFDEF PLATFORM_DEBUG}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - Tag Response Code Incorrect (Length=' + IntToHex(Tag.Length,8) + ')');
       {$ENDIF}
       {Result:=ERROR_FUNCTION_FAILED;} {Note: Recent firmware functions do not always set the response bit in the tag}
       {Exit;}                          {      The Linux firmware driver does not check this bit in the response}
      end;
     {Clear the Response bit so callers can read the length field without extra processing}
     Tag.Length:=Tag.Length and not(BCM2837_MBOX_TAG_RESPONSE_CODE);
     {Get Next Tag}
     Tag:=PBCM2837MailboxTagHeader(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagHeader)) + Tag.Size);
    end;
 
   Result:=ERROR_SUCCESS;
  end; 
end;

{==============================================================================}

function RPi3MailboxPropertyTag(Tag:LongWord;Data:Pointer;Size:LongWord):LongWord;
{Request a property tag (Get/Set) from the mailbox property channel}
{Note: Data does not need to include mailbox property channel header or footer}
{Note: Data pointer does not need any specific alignment or caching attributes}
{Note: Size must be a multiple of 4 bytes}
{Note: Size must include the size of the request and response which use the same buffer}
var
 Total:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 TagHeader:PBCM2837MailboxTagHeader;
 TagBuffer:Pointer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Tag}
 if Tag = 0 then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Check Size}
 if Size = 0 then Exit;
 
 {Check Size is a multiple of 4}
 if (Size and 3) <> 0 then Exit;
 
 {Calculate Total Size}
 Total:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagHeader) + Size + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Total,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Total,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Total,0);
 
  {Setup Header}
  Header.Size:=Total;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  TagHeader:=PBCM2837MailboxTagHeader(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  TagHeader.Tag:=Tag;
  TagHeader.Size:=Size;
  TagHeader.Length:=Size;

  {Copy Request}
  TagBuffer:=Pointer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)) + PtrUInt(SizeOf(TBCM2837MailboxTagHeader)));
  System.Move(Data^,TagBuffer^,Size);
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(TagHeader) + PtrUInt(SizeOf(TBCM2837MailboxTagHeader)) + Size);
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('MailboxPropertyTag - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Copy Response}
  System.Move(TagBuffer^,Data^,Size);
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3RequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified IRQ number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Get Mask}
 Mask:=CPUIDToMask(CPUID);
 
 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

 {Check Local}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check Mask Count}
   if CPUMaskCount(Mask) <> 1 then Exit;
    
   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;
  end;
 
 Result:=ERROR_NOT_ENOUGH_MEMORY;
 
 {Allocate Entry}
 Entry:=AllocMem(SizeOf(TInterruptEntry));
 if Entry = nil then Exit;

 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.Handler:=Handler;
 Entry.HandlerEx:=HandlerEx;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_DEFAULT;
 
 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_NONE;
 if RPi3InterruptIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL;
 
 {Register Entry}
 Result:=RPi3InterruptRegisterEntry(Entry^);
 
 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi3ReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified IRQ number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Get Mask}
 Mask:=CPUIDToMask(CPUID);
 
 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}
 
 {Check Local}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check Mask Count}
   if CPUMaskCount(Mask) <> 1 then Exit;
    
   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;
  end;
 
 {Clear Entry}
 FillChar(Entry,SizeOf(TInterruptEntry),0);

 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.Handler:=Handler;
 Entry.HandlerEx:=HandlerEx;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_DEFAULT;
 
 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_NONE;
 if RPi3InterruptIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL;
 
 {Deregister Entry}
 Result:=RPi3InterruptDeregisterEntry(Entry);
end;

{==============================================================================}

function RPi3RequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
{Request registration of the supplied handler to the specified FIQ number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Get Mask}
 Mask:=CPUIDToMask(CPUID);
 
 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}
 
 {Check Local}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check Mask Count}
   if CPUMaskCount(Mask) <> 1 then Exit;
    
   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;
  end;
 
 Result:=ERROR_NOT_ENOUGH_MEMORY;
 
 {Allocate Entry}
 Entry:=AllocMem(SizeOf(TInterruptEntry));
 if Entry = nil then Exit;

 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.Handler:=Handler;
 Entry.HandlerEx:=HandlerEx;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_FIQ;
 
 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_NONE or INTERRUPT_FLAG_FIQ;
 if RPi3InterruptIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL or INTERRUPT_FLAG_FIQ;
 
 {Register Entry}
 Result:=RPi3InterruptRegisterEntry(Entry^);
 
 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi3ReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
{Request deregistration of the supplied handler from the specified FIQ number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Get Mask}
 Mask:=CPUIDToMask(CPUID);
 
 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

 {Check Local}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check Mask Count}
   if CPUMaskCount(Mask) <> 1 then Exit;
    
   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;
  end;

 {Clear Entry}
 FillChar(Entry,SizeOf(TInterruptEntry),0);

 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.Handler:=Handler;
 Entry.HandlerEx:=HandlerEx;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_FIQ;
 
 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_NONE or INTERRUPT_FLAG_FIQ;
 if RPi3InterruptIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL or INTERRUPT_FLAG_FIQ;

 {Deregister Entry}
 Result:=RPi3InterruptDeregisterEntry(Entry);
end;

{==============================================================================}

function RPi3RegisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified interrupt number (Where Applicable)}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handler}
 if not Assigned(Handler) then Exit;
 
 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}
 
 {Check Local}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check Mask Count}
   if CPUMaskCount(Mask) <> 1 then Exit;
    
   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;
  end;
  
 Result:=ERROR_NOT_ENOUGH_MEMORY;
   
 {Allocate Entry}
 Entry:=AllocMem(SizeOf(TInterruptEntry));
 if Entry = nil then Exit;
 
 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.Priority:=Priority;
 Entry.Flags:=Flags;
 Entry.SharedHandler:=Handler;
 Entry.Parameter:=Parameter;
   
 {Update Flags}
 if RPi3InterruptIsLocal(Number) then Entry.IsLocal:=True;
   
 {Register Entry}
 Result:=RPi3InterruptRegisterEntry(Entry^);
 
 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;
 
{==============================================================================}

function RPi3DeregisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified interrupt number (Where Applicable)}
var
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;
 
 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}
 
 {Check Local}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check Mask Count}
   if CPUMaskCount(Mask) <> 1 then Exit;
    
   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;
  end;
 
 {Clear Entry}
 FillChar(Entry,SizeOf(TInterruptEntry),0);
 
 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.Priority:=Priority;
 Entry.Flags:=Flags;
 Entry.SharedHandler:=Handler;
 Entry.Parameter:=Parameter;
   
 {Update Flags}
 if RPi3InterruptIsLocal(Number) then Entry.IsLocal:=True;

 {Deregister Entry}
 Result:=RPi3InterruptDeregisterEntry(Entry);
end;

{==============================================================================}

function RPi3RegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request registration of the supplied extended handler to the specified System Call number}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check CPU}
 if (CPUID <> CPU_ID_ALL) and (CPUID > (CPUGetCount - 1)) then Exit;
 
 {Check CPU}
 {if CPUID = CPU_ID_ALL then 
  begin
   CPUID:=CPUGetCurrent;
  end;}
 
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
  SystemCallEntries[Number].CPUID:=CPUID;
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

function RPi3DeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request deregistration of the supplied extended handler from the specified System Call number}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check CPU}
 if (CPUID <> CPU_ID_ALL) and (CPUID > (CPUGetCount - 1)) then Exit;
 
 {Check CPU}
 {if CPUID = CPU_ID_ALL then 
  begin
   CPUID:=CPUGetCurrent;
  end;}
 
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

function RPi3GetInterruptEntry(Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
{Get the interrupt entry for the specified interrupt number and instance}
begin
 {}
 Result:=RPi3InterruptGetEntry(CPU_ID_ALL,Number,INTERRUPT_FLAG_NONE,Interrupt,Instance);
end;

{==============================================================================}

function RPi3GetLocalInterruptEntry(CPUID,Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
{Get the local interrupt entry for the specified interrupt number and instance}
begin
 {}
 Result:=RPi3InterruptGetEntry(CPUID,Number,INTERRUPT_FLAG_LOCAL,Interrupt,Instance);
end;

{==============================================================================}

function RPi3GetSystemCallEntry(Number:LongWord):TSystemCallEntry; 
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

function RPi3SystemRestart(Delay:LongWord):LongWord; 
var
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Delay}
 if Delay < 10 then Delay:=10;
 
 {Acquire Lock}
 if ShutdownLock.Lock <> INVALID_HANDLE_VALUE then ShutdownLock.AcquireLock(ShutdownLock.Lock);
 try 
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}
 
  {Enable Watchdog}
  WatchdogRegisters.WDOG:=BCM2837_PM_PASSWORD or ((Delay * BCM2837_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2837_PM_WDOG_TIME_MASK);
  
  {Enable Restart}  
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2837_PM_PASSWORD or (Current and BCM2837_PM_RSTC_WRCFG_CLR) or BCM2837_PM_RSTC_WRCFG_FULL_RESET;

  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read} 
 
  {Wait for Restart}
  MillisecondDelay(1000);

  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if ShutdownLock.Lock <> INVALID_HANDLE_VALUE then ShutdownLock.ReleaseLock(ShutdownLock.Lock);
 end;
end;

{==============================================================================}

function RPi3SystemShutdown(Delay:LongWord):LongWord;
var
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Delay}
 if Delay < 10 then Delay:=10;
 
 {Acquire Lock}
 if ShutdownLock.Lock <> INVALID_HANDLE_VALUE then ShutdownLock.AcquireLock(ShutdownLock.Lock);
 try 
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}
 
  {Enable Hard Reset}  
  Current:=WatchdogRegisters.RSTS;
  {WatchdogRegisters.RSTS:=BCM2837_PM_PASSWORD or (Current and BCM2837_PM_RSTC_WRCFG_CLR) or BCM2837_PM_RSTS_HADWRH_SET;} {RPi firmware changed to use a different value}
  WatchdogRegisters.RSTS:=Current or BCM2837_PM_PASSWORD or BCM2837_PM_RSTS_RASPBERRYPI_HALT;
  
  {Enable Watchdog}
  WatchdogRegisters.WDOG:=BCM2837_PM_PASSWORD or ((Delay * BCM2837_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2837_PM_WDOG_TIME_MASK);
  
  {Enable Restart}  
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2837_PM_PASSWORD or (Current and BCM2837_PM_RSTC_WRCFG_CLR) or BCM2837_PM_RSTC_WRCFG_FULL_RESET;

  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read} 
 
  {Wait for Shutdown}
  MillisecondDelay(1000);

  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if ShutdownLock.Lock <> INVALID_HANDLE_VALUE then ShutdownLock.ReleaseLock(ShutdownLock.Lock);
 end;
end;

{==============================================================================}

function RPi3SystemGetCommandLine:String;
{Get the Command Line from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetCommandLine;
begin
 {}
 Result:='';
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetCommandLine) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetCommandLine(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_COMMAND_LINE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetCommandLine) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetCommandLine)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('SystemGetCommandLine - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get Command Line}
  for Count:=0 to 1023 do
   begin
    if Tag.Response.CommandLine[Count] <> #0 then
     begin
      Result:=Result + Tag.Response.CommandLine[Count];
     end;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3CPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord; 
{Get the CPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetARMMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetARMMemory) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetARMMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_ARM_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetARMMemory) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetARMMemory)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('CPUGetMemory - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get CPU Memory}
  Address:=Tag.Response.Address;
  Length:=Tag.Response.Size;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3GPUGetState:LongWord;
begin
 {}
 Result:=GPU_STATE_NONE;

 //To Do
end;

{==============================================================================}

function RPi3GPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord; 
{Get the GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetVCMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetVCMemory) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetVCMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_VC_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetVCMemory) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetVCMemory)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('GPUGetMemory - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get GPU Memory}
  Address:=Tag.Response.Address;
  Length:=Tag.Response.Size;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3BoardGetModel:LongWord;
{Get the Board Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Model:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetBoardModel;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetBoardModel) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetBoardModel(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_BOARD_MODEL;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetBoardModel) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetBoardModel)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('BoardGetModel - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get Board Model}
  Model:=Tag.Response.Model;
  
  {Convert Board Model}
  Result:=Model;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3BoardGetSerial:Int64;
{Get the Board Serial from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetBoardSerial;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetBoardSerial) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetBoardSerial(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_BOARD_SERIAL;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetBoardSerial) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetBoardSerial)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('BoardGetSerial - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get Board Serial}
  Result:=Tag.Response.Serial;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3BoardGetRevision:LongWord;
{Get the Board Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetBoardRevision;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetBoardRevision) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try 
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetBoardRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_BOARD_REV;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetBoardRevision) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetBoardRevision)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('BoardGetRevision - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Board Revision}
  Result:=Tag.Response.Revision;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3BoardGetMACAddress:String;
{Get the Board MAC Address from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetMACAddress;
begin
 {}
 Result:='';
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetMACAddress) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetMACAddress(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_MAC_ADDRESS;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetMACAddress) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetMACAddress)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('BoardGetMACAddress - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get MAC Address}
  for Count:=0 to 5 do
   begin
    Result:=Result + IntToHex(Tag.Response.MAC[Count],2);
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FirmwareGetRevision:LongWord;
{Get the Firmware Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetFirmwareRevision;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetFirmwareRevision) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetFirmwareRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_FIRMWARE_REV;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetFirmwareRevision) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetFirmwareRevision)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FirmwareGetRevision - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get Firmware Revision}
  Result:=Tag.Response.Revision;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FirmwareGetThrottled:LongWord;
{Get the Firmware Throttling state from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetThrottled;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetThrottled) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetThrottled(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_THROTTLED;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetThrottled) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Value:=$FFFF; {Clear sticky bits}
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetThrottled)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FirmwareGetThrottled - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get Firmware Throttling}
  Result:=Tag.Response.Value;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3PowerGetWait(PowerId:LongWord):LongWord;
{Get the Power Wait from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetTiming;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetTiming) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetTiming(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_TIMING;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetTiming) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi3ConvertPowerIdRequest(PowerId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetTiming)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerGetWait - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Wait}
  Result:=Tag.Response.Wait;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3PowerGetState(PowerId:LongWord):LongWord;
{Get the Power State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetPowerState;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetPowerState) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetPowerState) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi3ConvertPowerIdRequest(PowerId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetPowerState)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerGetState - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Power State}
  Result:=RPi3ConvertPowerStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3PowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;
{Set the Power State in the Mailbox property tags channel}
{Note: Power Lock not required due to Mailbox Property Call serialization}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetPowerState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetPowerState) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetPowerState) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi3ConvertPowerIdRequest(PowerId);
  Tag.Request.State:=RPi3ConvertPowerStateRequest(State);
  if Wait then Tag.Request.State:=(Tag.Request.State or BCM2837_MBOX_SET_POWER_STATE_REQ_WAIT);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetPowerState)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if Wait then
   begin
    Result:=MailboxPropertyCallEx(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response,RPI3_MAILBOX_TIMEOUT_EX);
   end
  else
   begin  
    Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
   end; 
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerSetState - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Power State}
  if Wait then
   begin
    if RPi3ConvertPowerStateRequest(State) = BCM2837_MBOX_SET_POWER_STATE_REQ_ON then
     begin
      if (Tag.Response.State and BCM2837_MBOX_POWER_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
     end
    else
     begin
      if (Tag.Response.State and BCM2837_MBOX_POWER_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
     end;
   end
  else
   begin
    Result:=ERROR_SUCCESS;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3ClockGetCount:LongWord;
{Gets the current system clock count (32 least significant bits of total)}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz and therefore overflows every 4295 seconds}
begin
 {}
 {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
 {Get Value}
 {$IFDEF CPUARM}
 Result:=ARMv7GetTimerCount(ARMV7_CP15_C14_CNTV);
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 Result:=ARMv8GetTimerCount(ARMV8_CP15_C14_CNTV);
 {$ENDIF CPUAARCH64}
 {$ELSE}
 {Get Value}
 Result:=TimerRegisters.CLO;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 {$ENDIF}
end;

{==============================================================================}

function RPi3ClockGetTotal:Int64; 
{Gets the total system clock count}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz, the clock interrupt also uses this timer to
 increment the clock every second and therefore keep time}
{$IFDEF RPI3_CLOCK_SYSTEM_TIMER}
var
 Check:LongWord;
{$ENDIF}
begin
 {}
 {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
 {Get Value}
 {$IFDEF CPUARM}
 Result:=ARMv7GetTimerCount(ARMV7_CP15_C14_CNTV);
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 Result:=ARMv8GetTimerCount(ARMV8_CP15_C14_CNTV);
 {$ENDIF CPUAARCH64}
 {$ELSE}
 {Get High Value}
 Int64Rec(Result).Hi:=TimerRegisters.CHI;
 
 {Get Low Value}
 Int64Rec(Result).Lo:=TimerRegisters.CLO;
 
 {Check High Value}
 Check:=TimerRegisters.CHI;
 if Check <> Int64Rec(Result).Hi then
  begin
   {Rollover Occurred, Get Low Value Again}
   Int64Rec(Result).Hi:=Check;
   Int64Rec(Result).Lo:=TimerRegisters.CLO;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 {$ENDIF}
end;

{==============================================================================}

function RPi3ClockGetRate(ClockId:LongWord):LongWord;
{Get the Clock Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetClockRate;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetClockRate) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetClockRate) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi3ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetClockRate)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetRate - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Clock Rate}
  Result:=Tag.Response.Rate;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3ClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;
{Set the Clock Rate in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetClockRate;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Rate}
 if Rate = 0 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetClockRate) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetClockRate) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi3ConvertClockIdRequest(ClockId);
  Tag.Request.Rate:=Rate;
  Tag.Request.SkipTurbo:=0;
  if not(Turbo) then Tag.Request.SkipTurbo:=BCM2837_MBOX_CLOCK_RATE_REQ_SKIP_TURBO;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetClockRate)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockSetRate - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Clock Rate}
  if Tag.Response.Rate <> 0 then Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3ClockGetState(ClockId:LongWord):LongWord;
{Get the Clock State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetClockState;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetClockState) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetClockState) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi3ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetClockState)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetState - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Clock State}
  Result:=RPi3ConvertClockStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3ClockSetState(ClockId,State:LongWord):LongWord;
{Set the Clock State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetClockState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetClockState) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetClockState) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi3ConvertClockIdRequest(ClockId);
  Tag.Request.State:=RPi3ConvertClockStateRequest(State);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetClockState)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockSetState - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Clock State}
  if RPi3ConvertClockStateRequest(State) = BCM2837_MBOX_SET_CLOCK_STATE_REQ_ON then
   begin
    if (Tag.Response.State and BCM2837_MBOX_CLOCK_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
   end
  else
   begin
    if (Tag.Response.State and BCM2837_MBOX_CLOCK_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3ClockGetMinRate(ClockId:LongWord):LongWord;
{Get the Clock Min Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetClockMinRate;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetClockMinRate) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetClockMinRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_CLOCK_MIN_RATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetClockMinRate) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi3ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetClockMinRate)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetMinRate - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Clock Min Rate}
  Result:=Tag.Response.Rate;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3ClockGetMaxRate(ClockId:LongWord):LongWord;
{Get the Clock Max Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetClockMaxRate;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetClockMaxRate) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetClockMaxRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_CLOCK_MAX_RATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetClockMaxRate) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi3ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetClockMaxRate)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetMaxRate - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Clock Max Rate}
  Result:=Tag.Response.Rate;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3TurboGetState(TurboId:LongWord):LongWord;
{Get the Turbo State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetTurbo;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetTurbo) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetTurbo) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetTurbo)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('TurboGetState - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Turbo State}
  Result:=Tag.Response.Level; {0 to Off / 1 for On}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3TurboSetState(TurboId,State:LongWord):LongWord;
{Set the Turbo State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetTurbo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetTurbo) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetTurbo) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}
  Tag.Request.Level:=State; {0 to Off / 1 for On}
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetTurbo)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('TurboSetState - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Turbo State}
  if Tag.Response.Level = State then Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;


{==============================================================================}

function RPi3VoltageGetValue(VoltageId:LongWord):LongWord;
{Get the Voltage Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetVoltage;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetVoltage) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetVoltage) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi3ConvertVoltageIdRequest(VoltageId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetVoltage)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetValue - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Voltage Value}
  if (Tag.Response.Value <> BCM2837_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3VoltageSetValue(VoltageId,Value:LongWord):LongWord;
{Set the Voltage Value in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetVoltage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Value}
 if Value = 0 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetVoltage) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetVoltage) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi3ConvertVoltageIdRequest(VoltageId);
  Tag.Request.Value:=Value; {Offset from 1.2V in units of 0.025V}
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetVoltage)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageSetValue - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Voltage Value}
  if (Tag.Response.Value <> BCM2837_MBOX_VOLTAGE_INVALID) and (Tag.Response.Value = Value) then Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3VoltageGetMinValue(VoltageId:LongWord):LongWord;
{Get the Voltage Min Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetMinVoltage;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetMinVoltage) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetMinVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_MIN_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetMinVoltage) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi3ConvertVoltageIdRequest(VoltageId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetMinVoltage)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMinValue - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Voltage Min Value}
  if (Tag.Response.Value <> BCM2837_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;
 
{==============================================================================}

function RPi3VoltageGetMaxValue(VoltageId:LongWord):LongWord;
{Get the Voltage Max Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetMaxVoltage;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetMaxVoltage) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetMaxVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_MAX_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetMaxVoltage) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi3ConvertVoltageIdRequest(VoltageId);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetMaxVoltage)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMaxValue - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Voltage Max Value}
  if (Tag.Response.Value <> BCM2837_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3TemperatureGetCurrent(TemperatureId:LongWord):LongWord;
{Get the Temperature Current from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetTemperature;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetTemperature) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetTemperature) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPi3ConvertTemperatureIdRequest(TemperatureId);
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetTemperature)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('TemperatureGetCurrent - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get Temperature}
  Result:=Tag.Response.Temperature;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3TemperatureGetMaximum(TemperatureId:LongWord):LongWord;
{Get the Temperature Maximum Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetMaxTemperature;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetMaxTemperature) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetMaxTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_MAX_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetMaxTemperature) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPi3ConvertTemperatureIdRequest(TemperatureId);
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetMaxTemperature)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('TemperatureGetMaximum - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get Max Temperature}
  Result:=Tag.Response.Temperature;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3GPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
{Allocate GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagAllocateMemory;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagAllocateMemory) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagAllocateMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_ALLOCATE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagAllocateMemory) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Size:=Length;
  Tag.Request.Alignment:=Alignment;
  Tag.Request.Flags:=Flags;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagAllocateMemory)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('GPUMemoryAllocate - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  Result:=Tag.Response.Handle;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3GPUMemoryRelease(Handle:THandle):LongWord;
{Release GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagReleaseMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagReleaseMemory) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagReleaseMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_RELEASE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagReleaseMemory) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagReleaseMemory)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('GPUMemoryRelease - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3GPUMemoryLock(Handle:THandle):LongWord;
{Lock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagLockMemory;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagLockMemory) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagLockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_LOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagLockMemory) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagLockMemory)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('GPUMemoryLock - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=Tag.Response.Address;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3GPUMemoryUnlock(Handle:THandle):LongWord;
{Unlock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagUnlockMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagUnlockMemory) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagUnlockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_UNLOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagUnlockMemory) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagUnlockMemory)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('GPUMemoryUnlock - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3GPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;
{Execute GPU Code from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagExecuteCode;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagExecuteCode) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagExecuteCode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_EXECUTE_CODE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagExecuteCode) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
  Tag.Request.R0:=R0;
  Tag.Request.R1:=R1;
  Tag.Request.R2:=R2;
  Tag.Request.R3:=R3;
  Tag.Request.R4:=R4;
  Tag.Request.R5:=R5;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagExecuteCode)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('GPUExecuteCode - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  Result:=Tag.Response.R0;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3DispmanxHandleGet(Resource:THandle):THandle;
{Get Dispmanx Memory Handle from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetDispmanxHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetDispmanxHandle) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetDispmanxHandle(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_DISPMANX_HANDLE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetDispmanxHandle) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Resource:=Resource;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetDispmanxHandle)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('DispmanxHandleGet - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  if Tag.Response.Status = ERROR_SUCCESS then Result:=Tag.Response.Memory;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3EDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Get EDID Block from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetEDIDBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Length}
 if Length < 128 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetEDIDBlock) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetEDIDBlock(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_EDID_BLOCK;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetEDIDBlock) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Block:=Block;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetEDIDBlock)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('EDIDBlockGet - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  Result:=Tag.Response.Status;
  if Result = ERROR_SUCCESS then
   begin
    {Copy EDID}
    System.Move(Tag.Response.EDID,Buffer^,128);
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
{Allocate Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagAllocateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagAllocateBuffer) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagAllocateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_ALLOCATE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagAllocateBuffer) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Alignment:=Alignment;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagAllocateBuffer)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferAllocate - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Address:=Tag.Response.Address;
  Length:=Tag.Response.Size;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferRelease:LongWord;
{Release Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagReleaseBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagReleaseBuffer) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_RELEASE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagReleaseBuffer) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagReleaseBuffer)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferRelease - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetState(State:LongWord):LongWord;
{Set Framebuffer State (Blank Screen) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagBlankScreen;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagBlankScreen) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagBlankScreen(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_BLANK_SCREEN;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagBlankScreen) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.State:=0;
  if State = 0 then Tag.Request.State:=BCM2837_MBOX_BLANK_SCREEN_REQ_ON;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagBlankScreen)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetState - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  if Tag.Response.State = State then Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord; 
{Get Framebuffer Dimensions from the Mailbox property tags channel}
begin
 {}
 {Get Physical}
 Result:=RPi3FramebufferGetPhysical(Width,Height);
 if Result = ERROR_SUCCESS then
  begin
   {Get Overscan}
   Result:=RPi3FramebufferGetOverscan(Top,Bottom,Left,Right);
  end;
end;

{==============================================================================}

function RPi3FramebufferGetPhysical(var Width,Height:LongWord):LongWord;
{Get Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetPhysical) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetPhysical) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetPhysical)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetPhysical - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Width:=Tag.Response.Width;
  Height:=Tag.Response.Height;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetPhysical(var Width,Height:LongWord):LongWord;
{Set Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetPhysical) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetPhysical) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetPhysical)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetPhysical - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Width:=Tag.Response.Width;
  Height:=Tag.Response.Height;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestPhysical(var Width,Height:LongWord):LongWord;
{Test Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestPhysical) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestPhysical) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestPhysical)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestPhysical - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Width:=Tag.Response.Width;
  Height:=Tag.Response.Height;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetVirtual(var Width,Height:LongWord):LongWord;
{Get Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetVirtual) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetVirtual) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetVirtual)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetVirtual - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Width:=Tag.Response.Width;
  Height:=Tag.Response.Height;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetVirtual(var Width,Height:LongWord):LongWord;
{Set Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetVirtual) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtual) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetVirtual)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetVirtual - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Width:=Tag.Response.Width;
  Height:=Tag.Response.Height;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestVirtual(var Width,Height:LongWord):LongWord;
{Test Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestVirtual) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestVirtual) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestVirtual)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestVirtual - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Width:=Tag.Response.Width;
  Height:=Tag.Response.Height;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetDepth(var Depth:LongWord):LongWord;
{Get Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetDepth) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetDepth) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetDepth)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetDepth - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Depth:=Tag.Response.Depth;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetDepth(var Depth:LongWord):LongWord;
{Set Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetDepth) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetDepth) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetDepth)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetDepth - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Depth:=Tag.Response.Depth;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestDepth(var Depth:LongWord):LongWord;
{Test Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestDepth) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestDepth) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestDepth)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestDepth - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Depth:=Tag.Response.Depth;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetPixelOrder(var Order:LongWord):LongWord;
{Get Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetPixelOrder) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetPixelOrder) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetPixelOrder)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetPixelOrder - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Order:=Tag.Response.Order;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetPixelOrder(var Order:LongWord):LongWord;
{Set Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetPixelOrder) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetPixelOrder) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetPixelOrder)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetPixelOrder - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Order:=Tag.Response.Order;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestPixelOrder(var Order:LongWord):LongWord;
{Test Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestPixelOrder) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestPixelOrder) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestPixelOrder)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestPixelOrder - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Order:=Tag.Response.Order;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetAlphaMode(var Mode:LongWord):LongWord;
{Get Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetAlphaMode) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetAlphaMode) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetAlphaMode)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetAlphaMode - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Mode:=Tag.Response.Mode;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetAlphaMode(var Mode:LongWord):LongWord;
{Set Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetAlphaMode) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetAlphaMode) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetAlphaMode)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetAlphaMode - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Mode:=Tag.Response.Mode;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestAlphaMode(var Mode:LongWord):LongWord;
{Test Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestAlphaMode) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestAlphaMode) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestAlphaMode)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestAlphaMode - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Mode:=Tag.Response.Mode;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetPitch:LongWord;
{Get Framebuffer Pitch (Bytes per line) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetPitch;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetPitch) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetPitch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_PITCH;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetPitch) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetPitch)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetPitch - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  Result:=Tag.Response.Pitch;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetOffset(var X,Y:LongWord):LongWord;
{Get Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetVirtualOffset) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetVirtualOffset) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetVirtualOffset)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetOffset - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  X:=Tag.Response.X;
  Y:=Tag.Response.Y;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetOffset(var X,Y:LongWord):LongWord;
{Set Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetVirtualOffset) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtualOffset) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetVirtualOffset)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetOffset - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  X:=Tag.Response.X;
  Y:=Tag.Response.Y;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestOffset(var X,Y:LongWord):LongWord;
{Test Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestVirtualOffset) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestVirtualOffset) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestVirtualOffset)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestOffset - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  X:=Tag.Response.X;
  Y:=Tag.Response.Y;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Get Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetOverscan) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetOverscan) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetOverscan)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetOverscan - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Top:=Tag.Response.Top;
  Bottom:=Tag.Response.Bottom;
  Left:=Tag.Response.Left;
  Right:=Tag.Response.Right;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Set Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetOverscan) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetOverscan) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetOverscan)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetOverscan - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Top:=Tag.Response.Top;
  Bottom:=Tag.Response.Bottom;
  Left:=Tag.Response.Left;
  Right:=Tag.Response.Right;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Test Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestOverscan) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestOverscan) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestOverscan)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestOverscan - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Top:=Tag.Response.Top;
  Bottom:=Tag.Response.Bottom;
  Left:=Tag.Response.Left;
  Right:=Tag.Response.Right;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
{Get Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetPalette;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < 1024 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetPalette) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetPalette) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetPalette)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetPalette - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  System.Move(Tag.Response.Values,Buffer^,1024);
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Set Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetPalette;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Start}
 if Start > 255 then Exit;
 
 {Check Count}
 if Count < 1 then Exit;
 if Count > 256 then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < (Count * SizeOf(LongWord)) then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetPalette) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetPalette) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetPalette)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetPalette - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Test Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestPalette;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Start}
 if Start > 255 then Exit;
 
 {Check Count}
 if Count < 1 then Exit;
 if Count > 256 then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < (Count * SizeOf(LongWord)) then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestPalette) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TEST_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestPalette) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestPalette)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestPalette - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferTestVsync:LongWord;
{Test Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagTestVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagTestVsync) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagTestVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_TST_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagTestVsync) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagTestVsync)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestVsync - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetVsync:LongWord;
{Set Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetVsync) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetVsync) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetVsync)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetVsync - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetBacklight(Brightness:LongWord):LongWord;
{Set Framebuffer Backlight Brightness from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetBacklight;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetBacklight) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetBacklight(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_BACKLIGHT;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetBacklight) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Brightness:=Brightness;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetBacklight)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetBacklight - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Result}
  if LongInt(Tag.Response.Brightness) < 0 then Exit;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetNumDisplays(var NumDisplays:LongWord):LongWord;
{Get the number of displays from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetNumDisplays;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 NumDisplays:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetNumDisplays) + SizeOf(TBCM2837MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetNumDisplays(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_NUM_DISPLAYS;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetNumDisplays) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetNumDisplays)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetNumDisplays - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  NumDisplays:=Tag.Response.NumDisplays;
  
  Result:=ERROR_SUCCESS;

  {Check for Emulator (Assume error if no displays)}
  if EMULATOR_MODE and (NumDisplays = 0) then Result:=ERROR_NOT_SUPPORTED;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetDisplayId(DisplayNum:LongWord):LongWord;
{Get the display id for the specified display number from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetDisplayId;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetDisplayId) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetDisplayId(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_DISPLAY_ID;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetDisplayId) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetDisplayId)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetDisplayId - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Display Id}
  Result:=Tag.Response.DisplayId;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferSetDisplayNum(DisplayNum:LongWord):LongWord;
{Get the display number that all framebuffer requests will refer to using the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetDisplayNum;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetDisplayNum) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetDisplayNum(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_DISPLAY_NUM;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetDisplayNum) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetDisplayNum)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetDisplayNum - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferGetDisplaySettings(DisplayNum:LongWord;var DisplaySettings:TDisplaySettings):LongWord;
{Get the display settings for the specified display number from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetDisplaySettings;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 FillChar(DisplaySettings,SizeOf(TDisplaySettings),0);
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetDisplaySettings) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetDisplaySettings(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_DISPLAY_SETTINGS;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetDisplaySettings) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetDisplaySettings)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetDisplaySettings - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  DisplaySettings.DisplayNumber:=Tag.Response.DisplayNum;
  DisplaySettings.Width:=Tag.Response.Width;
  DisplaySettings.Height:=Tag.Response.Height;
  DisplaySettings.Depth:=Tag.Response.Depth;
  DisplaySettings.Pitch:=Tag.Response.Pitch;
  DisplaySettings.VirtualWidth:=Tag.Response.VirtualWidth;
  DisplaySettings.VirtualHeight:=Tag.Response.VirtualHeight;
  DisplaySettings.VirtualWidthOffset:=Tag.Response.VirtualWidthOffset;
  DisplaySettings.VirtualHeightOffset:=Tag.Response.VirtualHeightOffset;
  DisplaySettings.FramebufferAddress:=Tag.Response.BusAddress;
 
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3FramebufferDisplayIdToName(DisplayId:LongWord):String;
{Get the name for the specified display id}
begin
 {}
 case DisplayId of
  BCM2837_MBOX_DISPLAY_ID_MAIN_LCD:Result:='Main LCD';
  BCM2837_MBOX_DISPLAY_ID_AUX_LCD:Result:='Aux LCD';
  BCM2837_MBOX_DISPLAY_ID_HDMI0:Result:='HDMI0';
  BCM2837_MBOX_DISPLAY_ID_SDTV:Result:='SDTV';
  BCM2837_MBOX_DISPLAY_ID_FORCE_LCD:Result:='Force LCD';
  BCM2837_MBOX_DISPLAY_ID_FORCE_TV:Result:='Force TV';
  BCM2837_MBOX_DISPLAY_ID_FORCE_OTHER:Result:='Force Other';
  BCM2837_MBOX_DISPLAY_ID_HDMI1:Result:='HDMI1';
  BCM2837_MBOX_DISPLAY_ID_FORCE_TV2:Result:='Force TV2';
 else
  begin
   Result:='Unknown';
  end;  
 end;
end;

{==============================================================================}

function RPi3TouchGetBuffer(var Address:PtrUInt):LongWord;
{Get the Touchscreen buffer from the Mailbox property tags channel}

{Note: On current firmware versions calling TouchGetBuffer will allocate a buffer
       from GPU memory and render subsequent calls to TouchSetBuffer ineffective.
       
       After an initial call to TouchSetBuffer calls to TouchGetBuffer will always
       return the CPU allocated buffer}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetTouch) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetTouch) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetTouch)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('TouchGetBuffer - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Address:=Tag.Response.Address;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3TouchSetBuffer(Address:PtrUInt):LongWord;
{Set the Touchscreen buffer in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetTouch) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetTouch) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetTouch)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('TouchSetBuffer - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3VirtualGPIOGetBuffer(var Address:PtrUInt):LongWord;
{Get the Virtual GPIO buffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetVirtualGPIO;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetVirtualGPIO) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetVirtualGPIO(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_GPIOVIRTBUF;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetVirtualGPIO) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetVirtualGPIO)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VirtualGPIOGetBuffer - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Address:=Tag.Response.Address;
  
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3VirtualGPIOSetBuffer(Address:PtrUInt):LongWord;
{Set the Virtual GPIO buffer in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetVirtualGPIO;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetVirtualGPIO) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetVirtualGPIO(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_GPIOVIRTBUF;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtualGPIO) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetVirtualGPIO)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VirtualGPIOSetBuffer - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3CursorSetDefault:LongWord;
{Set Cursor Default (Pixels) from the Mailbox property tags channel}
var
 Row:LongWord;
 Col:LongWord;
 Offset:LongWord;
 Size:LongWord;
 Cursor:PLongWord;
 Address:LongWord;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;
 
 {Determine Cursor Size}
 Size:=CURSOR_ARROW_DEFAULT_WIDTH * CURSOR_ARROW_DEFAULT_HEIGHT * SizeOf(LongWord);
 
 {Allocate the Cursor (No Cache)}
 Cursor:=AllocNoCacheMem(Size);
 if Cursor <> nil then
  begin
    Offset:=0;
    for Row:=0 to CURSOR_ARROW_DEFAULT_HEIGHT - 1 do
     begin
      for Col:=0 to CURSOR_ARROW_DEFAULT_WIDTH - 1 do
       begin
        Cursor[Col + Offset]:=CURSOR_ARROW_DEFAULT[Row,Col];
       end;

      {Update Offset}
      Inc(Offset,CURSOR_ARROW_DEFAULT_WIDTH);
     end;
 
   {Convert to Physical Address}
   Address:=PhysicalToBusAddress(Cursor);
 
   {Set the Cursor}
   Result:=RPi3CursorSetInfo(CURSOR_ARROW_DEFAULT_WIDTH,CURSOR_ARROW_DEFAULT_HEIGHT,0,0,Pointer(Address),Size);
 
   {Free the Cursor}
   FreeMem(Cursor);
  end;
end;

{==============================================================================}

function RPi3CursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
{Set Cursor Info (Pixels) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetCursorInfo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Pixels}
 if Pixels = nil then Exit;
 
 {Check Length}
 if Length < 1 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetCursorInfo) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetCursorInfo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_CURSOR_INFO;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetCursorInfo) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  Tag.Request.Pixels:=Pixels;
  Tag.Request.HotspotX:=HotspotX;
  Tag.Request.HotspotY:=HotspotY;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetCursorInfo)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('CursorSetInfo - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3CursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;
{Set Cursor State (Enable, X, Y) from the Mailbox property tags channel}
{Relative: X, Y is relative to Display (Virtual) not Framebuffer (Physical)}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagSetCursorState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagSetCursorState) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagSetCursorState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_SET_CURSOR_STATE;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagSetCursorState) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Enable:=BCM2837_MBOX_CURSOR_INVISIBLE;
  if Enabled then Tag.Request.Enable:=BCM2837_MBOX_CURSOR_VISIBLE;
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
  Tag.Request.Flags:=BCM2837_MBOX_CURSOR_STATE_FRAMEBUFFER_COORDS;
  if Relative then Tag.Request.Flags:=BCM2837_MBOX_CURSOR_STATE_DISPLAY_COORDS;
  
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagSetCursorState)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('CursorSetState - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Get Result}
  Result:=Tag.Response.Status;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3DMAGetChannels:LongWord;
{Get the available DMA Channels from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagGetDMAChannels;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagGetDMAChannels) + SizeOf(TBCM2837MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2837_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2837MailboxTagGetDMAChannels(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
  Tag.Header.Tag:=BCM2837_MBOX_TAG_GET_DMA_CHANNELS;
  Tag.Header.Size:=SizeOf(TBCM2837MailboxTagGetDMAChannels) - SizeOf(TBCM2837MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagGetDMAChannels)));
  Footer.Tag:=BCM2837_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('DMAGetChannels - MailboxPropertyCall Failed');
    Exit;
   end; 
 
  {Get DMA Channels}
  Result:=Tag.Response.Channels;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi3VirtualGPIOAllocate:Boolean; 
{Allocate the Virtual GPIO buffer either from memory or from the firmware}
var
 Size:LongWord;
 Address:PtrUInt;
begin
 {}
 {Check Address}
 if VirtualGPIOBuffer.Address = 0 then
  begin
   Result:=False;
 
   {Acquire Lock}
   if UtilityLock.Lock <> INVALID_HANDLE_VALUE then UtilityLock.AcquireLock(UtilityLock.Lock);
   try
    {Recheck Address (After Lock)}
    if VirtualGPIOBuffer.Address = 0 then
     begin
      {Check Buffer}
      if VirtualGPIOBuffer.Buffer = nil then
       begin
        {Get Size}
        Size:=RoundUp(MEMORY_PAGE_SIZE,DMA_MULTIPLIER);
      
        {Allocate Non Cached}
        VirtualGPIOBuffer.Buffer:=AllocNoCacheAlignedMem(Size,DMA_ALIGNMENT);
        if VirtualGPIOBuffer.Buffer = nil then 
         begin
          {Allocate Normal}
          VirtualGPIOBuffer.Buffer:=AllocAlignedMem(Size,DMA_ALIGNMENT);
          
          {Set Caching}
          VirtualGPIOBuffer.CachedBuffer:=not(DMA_CACHE_COHERENT);
         end;  
       end;  
      
      {Set Buffer}
      Address:=PhysicalToBusAddress(VirtualGPIOBuffer.Buffer);
      if (VirtualGPIOBuffer.Buffer <> nil) and (RPi3VirtualGPIOSetBuffer(Address) = ERROR_SUCCESS) then
       begin
        {Update Address}
        VirtualGPIOBuffer.Address:=PtrUInt(VirtualGPIOBuffer.Buffer);
       end
      else
       begin      
        {Get Buffer}
        Address:=0;
        if RPi3VirtualGPIOGetBuffer(Address) <> ERROR_SUCCESS then Exit;
      
        {Update Address}
        VirtualGPIOBuffer.Address:=BusAddressToPhysical(Pointer(Address));
        
        {Set Caching}
        VirtualGPIOBuffer.CachedBuffer:=True;
        
        {Free Buffer}
        if VirtualGPIOBuffer.Buffer <> nil then FreeMem(VirtualGPIOBuffer.Buffer);
        VirtualGPIOBuffer.Buffer:=nil;
       end; 
     end; 
   finally  
    {Release Lock}
    if UtilityLock.Lock <> INVALID_HANDLE_VALUE then UtilityLock.ReleaseLock(UtilityLock.Lock);
   end; 
  end;
 
 Result:=True; 
end;

{==============================================================================}

function RPi3VirtualGPIOInputGet(Pin:LongWord):LongWord; 
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check Pin}
 if Pin >= BCM2837_VIRTUAL_GPIO_PIN_COUNT then Exit;
 
 {Check Address}
 if VirtualGPIOBuffer.Address = 0 then
  begin
   {Allocate Buffer}
   if not RPi3VirtualGPIOAllocate then Exit;
  end;
 
 {Check Address}
 if VirtualGPIOBuffer.Address > 0 then
  begin
   Result:=PLongWord(VirtualGPIOBuffer.Address + (Pin * SizeOf(LongWord)))^;
   Result:=(Result shr Pin) and 1;
  end;
end;

{==============================================================================}

function RPi3VirtualGPIOOutputSet(Pin,Level:LongWord):LongWord; 
var
 Enable:Word;
 Disable:Word;
 Difference:SmallInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Pin}
 if Pin >= BCM2837_VIRTUAL_GPIO_PIN_COUNT then Exit;
 
 {Check Level}
 if Level > GPIO_LEVEL_HIGH then Exit;
 
 {Check Address}
 if VirtualGPIOBuffer.Address = 0 then
  begin
   {Allocate Buffer}
   if not RPi3VirtualGPIOAllocate then Exit;
  end;
 
 {Check Address}
 if VirtualGPIOBuffer.Address > 0 then
  begin
   {Get Enable/Disable counts}
   Enable:=VirtualGPIOBuffer.EnableDisable[Pin] shr 16;
   Disable:=VirtualGPIOBuffer.EnableDisable[Pin] shr 0;
   
   {Get Difference}
   Difference:=Enable - Disable;
   
   {Check Level}
   if Level = GPIO_LEVEL_HIGH then
    begin
     {Check State}
     if Difference <= 0 then
      begin
       {Pin is Clear}
       Inc(Enable);
       
       {Set Enable/Disable counts}
       VirtualGPIOBuffer.EnableDisable[Pin]:=(Enable shl 16) or (Disable shl 0);
       
       {Write Value}
       PLongWord(VirtualGPIOBuffer.Address + (Pin * SizeOf(LongWord)))^:=VirtualGPIOBuffer.EnableDisable[Pin];
       
       {Clean Cache}
       CleanDataCacheRange(VirtualGPIOBuffer.Address,BCM2837_VIRTUAL_GPIO_PIN_COUNT * SizeOf(LongWord));
      end;
    end
   else
    begin
     {Check State}
     if Difference > 0 then
      begin
       {Pin is Set}
       Inc(Disable);
       
       {Set Enable/Disable counts}
       VirtualGPIOBuffer.EnableDisable[Pin]:=(Enable shl 16) or (Disable shl 0);
       
       {Write Value}
       PLongWord(VirtualGPIOBuffer.Address + (Pin * SizeOf(LongWord)))^:=VirtualGPIOBuffer.EnableDisable[Pin];
     
       {Clean Cache}
       CleanDataCacheRange(VirtualGPIOBuffer.Address,BCM2837_VIRTUAL_GPIO_PIN_COUNT * SizeOf(LongWord));
      end;
    end;
    
   Result:=ERROR_SUCCESS; 
  end;
end;

{==============================================================================}

function RPi3VirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Pin}
 if Pin >= BCM2837_VIRTUAL_GPIO_PIN_COUNT then Exit;

 {Check Mode}
 case Mode of
  VIRTUAL_GPIO_FUNCTION_OUT:begin
    Result:=ERROR_SUCCESS; 
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{RPi3 Thread Functions}
procedure RPi3SchedulerInit;
{Initialize the scheduler interrupt on the boot CPU}
var
 State:LongWord;
begin
 {}
 {Request the Scheduler IRQ/FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer FIQ}
     RequestExFIQ(RPI3_CPU_BOOT,BCM2837_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end
   else
    begin   
     {Physical Non Secure Timer FIQ}
     RequestExFIQ(RPI3_CPU_BOOT,BCM2837_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end; 
  end
 else
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer IRQ}
     RequestExIRQ(RPI3_CPU_BOOT,BCM2837_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer IRQ}
     RequestExIRQ(RPI3_CPU_BOOT,BCM2837_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end; 
  end;

 {Register the Scheduler SWI}
 RegisterSystemCall(SYSTEM_CALL_CONTEXT_SWITCH,RPi3SchedulerSystemCall);
  
 {Setup the Generic Timer}
 {$IFDEF CPUARM}
 State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode}
 State:=State and not(ARMV7_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV7_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv7SetTimerState(ARMV7_CP15_C14_CNTP,State); {Will set Secure or Non Secure depending on current mode}
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 State:=ARMv8GetTimerState(ARMV8_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode}
 State:=State and not(ARMV8_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV8_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv8SetTimerState(ARMV8_CP15_C14_CNTP,State); {Will set Secure or Non Secure depending on current mode}
 {$ENDIF CPUAARCH64}
  
 {Setup the first Scheduler Interrupt}
 RPi3SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[RPI3_CPU_BOOT]);
end;

{==============================================================================}

procedure RPi3SchedulerStart(CPUID:LongWord);
{Initialize the scheduler interrupt on the specified secondary CPU}
var
 State:LongWord;
begin
 {}
 {Check CPU}
 if CPUID > (CPUGetCount - 1) then Exit;
 
 {Check for Disable}
 if SCHEDULER_SECONDARY_DISABLED then Exit;
 
 {Request the Scheduler IRQ/FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer FIQ}
     RequestExFIQ(CPUID,BCM2837_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end
   else
    begin   
     {Physical Non Secure Timer FIQ}
     RequestExFIQ(CPUID,BCM2837_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end; 
  end
 else
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer IRQ}
     RequestExIRQ(CPUID,BCM2837_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer IRQ}
     RequestExIRQ(CPUID,BCM2837_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi3SchedulerInterrupt,nil);
    end; 
  end;
 
 {Setup the Generic Timer}
 {$IFDEF CPUARM}
 State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode}
 State:=State and not(ARMV7_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV7_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv7SetTimerState(ARMV7_CP15_C14_CNTP,State); {Will set Secure or Non Secure depending on current mode}
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 State:=ARMv8GetTimerState(ARMV8_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode}
 State:=State and not(ARMV8_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV8_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv8SetTimerState(ARMV8_CP15_C14_CNTP,State); {Will set Secure or Non Secure depending on current mode}
 {$ENDIF CPUAARCH64}
 
 {Setup the first Scheduler Interrupt}
 RPi3SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);
end;

{==============================================================================}

procedure RPi3SecondaryBoot(CPUID:LongWord);
var
 Timeout:LongWord;
begin
 {}
 {Check CPU}
 if CPUID > (CPUGetCount - 1) then Exit;

 {Setup Timeout}
 Timeout:=RPI3_LOCAL_MAILBOX_TIMEOUT;
 
 {Wait for Mailbox 3 Clear}
 while ARMLocalRegisters.MailboxReadClear[CPUID].Mailbox3ReadClear <> 0 do
  begin
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read (MicrosecondDelay also Reads)}
   
   {Check Timeout}
   if Timeout = 0 then
    begin
     if PLATFORM_LOG_ENABLED then PlatformLogError('SecondaryBoot - CPU ' + IntToStr(CPUID) + ' failed to respond');
     Exit;
    end;
   Dec(Timeout);
   MicrosecondDelay(1000);
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read / Before the First Write}
 
 {Write the Handler Address to Mailbox 3}
 ARMLocalRegisters.MailboxWrite[CPUID].Mailbox3Write:=LongWord(@RPi3SecondaryHandler);
 
 {Synchronization Barrier}
 DataSynchronizationBarrier;
 
 {Send Event to Wake CPUs}
 SendEvent;
 
 {Setup Timeout}
 Timeout:=RPI3_LOCAL_MAILBOX_TIMEOUT;
 
 {Wait for Mailbox 3 Clear}
 while ARMLocalRegisters.MailboxReadClear[CPUID].Mailbox3ReadClear <> 0 do
  begin
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read (MicrosecondDelay also Reads)}
   
   {Check Timeout}
   if Timeout = 0 then
    begin
     if PLATFORM_LOG_ENABLED then PlatformLogError('SecondaryBoot - CPU ' + IntToStr(CPUID) + ' failed to start');
     Exit;
    end;
   Dec(Timeout);
   MicrosecondDelay(1000);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}
{==============================================================================}
{RPi3 SWI Functions}
function RPi3DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 
{Process an SWI request}
{Called by ARMv7/8SoftwareInterruptHandler in PlatformARMv7/8}
{Note: A DataMemoryBarrier is executed before and after calling this function} 
var
 Entry:PSystemCallEntry;
begin
 {}
 Result:=Thread;
 
 {$IF DEFINED(SWI_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
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
{RPi3 Clock Functions}
procedure RPi3ClockInterrupt(Parameter:Pointer);
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
 RPi3ClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
  
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

procedure RPi3ClockUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a clock interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the timer interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by CLOCK_FREQUENCY}
var
 {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
 Current:LongInt;
 {$ELSE}
 Current:LongWord;
 {$ENDIF}
begin
 {}
 {$IFNDEF RPI3_CLOCK_SYSTEM_TIMER}
 {Get Timer Value}
 {$IFDEF CPUARM}
 Current:=ARMv7GetTimerValue(ARMV7_CP15_C14_CNTV); 
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 Current:=ARMv8GetTimerValue(ARMV8_CP15_C14_CNTV); 
 {$ENDIF CPUAARCH64}
 
 {Set Last}
 if Current < 0 then
  begin
   Last:=Current + Cycles;
   if LongInt(Last) < 0 then Last:=Cycles;
  end
 else
  begin
   Last:=Cycles;
  end;  
 
 {Set Timer Value}
 {$IFDEF CPUARM}
 ARMv7SetTimerValue(ARMV7_CP15_C14_CNTV,Last);
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 ARMv8SetTimerValue(ARMV8_CP15_C14_CNTV,Last);
 {$ENDIF CPUAARCH64}

 {$IFDEF CLOCK_DEBUG}
 ClockInterruptOffset:=Last;
 if ClockInterruptMinOffset = 0 then ClockInterruptMinOffset:=ClockInterruptOffset; 
 if ClockInterruptOffset < ClockInterruptMinOffset then ClockInterruptMinOffset:=ClockInterruptOffset;
 if ClockInterruptOffset > ClockInterruptMaxOffset then ClockInterruptMaxOffset:=ClockInterruptOffset; 
 {$ENDIF}
 {$ELSE}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Clear the Interrupt (C3)}
 TimerRegisters.CS:=BCM2837_SYSTEM_TIMER_CS_3; 
 
 {Get CLO Register}
 Current:=TimerRegisters.CLO;
 if Last = 0 then Last:=Current;
 if Current < Last then
  begin
   {Rollover}
   {Set Last}
   Last:=Current + Cycles;
   
   {$IFDEF CLOCK_DEBUG}
   Inc(ClockInterruptRollover);
   {$ENDIF}
   
   {Set C3 Register}
   TimerRegisters.C3:=Last;
  end
 else
  begin
   {Normal}
   {Increment Last}
   Inc(Last,Cycles);

   {$IFDEF CLOCK_DEBUG}
   if Last >= Current then ClockInterruptOffset:=Last - Current else ClockInterruptOffset:=Cycles;
   if ClockInterruptMinOffset = 0 then ClockInterruptMinOffset:=ClockInterruptOffset; 
   if ClockInterruptOffset < ClockInterruptMinOffset then ClockInterruptMinOffset:=ClockInterruptOffset;
   if ClockInterruptOffset > ClockInterruptMaxOffset then ClockInterruptMaxOffset:=ClockInterruptOffset; 
   {$ENDIF}
   
   {Check Last}
   if Last < (Current + CLOCK_CYCLES_TOLERANCE) then Last:=Current + Cycles;
   
   {Set C3 Register}
   TimerRegisters.C3:=Last;
  end;  

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 {$ENDIF}
end;

{==============================================================================}
{==============================================================================}
{RPi3 Scheduler Functions}
function RPi3SchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
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
 RPi3SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);
 
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

procedure RPi3SchedulerUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a scheduler interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the scheduler interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by RPI3_GENERIC_TIMER_FREQUENCY}
var
 Current:LongInt;
 {$IFDEF SCHEDULER_DEBUG}
 CurrentCPU:LongWord;
 {$ENDIF}
begin
 {}
 {Get Timer Value} 
 {$IFDEF CPUARM}
 Current:=ARMv7GetTimerValue(ARMV7_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode} 
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 Current:=ARMv8GetTimerValue(ARMV8_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode} 
 {$ENDIF CPUAARCH64}
  
 {Set Last}
 if Current < 0 then
  begin
   Last:=Current + Cycles;
   if LongInt(Last) < 0 then Last:=Cycles;
  end
 else
  begin
   Last:=Cycles;
  end;  
 
 {Set Timer Value}
 {$IFDEF CPUARM}
 ARMv7SetTimerValue(ARMV7_CP15_C14_CNTP,Last); {Will set Secure or Non Secure depending on current mode}
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 ARMv8SetTimerValue(ARMV8_CP15_C14_CNTP,Last); {Will set Secure or Non Secure depending on current mode}
 {$ENDIF CPUAARCH64}
  
 {$IFDEF SCHEDULER_DEBUG}
 CurrentCPU:=CPUGetCurrent;
 SchedulerInterruptOffset[CurrentCPU]:=Last;
 if SchedulerInterruptMinOffset[CurrentCPU] = 0 then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU]; 
 if SchedulerInterruptOffset[CurrentCPU] < SchedulerInterruptMinOffset[CurrentCPU] then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
 if SchedulerInterruptOffset[CurrentCPU] > SchedulerInterruptMaxOffset[CurrentCPU] then SchedulerInterruptMaxOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
 {$ENDIF}
end;

{==============================================================================}

procedure RPi3SchedulerSystemCall(Request:PSystemCallRequest);
{System Call handler for the scheduler. This is registered to receive requests for
 the SYSTEM_CALL_CONTEXT_SWITCH and will perform a context switch from within an SWI}
begin
 {}
 {$IFDEF CPUARM}
 ARMv7ContextSwitchSWI(Pointer(Request.Param1),Pointer(Request.Param2),Request.Param3);
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 ARMv8ContextSwitchSWI(Pointer(Request.Param1),Pointer(Request.Param2),Request.Param3);
 {$ENDIF CPUAARCH64}
end;

{==============================================================================}
{==============================================================================}
{RPi3 Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPi3FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using the Mailbox Property Tags}
var
 Size:LongWord;
 Count:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Defaults:TFramebufferProperties;
 Palette:array[0..255] of LongWord;
 Tag:PBCM2837MailboxTagCreateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PRPi3Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPi3Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Properties}
    if Properties = nil then
     begin
      {Use Defaults}
      Defaults.Depth:=FRAMEBUFFER_DEFAULT_DEPTH;
      Defaults.Order:=FRAMEBUFFER_DEFAULT_ORDER;
      Defaults.Mode:=FRAMEBUFFER_DEFAULT_MODE;
      Defaults.PhysicalWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.PhysicalHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.VirtualWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.VirtualHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.OffsetX:=FRAMEBUFFER_DEFAULT_OFFSET_X;
      Defaults.OffsetY:=FRAMEBUFFER_DEFAULT_OFFSET_Y;
      Defaults.OverscanTop:=FRAMEBUFFER_DEFAULT_OVERSCAN_TOP;
      Defaults.OverscanBottom:=FRAMEBUFFER_DEFAULT_OVERSCAN_BOTTOM;
      Defaults.OverscanLeft:=FRAMEBUFFER_DEFAULT_OVERSCAN_LEFT;
      Defaults.OverscanRight:=FRAMEBUFFER_DEFAULT_OVERSCAN_RIGHT;
     end
    else
     begin
      {Use Properties}
      Defaults.Depth:=Properties.Depth;
      Defaults.Order:=Properties.Order;
      Defaults.Mode:=Properties.Mode;
      Defaults.PhysicalWidth:=Properties.PhysicalWidth;
      Defaults.PhysicalHeight:=Properties.PhysicalHeight;
      Defaults.VirtualWidth:=Properties.VirtualWidth;
      Defaults.VirtualHeight:=Properties.VirtualHeight;
      Defaults.OffsetX:=Properties.OffsetX;
      Defaults.OffsetY:=Properties.OffsetY;
      Defaults.OverscanTop:=Properties.OverscanTop;
      Defaults.OverscanBottom:=Properties.OverscanBottom;
      Defaults.OverscanLeft:=Properties.OverscanLeft;
      Defaults.OverscanRight:=Properties.OverscanRight;
     end;   

    {Check Defaults}
    if (Defaults.PhysicalWidth = 0) or (Defaults.PhysicalHeight = 0) then
     begin
      {Get Dimensions Width and Height}
      Result:=RPi3FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
      if Result <> ERROR_SUCCESS then
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: FramebufferAllocate - FramebufferGetDimensions failed: ' + ErrorToString(Result));
        {Exit;} {Do not fail}
        
        {Set Defaults}
        Defaults.PhysicalWidth:=640;
        Defaults.PhysicalHeight:=480;
       end;
      
      {Set Defaults}
      Defaults.VirtualWidth:=Defaults.PhysicalWidth;
      Defaults.VirtualHeight:=Defaults.PhysicalHeight;
     end;
    
    {Calculate Size}
    Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagCreateBuffer) + SizeOf(TBCM2837MailboxFooter);
    
    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);
    
     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2837_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2837MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
     
     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2837_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2837MailboxTagSetPhysical) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;
     
     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtual) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2837_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2837MailboxTagSetDepth) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;
     
     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2837_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2837MailboxTagSetPixelOrder) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;
     
     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2837_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2837MailboxTagSetAlphaMode) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;
     
     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtualOffset) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;
     
     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2837_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2837MailboxTagSetOverscan) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;
     
     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2837_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2837MailboxTagAllocateBuffer) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2710FRAMEBUFFER_ALIGNMENT;
     
     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2837_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2837MailboxTagGetPitch) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);
     
     {Setup Footer}
     Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2837_MBOX_TAG_END;
    
     {Call Mailbox} 
     Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if PLATFORM_LOG_ENABLED then PlatformLogError('Platform: FramebufferAllocate - MailboxPropertyCall failed: ' + ErrorToString(Result));
       Exit;
      end; 
     
     {Update Framebuffer}
     Framebuffer.Address:=BusAddressToPhysical(Pointer(Tag.Allocate.Response.Address)); {Firmware may return address as a Bus address, writes must be to the Physical address}
     Framebuffer.Size:=Tag.Allocate.Response.Size;
     Framebuffer.Pitch:=Tag.Pitch.Response.Pitch;
     Framebuffer.Depth:=Tag.Depth.Response.Depth;
     Framebuffer.Order:=Tag.Order.Response.Order;
     Framebuffer.Mode:=Tag.Mode.Response.Mode;
     Framebuffer.PhysicalWidth:=Tag.Physical.Response.Width;
     Framebuffer.PhysicalHeight:=Tag.Physical.Response.Height;
     Framebuffer.VirtualWidth:=Tag.Vertual.Response.Width;
     Framebuffer.VirtualHeight:=Tag.Vertual.Response.Height;
     Framebuffer.OffsetX:=Tag.Offset.Response.X;
     Framebuffer.OffsetY:=Tag.Offset.Response.Y;
     Framebuffer.OverscanTop:=Tag.Overscan.Response.Top;
     Framebuffer.OverscanBottom:=Tag.Overscan.Response.Bottom;
     Framebuffer.OverscanLeft:=Tag.Overscan.Response.Left;
     Framebuffer.OverscanRight:=Tag.Overscan.Response.Right;
    
     {Check Depth}
     if Framebuffer.Depth = FRAMEBUFFER_DEPTH_8 then
      begin
       {Create Palette (Grayscale only)}
       FillChar(Palette,SizeOf(Palette),0);
       for Count:=0 to 255 do 
        begin
         Palette[Count]:=LongWord($FF000000 or ((Count and $FF) shl 16) or ((Count and $FF) shl 8) or (Count and $FF));
        end;
       
       {Set Palette}
       FramebufferSetPalette(0,256,@Palette,SizeOf(Palette));
      end;
    
     {Get Order}
     if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then
      begin
       Framebuffer.Order:=FRAMEBUFFER_ORDER_BGR;
      end
     else
      begin
       Framebuffer.Order:=FRAMEBUFFER_ORDER_RGB;
      end;      
      
     {Get Format}
     case Framebuffer.Depth of
      FRAMEBUFFER_DEPTH_8:begin
        {Order not relevant for indexed}
        Framebuffer.Format:=COLOR_FORMAT_INDEX8;
       end;
      FRAMEBUFFER_DEPTH_16:begin
        if Framebuffer.Order = FRAMEBUFFER_ORDER_RGB then
         begin
          Framebuffer.Format:=COLOR_FORMAT_RGB16;
         end
        else
         begin
          Framebuffer.Format:=COLOR_FORMAT_BGR16;
         end;
       end;
      FRAMEBUFFER_DEPTH_24:begin
        if Framebuffer.Order = FRAMEBUFFER_ORDER_RGB then
         begin
          Framebuffer.Format:=COLOR_FORMAT_RGB24;
         end
        else
         begin
          Framebuffer.Format:=COLOR_FORMAT_BGR24;
         end;
       end;
      FRAMEBUFFER_DEPTH_32:begin
        if Framebuffer.Order = FRAMEBUFFER_ORDER_RGB then
         begin
          Framebuffer.Format:=COLOR_FORMAT_ARGB32;
         end
        else
         begin
          Framebuffer.Format:=COLOR_FORMAT_ABGR32;
         end;
       end;
     end;  
     
     {Get Rotation}
     Framebuffer.Rotation:=FRAMEBUFFER_ROTATION_0;
    
     {Update Statistics}
     Inc(Framebuffer.AllocateCount);
    
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(Header);
    end;
   finally
    {Set Default Display}
    if PRPi3Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPi3FramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using a simple Mailbox Call}
var
 Response:LongWord;
 Defaults:TFramebufferProperties;
 MailboxFramebuffer:PBCM2837MailboxFramebuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PRPi3Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPi3Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Properties}
    if Properties = nil then
     begin
      {Use Defaults}
      Defaults.Depth:=FRAMEBUFFER_DEFAULT_DEPTH;
      Defaults.Order:=FRAMEBUFFER_DEFAULT_ORDER;
      Defaults.Mode:=FRAMEBUFFER_DEFAULT_MODE;
      Defaults.PhysicalWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.PhysicalHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.VirtualWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.VirtualHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.OffsetX:=FRAMEBUFFER_DEFAULT_OFFSET_X;
      Defaults.OffsetY:=FRAMEBUFFER_DEFAULT_OFFSET_Y;
      Defaults.OverscanTop:=FRAMEBUFFER_DEFAULT_OVERSCAN_TOP;
      Defaults.OverscanBottom:=FRAMEBUFFER_DEFAULT_OVERSCAN_BOTTOM;
      Defaults.OverscanLeft:=FRAMEBUFFER_DEFAULT_OVERSCAN_LEFT;
      Defaults.OverscanRight:=FRAMEBUFFER_DEFAULT_OVERSCAN_RIGHT;
     end
    else
     begin
      {Use Properties}
      Defaults.Depth:=Properties.Depth;
      Defaults.Order:=Properties.Order;
      Defaults.Mode:=Properties.Mode;
      Defaults.PhysicalWidth:=Properties.PhysicalWidth;
      Defaults.PhysicalHeight:=Properties.PhysicalHeight;
      Defaults.VirtualWidth:=Properties.VirtualWidth;
      Defaults.VirtualHeight:=Properties.VirtualHeight;
      Defaults.OffsetX:=Properties.OffsetX;
      Defaults.OffsetY:=Properties.OffsetY;
      Defaults.OverscanTop:=Properties.OverscanTop;
      Defaults.OverscanBottom:=Properties.OverscanBottom;
      Defaults.OverscanLeft:=Properties.OverscanLeft;
      Defaults.OverscanRight:=Properties.OverscanRight;
     end;   

    {Check Defaults}
    if (Defaults.PhysicalWidth = 0) or (Defaults.PhysicalHeight = 0) then
     begin
      {Get Dimensions Width and Height}
      Result:=RPi3FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
      if Result <> ERROR_SUCCESS then
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: FramebufferAllocate - FramebufferGetDimensions failed: ' + ErrorToString(Result));
        {Exit;} {Do not fail}
        
        {Set Defaults}
        Defaults.PhysicalWidth:=640;
        Defaults.PhysicalHeight:=480;
       end;
      
      {Set Defaults}
      Defaults.VirtualWidth:=Defaults.PhysicalWidth;
      Defaults.VirtualHeight:=Defaults.PhysicalHeight;
     end;

    {Allocate Mailbox Framebuffer}
    MailboxFramebuffer:=GetNoCacheAlignedMem(SizeOf(TBCM2837MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
    if MailboxFramebuffer = nil then MailboxFramebuffer:=GetAlignedMem(SizeOf(TBCM2837MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
    if MailboxFramebuffer = nil then Exit;
    try
     {Setup Mailbox Framebuffer}
     MailboxFramebuffer.PhysicalWidth:=Defaults.PhysicalWidth;
     MailboxFramebuffer.PhysicalHeight:=Defaults.PhysicalHeight;
     MailboxFramebuffer.VirtualWidth:=Defaults.VirtualWidth;
     MailboxFramebuffer.VirtualHeight:=Defaults.VirtualHeight;
     MailboxFramebuffer.Pitch:=0;  {Pass zero on request}
     MailboxFramebuffer.Depth:=Defaults.Depth;
     MailboxFramebuffer.OffsetX:=Defaults.OffsetX;
     MailboxFramebuffer.OffsetY:=Defaults.OffsetY;
     MailboxFramebuffer.Address:=0; {Pass zero on request} 
     MailboxFramebuffer.Size:=0;    {Pass zero on request}
     
     {Call Mailbox}
     Result:=MailboxCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_FRAMEBUFFER,PhysicalToBusAddress(MailboxFramebuffer),Response);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: FramebufferAllocate: MailboxCall failed: ' + ErrorToString(Result));
       Exit;
      end; 
      
     {Update Framebuffer} 
     Framebuffer.Address:=BusAddressToPhysical(Pointer(MailboxFramebuffer.Address)); {Mailbox returns address as a Bus address, writes must be to the Physical address}
     Framebuffer.Size:=MailboxFramebuffer.Size; 
     Framebuffer.Pitch:=MailboxFramebuffer.Pitch; 
     Framebuffer.Depth:=MailboxFramebuffer.Depth;
     Framebuffer.PhysicalWidth:=MailboxFramebuffer.PhysicalWidth;
     Framebuffer.PhysicalHeight:=MailboxFramebuffer.PhysicalHeight;
     Framebuffer.VirtualWidth:=MailboxFramebuffer.VirtualWidth;
     Framebuffer.VirtualHeight:=MailboxFramebuffer.VirtualHeight;
     Framebuffer.OffsetX:=MailboxFramebuffer.OffsetX;
     Framebuffer.OffsetY:=MailboxFramebuffer.OffsetY;
     
     {Update Framebuffer} 
     RPi3FramebufferGetPixelOrder(Framebuffer.Order);
     RPi3FramebufferGetAlphaMode(Framebuffer.Mode);
     RPi3FramebufferGetOverscan(Framebuffer.OverscanTop,Framebuffer.OverscanBottom,Framebuffer.OverscanLeft,Framebuffer.OverscanRight);
      
     {Update Statistics}
     Inc(Framebuffer.AllocateCount);
    
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(MailboxFramebuffer);
    end;    
   finally
    {Set Default Display}
    if PRPi3Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
   
{==============================================================================}

function RPi3FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PRPi3Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPi3Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Release Framebuffer}
    Result:=RPi3FramebufferRelease;
    if Result <> ERROR_SUCCESS then Exit;
     
    {Update Statistics}
    Inc(Framebuffer.ReleaseCount);
     
    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    {Set Default Display}
    if PRPi3Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
   
{==============================================================================}
   
function RPi3FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Set Current Display}
 if PRPi3Framebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PRPi3Framebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Check Blank}
  if Blank then
   begin
    Result:=RPi3FramebufferSetState(0);
   end
  else
   begin
    Result:=RPi3FramebufferSetState(1);
   end;
 finally
  {Set Default Display}
  if PRPi3Framebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end; 
end;

{==============================================================================}

function RPi3FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Flags}
 if (not(BCM2710DMA_CACHE_COHERENT) or ((Flags and FRAMEBUFFER_TRANSFER_DMA) = 0)) and BCM2710FRAMEBUFFER_CACHED then
  begin
   {Clean Cache}
   CleanAndInvalidateDataCacheRange(Address,Size);
  end;
 
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function RPi3FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Set Current Display}
 if PRPi3Framebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PRPi3Framebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Set Backlight}
  Result:=FramebufferSetBacklight(Brightness);
 finally
  {Set Default Display}
  if PRPi3Framebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end; 
end; 
   
{==============================================================================}

function RPi3FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PRPi3Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPi3Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Not Supported}
    Result:=ERROR_NOT_SUPPORTED;
   finally
    {Set Default Display}
    if PRPi3Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
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
{RPi3 Helper Functions}
procedure RPi3Wait; assembler; nostackframe; 
{$IFDEF CPUARM}
asm
 //Wait for a period of time in a loop
 mov r0,#0x9F00000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi3LongWait; assembler; nostackframe; 
{$IFDEF CPUARM}
asm
 //Wait for a long period of time in a loop
 ldr r0,=0x3FF00000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi3ShortWait; assembler; nostackframe; 
{$IFDEF CPUARM}
asm
 //Wait for a short period of time in a loop
 mov r0,#0x1F0000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi3SlowBlink; assembler; nostackframe; 
{$IFDEF CPUARM}
asm
 //Slow blink the Activity LED in a loop
 bl RPi3ActivityLEDEnable
.LLoop:
 bl RPi3ActivityLEDOn
 bl RPi3Wait
 bl RPi3ActivityLEDOff
 bl RPi3Wait
 b .LLoop
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi3FastBlink; assembler; nostackframe; 
{$IFDEF CPUARM}
asm
 //Fast blink the Activity LED in a loop
 bl RPi3ActivityLEDEnable
.LLoop:
 bl RPi3ActivityLEDOn
 bl RPi3ShortWait
 bl RPi3ActivityLEDOff
 bl RPi3ShortWait
 b .LLoop
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi3BootBlink; assembler; nostackframe;
{Blink the Activity LED without dependency on any other RTL setup}
{$IFDEF CPUARM}
asm
 //Blink the Activity LED in a loop
 //Enable the Activity LED
 //To Do //Virtual GPIO on RPi3
 
.LLoop:
 //Turn on the Activity LED
 //To Do //Virtual GPIO on RPi3
 
 //Wait
 //--bl RPi3ShortWait
 bl RPi3Wait
 //--bl RPi3LongWait
 
 //Turn off the Activity LED
 //To Do //Virtual GPIO on RPi3
 
 //Wait
 //--bl RPi3ShortWait
 bl RPi3Wait
 //--bl RPi3LongWait
 b .LLoop
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}
{==============================================================================}

procedure RPi3BootOutput(Value:LongWord);
{Output characters to UART0 without dependency on any other RTL setup}
{Based on hexstrings() function by dwelch67 (https://github.com/dwelch67)}

{Note: This function is primarily intended for testing QEMU boot because
       it doesn't initialize the UART and won't work on real hardware}
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
    
   PLongWord(BCM2837_PL011_REGS_BASE)^:=Character;
   
   if Bits = 0 then Break;
  end;
 
 {Line End}
 PLongWord(BCM2837_PL011_REGS_BASE)^:=$0D;
 PLongWord(BCM2837_PL011_REGS_BASE)^:=$0A;
end;

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi3BootConsoleStart;
begin
 ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
end;

{==============================================================================}

procedure RPi3BootConsoleWrite(const Value:String);
begin
 ConsoleWindowWriteLn(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),Value);
end;

{==============================================================================}

procedure RPi3BootConsoleWriteEx(const Value:String;X,Y:LongWord);
begin
 ConsoleWindowSetXY(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),X,Y);
 ConsoleWindowWriteLn(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),Value);
end;

{==============================================================================}

function RPi3BootConsoleGetX:LongWord;
begin
 Result:=ConsoleWindowGetX(ConsoleWindowGetDefault(ConsoleDeviceGetDefault));
end;

{==============================================================================}

function RPi3BootConsoleGetY:LongWord;
begin
 Result:=ConsoleWindowGetY(ConsoleWindowGetDefault(ConsoleDeviceGetDefault));
end;
{$ENDIF}
{==============================================================================}

function RPi3ConvertPowerIdRequest(PowerId:LongWord):LongWord;
{Convert Ultibo Power Id to BCM2837 Power Id}
begin
 {}
 Result:=BCM2837_MBOX_POWER_DEVID_UNKNOWN;
 
 case PowerId of 
  POWER_ID_MMC0:Result:=BCM2837_MBOX_POWER_DEVID_SDHCI;
  POWER_ID_UART0:Result:=BCM2837_MBOX_POWER_DEVID_UART0;
  POWER_ID_UART1:Result:=BCM2837_MBOX_POWER_DEVID_UART1;
  POWER_ID_USB0:Result:=BCM2837_MBOX_POWER_DEVID_USB_HCD;
  POWER_ID_I2C0:Result:=BCM2837_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C1:Result:=BCM2837_MBOX_POWER_DEVID_I2C1;
  POWER_ID_I2C2:Result:=BCM2837_MBOX_POWER_DEVID_I2C2;
  POWER_ID_SPI0:Result:=BCM2837_MBOX_POWER_DEVID_SPI;
  POWER_ID_CCP2TX:Result:=BCM2837_MBOX_POWER_DEVID_CCP2TX;
 end;
end;

{==============================================================================}

function RPi3ConvertPowerStateRequest(PowerState:LongWord):LongWord;
{Convert Ultibo Power State to BCM2837 Power State}
begin
 {}
 Result:=BCM2837_MBOX_SET_POWER_STATE_REQ_OFF;
 
 case PowerState of 
  POWER_STATE_OFF:Result:=BCM2837_MBOX_SET_POWER_STATE_REQ_OFF;
  POWER_STATE_ON:Result:=BCM2837_MBOX_SET_POWER_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPi3ConvertPowerStateResponse(PowerState:LongWord):LongWord;
{Convert BCM2837 Power State to Ultibo Power State}
begin
 {}
 Result:=POWER_STATE_OFF;
 
 case PowerState of 
  BCM2837_MBOX_POWER_STATE_RESP_OFF:Result:=POWER_STATE_OFF;
  BCM2837_MBOX_POWER_STATE_RESP_ON:Result:=POWER_STATE_ON;
 end;
end;

{==============================================================================}

function RPi3ConvertClockIdRequest(ClockId:LongWord):LongWord;
{Convert Ultibo Clock Id to BCM2837 Clock Id}
begin
 {}
 Result:=BCM2837_MBOX_CLOCK_ID_UNKNOWN;
 
 case ClockId of 
  CLOCK_ID_MMC0:Result:=BCM2837_MBOX_CLOCK_ID_EMMC;
  CLOCK_ID_MMC1:Result:=BCM2837_MBOX_CLOCK_ID_CORE; {MMC1 runs from core clock}
  CLOCK_ID_UART0:Result:=BCM2837_MBOX_CLOCK_ID_UART;
  CLOCK_ID_UART1:Result:=BCM2837_MBOX_CLOCK_ID_CORE; {UART1 runs from core clock}
  CLOCK_ID_CPU:Result:=BCM2837_MBOX_CLOCK_ID_ARM;
  CLOCK_ID_CORE:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_GPU:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_V3D:Result:=BCM2837_MBOX_CLOCK_ID_V3D;
  CLOCK_ID_H264:Result:=BCM2837_MBOX_CLOCK_ID_H264;
  CLOCK_ID_ISP:Result:=BCM2837_MBOX_CLOCK_ID_ISP;
  CLOCK_ID_SDRAM:Result:=BCM2837_MBOX_CLOCK_ID_SDRAM;
  CLOCK_ID_PIXEL:Result:=BCM2837_MBOX_CLOCK_ID_PIXEL;
  CLOCK_ID_PWM0:Result:=BCM2837_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_PWM1:Result:=BCM2837_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_I2C0:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C1:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C2:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI0:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI1:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI2:Result:=BCM2837_MBOX_CLOCK_ID_CORE;
 end; 
end;

{==============================================================================}

function RPi3ConvertClockStateRequest(ClockState:LongWord):LongWord;
{Convert Ultibo Clock State to BCM2837 Clock State}
begin
 {}
 Result:=BCM2837_MBOX_SET_CLOCK_STATE_REQ_OFF;
 
 case ClockState of 
  CLOCK_STATE_OFF:Result:=BCM2837_MBOX_SET_CLOCK_STATE_REQ_OFF;
  CLOCK_STATE_ON:Result:=BCM2837_MBOX_SET_CLOCK_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPi3ConvertClockStateResponse(ClockState:LongWord):LongWord;
{Convert BCM2837 Clock State to Ultibo Clock State}
begin
 {}
 Result:=CLOCK_STATE_OFF;
 
 case ClockState of 
  BCM2837_MBOX_CLOCK_STATE_RESP_OFF:Result:=CLOCK_STATE_OFF;
  BCM2837_MBOX_CLOCK_STATE_RESP_ON:Result:=CLOCK_STATE_ON;
 end; 
end;

{==============================================================================}

function RPi3ConvertVoltageIdRequest(VoltageId:LongWord):LongWord;
{Convert Ultibo Voltage Id to BCM2837 Voltage Id}
begin
 {}
 Result:=BCM2837_MBOX_VOLTAGE_ID_RESERVED;
 
 case VoltageId of 
  VOLTAGE_ID_CORE:Result:=BCM2837_MBOX_VOLTAGE_ID_CORE;
  VOLTAGE_ID_SDRAM_C:Result:=BCM2837_MBOX_VOLTAGE_ID_SDRAM_C;
  VOLTAGE_ID_SDRAM_P:Result:=BCM2837_MBOX_VOLTAGE_ID_SDRAM_P;
  VOLTAGE_ID_SDRAM_I:Result:=BCM2837_MBOX_VOLTAGE_ID_SDRAM_I;
 end;
end;

{==============================================================================}

function RPi3ConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;
{Convert Ultibo Temperature Id to BCM2837 Temperature Id}
begin
 {}
 Result:=BCM2837_MBOX_TEMP_ID_SOC;
 
 case TemperatureId of 
  TEMPERATURE_ID_SOC:Result:=BCM2837_MBOX_TEMP_ID_SOC;
 end;
end;

{==============================================================================}
{==============================================================================}
{RPi3 Internal Functions}
function RPi3InterruptIsValid(Number:LongWord):Boolean;
begin
 {}
 {Check Number}
 Result:=(Number < IRQ_COUNT);
end;

{==============================================================================}

function RPi3InterruptIsLocal(Number:LongWord):Boolean;
begin
 {}
 {Check Number}
 Result:=(Number >= IRQ_LOCAL_START) and (Number < IRQ_COUNT);
end;

{==============================================================================}

function RPi3InterruptIsGlobal(Number:LongWord):Boolean;
begin
 {}
 {Check Number}
 Result:=(Number < IRQ_LOCAL_START);
end;

{==============================================================================}

function RPi3InterruptCheckValid(const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Count:LongWord;
begin
 {}
 Result:=False;
 
 {Check Flags}
 if Entry.IsLocal then
  begin
   {Check Number (Local)}
   if not RPi3InterruptIsLocal(Entry.Number) then Exit;
   
   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;
   
   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Entry.CPUMask) <> CPUGetCurrent then Exit;
  end
 else if Entry.IsIPI then
  begin
   {Software}
   Exit;
  end
 else 
  begin
   {Check Number (Global)}
   if not RPi3InterruptIsGlobal(Entry.Number) then Exit;
   
   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;
 
   {Check Mask CPU (Single CPU only)} 
   if (IRQ_ROUTING <> CPU_ID_ALL) and (IRQ_ROUTING <> CPUMaskToID(Entry.CPUMask)) then Exit;
  end;  
 
 {Check Handlers}
 if not RPi3InterruptCheckHandlers(Entry) then Exit;
  
 {Check Priority}
 {Not applicable}
 
 {Check FIQ}
 if Entry.IsFIQ then
  begin
   if not FIQ_ENABLED then Exit;
  end;
 
 {Check IPI}
 {Not applicable}
 
 {Check Shared}
 if Entry.IsShared then
  begin
   if not Assigned(Entry.SharedHandler) then Exit;
  end;
  
 Result:=True;
end;  

{==============================================================================}

function RPi3InterruptCheckHandlers(const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
begin
 {}
 Result:=False;
 
 {Check Handlers}
 if Assigned(Entry.Handler) then
  begin
   {Check Other Handlers}
   if Assigned(Entry.HandlerEx) then Exit;
   if Assigned(Entry.SharedHandler) then Exit;
   
   Result:=True;
  end
 else if Assigned(Entry.HandlerEx) then 
  begin
   {Check Other Handlers}
   if Assigned(Entry.Handler) then Exit;
   if Assigned(Entry.SharedHandler) then Exit;
   
   Result:=True;
  end
 else if Assigned(Entry.SharedHandler) then
  begin
   {Check Other Handlers}
   if Assigned(Entry.Handler) then Exit;
   if Assigned(Entry.HandlerEx) then Exit;
   
   Result:=True;
  end;  
end;  

{==============================================================================}

function RPi3InterruptCompareHandlers(const Entry,Current:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
begin
 {}
 Result:=False;
 
 {Check Handlers}
 if Assigned(Entry.Handler) then
  begin
   {Check Current Handlers}
   if not Assigned(Current.Handler) then Exit;
   if @Entry.Handler <> @Current.Handler then Exit;
   
   Result:=True;
  end
 else if Assigned(Entry.HandlerEx) then 
  begin
   {Check Current Handlers}
   if not Assigned(Current.HandlerEx) then Exit;
   if @Entry.HandlerEx <> @Current.HandlerEx then Exit;
   
   Result:=True;
  end
 else if Assigned(Entry.SharedHandler) then
  begin
   {Check Current Handlers}
   if not Assigned(Current.SharedHandler) then Exit;
   if @Entry.SharedHandler <> @Current.SharedHandler then Exit;
   
   Result:=True;
  end;  
end;  

{==============================================================================}

function RPi3InterruptEnable(const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Group:LongWord;
 Offset:LongWord;
 Address:LongWord;
 Enable:LongWord;
begin
 {}
 Result:=False;
 
 {Get Group and Offset}
 if Entry.Number < 32 then
  begin
   Group:=0;
   Offset:=0;
   Address:=BCM2837_ARM_INTERRUPT_IRQ_ENABLE1;
  end
 else if Entry.Number < 64 then
  begin
   Group:=1;
   Offset:=32;
   Address:=BCM2837_ARM_INTERRUPT_IRQ_ENABLE2;
  end
 else if Entry.Number < 96 then
  begin
   Group:=2;
   Offset:=64;
   Address:=BCM2837_ARM_INTERRUPT_BASIC_ENABLE;
  end
 else
  begin
   Group:=LongWord(-1);
   Offset:=96;
   Address:=0;
  end;
 
 {Check Source}
 if Entry.Number < 96 then 
  begin
   {Global}
   if Entry.IsFIQ then
    begin
     {Check FIQ}
     if FIQEnabled <> LongWord(-1) then Exit; {FIQEnabled will be -1 when nothing enabled}
     
     {Check IRQ}
     if (IRQEnabled[Group] and (1 shl Entry.Number - Offset)) <> 0 then Exit;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Enable FIQ}
     InterruptRegisters.FIQ_control:=BCM2837_ARM_INTERRUPT_FIQ_ENABLE or (Entry.Number and BCM2837_ARM_INTERRUPT_FIQ_SOURCE);
     FIQEnabled:=Entry.Number;
    end
   else
    begin
     {Check FIQ}
     if FIQEnabled = Entry.Number then Exit; {FIQEnabled will be -1 when nothing enabled}
  
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
    
     {Enable IRQ}
     PLongWord(BCM2837_INTERRUPT_REGS_BASE + Address)^:=(1 shl (Entry.Number - Offset));
     IRQEnabled[Group]:=IRQEnabled[Group] or (1 shl (Entry.Number - Offset));
    end;
  end
 else
  begin
   {Local}
   if Entry.IsFIQ then
    begin
     {Check Local IRQ}
     if (LocalIRQEnabled[Entry.CPUID] and (1 shl (Entry.Number - Offset))) <> 0 then Exit;
    end
   else
    begin
     {Check Local FIQ}
     if (LocalFIQEnabled[Entry.CPUID] and (1 shl (Entry.Number - Offset))) <> 0 then Exit;
    end;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Check Number}
   case Entry.Number of
    BCM2837_IRQ_LOCAL_ARM_CNTPSIRQ:begin
      {Enable Physical Secure Timer}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSFIQ else Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_CNTPNSIRQ:begin
      {Enable Physical Non Secure Timer}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSFIQ else Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_CNTHPIRQ:begin
      {Enable Hypervisor Timer}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPFIQ else Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_CNTVIRQ:begin
      {Enable Virtual Timer}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTVFIQ else Enable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTVIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX0:begin
      {Enable Mailbox0}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0FIQ else Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX1:begin
      {Enable Mailbox1}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1FIQ else Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX2:begin
      {Enable Mailbox2}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2FIQ else Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX3:begin
      {Enable Mailbox3}
      if Entry.IsFIQ then Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3FIQ else Enable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_GPU:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PMU:begin
      {Enable Performance Monitors}
      if Entry.IsFIQ then Enable:=(Entry.CPUID + 4) else Enable:=Entry.CPUID;
      
      ARMLocalRegisters.PMInterruptRoutingSet:=(1 shl Enable);
     end;
    BCM2837_IRQ_LOCAL_ARM_AXI:begin
      {Enable AXI Outstanding Writes (CPU0 IRQ Only)}
      if Entry.IsFIQ then Exit;
      if Entry.CPUID <> CPU_ID_0 then Exit;
      
      ARMLocalRegisters.AXIOutstandingIRQ:=ARMLocalRegisters.AXIOutstandingIRQ or BCM2837_ARM_LOCAL_AXI_IRQ_ENABLE;
     end;
    BCM2837_IRQ_LOCAL_ARM_TIMER:begin
      {Enable Local Timer}
      if Entry.IsFIQ then Enable:=(Entry.CPUID + 4) else Enable:=Entry.CPUID;
      
      ARMLocalRegisters.LocalIntRouting0:=(ARMLocalRegisters.LocalIntRouting0 and not(7)) or Enable;
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL1:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL2:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL3:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL4:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL5:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL6:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL7:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL8:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL9:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL10:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL11:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL12:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL13:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL14:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL15:begin
      {Nothing}
     end;
   else
    begin
     Exit;
    end;
   end;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
    
   if Entry.IsFIQ then
    begin
     {Enable Local FIQ}
     LocalFIQEnabled[Entry.CPUID]:=LocalFIQEnabled[Entry.CPUID] or (1 shl (Entry.Number - Offset));
    end
   else
    begin
     {Enable Local IRQ}
     LocalIRQEnabled[Entry.CPUID]:=LocalIRQEnabled[Entry.CPUID] or (1 shl (Entry.Number - Offset));
    end; 
  end;

 Result:=True;
end;  

{==============================================================================}

function RPi3InterruptDisable(const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Group:LongWord;
 Offset:LongWord;
 Address:LongWord;
 Disable:LongWord;
begin
 {}
 Result:=False;
 
 {Get Group and Offset}
 if Entry.Number < 32 then
  begin
   Group:=0;
   Offset:=0;
   Address:=BCM2837_ARM_INTERRUPT_IRQ_DISABLE1;
  end
 else if Entry.Number < 64 then
  begin
   Group:=1;
   Offset:=32;
   Address:=BCM2837_ARM_INTERRUPT_IRQ_DISABLE2;
  end
 else if Entry.Number < 96 then
  begin
   Group:=2;
   Offset:=64;
   Address:=BCM2837_ARM_INTERRUPT_BASIC_DISABLE;
  end
 else
  begin
   Group:=LongWord(-1);
   Offset:=96;
   Address:=0;
  end;
 
 {Check Source}
 if Entry.Number < 96 then 
  begin
   {Global}
   if Entry.IsFIQ then
    begin
     {Check FIQ}
     if FIQEnabled <> Entry.Number then Exit; {FIQEnabled will be -1 when nothing enabled}
     
     {Check IRQ}
     if (IRQEnabled[Group] and (1 shl Entry.Number - Offset)) <> 0 then Exit;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Disable FIQ}
     InterruptRegisters.FIQ_control:=0;
     FIQEnabled:=LongWord(-1);
    end
   else
    begin
     {Check FIQ}
     if FIQEnabled = Entry.Number then Exit; {FIQEnabled will be -1 when nothing enabled}
    
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Disable IRQ}
     PLongWord(BCM2837_INTERRUPT_REGS_BASE + Address)^:=(1 shl (Entry.Number - Offset));
     IRQEnabled[Group]:=IRQEnabled[Group] and not(1 shl (Entry.Number - Offset));
    end;
  end
 else
  begin
   {Local}
   if Entry.IsFIQ then
    begin
     {Check Local IRQ}
     if (LocalIRQEnabled[Entry.CPUID] and (1 shl (Entry.Number - Offset))) <> 0 then Exit;
    end
   else
    begin
     {Check Local FIQ}
     if (LocalFIQEnabled[Entry.CPUID] and (1 shl (Entry.Number - Offset))) <> 0 then Exit;
    end;
    
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Check Number}
   case Entry.Number of
    BCM2837_IRQ_LOCAL_ARM_CNTPSIRQ:begin
      {Disable Physical Secure Timer}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSFIQ else Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_CNTPNSIRQ:begin
      {Disable Physical Non Secure Timer}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSFIQ else Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_CNTHPIRQ:begin
      {Disable Hypervisor Timer}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPFIQ else Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_CNTVIRQ:begin
      {Disable Virtual Timer}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTVFIQ else Disable:=BCM2837_ARM_LOCAL_TIMER_INT_CONTROL_CNTVIRQ;
      
      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX0:begin
      {Disable Mailbox0}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0FIQ else Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX1:begin
      {Disable Mailbox1}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1FIQ else Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX2:begin
      {Disable Mailbox2}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2FIQ else Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_MAILBOX3:begin
      {Disable Mailbox3}
      if Entry.IsFIQ then Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3FIQ else Disable:=BCM2837_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3IRQ;
      
      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_GPU:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PMU:begin
      {Disable Performance Monitors}
      if Entry.IsFIQ then Disable:=(Entry.CPUID + 4) else Disable:=Entry.CPUID;
      
      ARMLocalRegisters.PMInterruptRoutingClear:=(1 shl Disable);
     end;
    BCM2837_IRQ_LOCAL_ARM_AXI:begin
      {Disable AXI Outstanding Writes (CPU0 IRQ Only)}
      if Entry.IsFIQ then Exit;
      if Entry.CPUID <> CPU_ID_0 then Exit;
      
      ARMLocalRegisters.AXIOutstandingIRQ:=ARMLocalRegisters.AXIOutstandingIRQ and not(BCM2837_ARM_LOCAL_AXI_IRQ_ENABLE);
     end;
    BCM2837_IRQ_LOCAL_ARM_TIMER:begin
      {Disable Local Timer}
      ARMLocalRegisters.LocalIntRouting0:=ARMLocalRegisters.LocalIntRouting0 and not(7);
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL1:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL2:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL3:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL4:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL5:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL6:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL7:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL8:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL9:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL10:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL11:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL12:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL13:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL14:begin
      {Nothing}
     end;
    BCM2837_IRQ_LOCAL_ARM_PERIPHERAL15:begin
      {Nothing}
     end;
   else
    begin
     Exit;
    end;
   end;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
    
   if Entry.IsFIQ then
    begin
     {Disable Local FIQ}
     LocalFIQEnabled[Entry.CPUID]:=LocalFIQEnabled[Entry.CPUID] and not(1 shl (Entry.Number - Offset));
    end
   else
    begin
     {Disable Local IRQ}
     LocalIRQEnabled[Entry.CPUID]:=LocalIRQEnabled[Entry.CPUID] and not(1 shl (Entry.Number - Offset));
    end; 
  end;

 Result:=True;
end;  

{==============================================================================}

function RPi3InterruptGetCurrentCount(CPUID,Number:LongWord):LongWord;
{Note: Caller must hold the interrupt lock}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=0;
 
 {Setup Defaults}
 Entry:=nil;

 {Check Number}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI3_CPU_COUNT - 1 then Exit;
   
   {Count Local}
   Entry:=LocalInterruptEntries[Number,CPUID];
  end
 else if RPi3InterruptIsGlobal(Number) then
  begin
   {Count Global}
   Entry:=InterruptEntries[Number];
  end;
  
 {Count Entries}
 while Entry <> nil do
  begin
   Inc(Result);
   
   {Get Next}
   Entry:=Entry.Next;
  end;
end;  

{==============================================================================}

function RPi3InterruptGetCurrentEntry(CPUID,Number:LongWord;Index:LongWord):PInterruptEntry;
{Note: Caller must hold the interrupt lock (or be within an interrupt handler)}
var
 Count:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=nil;
 
 {Setup Defaults}
 Entry:=nil;

 {Check Number}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI3_CPU_COUNT - 1 then Exit;
   
   {Count Local}
   Entry:=LocalInterruptEntries[Number,CPUID];
  end
 else if RPi3InterruptIsGlobal(Number) then
  begin
   {Count Global}
   Entry:=InterruptEntries[Number];
  end;
  
 {Get Entry} 
 Count:=0;
 while Entry <> nil do
  begin
   {Check Count}
   if Count = Index then
    begin
     Result:=Entry; 
     Exit;       
    end;

   Inc(Count);
   
   {Get Next}
   Entry:=Entry.Next;
  end;
end;  

{==============================================================================}

function RPi3InterruptAddCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=False;
 
 {Check Entry}
 if Entry = nil then Exit;

 {Check Number}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI3_CPU_COUNT - 1 then Exit;
   
   {Add Local}
   Current:=LocalInterruptEntries[Number,CPUID];
   if Current = nil then
    begin
      {Set Local}
      LocalInterruptEntries[Number,CPUID]:=Entry;
      
      Result:=True;
    end;  
  end
 else if RPi3InterruptIsGlobal(Number) then
  begin
   {Add Global}
   Current:=InterruptEntries[Number];
   if Current = nil then
    begin
     {Set Global}
     InterruptEntries[Number]:=Entry;
     
     Result:=True;
    end;
  end;
 
 {Check Current}
 if Current <> nil then
  begin
   {Find last}
   while Current.Next <> nil do
    begin
     {Get Next}
     Current:=Current.Next;
    end;
    
   {Add to end of list}
   Current.Next:=Entry;
   Entry.Prev:=Current;
   Entry.Next:=nil;
   
   Result:=True;
  end;
end;  

{==============================================================================}

function RPi3InterruptDeleteCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=False;
 
 {Check Entry}
 if Entry = nil then Exit;

 {Check Number}
 if RPi3InterruptIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI3_CPU_COUNT - 1 then Exit;
   
   {Delete Local}
   Current:=LocalInterruptEntries[Number,CPUID];
   if Current = Entry then
    begin
     LocalInterruptEntries[Number,CPUID]:=nil;
    end;
  end
 else if RPi3InterruptIsGlobal(Number) then
  begin
   {Delete Global}
   Current:=InterruptEntries[Number];
   if Current = Entry then
    begin
     InterruptEntries[Number]:=nil;
    end;
  end;
  
 {Check Current}
 if Current <> nil then
  begin
   {Find Entry}
   while Current <> nil do
    begin
     if Current = Entry then
      begin
       Break;
      end;
      
     {Get Next}
     Current:=Current.Next;
    end;
 
   {Check Current}
   if Current <> nil then
    begin
     {Remove from list}
     if Current.Prev <> nil then
      begin
       Current.Prev.Next:=Current.Next;
      end;
     if Current.Next <> nil then
      begin
       Current.Next.Prev:=Current.Prev;
      end;
     Current.Prev:=nil;
     Current.Next:=nil;
     
     {Free Entry}
     FreeMem(Current);
     
     Result:=True;
    end;
  end;
end;  

{==============================================================================}

function RPi3InterruptFindMatchingEntry(const Entry:TInterruptEntry):PInterruptEntry;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=nil;
 
 {Get Current}
 Current:=RPi3InterruptGetCurrentEntry(Entry.CPUID,Entry.Number,0);
 
 {Find Match}
 while Current <> nil do
  begin
   if RPi3InterruptCompareHandlers(Entry,Current^) then
    begin
     if Entry.Parameter = Current.Parameter then
      begin
       Result:=Current;
       Exit;
      end; 
    end;
    
   {Get Next}
   Current:=Current.Next;
  end;
end;  

{==============================================================================}

function RPi3InterruptGetEntry(CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord; 
{Note: The returned Entry is a copy of the registered value. Caller should free Entry if required}
{      For shared entries the Index parameter indicates which entry in the chain to return (0 equals first etc)}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Defaults}
 FillChar(Entry,SizeOf(TInterruptEntry),0);

 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Check Flags}
  if (Flags and INTERRUPT_FLAG_IPI) <> 0 then
   begin
    {Software Entry}
    Exit;
   end
  else if (Flags and INTERRUPT_FLAG_LOCAL) <> 0 then
   begin
    {Local Entry}
    if not RPi3InterruptIsLocal(Number) then Exit;
    
    {Check CPU}
    if CPUID > RPI3_CPU_COUNT - 1 then Exit;
   end
  else
   begin
    {Global Entry}
    if not RPi3InterruptIsGlobal(Number) then Exit;
   end;   
   
  Result:=ERROR_NOT_FOUND;  
  
  {Get Current}
  Current:=RPi3InterruptGetCurrentEntry(CPUID,Number,Index);
  if Current <> nil then
   begin
    {Copy Entry}
    Entry.Number:=Current.Number;
    Entry.Flags:=Current.Flags;
    Entry.CPUMask:=Current.CPUMask;
    Entry.Priority:=Current.Priority;    
    Entry.Handler:=Current.Handler;
    Entry.HandlerEx:=Current.HandlerEx;
    Entry.SharedHandler:=Current.SharedHandler;
    Entry.Parameter:=Current.Parameter;
    
    {Return Result}    
    Result:=ERROR_SUCCESS;
   end;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;  

{==============================================================================}

function RPi3InterruptRegisterEntry(const Entry:TInterruptEntry):LongWord;
{Note: Entry must be allocated from heap as a pointer to it will be retained while 
       the interrupt remains registered. Entry must not be freed by the caller}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Check Entry}
  if not RPi3InterruptCheckValid(Entry) then Exit;
  
  Result:=ERROR_ALREADY_ASSIGNED;
  
  {Check Count}
  if RPi3InterruptGetCurrentCount(Entry.CPUID,Entry.Number) = 0 then
   begin
    {Single Entry}
    Result:=ERROR_OPERATION_FAILED;
    
    {Enable IRQ/FIQ}
    if not RPi3InterruptEnable(Entry) then Exit;
    
    {Add Entry}
    if not RPi3InterruptAddCurrentEntry(Entry.CPUID,Entry.Number,@Entry) then Exit;
   end
  else
   begin
    {Shared Entry}
    Result:=ERROR_ALREADY_ASSIGNED;
    
    {Check Shared}
    if not Entry.IsShared then Exit;
    
    {Get Match}
    Current:=RPi3InterruptFindMatchingEntry(Entry);
    if Current <> nil then Exit;
    
    {Get Current}
    Current:=RPi3InterruptGetCurrentEntry(Entry.CPUID,Entry.Number,0);
    if Current = nil then Exit;
    
    {Check Shared}
    if not Current.IsShared then Exit;
    
    {Check FIQ}
    if Entry.IsFIQ <> Current.IsFIQ then Exit;
    
    Result:=ERROR_OPERATION_FAILED;
    
    {Add Entry}
    if not RPi3InterruptAddCurrentEntry(Entry.CPUID,Entry.Number,@Entry) then Exit;
   end;
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;  

{==============================================================================}

function RPi3InterruptDeregisterEntry(const Entry:TInterruptEntry):LongWord;
{Note: The Entry can be a local temporary copy allocated either from the stack or on
       the heap, this routine will free the original Entry passed to Register once it
       is successfully deregistered. Caller should free Entry if required}
var       
 Current:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Check Entry}
  if not RPi3InterruptCheckValid(Entry) then Exit;
 
  Result:=ERROR_NOT_ASSIGNED;
 
  {Get Match}
  Current:=RPi3InterruptFindMatchingEntry(Entry);
  if Current = nil then Exit;
  
  Result:=ERROR_OPERATION_FAILED;
  
  {Check Count}
  if RPi3InterruptGetCurrentCount(Entry.CPUID,Entry.Number) = 1 then
   begin
    {Single Entry}
    {Disable IRQ/FIQ}
    if not RPi3InterruptDisable(Entry) then Exit;
   end;

  {Delete Entry}
  if not RPi3InterruptDeleteCurrentEntry(Entry.CPUID,Entry.Number,Current) then Exit;
   
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;  

{==============================================================================}

function RPi3DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
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

 {$IF DEFINED(IRQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 Inc(DispatchInterruptCounter[CPUID]);
 {$ENDIF}
 
 {Check Local IRQ Enabled}
 if LocalIRQEnabled[CPUID] <> 0 then
  begin
   {Check Local IRQ Pending}
   IRQMatch:=(LocalIRQEnabled[CPUID] and ARMLocalRegisters.IRQPending[CPUID]);
   {Check IRQ Match}
   while IRQMatch <> 0 do
    begin
     {Find first set bit}
     IRQBit:=FirstBitSet(IRQMatch);  
       
     {Clear set bit}
     IRQMatch:=IRQMatch xor (1 shl IRQBit);
   
     {Call Interrupt Handler}
     Result:=RPi3HandleInterrupt(IRQBit + IRQ_LOCAL_START,CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
    end; 
  end;
 
 {Check IRQ Routing}
 if (IRQ_ROUTING = CPUID) or (IRQ_ROUTING = CPU_ID_ALL) then
  begin
   {Check IRQ Groups}
   for Group:=0 to 2 do
    begin
     {Check IRQ Enabled}
     if IRQEnabled[Group] <> 0 then
      begin
       case Group of
        {Check IRQ Pending 1}
        0:IRQMatch:=(IRQEnabled[Group] and InterruptRegisters.IRQ_pending_1);
        {Check IRQ Pending 2}
        1:IRQMatch:=(IRQEnabled[Group] and InterruptRegisters.IRQ_pending_2);
        {Check IRQ Basic Pending}
        2:IRQMatch:=(IRQEnabled[Group] and InterruptRegisters.IRQ_basic_pending);
       end; 
       {Check IRQ Match}
       while IRQMatch <> 0 do
        begin
         {Find first set bit}
         IRQBit:=FirstBitSet(IRQMatch); 
           
         {Clear set bit}
         IRQMatch:=IRQMatch xor (1 shl IRQBit);
           
         {Call Interrupt Handler}
         Result:=RPi3HandleInterrupt(IRQBit + (Group shl 5),CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
        end; 
      end;
    end;  
  end;
end;

{==============================================================================}

function RPi3DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending FIQ requests}
{Called by ARMv7/8FIQHandler in PlatformARMv7/8}
{Note: A DataMemoryBarrier is executed before and after calling this function} 
var
 FIQBit:LongWord;
 FIQMatch:LongWord;
begin
 {}
 Result:=Thread;
 
 {$IF DEFINED(FIQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 Inc(DispatchFastInterruptCounter[CPUID]);
 {$ENDIF}
 
 {Check Local FIQ Enabled}
 if LocalFIQEnabled[CPUID] <> 0 then
  begin
   {Check Local FIQ Pending}
   FIQMatch:=(LocalFIQEnabled[CPUID] and ARMLocalRegisters.FIQPending[CPUID]);
   {Check FIQ Match}
   while FIQMatch <> 0 do
    begin
     {Find first set bit}
     FIQBit:=FirstBitSet(FIQMatch);  
       
     {Clear set bit}
     FIQMatch:=FIQMatch xor (1 shl FIQBit);
   
     {Call Interrupt Handler}
     Result:=RPi3HandleInterrupt(FIQBit + IRQ_LOCAL_START,CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
    end; 
  end;
 
 {Check FIQ Routing}
 if (FIQ_ROUTING = CPUID) or (FIQ_ROUTING = CPU_ID_ALL) then
  begin
   {Check FIQ Enabled}
   if FIQEnabled <> LongWord(-1) then
    begin
     {Call Interrupt Handler}
     Result:=RPi3HandleInterrupt(FIQEnabled,CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
    end;
  end;
end;

{==============================================================================}

function RPi3HandleInterrupt(Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for an IRQ/FIQ that was received, or halt if it doesn't exist}
var
 Status:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;

 {Get Entry}
 Entry:=RPi3InterruptGetCurrentEntry(CPUID,Number,0);
 if Entry = nil then
  begin
   {Halt}
   {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('No entry registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
   {$ENDIF} 
     
   Halt;   
  end;
  
 {Check Entry}
 if not Entry.IsIPI then
  begin
   {Global or Local}
   if Entry.IsShared then
    begin
     {Shared}
     if not Assigned(Entry.SharedHandler) then
      begin
       {Halt}
       {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('No shared handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
       {$ENDIF} 
       
       Halt;
      end;
      
     {Call Handler}
     Status:=Entry.SharedHandler(Number,CPUID,Entry.Flags,Entry.Parameter);
     while Status <> INTERRUPT_RETURN_HANDLED do
      begin
       {Get Next}
       Entry:=Entry.Next;
       if Entry = nil then
        begin
         {Halt}
         {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('Unhandled interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
         {$ENDIF} 
         
         Halt;
        end;
       
       if not Assigned(Entry.SharedHandler) then
        begin
         {Halt}
         {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('No shared handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
         {$ENDIF} 
         
         Halt;
        end;
       
       {Call Handler}
       Status:=Entry.SharedHandler(Number,CPUID,Entry.Flags,Entry.Parameter);
      end;
    end
   else
    begin
     {Single}
     if Assigned(Entry.Handler) then
      begin
       {Call Handler}
       Entry.Handler(Entry.Parameter); 
      end
     else if Assigned(Entry.HandlerEx) then
      begin
       {Call Handler}
       Result:=Entry.HandlerEx(CPUID,Thread,Entry.Parameter);  
      end
     else if Assigned(Entry.SharedHandler) then
      begin
       {Call Handler}
       Entry.SharedHandler(Number,CPUID,Entry.Flags,Entry.Parameter);
      end
     else
      begin
       {Halt}
       {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('No handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
       {$ENDIF} 
       
       Halt;
      end;        
    end;
  end;
end;  

{==============================================================================}
{==============================================================================}

end.
