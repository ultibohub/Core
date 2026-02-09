{
Ultibo Platform interface unit for Raspberry Pi 4.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 ARMv8 (Cortex A72)

Boards
======

 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

  Linux - https://github.com/raspberrypi/linux

  Circle - https://github.com/rsta2/circle

References
==========

 BCM2711 ARM Peripherals

 QA7 Rev3.4

 Cortex-A8 MPCore Technical Reference Manual (Revision: r0p4)

 ARM v8 Architecture Reference Manual

 ARM Architecture Reference Manual (ARMv8-A)

 ARM Generic Interrupt Controller (Architecture version 2.0) - Architecture Specification

Platform RPi4
=============

 Notes: The RPi4B has the Activity LED connected to GPIO Pin 42 (Power LED is only accessible via the GPIO expander driver)

 Detecting the GIC or legacy interrupt controller
 ------------------------------------------------

 The Raspberry Pi 4 contains a new GIC interrupt controller which includes modern features like
 the ability to service interrupts on all available cores, a full set of software generated
 processor to processor interrupts and complete FIQ handling for all interrupt sources.

 The Pi 4 also retains and expands the legacy interrupt controller used on earlier Pi models
 with support for routing interrupts to any core and FIQ handling for all interrupt sources.

 The documentation at https://www.raspberrypi.org/documentation/configuration/config-txt/boot.md
 states that the setting enable_gic=0 or 1 can be used to select between the legacy and GIC
 interrupt controllers and that the GIC (enable_gic=1) is the default.

 Testing however shows this is not completely correct, if no config.txt file exists and no
 device tree DTB files are present then the current firmware appears to enable the legacy
 interrupt controller by default.

 If device tree DTB files containing a compatible GIC node are available on the SD card during
 boot the firmware will enable the GIC, this appears to happen regardless of the setting in
 config.txt and a value of enable_gic=0 will be ignored in favour of the device tree setting.

 If the legacy interrupt controller is enabled and Ultibo configures the GIC controller during
 boot it will receive no interrupts and appear to hang (four color screen).

 While it might be possible to read the SD card during boot, check for a config.txt and parse
 it to determine the value of the enable_gic setting, to be fully functional this process would
 also need to parse the device tree information (if available) and check for a compatible GIC
 node. Even then that doesn't account for booting from USB, network or some other source and
 all of that would have to happen very early in the boot sequence to be usable.

 To make matters worse there doesn't appear to be a documented way to detect which interrupt
 controller is enabled by reading a register value, the GIC even behaves as though it is working
 when the legacy controller is enabled but no interrupts are received.

 To ensure a level of reliability regardless of what firmware and files and configuration settings
 are present Ultibo implements drivers for both the GIC and legacy interrupt controllers and
 includes a detection mechanism during early stage boot.

 In simple terms the boot process configures the GIC and then installs an interrupt handler
 for the generic virtual timer and configures the timer to fire in 1ms before enabling
 interrupts. It then waits 2ms and disables interrupts, unconfigures the timer and deregisters
 the interrupt handler.

 If the test worked the local variable GICAvailable will have been set by the handler
 to RPI4_GIC_AVAILABLE, if not the variable will remain at RPI4_GIC_UNAVAILABLE and
 the boot process will proceed to configure the legacy interrupt controller for use.

 This is not an ideal mechanism and could potentially fail for other reasons (although
 it is carefully constructed to avoid many potential failures) but at present is the
 best option for allowing the boot process to succeed regardless of the firmware behavior.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit PlatformRPi4;
{$ENDIF FPC_DOTTEDUNITS}

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE ..\core\GlobalDefines.inc}

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Platforms.BCM2838,
  Core.Platform,
  {$IFDEF CPUARM}
  Platforms.PlatformARM,
  Platforms.PlatformARMv7,
  Platforms.PlatformARMv7l,
  {$ENDIF CPUARM}
  {$IFDEF CPUAARCH64}
  Platforms.PlatformAARCH64,
  Platforms.PlatformARMv8,
  {$ENDIF CPUAARCH64}
  Platforms.ARMGIC,
  Core.HeapManager,
  Core.Threads,
  {$IFDEF CONSOLE_EARLY_INIT}
  Core.Devices,
  Core.Framebuffer,
  Core.Console,
  {$ENDIF}
  {$IFDEF LOGGING_EARLY_INIT}
  Core.Logging,
  {$ENDIF}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  BCM2838,
  Platform,
  {$IFDEF CPUARM}
  PlatformARM,
  PlatformARMv7,
  PlatformARMv7L,
  {$ENDIF CPUARM}
  {$IFDEF CPUAARCH64}
  PlatformAARCH64,
  PlatformARMv8,
  {$ENDIF CPUAARCH64}
  ARMGIC,
  HeapManager,
  Threads,
  {$IFDEF CONSOLE_EARLY_INIT}
  Devices,
  Framebuffer,
  Console,
  {$ENDIF}
  {$IFDEF LOGGING_EARLY_INIT}
  Logging,
  {$ENDIF}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
const
 {RPi4 specific constants}

 {ARM Physical to VC IO Mapping}
 RPI4_VCIO_ALIAS = BCM2838_VCIO_ALIAS;     {The VCIO Alias (For ARM Physcial to VC IO translation)}

 {ARM Physical to VC Bus Mapping}
 RPI4_VCBUS_ALIAS = BCM2838_VCBUS_C_ALIAS; {The currently selected VCBUS Alias (For ARM Physcial to VC Bus translation)}

const
 {Secure World Boot}
 {RPI4_SECURE_BOOT = $00000001;}           {If 1 then startup will attempt to switch back to secure world during boot process} {Moved to ARMSecureBoot}
 RPI4_SECURE_BOOT_OFFSET = $000000D4;      {The address of the Secure Boot marker in the ARM boot stub}
 RPI4_SECURE_BOOT_MARKER = $58495052;      {The Secure Boot marker (ASCII "RPIX")}
 RPI4_SECURE_BOOT_CIRCLE = $53514946;      {The alternate Secure Boot marker (ASCII "FIQS") (As used by Circle)}

{const}
 {Address of StartupHandler on Reset}
 {RPI4_STARTUP_ADDRESS = $00008000;} {Obtain from linker}

const
 {Page Directory Address and Size}
 RPI4_PAGE_DIRECTORY_BASE = $00002000; {Place the first level Page Directory after the interrupt vectors at 0x00001000 and before the first level Page Table at 0x00004000}
 RPI4_PAGE_DIRECTORY_SIZE = SIZE_4K;   {ARM Cortex A72 first level Page Table is up to 4KB in size (512 64 bit (8 byte) entries)}

const
 {Page Table Address and Size}
 RPI4_PAGE_TABLE_BASE = $00004000;     {Place the second level Page Table after the first level Page Directory at 0x00002000 and before the code start at 0x00008000}
 RPI4_PAGE_TABLE_SIZE = SIZE_16K;      {ARM Cortex A72 second level Page Table is exactly 16KB in size (4 x 512 64 bit (8 byte) entries)}

const
 {Vector Table Address and Size}
 RPI4_VECTOR_TABLE_BASE  = $00001000;  {Place the Interrupt Vector Table at 0x00001000 before the code start at 0x00008000}
 RPI4_VECTOR_TABLE_SIZE  = SIZE_64;    {The Interrupt Vector Table is exactly 64 bytes (16 32 bit (4 byte) entries)}
 RPI4_VECTOR_TABLE_COUNT = 8;          {The Interrupt Vector Table contains 8 entries on an ARMv7 device}

const
 {CPU Count}
 RPI4_CPU_COUNT = BCM2838_CPU_COUNT;
 RPI4_CPU_BOOT = CPU_ID_0;
 RPI4_CPU_MASK = CPU_AFFINITY_0 or CPU_AFFINITY_1 or CPU_AFFINITY_2 or CPU_AFFINITY_3;

const
 {SWI}
 RPI4_SWI_COUNT = 256;                 {Number of available SWI entries}

const
 {Interrupt constants}
 RPI4_GIC_UNAVAILABLE = 0;
 RPI4_GIC_AVAILABLE   = 1;

const
 {Core Timer Prescaler}
 {$IFNDEF RPI4_MAX_CLOCK_RATE}
 RPI4_CORE_TIMER_PRESCALER    = $25ED098;  {Divide the Crystal Clock by 54 to give a 1MHz Core Timer}
 RPI4_CORE_TIMER_FREQUENCY    = 1000000;   {The Core Timer frequency from the prescaler setting above}
 RPI4_GENERIC_TIMER_FREQUENCY = 1000000;   {The ARM Generic Timer frequency from the prescaler setting above}
 {$ELSE}
 RPI4_CORE_TIMER_PRESCALER    = $80000000; {Divide the Crystal Clock by 1 to give a 54MHz Core Timer}
 RPI4_CORE_TIMER_FREQUENCY    = 54000000;  {The Core Timer frequency from the prescaler setting above}
 RPI4_GENERIC_TIMER_FREQUENCY = 54000000;  {The ARM Generic Timer frequency from the prescaler setting above}
 {$ENDIF}

const
 {Kernel Image Name}
 {$IFDEF CPUARM}
 RPI4_KERNEL_NAME = 'kernel7l.img';
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 RPI4_KERNEL_NAME = 'kernel8.img';
 {$ENDIF CPUAARCH64}
 RPI4_KERNEL_CONFIG = 'config.txt';
 RPI4_KERNEL_COMMAND = 'cmdline.txt';
 RPI4_FIRMWARE_FILES = 'start4.elf,fixup4.dat'{$IFDEF CPUARM} + ',armstub32-rpi4.bin'{$ENDIF CPUARM}{$IFDEF CPUAARCH64} + ',armstub64-rpi4.bin'{$ENDIF CPUAARCH64};
 RPI4_DTB_FILES = 'bcm2711-rpi-4-b.dtb,bcm2711-rpi-400.dtb,bcm2711-rpi-cm4.dtb,bcm2711-rpi-cm4s.dtb';

const
 {GPIO Activity LED constants (GPIO Pin 42)}
 RPI4_GPIO_ACTLED_GPFSEL = BCM2838_GPFSEL4;          {GPFSEL register for ACT LED}
 RPI4_GPIO_ACTLED_GPSET = BCM2838_GPSET1;            {GPSET register for ACT LED}
 RPI4_GPIO_ACTLED_GPCLR = BCM2838_GPCLR1;            {GPCLR register for ACT LED}
 RPI4_GPIO_ACTLED_GPPUD = BCM2838_GPPUD2;            {GPPUD register for ACT LED}

 RPI4_GPIO_ACTLED_GPFSHIFT = 6;                      {GPFSEL register shift for ACT LED}
 RPI4_GPIO_ACTLED_GPFMASK = BCM2838_GPFSEL_MASK;     {GPFSEL register mask for ACT LED}

 RPI4_GPIO_ACTLED_GPSHIFT = (42 - 32);               {GPSET/GPCLR register shift for ACT LED}
 RPI4_GPIO_ACTLED_GPMASK = BCM2838_GPSET_MASK;       {GPSET/GPCLR register mask for ACT LED}

 RPI4_GPIO_ACTLED_GPPUDSHIFT = 20;                   {GPPUD register shift for ACT LED}
 RPI4_GPIO_ACTLED_GPPUDMASK = BCM2838_GPPUD_MASK;    {GPPUD register mask for ACT LED}

const
 {Mailbox constants}
 RPI4_MAILBOX_TIMEOUT = 100;                         {Default timeout to wait for mailbox calls to complete (Milliseconds)}
 RPI4_MAILBOX_TIMEOUT_EX = 1000;                     {Extended timeout to wait for mailbox calls to complete (Milliseconds)}

const
 {Mailbox constants}
 RPI4_LOCAL_MAILBOX_TIMEOUT = 100;                   {Default timeout to wait for local mailbox calls to complete (Milliseconds)}

const
 {Framebuffer constants}
 RPI4_FRAMEBUFFER_DESCRIPTION = 'BCM2838 Framebuffer';

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
type
 {RPi4 specific types}
 PRPi4Framebuffer = ^TRPi4Framebuffer;
 TRPi4Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {RPi4 Properties}
  MultiDisplay:LongBool;
  DisplayNum:LongWord;
  DisplaySettings:TDisplaySettings;
 end;
{$ENDIF}
{==============================================================================}
var
 {RPi4 specific Ultibo variables}
 RPi4Initialized:Boolean;

 RPi4CNTVOFFLow:LongWord = 0;               {The low 32 bits of the Virtual Counter Offset register at boot time (CPU0 only) (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 RPi4CNTVOFFHigh:LongWord = 0;              {The high 32 bits of the Virtual Counter Offset register at boot time (CPU0 only) (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}

var
 {Timer Variables}
 TimerRegisters:PBCM2838SystemTimerRegisters;

var
 {Mailbox Variables}
 Mailbox0Registers:PBCM2838Mailbox0Registers;
 Mailbox1Registers:PBCM2838Mailbox1Registers;

var
 {Interrupt Variables}
 GICDevice:PGICDevice;
 GICAvailable:LongWord = RPI4_GIC_AVAILABLE; {The status of the GIC interrupt controller, determined during boot}

var
 {System Call Variables}
 SystemCallEntries:array[0..RPI4_SWI_COUNT - 1] of TSystemCallEntry;

var
 {Watchdog Variables}
 WatchdogRegisters:PBCM2838PMWatchdogRegisters;

var
 {ARM Local Variables}
 ARMLocalRegisters:PBCM2838ARMLocalRegisters;

var
 {Virtual GPIO Variables}
 VirtualGPIOBuffer:TBCM2838VirtualGPIOBuffer;

{==============================================================================}
{Initialization Functions}
procedure RPi4Init;

procedure RPi4SecondarySwitch;
procedure RPi4SecondarySecure;
procedure RPi4SecondaryHandler;

{==============================================================================}
{RPi4 Platform Functions}
procedure RPi4SMPInit;
procedure RPi4BoardInit;
procedure RPi4MemoryInit;
procedure RPi4ClockInit;
procedure RPi4PowerInit;
procedure RPi4MailboxInit;
procedure RPi4InterruptInit;
procedure RPi4PeripheralInit;
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi4FramebufferInit;
{$ENDIF}
procedure RPi4PageTableInit;
{$IFNDEF RPI4_ENABLE_LPAE}
procedure RPi4PageTableInitLegacy;
{$ENDIF RPI4_ENABLE_LPAE}

procedure RPi4PowerLEDEnable;
procedure RPi4PowerLEDOn;
procedure RPi4PowerLEDOff;

procedure RPi4ActivityLEDEnable;
procedure RPi4ActivityLEDOn;
procedure RPi4ActivityLEDOff;

function RPi4MailboxReceive(Mailbox,Channel:LongWord):LongWord;
procedure RPi4MailboxSend(Mailbox,Channel,Data:LongWord);

function RPi4MailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
function RPi4MailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord;
function RPi4MailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
function RPi4MailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord;

function RPi4MailboxPropertyTag(Tag:LongWord;Data:Pointer;Size:LongWord):LongWord;

function RPi4RequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function RPi4ReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function RPi4RequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function RPi4ReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function RPi4RequestIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord;
function RPi4ReleaseIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord;

function RPi4RegisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
function RPi4DeregisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;

function RPi4RegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
function RPi4DeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;

function RPi4GetInterruptEntry(Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
function RPi4GetLocalInterruptEntry(CPUID,Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
function RPi4GetSoftwareInterruptEntry(CPUID,Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
function RPi4GetSystemCallEntry(Number:LongWord):TSystemCallEntry;

function RPi4SystemRestart(Delay:LongWord):LongWord;
function RPi4SystemShutdown(Delay:LongWord):LongWord;
function RPi4SystemGetCommandLine:String;

function RPi4CPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;

function RPi4GPUGetState:LongWord;
function RPi4GPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;

function RPi4BoardGetModel:LongWord;
function RPi4BoardGetSerial:Int64;
function RPi4BoardGetRevision:LongWord;
function RPi4BoardGetMACAddress:String;

function RPi4ChipGetRevision:LongWord;

function RPi4FirmwareGetRevision:LongWord;
function RPi4FirmwareGetThrottled:LongWord;

function RPi4PowerGetWait(PowerId:LongWord):LongWord;
function RPi4PowerGetState(PowerId:LongWord):LongWord;
function RPi4PowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;

function RPi4ClockGetCount:LongWord;
function RPi4ClockGetTotal:Int64;

function RPi4ClockGetRate(ClockId:LongWord):LongWord;
function RPi4ClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;

function RPi4ClockGetState(ClockId:LongWord):LongWord;
function RPi4ClockSetState(ClockId,State:LongWord):LongWord;

function RPi4ClockGetMinRate(ClockId:LongWord):LongWord;
function RPi4ClockGetMaxRate(ClockId:LongWord):LongWord;

function RPi4ClockGetMeasuredRate(ClockId:LongWord):LongWord;

function RPi4TurboGetState(TurboId:LongWord):LongWord;
function RPi4TurboSetState(TurboId,State:LongWord):LongWord;

function RPi4VoltageGetValue(VoltageId:LongWord):LongWord;
function RPi4VoltageSetValue(VoltageId,Value:LongWord):LongWord;

function RPi4VoltageGetMinValue(VoltageId:LongWord):LongWord;
function RPi4VoltageGetMaxValue(VoltageId:LongWord):LongWord;

function RPi4TemperatureGetCurrent(TemperatureId:LongWord):LongWord;
function RPi4TemperatureGetMaximum(TemperatureId:LongWord):LongWord;

function RPi4GPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
function RPi4GPUMemoryRelease(Handle:THandle):LongWord;
function RPi4GPUMemoryLock(Handle:THandle):LongWord;
function RPi4GPUMemoryUnlock(Handle:THandle):LongWord;

function RPi4GPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;

function RPi4DispmanxHandleGet(Resource:THandle):THandle;
function RPi4EDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPi4FramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
function RPi4FramebufferRelease:LongWord;
function RPi4FramebufferSetState(State:LongWord):LongWord;

function RPi4FramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord;

function RPi4FramebufferGetPhysical(var Width,Height:LongWord):LongWord;
function RPi4FramebufferSetPhysical(var Width,Height:LongWord):LongWord;
function RPi4FramebufferTestPhysical(var Width,Height:LongWord):LongWord;

function RPi4FramebufferGetVirtual(var Width,Height:LongWord):LongWord;
function RPi4FramebufferSetVirtual(var Width,Height:LongWord):LongWord;
function RPi4FramebufferTestVirtual(var Width,Height:LongWord):LongWord;

function RPi4FramebufferGetDepth(var Depth:LongWord):LongWord;
function RPi4FramebufferSetDepth(var Depth:LongWord):LongWord;
function RPi4FramebufferTestDepth(var Depth:LongWord):LongWord;

function RPi4FramebufferGetPixelOrder(var Order:LongWord):LongWord;
function RPi4FramebufferSetPixelOrder(var Order:LongWord):LongWord;
function RPi4FramebufferTestPixelOrder(var Order:LongWord):LongWord;

function RPi4FramebufferGetAlphaMode(var Mode:LongWord):LongWord;
function RPi4FramebufferSetAlphaMode(var Mode:LongWord):LongWord;
function RPi4FramebufferTestAlphaMode(var Mode:LongWord):LongWord;

function RPi4FramebufferGetPitch:LongWord;

function RPi4FramebufferGetOffset(var X,Y:LongWord):LongWord;
function RPi4FramebufferSetOffset(var X,Y:LongWord):LongWord;
function RPi4FramebufferTestOffset(var X,Y:LongWord):LongWord;

function RPi4FramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPi4FramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPi4FramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;

function RPi4FramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
function RPi4FramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
function RPi4FramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPi4FramebufferGetLayer(var Layer:LongInt):LongWord;
function RPi4FramebufferSetLayer(var Layer:LongInt):LongWord;
function RPi4FramebufferTestLayer(var Layer:LongInt):LongWord;

function RPi4FramebufferTestVsync:LongWord;
function RPi4FramebufferSetVsync:LongWord;

function RPi4FramebufferSetBacklight(Brightness:LongWord):LongWord;

function RPi4FramebufferGetNumDisplays(var NumDisplays:LongWord):LongWord;
function RPi4FramebufferGetDisplayId(DisplayNum:LongWord):LongWord;
function RPi4FramebufferSetDisplayNum(DisplayNum:LongWord):LongWord;
function RPi4FramebufferGetDisplaySettings(DisplayNum:LongWord;var DisplaySettings:TDisplaySettings):LongWord;
function RPi4FramebufferDisplayIdToName(DisplayId:LongWord):String;

function RPi4TouchGetBuffer(var Address:PtrUInt):LongWord;
function RPi4TouchSetBuffer(Address:PtrUInt):LongWord;

function RPi4VirtualGPIOGetBuffer(var Address:PtrUInt):LongWord;
function RPi4VirtualGPIOSetBuffer(Address:PtrUInt):LongWord;

function RPi4CursorSetDefault:LongWord;
function RPi4CursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
function RPi4CursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;

function RPi4DMAGetChannels:LongWord;

function RPi4VirtualGPIOInputGet(Pin:LongWord):LongWord;
function RPi4VirtualGPIOOutputSet(Pin,Level:LongWord):LongWord;
function RPi4VirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord;

{==============================================================================}
{RPi4 Thread Functions}
procedure RPi4SchedulerInit;
procedure RPi4SchedulerStart(CPUID:LongWord);

procedure RPi4SecondaryBoot(CPUID:LongWord);

{==============================================================================}
{RPi4 SWI Functions}
function RPi4DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle;

{==============================================================================}
{RPi4 Clock Functions}
procedure RPi4ClockInterrupt(Parameter:Pointer);
procedure RPi4ClockUpdate(Cycles:LongWord;var Last:LongWord);

{==============================================================================}
{RPi4 Scheduler Functions}
function RPi4SchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
procedure RPi4SchedulerUpdate(Cycles:LongWord;var Last:LongWord);

procedure RPi4SchedulerSystemCall(Request:PSystemCallRequest);

{==============================================================================}
{RPi4 Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPi4FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPi4FramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPi4FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;

function RPi4FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function RPi4FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;

function RPi4FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{$ENDIF}
{==============================================================================}
{RPi4 Helper Functions}
procedure RPi4Wait;
procedure RPi4LongWait;
procedure RPi4ShortWait;

procedure RPi4SlowBlink;
procedure RPi4FastBlink;

procedure RPi4BootBlink;

procedure RPi4BootOutput(Value:LongWord);
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi4BootConsoleStart;
procedure RPi4BootConsoleWrite(const Value:String);
procedure RPi4BootConsoleWriteEx(const Value:String;X,Y:LongWord);
function RPi4BootConsoleGetX:LongWord;
function RPi4BootConsoleGetY:LongWord;
{$ENDIF}
function RPi4ConvertPowerIdRequest(PowerId:LongWord):LongWord;
function RPi4ConvertPowerStateRequest(PowerState:LongWord):LongWord;
function RPi4ConvertPowerStateResponse(PowerState:LongWord):LongWord;

function RPi4ConvertClockIdRequest(ClockId:LongWord):LongWord;
function RPi4ConvertClockStateRequest(ClockState:LongWord):LongWord;
function RPi4ConvertClockStateResponse(ClockState:LongWord):LongWord;

function RPi4ConvertVoltageIdRequest(VoltageId:LongWord):LongWord;

function RPi4ConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
const
 {Legacy IRQ/FIQ Start/Routing}
 RPI4_GPU_LEGACY_IRQ_COUNT          = 64; {Number of IRQs shared between the GPU and ARM (These correspond to the IRQs that show up in the IRQ_PENDING0 and IRQ_PENDING1 registers)}
 RPI4_ARM_LEGACY_IRQ_COUNT          = 32; {Number of ARM specific IRQs (These correspond to IRQs that show up in the first 16 bits of the IRQ_PENDING2 register)}
 RPI4_ARM_LOCAL_LEGACY_IRQ_COUNT    = 32; {Number of ARM local IRQs (These correspond to the IRQs that show up in the IRQSOURCE/FIQSOURCE registers of the ARM Local registers}
 RPI4_ARM_SOFTWARE_LEGACY_IRQ_COUNT = 8;  {Number of ARM software IRQs (These correspond to IRQs that show up in the second 8 bits of the IRQ_PENDING2 register)}

 RPI4_LEGACY_IRQ_COUNT = RPI4_GPU_LEGACY_IRQ_COUNT + RPI4_ARM_LEGACY_IRQ_COUNT + RPI4_ARM_LOCAL_LEGACY_IRQ_COUNT; {128} {Total number of IRQs available}
 RPI4_LEGACY_FIQ_COUNT = RPI4_GPU_LEGACY_IRQ_COUNT + RPI4_ARM_LEGACY_IRQ_COUNT + RPI4_ARM_LOCAL_LEGACY_IRQ_COUNT; {128} {Total number of FIQs available}

 RPI4_LEGACY_IRQ_START = 0; {System wide IRQs start at zero}

 RPI4_LEGACY_IRQ_ROUTING = CPU_ID_0; {Route system wide IRQs to CPU0}
 RPI4_LEGACY_FIQ_ROUTING = CPU_ID_0; {Route system wide FIQs to CPU0}

 RPI4_LEGACY_IRQ_LOCAL_START = RPI4_GPU_LEGACY_IRQ_COUNT + RPI4_ARM_LEGACY_IRQ_COUNT; {Local IRQs start after GPU and ARM IRQs}

 RPI4_LEGACY_IRQ_SOFTWARE_START = RPI4_GPU_LEGACY_IRQ_COUNT + 8; {Software IRQs start after GPU IRQs and first 8 ARM IRQs}

{==============================================================================}
{==============================================================================}
var
 {RPi4 specific variables}
 RPi4PageTableInitialized:Boolean;

 {Interrupt variables}
 GICStatus:LongWord;

var
 {Legacy Interrupt Variables}
 LegacyInterruptEntries:array[0..(RPI4_GPU_LEGACY_IRQ_COUNT + RPI4_ARM_LEGACY_IRQ_COUNT - 1)] of PInterruptEntry;
 LocalLegacyInterruptEntries:array[RPI4_LEGACY_IRQ_LOCAL_START..(RPI4_LEGACY_IRQ_COUNT - 1),0..(RPI4_CPU_COUNT - 1)] of PInterruptEntry;

var
 {Legacy IRQ/FIQ Variables}
 LegacyIRQEnabled:array[0..2] of LongWord; {3 groups of IRQs to Enable/Disable (See: TBCM2838InterruptRegisters)}
 LegacyFIQEnabled:array[0..2] of LongWord; {3 groups of FIQs to Enable/Disable (See: TBCM2838InterruptRegisters)}

 LegacyLocalIRQEnabled:array[0..(RPI4_CPU_COUNT - 1)] of LongWord; {1 group of local IRQs to Enable/Disable per CPU (See: TBCM2838ARMLocalRegisters)}
 LegacyLocalFIQEnabled:array[0..(RPI4_CPU_COUNT - 1)] of LongWord; {1 group of local FIQs to Enable/Disable per CPU (See: TBCM2838ARMLocalRegisters)}

{==============================================================================}
{==============================================================================}
{RPi4 Forward Declarations}
function RPi4TestGICAvailable:Boolean; forward;
procedure RPi4TestGICIRQHandler; forward;
function RPi4TestGICDummyHandler(Number,CPUID,Flags:LongWord;Parameter:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF} forward;

function RPi4RequestExLegacyIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; forward;
function RPi4ReleaseExLegacyIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; forward;

function RPi4RequestExLegacyFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; forward;
function RPi4ReleaseExLegacyFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; forward;

function RPi4RequestLegacyIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord; forward;
function RPi4ReleaseLegacyIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord; forward;

function RPi4RegisterLegacyInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord; forward;
function RPi4DeregisterLegacyInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord; forward;

function RPi4LegacyIsValid(Number:LongWord):Boolean; forward;
function RPi4LegacyIsLocal(Number:LongWord):Boolean; forward;
function RPi4LegacyIsSoftware(Number:LongWord):Boolean; forward;
function RPi4LegacyIsGlobal(Number:LongWord):Boolean; forward;

function RPi4MapETHPCIToVC(Number:LongWord):LongWord; forward;
function RPi4MapGICToLegacy(Number:LongWord):LongWord; forward;
function RPi4MapLegacyToGIC(CPUID,Number:LongWord):LongWord; forward;

function RPi4LegacyCheckValid(const Entry:TInterruptEntry):Boolean; forward;
function RPi4LegacyCheckHandlers(const Entry:TInterruptEntry):Boolean; forward;
function RPi4LegacyCompareHandlers(const Entry,Current:TInterruptEntry):Boolean; forward;

function RPi4LegacyEnable(const Entry:TInterruptEntry):Boolean; forward;
function RPi4LegacyDisable(const Entry:TInterruptEntry):Boolean; forward;

function RPi4LegacyGetCurrentCount(CPUID,Number:LongWord):LongWord; forward;
function RPi4LegacyGetCurrentEntry(CPUID,Number:LongWord;Index:LongWord):PInterruptEntry; forward;

function RPi4LegacyAddCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;
function RPi4LegacyDeleteCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;

function RPi4LegacyFindMatchingEntry(const Entry:TInterruptEntry):PInterruptEntry; forward;

function RPi4GetLegacyEntry(CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord; forward;
function RPi4RegisterLegacyEntry(const Entry:TInterruptEntry):LongWord; forward;
function RPi4DeregisterLegacyEntry(const Entry:TInterruptEntry):LongWord; forward;

function RPi4DispatchLegacyIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;
function RPi4DispatchLegacyFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;

function RPi4HandleLegacyInterrupt(Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RPi4Init;
var
 SchedulerFrequency:LongWord;
begin
 {}
 if RPi4Initialized then Exit;

 {Check for Emulator}
 {$IFDEF CPUARM}
 if PLongWord(BCM2838_GPIO_REGS_BASE + BCM2838_GPSET0)^ <> BCM2838_GPIO_SIGNATURE then ARMEmulatorMode:=1;
 {if PBCM2838ARMLocalRegisters(BCM2838_ARM_LOCAL_REGS_BASE).CoreTimerPrescaler = 0 then ARMEmulatorMode:=1;} {Alternate detection option for RPi4}
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 if PLongWord(BCM2838_GPIO_REGS_BASE + BCM2838_GPSET0)^ <> BCM2838_GPIO_SIGNATURE then AARCH64EmulatorMode:=1;
 {if PBCM2838ARMLocalRegisters(BCM2838_ARM_LOCAL_REGS_BASE).CoreTimerPrescaler = 0 then AARCH64EmulatorMode:=1;} {Alternate detection option for RPi4}
 {$ENDIF CPUAARCH64}

 {Setup IO_BASE/IO_ALIAS}
 IO_BASE:=BCM2838_PERIPHERALS_BASE;
 IO_ALIAS:=RPI4_VCIO_ALIAS;

 {Setup BUS_ALIAS}
 BUS_ALIAS:=RPI4_VCBUS_ALIAS;

 {Setup SECURE_BOOT}
 SECURE_BOOT:={$IFDEF CPUARM}(ARMSecureBoot <> 0){$ENDIF CPUARM}{$IFDEF CPUAARCH64}(AARCH64SecureBoot <> 0){$ENDIF CPUAARCH64};

 {Setup EMULATOR_MODE}
 EMULATOR_MODE:={$IFDEF CPUARM}(ARMEmulatorMode <> 0){$ENDIF CPUARM}{$IFDEF CPUAARCH64}(AARCH64EmulatorMode <> 0){$ENDIF CPUAARCH64};

 {Setup STARTUP_ADDRESS}
 STARTUP_ADDRESS:=PtrUInt(@_text_start); {RPI4_STARTUP_ADDRESS} {Obtain from linker}

 {Setup PERIPHERALS_BASE and SIZE}
 PERIPHERALS_BASE:=BCM2838_PERIPHERALS_BASE;
 PERIPHERALS_SIZE:=BCM2838_PERIPHERALS_SIZE;

 {Setup LOCAL_PERIPHERALS_BASE and SIZE}
 LOCAL_PERIPHERALS_BASE:=BCM2838_ARM_LOCAL_BASE;
 LOCAL_PERIPHERALS_SIZE:=BCM2838_ARM_LOCAL_SIZE;

 {Setup MEMORY_BASE and SIZE}
 {Done by RPi4MemoryInit}

 {Setup MEMORY_IRQ/FIQ/LOCAL/SHARED/DEVICE/NOCACHE/NONSHARED_SIZE}
 {Done by RPi4MemoryInit}

 {Setup PAGE_TABLE_BASE and SIZE}
 PAGE_DIRECTORY_BASE:=RPI4_PAGE_DIRECTORY_BASE;
 PAGE_DIRECTORY_SIZE:=RPI4_PAGE_DIRECTORY_SIZE;

 {Setup PAGE_TABLE_BASE and SIZE}
 PAGE_TABLE_BASE:=RPI4_PAGE_TABLE_BASE;
 PAGE_TABLE_SIZE:=RPI4_PAGE_TABLE_SIZE;

 {Setup VECTOR_TABLE_BASE, SIZE and COUNT}
 VECTOR_TABLE_BASE:=RPI4_VECTOR_TABLE_BASE;
 VECTOR_TABLE_SIZE:=RPI4_VECTOR_TABLE_SIZE;
 VECTOR_TABLE_COUNT:=RPI4_VECTOR_TABLE_COUNT;

 {Setup MACHINE_TYPE}
 MACHINE_TYPE:=MACHINE_TYPE_UNKNOWN;
 {$IFDEF CPUARM}
 case ARMMachineType of
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 case AARCH64MachineType of
 {$ENDIF CPUAARCH64}
  ARM_MACHINE_BCM2711:MACHINE_TYPE:=MACHINE_TYPE_BCM2711;
 end;

 {Setup BOARD_TYPE}
 {Done by RPi4BoardInit}

 {Setup CPU_ARCH, TYPE and COUNT}
 CPU_ARCH:=CPU_ARCH_ARM32;
 CPU_TYPE:=CPU_TYPE_ARMV8;
 CPU_COUNT:=RPI4_CPU_COUNT;
 CPU_BOOT:=RPI4_CPU_BOOT;
 CPU_MASK:=RPI4_CPU_MASK;
 CPU_MAX_COUNT:=RPI4_CPU_COUNT;

 {Setup CPU_MEMORY_BASE and SIZE}
 {Done by RPi4MemoryInit}

 {Setup CPU_MEMORY_RESTRICTED}
 CPU_MEMORY_RESTRICTED:=True;

 {Setup FPU_TYPE}
 FPU_TYPE:=FPU_TYPE_VFPV3;

 {Setup GPU_TYPE}
 GPU_TYPE:=GPU_TYPE_VC6;

 {Setup GPU_MEMORY_BASE and SIZE}
 {Done by RPi4MemoryInit}

 {Setup GPU_MEMORY_CACHED}
 GPU_MEMORY_CACHED:=True;

 {Setup IRQ/FIQ_COUNT/START/ROUTING}
 {Done by RPi4InterruptInit}
 SWI_COUNT:=RPI4_SWI_COUNT;

 {Setup IRQ/FIQ/IPI/SWI/UNDEF/ABORT_ENABLED}
 IRQ_ENABLED:=True;
 FIQ_ENABLED:=SECURE_BOOT; {GIC only allows FIQ for secure world}
 IPI_ENABLED:=True;
 SWI_ENABLED:=True;
 ABORT_ENABLED:=True;
 UNDEFINED_ENABLED:=True;

 {Setup IRQ/FIQ/SWI/UNDEF/ABORT_STACK_ENABLED}
 IRQ_STACK_ENABLED:=True;
 FIQ_STACK_ENABLED:=SECURE_BOOT; {GIC only allows FIQ for secure world}
 SWI_STACK_ENABLED:=True;
 ABORT_STACK_ENABLED:=True;
 UNDEFINED_STACK_ENABLED:=True;

 {Setup CLOCK_FREQUENCY/TICKS/CYCLES}
 {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
 CLOCK_FREQUENCY:=RPI4_GENERIC_TIMER_FREQUENCY;
 if EMULATOR_MODE then CLOCK_FREQUENCY:={$IFDEF CPUARM}ARMv7GetTimerFrequency{$ENDIF CPUARM}{$IFDEF CPUAARCH64}ARMv8GetTimerFrequency{$ENDIF CPUAARCH64};
 {$ELSE}
 CLOCK_FREQUENCY:=BCM2838_SYSTEM_TIMER_FREQUENCY;
 {$ENDIF}
 CLOCK_TICKS_PER_SECOND:=1000;
 CLOCK_TICKS_PER_MILLISECOND:=1;
 CLOCK_CYCLES_PER_TICK:=CLOCK_FREQUENCY div CLOCK_TICKS_PER_SECOND;
 CLOCK_CYCLES_PER_MILLISECOND:=CLOCK_FREQUENCY div MILLISECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_MICROSECOND:=CLOCK_FREQUENCY div MICROSECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_NANOSECOND:=CLOCK_FREQUENCY div NANOSECONDS_PER_SECOND;
 CLOCK_CYCLES_TOLERANCE:=CLOCK_CYCLES_PER_TICK div 10;
 TIME_TICKS_PER_CLOCK_INTERRUPT:=TIME_TICKS_PER_MILLISECOND div CLOCK_TICKS_PER_MILLISECOND;

 {Setup HEAP Behaviour}
 HEAP_NORMAL_SHARED:=True;

 {Setup SCHEDULER_INTERRUPTS/CLOCKS}
 SCHEDULER_INTERRUPTS_PER_SECOND:=2000;
 SCHEDULER_INTERRUPTS_PER_MILLISECOND:=2;
 SchedulerFrequency:=RPI4_GENERIC_TIMER_FREQUENCY;
 if EMULATOR_MODE then
  begin
   SCHEDULER_INTERRUPTS_PER_SECOND:=1000;   {Note: QEMU uses the timeGetDevCaps() function on Windows which returns wPeriodMin as 1 millisecond}
   SCHEDULER_INTERRUPTS_PER_MILLISECOND:=1; {      That means that any timer interval less then 1ms will not be honoured, the result will be 1ms}
   SchedulerFrequency:={$IFDEF CPUARM}ARMv7GetTimerFrequency{$ENDIF CPUARM}{$IFDEF CPUAARCH64}ARMv8GetTimerFrequency{$ENDIF CPUAARCH64};
  end;
 SCHEDULER_CLOCKS_PER_INTERRUPT:=SchedulerFrequency div SCHEDULER_INTERRUPTS_PER_SECOND;
 SCHEDULER_CLOCKS_TOLERANCE:=SCHEDULER_CLOCKS_PER_INTERRUPT div 10;
 TIME_TICKS_PER_SCHEDULER_INTERRUPT:=TIME_TICKS_PER_MILLISECOND div SCHEDULER_INTERRUPTS_PER_MILLISECOND;

 {Setup SCHEDULER_IDLE}
 SCHEDULER_IDLE_WAIT:=True;
 SCHEDULER_IDLE_OFFSET:=1;
 SCHEDULER_IDLE_PER_SECOND:=SCHEDULER_INTERRUPTS_PER_SECOND;

 {Setup KERNEL_NAME/CONFIG/COMMAND}
 KERNEL_NAME:=RPI4_KERNEL_NAME;
 KERNEL_CONFIG:=RPI4_KERNEL_CONFIG;
 KERNEL_COMMAND:=RPI4_KERNEL_COMMAND;
 FIRMWARE_FILES:=RPI4_FIRMWARE_FILES;
 DTB_FILES:=RPI4_DTB_FILES;

 {Setup GPIO (Set early to support activity LED)}
 GPIO_REGS_BASE:=BCM2838_GPIO_REGS_BASE;

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
 SMPInitHandler:=RPi4SMPInit;

 {Register Platform BoardInit Handler}
 BoardInitHandler:=RPi4BoardInit;

 {Register Platform MemoryInit Handler}
 MemoryInitHandler:=RPi4MemoryInit;

 {Register Platform ClockInit Handler}
 ClockInitHandler:=RPi4ClockInit;

 {Register Platform PowerInit Handler}
 PowerInitHandler:=RPi4PowerInit;

 {Register Platform MailboxInit Handler}
 MailboxInitHandler:=RPi4MailboxInit;

 {Register Platform InterruptInit Handler}
 InterruptInitHandler:=RPi4InterruptInit;

 {Register Platform PeripheralInit Handler}
 PeripheralInitHandler:=RPi4PeripheralInit;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Framebuffer FramebufferInit Handler}
 FramebufferInitHandler:=RPi4FramebufferInit;
 {$ENDIF}

 {$IFDEF CPUARM}
 {Register PlatformARMv7 PageTableInit Handler}
 ARMv7PageTableInitHandler:={$IFDEF RPI4_ENABLE_LPAE}RPi4PageTableInit{$ELSE RPI4_ENABLE_LPAE}RPi4PageTableInitLegacy{$ENDIF RPI4_ENABLE_LPAE};
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformARMv8 PageTableInit Handler}
 ARMv8PageTableInitHandler:=RPi4PageTableInit;
 {$ENDIF CPUAARCH64}

 {Register Platform Boot Blink Handlers}
 BootBlinkHandler:=RPi4BootBlink;
 BootOutputHandler:=RPi4BootOutput;

 {Register Platform Boot Console Handlers}
 {$IFDEF CONSOLE_EARLY_INIT}
 BootConsoleStartHandler:=RPi4BootConsoleStart;
 BootConsoleWriteHandler:=RPi4BootConsoleWrite;
 BootConsoleWriteExHandler:=RPi4BootConsoleWriteEx;
 BootConsoleGetXHandler:=RPi4BootConsoleGetX;
 BootConsoleGetYHandler:=RPi4BootConsoleGetY;
 {$ENDIF}

 {Register Platform LED Handlers}
 PowerLEDEnableHandler:=RPi4PowerLEDEnable;
 PowerLEDOnHandler:=RPi4PowerLEDOn;
 PowerLEDOffHandler:=RPi4PowerLEDOff;
 ActivityLEDEnableHandler:=RPi4ActivityLEDEnable;
 ActivityLEDOnHandler:=RPi4ActivityLEDOn;
 ActivityLEDOffHandler:=RPi4ActivityLEDOff;

 {Register Platform Mailbox Handlers}
 MailboxReceiveHandler:=RPi4MailboxReceive;
 MailboxSendHandler:=RPi4MailboxSend;
 MailboxCallHandler:=RPi4MailboxCall;
 MailboxCallExHandler:=RPi4MailboxCallEx;
 MailboxPropertyCallHandler:=RPi4MailboxPropertyCall;
 MailboxPropertyCallExHandler:=RPi4MailboxPropertyCallEx;
 MailboxPropertyTagHandler:=RPi4MailboxPropertyTag;

 {Register Platform IRQ Handlers}
 RequestExIRQHandler:=RPi4RequestExIRQ;
 ReleaseExIRQHandler:=RPi4ReleaseExIRQ;

 {Register Platform FIQ Handlers}
 RequestExFIQHandler:=RPi4RequestExFIQ;
 ReleaseExFIQHandler:=RPi4ReleaseExFIQ;

 {Register Platform IPI Handlers}
 RequestIPIHandler:=RPi4RequestIPI;
 ReleaseIPIHandler:=RPi4ReleaseIPI;

 {Register Platform Interrupt Handlers}
 RegisterInterruptHandler:=RPi4RegisterInterrupt;
 DeregisterInterruptHandler:=RPi4DeregisterInterrupt;

 {Register Platform System Call Handlers}
 RegisterSystemCallExHandler:=RPi4RegisterSystemCallEx;
 DeregisterSystemCallExHandler:=RPi4DeregisterSystemCallEx;

 {Register Platform Interrupt Handlers}
 GetInterruptEntryHandler:=RPi4GetInterruptEntry;

 {Register Platform Local Interrupt Handlers}
 GetLocalInterruptEntryHandler:=RPi4GetLocalInterruptEntry;

 {Register Platform Software Interrupt Handlers}
 GetSoftwareInterruptEntryHandler:=RPi4GetSoftwareInterruptEntry;

 {Register Platform System Call Handlers}
 GetSystemCallEntryHandler:=RPi4GetSystemCallEntry;

 {Register Platform System Handlers}
 SystemRestartHandler:=RPi4SystemRestart;
 SystemShutdownHandler:=RPi4SystemShutdown;
 SystemGetCommandLineHandler:=RPi4SystemGetCommandLine;

 {Register Platform CPU Handlers}
 CPUGetMemoryHandler:=RPi4CPUGetMemory;

 {Register Platform GPU Handlers}
 GPUGetStateHandler:=RPi4GPUGetState;
 GPUGetMemoryHandler:=RPi4GPUGetMemory;

 {Register Platform Board Handlers}
 BoardGetModelHandler:=RPi4BoardGetModel;
 BoardGetSerialHandler:=RPi4BoardGetSerial;
 BoardGetRevisionHandler:=RPi4BoardGetRevision;
 BoardGetMACAddressHandler:=RPi4BoardGetMACAddress;

 {Register Platform Chip Handlers}
 ChipGetRevisionHandler:=RPi4ChipGetRevision;

 {Register Platform Firmware Handlers}
 FirmwareGetRevisionHandler:=RPi4FirmwareGetRevision;
 FirmwareGetThrottledHandler:=RPi4FirmwareGetThrottled;

 {Register Platform Power Handlers}
 PowerGetWaitHandler:=RPi4PowerGetWait;
 PowerGetStateHandler:=RPi4PowerGetState;
 PowerSetStateHandler:=RPi4PowerSetState;

 {Register Platform Clock Handlers}
 ClockGetCountHandler:=RPi4ClockGetCount;
 ClockGetTotalHandler:=RPi4ClockGetTotal;

 ClockGetRateHandler:=RPi4ClockGetRate;
 ClockSetRateHandler:=RPi4ClockSetRate;

 ClockGetStateHandler:=RPi4ClockGetState;
 ClockSetStateHandler:=RPi4ClockSetState;

 ClockGetMinRateHandler:=RPi4ClockGetMinRate;
 ClockGetMaxRateHandler:=RPi4ClockGetMaxRate;

 ClockGetMeasuredRateHandler:=RPi4ClockGetMeasuredRate;

 {Register Platform Turbo Handlers}
 TurboGetStateHandler:=RPi4TurboGetState;
 TurboSetStateHandler:=RPi4TurboSetState;

 {Register Platform Voltage Handlers}
 VoltageGetValueHandler:=RPi4VoltageGetValue;
 VoltageSetValueHandler:=RPi4VoltageSetValue;
 VoltageGetMinValueHandler:=RPi4VoltageGetMinValue;
 VoltageGetMaxValueHandler:=RPi4VoltageGetMaxValue;

 {Register Platform Temperature Handlers}
 TemperatureGetCurrentHandler:=RPi4TemperatureGetCurrent;
 TemperatureGetMaximumHandler:=RPi4TemperatureGetMaximum;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPi4GPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPi4GPUMemoryRelease;
 GPUMemoryLockHandler:=RPi4GPUMemoryLock;
 GPUMemoryUnlockHandler:=RPi4GPUMemoryUnlock;

 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPi4GPUExecuteCode;
 DispmanxHandleGetHandler:=RPi4DispmanxHandleGet;
 EDIDBlockGetHandler:=RPi4EDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPi4FramebufferAllocate;
 FramebufferReleaseHandler:=RPi4FramebufferRelease;
 FramebufferSetStateHandler:=RPi4FramebufferSetState;

 FramebufferGetDimensionsHandler:=RPi4FramebufferGetDimensions;

 FramebufferGetPhysicalHandler:=RPi4FramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPi4FramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPi4FramebufferTestPhysical;

 FramebufferGetVirtualHandler:=RPi4FramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPi4FramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPi4FramebufferTestVirtual;

 FramebufferGetDepthHandler:=RPi4FramebufferGetDepth;
 FramebufferSetDepthHandler:=RPi4FramebufferSetDepth;
 FramebufferTestDepthHandler:=RPi4FramebufferTestDepth;

 FramebufferGetPixelOrderHandler:=RPi4FramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPi4FramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPi4FramebufferTestPixelOrder;

 FramebufferGetAlphaModeHandler:=RPi4FramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPi4FramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPi4FramebufferTestAlphaMode;

 FramebufferGetPitchHandler:=RPi4FramebufferGetPitch;

 FramebufferGetOffsetHandler:=RPi4FramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPi4FramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPi4FramebufferTestOffset;

 FramebufferGetOverscanHandler:=RPi4FramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPi4FramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPi4FramebufferTestOverscan;

 FramebufferGetPaletteHandler:=RPi4FramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPi4FramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPi4FramebufferTestPalette;

 FramebufferGetLayerHandler:=RPi4FramebufferGetLayer;
 FramebufferSetLayerHandler:=RPi4FramebufferSetLayer;
 FramebufferTestLayerHandler:=RPi4FramebufferTestLayer;

 FramebufferTestVsyncHandler:=RPi4FramebufferTestVsync;
 FramebufferSetVsyncHandler:=RPi4FramebufferSetVsync;

 FramebufferSetBacklightHandler:=RPi4FramebufferSetBacklight;

 FramebufferGetNumDisplaysHandler:=RPi4FramebufferGetNumDisplays;
 FramebufferGetDisplayIdHandler:=RPi4FramebufferGetDisplayId;
 FramebufferSetDisplayNumHandler:=RPi4FramebufferSetDisplayNum;
 FramebufferGetDisplaySettingsHandler:=RPi4FramebufferGetDisplaySettings;
 FramebufferDisplayIdToNameHandler:=RPi4FramebufferDisplayIdToName;

 {Register Platform Touch Handlers}
 TouchGetBufferHandler:=RPi4TouchGetBuffer;
 TouchSetBufferHandler:=RPi4TouchSetBuffer;

 {Register Platform Cursor Handlers}
 CursorSetDefaultHandler:=RPi4CursorSetDefault;
 CursorSetInfoHandler:=RPi4CursorSetInfo;
 CursorSetStateHandler:=RPi4CursorSetState;
 {$ENDIF}
 {Register Platform DMA Handlers}
 DMAGetChannelsHandler:=RPi4DMAGetChannels;

 {Register Platform Virtual GPIO Handlers}
 VirtualGPIOInputGetHandler:=RPi4VirtualGPIOInputGet;
 VirtualGPIOOutputSetHandler:=RPi4VirtualGPIOOutputSet;
 VirtualGPIOFunctionSelectHandler:=RPi4VirtualGPIOFunctionSelect;

 {Register Threads SchedulerInit Handler}
 SchedulerInitHandler:=RPi4SchedulerInit;
 SchedulerStartHandler:=RPi4SchedulerStart;

 {Register Threads SecondaryBoot Handler}
 SecondaryBootHandler:=RPi4SecondaryBoot;

 {$IFDEF CPUARM}
 {Register PlatformARMv7 SWI Handlers}
 ARMv7DispatchSWIHandler:=RPi4DispatchSWI;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformARMv8 SWI Handlers}
 ARMv8DispatchSWIHandler:=RPi4DispatchSWI;
 {$ENDIF CPUAARCH64}

 {$IFDEF CPUARM}
 {Register PlatformARM Helper Handlers}
 ARMWaitHandler:=RPi4Wait;
 ARMLongWaitHandler:=RPi4LongWait;
 ARMShortWaitHandler:=RPi4ShortWait;
 ARMSlowBlinkHandler:=RPi4SlowBlink;
 ARMFastBlinkHandler:=RPi4FastBlink;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformAARCH64 Helper Handlers}
 AARCH64WaitHandler:=RPi4Wait;
 AARCH64LongWaitHandler:=RPi4LongWait;
 AARCH64ShortWaitHandler:=RPi4ShortWait;
 AARCH64SlowBlinkHandler:=RPi4SlowBlink;
 AARCH64FastBlinkHandler:=RPi4FastBlink;
 {$ENDIF CPUAARCH64}

 RPi4Initialized:=True;
end;

{==============================================================================}

procedure RPi4SecondarySwitch; assembler; nostackframe;
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

procedure RPi4SecondarySecure; assembler; nostackframe;
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

procedure RPi4SecondaryHandler; assembler; nostackframe;
{Secondary CPU startup handler routine}
{$IFDEF CPUARM}
asm
 //Call the HYP mode switch handler in case the CPU is in HYP mode
 bl RPi4SecondarySwitch

 //Call the secure mode switch handler to return to secure mode
 bl RPi4SecondarySecure

 //Invalidate Instruction Cache before starting the boot process
 bl ARMv7InvalidateInstructionCache

 //Invalidate L1 Data Cache before starting the boot process
 bl ARMv7InvalidateL1DataCache

 //Flush the Branch Target Cache before starting the boot process
 bl ARMv7FlushBranchTargetCache

 //Invalidate the TLB before starting the boot process
 bl ARMv7InvalidateTLB

 //Change to SYS mode and ensure all interrupts are disabled
 //so the ARM processor is in a known state.
 cpsid if, #ARM_MODE_SYS

 //Set the Vector Base Address register in the System Control
 //register to the address of the vector table base above.
 mov r0, #RPI4_VECTOR_TABLE_BASE
 mcr p15, #0, r0, cr12, cr0, #0

 //Enable Unaligned Memory Accesses (U Bit) in the System Control
 //Register to simplify memory access routines from Pascal code.
 //
 //This would normally occur in CPUInit but is done here to allow
 //calls to Pascal code during initialization. (Always enabled in ARMv8)
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
 {$IFDEF RPI4_ENABLE_LPAE}
 bl ARMv7LStartMMU
 {$ELSE RPI4_ENABLE_LPAE}
 bl ARMv7StartMMU
 {$ENDIF RPI4_ENABLE_LPAE}

 //Initialize the Caches
 bl ARMv7CacheInit

 //Check the secure boot configuration
 ldr r0, .LARMSecureBoot
 ldr r0, [r0]
 cmp r0, #0
 beq .LNoTimer

 //Set the ARM Generic Timer Frequency
 ldr r0, =RPI4_GENERIC_TIMER_FREQUENCY
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

 //Check for GIC available
 ldr r0, .LGICAvailable
 ldr r0, [r0]
 cmp r0, #RPI4_GIC_UNAVAILABLE
 beq .LNoGIC

 //Get the GIC device
 ldr r0, .LGICDevice
 ldr r0, [r0]

 //Setup the CPU GIC interface (GIC Device in R0)
 bl ARMGICStart

.LNoGIC:

 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5;
 //Mask off the CPUID value
 and r0, #ARMV7_CP15_C0_MPID_CPUID_MASK

 //Branch to the CPU Start function (Current CPU in R0)
 bl SecondaryStart

 //If startup fails halt the CPU
 b ARMv7Halt

.LGICDevice:
  .long GICDevice
.LGICAvailable:
  .long GICAvailable

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
{RPi4 Platform Functions}
procedure RPi4SMPInit;
var
 Control:LongWord;
begin
 {}
 {Setup ARM Local Registers}
 ARMLocalRegisters:=PBCM2838ARMLocalRegisters(BCM2838_ARM_LOCAL_REGS_BASE);

 {Setup Core Timer Clock}
 Control:=ARMLocalRegisters.Control;
 Control:=Control and not(BCM2838_ARM_LOCAL_CONTROL_APB_CLOCK or BCM2838_ARM_LOCAL_CONTROL_INCREMENT_2); {Disable APB Clock and Increment 2}
 Control:=Control or BCM2838_ARM_LOCAL_CONTROL_CRYSTAL_CLOCK or BCM2838_ARM_LOCAL_CONTROL_INCREMENT_1;   {Enable Crystal Clock and Increment 1}
 ARMLocalRegisters.Control:=Control;

 {Setup Core Timer Prescaler}
 ARMLocalRegisters.CoreTimerPrescaler:=RPI4_CORE_TIMER_PRESCALER;

 {Setup ARM Generic Timer}
 {$IFDEF CPUARM}
 if SECURE_BOOT then ARMv7TimerInit(RPI4_GENERIC_TIMER_FREQUENCY);
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 if SECURE_BOOT then ARMv8TimerInit(RPI4_GENERIC_TIMER_FREQUENCY);
 {$ENDIF CPUAARCH64}
end;

{==============================================================================}

procedure RPi4BoardInit;
var
 Revision:LongWord;
 ClockRateMax:LongWord;
begin
 {}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then InterruptInit;

 {$IFDEF RPI4_CLOCK_SYSTEM_TIMER}
 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then ClockInit;
 {$ENDIF}

 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then MailboxInit;

 {Get Board Revision}
 Revision:=RPi4BoardGetRevision;

 {Get Board Type}
 if (Revision and BCM2838_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   case (Revision and BCM2838_BOARD_REVISION_MODEL_MASK) of
    BCM2838_BOARD_REVISION_MODEL_4B:begin
      BOARD_TYPE:=BOARD_TYPE_RPI4B;
     end;
    BCM2838_BOARD_REVISION_MODEL_400:begin
      BOARD_TYPE:=BOARD_TYPE_RPI400;
     end;
    BCM2838_BOARD_REVISION_MODEL_CM4,
    BCM2838_BOARD_REVISION_MODEL_CM4S:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE4;
     end;
   end;
  end;

 {Get CPU Clock Maximum}
 ClockRateMax:=RPi4ClockGetMaxRate(CLOCK_ID_CPU);
 if ClockRateMax > 0 then
  begin
   {Set CPU Clock}
   RPi4ClockSetRate(CLOCK_ID_CPU,ClockRateMax,True);
  end;

 {Note: As of firmware dated 19 March 2021 the clock rates reported by the
        firmware may not be accurate unless the ARM has set the rate first}
 {Get Core Clock Maximum}
 ClockRateMax:=RPi4ClockGetMaxRate(CLOCK_ID_CORE);
 if ClockRateMax > 0 then
  begin
   {Set Core Clock}
   RPi4ClockSetRate(CLOCK_ID_CORE,ClockRateMax,True);
  end;

 {Get V3D Clock Maximum}
 ClockRateMax:=RPi4ClockGetMaxRate(CLOCK_ID_V3D);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPi4ClockSetRate(CLOCK_ID_V3D,ClockRateMax,True);
  end;

 {Get H264 Clock Maximum}
 ClockRateMax:=RPi4ClockGetMaxRate(CLOCK_ID_H264);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPi4ClockSetRate(CLOCK_ID_H264,ClockRateMax,True);
  end;

 {Get ISP Clock Maximum}
 ClockRateMax:=RPi4ClockGetMaxRate(CLOCK_ID_ISP);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPi4ClockSetRate(CLOCK_ID_ISP,ClockRateMax,True);
  end;

 {Get SDRAM Clock Maximum}
 ClockRateMax:=RPi4ClockGetMaxRate(CLOCK_ID_SDRAM);
 if ClockRateMax > 0 then
  begin
   {Set SDRAM Clock}
   RPi4ClockSetRate(CLOCK_ID_SDRAM,ClockRateMax,True);
  end;
end;

{==============================================================================}

procedure RPi4MemoryInit;
var
 Address:PtrUInt;
 Length:UInt64;
 Revision:LongWord;
begin
 {}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then InterruptInit;

 {$IFDEF RPI4_CLOCK_SYSTEM_TIMER}
 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then ClockInit;
 {$ENDIF}

 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then MailboxInit;

 {Get Board Revision}
 Revision:=RPi4BoardGetRevision;

 {Check Board Revision}
 if (Revision and BCM2838_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   case (Revision and BCM2838_BOARD_REVISION_MODEL_MASK) of
    BCM2838_BOARD_REVISION_MODEL_4B,
    BCM2838_BOARD_REVISION_MODEL_400,
    BCM2838_BOARD_REVISION_MODEL_CM4,
    BCM2838_BOARD_REVISION_MODEL_CM4S:begin
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      case (Revision and BCM2838_BOARD_REVISION_MEMORY_MASK) of
       BCM2838_BOARD_REVISION_MEMORY_8192M:MEMORY_SIZE:=SIZE_8G; {Note: 32-bit Ultibo will only support the first 4GB but the whole 8GB can be accessed using page table mapping}
       BCM2838_BOARD_REVISION_MEMORY_4096M:MEMORY_SIZE:=SIZE_4G;
       BCM2838_BOARD_REVISION_MEMORY_2048M:MEMORY_SIZE:=SIZE_2G;
       BCM2838_BOARD_REVISION_MEMORY_1024M:MEMORY_SIZE:=SIZE_1G;
      else
       MEMORY_SIZE:=SIZE_1G;
      end;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      {Get Memory Section Size}
      MEMORY_SECTION_SIZE:=SIZE_2M;
      MEMORY_LARGESECTION_SIZE:=SIZE_1G;
      {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
      MEMORY_IRQ_SIZE:=SIZE_8M;
      if FIQ_ENABLED then MEMORY_FIQ_SIZE:=SIZE_8M else MEMORY_FIQ_SIZE:=SIZE_0;
      MEMORY_LOCAL_SIZE:=SIZE_8M;
      MEMORY_SHARED_SIZE:=SIZE_32M;
      MEMORY_DEVICE_SIZE:=SIZE_0; {was SIZE_8M}
      MEMORY_NOCACHE_SIZE:=SIZE_16M;
      MEMORY_NONSHARED_SIZE:=SIZE_8M;
     end;
   end;
  end;

 {Get CPU Memory}
 if RPi4CPUGetMemory(Address,Length) = ERROR_SUCCESS then
  begin
   CPU_MEMORY_BASE:=Address;
   CPU_MEMORY_SIZE:=Length;

   {Handle 256MB or less Memory (Missing fixup4.dat)}
   if CPU_MEMORY_SIZE < SIZE_256M then
    begin
     {Get Memory Base/Size (Assume 256MB default)}
     MEMORY_BASE:=$00000000;
     MEMORY_SIZE:=SIZE_256M;
     {Get Memory Page Size}
     MEMORY_PAGE_SIZE:=SIZE_4K;
     {Get Memory Section Size}
     MEMORY_SECTION_SIZE:=SIZE_2M;
     MEMORY_LARGESECTION_SIZE:=SIZE_1G;
     {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
     MEMORY_IRQ_SIZE:=SIZE_2M;
     if FIQ_ENABLED then MEMORY_FIQ_SIZE:=SIZE_2M else MEMORY_FIQ_SIZE:=SIZE_0;
     MEMORY_LOCAL_SIZE:=SIZE_2M;
     MEMORY_SHARED_SIZE:=SIZE_8M;
     MEMORY_DEVICE_SIZE:=SIZE_0;
     MEMORY_NOCACHE_SIZE:=SIZE_4M;
     MEMORY_NONSHARED_SIZE:=SIZE_2M;
    end;
  end;

 {Get GPU Memory}
 if RPi4GPUGetMemory(Address,Length) = ERROR_SUCCESS then
  begin
   GPU_MEMORY_BASE:=Address;
   GPU_MEMORY_SIZE:=Length;
  end;

 {Check SysCalls Heap Base}
 if SYSCALLS_HEAP_BASE >= BCM2838_PCI_OUTBOUND_BASE then
  begin
   {Reset to default}
   SYSCALLS_HEAP_BASE:=$C0000000;
  end;

 {Check SysCalls Heap Size}
 if (SYSCALLS_HEAP_BASE + SYSCALLS_HEAP_MAX) > BCM2838_PCI_OUTBOUND_BASE then
  begin
   Dec(SYSCALLS_HEAP_MAX,(SYSCALLS_HEAP_BASE + SYSCALLS_HEAP_MAX) - BCM2838_PCI_OUTBOUND_BASE);
  end;
end;

{==============================================================================}

procedure RPi4ClockInit;
{$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
var
 State:LongWord;
{$ENDIF}
begin
 {}
 {Setup Timer Registers}
 TimerRegisters:=PBCM2838SystemTimerRegisters(BCM2838_SYSTEM_TIMER_REGS_BASE);

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
   {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
   RequestFIQ(RPI4_CPU_BOOT,BCM2838_IRQ_LOCAL_ARM_CNTVIRQ,RPi4ClockInterrupt,nil);
   {$ELSE}
   RequestFIQ(RPI4_CPU_BOOT,BCM2838_IRQ_SYSTEM_TIMER_3,RPi4ClockInterrupt,nil);
   {$ENDIF}
  end
 else
  begin
   {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
   RequestIRQ(RPI4_CPU_BOOT,BCM2838_IRQ_LOCAL_ARM_CNTVIRQ,RPi4ClockInterrupt,nil);
   {$ELSE}
   RequestIRQ(RPI4_CPU_BOOT,BCM2838_IRQ_SYSTEM_TIMER_3,RPi4ClockInterrupt,nil);
   {$ENDIF}
  end;

 {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
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
 RPi4ClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
end;

{==============================================================================}

procedure RPi4PowerInit;
begin
 {}
 {Setup Watchdog Registers}
 WatchdogRegisters:=PBCM2838PMWatchdogRegisters(BCM2838_PM_REGS_BASE);
end;

{==============================================================================}

procedure RPi4MailboxInit;
begin
 {}
 {Setup Mailbox0/1 Registers}
 Mailbox0Registers:=PBCM2838Mailbox0Registers(BCM2838_MAILBOX0_REGS_BASE);
 Mailbox1Registers:=PBCM2838Mailbox1Registers(BCM2838_MAILBOX1_REGS_BASE);
end;

{==============================================================================}

procedure RPi4InterruptInit;
var
 Count:LongWord;
 Counter:LongWord;
begin
 {}
 {$IFDEF CPUARM}
 {Register PlatformARMv7 IRQ Handlers}
 ARMv7DispatchIRQHandler:=ARMGICDispatchIRQ;

 {Register PlatformARMv7 FIQ Handlers}
 ARMv7DispatchFIQHandler:=ARMGICDispatchFIQ;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 {Register PlatformARMv8 IRQ Handlers}
 ARMv8DispatchIRQHandler:=ARMGICDispatchIRQ;

 {Register PlatformARMv8 FIQ Handlers}
 ARMv8DispatchFIQHandler:=ARMGICDispatchFIQ;
 {$ENDIF CPUAARCH64}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear Legacy IRQ/FIQ Enabled}
 for Count:=0 to RPI4_CPU_COUNT - 1 do
  begin
   {IRQ EN0/1/2}
   PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_CLR_EN_0 + (Count * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=$FFFFFFFF;
   PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_CLR_EN_1 + (Count * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=$FFFFFFFF;
   PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_CLR_EN_2 + (Count * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=$FFFFFFFF;
   {FIQ EN0/1/2}
   PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_CLR_EN_0 + (Count * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=$FFFFFFFF;
   PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_CLR_EN_1 + (Count * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=$FFFFFFFF;
   PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_CLR_EN_2 + (Count * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=$FFFFFFFF;
  end;

 {Create GIC}
 GICDevice:=ARMGICCreate(BCM2838_GICDIST_REGS_BASE,BCM2838_GICCPU_REGS_BASE);
 if GICDevice <> nil then
  begin
   {Start GIC}
   ARMGICStart(GICDevice);

   {Test GIC}
   if RPi4TestGICAvailable then
    begin
     {Mark GIC Available}
     GICAvailable:=RPI4_GIC_AVAILABLE;

     {Setup IRQ/FIQ_COUNT/START/ROUTING}
     IRQ_COUNT:=ARMGICGetIRQCount(GICDevice);
     FIQ_COUNT:=ARMGICGetFIQCount(GICDevice);

     IRQ_START:=ARMGICGetIRQStart(GICDevice);

     IRQ_ROUTING:=ARMGICGetIRQRouting(GICDevice);
     FIQ_ROUTING:=ARMGICGetFIQRouting(GICDevice);

     IRQ_LOCAL_COUNT:=ARMGICGetIRQLocalCount(GICDevice);
     FIQ_LOCAL_COUNT:=ARMGICGetFIQLocalCount(GICDevice);

     IRQ_LOCAL_START:=ARMGICGetIRQLocalStart(GICDevice);

     IRQ_SOFTWARE_COUNT:=ARMGICGetIRQSoftwareCount(GICDevice);
     FIQ_SOFTWARE_COUNT:=ARMGICGetFIQSoftwareCount(GICDevice);

     IRQ_SOFTWARE_START:=ARMGICGetIRQSoftwateStart(GICDevice);

     {Check FIQ Enabled}
     FIQ_ENABLED:=(FIQ_COUNT > 0) and SECURE_BOOT;
    end
   else
    begin
     {Mark GIC Unavailable}
     GICAvailable:=RPI4_GIC_UNAVAILABLE;

     {Setup Legacy IRQ/FIQ_COUNT/START/ROUTING}
     IRQ_COUNT:=RPI4_LEGACY_IRQ_COUNT;
     FIQ_COUNT:=RPI4_LEGACY_FIQ_COUNT;

     IRQ_START:=RPI4_LEGACY_IRQ_START;

     IRQ_ROUTING:=RPI4_LEGACY_IRQ_ROUTING;
     FIQ_ROUTING:=RPI4_LEGACY_FIQ_ROUTING;

     IRQ_LOCAL_COUNT:=RPI4_ARM_LOCAL_LEGACY_IRQ_COUNT;
     FIQ_LOCAL_COUNT:=RPI4_ARM_LOCAL_LEGACY_IRQ_COUNT;

     IRQ_LOCAL_START:=RPI4_LEGACY_IRQ_LOCAL_START;

     IRQ_SOFTWARE_COUNT:=RPI4_ARM_SOFTWARE_LEGACY_IRQ_COUNT;
     FIQ_SOFTWARE_COUNT:=RPI4_ARM_SOFTWARE_LEGACY_IRQ_COUNT;

     IRQ_SOFTWARE_START:=RPI4_LEGACY_IRQ_SOFTWARE_START;

     FIQ_ENABLED:=True;

     {Register Platform Legacy IRQ Handlers}
     RequestExIRQHandler:=RPi4RequestExLegacyIRQ;
     ReleaseExIRQHandler:=RPi4ReleaseExLegacyIRQ;

     {Register Platform Legacy FIQ Handlers}
     RequestExFIQHandler:=RPi4RequestExLegacyFIQ;
     ReleaseExFIQHandler:=RPi4ReleaseExLegacyFIQ;

     {Register Platform Legacy IPI Handlers}
     RequestIPIHandler:=RPi4RequestLegacyIPI;
     ReleaseIPIHandler:=RPi4ReleaseLegacyIPI;

     {Register Platform Legacy Interrupt Handlers}
     RegisterInterruptHandler:=RPi4RegisterLegacyInterrupt;
     DeregisterInterruptHandler:=RPi4DeregisterLegacyInterrupt;

     {$IFDEF CPUARM}
     {Register PlatformARMv7 Legacy IRQ Handlers}
     ARMv7DispatchIRQHandler:=RPi4DispatchLegacyIRQ;

     {Register PlatformARMv7 Legacy FIQ Handlers}
     ARMv7DispatchFIQHandler:=RPi4DispatchLegacyFIQ;
     {$ENDIF CPUARM}
     {$IFDEF CPUAARCH64}
     {Register PlatformARMv8 Legacy IRQ Handlers}
     ARMv8DispatchIRQHandler:=RPi4DispatchLegacyIRQ;

     {Register PlatformARMv8 Legacy FIQ Handlers}
     ARMv8DispatchFIQHandler:=RPi4DispatchLegacyFIQ;
     {$ENDIF CPUAARCH64}

     {Setup Legacy Interrupt Entries}
     for Count:=0 to RPI4_GPU_LEGACY_IRQ_COUNT + RPI4_ARM_LEGACY_IRQ_COUNT - 1 do
      begin
       LegacyInterruptEntries[Count]:=nil;
      end;

     {Setup Local Legacy Interrupt Entries}
     for Count:=RPI4_LEGACY_IRQ_LOCAL_START to RPI4_LEGACY_IRQ_COUNT - 1 do
      begin
       for Counter:=0 to RPI4_CPU_COUNT - 1 do
        begin
         LocalLegacyInterruptEntries[Count,Counter]:=nil;
        end;
      end;

     {Setup Enabled Legacy IRQ/FIQs}
     for Count:=0 to 2 do {Number of elements in LegacyIRQEnabled}
      begin
       LegacyIRQEnabled[Count]:=0;
       LegacyFIQEnabled[Count]:=0;
      end;

     {Setup Local Enabled Legacy IRQ/FIQs}
     for Count:=0 to RPI4_CPU_COUNT - 1 do
      begin
       LegacyLocalIRQEnabled[Count]:=0;
       LegacyLocalFIQEnabled[Count]:=0;
      end;
    end;
  end;

 {Setup System Call Entries}
 for Count:=0 to RPI4_SWI_COUNT - 1 do
  begin
   FillChar(SystemCallEntries[Count],SizeOf(TSystemCallEntry),0);

   SystemCallEntries[Count].Number:=Count;
   SystemCallEntries[Count].CPUID:=CPU_ID_ALL;
  end;
end;

{==============================================================================}

procedure RPi4PeripheralInit;
var
 CacheLineSize:LongWord;
begin
 {}
 {Get Cache Line Size}
 CacheLineSize:=Max(L1DataCacheGetLineSize,L2CacheGetLineSize);

 {Setup Peripherals}
 INTERRUPT_REGS_BASE:=BCM2838_INTERRUPT_REGS_BASE;
 SYSTEMTIMER_REGS_BASE:=BCM2838_SYSTEM_TIMER_REGS_BASE;
 TIMER_REGS_BASE:=BCM2838_TIMER_REGS_BASE;
 GPIO_REGS_BASE:=BCM2838_GPIO_REGS_BASE;
 UART_REGS_BASE:=BCM2838_PL011_REGS_BASE;

 {Setup GPIO}
 GPIO_PIN_COUNT:=BCM2838_GPIO_PIN_COUNT;

 {Setup Virtual GPIO}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    VIRTUAL_GPIO_PIN_COUNT:=BCM2838_VIRTUAL_GPIO_PIN_COUNT;
   end;
 end;

 {Setup LEDs}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    {Activity LED}
    ACTIVITY_LED_PIN:=GPIO_PIN_42;
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

 {Setup BCM2711}
 BCM2711DMA_ALIGNMENT:=SizeOf(LongWord);
 BCM2711DMA_MULTIPLIER:=SizeOf(LongWord);
 BCM2711DMA_SHARED_MEMORY:=False;
 BCM2711DMA_NOCACHE_MEMORY:=False;
 BCM2711DMA_BUS_ADDRESSES:=True;
 BCM2711DMA_CACHE_COHERENT:=False;
 if CacheLineSize > BCM2711DMA_ALIGNMENT then BCM2711DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > BCM2711DMA_MULTIPLIER then BCM2711DMA_MULTIPLIER:=CacheLineSize;

 BCM2711FRAMEBUFFER_ALIGNMENT:=SIZE_256;
 BCM2711FRAMEBUFFER_CACHED:=GPU_MEMORY_CACHED;

 {Setup RTC and XHCI}
 case BOARD_TYPE of
  BOARD_TYPE_RPI_COMPUTE4:begin
    {Check RTC}
    if BCM2711_REGISTER_RTC then
     begin
      {Enable I2C0}
      BCM2711_REGISTER_I2C0:=True;
     end;

    {Switch to Internal XHCI}
    BCM2711_REGISTER_PCI_XHCI:=False;
    BCM2711_REGISTER_INTERNAL_XHCI:=True;
   end;
  else
   begin
    {Disable RTC (CM4 only)}
    BCM2711_REGISTER_RTC:=False;
   end;
 end;

 {Setup DWCOTG}
 DWCOTG_IRQ:=BCM2838_IRQ_USB;
 DWCOTG_POWER_ID:=POWER_ID_USB0;
 DWCOTG_REGS_BASE:=BCM2838_USB_REGS_BASE;
 DWCOTG_DMA_ALIGNMENT:=SizeOf(LongWord);
 DWCOTG_DMA_MULTIPLIER:=SizeOf(LongWord);
 DWCOTG_DMA_SHARED_MEMORY:=False;
 DWCOTG_DMA_NOCACHE_MEMORY:=False;
 DWCOTG_DMA_BUS_ADDRESSES:=True;
 DWCOTG_DMA_CACHE_COHERENT:=False;
 DWCOTG_HOST_FRAME_INTERVAL:=True;
 if CacheLineSize > DWCOTG_DMA_ALIGNMENT then DWCOTG_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > DWCOTG_DMA_MULTIPLIER then DWCOTG_DMA_MULTIPLIER:=CacheLineSize;

 {Setup BCM434XX}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    {No 32KHz Clock pin on 4B/400/CM4}
    BCM434XX_WLAN_ON_PIN:=VIRTUAL_GPIO_PIN_1;
    BCM434XX_WLAN_ON_FUNCTION:=GPIO_FUNCTION_OUT;
    BCM434XX_WLAN_ON_VIRTUAL:=True;
   end;
 end;

 {Setup LAN}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    GENET_MAC_ADDRESS:=BoardGetMACAddress;
   end;
 end;
end;

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi4FramebufferInit;
var
 Status:LongWord;

 DisplayId:LongWord;
 DisplayNum:LongWord;
 DisplayCount:LongWord;
 MultiDisplay:Boolean;

 RPi4Framebuffer:PRPi4Framebuffer;
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
   RPi4Framebuffer:=PRPi4Framebuffer(FramebufferDeviceCreateEx(SizeOf(TRPi4Framebuffer)));
   if RPi4Framebuffer <> nil then
    begin
     {Device}
     RPi4Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO;
     RPi4Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
     RPi4Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_BACKLIGHT;
     RPi4Framebuffer.Framebuffer.Device.DeviceData:=nil;
     RPi4Framebuffer.Framebuffer.Device.DeviceDescription:=RPI4_FRAMEBUFFER_DESCRIPTION + ' (' + FramebufferDisplayIdToName(DisplayId) + ')';
     {Framebuffer}
     RPi4Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
     RPi4Framebuffer.Framebuffer.DeviceAllocate:=RPi4FramebufferDeviceAllocate;
     RPi4Framebuffer.Framebuffer.DeviceRelease:=RPi4FramebufferDeviceRelease;
     RPi4Framebuffer.Framebuffer.DeviceBlank:=RPi4FramebufferDeviceBlank;
     RPi4Framebuffer.Framebuffer.DeviceCommit:=RPi4FramebufferDeviceCommit;
     RPi4Framebuffer.Framebuffer.DeviceSetBacklight:=RPi4FramebufferDeviceSetBacklight;
     {Driver}
     RPi4Framebuffer.MultiDisplay:=MultiDisplay;
     RPi4Framebuffer.DisplayNum:=DisplayNum;
     FramebufferGetDisplaySettings(DisplayNum,RPi4Framebuffer.DisplaySettings);

     {Setup Flags}
     if BCM2711FRAMEBUFFER_CACHED then RPi4Framebuffer.Framebuffer.Device.DeviceFlags:=RPi4Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_COMMIT;
     if BCM2711FRAMEBUFFER_CACHED then RPi4Framebuffer.Framebuffer.Device.DeviceFlags:=RPi4Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
     {if EnvironmentGet('bcm2708_fb.fbswap') <> '1' then RPi4Framebuffer.Framebuffer.Device.DeviceFlags:=RPi4Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;} {Handled by FramebufferAllocate}

     {Register Framebuffer}
     Status:=FramebufferDeviceRegister(@RPi4Framebuffer.Framebuffer);
     if Status = ERROR_SUCCESS then
      begin
       {Allocate Framebuffer}
       Status:=FramebufferDeviceAllocate(@RPi4Framebuffer.Framebuffer,nil);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: Failed to allocate new framebuffer device: ' + ErrorToString(Status));

         {Deregister Framebuffer}
         FramebufferDeviceDeregister(@RPi4Framebuffer.Framebuffer);

         {Destroy Framebuffer}
         FramebufferDeviceDestroy(@RPi4Framebuffer.Framebuffer);
        end;
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: Failed to register new framebuffer device: ' + ErrorToString(Status));

       {Destroy Framebuffer}
       FramebufferDeviceDestroy(@RPi4Framebuffer.Framebuffer);
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

procedure RPi4PageTableInit;
{Initialize the Hardware Page Tables before enabling the MMU (LPAE version)}
var
 Count:Integer;
 Table:PtrUInt;
 Address:UInt64;
 MaxAddress:UInt64;
 PageCount:LongWord;
 ActualAddress:PtrUInt;
 RequestAddress:PtrUInt;
 StartAddress:UInt64;
 EndAddress:UInt64;
 ExcludeAddress:UInt64;
 ExcludeSize:UInt64;
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
 {Setup 1GB tables covering the entire 4GB address space}
 Table:=(PAGE_TABLE_BASE and ARMV7L_DESCRIPTOR_BASE_MASK);
 Address:=$00000000;
 while Address < SIZE_4G do
  begin
   ARMv7LSetPageTableLargeSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_NONE_ALL);
   ARMv7LSetPageTableLevel1(Address,Table,0);
   Inc(Table,SIZE_4K);
   Inc(Address,SIZE_1G);
  end;

 {Create the second level page tables}
 {Setup 2MB sections covering the entire 4GB address space with a default layout}
 {Set the 2MB sections up to the size of physical memory as ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=$00000000;
 MaxAddress:=MEMORY_SIZE;
 PageCount:=MaxAddress div SIZE_2M;
 for Count:=0 to PageCount - 1 do
  begin
   ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_2M);
  end;

 {Set the 2MB sections in the remainder of the 4GB address space as ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED (Shared)(Non Executable)(Read Write)}
 if PageCount < 2048 then
  begin
   for Count:=PageCount to 2047 do
    begin
     if CPU_MEMORY_RESTRICTED then
      begin
       ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_NONE_ALL);
      end
     else
      begin
       ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
      end;
     Inc(Address,SIZE_2M);
    end;
  end;

 {Set the 2MB sections containing the BCM2838_EXT_PERIPHERALS_BASE to ARMV7L_DESCRIPTOR_CACHE_DEVICE (Shared)(Non Executable)(Read Write)}
 if BCM2838_EXT_PERIPHERALS_SIZE > 0 then
  begin
   Address:=(BCM2838_EXT_PERIPHERALS_BASE and ARMV7L_SECTION_BASE_MASK);
   MaxAddress:=UInt64(BCM2838_EXT_PERIPHERALS_BASE + BCM2838_EXT_PERIPHERALS_SIZE); {Avoid 32-bit overflow}
   while Address < MaxAddress do
    begin
     ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_DEVICE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
     Inc(Address,SIZE_2M);
    end;
  end;

 {Set the 2MB sections containing the PERIPHERALS_BASE to ARMV7L_DESCRIPTOR_CACHE_DEVICE (Shared)(Non Executable)(Read Write)}
 if PERIPHERALS_SIZE > 0 then
  begin
   Address:=(PERIPHERALS_BASE and ARMV7L_SECTION_BASE_MASK);
   MaxAddress:=UInt64(PERIPHERALS_BASE + PERIPHERALS_SIZE); {Avoid 32-bit overflow}
   while Address < MaxAddress do
    begin
     ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_DEVICE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
     Inc(Address,SIZE_2M);
    end;
  end;

 {Set the 2MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV7L_DESCRIPTOR_CACHE_DEVICE (Shared)(Non Executable)(Read Write)}
 if LOCAL_PERIPHERALS_SIZE > 0 then
  begin
   Address:=(LOCAL_PERIPHERALS_BASE and ARMV7L_SECTION_BASE_MASK);
   MaxAddress:=UInt64(LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE); {Avoid 32-bit overflow}
   while Address < MaxAddress do
    begin
     ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_DEVICE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
     Inc(Address,SIZE_2M);
    end;
  end;

 {Create the currently used third level page tables}
 Table:=(PAGE_TABLES_ADDRESS and ARMV7L_DESCRIPTOR_BASE_MASK);
 Address:=$00000000;
 for Count:=0 to PAGE_TABLES_USED - 1 do
  begin
   ARMv7LSetPageTableLevel2(Address,Table,0);
   Inc(Table,SIZE_4K);
   Inc(Address,SIZE_2M);
  end;
 PAGE_TABLES_NEXT:=Table;

 {Set the 4KB zero page to ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED (Shared)(Non Executable)(No Access)}
 Address:=$00000000;
 ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_NONE_ALL);

 {Set the 4KB pages containing the VECTOR_TABLE_BASE to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)}
 {Changed to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_BACK (Shared)(Executable)(Read Only) due to no write through support for data caching on ARMv7 or later}
 {See 5.2.1 Memory types and attributes in the Cortex A7 Technical Reference Manual (http://infocenter.arm.com/help/topic/com.arm.doc.ddi0464f/CIHJCAAG.html)}
 Address:=(VECTOR_TABLE_BASE and ARMV7L_PAGE_BASE_MASK);
 MaxAddress:=UInt64(VECTOR_TABLE_BASE + VECTOR_TABLE_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
  begin
   {ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_ALL);}
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_BACK or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the first level page directory to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_DIRECTORY_BASE and ARMV7L_PAGE_BASE_MASK);
 MaxAddress:=UInt64(PAGE_DIRECTORY_BASE + PAGE_DIRECTORY_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
  begin
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the second level page table to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_TABLE_BASE and ARMV7L_PAGE_BASE_MASK);
 MaxAddress:=UInt64(PAGE_TABLE_BASE + PAGE_TABLE_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
  begin
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the TEXT (Code) section to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)}
 {Changed to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_BACK (Shared)(Executable)(Read Only) due to no write through support for data caching on ARMv7 or later}
 {See 5.2.1 Memory types and attributes in the Cortex A7 Technical Reference Manual (http://infocenter.arm.com/help/topic/com.arm.doc.ddi0464f/CIHJCAAG.html)}
 Address:=(PtrUInt(@_text_start) and ARMV7L_PAGE_BASE_MASK);
 while Address < (PtrUInt(@_data)) do
  begin
   {ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_ALL);}
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_BACK or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the DATA (Initialized) section to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PtrUInt(@_data) and ARMV7L_PAGE_BASE_MASK);
 while Address < (PtrUInt(@_bss_start)) do
  begin
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the BSS (Uninitialized) section to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PtrUInt(@_bss_start) and ARMV7L_PAGE_BASE_MASK);
 while Address < (PtrUInt(@_bss_end)) do
  begin
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the third level page tables to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_TABLES_ADDRESS and ARMV7L_PAGE_BASE_MASK);
 while Address < (PAGE_TABLES_ADDRESS + PAGE_TABLES_LENGTH) do
  begin
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the initial stack to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_STACK_BASE and ARMV7L_PAGE_BASE_MASK);
 MaxAddress:=UInt64(INITIAL_STACK_BASE + INITIAL_STACK_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
  begin
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the initial heap to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_HEAP_BASE and ARMV7L_PAGE_BASE_MASK);
 MaxAddress:=UInt64(INITIAL_HEAP_BASE + INITIAL_HEAP_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
  begin
   ARMv7LSetPageTablePage(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
   Inc(Address,SIZE_4K);
  end;

 {Set the starting address for NoCache/Device/Shared/Local/IRQ/FIQ Blocks}
 if CPU_MEMORY_SIZE > 0 then
  begin
   {Get the top of CPU memory}
   RequestAddress:=CPU_MEMORY_BASE + CPU_MEMORY_SIZE;

   {Round CPU memory to a 2MB multiple (Divide by 2MB / Multiply by 2MB)}
   RequestAddress:=(RequestAddress shr 21) shl 21;
   if RequestAddress > 0 then
    begin
     {Round NoCache/Device/Shared/Local/IRQ/FIQ sizes to a 2MB multiple}
     MEMORY_NONSHARED_SIZE:=(MEMORY_NONSHARED_SIZE shr 21) shl 21;
     MEMORY_NOCACHE_SIZE:=(MEMORY_NOCACHE_SIZE shr 21) shl 21;
     MEMORY_DEVICE_SIZE:=(MEMORY_DEVICE_SIZE shr 21) shl 21;
     MEMORY_SHARED_SIZE:=(MEMORY_SHARED_SIZE shr 21) shl 21;
     MEMORY_LOCAL_SIZE:=(MEMORY_LOCAL_SIZE shr 21) shl 21;
     MEMORY_IRQ_SIZE:=(MEMORY_IRQ_SIZE shr 21) shl 21;
     MEMORY_FIQ_SIZE:=(MEMORY_FIQ_SIZE shr 21) shl 21;

     {Subtract from top of CPU memory}
     Dec(RequestAddress,MEMORY_NONSHARED_SIZE);
     Dec(RequestAddress,MEMORY_NOCACHE_SIZE);
     Dec(RequestAddress,MEMORY_DEVICE_SIZE);
     Dec(RequestAddress,MEMORY_SHARED_SIZE);
     Dec(RequestAddress,MEMORY_LOCAL_SIZE * RPI4_CPU_COUNT); {Local memory is per CPU}
     if IRQ_ENABLED then Dec(RequestAddress,MEMORY_IRQ_SIZE * RPI4_CPU_COUNT); {IRQ memory is per CPU}
     if FIQ_ENABLED then Dec(RequestAddress,MEMORY_FIQ_SIZE * RPI4_CPU_COUNT); {FIQ memory is per CPU}

     {Register 2MB Non Shared Memory Blocks as ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_NONSHARED_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestNonSharedHeapBlock(Pointer(RequestAddress),MEMORY_NONSHARED_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         MaxAddress:=UInt64(ActualAddress + MEMORY_NONSHARED_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
          begin
           ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
           Inc(Address,SIZE_2M);
          end;
         Inc(RequestAddress,MEMORY_NONSHARED_SIZE);
        end;
      end;

     {Register 2MB Non Cached Memory Blocks as ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_NOCACHE_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestNoCacheHeapBlock(Pointer(RequestAddress),MEMORY_NOCACHE_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         MaxAddress:=UInt64(ActualAddress + MEMORY_NOCACHE_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
          begin
           ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
           Inc(Address,SIZE_2M);
          end;
         Inc(RequestAddress,MEMORY_NOCACHE_SIZE);
        end;
      end;

     {Register 2MB Device Memory Blocks as ARMV7L_DESCRIPTOR_CACHE_DEVICE (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_DEVICE_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestDeviceHeapBlock(Pointer(RequestAddress),MEMORY_DEVICE_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         MaxAddress:=UInt64(ActualAddress + MEMORY_DEVICE_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
          begin
           ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_DEVICE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
           Inc(Address,SIZE_2M);
          end;
         Inc(RequestAddress,MEMORY_DEVICE_SIZE);
        end;
      end;

     {Register 2MB Shared Memory Blocks as ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
     if MEMORY_SHARED_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestSharedHeapBlock(Pointer(RequestAddress),MEMORY_SHARED_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         MaxAddress:=UInt64(ActualAddress + MEMORY_SHARED_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
          begin
           ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
           Inc(Address,SIZE_2M);
          end;
         Inc(RequestAddress,MEMORY_SHARED_SIZE);
        end;
      end;

     {Register 2MB Local Memory Blocks as ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_LOCAL_SIZE > 0 then
      begin
       for Count:=0 to (RPI4_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestLocalHeapBlock(Pointer(RequestAddress),MEMORY_LOCAL_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           MaxAddress:=UInt64(ActualAddress + MEMORY_LOCAL_SIZE); {Avoid 32-bit overflow}
           while Address < MaxAddress do
            begin
             ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
             Inc(Address,SIZE_2M);
            end;
           Inc(RequestAddress,MEMORY_LOCAL_SIZE);
          end;
        end;
      end;

     {Register 2MB IRQ Memory Blocks as ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if IRQ_ENABLED and (MEMORY_IRQ_SIZE > 0) then
      begin
       for Count:=0 to (RPI4_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestIRQHeapBlock(Pointer(RequestAddress),MEMORY_IRQ_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           MaxAddress:=UInt64(ActualAddress + MEMORY_IRQ_SIZE); {Avoid 32-bit overflow}
           while Address < MaxAddress do
            begin
             ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
             Inc(Address,SIZE_2M);
            end;
           Inc(RequestAddress,MEMORY_IRQ_SIZE);
          end;
        end;
      end;

     {Register 2MB FIQ Memory Blocks as ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if FIQ_ENABLED and (MEMORY_FIQ_SIZE > 0) then
      begin
       for Count:=0 to (RPI4_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestFIQHeapBlock(Pointer(RequestAddress),MEMORY_FIQ_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           MaxAddress:=UInt64(ActualAddress + MEMORY_FIQ_SIZE); {Avoid 32-bit overflow}
           while Address < MaxAddress do
            begin
             ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
             Inc(Address,SIZE_2M);
            end;
           Inc(RequestAddress,MEMORY_FIQ_SIZE);
          end;
        end;
      end;
    end;
  end;

 {Set the 2MB sections containing the GPU_MEMORY to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 if GPU_MEMORY_SIZE > 0 then
  begin
   Address:=(GPU_MEMORY_BASE and ARMV7L_SECTION_BASE_MASK);
   MaxAddress:=UInt64(GPU_MEMORY_BASE + GPU_MEMORY_SIZE); {Avoid 32-bit overflow}
   while (Address < MaxAddress) and (Address < (PERIPHERALS_BASE and ARMV7L_SECTION_BASE_MASK)) do
    begin
     if GPU_MEMORY_CACHED then
      begin
       ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
      end
     else
      begin
       ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
      end;
     Inc(Address,SIZE_2M);
    end;
  end;
 {$ENDIF CPUARM}
 {$IFDEF CPUAARCH64}
 //To Do
 {$ENDIF CPUAARCH64}

 {If memory size is 2GB, 4GB or 8GB then the ARM tags and CPU_MEMORY_SIZE values will not include the memory above 1GB}
 {Register additional memory with the Heap Manager by calculating the free memory between 1GB and the physical limit}
 if MEMORY_SIZE > SIZE_1G then
  begin
   {Get starting address}
   StartAddress:=SIZE_1G;
   if GPU_MEMORY_SIZE > 0 then
    begin
     Address:=UInt64(GPU_MEMORY_BASE + GPU_MEMORY_SIZE); {Avoid 32-bit overflow}

     if Address > StartAddress then StartAddress:=Address;
    end;

   {Get ending address}
   if MEMORY_SIZE = SIZE_2G then
    begin
     EndAddress:=SIZE_2G;
    end
   else if MEMORY_SIZE = SIZE_4G then
    begin
     EndAddress:=SIZE_4G;
    end
   else if MEMORY_SIZE = SIZE_8G then
    begin
     {$IFDEF CPUARM}
     EndAddress:=SIZE_4G; {Only the first 4GB is usable}
     {$ENDIF CPUARM}
     {$IFDEF CPUAARCH64}
     EndAddress:=SIZE_8G;
     {$ENDIF CPUAARCH64}
    end;

   {Check for exclusions}
   {Peripherals space (Start: 0xF8000000 Size: 128MB}
   ExcludeAddress:=BCM2838_PCI_OUTBOUND_BASE;
   if ExcludeAddress > 0 then
    begin
     ExcludeSize:=BCM2838_PCI_OUTBOUND_SIZE + BCM2838_EXT_PERIPHERALS_SIZE + BCM2838_PERIPHERALS_SIZE + BCM2838_ARM_LOCAL_SIZE;

     {Check for overlap}
     if (ExcludeAddress + ExcludeSize) >= StartAddress then
      begin
       if ExcludeAddress < StartAddress then
        begin
         {Update start address}
         StartAddress:=ExcludeAddress + ExcludeSize;
        end;

       if ExcludeAddress < EndAddress then
        begin
         if (ExcludeAddress + ExcludeSize) < EndAddress then
          begin
           {Register with Heap Manager}
           RegisterHeapBlock(Pointer(ExcludeAddress + ExcludeSize),EndAddress - (ExcludeAddress + ExcludeSize));
          end;

         {Update end address}
         EndAddress:=ExcludeAddress;
        end;
      end;
    end;

   {SysCalls heap (Default Start: 0xC0000000 Default Size: 1GB)}
   ExcludeAddress:=SYSCALLS_HEAP_BASE;
   if ExcludeAddress > 0 then
    begin
     ExcludeSize:=SYSCALLS_HEAP_MAX;

     {Normalize exclude address}
     if (ExcludeAddress mod MEMORY_PAGE_SIZE) <> 0 then
      begin
       ExcludeAddress:=ExcludeAddress - (ExcludeAddress mod MEMORY_PAGE_SIZE);
      end;

     {Normalize exclude size}
     if (ExcludeSize mod SIZE_1M) <> 0 then
      begin
       ExcludeSize:=ExcludeSize - (ExcludeSize mod SIZE_1M);
       while ExcludeSize < SYSCALLS_HEAP_MAX do
        begin
         Inc(ExcludeSize,SIZE_1M);
        end;
      end;

     {Check for overlap}
     if (ExcludeAddress + ExcludeSize) >= StartAddress then
      begin
       if ExcludeAddress < StartAddress then
        begin
         {Update start address}
         StartAddress:=ExcludeAddress + ExcludeSize;
        end;

       if ExcludeAddress < EndAddress then
        begin
         if (ExcludeAddress + ExcludeSize) < EndAddress then
          begin
           {Register with Heap Manager}
           RegisterHeapBlock(Pointer(ExcludeAddress + ExcludeSize),EndAddress - (ExcludeAddress + ExcludeSize));
          end;

         {Update end address}
         EndAddress:=ExcludeAddress;
        end;
      end;
    end;

   {Register with Heap Manager}
   if (StartAddress < EndAddress) then
    begin
     RegisterHeapBlock(Pointer(StartAddress),EndAddress - StartAddress);
    end;
  end;

 {Synchronization Barrier}
 DataSynchronizationBarrier;

 RPi4PageTableInitialized:=True;
end;

{==============================================================================}
{$IFNDEF RPI4_ENABLE_LPAE}
procedure RPi4PageTableInitLegacy;
{Initialize the Hardware Page Tables before enabling the MMU (Legacy version)}
var
 Count:Integer;
 Table:PtrUInt;
 Address:UInt64;
 MaxAddress:UInt64;
 PageCount:LongWord;
 ActualAddress:PtrUInt;
 RequestAddress:PtrUInt;
 StartAddress:UInt64;
 EndAddress:UInt64;
 ExcludeAddress:UInt64;
 ExcludeSize:UInt64;
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

 {Create the first level page table}
 {Setup 1MB sections covering the entire 4GB address space with a default layout}
 {Set the 1MB sections up to the size of physical memory as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=$00000000;
 MaxAddress:=MEMORY_SIZE;
 PageCount:=MaxAddress div SIZE_1M;
 for Count:=0 to PageCount - 1 do
  begin
   ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
   Inc(Address,SIZE_1M);
  end;

 {Set the 1MB sections in the remainder of the 4GB address space as ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED (Shared)(Non Executable)(Read Write)}
 if PageCount < 4096 then
  begin
   for Count:=PageCount to 4095 do
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
  end;

 {Set the 1MB sections containing the BCM2838_EXT_PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Shared)(Non Executable)(Read Write)}
 if BCM2838_EXT_PERIPHERALS_SIZE > 0 then
  begin
   Address:=(BCM2838_EXT_PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   MaxAddress:=UInt64(BCM2838_EXT_PERIPHERALS_BASE + BCM2838_EXT_PERIPHERALS_SIZE); {Avoid 32-bit overflow}
   while Address < MaxAddress do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
  end;

 {Set the 1MB sections containing the PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Shared)(Non Executable)(Read Write)}
 if PERIPHERALS_SIZE > 0 then
  begin
   Address:=(PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   MaxAddress:=UInt64(PERIPHERALS_BASE + PERIPHERALS_SIZE); {Avoid 32-bit overflow}
   while Address < MaxAddress do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
  end;

 {Set the 1MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Shared)(Non Executable)(Read Write)}
 if LOCAL_PERIPHERALS_SIZE > 0 then
  begin
   Address:=(LOCAL_PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   MaxAddress:=UInt64(LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE); {Avoid 32-bit overflow}
   while Address < MaxAddress do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
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

 {Set the 4KB zero page to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED (Shared)(Non Executable)(No Access)}
 Address:=$00000000;
 ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_NONE);

 {Set the 4KB pages containing the VECTOR_TABLE_BASE to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)}
 {Changed to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK (Shared)(Executable)(Read Only) due to no write through support for data caching on ARMv7 or later}
 {See 5.2.1 Memory types and attributes in the Cortex A7 Technical Reference Manual (http://infocenter.arm.com/help/topic/com.arm.doc.ddi0464f/CIHJCAAG.html)}
 Address:=(VECTOR_TABLE_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 MaxAddress:=UInt64(VECTOR_TABLE_BASE + VECTOR_TABLE_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
  begin
   {ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L2D_ACCESS_READONLY);}
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the first level page table to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_TABLE_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 MaxAddress:=UInt64(PAGE_TABLE_BASE + PAGE_TABLE_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
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
 MaxAddress:=UInt64(INITIAL_STACK_BASE + INITIAL_STACK_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the initial heap to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_HEAP_BASE and ARMV7_L2D_SMALL_BASE_MASK);
 MaxAddress:=UInt64(INITIAL_HEAP_BASE + INITIAL_HEAP_SIZE); {Avoid 32-bit overflow}
 while Address < MaxAddress do
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
     Dec(RequestAddress,MEMORY_LOCAL_SIZE * RPI4_CPU_COUNT); {Local memory is per CPU}
     if IRQ_ENABLED then Dec(RequestAddress,MEMORY_IRQ_SIZE * RPI4_CPU_COUNT); {IRQ memory is per CPU}
     if FIQ_ENABLED then Dec(RequestAddress,MEMORY_FIQ_SIZE * RPI4_CPU_COUNT); {FIQ memory is per CPU}

     {Register 1MB Non Shared Memory Blocks as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Non Shared)(Non Executable)(Read Write)}
     if MEMORY_NONSHARED_SIZE > 0 then
      begin
       ActualAddress:=PtrUInt(RequestNonSharedHeapBlock(Pointer(RequestAddress),MEMORY_NONSHARED_SIZE));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         MaxAddress:=UInt64(ActualAddress + MEMORY_NONSHARED_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
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
         MaxAddress:=UInt64(ActualAddress + MEMORY_NOCACHE_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
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
         MaxAddress:=UInt64(ActualAddress + MEMORY_DEVICE_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
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
         MaxAddress:=UInt64(ActualAddress + MEMORY_SHARED_SIZE); {Avoid 32-bit overflow}
         while Address < MaxAddress do
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
       for Count:=0 to (RPI4_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestLocalHeapBlock(Pointer(RequestAddress),MEMORY_LOCAL_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           MaxAddress:=UInt64(ActualAddress + MEMORY_LOCAL_SIZE); {Avoid 32-bit overflow}
           while Address < MaxAddress do
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
       for Count:=0 to (RPI4_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestIRQHeapBlock(Pointer(RequestAddress),MEMORY_IRQ_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           MaxAddress:=UInt64(ActualAddress + MEMORY_IRQ_SIZE); {Avoid 32-bit overflow}
           while Address < MaxAddress do
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
       for Count:=0 to (RPI4_CPU_COUNT - 1) do
        begin
         ActualAddress:=PtrUInt(RequestFIQHeapBlock(Pointer(RequestAddress),MEMORY_FIQ_SIZE,(1 shl Count)));
         if ActualAddress > 0 then
          begin
           Address:=ActualAddress;
           MaxAddress:=UInt64(ActualAddress + MEMORY_FIQ_SIZE); {Avoid 32-bit overflow}
           while Address < MaxAddress do
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
   MaxAddress:=UInt64(GPU_MEMORY_BASE + GPU_MEMORY_SIZE); {Avoid 32-bit overflow}
   while (Address < MaxAddress) and (Address < (PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK)) do
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

 {If memory size is 2GB, 4GB or 8GB then the ARM tags and CPU_MEMORY_SIZE values will not include the memory above 1GB}
 {Register additional memory with the Heap Manager by calculating the free memory between 1GB and the physical limit}
 if MEMORY_SIZE > SIZE_1G then
  begin
   {Get starting address}
   StartAddress:=SIZE_1G;
   if GPU_MEMORY_SIZE > 0 then
    begin
     Address:=UInt64(GPU_MEMORY_BASE + GPU_MEMORY_SIZE); {Avoid 32-bit overflow}

     if Address > StartAddress then StartAddress:=Address;
    end;

   {Get ending address}
   if MEMORY_SIZE = SIZE_2G then
    begin
     EndAddress:=SIZE_2G;
    end
   else if MEMORY_SIZE = SIZE_4G then
    begin
     EndAddress:=SIZE_4G;
    end
   else if MEMORY_SIZE = SIZE_8G then
    begin
     EndAddress:=SIZE_4G; {Only the first 4GB is usable}
    end;

   {Check for exclusions}
   {Peripherals space (Start: 0xF8000000 Size: 128MB}
   ExcludeAddress:=BCM2838_PCI_OUTBOUND_BASE;
   if ExcludeAddress > 0 then
    begin
     ExcludeSize:=BCM2838_PCI_OUTBOUND_SIZE + BCM2838_EXT_PERIPHERALS_SIZE + BCM2838_PERIPHERALS_SIZE + BCM2838_ARM_LOCAL_SIZE;

     {Check for overlap}
     if (ExcludeAddress + ExcludeSize) >= StartAddress then
      begin
       if ExcludeAddress < StartAddress then
        begin
         {Update start address}
         StartAddress:=ExcludeAddress + ExcludeSize;
        end;

       if ExcludeAddress < EndAddress then
        begin
         if (ExcludeAddress + ExcludeSize) < EndAddress then
          begin
           {Register with Heap Manager}
           RegisterHeapBlock(Pointer(ExcludeAddress + ExcludeSize),EndAddress - (ExcludeAddress + ExcludeSize));
          end;

         {Update end address}
         EndAddress:=ExcludeAddress;
        end;
      end;
    end;

   {SysCalls heap (Default Start: 0xC0000000 Default Size: 1GB)}
   ExcludeAddress:=SYSCALLS_HEAP_BASE;
   if ExcludeAddress > 0 then
    begin
     ExcludeSize:=SYSCALLS_HEAP_MAX;

     {Normalize exclude address}
     if (ExcludeAddress mod MEMORY_PAGE_SIZE) <> 0 then
      begin
       ExcludeAddress:=ExcludeAddress - (ExcludeAddress mod MEMORY_PAGE_SIZE);
      end;

     {Normalize exclude size}
     if (ExcludeSize mod SIZE_1M) <> 0 then
      begin
       ExcludeSize:=ExcludeSize - (ExcludeSize mod SIZE_1M);
       while ExcludeSize < SYSCALLS_HEAP_MAX do
        begin
         Inc(ExcludeSize,SIZE_1M);
        end;
      end;

     {Check for overlap}
     if (ExcludeAddress + ExcludeSize) >= StartAddress then
      begin
       if ExcludeAddress < StartAddress then
        begin
         {Update start address}
         StartAddress:=ExcludeAddress + ExcludeSize;
        end;

       if ExcludeAddress < EndAddress then
        begin
         if (ExcludeAddress + ExcludeSize) < EndAddress then
          begin
           {Register with Heap Manager}
           RegisterHeapBlock(Pointer(ExcludeAddress + ExcludeSize),EndAddress - (ExcludeAddress + ExcludeSize));
          end;

         {Update end address}
         EndAddress:=ExcludeAddress;
        end;
      end;
    end;

   {Register with Heap Manager}
   if (StartAddress < EndAddress) then
    begin
     RegisterHeapBlock(Pointer(StartAddress),EndAddress - StartAddress);
    end;
  end;

 {Synchronization Barrier}
 DataSynchronizationBarrier;

 RPi4PageTableInitialized:=True;
end;
{$ENDIF RPI4_ENABLE_LPAE}
{==============================================================================}

procedure RPi4PowerLEDEnable;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    {Virtual GPIO}
    VirtualGPIOFunctionSelect(POWER_LED_PIN,POWER_LED_FUNCTION);
   end;
 end;
end;

{==============================================================================}

procedure RPi4PowerLEDOn;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
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

procedure RPi4PowerLEDOff;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
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

procedure RPi4ActivityLEDEnable;
var
 Value:LongWord;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    {Setup Pull Up/Down}
    GPIOPullSelect(ACTIVITY_LED_PIN,ACTIVITY_LED_PULL);
    {Enable Function}
    GPIOFunctionSelect(ACTIVITY_LED_PIN,ACTIVITY_LED_FUNCTION);
   end;
 end;
end;

{==============================================================================}

procedure RPi4ActivityLEDOn;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    {LED On}
    GPIOOutputSet(ACTIVITY_LED_PIN,GPIO_LEVEL_HIGH);
   end;
 end;
end;

{==============================================================================}

procedure RPi4ActivityLEDOff;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    {LED Off}
    GPIOOutputSet(ACTIVITY_LED_PIN,GPIO_LEVEL_LOW);
   end;
 end;
end;

{==============================================================================}

function RPi4MailboxReceive(Mailbox,Channel:LongWord):LongWord;
{Receive from specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 ResultCode:LongWord;
begin
 {}
 Result:=0;
 {Check Mailbox}
 if Mailbox = BCM2838_MAILBOX_0 then
  begin
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try
    {Setup Timeout}
    Timeout:=RPI4_MAILBOX_TIMEOUT;

    {Setup Result}
    ResultCode:=BCM2838_MAILBOX_CHANNEL_MASK; {Start with all channel bits set}

    {Check Channel}
    while ((ResultCode and BCM2838_MAILBOX_CHANNEL_MASK) <> Channel) do
     begin
      {Check Status}
      while (Mailbox0Registers.Status and BCM2838_MAILBOX_STATUS_EMPTY) = BCM2838_MAILBOX_STATUS_EMPTY do
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
    Result:=ResultCode and BCM2838_MAILBOX_DATA_MASK; {Account for channel offset}
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end;
end;

{==============================================================================}

procedure RPi4MailboxSend(Mailbox,Channel,Data:LongWord);
{Send to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 WriteData:LongWord;
begin
 {}
 {Check Mailbox}
 if Mailbox = BCM2838_MAILBOX_0 then
  begin
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try
    {Setup Timeout}
    Timeout:=RPI4_MAILBOX_TIMEOUT;

    {Setup Data}
    WriteData:=Channel or (Data and BCM2838_MAILBOX_DATA_MASK);

    {Check Status}
    while (Mailbox1Registers.Status and BCM2838_MAILBOX_STATUS_FULL) = BCM2838_MAILBOX_STATUS_FULL do
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

function RPi4MailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
{Perform a transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPi4MailboxCallEx(Mailbox,Channel,Data,Response,RPI4_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPi4MailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord;
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
 if Mailbox = BCM2838_MAILBOX_0 then
  begin
   {Check the Data (Must not use the lowest 4 bits)}
   if (Data and BCM2838_MAILBOX_CHANNEL_MASK) <> 0 then Exit;

   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try
    {Setup Timeout}
    Retries:=Timeout;

    {Wait for Mailbox 0 Empty}
    while (Mailbox0Registers.Status and BCM2838_MAILBOX_STATUS_EMPTY) <> BCM2838_MAILBOX_STATUS_EMPTY do
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
    while (Mailbox1Registers.Status and BCM2838_MAILBOX_STATUS_FULL) = BCM2838_MAILBOX_STATUS_FULL do
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
    WriteData:=Channel or (Data and BCM2838_MAILBOX_DATA_MASK);
    Mailbox1Registers.Write:=WriteData;

    {Setup Timeout}
    Retries:=Timeout;

    {Wait for Mailbox 0 not Empty}
    while (Mailbox0Registers.Status and BCM2838_MAILBOX_STATUS_EMPTY) = BCM2838_MAILBOX_STATUS_EMPTY do
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
    if (ResultCode and BCM2838_MAILBOX_CHANNEL_MASK) <> Channel then
     begin
      Result:=ERROR_INVALID_DATA;
      Exit;
     end;

    {Return the Response}
    Response:=ResultCode and BCM2838_MAILBOX_DATA_MASK; {Account for channel offset}

    Result:=ERROR_SUCCESS;
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end;
end;

{==============================================================================}

function RPi4MailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPi4MailboxPropertyCallEx(Mailbox,Channel,Data,Response,RPI4_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPi4MailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord;
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
var
 Tag:PBCM2838MailboxTagHeader;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF PLATFORM_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - (Mailbox=' + IntToHex(Mailbox,8) + ' Channel=' + IntToHex(Channel,8) + ' Data=' + IntToHex(PtrUInt(Data),8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}

 {Check Mailbox}
 if Mailbox = BCM2838_MAILBOX_0 then
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
   if PBCM2838MailboxHeader(Data).Code <> BCM2838_MBOX_RESPONSE_CODE_SUCCESS then
    begin
     Result:=ERROR_FUNCTION_FAILED;
     if PLATFORM_LOG_ENABLED then PlatformLogError('MailboxPropertyCallEx - Response Code Failed: (Code=' + IntToHex(PBCM2838MailboxHeader(Data).Code,8) + ')');
     Exit;
    end;

   {Check each tags Response Code}
   Tag:=PBCM2838MailboxTagHeader(PtrUInt(Data) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
   while Tag.Tag <> BCM2838_MBOX_TAG_END do
    begin
     if (Tag.Length and BCM2838_MBOX_TAG_RESPONSE_CODE) = 0 then
      begin
       {$IFDEF PLATFORM_DEBUG}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - Tag Response Code Incorrect (Length=' + IntToHex(Tag.Length,8) + ')');
       {$ENDIF}
       {Result:=ERROR_FUNCTION_FAILED;} {Note: Recent firmware functions do not always set the response bit in the tag}
       {Exit;}                          {      The Linux firmware driver does not check this bit in the response}
      end;
     {Clear the Response bit so callers can read the length field without extra processing}
     Tag.Length:=Tag.Length and not(BCM2838_MBOX_TAG_RESPONSE_CODE);
     {Get Next Tag}
     Tag:=PBCM2838MailboxTagHeader(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagHeader)) + Tag.Size);
    end;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function RPi4MailboxPropertyTag(Tag:LongWord;Data:Pointer;Size:LongWord):LongWord;
{Request a property tag (Get/Set) from the mailbox property channel}
{Note: Data does not need to include mailbox property channel header or footer}
{Note: Data pointer does not need any specific alignment or caching attributes}
{Note: Size must be a multiple of 4 bytes}
{Note: Size must include the size of the request and response which use the same buffer}
var
 Total:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 TagHeader:PBCM2838MailboxTagHeader;
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
 Total:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagHeader) + Size + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Total,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Total,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Total,0);

  {Setup Header}
  Header.Size:=Total;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  TagHeader:=PBCM2838MailboxTagHeader(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  TagHeader.Tag:=Tag;
  TagHeader.Size:=Size;
  TagHeader.Length:=Size;

  {Copy Request}
  TagBuffer:=Pointer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)) + PtrUInt(SizeOf(TBCM2838MailboxTagHeader)));
  System.Move(Data^,TagBuffer^,Size);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(TagHeader) + PtrUInt(SizeOf(TBCM2838MailboxTagHeader)) + Size);
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4RequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied extended handler to the specified IRQ number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Local}
 if ARMGICIsLocal(GICDevice,Number) then
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
 if ARMGICIsLocal(GICDevice,Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL;

 {Register Entry}
 Result:=ARMGICRegisterEntry(GICDevice,Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4ReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied extended handler from the specified IRQ number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Local}
 if ARMGICIsLocal(GICDevice,Number) then
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
 if ARMGICIsLocal(GICDevice,Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL;

 {Deregister Entry}
 Result:=ARMGICDeregisterEntry(GICDevice,Entry);
end;

{==============================================================================}

function RPi4RequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied extended handler to the specified FIQ number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Local}
 if ARMGICIsLocal(GICDevice,Number) then
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
 if ARMGICIsLocal(GICDevice,Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL or INTERRUPT_FLAG_FIQ;

 {Register Entry}
 Result:=ARMGICRegisterEntry(GICDevice,Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4ReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied extended handler from the specified FIQ number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Local}
 if ARMGICIsLocal(GICDevice,Number) then
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
 if ARMGICIsLocal(GICDevice,Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL or INTERRUPT_FLAG_FIQ;

 {Deregister Entry}
 Result:=ARMGICDeregisterEntry(GICDevice,Entry);
end;

{==============================================================================}

function RPi4RequestIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified IPI number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Mask Count}
 if CPUMaskCount(Mask) <> 1 then Exit;

 {Check Mask CPU (Only current CPU)}
 if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;

 Result:=ERROR_NOT_ENOUGH_MEMORY;

 {Allocate Entry}
 Entry:=AllocMem(SizeOf(TInterruptEntry));
 if Entry = nil then Exit;

 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.SharedHandler:=Handler;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_DEFAULT;

 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_IPI;

 {Register Entry}
 Result:=ARMGICRegisterEntry(GICDevice,Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4ReleaseIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified IPI number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Mask Count}
 if CPUMaskCount(Mask) <> 1 then Exit;

 {Check Mask CPU (Only current CPU)}
 if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;

 {Clear Entry}
 FillChar(Entry,SizeOf(TInterruptEntry),0);

 {Update Entry}
 Entry.CPUID:=CPUID;
 Entry.Number:=Number;
 Entry.SharedHandler:=Handler;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_DEFAULT;

 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_IPI;

 {Deregister Entry}
 Result:=ARMGICDeregisterEntry(GICDevice,Entry);
end;

{==============================================================================}

function RPi4RegisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified interrupt number (Where Applicable)}
{Number: The interrupt number to register the hanlder for}
{Mask: The mask of CPUs to register the handler for (eg CPU_MASK_0, CPU_MASK_1) (Where Applicable)}
{Priority: The priroty level of the interrupt to be registered (eg INTERRUPT_PRIORITY_MAXIMUM) (Where Applicable)}
{Flags: The flags to control the registration of the interrupt (eg INTERRUPT_FLAG_SHARED) (Where Applicable)}
{Handler: The shared interrupt handler to be called when the interrupt occurs}
{Parameter: A pointer to be passed to the handler when the interrupt occurs (Optional)}
{Return: ERROR_SUCCESS if the callback was scheduled successfully or another error code on failure}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Local or Software}
 if ARMGICIsLocal(GICDevice,Number) or ARMGICIsSoftware(GICDevice,Number) then
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
 if ARMGICIsLocal(GICDevice,Number) then Entry.IsLocal:=True;
 if ARMGICIsSoftware(GICDevice,Number) then Entry.IsIPI:=True;

 {Register Entry}
 Result:=ARMGICRegisterEntry(GICDevice,Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4DeregisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified interrupt number (Where Applicable)}
{Number: The interrupt number to deregister the hanlder for}
{Mask: The mask of CPUs to deregister the handler for (eg CPU_MASK_0, CPU_MASK_1) (Where Applicable)}
{Priority: The priroty level of the interrupt to be deregistered (eg INTERRUPT_PRIORITY_MAXIMUM) (Where Applicable)}
{Flags: The flags to control the deregistration of the interrupt (eg INTERRUPT_FLAG_SHARED, INTERRUPT_FLAG_LOCAL, INTERRUPT_FLAG_FIQ) (Where Applicable)}
{Handler: The shared interrupt handler to be called when the interrupt occurs}
{Parameter: A pointer to be passed to the handler when the interrupt occurs (Optional)}
{Return: ERROR_SUCCESS if the callback was scheduled successfully or another error code on failure}
var
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUGetMask;

 {Check Local or Software}
 if ARMGICIsLocal(GICDevice,Number) or ARMGICIsSoftware(GICDevice,Number) then
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
 if ARMGICIsLocal(GICDevice,Number) then Entry.IsLocal:=True;
 if ARMGICIsSoftware(GICDevice,Number) then Entry.IsIPI:=True;

 {Deregister Entry}
 Result:=ARMGICDeregisterEntry(GICDevice,Entry);
end;

{==============================================================================}

function RPi4RegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request registration of the supplied extended handler to the specified System Call number}
{CPUID: The CPU ID to register the System Call against (or CPU_ID_ALL)}
{Number: The System Call number to be registered}
{Handler: The handler function to be registered}
{HandlerEx: The extended handler function to be registered}
{Note: Only one of Handler or HandlerEx can be specified}
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
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

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

function RPi4DeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request deregistration of the supplied extended handler from the specified System Call number}
{CPUID: The CPU ID to deregister the System Call from (or CPU_ID_ALL)}
{Number: The System Call number to be deregistered}
{Handler: The handler function to be deregistered}
{HandlerEx: The extended handler function to be deregistered}
{Note: Only one of Handler or HandlerEx can be specified}
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
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

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

function RPi4GetInterruptEntry(Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
{Get the interrupt entry for the specified interrupt number and instance}
begin
 {}
 if GICAvailable = RPI4_GIC_AVAILABLE then
  begin
   Result:=ARMGICGetEntry(GICDevice,CPU_ID_ALL,Number,INTERRUPT_FLAG_NONE,Interrupt,Instance);
  end
 else
  begin
   Result:=RPi4GetLegacyEntry(CPU_ID_ALL,Number,INTERRUPT_FLAG_NONE,Interrupt,Instance);
  end;
end;

{==============================================================================}

function RPi4GetLocalInterruptEntry(CPUID,Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
{Get the local interrupt entry for the specified interrupt number and instance}
begin
 {}
 if GICAvailable = RPI4_GIC_AVAILABLE then
  begin
   Result:=ARMGICGetEntry(GICDevice,CPUID,Number,INTERRUPT_FLAG_LOCAL,Interrupt,Instance);
  end
 else
  begin
   Result:=RPi4GetLegacyEntry(CPUID,Number,INTERRUPT_FLAG_LOCAL,Interrupt,Instance);
  end;
end;

{==============================================================================}

function RPi4GetSoftwareInterruptEntry(CPUID,Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
{Get the software interrupt entry for the specified interrupt number and instance}
begin
 {}
 if GICAvailable = RPI4_GIC_AVAILABLE then
  begin
   Result:=ARMGICGetEntry(GICDevice,CPUID,Number,INTERRUPT_FLAG_IPI,Interrupt,Instance);
  end
 else
  begin
   Result:=RPi4GetLegacyEntry(CPUID,Number,INTERRUPT_FLAG_IPI,Interrupt,Instance);
  end;
end;

{==============================================================================}

function RPi4GetSystemCallEntry(Number:LongWord):TSystemCallEntry;
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

function RPi4SystemRestart(Delay:LongWord):LongWord;
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
  WatchdogRegisters.WDOG:=BCM2838_PM_PASSWORD or ((Delay * BCM2838_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2838_PM_WDOG_TIME_MASK);

  {Enable Restart}
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2838_PM_PASSWORD or (Current and BCM2838_PM_RSTC_WRCFG_CLR) or BCM2838_PM_RSTC_WRCFG_FULL_RESET;

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

function RPi4SystemShutdown(Delay:LongWord):LongWord;
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
  {WatchdogRegisters.RSTS:=BCM2838_PM_PASSWORD or (Current and BCM2838_PM_RSTC_WRCFG_CLR) or BCM2838_PM_RSTS_HADWRH_SET;} {RPi firmware changed to use a different value}
  WatchdogRegisters.RSTS:=Current or BCM2838_PM_PASSWORD or BCM2838_PM_RSTS_RASPBERRYPI_HALT;

  {Enable Watchdog}
  WatchdogRegisters.WDOG:=BCM2838_PM_PASSWORD or ((Delay * BCM2838_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2838_PM_WDOG_TIME_MASK);

  {Enable Restart}
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2838_PM_PASSWORD or (Current and BCM2838_PM_RSTC_WRCFG_CLR) or BCM2838_PM_RSTC_WRCFG_FULL_RESET;

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

function RPi4SystemGetCommandLine:String;
{Get the Command Line from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetCommandLine;
begin
 {}
 Result:='';

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetCommandLine) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetCommandLine(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_COMMAND_LINE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetCommandLine) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetCommandLine)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4CPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;
{Get the CPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetARMMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetARMMemory) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetARMMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_ARM_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetARMMemory) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetARMMemory)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4GPUGetState:LongWord;
begin
 {}
 Result:=GPU_STATE_NONE;

 //To Do
end;

{==============================================================================}

function RPi4GPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;
{Get the GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetVCMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetVCMemory) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetVCMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_VC_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetVCMemory) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetVCMemory)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4BoardGetModel:LongWord;
{Get the Board Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Model:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetBoardModel;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetBoardModel) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetBoardModel(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_BOARD_MODEL;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetBoardModel) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetBoardModel)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4BoardGetSerial:Int64;
{Get the Board Serial from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetBoardSerial;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetBoardSerial) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetBoardSerial(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_BOARD_SERIAL;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetBoardSerial) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetBoardSerial)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4BoardGetRevision:LongWord;
{Get the Board Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetBoardRevision;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetBoardRevision) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetBoardRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_BOARD_REV;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetBoardRevision) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetBoardRevision)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4BoardGetMACAddress:String;
{Get the Board MAC Address from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetMACAddress;
begin
 {}
 Result:='';

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetMACAddress) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetMACAddress(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_MAC_ADDRESS;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetMACAddress) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetMACAddress)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4ChipGetRevision:LongWord;
{Get the Chip Revision from the revision register}
begin
 {}
 Result:=PLongWord(BCM2838_CHIP_REVISION_BASE)^;
end;

{==============================================================================}

function RPi4FirmwareGetRevision:LongWord;
{Get the Firmware Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetFirmwareRevision;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetFirmwareRevision) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetFirmwareRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_FIRMWARE_REV;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetFirmwareRevision) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetFirmwareRevision)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4FirmwareGetThrottled:LongWord;
{Get the Firmware Throttling state from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetThrottled;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetThrottled) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetThrottled(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_THROTTLED;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetThrottled) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Value:=$FFFF; {Clear sticky bits}

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetThrottled)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4PowerGetWait(PowerId:LongWord):LongWord;
{Get the Power Wait from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetTiming;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetTiming) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetTiming(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_TIMING;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetTiming) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi4ConvertPowerIdRequest(PowerId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetTiming)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4PowerGetState(PowerId:LongWord):LongWord;
{Get the Power State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetPowerState;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetPowerState) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetPowerState) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi4ConvertPowerIdRequest(PowerId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetPowerState)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerGetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Power State}
  Result:=RPi4ConvertPowerStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4PowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;
{Set the Power State in the Mailbox property tags channel}
{Note: Power Lock not required due to Mailbox Property Call serialization}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetPowerState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetPowerState) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetPowerState) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi4ConvertPowerIdRequest(PowerId);
  Tag.Request.State:=RPi4ConvertPowerStateRequest(State);
  if Wait then Tag.Request.State:=(Tag.Request.State or BCM2838_MBOX_SET_POWER_STATE_REQ_WAIT);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetPowerState)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if Wait then
   begin
    Result:=MailboxPropertyCallEx(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response,RPI4_MAILBOX_TIMEOUT_EX);
   end
  else
   begin
    Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
   end;
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerSetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Check Power State}
  if Wait then
   begin
    if RPi4ConvertPowerStateRequest(State) = BCM2838_MBOX_SET_POWER_STATE_REQ_ON then
     begin
      if (Tag.Response.State and BCM2838_MBOX_POWER_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
     end
    else
     begin
      if (Tag.Response.State and BCM2838_MBOX_POWER_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
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

function RPi4ClockGetCount:LongWord;
{Gets the current system clock count (32 least significant bits of total)}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz and therefore overflows every 4295 seconds}
begin
 {}
 {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
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

function RPi4ClockGetTotal:Int64;
{Gets the total system clock count}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz, the clock interrupt also uses this timer to
 increment the clock every second and therefore keep time}
{$IFDEF RPI4_CLOCK_SYSTEM_TIMER}
var
 Check:LongWord;
{$ENDIF}
begin
 {}
 {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
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

function RPi4ClockGetRate(ClockId:LongWord):LongWord;
{Get the Clock Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetClockRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetClockRate) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetClockRate) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi4ConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetClockRate)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4ClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;
{Set the Clock Rate in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetClockRate;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Rate}
 if Rate = 0 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetClockRate) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetClockRate) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi4ConvertClockIdRequest(ClockId);
  Tag.Request.Rate:=Rate;
  Tag.Request.SkipTurbo:=0;
  if not(Turbo) then Tag.Request.SkipTurbo:=BCM2838_MBOX_CLOCK_RATE_REQ_SKIP_TURBO;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetClockRate)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);

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

function RPi4ClockGetState(ClockId:LongWord):LongWord;
{Get the Clock State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetClockState;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetClockState) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetClockState) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi4ConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetClockState)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Clock State}
  Result:=RPi4ConvertClockStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4ClockSetState(ClockId,State:LongWord):LongWord;
{Set the Clock State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetClockState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetClockState) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetClockState) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi4ConvertClockIdRequest(ClockId);
  Tag.Request.State:=RPi4ConvertClockStateRequest(State);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetClockState)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockSetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Check Clock State}
  if RPi4ConvertClockStateRequest(State) = BCM2838_MBOX_SET_CLOCK_STATE_REQ_ON then
   begin
    if (Tag.Response.State and BCM2838_MBOX_CLOCK_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
   end
  else
   begin
    if (Tag.Response.State and BCM2838_MBOX_CLOCK_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4ClockGetMinRate(ClockId:LongWord):LongWord;
{Get the Clock Min Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetClockMinRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetClockMinRate) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetClockMinRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_CLOCK_MIN_RATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetClockMinRate) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi4ConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetClockMinRate)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4ClockGetMaxRate(ClockId:LongWord):LongWord;
{Get the Clock Max Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetClockMaxRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetClockMaxRate) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetClockMaxRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_CLOCK_MAX_RATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetClockMaxRate) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi4ConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetClockMaxRate)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4ClockGetMeasuredRate(ClockId:LongWord):LongWord;
{Get the Clock Measured Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetClockMeasuredRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetClockMeasuredRate) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetClockMeasuredRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_CLOCK_MEASURED;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetClockMeasuredRate) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi4ConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetClockMeasuredRate)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetMeasuredRate - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Clock Measured Rate}
  Result:=Tag.Response.Rate;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4TurboGetState(TurboId:LongWord):LongWord;
{Get the Turbo State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetTurbo;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetTurbo) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetTurbo) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetTurbo)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4TurboSetState(TurboId,State:LongWord):LongWord;
{Set the Turbo State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetTurbo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetTurbo) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetTurbo) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}
  Tag.Request.Level:=State; {0 to Off / 1 for On}

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetTurbo)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4VoltageGetValue(VoltageId:LongWord):LongWord;
{Get the Voltage Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetVoltage;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetVoltage) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetVoltage) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi4ConvertVoltageIdRequest(VoltageId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetVoltage)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Voltage Value}
  if (Tag.Response.Value <> BCM2838_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4VoltageSetValue(VoltageId,Value:LongWord):LongWord;
{Set the Voltage Value in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetVoltage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Value}
 if Value = 0 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetVoltage) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetVoltage) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi4ConvertVoltageIdRequest(VoltageId);
  Tag.Request.Value:=Value; {Offset from 1.2V in units of 0.025V}

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetVoltage)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageSetValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Check Voltage Value}
  if (Tag.Response.Value <> BCM2838_MBOX_VOLTAGE_INVALID) and (Tag.Response.Value = Value) then Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4VoltageGetMinValue(VoltageId:LongWord):LongWord;
{Get the Voltage Min Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetMinVoltage;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetMinVoltage) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetMinVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_MIN_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetMinVoltage) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi4ConvertVoltageIdRequest(VoltageId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetMinVoltage)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMinValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Voltage Min Value}
  if (Tag.Response.Value <> BCM2838_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4VoltageGetMaxValue(VoltageId:LongWord):LongWord;
{Get the Voltage Max Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetMaxVoltage;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetMaxVoltage) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetMaxVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_MAX_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetMaxVoltage) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi4ConvertVoltageIdRequest(VoltageId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetMaxVoltage)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMaxValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Voltage Max Value}
  if (Tag.Response.Value <> BCM2838_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4TemperatureGetCurrent(TemperatureId:LongWord):LongWord;
{Get the Temperature Current from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetTemperature;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetTemperature) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetTemperature) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPi4ConvertTemperatureIdRequest(TemperatureId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetTemperature)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4TemperatureGetMaximum(TemperatureId:LongWord):LongWord;
{Get the Temperature Maximum Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetMaxTemperature;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetMaxTemperature) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetMaxTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_MAX_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetMaxTemperature) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPi4ConvertTemperatureIdRequest(TemperatureId);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetMaxTemperature)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4GPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
{Allocate GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagAllocateMemory;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagAllocateMemory) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagAllocateMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_ALLOCATE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagAllocateMemory) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Size:=Length;
  Tag.Request.Alignment:=Alignment;
  Tag.Request.Flags:=Flags;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagAllocateMemory)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4GPUMemoryRelease(Handle:THandle):LongWord;
{Release GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagReleaseMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagReleaseMemory) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagReleaseMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_RELEASE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagReleaseMemory) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagReleaseMemory)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4GPUMemoryLock(Handle:THandle):LongWord;
{Lock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagLockMemory;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagLockMemory) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagLockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_LOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagLockMemory) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagLockMemory)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4GPUMemoryUnlock(Handle:THandle):LongWord;
{Unlock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagUnlockMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagUnlockMemory) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagUnlockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_UNLOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagUnlockMemory) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagUnlockMemory)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4GPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;
{Execute GPU Code from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagExecuteCode;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagExecuteCode) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagExecuteCode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_EXECUTE_CODE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagExecuteCode) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
  Tag.Request.R0:=R0;
  Tag.Request.R1:=R1;
  Tag.Request.R2:=R2;
  Tag.Request.R3:=R3;
  Tag.Request.R4:=R4;
  Tag.Request.R5:=R5;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagExecuteCode)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4DispmanxHandleGet(Resource:THandle):THandle;
{Get Dispmanx Memory Handle from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetDispmanxHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetDispmanxHandle) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetDispmanxHandle(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_DISPMANX_HANDLE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetDispmanxHandle) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Resource:=Resource;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetDispmanxHandle)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4EDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Get EDID Block from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetEDIDBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < 128 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetEDIDBlock) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetEDIDBlock(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_EDID_BLOCK;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetEDIDBlock) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Block:=Block;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetEDIDBlock)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
{Allocate Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagAllocateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagAllocateBuffer) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagAllocateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_ALLOCATE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagAllocateBuffer) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Alignment:=Alignment;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagAllocateBuffer)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferRelease:LongWord;
{Release Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagReleaseBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagReleaseBuffer) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_RELEASE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagReleaseBuffer) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagReleaseBuffer)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetState(State:LongWord):LongWord;
{Set Framebuffer State (Blank Screen) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagBlankScreen;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagBlankScreen) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagBlankScreen(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_BLANK_SCREEN;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagBlankScreen) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.State:=0;
  if State = 0 then Tag.Request.State:=BCM2838_MBOX_BLANK_SCREEN_REQ_ON;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagBlankScreen)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord;
{Get Framebuffer Dimensions from the Mailbox property tags channel}
begin
 {}
 {Get Physical}
 Result:=RPi4FramebufferGetPhysical(Width,Height);
 if Result = ERROR_SUCCESS then
  begin
   {Get Overscan}
   Result:=RPi4FramebufferGetOverscan(Top,Bottom,Left,Right);
  end;
end;

{==============================================================================}

function RPi4FramebufferGetPhysical(var Width,Height:LongWord):LongWord;
{Get Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetPhysical) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetPhysical) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetPhysical)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetPhysical(var Width,Height:LongWord):LongWord;
{Set Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetPhysical) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetPhysical) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetPhysical)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestPhysical(var Width,Height:LongWord):LongWord;
{Test Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestPhysical) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestPhysical) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestPhysical)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetVirtual(var Width,Height:LongWord):LongWord;
{Get Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetVirtual) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetVirtual) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetVirtual)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetVirtual(var Width,Height:LongWord):LongWord;
{Set Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetVirtual) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetVirtual) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetVirtual)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestVirtual(var Width,Height:LongWord):LongWord;
{Test Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestVirtual) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestVirtual) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestVirtual)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetDepth(var Depth:LongWord):LongWord;
{Get Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetDepth) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetDepth) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetDepth)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetDepth(var Depth:LongWord):LongWord;
{Set Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetDepth) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetDepth) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetDepth)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestDepth(var Depth:LongWord):LongWord;
{Test Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestDepth) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestDepth) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestDepth)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetPixelOrder(var Order:LongWord):LongWord;
{Get Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetPixelOrder) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetPixelOrder) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetPixelOrder)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetPixelOrder(var Order:LongWord):LongWord;
{Set Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetPixelOrder) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetPixelOrder) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetPixelOrder)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestPixelOrder(var Order:LongWord):LongWord;
{Test Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestPixelOrder) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestPixelOrder) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestPixelOrder)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetAlphaMode(var Mode:LongWord):LongWord;
{Get Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetAlphaMode) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetAlphaMode) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetAlphaMode)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetAlphaMode(var Mode:LongWord):LongWord;
{Set Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetAlphaMode) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetAlphaMode) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetAlphaMode)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestAlphaMode(var Mode:LongWord):LongWord;
{Test Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestAlphaMode) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestAlphaMode) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestAlphaMode)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetPitch:LongWord;
{Get Framebuffer Pitch (Bytes per line) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetPitch;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetPitch) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetPitch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_PITCH;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetPitch) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetPitch)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4FramebufferGetOffset(var X,Y:LongWord):LongWord;
{Get Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetVirtualOffset) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetVirtualOffset) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetVirtualOffset)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetOffset(var X,Y:LongWord):LongWord;
{Set Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetVirtualOffset) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetVirtualOffset) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetVirtualOffset)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestOffset(var X,Y:LongWord):LongWord;
{Test Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestVirtualOffset) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestVirtualOffset) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestVirtualOffset)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Get Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetOverscan) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetOverscan) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetOverscan)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Set Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetOverscan) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetOverscan) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetOverscan)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Test Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestOverscan) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestOverscan) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestOverscan)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
{Get Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetPalette;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < 1024 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetPalette) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetPalette) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetPalette)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Set Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetPalette;
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
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetPalette) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetPalette) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetPalette)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Test Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestPalette;
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
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestPalette) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestPalette) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestPalette)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetLayer(var Layer:LongInt):LongWord;
{Get Framebuffer Layer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetLayer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetLayer) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetLayer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_LAYER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetLayer) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetLayer)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferGetLayer - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Result}
  Layer:=Tag.Response.Layer;

  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4FramebufferSetLayer(var Layer:LongInt):LongWord;
{Set Framebuffer Layer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetLayer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetLayer) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetLayer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_LAYER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetLayer) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Layer:=Layer;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetLayer)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferSetLayer - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Result}
  Layer:=Tag.Response.Layer;

  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4FramebufferTestLayer(var Layer:LongInt):LongWord;
{Test Framebuffer Layer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestLayer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestLayer) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestLayer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_LAYER;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestLayer) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Layer:=Layer;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestLayer)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('FramebufferTestLayer - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Result}
  Layer:=Tag.Response.Layer;

  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi4FramebufferTestVsync:LongWord;
{Test Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagTestVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagTestVsync) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagTestVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_TEST_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagTestVsync) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagTestVsync)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetVsync:LongWord;
{Set Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetVsync) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetVsync) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetVsync)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferSetBacklight(Brightness:LongWord):LongWord;
{Set Framebuffer Backlight Brightness from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetBacklight;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetBacklight) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetBacklight(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_BACKLIGHT;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetBacklight) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Brightness:=Brightness;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetBacklight)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetNumDisplays(var NumDisplays:LongWord):LongWord;
{Get the number of displays from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetNumDisplays;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 NumDisplays:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetNumDisplays) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetNumDisplays(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_NUM_DISPLAYS;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetNumDisplays) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetNumDisplays)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetDisplayId(DisplayNum:LongWord):LongWord;
{Get the display id for the specified display number from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetDisplayId;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetDisplayId) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetDisplayId(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_DISPLAY_ID;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetDisplayId) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetDisplayId)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4FramebufferSetDisplayNum(DisplayNum:LongWord):LongWord;
{Get the display number that all framebuffer requests will refer to using the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetDisplayNum;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetDisplayNum) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetDisplayNum(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_DISPLAY_NUM;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetDisplayNum) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetDisplayNum)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferGetDisplaySettings(DisplayNum:LongWord;var DisplaySettings:TDisplaySettings):LongWord;
{Get the display settings for the specified display number from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetDisplaySettings;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 FillChar(DisplaySettings,SizeOf(TDisplaySettings),0);

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetDisplaySettings) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetDisplaySettings(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_DISPLAY_SETTINGS;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetDisplaySettings) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetDisplaySettings)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4FramebufferDisplayIdToName(DisplayId:LongWord):String;
{Get the name for the specified display id}
begin
 {}
 case DisplayId of
  BCM2838_MBOX_DISPLAY_ID_MAIN_LCD:Result:='Main LCD';
  BCM2838_MBOX_DISPLAY_ID_AUX_LCD:Result:='Aux LCD';
  BCM2838_MBOX_DISPLAY_ID_HDMI0:Result:='HDMI0';
  BCM2838_MBOX_DISPLAY_ID_SDTV:Result:='SDTV';
  BCM2838_MBOX_DISPLAY_ID_FORCE_LCD:Result:='Force LCD';
  BCM2838_MBOX_DISPLAY_ID_FORCE_TV:Result:='Force TV';
  BCM2838_MBOX_DISPLAY_ID_FORCE_OTHER:Result:='Force Other';
  BCM2838_MBOX_DISPLAY_ID_HDMI1:Result:='HDMI1';
  BCM2838_MBOX_DISPLAY_ID_FORCE_TV2:Result:='Force TV2';
 else
  begin
   Result:='Unknown';
  end;
 end;
end;

{==============================================================================}

function RPi4TouchGetBuffer(var Address:PtrUInt):LongWord;
{Get the Touchscreen buffer from the Mailbox property tags channel}

{Note: On current firmware versions calling TouchGetBuffer will allocate a buffer
       from GPU memory and render subsequent calls to TouchSetBuffer ineffective.

       After an initial call to TouchSetBuffer calls to TouchGetBuffer will always
       return the CPU allocated buffer}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetTouch) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetTouch) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetTouch)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4TouchSetBuffer(Address:PtrUInt):LongWord;
{Set the Touchscreen buffer in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetTouch) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetTouch) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetTouch)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4VirtualGPIOGetBuffer(var Address:PtrUInt):LongWord;
{Get the Virtual GPIO buffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetVirtualGPIO;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetVirtualGPIO) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetVirtualGPIO(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_GPIOVIRTBUF;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetVirtualGPIO) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetVirtualGPIO)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4VirtualGPIOSetBuffer(Address:PtrUInt):LongWord;
{Set the Virtual GPIO buffer in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetVirtualGPIO;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetVirtualGPIO) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetVirtualGPIO(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_GPIOVIRTBUF;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetVirtualGPIO) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetVirtualGPIO)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4CursorSetDefault:LongWord;
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
   Result:=RPi4CursorSetInfo(CURSOR_ARROW_DEFAULT_WIDTH,CURSOR_ARROW_DEFAULT_HEIGHT,0,0,Pointer(Address),Size);

   {Free the Cursor}
   FreeMem(Cursor);
  end;
end;

{==============================================================================}

function RPi4CursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
{Set Cursor Info (Pixels) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetCursorInfo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Pixels}
 if Pixels = nil then Exit;

 {Check Length}
 if Length < 1 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetCursorInfo) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetCursorInfo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_CURSOR_INFO;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetCursorInfo) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  Tag.Request.Pixels:=Pixels;
  Tag.Request.HotspotX:=HotspotX;
  Tag.Request.HotspotY:=HotspotY;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetCursorInfo)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4CursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;
{Set Cursor State (Enable, X, Y) from the Mailbox property tags channel}
{Relative: X, Y is relative to Display (Virtual) not Framebuffer (Physical)}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagSetCursorState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagSetCursorState) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagSetCursorState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_SET_CURSOR_STATE;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagSetCursorState) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Enable:=BCM2838_MBOX_CURSOR_INVISIBLE;
  if Enabled then Tag.Request.Enable:=BCM2838_MBOX_CURSOR_VISIBLE;
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
  Tag.Request.Flags:=BCM2838_MBOX_CURSOR_STATE_FRAMEBUFFER_COORDS;
  if Relative then Tag.Request.Flags:=BCM2838_MBOX_CURSOR_STATE_DISPLAY_COORDS;

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagSetCursorState)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi4DMAGetChannels:LongWord;
{Get the available DMA Channels from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagGetDMAChannels;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagGetDMAChannels) + SizeOf(TBCM2838MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2838_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2838MailboxTagGetDMAChannels(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
  Tag.Header.Tag:=BCM2838_MBOX_TAG_GET_DMA_CHANNELS;
  Tag.Header.Size:=SizeOf(TBCM2838MailboxTagGetDMAChannels) - SizeOf(TBCM2838MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagGetDMAChannels)));
  Footer.Tag:=BCM2838_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi4VirtualGPIOAllocate:Boolean;
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
      if (VirtualGPIOBuffer.Buffer <> nil) and (RPi4VirtualGPIOSetBuffer(Address) = ERROR_SUCCESS) then
       begin
        {Update Address}
        VirtualGPIOBuffer.Address:=PtrUInt(VirtualGPIOBuffer.Buffer);
       end
      else
       begin
        {Get Buffer}
        Address:=0;
        if RPi4VirtualGPIOGetBuffer(Address) <> ERROR_SUCCESS then Exit;

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

function RPi4VirtualGPIOInputGet(Pin:LongWord):LongWord;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 {Check Pin}
 if Pin >= BCM2838_VIRTUAL_GPIO_PIN_COUNT then Exit;

 {Check Address}
 if VirtualGPIOBuffer.Address = 0 then
  begin
   {Allocate Buffer}
   if not RPi4VirtualGPIOAllocate then Exit;
  end;

 {Check Address}
 if VirtualGPIOBuffer.Address > 0 then
  begin
   Result:=PLongWord(VirtualGPIOBuffer.Address + (Pin * SizeOf(LongWord)))^;
   Result:=(Result shr Pin) and 1;
  end;
end;

{==============================================================================}

function RPi4VirtualGPIOOutputSet(Pin,Level:LongWord):LongWord;
var
 Enable:Word;
 Disable:Word;
 Difference:SmallInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Pin}
 if Pin >= BCM2838_VIRTUAL_GPIO_PIN_COUNT then Exit;

 {Check Level}
 if Level > GPIO_LEVEL_HIGH then Exit;

 {Check Address}
 if VirtualGPIOBuffer.Address = 0 then
  begin
   {Allocate Buffer}
   if not RPi4VirtualGPIOAllocate then Exit;
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
       CleanDataCacheRange(VirtualGPIOBuffer.Address,BCM2838_VIRTUAL_GPIO_PIN_COUNT * SizeOf(LongWord));
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
       CleanDataCacheRange(VirtualGPIOBuffer.Address,BCM2838_VIRTUAL_GPIO_PIN_COUNT * SizeOf(LongWord));
      end;
    end;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function RPi4VirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Pin}
 if Pin >= BCM2838_VIRTUAL_GPIO_PIN_COUNT then Exit;

 {Check Mode}
 case Mode of
  VIRTUAL_GPIO_FUNCTION_OUT:begin
    Result:=ERROR_SUCCESS;
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{RPi4 Thread Functions}
procedure RPi4SchedulerInit;
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
     RequestExFIQ(RPI4_CPU_BOOT,BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi4SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer FIQ}
     RequestExFIQ(RPI4_CPU_BOOT,BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi4SchedulerInterrupt,nil);
    end;
  end
 else
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer IRQ}
     RequestExIRQ(RPI4_CPU_BOOT,BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi4SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer IRQ}
     RequestExIRQ(RPI4_CPU_BOOT,BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi4SchedulerInterrupt,nil);
    end;
  end;

 {Register the Scheduler SWI}
 RegisterSystemCall(SYSTEM_CALL_CONTEXT_SWITCH,RPi4SchedulerSystemCall);

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
 RPi4SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[RPI4_CPU_BOOT]);
end;

{==============================================================================}

procedure RPi4SchedulerStart(CPUID:LongWord);
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
     RequestExFIQ(CPUID,BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi4SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer FIQ}
     RequestExFIQ(CPUID,BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi4SchedulerInterrupt,nil);
    end;
  end
 else
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer IRQ}
     RequestExIRQ(CPUID,BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi4SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer IRQ}
     RequestExIRQ(CPUID,BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi4SchedulerInterrupt,nil);
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
 RPi4SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);
end;

{==============================================================================}

procedure RPi4SecondaryBoot(CPUID:LongWord);
var
 Timeout:LongWord;
begin
 {}
 {Check CPU}
 if CPUID > (CPUGetCount - 1) then Exit;

 {Setup Timeout}
 Timeout:=RPI4_LOCAL_MAILBOX_TIMEOUT;

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
 ARMLocalRegisters.MailboxWrite[CPUID].Mailbox3Write:=LongWord(@RPi4SecondaryHandler);

 {Synchronization Barrier}
 DataSynchronizationBarrier;

 {Send Event to Wake CPUs}
 SendEvent;

 {Setup Timeout}
 Timeout:=RPI4_LOCAL_MAILBOX_TIMEOUT;

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
{RPi4 SWI Functions}
function RPi4DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle;
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
{RPi4 Clock Functions}
procedure RPi4ClockInterrupt(Parameter:Pointer);
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
 RPi4ClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);

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

procedure RPi4ClockUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a clock interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the timer interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by CLOCK_FREQUENCY}
var
 {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
 Current:LongInt;
 {$ELSE}
 Current:LongWord;
 {$ENDIF}
begin
 {}
 {$IFNDEF RPI4_CLOCK_SYSTEM_TIMER}
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
 TimerRegisters.CS:=BCM2838_SYSTEM_TIMER_CS_3;

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
{RPi4 Scheduler Functions}
function RPi4SchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
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
 RPi4SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);

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

procedure RPi4SchedulerUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a scheduler interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the scheduler interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by RPI4_GENERIC_TIMER_FREQUENCY}
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

procedure RPi4SchedulerSystemCall(Request:PSystemCallRequest);
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
{RPi4 Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPi4FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using the Mailbox Property Tags}
var
 Size:LongWord;
 Count:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Defaults:TFramebufferProperties;
 Palette:array[0..255] of LongWord;
 Tag:PBCM2838MailboxTagCreateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then
  begin
   {Set Current Display}
   if PRPi4Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPi4Framebuffer(Framebuffer).DisplayNum);
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
      Result:=RPi4FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
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
    Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagCreateBuffer) + SizeOf(TBCM2838MailboxFooter);

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
     Header.Code:=BCM2838_MBOX_REQUEST_CODE;

     {Setup Tag}
     Tag:=PBCM2838MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));

     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2838_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2838MailboxTagSetPhysical) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;

     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2838_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2838MailboxTagSetVirtual) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2838_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2838MailboxTagSetDepth) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;

     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2838_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2838MailboxTagSetPixelOrder) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;

     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2838_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2838MailboxTagSetAlphaMode) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;

     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2838_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2838MailboxTagSetVirtualOffset) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;

     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2838_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2838MailboxTagSetOverscan) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;

     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2838_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2838MailboxTagAllocateBuffer) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2711FRAMEBUFFER_ALIGNMENT;

     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2838_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2838MailboxTagGetPitch) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);

     {Setup Footer}
     Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2838_MBOX_TAG_END;

     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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
     if EnvironmentGet('bcm2708_fb.fbswap') <> '1' then
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
    if PRPi4Framebuffer(Framebuffer).MultiDisplay then
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

function RPi4FramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using a simple Mailbox Call}
var
 Response:LongWord;
 Defaults:TFramebufferProperties;
 MailboxFramebuffer:PBCM2838MailboxFramebuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then
  begin
   {Set Current Display}
   if PRPi4Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPi4Framebuffer(Framebuffer).DisplayNum);
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
      Result:=RPi4FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
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
    MailboxFramebuffer:=GetNoCacheAlignedMem(SizeOf(TBCM2838MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
    if MailboxFramebuffer = nil then MailboxFramebuffer:=GetAlignedMem(SizeOf(TBCM2838MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
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
     Result:=MailboxCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_FRAMEBUFFER,PhysicalToBusAddress(MailboxFramebuffer),Response);
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
     RPi4FramebufferGetPixelOrder(Framebuffer.Order);
     RPi4FramebufferGetAlphaMode(Framebuffer.Mode);
     RPi4FramebufferGetOverscan(Framebuffer.OverscanTop,Framebuffer.OverscanBottom,Framebuffer.OverscanLeft,Framebuffer.OverscanRight);

     {Update Statistics}
     Inc(Framebuffer.AllocateCount);

     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(MailboxFramebuffer);
    end;
   finally
    {Set Default Display}
    if PRPi4Framebuffer(Framebuffer).MultiDisplay then
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

function RPi4FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then
  begin
   {Set Current Display}
   if PRPi4Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPi4Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Release Framebuffer}
    Result:=RPi4FramebufferRelease;
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Framebuffer.ReleaseCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    {Set Default Display}
    if PRPi4Framebuffer(Framebuffer).MultiDisplay then
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

function RPi4FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set Current Display}
 if PRPi4Framebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PRPi4Framebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Check Blank}
  if Blank then
   begin
    Result:=RPi4FramebufferSetState(0);
   end
  else
   begin
    Result:=RPi4FramebufferSetState(1);
   end;
 finally
  {Set Default Display}
  if PRPi4Framebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end;
end;

{==============================================================================}

function RPi4FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Flags}
 if (not(BCM2711DMA_CACHE_COHERENT) or ((Flags and FRAMEBUFFER_TRANSFER_DMA) = 0)) and BCM2711FRAMEBUFFER_CACHED then
  begin
   {Clean Cache}
   CleanAndInvalidateDataCacheRange(Address,Size);
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RPi4FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set Current Display}
 if PRPi4Framebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PRPi4Framebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Set Backlight}
  Result:=FramebufferSetBacklight(Brightness);
 finally
  {Set Default Display}
  if PRPi4Framebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end;
end;
{$ENDIF}
{==============================================================================}
{==============================================================================}
{RPi4 Helper Functions}
procedure RPi4Wait; assembler; nostackframe;
{$IFDEF CPUARM}
asm
 //Wait for a period of time in a loop
 mov r0,#0x1F00000 //0x0900000 //0x1F00000
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

procedure RPi4LongWait; assembler; nostackframe;
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

procedure RPi4ShortWait; assembler; nostackframe;
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

procedure RPi4SlowBlink; assembler; nostackframe;
{$IFDEF CPUARM}
asm
 //Slow blink the Activity LED in a loop
 bl RPi4ActivityLEDEnable
.LLoop:
 bl RPi4ActivityLEDOn
 bl RPi4Wait
 bl RPi4ActivityLEDOff
 bl RPi4Wait
 b .LLoop
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi4FastBlink; assembler; nostackframe;
{$IFDEF CPUARM}
asm
 //Fast blink the Activity LED in a loop
 bl RPi4ActivityLEDEnable
.LLoop:
 bl RPi4ActivityLEDOn
 bl RPi4ShortWait
 bl RPi4ActivityLEDOff
 bl RPi4ShortWait
 b .LLoop
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure RPi4BootBlink; assembler; nostackframe;
{Blink the Activity LED without dependency on any other RTL setup}
{$IFDEF CPUARM}
asm
 //Blink the Activity LED in a loop
 //Enable the Activity LED
 ldr r0,=BCM2838_GPIO_REGS_BASE


 //Get the GPIO pull up/down
 ldr r2, [r0,#RPI4_GPIO_ACTLED_GPPUD]

 //Mask of the relevant bits
 mov r1, #RPI4_GPIO_ACTLED_GPPUDMASK
 lsl r1, #RPI4_GPIO_ACTLED_GPPUDSHIFT
 bic r2, r1

 //Clear the 20th bit of r1
 mov r1,#0
 lsl r1,#RPI4_GPIO_ACTLED_GPPUDSHIFT

 //Add the new bits
 orr r2, r1

 //Set the GPIO pull up/down
 str r2,[r0,#RPI4_GPIO_ACTLED_GPPUD]


 //Get the GPIO function select
 ldr r2, [r0,#RPI4_GPIO_ACTLED_GPFSEL]

 //Mask of the relevant bits
 mov r1, #RPI4_GPIO_ACTLED_GPFMASK
 lsl r1, #RPI4_GPIO_ACTLED_GPFSHIFT
 bic r2, r1

 //Set the 6th bit of r1
 mov r1,#1
 lsl r1,#RPI4_GPIO_ACTLED_GPFSHIFT

 //Add the new bits
 orr r2, r1

 //Set the GPIO function select
 str r2,[r0,#RPI4_GPIO_ACTLED_GPFSEL]


.LLoop:
 //Turn on the Activity LED
 ldr r0,=BCM2838_GPIO_REGS_BASE

 //Set the 10th bit of r1
 mov r1,#1
 lsl r1,#RPI4_GPIO_ACTLED_GPSHIFT

 //Set GPIO 42 to high, causing the LED to turn on
 str r1,[r0,#RPI4_GPIO_ACTLED_GPSET]

 //Wait
 bl RPi4Wait

 //Turn off the Activity LED
 ldr r0,=BCM2838_GPIO_REGS_BASE

 //Set the 10th bit of r1
 mov r1,#1
 lsl r1,#RPI4_GPIO_ACTLED_GPSHIFT

 //Set GPIO 42 to low, causing the LED to turn off
 str r1,[r0,#RPI4_GPIO_ACTLED_GPCLR]

 //Wait
 bl RPi4Wait

 b .LLoop
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}
{==============================================================================}

procedure RPi4BootOutput(Value:LongWord);
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

   PLongWord(BCM2838_UART0_REGS_BASE)^:=Character;

   if Bits = 0 then Break;
  end;

 {Line End}
 PLongWord(BCM2838_UART0_REGS_BASE)^:=$0D;
 PLongWord(BCM2838_UART0_REGS_BASE)^:=$0A;
end;

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi4BootConsoleStart;
begin
 ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
end;

{==============================================================================}

procedure RPi4BootConsoleWrite(const Value:String);
begin
 ConsoleWindowWriteLn(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),Value);
end;

{==============================================================================}

procedure RPi4BootConsoleWriteEx(const Value:String;X,Y:LongWord);
begin
 ConsoleWindowSetXY(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),X,Y);
 ConsoleWindowWriteLn(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),Value);
end;

{==============================================================================}

function RPi4BootConsoleGetX:LongWord;
begin
 Result:=ConsoleWindowGetX(ConsoleWindowGetDefault(ConsoleDeviceGetDefault));
end;

{==============================================================================}

function RPi4BootConsoleGetY:LongWord;
begin
 Result:=ConsoleWindowGetY(ConsoleWindowGetDefault(ConsoleDeviceGetDefault));
end;
{$ENDIF}
{==============================================================================}

function RPi4ConvertPowerIdRequest(PowerId:LongWord):LongWord;
{Convert Ultibo Power Id to BCM2838 Power Id}
begin
 {}
 Result:=BCM2838_MBOX_POWER_DEVID_UNKNOWN;

 case PowerId of
  POWER_ID_MMC0:Result:=BCM2838_MBOX_POWER_DEVID_SDHCI;
  POWER_ID_MMC2:Result:=BCM2838_MBOX_POWER_DEVID_SDHCI;
  POWER_ID_UART0:Result:=BCM2838_MBOX_POWER_DEVID_UART0;
  POWER_ID_UART1:Result:=BCM2838_MBOX_POWER_DEVID_UART1;
  POWER_ID_USB0:Result:=BCM2838_MBOX_POWER_DEVID_USB_HCD;
  POWER_ID_I2C0:Result:=BCM2838_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C1:Result:=BCM2838_MBOX_POWER_DEVID_I2C1;
  POWER_ID_I2C2:Result:=BCM2838_MBOX_POWER_DEVID_I2C2;
  POWER_ID_SPI0:Result:=BCM2838_MBOX_POWER_DEVID_SPI;
  POWER_ID_CCP2TX:Result:=BCM2838_MBOX_POWER_DEVID_CCP2TX;
  {Additional devices}
  POWER_ID_UART2:Result:=BCM2838_MBOX_POWER_DEVID_UART0;
  POWER_ID_UART3:Result:=BCM2838_MBOX_POWER_DEVID_UART0;
  POWER_ID_UART4:Result:=BCM2838_MBOX_POWER_DEVID_UART0;
  POWER_ID_UART5:Result:=BCM2838_MBOX_POWER_DEVID_UART0;
  POWER_ID_I2C3:Result:=BCM2838_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C4:Result:=BCM2838_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C5:Result:=BCM2838_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C6:Result:=BCM2838_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C7:Result:=BCM2838_MBOX_POWER_DEVID_I2C0;
  POWER_ID_SPI3:Result:=BCM2838_MBOX_POWER_DEVID_SPI;
  POWER_ID_SPI4:Result:=BCM2838_MBOX_POWER_DEVID_SPI;
  POWER_ID_SPI5:Result:=BCM2838_MBOX_POWER_DEVID_SPI;
  POWER_ID_SPI6:Result:=BCM2838_MBOX_POWER_DEVID_SPI;
 end;
end;

{==============================================================================}

function RPi4ConvertPowerStateRequest(PowerState:LongWord):LongWord;
{Convert Ultibo Power State to BCM2838 Power State}
begin
 {}
 Result:=BCM2838_MBOX_SET_POWER_STATE_REQ_OFF;

 case PowerState of
  POWER_STATE_OFF:Result:=BCM2838_MBOX_SET_POWER_STATE_REQ_OFF;
  POWER_STATE_ON:Result:=BCM2838_MBOX_SET_POWER_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPi4ConvertPowerStateResponse(PowerState:LongWord):LongWord;
{Convert BCM2838 Power State to Ultibo Power State}
begin
 {}
 Result:=POWER_STATE_OFF;

 case PowerState of
  BCM2838_MBOX_POWER_STATE_RESP_OFF:Result:=POWER_STATE_OFF;
  BCM2838_MBOX_POWER_STATE_RESP_ON:Result:=POWER_STATE_ON;
 end;
end;

{==============================================================================}

function RPi4ConvertClockIdRequest(ClockId:LongWord):LongWord;
{Convert Ultibo Clock Id to BCM2838 Clock Id}
begin
 {}
 Result:=BCM2838_MBOX_CLOCK_ID_UNKNOWN;

 case ClockId of
  CLOCK_ID_MMC0:Result:=BCM2838_MBOX_CLOCK_ID_EMMC;
  CLOCK_ID_MMC1:Result:=BCM2838_MBOX_CLOCK_ID_CORE; {MMC1 runs from core clock}
  CLOCK_ID_MMC2:Result:=BCM2838_MBOX_CLOCK_ID_EMMC2;
  CLOCK_ID_UART0:Result:=BCM2838_MBOX_CLOCK_ID_UART;
  CLOCK_ID_UART1:Result:=BCM2838_MBOX_CLOCK_ID_CORE; {UART1 runs from core clock}
  CLOCK_ID_CPU:Result:=BCM2838_MBOX_CLOCK_ID_ARM;
  CLOCK_ID_CORE:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_GPU:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_V3D:Result:=BCM2838_MBOX_CLOCK_ID_V3D;
  CLOCK_ID_H264:Result:=BCM2838_MBOX_CLOCK_ID_H264;
  CLOCK_ID_ISP:Result:=BCM2838_MBOX_CLOCK_ID_ISP;
  CLOCK_ID_SDRAM:Result:=BCM2838_MBOX_CLOCK_ID_SDRAM;
  CLOCK_ID_PIXEL:Result:=BCM2838_MBOX_CLOCK_ID_PIXEL;
  CLOCK_ID_PWM0:Result:=BCM2838_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_PWM1:Result:=BCM2838_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_I2C0:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C1:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C2:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI0:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI1:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI2:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  {Additional devices}
  CLOCK_ID_UART2:Result:=BCM2838_MBOX_CLOCK_ID_UART;
  CLOCK_ID_UART3:Result:=BCM2838_MBOX_CLOCK_ID_UART;
  CLOCK_ID_UART4:Result:=BCM2838_MBOX_CLOCK_ID_UART;
  CLOCK_ID_UART5:Result:=BCM2838_MBOX_CLOCK_ID_UART;
  CLOCK_ID_I2C3:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C4:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C5:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C6:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C7:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI3:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI4:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI5:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI6:Result:=BCM2838_MBOX_CLOCK_ID_CORE;
 end;
end;

{==============================================================================}

function RPi4ConvertClockStateRequest(ClockState:LongWord):LongWord;
{Convert Ultibo Clock State to BCM2838 Clock State}
begin
 {}
 Result:=BCM2838_MBOX_SET_CLOCK_STATE_REQ_OFF;

 case ClockState of
  CLOCK_STATE_OFF:Result:=BCM2838_MBOX_SET_CLOCK_STATE_REQ_OFF;
  CLOCK_STATE_ON:Result:=BCM2838_MBOX_SET_CLOCK_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPi4ConvertClockStateResponse(ClockState:LongWord):LongWord;
{Convert BCM2838 Clock State to Ultibo Clock State}
begin
 {}
 Result:=CLOCK_STATE_OFF;

 case ClockState of
  BCM2838_MBOX_CLOCK_STATE_RESP_OFF:Result:=CLOCK_STATE_OFF;
  BCM2838_MBOX_CLOCK_STATE_RESP_ON:Result:=CLOCK_STATE_ON;
 end;
end;

{==============================================================================}

function RPi4ConvertVoltageIdRequest(VoltageId:LongWord):LongWord;
{Convert Ultibo Voltage Id to BCM2838 Voltage Id}
begin
 {}
 Result:=BCM2838_MBOX_VOLTAGE_ID_RESERVED;

 case VoltageId of
  VOLTAGE_ID_CORE:Result:=BCM2838_MBOX_VOLTAGE_ID_CORE;
  VOLTAGE_ID_SDRAM_C:Result:=BCM2838_MBOX_VOLTAGE_ID_SDRAM_C;
  VOLTAGE_ID_SDRAM_P:Result:=BCM2838_MBOX_VOLTAGE_ID_SDRAM_P;
  VOLTAGE_ID_SDRAM_I:Result:=BCM2838_MBOX_VOLTAGE_ID_SDRAM_I;
 end;
end;

{==============================================================================}

function RPi4ConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;
{Convert Ultibo Temperature Id to BCM2838 Temperature Id}
begin
 {}
 Result:=BCM2838_MBOX_TEMP_ID_SOC;

 case TemperatureId of
  TEMPERATURE_ID_SOC:Result:=BCM2838_MBOX_TEMP_ID_SOC;
 end;
end;

{==============================================================================}
{==============================================================================}
{RPi4 Internal Functions}
function RPi4TestGICAvailable:Boolean;
{Test the GIC device to see if an interrupt is triggerred when enabled}
{Note: Must only be called by InterruptInit during boot to test if the GIC is enabled}

 function TestGICGetVector(Number:LongWord):PtrUInt;
 var
  Offset:PtrUInt;
 begin
   if RPi4PageTableInitialized then
    begin
     Result:=VectorTableGetEntry(Number);
    end
   else
    begin
     Result:=0;

     {Check Number}
     if Number >= VECTOR_TABLE_COUNT then Exit;

     {Calculate Offset}
     {$IFDEF CPUARM}
     Offset:=VECTOR_TABLE_BASE + (Number shl 2) + 32; {Vector entries use "ldr pc, [pc, #24]" for each entry}
     {$ENDIF CPUARM}
     {$IFDEF CPUAARCH64}
     //To Do
     {$ENDIF CPUAARCH64}

     {Get Vector}
     Result:=PPtrUInt(Offset)^;
    end;
 end;

 function TestGICSetVector(Number:LongWord;Address:PtrUInt):LongWord;
 var
  Offset:PtrUInt;
 begin
   if RPi4PageTableInitialized then
    begin
     Result:=VectorTableSetEntry(Number,Address);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;

     {Check Number}
     if Number >= VECTOR_TABLE_COUNT then Exit;

     {Check Address}
     {Zero may be valid}

     {Calculate Offset}
     {$IFDEF CPUARM}
     Offset:=VECTOR_TABLE_BASE + (Number shl 2) + 32; {Vector entries use "ldr pc, [pc, #24]" for each entry}
     {$ENDIF CPUARM}
     {$IFDEF CPUAARCH64}
     //To Do
     {$ENDIF CPUAARCH64}

     {Set Vector}
     PPtrUInt(Offset)^:=Address;
    end;
 end;

var
 State:LongWord;
 Vector:PtrUInt;
begin
 {}
 Result:=False;

 {Check GIC}
 if GICDevice = nil then Exit;

 {Set Status}
 GICStatus:=RPI4_GIC_UNAVAILABLE;

 {Save the current IRQ Handler}
 Vector:=TestGICGetVector(VECTOR_TABLE_ENTRY_ARM_IRQ);
 try
  {Set a temporary IRQ Handler}
  TestGICSetVector(VECTOR_TABLE_ENTRY_ARM_IRQ,PtrUInt(@RPi4TestGICIRQHandler));

  {Register an Interrupt Handler for the Generic Timer (To enable the timer interrupt)}
  if RPi4RegisterInterrupt(BCM2838_IRQ_LOCAL_ARM_CNTVIRQ,CPUIDToMask(CPUGetCurrent),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,RPi4TestGICDummyHandler,@GICStatus) = ERROR_SUCCESS then
   begin
    {Enable the Generic Timer}
    {$IFDEF CPUARM}
    State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTV);
    State:=State and not(ARMV7_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
    State:=State or ARMV7_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
    ARMv7SetTimerState(ARMV7_CP15_C14_CNTV,State);

    {Configure the Generic Timer (1 millisecond interval)}
    ARMv7SetTimerValue(ARMV7_CP15_C14_CNTV,CLOCK_CYCLES_PER_MILLISECOND);
    {$ENDIF CPUARM}
    {$IFDEF CPUAARCH64}
    State:=ARMv8GetTimerState(ARMV8_CP15_C14_CNTV);
    State:=State and not(ARMV8_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
    State:=State or ARMV8_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
    ARMv8SetTimerState(ARMV8_CP15_C14_CNTV,State);

    {Configure the Generic Timer (1 millisecond interval)}
    ARMv8SetTimerValue(ARMV8_CP15_C14_CNTV,CLOCK_CYCLES_PER_MILLISECOND);
    {$ENDIF CPUAARCH64}

    {Enable IRQ}
    EnableIRQ;

    {Wait 2 milliseconds}
    MillisecondDelay(2);

    {Disable IRQ}
    DisableIRQ;

    {Disable the Generic Timer}
    {$IFDEF CPUARM}
    State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTV);
    State:=State and not(ARMV7_CP15_C14_CNT_CTL_ENABLE); {Clear the enable bit}
    State:=State or ARMV7_CP15_C14_CNT_CTL_IMASK;        {Set the mask bit}
    ARMv7SetTimerState(ARMV7_CP15_C14_CNTV,State);
    {$ENDIF CPUARM}
    {$IFDEF CPUAARCH64}
    State:=ARMv8GetTimerState(ARMV8_CP15_C14_CNTV);
    State:=State and not(ARMV8_CP15_C14_CNT_CTL_ENABLE); {Clear the enable bit}
    State:=State or ARMV8_CP15_C14_CNT_CTL_IMASK;        {Set the mask bit}
    ARMv8SetTimerState(ARMV8_CP15_C14_CNTV,State);
    {$ENDIF CPUAARCH64}

    {Deregister the Interrupt Handler (To disable the timer interrupt)}
    RPi4DeregisterInterrupt(BCM2838_IRQ_LOCAL_ARM_CNTVIRQ,CPUIDToMask(CPUGetCurrent),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,RPi4TestGICDummyHandler,@GICStatus);

    {Check Result}
    Result:=(GICStatus = RPI4_GIC_AVAILABLE);
   end;
 finally
  {Restore the original IRQ Handler}
  TestGICSetVector(VECTOR_TABLE_ENTRY_ARM_IRQ,Vector);
 end;
end;

{==============================================================================}

procedure RPi4TestGICIRQHandler; assembler; nostackframe;
{Temporary IRQ handler for use only by the GIC test function above}
{$IFDEF CPUARM}
asm
 //On entry, processor will be in IRQ mode, IRQ will be disabled and SP will point to the IRQ stack
 //See: A2.6.8 Interrupt request (IRQ) exception in the ARM Architecture Reference Manual (arm_arm)

 //Adjust the IRQ mode link register (LR_irq) for the return
 //See: A2.6.8 Interrupt request (IRQ) exception in the ARM Architecture Reference Manual (arm_arm)
 sub lr, lr, #4

 //Store Return State (SRSDB) on the SYS mode stack which will be the stack of the interrupted thread
 //This will store the IRQ mode link register (LR_irq) and saved program status register (SPSR_irq)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d051f  //srsdb #ARM_MODE_SYS!

 //Change Program State (CPSID) to SYS mode with IRQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid i, #ARM_MODE_SYS

 //Save r0 and r1 to the SYS mode stack so they can be used to disable the timer
 push {r0, r1}

 //Get the Virtual Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1544 of the ARM Architecture Reference Manual
 mrc p15, #0, r0, cr14, cr3, #1

 //Clear the enable bit
 bic r0, r0, #ARMV7_CP15_C14_CNT_CTL_ENABLE

 //Set the mask bit
 orr r0, r0, #ARMV7_CP15_C14_CNT_CTL_IMASK

 //Set the Virtual Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1544 of the ARM Architecture Reference Manual
 mcr p15, #0, r0, cr14, cr3, #1

 //Update the GIC Status
 ldr r0, .LGICStatus
 mov r1, #RPI4_GIC_AVAILABLE
 str r1, [r0]

 //Restore r0 and r1 from the SYS mode stack ready for return from interrupt
 pop {r0, r1}

 //Return From Exception (RFEIA) loading PC and CPSR from the SYS mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!

.LGICStatus:
  .long GICStatus

 //Note: Compiler adds "mov    pc, lr" or "bx lr" to the end of this. Should not be an issue because of rfe above
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

function RPi4TestGICDummyHandler(Number,CPUID,Flags:LongWord;Parameter:Pointer):LongWord;
{Note: This ia a dummy handler used to satisfy the call to RPi4RegisterInterrupt above}
begin
 {}
 Result:=INTERRUPT_RETURN_HANDLED;
end;

{==============================================================================}

function RPi4RequestExLegacyIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied extended handler to the specified legacy IRQ number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Local}
 if RPi4LegacyIsLocal(Number) then
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
 if RPi4LegacyIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL;

 {Register Entry}
 Result:=RPi4RegisterLegacyEntry(Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4ReleaseExLegacyIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied extended handler from the specified legacy IRQ number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Local}
 if RPi4LegacyIsLocal(Number) then
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
 if RPi4LegacyIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL;

 {Deregister Entry}
 Result:=RPi4DeregisterLegacyEntry(Entry);
end;

{==============================================================================}

function RPi4RequestExLegacyFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied extended handler to the specified legacy FIQ number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Local}
 if RPi4LegacyIsLocal(Number) then
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
 if RPi4LegacyIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL or INTERRUPT_FLAG_FIQ;

 {Register Entry}
 Result:=RPi4RegisterLegacyEntry(Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4ReleaseExLegacyFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied extended handler from the specified legacy FIQ number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(@Handler)) and not(Assigned(@HandlerEx)) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Local}
 if RPi4LegacyIsLocal(Number) then
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
 if RPi4LegacyIsLocal(Number) then Entry.Flags:=INTERRUPT_FLAG_LOCAL or INTERRUPT_FLAG_FIQ;

 {Deregister Entry}
 Result:=RPi4DeregisterLegacyEntry(Entry);
end;

{==============================================================================}

function RPi4RequestLegacyIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified legacy IPI number}
var
 Mask:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Mask Count}
 if CPUMaskCount(Mask) <> 1 then Exit;

 {Check Mask CPU (Only current CPU)}
 if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;

 Result:=ERROR_NOT_ENOUGH_MEMORY;

 {Allocate Entry}
 Entry:=AllocMem(SizeOf(TInterruptEntry));
 if Entry = nil then Exit;

 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.SharedHandler:=Handler;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_DEFAULT;

 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_IPI;

 {Register Entry}
 Result:=RPi4RegisterLegacyEntry(Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4ReleaseLegacyIPI(CPUID,Number:LongWord;Handler:TIPIHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified legacy IPI number}
var
 Mask:LongWord;
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUID);

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Mask Count}
 if CPUMaskCount(Mask) <> 1 then Exit;

 {Check Mask CPU (Only current CPU)}
 if CPUMaskToID(Mask) <> CPUGetCurrent then Exit;

 {Clear Entry}
 FillChar(Entry,SizeOf(TInterruptEntry),0);

 {Update Entry}
 Entry.CPUID:=CPUID;
 Entry.Number:=Number;
 Entry.SharedHandler:=Handler;
 Entry.Parameter:=Parameter;
 Entry.Priority:=INTERRUPT_PRIORITY_DEFAULT;

 {Get Flags}
 Entry.Flags:=INTERRUPT_FLAG_IPI;

 {Deregister Entry}
 Result:=RPi4DeregisterLegacyEntry(Entry);
end;

{==============================================================================}

function RPi4RegisterLegacyInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified legacy interrupt number (Where Applicable)}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Local or Software}
 if RPi4LegacyIsLocal(Number) or RPi4LegacyIsSoftware(Number) then
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
 if RPi4LegacyIsLocal(Number) then Entry.IsLocal:=True;
 if RPi4LegacyIsSoftware(Number) then Entry.IsIPI:=True;

 {Register Entry}
 Result:=RPi4RegisterLegacyEntry(Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPi4DeregisterLegacyInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified legacy interrupt number (Where Applicable)}
var
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Check Mask}
 if Mask = CPU_MASK_NONE then Exit;
 if Mask = CPU_MASK_ALL then Mask:=CPUIDToMask(CPUGetCurrent); {Legacy only supports single CPU per interrupt}

 {Remap ETH PCIe}
 Number:=RPi4MapETHPCIToVC(Number);

 {Check Local or Software}
 if RPi4LegacyIsLocal(Number) or RPi4LegacyIsSoftware(Number) then
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
 if RPi4LegacyIsLocal(Number) then Entry.IsLocal:=True;
 if RPi4LegacyIsSoftware(Number) then Entry.IsIPI:=True;

 {Deregister Entry}
 Result:=RPi4DeregisterLegacyEntry(Entry);
end;

{==============================================================================}

function RPi4LegacyIsValid(Number:LongWord):Boolean;
var
 LegacyNumber:LongWord;
begin
 {}
 Result:=False;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Legacy}
 Result:=(LegacyNumber < RPI4_LEGACY_IRQ_COUNT);
end;

{==============================================================================}

function RPi4LegacyIsLocal(Number:LongWord):Boolean;
var
 LegacyNumber:LongWord;
begin
 {}
 Result:=False;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Legacy}
 Result:=(LegacyNumber >= RPI4_LEGACY_IRQ_LOCAL_START) and (LegacyNumber < RPI4_LEGACY_IRQ_COUNT);
end;

{==============================================================================}

function RPi4LegacyIsSoftware(Number:LongWord):Boolean;
var
 LegacyNumber:LongWord;
begin
 {}
 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Legacy}
 Result:=(LegacyNumber >= RPI4_LEGACY_IRQ_SOFTWARE_START) and (LegacyNumber < RPI4_LEGACY_IRQ_SOFTWARE_START + RPI4_ARM_SOFTWARE_LEGACY_IRQ_COUNT);
end;

{==============================================================================}

function RPi4LegacyIsGlobal(Number:LongWord):Boolean;
var
 LegacyNumber:LongWord;
begin
 {}
 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Legacy}
 Result:=(LegacyNumber < RPI4_LEGACY_IRQ_LOCAL_START);
end;

{==============================================================================}

function RPi4MapETHPCIToVC(Number:LongWord):LongWord;
{Map all of the ETH PCIe interrupts to the common VC ETH PCIe interrupt
 as they do not appear individually on the legacy interrupt controller}
begin
 {}
 Result:=Number;

 {ARM ETH_PCIe L2 IRQs (BCM2838_SPI_BASE + 128 to 184)}
 case Number of
  {Mapped to BCM2838_IRQ_ETH_PCIE}
  BCM2838_IRQ_THERMAL..BCM2838_IRQ_XHCI:Result:=BCM2838_IRQ_ETH_PCIE;
 end;
end;

{==============================================================================}

function RPi4MapGICToLegacy(Number:LongWord):LongWord;
{Map a GIC interrupt number (listed in BCM2838) to a legacy interrupt}
begin
 {}
 Result:=LongWord(-1);

 {VC peripheral IRQs (BCM2838_SPI_BASE + 64 to 127)}
 case Number of
  {Mapped to Legacy IRQs 0..63}
  BCM2838_IRQ_SYSTEM_TIMER_0..BCM2838_IRQ_ETH_PCIE_SEC:Result:=Number - 96;
 end;

 {ARM peripheral IRQs (BCM2838_SPI_BASE + 32 to 47)}
 case Number of
  {Mapped to Legacy IRQs 64..95 (64..79)}
  BCM2838_IRQ_ARM_TIMER..BCM2838_IRQ_ARM_SWI7:Result:=Number;
 end;

 {ARM Local Interrupts}
 case Number of
  {Mapped to Legacy IRQs 96..127}
  {ARM Generic Timers}
  BCM2838_IRQ_TIMER0:Result:=96;          {BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ}
  BCM2838_IRQ_TIMER1:Result:=97;          {BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ}
  BCM2838_IRQ_TIMER2:Result:=99;          {BCM2838_IRQ_LOCAL_ARM_CNTVIRQ}
  BCM2838_IRQ_TIMER3:Result:=98;          {BCM2838_IRQ_LOCAL_ARM_CNTHPIRQ}

  {ARM Mailboxes0-3}
  BCM2838_IRQ_MAILBOX0_0,
  BCM2838_IRQ_MAILBOX0_1,
  BCM2838_IRQ_MAILBOX0_2,
  BCM2838_IRQ_MAILBOX0_3:Result:=100;     {BCM2838_IRQ_LOCAL_ARM_MAILBOX0}

  BCM2838_IRQ_MAILBOX1_0,
  BCM2838_IRQ_MAILBOX1_1,
  BCM2838_IRQ_MAILBOX1_2,
  BCM2838_IRQ_MAILBOX1_3:Result:=101;     {BCM2838_IRQ_LOCAL_ARM_MAILBOX1}

  BCM2838_IRQ_MAILBOX2_0,
  BCM2838_IRQ_MAILBOX2_1,
  BCM2838_IRQ_MAILBOX2_2,
  BCM2838_IRQ_MAILBOX2_3:Result:=102;     {BCM2838_IRQ_LOCAL_ARM_MAILBOX2}

  BCM2838_IRQ_MAILBOX3_0,
  BCM2838_IRQ_MAILBOX3_1,
  BCM2838_IRQ_MAILBOX3_2,
  BCM2838_IRQ_MAILBOX3_3:Result:=103;     {BCM2838_IRQ_LOCAL_ARM_MAILBOX3}

  {ARM PMU}
  BCM2838_IRQ_PMU0,
  BCM2838_IRQ_PMU1,
  BCM2838_IRQ_PMU2,
  BCM2838_IRQ_PMU3:Result:=105;           {BCM2838_IRQ_LOCAL_ARM_PMU}

  {AXI Quiet}
  BCM2838_IRQ_LOCAL_AXIQUIET:Result:=106; {BCM2838_IRQ_LOCAL_ARM_AXIQUIET}

  {ARM Local Timer}
  BCM2838_IRQ_LOCAL_TIMER:Result:=107;    {BCM2838_IRQ_LOCAL_ARM_TIMER}

  {AXI Error}
  BCM2838_IRQ_LOCAL_AXIERR:Result:=126;   {BCM2838_IRQ_LOCAL_ARM_AXIERR}
 end;
end;

{==============================================================================}

function RPi4MapLegacyToGIC(CPUID,Number:LongWord):LongWord;
{Map a legacy interrupt number to a GIC interrupt (listed in BCM2838)}
begin
 {}
 Result:=LongWord(-1);

 {VC Legacy IRQs 0..63}
 case Number of
  {Mapped to peripheral IRQs (BCM2838_SPI_BASE + 64 to 127)}
  0..63:Result:=Number + 96;
 end;

 {ARM Legacy IRQs 64..95 (64..79)}
 case Number of
  {Mapped to peripheral IRQs (BCM2838_SPI_BASE + 32 to 47)}
  64..95:Result:=Number;
 end;

 {ARM Local Legacy IRQs 96..127}
 case Number of
  {Mapped to Local Interrupts}
  {ARM Generic Timers}
  96:Result:=BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ;
  97:Result:=BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ;
  99:Result:=BCM2838_IRQ_LOCAL_ARM_CNTVIRQ;
  98:Result:=BCM2838_IRQ_LOCAL_ARM_CNTHPIRQ;

  {ARM Mailboxes0-3}
  100:Result:=BCM2838_IRQ_LOCAL_ARM_MAILBOX0[CPUID];
  101:Result:=BCM2838_IRQ_LOCAL_ARM_MAILBOX1[CPUID];
  102:Result:=BCM2838_IRQ_LOCAL_ARM_MAILBOX2[CPUID];
  103:Result:=BCM2838_IRQ_LOCAL_ARM_MAILBOX3[CPUID];

  {ARM PMU}
  105:Result:=BCM2838_IRQ_LOCAL_ARM_PMU[CPUID];

  {AXI Quiet}
  106:Result:=BCM2838_IRQ_LOCAL_ARM_AXIQUIET;

  {ARM Local Timer}
  107:Result:=BCM2838_IRQ_LOCAL_ARM_TIMER;

  {AXI Error}
  126:Result:=BCM2838_IRQ_LOCAL_ARM_AXIERR;
 end;
end;

{==============================================================================}

function RPi4LegacyCheckValid(const Entry:TInterruptEntry):Boolean;
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
   if not RPi4LegacyIsLocal(Entry.Number) then Exit;

   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;

   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Entry.CPUMask) <> CPUGetCurrent then Exit;
  end
 else if Entry.IsIPI then
  begin
   {Check Number (Software)}
   if not RPi4LegacyIsSoftware(Entry.Number) then Exit;

   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;

   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Entry.CPUMask) <> CPUGetCurrent then Exit;
  end
 else
  begin
   {Check Number (Global)}
   if not RPi4LegacyIsGlobal(Entry.Number) then Exit;

   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;

   {Check Mask CPUs (Legacy only supports single CPU per interrupt)}
   if (IRQ_ROUTING <> CPU_ID_ALL) and (IRQ_ROUTING <> CPUMaskToID(Entry.CPUMask)) then Exit;
  end;

 {Check Handlers}
 if not RPi4LegacyCheckHandlers(Entry) then Exit;

 {Check Priority}
 {Not applicable}

 {Check FIQ}
 if Entry.IsFIQ then
  begin
   if not FIQ_ENABLED then Exit;
  end;

 {Check IPI}
 if Entry.IsIPI then
  begin
   if not Assigned(Entry.SharedHandler) then Exit;
  end;

 {Check Shared}
 if Entry.IsShared then
  begin
   if not Assigned(Entry.SharedHandler) then Exit;
  end;

 Result:=True;
end;

{==============================================================================}

function RPi4LegacyCheckHandlers(const Entry:TInterruptEntry):Boolean;
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

function RPi4LegacyCompareHandlers(const Entry,Current:TInterruptEntry):Boolean;
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

function RPi4LegacyEnable(const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Group:LongWord;
 Offset:LongWord;
 Enable:LongWord;
 LegacyNumber:LongWord;
begin
 {}
 Result:=False;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Entry.Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Get Group and Offset}
 if LegacyNumber < 32 then
  begin
   Group:=0;
   Offset:=0;
  end
 else if LegacyNumber < 64 then
  begin
   Group:=1;
   Offset:=32;
  end
 else if LegacyNumber < 96 then
  begin
   Group:=2;
   Offset:=64;
  end
 else
  begin
   Group:=LongWord(-1);
   Offset:=96;
  end;

 {Check Source}
 if LegacyNumber < 96 then
  begin
   {Global}
   if Entry.IsFIQ then
    begin
     {Check IRQ}
     if (LegacyIRQEnabled[Group] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}

     {Enable FIQ}
     PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_SET_EN_0 + (Group * SizeOf(LongWord)) + (Entry.CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=(1 shl (LegacyNumber - Offset));
     LegacyFIQEnabled[Group]:=LegacyFIQEnabled[Group] or (1 shl (LegacyNumber - Offset));
    end
   else
    begin
     {Check FIQ}
     if (LegacyFIQEnabled[Group] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}

     {Enable IRQ}
     PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_SET_EN_0 + (Group * SizeOf(LongWord)) + (Entry.CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=(1 shl (LegacyNumber - Offset));
     LegacyIRQEnabled[Group]:=LegacyIRQEnabled[Group] or (1 shl (LegacyNumber - Offset));
    end;
  end
 else
  begin
   {Local}
   if Entry.IsFIQ then
    begin
     {Check Local IRQ}
     if (LegacyLocalIRQEnabled[Entry.CPUID] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;
    end
   else
    begin
     {Check Local FIQ}
     if (LegacyLocalFIQEnabled[Entry.CPUID] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;
    end;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Check Legacy}
   case LegacyNumber of
    96:begin {BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ}
      {Enable Physical Secure Timer}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSFIQ else Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    97:begin {BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ}
      {Enable Physical Non Secure Timer}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSFIQ else Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    98:begin {BCM2838_IRQ_LOCAL_ARM_CNTHPIRQ}
      {Enable Hypervisor Timer}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPFIQ else Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    99:begin {BCM2838_IRQ_LOCAL_ARM_CNTVIRQ}
      {Enable Virtual Timer}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTVFIQ else Enable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTVIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] or Enable;
     end;
    100:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX0}
      {Enable Mailbox0}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0FIQ else Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    101:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX1}
      {Enable Mailbox1}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1FIQ else Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    102:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX2}
      {Enable Mailbox2}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2FIQ else Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    103:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX3}
      {Enable Mailbox3}
      if Entry.IsFIQ then Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3FIQ else Enable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] or Enable;
     end;
    105:begin {BCM2838_IRQ_LOCAL_ARM_PMU}
      {Enable Performance Monitors}
      if Entry.IsFIQ then Enable:=(Entry.CPUID + 4) else Enable:=Entry.CPUID;

      ARMLocalRegisters.PMInterruptRoutingSet:=(1 shl Enable);
     end;
    106:begin {BCM2838_IRQ_LOCAL_ARM_AXIQUIET}
      {Enable AXI Quiet (CPU0 IRQ Only)}
      if Entry.IsFIQ then Exit;
      if Entry.CPUID <> CPU_ID_0 then Exit;

      ARMLocalRegisters.AXIOutstandingIRQ:=ARMLocalRegisters.AXIOutstandingIRQ or BCM2838_ARM_LOCAL_AXI_QUIET_IRQ_ENABLE;
     end;
    107:begin {BCM2838_IRQ_LOCAL_ARM_TIMER}
      {Enable Local Timer}
      if Entry.IsFIQ then Enable:=(Entry.CPUID + 4) else Enable:=Entry.CPUID;

      Enable:=BCM2838_ARM_LOCAL_PERIPHERAL_WRITE_MASKS or (Enable shl BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_SHIFT);

      ARMLocalRegisters.PeripheralIntRouting0:=(ARMLocalRegisters.PeripheralIntRouting0 and not(BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_MASK)) or Enable;
     end;
    126:begin {BCM2838_IRQ_LOCAL_ARM_AXIERR}
      {Enable AXI Error}
      if Entry.IsFIQ then Enable:=(Entry.CPUID + 4) else Enable:=Entry.CPUID;

      Enable:=(Enable shl BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_SHIFT);

      ARMLocalRegisters.CoreInterruptRouting:=(ARMLocalRegisters.CoreInterruptRouting and not(BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_MASK)) or Enable;
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
     LegacyLocalFIQEnabled[Entry.CPUID]:=LegacyLocalFIQEnabled[Entry.CPUID] or (1 shl (LegacyNumber - Offset));
    end
   else
    begin
     {Enable Local IRQ}
     LegacyLocalIRQEnabled[Entry.CPUID]:=LegacyLocalIRQEnabled[Entry.CPUID] or (1 shl (LegacyNumber - Offset));
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function RPi4LegacyDisable(const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Group:LongWord;
 Offset:LongWord;
 Disable:LongWord;
 LegacyNumber:LongWord;
begin
 {}
 Result:=False;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Entry.Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Get Group and Offset}
 if LegacyNumber < 32 then
  begin
   Group:=0;
   Offset:=0;
  end
 else if LegacyNumber < 64 then
  begin
   Group:=1;
   Offset:=32;
  end
 else if LegacyNumber < 96 then
  begin
   Group:=2;
   Offset:=64;
  end
 else
  begin
   Group:=LongWord(-1);
   Offset:=96;
  end;

 {Check Source}
 if LegacyNumber < 96 then
  begin
   {Global}
   if Entry.IsFIQ then
    begin
     {Check IRQ}
     if (LegacyIRQEnabled[Group] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}

     {Disable FIQ}
     PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_CLR_EN_0 + (Group * SizeOf(LongWord)) + (Entry.CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=(1 shl (LegacyNumber - Offset));
     LegacyFIQEnabled[Group]:=LegacyFIQEnabled[Group] and not(1 shl (LegacyNumber - Offset));
    end
   else
    begin
     {Check FIQ}
     if (LegacyFIQEnabled[Group] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}

     {Disable IRQ}
     PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_CLR_EN_0 + (Group * SizeOf(LongWord)) + (Entry.CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^:=(1 shl (LegacyNumber - Offset));
     LegacyIRQEnabled[Group]:=LegacyIRQEnabled[Group] and not(1 shl (LegacyNumber - Offset));
    end;
  end
 else
  begin
   {Local}
   if Entry.IsFIQ then
    begin
     {Check Local IRQ}
     if (LegacyLocalIRQEnabled[Entry.CPUID] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;
    end
   else
    begin
     {Check Local FIQ}
     if (LegacyLocalFIQEnabled[Entry.CPUID] and (1 shl (LegacyNumber - Offset))) <> 0 then Exit;
    end;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Check Legacy}
   case LegacyNumber of
    96:begin {BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ}
      {Disable Physical Secure Timer}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSFIQ else Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    97:begin {BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ}
      {Disable Physical Non Secure Timer}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSFIQ else Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    98:begin {BCM2838_IRQ_LOCAL_ARM_CNTHPIRQ}
      {Disable Hypervisor Timer}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPFIQ else Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    99:begin {BCM2838_IRQ_LOCAL_ARM_CNTVIRQ}
      {Disable Virtual Timer}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTVFIQ else Disable:=BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTVIRQ;

      ARMLocalRegisters.TimersIntControl[Entry.CPUID]:=ARMLocalRegisters.TimersIntControl[Entry.CPUID] and not(Disable);
     end;
    100:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX0}
      {Disable Mailbox0}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0FIQ else Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    101:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX1}
      {Disable Mailbox1}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1FIQ else Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    102:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX2}
      {Disable Mailbox2}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2FIQ else Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    103:begin {BCM2838_IRQ_LOCAL_ARM_MAILBOX3}
      {Disable Mailbox3}
      if Entry.IsFIQ then Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3FIQ else Disable:=BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3IRQ;

      ARMLocalRegisters.MailboxIntControl[Entry.CPUID]:=ARMLocalRegisters.MailboxIntControl[Entry.CPUID] and not(Disable);
     end;
    105:begin {BCM2838_IRQ_LOCAL_ARM_PMU}
      {Disable Performance Monitors}
      if Entry.IsFIQ then Disable:=(Entry.CPUID + 4) else Disable:=Entry.CPUID;

      ARMLocalRegisters.PMInterruptRoutingClear:=(1 shl Disable);
     end;
    106:begin {BCM2838_IRQ_LOCAL_ARM_AXIQUIET}
      {Disable AXI Quiet (CPU0 IRQ Only)}
      if Entry.IsFIQ then Exit;
      if Entry.CPUID <> CPU_ID_0 then Exit;

      ARMLocalRegisters.AXIOutstandingIRQ:=ARMLocalRegisters.AXIOutstandingIRQ and not(BCM2838_ARM_LOCAL_AXI_QUIET_IRQ_ENABLE);
     end;
    107:begin {BCM2838_IRQ_LOCAL_ARM_TIMER}
      {Disable Local Timer}
      Disable:=BCM2838_ARM_LOCAL_PERIPHERAL_WRITE_MASKS;

      ARMLocalRegisters.PeripheralIntRouting0:=(ARMLocalRegisters.PeripheralIntRouting0 and not(BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_MASK)) or Disable;
     end;
    126:begin {BCM2838_IRQ_LOCAL_ARM_AXIERR}
      {Disable AXI Error}
      ARMLocalRegisters.CoreInterruptRouting:=(ARMLocalRegisters.CoreInterruptRouting and not(BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_MASK));
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
     LegacyLocalFIQEnabled[Entry.CPUID]:=LegacyLocalFIQEnabled[Entry.CPUID] and not(1 shl (LegacyNumber - Offset));
    end
   else
    begin
     {Disable Local IRQ}
     LegacyLocalIRQEnabled[Entry.CPUID]:=LegacyLocalIRQEnabled[Entry.CPUID] and not(1 shl (LegacyNumber - Offset));
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function RPi4LegacyGetCurrentCount(CPUID,Number:LongWord):LongWord;
{Note: Caller must hold the interrupt lock}
var
 Entry:PInterruptEntry;
 LegacyNumber:LongWord;
begin
 {}
 Result:=0;

 {Setup Defaults}
 Entry:=nil;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Number}
 if RPi4LegacyIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI4_CPU_COUNT - 1 then Exit;

   {Count Local}
   Entry:=LocalLegacyInterruptEntries[LegacyNumber,CPUID];
  end
 else if RPi4LegacyIsSoftware(Number) then
  begin
   {Count Software}
   Entry:=LegacyInterruptEntries[LegacyNumber];
  end
 else if RPi4LegacyIsGlobal(Number) then
  begin
   {Count Global}
   Entry:=LegacyInterruptEntries[LegacyNumber];
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

function RPi4LegacyGetCurrentEntry(CPUID,Number:LongWord;Index:LongWord):PInterruptEntry;
{Note: Caller must hold the interrupt lock (or be within an interrupt handler)}
var
 Count:LongWord;
 Entry:PInterruptEntry;
 LegacyNumber:LongWord;
begin
 {}
 Result:=nil;

 {Setup Defaults}
 Entry:=nil;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Number}
 if RPi4LegacyIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI4_CPU_COUNT - 1 then Exit;

   {Count Local}
   Entry:=LocalLegacyInterruptEntries[LegacyNumber,CPUID];
  end
 else if RPi4LegacyIsSoftware(Number) then
  begin
   {Count Software}
   Entry:=LegacyInterruptEntries[LegacyNumber];
  end
 else if RPi4LegacyIsGlobal(Number) then
  begin
   {Count Global}
   Entry:=LegacyInterruptEntries[LegacyNumber];
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

function RPi4LegacyAddCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 LegacyNumber:LongWord;
 Current:PInterruptEntry;
begin
 {}
 Result:=False;

 {Check Entry}
 if Entry = nil then Exit;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Number}
 if RPi4LegacyIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI4_CPU_COUNT - 1 then Exit;

   {Add Local}
   Current:=LocalLegacyInterruptEntries[LegacyNumber,CPUID];
   if Current = nil then
    begin
      {Set Local}
      LocalLegacyInterruptEntries[LegacyNumber,CPUID]:=Entry;

      Result:=True;
    end;
  end
 else if RPi4LegacyIsSoftware(Number) then
  begin
   {Add Software}
   Current:=LegacyInterruptEntries[LegacyNumber];
   if Current = nil then
    begin
     {Set Software}
     LegacyInterruptEntries[LegacyNumber]:=Entry;

     Result:=True;
    end;
  end
 else if RPi4LegacyIsGlobal(Number) then
  begin
   {Add Global}
   Current:=LegacyInterruptEntries[LegacyNumber];
   if Current = nil then
    begin
     {Set Global}
     LegacyInterruptEntries[LegacyNumber]:=Entry;

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

function RPi4LegacyDeleteCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 LegacyNumber:LongWord;
 Current:PInterruptEntry;
begin
 {}
 Result:=False;

 {Check Entry}
 if Entry = nil then Exit;

 {Map to Legacy}
 LegacyNumber:=RPi4MapGICToLegacy(Number);
 if LegacyNumber = LongWord(-1) then Exit;

 {Check Number}
 if RPi4LegacyIsLocal(Number) then
  begin
   {Check CPU}
   if CPUID > RPI4_CPU_COUNT - 1 then Exit;

   {Delete Local}
   Current:=LocalLegacyInterruptEntries[LegacyNumber,CPUID];
   if Current = Entry then
    begin
     LocalLegacyInterruptEntries[LegacyNumber,CPUID]:=nil;
    end;
  end
 else if RPi4LegacyIsSoftware(Number) then
  begin
   {Delete Software}
   Current:=LegacyInterruptEntries[LegacyNumber];
   if Current = Entry then
    begin
     LegacyInterruptEntries[LegacyNumber]:=nil;
    end;
  end
 else if RPi4LegacyIsGlobal(Number) then
  begin
   {Delete Global}
   Current:=LegacyInterruptEntries[LegacyNumber];
   if Current = Entry then
    begin
     LegacyInterruptEntries[LegacyNumber]:=nil;
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

function RPi4LegacyFindMatchingEntry(const Entry:TInterruptEntry):PInterruptEntry;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=nil;

 {Get Current}
 Current:=RPi4LegacyGetCurrentEntry(Entry.CPUID,Entry.Number,0);

 {Find Match}
 while Current <> nil do
  begin
   if RPi4LegacyCompareHandlers(Entry,Current^) then
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

function RPi4GetLegacyEntry(CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord;
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
    if not RPi4LegacyIsSoftware(Number) then Exit;
   end
  else if (Flags and INTERRUPT_FLAG_LOCAL) <> 0 then
   begin
    {Local Entry}
    if not RPi4LegacyIsLocal(Number) then Exit;

    {Check CPU}
    if CPUID > RPI4_CPU_COUNT - 1 then Exit;
   end
  else
   begin
    {Global Entry}
    if not RPi4LegacyIsGlobal(Number) then Exit;
   end;

  Result:=ERROR_NOT_FOUND;

  {Get Current}
  Current:=RPi4LegacyGetCurrentEntry(CPUID,Number,Index);
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

function RPi4RegisterLegacyEntry(const Entry:TInterruptEntry):LongWord;
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
  if not RPi4LegacyCheckValid(Entry) then Exit;

  Result:=ERROR_ALREADY_ASSIGNED;

  {Check Count}
  if RPi4LegacyGetCurrentCount(Entry.CPUID,Entry.Number) = 0 then
   begin
    {Single Entry}
    Result:=ERROR_OPERATION_FAILED;

    {Enable IRQ/FIQ}
    if not RPi4LegacyEnable(Entry) then Exit;

    {Add Entry}
    if not RPi4LegacyAddCurrentEntry(Entry.CPUID,Entry.Number,@Entry) then Exit;
   end
  else
   begin
    {Shared Entry}
    Result:=ERROR_ALREADY_ASSIGNED;

    {Check Shared}
    if not Entry.IsShared then Exit;

    {Get Match}
    Current:=RPi4LegacyFindMatchingEntry(Entry);
    if Current <> nil then Exit;

    {Get Current}
    Current:=RPi4LegacyGetCurrentEntry(Entry.CPUID,Entry.Number,0);
    if Current = nil then Exit;

    {Check Shared}
    if not Current.IsShared then Exit;

    {Check FIQ}
    if Entry.IsFIQ <> Current.IsFIQ then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Add Entry}
    if not RPi4LegacyAddCurrentEntry(Entry.CPUID,Entry.Number,@Entry) then Exit;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPi4DeregisterLegacyEntry(const Entry:TInterruptEntry):LongWord;
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
  if not RPi4LegacyCheckValid(Entry) then Exit;

  Result:=ERROR_NOT_ASSIGNED;

  {Get Match}
  Current:=RPi4LegacyFindMatchingEntry(Entry);
  if Current = nil then Exit;

  Result:=ERROR_OPERATION_FAILED;

  {Check Count}
  if RPi4LegacyGetCurrentCount(Entry.CPUID,Entry.Number) = 1 then
   begin
    {Single Entry}
    {Disable IRQ/FIQ}
    if not RPi4LegacyDisable(Entry) then Exit;
   end;

  {Delete Entry}
  if not RPi4LegacyDeleteCurrentEntry(Entry.CPUID,Entry.Number,Current) then Exit;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPi4DispatchLegacyIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending legacy IRQ requests}
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

 {Check Legacy Local IRQ Enabled}
 if LegacyLocalIRQEnabled[CPUID] <> 0 then
  begin
   {Check Legacy Local IRQ Pending}
   IRQMatch:=(LegacyLocalIRQEnabled[CPUID] and ARMLocalRegisters.IRQSource[CPUID]);
   {Check IRQ Match}
   while IRQMatch <> 0 do
    begin
     {Find first set bit}
     IRQBit:=FirstBitSet(IRQMatch);

     {Clear set bit}
     IRQMatch:=IRQMatch xor (1 shl IRQBit);

     {Call Interrupt Handler}
     Result:=RPi4HandleLegacyInterrupt(RPi4MapLegacyToGIC(CPUID,IRQBit + IRQ_LOCAL_START),CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
    end;
  end;

 {Check IRQ Routing}
 if (IRQ_ROUTING = CPUID) or (IRQ_ROUTING = CPU_ID_ALL) then
  begin
   {Check Legacy IRQ Groups}
   for Group:=0 to 2 do
    begin
     {Check Legacy IRQ Enabled}
     if LegacyIRQEnabled[Group] <> 0 then
      begin
       case Group of
        {Check IRQ PENDING0}
        0:IRQMatch:=(LegacyIRQEnabled[Group] and PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_PENDING0 + (CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^);
        {Check IRQ PENDING1}
        1:IRQMatch:=(LegacyIRQEnabled[Group] and PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_PENDING1 + (CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^);
        {Check IRQ PENDING2}
        2:IRQMatch:=(LegacyIRQEnabled[Group] and PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_IRQ0_PENDING2 + (CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^);
       end;
       {Check IRQ Match}
       while IRQMatch <> 0 do
        begin
         {Find first set bit}
         IRQBit:=FirstBitSet(IRQMatch);

         {Clear set bit}
         IRQMatch:=IRQMatch xor (1 shl IRQBit);

         {Call Interrupt Handler}
         Result:=RPi4HandleLegacyInterrupt(RPi4MapLegacyToGIC(CPUID,IRQBit + (Group shl 5)),CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function RPi4DispatchLegacyFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending legacy FIQ requests}
{Called by ARMv7/8FIQHandler in PlatformARMv7/8}
{Note: A DataMemoryBarrier is executed before and after calling this function}
var
 Group:LongWord;
 FIQBit:LongWord;
 FIQMatch:LongWord;
begin
 {}
 Result:=Thread;

 {$IF DEFINED(FIQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 Inc(DispatchFastInterruptCounter[CPUID]);
 {$ENDIF}

 {Check Legacy Local FIQ Enabled}
 if LegacyLocalFIQEnabled[CPUID] <> 0 then
  begin
   {Check Legacy Local FIQ Pending}
   FIQMatch:=(LegacyLocalFIQEnabled[CPUID] and ARMLocalRegisters.FIQSource[CPUID]);
   {Check FIQ Match}
   while FIQMatch <> 0 do
    begin
     {Find first set bit}
     FIQBit:=FirstBitSet(FIQMatch);

     {Clear set bit}
     FIQMatch:=FIQMatch xor (1 shl FIQBit);

     {Call Interrupt Handler}
     Result:=RPi4HandleLegacyInterrupt(RPi4MapLegacyToGIC(CPUID,FIQBit + IRQ_LOCAL_START),CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
    end;
  end;

 {Check FIQ Routing}
 if (FIQ_ROUTING = CPUID) or (FIQ_ROUTING = CPU_ID_ALL) then
  begin
   {Check Legacy FIQ Groups}
   for Group:=0 to 2 do
    begin
     {Check Legacy FIQ Enabled}
     if LegacyFIQEnabled[Group] <> 0 then
      begin
       case Group of
        {Check FIQ PENDING0}
        0:FIQMatch:=(LegacyFIQEnabled[Group] and PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_PENDING0 + (CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^);
        {Check FIQ PENDING1}
        1:FIQMatch:=(LegacyFIQEnabled[Group] and PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_PENDING1 + (CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^);
        {Check FIQ PENDING2}
        2:FIQMatch:=(LegacyFIQEnabled[Group] and PLongWord(BCM2838_INTERRUPT_REGS_BASE + BCM2838_ARM_INTERRUPT_FIQ0_PENDING2 + (CPUID * BCM2838_ARM_INTERRUPT_CORE_OFFSET))^);
       end;
       {Check FIQ Match}
       while FIQMatch <> 0 do
        begin
         {Find first set bit}
         FIQBit:=FirstBitSet(FIQMatch);

         {Clear set bit}
         FIQMatch:=FIQMatch xor (1 shl FIQBit);

         {Call Interrupt Handler}
         Result:=RPi4HandleLegacyInterrupt(RPi4MapLegacyToGIC(CPUID,FIQBit + (Group shl 5)),CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function RPi4HandleLegacyInterrupt(Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for a legacy IRQ/FIQ that was received, or halt if it doesn't exist}
var
 Status:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;

 {Get Entry}
 Entry:=RPi4LegacyGetCurrentEntry(CPUID,Number,0);
 if Entry = nil then
  begin
   {Check Software}
   if RPi4LegacyIsSoftware(Number) then Exit;

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
  end
 else
  begin
   {Software}
   if not Assigned(Entry.SharedHandler) then
    begin
     {Halt}
     {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('No IPI handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
     {$ENDIF}

     Halt;
    end;

   {Call Handler}
   Status:=Entry.SharedHandler(Number,Source,Entry.Flags,Entry.Parameter);

   {Check Entry}
   if Entry.IsShared then
    begin
     {Shared}
     while Status <> INTERRUPT_RETURN_HANDLED do
      begin
       {Get Next}
       Entry:=Entry.Next;
       if Entry = nil then Exit;

       if not Assigned(Entry.SharedHandler) then
        begin
         {Halt}
         {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('No IPI handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
         {$ENDIF}

         Halt;
        end;

       {Call Handler}
       Status:=Entry.SharedHandler(Number,Source,Entry.Flags,Entry.Parameter);
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}

end.

