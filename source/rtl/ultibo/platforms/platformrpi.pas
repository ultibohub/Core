{
Ultibo Platform interface unit for Raspberry Pi.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)

Boards
======

 Raspberry Pi - Model A/B/A+/B+/CM1
 Raspberry Pi - Model Zero/ZeroW

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

References
==========

 BCM2835 ARM Peripherals

 Raspberry Pi Mailboxes

  https://github.com/raspberrypi/firmware/wiki/Mailboxes

 RPi Framebuffer

  http://elinux.org/RPi_Framebuffer

Platform RPi
============

 Notes: The RPi A+/B+ Have the Power LED connected to GPIO Pin 35 (Activity LED is now on GPIO Pin 47 instead of Pin 16 in model A/B)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformRPi;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE ..\core\GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2835,Platform,PlatformARM,PlatformARMv6,HeapManager,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF},SysUtils;

{==============================================================================}
const
 {RPi specific constants}

 {ARM Physical to VC IO Mapping}
 RPI_VCIO_ALIAS = BCM2835_VCIO_ALIAS;     {The VCIO Alias (For ARM Physcial to VC IO translation)}

 {ARM Physical to VC Bus Mapping}
 RPI_VCBUS_ALIAS = BCM2835_VCBUS_4_ALIAS; {The currently selected VCBUS Alias (For ARM Physcial to VC Bus translation) (Affected by disable_l2cache setting in config.txt)}

{const}
 {Address of StartupHandler on Reset}
 {RPI_STARTUP_ADDRESS = $00008000;} {Obtain from linker}

const
 {Page Table Address and Size}
 RPI_PAGE_TABLE_BASE = $00004000;     {Place the first level Page Table after the interrupt vectors at 0x00001000 and before the code start at 0x00008000}
 RPI_PAGE_TABLE_SIZE = SIZE_16K;      {ARM1176 first level Page Table is exactly 16KB in size (4096 32 bit (4 byte) entries)}

const
 {Vector Table Address and Size}
 RPI_VECTOR_TABLE_BASE  = $00001000;  {Place the Interrupt Vector Table at 0x00001000 before the code start at 0x00008000}
 RPI_VECTOR_TABLE_SIZE  = SIZE_64;    {The Interrupt Vector Table is exactly 64 bytes (16 32 bit (4 byte) entries)}
 RPI_VECTOR_TABLE_COUNT = 8;          {The Interrupt Vector Table contains 8 entries on an ARMv6 device}

const
 {CPU Count, Boot and Mask}
 RPI_CPU_COUNT = BCM2835_CPU_COUNT;
 RPI_CPU_BOOT = CPU_ID_0;
 RPI_CPU_MASK = CPU_AFFINITY_0;

const
 {SWI}
 RPI_SWI_COUNT = 256;                 {Number of available SWI entries}

const
 {Kernel Image Name}
 RPI_KERNEL_NAME = 'kernel.img';
 RPI_KERNEL_CONFIG = 'config.txt';
 RPI_KERNEL_COMMAND = 'cmdline.txt';
 RPI_FIRMWARE_FILES = 'bootcode.bin,start.elf,fixup.dat';
 RPI_DTB_FILES = 'bcm2708-rpi-b.dtb,bcm2708-rpi-b-plus.dtb,bcm2708-rpi-b-rev1.dtb,bcm2708-rpi-cm.dtb,bcm2708-rpi-zero.dtb,bcm2708-rpi-zero-w.dtb';

const
 {GPIO Power LED constants (GPIO Pin 35 - A+/B+ Only)}
 {Note: GPIO Pin 35 on the A+/B+ is set on boot to Pull Up/Down Enable which must be cleared before using the pin}
 RPIPLUS_GPIO_PWRLED_GPFSEL = BCM2835_GPFSEL3;      {GPFSEL register for PWR LED}
 RPIPLUS_GPIO_PWRLED_GPSET = BCM2835_GPSET1;        {GPSET register for PWR LED}
 RPIPLUS_GPIO_PWRLED_GPCLR = BCM2835_GPCLR1;        {GPCLR register for PWR LED}

 RPIPLUS_GPIO_PWRLED_GPFSHIFT = 15;                 {GPFSEL register shift for PWR LED}
 RPIPLUS_GPIO_PWRLED_GPFMASK = BCM2835_GPFSEL_MASK; {GPFSEL register mask for PWR LED}
 //To Do RPIPLUS_GPIO_PWRLED_GPFVALUE = ALT0/1/2/3 etc

 RPIPLUS_GPIO_PWRLED_GPSHIFT = (35 - 32);           {GPSET/GPCLR register shift for PWR LED}
 RPIPLUS_GPIO_PWRLED_GPMASK = BCM2835_GPSET_MASK;   {GPSET/GPCLR register mask for PWR LED}

 //GPIO Pin 35 on A+/B+
 //See: http://www.raspberrypi.org/forums/viewtopic.php?t=72260
 //See also for how to control GPPUD etc: http://wiki.osdev.org/Raspberry_Pi_Bare_Bones

const
 {GPIO Activity LED constants (GPIO Pin 16 - A/B Only)}
 {Note: GPIO Pin 47 on the A+/B+ is Pull High instead of Pull Low, to turn on the LED use RPIPLUS_GPIO_ACTLED_GPSET instead of RPIPLUS_GPIO_ACTLED_GPCLR}
 RPI_GPIO_ACTLED_GPFSEL = BCM2835_GPFSEL1;          {GPFSEL register for ACT LED}
 RPI_GPIO_ACTLED_GPSET = BCM2835_GPSET0;            {GPSET register for ACT LED}
 RPI_GPIO_ACTLED_GPCLR = BCM2835_GPCLR0;            {GPCLR register for ACT LED}

 RPI_GPIO_ACTLED_GPFSHIFT = 18;                     {GPFSEL register shift for ACT LED}
 RPI_GPIO_ACTLED_GPFMASK = BCM2835_GPFSEL_MASK;     {GPFSEL register mask for ACT LED}
 //To Do RPI_GPIO_ACTLED_GPFVALUE = ALT0/1/2/3 etc

 RPI_GPIO_ACTLED_GPSHIFT = 16;                      {GPSET/GPCLR register shift for ACT LED}
 RPI_GPIO_ACTLED_GPMASK = BCM2835_GPSET_MASK;       {GPSET/GPCLR register mask for ACT LED}

const
 {GPIO Activity LED constants (GPIO Pin 47 - A+/B+ Only)}
 RPIPLUS_GPIO_ACTLED_GPFSEL = BCM2835_GPFSEL4;      {GPFSEL register for ACT LED}
 RPIPLUS_GPIO_ACTLED_GPSET = BCM2835_GPSET1;        {GPSET register for ACT LED}
 RPIPLUS_GPIO_ACTLED_GPCLR = BCM2835_GPCLR1;        {GPCLR register for ACT LED}

 RPIPLUS_GPIO_ACTLED_GPFSHIFT = 21;                 {GPFSEL register shift for ACT LED}
 RPIPLUS_GPIO_ACTLED_GPFMASK = BCM2835_GPFSEL_MASK; {GPFSEL register mask for ACT LED}
 //To Do RPIPLUS_GPIO_ACTLED_GPFVALUE = ALT0/1/2/3 etc

 RPIPLUS_GPIO_ACTLED_GPSHIFT = (47 - 32);           {GPSET/GPCLR register shift for ACT LED}
 RPIPLUS_GPIO_ACTLED_GPMASK = BCM2835_GPSET_MASK;   {GPSET/GPCLR register mask for ACT LED}

const
 {Mailbox constants}
 RPI_MAILBOX_TIMEOUT = 100;                         {Default timeout to wait for mailbox calls to complete (Milliseconds)}
 RPI_MAILBOX_TIMEOUT_EX = 1000;                     {Extended timeout to wait for mailbox calls to complete (Milliseconds)}

const
 {Framebuffer constants}
 RPI_FRAMEBUFFER_DESCRIPTION = 'BCM2835 Framebuffer';

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
type
 {RPi specific types}
 PRPiFramebuffer = ^TRPiFramebuffer;
 TRPiFramebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {RPi Properties}
  MultiDisplay:LongBool;
  DisplayNum:LongWord;
  DisplaySettings:TDisplaySettings;
 end;
{$ENDIF}
{==============================================================================}
var
 {RPi specific Ultibo variables}
 RPiInitialized:Boolean;

var
 {Timer Variables}
 TimerRegisters:PBCM2835SystemTimerRegisters;

var
 {Mailbox Variables}
 Mailbox0Registers:PBCM2835Mailbox0Registers;
 Mailbox1Registers:PBCM2835Mailbox1Registers;

var
 {Interrupt Variables}
 InterruptRegisters:PBCM2835InterruptRegisters;

 InterruptEntries:array[0..(BCM2835_IRQ_COUNT - 1)] of PInterruptEntry;

var
 {System Call Variables}
 SystemCallEntries:array[0..RPI_SWI_COUNT - 1] of TSystemCallEntry;

var
 {IRQ/FIQ Variables}
 IRQEnabled:array[0..2] of LongWord; {3 groups of IRQs to Enable/Disable (See: TBCM2835InterruptRegisters)}
 FIQEnabled:LongWord;                {The single IRQ number to Enable as FIQ instead (See: TBCM2835InterruptRegisters)}

var
 {Watchdog Variables}
 WatchdogRegisters:PBCM2835PMWatchdogRegisters;

{==============================================================================}
{Initialization Functions}
procedure RPiInit;

{==============================================================================}
{RPi Platform Functions}
procedure RPiBoardInit;
procedure RPiMemoryInit;
procedure RPiClockInit;
procedure RPiPowerInit;
procedure RPiMailboxInit;
procedure RPiInterruptInit;
procedure RPiPeripheralInit;
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPiFramebufferInit;
{$ENDIF}
procedure RPiPageTableInit;

procedure RPiPowerLEDEnable;
procedure RPiPowerLEDOn;
procedure RPiPowerLEDOff;

procedure RPiActivityLEDEnable;
procedure RPiActivityLEDOn;
procedure RPiActivityLEDOff;

function RPiMailboxReceive(Mailbox,Channel:LongWord):LongWord;
procedure RPiMailboxSend(Mailbox,Channel,Data:LongWord);

function RPiMailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
function RPiMailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord;
function RPiMailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
function RPiMailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord;

function RPiMailboxPropertyTag(Tag:LongWord;Data:Pointer;Size:LongWord):LongWord;

function RPiRequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function RPiReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function RPiRequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function RPiReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function RPiRegisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
function RPiDeregisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;

function RPiRegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
function RPiDeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;

function RPiGetInterruptEntry(Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
function RPiGetSystemCallEntry(Number:LongWord):TSystemCallEntry;

function RPiSystemRestart(Delay:LongWord):LongWord;
function RPiSystemShutdown(Delay:LongWord):LongWord;
function RPiSystemGetCommandLine:String;

function RPiCPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;

function RPiGPUGetState:LongWord;
function RPiGPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;

function RPiBoardGetModel:LongWord;
function RPiBoardGetSerial:Int64;
function RPiBoardGetRevision:LongWord;
function RPiBoardGetMACAddress:String;

function RPiFirmwareGetRevision:LongWord;
function RPiFirmwareGetThrottled:LongWord;

function RPiPowerGetWait(PowerId:LongWord):LongWord;
function RPiPowerGetState(PowerId:LongWord):LongWord;
function RPiPowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;

function RPiClockGetCount:LongWord;
function RPiClockGetTotal:Int64;

function RPiClockGetRate(ClockId:LongWord):LongWord;
function RPiClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;

function RPiClockGetState(ClockId:LongWord):LongWord;
function RPiClockSetState(ClockId,State:LongWord):LongWord;

function RPiClockGetMinRate(ClockId:LongWord):LongWord;
function RPiClockGetMaxRate(ClockId:LongWord):LongWord;

function RPiClockGetMeasuredRate(ClockId:LongWord):LongWord;

function RPiTurboGetState(TurboId:LongWord):LongWord;
function RPiTurboSetState(TurboId,State:LongWord):LongWord;

function RPiVoltageGetValue(VoltageId:LongWord):LongWord;
function RPiVoltageSetValue(VoltageId,Value:LongWord):LongWord;

function RPiVoltageGetMinValue(VoltageId:LongWord):LongWord;
function RPiVoltageGetMaxValue(VoltageId:LongWord):LongWord;

function RPiTemperatureGetCurrent(TemperatureId:LongWord):LongWord;
function RPiTemperatureGetMaximum(TemperatureId:LongWord):LongWord;

function RPiGPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
function RPiGPUMemoryRelease(Handle:THandle):LongWord;
function RPiGPUMemoryLock(Handle:THandle):LongWord;
function RPiGPUMemoryUnlock(Handle:THandle):LongWord;

function RPiGPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;

function RPiDispmanxHandleGet(Resource:THandle):THandle;
function RPiEDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPiFramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
function RPiFramebufferRelease:LongWord;
function RPiFramebufferSetState(State:LongWord):LongWord;

function RPiFramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord;

function RPiFramebufferGetPhysical(var Width,Height:LongWord):LongWord;
function RPiFramebufferSetPhysical(var Width,Height:LongWord):LongWord;
function RPiFramebufferTestPhysical(var Width,Height:LongWord):LongWord;

function RPiFramebufferGetVirtual(var Width,Height:LongWord):LongWord;
function RPiFramebufferSetVirtual(var Width,Height:LongWord):LongWord;
function RPiFramebufferTestVirtual(var Width,Height:LongWord):LongWord;

function RPiFramebufferGetDepth(var Depth:LongWord):LongWord;
function RPiFramebufferSetDepth(var Depth:LongWord):LongWord;
function RPiFramebufferTestDepth(var Depth:LongWord):LongWord;

function RPiFramebufferGetPixelOrder(var Order:LongWord):LongWord;
function RPiFramebufferSetPixelOrder(var Order:LongWord):LongWord;
function RPiFramebufferTestPixelOrder(var Order:LongWord):LongWord;

function RPiFramebufferGetAlphaMode(var Mode:LongWord):LongWord;
function RPiFramebufferSetAlphaMode(var Mode:LongWord):LongWord;
function RPiFramebufferTestAlphaMode(var Mode:LongWord):LongWord;

function RPiFramebufferGetPitch:LongWord;

function RPiFramebufferGetOffset(var X,Y:LongWord):LongWord;
function RPiFramebufferSetOffset(var X,Y:LongWord):LongWord;
function RPiFramebufferTestOffset(var X,Y:LongWord):LongWord;

function RPiFramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPiFramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPiFramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;

function RPiFramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
function RPiFramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
function RPiFramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPiFramebufferGetLayer(var Layer:LongInt):LongWord;
function RPiFramebufferSetLayer(var Layer:LongInt):LongWord;
function RPiFramebufferTestLayer(var Layer:LongInt):LongWord;

function RPiFramebufferTestVsync:LongWord;
function RPiFramebufferSetVsync:LongWord;

function RPiFramebufferSetBacklight(Brightness:LongWord):LongWord;

function RPiFramebufferGetNumDisplays(var NumDisplays:LongWord):LongWord;
function RPiFramebufferGetDisplayId(DisplayNum:LongWord):LongWord;
function RPiFramebufferSetDisplayNum(DisplayNum:LongWord):LongWord;
function RPiFramebufferGetDisplaySettings(DisplayNum:LongWord;var DisplaySettings:TDisplaySettings):LongWord;
function RPiFramebufferDisplayIdToName(DisplayId:LongWord):String;

function RPiTouchGetBuffer(var Address:PtrUInt):LongWord;
function RPiTouchSetBuffer(Address:PtrUInt):LongWord;

function RPiCursorSetDefault:LongWord;
function RPiCursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
function RPiCursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;

function RPiDMAGetChannels:LongWord;

{==============================================================================}
{RPi Thread Functions}
procedure RPiSchedulerInit;

{==============================================================================}
{RPi SWI Functions}
function RPiDispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle;

{==============================================================================}
{RPi Clock Functions}
procedure RPiClockInterrupt(Parameter:Pointer);
procedure RPiClockUpdate(Cycles:LongWord;var Last:LongWord);

{==============================================================================}
{RPi Scheduler Functions}
function RPiSchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
procedure RPiSchedulerUpdate(Cycles:LongWord;var Last:LongWord);

procedure RPiSchedulerSystemCall(Request:PSystemCallRequest);

{==============================================================================}
{RPi Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPiFramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPiFramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPiFramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;

function RPiFramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function RPiFramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;

function RPiFramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{$ENDIF}
{==============================================================================}
{RPi Helper Functions}
procedure RPiWait;
procedure RPiLongWait;
procedure RPiShortWait;

procedure RPiSlowBlink;
procedure RPiFastBlink;

procedure RPiBootBlink;

procedure RPiBootOutput(Value:LongWord);
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPiBootConsoleStart;
procedure RPiBootConsoleWrite(const Value:String);
procedure RPiBootConsoleWriteEx(const Value:String;X,Y:LongWord);
function RPiBootConsoleGetX:LongWord;
function RPiBootConsoleGetY:LongWord;
{$ENDIF}
function RPiConvertPowerIdRequest(PowerId:LongWord):LongWord;
function RPiConvertPowerStateRequest(PowerState:LongWord):LongWord;
function RPiConvertPowerStateResponse(PowerState:LongWord):LongWord;

function RPiConvertClockIdRequest(ClockId:LongWord):LongWord;
function RPiConvertClockStateRequest(ClockState:LongWord):LongWord;
function RPiConvertClockStateResponse(ClockState:LongWord):LongWord;

function RPiConvertVoltageIdRequest(VoltageId:LongWord):LongWord;

function RPiConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{RPi Forward Declarations}
function RPiInterruptIsValid(Number:LongWord):Boolean; forward;
function RPiInterruptIsGlobal(Number:LongWord):Boolean; forward;

function RPiInterruptCheckValid(const Entry:TInterruptEntry):Boolean; forward;
function RPiInterruptCheckHandlers(const Entry:TInterruptEntry):Boolean; forward;
function RPiInterruptCompareHandlers(const Entry,Current:TInterruptEntry):Boolean; forward;

function RPiInterruptEnable(const Entry:TInterruptEntry):Boolean; forward;
function RPiInterruptDisable(const Entry:TInterruptEntry):Boolean; forward;

function RPiInterruptGetCurrentCount(CPUID,Number:LongWord):LongWord; forward;
function RPiInterruptGetCurrentEntry(CPUID,Number:LongWord;Index:LongWord):PInterruptEntry; forward;

function RPiInterruptAddCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;
function RPiInterruptDeleteCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;

function RPiInterruptFindMatchingEntry(const Entry:TInterruptEntry):PInterruptEntry; forward;

function RPiInterruptGetEntry(CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord; forward;
function RPiInterruptRegisterEntry(const Entry:TInterruptEntry):LongWord; forward;
function RPiInterruptDeregisterEntry(const Entry:TInterruptEntry):LongWord; forward;

function RPiDispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;
function RPiDispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;

function RPiHandleInterrupt(Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RPiInit;
begin
 {}
 if RPiInitialized then Exit;

 {Check for Emulator}
 if PLongWord(BCM2835_GPIO_REGS_BASE + BCM2835_GPSET0)^ <> BCM2835_GPIO_SIGNATURE then ARMEmulatorMode:=1;

 {Setup IO_BASE/IO_ALIAS}
 IO_BASE:=BCM2835_PERIPHERALS_BASE;
 IO_ALIAS:=RPI_VCIO_ALIAS;

 {Setup BUS_ALIAS}
 BUS_ALIAS:=RPI_VCBUS_ALIAS;

 {Setup SECURE_BOOT}
 SECURE_BOOT:=True;

 {Setup EMULATOR_MODE}
 EMULATOR_MODE:=(ARMEmulatorMode <> 0);

 {Setup STARTUP_ADDRESS}
 STARTUP_ADDRESS:=PtrUInt(@_text_start); {RPI_STARTUP_ADDRESS} {Obtain from linker}

 {Setup PERIPHERALS_BASE and SIZE}
 PERIPHERALS_BASE:=BCM2835_PERIPHERALS_BASE;
 PERIPHERALS_SIZE:=BCM2835_PERIPHERALS_SIZE;

 {Setup MEMORY_BASE and SIZE}
 {Done by RPiMemoryInit}

 {Setup MEMORY_IRQ/FIQ/LOCAL/SHARED/DEVICE/NOCACHE/NONSHARED_SIZE}
 {Done by RPiMemoryInit}

 {Setup PAGE_TABLE_BASE and SIZE}
 PAGE_TABLE_BASE:=RPI_PAGE_TABLE_BASE;
 PAGE_TABLE_SIZE:=RPI_PAGE_TABLE_SIZE;

 {Setup VECTOR_TABLE_BASE, SIZE and COUNT}
 VECTOR_TABLE_BASE:=RPI_VECTOR_TABLE_BASE;
 VECTOR_TABLE_SIZE:=RPI_VECTOR_TABLE_SIZE;
 VECTOR_TABLE_COUNT:=RPI_VECTOR_TABLE_COUNT;

 {Setup MACHINE_TYPE}
 MACHINE_TYPE:=MACHINE_TYPE_UNKNOWN;
 case ARMMachineType of
  ARM_MACHINE_BCM2708:MACHINE_TYPE:=MACHINE_TYPE_BCM2708;
 end;

 {Setup BOARD_TYPE}
 {Done by RPiBoardInit}

 {Setup CPU_ARCH, TYPE and COUNT}
 CPU_ARCH:=CPU_ARCH_ARM32;
 CPU_TYPE:=CPU_TYPE_ARMV6;
 CPU_COUNT:=RPI_CPU_COUNT;
 CPU_BOOT:=RPI_CPU_BOOT;
 CPU_MASK:=RPI_CPU_MASK;
 CPU_MAX_COUNT:=RPI_CPU_COUNT;

 {Setup CPU_MEMORY_BASE and SIZE}
 {Done by RPiMemoryInit}

 {Setup CPU_MEMORY_RESTRICTED}
 CPU_MEMORY_RESTRICTED:=True;

 {Setup FPU_TYPE}
 FPU_TYPE:=FPU_TYPE_VFPV2;

 {Setup GPU_TYPE}
 GPU_TYPE:=GPU_TYPE_VC4;

 {Setup GPU_MEMORY_BASE and SIZE}
 {Done by RPiMemoryInit}

 {Setup GPU_MEMORY_CACHED}
 GPU_MEMORY_CACHED:=True;

 {Setup IRQ/FIQ/SWI_COUNT/START/ROUTING}
 IRQ_COUNT:=BCM2835_IRQ_COUNT;
 FIQ_COUNT:=BCM2835_FIQ_COUNT;

 IRQ_START:=0;                          {System wide IRQs start at zero}

 IRQ_ROUTING:=CPU_ID_0;                 {Route system wide IRQs to CPU0}
 FIQ_ROUTING:=CPU_ID_0;                 {Route system wide FIQs to CPU0}

 IRQ_LOCAL_COUNT:=0;                    {There are no Local IRQs}
 FIQ_LOCAL_COUNT:=0;                    {There are no Local FIQs}

 IRQ_LOCAL_START:=BCM2835_IRQ_COUNT;    {There are no Local IRQs}

 SWI_COUNT:=RPI_SWI_COUNT;

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
 CLOCK_FREQUENCY:=BCM2835_SYSTEM_TIMER_FREQUENCY;
 CLOCK_TICKS_PER_SECOND:=1000;
 CLOCK_TICKS_PER_MILLISECOND:=1;
 CLOCK_CYCLES_PER_TICK:=CLOCK_FREQUENCY div CLOCK_TICKS_PER_SECOND;
 CLOCK_CYCLES_PER_MILLISECOND:=CLOCK_FREQUENCY div MILLISECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_MICROSECOND:=CLOCK_FREQUENCY div MICROSECONDS_PER_SECOND;
 CLOCK_CYCLES_PER_NANOSECOND:=CLOCK_FREQUENCY div NANOSECONDS_PER_SECOND;
 CLOCK_CYCLES_TOLERANCE:=CLOCK_CYCLES_PER_TICK div 10;
 TIME_TICKS_PER_CLOCK_INTERRUPT:=TIME_TICKS_PER_MILLISECOND div CLOCK_TICKS_PER_MILLISECOND;

 {Setup HEAP Behaviour}
 HEAP_NORMAL_NONSHARED:=True;
 HEAP_IRQ_CACHE_COHERENT:=True;
 HEAP_FIQ_CACHE_COHERENT:=True;

 {Setup SCHEDULER_INTERRUPTS/CLOCKS}
 SCHEDULER_INTERRUPTS_PER_SECOND:=2000;
 SCHEDULER_INTERRUPTS_PER_MILLISECOND:=2;
 if EMULATOR_MODE then
  begin
   SCHEDULER_INTERRUPTS_PER_SECOND:=1000;   {Note: QEMU uses the timeGetDevCaps() function on Windows which returns wPeriodMin as 1 millisecond}
   SCHEDULER_INTERRUPTS_PER_MILLISECOND:=1; {      That means that any timer interval less then 1ms will not be honoured, the result will be 1ms}
  end;
 SCHEDULER_CLOCKS_PER_INTERRUPT:=CLOCK_FREQUENCY div SCHEDULER_INTERRUPTS_PER_SECOND;
 SCHEDULER_CLOCKS_TOLERANCE:=SCHEDULER_CLOCKS_PER_INTERRUPT div 10;
 TIME_TICKS_PER_SCHEDULER_INTERRUPT:=TIME_TICKS_PER_MILLISECOND div SCHEDULER_INTERRUPTS_PER_MILLISECOND;

 {Setup SCHEDULER_IDLE}
 SCHEDULER_IDLE_WAIT:=False;
 SCHEDULER_IDLE_OFFSET:=1;
 SCHEDULER_IDLE_PER_SECOND:=SCHEDULER_INTERRUPTS_PER_SECOND;

 {Setup KERNEL_NAME/CONFIG/COMMAND}
 KERNEL_NAME:=RPI_KERNEL_NAME;
 KERNEL_CONFIG:=RPI_KERNEL_CONFIG;
 KERNEL_COMMAND:=RPI_KERNEL_COMMAND;
 FIRMWARE_FILES:=RPI_FIRMWARE_FILES;
 DTB_FILES:=RPI_DTB_FILES;

 {Setup GPIO (Set early to support activity LED)}
 GPIO_REGS_BASE:=BCM2835_GPIO_REGS_BASE;

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

 {Register Platform BoardInit Handler}
 BoardInitHandler:=RPiBoardInit;

 {Register Platform MemoryInit Handler}
 MemoryInitHandler:=RPiMemoryInit;

 {Register Platform ClockInit Handler}
 ClockInitHandler:=RPiClockInit;

 {Register Platform PowerInit Handler}
 PowerInitHandler:=RPiPowerInit;

 {Register Platform MailboxInit Handler}
 MailboxInitHandler:=RPiMailboxInit;

 {Register Platform InterruptInit Handler}
 InterruptInitHandler:=RPiInterruptInit;

 {Register Platform PeripheralInit Handler}
 PeripheralInitHandler:=RPiPeripheralInit;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Framebuffer FramebufferInit Handler}
 FramebufferInitHandler:=RPiFramebufferInit;
 {$ENDIF}
 {Register PlatformARMv6 PageTableInit Handler}
 ARMv6PageTableInitHandler:=RPiPageTableInit;

 {Register Platform Boot Blink Handlers}
 BootBlinkHandler:=RPiBootBlink;
 BootOutputHandler:=RPiBootOutput;

 {Register Platform Boot Console Handlers}
 {$IFDEF CONSOLE_EARLY_INIT}
 BootConsoleStartHandler:=RPiBootConsoleStart;
 BootConsoleWriteHandler:=RPiBootConsoleWrite;
 BootConsoleWriteExHandler:=RPiBootConsoleWriteEx;
 BootConsoleGetXHandler:=RPiBootConsoleGetX;
 BootConsoleGetYHandler:=RPiBootConsoleGetY;
 {$ENDIF}

 {Register Platform LED Handlers}
 PowerLEDEnableHandler:=RPiPowerLEDEnable;
 PowerLEDOnHandler:=RPiPowerLEDOn;
 PowerLEDOffHandler:=RPiPowerLEDOff;
 ActivityLEDEnableHandler:=RPiActivityLEDEnable;
 ActivityLEDOnHandler:=RPiActivityLEDOn;
 ActivityLEDOffHandler:=RPiActivityLEDOff;

 {Register Platform Mailbox Handlers}
 MailboxReceiveHandler:=RPiMailboxReceive;
 MailboxSendHandler:=RPiMailboxSend;
 MailboxCallHandler:=RPiMailboxCall;
 MailboxCallExHandler:=RPiMailboxCallEx;
 MailboxPropertyCallHandler:=RPiMailboxPropertyCall;
 MailboxPropertyCallExHandler:=RPiMailboxPropertyCallEx;
 MailboxPropertyTagHandler:=RPiMailboxPropertyTag;

 {Register Platform IRQ Handlers}
 RequestExIRQHandler:=RPiRequestExIRQ;
 ReleaseExIRQHandler:=RPiReleaseExIRQ;

 {Register Platform FIQ Handlers}
 RequestExFIQHandler:=RPiRequestExFIQ;
 ReleaseExFIQHandler:=RPiReleaseExFIQ;

 {Register Platform Interrupt Handlers}
 RegisterInterruptHandler:=RPiRegisterInterrupt;
 DeregisterInterruptHandler:=RPiDeregisterInterrupt;

 {Register Platform System Call Handlers}
 RegisterSystemCallExHandler:=RPiRegisterSystemCallEx;
 DeregisterSystemCallExHandler:=RPiDeregisterSystemCallEx;

 {Register Platform Interrupt Handlers}
 GetInterruptEntryHandler:=RPiGetInterruptEntry;

 {Register Platform System Call Handlers}
 GetSystemCallEntryHandler:=RPiGetSystemCallEntry;

 {Register Platform System Handlers}
 SystemRestartHandler:=RPiSystemRestart;
 SystemShutdownHandler:=RPiSystemShutdown;
 SystemGetCommandLineHandler:=RPiSystemGetCommandLine;

 {Register Platform CPU Handlers}
 CPUGetMemoryHandler:=RPiCPUGetMemory;

 {Register Platform GPU Handlers}
 GPUGetStateHandler:=RPiGPUGetState;
 GPUGetMemoryHandler:=RPiGPUGetMemory;

 {Register Platform Board Handlers}
 BoardGetModelHandler:=RPiBoardGetModel;
 BoardGetSerialHandler:=RPiBoardGetSerial;
 BoardGetRevisionHandler:=RPiBoardGetRevision;
 BoardGetMACAddressHandler:=RPiBoardGetMACAddress;

 {Register Platform Firmware Handlers}
 FirmwareGetRevisionHandler:=RPiFirmwareGetRevision;
 FirmwareGetThrottledHandler:=RPiFirmwareGetThrottled;

 {Register Platform Power Handlers}
 PowerGetWaitHandler:=RPiPowerGetWait;
 PowerGetStateHandler:=RPiPowerGetState;
 PowerSetStateHandler:=RPiPowerSetState;

 {Register Platform Clock Handlers}
 ClockGetCountHandler:=RPiClockGetCount;
 ClockGetTotalHandler:=RPiClockGetTotal;

 ClockGetRateHandler:=RPiClockGetRate;
 ClockSetRateHandler:=RPiClockSetRate;

 ClockGetStateHandler:=RPiClockGetState;
 ClockSetStateHandler:=RPiClockSetState;

 ClockGetMinRateHandler:=RPiClockGetMinRate;
 ClockGetMaxRateHandler:=RPiClockGetMaxRate;

 ClockGetMeasuredRateHandler:=RPiClockGetMeasuredRate;

 {Register Platform Turbo Handlers}
 TurboGetStateHandler:=RPiTurboGetState;
 TurboSetStateHandler:=RPiTurboSetState;

 {Register Platform Voltage Handlers}
 VoltageGetValueHandler:=RPiVoltageGetValue;
 VoltageSetValueHandler:=RPiVoltageSetValue;
 VoltageGetMinValueHandler:=RPiVoltageGetMinValue;
 VoltageGetMaxValueHandler:=RPiVoltageGetMaxValue;

 {Register Platform Temperature Handlers}
 TemperatureGetCurrentHandler:=RPiTemperatureGetCurrent;
 TemperatureGetMaximumHandler:=RPiTemperatureGetMaximum;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPiGPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPiGPUMemoryRelease;
 GPUMemoryLockHandler:=RPiGPUMemoryLock;
 GPUMemoryUnlockHandler:=RPiGPUMemoryUnlock;

 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPiGPUExecuteCode;
 DispmanxHandleGetHandler:=RPiDispmanxHandleGet;
 EDIDBlockGetHandler:=RPiEDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPiFramebufferAllocate;
 FramebufferReleaseHandler:=RPiFramebufferRelease;
 FramebufferSetStateHandler:=RPiFramebufferSetState;

 FramebufferGetDimensionsHandler:=RPiFramebufferGetDimensions;

 FramebufferGetPhysicalHandler:=RPiFramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPiFramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPiFramebufferTestPhysical;

 FramebufferGetVirtualHandler:=RPiFramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPiFramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPiFramebufferTestVirtual;

 FramebufferGetDepthHandler:=RPiFramebufferGetDepth;
 FramebufferSetDepthHandler:=RPiFramebufferSetDepth;
 FramebufferTestDepthHandler:=RPiFramebufferTestDepth;

 FramebufferGetPixelOrderHandler:=RPiFramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPiFramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPiFramebufferTestPixelOrder;

 FramebufferGetAlphaModeHandler:=RPiFramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPiFramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPiFramebufferTestAlphaMode;

 FramebufferGetPitchHandler:=RPiFramebufferGetPitch;

 FramebufferGetOffsetHandler:=RPiFramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPiFramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPiFramebufferTestOffset;

 FramebufferGetOverscanHandler:=RPiFramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPiFramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPiFramebufferTestOverscan;

 FramebufferGetPaletteHandler:=RPiFramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPiFramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPiFramebufferTestPalette;

 FramebufferGetLayerHandler:=RPiFramebufferGetLayer;
 FramebufferSetLayerHandler:=RPiFramebufferSetLayer;
 FramebufferTestLayerHandler:=RPiFramebufferTestLayer;

 FramebufferTestVsyncHandler:=RPiFramebufferTestVsync;
 FramebufferSetVsyncHandler:=RPiFramebufferSetVsync;

 FramebufferSetBacklightHandler:=RPiFramebufferSetBacklight;

 FramebufferGetNumDisplaysHandler:=RPiFramebufferGetNumDisplays;
 FramebufferGetDisplayIdHandler:=RPiFramebufferGetDisplayId;
 FramebufferSetDisplayNumHandler:=RPiFramebufferSetDisplayNum;
 FramebufferGetDisplaySettingsHandler:=RPiFramebufferGetDisplaySettings;
 FramebufferDisplayIdToNameHandler:=RPiFramebufferDisplayIdToName;

 {Register Platform Touch Handlers}
 TouchGetBufferHandler:=RPiTouchGetBuffer;
 TouchSetBufferHandler:=RPiTouchSetBuffer;

 {Register Platform Cursor Handlers}
 CursorSetDefaultHandler:=RPiCursorSetDefault;
 CursorSetInfoHandler:=RPiCursorSetInfo;
 CursorSetStateHandler:=RPiCursorSetState;
 {$ENDIF}
 {Register Platform DMA Handlers}
 DMAGetChannelsHandler:=RPiDMAGetChannels;

 {Register Threads SchedulerInit Handler}
 SchedulerInitHandler:=RPiSchedulerInit;
 {No SchedulerStart, RPi is Uniprocessor}

 {Register Threads SecondaryBoot Handler}
 {Nothing, RPi is Uniprocessor}

 {Register PlatformARMv6 IRQ Handlers}
 ARMv6DispatchIRQHandler:=RPiDispatchIRQ;

 {Register PlatformARMv6 FIQ Handlers}
 ARMv6DispatchFIQHandler:=RPiDispatchFIQ;

 {Register PlatformARMv6 SWI Handlers}
 ARMv6DispatchSWIHandler:=RPiDispatchSWI;

 {Register PlatformARM Helper Handlers}
 ARMWaitHandler:=RPiWait;
 ARMLongWaitHandler:=RPiLongWait;
 ARMShortWaitHandler:=RPiShortWait;
 ARMSlowBlinkHandler:=RPiSlowBlink;
 ARMFastBlinkHandler:=RPiFastBlink;

 RPiInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{RPi Platform Functions}
procedure RPiBoardInit;
var
 Revision:LongWord;
 ClockRateMax:LongWord;
begin
 {}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then InterruptInit;

 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then ClockInit;

 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then MailboxInit;

 {Get Board Revision}
 Revision:=RPiBoardGetRevision;

 {Get Board Type}
 if (Revision and BCM2835_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   case (Revision and BCM2835_BOARD_REVISION_MODEL_MASK) of
    BCM2835_BOARD_REVISION_MODEL_A:BOARD_TYPE:=BOARD_TYPE_RPIA;
    BCM2835_BOARD_REVISION_MODEL_B:BOARD_TYPE:=BOARD_TYPE_RPIB;
    BCM2835_BOARD_REVISION_MODEL_APLUS:BOARD_TYPE:=BOARD_TYPE_RPIA_PLUS;
    BCM2835_BOARD_REVISION_MODEL_BPLUS:BOARD_TYPE:=BOARD_TYPE_RPIB_PLUS;
    BCM2835_BOARD_REVISION_MODEL_COMPUTE:BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE;
    BCM2835_BOARD_REVISION_MODEL_ZERO:BOARD_TYPE:=BOARD_TYPE_RPI_ZERO;
    BCM2835_BOARD_REVISION_MODEL_ZERO_W:BOARD_TYPE:=BOARD_TYPE_RPI_ZERO_W;
   end;
  end
 else
  begin
   {Old Style Revision}
   case (Revision and BCM2835_BOARD_REV_MASK) of
    BCM2835_BOARD_REV_A_7,BCM2835_BOARD_REV_A_8,BCM2835_BOARD_REV_A_9:begin
      BOARD_TYPE:=BOARD_TYPE_RPIA;
     end;
    BCM2835_BOARD_REV_B_I2C0_2,BCM2835_BOARD_REV_B_I2C0_3,BCM2835_BOARD_REV_B_I2C1_4,BCM2835_BOARD_REV_B_I2C1_5,
    BCM2835_BOARD_REV_B_I2C1_6,BCM2835_BOARD_REV_B_REV2_d,BCM2835_BOARD_REV_B_REV2_e,BCM2835_BOARD_REV_B_REV2_f:begin
      BOARD_TYPE:=BOARD_TYPE_RPIB;
     end;
    BCM2835_BOARD_REV_CM,BCM2835_BOARD_REV_CM_2:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE;
     end;
    BCM2835_BOARD_REV_A_PLUS,BCM2835_BOARD_REV_A_PLUS_2,BCM2835_BOARD_REV_APLUS_1:begin
      BOARD_TYPE:=BOARD_TYPE_RPIA_PLUS;
     end;
    BCM2835_BOARD_REV_B_PLUS,BCM2835_BOARD_REV_B_PLUS_2:begin
      BOARD_TYPE:=BOARD_TYPE_RPIB_PLUS;
     end;
    BCM2835_BOARD_REV_ZERO_1,BCM2835_BOARD_REV_ZERO_2,BCM2835_BOARD_REV_ZERO_3:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_ZERO;
     end;
    BCM2835_BOARD_REV_ZERO_W_1:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_ZERO_W;
     end;
   end;
  end;

 {Get CPU Clock Maximum}
 ClockRateMax:=RPiClockGetMaxRate(CLOCK_ID_CPU);
 if ClockRateMax > 0 then
  begin
   {Set CPU Clock}
   RPiClockSetRate(CLOCK_ID_CPU,ClockRateMax,True);
  end;

 {Note: As of firmware dated 19 March 2021 the clock rates reported by the
        firmware may not be accurate unless the ARM has set the rate first}
 {Get Core Clock Maximum}
 ClockRateMax:=RPiClockGetMaxRate(CLOCK_ID_CORE);
 if ClockRateMax > 0 then
  begin
   {Set Core Clock}
   RPiClockSetRate(CLOCK_ID_CORE,ClockRateMax,True);
  end;

 {Get V3D Clock Maximum}
 ClockRateMax:=RPiClockGetMaxRate(CLOCK_ID_V3D);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPiClockSetRate(CLOCK_ID_V3D,ClockRateMax,True);
  end;

 {Get H264 Clock Maximum}
 ClockRateMax:=RPiClockGetMaxRate(CLOCK_ID_H264);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPiClockSetRate(CLOCK_ID_H264,ClockRateMax,True);
  end;

 {Get ISP Clock Maximum}
 ClockRateMax:=RPiClockGetMaxRate(CLOCK_ID_ISP);
 if ClockRateMax > 0 then
  begin
   {Set V3D Clock}
   RPiClockSetRate(CLOCK_ID_ISP,ClockRateMax,True);
  end;

 {Get SDRAM Clock Maximum}
 ClockRateMax:=RPiClockGetMaxRate(CLOCK_ID_SDRAM);
 if ClockRateMax > 0 then
  begin
   {Set SDRAM Clock}
   RPiClockSetRate(CLOCK_ID_SDRAM,ClockRateMax,True);
  end;
end;

{==============================================================================}

procedure RPiMemoryInit;
var
 Address:PtrUInt;
 Length:UInt64;
 Revision:LongWord;
begin
 {}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then InterruptInit;

 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then ClockInit;

 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then MailboxInit;

 {Get Board Revision}
 Revision:=RPiBoardGetRevision;

 {Check Board Revision}
 if (Revision and BCM2835_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   {Model Type}
   case (Revision and BCM2835_BOARD_REVISION_MODEL_MASK) of
    BCM2835_BOARD_REVISION_MODEL_A,BCM2835_BOARD_REVISION_MODEL_APLUS,
    BCM2835_BOARD_REVISION_MODEL_B,BCM2835_BOARD_REVISION_MODEL_BPLUS,BCM2835_BOARD_REVISION_MODEL_COMPUTE,
    BCM2835_BOARD_REVISION_MODEL_ZERO,BCM2835_BOARD_REVISION_MODEL_ZERO_W:begin
      {Memory Size}
      case (Revision and BCM2835_BOARD_REVISION_MEMORY_MASK) of
       BCM2835_BOARD_REVISION_MEMORY_256M:begin
         {Get Memory Base/Size}
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
         MEMORY_LOCAL_SIZE:=SIZE_0;
         MEMORY_SHARED_SIZE:=SIZE_32M;
         MEMORY_DEVICE_SIZE:=SIZE_0;
         MEMORY_NOCACHE_SIZE:=SIZE_0;
         MEMORY_NONSHARED_SIZE:=SIZE_0;
        end;
       BCM2835_BOARD_REVISION_MEMORY_512M:begin
         {Get Memory Base/Size}
         MEMORY_BASE:=$00000000;
         MEMORY_SIZE:=SIZE_512M;
         {Get Memory Page Size}
         MEMORY_PAGE_SIZE:=SIZE_4K;
         MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
         {Get Memory Section Size}
         MEMORY_SECTION_SIZE:=SIZE_1M;
         {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
         MEMORY_IRQ_SIZE:=SIZE_4M;
         MEMORY_FIQ_SIZE:=SIZE_4M;
         MEMORY_LOCAL_SIZE:=SIZE_0;
         MEMORY_SHARED_SIZE:=SIZE_64M;
         MEMORY_DEVICE_SIZE:=SIZE_0;
         MEMORY_NOCACHE_SIZE:=SIZE_0;
         MEMORY_NONSHARED_SIZE:=SIZE_0;
        end;
       BCM2835_BOARD_REVISION_MEMORY_1024M:begin
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
         MEMORY_LOCAL_SIZE:=SIZE_0;
         MEMORY_SHARED_SIZE:=SIZE_64M;
         MEMORY_DEVICE_SIZE:=SIZE_0;
         MEMORY_NOCACHE_SIZE:=SIZE_0;
         MEMORY_NONSHARED_SIZE:=SIZE_0;
        end;
      end;
     end;
   end;

   {New Style Revision}
   (*case (Revision and BCM2835_BOARD_REVISION_MODEL_MASK) of
    BCM2835_BOARD_REVISION_MODEL_A,BCM2835_BOARD_REVISION_MODEL_APLUS:begin
      {Get Memory Base/Size}
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
      MEMORY_LOCAL_SIZE:=SIZE_0;
      MEMORY_SHARED_SIZE:=SIZE_32M;
      MEMORY_DEVICE_SIZE:=SIZE_0;
      MEMORY_NOCACHE_SIZE:=SIZE_0;
      MEMORY_NONSHARED_SIZE:=SIZE_0;
     end;
    BCM2835_BOARD_REVISION_MODEL_B,BCM2835_BOARD_REVISION_MODEL_BPLUS,BCM2835_BOARD_REVISION_MODEL_COMPUTE,
    BCM2835_BOARD_REVISION_MODEL_ZERO,BCM2835_BOARD_REVISION_MODEL_ZERO_W:begin
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_512M;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
      {Get Memory Section Size}
      MEMORY_SECTION_SIZE:=SIZE_1M;
      {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
      MEMORY_IRQ_SIZE:=SIZE_4M;
      MEMORY_FIQ_SIZE:=SIZE_4M;
      MEMORY_LOCAL_SIZE:=SIZE_0;
      MEMORY_SHARED_SIZE:=SIZE_64M;
      MEMORY_DEVICE_SIZE:=SIZE_0;
      MEMORY_NOCACHE_SIZE:=SIZE_0;
      MEMORY_NONSHARED_SIZE:=SIZE_0;
     end;
   end;*)
  end
 else
  begin
   {Old Style Revision}
   case (Revision and BCM2835_BOARD_REV_MASK) of
    BCM2835_BOARD_REV_A_7,BCM2835_BOARD_REV_A_8,BCM2835_BOARD_REV_A_9,BCM2835_BOARD_REV_A_PLUS:begin
      {Get Memory Base/Size}
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
      MEMORY_LOCAL_SIZE:=SIZE_0;
      MEMORY_SHARED_SIZE:=SIZE_32M;
      MEMORY_DEVICE_SIZE:=SIZE_0;
      MEMORY_NOCACHE_SIZE:=SIZE_0;
      MEMORY_NONSHARED_SIZE:=SIZE_0;
     end;
    BCM2835_BOARD_REV_B_I2C0_2,BCM2835_BOARD_REV_B_I2C0_3,BCM2835_BOARD_REV_B_I2C1_4,BCM2835_BOARD_REV_B_I2C1_5,BCM2835_BOARD_REV_B_I2C1_6,
    BCM2835_BOARD_REV_B_REV2_d,BCM2835_BOARD_REV_B_REV2_e,BCM2835_BOARD_REV_B_REV2_f,BCM2835_BOARD_REV_B_PLUS,BCM2835_BOARD_REV_B_PLUS_2,
    BCM2835_BOARD_REV_CM,BCM2835_BOARD_REV_CM_2,BCM2835_BOARD_REV_A_PLUS_2,BCM2835_BOARD_REV_APLUS_1,BCM2835_BOARD_REV_ZERO_1,
    BCM2835_BOARD_REV_ZERO_2,BCM2835_BOARD_REV_ZERO_3,BCM2835_BOARD_REV_ZERO_W_1:begin
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_512M;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
      {Get Memory Section Size}
      MEMORY_SECTION_SIZE:=SIZE_1M;
      {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
      MEMORY_IRQ_SIZE:=SIZE_4M;
      MEMORY_FIQ_SIZE:=SIZE_4M;
      MEMORY_LOCAL_SIZE:=SIZE_0;
      MEMORY_SHARED_SIZE:=SIZE_64M;
      MEMORY_DEVICE_SIZE:=SIZE_0;
      MEMORY_NOCACHE_SIZE:=SIZE_0;
      MEMORY_NONSHARED_SIZE:=SIZE_0;
     end;
   end;
  end;

 {Get CPU Memory}
 if RPiCPUGetMemory(Address,Length) = ERROR_SUCCESS then
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
     MEMORY_LOCAL_SIZE:=SIZE_0;
     MEMORY_SHARED_SIZE:=SIZE_32M;
     MEMORY_DEVICE_SIZE:=SIZE_0;
     MEMORY_NOCACHE_SIZE:=SIZE_0;
     MEMORY_NONSHARED_SIZE:=SIZE_0;
    end;
  end;

 {Get GPU Memory}
 if RPiGPUGetMemory(Address,Length) = ERROR_SUCCESS then
  begin
   GPU_MEMORY_BASE:=Address;
   GPU_MEMORY_SIZE:=Length;
  end;
end;

{==============================================================================}

procedure RPiClockInit;
begin
 {}
 {Setup Timer Registers}
 TimerRegisters:=PBCM2835SystemTimerRegisters(BCM2835_SYSTEM_TIMER_REGS_BASE);

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
   RequestFIQ(RPI_CPU_BOOT,BCM2835_IRQ_SYSTEM_TIMER_3,RPiClockInterrupt,nil);
  end
 else
  begin
   RequestIRQ(RPI_CPU_BOOT,BCM2835_IRQ_SYSTEM_TIMER_3,RPiClockInterrupt,nil);
  end;

 {Setup the first Clock Interrupt}
 RPiClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
end;

{==============================================================================}

procedure RPiPowerInit;
begin
 {}
 {Setup Watchdog Registers}
 WatchdogRegisters:=PBCM2835PMWatchdogRegisters(BCM2835_PM_REGS_BASE);
end;

{==============================================================================}

procedure RPiMailboxInit;
begin
 {}
 {Setup Mailbox0/1 Registers}
 Mailbox0Registers:=PBCM2835Mailbox0Registers(BCM2835_MAILBOX0_REGS_BASE);
 Mailbox1Registers:=PBCM2835Mailbox1Registers(BCM2835_MAILBOX1_REGS_BASE);
end;

{==============================================================================}

procedure RPiInterruptInit;
var
 Count:Integer;
begin
 {}
 {Setup Interrupt Registers}
 InterruptRegisters:=PBCM2835InterruptRegisters(BCM2835_INTERRUPT_REGS_BASE);

 {Setup Interrupt Entries}
 for Count:=0 to BCM2835_IRQ_COUNT - 1 do
  begin
   InterruptEntries[Count]:=nil;
  end;

 {Setup System Call Entries}
 for Count:=0 to RPI_SWI_COUNT - 1 do
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

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear Interrupt Enabled}
 InterruptRegisters.FIQ_control:=0;
 InterruptRegisters.Disable_IRQs_1:=$FFFFFFFF;
 InterruptRegisters.Disable_IRQs_2:=$FFFFFFFF;
 InterruptRegisters.Disable_Basic_IRQs:=$FFFFFFFF;
end;

{==============================================================================}

procedure RPiPeripheralInit;
var
 CacheLineSize:LongWord;
begin
 {}
 {Get Cache Line Size}
 CacheLineSize:=Max(L1DataCacheGetLineSize,L2CacheGetLineSize);

 {Setup Peripherals}
 INTERRUPT_REGS_BASE:=BCM2835_INTERRUPT_REGS_BASE;
 SYSTEMTIMER_REGS_BASE:=BCM2835_SYSTEM_TIMER_REGS_BASE;
 TIMER_REGS_BASE:=BCM2835_TIMER_REGS_BASE;
 GPIO_REGS_BASE:=BCM2835_GPIO_REGS_BASE;
 UART_REGS_BASE:=BCM2835_PL011_REGS_BASE;

 {Setup GPIO}
 GPIO_PIN_COUNT:=BCM2835_GPIO_PIN_COUNT;

 {Setup LEDs}
 case BOARD_TYPE of
  BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
    {Activity LED}
    ACTIVITY_LED_PIN:=GPIO_PIN_16;
    ACTIVITY_LED_PULL:=GPIO_PULL_NONE;
    ACTIVITY_LED_FUNCTION:=GPIO_FUNCTION_OUT;
    ACTIVITY_LED_ACTIVE_LOW:=True;
   end;
  BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS:begin
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
  BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
    {Activity LED}
    ACTIVITY_LED_PIN:=GPIO_PIN_47;
    ACTIVITY_LED_PULL:=GPIO_PULL_NONE;
    ACTIVITY_LED_FUNCTION:=GPIO_FUNCTION_OUT;
    ACTIVITY_LED_ACTIVE_LOW:=True;
   end;
 end;

 {Setup DMA}
 DMA_ALIGNMENT:=SizeOf(LongWord);
 DMA_MULTIPLIER:=SizeOf(LongWord);
 DMA_SHARED_MEMORY:=True;
 DMA_NOCACHE_MEMORY:=False;
 DMA_BUS_ADDRESSES:=True;
 DMA_CACHE_COHERENT:=False; {True;} {L1 Cache is not coherent for normal memory}
 if CacheLineSize > DMA_ALIGNMENT then DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > DMA_MULTIPLIER then DMA_MULTIPLIER:=CacheLineSize;

 {Setup USB}
 USB_DMA_ALIGNMENT:=SizeOf(LongWord);
 USB_DMA_MULTIPLIER:=SizeOf(LongWord);
 USB_DMA_SHARED_MEMORY:=True;
 USB_DMA_NOCACHE_MEMORY:=False;
 USB_DMA_BUS_ADDRESSES:=True;
 USB_DMA_CACHE_COHERENT:=True;
 if CacheLineSize > USB_DMA_ALIGNMENT then USB_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > USB_DMA_MULTIPLIER then USB_DMA_MULTIPLIER:=CacheLineSize;

 {Setup MMC}
 MMC_DMA_ALIGNMENT:=SizeOf(LongWord);
 MMC_DMA_MULTIPLIER:=SizeOf(LongWord);
 MMC_DMA_SHARED_MEMORY:=True;
 MMC_DMA_NOCACHE_MEMORY:=False;
 MMC_DMA_BUS_ADDRESSES:=True;
 MMC_DMA_CACHE_COHERENT:=True;
 if CacheLineSize > MMC_DMA_ALIGNMENT then MMC_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > MMC_DMA_MULTIPLIER then MMC_DMA_MULTIPLIER:=CacheLineSize;

 {Setup BCM2708}
 BCM2708DMA_ALIGNMENT:=SizeOf(LongWord);
 BCM2708DMA_MULTIPLIER:=SizeOf(LongWord);
 BCM2708DMA_SHARED_MEMORY:=True;
 BCM2708DMA_NOCACHE_MEMORY:=False;
 BCM2708DMA_BUS_ADDRESSES:=True;
 BCM2708DMA_CACHE_COHERENT:=True; {Only if buffers are allocated from Shared memory}
 if CacheLineSize > BCM2708DMA_ALIGNMENT then BCM2708DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > BCM2708DMA_MULTIPLIER then BCM2708DMA_MULTIPLIER:=CacheLineSize;

 BCM2708FRAMEBUFFER_ALIGNMENT:=SIZE_256;
 BCM2708FRAMEBUFFER_CACHED:=False; {GPU_MEMORY_CACHED} {Always False on RPi as GPU memory is marked as Shared}

 {Setup DWCOTG}
 DWCOTG_IRQ:=BCM2835_IRQ_USB;
 DWCOTG_POWER_ID:=POWER_ID_USB0;
 DWCOTG_REGS_BASE:=BCM2835_USB_REGS_BASE;
 DWCOTG_DMA_ALIGNMENT:=SizeOf(LongWord);
 DWCOTG_DMA_MULTIPLIER:=SizeOf(LongWord);
 DWCOTG_DMA_SHARED_MEMORY:=True;
 DWCOTG_DMA_NOCACHE_MEMORY:=False;
 DWCOTG_DMA_BUS_ADDRESSES:=True;
 DWCOTG_DMA_CACHE_COHERENT:=True;
 DWCOTG_HOST_FRAME_INTERVAL:=False;
 if CacheLineSize > DWCOTG_DMA_ALIGNMENT then DWCOTG_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > DWCOTG_DMA_MULTIPLIER then DWCOTG_DMA_MULTIPLIER:=CacheLineSize;

 {Setup BCM434XX}
 case BOARD_TYPE of
  BOARD_TYPE_RPI_ZERO_W:begin
    BCM434XX_WLAN_CLK_PIN:=GPIO_PIN_43;
    BCM434XX_WLAN_CLK_PULL:=GPIO_PULL_DOWN;
    BCM434XX_WLAN_CLK_FUNCTION:=GPIO_FUNCTION_ALT0;
    BCM434XX_WLAN_ON_PIN:=GPIO_PIN_41;
    BCM434XX_WLAN_ON_FUNCTION:=GPIO_FUNCTION_OUT;
   end;
 end;

 {Setup LAN}
 case BOARD_TYPE of
  BOARD_TYPE_RPIB,BOARD_TYPE_RPI_COMPUTE,BOARD_TYPE_RPIB_PLUS:begin
    SMSC95XX_MAC_ADDRESS:=BoardGetMACAddress;
   end;
 end;
end;

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPiFramebufferInit;
var
 Status:LongWord;

 DisplayId:LongWord;
 DisplayNum:LongWord;
 DisplayCount:LongWord;
 MultiDisplay:Boolean;

 RPiFramebuffer:PRPiFramebuffer;
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
   RPiFramebuffer:=PRPiFramebuffer(FramebufferDeviceCreateEx(SizeOf(TRPiFramebuffer)));
   if RPiFramebuffer <> nil then
    begin
     {Device}
     RPiFramebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO;
     RPiFramebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
     RPiFramebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_BACKLIGHT;
     RPiFramebuffer.Framebuffer.Device.DeviceData:=nil;
     RPiFramebuffer.Framebuffer.Device.DeviceDescription:=RPI_FRAMEBUFFER_DESCRIPTION + ' (' + FramebufferDisplayIdToName(DisplayId) + ')';
     {Framebuffer}
     RPiFramebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
     RPiFramebuffer.Framebuffer.DeviceAllocate:=RPiFramebufferDeviceAllocate;
     RPiFramebuffer.Framebuffer.DeviceRelease:=RPiFramebufferDeviceRelease;
     RPiFramebuffer.Framebuffer.DeviceBlank:=RPiFramebufferDeviceBlank;
     RPiFramebuffer.Framebuffer.DeviceCommit:=RPiFramebufferDeviceCommit;
     RPiFramebuffer.Framebuffer.DeviceSetBacklight:=RPiFramebufferDeviceSetBacklight;
     {Driver}
     RPiFramebuffer.MultiDisplay:=MultiDisplay;
     RPiFramebuffer.DisplayNum:=DisplayNum;
     FramebufferGetDisplaySettings(DisplayNum,RPiFramebuffer.DisplaySettings);

     {Setup Flags}
     if BCM2708FRAMEBUFFER_CACHED then RPiFramebuffer.Framebuffer.Device.DeviceFlags:=RPiFramebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_COMMIT;
     if BCM2708FRAMEBUFFER_CACHED then RPiFramebuffer.Framebuffer.Device.DeviceFlags:=RPiFramebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
     {if EnvironmentGet('bcm2708_fb.fbswap') <> '1' then RPiFramebuffer.Framebuffer.Device.DeviceFlags:=RPiFramebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;} {Handled by FramebufferAllocate}

     {Register Framebuffer}
     Status:=FramebufferDeviceRegister(@RPiFramebuffer.Framebuffer);
     if Status = ERROR_SUCCESS then
      begin
       {Allocate Framebuffer}
       Status:=FramebufferDeviceAllocate(@RPiFramebuffer.Framebuffer,nil);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: Failed to allocate new framebuffer device: ' + ErrorToString(Status));

         {Deregister Framebuffer}
         FramebufferDeviceDeregister(@RPiFramebuffer.Framebuffer);

         {Destroy Framebuffer}
         FramebufferDeviceDestroy(@RPiFramebuffer.Framebuffer);
        end;
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Platform: Failed to register new framebuffer device: ' + ErrorToString(Status));

       {Destroy Framebuffer}
       FramebufferDeviceDestroy(@RPiFramebuffer.Framebuffer);
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

procedure RPiPageTableInit;
{Initialize the Hardware Page Tables before enabling the MMU
 See page 6-36 of the ARM1176JZF-S Technical Reference Manual}
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

 {Create the first level page table}
 {Setup 1MB sections covering the entire 4GB address space with a default layout}
 {Set the 1MB sections in the first 1GB as ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
 Address:=$00000000;
 for Count:=0 to 1023 do
  begin
   ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
   Inc(Address,SIZE_1M);
  end;

 {Set the 1MB sections in the second 1GB as ARMV6_L1D_CACHE_NORMAL_WRITE_THROUGH (Non Shared)(Non Executable)(Read Write)}
 for Count:=1024 to 2047 do
  begin
   if CPU_MEMORY_RESTRICTED then
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_NONCACHED or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_NONE);
    end
   else
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_THROUGH or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
    end;
   Inc(Address,SIZE_1M);
  end;

 {Set the 1MB sections in the remaining 2GB as ARMV6_L1D_CACHE_NORMAL_NONCACHED (Non Shared)(Non Executable)(Read Write)}
 for Count:=2048 to 4095 do
  begin
   if CPU_MEMORY_RESTRICTED then
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_NONCACHED or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_NONE);
    end
   else
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_NONCACHED or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
    end;
   Inc(Address,SIZE_1M);
  end;

 {Set the 1MB sections containing the PERIPHERALS_BASE to ARMV6_L1D_CACHE_SHARED_DEVICE (Non Shared)(Non Executable)(Read Write)}
 if PERIPHERALS_SIZE > 0 then
  begin
   Address:=(PERIPHERALS_BASE and ARMV6_L1D_SECTION_BASE_MASK);
   while Address < (PERIPHERALS_BASE + PERIPHERALS_SIZE) do
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_SHARED_DEVICE or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
  end;

 {Set the 1MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV6_L1D_CACHE_SHARED_DEVICE (Non Shared)(Non Executable)(Read Write)}
 if LOCAL_PERIPHERALS_SIZE > 0 then
  begin
   Address:=(LOCAL_PERIPHERALS_BASE and ARMV6_L1D_SECTION_BASE_MASK);
   while Address < (LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE) do
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_SHARED_DEVICE or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
  end;

 {Create the second level (Coarse) page tables}
 Table:=(PAGE_TABLES_ADDRESS and ARMV6_L1D_COARSE_BASE_MASK);
 Address:=$00000000;
 for Count:=0 to PAGE_TABLES_USED - 1 do
  begin
   ARMv6SetPageTableCoarse(Address,Table,0);
   Inc(Table,SIZE_1K);
   Inc(Address,SIZE_1M);
  end;
 PAGE_TABLES_NEXT:=Table;

 {Set the 4KB zero page to ARMV6_L2D_SMALL_CACHE_NORMAL_NONCACHED (Non Shared)(Non Executable)(No Access)}
 Address:=$00000000;
 ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_NONCACHED or ARMV6_L2D_FLAG_SMALL_XN or ARMV6_L2D_ACCESS_NONE);

 {Set the 4KB pages containing the VECTOR_TABLE_BASE to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)}
 Address:=(VECTOR_TABLE_BASE and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (VECTOR_TABLE_BASE + VECTOR_TABLE_SIZE) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH or ARMV6_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the first level page table to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_TABLE_BASE and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (PAGE_TABLE_BASE + PAGE_TABLE_SIZE) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK or ARMV6_L2D_FLAG_SMALL_XN or ARMV6_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the TEXT (Code) section to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH (Non Shared)(Executable)(Read Only)}
 Address:=(PtrUInt(@_text_start) and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (PtrUInt(@_data)) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH or ARMV6_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the DATA (Initialized) section to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
 Address:=(PtrUInt(@_data) and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (PtrUInt(@_bss_start)) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK or ARMV6_L2D_FLAG_SMALL_XN or ARMV6_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the BSS (Uninitialized) section to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
 Address:=(PtrUInt(@_bss_start) and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (PtrUInt(@_bss_end)) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK or ARMV6_L2D_FLAG_SMALL_XN or ARMV6_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the second level page tables to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
 Address:=(PAGE_TABLES_ADDRESS and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (PAGE_TABLES_ADDRESS + PAGE_TABLES_LENGTH) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK or ARMV6_L2D_FLAG_SMALL_XN or ARMV6_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the initial stack to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_STACK_BASE and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (INITIAL_STACK_BASE + INITIAL_STACK_SIZE) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK or ARMV6_L2D_FLAG_SMALL_XN or ARMV6_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the initial heap to ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
 Address:=(INITIAL_HEAP_BASE and ARMV6_L2D_SMALL_BASE_MASK);
 while Address < (INITIAL_HEAP_BASE + INITIAL_HEAP_SIZE) do
  begin
   ARMv6SetPageTableSmall(Address,Address,ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK or ARMV6_L2D_FLAG_SMALL_XN or ARMV6_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the starting address for NoCache/Device/Shared/Local/IRQ/FIQ Blocks}
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
   Dec(RequestAddress,MEMORY_LOCAL_SIZE * RPI_CPU_COUNT); {Local memory is per CPU}
   if IRQ_ENABLED then Dec(RequestAddress,MEMORY_IRQ_SIZE * RPI_CPU_COUNT); {IRQ memory is per CPU}
   if FIQ_ENABLED then Dec(RequestAddress,MEMORY_FIQ_SIZE * RPI_CPU_COUNT); {FIQ memory is per CPU}

   {Register 1MB Non Shared Memory Blocks as ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
   if MEMORY_NONSHARED_SIZE > 0 then
    begin
     ActualAddress:=PtrUInt(RequestNonSharedHeapBlock(Pointer(RequestAddress),MEMORY_NONSHARED_SIZE));
     if ActualAddress > 0 then
      begin
       Address:=ActualAddress;
       while Address < (ActualAddress + MEMORY_NONSHARED_SIZE) do
        begin
         ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
         Inc(Address,SIZE_1M);
        end;
       Inc(RequestAddress,MEMORY_NONSHARED_SIZE);
      end;
    end;

   {Register 1MB Non Cached Memory Blocks as ARMV6_L1D_CACHE_NORMAL_NONCACHED (Non Shared)(Non Executable)(Read Write)}
   if MEMORY_NOCACHE_SIZE > 0 then
    begin
     ActualAddress:=PtrUInt(RequestNoCacheHeapBlock(Pointer(RequestAddress),MEMORY_NOCACHE_SIZE));
     if ActualAddress > 0 then
      begin
       Address:=ActualAddress;
       while Address < (ActualAddress + MEMORY_NOCACHE_SIZE) do
        begin
         ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_NONCACHED or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
         Inc(Address,SIZE_1M);
        end;
       Inc(RequestAddress,MEMORY_NOCACHE_SIZE);
      end;
    end;

   {Register 1MB Device Memory Blocks as ARMV6_L1D_CACHE_SHARED_DEVICE (Non Shared)(Non Executable)(Read Write)}
   if MEMORY_DEVICE_SIZE > 0 then
    begin
     ActualAddress:=PtrUInt(RequestDeviceHeapBlock(Pointer(RequestAddress),MEMORY_DEVICE_SIZE));
     if ActualAddress > 0 then
      begin
       Address:=ActualAddress;
       while Address < (ActualAddress + MEMORY_DEVICE_SIZE) do
        begin
         ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_SHARED_DEVICE or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
         Inc(Address,SIZE_1M);
        end;
       Inc(RequestAddress,MEMORY_DEVICE_SIZE);
      end;
    end;

   {Register 1MB Shared Memory Blocks as ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Shared)(Non Executable)(Read Write)}
   if MEMORY_SHARED_SIZE > 0 then
    begin
     ActualAddress:=PtrUInt(RequestSharedHeapBlock(Pointer(RequestAddress),MEMORY_SHARED_SIZE));
     if ActualAddress > 0 then
      begin
       Address:=ActualAddress;
       while Address < (ActualAddress + MEMORY_SHARED_SIZE) do
        begin
         ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_FLAG_SHARED or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
         Inc(Address,SIZE_1M);
        end;
       Inc(RequestAddress,MEMORY_SHARED_SIZE);
      end;
    end;

   {Register 1MB Local Memory Blocks as ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
   if MEMORY_LOCAL_SIZE > 0 then
    begin
     for Count:=0 to (RPI_CPU_COUNT - 1) do
      begin
       ActualAddress:=PtrUInt(RequestLocalHeapBlock(Pointer(RequestAddress),MEMORY_LOCAL_SIZE,(1 shl Count)));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         while Address < (ActualAddress + MEMORY_LOCAL_SIZE) do
          begin
           ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
           Inc(Address,SIZE_1M);
          end;
         Inc(RequestAddress,MEMORY_LOCAL_SIZE);
        end;
      end;
    end;

   {Register 1MB IRQ Memory Blocks as ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
   if IRQ_ENABLED and (MEMORY_IRQ_SIZE > 0) then
    begin
     for Count:=0 to (RPI_CPU_COUNT - 1) do
      begin
       ActualAddress:=PtrUInt(RequestIRQHeapBlock(Pointer(RequestAddress),MEMORY_IRQ_SIZE,(1 shl Count)));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         while Address < (ActualAddress + MEMORY_IRQ_SIZE) do
          begin
           ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
           Inc(Address,SIZE_1M);
          end;
         Inc(RequestAddress,MEMORY_IRQ_SIZE);
        end;
      end;
    end;

   {Register 1MB FIQ Memory Blocks as ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Non Shared)(Non Executable)(Read Write)}
   if FIQ_ENABLED and (MEMORY_FIQ_SIZE > 0) then
    begin
     for Count:=0 to (RPI_CPU_COUNT - 1) do
      begin
       ActualAddress:=PtrUInt(RequestFIQHeapBlock(Pointer(RequestAddress),MEMORY_FIQ_SIZE,(1 shl Count)));
       if ActualAddress > 0 then
        begin
         Address:=ActualAddress;
         while Address < (ActualAddress + MEMORY_FIQ_SIZE) do
          begin
           ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
           Inc(Address,SIZE_1M);
          end;
         Inc(RequestAddress,MEMORY_FIQ_SIZE);
        end;
      end;
    end;
  end;

 {Set the 1MB sections containing the GPU_MEMORY to ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Shared)(Non Executable)(Read Write)}
 if GPU_MEMORY_SIZE > 0 then
  begin
   Address:=(GPU_MEMORY_BASE and ARMV6_L1D_SECTION_BASE_MASK);
   while (Address < (GPU_MEMORY_BASE + GPU_MEMORY_SIZE)) and (Address < (PERIPHERALS_BASE and ARMV6_L1D_SECTION_BASE_MASK)) do
    begin
     if GPU_MEMORY_CACHED then
      begin
       ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_FLAG_SHARED or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
      end
     else
      begin
       ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_THROUGH or ARMV6_L1D_FLAG_SHARED or ARMV6_L1D_FLAG_XN or ARMV6_L1D_ACCESS_READWRITE);
      end;
     Inc(Address,SIZE_1M);
    end;
  end;

 {Synchronization Barrier}
 DataSynchronizationBarrier;
end;

{==============================================================================}

procedure RPiPowerLEDEnable;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
    {Not Supported}
   end;
  BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS:begin
    {Disable Pull Up/Down}
    GPIOPullSelect(GPIO_PIN_35,GPIO_PULL_NONE);
    {Enable Output}
    GPIOFunctionSelect(GPIO_PIN_35,GPIO_FUNCTION_OUT);
   end;
  BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
    {Not Supported}
   end;
 end;
end;

{==============================================================================}

procedure RPiPowerLEDOn;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
    {Not Supported}
   end;
  BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS:begin
    {LED On}
    GPIOOutputSet(GPIO_PIN_35,GPIO_LEVEL_HIGH);
   end;
  BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
    {Not Supported}
   end;
 end;
end;

{==============================================================================}

procedure RPiPowerLEDOff;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
    {Not Supported}
   end;
  BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS:begin
    {LED Off}
    GPIOOutputSet(GPIO_PIN_35,GPIO_LEVEL_LOW);
   end;
  BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
    {Not Supported}
   end;
 end;
end;

{==============================================================================}

procedure RPiActivityLEDEnable;
var
 Value:LongWord;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {Read current value of GPFSEL}
      Value:=GPIORead(RPI_GPIO_ACTLED_GPFSEL);
      {Mask off relevant bits}
      Value:=Value and not(RPI_GPIO_ACTLED_GPFMASK shl RPI_GPIO_ACTLED_GPFSHIFT);
      {Include required bits}
      Value:=Value or (1 shl RPI_GPIO_ACTLED_GPFSHIFT);
      {Write new value to GPFSEL}
      GPIOWrite(RPI_GPIO_ACTLED_GPFSEL,Value);
     end
    else
     begin
      {Disable Pull Up/Down}
      GPIOPullSelect(GPIO_PIN_16,GPIO_PULL_NONE);
      {Enable Output}
      GPIOFunctionSelect(GPIO_PIN_16,GPIO_FUNCTION_OUT);
     end;
   end;
  BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS,BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {Read current value of GPFSEL}
      Value:=GPIORead(RPIPLUS_GPIO_ACTLED_GPFSEL);
      {Mask off relevant bits}
      Value:=Value and not(RPIPLUS_GPIO_ACTLED_GPFMASK shl RPIPLUS_GPIO_ACTLED_GPFSHIFT);
      {Include required bits}
      Value:=Value or (1 shl RPIPLUS_GPIO_ACTLED_GPFSHIFT);
      {Write new value to GPFSEL}
      GPIOWrite(RPIPLUS_GPIO_ACTLED_GPFSEL,Value);
     end
    else
     begin
      {Disable Pull Up/Down}
      GPIOPullSelect(GPIO_PIN_47,GPIO_PULL_NONE);
      {Enable Output}
      GPIOFunctionSelect(GPIO_PIN_47,GPIO_FUNCTION_OUT);
     end;
   end;
 end;
end;

{==============================================================================}

procedure RPiActivityLEDOn;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED On}
      GPIOWrite(RPI_GPIO_ACTLED_GPCLR,(RPI_GPIO_ACTLED_GPMASK shl RPI_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED On}
      GPIOOutputSet(GPIO_PIN_16,GPIO_LEVEL_LOW);
     end;
   end;
  BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED On}
      GPIOWrite(RPIPLUS_GPIO_ACTLED_GPSET,(RPIPLUS_GPIO_ACTLED_GPMASK shl RPIPLUS_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED On}
      GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_HIGH);
     end;
   end;
  BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED On}
      GPIOWrite(RPIPLUS_GPIO_ACTLED_GPCLR,(RPIPLUS_GPIO_ACTLED_GPMASK shl RPIPLUS_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED On}
      GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_LOW);
     end;
   end;
 end;
end;

{==============================================================================}

procedure RPiActivityLEDOff;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED Off}
      GPIOWrite(RPI_GPIO_ACTLED_GPSET,(RPI_GPIO_ACTLED_GPMASK shl RPI_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED Off}
      GPIOOutputSet(GPIO_PIN_16,GPIO_LEVEL_HIGH);
     end;
   end;
  BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED Off}
      GPIOWrite(RPIPLUS_GPIO_ACTLED_GPCLR,(RPIPLUS_GPIO_ACTLED_GPMASK shl RPIPLUS_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED Off}
      GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_LOW);
     end;
   end;
  BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED Off}
      GPIOWrite(RPIPLUS_GPIO_ACTLED_GPSET,(RPIPLUS_GPIO_ACTLED_GPMASK shl RPIPLUS_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED Off}
      GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_HIGH);
     end;
   end;
 end;
end;

{==============================================================================}

function RPiMailboxReceive(Mailbox,Channel:LongWord):LongWord;
{Receive from specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 ResultCode:LongWord;
begin
 {}
 Result:=0;
 {Check Mailbox}
 if Mailbox = BCM2835_MAILBOX_0 then
  begin
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try
    {Setup Timeout}
    Timeout:=RPI_MAILBOX_TIMEOUT;

    {Setup Result}
    ResultCode:=BCM2835_MAILBOX_CHANNEL_MASK; {Start with all channel bits set}

    {Check Channel}
    while ((ResultCode and BCM2835_MAILBOX_CHANNEL_MASK) <> Channel) do
     begin
      {Check Status}
      while (Mailbox0Registers.Status and BCM2835_MAILBOX_STATUS_EMPTY) = BCM2835_MAILBOX_STATUS_EMPTY do
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
    Result:=ResultCode and BCM2835_MAILBOX_DATA_MASK; {Account for channel offset}
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end;
end;

{==============================================================================}

procedure RPiMailboxSend(Mailbox,Channel,Data:LongWord);
{Send to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 WriteData:LongWord;
begin
 {}
 {Check Mailbox}
 if Mailbox = BCM2835_MAILBOX_0 then
  begin
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try
    {Setup Timeout}
    Timeout:=RPI_MAILBOX_TIMEOUT;

    {Setup Data}
    WriteData:=Channel or (Data and BCM2835_MAILBOX_DATA_MASK);

    {Check Status}
    while (Mailbox1Registers.Status and BCM2835_MAILBOX_STATUS_FULL) = BCM2835_MAILBOX_STATUS_FULL do
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

function RPiMailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
{Perform a transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPiMailboxCallEx(Mailbox,Channel,Data,Response,RPI_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPiMailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord;
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
 if Mailbox = BCM2835_MAILBOX_0 then
  begin
   {Check the Data (Must not use the lowest 4 bits)}
   if (Data and BCM2835_MAILBOX_CHANNEL_MASK) <> 0 then Exit;

   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try
    {Setup Timeout}
    Retries:=Timeout;

    {Wait for Mailbox 0 Empty}
    while (Mailbox0Registers.Status and BCM2835_MAILBOX_STATUS_EMPTY) <> BCM2835_MAILBOX_STATUS_EMPTY do
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
    while (Mailbox1Registers.Status and BCM2835_MAILBOX_STATUS_FULL) = BCM2835_MAILBOX_STATUS_FULL do
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
    WriteData:=Channel or (Data and BCM2835_MAILBOX_DATA_MASK);
    Mailbox1Registers.Write:=WriteData;

    {Setup Timeout}
    Retries:=Timeout;

    {Wait for Mailbox 0 not Empty}
    while (Mailbox0Registers.Status and BCM2835_MAILBOX_STATUS_EMPTY) = BCM2835_MAILBOX_STATUS_EMPTY do
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
    if (ResultCode and BCM2835_MAILBOX_CHANNEL_MASK) <> Channel then
     begin
      Result:=ERROR_INVALID_DATA;
      Exit;
     end;

    {Return the Response}
    Response:=ResultCode and BCM2835_MAILBOX_DATA_MASK; {Account for channel offset}

    Result:=ERROR_SUCCESS;
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end;
end;

{==============================================================================}

function RPiMailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPiMailboxPropertyCallEx(Mailbox,Channel,Data,Response,RPI_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPiMailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord;
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
var
 Tag:PBCM2835MailboxTagHeader;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF PLATFORM_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - (Mailbox=' + IntToHex(Mailbox,8) + ' Channel=' + IntToHex(Channel,8) + ' Data=' + IntToHex(PtrUInt(Data),8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}

 {Check Mailbox}
 if Mailbox = BCM2835_MAILBOX_0 then
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
   if PBCM2835MailboxHeader(Data).Code <> BCM2835_MBOX_RESPONSE_CODE_SUCCESS then
    begin
     Result:=ERROR_FUNCTION_FAILED;
     if PLATFORM_LOG_ENABLED then PlatformLogError('MailboxPropertyCallEx - Response Code Failed: (Code=' + IntToHex(PBCM2835MailboxHeader(Data).Code,8) + ')');
     Exit;
    end;

   {Check each tags Response Code}
   Tag:=PBCM2835MailboxTagHeader(PtrUInt(Data) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
   while Tag.Tag <> BCM2835_MBOX_TAG_END do
    begin
     if (Tag.Length and BCM2835_MBOX_TAG_RESPONSE_CODE) = 0 then
      begin
       {$IFDEF PLATFORM_DEBUG}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - Tag Response Code Incorrect (Length=' + IntToHex(Tag.Length,8) + ')');
       {$ENDIF}
       {Result:=ERROR_FUNCTION_FAILED;} {Note: Recent firmware functions do not always set the response bit in the tag}
       {Exit;}                          {      The Linux firmware driver does not check this bit in the response}
      end;
     {Clear the Response bit so callers can read the length field without extra processing}
     Tag.Length:=Tag.Length and not(BCM2835_MBOX_TAG_RESPONSE_CODE);
     {Get Next Tag}
     Tag:=PBCM2835MailboxTagHeader(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagHeader)) + Tag.Size);
    end;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function RPiMailboxPropertyTag(Tag:LongWord;Data:Pointer;Size:LongWord):LongWord;
{Request a property tag (Get/Set) from the mailbox property channel}
{Note: Data does not need to include mailbox property channel header or footer}
{Note: Data pointer does not need any specific alignment or caching attributes}
{Note: Size must be a multiple of 4 bytes}
{Note: Size must include the size of the request and response which use the same buffer}
var
 Total:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 TagHeader:PBCM2835MailboxTagHeader;
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
 Total:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagHeader) + Size + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Total,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Total,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Total,0);

  {Setup Header}
  Header.Size:=Total;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  TagHeader:=PBCM2835MailboxTagHeader(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  TagHeader.Tag:=Tag;
  TagHeader.Size:=Size;
  TagHeader.Length:=Size;

  {Copy Request}
  TagBuffer:=Pointer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)) + PtrUInt(SizeOf(TBCM2835MailboxTagHeader)));
  System.Move(Data^,TagBuffer^,Size);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(TagHeader) + PtrUInt(SizeOf(TBCM2835MailboxTagHeader)) + Size);
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiRequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified IRQ number}
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
 Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

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

 {Register Entry}
 Result:=RPiInterruptRegisterEntry(Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPiReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified IRQ number}
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
 Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

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

 {Deregister Entry}
 Result:=RPiInterruptDeregisterEntry(Entry);
end;

{==============================================================================}

function RPiRequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified FIQ number}
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
 Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

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

 {Register Entry}
 Result:=RPiInterruptRegisterEntry(Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPiReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified FIQ number}
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
 Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

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

 {Deregister Entry}
 Result:=RPiInterruptDeregisterEntry(Entry);
end;

{==============================================================================}

function RPiRegisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified interrupt number (Where Applicable)}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

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

 {Register Entry}
 Result:=RPiInterruptRegisterEntry(Entry^);

 {Release Entry on failure}
 if Result <> ERROR_SUCCESS then FreeMem(Entry);
end;

{==============================================================================}

function RPiDeregisterInterrupt(Number,Mask,Priority,Flags:LongWord;Handler:TSharedInterruptHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified interrupt number (Where Applicable)}
var
 Entry:TInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {Get Mask}
 Mask:=CPUIDToMask(CPUGetCurrent); {Single CPU only}

 {Clear Entry}
 FillChar(Entry,SizeOf(TInterruptEntry),0);

 {Update Entry}
 Entry.CPUMask:=Mask;
 Entry.Number:=Number;
 Entry.Priority:=Priority;
 Entry.Flags:=Flags;
 Entry.SharedHandler:=Handler;
 Entry.Parameter:=Parameter;

 {Deregister Entry}
 Result:=RPiInterruptDeregisterEntry(Entry);
end;

{==============================================================================}

function RPiRegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request registration of the supplied extended handler to the specified System Call number}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

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

function RPiDeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
{Request deregistration of the supplied extended handler from the specified System Call number}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

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

function RPiGetInterruptEntry(Number,Instance:LongWord;var Interrupt:TInterruptEntry):LongWord;
{Get the interrupt entry for the specified interrupt number and instance}
begin
 {}
 Result:=RPiInterruptGetEntry(CPU_ID_ALL,Number,INTERRUPT_FLAG_NONE,Interrupt,Instance);
end;

{==============================================================================}

function RPiGetSystemCallEntry(Number:LongWord):TSystemCallEntry;
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

function RPiSystemRestart(Delay:LongWord):LongWord;
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
  WatchdogRegisters.WDOG:=BCM2835_PM_PASSWORD or ((Delay * BCM2835_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2835_PM_WDOG_TIME_MASK);

  {Enable Restart}
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2835_PM_PASSWORD or (Current and BCM2835_PM_RSTC_WRCFG_CLR) or BCM2835_PM_RSTC_WRCFG_FULL_RESET;

  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read}

  {Wait for Restart}
  MillisecondDelay(1000);

  Result:=ERROR_SUCCESS;

  //See: bcm2709_restart in \linux-rpi-3.18.y\arch\arm\mach-bcm2709\bcm2709.c
  //     bcm2708_restart in \linux-rpi-3.18.y\arch\arm\mach-bcm2708\bcm2708.c
 finally
  {Release Lock}
  if ShutdownLock.Lock <> INVALID_HANDLE_VALUE then ShutdownLock.ReleaseLock(ShutdownLock.Lock);
 end;
end;

{==============================================================================}

function RPiSystemShutdown(Delay:LongWord):LongWord;
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
  {WatchdogRegisters.RSTS:=BCM2835_PM_PASSWORD or (Current and BCM2835_PM_RSTC_WRCFG_CLR) or BCM2835_PM_RSTS_HADWRH_SET;} {RPi firmware changed to use a different value}
  WatchdogRegisters.RSTS:=Current or BCM2835_PM_PASSWORD or BCM2835_PM_RSTS_RASPBERRYPI_HALT;

  {Enable Watchdog}
  WatchdogRegisters.WDOG:=BCM2835_PM_PASSWORD or ((Delay * BCM2835_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2835_PM_WDOG_TIME_MASK);

  {Enable Restart}
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2835_PM_PASSWORD or (Current and BCM2835_PM_RSTC_WRCFG_CLR) or BCM2835_PM_RSTC_WRCFG_FULL_RESET;

  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read}

  {Wait for Shutdown}
  MillisecondDelay(1000);

  Result:=ERROR_SUCCESS;

  //See: bcm2709_power_off in \linux-rpi-3.18.y\arch\arm\mach-bcm2709\bcm2709.c
  //     bcm2708_power_off in \linux-rpi-3.18.y\arch\arm\mach-bcm2708\bcm2708.c
 finally
  {Release Lock}
  if ShutdownLock.Lock <> INVALID_HANDLE_VALUE then ShutdownLock.ReleaseLock(ShutdownLock.Lock);
 end;
end;

{==============================================================================}

function RPiSystemGetCommandLine:String;
{Get the Command Line from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetCommandLine;
begin
 {}
 Result:='';

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetCommandLine) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetCommandLine(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_COMMAND_LINE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetCommandLine) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetCommandLine)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiCPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;
{Get the CPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetARMMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetARMMemory) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetARMMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_ARM_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetARMMemory) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetARMMemory)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiGPUGetState:LongWord;
begin
 {}
 Result:=GPU_STATE_NONE;

 //To Do
end;

{==============================================================================}

function RPiGPUGetMemory(var Address:PtrUInt;var Length:UInt64):LongWord;
{Get the GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetVCMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetVCMemory) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetVCMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_VC_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetVCMemory) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetVCMemory)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiBoardGetModel:LongWord;
{Get the Board Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Model:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetBoardModel;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetBoardModel) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetBoardModel(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_BOARD_MODEL;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetBoardModel) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetBoardModel)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiBoardGetSerial:Int64;
{Get the Board Serial from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetBoardSerial;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetBoardSerial) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetBoardSerial(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_BOARD_SERIAL;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetBoardSerial) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetBoardSerial)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiBoardGetRevision:LongWord;
{Get the Board Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetBoardRevision;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetBoardRevision) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetBoardRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_BOARD_REV;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetBoardRevision) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetBoardRevision)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiBoardGetMACAddress:String;
{Get the Board MAC Address from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetMACAddress;
begin
 {}
 Result:='';

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetMACAddress) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetMACAddress(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_MAC_ADDRESS;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetMACAddress) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetMACAddress)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiFirmwareGetRevision:LongWord;
{Get the Firmware Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetFirmwareRevision;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetFirmwareRevision) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetFirmwareRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_FIRMWARE_REV;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetFirmwareRevision) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetFirmwareRevision)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiFirmwareGetThrottled:LongWord;
{Get the Firmware Throttling state from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetThrottled;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetThrottled) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetThrottled(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_THROTTLED;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetThrottled) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Value:=$FFFF; {Clear sticky bits}

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetThrottled)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiPowerGetWait(PowerId:LongWord):LongWord;
{Get the Power Wait from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetTiming;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetTiming) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetTiming(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_TIMING;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetTiming) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPiConvertPowerIdRequest(PowerId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetTiming)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiPowerGetState(PowerId:LongWord):LongWord;
{Get the Power State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetPowerState;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetPowerState) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetPowerState) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPiConvertPowerIdRequest(PowerId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetPowerState)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerGetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Power State}
  Result:=RPiConvertPowerStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiPowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;
{Set the Power State in the Mailbox property tags channel}
{Note: Power Lock not required due to Mailbox Property Call serialization}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetPowerState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetPowerState) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetPowerState) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPiConvertPowerIdRequest(PowerId);
  Tag.Request.State:=RPiConvertPowerStateRequest(State);
  if Wait then Tag.Request.State:=(Tag.Request.State or BCM2835_MBOX_SET_POWER_STATE_REQ_WAIT);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetPowerState)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if Wait then
   begin
    Result:=MailboxPropertyCallEx(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response,RPI_MAILBOX_TIMEOUT_EX);
   end
  else
   begin
    Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
   end;
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerSetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Check Power State}
  if Wait then
   begin
    if RPiConvertPowerStateRequest(State) = BCM2835_MBOX_SET_POWER_STATE_REQ_ON then
     begin
      if (Tag.Response.State and BCM2835_MBOX_POWER_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
     end
    else
     begin
      if (Tag.Response.State and BCM2835_MBOX_POWER_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
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

function RPiClockGetCount:LongWord;
{Gets the current system clock count (32 least significant bits of total)}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz and therefore overflows every 4295 seconds}
begin
 {}
 {Get Value}
 Result:=TimerRegisters.CLO;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}

function RPiClockGetTotal:Int64;
{Gets the total system clock count}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz, the clock interrupt also uses this timer to
 increment the clock every second and therefore keep time}
var
 Check:LongWord;
begin
 {}
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
end;


{==============================================================================}

function RPiClockGetRate(ClockId:LongWord):LongWord;
{Get the Clock Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetClockRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetClockRate) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetClockRate) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPiConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetClockRate)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;
{Set the Clock Rate in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetClockRate;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Rate}
 if Rate = 0 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetClockRate) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetClockRate) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPiConvertClockIdRequest(ClockId);
  Tag.Request.Rate:=Rate;
  Tag.Request.SkipTurbo:=0;
  if not(Turbo) then Tag.Request.SkipTurbo:=BCM2835_MBOX_CLOCK_RATE_REQ_SKIP_TURBO;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetClockRate)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiClockGetState(ClockId:LongWord):LongWord;
{Get the Clock State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetClockState;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetClockState) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetClockState) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPiConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetClockState)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Clock State}
  Result:=RPiConvertClockStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiClockSetState(ClockId,State:LongWord):LongWord;
{Set the Clock State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetClockState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetClockState) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetClockState) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPiConvertClockIdRequest(ClockId);
  Tag.Request.State:=RPiConvertClockStateRequest(State);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetClockState)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockSetState - MailboxPropertyCall Failed');
    Exit;
   end;

  {Check Clock State}
  if RPiConvertClockStateRequest(State) = BCM2835_MBOX_SET_CLOCK_STATE_REQ_ON then
   begin
    if (Tag.Response.State and BCM2835_MBOX_CLOCK_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
   end
  else
   begin
    if (Tag.Response.State and BCM2835_MBOX_CLOCK_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiClockGetMinRate(ClockId:LongWord):LongWord;
{Get the Clock Min Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetClockMinRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetClockMinRate) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetClockMinRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_CLOCK_MIN_RATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetClockMinRate) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPiConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetClockMinRate)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiClockGetMaxRate(ClockId:LongWord):LongWord;
{Get the Clock Max Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetClockMaxRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetClockMaxRate) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetClockMaxRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_CLOCK_MAX_RATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetClockMaxRate) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPiConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetClockMaxRate)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiClockGetMeasuredRate(ClockId:LongWord):LongWord;
{Get the Clock Measured Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetClockMeasuredRate;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetClockMeasuredRate) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetClockMeasuredRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_CLOCK_MEASURED;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetClockMeasuredRate) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPiConvertClockIdRequest(ClockId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetClockMeasuredRate)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiTurboGetState(TurboId:LongWord):LongWord;
{Get the Turbo State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetTurbo;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetTurbo) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetTurbo) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetTurbo)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiTurboSetState(TurboId,State:LongWord):LongWord;
{Set the Turbo State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetTurbo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetTurbo) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetTurbo) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}
  Tag.Request.Level:=State; {0 to Off / 1 for On}

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetTurbo)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiVoltageGetValue(VoltageId:LongWord):LongWord;
{Get the Voltage Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetVoltage;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetVoltage) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetVoltage) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPiConvertVoltageIdRequest(VoltageId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetVoltage)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Voltage Value}
  if (Tag.Response.Value <> BCM2835_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiVoltageSetValue(VoltageId,Value:LongWord):LongWord;
{Set the Voltage Value in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetVoltage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Value}
 if Value = 0 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetVoltage) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetVoltage) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPiConvertVoltageIdRequest(VoltageId);
  Tag.Request.Value:=Value; {Offset from 1.2V in units of 0.025V}

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetVoltage)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageSetValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Check Voltage Value}
  if (Tag.Response.Value <> BCM2835_MBOX_VOLTAGE_INVALID) and (Tag.Response.Value = Value) then Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiVoltageGetMinValue(VoltageId:LongWord):LongWord;
{Get the Voltage Min Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetMinVoltage;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetMinVoltage) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetMinVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_MIN_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetMinVoltage) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPiConvertVoltageIdRequest(VoltageId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetMinVoltage)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMinValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Voltage Min Value}
  if (Tag.Response.Value <> BCM2835_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiVoltageGetMaxValue(VoltageId:LongWord):LongWord;
{Get the Voltage Max Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetMaxVoltage;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetMaxVoltage) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetMaxVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_MAX_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetMaxVoltage) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPiConvertVoltageIdRequest(VoltageId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetMaxVoltage)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMaxValue - MailboxPropertyCall Failed');
    Exit;
   end;

  {Get Voltage Max Value}
  if (Tag.Response.Value <> BCM2835_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiTemperatureGetCurrent(TemperatureId:LongWord):LongWord;
{Get the Temperature Current from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetTemperature;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetTemperature) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetTemperature) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPiConvertTemperatureIdRequest(TemperatureId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetTemperature)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiTemperatureGetMaximum(TemperatureId:LongWord):LongWord;
{Get the Temperature Maximum Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetMaxTemperature;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetMaxTemperature) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetMaxTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_MAX_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetMaxTemperature) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPiConvertTemperatureIdRequest(TemperatureId);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetMaxTemperature)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiGPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
{Allocate GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagAllocateMemory;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagAllocateMemory) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagAllocateMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_ALLOCATE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagAllocateMemory) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Size:=Length;
  Tag.Request.Alignment:=Alignment;
  Tag.Request.Flags:=Flags;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagAllocateMemory)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiGPUMemoryRelease(Handle:THandle):LongWord;
{Release GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagReleaseMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagReleaseMemory) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagReleaseMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_RELEASE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagReleaseMemory) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagReleaseMemory)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiGPUMemoryLock(Handle:THandle):LongWord;
{Lock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagLockMemory;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagLockMemory) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagLockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_LOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagLockMemory) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagLockMemory)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiGPUMemoryUnlock(Handle:THandle):LongWord;
{Unlock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagUnlockMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagUnlockMemory) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagUnlockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_UNLOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagUnlockMemory) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagUnlockMemory)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiGPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;
{Execute GPU Code from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagExecuteCode;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagExecuteCode) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagExecuteCode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_EXECUTE_CODE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagExecuteCode) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
  Tag.Request.R0:=R0;
  Tag.Request.R1:=R1;
  Tag.Request.R2:=R2;
  Tag.Request.R3:=R3;
  Tag.Request.R4:=R4;
  Tag.Request.R5:=R5;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagExecuteCode)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiDispmanxHandleGet(Resource:THandle):THandle;
{Get Dispmanx Memory Handle from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetDispmanxHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetDispmanxHandle) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetDispmanxHandle(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_DISPMANX_HANDLE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetDispmanxHandle) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Resource:=Resource;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetDispmanxHandle)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiEDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Get EDID Block from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetEDIDBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < 128 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetEDIDBlock) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetEDIDBlock(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_EDID_BLOCK;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetEDIDBlock) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Block:=Block;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetEDIDBlock)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
{Allocate Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagAllocateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagAllocateBuffer) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagAllocateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_ALLOCATE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagAllocateBuffer) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Alignment:=Alignment;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagAllocateBuffer)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferRelease:LongWord;
{Release Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagReleaseBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagReleaseBuffer) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_RELEASE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagReleaseBuffer) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagReleaseBuffer)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetState(State:LongWord):LongWord;
{Set Framebuffer State (Blank Screen) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagBlankScreen;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagBlankScreen) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagBlankScreen(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_BLANK_SCREEN;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagBlankScreen) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.State:=0;
  if State = 0 then Tag.Request.State:=BCM2835_MBOX_BLANK_SCREEN_REQ_ON;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagBlankScreen)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord;
{Get Framebuffer Dimensions from the Mailbox property tags channel}
begin
 {}
 {Get Physical}
 Result:=RPiFramebufferGetPhysical(Width,Height);
 if Result = ERROR_SUCCESS then
  begin
   {Get Overscan}
   Result:=RPiFramebufferGetOverscan(Top,Bottom,Left,Right);
  end;
end;

{==============================================================================}

function RPiFramebufferGetPhysical(var Width,Height:LongWord):LongWord;
{Get Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetPhysical) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetPhysical) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetPhysical)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetPhysical(var Width,Height:LongWord):LongWord;
{Set Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetPhysical) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetPhysical) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetPhysical)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestPhysical(var Width,Height:LongWord):LongWord;
{Test Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestPhysical) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestPhysical) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestPhysical)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetVirtual(var Width,Height:LongWord):LongWord;
{Get Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetVirtual) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetVirtual) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetVirtual)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetVirtual(var Width,Height:LongWord):LongWord;
{Set Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetVirtual) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetVirtual) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetVirtual)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestVirtual(var Width,Height:LongWord):LongWord;
{Test Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestVirtual) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestVirtual) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestVirtual)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetDepth(var Depth:LongWord):LongWord;
{Get Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetDepth) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetDepth) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetDepth)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetDepth(var Depth:LongWord):LongWord;
{Set Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetDepth) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetDepth) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetDepth)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestDepth(var Depth:LongWord):LongWord;
{Test Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestDepth) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestDepth) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestDepth)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetPixelOrder(var Order:LongWord):LongWord;
{Get Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetPixelOrder) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetPixelOrder) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetPixelOrder)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetPixelOrder(var Order:LongWord):LongWord;
{Set Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetPixelOrder) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetPixelOrder) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetPixelOrder)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestPixelOrder(var Order:LongWord):LongWord;
{Test Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestPixelOrder) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestPixelOrder) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestPixelOrder)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetAlphaMode(var Mode:LongWord):LongWord;
{Get Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetAlphaMode) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetAlphaMode) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetAlphaMode)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetAlphaMode(var Mode:LongWord):LongWord;
{Set Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetAlphaMode) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetAlphaMode) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetAlphaMode)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestAlphaMode(var Mode:LongWord):LongWord;
{Test Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestAlphaMode) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestAlphaMode) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestAlphaMode)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetPitch:LongWord;
{Get Framebuffer Pitch (Bytes per line) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetPitch;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetPitch) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetPitch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_PITCH;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetPitch) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetPitch)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiFramebufferGetOffset(var X,Y:LongWord):LongWord;
{Get Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetVirtualOffset) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetVirtualOffset) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetVirtualOffset)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetOffset(var X,Y:LongWord):LongWord;
{Set Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetVirtualOffset) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetVirtualOffset) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetVirtualOffset)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestOffset(var X,Y:LongWord):LongWord;
{Test Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestVirtualOffset) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestVirtualOffset) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestVirtualOffset)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Get Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetOverscan) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetOverscan) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetOverscan)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Set Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetOverscan) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetOverscan) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetOverscan)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Test Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestOverscan) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestOverscan) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestOverscan)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
{Get Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetPalette;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < 1024 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetPalette) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetPalette) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetPalette)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Set Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetPalette;
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
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetPalette) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetPalette) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetPalette)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Test Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestPalette;
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
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestPalette) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestPalette) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestPalette)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetLayer(var Layer:LongInt):LongWord;
{Get Framebuffer Layer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetLayer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetLayer) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetLayer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_LAYER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetLayer) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetLayer)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetLayer(var Layer:LongInt):LongWord;
{Set Framebuffer Layer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetLayer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetLayer) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetLayer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_LAYER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetLayer) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Layer:=Layer;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetLayer)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestLayer(var Layer:LongInt):LongWord;
{Test Framebuffer Layer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestLayer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestLayer) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestLayer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_LAYER;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestLayer) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Layer:=Layer;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestLayer)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferTestVsync:LongWord;
{Test Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagTestVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagTestVsync) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagTestVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_TEST_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagTestVsync) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagTestVsync)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetVsync:LongWord;
{Set Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetVsync) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetVsync) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetVsync)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferSetBacklight(Brightness:LongWord):LongWord;
{Set Framebuffer Backlight Brightness from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetBacklight;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetBacklight) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetBacklight(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_BACKLIGHT;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetBacklight) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Brightness:=Brightness;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetBacklight)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetNumDisplays(var NumDisplays:LongWord):LongWord;
{Get the number of displays from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetNumDisplays;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 NumDisplays:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetNumDisplays) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetNumDisplays(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_NUM_DISPLAYS;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetNumDisplays) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetNumDisplays)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetDisplayId(DisplayNum:LongWord):LongWord;
{Get the display id for the specified display number from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetDisplayId;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetDisplayId) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetDisplayId(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_DISPLAY_ID;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetDisplayId) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetDisplayId)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPiFramebufferSetDisplayNum(DisplayNum:LongWord):LongWord;
{Get the display number that all framebuffer requests will refer to using the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetDisplayNum;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetDisplayNum) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetDisplayNum(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_DISPLAY_NUM;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetDisplayNum) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetDisplayNum)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferGetDisplaySettings(DisplayNum:LongWord;var DisplaySettings:TDisplaySettings):LongWord;
{Get the display settings for the specified display number from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetDisplaySettings;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 FillChar(DisplaySettings,SizeOf(TDisplaySettings),0);

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetDisplaySettings) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetDisplaySettings(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_DISPLAY_SETTINGS;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetDisplaySettings) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DisplayNum:=DisplayNum;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetDisplaySettings)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiFramebufferDisplayIdToName(DisplayId:LongWord):String;
{Get the name for the specified display id}
begin
 {}
 case DisplayId of
  BCM2835_MBOX_DISPLAY_ID_MAIN_LCD:Result:='Main LCD';
  BCM2835_MBOX_DISPLAY_ID_AUX_LCD:Result:='Aux LCD';
  BCM2835_MBOX_DISPLAY_ID_HDMI0:Result:='HDMI0';
  BCM2835_MBOX_DISPLAY_ID_SDTV:Result:='SDTV';
  BCM2835_MBOX_DISPLAY_ID_FORCE_LCD:Result:='Force LCD';
  BCM2835_MBOX_DISPLAY_ID_FORCE_TV:Result:='Force TV';
  BCM2835_MBOX_DISPLAY_ID_FORCE_OTHER:Result:='Force Other';
  BCM2835_MBOX_DISPLAY_ID_HDMI1:Result:='HDMI1';
  BCM2835_MBOX_DISPLAY_ID_FORCE_TV2:Result:='Force TV2';
 else
  begin
   Result:='Unknown';
  end;
 end;
end;

{==============================================================================}

function RPiTouchGetBuffer(var Address:PtrUInt):LongWord;
{Get the Touchscreen buffer from the Mailbox property tags channel}

{Note: On current firmware versions calling TouchGetBuffer will allocate a buffer
       from GPU memory and render subsequent calls to TouchSetBuffer ineffective.

       After an initial call to TouchSetBuffer calls to TouchGetBuffer will always
       return the CPU allocated buffer}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetTouch) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetTouch) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetTouch)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiTouchSetBuffer(Address:PtrUInt):LongWord;
{Set the Touchscreen buffer in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetTouch) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetTouch) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetTouch)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiCursorSetDefault:LongWord;
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

 {Allocate the Cursor (Shared)}
 Cursor:=AllocSharedMem(Size);
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
   Result:=RPiCursorSetInfo(CURSOR_ARROW_DEFAULT_WIDTH,CURSOR_ARROW_DEFAULT_HEIGHT,0,0,Pointer(Address),Size);

   {Free the Cursor}
   FreeMem(Cursor);
  end;
end;

{==============================================================================}

function RPiCursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
{Set Cursor Info (Pixels) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetCursorInfo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Pixels}
 if Pixels = nil then Exit;

 {Check Length}
 if Length < 1 then Exit;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetCursorInfo) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetCursorInfo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_CURSOR_INFO;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetCursorInfo) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  Tag.Request.Pixels:=Pixels;
  Tag.Request.HotspotX:=HotspotX;
  Tag.Request.HotspotY:=HotspotY;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetCursorInfo)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiCursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;
{Set Curson State (Enable, X, Y) from the Mailbox property tags channel}
{Relative: X, Y is relative to Display (Virtual) not Framebuffer (Physical)}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagSetCursorState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagSetCursorState) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagSetCursorState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_SET_CURSOR_STATE;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagSetCursorState) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Enable:=BCM2835_MBOX_CURSOR_INVISIBLE;
  if Enabled then Tag.Request.Enable:=BCM2835_MBOX_CURSOR_VISIBLE;
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
  Tag.Request.Flags:=BCM2835_MBOX_CURSOR_STATE_FRAMEBUFFER_COORDS;
  if Relative then Tag.Request.Flags:=BCM2835_MBOX_CURSOR_STATE_DISPLAY_COORDS;

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagSetCursorState)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPiDMAGetChannels:LongWord;
{Get the available DMA Channels from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagGetDMAChannels;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagGetDMAChannels) + SizeOf(TBCM2835MailboxFooter);

 {Allocate Mailbox Buffer}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2835_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PBCM2835MailboxTagGetDMAChannels(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
  Tag.Header.Tag:=BCM2835_MBOX_TAG_GET_DMA_CHANNELS;
  Tag.Header.Size:=SizeOf(TBCM2835MailboxTagGetDMAChannels) - SizeOf(TBCM2835MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);

  {Setup Footer}
  Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagGetDMAChannels)));
  Footer.Tag:=BCM2835_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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
{==============================================================================}
{RPi Thread Functions}
procedure RPiSchedulerInit;
begin
 {}
 {Request the Scheduler IRQ/FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   RequestExFIQ(RPI_CPU_BOOT,BCM2835_IRQ_SYSTEM_TIMER_1,nil,RPiSchedulerInterrupt,nil);
  end
 else
  begin
   RequestExIRQ(RPI_CPU_BOOT,BCM2835_IRQ_SYSTEM_TIMER_1,nil,RPiSchedulerInterrupt,nil);
  end;

 {Register the Scheduler SWI}
 RegisterSystemCall(SYSTEM_CALL_CONTEXT_SWITCH,RPiSchedulerSystemCall);

 {Setup the first Clock Interrupt}
 RPiSchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[SCHEDULER_CPU_BOOT]);
end;

{==============================================================================}
{==============================================================================}
{RPi SWI Functions}
function RPiDispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle;
{Process an SWI request}
{Called by ARMv6SoftwareInterruptHandler in PlatformARMv6}
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
{RPi Clock Functions}
procedure RPiClockInterrupt(Parameter:Pointer);
{Interrupt handler function for the clock interrupt. This schedules another clock
 interrupt to occur CLOCK_CYCLES_PER_TICK in the future, then updates ClockTicks
 and ClockSeconds}
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
 RPiClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);

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

procedure RPiClockUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a clock interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the timer interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by CLOCK_FREQUENCY}
var
 Current:LongWord;
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear the Interrupt (C3)}
 TimerRegisters.CS:=BCM2835_SYSTEM_TIMER_CS_3;

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
end;

{==============================================================================}
{==============================================================================}
{RPi Scheduler Functions}
function RPiSchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
{Interrupt handler function for the scheduler interrupt. This schedules another
 scheduler interrupt to occur SCHEDULER_CLOCKS_PER_INTERRUPT in the future, then
 checks for threads to wakeup and the next thread to schedule}
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
 RPiSchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);

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

procedure RPiSchedulerUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a scheduler interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the scheduler interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by CLOCK_FREQUENCY}
var
 Current:LongWord;
 {$IFDEF SCHEDULER_DEBUG}
 CurrentCPU:LongWord;
 {$ENDIF}
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear the Interrupt (C1)}
 TimerRegisters.CS:=BCM2835_SYSTEM_TIMER_CS_1;

 {Get CLO Register}
 Current:=TimerRegisters.CLO;
 if Last = 0 then Last:=Current;
 if Current < Last then
  begin
   {Rollover}
   {Set Last}
   Last:=Current + Cycles;

   {$IFDEF SCHEDULER_DEBUG}
   CurrentCPU:=CPUGetCurrent;
   Inc(SchedulerInterruptRollover[CurrentCPU]);
   {$ENDIF}

   {Set C1 Register}
   TimerRegisters.C1:=Last;
  end
 else
  begin
   {Normal}
   {Increment Last}
   Inc(Last,Cycles);

   {$IFDEF SCHEDULER_DEBUG}
   CurrentCPU:=CPUGetCurrent;
   if Last >= Current then SchedulerInterruptOffset[CurrentCPU]:=Last - Current else SchedulerInterruptOffset[CurrentCPU]:=Cycles;
   if SchedulerInterruptMinOffset[CurrentCPU] = 0 then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
   if SchedulerInterruptOffset[CurrentCPU] < SchedulerInterruptMinOffset[CurrentCPU] then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
   if SchedulerInterruptOffset[CurrentCPU] > SchedulerInterruptMaxOffset[CurrentCPU] then SchedulerInterruptMaxOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
   {$ENDIF}

   {Check Last}
   if Last < (Current + SCHEDULER_CLOCKS_TOLERANCE) then Last:=Current + Cycles;

   {Set C1 Register}
   TimerRegisters.C1:=Last;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}

procedure RPiSchedulerSystemCall(Request:PSystemCallRequest);
{System Call handler for the scheduler. This is registered to receive requests for
 the SYSTEM_CALL_CONTEXT_SWITCH and will perform a context switch from within an SWI}
begin
 {}
 ARMv6ContextSwitchSWI(Pointer(Request.Param1),Pointer(Request.Param2),Request.Param3);
end;

{==============================================================================}
{==============================================================================}
{RPi Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPiFramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using the Mailbox Property Tags}
var
 Size:LongWord;
 Count:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Defaults:TFramebufferProperties;
 Palette:array[0..255] of LongWord;
 Tag:PBCM2835MailboxTagCreateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then
  begin
   {Set Current Display}
   if PRPiFramebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPiFramebuffer(Framebuffer).DisplayNum);
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
      Result:=RPiFramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
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
    Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagCreateBuffer) + SizeOf(TBCM2835MailboxFooter);

    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);

     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2835_MBOX_REQUEST_CODE;

     {Setup Tag}
     Tag:=PBCM2835MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));

     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2835_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2835MailboxTagSetPhysical) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;

     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2835_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2835MailboxTagSetVirtual) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2835_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2835MailboxTagSetDepth) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;

     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2835_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2835MailboxTagSetPixelOrder) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;

     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2835_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2835MailboxTagSetAlphaMode) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;

     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2835_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2835MailboxTagSetVirtualOffset) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;

     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2835_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2835MailboxTagSetOverscan) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;

     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2835_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2835MailboxTagAllocateBuffer) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2708FRAMEBUFFER_ALIGNMENT;

     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2835_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2835MailboxTagGetPitch) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);

     {Setup Footer}
     Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2835_MBOX_TAG_END;

     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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
    if PRPiFramebuffer(Framebuffer).MultiDisplay then
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

function RPiFramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using a simple Mailbox Call}
var
 Response:LongWord;
 Defaults:TFramebufferProperties;
 MailboxFramebuffer:PBCM2835MailboxFramebuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then
  begin
   {Set Current Display}
   if PRPiFramebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPiFramebuffer(Framebuffer).DisplayNum);
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
      Result:=RPiFramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
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
    MailboxFramebuffer:=GetSharedAlignedMem(SizeOf(TBCM2835MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
    if MailboxFramebuffer = nil then MailboxFramebuffer:=GetAlignedMem(SizeOf(TBCM2835MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
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
     Result:=MailboxCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_FRAMEBUFFER,PhysicalToBusAddress(MailboxFramebuffer),Response);
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
     RPiFramebufferGetPixelOrder(Framebuffer.Order);
     RPiFramebufferGetAlphaMode(Framebuffer.Mode);
     RPiFramebufferGetOverscan(Framebuffer.OverscanTop,Framebuffer.OverscanBottom,Framebuffer.OverscanLeft,Framebuffer.OverscanRight);

     {Update Statistics}
     Inc(Framebuffer.AllocateCount);

     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(MailboxFramebuffer);
    end;
   finally
    {Set Default Display}
    if PRPiFramebuffer(Framebuffer).MultiDisplay then
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

function RPiFramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then
  begin
   {Set Current Display}
   if PRPiFramebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PRPiFramebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Release Framebuffer}
    Result:=RPiFramebufferRelease;
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Framebuffer.ReleaseCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    {Set Default Display}
    if PRPiFramebuffer(Framebuffer).MultiDisplay then
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

function RPiFramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set Current Display}
 if PRPiFramebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PRPiFramebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Check Blank}
  if Blank then
   begin
    Result:=RPiFramebufferSetState(0);
   end
  else
   begin
    Result:=RPiFramebufferSetState(1);
   end;
 finally
  {Set Default Display}
  if PRPiFramebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end;
end;

{==============================================================================}

function RPiFramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Flags}
 if (not(BCM2708DMA_CACHE_COHERENT) or ((Flags and FRAMEBUFFER_TRANSFER_DMA) = 0)) and BCM2708FRAMEBUFFER_CACHED then
  begin
   {Clean Cache}
   CleanAndInvalidateDataCacheRange(Address,Size);
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RPiFramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set Current Display}
 if PRPiFramebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PRPiFramebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Set Backlight}
  Result:=FramebufferSetBacklight(Brightness);
 finally
  {Set Default Display}
  if PRPiFramebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end;
end;
{$ENDIF}
{==============================================================================}
{==============================================================================}
{RPi Helper Functions}
procedure RPiWait; assembler; nostackframe;
asm
 //Wait for a period of time in a loop
 mov r0,#0x8F00000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;

{==============================================================================}

procedure RPiLongWait; assembler; nostackframe;
asm
 //Wait for a long period of time in a loop
 ldr r0,=0x2FF00000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;

{==============================================================================}

procedure RPiShortWait; assembler; nostackframe;
asm
 //Wait for a short period of time in a loop
 mov r0,#0x1F0000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;

{==============================================================================}

procedure RPiSlowBlink; assembler; nostackframe;
asm
 //Slow blink the Activity LED in a loop
 bl RPiActivityLEDEnable
.LLoop:
 bl RPiActivityLEDOn
 bl RPiWait
 bl RPiActivityLEDOff
 bl RPiWait
 b .LLoop
end;

{==============================================================================}

procedure RPiFastBlink; assembler; nostackframe;
asm
 //Fast blink the Activity LED in a loop
 bl RPiActivityLEDEnable
.LLoop:
 bl RPiActivityLEDOn
 bl RPiShortWait
 bl RPiActivityLEDOff
 bl RPiShortWait
 b .LLoop
end;

{==============================================================================}

procedure RPiBootBlink; assembler; nostackframe;
{Blink the Activity LED without dependency on any other RTL setup}
{Note: This currently only works for RPiA+/B+}
asm
 //Blink the Activity LED in a loop
 //Enable the Activity LED
 ldr r0,=BCM2835_GPIO_REGS_BASE

 //Set the 21st bit of r1
 mov r1,#1
 lsl r1,#RPI_GPIO_ACTLED_GPFSHIFT

 //Set the GPIO function select
 str r1,[r0,#RPI_GPIO_ACTLED_GPFSEL]

.LLoop:
 //Turn on the Activity LED
 ldr r0,=BCM2835_GPIO_REGS_BASE

 //Set the 15th bit of r1
 mov r1,#1
 lsl r1,#RPI_GPIO_ACTLED_GPSHIFT

 //Set GPIO 47 to high, causing the LED to turn on
 str r1,[r0,#RPI_GPIO_ACTLED_GPSET]

 //Wait
 //--bl RPiShortWait
 bl RPiWait
 //--bl RPiLongWait

 //Turn off the Activity LED
 ldr r0,=BCM2835_GPIO_REGS_BASE

 //Set the 15th bit of r1
 mov r1,#1
 lsl r1,#RPI_GPIO_ACTLED_GPSHIFT

 //Set GPIO 47 to low, causing the LED to turn off
 str r1,[r0,#RPI_GPIO_ACTLED_GPCLR]

 //Wait
 //--bl RPiShortWait
 bl RPiWait
 //--bl RPiLongWait
 b .LLoop
end;

{==============================================================================}

procedure RPiBootOutput(Value:LongWord);
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

   PLongWord(BCM2835_PL011_REGS_BASE)^:=Character;

   if Bits = 0 then Break;
  end;

 {Line End}
 PLongWord(BCM2835_PL011_REGS_BASE)^:=$0D;
 PLongWord(BCM2835_PL011_REGS_BASE)^:=$0A;
end;

{==============================================================================}
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPiBootConsoleStart;
begin
 ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
end;

{==============================================================================}

procedure RPiBootConsoleWrite(const Value:String);
begin
 ConsoleWindowWriteLn(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),Value);
end;

{==============================================================================}

procedure RPiBootConsoleWriteEx(const Value:String;X,Y:LongWord);
begin
 ConsoleWindowSetXY(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),X,Y);
 ConsoleWindowWriteLn(ConsoleWindowGetDefault(ConsoleDeviceGetDefault),Value);
end;

{==============================================================================}

function RPiBootConsoleGetX:LongWord;
begin
 Result:=ConsoleWindowGetX(ConsoleWindowGetDefault(ConsoleDeviceGetDefault));
end;

{==============================================================================}

function RPiBootConsoleGetY:LongWord;
begin
 Result:=ConsoleWindowGetY(ConsoleWindowGetDefault(ConsoleDeviceGetDefault));
end;
{$ENDIF}
{==============================================================================}

function RPiConvertPowerIdRequest(PowerId:LongWord):LongWord;
{Convert Ultibo Power Id to BCM2835 Power Id}
begin
 {}
 Result:=BCM2835_MBOX_POWER_DEVID_UNKNOWN;

 case PowerId of
  POWER_ID_MMC0:Result:=BCM2835_MBOX_POWER_DEVID_SDHCI;
  POWER_ID_UART0:Result:=BCM2835_MBOX_POWER_DEVID_UART0;
  POWER_ID_UART1:Result:=BCM2835_MBOX_POWER_DEVID_UART1;
  POWER_ID_USB0:Result:=BCM2835_MBOX_POWER_DEVID_USB_HCD;
  POWER_ID_I2C0:Result:=BCM2835_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C1:Result:=BCM2835_MBOX_POWER_DEVID_I2C1;
  POWER_ID_I2C2:Result:=BCM2835_MBOX_POWER_DEVID_I2C2;
  POWER_ID_SPI0:Result:=BCM2835_MBOX_POWER_DEVID_SPI;
  POWER_ID_CCP2TX:Result:=BCM2835_MBOX_POWER_DEVID_CCP2TX;
 end;
end;

{==============================================================================}

function RPiConvertPowerStateRequest(PowerState:LongWord):LongWord;
{Convert Ultibo Power State to BCM2835 Power State}
begin
 {}
 Result:=BCM2835_MBOX_SET_POWER_STATE_REQ_OFF;

 case PowerState of
  POWER_STATE_OFF:Result:=BCM2835_MBOX_SET_POWER_STATE_REQ_OFF;
  POWER_STATE_ON:Result:=BCM2835_MBOX_SET_POWER_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPiConvertPowerStateResponse(PowerState:LongWord):LongWord;
{Convert BCM2835 Power State to Ultibo Power State}
begin
 {}
 Result:=POWER_STATE_OFF;

 case PowerState of
  BCM2835_MBOX_POWER_STATE_RESP_OFF:Result:=POWER_STATE_OFF;
  BCM2835_MBOX_POWER_STATE_RESP_ON:Result:=POWER_STATE_ON;
 end;
end;

{==============================================================================}

function RPiConvertClockIdRequest(ClockId:LongWord):LongWord;
{Convert Ultibo Clock Id to BCM2835 Clock Id}
begin
 {}
 Result:=BCM2835_MBOX_CLOCK_ID_UNKNOWN;

 case ClockId of
  CLOCK_ID_MMC0:Result:=BCM2835_MBOX_CLOCK_ID_EMMC;
  CLOCK_ID_MMC1:Result:=BCM2835_MBOX_CLOCK_ID_CORE; {MMC1 runs from core clock}
  CLOCK_ID_UART0:Result:=BCM2835_MBOX_CLOCK_ID_UART;
  CLOCK_ID_UART1:Result:=BCM2835_MBOX_CLOCK_ID_CORE; {UART1 runs from core clock}
  CLOCK_ID_CPU:Result:=BCM2835_MBOX_CLOCK_ID_ARM;
  CLOCK_ID_CORE:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_GPU:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_V3D:Result:=BCM2835_MBOX_CLOCK_ID_V3D;
  CLOCK_ID_H264:Result:=BCM2835_MBOX_CLOCK_ID_H264;
  CLOCK_ID_ISP:Result:=BCM2835_MBOX_CLOCK_ID_ISP;
  CLOCK_ID_SDRAM:Result:=BCM2835_MBOX_CLOCK_ID_SDRAM;
  CLOCK_ID_PIXEL:Result:=BCM2835_MBOX_CLOCK_ID_PIXEL;
  CLOCK_ID_PWM0:Result:=BCM2835_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_PWM1:Result:=BCM2835_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_I2C0:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C1:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C2:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI0:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI1:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI2:Result:=BCM2835_MBOX_CLOCK_ID_CORE;
 end;
end;

{==============================================================================}

function RPiConvertClockStateRequest(ClockState:LongWord):LongWord;
{Convert Ultibo Clock State to BCM2835 Clock State}
begin
 {}
 Result:=BCM2835_MBOX_SET_CLOCK_STATE_REQ_OFF;

 case ClockState of
  CLOCK_STATE_OFF:Result:=BCM2835_MBOX_SET_CLOCK_STATE_REQ_OFF;
  CLOCK_STATE_ON:Result:=BCM2835_MBOX_SET_CLOCK_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPiConvertClockStateResponse(ClockState:LongWord):LongWord;
{Convert BCM2835 Clock State to Ultibo Clock State}
begin
 {}
 Result:=CLOCK_STATE_OFF;

 case ClockState of
  BCM2835_MBOX_CLOCK_STATE_RESP_OFF:Result:=CLOCK_STATE_OFF;
  BCM2835_MBOX_CLOCK_STATE_RESP_ON:Result:=CLOCK_STATE_ON;
 end;
end;

{==============================================================================}

function RPiConvertVoltageIdRequest(VoltageId:LongWord):LongWord;
{Convert Ultibo Voltage Id to BCM2835 Voltage Id}
begin
 {}
 Result:=BCM2835_MBOX_VOLTAGE_ID_RESERVED;

 case VoltageId of
  VOLTAGE_ID_CORE:Result:=BCM2835_MBOX_VOLTAGE_ID_CORE;
  VOLTAGE_ID_SDRAM_C:Result:=BCM2835_MBOX_VOLTAGE_ID_SDRAM_C;
  VOLTAGE_ID_SDRAM_P:Result:=BCM2835_MBOX_VOLTAGE_ID_SDRAM_P;
  VOLTAGE_ID_SDRAM_I:Result:=BCM2835_MBOX_VOLTAGE_ID_SDRAM_I;
 end;
end;

{==============================================================================}

function RPiConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;
{Convert Ultibo Temperature Id to BCM2835 Temperature Id}
begin
 {}
 Result:=BCM2835_MBOX_TEMP_ID_SOC;

 case TemperatureId of
  TEMPERATURE_ID_SOC:Result:=BCM2835_MBOX_TEMP_ID_SOC;
 end;
end;


{==============================================================================}
{==============================================================================}
{RPi Internal Functions}
function RPiInterruptIsValid(Number:LongWord):Boolean;
begin
 {}
 {Check Number}
 Result:=(Number < IRQ_COUNT);
end;

{==============================================================================}

function RPiInterruptIsGlobal(Number:LongWord):Boolean;
begin
 {}
 {Check Number}
 Result:=(Number < IRQ_LOCAL_START);
end;

{==============================================================================}

function RPiInterruptCheckValid(const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Count:LongWord;
begin
 {}
 Result:=False;

 {Check Flags}
 if Entry.IsLocal then
  begin
   {Local}
   Exit;
  end
 else if Entry.IsIPI then
  begin
   {Software}
   Exit;
  end
 else
  begin
   {Check Number (Global)}
   if not RPiInterruptIsGlobal(Entry.Number) then Exit;

   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;

   {Check Mask CPU (Single CPU only)}
   if (IRQ_ROUTING <> CPU_ID_ALL) and (IRQ_ROUTING <> CPUMaskToID(Entry.CPUMask)) then Exit;
  end;

 {Check Handlers}
 if not RPiInterruptCheckHandlers(Entry) then Exit;

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

function RPiInterruptCheckHandlers(const Entry:TInterruptEntry):Boolean;
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

function RPiInterruptCompareHandlers(const Entry,Current:TInterruptEntry):Boolean;
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

function RPiInterruptEnable(const Entry:TInterruptEntry):Boolean;
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
   Address:=BCM2835_ARM_INTERRUPT_IRQ_ENABLE1;
  end
 else if Entry.Number < 64 then
  begin
   Group:=1;
   Offset:=32;
   Address:=BCM2835_ARM_INTERRUPT_IRQ_ENABLE2;
  end
 else if Entry.Number < 96 then
  begin
   Group:=2;
   Offset:=64;
   Address:=BCM2835_ARM_INTERRUPT_BASIC_ENABLE;
  end
 else
  begin
   Exit;
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
     if (IRQEnabled[Group] and (1 shl (Entry.Number - Offset))) <> 0 then Exit;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}

     {Enable FIQ}
     InterruptRegisters.FIQ_control:=BCM2835_ARM_INTERRUPT_FIQ_ENABLE or (Entry.Number and BCM2835_ARM_INTERRUPT_FIQ_SOURCE);
     FIQEnabled:=Entry.Number;
    end
   else
    begin
     {Check FIQ}
     if FIQEnabled = Entry.Number then Exit; {FIQEnabled will be -1 when nothing enabled}

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}

     {Enable IRQ}
     PLongWord(BCM2835_INTERRUPT_REGS_BASE + Address)^:=(1 shl (Entry.Number - Offset));
     IRQEnabled[Group]:=IRQEnabled[Group] or (1 shl (Entry.Number - Offset));
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function RPiInterruptDisable(const Entry:TInterruptEntry):Boolean;
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
   Address:=BCM2835_ARM_INTERRUPT_IRQ_DISABLE1;
  end
 else if Entry.Number < 64 then
  begin
   Group:=1;
   Offset:=32;
   Address:=BCM2835_ARM_INTERRUPT_IRQ_DISABLE2;
  end
 else if Entry.Number < 96 then
  begin
   Group:=2;
   Offset:=64;
   Address:=BCM2835_ARM_INTERRUPT_BASIC_DISABLE;
  end
 else
  begin
   Exit;
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
     if (IRQEnabled[Group] and (1 shl (Entry.Number - Offset))) <> 0 then Exit;

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
     PLongWord(BCM2835_INTERRUPT_REGS_BASE + Address)^:=(1 shl (Entry.Number - Offset));
     IRQEnabled[Group]:=IRQEnabled[Group] and not(1 shl (Entry.Number - Offset));
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function RPiInterruptGetCurrentCount(CPUID,Number:LongWord):LongWord;
{Note: Caller must hold the interrupt lock}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=0;

 {Setup Defaults}
 Entry:=nil;

 {Check Number}
 if RPiInterruptIsGlobal(Number) then
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

function RPiInterruptGetCurrentEntry(CPUID,Number:LongWord;Index:LongWord):PInterruptEntry;
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
 if RPiInterruptIsGlobal(Number) then
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

function RPiInterruptAddCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=False;

 {Check Entry}
 if Entry = nil then Exit;

 {Check Number}
 if RPiInterruptIsGlobal(Number) then
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

function RPiInterruptDeleteCurrentEntry(CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=False;

 {Check Entry}
 if Entry = nil then Exit;

 {Check Number}
 if RPiInterruptIsGlobal(Number) then
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

function RPiInterruptFindMatchingEntry(const Entry:TInterruptEntry):PInterruptEntry;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=nil;

 {Get Current}
 Current:=RPiInterruptGetCurrentEntry(Entry.CPUID,Entry.Number,0);

 {Find Match}
 while Current <> nil do
  begin
   if RPiInterruptCompareHandlers(Entry,Current^) then
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

function RPiInterruptGetEntry(CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord;
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
    Exit;
   end
  else
   begin
    {Global Entry}
    if not RPiInterruptIsGlobal(Number) then Exit;
   end;

  Result:=ERROR_NOT_FOUND;

  {Get Current}
  Current:=RPiInterruptGetCurrentEntry(CPUID,Number,Index);
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

function RPiInterruptRegisterEntry(const Entry:TInterruptEntry):LongWord;
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
  if not RPiInterruptCheckValid(Entry) then Exit;

  Result:=ERROR_ALREADY_ASSIGNED;

  {Check Count}
  if RPiInterruptGetCurrentCount(Entry.CPUID,Entry.Number) = 0 then
   begin
    {Single Entry}
    Result:=ERROR_OPERATION_FAILED;

    {Enable IRQ/FIQ}
    if not RPiInterruptEnable(Entry) then Exit;

    {Add Entry}
    if not RPiInterruptAddCurrentEntry(Entry.CPUID,Entry.Number,@Entry) then Exit;
   end
  else
   begin
    {Shared Entry}
    Result:=ERROR_ALREADY_ASSIGNED;

    {Check Shared}
    if not Entry.IsShared then Exit;

    {Get Match}
    Current:=RPiInterruptFindMatchingEntry(Entry);
    if Current <> nil then Exit;

    {Get Current}
    Current:=RPiInterruptGetCurrentEntry(Entry.CPUID,Entry.Number,0);
    if Current = nil then Exit;

    {Check Shared}
    if not Current.IsShared then Exit;

    {Check FIQ}
    if Entry.IsFIQ <> Current.IsFIQ then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Add Entry}
    if not RPiInterruptAddCurrentEntry(Entry.CPUID,Entry.Number,@Entry) then Exit;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPiInterruptDeregisterEntry(const Entry:TInterruptEntry):LongWord;
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
  if not RPiInterruptCheckValid(Entry) then Exit;

  Result:=ERROR_NOT_ASSIGNED;

  {Get Match}
  Current:=RPiInterruptFindMatchingEntry(Entry);
  if Current = nil then Exit;

  Result:=ERROR_OPERATION_FAILED;

  {Check Count}
  if RPiInterruptGetCurrentCount(Entry.CPUID,Entry.Number) = 1 then
   begin
    {Single Entry}
    {Disable IRQ/FIQ}
    if not RPiInterruptDisable(Entry) then Exit;
   end;

  {Delete Entry}
  if not RPiInterruptDeleteCurrentEntry(Entry.CPUID,Entry.Number,Current) then Exit;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPiDispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending IRQ requests}
{Called by ARMv6IRQHandler in PlatformARMv6}
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

       {Call IRQ Handler}
       Result:=RPiHandleInterrupt(IRQBit + (Group shl 5),CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
      end;
    end;
  end;
end;

{==============================================================================}

function RPiDispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending FIQ requests}
{Called by ARMv6FIQHandler in PlatformARMv6}
{Note: A DataMemoryBarrier is executed before and after calling this function}
begin
 {}
 Result:=Thread;

 {$IF DEFINED(FIQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 Inc(DispatchFastInterruptCounter[CPUID]);
 {$ENDIF}

 {Check FIQ Enabled}
 if FIQEnabled <> LongWord(-1) then
  begin
   {Call FIQ Handler}
   Result:=RPiHandleInterrupt(FIQEnabled,CPU_ID_ALL,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
  end;
end;

{==============================================================================}

function RPiHandleInterrupt(Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for an IRQ/FIQ that was received, or halt if it doesn't exist}
var
 Status:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;

 {Get Entry}
 Entry:=RPiInterruptGetCurrentEntry(CPUID,Number,0);
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
