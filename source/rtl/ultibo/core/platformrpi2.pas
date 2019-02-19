{
Ultibo Platform interface unit for Raspberry Pi 2.

Copyright (C) 2019 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A7)

Boards
======

 Raspberry Pi 2 - Model B
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

 Cortex-A7 MPCore Technical Reference Manual (Revision: r0p5)
 
 ARM v7 Architecture Reference Manual
 
 ARM Architecture Reference Manual (ARMv7-A and ARMv7-R edition)
 
 Raspberry Pi Mailboxes
 
  https://github.com/raspberrypi/firmware/wiki/Mailboxes

 RPi Framebuffer
 
  http://elinux.org/RPi_Framebuffer
 
Platform RPi2
=============

 Notes: The RPi2 B has the Power LED connected to GPIO Pin 35 (Activity LED is now on GPIO Pin 47)
        The RPi3 B has the Activity LED connected to the GPU, access is via a Virtual GPIO (Power LED is only accessible via the GPIO expander driver)
        The RPi3 B+ has the Activity LED connected to GPIO Pin 29 (Power LED is only accessible via the GPIO expander driver)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformRPi2; 

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}
{--$DEFINE RPI2_CLOCK_SYSTEM_TIMER} {Use the System Timer for the Clock instead of the Virtual Timer}

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2836,Platform,PlatformARM,PlatformARMv7,HeapManager,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF},SysUtils;

{==============================================================================}
const
 {RPi2 specific constants}

 {ARM Physical to VC IO Mapping}
 RPI2_VCIO_ALIAS = BCM2836_VCIO_ALIAS;     {The VCIO Alias (For ARM Physcial to VC IO translation)}
 
 {ARM Physical to VC Bus Mapping}
 RPI2_VCBUS_ALIAS = BCM2836_VCBUS_C_ALIAS; {The currently selected VCBUS Alias (For ARM Physcial to VC Bus translation)}

const
 {Secure World Boot} 
 RPI2_SECURE_BOOT = $00000001;         {If 1 then startup will attempt to switch back to secure world during boot process}
 
const 
 {Address of StartupHandler on Reset}
 RPI2_STARTUP_ADDRESS = $00008000;
 
const
 {Page Table Address and Size}
 RPI2_PAGE_TABLE_BASE = $00004000;     {Place the first level Page Table after the interrupt vectors at 0x00001000 and before the code start at 0x00008000}
 RPI2_PAGE_TABLE_SIZE = SIZE_16K;      {ARM Cortex A7 first level Page Table is exactly 16KB in size (4096 32 bit (4 byte) entries)}
 
const
 {Vector Table Address and Size} 
 RPI2_VECTOR_TABLE_BASE  = $00001000;  {Place the Interrupt Vector Table at 0x00001000 before the code start at 0x00008000}
 RPI2_VECTOR_TABLE_SIZE  = SIZE_64;    {The Interrupt Vector Table is exactly 64 bytes (16 32 bit (4 byte) entries)}
 RPI2_VECTOR_TABLE_COUNT = 8;          {The Interrupt Vector Table contains 8 entries on an ARMv7 device}
 
const
 {CPU Count}
 RPI2_CPU_COUNT = BCM2836_CPU_COUNT;
 RPI2_CPU_BOOT = CPU_ID_0;
 RPI2_CPU_MASK = CPU_AFFINITY_0 or CPU_AFFINITY_1 or CPU_AFFINITY_2 or CPU_AFFINITY_3;

const
 {IRQ/FIQ Start/Routing}
 RPI2_IRQ_START = 0;                   {System wide IRQs start at zero}
 
 RPI2_IRQ_ROUTING = CPU_ID_0;          {Route system wide IRQs to CPU0}
 RPI2_FIQ_ROUTING = CPU_ID_0;          {Route system wide FIQs to CPU0}

 RPI2_IRQ_LOCAL_START = BCM2836_GPU_IRQ_COUNT + BCM2836_ARM_IRQ_COUNT; {Local IRQs start after GPU and ARM IRQs}
 
const
 {SWI}
 RPI2_SWI_COUNT = 256;                 {Number of available SWI entries}
 
const
 {Core Timer Prescaler}
 RPI2_CORE_TIMER_PRESCALER    = $06AAAAAB; {Divide the Crystal Clock by 19.2 to give a 1MHz Core Timer}
 RPI2_CORE_TIMER_FREQUENCY    = 1000000;   {The Core Timer frequency from the prescaler setting above}
 RPI2_GENERIC_TIMER_FREQUENCY = 1000000;   {The ARM Generic Timer frequency from the prescaler setting above}
 
const
 {Kernel Image Name}
 RPI2_KERNEL_NAME = 'kernel7.img';
 RPI2_KERNEL_CONFIG = 'config.txt';
 RPI2_KERNEL_COMMAND = 'cmdline.txt';
 RPI2_FIRMWARE_FILES = 'bootcode.bin,start.elf,fixup.dat';
 
const
 {GPIO Power LED constants (GPIO Pin 35)} 
 {Note: GPIO Pin 35 on the RPi2 is set on boot to Pull Up/Down Enable which must be cleared before using the pin}
 RPI2_GPIO_PWRLED_GPFSEL = BCM2836_GPFSEL3;          {GPFSEL register for PWR LED}
 RPI2_GPIO_PWRLED_GPSET = BCM2836_GPSET1;            {GPSET register for PWR LED}
 RPI2_GPIO_PWRLED_GPCLR = BCM2836_GPCLR1;            {GPCLR register for PWR LED}
 
 RPI2_GPIO_PWRLED_GPFSHIFT = 15;                     {GPFSEL register shift for PWR LED}
 RPI2_GPIO_PWRLED_GPFMASK = BCM2836_GPFSEL_MASK;     {GPFSEL register mask for PWR LED}
 //To Do RPI2_GPIO_PWRLED_GPFVALUE = ALT0/1/2/3 etc
 
 RPI2_GPIO_PWRLED_GPSHIFT = (35 - 32);               {GPSET/GPCLR register shift for PWR LED}
 RPI2_GPIO_PWRLED_GPMASK = BCM2836_GPSET_MASK;       {GPSET/GPCLR register mask for PWR LED}
 
 //GPIO Pin 35 on RPi2
 //See: http://www.raspberrypi.org/forums/viewtopic.php?t=72260
 //See also for how to control GPPUD etc: http://wiki.osdev.org/Raspberry_Pi_Bare_Bones 
 
const 
 {GPIO Activity LED constants (GPIO Pin 47)} 
 {Note: GPIO Pin 47 on the RPi2 is Pull High instead of Pull Low, to turn on the LED use RPI2_GPIO_ACTLED_GPSET instead of RPI2_GPIO_ACTLED_GPCLR}
 RPI2_GPIO_ACTLED_GPFSEL = BCM2836_GPFSEL4;          {GPFSEL register for ACT LED}
 RPI2_GPIO_ACTLED_GPSET = BCM2836_GPSET1;            {GPSET register for ACT LED}
 RPI2_GPIO_ACTLED_GPCLR = BCM2836_GPCLR1;            {GPCLR register for ACT LED}
 
 RPI2_GPIO_ACTLED_GPFSHIFT = 21;                     {GPFSEL register shift for ACT LED}
 RPI2_GPIO_ACTLED_GPFMASK = BCM2836_GPFSEL_MASK;     {GPFSEL register mask for ACT LED}
 //To Do RPI2_GPIO_ACTLED_GPFVALUE = ALT0/1/2/3 etc

 RPI2_GPIO_ACTLED_GPSHIFT = (47 - 32);               {GPSET/GPCLR register shift for ACT LED}
 RPI2_GPIO_ACTLED_GPMASK = BCM2836_GPSET_MASK;       {GPSET/GPCLR register mask for ACT LED}
 
const
 {Mailbox constants}
 RPI2_MAILBOX_TIMEOUT = 100;                         {Default timeout to wait for mailbox calls to complete (Milliseconds)}
 RPI2_MAILBOX_TIMEOUT_EX = 1000;                     {Extended timeout to wait for mailbox calls to complete (Milliseconds)}
 
const
 {Mailbox constants}
 RPI2_LOCAL_MAILBOX_TIMEOUT = 100;                   {Default timeout to wait for local mailbox calls to complete (Milliseconds)}
 
const
 {Framebuffer constants}
 RPI2_FRAMEBUFFER_DESCRIPTION = 'BCM2836 Framebuffer';
 
{==============================================================================}
{type}
 {RPi2 specific types}
 
{==============================================================================}
var
 {RPi2 specific Ultibo variables}
 RPi2Initialized:Boolean;

 RPi2CNTVOFFLow:LongWord = 0;               {The low 32 bits of the Virtual Counter Offset register at boot time (CPU0 only) (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 RPi2CNTVOFFHigh:LongWord = 0;              {The high 32 bits of the Virtual Counter Offset register at boot time (CPU0 only) (Set by Startup)} {Must be initialized to remain in .data or else rewritten to zero with .bss}
 
var
 {Timer Variables}
 TimerRegisters:PBCM2836SystemTimerRegisters;
 
var
 {Mailbox Variables}
 Mailbox0Registers:PBCM2836Mailbox0Registers;
 Mailbox1Registers:PBCM2836Mailbox1Registers;
 
var
 {Interrupt Variables}
 InterruptRegisters:PBCM2836InterruptRegisters;
 
 InterruptEntries:array[0..(BCM2836_GPU_IRQ_COUNT + BCM2836_ARM_IRQ_COUNT - 1)] of TInterruptEntry;
 LocalInterruptEntries:array[RPI2_IRQ_LOCAL_START..(BCM2836_IRQ_COUNT - 1),0..(RPI2_CPU_COUNT - 1)] of TInterruptEntry;

var
 {System Call Variables}
 SystemCallEntries:array[0..RPI2_SWI_COUNT - 1] of TSystemCallEntry;
 
var
 {IRQ/FIQ Variables}
 IRQEnabled:array[0..2] of LongWord; {3 groups of IRQs to Enable/Disable (See: TBCM2836InterruptRegisters)}
 FIQEnabled:LongWord;                {The single IRQ number to Enable as FIQ instead (See: TBCM2836InterruptRegisters)}
 
 IRQLocalEnabled:array[0..(RPI2_CPU_COUNT - 1)] of LongWord; {1 group of local IRQs to Enable/Disable per CPU (See: TBCM2836ARMLocalRegisters)}
 FIQLocalEnabled:array[0..(RPI2_CPU_COUNT - 1)] of LongWord; {1 group of local FIQs to Enable/Disable per CPU (See: TBCM2836ARMLocalRegisters)}
 
var
 {Watchdog Variables}
 WatchdogRegisters:PBCM2836PMWatchdogRegisters;
 
var
 {ARM Local Variables}
 ARMLocalRegisters:PBCM2836ARMLocalRegisters;
 
var
 {Virtual GPIO Variables}
 VirtualGPIOBuffer:TBCM2837VirtualGPIOBuffer;
 
{==============================================================================}
{Initialization Functions}
procedure RPi2Init;

procedure RPi2SecondarySwitch;
procedure RPi2SecondarySecure;
procedure RPi2SecondaryHandler;

{==============================================================================}
{RPi2 Platform Functions}
procedure RPi2SMPInit;
procedure RPi2BoardInit;
procedure RPi2MemoryInit;
procedure RPi2ClockInit;
procedure RPi2PowerInit;
procedure RPi2MailboxInit;
procedure RPi2InterruptInit;
procedure RPi2PeripheralInit;
{$IFDEF CONSOLE_EARLY_INIT}
procedure RPi2FramebufferInit;
{$ENDIF}
procedure RPi2PageTableInit;

procedure RPi2PowerLEDEnable;
procedure RPi2PowerLEDOn;
procedure RPi2PowerLEDOff;

procedure RPi2ActivityLEDEnable;
procedure RPi2ActivityLEDOn;
procedure RPi2ActivityLEDOff;

function RPi2MailboxReceive(Mailbox,Channel:LongWord):LongWord;
procedure RPi2MailboxSend(Mailbox,Channel,Data:LongWord);

function RPi2MailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
function RPi2MailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord;
function RPi2MailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
function RPi2MailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord;

function RPi2RequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
function RPi2ReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;

function RPi2RequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
function RPi2ReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 

function RPi2RegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
function RPi2DeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;

function RPi2GetInterruptEntry(Number:LongWord):TInterruptEntry; 
function RPi2GetLocalInterruptEntry(CPUID,Number:LongWord):TInterruptEntry; 
function RPi2GetSystemCallEntry(Number:LongWord):TSystemCallEntry; 

function RPi2SystemRestart(Delay:LongWord):LongWord; 
function RPi2SystemShutdown(Delay:LongWord):LongWord;
function RPi2SystemGetCommandLine:String;

function RPi2CPUGetMemory(var Address:PtrUInt;var Length:LongWord):LongWord; 

function RPi2GPUGetState:LongWord;
function RPi2GPUGetMemory(var Address:PtrUInt;var Length:LongWord):LongWord; 

function RPi2BoardGetModel:LongWord;
function RPi2BoardGetSerial:Int64;
function RPi2BoardGetRevision:LongWord;
function RPi2BoardGetMACAddress:String;

function RPi2FirmwareGetRevision:LongWord;
function RPi2FirmwareGetThrottled:LongWord;

function RPi2PowerGetWait(PowerId:LongWord):LongWord;
function RPi2PowerGetState(PowerId:LongWord):LongWord;
function RPi2PowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;

function RPi2ClockGetCount:LongWord;
function RPi2ClockGetTotal:Int64; 

function RPi2ClockGetRate(ClockId:LongWord):LongWord;
function RPi2ClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;

function RPi2ClockGetState(ClockId:LongWord):LongWord;
function RPi2ClockSetState(ClockId,State:LongWord):LongWord;

function RPi2ClockGetMinRate(ClockId:LongWord):LongWord;
function RPi2ClockGetMaxRate(ClockId:LongWord):LongWord;

function RPi2TurboGetState(TurboId:LongWord):LongWord;
function RPi2TurboSetState(TurboId,State:LongWord):LongWord;

function RPi2VoltageGetValue(VoltageId:LongWord):LongWord;
function RPi2VoltageSetValue(VoltageId,Value:LongWord):LongWord;

function RPi2VoltageGetMinValue(VoltageId:LongWord):LongWord;
function RPi2VoltageGetMaxValue(VoltageId:LongWord):LongWord;

function RPi2TemperatureGetCurrent(TemperatureId:LongWord):LongWord;
function RPi2TemperatureGetMaximum(TemperatureId:LongWord):LongWord;

function RPi2GPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
function RPi2GPUMemoryRelease(Handle:THandle):LongWord;
function RPi2GPUMemoryLock(Handle:THandle):LongWord;
function RPi2GPUMemoryUnlock(Handle:THandle):LongWord;

function RPi2GPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;

function RPi2DispmanxHandleGet(Resource:THandle):THandle;
function RPi2EDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPi2FramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
function RPi2FramebufferRelease:LongWord;
function RPi2FramebufferSetState(State:LongWord):LongWord;

function RPi2FramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord; 

function RPi2FramebufferGetPhysical(var Width,Height:LongWord):LongWord;
function RPi2FramebufferSetPhysical(var Width,Height:LongWord):LongWord;
function RPi2FramebufferTestPhysical(var Width,Height:LongWord):LongWord;

function RPi2FramebufferGetVirtual(var Width,Height:LongWord):LongWord;
function RPi2FramebufferSetVirtual(var Width,Height:LongWord):LongWord;
function RPi2FramebufferTestVirtual(var Width,Height:LongWord):LongWord;

function RPi2FramebufferGetDepth(var Depth:LongWord):LongWord;
function RPi2FramebufferSetDepth(var Depth:LongWord):LongWord;
function RPi2FramebufferTestDepth(var Depth:LongWord):LongWord;

function RPi2FramebufferGetPixelOrder(var Order:LongWord):LongWord;
function RPi2FramebufferSetPixelOrder(var Order:LongWord):LongWord;
function RPi2FramebufferTestPixelOrder(var Order:LongWord):LongWord;

function RPi2FramebufferGetAlphaMode(var Mode:LongWord):LongWord;
function RPi2FramebufferSetAlphaMode(var Mode:LongWord):LongWord;
function RPi2FramebufferTestAlphaMode(var Mode:LongWord):LongWord;

function RPi2FramebufferGetPitch:LongWord;

function RPi2FramebufferGetOffset(var X,Y:LongWord):LongWord;
function RPi2FramebufferSetOffset(var X,Y:LongWord):LongWord;
function RPi2FramebufferTestOffset(var X,Y:LongWord):LongWord;

function RPi2FramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPi2FramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
function RPi2FramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;

function RPi2FramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
function RPi2FramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
function RPi2FramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;

function RPi2FramebufferTestVsync:LongWord;
function RPi2FramebufferSetVsync:LongWord;

function RPi2FramebufferSetBacklight(Brightness:LongWord):LongWord;

function RPi2TouchGetBuffer(var Address:LongWord):LongWord;
function RPi2TouchSetBuffer(Address:PtrUInt):LongWord;

function RPi2VirtualGPIOGetBuffer(var Address:LongWord):LongWord;
function RPi2VirtualGPIOSetBuffer(Address:PtrUInt):LongWord;

function RPi2CursorSetDefault:LongWord;
function RPi2CursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
function RPi2CursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;

function RPi2DMAGetChannels:LongWord;

function RPi2VirtualGPIOInputGet(Pin:LongWord):LongWord; 
function RPi2VirtualGPIOOutputSet(Pin,Level:LongWord):LongWord; 
function RPi2VirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord; 

{==============================================================================}
{RPi2 Thread Functions}
procedure RPi2SchedulerInit;
procedure RPi2SchedulerStart(CPUID:LongWord);

procedure RPi2SecondaryBoot(CPUID:LongWord);

{==============================================================================}
{RPi2 IRQ Functions}
function RPi2DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

function RPi2HandleIRQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

{==============================================================================}
{RPi2 FIQ Functions}
function RPi2DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

function RPi2HandleFIQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

{==============================================================================}
{RPi2 SWI Functions}
function RPi2DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 

{==============================================================================}
{RPi2 Clock Functions}
procedure RPi2ClockInterrupt(Parameter:Pointer);
procedure RPi2ClockUpdate(Cycles:LongWord;var Last:LongWord);

{==============================================================================}
{RPi2 Scheduler Functions}
function RPi2SchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
procedure RPi2SchedulerUpdate(Cycles:LongWord;var Last:LongWord);

procedure RPi2SchedulerSystemCall(Request:PSystemCallRequest);

{==============================================================================}
{RPi2 Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPi2FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPi2FramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPi2FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;

function RPi2FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function RPi2FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;

function RPi2FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;

function RPi2FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{$ENDIF}
{==============================================================================}
{RPi2 Helper Functions}
procedure RPi2Wait;
procedure RPi2LongWait;
procedure RPi2ShortWait;

procedure RPi2SlowBlink;
procedure RPi2FastBlink;

procedure RPi2BootBlink;

function RPi2ConvertPowerIdRequest(PowerId:LongWord):LongWord;
function RPi2ConvertPowerStateRequest(PowerState:LongWord):LongWord;
function RPi2ConvertPowerStateResponse(PowerState:LongWord):LongWord;

function RPi2ConvertClockIdRequest(ClockId:LongWord):LongWord;
function RPi2ConvertClockStateRequest(ClockState:LongWord):LongWord;
function RPi2ConvertClockStateResponse(ClockState:LongWord):LongWord;

function RPi2ConvertVoltageIdRequest(VoltageId:LongWord):LongWord;

function RPi2ConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RPi2Init;
begin
 {}
 if RPi2Initialized then Exit;

 {Setup IO_BASE/IO_ALIAS}
 IO_BASE:=BCM2836_PERIPHERALS_BASE;
 IO_ALIAS:=RPI2_VCIO_ALIAS;

 {Setup BUS_ALIAS}
 BUS_ALIAS:=RPI2_VCBUS_ALIAS;
 
 {Setup SECURE_BOOT}
 SECURE_BOOT:=(RPI2_SECURE_BOOT <> 0);
 
 {Setup STARTUP_ADDRESS}
 STARTUP_ADDRESS:=PtrUInt(@_text_start); {RPI2_STARTUP_ADDRESS} {Obtain from linker}
 
 {Setup PERIPHERALS_BASE and SIZE}
 PERIPHERALS_BASE:=BCM2836_PERIPHERALS_BASE;
 PERIPHERALS_SIZE:=BCM2836_PERIPHERALS_SIZE;

 {Setup LOCAL_PERIPHERALS_BASE and SIZE}
 LOCAL_PERIPHERALS_BASE:=BCM2836_ARM_LOCAL_BASE;
 LOCAL_PERIPHERALS_SIZE:=BCM2836_ARM_LOCAL_SIZE;
 
 {Setup MEMORY_BASE and SIZE}
 {Done by RPi2MemoryInit}
 
 {Setup MEMORY_IRQ/FIQ/LOCAL/SHARED/DEVICE/NOCACHE/NONSHARED_SIZE}
 {Done by RPi2MemoryInit}
 
 {Setup PAGE_TABLE_BASE and SIZE}
 PAGE_TABLE_BASE:=RPI2_PAGE_TABLE_BASE;
 PAGE_TABLE_SIZE:=RPI2_PAGE_TABLE_SIZE;
 
 {Setup VECTOR_TABLE_BASE, SIZE and COUNT}
 VECTOR_TABLE_BASE:=RPI2_VECTOR_TABLE_BASE;
 VECTOR_TABLE_SIZE:=RPI2_VECTOR_TABLE_SIZE;
 VECTOR_TABLE_COUNT:=RPI2_VECTOR_TABLE_COUNT;
 
 {Setup MACHINE_TYPE} 
 MACHINE_TYPE:=MACHINE_TYPE_UNKNOWN;
 case ARMMachineType of 
  ARM_MACHINE_BCM2709:MACHINE_TYPE:=MACHINE_TYPE_BCM2709;
 end;
 
 {Setup BOARD_TYPE}
 {Done by RPi2BoardInit}
 
 {Setup CPU_ARCH, TYPE and COUNT}
 CPU_ARCH:=CPU_ARCH_ARM32;
 CPU_TYPE:=CPU_TYPE_ARMV7;
 CPU_COUNT:=RPI2_CPU_COUNT;
 CPU_BOOT:=RPI2_CPU_BOOT;
 CPU_MASK:=RPI2_CPU_MASK;
 CPU_MAX_COUNT:=RPI2_CPU_COUNT;
 
 {Setup CPU_MEMORY_BASE and SIZE}
 {Done by RPi2MemoryInit}
 
 {Setup CPU_MEMORY_RESTRICTED}
 CPU_MEMORY_RESTRICTED:=True;
 
 {Setup FPU_TYPE}
 FPU_TYPE:=FPU_TYPE_VFPV3;
 
 {Setup GPU_TYPE}
 GPU_TYPE:=GPU_TYPE_VC4;
 
 {Setup GPU_MEMORY_BASE and SIZE}
 {Done by RPi2MemoryInit}
 
 {Setup GPU_MEMORY_CACHED}
 GPU_MEMORY_CACHED:=True;
 
 {Setup IRQ/FIQ/SWI_COUNT/START/ROUTING}
 IRQ_COUNT:=BCM2836_IRQ_COUNT;
 FIQ_COUNT:=BCM2836_FIQ_COUNT;
 
 IRQ_START:=RPI2_IRQ_START; 

 IRQ_ROUTING:=RPI2_IRQ_ROUTING;
 FIQ_ROUTING:=RPI2_FIQ_ROUTING; 

 IRQ_LOCAL_COUNT:=BCM2836_ARM_LOCAL_IRQ_COUNT;
 FIQ_LOCAL_COUNT:=BCM2836_ARM_LOCAL_IRQ_COUNT;
 
 IRQ_LOCAL_START:=RPI2_IRQ_LOCAL_START;
 
 SWI_COUNT:=RPI2_SWI_COUNT;
 
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
 CLOCK_FREQUENCY:={$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}RPI2_GENERIC_TIMER_FREQUENCY{$ELSE}BCM2836_SYSTEM_TIMER_FREQUENCY{$ENDIF};
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
 SCHEDULER_CLOCKS_PER_INTERRUPT:=RPI2_GENERIC_TIMER_FREQUENCY div SCHEDULER_INTERRUPTS_PER_SECOND;
 SCHEDULER_CLOCKS_TOLERANCE:=SCHEDULER_CLOCKS_PER_INTERRUPT div 10;
 TIME_TICKS_PER_SCHEDULER_INTERRUPT:=SCHEDULER_INTERRUPTS_PER_MILLISECOND * TIME_TICKS_PER_MILLISECOND;
 
 {Setup SCHEDULER_IDLE}
 SCHEDULER_IDLE_WAIT:=True;
 SCHEDULER_IDLE_OFFSET:=1;
 SCHEDULER_IDLE_PER_SECOND:=SCHEDULER_INTERRUPTS_PER_SECOND;
 
 {Setup KERNEL_NAME/CONFIG/COMMAND}
 KERNEL_NAME:=RPI2_KERNEL_NAME;
 KERNEL_CONFIG:=RPI2_KERNEL_CONFIG;
 KERNEL_COMMAND:=RPI2_KERNEL_COMMAND;
 FIRMWARE_FILES:=RPI2_FIRMWARE_FILES;

 {Setup GPIO (Set early to support activity LED)}
 GPIO_REGS_BASE:=BCM2836_GPIO_REGS_BASE;
 
 {Register Platform SMPInit Handler}
 SMPInitHandler:=RPi2SMPInit;
 
 {Register Platform BoardInit Handler}
 BoardInitHandler:=RPi2BoardInit;
 
 {Register Platform MemoryInit Handler}
 MemoryInitHandler:=RPi2MemoryInit;
 
 {Register Platform ClockInit Handler}
 ClockInitHandler:=RPi2ClockInit;
 
 {Register Platform PowerInit Handler}
 PowerInitHandler:=RPi2PowerInit;
 
 {Register Platform MailboxInit Handler}
 MailboxInitHandler:=RPi2MailboxInit;
 
 {Register Platform InterruptInit Handler}
 InterruptInitHandler:=RPi2InterruptInit;
 
 {Register Platform PeripheralInit Handler}
 PeripheralInitHandler:=RPi2PeripheralInit;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Framebuffer FramebufferInit Handler}
 FramebufferInitHandler:=RPi2FramebufferInit;
 {$ENDIF}
 {Register PlatformARMv7 PageTableInit Handler}
 ARMv7PageTableInitHandler:=RPi2PageTableInit;

 {Register Platform Blink Handlers}
 BootBlinkHandler:=RPi2BootBlink;
 
 {Register Platform LED Handlers}
 PowerLEDEnableHandler:=RPi2PowerLEDEnable;
 PowerLEDOnHandler:=RPi2PowerLEDOn;
 PowerLEDOffHandler:=RPi2PowerLEDOff;
 ActivityLEDEnableHandler:=RPi2ActivityLEDEnable;
 ActivityLEDOnHandler:=RPi2ActivityLEDOn;
 ActivityLEDOffHandler:=RPi2ActivityLEDOff;
 
 {Register Platform Mailbox Handlers}
 MailboxReceiveHandler:=RPi2MailboxReceive;
 MailboxSendHandler:=RPi2MailboxSend;
 MailboxCallHandler:=RPi2MailboxCall;
 MailboxCallExHandler:=RPi2MailboxCallEx;
 MailboxPropertyCallHandler:=RPi2MailboxPropertyCall;
 MailboxPropertyCallExHandler:=RPi2MailboxPropertyCallEx;

 {Register Platform IRQ Handlers}
 RequestExIRQHandler:=RPi2RequestExIRQ;
 ReleaseExIRQHandler:=RPi2ReleaseExIRQ;

 {Register Platform FIQ Handlers}
 RequestExFIQHandler:=RPi2RequestExFIQ;
 ReleaseExFIQHandler:=RPi2ReleaseExFIQ;

 {Register Platform System Call Handlers}
 RegisterSystemCallExHandler:=RPi2RegisterSystemCallEx;
 DeregisterSystemCallExHandler:=RPi2DeregisterSystemCallEx;

 {Register Platform Interrupt Handlers}
 GetInterruptEntryHandler:=RPi2GetInterruptEntry;
 
 {Register Platform Local Interrupt Handlers}
 GetLocalInterruptEntryHandler:=RPi2GetLocalInterruptEntry;
 
 {Register Platform System Call Handlers}
 GetSystemCallEntryHandler:=RPi2GetSystemCallEntry;
 
 {Register Platform System Handlers}
 SystemRestartHandler:=RPi2SystemRestart;
 SystemShutdownHandler:=RPi2SystemShutdown;
 SystemGetCommandLineHandler:=RPi2SystemGetCommandLine;

 {Register Platform CPU Handlers}
 CPUGetMemoryHandler:=RPi2CPUGetMemory;

 {Register Platform GPU Handlers}
 GPUGetStateHandler:=RPi2GPUGetState;
 GPUGetMemoryHandler:=RPi2GPUGetMemory;

 {Register Platform Board Handlers}
 BoardGetModelHandler:=RPi2BoardGetModel;
 BoardGetSerialHandler:=RPi2BoardGetSerial;
 BoardGetRevisionHandler:=RPi2BoardGetRevision;
 BoardGetMACAddressHandler:=RPi2BoardGetMACAddress;

 {Register Platform Firmware Handlers}
 FirmwareGetRevisionHandler:=RPi2FirmwareGetRevision;
 FirmwareGetThrottledHandler:=RPi2FirmwareGetThrottled;

 {Register Platform Power Handlers}
 PowerGetWaitHandler:=RPi2PowerGetWait;
 PowerGetStateHandler:=RPi2PowerGetState;
 PowerSetStateHandler:=RPi2PowerSetState;

 {Register Platform Clock Handlers}
 ClockGetCountHandler:=RPi2ClockGetCount;
 ClockGetTotalHandler:=RPi2ClockGetTotal;

 ClockGetRateHandler:=RPi2ClockGetRate;
 ClockSetRateHandler:=RPi2ClockSetRate;

 ClockGetStateHandler:=RPi2ClockGetState;
 ClockSetStateHandler:=RPi2ClockSetState;

 ClockGetMinRateHandler:=RPi2ClockGetMinRate;
 ClockGetMaxRateHandler:=RPi2ClockGetMaxRate;

 {Register Platform Turbo Handlers}
 TurboGetStateHandler:=RPi2TurboGetState;
 TurboSetStateHandler:=RPi2TurboSetState;

 {Register Platform Voltage Handlers}
 VoltageGetValueHandler:=RPi2VoltageGetValue;
 VoltageSetValueHandler:=RPi2VoltageSetValue;
 VoltageGetMinValueHandler:=RPi2VoltageGetMinValue;
 VoltageGetMaxValueHandler:=RPi2VoltageGetMaxValue;
 
 {Register Platform Temperature Handlers}
 TemperatureGetCurrentHandler:=RPi2TemperatureGetCurrent;
 TemperatureGetMaximumHandler:=RPi2TemperatureGetMaximum;
 {$IFDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPi2GPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPi2GPUMemoryRelease;
 GPUMemoryLockHandler:=RPi2GPUMemoryLock;
 GPUMemoryUnlockHandler:=RPi2GPUMemoryUnlock;
 
 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPi2GPUExecuteCode;
 DispmanxHandleGetHandler:=RPi2DispmanxHandleGet;
 EDIDBlockGetHandler:=RPi2EDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPi2FramebufferAllocate;
 FramebufferReleaseHandler:=RPi2FramebufferRelease;
 FramebufferSetStateHandler:=RPi2FramebufferSetState;

 FramebufferGetDimensionsHandler:=RPi2FramebufferGetDimensions;
 
 FramebufferGetPhysicalHandler:=RPi2FramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPi2FramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPi2FramebufferTestPhysical;
 
 FramebufferGetVirtualHandler:=RPi2FramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPi2FramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPi2FramebufferTestVirtual;
 
 FramebufferGetDepthHandler:=RPi2FramebufferGetDepth;
 FramebufferSetDepthHandler:=RPi2FramebufferSetDepth;
 FramebufferTestDepthHandler:=RPi2FramebufferTestDepth;
 
 FramebufferGetPixelOrderHandler:=RPi2FramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPi2FramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPi2FramebufferTestPixelOrder;
 
 FramebufferGetAlphaModeHandler:=RPi2FramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPi2FramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPi2FramebufferTestAlphaMode;
 
 FramebufferGetPitchHandler:=RPi2FramebufferGetPitch;
 
 FramebufferGetOffsetHandler:=RPi2FramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPi2FramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPi2FramebufferTestOffset;
 
 FramebufferGetOverscanHandler:=RPi2FramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPi2FramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPi2FramebufferTestOverscan;
 
 FramebufferGetPaletteHandler:=RPi2FramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPi2FramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPi2FramebufferTestPalette;

 FramebufferTestVsyncHandler:=RPi2FramebufferTestVsync;
 FramebufferSetVsyncHandler:=RPi2FramebufferSetVsync;
 
 FramebufferSetBacklightHandler:=RPi2FramebufferSetBacklight;
 
 {Register Platform Touch Handlers}
 TouchGetBufferHandler:=RPi2TouchGetBuffer;
 TouchSetBufferHandler:=RPi2TouchSetBuffer;
 
 {Register Platform Cursor Handlers}
 CursorSetDefaultHandler:=RPi2CursorSetDefault;
 CursorSetInfoHandler:=RPi2CursorSetInfo;
 CursorSetStateHandler:=RPi2CursorSetState;
 {$ENDIF}
 {Register Platform DMA Handlers}
 DMAGetChannelsHandler:=RPi2DMAGetChannels;
 
 {Register Platform Virtual GPIO Handlers}
 VirtualGPIOInputGetHandler:=RPi2VirtualGPIOInputGet;
 VirtualGPIOOutputSetHandler:=RPi2VirtualGPIOOutputSet;
 VirtualGPIOFunctionSelectHandler:=RPi2VirtualGPIOFunctionSelect;
 
 {Register Threads SchedulerInit Handler}
 SchedulerInitHandler:=RPi2SchedulerInit;
 SchedulerStartHandler:=RPi2SchedulerStart;
 
 {Register Threads SecondaryBoot Handler}
 SecondaryBootHandler:=RPi2SecondaryBoot;
 
 {Register PlatformARMv7 IRQ Handlers}
 ARMv7DispatchIRQHandler:=RPi2DispatchIRQ;

 {Register PlatformARMv7 FIQ Handlers}
 ARMv7DispatchFIQHandler:=RPi2DispatchFIQ;
 
 {Register PlatformARMv7 SWI Handlers}
 ARMv7DispatchSWIHandler:=RPi2DispatchSWI;
 
 {Register PlatformARM Helper Handlers}
 ARMWaitHandler:=RPi2Wait;
 ARMLongWaitHandler:=RPi2LongWait;
 ARMShortWaitHandler:=RPi2ShortWait;
 ARMSlowBlinkHandler:=RPi2SlowBlink;
 ARMFastBlinkHandler:=RPi2FastBlink;
 
 RPi2Initialized:=True;
end;

{==============================================================================}

procedure RPi2SecondarySwitch; assembler; nostackframe; 
{Secondary CPU switch from HYP mode handler}
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

{==============================================================================}

procedure RPi2SecondarySecure; assembler; nostackframe; 
{Secondary CPU switch to secure mode handler}
asm
 //Check the secure boot configuration
 mov r0, #RPI2_SECURE_BOOT
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
end;

{==============================================================================}

procedure RPi2SecondaryHandler; assembler; nostackframe; 
{Secondary CPU startup handler routine}
asm
 //Call the HYP mode switch handler in case the CPU is in HYP mode
 bl RPi2SecondarySwitch
 
 //Call the secure mode switch handler to return to secure mode
 bl RPi2SecondarySecure
 
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
 mov r0, #RPI2_VECTOR_TABLE_BASE
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
 mov r0, #RPI2_SECURE_BOOT
 cmp r0, #0
 beq .LNoTimer
 
 //Set the ARM Generic Timer Frequency
 ldr r0, =RPI2_GENERIC_TIMER_FREQUENCY
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
end;

{==============================================================================}
{==============================================================================}
{RPi2 Platform Functions}
procedure RPi2SMPInit;
var
 Control:LongWord;
 GPUInterruptRouting:LongWord;
begin
 {}
 {Setup ARM Local Registers}
 ARMLocalRegisters:=PBCM2836ARMLocalRegisters(BCM2836_ARM_LOCAL_REGS_BASE);

 {Setup Core Timer Clock}
 Control:=ARMLocalRegisters.Control;
 Control:=Control and not(BCM2836_ARM_LOCAL_CONTROL_APB_CLOCK or BCM2836_ARM_LOCAL_CONTROL_INCREMENT_2); {Disable APB Clock and Increment 2}
 Control:=Control or BCM2836_ARM_LOCAL_CONTROL_CRYSTAL_CLOCK or BCM2836_ARM_LOCAL_CONTROL_INCREMENT_1;   {Enable Crystal Clock and Increment 1}
 ARMLocalRegisters.Control:=Control;
 
 {Setup Core Timer Prescaler}
 ARMLocalRegisters.CoreTimerPrescaler:=RPI2_CORE_TIMER_PRESCALER;
 
 {Setup GPU IRQ/FIQ Routing}
 GPUInterruptRouting:=ARMLocalRegisters.GPUInterruptRouting;
 GPUInterruptRouting:=GPUInterruptRouting and not(BCM2836_ARM_LOCAL_GPU_INT_ROUTING_IRQ_MASK or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_FIQ_MASK); {Clear all routing}
 case IRQ_ROUTING of {Setup IRQ Routing}
  CPU_ID_0:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_IRQ0;
  CPU_ID_1:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_IRQ1;
  CPU_ID_2:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_IRQ2;
  CPU_ID_3:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_IRQ3;
 end;
 case FIQ_ROUTING of {Setup FIQ Routing}
  CPU_ID_0:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_FIQ0;
  CPU_ID_1:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_FIQ1;
  CPU_ID_2:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_FIQ2;
  CPU_ID_3:GPUInterruptRouting:=GPUInterruptRouting or BCM2836_ARM_LOCAL_GPU_INT_ROUTING_FIQ3;
 end;
 ARMLocalRegisters.GPUInterruptRouting:=GPUInterruptRouting;
 
 {Setup ARM Generic Timer}
 if SECURE_BOOT then ARMv7TimerInit(RPI2_GENERIC_TIMER_FREQUENCY);
end;

{==============================================================================}

procedure RPi2BoardInit;
var
 Revision:LongWord;
 ClockCPUMax:LongWord;
begin
 {}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then RPi2InterruptInit;
 
 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then RPi2ClockInit;
 
 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then RPi2MailboxInit;
 
 {Get Board Revision}
 Revision:=RPi2BoardGetRevision;
 
 {Get Board Type}
 if (Revision and BCM2836_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   case (Revision and BCM2836_BOARD_REVISION_MODEL_MASK) of
    BCM2836_BOARD_REVISION_MODEL_2B:begin
      BOARD_TYPE:=BOARD_TYPE_RPI2B;
      
      {Check for 2B Revision 2 (BCM2837 with Cortex A53)}
      case (Revision and BCM2836_BOARD_REVISION_PROCESSOR_MASK) of
       BCM2836_BOARD_REVISION_PROCESSOR_BCM2837:begin
         {Adjust CPU Type}
         CPU_TYPE:=CPU_TYPE_ARMV8;
      
         {Adjust Machine Type}
         MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
        end;
      end;  
     end; 
    BCM2836_BOARD_REVISION_MODEL_3B:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3B;
      
      {Adjust CPU Type}
      CPU_TYPE:=CPU_TYPE_ARMV8;
      
      {Adjust Machine Type}
      MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
     end; 
    BCM2836_BOARD_REVISION_MODEL_3BPLUS:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3B_PLUS;
      
      {Adjust CPU Type}
      CPU_TYPE:=CPU_TYPE_ARMV8;
      
      {Adjust Machine Type}
      MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
     end;
    BCM2836_BOARD_REVISION_MODEL_3APLUS:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3A_PLUS;
      
      {Adjust CPU Type}
      CPU_TYPE:=CPU_TYPE_ARMV8;
      
      {Adjust Machine Type}
      MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
     end;
    BCM2836_BOARD_REVISION_MODEL_COMPUTE3:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE3;
      
      {Adjust CPU Type}
      CPU_TYPE:=CPU_TYPE_ARMV8;
      
      {Adjust Machine Type}
      MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
     end; 
    BCM2836_BOARD_REVISION_MODEL_COMPUTE3PlUS:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE3_PLUS;
      
      {Adjust CPU Type}
      CPU_TYPE:=CPU_TYPE_ARMV8;
      
      {Adjust Machine Type}
      MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
     end; 
   end;
  end
 else
  begin 
   {Old Style Revision}
   case (Revision and BCM2836_BOARD_REV_MASK) of
    BCM2836_BOARD_REV_2B_1,BCM2836_BOARD_REV_2B_2,BCM2836_BOARD_REV_2B_3:begin
      BOARD_TYPE:=BOARD_TYPE_RPI2B;
     end;
    BCM2836_BOARD_REV_3B_1,BCM2836_BOARD_REV_3B_2,BCM2836_BOARD_REV_3B_3:begin
      BOARD_TYPE:=BOARD_TYPE_RPI3B;

      {Adjust CPU Type}
      CPU_TYPE:=CPU_TYPE_ARMV8;
      
      {Adjust Machine Type}
      MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
     end;    
    BCM2836_BOARD_REV_CM3_1,BCM2836_BOARD_REV_CM3_2:begin
      BOARD_TYPE:=BOARD_TYPE_RPI_COMPUTE3;

      {Adjust CPU Type}
      CPU_TYPE:=CPU_TYPE_ARMV8;
      
      {Adjust Machine Type}
      MACHINE_TYPE:=MACHINE_TYPE_BCM2710;
     end;    
   end;
  end; 
 
 {Get CPU Clock Maximum}
 ClockCPUMax:=RPi2ClockGetMaxRate(CLOCK_ID_CPU);
 if ClockCPUMax > 0 then
  begin
   {Set CPU Clock}
   RPi2ClockSetRate(CLOCK_ID_CPU,ClockCPUMax,True); 
  end;
end;

{==============================================================================}

procedure RPi2MemoryInit;
var
 Address:PtrUInt;
 Length:LongWord;
 Revision:LongWord;
begin
 {}
 {Initialize Interrupts (Used by ClockInit}
 if not(InterruptsInitialized) then RPi2InterruptInit;
 
 {Initialize Clock (Used by BoardGetRevision)}
 if not(ClockInitialized) then RPi2ClockInit;
 
 {Initialize Mailbox (Used by BoardGetRevision)}
 if not(MailboxInitialized) then RPi2MailboxInit;
 
 {Get Board Revision}
 Revision:=RPi2BoardGetRevision;
 
 {Check Board Revision}
 if (Revision and BCM2836_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Style Revision}
   case (Revision and BCM2836_BOARD_REVISION_MODEL_MASK) of
    BCM2836_BOARD_REVISION_MODEL_2B,
    BCM2836_BOARD_REVISION_MODEL_3B,
    BCM2836_BOARD_REVISION_MODEL_3BPLUS,
    BCM2836_BOARD_REVISION_MODEL_COMPUTE3,
    BCM2836_BOARD_REVISION_MODEL_COMPUTE3PlUS:begin
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_1G;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
      {Get IRQ/FIQ/Local/Shared/Device/NoCache/NonShared Sizes}
      MEMORY_IRQ_SIZE:=SIZE_8M;
      MEMORY_FIQ_SIZE:=SIZE_8M;
      MEMORY_LOCAL_SIZE:=SIZE_8M;
      MEMORY_SHARED_SIZE:=SIZE_32M;
      MEMORY_DEVICE_SIZE:=SIZE_0; {was SIZE_8M}
      MEMORY_NOCACHE_SIZE:=SIZE_16M;
      MEMORY_NONSHARED_SIZE:=SIZE_8M;
     end;
    BCM2836_BOARD_REVISION_MODEL_3APLUS:begin
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_512M;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
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
   case (Revision and BCM2836_BOARD_REV_MASK) of
    BCM2836_BOARD_REV_2B_1,BCM2836_BOARD_REV_2B_2,BCM2836_BOARD_REV_2B_3,
    BCM2836_BOARD_REV_3B_1,BCM2836_BOARD_REV_3B_2,BCM2836_BOARD_REV_3B_3,
    BCM2836_BOARD_REV_CM3_1,BCM2836_BOARD_REV_CM3_2:begin 
      {Get Memory Base/Size}
      MEMORY_BASE:=$00000000;
      MEMORY_SIZE:=SIZE_1G;
      {Get Memory Page Size}
      MEMORY_PAGE_SIZE:=SIZE_4K;
      MEMORY_LARGEPAGE_SIZE:=SIZE_64K;
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
 if RPi2CPUGetMemory(Address,Length) = ERROR_SUCCESS then
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
 if RPi2GPUGetMemory(Address,Length) = ERROR_SUCCESS then
  begin
   GPU_MEMORY_BASE:=Address;
   GPU_MEMORY_SIZE:=Length;
  end;
end;

{==============================================================================}

procedure RPi2ClockInit;
{$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
var
 State:LongWord;
{$ENDIF}
begin
 {}
 {Setup Timer Registers}
 TimerRegisters:=PBCM2836SystemTimerRegisters(BCM2836_SYSTEM_TIMER_REGS_BASE);
 
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
   {$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
   RequestFIQ(RPI2_CPU_BOOT,BCM2836_IRQ_LOCAL_ARM_CNTVIRQ,RPi2ClockInterrupt,nil); 
   {$ELSE}
   RequestFIQ(RPI2_CPU_BOOT,BCM2836_IRQ_SYSTEM_TIMER_3,RPi2ClockInterrupt,nil); 
   {$ENDIF}
  end
 else
  begin
   {$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
   RequestIRQ(RPI2_CPU_BOOT,BCM2836_IRQ_LOCAL_ARM_CNTVIRQ,RPi2ClockInterrupt,nil); 
   {$ELSE}
   RequestIRQ(RPI2_CPU_BOOT,BCM2836_IRQ_SYSTEM_TIMER_3,RPi2ClockInterrupt,nil); 
   {$ENDIF}
  end;

 {$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
 {Setup the Generic Timer}
 State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTV);
 State:=State and not(ARMV7_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV7_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv7SetTimerState(ARMV7_CP15_C14_CNTV,State); 
 {$ENDIF}
 
 {Setup the first Clock Interrupt}
 RPi2ClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
end;

{==============================================================================}

procedure RPi2PowerInit;
begin
 {}
 {Setup Watchdog Registers}
 WatchdogRegisters:=PBCM2836PMWatchdogRegisters(BCM2836_PM_REGS_BASE);
end;

{==============================================================================}

procedure RPi2MailboxInit;
begin
 {}
 {Setup Mailbox0/1 Registers}
 Mailbox0Registers:=PBCM2836Mailbox0Registers(BCM2836_MAILBOX0_REGS_BASE);
 Mailbox1Registers:=PBCM2836Mailbox1Registers(BCM2836_MAILBOX1_REGS_BASE);
end;

{==============================================================================}

procedure RPi2InterruptInit;
var
 Count:LongWord;
 Counter:LongWord;
begin
 {}
 {Setup Interrupt Registers}
 InterruptRegisters:=PBCM2836InterruptRegisters(BCM2836_INTERRUPT_REGS_BASE);
 
 {Setup Interrupt Entries}
 for Count:=0 to BCM2836_GPU_IRQ_COUNT + BCM2836_ARM_IRQ_COUNT - 1 do
  begin
   FillChar(InterruptEntries[Count],SizeOf(TInterruptEntry),0);
   
   InterruptEntries[Count].Number:=Count;
   InterruptEntries[Count].CPUID:=CPU_ID_ALL;
  end; 
 
 {Setup Local Interrupt Entries}
 for Count:=RPI2_IRQ_LOCAL_START to BCM2836_IRQ_COUNT - 1 do
  begin
   for Counter:=0 to RPI2_CPU_COUNT - 1 do
    begin
     FillChar(LocalInterruptEntries[Count,Counter],SizeOf(TInterruptEntry),0);
     
     LocalInterruptEntries[Count,Counter].Number:=Count;
     LocalInterruptEntries[Count,Counter].CPUID:=Counter;
    end; 
  end; 
 
 {Setup System Call Entries}
 for Count:=0 to RPI2_SWI_COUNT - 1 do
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
 for Count:=0 to RPI2_CPU_COUNT - 1 do
  begin
   IRQLocalEnabled[Count]:=0;
  end;
  
 {Setup Local Enabled FIQs}
 for Count:=0 to RPI2_CPU_COUNT - 1 do
  begin
   FIQLocalEnabled[Count]:=0;
  end;
  
 {Clear Interrupt Enabled}
 InterruptRegisters.FIQ_control:=0;
 InterruptRegisters.Disable_IRQs_1:=$FFFFFFFF;
 InterruptRegisters.Disable_IRQs_2:=$FFFFFFFF;
 InterruptRegisters.Disable_Basic_IRQs:=$FFFFFFFF;
end;

{==============================================================================}

procedure RPi2PeripheralInit;
var
 CacheLineSize:LongWord;
begin
 {}
 {Get Cache Line Size}
 CacheLineSize:=Max(L1DataCacheGetLineSize,L2CacheGetLineSize);
 
 {Setup Peripherals}
 INTERRUPT_REGS_BASE:=BCM2836_INTERRUPT_REGS_BASE;
 SYSTEMTIMER_REGS_BASE:=BCM2836_SYSTEM_TIMER_REGS_BASE;
 TIMER_REGS_BASE:=BCM2836_TIMER_REGS_BASE;
 GPIO_REGS_BASE:=BCM2836_GPIO_REGS_BASE;
 UART_REGS_BASE:=BCM2836_PL011_REGS_BASE;

 {Setup Interrupts}
 //To Do 
 
 {Setup GPIO}
 GPIO_PIN_COUNT:=BCM2836_GPIO_PIN_COUNT;
 
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
 
 {Setup BCM2709}
 BCM2709DMA_ALIGNMENT:=SizeOf(LongWord);
 BCM2709DMA_MULTIPLIER:=SizeOf(LongWord);
 BCM2709DMA_SHARED_MEMORY:=False;
 BCM2709DMA_NOCACHE_MEMORY:=False;
 BCM2709DMA_BUS_ADDRESSES:=True;
 BCM2709DMA_CACHE_COHERENT:=False;
 if CacheLineSize > BCM2709DMA_ALIGNMENT then BCM2709DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > BCM2709DMA_MULTIPLIER then BCM2709DMA_MULTIPLIER:=CacheLineSize;
 
 BCM2709FRAMEBUFFER_ALIGNMENT:=SIZE_256;
 BCM2709FRAMEBUFFER_CACHED:=GPU_MEMORY_CACHED;
 
 BCM2709_REGISTER_I2C0:=False; {I2C0 is not available on the header except on original Revision 1 boards}
 
 {Setup DWCOTG}
 DWCOTG_IRQ:=BCM2836_IRQ_USB;
 DWCOTG_POWER_ID:=POWER_ID_USB0;
 DWCOTG_REGS_BASE:=BCM2836_USB_REGS_BASE;
 DWCOTG_DMA_ALIGNMENT:=SizeOf(LongWord);
 DWCOTG_DMA_MULTIPLIER:=SizeOf(LongWord);
 DWCOTG_DMA_SHARED_MEMORY:=False;
 DWCOTG_DMA_NOCACHE_MEMORY:=False;
 DWCOTG_DMA_BUS_ADDRESSES:=True; 
 DWCOTG_DMA_CACHE_COHERENT:=False;
 DWCOTG_HOST_FRAME_INTERVAL:=True;
 if CacheLineSize > DWCOTG_DMA_ALIGNMENT then DWCOTG_DMA_ALIGNMENT:=CacheLineSize;
 if CacheLineSize > DWCOTG_DMA_MULTIPLIER then DWCOTG_DMA_MULTIPLIER:=CacheLineSize;
 
 {Setup SMSC95XX}
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
procedure RPi2FramebufferInit;
var
 Status:LongWord;
 
 RPi2Framebuffer:PFramebufferDevice;
begin
 {}
 {Create Framebuffer}
 RPi2Framebuffer:=PFramebufferDevice(FramebufferDeviceCreateEx(SizeOf(TFramebufferDevice)));
 if RPi2Framebuffer <> nil then
  begin
   {Device}
   RPi2Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   RPi2Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   RPi2Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_BACKLIGHT;
   RPi2Framebuffer.Device.DeviceData:=nil;
   RPi2Framebuffer.Device.DeviceDescription:=RPI2_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   RPi2Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   RPi2Framebuffer.DeviceAllocate:=RPi2FramebufferDeviceAllocate;
   RPi2Framebuffer.DeviceRelease:=RPi2FramebufferDeviceRelease;
   RPi2Framebuffer.DeviceBlank:=RPi2FramebufferDeviceBlank;
   RPi2Framebuffer.DeviceCommit:=RPi2FramebufferDeviceCommit;
   RPi2Framebuffer.DeviceSetBacklight:=RPi2FramebufferDeviceSetBacklight;   
   RPi2Framebuffer.DeviceSetProperties:=RPi2FramebufferDeviceSetProperties;
   {Driver}
 
   {Setup Flags}
   if BCM2709FRAMEBUFFER_CACHED then RPi2Framebuffer.Device.DeviceFlags:=RPi2Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_COMMIT;
   if BCM2709FRAMEBUFFER_CACHED then RPi2Framebuffer.Device.DeviceFlags:=RPi2Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
   {if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then RPi2Framebuffer.Device.DeviceFlags:=RPi2Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;} {Handled by FramebufferAllocate}
   
   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(RPi2Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(RPi2Framebuffer,nil);
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
{$ENDIF}
{==============================================================================}

procedure RPi2PageTableInit;
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
 {Initialize Memory (Get values for CPU_MEMORY_BASE/SIZE)}
 if not(MemoryInitialized) then RPi2MemoryInit;
 
 {Parse Boot Tags (Register all memory with Heap manager)}
 if not(ParseBootTagsCompleted) then ARMParseBootTags;

 {Parse Command Line (Copy command line from zero page)}
 if not(ParseCommandLineCompleted) then ARMParseCommandLine;

 {Parse Environment (Copy environment from zero page)}
 if not(ParseEnvironmentCompleted) then ARMParseEnvironment;
 
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
 {Changed to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK (Shared)(Executable)(Read Only) due to no write through support for data caching on ARMv7}
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
 {Changed to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK (Shared)(Executable)(Read Only) due to no write through support for data caching on ARMv7}
 {See 5.2.1 Memory types and attributes in the Cortex A7 Technical Reference Manual (http://infocenter.arm.com/help/topic/com.arm.doc.ddi0464f/CIHJCAAG.html)}
 Address:=(LongWord(@_text_start) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (LongWord(@_data)) do
  begin
   {ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH or ARMV7_L2D_ACCESS_READONLY);}
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_ACCESS_READONLY);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the DATA (Initialized) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(LongWord(@_data) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (LongWord(@_bss_start)) do
  begin
   ARMv7SetPageTableSmall(Address,Address,ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L2D_FLAG_SHARED or ARMV7_L2D_FLAG_SMALL_XN or ARMV7_L2D_ACCESS_READWRITE);
   Inc(Address,SIZE_4K);
  end;

 {Set the 4KB pages containing the BSS (Uninitialized) section to ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Non Executable)(Read Write)}
 Address:=(LongWord(@_bss_start) and ARMV7_L2D_SMALL_BASE_MASK);
 while Address < (LongWord(@_bss_end)) do
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
     Dec(RequestAddress,MEMORY_LOCAL_SIZE * RPI2_CPU_COUNT); {Local memory is per CPU}
     if IRQ_ENABLED then Dec(RequestAddress,MEMORY_IRQ_SIZE * RPI2_CPU_COUNT); {IRQ memory is per CPU}
     if FIQ_ENABLED then Dec(RequestAddress,MEMORY_FIQ_SIZE * RPI2_CPU_COUNT); {FIQ memory is per CPU}
     
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
       for Count:=0 to (RPI2_CPU_COUNT - 1) do
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
       for Count:=0 to (RPI2_CPU_COUNT - 1) do
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
       for Count:=0 to (RPI2_CPU_COUNT - 1) do
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
  
 {Synchronization Barrier}
 DataSynchronizationBarrier;
end;

{==============================================================================}

procedure RPi2PowerLEDEnable;
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

procedure RPi2PowerLEDOn;
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

procedure RPi2PowerLEDOff;
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

procedure RPi2ActivityLEDEnable;
var
 Value:LongWord;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {Read current value of GPFSEL}
      Value:=GPIORead(RPI2_GPIO_ACTLED_GPFSEL);
      {Mask off relevant bits}
      Value:=Value and not(RPI2_GPIO_ACTLED_GPFMASK shl RPI2_GPIO_ACTLED_GPFSHIFT);
      {Include required bits}
      Value:=Value or (1 shl RPI2_GPIO_ACTLED_GPFSHIFT);
      {Write new value to GPFSEL} 
      GPIOWrite(RPI2_GPIO_ACTLED_GPFSEL,Value);
     end
    else
     begin
      {Disable Pull Up/Down}
      GPIOPullSelect(GPIO_PIN_47,GPIO_PULL_NONE);
      {Enable Output}
      GPIOFunctionSelect(GPIO_PIN_47,GPIO_FUNCTION_OUT);
     end; 
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

procedure RPi2ActivityLEDOn;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED On}
      GPIOWrite(RPI2_GPIO_ACTLED_GPSET,(RPI2_GPIO_ACTLED_GPMASK shl RPI2_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED On}
      GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_HIGH);
     end; 
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

procedure RPi2ActivityLEDOff;
begin
 {}
 case BOARD_TYPE of
  BOARD_TYPE_RPI2B:begin
    {Check Available}
    if not GPIOAvailable then
     begin
      {LED Off}
      GPIOWrite(RPI2_GPIO_ACTLED_GPCLR,(RPI2_GPIO_ACTLED_GPMASK shl RPI2_GPIO_ACTLED_GPSHIFT));
     end
    else
     begin
      {LED Off}
      GPIOOutputSet(GPIO_PIN_47,GPIO_LEVEL_LOW);
     end; 
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

function RPi2MailboxReceive(Mailbox,Channel:LongWord):LongWord;
{Receive from specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 ResultCode:LongWord;
begin
 {}
 Result:=0;
 {Check Mailbox}
 if Mailbox = BCM2836_MAILBOX_0 then
  begin 
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try 
    {Setup Timeout}
    Timeout:=RPI2_MAILBOX_TIMEOUT;
   
    {Setup Result}
    ResultCode:=BCM2836_MAILBOX_CHANNEL_MASK; {Start with all channel bits set}
   
    {Check Channel}
    while ((ResultCode and BCM2836_MAILBOX_CHANNEL_MASK) <> Channel) do
     begin
      {Check Status}
      while (Mailbox0Registers.Status and BCM2836_MAILBOX_STATUS_EMPTY) = BCM2836_MAILBOX_STATUS_EMPTY do
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
    Result:=ResultCode and BCM2836_MAILBOX_DATA_MASK; {Account for channel offset}
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end; 
end;

{==============================================================================}

procedure RPi2MailboxSend(Mailbox,Channel,Data:LongWord);
{Send to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
var
 Timeout:LongWord;
 WriteData:LongWord;
begin
 {}
 {Check Mailbox}
 if Mailbox = BCM2836_MAILBOX_0 then
  begin 
   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try 
    {Setup Timeout}
    Timeout:=RPI2_MAILBOX_TIMEOUT;
   
    {Setup Data}
    WriteData:=Channel or (Data and BCM2836_MAILBOX_DATA_MASK);
   
    {Check Status}
    while (Mailbox1Registers.Status and BCM2836_MAILBOX_STATUS_FULL) = BCM2836_MAILBOX_STATUS_FULL do
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

function RPi2MailboxCall(Mailbox,Channel,Data:LongWord;var Response:LongWord):LongWord;
{Perform a transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPi2MailboxCallEx(Mailbox,Channel,Data,Response,RPI2_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPi2MailboxCallEx(Mailbox,Channel,Data:LongWord;var Response:LongWord;Timeout:LongWord):LongWord; 
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
 if Mailbox = BCM2836_MAILBOX_0 then
  begin 
   {Check the Data (Must not use the lowest 4 bits)}
   if (Data and BCM2836_MAILBOX_CHANNEL_MASK) <> 0 then Exit;

   {Acquire Lock}
   if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.AcquireLock(MailboxLock.Lock);
   try 
    {Setup Timeout}
    Retries:=Timeout;
    
    {Wait for Mailbox 0 Empty} 
    while (Mailbox0Registers.Status and BCM2836_MAILBOX_STATUS_EMPTY) <> BCM2836_MAILBOX_STATUS_EMPTY do
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
    while (Mailbox1Registers.Status and BCM2836_MAILBOX_STATUS_FULL) = BCM2836_MAILBOX_STATUS_FULL do
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
    WriteData:=Channel or (Data and BCM2836_MAILBOX_DATA_MASK);
    Mailbox1Registers.Write:=WriteData; 
 
    {Setup Timeout}
    Retries:=Timeout;
    
    {Wait for Mailbox 0 not Empty}
    while (Mailbox0Registers.Status and BCM2836_MAILBOX_STATUS_EMPTY) = BCM2836_MAILBOX_STATUS_EMPTY do
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
    if (ResultCode and BCM2836_MAILBOX_CHANNEL_MASK) <> Channel then
     begin
      Result:=ERROR_INVALID_DATA;
      Exit;
     end; 
  
    {Return the Response}
    Response:=ResultCode and BCM2836_MAILBOX_DATA_MASK; {Account for channel offset}
    
    Result:=ERROR_SUCCESS;
   finally
    {Release Lock}
    if MailboxLock.Lock <> INVALID_HANDLE_VALUE then MailboxLock.ReleaseLock(MailboxLock.Lock);
   end;
  end; 
end;

{==============================================================================}

function RPi2MailboxPropertyCall(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord):LongWord;
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
begin
 {}
 Result:=RPi2MailboxPropertyCallEx(Mailbox,Channel,Data,Response,RPI2_MAILBOX_TIMEOUT);
end;

{==============================================================================}

function RPi2MailboxPropertyCallEx(Mailbox,Channel:LongWord;Data:Pointer;var Response:LongWord;Timeout:LongWord):LongWord; 
{Perform a property tag transaction (Send/Receive) to specified mailbox on specified channel}
{Note: Data = first 28 bits, Channel = last 4 bits}
{Note: Data pointer must be 16 byte aligned to allow for the 4 bit channel number}
var
 Tag:PBCM2836MailboxTagHeader;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF PLATFORM_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - (Mailbox=' + IntToHex(Mailbox,8) + ' Channel=' + IntToHex(Channel,8) + ' Data=' + IntToHex(PtrUInt(Data),8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Mailbox}
 if Mailbox = BCM2836_MAILBOX_0 then
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
   if PBCM2836MailboxHeader(Data).Code <> BCM2836_MBOX_RESPONSE_CODE_SUCCESS then
    begin
     Result:=ERROR_FUNCTION_FAILED;
     if PLATFORM_LOG_ENABLED then PlatformLogError('MailboxPropertyCallEx - Response Code Failed: (Code=' + IntToHex(PBCM2836MailboxHeader(Data).Code,8) + ')');
     Exit;
    end;
 
   {Check each tags Response Code}
   Tag:=PBCM2836MailboxTagHeader(PtrUInt(Data) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
   while Tag.Tag <> BCM2836_MBOX_TAG_END do
    begin
     if (Tag.Length and BCM2836_MBOX_TAG_RESPONSE_CODE) = 0 then
      begin
       {$IFDEF PLATFORM_DEBUG}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('MailboxPropertyCallEx - Tag Response Code Incorrect (Length=' + IntToHex(Tag.Length,8) + ')');
       {$ENDIF}
       {Result:=ERROR_FUNCTION_FAILED;} {Note: Recent firmware functions do not always set the response bit in the tag}
       {Exit;}                          {      The Linux firmware driver does not check this bit in the response}
      end;
     {Clear the Response bit so callers can read the length field without extra processing}
     Tag.Length:=Tag.Length and not(BCM2836_MBOX_TAG_RESPONSE_CODE);
     {Get Next Tag}
     Tag:=PBCM2836MailboxTagHeader(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagHeader)) + Tag.Size);
    end;
 
   Result:=ERROR_SUCCESS;
  end; 
end;

{==============================================================================}

function RPi2RequestExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request registration of the supplied handler to the specified IRQ number}
{CPUID: CPU to route IRQ to}
{Number: IRQ number to register}
{Handler: Interrupt handler function to register}
{HandlerEx: Extended Interrupt handler function to register}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check CPU}
 if (CPUID <> CPU_ID_ALL) and (CPUID > (CPUGetCount - 1)) then Exit;
 
 {Check CPU}
 if CPUID = CPU_ID_ALL then 
  begin
   CPUID:=CPUGetCurrent;
  end;
  
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit;

 {Check Routing}
 if Number < IRQ_LOCAL_START then
  begin
   if (IRQ_ROUTING <> CPU_ID_ALL) and (IRQ_ROUTING <> CPUID) then Exit;
  end;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  if Number < IRQ_LOCAL_START then
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
    if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
   end
  else
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(LocalInterruptEntries[Number,CPUID].Handler) and (@LocalInterruptEntries[Number,CPUID].Handler <> @Handler) then Exit;
    if Assigned(LocalInterruptEntries[Number,CPUID].HandlerEx) and (@LocalInterruptEntries[Number,CPUID].HandlerEx <> @HandlerEx) then Exit;
   end;   
 
  {Find Group}
  if Number < 32 then
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}
  
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Enable IRQ}
    InterruptRegisters.Enable_IRQs_1:=(1 shl Number);
    IRQEnabled[0]:=IRQEnabled[0] or (1 shl Number);
    
    {Register Entry}
    InterruptEntries[Number].CPUID:=CPUID;
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
    InterruptRegisters.Enable_IRQs_2:=(1 shl (Number - 32));
    IRQEnabled[1]:=IRQEnabled[1] or (1 shl (Number - 32));
    
    {Register Entry}
    InterruptEntries[Number].CPUID:=CPUID;
    InterruptEntries[Number].Handler:=Handler;
    InterruptEntries[Number].HandlerEx:=HandlerEx;
    InterruptEntries[Number].Parameter:=Parameter;
   end
  else if Number < 96 then
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Enable IRQ}
    InterruptRegisters.Enable_Basic_IRQs:=(1 shl (Number - 64));
    IRQEnabled[2]:=IRQEnabled[2] or (1 shl (Number - 64));
    
    {Register Entry}
    InterruptEntries[Number].CPUID:=CPUID;
    InterruptEntries[Number].Handler:=Handler;
    InterruptEntries[Number].HandlerEx:=HandlerEx;
    InterruptEntries[Number].Parameter:=Parameter;
   end
  else
   begin
    {Check FIQ}
    if (FIQLocalEnabled[CPUID] and (1 shl (Number - 96))) <> 0 then Exit;

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}

    {Check Number}
    case Number of
     BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ:begin
       {Enable Physical Secure Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSIRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ:begin
       {Enable Physical Non Secure Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSIRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTHPIRQ:begin
       {Enable Hypervisor Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPIRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTVIRQ:begin
       {Enable Virtual Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTVIRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX0:begin
       {Enable Mailbox0 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0IRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX1:begin
       {Enable Mailbox1 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1IRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX2:begin
       {Enable Mailbox2 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2IRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX3:begin
       {Enable Mailbox3 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3IRQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_GPU:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PMU:begin
       {Enable Performance Monitors IRQ}
       ARMLocalRegisters.PMInterruptRoutingSet:=(1 shl CPUID);
      end;
     BCM2836_IRQ_LOCAL_ARM_AXI:begin
       {Enable AXI Outstanding Writes IRQ}
       if CPUID <> CPU_ID_0 then Exit;
       ARMLocalRegisters.AXIOutstandingIRQ:=ARMLocalRegisters.AXIOutstandingIRQ or BCM2836_ARM_LOCAL_AXI_IRQ_ENABLE;
      end;
     BCM2836_IRQ_LOCAL_ARM_TIMER:begin
       {Enable Local Timer IRQ}
       ARMLocalRegisters.LocalIntRouting0:=(ARMLocalRegisters.LocalIntRouting0 and not(7)) or CPUID;
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL1:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL2:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL3:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL4:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL5:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL6:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL7:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL8:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL9:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL10:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL11:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL12:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL13:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL14:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL15:begin
       {Nothing}
      end;
    else
     begin
      Exit;
     end;
    end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
    
    {Enable Local IRQ}
    IRQLocalEnabled[CPUID]:=IRQLocalEnabled[CPUID] or (1 shl (Number - 96));
   
    {Register Entry}
    LocalInterruptEntries[Number,CPUID].Handler:=Handler;
    LocalInterruptEntries[Number,CPUID].HandlerEx:=HandlerEx;
    LocalInterruptEntries[Number,CPUID].Parameter:=Parameter;
   end;   
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPi2ReleaseExIRQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord;
{Request deregistration of the supplied handler from the specified IRQ number}
{CPUID: CPU to route IRQ to}
{Number: IRQ number to deregister}
{Handler: Interrupt handler function to deregister}
{HandlerEx: Extended Interrupt handler function to deregister}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check CPU}
 if (CPUID <> CPU_ID_ALL) and (CPUID > (CPUGetCount - 1)) then Exit;
 
 {Check CPU}
 if CPUID = CPU_ID_ALL then 
  begin
   CPUID:=CPUGetCurrent;
  end;
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit;
 
 {Check Routing}
 if Number < IRQ_LOCAL_START then
  begin
   if (IRQ_ROUTING <> CPU_ID_ALL) and (IRQ_ROUTING <> CPUID) then Exit;
  end;
 
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
  if Number < IRQ_LOCAL_START then
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
    if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
   end
  else
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(LocalInterruptEntries[Number,CPUID].Handler) and (@LocalInterruptEntries[Number,CPUID].Handler <> @Handler) then Exit;
    if Assigned(LocalInterruptEntries[Number,CPUID].HandlerEx) and (@LocalInterruptEntries[Number,CPUID].HandlerEx <> @HandlerEx) then Exit;
   end;
 
  {Find Group}
  if Number < 32 then
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Disable IRQ}
    InterruptRegisters.Disable_IRQs_1:=(1 shl Number);
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
    InterruptRegisters.Disable_IRQs_2:=(1 shl (Number - 32));
    IRQEnabled[1]:=IRQEnabled[1] and not(1 shl (Number - 32));
    
    {Deregister Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=nil;
    InterruptEntries[Number].HandlerEx:=nil;
    InterruptEntries[Number].Parameter:=nil;
   end
  else if Number < 96 then 
   begin
    {Check FIQ}
    if FIQEnabled = Number then Exit; {FIQEnabled will be -1 when nothing enabled}

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Disable IRQ}
    InterruptRegisters.Disable_Basic_IRQs:=(1 shl (Number - 64));
    IRQEnabled[2]:=IRQEnabled[2] and not(1 shl (Number - 64));
    
    {Deregister Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=nil;
    InterruptEntries[Number].HandlerEx:=nil;
    InterruptEntries[Number].Parameter:=nil;
   end
  else
   begin
    {Check FIQ}
    if (FIQLocalEnabled[CPUID] and (1 shl (Number - 96))) <> 0 then Exit;

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}

    {Check Number}
    case Number of
     BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ:begin
       {Disable Physical Secure Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSIRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ:begin
       {Disable Physical Non Secure Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSIRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTHPIRQ:begin
       {Disable Hypervisor Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPIRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTVIRQ:begin
       {Disable Virtual Timer IRQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTVIRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX0:begin
       {Disable Mailbox0 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0IRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX1:begin
       {Disable Mailbox1 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1IRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX2:begin
       {Disable Mailbox2 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2IRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX3:begin
       {Disable Mailbox3 IRQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3IRQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_GPU:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PMU:begin
       {Disable Performance Monitors IRQ}
       ARMLocalRegisters.PMInterruptRoutingClear:=(1 shl CPUID);
      end;
     BCM2836_IRQ_LOCAL_ARM_AXI:begin
       {Disable AXI Outstanding Writes IRQ}
       if CPUID <> CPU_ID_0 then Exit;
       ARMLocalRegisters.AXIOutstandingIRQ:=ARMLocalRegisters.AXIOutstandingIRQ and not(BCM2836_ARM_LOCAL_AXI_IRQ_ENABLE);
      end;
     BCM2836_IRQ_LOCAL_ARM_TIMER:begin
       {Disable Local Timer IRQ}
       ARMLocalRegisters.LocalIntRouting0:=ARMLocalRegisters.LocalIntRouting0 and not(7);
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL1:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL2:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL3:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL4:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL5:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL6:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL7:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL8:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL9:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL10:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL11:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL12:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL13:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL14:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL15:begin
       {Nothing}
      end;
    else
     begin
      Exit;
     end;
    end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
    
    {Disable Local IRQ}
    IRQLocalEnabled[CPUID]:=IRQLocalEnabled[CPUID] and not(1 shl (Number - 96));
    
    {Deregister Entry}
    LocalInterruptEntries[Number,CPUID].Handler:=nil;
    LocalInterruptEntries[Number,CPUID].HandlerEx:=nil;
    LocalInterruptEntries[Number,CPUID].Parameter:=nil;
   end;   
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPi2RequestExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
{Request registration of the supplied handler to the specified FIQ number}
{CPUID: CPU to route FIQ to}
{Number: FIQ number to register}
{Handler: Interrupt handler function to register}
{HandlerEx: Extended Interrupt handler function to register}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check CPU}
 if (CPUID <> CPU_ID_ALL) and (CPUID > (CPUGetCount - 1)) then Exit;
 
 {Check CPU}
 if CPUID = CPU_ID_ALL then 
  begin
   CPUID:=CPUGetCurrent;
  end;
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit; {IRQ Count not FIQ Count}

 {Check Routing}
 if Number < IRQ_LOCAL_START then {IRQ Local Start}
  begin
   if (FIQ_ROUTING <> CPU_ID_ALL) and (FIQ_ROUTING <> CPUID) then Exit;
  end;
 
 {Check Handlers}
 if Assigned(Handler) and Assigned(HandlerEx) then Exit;
 if not(Assigned(Handler)) and not(Assigned(HandlerEx)) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Check Handlers}
  if Number < IRQ_LOCAL_START then
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
    if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
   end
  else
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(LocalInterruptEntries[Number,CPUID].Handler) and (@LocalInterruptEntries[Number,CPUID].Handler <> @Handler) then Exit;
    if Assigned(LocalInterruptEntries[Number,CPUID].HandlerEx) and (@LocalInterruptEntries[Number,CPUID].HandlerEx <> @HandlerEx) then Exit;
   end;   
 
  {Find Group}
  if Number < 96 then
   begin
    {Check FIQ}
    if FIQEnabled <> LongWord(-1) then Exit; {FIQEnabled will be -1 when nothing enabled}
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Enable FIQ}
    InterruptRegisters.FIQ_control:=BCM2836_ARM_INTERRUPT_FIQ_ENABLE or (Number and BCM2836_ARM_INTERRUPT_FIQ_SOURCE);
    FIQEnabled:=Number;
    
    {Register Entry}
    InterruptEntries[Number].CPUID:=CPUID;
    InterruptEntries[Number].Handler:=Handler;
    InterruptEntries[Number].HandlerEx:=HandlerEx;
    InterruptEntries[Number].Parameter:=Parameter;
   end
  else
   begin
    {Check FIQ}
    if (FIQLocalEnabled[CPUID] and (1 shl (Number - 96))) <> 0 then Exit;

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Check Number}
    case Number of
     BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ:begin
       {Enable Physical Secure Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSFIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ:begin
       {Enable Physical Non Secure Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSFIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTHPIRQ:begin
       {Enable Hypervisor Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPFIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTVIRQ:begin
       {Enable Virtual Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] or BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTVFIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX0:begin
       {Enable Mailbox0 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0FIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX1:begin
       {Enable Mailbox1 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1FIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX2:begin
       {Enable Mailbox2 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2FIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX3:begin
       {Enable Mailbox3 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] or BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3FIQ;
      end;
     BCM2836_IRQ_LOCAL_ARM_GPU:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PMU:begin
       {Enable Performance Monitors FIQ}
       ARMLocalRegisters.PMInterruptRoutingSet:=(1 shl (CPUID + 4));
      end;
     BCM2836_IRQ_LOCAL_ARM_AXI:begin
       {Nothing}
       if CPUID <> CPU_ID_0 then Exit;
      end;
     BCM2836_IRQ_LOCAL_ARM_TIMER:begin
       {Enable Local Timer FIQ}
       ARMLocalRegisters.LocalIntRouting0:=(ARMLocalRegisters.LocalIntRouting0 and not(7)) or (CPUID + 4);
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL1:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL2:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL3:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL4:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL5:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL6:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL7:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL8:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL9:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL10:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL11:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL12:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL13:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL14:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL15:begin
       {Nothing}
      end;
    else
     begin
      Exit;
     end;
    end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
    
    {Enable Local FIQ}
    FIQLocalEnabled[CPUID]:=FIQLocalEnabled[CPUID] or (1 shl (Number - 96));
    
    {Register Entry}
    LocalInterruptEntries[Number,CPUID].Handler:=Handler;
    LocalInterruptEntries[Number,CPUID].HandlerEx:=HandlerEx;
    LocalInterruptEntries[Number,CPUID].Parameter:=Parameter;
   end;
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPi2ReleaseExFIQ(CPUID,Number:LongWord;Handler:TInterruptHandler;HandlerEx:TInterruptExHandler;Parameter:Pointer):LongWord; 
{Request deregistration of the supplied handler from the specified FIQ number}
{CPUID: CPU to route FIQ to}
{Number: FIQ number to deregister}
{Handler: Interrupt handler function to deregister}
{HandlerEx: Extended Interrupt handler function to deregister}
{Note: Only one of Handler or HandlerEx can be specified}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check CPU}
 if (CPUID <> CPU_ID_ALL) and (CPUID > (CPUGetCount - 1)) then Exit;
 
 {Check CPU}
 if CPUID = CPU_ID_ALL then 
  begin
   CPUID:=CPUGetCurrent;
  end;
 
 {Check Number}
 if Number > (IRQ_COUNT - 1) then Exit; {IRQ Count not FIQ Count}

 {Check Routing}
 if Number < IRQ_LOCAL_START then {IRQ Local Start}
  begin
   if (FIQ_ROUTING <> CPU_ID_ALL) and (FIQ_ROUTING <> CPUID) then Exit;
  end;
 
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
  if Number < IRQ_LOCAL_START then
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(InterruptEntries[Number].Handler) and (@InterruptEntries[Number].Handler <> @Handler) then Exit;
    if Assigned(InterruptEntries[Number].HandlerEx) and (@InterruptEntries[Number].HandlerEx <> @HandlerEx) then Exit;
   end
  else
   begin
    Result:=ERROR_ALREADY_ASSIGNED;
    if Assigned(LocalInterruptEntries[Number,CPUID].Handler) and (@LocalInterruptEntries[Number,CPUID].Handler <> @Handler) then Exit;
    if Assigned(LocalInterruptEntries[Number,CPUID].HandlerEx) and (@LocalInterruptEntries[Number,CPUID].HandlerEx <> @HandlerEx) then Exit;
   end;
 
  {Find Group}
  if Number < 96 then 
   begin
    {Check FIQ}
    if FIQEnabled <> Number then Exit; {FIQEnabled will be -1 when nothing enabled}

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Disable FIQ}
    InterruptRegisters.FIQ_control:=0;
    FIQEnabled:=LongWord(-1);
    
    {Deregister Entry}
    InterruptEntries[Number].CPUID:=CPU_ID_ALL;
    InterruptEntries[Number].Handler:=nil;
    InterruptEntries[Number].HandlerEx:=nil;
    InterruptEntries[Number].Parameter:=nil;
   end
  else
   begin
    {Check FIQ}
    if (FIQLocalEnabled[CPUID] and (1 shl (Number - 96))) = 0 then Exit;
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}

    {Check Number}
    case Number of
     BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ:begin
       {Disable Physical Secure Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSFIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ:begin
       {Disable Physical Non Secure Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSFIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTHPIRQ:begin
       {Disable Hypervisor Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPFIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_CNTVIRQ:begin
       {Disable Virtual Timer FIQ}
       ARMLocalRegisters.TimersIntControl[CPUID]:=ARMLocalRegisters.TimersIntControl[CPUID] and not(BCM2836_ARM_LOCAL_TIMER_INT_CONTROL_CNTVFIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX0:begin
       {Disable Mailbox0 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0FIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX1:begin
       {Disable Mailbox1 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1FIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX2:begin
       {Disable Mailbox2 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2FIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_MAILBOX3:begin
       {Disable Mailbox3 FIQ}
       ARMLocalRegisters.MailboxIntControl[CPUID]:=ARMLocalRegisters.MailboxIntControl[CPUID] and not(BCM2836_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3FIQ);
      end;
     BCM2836_IRQ_LOCAL_ARM_GPU:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PMU:begin
       {Disable Performance Monitors FIQ}
       ARMLocalRegisters.PMInterruptRoutingClear:=(1 shl (CPUID + 4));
      end;
     BCM2836_IRQ_LOCAL_ARM_AXI:begin
       {Nothing}
       if CPUID <> CPU_ID_0 then Exit;
      end;
     BCM2836_IRQ_LOCAL_ARM_TIMER:begin
       {Disable Local Timer FIQ}
       ARMLocalRegisters.LocalIntRouting0:=ARMLocalRegisters.LocalIntRouting0 and not(7);
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL1:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL2:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL3:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL4:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL5:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL6:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL7:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL8:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL9:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL10:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL11:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL12:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL13:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL14:begin
       {Nothing}
      end;
     BCM2836_IRQ_LOCAL_ARM_PERIPHERAL15:begin
       {Nothing}
      end;
    else
     begin
      Exit;
     end;
    end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
    
    {Disable Local FIQ}
    FIQLocalEnabled[CPUID]:=FIQLocalEnabled[CPUID] and not(1 shl (Number - 96));
    
    {Deregister Entry}
    LocalInterruptEntries[Number,CPUID].Handler:=nil;
    LocalInterruptEntries[Number,CPUID].HandlerEx:=nil;
    LocalInterruptEntries[Number,CPUID].Parameter:=nil;
   end;   
 
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPi2RegisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
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

function RPi2DeregisterSystemCallEx(CPUID,Number:LongWord;Handler:TSystemCallHandler;HandlerEx:TSystemCallExHandler):LongWord;
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

function RPi2GetInterruptEntry(Number:LongWord):TInterruptEntry; 
{Get the interrupt entry for the specified interrupt number}
begin
 {}
 FillChar(Result,SizeOf(TInterruptEntry),0);
 
 {Check Number}
 if Number >= IRQ_LOCAL_START then Exit;
 
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

function RPi2GetLocalInterruptEntry(CPUID,Number:LongWord):TInterruptEntry; 
{Get the local interrupt entry for the specified interrupt number}
begin
 {}
 FillChar(Result,SizeOf(TInterruptEntry),0);
 
 {Check CPU}
 if (CPUID <> CPU_ID_ALL) and (CPUID > (CPUGetCount - 1)) then Exit;
 
 {Check CPU}
 if CPUID = CPU_ID_ALL then 
  begin
   CPUID:=CPUGetCurrent;
  end;
 
 {Check Number}
 if Number < IRQ_LOCAL_START then Exit;
 if Number > (IRQ_COUNT - 1) then Exit;
 
 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try 
  {Return Entry}
  Result:=LocalInterruptEntries[Number,CPUID];
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function RPi2GetSystemCallEntry(Number:LongWord):TSystemCallEntry; 
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

function RPi2SystemRestart(Delay:LongWord):LongWord; 
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
  WatchdogRegisters.WDOG:=BCM2836_PM_PASSWORD or ((Delay * BCM2836_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2836_PM_WDOG_TIME_MASK);
  
  {Enable Restart}  
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2836_PM_PASSWORD or (Current and BCM2836_PM_RSTC_WRCFG_CLR) or BCM2836_PM_RSTC_WRCFG_FULL_RESET;

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

function RPi2SystemShutdown(Delay:LongWord):LongWord;
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
  {WatchdogRegisters.RSTS:=BCM2836_PM_PASSWORD or (Current and BCM2836_PM_RSTC_WRCFG_CLR) or BCM2836_PM_RSTS_HADWRH_SET;} {RPi firmware changed to use a different value}
  WatchdogRegisters.RSTS:=Current or BCM2836_PM_PASSWORD or BCM2836_PM_RSTS_RASPBERRYPI_HALT;
  
  {Enable Watchdog}
  WatchdogRegisters.WDOG:=BCM2836_PM_PASSWORD or ((Delay * BCM2836_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2836_PM_WDOG_TIME_MASK);
  
  {Enable Restart}  
  Current:=WatchdogRegisters.RSTC;
  WatchdogRegisters.RSTC:=BCM2836_PM_PASSWORD or (Current and BCM2836_PM_RSTC_WRCFG_CLR) or BCM2836_PM_RSTC_WRCFG_FULL_RESET;

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

function RPi2SystemGetCommandLine:String;
{Get the Command Line from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetCommandLine;
begin
 {}
 Result:='';
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetCommandLine) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetCommandLine(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_COMMAND_LINE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetCommandLine) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetCommandLine)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2CPUGetMemory(var Address:PtrUInt;var Length:LongWord):LongWord; 
{Get the CPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetARMMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetARMMemory) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetARMMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_ARM_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetARMMemory) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetARMMemory)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2GPUGetState:LongWord;
begin
 {}
 Result:=GPU_STATE_NONE;

 //To Do
end;

{==============================================================================}

function RPi2GPUGetMemory(var Address:PtrUInt;var Length:LongWord):LongWord; 
{Get the GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetVCMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetVCMemory) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetVCMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_VC_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetVCMemory) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetVCMemory)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2BoardGetModel:LongWord;
{Get the Board Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Model:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetBoardModel;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetBoardModel) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetBoardModel(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_BOARD_MODEL;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetBoardModel) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetBoardModel)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2BoardGetSerial:Int64;
{Get the Board Serial from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetBoardSerial;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetBoardSerial) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetBoardSerial(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_BOARD_SERIAL;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetBoardSerial) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetBoardSerial)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2BoardGetRevision:LongWord;
{Get the Board Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetBoardRevision;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetBoardRevision) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try 
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetBoardRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_BOARD_REV;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetBoardRevision) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetBoardRevision)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2BoardGetMACAddress:String;
{Get the Board MAC Address from the Mailbox property tags channel}
var
 Size:LongWord;
 Count:Integer;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetMACAddress;
begin
 {}
 Result:='';
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetMACAddress) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetMACAddress(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_MAC_ADDRESS;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetMACAddress) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetMACAddress)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2FirmwareGetRevision:LongWord;
{Get the Firmware Revision from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetFirmwareRevision;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetFirmwareRevision) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetFirmwareRevision(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_FIRMWARE_REV;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetFirmwareRevision) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetFirmwareRevision)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2FirmwareGetThrottled:LongWord;
{Get the Firmware Throttling state from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetThrottled;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetThrottled) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetThrottled(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_THROTTLED;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetThrottled) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Value:=$FFFF; {Clear sticky bits}
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetThrottled)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2PowerGetWait(PowerId:LongWord):LongWord;
{Get the Power Wait from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetTiming;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetTiming) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetTiming(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_TIMING;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetTiming) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi2ConvertPowerIdRequest(PowerId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetTiming)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2PowerGetState(PowerId:LongWord):LongWord;
{Get the Power State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetPowerState;
begin
 {}
 Result:=0;

 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetPowerState) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetPowerState) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi2ConvertPowerIdRequest(PowerId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetPowerState)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerGetState - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Power State}
  Result:=RPi2ConvertPowerStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi2PowerSetState(PowerId,State:LongWord;Wait:Boolean):LongWord;
{Set the Power State in the Mailbox property tags channel}
{Note: Power Lock not required due to Mailbox Property Call serialization}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetPowerState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetPowerState) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetPowerState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_POWER_STATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetPowerState) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.DeviceId:=RPi2ConvertPowerIdRequest(PowerId);
  Tag.Request.State:=RPi2ConvertPowerStateRequest(State);
  if Wait then Tag.Request.State:=(Tag.Request.State or BCM2836_MBOX_SET_POWER_STATE_REQ_WAIT);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetPowerState)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if Wait then
   begin
    Result:=MailboxPropertyCallEx(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response,RPI2_MAILBOX_TIMEOUT_EX);
   end
  else
   begin  
    Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
   end; 
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('PowerSetState - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Power State}
  if Wait then
   begin
    if RPi2ConvertPowerStateRequest(State) = BCM2836_MBOX_SET_POWER_STATE_REQ_ON then
     begin
      if (Tag.Response.State and BCM2836_MBOX_POWER_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
     end
    else
     begin
      if (Tag.Response.State and BCM2836_MBOX_POWER_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
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

function RPi2ClockGetCount:LongWord;
{Gets the current system clock count (32 least significant bits of total)}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz and therefore overflows every 4295 seconds}
begin
 {}
 {$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
 {Get Value}
 Result:=ARMv7GetTimerCount(ARMV7_CP15_C14_CNTV);
 {$ELSE}
 {Get Value}
 Result:=TimerRegisters.CLO;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 {$ENDIF}
end;

{==============================================================================}

function RPi2ClockGetTotal:Int64; 
{Gets the total system clock count}
{Note: On the Raspberry Pi this comes from the System Timer free running
 counter which runs at 1MHz, the clock interrupt also uses this timer to
 increment the clock every second and therefore keep time}
{$IFDEF RPI2_CLOCK_SYSTEM_TIMER}
var
 Check:LongWord;
{$ENDIF}
begin
 {}
 {$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
 {Get Value}
 Result:=ARMv7GetTimerCount(ARMV7_CP15_C14_CNTV);
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

function RPi2ClockGetRate(ClockId:LongWord):LongWord;
{Get the Clock Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetClockRate;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetClockRate) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetClockRate) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi2ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetClockRate)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2ClockSetRate(ClockId,Rate:LongWord;Turbo:Boolean):LongWord;
{Set the Clock Rate in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetClockRate;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Rate}
 if Rate = 0 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetClockRate) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetClockRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_CLOCK_RATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetClockRate) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi2ConvertClockIdRequest(ClockId);
  Tag.Request.Rate:=Rate;
  Tag.Request.SkipTurbo:=0;
  if not(Turbo) then Tag.Request.SkipTurbo:=BCM2836_MBOX_CLOCK_RATE_REQ_SKIP_TURBO;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetClockRate)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2ClockGetState(ClockId:LongWord):LongWord;
{Get the Clock State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetClockState;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetClockState) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetClockState) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi2ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetClockState)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockGetState - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Clock State}
  Result:=RPi2ConvertClockStateResponse(Tag.Response.State);
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi2ClockSetState(ClockId,State:LongWord):LongWord;
{Set the Clock State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetClockState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetClockState) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetClockState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_CLOCK_STATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetClockState) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi2ConvertClockIdRequest(ClockId);
  Tag.Request.State:=RPi2ConvertClockStateRequest(State);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetClockState)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('ClockSetState - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Clock State}
  if RPi2ConvertClockStateRequest(State) = BCM2836_MBOX_SET_CLOCK_STATE_REQ_ON then
   begin
    if (Tag.Response.State and BCM2836_MBOX_CLOCK_STATE_RESP_ON) <> 0 then Result:=ERROR_SUCCESS;
   end
  else
   begin
    if (Tag.Response.State and BCM2836_MBOX_CLOCK_STATE_RESP_ON) = 0 then Result:=ERROR_SUCCESS;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi2ClockGetMinRate(ClockId:LongWord):LongWord;
{Get the Clock Min Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetClockMinRate;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetClockMinRate) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetClockMinRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_CLOCK_MIN_RATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetClockMinRate) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi2ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetClockMinRate)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2ClockGetMaxRate(ClockId:LongWord):LongWord;
{Get the Clock Max Rate from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetClockMaxRate;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetClockMaxRate) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetClockMaxRate(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_CLOCK_MAX_RATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetClockMaxRate) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.ClockId:=RPi2ConvertClockIdRequest(ClockId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetClockMaxRate)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2TurboGetState(TurboId:LongWord):LongWord;
{Get the Turbo State from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetTurbo;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetTurbo) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetTurbo) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetTurbo)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2TurboSetState(TurboId,State:LongWord):LongWord;
{Set the Turbo State in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetTurbo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetTurbo) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetTurbo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_TURBO;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetTurbo) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Id:=0; {Must be zero}
  Tag.Request.Level:=State; {0 to Off / 1 for On}
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetTurbo)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2VoltageGetValue(VoltageId:LongWord):LongWord;
{Get the Voltage Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetVoltage;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetVoltage) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetVoltage) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi2ConvertVoltageIdRequest(VoltageId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetVoltage)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetValue - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Voltage Value}
  if (Tag.Response.Value <> BCM2836_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi2VoltageSetValue(VoltageId,Value:LongWord):LongWord;
{Set the Voltage Value in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetVoltage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Value}
 if Value = 0 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetVoltage) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetVoltage) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi2ConvertVoltageIdRequest(VoltageId);
  Tag.Request.Value:=Value; {Offset from 1.2V in units of 0.025V}
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetVoltage)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Result <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageSetValue - MailboxPropertyCall Failed');
    Exit;
   end; 
  
  {Check Voltage Value}
  if (Tag.Response.Value <> BCM2836_MBOX_VOLTAGE_INVALID) and (Tag.Response.Value = Value) then Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi2VoltageGetMinValue(VoltageId:LongWord):LongWord;
{Get the Voltage Min Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetMinVoltage;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetMinVoltage) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetMinVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_MIN_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetMinVoltage) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi2ConvertVoltageIdRequest(VoltageId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetMinVoltage)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMinValue - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Voltage Min Value}
  if (Tag.Response.Value <> BCM2836_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;
 
{==============================================================================}

function RPi2VoltageGetMaxValue(VoltageId:LongWord):LongWord;
{Get the Voltage Max Value from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetMaxVoltage;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetMaxVoltage) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetMaxVoltage(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_MAX_VOLTAGE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetMaxVoltage) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.VoltageId:=RPi2ConvertVoltageIdRequest(VoltageId);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetMaxVoltage)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    if PLATFORM_LOG_ENABLED then PlatformLogError('VoltageGetMaxValue - MailboxPropertyCall Failed');
    Exit;
   end; 

  {Get Voltage Max Value}
  if (Tag.Response.Value <> BCM2836_MBOX_VOLTAGE_INVALID) then Result:=Tag.Response.Value; {Offset from 1.2V in units of 0.025V}
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPi2TemperatureGetCurrent(TemperatureId:LongWord):LongWord;
{Get the Temperature Current from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetTemperature;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetTemperature) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetTemperature) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPi2ConvertTemperatureIdRequest(TemperatureId);
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetTemperature)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2TemperatureGetMaximum(TemperatureId:LongWord):LongWord;
{Get the Temperature Maximum Model from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetMaxTemperature;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetMaxTemperature) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetMaxTemperature(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_MAX_TEMP;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetMaxTemperature) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.TemperatureId:=RPi2ConvertTemperatureIdRequest(TemperatureId);
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetMaxTemperature)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2GPUMemoryAllocate(Length,Alignment,Flags:LongWord):THandle;
{Allocate GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagAllocateMemory;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagAllocateMemory) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagAllocateMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_ALLOCATE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagAllocateMemory) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Size:=Length;
  Tag.Request.Alignment:=Alignment;
  Tag.Request.Flags:=Flags;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagAllocateMemory)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2GPUMemoryRelease(Handle:THandle):LongWord;
{Release GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagReleaseMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagReleaseMemory) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagReleaseMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_RELEASE_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagReleaseMemory) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagReleaseMemory)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2GPUMemoryLock(Handle:THandle):LongWord;
{Lock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagLockMemory;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagLockMemory) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagLockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_LOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagLockMemory) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagLockMemory)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2GPUMemoryUnlock(Handle:THandle):LongWord;
{Unlock GPU Memory from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagUnlockMemory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagUnlockMemory) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagUnlockMemory(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_UNLOCK_MEMORY;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagUnlockMemory) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Handle:=Handle;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagUnlockMemory)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2GPUExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord;
{Execute GPU Code from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagExecuteCode;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagExecuteCode) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagExecuteCode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_EXECUTE_CODE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagExecuteCode) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
  Tag.Request.R0:=R0;
  Tag.Request.R1:=R1;
  Tag.Request.R2:=R2;
  Tag.Request.R3:=R3;
  Tag.Request.R4:=R4;
  Tag.Request.R5:=R5;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagExecuteCode)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2DispmanxHandleGet(Resource:THandle):THandle;
{Get Dispmanx Memory Handle from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetDispmanxHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetDispmanxHandle) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetDispmanxHandle(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_DISPMANX_HANDLE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetDispmanxHandle) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Resource:=Resource;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetDispmanxHandle)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2EDIDBlockGet(Block:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Get EDID Block from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetEDIDBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Length}
 if Length < 128 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetEDIDBlock) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetEDIDBlock(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_EDID_BLOCK;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetEDIDBlock) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Block:=Block;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetEDIDBlock)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferAllocate(Alignment:LongWord;var Address,Length:LongWord):LongWord;
{Allocate Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagAllocateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagAllocateBuffer) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagAllocateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_ALLOCATE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagAllocateBuffer) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Alignment:=Alignment;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagAllocateBuffer)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferRelease:LongWord;
{Release Framebuffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagReleaseBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagReleaseBuffer) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_RELEASE_BUFFER;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagReleaseBuffer) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagReleaseBuffer)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetState(State:LongWord):LongWord;
{Set Framebuffer State (Blank Screen) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagBlankScreen;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagBlankScreen) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagBlankScreen(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_BLANK_SCREEN;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagBlankScreen) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.State:=0;
  if State = 0 then Tag.Request.State:=BCM2836_MBOX_BLANK_SCREEN_REQ_ON;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagBlankScreen)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetDimensions(var Width,Height,Top,Bottom,Left,Right:LongWord):LongWord; 
{Get Framebuffer Dimensions from the Mailbox property tags channel}
begin
 {}
 {Get Physical}
 Result:=RPi2FramebufferGetPhysical(Width,Height);
 if Result = ERROR_SUCCESS then
  begin
   {Get Overscan}
   Result:=RPi2FramebufferGetOverscan(Top,Bottom,Left,Right);
  end;
end;

{==============================================================================}

function RPi2FramebufferGetPhysical(var Width,Height:LongWord):LongWord;
{Get Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetPhysical) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetPhysical) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetPhysical)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetPhysical(var Width,Height:LongWord):LongWord;
{Set Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetPhysical) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetPhysical) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetPhysical)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestPhysical(var Width,Height:LongWord):LongWord;
{Test Framebuffer Physical size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestPhysical;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestPhysical) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestPhysical(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_PHYSICAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestPhysical) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestPhysical)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetVirtual(var Width,Height:LongWord):LongWord;
{Get Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetVirtual) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetVirtual) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetVirtual)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetVirtual(var Width,Height:LongWord):LongWord;
{Set Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetVirtual) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetVirtual) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetVirtual)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestVirtual(var Width,Height:LongWord):LongWord;
{Test Framebuffer Virtual size from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestVirtual;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestVirtual) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestVirtual(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_VIRTUAL_W_H;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestVirtual) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestVirtual)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetDepth(var Depth:LongWord):LongWord;
{Get Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetDepth) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetDepth) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetDepth)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetDepth(var Depth:LongWord):LongWord;
{Set Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetDepth) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetDepth) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetDepth)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestDepth(var Depth:LongWord):LongWord;
{Test Framebuffer Depth (Bits per pixel) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestDepth;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestDepth) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestDepth(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_DEPTH;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestDepth) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Depth:=Depth;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestDepth)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetPixelOrder(var Order:LongWord):LongWord;
{Get Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetPixelOrder) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetPixelOrder) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetPixelOrder)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetPixelOrder(var Order:LongWord):LongWord;
{Set Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetPixelOrder) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetPixelOrder) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetPixelOrder)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestPixelOrder(var Order:LongWord):LongWord;
{Test Framebuffer Pixel Order (RGB) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestPixelOrder;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestPixelOrder) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestPixelOrder(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_PIXEL_ORDER;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestPixelOrder) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Order:=Order;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestPixelOrder)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetAlphaMode(var Mode:LongWord):LongWord;
{Get Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetAlphaMode) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetAlphaMode) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetAlphaMode)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetAlphaMode(var Mode:LongWord):LongWord;
{Set Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetAlphaMode) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetAlphaMode) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetAlphaMode)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestAlphaMode(var Mode:LongWord):LongWord;
{Test Framebuffer Alpha Mode from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestAlphaMode;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestAlphaMode) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestAlphaMode(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_ALPHA_MODE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestAlphaMode) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Mode:=Mode;
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestAlphaMode)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetPitch:LongWord;
{Get Framebuffer Pitch (Bytes per line) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetPitch;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetPitch) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetPitch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_PITCH;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetPitch) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetPitch)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2FramebufferGetOffset(var X,Y:LongWord):LongWord;
{Get Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetVirtualOffset) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetVirtualOffset) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetVirtualOffset)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetOffset(var X,Y:LongWord):LongWord;
{Set Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetVirtualOffset) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetVirtualOffset) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetVirtualOffset)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestOffset(var X,Y:LongWord):LongWord;
{Test Framebuffer Virtual Offset from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestVirtualOffset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestVirtualOffset) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestVirtualOffset(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_VIRTUAL_OFFSET;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestVirtualOffset) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestVirtualOffset)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Get Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetOverscan) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetOverscan) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetOverscan)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Set Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetOverscan) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetOverscan) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetOverscan)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestOverscan(var Top,Bottom,Left,Right:LongWord):LongWord;
{Test Framebuffer Overscan from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestOverscan;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestOverscan) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestOverscan(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_OVERSCAN;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestOverscan) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Top:=Top;
  Tag.Request.Bottom:=Bottom;
  Tag.Request.Left:=Left;
  Tag.Request.Right:=Right;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestOverscan)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferGetPalette(Buffer:Pointer;Length:LongWord):LongWord;
{Get Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetPalette;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < 1024 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetPalette) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetPalette) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetPalette)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Set Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetPalette;
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
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetPalette) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetPalette) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetPalette)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestPalette(Start,Count:LongWord;Buffer:Pointer;Length:LongWord):LongWord;
{Test Framebuffer Palette from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestPalette;
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
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestPalette) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestPalette(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TEST_PALETTE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestPalette) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Offset:=Start;
  Tag.Request.Length:=Count;
  System.Move(Buffer^,Tag.Request.Values,Count * SizeOf(LongWord));
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestPalette)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferTestVsync:LongWord;
{Test Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagTestVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagTestVsync) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagTestVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_TST_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagTestVsync) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagTestVsync)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetVsync:LongWord;
{Set Framebuffer Vertical Sync from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetVsync;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetVsync) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetVsync(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_VSYNC;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetVsync) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetVsync)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2FramebufferSetBacklight(Brightness:LongWord):LongWord;
{Set Framebuffer Backlight Brightness from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetBacklight;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetBacklight) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetBacklight(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_BACKLIGHT;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetBacklight) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Brightness:=Brightness;
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetBacklight)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2TouchGetBuffer(var Address:LongWord):LongWord;
{Get the Touchscreen buffer from the Mailbox property tags channel}

{Note: On current firmware versions calling TouchGetBuffer will allocate a buffer
       from GPU memory and render subsequent calls to TouchSetBuffer ineffective.
       
       After an initial call to TouchSetBuffer calls to TouchGetBuffer will always
       return the CPU allocated buffer}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetTouch) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetTouch) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetTouch)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2TouchSetBuffer(Address:PtrUInt):LongWord;
{Set the Touchscreen buffer in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetTouch;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetTouch) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetTouch(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_TOUCHBUF;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetTouch) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetTouch)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2VirtualGPIOGetBuffer(var Address:LongWord):LongWord;
{Get the Virtual GPIO buffer from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetVirtualGPIO;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetVirtualGPIO) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetVirtualGPIO(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_GPIOVIRTBUF;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetVirtualGPIO) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetVirtualGPIO)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2VirtualGPIOSetBuffer(Address:PtrUInt):LongWord;
{Set the Virtual GPIO buffer in the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetVirtualGPIO;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetVirtualGPIO) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetVirtualGPIO(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_GPIOVIRTBUF;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetVirtualGPIO) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetVirtualGPIO)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2CursorSetDefault:LongWord;
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
   Result:=RPi2CursorSetInfo(CURSOR_ARROW_DEFAULT_WIDTH,CURSOR_ARROW_DEFAULT_HEIGHT,0,0,Pointer(Address),Size);
 
   {Free the Cursor}
   FreeMem(Cursor);
  end;
end;

{==============================================================================}

function RPi2CursorSetInfo(Width,Height,HotspotX,HotspotY:LongWord;Pixels:Pointer;Length:LongWord):LongWord;
{Set Cursor Info (Pixels) from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetCursorInfo;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Pixels}
 if Pixels = nil then Exit;
 
 {Check Length}
 if Length < 1 then Exit;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetCursorInfo) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetCursorInfo(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_CURSOR_INFO;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetCursorInfo) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Width:=Width;
  Tag.Request.Height:=Height;
  Tag.Request.Pixels:=Pixels;
  Tag.Request.HotspotX:=HotspotX;
  Tag.Request.HotspotY:=HotspotY;
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetCursorInfo)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2CursorSetState(Enabled:Boolean;X,Y:LongWord;Relative:Boolean):LongWord;
{Set Cursor State (Enable, X, Y) from the Mailbox property tags channel}
{Relative: X, Y is relative to Display (Virtual) not Framebuffer (Physical)}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagSetCursorState;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagSetCursorState) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagSetCursorState(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_SET_CURSOR_STATE;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagSetCursorState) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Enable:=BCM2836_MBOX_CURSOR_INVISIBLE;
  if Enabled then Tag.Request.Enable:=BCM2836_MBOX_CURSOR_VISIBLE;
  Tag.Request.X:=X;
  Tag.Request.Y:=Y;
  Tag.Request.Flags:=BCM2836_MBOX_CURSOR_STATE_FRAMEBUFFER_COORDS;
  if Relative then Tag.Request.Flags:=BCM2836_MBOX_CURSOR_STATE_DISPLAY_COORDS;
  
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagSetCursorState)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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

function RPi2DMAGetChannels:LongWord;
{Get the available DMA Channels from the Mailbox property tags channel}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagGetDMAChannels;
begin
 {}
 Result:=0;
 
 {Calculate Size}
 Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagGetDMAChannels) + SizeOf(TBCM2836MailboxFooter);
 
 {Allocate Mailbox Buffer}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=BCM2836_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PBCM2836MailboxTagGetDMAChannels(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
  Tag.Header.Tag:=BCM2836_MBOX_TAG_GET_DMA_CHANNELS;
  Tag.Header.Size:=SizeOf(TBCM2836MailboxTagGetDMAChannels) - SizeOf(TBCM2836MailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
 
  {Setup Footer}
  Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagGetDMAChannels)));
  Footer.Tag:=BCM2836_MBOX_TAG_END;
  
  {Call Mailbox}
  if MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
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

function RPi2VirtualGPIOAllocate:Boolean; 
{Allocate the Virtual GPIO buffer either from memory or from the firmware}
var
 Size:LongWord;
 Address:LongWord;
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
      if (VirtualGPIOBuffer.Buffer <> nil) and (RPi2VirtualGPIOSetBuffer(Address) = ERROR_SUCCESS) then
       begin
        {Update Address}
        VirtualGPIOBuffer.Address:=LongWord(VirtualGPIOBuffer.Buffer);
       end
      else
       begin      
        {Get Buffer}
        Address:=0;
        if RPi2VirtualGPIOGetBuffer(Address) <> ERROR_SUCCESS then Exit;
      
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

function RPi2VirtualGPIOInputGet(Pin:LongWord):LongWord; 
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check Pin}
 if Pin >= BCM2837_VIRTUAL_GPIO_PIN_COUNT then Exit;
 
 {Check Address}
 if VirtualGPIOBuffer.Address = 0 then
  begin
   {Allocate Buffer}
   if not RPi2VirtualGPIOAllocate then Exit;
  end;
 
 {Check Address}
 if VirtualGPIOBuffer.Address > 0 then
  begin
   {Invalidate Cache}
   if VirtualGPIOBuffer.CachedBuffer then InvalidateDataCacheRange(VirtualGPIOBuffer.Address,BCM2837_VIRTUAL_GPIO_PIN_COUNT * SizeOf(LongWord));
   
   {Read Value}
   Result:=PLongWord(VirtualGPIOBuffer.Address + (Pin * SizeOf(LongWord)))^;
   Result:=(Result shr Pin) and 1;
  end;
end;

{==============================================================================}

function RPi2VirtualGPIOOutputSet(Pin,Level:LongWord):LongWord; 
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
   if not RPi2VirtualGPIOAllocate then Exit;
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
       if VirtualGPIOBuffer.CachedBuffer then CleanDataCacheRange(VirtualGPIOBuffer.Address,BCM2837_VIRTUAL_GPIO_PIN_COUNT * SizeOf(LongWord));
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
       if VirtualGPIOBuffer.CachedBuffer then CleanDataCacheRange(VirtualGPIOBuffer.Address,BCM2837_VIRTUAL_GPIO_PIN_COUNT * SizeOf(LongWord));
      end;
    end;
    
   Result:=ERROR_SUCCESS; 
  end;
end;

{==============================================================================}

function RPi2VirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord; 
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
{RPi2 Thread Functions}
procedure RPi2SchedulerInit;
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
     RequestExFIQ(RPI2_CPU_BOOT,BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end
   else
    begin   
     {Physical Non Secure Timer FIQ}
     RequestExFIQ(RPI2_CPU_BOOT,BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end; 
  end
 else
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer IRQ}
     RequestExIRQ(RPI2_CPU_BOOT,BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer IRQ}
     RequestExIRQ(RPI2_CPU_BOOT,BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end; 
  end;

 {Register the Scheduler SWI}
 RegisterSystemCall(SYSTEM_CALL_CONTEXT_SWITCH,RPi2SchedulerSystemCall);
 
 {Setup the Generic Timer}
 State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode}
 State:=State and not(ARMV7_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV7_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv7SetTimerState(ARMV7_CP15_C14_CNTP,State); {Will set Secure or Non Secure depending on current mode}
  
 {Setup the first Scheduler Interrupt}
 RPi2SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[RPI2_CPU_BOOT]);
end;

{==============================================================================}

procedure RPi2SchedulerStart(CPUID:LongWord);
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
     RequestExFIQ(CPUID,BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end
   else
    begin   
     {Physical Non Secure Timer FIQ}
     RequestExFIQ(CPUID,BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end; 
  end
 else
  begin
   if SECURE_BOOT then
    begin
     {Physical Secure Timer IRQ}
     RequestExIRQ(CPUID,BCM2836_IRQ_LOCAL_ARM_CNTPSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end
   else
    begin
     {Physical Non Secure Timer IRQ}
     RequestExIRQ(CPUID,BCM2836_IRQ_LOCAL_ARM_CNTPNSIRQ,nil,RPi2SchedulerInterrupt,nil);
    end; 
  end;
 
 {Setup the Generic Timer}
 State:=ARMv7GetTimerState(ARMV7_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode}
 State:=State and not(ARMV7_CP15_C14_CNT_CTL_IMASK); {Clear the mask bit}
 State:=State or ARMV7_CP15_C14_CNT_CTL_ENABLE;      {Set the enable bit}
 ARMv7SetTimerState(ARMV7_CP15_C14_CNTP,State); {Will set Secure or Non Secure depending on current mode}
 
 {Setup the first Scheduler Interrupt}
 RPi2SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);
end;

{==============================================================================}

procedure RPi2SecondaryBoot(CPUID:LongWord);
var
 Timeout:LongWord;
begin
 {}
 {Check CPU}
 if CPUID > (CPUGetCount - 1) then Exit;

 {Setup Timeout}
 Timeout:=RPI2_LOCAL_MAILBOX_TIMEOUT;
 
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
 ARMLocalRegisters.MailboxWrite[CPUID].Mailbox3Write:=LongWord(@RPi2SecondaryHandler);
 
 {Synchronization Barrier}
 DataSynchronizationBarrier;
 
 {Send Event to Wake CPUs}
 SendEvent;
 
 {Setup Timeout}
 Timeout:=RPI2_LOCAL_MAILBOX_TIMEOUT;
 
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
{RPi2 IRQ Functions}
function RPi2DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending IRQ requests}
{Called by ARMv7IRQHandler in PlatformARMv7}
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
 
 {Check Local IRQ Enabled}
 if IRQLocalEnabled[CPUID] <> 0 then
  begin
   {Check Local IRQ Pending}
   IRQMatch:=(IRQLocalEnabled[CPUID] and ARMLocalRegisters.IRQPending[CPUID]);
   {Check IRQ Match}
   while IRQMatch <> 0 do
    begin
     {Find first set bit}
     IRQBit:=FirstBitSet(IRQMatch);  
       
     {Clear set bit}
     IRQMatch:=IRQMatch xor (1 shl IRQBit);
   
     {Call IRQ Handler}
     Result:=RPi2HandleIRQ(IRQBit + IRQ_LOCAL_START,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
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
           
         {Call IRQ Handler}
         Result:=RPi2HandleIRQ(IRQBit + (Group shl 5),CPUID,Result); {Pass Result as Thread to allow for multiple calls}
        end; 
      end;
    end;  
  end;
end;

{==============================================================================}

function RPi2HandleIRQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for an IRQ that was received, or halt if it doesn't exist}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;
 
 {Check Number}
 if Number < IRQ_LOCAL_START then
  begin
   {Get Entry}
   Entry:=@InterruptEntries[Number];
   
   {Check CPUID}
   if Entry.CPUID = CPUID then
    begin
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
    end
   else
    begin
     {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Incorrect CPUID registered for interrupt ' + IntToStr(Number));
     {$ENDIF} 
     
     Halt;   
    end;
  end
 else
  begin
   {Get Entry}
   Entry:=@LocalInterruptEntries[Number,CPUID];
   
   {Check Local Interrupt Handler}
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
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('No handler registered for local interrupt ' + IntToStr(Number));
       {$ENDIF} 
       
       Halt;   
      end; 
    end;  
  end;  
end;

{==============================================================================}
{==============================================================================}
{RPi2 FIQ Functions}
function RPi2DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending FIQ requests}
{Called by ARMv7FIQHandler in PlatformARMv7}
{Note: A DataMemoryBarrier is executed before and after calling this function} 
var
 FIQBit:LongWord;
 FIQMatch:LongWord;
begin
 {}
 Result:=Thread;
 
 {$IFDEF INTERRUPT_DEBUG}
 Inc(DispatchFastInterruptCounter[CPUID]);
 {$ENDIF}
 
 {Check Local FIQ Enabled}
 if FIQLocalEnabled[CPUID] <> 0 then
  begin
   {Check Local FIQ Pending}
   FIQMatch:=(FIQLocalEnabled[CPUID] and ARMLocalRegisters.FIQPending[CPUID]);
   {Check FIQ Match}
   while FIQMatch <> 0 do
    begin
     {Find first set bit}
     FIQBit:=FirstBitSet(FIQMatch);  
       
     {Clear set bit}
     FIQMatch:=FIQMatch xor (1 shl FIQBit);
   
     {Call FIQ Handler}
     Result:=RPi2HandleFIQ(FIQBit + IRQ_LOCAL_START,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
    end; 
  end;
  
 {Check FIQ Routing}
 if (FIQ_ROUTING = CPUID) or (FIQ_ROUTING = CPU_ID_ALL) then
  begin
   {Check FIQ Enabled}
   if FIQEnabled <> LongWord(-1) then
    begin
     {Call FIQ Handler}
     Result:=RPi2HandleFIQ(FIQEnabled,CPUID,Result); {Pass Result as Thread to allow for multiple calls}
    end;
  end;
end;

{==============================================================================}

function RPi2HandleFIQ(Number,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for an FIQ that was received, or halt if it doesn't exist}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;
 
 {Check Number}
 if Number < IRQ_LOCAL_START then
  begin
   {Get Entry}
   Entry:=@InterruptEntries[Number];

   {Check CPUID}
   if Entry.CPUID = CPUID then
    begin
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
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('No handler registered for fast interrupt ' + IntToStr(Number));
         {$ENDIF} 
         
         Halt;   
        end; 
      end; 
    end
   else
    begin
     {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Incorrect CPUID registered for fast interrupt ' + IntToStr(Number));
     {$ENDIF} 
     
     Halt;   
    end;
  end
 else
  begin
   {Get Entry}
   Entry:=@LocalInterruptEntries[Number,CPUID];
   
   {Check Local Interrupt Handler}
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
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('No handler registered for local fast interrupt ' + IntToStr(Number));
       {$ENDIF} 
       
       Halt;   
      end; 
    end;  
  end;  
end;

{==============================================================================}
{==============================================================================}
{RPi2 SWI Functions}
function RPi2DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 
{Process an SWI request}
{Called by ARMv7SoftwareInterruptHandler in PlatformARMv7}
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
{RPi2 Clock Functions}
procedure RPi2ClockInterrupt(Parameter:Pointer);
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
 RPi2ClockUpdate(CLOCK_CYCLES_PER_TICK,ClockLast);
  
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

procedure RPi2ClockUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a clock interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the timer interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by CLOCK_FREQUENCY}
var
 {$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
 Current:LongInt;
 {$ELSE}
 Current:LongWord;
 {$ENDIF}
begin
 {}
 {$IFNDEF RPI2_CLOCK_SYSTEM_TIMER}
 {Get Timer Value}
 Current:=ARMv7GetTimerValue(ARMV7_CP15_C14_CNTV); 
 
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
 ARMv7SetTimerValue(ARMV7_CP15_C14_CNTV,Last);

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
 TimerRegisters.CS:=BCM2836_SYSTEM_TIMER_CS_3; 
 
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
{RPi2 Scheduler Functions}
function RPi2SchedulerInterrupt(CPUID:LongWord;Thread:TThreadHandle;Parameter:Pointer):TThreadHandle;
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
 RPi2SchedulerUpdate(SCHEDULER_CLOCKS_PER_INTERRUPT,SchedulerLast[CPUID]);
 
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

procedure RPi2SchedulerUpdate(Cycles:LongWord;var Last:LongWord);
{Setup a scheduler interrupt to trigger after the specified number of clock cycles}
{Cycles: Number of cycles after which the scheduler interrupt is to be triggered}
{Note: This refers to native clock cycles as specified by RPI2_GENERIC_TIMER_FREQUENCY}
var
 Current:LongInt;
 {$IFDEF SCHEDULER_DEBUG}
 CurrentCPU:LongWord;
 {$ENDIF}
begin
 {}
 {Get Timer Value} 
 Current:=ARMv7GetTimerValue(ARMV7_CP15_C14_CNTP); {Will get Secure or Non Secure depending on current mode} 
  
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
 ARMv7SetTimerValue(ARMV7_CP15_C14_CNTP,Last); {Will set Secure or Non Secure depending on current mode}
  
 {$IFDEF SCHEDULER_DEBUG}
 CurrentCPU:=CPUGetCurrent;
 SchedulerInterruptOffset[CurrentCPU]:=Last;
 if SchedulerInterruptMinOffset[CurrentCPU] = 0 then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU]; 
 if SchedulerInterruptOffset[CurrentCPU] < SchedulerInterruptMinOffset[CurrentCPU] then SchedulerInterruptMinOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
 if SchedulerInterruptOffset[CurrentCPU] > SchedulerInterruptMaxOffset[CurrentCPU] then SchedulerInterruptMaxOffset[CurrentCPU]:=SchedulerInterruptOffset[CurrentCPU];
 {$ENDIF}
end;

{==============================================================================}

procedure RPi2SchedulerSystemCall(Request:PSystemCallRequest);
{System Call handler for the scheduler. This is registered to receive requests for
 the SYSTEM_CALL_CONTEXT_SWITCH and will perform a context switch from within an SWI}
begin
 {}
 ARMv7ContextSwitchSWI(Pointer(Request.Param1),Pointer(Request.Param2),Request.Param3);
end;

{==============================================================================}
{==============================================================================}
{RPi2 Framebuffer Functions}
{$IFDEF CONSOLE_EARLY_INIT}
function RPi2FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using the Mailbox Property Tags}
var
 Size:LongWord;
 Count:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Defaults:TFramebufferProperties;
 Palette:array[0..255] of LongWord;
 Tag:PBCM2836MailboxTagCreateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
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
      Result:=RPi2FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
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
    Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagCreateBuffer) + SizeOf(TBCM2836MailboxFooter);
    
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
     Header.Code:=BCM2836_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2836MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
     
     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2836_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2836MailboxTagSetPhysical) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;
     
     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2836_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2836MailboxTagSetVirtual) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2836_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2836MailboxTagSetDepth) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;
     
     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2836_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2836MailboxTagSetPixelOrder) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;
     
     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2836_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2836MailboxTagSetAlphaMode) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;
     
     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2836_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2836MailboxTagSetVirtualOffset) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;
     
     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2836_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2836MailboxTagSetOverscan) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;
     
     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2836_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2836MailboxTagAllocateBuffer) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2709FRAMEBUFFER_ALIGNMENT;
     
     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2836_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2836MailboxTagGetPitch) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);
     
     {Setup Footer}
     Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2836_MBOX_TAG_END;
    
     {Call Mailbox} 
     Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
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
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPi2FramebufferDeviceAllocateAlt(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate a framebuffer using a simple Mailbox Call}
var
 Response:LongWord;
 Defaults:TFramebufferProperties;
 MailboxFramebuffer:PBCM2836MailboxFramebuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
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
      Result:=RPi2FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
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
    MailboxFramebuffer:=GetNoCacheAlignedMem(SizeOf(TBCM2836MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
    if MailboxFramebuffer = nil then MailboxFramebuffer:=GetAlignedMem(SizeOf(TBCM2836MailboxFramebuffer),SIZE_16); {Must be 16 byte aligned}
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
     Result:=MailboxCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_FRAMEBUFFER,PhysicalToBusAddress(MailboxFramebuffer),Response);
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
     RPi2FramebufferGetPixelOrder(Framebuffer.Order);
     RPi2FramebufferGetAlphaMode(Framebuffer.Mode);
     RPi2FramebufferGetOverscan(Framebuffer.OverscanTop,Framebuffer.OverscanBottom,Framebuffer.OverscanLeft,Framebuffer.OverscanRight);
      
     {Update Statistics}
     Inc(Framebuffer.AllocateCount);
    
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(MailboxFramebuffer);
    end;    
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

function RPi2FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Release Framebuffer}
    Result:=RPi2FramebufferRelease;
    if Result <> ERROR_SUCCESS then Exit;
     
    {Update Statistics}
    Inc(Framebuffer.ReleaseCount);
     
    {Get Result}
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
   
function RPi2FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Blank}
 if Blank then
  begin
   Result:=RPi2FramebufferSetState(0);
  end
 else
  begin
   Result:=RPi2FramebufferSetState(1);
  end;
end;

{==============================================================================}

function RPi2FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Flags}
 if ((Flags and FRAMEBUFFER_TRANSFER_DMA) = 0) and BCM2709FRAMEBUFFER_CACHED then
  begin
   {Clean Cache}
   CleanAndInvalidateDataCacheRange(Address,Size);
  end;
 
 Result:=ERROR_SUCCESS; 
end;
   
{==============================================================================}

function RPi2FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Set Backlight}
 Result:=FramebufferSetBacklight(Brightness);
end; 
   
{==============================================================================}

function RPi2FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
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
   try
    {Not Supported}
    Result:=ERROR_NOT_SUPPORTED;
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
{RPi2 Helper Functions}
procedure RPi2Wait; assembler; nostackframe; 
asm
 //Wait for a period of time in a loop
 mov r0,#0x9F00000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;

{==============================================================================}

procedure RPi2LongWait; assembler; nostackframe; 
asm
 //Wait for a long period of time in a loop
 ldr r0,=0x3FF00000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;

{==============================================================================}

procedure RPi2ShortWait; assembler; nostackframe; 
asm
 //Wait for a short period of time in a loop
 mov r0,#0x1F0000
.LWait:
 sub r0,#1
 cmp r0,#0
 bne .LWait
end;

{==============================================================================}

procedure RPi2SlowBlink; assembler; nostackframe; 
asm
 //Slow blink the Activity LED in a loop
 bl RPi2ActivityLEDEnable
.LLoop:
 bl RPi2ActivityLEDOn
 bl RPi2Wait
 bl RPi2ActivityLEDOff
 bl RPi2Wait
 b .LLoop
end;

{==============================================================================}

procedure RPi2FastBlink; assembler; nostackframe; 
asm
 //Fast blink the Activity LED in a loop
 bl RPi2ActivityLEDEnable
.LLoop:
 bl RPi2ActivityLEDOn
 bl RPi2ShortWait
 bl RPi2ActivityLEDOff
 bl RPi2ShortWait
 b .LLoop
end;

{==============================================================================}

procedure RPi2BootBlink; assembler; nostackframe;
{Blink the Activity LED without dependency on any other RTL setup}
asm
 //Blink the Activity LED in a loop
 //Enable the Activity LED
 ldr r0,=BCM2836_GPIO_REGS_BASE
 
 //Get the GPIO function select
 ldr r2, [r0,#RPI2_GPIO_ACTLED_GPFSEL]
 
 //Mask of the relevant bits
 mov r1, #RPI2_GPIO_ACTLED_GPFMASK
 lsl r1, #RPI2_GPIO_ACTLED_GPFSHIFT
 bic r2, r1
 
 //Set the 21st bit of r1
 mov r1,#1
 lsl r1,#RPI2_GPIO_ACTLED_GPFSHIFT

 //Add the new bits
 orr r2, r1
 
 //Set the GPIO function select
 str r2,[r0,#RPI2_GPIO_ACTLED_GPFSEL]
 
.LLoop:
 //Turn on the Activity LED
 ldr r0,=BCM2836_GPIO_REGS_BASE 
 
 //Set the 15th bit of r1
 mov r1,#1
 lsl r1,#RPI2_GPIO_ACTLED_GPSHIFT
 
 //Set GPIO 47 to high, causing the LED to turn on
 str r1,[r0,#RPI2_GPIO_ACTLED_GPSET]
 
 //Wait
 //--bl RPi2ShortWait
 bl RPi2Wait
 //--bl RPi2LongWait
 
 //Turn off the Activity LED
 ldr r0,=BCM2836_GPIO_REGS_BASE 
 
 //Set the 15th bit of r1
 mov r1,#1
 lsl r1,#RPI2_GPIO_ACTLED_GPSHIFT
 
 //Set GPIO 47 to low, causing the LED to turn off
 str r1,[r0,#RPI2_GPIO_ACTLED_GPCLR]

 //Wait
 //--bl RPi2ShortWait
 bl RPi2Wait
 //--bl RPi2LongWait
 b .LLoop
end;

{==============================================================================}

function RPi2ConvertPowerIdRequest(PowerId:LongWord):LongWord;
{Convert Ultibo Power Id to BCM2836 Power Id}
begin
 {}
 Result:=BCM2836_MBOX_POWER_DEVID_UNKNOWN;
 
 case PowerId of 
  POWER_ID_MMC0:Result:=BCM2836_MBOX_POWER_DEVID_SDHCI;
  POWER_ID_UART0:Result:=BCM2836_MBOX_POWER_DEVID_UART0;
  POWER_ID_UART1:Result:=BCM2836_MBOX_POWER_DEVID_UART1;
  POWER_ID_USB0:Result:=BCM2836_MBOX_POWER_DEVID_USB_HCD;
  POWER_ID_I2C0:Result:=BCM2836_MBOX_POWER_DEVID_I2C0;
  POWER_ID_I2C1:Result:=BCM2836_MBOX_POWER_DEVID_I2C1;
  POWER_ID_I2C2:Result:=BCM2836_MBOX_POWER_DEVID_I2C2;
  POWER_ID_SPI0:Result:=BCM2836_MBOX_POWER_DEVID_SPI;
  POWER_ID_CCP2TX:Result:=BCM2836_MBOX_POWER_DEVID_CCP2TX;
 end;
end;

{==============================================================================}

function RPi2ConvertPowerStateRequest(PowerState:LongWord):LongWord;
{Convert Ultibo Power State to BCM2836 Power State}
begin
 {}
 Result:=BCM2836_MBOX_SET_POWER_STATE_REQ_OFF;
 
 case PowerState of 
  POWER_STATE_OFF:Result:=BCM2836_MBOX_SET_POWER_STATE_REQ_OFF;
  POWER_STATE_ON:Result:=BCM2836_MBOX_SET_POWER_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPi2ConvertPowerStateResponse(PowerState:LongWord):LongWord;
{Convert BCM2836 Power State to Ultibo Power State}
begin
 {}
 Result:=POWER_STATE_OFF;
 
 case PowerState of 
  BCM2836_MBOX_POWER_STATE_RESP_OFF:Result:=POWER_STATE_OFF;
  BCM2836_MBOX_POWER_STATE_RESP_ON:Result:=POWER_STATE_ON;
 end;
end;

{==============================================================================}

function RPi2ConvertClockIdRequest(ClockId:LongWord):LongWord;
{Convert Ultibo Clock Id to BCM2836 Clock Id}
begin
 {}
 Result:=BCM2836_MBOX_CLOCK_ID_UNKNOWN;
 
 case ClockId of 
  CLOCK_ID_MMC0:Result:=BCM2836_MBOX_CLOCK_ID_EMMC;
  CLOCK_ID_UART0:Result:=BCM2836_MBOX_CLOCK_ID_UART;
  CLOCK_ID_UART1:Result:=BCM2836_MBOX_CLOCK_ID_CORE; {UART1 runs from core clock}
  CLOCK_ID_CPU:Result:=BCM2836_MBOX_CLOCK_ID_ARM;
  CLOCK_ID_CORE:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_GPU:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_V3D:Result:=BCM2836_MBOX_CLOCK_ID_V3D;
  CLOCK_ID_H264:Result:=BCM2836_MBOX_CLOCK_ID_H264;
  CLOCK_ID_ISP:Result:=BCM2836_MBOX_CLOCK_ID_ISP;
  CLOCK_ID_SDRAM:Result:=BCM2836_MBOX_CLOCK_ID_SDRAM;
  CLOCK_ID_PIXEL:Result:=BCM2836_MBOX_CLOCK_ID_PIXEL;
  CLOCK_ID_PWM0:Result:=BCM2836_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_PWM1:Result:=BCM2836_MBOX_CLOCK_ID_PWM;
  CLOCK_ID_I2C0:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C1:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_I2C2:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI0:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI1:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
  CLOCK_ID_SPI2:Result:=BCM2836_MBOX_CLOCK_ID_CORE;
 end; 
end;

{==============================================================================}

function RPi2ConvertClockStateRequest(ClockState:LongWord):LongWord;
{Convert Ultibo Clock State to BCM2836 Clock State}
begin
 {}
 Result:=BCM2836_MBOX_SET_CLOCK_STATE_REQ_OFF;
 
 case ClockState of 
  CLOCK_STATE_OFF:Result:=BCM2836_MBOX_SET_CLOCK_STATE_REQ_OFF;
  CLOCK_STATE_ON:Result:=BCM2836_MBOX_SET_CLOCK_STATE_REQ_ON;
 end;
end;

{==============================================================================}

function RPi2ConvertClockStateResponse(ClockState:LongWord):LongWord;
{Convert BCM2836 Clock State to Ultibo Clock State}
begin
 {}
 Result:=CLOCK_STATE_OFF;
 
 case ClockState of 
  BCM2836_MBOX_CLOCK_STATE_RESP_OFF:Result:=CLOCK_STATE_OFF;
  BCM2836_MBOX_CLOCK_STATE_RESP_ON:Result:=CLOCK_STATE_ON;
 end; 
end;

{==============================================================================}

function RPi2ConvertVoltageIdRequest(VoltageId:LongWord):LongWord;
{Convert Ultibo Voltage Id to BCM2836 Voltage Id}
begin
 {}
 Result:=BCM2836_MBOX_VOLTAGE_ID_RESERVED;
 
 case VoltageId of 
  VOLTAGE_ID_CORE:Result:=BCM2836_MBOX_VOLTAGE_ID_CORE;
  VOLTAGE_ID_SDRAM_C:Result:=BCM2836_MBOX_VOLTAGE_ID_SDRAM_C;
  VOLTAGE_ID_SDRAM_P:Result:=BCM2836_MBOX_VOLTAGE_ID_SDRAM_P;
  VOLTAGE_ID_SDRAM_I:Result:=BCM2836_MBOX_VOLTAGE_ID_SDRAM_I;
 end;
end;

{==============================================================================}

function RPi2ConvertTemperatureIdRequest(TemperatureId:LongWord):LongWord;
{Convert Ultibo Temperature Id to BCM2836 Temperature Id}
begin
 {}
 Result:=BCM2836_MBOX_TEMP_ID_SOC;
 
 case TemperatureId of 
  TEMPERATURE_ID_SOC:Result:=BCM2836_MBOX_TEMP_ID_SOC;
 end;
end;

{==============================================================================}
{==============================================================================}

end.
