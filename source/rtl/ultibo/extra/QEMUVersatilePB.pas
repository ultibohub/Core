{
Ultibo QEMU VersatilePB unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A8)

Boards
======

 QEMU - VersatilePB

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:


References
==========


QEMU VersatilePB
================

 This unit serves to include all units relevant to the QEMU VersatilePB and to register the
 drivers that are appropriate to the configuration.

 This includes standard interfaces such as network, filesystem and storage as well as drivers
 that are specific to the VersatilePB and may or may not be included by anything else.

 The unit also provides a very simple clock device driver for the 24MHz clock included in the
 Versatile PB System Control registers. This clock is 32bit only, runs at a fixed rate of 24MHz
 and cannot be stopped or reset.

 Additional units can be included anywhere within a program and they will be linked during the
 compile process. This unit simply provides a convenient way to ensure all relevant units have
 been included and the standard drivers registered.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit QEMUVersatilePB;

interface

uses GlobalConfig,
     GlobalConst,
     GlobalTypes,
     VersatilePB,
     Platform,
     Threads,
     PlatformQEMUVPB,
     Devices,
     DMA,
     //PL08X,        {ARM PrimeCell PL080/PL081 DMA driver} //To Do //Continuing
     Serial,
     UART,
     PL011,        {ARM PrimeCell PL011 UART driver}
     RTC,
     PL031,        {ARM PrimeCell PL031 Real Time Clock driver}
     MMC,
     PL18X,        {ARM PrimeCell PL181 SDHCI driver}
     USB,
     //OHCI,         {USB OHCI Controller driver}  //To Do //Continuing
     USBStorage,
     Network,
     SMC91X,       {SMC LAN91C11 Network driver}
     Framebuffer,
     PL110,        {ARM PrimeCell PL110 Color LCD driver}
     Console,
     Keyboard,
     Mouse,
     PS2,
     PL050,        {ARM PrimeCell PL050 PS2 Keyboard/Mouse driver}
     HID,
     USBHID,
     HIDKeyboard,
     HIDMouse,
     Filesystem,
     EXTFS,
     FATFS,
     NTFS,
     CDFS,
     VirtualDisk,
     Logging,
     Sockets,
     Winsock2,
     Services,
     SysUtils;


{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {QEMUVersatilePB specific constants}
 VERSATILEPB_CLOCK_DESCRIPTION = 'VersatilePB 24MHz Clock';  {Description of 24MHz clock device}

{==============================================================================}
{var}
 {QEMUVersatilePB specific variables}

{==============================================================================}
{Initialization Functions}
procedure QEMUVersatilePBInit;

{==============================================================================}
{QEMUVersatilePB Functions}

{==============================================================================}
{QEMUVersatilePB Clock Functions}
function VersatilePBClockRead(Clock:PClockDevice):LongWord;
function VersatilePBClockRead64(Clock:PClockDevice):Int64;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {QEMUVersatilePB specific variables}
 QEMUVersatilePBInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{QEMUVersatilePB forward declarations}
function MMCIGetCardDetect(MMC:PMMCDevice):LongWord; forward;
function MMCIGetWriteProtect(MMC:PMMCDevice):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure QEMUVersatilePBInit;
{Initialize the QEMUVersatilePB unit and parameters}

{Note: Called only during system startup}
var
 Status:LongWord;
 VersatilePBClock:PClockDevice;
begin
 {}
 {Check Initialized}
 if QEMUVersatilePBInitialized then Exit;

 {Initialize Peripherals (Using information from QEMUVPBPeripheralInit)}
 //PL08XDMA_ALIGNMENT:=DMA_ALIGNMENT;   //To Do //Continuing
 //PL08XDMA_MULTIPLIER:=DMA_MULTIPLIER;
 //PL08XDMA_SHARED_MEMORY:=DMA_SHARED_MEMORY;
 //PL08XDMA_NOCACHE_MEMORY:=DMA_NOCACHE_MEMORY;
 //PL08XDMA_BUS_ADDRESSES:=DMA_BUS_ADDRESSES;
 //PL08XDMA_CACHE_COHERENT:=DMA_CACHE_COHERENT;

 {Initialize Peripherals}
 PL18X_MMCI_MAX_FREQ:=VERSATILEPB_SYS_24MHZ_FREQUENCY; {Connected to the 24MHz reference clock}

 {Check Environment Variables}
 //To Do //Continuing

 {Check DMA}
 if QEMUVPB_REGISTER_DMA then
  begin
   {Create DMA}
   //PL080DMACreate(VERSATILEPB_DMAC_REGS_BASE,'',VERSATILEPB_IRQ_DMA); //To Do //Continuing
  end;

 {Check UART0}
 if QEMUVPB_REGISTER_UART0 then
  begin
   {Create UART0}
   PL011UARTCreate(VERSATILEPB_UART0_REGS_BASE,PL011_UART_DESCRIPTION + ' (UART0)',VERSATILEPB_IRQ_UART0,PL011_UART_CLOCK_RATE);
  end;

 {Check UART1}
 if QEMUVPB_REGISTER_UART1 then
  begin
   {Create UART1}
   PL011UARTCreate(VERSATILEPB_UART1_REGS_BASE,PL011_UART_DESCRIPTION + ' (UART1)',VERSATILEPB_IRQ_UART1,PL011_UART_CLOCK_RATE);
  end;

 {Check UART2}
 if QEMUVPB_REGISTER_UART2 then
  begin
   {Create UART2}
   PL011UARTCreate(VERSATILEPB_UART2_REGS_BASE,PL011_UART_DESCRIPTION + ' (UART2)',VERSATILEPB_IRQ_UART2,PL011_UART_CLOCK_RATE);
  end;

 {Check UART3}
 if QEMUVPB_REGISTER_UART3 then
  begin
   {Create UART3}
   PL011UARTCreate(VERSATILEPB_UART3_REGS_BASE,PL011_UART_DESCRIPTION + ' (UART3)',VERSATILEPB_IRQ_SIC_UART3,PL011_UART_CLOCK_RATE);
  end;

 {Check RTC}
 if QEMUVPB_REGISTER_RTC then
  begin
   {Create RTC}
   PL031RTCCreate(VERSATILEPB_RTC_REGS_BASE,'',VERSATILEPB_IRQ_RTC);
  end;

 {Check Clock}
 if QEMUVPB_REGISTER_CLOCK then
  begin
   {Create Clock}
   VersatilePBClock:=PClockDevice(ClockDeviceCreateEx(SizeOf(TClockDevice)));
   if VersatilePBClock <> nil then
    begin
     {Update Clock}
     {Device}
     VersatilePBClock.Device.DeviceBus:=DEVICE_BUS_MMIO;
     VersatilePBClock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     VersatilePBClock.Device.DeviceFlags:=CLOCK_FLAG_NONE;
     VersatilePBClock.Device.DeviceData:=nil;
     VersatilePBClock.Device.DeviceDescription:=VERSATILEPB_CLOCK_DESCRIPTION;
     {Clock}
     VersatilePBClock.ClockState:=CLOCK_STATE_DISABLED;
     VersatilePBClock.DeviceRead:=VersatilePBClockRead;
     VersatilePBClock.DeviceRead64:=VersatilePBClockRead64;
     {Driver}
     VersatilePBClock.Address:=Pointer(VERSATILEPB_SYS_24MHZ);
     VersatilePBClock.Rate:=VERSATILEPB_SYS_24MHZ_FREQUENCY;
     VersatilePBClock.MinRate:=VERSATILEPB_SYS_24MHZ_FREQUENCY;
     VersatilePBClock.MaxRate:=VERSATILEPB_SYS_24MHZ_FREQUENCY;

     {Register Clock}
     Status:=ClockDeviceRegister(VersatilePBClock);
     if Status = ERROR_SUCCESS then
      begin
       {Start Clock}
       Status:=ClockDeviceStart(VersatilePBClock);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'QEMUVPB: Failed to start new clock device: ' + ErrorToString(Status));

         {Deregister Clock}
         ClockDeviceDeregister(VersatilePBClock);

         {Destroy Clock}
         ClockDeviceDestroy(VersatilePBClock);
        end;
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'QEMUVPB: Failed to register new clock device: ' + ErrorToString(Status));

       {Destroy Clock}
       ClockDeviceDestroy(VersatilePBClock);
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'QEMUVPB: Failed to create new clock device');
    end;
  end;

 //To Do //Continuing  //Timer devices

 {Check MMC0}
 if QEMUVPB_REGISTER_MMC0 then
  begin
   PL181SDHCICreate(VERSATILEPB_MMCI0_REGS_BASE,PL181_MMCI_DESCRIPTION + ' (MMC0)',VERSATILEPB_IRQ_MMCI0A,VERSATILEPB_IRQ_SIC_MMCI0B,PL18X_MMCI_MIN_FREQ,PL18X_MMCI_MAX_FREQ);
  end;

 {Check MMC1}
 if QEMUVPB_REGISTER_MMC1 then
  begin
   PL181SDHCICreate(VERSATILEPB_MMCI1_REGS_BASE,PL181_MMCI_DESCRIPTION + ' (MMC1)',VERSATILEPB_IRQ_MMCI1A,VERSATILEPB_IRQ_SIC_MMCI1B,PL18X_MMCI_MIN_FREQ,PL18X_MMCI_MAX_FREQ);
  end;

 {Check Network}
 if QEMUVPB_REGISTER_NETWORK then
  begin
   {Create Network}
   SMC91XNetworkCreate(VERSATILEPB_ETH_REGS_BASE,'',VERSATILEPB_IRQ_ETH);
  end;

 {Check Keyboard}
 if QEMUVPB_REGISTER_KEYBOARD then
  begin
   {Create Keyboard}
   PL050KeyboardCreate(VERSATILEPB_KMI0_REGS_BASE,'',VERSATILEPB_IRQ_SIC_KMI0,PL050_KEYBOARD_CLOCK_RATE);
  end;

 {Check Mouse}
 if QEMUVPB_REGISTER_MOUSE then
  begin
   {Create Mouse}
   PL050MouseCreate(VERSATILEPB_KMI1_REGS_BASE,'',VERSATILEPB_IRQ_SIC_KMI1,PL050_MOUSE_CLOCK_RATE);
  end;

 {$IFNDEF CONSOLE_EARLY_INIT}
 {Check Framebuffer}
 if QEMUVPB_REGISTER_FRAMEBUFFER then
  begin
   {Check Defaults}
   if (FRAMEBUFFER_DEFAULT_WIDTH = 0) or (FRAMEBUFFER_DEFAULT_HEIGHT = 0) then
    begin
     FRAMEBUFFER_DEFAULT_WIDTH:=1024;
     FRAMEBUFFER_DEFAULT_HEIGHT:=768;
    end;

   {Check Defaults}
   if (FRAMEBUFFER_DEFAULT_DEPTH <> FRAMEBUFFER_DEPTH_16) and (FRAMEBUFFER_DEFAULT_DEPTH <> FRAMEBUFFER_DEPTH_32) then
    begin
     FRAMEBUFFER_DEFAULT_DEPTH:=FRAMEBUFFER_DEPTH_16;
    end;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Setup CLCD Mode}
   PLongWord(VERSATILEPB_SYS_CLCD)^:=(PLongWord(VERSATILEPB_SYS_CLCD)^ and not(VERSATILEPB_SYS_CLCD_MODEMASK)) or VERSATILEPB_SYS_CLCD_MODE565RGB;

   {Start CLCD Clock}
   //To Do //Continuing

   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}

   {Create Framebuffer}
   PL110FramebufferCreateSVGA(VERSATILEPB_CLCD_REGS_BASE,'',FRAMEBUFFER_DEFAULT_ROTATION,FRAMEBUFFER_DEFAULT_WIDTH,FRAMEBUFFER_DEFAULT_HEIGHT,FRAMEBUFFER_DEFAULT_DEPTH);
  end;
 {$ENDIF}

 {Create Timer for ClockGetTotal (Every 60 seconds)}
 ClockGetTimer:=TimerCreateEx(60000,TIMER_STATE_ENABLED,TIMER_FLAG_RESCHEDULE,TTimerEvent(QEMUVPBClockGetTimer),nil); {Rescheduled Automatically}

 QEMUVersatilePBInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{QEMUVersatilePB Functions}

{==============================================================================}
{==============================================================================}
{QEMUVersatilePB MMCI Functions}
function MMCIGetCardDetect(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceGetCardDetect API for QEMUVPB MMCI}
{Note: Not intended to be called directly by applications, use MMCDeviceGetCardDetect instead}

{Note: Not currently used as the VERSATILEPB_SYS_MCI register is not connected in QEMUVPB}
var
 Mask:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: MMC Device Get Card Detect');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check MMC State}
 if MMC.MMCState = MMC_STATE_INSERTED then
  begin
   {Get Card Status}
   if MMCDeviceSendCardStatus(MMC) <> MMC_STATUS_SUCCESS then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);

     {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;
  end
 else
  begin
   {Check Address}
   if PtrUInt(SDHCI.Address) = VERSATILEPB_MMCI0_REGS_BASE then
    begin
     Mask:=VERSATILEPB_SYS_MCI_CD0;
    end
   else if PtrUInt(SDHCI.Address) = VERSATILEPB_MMCI1_REGS_BASE then
    begin
     Mask:=VERSATILEPB_SYS_MCI_CD1;
    end
   else
    begin
     Mask:=0;
    end;

   {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: VERSATILEPB_SYS_MCI=' + IntToHex(PLongWord(VERSATILEPB_SYS_MCI)^,8));
   {$ENDIF}

   {Read Status}
   if (PLongWord(VERSATILEPB_SYS_MCI)^ and Mask) <> 0 then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);

     {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: Get Card Detect (Flags=MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end
   else
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);

     {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;

   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
  end;

 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}

function MMCIGetWriteProtect(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceGetWriteProtect API for QEMUVPB MMCI}
{Note: Not intended to be called directly by applications, use MMCDeviceGetWriteProtect instead}

{Note: Not currently used as the VERSATILEPB_SYS_MCI register is not connected in QEMUVPB}
var
 Mask:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: MMC Device Get Write Protect');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check MMC State}
 if MMC.MMCState = MMC_STATE_INSERTED then
  begin
   {Check Address}
   if PtrUInt(SDHCI.Address) = VERSATILEPB_MMCI0_REGS_BASE then
    begin
     Mask:=VERSATILEPB_SYS_MCI_WP0;
    end
   else if PtrUInt(SDHCI.Address) = VERSATILEPB_MMCI1_REGS_BASE then
    begin
     Mask:=VERSATILEPB_SYS_MCI_WP1;
    end
   else
    begin
     Mask:=0;
    end;

   {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: VERSATILEPB_SYS_MCI=' + IntToHex(PLongWord(VERSATILEPB_SYS_MCI)^,8));
   {$ENDIF}

   {Read Status}
   if (PLongWord(VERSATILEPB_SYS_MCI)^ and Mask) <> 0 then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_WRITE_PROTECT);

     {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: Get Card Detect (Flags=MMC_FLAG_WRITE_PROTECT)');
     {$ENDIF}
    end
   else
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_WRITE_PROTECT);

     {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'QEMUVPB: Get Card Detect (Flags=not MMC_FLAG_WRITE_PROTECT)');
     {$ENDIF}
    end;

   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
  end;

 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{QEMUVersatilePB Clock Functions}
function VersatilePBClockRead(Clock:PClockDevice):LongWord;
{Implementation of ClockDeviceRead API for the 24MHz Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead instead}
begin
 {}
 Result:=0;

 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;

 {Read Clock}
 Result:=PLongWord(Clock.Address)^;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Update Statistics}
 Inc(Clock.ReadCount);

 MutexUnlock(Clock.Lock);
end;

{==============================================================================}

function VersatilePBClockRead64(Clock:PClockDevice):Int64;
{Implementation of ClockDeviceRead64 API for the 24MHz Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead64 instead}
begin
 {}
 Result:=0;

 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;

 {Read Clock}
 Result:=PLongWord(Clock.Address)^;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Update Statistics}
 Inc(Clock.ReadCount);

 MutexUnlock(Clock.Lock);
end;

{==============================================================================}
{==============================================================================}

initialization
 QEMUVersatilePBInit;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
