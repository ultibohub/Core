{
Ultibo QEMU VersatilePB unit.

Copyright (C) 2016 - SoftOz Pty Ltd.

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
     DMA,
     //PL08X,        {ARM PrimeCell PL080/PL081 DMA driver} //To Do //Continuing
     Serial,
     UART,
     PL011,        {ARM PrimeCell PL011 UART driver}
     RTC,
     PL031,        {ARM PrimeCell PL031 Real Time Clock driver}
     MMC,
     //PL180,        {ARM PrimeCell PL180 SDHCI driver}  //To Do //Continuing
     USB,
     //OHCI,         {USB OHCI Controller driver}  //To Do //Continuing
     Network,
     SMC91X,       {SMC LAN91C11 Network driver}
     Framebuffer,
     PL110,        {ARM PrimeCell PL110 Color LCD driver}
     Console,
     Keyboard,
     Mouse,
     PS2,
     PL050,        {ARM PrimeCell PL050 PS2 Keyboard/Mouse driver}
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
var
 {QEMUVersatilePB specific variables}
 QEMUVPB_REGISTER_DMA:LongBool = True;         {If True then register the QEMU VersatilePB DMA device during boot}
 QEMUVPB_REGISTER_UART0:LongBool = True;       {If True then register the QEMU VersatilePB UART0 device during boot}
 QEMUVPB_REGISTER_RTC:LongBool = True;         {If True then register the QEMU VersatilePB RTC device during boot}
 QEMUVPB_REGISTER_MMC0:LongBool = True;        {If True then register the QEMU VersatilePB MMC0 device during boot}
 QEMUVPB_REGISTER_MMC1:LongBool = True;        {If True then register the QEMU VersatilePB MMC1 device during boot}
 QEMUVPB_REGISTER_NETWORK:LongBool = True;     {If True then register the QEMU VersatilePB Network device during boot}
 QEMUVPB_REGISTER_FRAMEBUFFER:LongBool = True; {If True then register the QEMU VersatilePB Framebuffer device during boot}
 QEMUVPB_REGISTER_MOUSE:LongBool = True;       {If True then register the QEMU VersatilePB Mouse device during boot}
 QEMUVPB_REGISTER_KEYBOARD:LongBool = True;    {If True then register the QEMU VersatilePB Keyboard device during boot}
 
{==============================================================================}
{Initialization Functions}
procedure QEMUVersatilePBInit;

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
{Initialization Functions}
procedure QEMUVersatilePBInit;
{Initialize the QEMUVersatilePB unit and parameters}

{Note: Called only during system startup}
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
   PL011UARTCreate(VERSATILEPB_UART0_REGS_BASE,'',VERSATILEPB_IRQ_UART0,PL011_UART_CLOCK_RATE);
  end;
  
 {Check RTC}
 if QEMUVPB_REGISTER_RTC then
  begin
   {Create RTC}
   PL031RTCCreate(VERSATILEPB_RTC_REGS_BASE,'',VERSATILEPB_IRQ_RTC);
  end; 

 //To Do //Continuing  //Clock and Timer devices
 
 {Check MMC0}
 if QEMUVPB_REGISTER_MMC0 then
  begin
   //To Do //Continuing
  end;
  
 {Check MMC1}
 if QEMUVPB_REGISTER_MMC1 then
  begin
   //To Do //Continuing
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
 
initialization
 QEMUVersatilePBInit;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}
 
end.
