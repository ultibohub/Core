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
     DMA,
     //PL080,        {ARM PrimeCell PL080 DMA driver} //To Do //Continuing
     PL011,        {ARM PrimeCell PL011 UART driver}
     MMC,
     //PL180,        {ARM PrimeCell PL180 SDHCI driver}  //To Do //Continuing
     USB,
     //OHCI,         {USB OHCI Controller driver}  //To Do //Continuing
     //SMSC91C11,    {SMC LAN91C11 Network driver} //To Do //Continuing
     Framebuffer,
     PL110,        {ARM PrimeCell PL110 Color LCD driver}
     Console,
     Keyboard,
     Mouse,
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
 //To Do //Continuing 
 QEMUVPB_REGISTER_UART0:LongBool = True;       {If True then register the QEMU VersatilePB UART0 device during boot}
 //To Do //Continuing 
 QEMUVPB_REGISTER_FRAMEBUFFER:LongBool = True; {If True then register the QEMU VersatilePB Framebuffer device during boot}
 
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
 
 {Check Environment Variables}
 //To Do //Continuing 

 {Check DMA}
 //To Do //Continuing
 
 {Check UART0}
 if QEMUVPB_REGISTER_UART0 then
  begin
   {Create UART0}
   PL011UARTCreate(VERSATILEPB_UART0_REGS_BASE,'',VERSATILEPB_IRQ_UART0,PL011_UART_CLOCK_RATE);
  end;
  
 //To Do //Continuing //More devices //MMC, SMSC91C11, Mouse, Keyboard 
 
 //To Do //Clock and Timer devices
 
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
