{
Ultibo Raspberry Pi 4 unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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

 
References
==========

 

Raspberry Pi 4
==============
 
 This unit has no functionality other than to include all units relevant to the Raspberry Pi 4.
 
 This includes standard interfaces such as network, filesystem and storage as well as drivers
 that are specific to the BCM2838 / BCM2711 and are not included by anything else.
 
 Additional units can be included anywhere within a program and they will be linked during the
 compile process. This unit simply provides a convenient way to ensure all relevant units have
 been included.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit RaspberryPi4;

interface

uses GlobalConfig,
     GlobalConst,
     GlobalTypes,
     BCM2838,
     Platform,
     Threads,
     MMC,
     BCM2711,
     BCMSDHOST,
     USB,
     PCI,
     XHCI,
     DWCOTG,
     GENET,
     //BRCMSTBPCIe, //To Do //PCIe Host driver
     RPiGPIOExpander,
     Framebuffer,
     Console,
     Keyboard,
     Mouse,
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
{Initialization Functions}
procedure RaspberryPi4Init;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RaspberryPi4 specific variables}
 RaspberryPi4Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RaspberryPi4Init;
{Initialize the RaspberryPi4 unit and parameters}

{Note: Called only during system startup}
var
 BoardType:LongWord;
 ClockMaximum:LongWord;
begin
 {}
 {Check Initialized}
 if RaspberryPi4Initialized then Exit;
 
 {Get Board Type}
 BoardType:=BoardGetType;

 {Check SDHOST}
 if BCM2711_REGISTER_EMMC1 then
  begin
   {Set Parameters}
   ClockMaximum:=ClockGetRate(CLOCK_ID_MMC1);
   if ClockMaximum = 0 then ClockMaximum:=BCM2711_EMMC1_MAX_FREQ;
   
   {Create Device}
   BCMSDHOSTCreate(BCM2838_EMMC1_REGS_BASE,BCM2711_EMMC1_DESCRIPTION,BCM2838_IRQ_EMMC1,DMA_DREQ_ID_EMMC1,BCM2711_EMMC1_MIN_FREQ,ClockMaximum,GPIO_PIN_22,GPIO_PIN_27,GPIO_FUNCTION_ALT0,BCM2711EMMC1_FIQ_ENABLED);
  end;
 
 {Check RTC}
 if BCM2711_REGISTER_RTC then
  begin
   {Set Parameters}
   //PCF85063_CHIP_TYPE:=PCF85063_CHIP_PCF85063A; //To Do
   //PCF85063_I2C_ADDRESS:=$51; //To Do
   //PCF85063_I2C_DEVICE:='I2C0'; //To Do
   
   {Create Device}
   //PCF85063RTCCreate(PCF85063_I2C_DEVICE,PCF85063_I2C_ADDRESS,PCF85063_CHIP_TYPE); //To Do
  end;
  
 {Check PCI}
 if BCM2711_REGISTER_PCI then
  begin
   {Set Parameters}
   //To Do
 
   {Create Host}
   //BRCMSTBHostCreate(BCM2838_PCIE_REGS_BASE,BCM2838_IRQ_PCIE_MSI); //To Do
  end;
  
 {Check PCI XHCI}
 if BCM2711_REGISTER_PCI_XHCI then
  begin
   {Set Parameters}
   //To Do
  
   {Create Host}
   //To Do
  end;
  
 {Check Internal XHCI}
 if BCM2711_REGISTER_INTERNAL_XHCI then
  begin
   {Set Parameters}
   //To Do
  
   {Create Host}
   //To Do
  end;
 
 {Check Network}
 if BCM2711_REGISTER_NETWORK then
  begin
   {Set Parameters}
   GENET_PHY_MODE:='rgmii-rxid';
   GENET_PHY_ADDR:=1;
   if BoardType = BOARD_TYPE_RPI_COMPUTE4 then GENET_PHY_ADDR:=0;
   GENET_NO_PHY_INTERRUPT:=True;
   
   {Create Network}
   GENETNetworkCreate(BCM2838_GENET_REGS_BASE,BCM2838_GENET_MDIO_OFFSET,BCM2838_IRQ_GENET_0,BCM2838_IRQ_GENET_1);
  end;
 
 RaspberryPi4Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
 
initialization
 RaspberryPi4Init;
 
{==============================================================================}
 
{finalization}
 {Nothing}
 
end.
