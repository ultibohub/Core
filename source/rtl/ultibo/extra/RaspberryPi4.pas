{
Ultibo Raspberry Pi 4 unit.

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
     USBStorage,
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
 Path:String;
 Buses:PPCIBuses;
 Ranges:PPCIRange;
 Windows:PPCIWindow;
 BoardType:LongWord;
begin
 {}
 {Check Initialized}
 if RaspberryPi4Initialized then Exit;
 
 {Get Board Type}
 BoardType:=BoardGetType;

 {Check SDHOST}
 {Note: SDHOST initialization moved to BCM2711Init}
 
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
  
 BCM2711_REGISTER_PCI:=False; //To Do
 
 {Check PCI}
 if BCM2711_REGISTER_PCI then
  begin
   {Get PCI Alias}
   DeviceTreeReadString('/aliases','pcie0',Path);
   
   {Parse Bus Range}
   PCIParseDeviceTreeBuses(Path,Buses);
   
   {Parse DMA Ranges}
   if not PCIParseDeviceTreeDMARanges(Path,Ranges) then 
    begin
     {Use Defaults}
     {Inbound DMA Range (CPU 0x00000000 to PCI 0x400000000 for up to 4GB)}
     Ranges:=AllocMem(SizeOf(TPCIRange));
     Ranges.PCIAddress:=BCM2838_PCI_DMA_RANGE_BASE;
     Ranges.CPUAddress:=BCM2838_PCI_INBOUND_BASE;
     Ranges.Size:=BCM2838_PCI_INBOUND_SIZE;
     Ranges.Flags:=PCI_RANGE_FLAG_MEMORY32;
     
     {Limit the DMA range to 3GB on older chip revisions}
     if ((ChipGetRevision and $FF) < $20) then Ranges.Size:=SIZE_1G + SIZE_2G;
     
     {Limit the DMA range to the maximum memory on the board}
     if MEMORY_SIZE < Ranges.Size then Ranges.Size:=MEMORY_SIZE;
    end;
   
   {Parse Outbound Windows}
   if PCIParseDeviceTreeWindows(Path,Windows) then 
    begin
     {Adjust Outbound Windows}
     if PCICountOutboundWindows(Windows) = 1 then
      begin
       {Check PCI Address}
       if Windows.PCIAddress < BCM2838_PCI_OUTBOUND_BASE then
        begin
         {Ensure Window is not lower than 0xF8000000}
         Windows.PCIAddress:=BCM2838_PCI_OUTBOUND_BASE;
        end;
       {Check Window Size}
       if Windows.Size > BCM2838_PCI_OUTBOUND_SIZE then
        begin
         {Ensure Window is not larger than 64MB}
         Windows.Size:=BCM2838_PCI_OUTBOUND_SIZE;
        end;
      end;
    end
   else 
    begin
     {Use Defaults}
     {Outbound Window (CPU 0x600000000 to PCI 0xF8000000 for 64MB}
     Windows:=AllocMem(SizeOf(TPCIWindow));
     Windows.PCIAddress:=BCM2838_PCI_OUTBOUND_BASE;
     Windows.CPUAddress:=BCM2838_PCI_ADDRESS_BASE;
     Windows.Size:=BCM2838_PCI_OUTBOUND_SIZE;
     Windows.Flags:=PCI_RANGE_FLAG_MEMORY32;
    end;
   
   {Map Outbound Windows}
   PCIMapOutboundWindows(Windows);

   {Set Parameters}
   //brcm,enable-ssc (from DeviceTree)
   //brcm,enable-l1ss (from DeviceTree)
   //max-link-speed (from DeviceTree)
   //aspm-no-l0s (from DeviceTree)
   //msi-parent
   //To Do
   
   {Create Host}
   //BRCMSTBHostCreate(BCM2838_PCIE_REGS_BASE,BCM2838_IRQ_PCIE_MSI,BRCMSTB_MODEL_BCM2711,Buses,Ranges,Windows); //To Do
  end;
  
 {Check XHCI (Internal)}
 if BCM2711_REGISTER_XHCI then
  begin
   {Set Parameters}
   //To Do
   //Power On USB if not a Pi4B ?
   //Pass to XHCIHostCreate as POWER_ID_USB0 etc ?
  
   {Create Host}
   //XHCIHostCreate(BCM2838_XHCI_REGS_BASE,BCM2838_IRQ_XHCI,0); //To Do 
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
