{
USB CDC Ethernet Driver.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\net\usb\cdc_ether.c - Copyright (C) 2006 by Ole Andre Vadla Ravnas (ActiveSync)

  Linux - \drivers\net\usb\usbnet.c - Copyright (C) 2000-2005 by David Brownell

References
==========

  USB CDC - https://en.wikipedia.org/wiki/USB_communications_device_class
            http://www.usb.org/developers/docs/devclass_docs/CDC1.2_WMC1.1_012011.zip

USB CDC Ethernet
================

 The Ethernet model is part of the USB Communications Device Class (CDC) standard which allows
 ethernet based networking devices to be supported by USB with a standardized protocol for data
 transfer and link status.

 A CDC Ethernet device is defined by information in the interface descriptors and is not specific
 to any product and vendor ID. There are numerous devices that support the CDC Ethernet protocol
 including a number of external adapters and combination hubs.

 This driver presents any device recognized as a CDC Ethernet as a standard network device that
 can be accessed using the API in the Network, Winsock, Winsock2 and Sockets units. It should not
 be necessary to directly call any of the functions in this unit from application code.

 Note: The QEMU emulations of the Raspberry Pi Zero/A+/2B/3A+/3B use a USB CDC Ethernet device.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit USBCDCEthernet;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  USB,
  USBCDC,
  Network,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {CDC Ethernet specific constants}
 CDCETHERNET_DRIVER_NAME = 'USB CDC Ethernet Driver'; {Name of CDC Ethernet driver}

 CDCETHERNET_NETWORK_DESCRIPTION = 'USB CDC Ethernet Adapter'; {Description of CDC Ethernet device}

 CDCETHERNET_MAX_QUEUE_MEMORY = 60 * ETHERNET_MAX_PACKET_SIZE;

 CDCETHERNET_RX_REQUEST_SIZE = SIZE_2K;
 CDCETHERNET_TX_REQUEST_SIZE = ETHERNET_MAX_PACKET_SIZE;

 {Driver Info constants}
 DRIVER_INFO_NONE    = 0; {Blacklisted Device}
 DRIVER_INFO_CDC     = 1; {CDC Ethernet Device}
 DRIVER_INFO_ZTE_CDC = 2; {ZTE CDC Ethernet Device}
 DRIVER_INFO_WWAN    = 3; {Mobile Broadband Network Device}

 {Vendor Id constants}
 HUAWEI_VENDOR_ID    = $12D1;
 NOVATEL_VENDOR_ID   = $1410;
 ZTE_VENDOR_ID       = $19D2;
 DELL_VENDOR_ID      = $413C;
 REALTEK_VENDOR_ID   = $0bda;
 SAMSUNG_VENDOR_ID   = $04e8;
 LENOVO_VENDOR_ID    = $17ef;
 LINKSYS_VENDOR_ID   = $13b1;
 NVIDIA_VENDOR_ID    = $0955;
 HP_VENDOR_ID        = $03f0;
 MICROSOFT_VENDOR_ID = $045e;
 UBLOX_VENDOR_ID     = $1546;
 TPLINK_VENDOR_ID    = $2357;
 AQUANTIA_VENDOR_ID  = $2eca;
 ASIX_VENDOR_ID      = $0b95;

 {GUID for Ericsson MBM devices}
 MBM_GUID:array[0..15] of Byte = ($a3, $17, $a8, $8b, $04, $5e, $4f, $01, $a6, $07, $c0, $ff, $cb, $7e, $39, $2a);

type
 {CDC Ethernet Device and Interface Id type}
 PCDCEthernetDeviceId = ^TCDCEthernetDeviceId;
 TCDCEthernetDeviceId = record
  idVendor:Word;
  idProduct:Word;
  bInterfaceClass:Byte;
  bInterfaceSubClass:Byte;
  bInterfaceProtocol:Byte;
  DriverInfo:LongWord;
 end;

const
 {CDC Ethernet Device and Interface Id constants}
 CDCETHERNET_DEVICE_ID_COUNT = 57; {Number of supported Device and Interface Ids}

 CDCETHERNET_DEVICE_ID:array[0..CDCETHERNET_DEVICE_ID_COUNT - 1] of TCDCEthernetDeviceId = (
  {Blacklisted devices}
  (idVendor:$04DD;               idProduct:$8004; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {SA-1100 based Sharp Zaurus}

  (idVendor:$04DD;               idProduct:$8005; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {PXA-25x based Sharp Zaurii (A-300)}
  (idVendor:$04DD;               idProduct:$8006; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {(B-500/SL-5600)}
  (idVendor:$04DD;               idProduct:$8007; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {(C-700)}
  (idVendor:$04DD;               idProduct:$9031; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {(C-750 C-760)}
  (idVendor:$04DD;               idProduct:$9032; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {(SL-6000)}
  (idVendor:$04DD;               idProduct:$9050; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {(C-860)}
  (idVendor:$07B4;               idProduct:$0F02; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {(R-1000)}

  (idVendor:$1004;               idProduct:$61aa; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {LG Electronics VL600}
  (idVendor:$046d;               idProduct:$c11f; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_MDLM;     bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Logitech Harmony 900}
  (idVendor:NOVATEL_VENDOR_ID;   idProduct:$B001; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Novatel USB551L and MC551}
  (idVendor:NOVATEL_VENDOR_ID;   idProduct:$9010; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Novatel E362}
  (idVendor:DELL_VENDOR_ID;      idProduct:$8195; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Dell Wireless 5800 (Novatel E362)}
  (idVendor:DELL_VENDOR_ID;      idProduct:$8196; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Dell Wireless 5800 (Novatel E362)}
  (idVendor:DELL_VENDOR_ID;      idProduct:$819b; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Dell Wireless 5804 (Novatel E371)}
  (idVendor:NOVATEL_VENDOR_ID;   idProduct:$9011; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Novatel Expedite E371}
  (idVendor:HP_VENDOR_ID;        idProduct:$421d; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {HP lt2523 (Novatel E371)}
  (idVendor:$16d5;               idProduct:$650a; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {AnyDATA ADU960S}
  (idVendor:HUAWEI_VENDOR_ID;    idProduct:$14ac; bInterfaceClass:0;                    bInterfaceSubClass:0;                         bInterfaceProtocol:0;                         DriverInfo:DRIVER_INFO_NONE), {Huawei E1820} //Interface Number 1
  (idVendor:REALTEK_VENDOR_ID;   idProduct:$8152; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Realtek RTL8152 Based USB 2.0 Ethernet Adapters}
  (idVendor:REALTEK_VENDOR_ID;   idProduct:$8153; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Realtek RTL8153 Based USB 3.0 Ethernet Adapters}
  (idVendor:SAMSUNG_VENDOR_ID;   idProduct:$a101; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Samsung USB Ethernet Adapters}
  (idVendor:LINKSYS_VENDOR_ID;   idProduct:$0041; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Linksys USB3GIGV1 Ethernet Adapter}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$3062; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {ThinkPad USB-C Dock (Realtek RTL8153)}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$3069; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {ThinkPad Thunderbolt 3 Dock (Realtek RTL8153)}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$3082; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {ThinkPad Thunderbolt 3 Dock Gen 2 (Realtek RTL8153)}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$7205; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Lenovo Thinkpad USB 3.0 Ethernet Adapters (Realtek RTL8153)}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$720c; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Lenovo USB C to Ethernet Adapter (Realtek RTL8153)}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$7214; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Lenovo USB-C Travel Hub (Realtek RTL8153)}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$721e; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Lenovo Powered USB-C Travel Hub (Realtek RTL8153)}
  (idVendor:LENOVO_VENDOR_ID;    idProduct:$a387; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {ThinkPad USB-C Dock Gen 2 (Realtek RTL8153)}
  (idVendor:NVIDIA_VENDOR_ID;    idProduct:$09ff; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {NVIDIA Tegra USB 3.0 Ethernet Adapters (Realtek RTL8153)}
  (idVendor:MICROSOFT_VENDOR_ID; idProduct:$07ab; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Microsoft Surface 2 dock (Realtek RTL8152)}
  (idVendor:MICROSOFT_VENDOR_ID; idProduct:$07c6; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Microsoft Surface Ethernet Adapter (Realtek RTL8153)}
  (idVendor:MICROSOFT_VENDOR_ID; idProduct:$0927; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Microsoft Surface Ethernet Adapter (Realtek RTL8153B)}

  (idVendor:TPLINK_VENDOR_ID;    idProduct:$0601; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {TP-LINK UE300 USB 3.0 Ethernet Adapters (Realtek RTL8153)}
  (idVendor:AQUANTIA_VENDOR_ID;  idProduct:$c101; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {Aquantia AQtion USB to 5GbE Controller (AQC111U)}
  (idVendor:ASIX_VENDOR_ID;      idProduct:$2790; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {ASIX USB 3.1 Gen1 to 5G Multi-Gigabit Ethernet Adapter (AQC111U)}
  (idVendor:ASIX_VENDOR_ID;      idProduct:$2791; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {ASIX USB 3.1 Gen1 to 2.5G Multi-Gigabit Ethernet Adapter (AQC112U)}
  (idVendor:$20f4;               idProduct:$e05a; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {USB-C 3.1 to 5GBASE-T Ethernet Adapter (AQC111U)}
  (idVendor:$1c04;               idProduct:$0015; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_NONE), {QNAP QNA-UC5G1T USB to 5GbE Adapter (AQC111U)}

  {Supported devices}
  (idVendor:ZTE_VENDOR_ID;       idProduct:$1003; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {ZTE (Vodafone) K3805-Z}
  (idVendor:ZTE_VENDOR_ID;       idProduct:$1015; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {ZTE (Vodafone) K3806-Z}
  (idVendor:ZTE_VENDOR_ID;       idProduct:$1173; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {ZTE (Vodafone) K4510-Z}
  (idVendor:ZTE_VENDOR_ID;       idProduct:$1177; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {ZTE (Vodafone) K3770-Z}
  (idVendor:ZTE_VENDOR_ID;       idProduct:$1181; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {ZTE (Vodafone) K3772-Z}
  (idVendor:$1bc7;               idProduct:$0000; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {Telit modules}
  (idVendor:DELL_VENDOR_ID;      idProduct:$81ba; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {Dell DW5580 modules}
  (idVendor:HUAWEI_VENDOR_ID;    idProduct:$15c1; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {Huawei ME906 and ME909}
  (idVendor:ZTE_VENDOR_ID;       idProduct:$0000; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_ZTE_CDC), {ZTE modules}
  (idVendor:UBLOX_VENDOR_ID;     idProduct:$1143; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {U-blox TOBY-L2}
  (idVendor:UBLOX_VENDOR_ID;     idProduct:$1104; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {U-blox SARA-U2}
  (idVendor:$1e2d;               idProduct:$0061; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {Cinterion PLS8 modem by GEMALTO}
  (idVendor:$1e2d;               idProduct:$0055; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN), {Cinterion AHS3 modem by GEMALTO}

  (idVendor:HUAWEI_VENDOR_ID;    idProduct:$0000; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:$FF;                       DriverInfo:DRIVER_INFO_WWAN), {Various Huawei modems with a network port like the UMG1831}

  (idVendor:$0000;               idProduct:$0000; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_ETHERNET; bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_CDC),  {Generic CDC Ethernet}
  (idVendor:$0000;               idProduct:$0000; bInterfaceClass:USB_CLASS_CODE_COMMS; bInterfaceSubClass:USB_SUBCLASS_CDC_MDLM;     bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE; DriverInfo:DRIVER_INFO_WWAN)  {Generic CDC MDLM}
  );

{==============================================================================}
type
 {CDC Ethernet specific types}
 {CDC Ethernet Device}
 PCDCEthernetNetwork = ^TCDCEthernetNetwork;
 TCDCEthernetNetwork = record
  {Network Properties}
  Network:TNetworkDevice;
  {Driver Properties}
  HardMTU:LongWord;                         {Hard MTU (Maximum Transmission Unit) value for the CDC Ethernet device}
  DriverInfo:LongWord;                      {Driver Info for the CDC Ethernet device (eg DRIVER_INFO_CDC}
  LinkStatus:LongWord;                      {Last reported link status}
  HardwareAddress:THardwareAddress;         {Current Ethernet MAC Address}
  ReceiveRequestSize:LongWord;              {Size of each USB receive request buffer}
  TransmitRequestSize:LongWord;             {Size of each USB transmit request buffer}
  ReceiveEntryCount:LongWord;               {Number of entries in the receive queue}
  TransmitEntryCount:LongWord;              {Number of entries in the transmit queue}
  {USB Properties}
  DataInterface:PUSBInterface;              {USB interface for data requests}
  ControlInterface:PUSBInterface;           {USB interface for control requests}
  ReceiveRequest:PUSBRequest;               {USB request Bulk IN Endpoint}
  ReceiveEndpoint:PUSBEndpointDescriptor;   {CDC Ethernet Bulk IN Endpoint}
  TransmitRequest:PUSBRequest;              {USB request for Bulk OUT Endpoint}
  TransmitEndpoint:PUSBEndpointDescriptor;  {CDC Ethernet Bulk OUT Endpoint}
  InterruptRequest:PUSBRequest;             {USB request for Interrupt IN Endpoint}
  InterruptEndpoint:PUSBEndpointDescriptor; {CDC Ethernet Interrupt IN Endpoint}
  PendingCount:LongWord;                    {Number of USB requests pending for this device}
  WaiterThread:TThreadId;                   {Thread waiting for pending requests to complete (for device detachment)}
 end;

{==============================================================================}
{var}
 {CDC Ethernet specific variables}

{==============================================================================}
{Initialization Functions}
procedure CDCEthernetInit;

{==============================================================================}
{CDC Ethernet Network Functions}
function CDCEthernetNetworkOpen(Network:PNetworkDevice):LongWord;
function CDCEthernetNetworkClose(Network:PNetworkDevice):LongWord;
function CDCEthernetNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function CDCEthernetBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function CDCEthernetBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
function CDCEthernetBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function CDCEthernetBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;

{==============================================================================}
{CDC Ethernet USB Functions}
function CDCEthernetDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function CDCEthernetDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure CDCEthernetReceiveWorker(Request:PUSBRequest);
procedure CDCEthernetReceiveComplete(Request:PUSBRequest);

procedure CDCEthernetTransmitWorker(Request:PUSBRequest);
procedure CDCEthernetTransmitComplete(Request:PUSBRequest);

procedure CDCEthernetInterruptWorker(Request:PUSBRequest);
procedure CDCEthernetInterruptComplete(Request:PUSBRequest);

{==============================================================================}
{CDC Ethernet Helper Functions}
function CDCEthernetCheckDeviceAndInterface(Device:PUSBDevice;Interrface:PUSBInterface;out DriverInfo:LongWord):LongWord;

function CDCEthernetGetMacAddress(Device:PUSBDevice;Index:LongWord;Address:PHardwareAddress):LongWord;

function CDCEthernetUpdateFilter(Network:PCDCEthernetNetwork):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {CDC Ethernet specific variables}
 CDCEthernetInitialized:Boolean;

 CDCEthernetDriver:PUSBDriver;  {CDC Ethernet Driver interface (Set by CDCEthernetInit)}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
procedure CDCEthernetTransmitStart(Network:PCDCEthernetNetwork); forward;

function CDCEthernetIsRNDIS(Descriptor:PUSBInterfaceDescriptor):Boolean; forward;
function CDCEthernetIsActiveSync(Descriptor:PUSBInterfaceDescriptor):Boolean; forward;
function CDCEthernetIsWirelessRNDIS(Descriptor:PUSBInterfaceDescriptor):Boolean; forward;
function CDCEthernetIsNovatelRNDIS(Descriptor:PUSBInterfaceDescriptor):Boolean; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CDCEthernetInit;
{Initialize the CDC Ethernet unit, create and register the driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if CDCEthernetInitialized then Exit;

 {Create CDC Ethernet Network Driver}
 CDCEthernetDriver:=USBDriverCreate;
 if CDCEthernetDriver <> nil then
  begin
   {Update CDC Ethernet Network Driver}
   {Driver}
   CDCEthernetDriver.Driver.DriverName:=CDCETHERNET_DRIVER_NAME;
   {USB}
   CDCEthernetDriver.DriverBind:=CDCEthernetDriverBind;
   CDCEthernetDriver.DriverUnbind:=CDCEthernetDriverUnbind;

   {Register CDC Ethernet Network Driver}
   Status:=USBDriverRegister(CDCEthernetDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'CDC Ethernet: Failed to register CDC Ethernet driver: ' + USBStatusToString(Status));

     {Destroy Driver}
     USBDriverDestroy(CDCEthernetDriver);
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'CDC Ethernet: Failed to create CDC Ethernet driver');
  end;

 CDCEthernetInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{CDCEthernet Network Functions}
function CDCEthernetNetworkOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen for the CDC Ethernet device}
{Note: Not intended to be called directly by applications, use NetworkDeviceOpen instead}
var
 Value:Word;
 Status:LongWord;
 Device:PUSBDevice;
 Entry:PNetworkEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    Result:=ERROR_ALREADY_OPEN;
    if Network.NetworkState <> NETWORK_STATE_CLOSED then Exit;

    {Set Result}
    Result:=ERROR_OPERATION_FAILED;

    {Update Filter}
    Status:=CDCEthernetUpdateFilter(PCDCEthernetNetwork(Network));
    if Status <> USB_STATUS_SUCCESS then Exit;

    //usbnet_cdc_zte_bind
    try
     {Allocate Receive Queue Buffer}
     Network.ReceiveQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),PCDCEthernetNetwork(Network).ReceiveEntryCount);
     if Network.ReceiveQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'CDC Ethernet: Failed to create receive queue buffer');

       Exit;
      end;

     {Allocate Receive Queue Semaphore}
     Network.ReceiveQueue.Wait:=SemaphoreCreate(0);
     if Network.ReceiveQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'CDC Ethernet: Failed to create receive queue semaphore');

       Exit;
      end;

     {Allocate Receive Queue Buffers}
     Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=PCDCEthernetNetwork(Network).ReceiveRequestSize;
       Entry.Offset:=0;
       Entry.Count:=1;

       {Allocate USB Request Buffer}
       Entry.Buffer:=USBBufferAllocate(Device,Entry.Size);
       if Entry.Buffer = nil then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to allocate USB receive buffer');

         Exit;
        end;

       {Initialize Packets}
       SetLength(Entry.Packets,Entry.Count);

       {Initialize First Packet}
       Entry.Packets[0].Buffer:=Entry.Buffer;
       Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
       Entry.Packets[0].Length:=Entry.Size - Entry.Offset;

       Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
      end;

     {Allocate Receive Queue Entries}
     SetLength(Network.ReceiveQueue.Entries,PCDCEthernetNetwork(Network).ReceiveEntryCount);

     {Allocate Transmit Queue Buffer}
     Network.TransmitQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),PCDCEthernetNetwork(Network).TransmitEntryCount);
     if Network.TransmitQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to create transmit queue buffer');

       Exit;
      end;

     {Allocate Transmit Queue Semaphore}
     Network.TransmitQueue.Wait:=SemaphoreCreate(PCDCEthernetNetwork(Network).TransmitEntryCount);
     if Network.TransmitQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to create transmit queue semaphore');

       Exit;
      end;

     {Allocate Transmit Queue Buffers}
     Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=PCDCEthernetNetwork(Network).TransmitRequestSize;
       Entry.Offset:=0;
       Entry.Count:=1;

       {Allocate USB Request Buffer}
       Entry.Buffer:=USBBufferAllocate(Device,Entry.Size);
       if Entry.Buffer = nil then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to allocate USB transmit buffer');

         Exit;
        end;

       {Initialize Packets}
       SetLength(Entry.Packets,Entry.Count);

       {Initialize First Packet}
       Entry.Packets[0].Buffer:=Entry.Buffer;
       Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
       Entry.Packets[0].Length:=Entry.Size - Entry.Offset;

       Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
      end;

     {Allocate Transmit Queue Entries}
     SetLength(Network.TransmitQueue.Entries,PCDCEthernetNetwork(Network).TransmitEntryCount);

     {Allocate Transmit Request}
     PCDCEthernetNetwork(Network).TransmitRequest:=USBRequestAllocate(Device,PCDCEthernetNetwork(Network).TransmitEndpoint,CDCEthernetTransmitComplete,0,nil);
     if PCDCEthernetNetwork(Network).TransmitRequest = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to allocate transmit request');

       Exit;
      end;

     {Allocate Receive Request}
     PCDCEthernetNetwork(Network).ReceiveRequest:=USBRequestAllocate(Device,PCDCEthernetNetwork(Network).ReceiveEndpoint,CDCEthernetReceiveComplete,0,nil);
     if PCDCEthernetNetwork(Network).ReceiveRequest = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to allocate receive request');

       Exit;
      end;

     {Submit Receive Request}
     {Get Entry}
     Entry:=BufferGet(Network.ReceiveQueue.Buffer);
     if Entry <> nil then
      begin
       {Update Pending}
       Inc(PCDCEthernetNetwork(Network).PendingCount);

       {Update Entry}
       Entry.DriverData:=Network;

       {Initialize Request}
       USBRequestInitialize(PCDCEthernetNetwork(Network).ReceiveRequest,CDCEthernetReceiveComplete,Entry.Buffer,Entry.Size,Entry);

       {$IFDEF CDCETHERNET_DEBUG}
       if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Submitting receive request');
       {$ENDIF}

       {Submit Request}
       Status:=USBRequestSubmit(PCDCEthernetNetwork(Network).ReceiveRequest);
       if Status <> USB_STATUS_SUCCESS then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to submit receive request: ' + USBStatusToString(Status));

         {Update Pending}
         Dec(PCDCEthernetNetwork(Network).PendingCount);

         {Update Entry}
         Entry.DriverData:=nil;

         {Free Entry}
         BufferFree(Entry);
         Exit;
        end;
      end
     else
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to get receive buffer entry');

       Exit;
      end;

     {Allocate Interrupt Request}
     if PCDCEthernetNetwork(Network).InterruptEndpoint <> nil then
      begin
       PCDCEthernetNetwork(Network).InterruptRequest:=USBRequestAllocate(Device,PCDCEthernetNetwork(Network).InterruptEndpoint,CDCEthernetInterruptComplete,PCDCEthernetNetwork(Network).InterruptEndpoint.wMaxPacketSize,Network);
       if PCDCEthernetNetwork(Network).InterruptRequest = nil then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to allocate interrupt request');

         Exit;
        end;

       {Update Pending}
       Inc(PCDCEthernetNetwork(Network).PendingCount);

       {$IFDEF CDCETHERNET_DEBUG}
       if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Submitting interrupt request');
       {$ENDIF}

       {Submit Interrupt Request}
       Status:=USBRequestSubmit(PCDCEthernetNetwork(Network).InterruptRequest);
       if Status <> USB_STATUS_SUCCESS then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to submit interrupt request: ' + USBStatusToString(Status));

         {Update Pending}
         Dec(PCDCEthernetNetwork(Network).PendingCount);
         Exit;
        end;
      end;

     {Set State to Open}
     Network.NetworkState:=NETWORK_STATE_OPEN;

     {Notify the State}
     NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_OPEN);

     {Get Network Status}
     if EMULATOR_MODE or (PCDCEthernetNetwork(Network).LinkStatus = 1) then
      begin
       {Set Status to Up}
       Network.NetworkStatus:=NETWORK_STATUS_UP;

       {Notify the Status}
       NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_UP);
      end;

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Check Result}
     if Result <> ERROR_SUCCESS then
      begin
       {Check Interrupt Request}
       if PCDCEthernetNetwork(Network).InterruptRequest <> nil then
        begin
         {Cancel Interrupt Request}
         USBRequestCancel(PCDCEthernetNetwork(Network).InterruptRequest);

         {Release Interrupt Request}
         USBRequestRelease(PCDCEthernetNetwork(Network).InterruptRequest);
        end;

       {Check Receive Request}
       if PCDCEthernetNetwork(Network).ReceiveRequest <> nil then
        begin
         {Cancel Receive Request}
         USBRequestCancel(PCDCEthernetNetwork(Network).ReceiveRequest);

         {Release Receive Request}
         USBRequestRelease(PCDCEthernetNetwork(Network).ReceiveRequest);
        end;

       {Check Transmit Request}
       if PCDCEthernetNetwork(Network).TransmitRequest <> nil then
        begin
         {Release Transmit Request}
         USBRequestRelease(PCDCEthernetNetwork(Network).TransmitRequest);
        end;

       {Check Transmit Queue Buffer}
       if Network.TransmitQueue.Buffer <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Transmit Queue Entries}
         SetLength(Network.TransmitQueue.Entries,0);

         {Deallocate Transmit Queue Buffers}
         Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
         while Entry <> nil do
          begin
           {Release USB Request Buffer}
           USBBufferRelease(Entry.Buffer);

           {Deinitialize Packets}
           SetLength(Entry.Packets,0);

           Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
          end;

         {Deallocate Transmit Queue Buffer}
         BufferDestroy(Network.TransmitQueue.Buffer);

         Network.TransmitQueue.Buffer:=INVALID_HANDLE_VALUE;
        end;

       {Check Transmit Queue Semaphore}
       if Network.TransmitQueue.Wait <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Transmit Queue Semaphore}
         SemaphoreDestroy(Network.TransmitQueue.Wait);

         Network.TransmitQueue.Wait:=INVALID_HANDLE_VALUE;
        end;

       {Check Receive Queue Buffer}
       if Network.ReceiveQueue.Buffer <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Receive Queue Entries}
         SetLength(Network.ReceiveQueue.Entries,0);

         {Deallocate Receive Queue Buffers}
         Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
         while Entry <> nil do
          begin
           {Release USB Request Buffer}
           USBBufferRelease(Entry.Buffer);

           {Initialize Packets}
           SetLength(Entry.Packets,0);

           Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
          end;

         {Deallocate Receive Queue Buffer}
         BufferDestroy(Network.ReceiveQueue.Buffer);

         Network.ReceiveQueue.Buffer:=INVALID_HANDLE_VALUE;
        end;

       {Check Receive Queue Semaphore}
       if Network.ReceiveQueue.Wait <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Receive Queue Semaphore}
         SemaphoreDestroy(Network.ReceiveQueue.Wait);

         Network.ReceiveQueue.Wait:=INVALID_HANDLE_VALUE;
        end;
      end;
    end;
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CDCEthernetNetworkClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose for the CDC Ethernet device}
{Note: Not intended to be called directly by applications, use NetworkDeviceClose instead}
var
 Message:TMessage;
 Device:PUSBDevice;
 Entry:PNetworkEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Set State to Closing}
 Result:=ERROR_OPERATION_FAILED;
 if NetworkDeviceSetState(Network,NETWORK_STATE_CLOSING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Cancel Interrupt Request}
    USBRequestCancel(PCDCEthernetNetwork(Network).InterruptRequest);

    {Cancel Receive Request}
    USBRequestCancel(PCDCEthernetNetwork(Network).ReceiveRequest);

    {Check Pending}
    if PCDCEthernetNetwork(Network).PendingCount <> 0 then
     begin
      {$IFDEF CDCETHERNET_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Waiting for ' + IntToStr(PCDCEthernetNetwork(Network).PendingCount) + ' pending requests to complete');
      {$ENDIF}

      {Wait for Pending}

      {Setup Waiter}
      PCDCEthernetNetwork(Network).WaiterThread:=GetCurrentThreadId;

      {Release the Lock}
      MutexUnlock(Network.Lock);

      {Wait for Message}
      ThreadReceiveMessage(Message);

      {Acquire the Lock}
      if MutexLock(Network.Lock) <> ERROR_SUCCESS then Exit;
     end;

    {Set State to Closed}
    Network.NetworkState:=NETWORK_STATE_CLOSED;

    {Notify the State}
    NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_CLOSE);

    {Check Interrupt Request}
    if PCDCEthernetNetwork(Network).InterruptRequest <> nil then
     begin
      {Release Interrupt Request}
      USBRequestRelease(PCDCEthernetNetwork(Network).InterruptRequest);
     end;

    {Check Receive Request}
    if PCDCEthernetNetwork(Network).ReceiveRequest <> nil then
     begin
      {Release Receive Request}
      USBRequestRelease(PCDCEthernetNetwork(Network).ReceiveRequest);
     end;

    {Check Transmit Request}
    if PCDCEthernetNetwork(Network).TransmitRequest <> nil then
     begin
      {Release Transmit Request}
      USBRequestRelease(PCDCEthernetNetwork(Network).TransmitRequest);
     end;

    {Check Transmit Queue Buffer}
    if Network.TransmitQueue.Buffer <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Transmit Queue Entries}
      SetLength(Network.TransmitQueue.Entries,0);

      {Deallocate Transmit Queue Buffers}
      Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
      while Entry <> nil do
       begin
        {Release USB Request Buffer}
        USBBufferRelease(Entry.Buffer);

        {Deinitialize Packets}
        SetLength(Entry.Packets,0);

        Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
       end;

      {Deallocate Transmit Queue Buffer}
      BufferDestroy(Network.TransmitQueue.Buffer);

      Network.TransmitQueue.Buffer:=INVALID_HANDLE_VALUE;
     end;

    {Check Transmit Queue Semaphore}
    if Network.TransmitQueue.Wait <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Transmit Queue Semaphore}
      SemaphoreDestroy(Network.TransmitQueue.Wait);

      Network.TransmitQueue.Wait:=INVALID_HANDLE_VALUE;
     end;

    {Check Receive Queue Buffer}
    if Network.ReceiveQueue.Buffer <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Receive Queue Entries}
      SetLength(Network.ReceiveQueue.Entries,0);

      {Deallocate Receive Queue Buffers}
      Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
      while Entry <> nil do
       begin
        {Release USB Request Buffer}
        USBBufferRelease(Entry.Buffer);

        {Initialize Packets}
        SetLength(Entry.Packets,0);

        Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
       end;

      {Deallocate Receive Queue Buffer}
      BufferDestroy(Network.ReceiveQueue.Buffer);

      Network.ReceiveQueue.Buffer:=INVALID_HANDLE_VALUE;
     end;

    {Check Receive Queue Semaphore}
    if Network.ReceiveQueue.Wait <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Receive Queue Semaphore}
      SemaphoreDestroy(Network.ReceiveQueue.Wait);

      Network.ReceiveQueue.Wait:=INVALID_HANDLE_VALUE;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CDCEthernetNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of NetworkDeviceControl for the CDC Ethernet device}
{Note: Not intended to be called directly by applications, use NetworkDeviceControl instead}
var
 Value:Word;
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet: Network Control (Request=' + IntToStr(Request) + ')');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Result}
    Result:=ERROR_OPERATION_FAILED;
    Status:=USB_STATUS_SUCCESS;

    {Check Request}
    case Request of
     NETWORK_CONTROL_CLEAR_STATS:begin
       {Clear Statistics}
       {Network}
       Network.ReceiveBytes:=0;
       Network.ReceiveCount:=0;
       Network.ReceiveErrors:=0;
       Network.TransmitBytes:=0;
       Network.TransmitCount:=0;
       Network.TransmitErrors:=0;
       Network.StatusCount:=0;
       Network.StatusErrors:=0;
       Network.BufferOverruns:=0;
       Network.BufferUnavailable:=0;
      end;
     NETWORK_CONTROL_GET_STATS:begin
       {Get Statistics}
       if Argument2 < SizeOf(TNetworkStatistics) then Exit;

       {Network}
       PNetworkStatistics(Argument1).ReceiveBytes:=Network.ReceiveBytes;
       PNetworkStatistics(Argument1).ReceiveCount:=Network.ReceiveCount;
       PNetworkStatistics(Argument1).ReceiveErrors:=Network.ReceiveErrors;
       PNetworkStatistics(Argument1).TransmitBytes:=Network.TransmitBytes;
       PNetworkStatistics(Argument1).TransmitCount:=Network.TransmitCount;
       PNetworkStatistics(Argument1).TransmitErrors:=Network.TransmitErrors;
       PNetworkStatistics(Argument1).StatusCount:=Network.StatusCount;
       PNetworkStatistics(Argument1).StatusErrors:=Network.StatusErrors;
       PNetworkStatistics(Argument1).BufferOverruns:=Network.BufferOverruns;
       PNetworkStatistics(Argument1).BufferUnavailable:=Network.BufferUnavailable;
      end;
     NETWORK_CONTROL_SET_MAC:begin
       {Set the MAC for this device}
       Status:=USB_STATUS_UNSUPPORTED_REQUEST;
      end;
     NETWORK_CONTROL_GET_MAC:begin
       {Get the MAC for this device}
       PHardwareAddress(Argument1)^:=PCDCEthernetNetwork(Network).HardwareAddress;
      end;
     NETWORK_CONTROL_SET_LOOPBACK:begin
       {Set Loopback Mode}
       Status:=USB_STATUS_UNSUPPORTED_REQUEST;
      end;
     NETWORK_CONTROL_RESET:begin
       {Reset the device}
       Status:=USB_STATUS_UNSUPPORTED_REQUEST;
      end;
     NETWORK_CONTROL_DISABLE:begin
       {Disable the device}
       Status:=USB_STATUS_UNSUPPORTED_REQUEST;
      end;
     NETWORK_CONTROL_GET_HARDWARE:begin
       {Get Hardware address for this device}
       PHardwareAddress(Argument1)^:=PCDCEthernetNetwork(Network).HardwareAddress;
      end;
     NETWORK_CONTROL_GET_BROADCAST:begin
       {Get Broadcast address for this device}
       PHardwareAddress(Argument1)^:=ETHERNET_BROADCAST;
      end;
     NETWORK_CONTROL_GET_MTU:begin
       {Get MTU for this device}
       Argument2:=ETHERNET_MTU;
       if PCDCEthernetNetwork(Network).HardMTU <> 0 then Argument2:=PCDCEthernetNetwork(Network).HardMTU;
      end;
     NETWORK_CONTROL_GET_HEADERLEN:begin
       {Get Header length for this device}
       Argument2:=ETHERNET_HEADER_SIZE;
      end;
     NETWORK_CONTROL_GET_LINK:begin
       {Get Link State for this device}
       if EMULATOR_MODE or (PCDCEthernetNetwork(Network).LinkStatus = 1) then
        begin
         {Link Up}
         Argument2:=NETWORK_LINK_UP;
        end
       else
        begin
         {Link Down}
         Argument2:=NETWORK_LINK_DOWN;
        end;
      end;
     else
      begin
       Exit;
      end;
    end;

    {Check Status}
    if Status <> USB_STATUS_SUCCESS then Exit;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CDCEthernetBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferAllocate for the CDC Ethernet device}
{Note: Not intended to be called directly by applications, use NetworkBufferAllocate instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet: Buffer Allocate');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Set Result}
 Result:=ERROR_OPERATION_FAILED;

 {Wait for Entry (Transmit Buffer)}
 Entry:=BufferGet(Network.TransmitQueue.Buffer);
 if Entry <> nil then
  begin
   {Update Entry}
   Entry.Size:=PCDCEthernetNetwork(Network).TransmitRequestSize;
   Entry.Offset:=0;
   Entry.Count:=1;

   {Update First Packet}
   Entry.Packets[0].Buffer:=Entry.Buffer;
   Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
   Entry.Packets[0].Length:=Entry.Size - Entry.Offset;

   {$IFDEF CDCETHERNET_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet:  Entry.Size = ' + IntToStr(Entry.Size));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet:  Entry.Offset = ' + IntToStr(Entry.Offset));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet:  Entry.Count = ' + IntToStr(Entry.Count));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet:  Entry.Packets[0].Length = ' + IntToStr(Entry.Packets[0].Length));
   {$ENDIF}

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function CDCEthernetBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferRelease for the CDC Ethernet device}
{Note: Not intended to be called directly by applications, use NetworkBufferRelease instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet: Buffer Release');
 {$ENDIF}

 {Check Entry}
 if Entry = nil then Exit;

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Free Entry (Receive Buffer)}
    Result:=BufferFree(Entry);
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CDCEthernetBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferReceive for the CDC Ethernet device}
{Note: Not intended to be called directly by applications, use NetworkBufferReceive instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet: Buffer Receive');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Wait for Entry}
 if SemaphoreWait(Network.ReceiveQueue.Wait) = ERROR_SUCCESS then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Remove Entry}
      Entry:=Network.ReceiveQueue.Entries[Network.ReceiveQueue.Start];

      {Update Start}
      Network.ReceiveQueue.Start:=(Network.ReceiveQueue.Start + 1) mod PCDCEthernetNetwork(Network).ReceiveEntryCount;

      {Update Count}
      Dec(Network.ReceiveQueue.Count);

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Network.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CDCEthernetBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferTransmit for the CDC Ethernet device}
{Note: Not intended to be called directly by applications, use NetworkBufferTransmit instead}
var
 Empty:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'CDC Ethernet: Buffer Transmit');
 {$ENDIF}

 {Check Entry}
 if Entry = nil then Exit;
 if (Entry.Count = 0) or (Entry.Count > 1) then Exit;

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Wait for Entry}
 if SemaphoreWait(Network.TransmitQueue.Wait) = ERROR_SUCCESS then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Empty}
      Empty:=(Network.TransmitQueue.Count = 0);

      {Add Entry}
      Network.TransmitQueue.Entries[(Network.TransmitQueue.Start + Network.TransmitQueue.Count) mod PCDCEthernetNetwork(Network).TransmitEntryCount]:=Entry;

      {Update Count}
      Inc(Network.TransmitQueue.Count);

      {Check Empty}
      if Empty then
       begin
        {Start Transmit}
        CDCEthernetTransmitStart(PCDCEthernetNetwork(Network));
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Network.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{CDC Ethernet USB Functions}
function CDCEthernetDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the CDC Ethernet driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Data:Pointer;
 Size:LongWord;
 RNDIS:Boolean;
 RNDISQuirk:Boolean;

 Count:LongWord;
 Status:LongWord;
 HardMTU:LongWord;
 DriverInfo:LongWord;
 Headers:TUSBCDCHeaders;
 Network:PCDCEthernetNetwork;
 DataInterface:PUSBInterface;
 DataAlternate:PUSBAlternate;
 ControlInterface:PUSBInterface;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
 InterruptEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Check Interface (Bind to interface only)}
 if Interrface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check CDC Ethernet Device and Interface}
 if CDCEthernetCheckDeviceAndInterface(Device,Interrface,DriverInfo) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Device not found in supported interface list');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Blacklisted Device}
 if DriverInfo = DRIVER_INFO_NONE then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Device found in blacklisted device list');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Get class specific descriptors}
 Data:=Interrface.ClassData;
 Size:=Interrface.ClassSize;

 {Parse class specific descriptors}
 if USBCDCParseHeaders(Device,@Headers,Data,Size) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Failed to parse CDC headers');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Save Control Interface}
 ControlInterface:=Interrface;

 {Check for RNDIS}
 RNDIS:=CDCEthernetIsRNDIS(Interrface.Descriptor) or CDCEthernetIsActiveSync(Interrface.Descriptor)
     or CDCEthernetIsWirelessRNDIS(Interrface.Descriptor) or CDCEthernetIsNovatelRNDIS(Interrface.Descriptor);
 RNDISQuirk:=False;

 {Check Union Headers}
 if (Headers.UnionDescriptor = nil) and not(RNDIS) then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: No union descriptor found');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Locate the Control and Data interfaces}
 if Headers.UnionDescriptor <> nil then
  begin
   ControlInterface:=USBDeviceFindInterfaceByIndex(Device,Headers.UnionDescriptor.bMasterInterface0);
   DataInterface:=USBDeviceFindInterfaceByIndex(Device,Headers.UnionDescriptor.bSlaveInterface0);
   if (ControlInterface = nil) or (DataInterface = nil) then
    begin
     {$IFDEF CDCETHERNET_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Invalid Control or Data interfaces');
     {$ENDIF}

     if not RNDIS then
      begin
       {Return Result}
       Result:=USB_STATUS_DEVICE_UNSUPPORTED;
       Exit;
      end;

     {Fall back to hard wired}
     RNDISQuirk:=True;
    end;

   if (ControlInterface <> Interrface) and not(RNDISQuirk) then
    begin
     {Reversed Control and Data interfaces}
     if DataInterface = Interrface then
      begin
       DataInterface:=ControlInterface;
       ControlInterface:=Interrface;
      end
     else
      begin
       {$IFDEF CDCETHERNET_DEBUG}
       if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Invalid master and slave information in union descriptor');
       {$ENDIF}

       {Return Result}
       Result:=USB_STATUS_DEVICE_UNSUPPORTED;
       Exit;
      end;
    end;

   if (ControlInterface <> DataInterface) and not(RNDISQuirk) then
    begin
     {Check Data interface class}
     if DataInterface.Descriptor.bInterfaceClass <> USB_CLASS_CODE_CDC_DATA then
      begin
       {$IFDEF CDCETHERNET_DEBUG}
       if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Invalid class code for Data interface');
       {$ENDIF}

       {Return Result}
       Result:=USB_STATUS_DEVICE_UNSUPPORTED;
       Exit;
      end;
    end;
  end;

 {Check for RNDIS with bmCapabilities}
 if RNDIS and CDCEthernetIsRNDIS(Interrface.Descriptor) and (Headers.ACMDescriptor <> nil) and (Headers.ACMDescriptor.bmCapabilities <> 0) then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: RNDIS descriptor with bmCapabilities set');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Ethernet Max Segment Size}
 HardMTU:=0;
 if (Headers.EthernetDescriptor <> nil) and (Headers.EthernetDescriptor.wMaxSegmentSize <> 0) then
  begin
   HardMTU:=Headers.EthernetDescriptor.wMaxSegmentSize;
  end;

 {Check MDLM Descriptor}
 if (Headers.MDLMDescriptor <> nil) and CompareMem(@Headers.MDLMDescriptor.bGUID,@MBM_GUID,SizeOf(MBM_GUID)) then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: MDLM descriptor GUID mismatch');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check MDLM Detail Descriptor}
 if (Headers.MDLMDetailDescriptor <> nil) and (Headers.MDLMDetailDescriptor.bFunctionLength < SizeOf(TUSBCDCMDLMDetailDescriptor)) then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: MDLM detail descriptor too short');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check for RNDIS devices that need to have the interfaces hard wired}
 if RNDIS and ((Headers.UnionDescriptor = nil) or RNDISQuirk) then
  begin
   ControlInterface:=USBDeviceFindInterfaceByIndex(Device,0);
   DataInterface:=USBDeviceFindInterfaceByIndex(Device,1);
   if (ControlInterface = nil) or (DataInterface = nil) or (ControlInterface <> Interrface) then
    begin
     {$IFDEF CDCETHERNET_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Invalid Control or Data interfaces');
     {$ENDIF}

     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end
 else if (Headers.HeaderDescriptor = nil) or (not(RNDIS) and (Headers.EthernetDescriptor = nil)) then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Missing CDC header, union or ethernet descriptor');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Get Endpoints}
 ReceiveEndpoint:=USBDeviceFindEndpointByType(Device,DataInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
 TransmitEndpoint:=USBDeviceFindEndpointByType(Device,DataInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
 InterruptEndpoint:=USBDeviceFindEndpointByType(Device,DataInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);

 {Check Endpoints}
 if ((ReceiveEndpoint = nil) or (TransmitEndpoint = nil)) and (DataInterface.AlternateCount > 0) then
  begin
   {Check Alternates}
   for Count:=0 to DataInterface.AlternateCount - 1 do
    begin
     DataAlternate:=USBDeviceFindAlternateByIndex(Device,DataInterface,Count);

     {Get Endpoints}
     ReceiveEndpoint:=USBDeviceFindAlternateEndpointByType(Device,DataInterface,DataAlternate,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
     TransmitEndpoint:=USBDeviceFindAlternateEndpointByType(Device,DataInterface,DataAlternate,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
     InterruptEndpoint:=USBDeviceFindAlternateEndpointByType(Device,DataInterface,DataAlternate,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);

     {Check Endpoints}
     if (ReceiveEndpoint <> nil) and (TransmitEndpoint <> nil) then
      begin
       {Set Interface}
       if USBDeviceSetInterface(Device,DataAlternate.Descriptor.bInterfaceNumber,DataAlternate.Descriptor.bAlternateSetting) <> USB_STATUS_SUCCESS then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to set interface alternate setting');

         {Return Result}
         Result:=USB_STATUS_DEVICE_UNSUPPORTED;
         Exit;
        end;

       Break;
      end;
    end;
  end;

 {Check Interrupt Endpoint}
 if InterruptEndpoint = nil then
  begin
   InterruptEndpoint:=USBDeviceFindEndpointByType(Device,ControlInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
  end;

 {Check Receive and Transmit Endpoints}
 if (ReceiveEndpoint = nil) or (TransmitEndpoint = nil) then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Invalid transmit or receive endpoint');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Interrupt Endpoint}
 if InterruptEndpoint <> nil then
  begin
   if (InterruptEndpoint.wMaxPacketSize < SizeOf(TUSBCDCNotification)) or (InterruptEndpoint.bInterval = 0) then
    begin
     {$IFDEF CDCETHERNET_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Invalid notification endpoint');
     {$ENDIF}

     InterruptEndpoint:=nil;
    end;
  end;

 {Check RNDIS Status Endpoint}
 if RNDIS and (InterruptEndpoint = nil) then
  begin
   {$IFDEF CDCETHERNET_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Missing RNDIS status endpoint');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {USB device reset not required because the USB core already did a reset on the port during attach}

 {Create Network}
 Network:=PCDCEthernetNetwork(NetworkDeviceCreateEx(SizeOf(TCDCEthernetNetwork)));
 if Network = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to create new network device');

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update Network}
 {Device}
 Network.Network.Device.DeviceBus:=DEVICE_BUS_USB;
 Network.Network.Device.DeviceType:=NETWORK_TYPE_ETHERNET;
 Network.Network.Device.DeviceFlags:=NETWORK_FLAG_RX_BUFFER or NETWORK_FLAG_TX_BUFFER;
 Network.Network.Device.DeviceData:=Device;
 Network.Network.Device.DeviceDescription:=CDCETHERNET_NETWORK_DESCRIPTION;
 {Network}
 Network.Network.NetworkState:=NETWORK_STATE_CLOSED;
 Network.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
 Network.Network.DeviceOpen:=CDCEthernetNetworkOpen;
 Network.Network.DeviceClose:=CDCEthernetNetworkClose;
 Network.Network.DeviceControl:=CDCEthernetNetworkControl;
 Network.Network.BufferAllocate:=CDCEthernetBufferAllocate;
 Network.Network.BufferRelease:=CDCEthernetBufferRelease;
 Network.Network.BufferReceive:=CDCEthernetBufferReceive;
 Network.Network.BufferTransmit:=CDCEthernetBufferTransmit;
 {Driver}
 Network.HardMTU:=HardMTU;
 Network.DriverInfo:=DriverInfo;
 Network.ReceiveRequestSize:=CDCETHERNET_RX_REQUEST_SIZE;
 Network.TransmitRequestSize:=CDCETHERNET_TX_REQUEST_SIZE;
 if Network.HardMTU <> 0 then Network.TransmitRequestSize:=Network.HardMTU;
 Network.ReceiveEntryCount:=4;
 Network.TransmitEntryCount:=4;
 if (Device.Speed = USB_SPEED_HIGH) or EMULATOR_MODE then
  begin
   Network.ReceiveEntryCount:=CDCETHERNET_MAX_QUEUE_MEMORY div Network.ReceiveRequestSize;
   Network.TransmitEntryCount:=CDCETHERNET_MAX_QUEUE_MEMORY div Network.TransmitRequestSize;
  end
 else if (Device.Speed = USB_SPEED_SUPER) or (Device.Speed = USB_SPEED_SUPERPLUS) then
  begin
   Network.ReceiveEntryCount:=5 * CDCETHERNET_MAX_QUEUE_MEMORY div Network.ReceiveRequestSize;
   Network.TransmitEntryCount:=5 * CDCETHERNET_MAX_QUEUE_MEMORY div Network.TransmitRequestSize;
  end;
 {USB}
 Network.DataInterface:=DataInterface;
 Network.ControlInterface:=ControlInterface;
 Network.ReceiveEndpoint:=ReceiveEndpoint;
 Network.TransmitEndpoint:=TransmitEndpoint;
 Network.InterruptEndpoint:=InterruptEndpoint;
 Network.WaiterThread:=INVALID_HANDLE_VALUE;

 {Get MAC Address}
 if CDCEthernetGetMacAddress(Device,Headers.EthernetDescriptor.iMACAddress,@Network.HardwareAddress) <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to get MAC address');

   {Destroy Network}
   NetworkDeviceDestroy(@Network.Network);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {$IFDEF CDCETHERNET_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: MAC Address = ' + HardwareAddressToString(Network.HardwareAddress));
 {$ENDIF}

 {Register Network}
 if NetworkDeviceRegister(@Network.Network) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'CDC Ethernet: Failed to register new network device');

   {Destroy Network}
   NetworkDeviceDestroy(@Network.Network);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update Interface}
 Interrface.DriverData:=Network;

 {Update Data Interface}
 DataInterface.DriverData:=Network;
 DataInterface.Driver:=CDCEthernetDriver;

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function CDCEthernetDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the CDC Ethernet driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Network:PCDCEthernetNetwork;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Driver}
 if Interrface.Driver <> CDCEthernetDriver then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'CDC Ethernet: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Get Network}
 Network:=PCDCEthernetNetwork(Interrface.DriverData);
 if Network = nil then Exit;

 {Check Interface}
 if (Interrface <> nil) and (Interrface <> Network.ControlInterface) then
  begin
   {Unbind on Control Interface}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;

 {Close Network}
 CDCEthernetNetworkClose(@Network.Network);

 {Update Interface}
 Interrface.DriverData:=nil;

 {Update Data Interface}
 Network.DataInterface.DriverData:=nil;
 Network.DataInterface.Driver:=nil;

 {Deregister Network}
 if NetworkDeviceDeregister(@Network.Network) <> ERROR_SUCCESS then Exit;

 {Destroy Network}
 NetworkDeviceDestroy(@Network.Network);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure CDCEthernetReceiveWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from the CDC Ethernet bulk IN endpoint}
{Request: The USB request which has completed}
var
 Status:LongWord;
 Message:TMessage;
 Next:PNetworkEntry;
 Entry:PNetworkEntry;
 Network:PCDCEthernetNetwork;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Entry}
 Entry:=PNetworkEntry(Request.DriverData);
 if Entry = nil then Exit;

 {Get Network}
 Network:=PCDCEthernetNetwork(Entry.DriverData);
 if Network <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IFDEF CDCETHERNET_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF CDCETHERNET_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Receive complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}

        {Check Size}
        if Request.ActualSize > 0 then
         begin
          {Get Next}
          Next:=nil;
          if BufferAvailable(Network.Network.ReceiveQueue.Buffer) > 0 then
           begin
            Next:=BufferGet(Network.Network.ReceiveQueue.Buffer);
           end;

          {Check Next}
          if Next <> nil then
           begin
            {Check Receive Queue Count}
            if Network.Network.ReceiveQueue.Count < Network.ReceiveEntryCount then
             begin
              {Update Entry}
              Entry.Count:=1;

              {Update Packet}
              Entry.Packets[Entry.Count - 1].Buffer:=Entry.Buffer;
              Entry.Packets[Entry.Count - 1].Data:=Entry.Buffer + Entry.Offset;
              Entry.Packets[Entry.Count - 1].Length:=Request.ActualSize - Entry.Offset;

              //usbnet_cdc_zte_rx_fixup

              {$IFDEF CDCETHERNET_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Receiving packet (Length=' + IntToStr(Entry.Packets[Entry.Count - 1].Length) + ', Count=' + IntToStr(Entry.Count) + ')');
              {$ENDIF}

              {Update Statistics}
              Inc(Network.Network.ReceiveCount);
              Inc(Network.Network.ReceiveBytes,Entry.Packets[Entry.Count - 1].Length);

              {Add Entry}
              Network.Network.ReceiveQueue.Entries[(Network.Network.ReceiveQueue.Start + Network.Network.ReceiveQueue.Count) mod Network.ReceiveEntryCount]:=Entry;

              {Update Count}
              Inc(Network.Network.ReceiveQueue.Count);

              {Signal Packet Received}
              SemaphoreSignal(Network.Network.ReceiveQueue.Wait);
             end
            else
             begin
              if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Receive queue overrun, packet discarded');

              {Free Entry}
              BufferFree(Entry);

              {Update Statistics}
              Inc(Network.Network.ReceiveErrors);
              Inc(Network.Network.BufferOverruns);
             end;
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: No receive buffer available, packet discarded');

            {Get Next}
            Next:=Entry;

            {Update Statistics}
            Inc(Network.Network.ReceiveErrors);
            Inc(Network.Network.BufferUnavailable);
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed receive request (ActualSize=' + IntToStr(Request.ActualSize) + ')');

          {Get Next}
          Next:=Entry;

          {Update Statistics}
          Inc(Network.Network.ReceiveErrors);
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed receive request (Status=' + USBStatusToString(Request.Status) + ')');

        {Get Next}
        Next:=Entry;

        {Update Statistics}
        Inc(Network.Network.ReceiveErrors);
       end;

      {Update Pending}
      Dec(Network.PendingCount);

      {Update Next}
      Next.DriverData:=nil;

      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Free Next}
        BufferFree(Next);

        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {Check Waiter}
          if Network.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF CDCETHERNET_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Check Next}
        if Next <> nil then
         begin
          {Update Pending}
          Inc(Network.PendingCount);

          {Update Next}
          Next.DriverData:=Network;

          {Initialize Request}
          USBRequestInitialize(Request,CDCEthernetReceiveComplete,Next.Buffer,Next.Size,Next);

          {$IFDEF CDCETHERNET_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Resubmitting receive request');
          {$ENDIF}

          {Resubmit Request}
          Status:=USBRequestSubmit(Request);
          if Status <> USB_STATUS_SUCCESS then
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed to resubmit receive request: ' + USBStatusToString(Status));

            {Update Pending}
            Dec(Network.PendingCount);

            {Update Next}
            Next.DriverData:=nil;

            {Free Next}
            BufferFree(Next);
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: No receive buffer available, cannot resubmit receive request');

          {Update Statistics}
          Inc(Network.Network.BufferUnavailable);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Network.Network.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Receive request invalid');
  end;
end;

{==============================================================================}

procedure CDCEthernetReceiveComplete(Request:PUSBRequest);
{Called when a USB request from the CDC Ethernet bulk IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerScheduleEx(0,WORKER_FLAG_PRIORITY,TWorkerTask(CDCEthernetReceiveWorker),Request,nil);
end;

{==============================================================================}

procedure CDCEthernetTransmitWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request to the CDC Ethernet bulk OUT endpoint}
{Request: The USB request which has completed}
var
 Message:TMessage;
 Entry:PNetworkEntry;
 Network:PCDCEthernetNetwork;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Entry}
 Entry:=PNetworkEntry(Request.DriverData);
 if Entry = nil then Exit;

 {Get Network}
 Network:=PCDCEthernetNetwork(Entry.DriverData);
 if Network <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IFDEF CDCETHERNET_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Close pending, setting transmit request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF CDCETHERNET_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Transmit complete');
        {$ENDIF}

        {Update Statistics}
        Inc(Network.Network.TransmitCount);
        Inc(Network.Network.TransmitBytes,Entry.Packets[0].Length);
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed transmit request (Status=' + USBStatusToString(Request.Status) + ')');

        {Update Statistics}
        Inc(Network.Network.TransmitErrors);
       end;

      {Update Start}
      Network.Network.TransmitQueue.Start:=(Network.Network.TransmitQueue.Start + 1) mod PCDCEthernetNetwork(Network).TransmitEntryCount;

      {Update Count}
      Dec(Network.Network.TransmitQueue.Count);

      {Signal Queue Free}
      SemaphoreSignal(Network.Network.TransmitQueue.Wait);

      {Update Entry}
      Entry.DriverData:=nil;

      {Free Entry (Transmit Buffer)}
      BufferFree(Entry);

      {Update Pending}
      Dec(Network.PendingCount);

      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {Check Waiter}
          if Network.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF CDCETHERNET_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Check Count}
        if Network.Network.TransmitQueue.Count > 0 then
         begin
          {Start Transmit}
          CDCEthernetTransmitStart(Network);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Network.Network.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Transmit request invalid');
  end;
end;

{==============================================================================}

procedure CDCEthernetTransmitComplete(Request:PUSBRequest);
{Called when a USB request to the CDC Ethernet bulk OUT endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerScheduleEx(0,WORKER_FLAG_PRIORITY,TWorkerTask(CDCEthernetTransmitWorker),Request,nil);
end;

{==============================================================================}

procedure CDCEthernetInterruptWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from the CDC Ethernet interrupt IN endpoint}
{Request: The USB request which has completed}
var
 Status:LongWord;
 Message:TMessage;
 LinkStatus:LongWord;
 Network:PCDCEthernetNetwork;
 Notification:PUSBCDCNotification;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Network}
 Network:=PCDCEthernetNetwork(Request.DriverData);
 if Network <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Network.Network.StatusCount);

      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IFDEF CDCETHERNET_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Close pending, setting interrupt request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF CDCETHERNET_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Interrupt complete');
        {$ENDIF}

        {Check Size}
        if Request.ActualSize >= SizeOf(TUSBCDCNotification) then
         begin
          {Get Notification}
          Notification:=PUSBCDCNotification(Request.Data);

          case Notification.bNotificationType of
           USB_CDC_NOTIFY_NETWORK_CONNECTION:begin
             {Network Connection Notification}
             {Get Link Status}
             LinkStatus:=0;
             if Notification.wValue > 0 then LinkStatus:=1;

             {Check Link Status}
             if LinkStatus <> Network.LinkStatus then
              begin
               {Update Link Status}
               Network.LinkStatus:=LinkStatus;

               if Network.LinkStatus = 1 then
                begin
                 {Check Status}
                 if Network.Network.NetworkStatus <> NETWORK_STATUS_UP then
                  begin
                   {Set Status to Up}
                   Network.Network.NetworkStatus:=NETWORK_STATUS_UP;

                   {Notify the Status}
                   NotifierNotify(@Network.Network.Device,DEVICE_NOTIFICATION_UP);
                  end;
                end
               else
                begin
                 {Check Status}
                 if Network.Network.NetworkStatus <> NETWORK_STATUS_DOWN then
                  begin
                   {Set Status to Down}
                   Network.Network.NetworkStatus:=NETWORK_STATUS_DOWN;

                   {Notify the Status}
                   NotifierNotify(@Network.Network.Device,DEVICE_NOTIFICATION_DOWN);
                  end;
                end;
              end;
            end;
          end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed interrupt request (ActualSize=' + IntToStr(Request.ActualSize) + ')');

          {Update Statistics}
          Inc(Network.Network.StatusErrors);
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed interrupt request (Status=' + USBStatusToString(Request.Status) + ')');

        {Update Statistics}
        Inc(Network.Network.StatusErrors);
       end;

      {Update Pending}
      Dec(Network.PendingCount);

      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {Check Waiter}
          if Network.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF CDCETHERNET_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Update Pending}
        Inc(Network.PendingCount);

        {$IFDEF CDCETHERNET_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Resubmitting interrupt request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed to resubmit interrupt request: ' + USBStatusToString(Status));

          {Update Pending}
          Dec(Network.PendingCount);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Network.Network.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Interrupt request invalid');
  end;
end;

{==============================================================================}

procedure CDCEthernetInterruptComplete(Request:PUSBRequest);
{Called when a USB request from the CDC Ethernet interrupt IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerScheduleEx(0,WORKER_FLAG_PRIORITY,TWorkerTask(CDCEthernetInterruptWorker),Request,nil);
end;

{==============================================================================}
{==============================================================================}
{CDC Ethernet Helper Functions}
function CDCEthernetCheckDeviceAndInterface(Device:PUSBDevice;Interrface:PUSBInterface;out DriverInfo:LongWord):LongWord;
{Check the Device and Interface against the supported and blacklisted devices}
{Device: USB device to check}
{Interrface: USB interface to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Setup Defaults}
 DriverInfo:=DRIVER_INFO_NONE;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Device and Interface IDs}
 for Count:=0 to CDCETHERNET_DEVICE_ID_COUNT - 1 do
  begin
   {Check Device Vendor}
   if (CDCETHERNET_DEVICE_ID[Count].idVendor = 0) or (CDCETHERNET_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) then
    begin
     {Check Device Product}
     if (CDCETHERNET_DEVICE_ID[Count].idProduct = 0) or (CDCETHERNET_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
      begin
       {Check Interface Class}
       if (CDCETHERNET_DEVICE_ID[Count].bInterfaceClass = 0) or (CDCETHERNET_DEVICE_ID[Count].bInterfaceClass = Interrface.Descriptor.bInterfaceClass) then
        begin
         {Check Interface SubClass}
         if (CDCETHERNET_DEVICE_ID[Count].bInterfaceSubClass = 0) or (CDCETHERNET_DEVICE_ID[Count].bInterfaceSubClass = Interrface.Descriptor.bInterfaceSubClass) then
          begin
           {Check Interface Protocol}
           if (CDCETHERNET_DEVICE_ID[Count].bInterfaceProtocol = 0) or (CDCETHERNET_DEVICE_ID[Count].bInterfaceProtocol = Interrface.Descriptor.bInterfaceProtocol) then
            begin
             {Return Driver Info}
             DriverInfo:=CDCETHERNET_DEVICE_ID[Count].DriverInfo;

             Result:=USB_STATUS_SUCCESS;
             Exit;
            end;
          end;
        end;
      end;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function CDCEthernetGetMacAddress(Device:PUSBDevice;Index:LongWord;Address:PHardwareAddress):LongWord;
{Get the MAC address of a CDC Ethernet device}
{Device: USB device to read from}
{Index: The string descriptor index containing the MAC address}
{Address: Value to read the MAC address into}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Value:String;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Read String}
 Value:=USBDeviceReadStringDescriptor(Device,Index);
 if Length(Value) = 12 then
  begin
   {Convert Address}
   Address^:=StringToHardwareAddress(Value);

   Result:=USB_STATUS_SUCCESS;
  end;
end;

{==============================================================================}

function CDCEthernetUpdateFilter(Network:PCDCEthernetNetwork):LongWord;
{Update the packet filter of the CDC Ethernet device}
{Network: The network device to update the packet filter for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 CDCFilter:Word;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Network.Device.DeviceData);
 if Device = nil then Exit;

 {Setup Filter}
 CDCFilter:=USB_CDC_PACKET_TYPE_DIRECTED or USB_CDC_PACKET_TYPE_BROADCAST;

 {Promiscuous Mode}
 {CDCFilter:=CDCFilter or USB_CDC_PACKET_TYPE_PROMISCUOUS;}

 {Multicast Mode}
 {CDCFilter:=CDCFilter or USB_CDC_PACKET_TYPE_ALL_MULTICAST;}

 {Send Control Request}
 Result:=USBControlRequestEx(Device,nil,USB_CDC_SET_ETHERNET_PACKET_FILTER,USB_REQUEST_TYPE_CLASS or USB_REQUEST_RECIPIENT_INTERFACE,CDCFilter,Network.ControlInterface.Descriptor.bInterfaceNumber,nil,0,INFINITE,False);

 {QEMU does not support USB_CDC_SET_ETHERNET_PACKET_FILTER}
 if (Result <> USB_STATUS_SUCCESS) and EMULATOR_MODE then Result:=USB_STATUS_SUCCESS;
end;


{==============================================================================}
{==============================================================================}
{Internal Functions}
procedure CDCEthernetTransmitStart(Network:PCDCEthernetNetwork);
{Transmit start function for the CDC Ethernet Network device}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the network lock}
var
 Status:LongWord;
 Request:PUSBRequest;
 Entry:PNetworkEntry;
 Packet:PNetworkPacket;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IFDEF CDCETHERNET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'CDC Ethernet: Transmit Start');
 {$ENDIF}

 {Check Count}
 if Network.Network.TransmitQueue.Count = 0 then Exit;

 {Get Entry}
 Entry:=Network.Network.TransmitQueue.Entries[Network.Network.TransmitQueue.Start];
 if Entry = nil then Exit;

 {Get Request}
 Request:=Network.TransmitRequest;

 {Update Entry}
 Entry.DriverData:=Network;

 {Initialize Request}
 USBRequestInitialize(Request,CDCEthernetTransmitComplete,Entry.Buffer,Entry.Size,Entry);

 {Get Packet}
 Packet:=@Entry.Packets[0];

 {$IFDEF CDCETHERNET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'CDC Ethernet: Packet Length = ' + IntToStr(Packet.Length));
 {$ENDIF}

 {Update Request}
 Request.Size:=Packet.Length;

 {Update Pending}
 Inc(Network.PendingCount);

 {$IFDEF CDCETHERNET_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC Ethernet: Submitting transmit request');
 {$ENDIF}

 {Submit the Request}
 Status:=USBRequestSubmit(Request);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC Ethernet: Failed to submit transmit request: ' + USBStatusToString(Status));

   {Update Entry}
   Entry.DriverData:=nil;

   {Update Pending}
   Dec(Network.PendingCount);
  end;
end;

{==============================================================================}

function CDCEthernetIsRNDIS(Descriptor:PUSBInterfaceDescriptor):Boolean;
{Check the supplied descriptor for an RNDIS interface}
begin
 {}
 Result:=False;

 if Descriptor = nil then Exit;

 Result:=(Descriptor.bInterfaceClass = USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL)
     and (Descriptor.bInterfaceSubClass = 2) and (Descriptor.bInterfaceProtocol = $ff);
end;

{==============================================================================}

function CDCEthernetIsActiveSync(Descriptor:PUSBInterfaceDescriptor):Boolean;
{Check the supplied descriptor for an ActiveSync interface}
begin
 {}
 Result:=False;

 if Descriptor = nil then Exit;

 Result:=(Descriptor.bInterfaceClass = USB_CLASS_CODE_MISCELLANEOUS)
     and (Descriptor.bInterfaceSubClass = 1) and (Descriptor.bInterfaceProtocol = 1);
end;

{==============================================================================}

function CDCEthernetIsWirelessRNDIS(Descriptor:PUSBInterfaceDescriptor):Boolean;
{Check the supplied descriptor for a Wireless RNDIS interface}
begin
 {}
 Result:=False;

 if Descriptor = nil then Exit;

 Result:=(Descriptor.bInterfaceClass = USB_CLASS_CODE_WIRELESS_CONTROLLER)
     and (Descriptor.bInterfaceSubClass = 1) and (Descriptor.bInterfaceProtocol = 3);
end;

{==============================================================================}

function CDCEthernetIsNovatelRNDIS(Descriptor:PUSBInterfaceDescriptor):Boolean;
{Check the supplied descriptor for a Novatel RNDIS interface}
begin
 {}
 Result:=False;

 if Descriptor = nil then Exit;

 Result:=(Descriptor.bInterfaceClass = USB_CLASS_CODE_MISCELLANEOUS)
     and (Descriptor.bInterfaceSubClass = 4) and (Descriptor.bInterfaceProtocol = 3);
end;

{==============================================================================}
{==============================================================================}

initialization
 CDCEthernetInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
