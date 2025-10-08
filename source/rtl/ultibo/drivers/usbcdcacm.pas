{
USB CDC ACM Driver.

Copyright (C) 2025 - SoftOz Pty Ltd.

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

  Linux - \drivers\usb\class\cdc-acm.c - Copyright (c) 2011 Johan Hovold and others.
  Linux - \drivers\usb\class\cdc-acm.h
  Linux - \drivers\usb\serial\usb-serial.c - Copyright (C) 2009 - 2013 Johan Hovold and others.
  Linux - \drivers\usb\serial\generic.c - Copyright (C) 2010 - 2013 Johan Hovold and others.

 Thanks to Kerry Shipman who generously made Arduino hardware available for development of this driver.

References
==========

  USB CDC - https://en.wikipedia.org/wiki/USB_communications_device_class
            http://www.usb.org/developers/docs/devclass_docs/CDC1.2_WMC1.1_012011.zip

USB CDC ACM
===========

 The Abstract Control Model (ACM) is part of the USB Communications Device Class (CDC) standard
 which allows modem like devices to be supported by USB with a standardized protocol for data
 transfer and modem control.

 A CDC ACM device is defined by information in the interface descriptors and is not specific to
 any product and vendor ID. There are numerous devices that support the CDC ACM protocol and some
 of the most important from the Ultibo are the Arduino Mega 2560 R3 and the Arduino Due.

 This driver presents any device recognized as a CDC ACM as a generic serial interface device that
 can be accessed using the API in the Serial unit. It should not be necessary to directly call any
 of the functions in this unit from application code.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit USBCDCACM;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.USB,
  Core.USBCDC,
  Core.Serial,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  USB,
  USBCDC,
  Serial,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {CDC ACM specific constants}
 CDCACM_DRIVER_NAME = 'USB CDC ACM Serial Driver'; {Name of CDC ACM driver}

 CDCACM_SERIAL_DESCRIPTION = 'USB CDC ACM Serial'; {Description of CDC ACM device}

 CDCACM_MIN_BAUD = 0; {Minimum is device specific}
 CDCACM_MAX_BAUD = 0; {Maximum is device specific}

 CDCACM_MIN_DATABITS = SERIAL_DATA_5BIT;
 CDCACM_MAX_DATABITS = SERIAL_DATA_8BIT;

 CDCACM_MIN_STOPBITS = SERIAL_STOP_1BIT;
 CDCACM_MAX_STOPBITS = SERIAL_STOP_1BIT5;

 CDCACM_MAX_PARITY = SERIAL_PARITY_SPACE;

 CDCACM_MAX_FLOW = SERIAL_FLOW_DSR_DTR;

 {CDC ACM USB requests}
 CDCACM_USB_REQUEST_TYPE = (USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE);

 {CDC ACM device quirks}
 CDCACM_QUIRK_NONE                  = $00000000;
 CDCACM_QUIRK_NO_UNION_NORMAL       = $00000001;
 CDCACM_QUIRK_SINGLE_RX_URB         = $00000002;
 CDCACM_QUIRK_NO_CAP_LINE           = $00000004;
 CDCACM_QUIRK_NO_DATA_INTERFACE     = $00000008;
 CDCACM_QUIRK_IGNORE_DEVICE         = $00000010;
 CDCACM_QUIRK_CONTROL_LINE_STATE    = $00000020;
 CDCACM_QUIRK_CLEAR_HALT_CONDITIONS = $00000040;
 CDCACM_QUIRK_SEND_ZERO_PACKET      = $00000080;
 CDCACM_QUIRK_DISABLE_ECHO          = $00000100;

type
 {CDC ACM Device ID type}
 PCDCACMDeviceId = ^TCDCACMDeviceId;
 TCDCACMDeviceId = record
  idVendor:Word;
  idProduct:Word;
  Quirks:LongWord;
 end;

const
 {CDC ACM Device ID constants}
 CDCACM_DEVICE_ID_COUNT = 49; {Number of supported Device IDs}

 CDCACM_DEVICE_ID:array[0..CDCACM_DEVICE_ID_COUNT - 1] of TCDCACMDeviceId = (
  (idVendor:$076d;idProduct:$0006;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Denso Cradle CU-321 (has no union descriptor)}
  (idVendor:$17ef;idProduct:$7000;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Lenovo USB modem (has no union descriptor)}
  (idVendor:$0870;idProduct:$0001;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Metricom GS Modem (has no union descriptor)}
  (idVendor:$0e8d;idProduct:$0003;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {FIREFLY, MediaTek Inc (has no union descriptor)}
  (idVendor:$0e8d;idProduct:$2000;Quirks:CDCACM_QUIRK_DISABLE_ECHO),          {MediaTek Inc Preloader}
  (idVendor:$0e8d;idProduct:$3329;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {MediaTek Inc GPS (has no union descriptor)}
  (idVendor:$0482;idProduct:$0203;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {KYOCERA AH-K3001V (has no union descriptor)}
  (idVendor:$079b;idProduct:$000f;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {BT On-Air USB MODEM (has no union descriptor)}
  (idVendor:$0ace;idProduct:$1602;Quirks:CDCACM_QUIRK_SINGLE_RX_URB),         {ZyDAS 56K USB MODEM}
  (idVendor:$0ace;idProduct:$1608;Quirks:CDCACM_QUIRK_SINGLE_RX_URB),         {ZyDAS 56K USB MODEM (firmware bug)}
  (idVendor:$0ace;idProduct:$1611;Quirks:CDCACM_QUIRK_SINGLE_RX_URB),         {ZyDAS 56K USB MODEM - new version (firmware bug)}
  (idVendor:$11ca;idProduct:$0201;Quirks:CDCACM_QUIRK_SINGLE_RX_URB),         {VeriFone Mx870 Gadget Serial}
  (idVendor:$1965;idProduct:$0018;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Uniden UBC125XLT}
  (idVendor:$22b8;idProduct:$7000;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Motorola Q Phone (has no union descriptor)}
  (idVendor:$0803;idProduct:$3095;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Zoom Telephonics Model 3095F USB MODEM (has no union descriptor)}
  (idVendor:$0572;idProduct:$1321;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Conexant USB MODEM CX93010 (has no union descriptor)}
  (idVendor:$0572;idProduct:$1324;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Conexant USB MODEM RD02-D400 (has no union descriptor)}
  (idVendor:$0572;idProduct:$1328;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Shiro / Aztech USB MODEM UM-3100 (has no union descriptor)}
  (idVendor:$0572;idProduct:$1349;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Hiro (Conexant) USB MODEM H50228}
  (idVendor:$20df;idProduct:$0001;Quirks:CDCACM_QUIRK_CONTROL_LINE_STATE),    {Simtec Electronics Entropy Key}
  (idVendor:$2184;idProduct:$001c;Quirks:CDCACM_QUIRK_NONE),                  {GW Instek AFG-2225}
  (idVendor:$2184;idProduct:$0036;Quirks:CDCACM_QUIRK_NONE),                  {GW Instek AFG-125}
  (idVendor:$22b8;idProduct:$6425;Quirks:CDCACM_QUIRK_NONE),                  {Motorola MOTOMAGX phones}
  {Motorola H24 HSPA module:}
  (idVendor:$22b8;idProduct:$2d91;Quirks:CDCACM_QUIRK_NONE),                  {modem}
  (idVendor:$22b8;idProduct:$2d92;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {modem           + diagnostics        (handle only modem interface)}
  (idVendor:$22b8;idProduct:$2d93;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {modem + AT port                      (handle only modem interface)}
  (idVendor:$22b8;idProduct:$2d95;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {modem + AT port + diagnostics        (handle only modem interface)}
  (idVendor:$22b8;idProduct:$2d96;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {modem                         + NMEA (handle only modem interface)}
  (idVendor:$22b8;idProduct:$2d97;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {modem           + diagnostics + NMEA (handle only modem interface)}
  (idVendor:$22b8;idProduct:$2d99;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {modem + AT port               + NMEA (handle only modem interface)}
  (idVendor:$22b8;idProduct:$2d9a;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {modem + AT port + diagnostics + NMEA (handle only modem interface)}
  (idVendor:$0572;idProduct:$1329;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Hummingbird huc56s (Conexant) (union descriptor misplaced on data interface instead of communications interface)}
  (idVendor:$0572;idProduct:$1340;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Conexant CX93010-2x UCMxx}
  (idVendor:$05f9;idProduct:$4002;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {PSC Scanning, Magellan 800i}
  (idVendor:$1bbb;idProduct:$0003;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Alcatel OT-I650 (reports zero length descriptor)}
  (idVendor:$1576;idProduct:$03b1;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Maretron USB100 (reports zero length descriptor)}
  (idVendor:$fff0;idProduct:$0100;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {DATECS FP-2000}
  (idVendor:$09d8;idProduct:$0320;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Elatec GmbH TWN3}
  (idVendor:$0ca6;idProduct:$a050;Quirks:CDCACM_QUIRK_NO_UNION_NORMAL),       {Castles VEGA3000}
  (idVendor:$2912;idProduct:$0001;Quirks:CDCACM_QUIRK_CLEAR_HALT_CONDITIONS), {ATOL FPrint}
  {Support for Owen devices}
  (idVendor:$03eb;idProduct:$0030;Quirks:CDCACM_QUIRK_NONE), {Owen SI30}
  {Support for Droids MuIn LCD}
  (idVendor:$04d8;idProduct:$000b;Quirks:CDCACM_QUIRK_NO_DATA_INTERFACE),
  (idVendor:$04d8;idProduct:$0082;Quirks:CDCACM_QUIRK_IGNORE_DEVICE),          {Application mode}
  (idVendor:$04d8;idProduct:$0083;Quirks:CDCACM_QUIRK_IGNORE_DEVICE),          {Bootloader mode}
  {Samsung phone in firmware update mode}
  (idVendor:$04e8;idProduct:$685d;Quirks:CDCACM_QUIRK_IGNORE_DEVICE),
  {Exclude Infineon Flash Loader utility}
  (idVendor:$058b;idProduct:$0041;Quirks:CDCACM_QUIRK_IGNORE_DEVICE),
  (idVendor:$1bc7;idProduct:$0021;Quirks:CDCACM_QUIRK_SEND_ZERO_PACKET),       {Telit 3G ACM only composition}
  (idVendor:$1bc7;idProduct:$0023;Quirks:CDCACM_QUIRK_SEND_ZERO_PACKET),       {Telit 3G ACM + ECM composition}
  (idVendor:$1519;idProduct:$0452;Quirks:CDCACM_QUIRK_SEND_ZERO_PACKET));      {Intel 7260 modem}

 {CDC ACM Interface ID constants}
 CDCACM_INTERFACE_ID_COUNT = 7; {Number of supported Interface IDs}

 CDCACM_INTERFACE_ID:array[0..CDCACM_INTERFACE_ID_COUNT - 1] of TUSBInterfaceId = (
  {Control interfaces with no protocol set}
  (bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_NONE),
  {Control interfaces with AT-command sets}
  (bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_AT_V25TER),
  (bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_AT_PCCA101),
  (bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_AT_PCCA101_WAKE),
  (bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_AT_GSM),
  (bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_AT_3G),
  (bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_AT_CDMA));

 {CDC ACM Device and Interface ID constants}
 CDCACM_DEVICE_INTERFACE_ID_COUNT = 58; {Number of supported Device and Interface IDs}

 CDCACM_DEVICE_INTERFACE_ID:array[0..CDCACM_DEVICE_INTERFACE_ID_COUNT - 1] of TUSBDeviceAndInterfaceId = (
  {Nokia S60 phones expose two ACM channels. The first is a modem and is picked up by the standard AT-command information below. The second is 'vendor-specific' but is treated as a serial device}
  {Other USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL/USB_SUBCLASS_CDC_ACM/USB_PROTOCOL_CDC_ACM_VENDOR devices are likely to be Microsoft RNDIS not a modem}
  (idVendor:$0421;idProduct:$042D;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 3250}
  (idVendor:$0421;idProduct:$04D8;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 5500 Sport}
  (idVendor:$0421;idProduct:$04C9;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E50}
  (idVendor:$0421;idProduct:$0419;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E60}
  (idVendor:$0421;idProduct:$044D;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E61}
  (idVendor:$0421;idProduct:$0001;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E61i}
  (idVendor:$0421;idProduct:$0475;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E62}
  (idVendor:$0421;idProduct:$0508;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E65}
  (idVendor:$0421;idProduct:$0418;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E70}
  (idVendor:$0421;idProduct:$0425;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N71}
  (idVendor:$0421;idProduct:$0486;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N73}
  (idVendor:$0421;idProduct:$04DF;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N75}
  (idVendor:$0421;idProduct:$000e;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N77}
  (idVendor:$0421;idProduct:$0445;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N80}
  (idVendor:$0421;idProduct:$042F;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N91 & N91 8GB}
  (idVendor:$0421;idProduct:$048E;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N92}
  (idVendor:$0421;idProduct:$0420;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N93}
  (idVendor:$0421;idProduct:$04E6;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N93i }
  (idVendor:$0421;idProduct:$04B2;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 5700 XpressMusic}
  (idVendor:$0421;idProduct:$0134;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6110 Navigator (China)}
  (idVendor:$0421;idProduct:$046E;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6110 Navigator}
  (idVendor:$0421;idProduct:$002f;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6120 classic & }
  (idVendor:$0421;idProduct:$0088;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6121 classic}
  (idVendor:$0421;idProduct:$00fc;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6124 classic}
  (idVendor:$0421;idProduct:$0042;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E51}
  (idVendor:$0421;idProduct:$00b0;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E66}
  (idVendor:$0421;idProduct:$00ab;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E71}
  (idVendor:$0421;idProduct:$0481;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N76}
  (idVendor:$0421;idProduct:$0007;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N81 & N81 8GB}
  (idVendor:$0421;idProduct:$0071;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N82}
  (idVendor:$0421;idProduct:$04F0;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N95 & N95-3 NAM}
  (idVendor:$0421;idProduct:$0070;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N95 8GB }
  (idVendor:$0421;idProduct:$00e9;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 5320 XpressMusic}
  (idVendor:$0421;idProduct:$0099;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6210 Navigator, RM-367}
  (idVendor:$0421;idProduct:$0128;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6210 Navigator, RM-419}
  (idVendor:$0421;idProduct:$008f;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6220 Classic}
  (idVendor:$0421;idProduct:$00a0;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6650}
  (idVendor:$0421;idProduct:$007b;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N78}
  (idVendor:$0421;idProduct:$0094;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N85}
  (idVendor:$0421;idProduct:$003a;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N96 & N96-3 }
  (idVendor:$0421;idProduct:$00e9;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 5320 XpressMusic}
  (idVendor:$0421;idProduct:$0108;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 5320 XpressMusic 2G}
  (idVendor:$0421;idProduct:$01f5;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N97, RM-505}
  (idVendor:$0421;idProduct:$02e3;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 5230, RM-588}
  (idVendor:$0421;idProduct:$0178;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E63}
  (idVendor:$0421;idProduct:$010e;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E75}
  (idVendor:$0421;idProduct:$02d9;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 6760 Slide}
  (idVendor:$0421;idProduct:$01d0;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E52}
  (idVendor:$0421;idProduct:$0223;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E72}
  (idVendor:$0421;idProduct:$0275;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia X6}
  (idVendor:$0421;idProduct:$026c;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N97 Mini}
  (idVendor:$0421;idProduct:$0154;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia 5800 XpressMusic}
  (idVendor:$0421;idProduct:$04ce;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E90}
  (idVendor:$0421;idProduct:$01d4;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E55}
  (idVendor:$0421;idProduct:$0302;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia N8}
  (idVendor:$0421;idProduct:$0335;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia E7}
  (idVendor:$0421;idProduct:$03cd;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR),  {Nokia C7}
  (idVendor:$04e7;idProduct:$6651;bInterfaceClass:USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL;bInterfaceSubClass:USB_SUBCLASS_CDC_ACM;bInterfaceProtocol:USB_PROTOCOL_CDC_ACM_VENDOR)); {Samsung GTi8510 (INNOV8)}

{==============================================================================}
type
 {CDC ACM specific types}
 PCDCACMDevice = ^TCDCACMDevice;
 TCDCACMDevice = record
  {Serial Properties}
  Serial:TSerialDevice;
  {USB Properties}
  Quirks:LongWord;                          {Unusual behaviours of specific chip versions}
  ReceiveSize:LongWord;                     {Maximum Receive size for Bulk IN Endpoint}
  TransmitSize:LongWord;                    {Maximum Transmit size for Bulk OUT Endpoint}
  ReceiveActive:LongBool;                   {True if a Receive request is currently in progress}
  TransmitActive:LongBool;                  {True if a Transmit request is currently in progress}
  DataInterface:PUSBInterface;              {USB interface for data requests}
  ControlInterface:PUSBInterface;           {USB interface for control requests}
  ReceiveRequest:PUSBRequest;               {USB request Bulk IN Endpoint}
  ReceiveEndpoint:PUSBEndpointDescriptor;   {CDC ACM Bulk IN Endpoint}
  TransmitRequest:PUSBRequest;              {USB request for Bulk OUT Endpoint}
  TransmitEndpoint:PUSBEndpointDescriptor;  {CDC ACM Bulk OUT Endpoint}
  InterruptRequest:PUSBRequest;             {USB request for Interrupt IN Endpoint}
  InterruptEndpoint:PUSBEndpointDescriptor; {CDC ACM Interrupt IN Endpoint}
  PendingCount:LongWord;                    {Number of USB requests pending for this device}
  WaiterThread:TThreadId;                   {Thread waiting for pending requests to complete (for device detachment)}
  {Statistics Properties}
  ReceiveComplete:LongWord;
  TransmitComplete:LongWord;
  InterruptComplete:LongWord;
  InterruptErrors:LongWord;
 end;

{==============================================================================}
{var}
 {CDC ACM specific variables}

{==============================================================================}
{Initialization Functions}
procedure CDCACMInit;

{==============================================================================}
{CDC ACM Serial Functions}
function CDCACMSerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
function CDCACMSerialDeviceClose(Serial:PSerialDevice):LongWord;

function CDCACMSerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function CDCACMSerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

{==============================================================================}
{CDC ACM USB Functions}
function CDCACMDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function CDCACMDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure CDCACMReceiveStart(Request:PUSBRequest);
procedure CDCACMReceiveWorker(Request:PUSBRequest);
procedure CDCACMReceiveComplete(Request:PUSBRequest);

procedure CDCACMTransmitStart(Request:PUSBRequest);
procedure CDCACMTransmitWorker(Request:PUSBRequest);
procedure CDCACMTransmitComplete(Request:PUSBRequest);

procedure CDCACMInterruptWorker(Request:PUSBRequest);
procedure CDCACMInterruptComplete(Request:PUSBRequest);

{==============================================================================}
{CDC ACM Helper Functions}
function CDCACMCheckDevice(Device:PUSBDevice;var Quirks:LongWord):LongWord;
function CDCACMCheckInterface(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function CDCACMCheckDeviceAndInterface(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

function CDCACMFindInterfaces(Device:PUSBDevice;Interrface:PUSBInterface;var DataInterface,ControlInterface:PUSBInterface;Quirks:LongWord;var Capabilities:Byte):LongWord;

function CDCACMControlRequest(Serial:PCDCACMDevice;Request:Byte;Value:Word;Data:Pointer;Size:Word):LongWord;

function CDCACMGetLineRequest(Serial:PCDCACMDevice;var LineCoding:TUSBCDCLineCoding):LongWord;
function CDCACMSetLineRequest(Serial:PCDCACMDevice;const LineCoding:TUSBCDCLineCoding):LongWord;

function CDCACMSetControlRequest(Serial:PCDCACMDevice;Control:Word):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {CDC ACM specific variables}
 CDCACMInitialized:Boolean;

 CDCACMDriver:PUSBDriver;  {CDC ACM Driver interface (Set by CDCACMInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CDCACMInit;
var
 Status:LongWord;
 WorkInt:LongWord;
begin
 {}
 {Check Initialized}
 if CDCACMInitialized then Exit;

 {Create CDC ACM Driver}
 CDCACMDriver:=USBDriverCreate;
 if CDCACMDriver <> nil then
  begin
   {Update CDC ACM Driver}
   {Driver}
   CDCACMDriver.Driver.DriverName:=CDCACM_DRIVER_NAME;
   {USB}
   CDCACMDriver.DriverBind:=CDCACMDriverBind;
   CDCACMDriver.DriverUnbind:=CDCACMDriverUnbind;

   {Register CDC ACM Driver}
   Status:=USBDriverRegister(CDCACMDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'CDC ACM: Failed to register CDC ACM driver: ' + USBStatusToString(Status));

     {Destroy Driver}
     USBDriverDestroy(CDCACMDriver);
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'CDC ACM: Failed to create CDC ACM driver');
  end;

 {Check Environment Variables}
 {CDCACM_BIND_DELAY}
 WorkInt:=StrToIntDef(EnvironmentGet('CDCACM_BIND_DELAY'),CDCACM_BIND_DELAY);
 if WorkInt <> CDCACM_BIND_DELAY then CDCACM_BIND_DELAY:=WorkInt;

 {CDCACM_MAX_TRANSMIT}
 WorkInt:=StrToIntDef(EnvironmentGet('CDCACM_MAX_TRANSMIT'),CDCACM_MAX_TRANSMIT);
 if WorkInt <> CDCACM_MAX_TRANSMIT then CDCACM_MAX_TRANSMIT:=WorkInt;

 CDCACMInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{CDC ACM Serial Functions}
function CDCACMSerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
{Implementation of SerialDeviceOpen API for CDCACM Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceOpen instead}
var
 Control:Word;
 Status:LongWord;
 Device:PUSBDevice;
 LineCoding:TUSBCDCLineCoding;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM: Device Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + SerialDataBitsToString(DataBits) + ' StopBits=' + SerialStopBitsToString(StopBits) + ' Parity=' + SerialParityToString(Parity) + ' FlowControl=' + SerialFlowControlToString(FlowControl) + ')');
 {$ENDIF}

 {Get Device}
 Device:=PUSBDevice(Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Check Baud Rate}
 {Nothing}

 {Check Data Bits}
 if (DataBits < CDCACM_MIN_DATABITS) or (DataBits > CDCACM_MAX_DATABITS) then Exit;

 {Check Stop Bits}
 if (StopBits < CDCACM_MIN_STOPBITS) or (StopBits > CDCACM_MAX_STOPBITS) then Exit;

 {Check Parity}
 if Parity > CDCACM_MAX_PARITY then Exit;

 {Check Flow Control}
 if FlowControl > CDCACM_MAX_FLOW then Exit;

 {Adjust Baud Rate}
 if BaudRate = SERIAL_BAUD_RATE_DEFAULT then
  begin
   BaudRate:=SERIAL_BAUD_RATE_STANDARD;
  end;

 {Check Quirks}
 if (PCDCACMDevice(Serial).Quirks and CDCACM_QUIRK_CLEAR_HALT_CONDITIONS) <> 0 then
  begin
   {Clear Bulk IN/OUT Endpoints}
   USBDeviceClearFeature(Device,PCDCACMDevice(Serial).ReceiveEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);  //To Do //USB_CONTROL_SET_TIMEOUT
   USBDeviceClearFeature(Device,PCDCACMDevice(Serial).TransmitEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);  //To Do //USB_CONTROL_SET_TIMEOUT
  end;

 {Get Line Coding} {May cause failures with some devices}
 {if CDCACMGetLineRequest(PCDCACMDevice(Serial),LineCoding) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;}

 {Setup Line Coding}
 FillChar(LineCoding,SizeOf(TUSBCDCLineCoding),0);
 {Baud Rate}
 LineCoding.dwDTERate:=BaudRate;
 {Data Bits}
 case DataBits of
  SERIAL_DATA_8BIT:LineCoding.bDataBits:=8;
  SERIAL_DATA_7BIT:LineCoding.bDataBits:=7;
  SERIAL_DATA_6BIT:LineCoding.bDataBits:=6;
  SERIAL_DATA_5BIT:LineCoding.bDataBits:=5;
 end;
 {Stop Bits}
 case StopBits of
  SERIAL_STOP_1BIT:LineCoding.bCharFormat:=USB_CDC_1_STOP_BITS;
  SERIAL_STOP_1BIT5:LineCoding.bCharFormat:=USB_CDC_1_5_STOP_BITS;
  SERIAL_STOP_2BIT:LineCoding.bCharFormat:=USB_CDC_2_STOP_BITS;
 end;
 {Parity}
 case Parity of
  SERIAL_PARITY_NONE:LineCoding.bParityType:=USB_CDC_NO_PARITY;
  SERIAL_PARITY_ODD:LineCoding.bParityType:=USB_CDC_ODD_PARITY;
  SERIAL_PARITY_EVEN:LineCoding.bParityType:=USB_CDC_EVEN_PARITY;
  SERIAL_PARITY_MARK:LineCoding.bParityType:=USB_CDC_MARK_PARITY;
  SERIAL_PARITY_SPACE:LineCoding.bParityType:=USB_CDC_SPACE_PARITY;
 end;

 {Setup Line Control}
 {Flow Control}
 case FlowControl of
  SERIAL_FLOW_NONE:Control:=0;
  SERIAL_FLOW_RTS_CTS:Control:=USB_CDC_ACM_CTRL_RTS;
  SERIAL_FLOW_DSR_DTR:Control:=USB_CDC_ACM_CTRL_DTR;
 end;

 {Set Control Request}
 if CDCACMSetControlRequest(PCDCACMDevice(Serial),Control) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Set Line Request}
 if CDCACMSetLineRequest(PCDCACMDevice(Serial),LineCoding) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Check Max Transmit}
 if CDCACM_MAX_TRANSMIT = 0 then CDCACM_MAX_TRANSMIT:=PCDCACMDevice(Serial).TransmitSize;

 {Check Receive Depth}
 if ReceiveDepth = 0 then ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;

 {Check Transmit Depth}
 if TransmitDepth = 0 then TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;

 {Allocate Recieve}
 EventReset(Serial.Receive.Wait);
 Serial.Receive.Start:=0;
 Serial.Receive.Count:=0;
 Serial.Receive.Size:=ReceiveDepth;
 Serial.Receive.Data:=GetMem(ReceiveDepth);
 if Serial.Receive.Data = nil then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Allocate Transmit}
 EventSet(Serial.Transmit.Wait);
 Serial.Transmit.Start:=0;
 Serial.Transmit.Count:=0;
 Serial.Transmit.Size:=TransmitDepth;
 Serial.Transmit.Data:=GetMem(TransmitDepth);
 if Serial.Transmit.Data = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Allocate Receive Request}
 PCDCACMDevice(Serial).ReceiveRequest:=USBRequestAllocate(Device,PCDCACMDevice(Serial).ReceiveEndpoint,CDCACMReceiveComplete,PCDCACMDevice(Serial).ReceiveSize,Serial);
 if PCDCACMDevice(Serial).ReceiveRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Allocate Transmit Request}
 PCDCACMDevice(Serial).TransmitRequest:=USBRequestAllocate(Device,PCDCACMDevice(Serial).TransmitEndpoint,CDCACMTransmitComplete,PCDCACMDevice(Serial).TransmitSize,Serial);
 if PCDCACMDevice(Serial).TransmitRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   {Release Receive Request}
   USBRequestRelease(PCDCACMDevice(Serial).ReceiveRequest);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Allocate Interrupt Request}
 PCDCACMDevice(Serial).InterruptRequest:=USBRequestAllocate(Device,PCDCACMDevice(Serial).InterruptEndpoint,CDCACMInterruptComplete,PCDCACMDevice(Serial).InterruptEndpoint.wMaxPacketSize,Serial);
 if PCDCACMDevice(Serial).InterruptRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   {Release Receive Request}
   USBRequestRelease(PCDCACMDevice(Serial).ReceiveRequest);

   {Release Transmit Request}
   USBRequestRelease(PCDCACMDevice(Serial).TransmitRequest);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Update Pending}
 Inc(PCDCACMDevice(Serial).PendingCount);

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM: Submitting interrupt request');
 {$ENDIF}

 {Submit Interrupt Request}
 Status:=USBRequestSubmit(PCDCACMDevice(Serial).InterruptRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'CDC ACM: Failed to submit interrupt request: ' + USBStatusToString(Status));

   {Update Pending}
   Dec(PCDCACMDevice(Serial).PendingCount);

   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   {Release Receive Request}
   USBRequestRelease(PCDCACMDevice(Serial).ReceiveRequest);

   {Release Transmit Request}
   USBRequestRelease(PCDCACMDevice(Serial).TransmitRequest);

   {Release Interrupt Request}
   USBRequestRelease(PCDCACMDevice(Serial).InterruptRequest);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Update Pending}
 Inc(PCDCACMDevice(Serial).PendingCount);

 {Set Active}
 PCDCACMDevice(Serial).ReceiveActive:=True;

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM: Submitting receive request');
 {$ENDIF}

 {Submit Receive Request}
 Status:=USBRequestSubmit(PCDCACMDevice(Serial).ReceiveRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'CDC ACM: Failed to submit receive request: ' + USBStatusToString(Status));

   {Reset Active}
   PCDCACMDevice(Serial).ReceiveActive:=False;

   {Update Pending}
   Dec(PCDCACMDevice(Serial).PendingCount);

   {Cancel Interrupt Request}
   USBRequestCancel(PCDCACMDevice(Serial).InterruptRequest);

   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   {Release Receive Request}
   USBRequestRelease(PCDCACMDevice(Serial).ReceiveRequest);

   {Release Transmit Request}
   USBRequestRelease(PCDCACMDevice(Serial).TransmitRequest);

   {Release Interrupt Request}
   USBRequestRelease(PCDCACMDevice(Serial).InterruptRequest);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Update Properties}
 Serial.Properties.BaudRate:=BaudRate;
 Serial.Properties.DataBits:=DataBits;
 Serial.Properties.StopBits:=StopBits;
 Serial.Properties.Parity:=Parity;
 Serial.Properties.FlowControl:=FlowControl;
 Serial.Properties.ReceiveDepth:=ReceiveDepth;
 Serial.Properties.TransmitDepth:=TransmitDepth;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function CDCACMSerialDeviceClose(Serial:PSerialDevice):LongWord;
{Implementation of SerialDeviceClose API for CDCACM Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceClose instead}
var
 Message:TMessage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM: Device Close');
 {$ENDIF}

 {Cancel Receive Request}
 USBRequestCancel(PCDCACMDevice(Serial).ReceiveRequest);

 {Cancel Interrupt Request}
 USBRequestCancel(PCDCACMDevice(Serial).InterruptRequest);

 {Check Pending}
 if PCDCACMDevice(Serial).PendingCount <> 0 then
  begin
   {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
   if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM: Waiting for ' + IntToStr(PCDCACMDevice(Serial).PendingCount) + ' pending requests to complete');
   {$ENDIF}

   {Wait for Pending}

   {Setup Waiter}
   PCDCACMDevice(Serial).WaiterThread:=GetCurrentThreadId;

   {Release the Lock}
   MutexUnlock(Serial.Lock);

   {Wait for Message}
   ThreadReceiveMessage(Message);

   {Acquire the Lock}
   if MutexLock(Serial.Lock) <> ERROR_SUCCESS then Exit;
  end;

 {Release Interrupt Request}
 USBRequestRelease(PCDCACMDevice(Serial).InterruptRequest);

 {Release Transmit Request}
 USBRequestRelease(PCDCACMDevice(Serial).TransmitRequest);

 {Release Receive Request}
 USBRequestRelease(PCDCACMDevice(Serial).ReceiveRequest);

 {Release Receive}
 Serial.Receive.Start:=0;
 Serial.Receive.Count:=0;
 Serial.Receive.Size:=0;
 FreeMem(Serial.Receive.Data);

 {Release Transmit}
 Serial.Transmit.Start:=0;
 Serial.Transmit.Count:=0;
 Serial.Transmit.Size:=0;
 FreeMem(Serial.Transmit.Data);

 {Update Properties}
 Serial.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 Serial.Properties.DataBits:=SERIAL_DATA_8BIT;
 Serial.Properties.StopBits:=SERIAL_STOP_1BIT;
 Serial.Properties.Parity:=SERIAL_PARITY_NONE;
 Serial.Properties.FlowControl:=SERIAL_FLOW_NONE;
 Serial.Properties.ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
 Serial.Properties.TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function CDCACMSerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceRead API for CDCACM Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceRead instead}
var
 Data:Pointer;
 Total:LongWord;
 Offset:PtrUint;
 Removed:LongWord;
 Available:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM: Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if not(PCDCACMDevice(Serial).ReceiveActive) and ((Serial.Receive.Size - Serial.Receive.Count) >= PCDCACMDevice(Serial).ReceiveSize) then
    begin
     {Start Receive}
     CDCACMReceiveStart(PCDCACMDevice(Serial).ReceiveRequest);
    end;

   {Check Non Blocking}
   if ((Flags and SERIAL_READ_NON_BLOCK) <> 0) and (Serial.Receive.Count = 0) then
    begin
     Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;

   {Check Peek Buffer}
   if (Flags and SERIAL_READ_PEEK_BUFFER) <> 0 then
    begin
     Count:=Serial.Receive.Count;
     Result:=ERROR_SUCCESS;
     Break;
    end;

   {Release the Lock}
   MutexUnlock(Serial.Lock);

   {Wait for Data}
   if EventWait(Serial.Receive.Wait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Serial.Lock) = ERROR_SUCCESS then
      begin
       {Start Read}
       Data:=SerialBufferReadStart(@Serial.Receive,Available);
       while (Data <> nil) and (Available > 0) and (Size > 0) do
        begin
         {Get Removed}
         Removed:=Min(Size,Available);

         {Copy Data}
         System.Move(Data^,Pointer(Buffer + Offset)^,Removed);

         {Update Statistics}
         Inc(Serial.ReceiveCount,Removed);

         {Update Count}
         Inc(Count,Removed);

         {Update Size and Offset}
         Dec(Size,Removed);
         Inc(Offset,Removed);

         {Complete Read}
         SerialBufferReadComplete(@Serial.Receive,Removed);

         {Start Read}
         Data:=SerialBufferReadStart(@Serial.Receive,Available);
        end;

       {Check Available}
       if Available = 0 then
        begin
         {Reset Event}
         EventReset(Serial.Receive.Wait);
        end;

       {Check State}
       if (Size = 0) and not(PCDCACMDevice(Serial).ReceiveActive) and ((Serial.Receive.Size - Serial.Receive.Count) >= PCDCACMDevice(Serial).ReceiveSize) then
        begin
         {Start Receive}
         CDCACMReceiveStart(PCDCACMDevice(Serial).ReceiveRequest);
        end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;
  end;

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function CDCACMSerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceWrite API for CDCACM Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceWrite instead}
var
 Data:Pointer;
 Empty:Boolean;
 Total:LongWord;
 Offset:PtrUint;
 Added:LongWord;
 Available:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM: Device Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check Non Blocking}
   if ((Flags and SERIAL_WRITE_NON_BLOCK) <> 0) and ((Serial.Transmit.Size - Serial.Transmit.Count) = 0) then
    begin
     Result:=ERROR_INSUFFICIENT_BUFFER;
     Break;
    end;

   {Check Peek Buffer}
   if (Flags and SERIAL_WRITE_PEEK_BUFFER) <> 0 then
    begin
     Count:=Serial.Transmit.Size - Serial.Transmit.Count;
     Result:=ERROR_SUCCESS;
     Break;
    end;

   {Release the Lock}
   MutexUnlock(Serial.Lock);

   {Wait for Space}
   if EventWait(Serial.Transmit.Wait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Serial.Lock) = ERROR_SUCCESS then
      begin
       {Check Empty}
       Empty:=(Serial.Transmit.Count = 0);

       {Start Write}
       Data:=SerialBufferWriteStart(@Serial.Transmit,Available);
       while (Data <> nil) and (Available > 0) and (Size > 0) do
        begin
         {Get Added}
         Added:=Min(Size,Available);

         {Copy Data}
         System.Move(Pointer(Buffer + Offset)^,Data^,Added);

         {Update Statistics}
         Inc(Serial.TransmitCount,Added);

         {Update Count}
         Inc(Count,Added);

         {Update Size and Offset}
         Dec(Size,Added);
         Inc(Offset,Added);

         {Complete Write}
         SerialBufferWriteComplete(@Serial.Transmit,Added);

         {Start Write}
         Data:=SerialBufferWriteStart(@Serial.Transmit,Available);
        end;

       {Check Available}
       if Available = 0 then
        begin
         {Reset Event}
         EventReset(Serial.Transmit.Wait);
        end;

       {Check Empty}
       if Empty and not(PCDCACMDevice(Serial).TransmitActive) then
        begin
         {Start Transmit}
         CDCACMTransmitStart(PCDCACMDevice(Serial).TransmitRequest);
        end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;
  end;

 {$IF DEFINED(CDCACM_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'CDC ACM:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{CDC ACM USB Functions}
function CDCACMDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the CDC ACM driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Quirks:LongWord;
 Status:LongWord;
 Capabilities:Byte;
 Serial:PCDCACMDevice;
 TempInterface:PUSBInterface;
 DataInterface:PUSBInterface;
 ControlInterface:PUSBInterface;
 TempEndpoint:PUSBEndpointDescriptor;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
 InterruptEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF CDCACM_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Setup Defaults}
 Quirks:=CDCACM_QUIRK_NONE;

 {Check Interface (Bind to either device or interface depending on match)}
 if Interrface = nil then
  begin
   {Check CDC ACM Device}
   if CDCACMCheckDevice(Device,Quirks) <> USB_STATUS_SUCCESS then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Device not found in supported device list');
     {$ENDIF}

     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end
 else
  begin
   {Check CDC ACM Interface}
   if CDCACMCheckInterface(Device,Interrface) <> USB_STATUS_SUCCESS then
    begin
     {Check CDC ACM Device and Interface}
     if CDCACMCheckDeviceAndInterface(Device,Interrface) <> USB_STATUS_SUCCESS then
      begin
       {$IFDEF CDCACM_DEBUG}
       if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Device not found in supported interface list');
       {$ENDIF}

       {Return Result}
       Result:=USB_STATUS_DEVICE_UNSUPPORTED;
       Exit;
      end;
    end;
  end;

 {Check Quirks}
 if (Quirks and CDCACM_QUIRK_IGNORE_DEVICE) <> 0 then
  begin
   {$IFDEF CDCACM_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Ignoring device due to CDCACM_QUIRK_IGNORE_DEVICE flag');
   {$ENDIF}

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Quirks}
 if (Quirks and CDCACM_QUIRK_NO_UNION_NORMAL) <> 0 then
  begin
   {$IFDEF CDCACM_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: No union descriptor, assuming standard interfaces');
   {$ENDIF}

   {Device does not contain a union functional descriptor}
   {Assume that the data and control interfaces are fixed}
   {Get Data Interface}
   DataInterface:=USBDeviceFindInterfaceByIndex(Device,1);

   {Get Control Interface}
   ControlInterface:=USBDeviceFindInterfaceByIndex(Device,0);

   {Setup Capabilities}
   Capabilities:=0;

   {Check the interfaces}
   if (DataInterface = nil) or (ControlInterface = nil) then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Device does not have required interfaces available');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end
 else
  begin
   {Determine the interfaces by reading the functional descriptors}
   Status:=CDCACMFindInterfaces(Device,Interrface,DataInterface,ControlInterface,Quirks,Capabilities);

   {Check the result and then interfaces}
   if (Status <> USB_STATUS_SUCCESS) or (DataInterface = nil) or (ControlInterface = nil) then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Device does not have required interfaces available');
     {$ENDIF}

     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;

 {Check Interfaces}
 if DataInterface = ControlInterface then
  begin
   {Combined interfaces}
   if USB_LOG_ENABLED then USBLogInfo(Device,'CDC ACM: Device has combined control and data interfaces');

   {Check Endpoints}
   if DataInterface.Descriptor.bNumEndpoints <> 3 then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Combined interface must have 3 endpoints');
     {$ENDIF}

     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Update Quirks}
   Quirks:=Quirks or CDCACM_QUIRK_NO_CAP_LINE;

   {Get Endpoints}
   ReceiveEndpoint:=USBDeviceFindEndpointByType(Device,DataInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
   TransmitEndpoint:=USBDeviceFindEndpointByType(Device,DataInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
   InterruptEndpoint:=USBDeviceFindEndpointByType(Device,DataInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);

   {Check Endpoints}
   if (ReceiveEndpoint = nil) or (TransmitEndpoint = nil) or (InterruptEndpoint = nil) then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Device does not have required endpoints available');
     {$ENDIF}

     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end
 else
  begin
   {Separate Interfaces}
   {Check for switched interfaces}
   if DataInterface.Descriptor.bInterfaceClass <> USB_CLASS_CODE_CDC_DATA then
    begin
     if ControlInterface.Descriptor.bInterfaceClass = USB_CLASS_CODE_CDC_DATA then
      begin
       if USB_LOG_ENABLED then USBLogInfo(Device,'CDC ACM: Device has swapped control and data interfaces');

       {Swap Interfaces}
       TempInterface:=DataInterface;
       DataInterface:=ControlInterface;
       ControlInterface:=TempInterface;
      end
     else
      begin
       {$IFDEF CDCACM_DEBUG}
       if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Device has invalid interface classes');
       {$ENDIF}

       Result:=USB_STATUS_DEVICE_UNSUPPORTED;
       Exit;
      end;
    end;

   {Check the passed interfaces}
   if (Interrface <> nil) and (Interrface <> ControlInterface) then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Binding only supported on the control interface');
     {$ENDIF}

     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Check the data interface}
   if (Interrface <> nil) and (DataInterface.Driver <> nil) then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Data interface already bound to another driver');
     {$ENDIF}

     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Check Endpoints}
   if DataInterface.Descriptor.bNumEndpoints < 2 then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Data interface must have at least 2 endpoints');
     {$ENDIF}

     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Check Endpoints}
   if ControlInterface.Descriptor.bNumEndpoints < 1 then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Control interface must have at least 1 endpoint');
     {$ENDIF}

     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Get Endpoints (May be Bulk or Interrupt)}
   ReceiveEndpoint:=DataInterface.Endpoints[0];
   TransmitEndpoint:=DataInterface.Endpoints[1];
   InterruptEndpoint:=ControlInterface.Endpoints[0];

   {Check Endpoints}
   if not USBIsInEndpoint(ReceiveEndpoint) then
    begin
     if USB_LOG_ENABLED then USBLogInfo(Device,'CDC ACM: Device has swapped receive and transmit endpoints');

     {Swap Endpoints}
     TempEndpoint:=ReceiveEndpoint;
     ReceiveEndpoint:=TransmitEndpoint;
     TransmitEndpoint:=TempEndpoint;
    end;
  end;

 {Check Configuration}
 if (Interrface = nil) and (Device.ConfigurationValue = 0) then
  begin
   {$IFDEF CDCACM_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Assigning configuration ' + IntToStr(Device.Configuration.Descriptor.bConfigurationValue) + ' (' + IntToStr(Device.Configuration.Descriptor.bNumInterfaces) + ' interfaces available)');
   {$ENDIF}

   {Set Configuration}
   Status:=USBDeviceSetConfiguration(Device,Device.Configuration.Descriptor.bConfigurationValue);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Failed to set device configuration: ' + USBStatusToString(Status));

     {Return Result}
     Result:=Status;
     Exit;
    end;
  end;

 {Set Interface}
 if USBDeviceSetInterface(Device,DataInterface.Descriptor.bInterfaceNumber,DataInterface.Descriptor.bAlternateSetting) <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'CDC ACM: Failed to set interface alternate setting');

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {USB device reset not required because the USB core already did a reset on the port during attach}

 {Some devices need a short time to settle before proceeding}
 Sleep(CDCACM_BIND_DELAY);

 {Create Serial}
 Serial:=PCDCACMDevice(SerialDeviceCreateEx(SizeOf(TCDCACMDevice)));
 if Serial = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'CDC ACM: Failed to create new serial device');

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update Serial}
 {Device}
 Serial.Serial.Device.DeviceBus:=DEVICE_BUS_USB;
 Serial.Serial.Device.DeviceType:=SERIAL_TYPE_USB;
 Serial.Serial.Device.DeviceFlags:=SERIAL_FLAG_DATA_8BIT or SERIAL_FLAG_DATA_7BIT or SERIAL_FLAG_DATA_6BIT or SERIAL_FLAG_DATA_5BIT or SERIAL_FLAG_STOP_1BIT or SERIAL_FLAG_STOP_2BIT or SERIAL_FLAG_STOP_1BIT5 or SERIAL_FLAG_PARITY_ODD or SERIAL_FLAG_PARITY_EVEN or SERIAL_FLAG_PARITY_MARK or SERIAL_FLAG_PARITY_SPACE or SERIAL_FLAG_FLOW_RTS_CTS or SERIAL_FLAG_FLOW_DSR_DTR;
 Serial.Serial.Device.DeviceData:=Device;
 Serial.Serial.Device.DeviceDescription:=CDCACM_SERIAL_DESCRIPTION;
 {Serial}
 Serial.Serial.SerialState:=SERIAL_STATE_CLOSED;
 Serial.Serial.SerialStatus:=SERIAL_STATUS_NONE;
 Serial.Serial.DeviceOpen:=CDCACMSerialDeviceOpen;
 Serial.Serial.DeviceClose:=CDCACMSerialDeviceClose;
 Serial.Serial.DeviceRead:=CDCACMSerialDeviceRead;
 Serial.Serial.DeviceWrite:=CDCACMSerialDeviceWrite;
 {Driver}
 Serial.Serial.Properties.Flags:=Serial.Serial.Device.DeviceFlags;
 Serial.Serial.Properties.MinRate:=CDCACM_MIN_BAUD;
 Serial.Serial.Properties.MaxRate:=CDCACM_MAX_BAUD;
 Serial.Serial.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 Serial.Serial.Properties.DataBits:=SERIAL_DATA_8BIT;
 Serial.Serial.Properties.StopBits:=SERIAL_STOP_1BIT;
 Serial.Serial.Properties.Parity:=SERIAL_PARITY_NONE;
 Serial.Serial.Properties.FlowControl:=SERIAL_FLOW_NONE;
 Serial.Serial.Properties.ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
 Serial.Serial.Properties.TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;
 {USB}
 Serial.Quirks:=Quirks;
 Serial.ReceiveSize:=ReceiveEndpoint.wMaxPacketSize;
 Serial.TransmitSize:=TransmitEndpoint.wMaxPacketSize;
 Serial.DataInterface:=DataInterface;
 Serial.ControlInterface:=ControlInterface;
 Serial.ReceiveEndpoint:=ReceiveEndpoint;
 Serial.TransmitEndpoint:=TransmitEndpoint;
 Serial.InterruptEndpoint:=InterruptEndpoint;
 Serial.WaiterThread:=INVALID_HANDLE_VALUE;

 {Register Serial}
 if SerialDeviceRegister(@Serial.Serial) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'CDC ACM: Failed to register new serial device');

   {Destroy Serial}
   SerialDeviceDestroy(@Serial.Serial);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Endpoint Interval}
 if InterruptEndpoint.bInterval < 1 then
  begin
   InterruptEndpoint.bInterval:=16;
  end;

 {Check Interface}
 if Interrface = nil then
  begin
   {Update Device}
   Device.DriverData:=Serial;
  end
 else
  begin
   {Update Interface}
   Interrface.DriverData:=Serial;

   {Update Data Interface}
   DataInterface.DriverData:=Serial;
   DataInterface.Driver:=CDCACMDriver;
  end;

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function CDCACMDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the CDC ACM driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Serial:PCDCACMDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then
  begin
   {Check Driver}
   if Device.Driver <> CDCACMDriver then Exit;
  end
 else
  begin
   {Check Driver}
   if Interrface.Driver <> CDCACMDriver then Exit;
  end;

 {$IFDEF CDCACM_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Check Interface}
 if Interrface = nil then
  begin
   {Get Serial}
   Serial:=PCDCACMDevice(Device.DriverData);
  end
 else
  begin
   {Get Serial}
   Serial:=PCDCACMDevice(Interrface.DriverData);
  end;
 if Serial = nil then Exit;

 {Check Interface}
 if (Interrface <> nil) and (Interrface <> Serial.ControlInterface) then
  begin
   {Unbind on Control Interface}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;

 {Close Serial}
 SerialDeviceClose(@Serial.Serial);

 {Check Interface}
 if Interrface = nil then
  begin
   {Update Device}
   Device.DriverData:=nil;
  end
 else
  begin
   {Update Interface}
   Interrface.DriverData:=nil;

   {Update Data Interface}
   Serial.DataInterface.DriverData:=nil;
   Serial.DataInterface.Driver:=nil;
  end;

 {Deregister Serial}
 if SerialDeviceDeregister(@Serial.Serial) <> ERROR_SUCCESS then Exit;

 {Destroy Serial}
 SerialDeviceDestroy(@Serial.Serial);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure CDCACMReceiveStart(Request:PUSBRequest);
{Called to continue reception of data to the receive buffer}
{Request: The USB receive request to use}

{Note: Caller must hold the lock on the serial device}
var
 Count:LongWord;
 Available:LongWord;
 Status:LongWord;
 Serial:PCDCACMDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PCDCACMDevice(Request.DriverData);
 if Serial = nil then Exit;

 {Setup Count}
 Count:=0;
 Available:=Serial.Serial.Receive.Size - Serial.Serial.Receive.Count;
 if Available >= Serial.ReceiveSize then
  begin
   Count:=Serial.ReceiveSize;
  end;

 {Check Count}
 if Count > 0 then
  begin
   {Update Request}
   Request.Size:=Count;

   {Update Pending}
   Inc(Serial.PendingCount);

   {Set Active}
   Serial.ReceiveActive:=True;

   {$IFDEF CDCACM_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Resubmitting receive request');
   {$ENDIF}

   {Resubmit Request}
   Status:=USBRequestSubmit(Request);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed to resubmit receive request: ' + USBStatusToString(Status));

     {Reset Active}
     Serial.ReceiveActive:=False;

     {Update Pending}
     Dec(Serial.PendingCount);
    end;
  end;
end;

{==============================================================================}

procedure CDCACMReceiveWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from the CDC ACM bulk IN endpoint}
{Request: The USB request which has completed}
var
 Data:Pointer;
 Size:LongWord;
 Offset:PtrUInt;
 Count:LongWord;
 Added:LongWord;
 Status:LongWord;
 Message:TMessage;
 Available:LongWord;
 Serial:PCDCACMDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PCDCACMDevice(Request.DriverData);
 if Serial <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Serial.Serial.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Serial.ReceiveComplete);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {$IFDEF CDCACM_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF CDCACM_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Receive complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}

        {Update Status}
        Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus and not(SERIAL_STATUS_BREAK_ERROR or SERIAL_STATUS_PARITY_ERROR or SERIAL_STATUS_FRAMING_ERROR or SERIAL_STATUS_OVERRUN_ERROR);

        {Check Size}
        if Request.ActualSize > 0 then
         begin
          {Setup Count, Size and Offset}
          Size:=Request.ActualSize;
          Count:=0;
          Offset:=0;

          {Start Write}
          Data:=SerialBufferWriteStart(@Serial.Serial.Receive,Available);
          while (Data <> nil) and (Available > 0) and (Size > 0) do
           begin
            {Get Added}
            Added:=Min(Size,Available);

            {Copy Data}
            System.Move(Pointer(Request.Data + Offset)^,Data^,Added);

            {Update Count}
            Inc(Count,Added);

            {Update Size and Offset}
            Dec(Size,Added);
            Inc(Offset,Added);

            {Complete Write}
            SerialBufferWriteComplete(@Serial.Serial.Receive,Added);

            {Start Write}
            Data:=SerialBufferWriteStart(@Serial.Serial.Receive,Available);
           end;

          {Check Size}
          if Size > 0 then
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Buffer overrun, ' + IntToStr(Size) + ' bytes discarded');
           end;

          {Check Count}
          if Count > 0 then
           begin
            {Set Event}
            EventSet(Serial.Serial.Receive.Wait);
           end;
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed receive request (Status=' + USBStatusToString(Request.Status) + ')');

        {Update Statistics}
        Inc(Serial.Serial.ReceiveErrors);
       end;

      {Reset Active}
      Serial.ReceiveActive:=False;

      {Update Pending}
      Dec(Serial.PendingCount);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {Check Pending}
        if Serial.PendingCount = 0 then
         begin
          {Check Waiter}
          if Serial.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF CDCACM_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Serial.WaiterThread,Message);
            Serial.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Start Receive}
        CDCACMReceiveStart(Request);
       end;
     finally
      {Release the Lock}
      MutexUnlock(Serial.Serial.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Receive request invalid');
  end;
end;

{==============================================================================}

procedure CDCACMReceiveComplete(Request:PUSBRequest);
{Called when a USB request from the CDC ACM bulk IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(CDCACMReceiveWorker),Request,nil)
end;

{==============================================================================}

procedure CDCACMTransmitStart(Request:PUSBRequest);
{Called to continue transmission of data from the transmit buffer}
{Request: The USB transmit request to use}

{Note: Caller must hold the lock on the serial device}
var
 Data:Pointer;
 Size:LongWord;
 Offset:PtrUInt;
 Count:LongWord;
 Status:LongWord;
 Removed:LongWord;
 Available:LongWord;
 Serial:PCDCACMDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PCDCACMDevice(Request.DriverData);
 if Serial = nil then Exit;

 {Setup Count, Size and Offset}
 Size:=Min(CDCACM_MAX_TRANSMIT,Serial.TransmitSize);
 Count:=0;
 Offset:=0;

 {Start Read}
 Data:=SerialBufferReadStart(@Serial.Serial.Transmit,Available);
 while (Data <> nil) and (Available > 0) and (Size > 0) do
  begin
   {Get Removed}
   Removed:=Min(Size,Available);

   {Copy Data}
   System.Move(Data^,Pointer(Request.Data + Offset)^,Removed);

   {Update Count}
   Inc(Count,Removed);

   {Update Size and Offset}
   Dec(Size,Removed);
   Inc(Offset,Removed);

   {Complete Read}
   SerialBufferReadComplete(@Serial.Serial.Transmit,Removed);

   {Start Read}
   Data:=SerialBufferReadStart(@Serial.Serial.Transmit,Available);
  end;

 {Check Count}
 if Count > 0 then
  begin
   {Set Event}
   EventSet(Serial.Serial.Transmit.Wait);

   {Update Request}
   Request.Size:=Count;

   {Update Pending}
   Inc(Serial.PendingCount);

   {Set Active}
   Serial.TransmitActive:=True;

   {$IFDEF CDCACM_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Submitting transmit request');
   {$ENDIF}

   {Submit Request}
   Status:=USBRequestSubmit(Request);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed to submit transmit request: ' + USBStatusToString(Status));

     {Reset Active}
     Serial.TransmitActive:=False;

     {Update Pending}
     Dec(Serial.PendingCount);
    end;
  end;
end;

{==============================================================================}

procedure CDCACMTransmitWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request to the CDC ACM bulk OUT endpoint}
{Request: The USB request which has completed}
var
 Message:TMessage;
 Serial:PCDCACMDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PCDCACMDevice(Request.DriverData);
 if Serial <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Serial.Serial.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Serial.TransmitComplete);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {$IFDEF CDCACM_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Close pending, setting transmit request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF CDCACM_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Transmit complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed transmit request (Status=' + USBStatusToString(Request.Status) + ')');

        {Update Statistics}
        Inc(Serial.Serial.TransmitErrors);
       end;

      {Reset Active}
      Serial.TransmitActive:=False;

      {Update Pending}
      Dec(Serial.PendingCount);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {Check Pending}
        if Serial.PendingCount = 0 then
         begin
          {Check Waiter}
          if Serial.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF CDCACM_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Serial.WaiterThread,Message);
            Serial.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Start Transmit}
        CDCACMTransmitStart(Request);
       end;
     finally
      {Release the Lock}
      MutexUnlock(Serial.Serial.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Transmit request invalid');
  end;
end;

{==============================================================================}

procedure CDCACMTransmitComplete(Request:PUSBRequest);
{Called when a USB request to the CDC ACM bulk OUT endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(CDCACMTransmitWorker),Request,nil)
end;

{==============================================================================}

procedure CDCACMInterruptWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request to the CDC ACM interrupt IN endpoint}
{Request: The USB request which has completed}
var
 State:Word;
 Status:LongWord;
 Message:TMessage;
 Serial:PCDCACMDevice;
 Notification:PUSBCDCNotification;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PCDCACMDevice(Request.DriverData);
 if Serial <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Serial.Serial.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Serial.InterruptComplete);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {$IFDEF CDCACM_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Close pending, setting interrupt request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF CDCACM_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Interrupt complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}

        {Check Size}
        if Request.ActualSize >= (SizeOf(TUSBCDCNotification)) then
         begin
          {Get Notification}
          Notification:=PUSBCDCNotification(Request.Data);

          {Check Notification}
          case Notification.bNotificationType of
           USB_CDC_NOTIFY_SERIAL_STATE:begin
             {$IFDEF CDCACM_DEBUG}
             if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Serial State Notification (wValue=' + IntToHex(Notification.wValue,4) + ')');
             {$ENDIF}

             {Get State}
             State:=Notification.wValue;

             {Set Status}
             Serial.Serial.SerialStatus:=SERIAL_STATUS_NONE;

             {Check State}
             if (State and USB_CDC_ACM_CTRL_CONTROL_MASK) <> 0 then
              begin
               if (State and USB_CDC_ACM_CTRL_DCD) <> 0 then
                begin
                 Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_DCD;
                end;
               if (State and USB_CDC_ACM_CTRL_DSR) <> 0 then
                begin
                 Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_DSR;
                end;
               if (State and USB_CDC_ACM_CTRL_RI) <> 0 then
                begin
                 Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_RI;
                end;
              end;

             {Check State}
             if (State and USB_CDC_ACM_CTRL_TRANSIENT_MASK) <> 0 then
              begin
               if (State and USB_CDC_ACM_CTRL_BRK) <> 0 then
                begin
                 Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_BREAK_ERROR;

                 if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'CDC ACM: Break error on receive character');
                end;
               if (State and USB_CDC_ACM_CTRL_PARITY) <> 0 then
                begin
                 Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_PARITY_ERROR;

                 if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'CDC ACM: Parity error on receive character');
                end;
               if (State and USB_CDC_ACM_CTRL_FRAMING) <> 0 then
                begin
                 Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_FRAMING_ERROR;

                 if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'CDC ACM: Framing error on receive character');
                end;
               if (State and USB_CDC_ACM_CTRL_OVERRUN) <> 0 then
                begin
                 Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_OVERRUN_ERROR;

                 if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'CDC ACM: Overrun error on receive character');
                end;
              end;
            end;
           else
            begin
             if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Other Notification (bNotificationType=' + IntToHex(Notification.bNotificationType,2) + ')');
            end;
          end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Interrupt request invalid size');

          {Update Statistics}
          Inc(Serial.InterruptErrors);
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed interrupt request (Status=' + USBStatusToString(Request.Status) + ')');

        {Update Statistics}
        Inc(Serial.InterruptErrors);
       end;

      {Update Pending}
      Dec(Serial.PendingCount);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {Check Pending}
        if Serial.PendingCount = 0 then
         begin
          {Check Waiter}
          if Serial.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF CDCACM_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Serial.WaiterThread,Message);
            Serial.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Update Pending}
        Inc(Serial.PendingCount);

        {$IFDEF CDCACM_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'CDC ACM: Resubmitting interrupt request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed to resubmit interrupt request: ' + USBStatusToString(Status));

          {Update Pending}
          Dec(Serial.PendingCount);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Serial.Serial.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'CDC ACM: Interrupt request invalid');
  end;
end;

{==============================================================================}

procedure CDCACMInterruptComplete(Request:PUSBRequest);
{Called when a USB request from the CDC ACM interrupt IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(CDCACMInterruptWorker),Request,nil)
end;

{==============================================================================}
{==============================================================================}
{CDC ACM Helper Functions}
function CDCACMCheckDevice(Device:PUSBDevice;var Quirks:LongWord):LongWord;
{Check the Vendor and Device ID against the supported devices}
{Device: USB device to check}
{Quirks: Return value for any device specific quirks (eg CDCACM_QUIRK_NO_UNION_NORMAL)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Setup Defaults}
 Quirks:=CDCACM_QUIRK_NONE;

 {Check Device IDs}
 for Count:=0 to CDCACM_DEVICE_ID_COUNT - 1 do
  begin
   if (CDCACM_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (CDCACM_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     {Return Quirks}
     Quirks:=CDCACM_DEVICE_ID[Count].Quirks;

     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function CDCACMCheckInterface(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Check the Interface against the supported devices}
{Device: USB device to check}
{Interrface: USB interface to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Interface IDs}
 for Count:=0 to CDCACM_INTERFACE_ID_COUNT - 1 do
  begin
   if (CDCACM_INTERFACE_ID[Count].bInterfaceClass = Interrface.Descriptor.bInterfaceClass) and (CDCACM_INTERFACE_ID[Count].bInterfaceSubClass = Interrface.Descriptor.bInterfaceSubClass) and (CDCACM_INTERFACE_ID[Count].bInterfaceProtocol = Interrface.Descriptor.bInterfaceProtocol) then
    begin
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function CDCACMCheckDeviceAndInterface(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Check the Device and Interface against the supported devices}
{Device: USB device to check}
{Interrface: USB interface to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Device and Interface IDs}
 for Count:=0 to CDCACM_DEVICE_INTERFACE_ID_COUNT - 1 do
  begin
   if (CDCACM_DEVICE_INTERFACE_ID[Count].idVendor = Device.Descriptor.idVendor) and (CDCACM_DEVICE_INTERFACE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     if (CDCACM_DEVICE_INTERFACE_ID[Count].bInterfaceClass = Interrface.Descriptor.bInterfaceClass) and (CDCACM_DEVICE_INTERFACE_ID[Count].bInterfaceSubClass = Interrface.Descriptor.bInterfaceSubClass) and (CDCACM_DEVICE_INTERFACE_ID[Count].bInterfaceProtocol = Interrface.Descriptor.bInterfaceProtocol) then
      begin
       Result:=USB_STATUS_SUCCESS;
       Exit;
      end;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function CDCACMFindInterfaces(Device:PUSBDevice;Interrface:PUSBInterface;var DataInterface,ControlInterface:PUSBInterface;Quirks:LongWord;var Capabilities:Byte):LongWord;
{Locate the data and control interfaces from the functional descriptors}
{Device: USB device to check}
{Interrface: USB interface to check (Optional)}
{DataInterface: Return value for the located data interface}
{ControlInterface: Return value for the located control interface}
{Quirks: Any known quirks for this device from identification (eg CDCACM_QUIRK_NO_UNION_NORMAL)}
{Capabilities: Return value for ACM capabilities (eg USB_CDC_ACM_CAP_LINE)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Len:Byte;
 Buffer:PByte;
 Size:LongWord;

 Count:LongWord;
 CallIndex:Integer;
 DataIndex:Integer;
 NextInterface:PUSBInterface;
 Descriptor:PUSBCDCDescriptor;
 UnionDescriptor:PUSBCDCUnionDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 {if Interrface = nil then Exit;} {May be nil}

 {$IFDEF CDCACM_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Locating control and data interfaces');
 {$ENDIF}

 {Setup Defaults}
 DataInterface:=nil;
 ControlInterface:=nil;
 Capabilities:=0;

 {Setup Descriptors}
 CallIndex:=-1;
 DataIndex:=-1;
 UnionDescriptor:=nil;

 {Check Interface}
 Count:=0;
 if Interrface = nil then
  begin
   {Check Configuration}
   if Device.Configuration = nil then Exit;

   {Get First Interface}
   NextInterface:=Device.Configuration.Interfaces[Count];
  end
 else
  begin
   {Use Provided Interface}
   NextInterface:=Interrface;
  end;

 while NextInterface <> nil do
  begin
   {Setup Buffer}
   Buffer:=nil;
   Size:=0;

   {Get start of class specific descriptors for this interface}
   if (NextInterface.ClassData <> nil) and (NextInterface.ClassSize > 0) then
    begin
     {Setup Buffer}
     Buffer:=NextInterface.ClassData;
     Size:=NextInterface.ClassSize;
    end;

   {Check Buffer}
   if (Buffer <> nil) and (Size > 0) then
    begin
     {Look for CDC descriptors}
     while Size > 0 do
      begin
       {Get Header}
       Descriptor:=PUSBCDCDescriptor(Buffer);

       {Get Length}
       Len:=Descriptor.bFunctionLength;
       if Len > 0 then
        begin
         {Check Type}
         if Descriptor.bDescriptorType = USB_DESCRIPTOR_TYPE_CLASS_INTERFACE then
          begin
           {Check Sub Type}
           case Descriptor.bDescriptorSubType of
            USB_CDC_HEADER_TYPE:begin
              {Header}
              {$IFDEF CDCACM_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Found header type descriptor');
              {$ENDIF}

              {Nothing}
             end;
            USB_CDC_CALL_MANAGEMENT_TYPE:begin
              {Call Management}
              {$IFDEF CDCACM_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Found call management type descriptor');
              {$ENDIF}

              if Len >=5 then
               begin
                CallIndex:=Buffer[4];

                {$IFDEF CDCACM_DEBUG}
                if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM:  CallIndex=' + IntToStr(CallIndex));
                {$ENDIF}
               end;
             end;
            USB_CDC_ACM_TYPE:begin
              {Abstract Control Model}
              {$IFDEF CDCACM_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Found abstract control model type descriptor');
              {$ENDIF}

              if Len >= SizeOf(TUSBCDCACMDescriptor) then
               begin
                Capabilities:=PUSBCDCACMDescriptor(Buffer).bmCapabilities;

                {$IFDEF CDCACM_DEBUG}
                if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM:  Capabilities=' + IntToHex(Capabilities,2));
                {$ENDIF}
               end;
             end;
            USB_CDC_UNION_TYPE:begin
              {Union}
              {$IFDEF CDCACM_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Found union type descriptor');
              {$ENDIF}

              if Len >= SizeOf(TUSBCDCUnionDescriptor) then
               begin
                if UnionDescriptor = nil then
                 begin
                  UnionDescriptor:=PUSBCDCUnionDescriptor(Buffer)
                 end;
               end;
             end;
            USB_CDC_COUNTRY_TYPE:begin
              {Country}
              {$IFDEF CDCACM_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Found country type descriptor');
              {$ENDIF}

              if Len >= SizeOf(TUSBCDCCountryDescriptor) then
               begin
                {Nothing}
               end;
             end;
           end;
          end;
        end
       else
        begin
         Len:=1;
        end;

       Dec(Size,Len);
       Inc(Buffer,Len);
      end;
    end;

   {Check Interface}
   if Interrface <> nil then Break;

   {Update Count}
   Inc(Count);
   if Count >= Length(Device.Configuration.Interfaces) then Break;

   {Get Next Interface}
   NextInterface:=Device.Configuration.Interfaces[Count];
  end;

 {Check Descriptor}
 if UnionDescriptor = nil then
  begin
   {$IFDEF CDCACM_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: No union descriptor found');
   {$ENDIF}

   {Check Interface}
   if Interrface = nil then
    begin
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Check Call Index}
   if CallIndex > 0 then
    begin
     {$IFDEF CDCACM_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Using call management interface');
     {$ENDIF}

     {Get Interfaces}
     DataIndex:=CallIndex;
     if (Quirks and CDCACM_QUIRK_NO_DATA_INTERFACE) <> 0 then
      begin
       DataInterface:=USBDeviceFindInterfaceByIndex(Device,0);
      end
     else
      begin
       DataInterface:=USBDeviceFindInterfaceByIndex(Device,CallIndex);
      end;
     ControlInterface:=Interrface;
    end
   else
    begin
     {Check Endpoints}
     if Interrface.Descriptor.bNumEndpoints <> 3 then
      begin
       Result:=USB_STATUS_DEVICE_UNSUPPORTED;
       Exit;
      end;

     {Get Interfaces}
     DataInterface:=Interrface;
     ControlInterface:=Interrface;
    end;
  end
 else
  begin
   {$IFDEF CDCACM_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'CDC ACM: Using union descriptor (bSlaveInterface0=' + IntToStr(UnionDescriptor.bSlaveInterface0) + ' bMasterInterface0=' + IntToStr(UnionDescriptor.bMasterInterface0) + ')');
   {$ENDIF}

   {Reset CallIndex}
   CallIndex:=-1;

   {Get Interfaces}
   DataInterface:=USBDeviceFindInterfaceByIndex(Device,UnionDescriptor.bSlaveInterface0);
   ControlInterface:=USBDeviceFindInterfaceByIndex(Device,UnionDescriptor.bMasterInterface0);
  end;

 {Check the Interfaces}
 if (DataInterface = nil) or (ControlInterface = nil) then
  begin
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check the Indexes}
 if (CallIndex <> -1) and (DataIndex <> CallIndex) then
  begin
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function CDCACMControlRequest(Serial:PCDCACMDevice;Request:Byte;Value:Word;Data:Pointer;Size:Word):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Send Control Request}
 Result:=USBControlRequestEx(Device,nil,Request,CDCACM_USB_REQUEST_TYPE,Value,Serial.ControlInterface.Descriptor.bInterfaceNumber,Data,Size,5000,False);
end;

{==============================================================================}

function CDCACMGetLineRequest(Serial:PCDCACMDevice;var LineCoding:TUSBCDCLineCoding):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Send Control Request}
 Result:=CDCACMControlRequest(Serial,USB_CDC_REQ_GET_LINE_CODING,0,@LineCoding,SizeOf(TUSBCDCLineCoding));
end;

{==============================================================================}

function CDCACMSetLineRequest(Serial:PCDCACMDevice;const LineCoding:TUSBCDCLineCoding):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Send Control Request}
 Result:=CDCACMControlRequest(Serial,USB_CDC_REQ_SET_LINE_CODING,0,@LineCoding,SizeOf(TUSBCDCLineCoding));
end;

{==============================================================================}

function CDCACMSetControlRequest(Serial:PCDCACMDevice;Control:Word):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Send Control Request}
 Result:=CDCACMControlRequest(Serial,USB_CDC_REQ_SET_CONTROL_LINE_STATE,Control,nil,0);
end;

{==============================================================================}
{==============================================================================}

initialization
 CDCACMInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

