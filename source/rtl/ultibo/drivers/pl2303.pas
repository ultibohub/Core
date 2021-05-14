{
Prolific PL2303 USB to Serial Driver.

Copyright (C) 2020 - SoftOz Pty Ltd.

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

  Linux - \drivers\usb\serial\pl2303.c - Copyright (C) 2001-2007 Greg Kroah-Hartman.
  Linux - \drivers\usb\serial\pl2303.h
  Linux - \drivers\usb\serial\usb-serial.c - Copyright (C) 2009 - 2013 Johan Hovold and others.
  Linux - \drivers\usb\serial\generic.c - Copyright (C) 2010 - 2013 Johan Hovold and others.
  
References
==========

  
Prolific PL2303
===============

 The Prolific PL2303 is a USB to serial interface chip used by multiple vendors.
 It includes a single serial port with bulk IN and OUT plus interrupt IN endpoints.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PL2303; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,Serial,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PL2303 specific constants}
 PL2303_DRIVER_NAME = 'Prolific PL2303 USB to Serial Driver'; {Name of PL2303 driver}
 
 PL2303_SERIAL_DESCRIPTION = 'Prolific PL2303 USB to Serial'; {Description of PL2303 device}
 
 PL2303_MIN_BAUD = 75;
 PL2303_MAX_BAUD = 1228800;
 PL2303_MAX_BAUD_HX = 12000000;
 
 PL2303_MIN_DATABITS = SERIAL_DATA_5BIT;
 PL2303_MAX_DATABITS = SERIAL_DATA_8BIT;
 
 PL2303_MIN_STOPBITS = SERIAL_STOP_1BIT;
 PL2303_MAX_STOPBITS = SERIAL_STOP_1BIT5;
 
 PL2303_MAX_PARITY = SERIAL_PARITY_SPACE;
 
 PL2303_MAX_FLOW = SERIAL_FLOW_DSR_DTR;
 
 {PL2303 device models}
 PL2303_TYPE_01 = 0; {Type 0 and 1 (difference unknown)}
 PL2303_TYPE_HX = 1; {HX version of the PL2303 chip}
 
 {PL2303 Bulk IN/OUT sizes}
 PL2303_BULK_IN_SIZE  = 256;
 PL2303_BULK_OUT_SIZE = 256;
 
 {PL2303 USB requests}
 PL2303_SET_LINE_REQUEST_TYPE     = $21;
 PL2303_SET_LINE_REQUEST          = $20;
                                  
 PL2303_SET_CONTROL_REQUEST_TYPE  = $21;
 PL2303_SET_CONTROL_REQUEST       = $22;
 PL2303_CONTROL_DTR               = $01;
 PL2303_CONTROL_RTS               = $02;
 
 PL2303_BREAK_REQUEST_TYPE        = $21;
 PL2303_BREAK_REQUEST             = $23;
 PL2303_BREAK_ON                  = $ffff;
 PL2303_BREAK_OFF                 = $0000;
 
 PL2303_GET_LINE_REQUEST_TYPE     = $a1;
 PL2303_GET_LINE_REQUEST          = $21;
 
 PL2303_VENDOR_WRITE_REQUEST_TYPE = $40;
 PL2303_VENDOR_WRITE_REQUEST      = $01;
 
 PL2303_VENDOR_READ_REQUEST_TYPE  = $c0;
 PL2303_VENDOR_READ_REQUEST       = $01;
 
 PL2303_UART_STATE_INDEX          = 8;
 PL2303_UART_DCD                  = $01;
 PL2303_UART_DSR                  = $02;
 PL2303_UART_BREAK_ERROR          = $04;
 PL2303_UART_RING                 = $08;
 PL2303_UART_FRAME_ERROR          = $10;
 PL2303_UART_PARITY_ERROR         = $20;
 PL2303_UART_OVERRUN_ERROR        = $40;
 PL2303_UART_CTS                  = $80;
 
 PL2303_UART_STATE_MSR_MASK       = PL2303_UART_DCD or PL2303_UART_DSR or PL2303_UART_RING or PL2303_UART_CTS; { $8b }
 PL2303_UART_STATE_TRANSIENT_MASK = PL2303_UART_BREAK_ERROR or PL2303_UART_FRAME_ERROR or PL2303_UART_PARITY_ERROR or PL2303_UART_OVERRUN_ERROR; { $74 }
 
 {PL2303 device quirks}
 PL2303_QUIRK_NONE            = $00000000;
 PL2303_QUIRK_UART_STATE_IDX0 = $00000001;
 PL2303_QUIRK_LEGACY          = $00000002;
 PL2303_QUIRK_ENDPOINT_HACK   = $00000004;
 
 {PL2303 supported baud rates}
 PL2303_SUPPORTED_BAUD_RATE_COUNT = 25; 
 PL2303_SUPPORTED_BAUD_RATES:array[0..PL2303_SUPPORTED_BAUD_RATE_COUNT - 1] of LongWord = (
  75,
  150,
  300,
  600,
  1200,
  1800,
  2400,
  3600,
  4800,
  7200,
  9600,
  14400,
  19200,
  28800,
  38400,
  57600,
  115200,
  230400,
  460800,
  614400,
  921600,
  1228800,
  2457600,
  3000000,
  6000000);
 
 {PL2303 Vendor and Product ID constants}
 PL2303_VENDOR_ID               = $067b;
 PL2303_PRODUCT_ID              = $2303;
 PL2303_PRODUCT_ID_TB           = $2304;
 PL2303_PRODUCT_ID_RSAQ2        = $04bb;
 PL2303_PRODUCT_ID_DCU11        = $1234;
 PL2303_PRODUCT_ID_PHAROS       = $aaa0;
 PL2303_PRODUCT_ID_RSAQ3        = $aaa2;
 PL2303_PRODUCT_ID_CHILITAG     = $aaa8;
 PL2303_PRODUCT_ID_ALDIGA       = $0611;
 PL2303_PRODUCT_ID_MMX          = $0612;
 PL2303_PRODUCT_ID_GPRS         = $0609;
 PL2303_PRODUCT_ID_HCR331       = $331a;
 PL2303_PRODUCT_ID_MOTOROLA     = $0307;
 PL2303_PRODUCT_ID_ZTEK         = $e1f1;
                                
 PL2303_ATEN_VENDOR_ID          = $0557;
 PL2303_ATEN_VENDOR_ID2         = $0547;
 PL2303_ATEN_PRODUCT_ID         = $2008;
 PL2303_ATEN_PRODUCT_UC485      = $2021;
 PL2303_ATEN_PRODUCT_UC232B     = $2022;
 PL2303_ATEN_PRODUCT_ID2        = $2118;
                                
 PL2303_BENQ_VENDOR_ID          = $04a5;
 PL2303_BENQ_PRODUCT_ID_S81     = $4027;

 PL2303_IODATA_VENDOR_ID        = $04bb;
 PL2303_IODATA_PRODUCT_ID       = $0a03;
 PL2303_IODATA_PRODUCT_ID_RSAQ5 = $0a0e;

 PL2303_ELCOM_VENDOR_ID         = $056e;
 PL2303_ELCOM_PRODUCT_ID        = $5003;
 PL2303_ELCOM_PRODUCT_ID_UCSGT  = $5004;

 PL2303_ITEGNO_VENDOR_ID        = $0eba;
 PL2303_ITEGNO_PRODUCT_ID       = $1080;
 PL2303_ITEGNO_PRODUCT_ID_2080  = $2080;

 PL2303_MA620_VENDOR_ID         = $0df7;
 PL2303_MA620_PRODUCT_ID        = $0620;
                                
 PL2303_RATOC_VENDOR_ID         = $0584;
 PL2303_RATOC_PRODUCT_ID        = $b000;
                                
 PL2303_TRIPP_VENDOR_ID         = $2478;
 PL2303_TRIPP_PRODUCT_ID        = $2008;

 PL2303_RADIOSHACK_VENDOR_ID    = $1453;
 PL2303_RADIOSHACK_PRODUCT_ID   = $4026;

 PL2303_DCU10_VENDOR_ID         = $0731;
 PL2303_DCU10_PRODUCT_ID        = $0528;

 PL2303_SITECOM_VENDOR_ID       = $6189;
 PL2303_SITECOM_PRODUCT_ID      = $2068;

 {Alcatel OT535/735 USB cable}
 PL2303_ALCATEL_VENDOR_ID       = $11f7;
 PL2303_ALCATEL_PRODUCT_ID      = $02df;

 PL2303_SIEMENS_VENDOR_ID       = $11f5;
 PL2303_SIEMENS_PRODUCT_ID_SX1  = $0001;
 PL2303_SIEMENS_PRODUCT_ID_X65  = $0003;
 PL2303_SIEMENS_PRODUCT_ID_X75  = $0004;
 PL2303_SIEMENS_PRODUCT_ID_EF81 = $0005;

 PL2303_SYNTECH_VENDOR_ID       = $0745;
 PL2303_SYNTECH_PRODUCT_ID      = $0001;

 {Nokia CA-42 Cable}
 PL2303_NOKIA_CA42_VENDOR_ID    = $078b;
 PL2303_NOKIA_CA42_PRODUCT_ID   = $1234;

 {CA-42 CLONE Cable www.ca-42.com chipset: Prolific Technology Inc}
 PL2303_CA_42_CA42_VENDOR_ID    = $10b5;
 PL2303_CA_42_CA42_PRODUCT_ID   = $ac70;

 PL2303_SAGEM_VENDOR_ID         = $079b;
 PL2303_SAGEM_PRODUCT_ID        = $0027;

 {Leadtek GPS 9531 (ID 0413:2101)}
 PL2303_LEADTEK_VENDOR_ID       = $0413;
 PL2303_LEADTEK_9531_PRODUCT_ID = $2101;

 {USB GSM cable from Speed Dragon Multimedia, Ltd}
 PL2303_SPEEDDRAGON_VENDOR_ID   = $0e55;
 PL2303_SPEEDDRAGON_PRODUCT_ID  = $110b;

 {DATAPILOT Universal-2 Phone Cable}
 PL2303_DATAPILOT_U2_VENDOR_ID  = $0731;
 PL2303_DATAPILOT_U2_PRODUCT_ID = $2003;

 {Belkin "F5U257" Serial Adapter}
 PL2303_BELKIN_VENDOR_ID        = $050d;
 PL2303_BELKIN_PRODUCT_ID       = $0257;

 {Alcor Micro Corp. USB 2.0 TO RS-232}
 PL2303_ALCOR_VENDOR_ID         = $058F;
 PL2303_ALCOR_PRODUCT_ID        = $9720;

 {Willcom WS002IN Data Driver (by NetIndex Inc.)}
 PL2303_WS002IN_VENDOR_ID       = $11f6;
 PL2303_WS002IN_PRODUCT_ID      = $2001;

 {Corega CG-USBRS232R Serial Adapter}
 PL2303_COREGA_VENDOR_ID        = $07aa;
 PL2303_COREGA_PRODUCT_ID       = $002a;

 {Y.C. Cable U.S.A., Inc - USB to RS-232}
 PL2303_YCCABLE_VENDOR_ID       = $05ad;
 PL2303_YCCABLE_PRODUCT_ID      = $0fba;

 {"Superial" USB - Serial}
 PL2303_SUPERIAL_VENDOR_ID      = $5372;
 PL2303_SUPERIAL_PRODUCT_ID     = $2303;

 {Hewlett-Packard POS Pole Displays}
 PL2303_HP_VENDOR_ID            = $03f0;
 PL2303_HP_LM920_PRODUCT_ID     = $026b;
 PL2303_HP_TD620_PRODUCT_ID     = $0956;
 PL2303_HP_LD960_PRODUCT_ID     = $0b39;
 PL2303_HP_LCM220_PRODUCT_ID    = $3139;
 PL2303_HP_LCM960_PRODUCT_ID    = $3239;
 PL2303_HP_LD220_PRODUCT_ID     = $3524;
 PL2303_HP_LD220TA_PRODUCT_ID   = $4349;
 PL2303_HP_LD960TA_PRODUCT_ID   = $4439;
 PL2303_HP_LM940_PRODUCT_ID     = $5039;

 {Cressi Edy (diving computer) PC interface}
 PL2303_CRESSI_VENDOR_ID        = $04b8;
 PL2303_CRESSI_EDY_PRODUCT_ID   = $0521;

 {Zeagle dive computer interface}
 PL2303_ZEAGLE_VENDOR_ID           = $04b8;
 PL2303_ZEAGLE_N2ITION3_PRODUCT_ID = $0522;

 {Sony, USB data cable for CMD-Jxx mobile phones}
 PL2303_SONY_VENDOR_ID          = $054c;
 PL2303_SONY_QN3USB_PRODUCT_ID  = $0437;

 {Sanwa KB-USB2 multimeter cable (ID: 11ad:0001)}
 PL2303_SANWA_VENDOR_ID         = $11ad;
 PL2303_SANWA_PRODUCT_ID        = $0001;

 {ADLINK ND-6530 RS232,RS485 and RS422 adapter}
 PL2303_ADLINK_VENDOR_ID         = $0b63;
 PL2303_ADLINK_ND6530_PRODUCT_ID = $6530;

 {SMART USB Serial Adapter}
 PL2303_SMART_VENDOR_ID         = $0b8c;
 PL2303_SMART_PRODUCT_ID        = $2303;
 
 {PL2303 Device ID constants}
 PL2303_DEVICE_ID_COUNT = 64; {Number of supported Device IDs}
 
 PL2303_DEVICE_ID:array[0..PL2303_DEVICE_ID_COUNT - 1] of TUSBDeviceId = (
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_RSAQ2),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_DCU11),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_RSAQ3),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_CHILITAG),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_PHAROS),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_ALDIGA),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_MMX),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_GPRS),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_HCR331),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_MOTOROLA),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_ZTEK),
  (idVendor:PL2303_VENDOR_ID;idProduct:PL2303_PRODUCT_ID_TB),
  (idVendor:PL2303_IODATA_VENDOR_ID;idProduct:PL2303_IODATA_PRODUCT_ID),
  (idVendor:PL2303_IODATA_VENDOR_ID;idProduct:PL2303_IODATA_PRODUCT_ID_RSAQ5),
  (idVendor:PL2303_ATEN_VENDOR_ID;idProduct:PL2303_ATEN_PRODUCT_ID),
  (idVendor:PL2303_ATEN_VENDOR_ID;idProduct:PL2303_ATEN_PRODUCT_UC485),
  (idVendor:PL2303_ATEN_VENDOR_ID;idProduct:PL2303_ATEN_PRODUCT_UC232B),
  (idVendor:PL2303_ATEN_VENDOR_ID;idProduct:PL2303_ATEN_PRODUCT_ID2),
  (idVendor:PL2303_ATEN_VENDOR_ID2;idProduct:PL2303_ATEN_PRODUCT_ID),
  (idVendor:PL2303_ELCOM_VENDOR_ID;idProduct:PL2303_ELCOM_PRODUCT_ID),
  (idVendor:PL2303_ELCOM_VENDOR_ID;idProduct:PL2303_ELCOM_PRODUCT_ID_UCSGT),
  (idVendor:PL2303_ITEGNO_VENDOR_ID;idProduct:PL2303_ITEGNO_PRODUCT_ID),
  (idVendor:PL2303_ITEGNO_VENDOR_ID;idProduct:PL2303_ITEGNO_PRODUCT_ID_2080),
  (idVendor:PL2303_MA620_VENDOR_ID;idProduct:PL2303_MA620_PRODUCT_ID),
  (idVendor:PL2303_RATOC_VENDOR_ID;idProduct:PL2303_RATOC_PRODUCT_ID),
  (idVendor:PL2303_TRIPP_VENDOR_ID;idProduct:PL2303_TRIPP_PRODUCT_ID),
  (idVendor:PL2303_RADIOSHACK_VENDOR_ID;idProduct:PL2303_RADIOSHACK_PRODUCT_ID),
  (idVendor:PL2303_DCU10_VENDOR_ID;idProduct:PL2303_DCU10_PRODUCT_ID),
  (idVendor:PL2303_SITECOM_VENDOR_ID;idProduct:PL2303_SITECOM_PRODUCT_ID),
  (idVendor:PL2303_ALCATEL_VENDOR_ID;idProduct:PL2303_ALCATEL_PRODUCT_ID),
  (idVendor:PL2303_SIEMENS_VENDOR_ID;idProduct:PL2303_SIEMENS_PRODUCT_ID_SX1),  {PL2303_QUIRK_UART_STATE_IDX0}
  (idVendor:PL2303_SIEMENS_VENDOR_ID;idProduct:PL2303_SIEMENS_PRODUCT_ID_X65),  {PL2303_QUIRK_UART_STATE_IDX0}
  (idVendor:PL2303_SIEMENS_VENDOR_ID;idProduct:PL2303_SIEMENS_PRODUCT_ID_X75),  {PL2303_QUIRK_UART_STATE_IDX0}
  (idVendor:PL2303_SIEMENS_VENDOR_ID;idProduct:PL2303_SIEMENS_PRODUCT_ID_EF81),
  (idVendor:PL2303_BENQ_VENDOR_ID;idProduct:PL2303_BENQ_PRODUCT_ID_S81),        {Benq/Siemens S81}
  (idVendor:PL2303_SYNTECH_VENDOR_ID;idProduct:PL2303_SYNTECH_PRODUCT_ID),
  (idVendor:PL2303_NOKIA_CA42_VENDOR_ID;idProduct:PL2303_NOKIA_CA42_PRODUCT_ID),
  (idVendor:PL2303_CA_42_CA42_VENDOR_ID;idProduct:PL2303_CA_42_CA42_PRODUCT_ID),
  (idVendor:PL2303_SAGEM_VENDOR_ID;idProduct:PL2303_SAGEM_PRODUCT_ID),
  (idVendor:PL2303_LEADTEK_VENDOR_ID;idProduct:PL2303_LEADTEK_9531_PRODUCT_ID),
  (idVendor:PL2303_SPEEDDRAGON_VENDOR_ID;idProduct:PL2303_SPEEDDRAGON_PRODUCT_ID),
  (idVendor:PL2303_DATAPILOT_U2_VENDOR_ID;idProduct:PL2303_DATAPILOT_U2_PRODUCT_ID),
  (idVendor:PL2303_BELKIN_VENDOR_ID;idProduct:PL2303_BELKIN_PRODUCT_ID),
  (idVendor:PL2303_ALCOR_VENDOR_ID;idProduct:PL2303_ALCOR_PRODUCT_ID),
  (idVendor:PL2303_WS002IN_VENDOR_ID;idProduct:PL2303_WS002IN_PRODUCT_ID),
  (idVendor:PL2303_COREGA_VENDOR_ID;idProduct:PL2303_COREGA_PRODUCT_ID),
  (idVendor:PL2303_YCCABLE_VENDOR_ID;idProduct:PL2303_YCCABLE_PRODUCT_ID),
  (idVendor:PL2303_SUPERIAL_VENDOR_ID;idProduct:PL2303_SUPERIAL_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LD220_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LD220TA_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LD960_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LD960TA_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LCM220_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LCM960_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LM920_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_LM940_PRODUCT_ID),
  (idVendor:PL2303_HP_VENDOR_ID;idProduct:PL2303_HP_TD620_PRODUCT_ID),
  (idVendor:PL2303_CRESSI_VENDOR_ID;idProduct:PL2303_CRESSI_EDY_PRODUCT_ID),
  (idVendor:PL2303_ZEAGLE_VENDOR_ID;idProduct:PL2303_ZEAGLE_N2ITION3_PRODUCT_ID),
  (idVendor:PL2303_SONY_VENDOR_ID;idProduct:PL2303_SONY_QN3USB_PRODUCT_ID),
  (idVendor:PL2303_SANWA_VENDOR_ID;idProduct:PL2303_SANWA_PRODUCT_ID),
  (idVendor:PL2303_ADLINK_VENDOR_ID;idProduct:PL2303_ADLINK_ND6530_PRODUCT_ID),
  (idVendor:PL2303_SMART_VENDOR_ID;idProduct:PL2303_SMART_PRODUCT_ID));
 
{==============================================================================}
type
 {PL2303 specific types}
 PPL2303SerialDevice = ^TPL2303SerialDevice;
 TPL2303SerialDevice = record
  {Serial Properties}
  Serial:TSerialDevice;
  {USB Properties}
  Model:LongWord;                           {Device model (eg PL2303_TYPE_01)}
  Quirks:LongWord;                          {Unusual behaviours of specific chip versions}
  Control:LongWord;                         {Status of the control lines RTS and DTR}
  ReceiveSize:LongWord;                     {Maximum Receive size for Bulk IN Endpoint}
  TransmitSize:LongWord;                    {Maximum Transmit size for Bulk OUT Endpoint}
  ReceiveActive:LongBool;                   {True if a Receive request is currently in progress}
  TransmitActive:LongBool;                  {True if a Transmit request is currently in progress}
  ReceiveRequest:PUSBRequest;               {USB request Bulk IN Endpoint}
  ReceiveEndpoint:PUSBEndpointDescriptor;   {PL2303 Bulk IN Endpoint}
  TransmitRequest:PUSBRequest;              {USB request for Bulk OUT Endpoint}
  TransmitEndpoint:PUSBEndpointDescriptor;  {PL2303 Bulk OUT Endpoint}
  InterruptRequest:PUSBRequest;             {USB request for Interrupt IN Endpoint}
  InterruptEndpoint:PUSBEndpointDescriptor; {PL2303 Interrupt IN Endpoint}
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
 {PL2303 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure PL2303Init;

{==============================================================================}
{PL2303 Serial Functions}
function PL2303SerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
function PL2303SerialDeviceClose(Serial:PSerialDevice):LongWord;

function PL2303SerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function PL2303SerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

{==============================================================================}
{PL2303 USB Functions}
function PL2303DriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function PL2303DriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure PL2303ReceiveStart(Request:PUSBRequest);  
procedure PL2303ReceiveWorker(Request:PUSBRequest); 
procedure PL2303ReceiveComplete(Request:PUSBRequest); 

procedure PL2303TransmitStart(Request:PUSBRequest);  
procedure PL2303TransmitWorker(Request:PUSBRequest); 
procedure PL2303TransmitComplete(Request:PUSBRequest); 

procedure PL2303InterruptWorker(Request:PUSBRequest);
procedure PL2303InterruptComplete(Request:PUSBRequest);

{==============================================================================}
{PL2303 Helper Functions}
function PL2303CheckDevice(Device:PUSBDevice):LongWord;
function PL2303PatchDevice(Device:PUSBDevice;var Model,Quirks:LongWord):LongWord;

function PL2303VendorRead(Device:PUSBDevice;Value:Word;Data:PByte):LongWord;
function PL2303VendorWrite(Device:PUSBDevice;Value,Index:Word):LongWord;

function PL2303GetLineRequest(Device:PUSBDevice;Data:PByte):LongWord;
function PL2303SetLineRequest(Device:PUSBDevice;Data:PByte):LongWord;

function PL2303SetControlRequest(Device:PUSBDevice;Value:Word):LongWord;

function PL2303EncodeBaudRate(Device:PUSBDevice;BaudRate:LongWord;Data:PByte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PL2303 specific variables}
 PL2303Initialized:Boolean; 
 
 PL2303Driver:PUSBDriver;  {PL2303 Driver interface (Set by PL2303Init)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PL2303Init;
var
 Status:LongWord;
 WorkInt:LongWord;
begin
 {}
 {Check Initialized}
 if PL2303Initialized then Exit;

 {Create PL2303 Driver}
 PL2303Driver:=USBDriverCreate;
 if PL2303Driver <> nil then
  begin
   {Update PL2303 Driver}
   {Driver}
   PL2303Driver.Driver.DriverName:=PL2303_DRIVER_NAME; 
   {USB}
   PL2303Driver.DriverBind:=PL2303DriverBind;
   PL2303Driver.DriverUnbind:=PL2303DriverUnbind;

   {Register PL2303 Driver}
   Status:=USBDriverRegister(PL2303Driver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'PL2303: Failed to register PL2303 driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'PL2303: Failed to create PL2303 driver');
  end;
 
 {Check Environment Variables}
 {PL2303_MAX_TRANSMIT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('PL2303_MAX_TRANSMIT'),0);
 if WorkInt <> 0 then PL2303_MAX_TRANSMIT:=WorkInt;
 
 PL2303Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{PL2303 Serial Functions}
function PL2303SerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
{Implementation of SerialDeviceOpen API for PL2303 Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceOpen instead}
var
 Status:LongWord;
 Device:PUSBDevice;
 Data:array[0..6] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303: Device Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + SerialDataBitsToString(DataBits) + ' StopBits=' + SerialStopBitsToString(StopBits) + ' Parity=' + SerialParityToString(Parity) + ' FlowControl=' + SerialFlowControlToString(FlowControl) + ')');
 {$ENDIF}
 
 {Get Device}
 Device:=PUSBDevice(Serial.Device.DeviceData);
 if Device = nil then Exit;
 
 {Check Baud Rate}
 if ((BaudRate < PL2303_MIN_BAUD) or (BaudRate > Serial.Properties.MaxRate)) and (BaudRate <> SERIAL_BAUD_RATE_DEFAULT) then Exit;
 
 {Check Data Bits}
 if (DataBits < PL2303_MIN_DATABITS) or (DataBits > PL2303_MAX_DATABITS) then Exit;
 
 {Check Stop Bits}
 if (StopBits < PL2303_MIN_STOPBITS) or (StopBits > PL2303_MAX_STOPBITS) then Exit;
 
 {Check Parity}
 if Parity > PL2303_MAX_PARITY then Exit;
 
 {Check Flow Control}
 if FlowControl > PL2303_MAX_FLOW then Exit;
 
 {Adjust Baud Rate}
 if BaudRate = SERIAL_BAUD_RATE_DEFAULT then
  begin
   BaudRate:=SERIAL_BAUD_RATE_STANDARD;
   if (BaudRate > Serial.Properties.MaxRate) then BaudRate:=SERIAL_BAUD_RATE_FALLBACK;
  end; 
 
 {Initialize Device}
 if (PPL2303SerialDevice(Serial).Quirks and PL2303_QUIRK_LEGACY) <> 0 then
  begin
   {Clear Bulk IN/OUT Endpoints}
   USBDeviceClearFeature(Device,PPL2303SerialDevice(Serial).ReceiveEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);  //To Do //USB_CONTROL_SET_TIMEOUT
   USBDeviceClearFeature(Device,PPL2303SerialDevice(Serial).TransmitEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);  //To Do //USB_CONTROL_SET_TIMEOUT
  end
 else
  begin
   {Reset Upstream Data Pipes}
   PL2303VendorWrite(Device,8,0);
   PL2303VendorWrite(Device,9,0);
  end;
  
 {Get Line Request}
 if PL2303GetLineRequest(Device,@Data) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Data Bits (Data[6])}
 case DataBits of
  SERIAL_DATA_8BIT:Data[6]:=8;
  SERIAL_DATA_7BIT:Data[6]:=7;
  SERIAL_DATA_6BIT:Data[6]:=6;
  SERIAL_DATA_5BIT:Data[6]:=5;
 end;
 
 {Stop Bits (Data[4])}
 { Data[4]=0 is 1 stop bits}
 { Data[4]=1 is 1.5 stop bits}
 { Data[4]=2 is 2 stop bits}
 case StopBits of
  SERIAL_STOP_1BIT:Data[4]:=0;
  SERIAL_STOP_1BIT5:Data[4]:=1;
  SERIAL_STOP_2BIT:Data[4]:=2;
 end;
 
 {Parity (Data[5])}
 { Data[5]=0 is no parity}
 { Data[5]=1 is odd parity}
 { Data[5]=2 is even parity}
 { Data[5]=3 is mark parity}
 { Data[5]=4 is space parity}
 case Parity of
  SERIAL_PARITY_NONE:Data[5]:=0;
  SERIAL_PARITY_ODD:Data[5]:=1;
  SERIAL_PARITY_EVEN:Data[5]:=2;
  SERIAL_PARITY_MARK:Data[5]:=3;
  SERIAL_PARITY_SPACE:Data[5]:=4;
 end;
 
 {Baud Rate (Data[0]:Data[3])}
 BaudRate:=PL2303EncodeBaudRate(Device,BaudRate,@Data[0]);
 
 {Set Line Request}
 if PL2303SetLineRequest(Device,@Data) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Flow Control}
 PPL2303SerialDevice(Serial).Control:=PL2303_CONTROL_DTR or PL2303_CONTROL_RTS;
 PL2303SetControlRequest(Device,PPL2303SerialDevice(Serial).Control);
 case FlowControl of
  SERIAL_FLOW_NONE:begin
    PL2303VendorWrite(Device,$0,$0);
   end;
  SERIAL_FLOW_RTS_CTS,SERIAL_FLOW_DSR_DTR:begin
    if (PPL2303SerialDevice(Serial).Quirks and PL2303_QUIRK_LEGACY) <> 0 then
     begin
      PL2303VendorWrite(Device,$0,$41);
     end
    else
     begin
      PL2303VendorWrite(Device,$0,$61);
     end;     
   end;  
 end;
 
 {Check Max Transmit}
 if PL2303_MAX_TRANSMIT = 0 then PL2303_MAX_TRANSMIT:=PL2303_BULK_OUT_SIZE;
 
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
 PPL2303SerialDevice(Serial).ReceiveRequest:=USBRequestAllocate(Device,PPL2303SerialDevice(Serial).ReceiveEndpoint,PL2303ReceiveComplete,PL2303_BULK_IN_SIZE,Serial); 
 if PPL2303SerialDevice(Serial).ReceiveRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Allocate Transmit Request}
 PPL2303SerialDevice(Serial).TransmitRequest:=USBRequestAllocate(Device,PPL2303SerialDevice(Serial).TransmitEndpoint,PL2303TransmitComplete,PL2303_BULK_OUT_SIZE,Serial); 
 if PPL2303SerialDevice(Serial).TransmitRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);
   
   {Release Receive Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).ReceiveRequest);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Allocate Interrupt Request}
 PPL2303SerialDevice(Serial).InterruptRequest:=USBRequestAllocate(Device,PPL2303SerialDevice(Serial).InterruptEndpoint,PL2303InterruptComplete,PPL2303SerialDevice(Serial).InterruptEndpoint.wMaxPacketSize,Serial); 
 if PPL2303SerialDevice(Serial).InterruptRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);
   
   {Release Receive Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).ReceiveRequest);

   {Release Transmit Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).TransmitRequest);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Update Pending}
 Inc(PPL2303SerialDevice(Serial).PendingCount);
       
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303: Submitting interrupt request');
 {$ENDIF}
 
 {Submit Interrupt Request}
 Status:=USBRequestSubmit(PPL2303SerialDevice(Serial).InterruptRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'PL2303: Failed to submit interrupt request: ' + USBStatusToString(Status));
        
   {Update Pending}
   Dec(PPL2303SerialDevice(Serial).PendingCount);
   
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);
   
   {Release Receive Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).ReceiveRequest);

   {Release Transmit Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).TransmitRequest);

   {Release Interrupt Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).InterruptRequest);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Update Pending}
 Inc(PPL2303SerialDevice(Serial).PendingCount);

 {Set Active}
 PPL2303SerialDevice(Serial).ReceiveActive:=True;
 
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303: Submitting receive request');
 {$ENDIF}
 
 {Submit Receive Request}
 Status:=USBRequestSubmit(PPL2303SerialDevice(Serial).ReceiveRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'PL2303: Failed to submit receive request: ' + USBStatusToString(Status));
        
   {Reset Active}
   PPL2303SerialDevice(Serial).ReceiveActive:=False;
        
   {Update Pending}
   Dec(PPL2303SerialDevice(Serial).PendingCount);
   
   {Cancel Interrupt Request}
   USBRequestCancel(PPL2303SerialDevice(Serial).InterruptRequest);
   
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);
   
   {Release Receive Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).ReceiveRequest);

   {Release Transmit Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).TransmitRequest);

   {Release Interrupt Request}
   USBRequestRelease(PPL2303SerialDevice(Serial).InterruptRequest);
   
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

function PL2303SerialDeviceClose(Serial:PSerialDevice):LongWord;
{Implementation of SerialDeviceClose API for PL2303 Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceClose instead}
var 
 Message:TMessage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303: Device Close');
 {$ENDIF}
 
 {Cancel Receive Request}
 USBRequestCancel(PPL2303SerialDevice(Serial).ReceiveRequest);

 {Cancel Interrupt Request}
 USBRequestCancel(PPL2303SerialDevice(Serial).InterruptRequest);
 
 {Check Pending}
 if PPL2303SerialDevice(Serial).PendingCount <> 0 then
  begin
   {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
   if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303: Waiting for ' + IntToStr(PPL2303SerialDevice(Serial).PendingCount) + ' pending requests to complete');
   {$ENDIF}

   {Wait for Pending}
 
   {Setup Waiter}
   PPL2303SerialDevice(Serial).WaiterThread:=GetCurrentThreadId; 
 
   {Release the Lock}
   MutexUnlock(Serial.Lock);
 
   {Wait for Message}
   ThreadReceiveMessage(Message); 
   
   {Acquire the Lock}
   if MutexLock(Serial.Lock) <> ERROR_SUCCESS then Exit;
  end;
 
 {Release Interrupt Request}
 USBRequestRelease(PPL2303SerialDevice(Serial).InterruptRequest);
 
 {Release Transmit Request}
 USBRequestRelease(PPL2303SerialDevice(Serial).TransmitRequest);
 
 {Release Receive Request}
 USBRequestRelease(PPL2303SerialDevice(Serial).ReceiveRequest);
 
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

function PL2303SerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceRead API for PL2303 Serial}
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
 
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303: Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if not(PPL2303SerialDevice(Serial).ReceiveActive) and ((Serial.Receive.Size - Serial.Receive.Count) >= PPL2303SerialDevice(Serial).ReceiveSize) then
    begin
     {Start Receive}
     PL2303ReceiveStart(PPL2303SerialDevice(Serial).ReceiveRequest);
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
       if (Size = 0) and not(PPL2303SerialDevice(Serial).ReceiveActive) and ((Serial.Receive.Size - Serial.Receive.Count) >= PPL2303SerialDevice(Serial).ReceiveSize) then
        begin
         {Start Receive}
         PL2303ReceiveStart(PPL2303SerialDevice(Serial).ReceiveRequest);
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
 
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function PL2303SerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceWrite API for Pl2303 Serial}
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
 
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303: Device Write (Size=' + IntToStr(Size) + ')');
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
       if Empty and not(PPL2303SerialDevice(Serial).TransmitActive) then
        begin
         {Start Transmit}
         PL2303TransmitStart(PPL2303SerialDevice(Serial).TransmitRequest);
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
  
 {$IF DEFINED(PL2303_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'PL2303:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{PL2303 USB Functions}
function PL2303DriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the PL2303 driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Data:Byte;
 Model:LongWord;
 Quirks:LongWord;
 Status:LongWord;
 Serial:PPL2303SerialDevice;
 SerialInterface:PUSBInterface;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
 InterruptEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF PL2303_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}
 
 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {$IFDEF PL2303_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Interface bind not supported by driver');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check PL2303 Device}
 if PL2303CheckDevice(Device) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF PL2303_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Device not found in supported device list');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Interface}
 SerialInterface:=USBDeviceFindInterfaceByIndex(Device,0);
 if SerialInterface = nil then
  begin
   {$IFDEF PL2303_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Device has no available interface');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF PL2303_DEBUG}
  if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Interface.bInterfaceNumber=' + IntToStr(SerialInterface.Descriptor.bInterfaceNumber));
 {$ENDIF}

 {Check Bulk IN Endpoint}
 ReceiveEndpoint:=USBDeviceFindEndpointByType(Device,SerialInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
 if ReceiveEndpoint = nil then
  begin
   {$IFDEF PL2303_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Device has no BULK IN endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF PL2303_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: BULK IN Endpoint Count=' + IntToStr(USBDeviceCountEndpointsByType(Device,SerialInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK)));
 {$ENDIF}

 {Check Bulk OUT Endpoint}
 TransmitEndpoint:=USBDeviceFindEndpointByType(Device,SerialInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
 if TransmitEndpoint = nil then
  begin
   {$IFDEF PL2303_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Device has no BULK OUT endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF PL2303_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: BULK OUT Endpoint Count=' + IntToStr(USBDeviceCountEndpointsByType(Device,SerialInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK)));
 {$ENDIF}

 {Check Interrupt IN Endpoint}
 InterruptEndpoint:=USBDeviceFindEndpointByType(Device,SerialInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 if InterruptEndpoint = nil then
  begin
   {$IFDEF PL2303_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Device has no INTERRUPT IN endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF PL2303_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: INTERRUPT IN Endpoint Count=' + IntToStr(USBDeviceCountEndpointsByType(Device,SerialInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT)));
 {$ENDIF}

 {Check Configuration}
 if Device.ConfigurationValue = 0 then
  begin
   {$IFDEF PL2303_DEBUG}
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
 
 {USB device reset not required because the USB core already did a reset on the port during attach}

 {Patch PL2303 Device}
 if PL2303PatchDevice(Device,Model,Quirks) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF PL2303_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Unable to determine device information');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Initialize Device}
 PL2303VendorRead(Device,$8484,@Data);
 PL2303VendorWrite(Device,$0404,0);
 PL2303VendorRead(Device,$8484,@Data);
 PL2303VendorRead(Device,$8383,@Data);
 PL2303VendorRead(Device,$8484,@Data);
 PL2303VendorWrite(Device,$0404,1);
 PL2303VendorRead(Device,$8484,@Data);
 PL2303VendorRead(Device,$8383,@Data);
 PL2303VendorWrite(Device,0,1);
 PL2303VendorWrite(Device,1,0);
 if (Quirks and PL2303_QUIRK_LEGACY) <> 0 then
  begin
   PL2303VendorWrite(Device,2,$24);
  end 
 else
  begin
   PL2303VendorWrite(Device,2,$44);
  end; 
 
 {Create Serial}
 Serial:=PPL2303SerialDevice(SerialDeviceCreateEx(SizeOf(TPL2303SerialDevice)));
 if Serial = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'PL2303: Failed to create new serial device');
   
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
 Serial.Serial.Device.DeviceDescription:=PL2303_SERIAL_DESCRIPTION;
 {Serial}
 Serial.Serial.SerialState:=SERIAL_STATE_CLOSED;
 Serial.Serial.SerialStatus:=SERIAL_STATUS_NONE;
 Serial.Serial.DeviceOpen:=PL2303SerialDeviceOpen;
 Serial.Serial.DeviceClose:=PL2303SerialDeviceClose;
 Serial.Serial.DeviceRead:=PL2303SerialDeviceRead;
 Serial.Serial.DeviceWrite:=PL2303SerialDeviceWrite;
 {Driver}
 Serial.Serial.Properties.Flags:=Serial.Serial.Device.DeviceFlags;
 Serial.Serial.Properties.MinRate:=PL2303_MIN_BAUD;
 Serial.Serial.Properties.MaxRate:=PL2303_MAX_BAUD;
 if Model = PL2303_TYPE_HX then Serial.Serial.Properties.MaxRate:=PL2303_MAX_BAUD_HX; 
 Serial.Serial.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 Serial.Serial.Properties.DataBits:=SERIAL_DATA_8BIT;
 Serial.Serial.Properties.StopBits:=SERIAL_STOP_1BIT;
 Serial.Serial.Properties.Parity:=SERIAL_PARITY_NONE;
 Serial.Serial.Properties.FlowControl:=SERIAL_FLOW_NONE;
 Serial.Serial.Properties.ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
 Serial.Serial.Properties.TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;
 {USB}
 Serial.Model:=Model;
 Serial.Quirks:=Quirks;
 Serial.ReceiveSize:=ReceiveEndpoint.wMaxPacketSize;
 Serial.TransmitSize:=TransmitEndpoint.wMaxPacketSize;
 Serial.ReceiveEndpoint:=ReceiveEndpoint;
 Serial.TransmitEndpoint:=TransmitEndpoint;
 Serial.InterruptEndpoint:=InterruptEndpoint;
 Serial.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Register Serial} 
 if SerialDeviceRegister(@Serial.Serial) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'PL2303: Failed to register new serial device');
   
   {Destroy Serial}
   SerialDeviceDestroy(@Serial.Serial);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update Device}
 Device.DriverData:=Serial;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
  
{==============================================================================}

function PL2303DriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the PL2303 driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Serial:PPL2303SerialDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then Exit;
 
 {Check Driver}
 if Device.Driver <> PL2303Driver then Exit;
 
 {$IFDEF PL2303_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'PL2303: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Get Serial}
 Serial:=PPL2303SerialDevice(Device.DriverData);
 if Serial = nil then Exit;
 
 {Close Serial}
 SerialDeviceClose(@Serial.Serial); 
 
 {Update Device}
 Device.DriverData:=nil;

 {Deregister Serial}
 if SerialDeviceDeregister(@Serial.Serial) <> ERROR_SUCCESS then Exit;
 
 {Destroy Serial}
 SerialDeviceDestroy(@Serial.Serial);
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure PL2303ReceiveStart(Request:PUSBRequest);  
{Called to continue reception of data to the receive buffer}
{Request: The USB receive request to use}

{Note: Caller must hold the lock on the serial device}
var
 Count:LongWord;
 Available:LongWord;
 Status:LongWord;
 Serial:PPL2303SerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Serial}
 Serial:=PPL2303SerialDevice(Request.DriverData);
 if Serial = nil then Exit;
 
 {Setup Count}
 Count:=0;
 Available:=Serial.Serial.Receive.Size - Serial.Serial.Receive.Count;
 if Available >= PL2303_BULK_IN_SIZE then
  begin
   Count:=PL2303_BULK_IN_SIZE;
  end
 else if Available >= Serial.ReceiveSize then 
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

   {$IFDEF PL2303_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Resubmitting receive request');
   {$ENDIF}

   {Resubmit Request}
   Status:=USBRequestSubmit(Request);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed to resubmit receive request: ' + USBStatusToString(Status));

     {Reset Active}
     Serial.ReceiveActive:=False;
     
     {Update Pending}
     Dec(Serial.PendingCount);
    end;
  end;
end;

{==============================================================================}

procedure PL2303ReceiveWorker(Request:PUSBRequest); 
{Called (by a Worker thread) to process a completed USB request from the PL2303 bulk IN endpoint}
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
 Serial:PPL2303SerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Serial}
 Serial:=PPL2303SerialDevice(Request.DriverData);
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
        {$IFDEF PL2303_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
      
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF PL2303_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Receive complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
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
            if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Buffer overrun, ' + IntToStr(Size) + ' bytes discarded');
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
        if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed receive request (Status=' + USBStatusToString(Request.Status) + ')');
   
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
            {$IFDEF PL2303_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
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
        PL2303ReceiveStart(Request);
       end;  
     finally
      {Release the Lock}
      MutexUnlock(Serial.Serial.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Receive request invalid');
  end;    
end;

{==============================================================================}

procedure PL2303ReceiveComplete(Request:PUSBRequest); 
{Called when a USB request from the PL2303 bulk IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerSchedule(0,TWorkerTask(PL2303ReceiveWorker),Request,nil)
end;

{==============================================================================}

procedure PL2303TransmitStart(Request:PUSBRequest);  
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
 Serial:PPL2303SerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Serial}
 Serial:=PPL2303SerialDevice(Request.DriverData);
 if Serial = nil then Exit;
 
 {Setup Count, Size and Offset}
 Size:=Min(PL2303_MAX_TRANSMIT,PL2303_BULK_OUT_SIZE);
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
 
   {$IFDEF PL2303_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Submitting transmit request');
   {$ENDIF}
   
   {Submit Request}
   Status:=USBRequestSubmit(Request);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed to submit transmit request: ' + USBStatusToString(Status));
   
     {Reset Active}
     Serial.TransmitActive:=False;
   
     {Update Pending}
     Dec(Serial.PendingCount);
    end;
  end; 
end;

{==============================================================================}

procedure PL2303TransmitWorker(Request:PUSBRequest); 
{Called (by a Worker thread) to process a completed USB request to the PL2303 bulk OUT endpoint}
{Request: The USB request which has completed}
var
 Message:TMessage;
 Serial:PPL2303SerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Serial}
 Serial:=PPL2303SerialDevice(Request.DriverData);
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
        {$IFDEF PL2303_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Close pending, setting transmit request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
      
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF PL2303_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Transmit complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed transmit request (Status=' + USBStatusToString(Request.Status) + ')');
        
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
            {$IFDEF PL2303_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
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
        PL2303TransmitStart(Request);
       end;       
     finally
      {Release the Lock}
      MutexUnlock(Serial.Serial.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Transmit request invalid');
  end;    
end;

{==============================================================================}

procedure PL2303TransmitComplete(Request:PUSBRequest); 
{Called when a USB request to the PL2303 bulk OUT endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerSchedule(0,TWorkerTask(PL2303TransmitWorker),Request,nil)
end;

{==============================================================================}

procedure PL2303InterruptWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request to the PL2303 interrupt IN endpoint}
{Request: The USB request which has completed}
var
 State:Byte;
 Index:LongWord;
 Status:LongWord;
 Message:TMessage;
 Serial:PPL2303SerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Serial}
 Serial:=PPL2303SerialDevice(Request.DriverData);
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
        {$IFDEF PL2303_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Close pending, setting interrupt request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
 
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF PL2303_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Interrupt complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}
        
        {Get Index}
        Index:=PL2303_UART_STATE_INDEX;
        if (Serial.Quirks and PL2303_QUIRK_UART_STATE_IDX0) <> 0 then
         begin
          Index:=0;
         end;

        {Check Size}
        if Request.ActualSize >= (Index + 1) then
         begin
          {Get State}
          State:=PByte(Request.Data + Index)^;
          
          {$IFDEF PL2303_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: State=' + IntToHex(State,2));
          {$ENDIF}
          
          {Set Status}
          Serial.Serial.SerialStatus:=SERIAL_STATUS_NONE;
          
          {Check State}
          if (State and PL2303_UART_STATE_MSR_MASK) <> 0 then
           begin
            if (State and PL2303_UART_CTS) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_CTS;
             end;
            if (State and PL2303_UART_DCD) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_DCD;
             end;
            if (State and PL2303_UART_DSR) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_DSR;
             end;
            if (State and PL2303_UART_RING) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_RI;
             end;
           end;
           
          {Check State}
          if (State and PL2303_UART_STATE_TRANSIENT_MASK) <> 0 then
           begin
            if (State and PL2303_UART_BREAK_ERROR) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_BREAK_ERROR;
              
              if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'PL2303: Break error on receive character'); 
             end;
            if (State and PL2303_UART_PARITY_ERROR) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_PARITY_ERROR;
              
              if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'PL2303: Parity error on receive character'); 
             end;
            if (State and PL2303_UART_FRAME_ERROR) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_FRAMING_ERROR;
              
              if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'PL2303: Framing error on receive character'); 
             end;
            if (State and PL2303_UART_OVERRUN_ERROR) <> 0 then
             begin
              Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_OVERRUN_ERROR;
              
              if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'PL2303: Overrun error on receive character'); 
             end;
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Interrupt request invalid size');
          
          {Update Statistics}
          Inc(Serial.InterruptErrors); 
         end;
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed interrupt request (Status=' + USBStatusToString(Request.Status) + ')');
   
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
            {$IFDEF PL2303_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
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
 
        {$IFDEF PL2303_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'PL2303: Resubmitting interrupt request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed to resubmit interrupt request: ' + USBStatusToString(Status));
   
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
     if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'PL2303: Interrupt request invalid');
  end;    
end;

{==============================================================================}

procedure PL2303InterruptComplete(Request:PUSBRequest);
{Called when a USB request from the PL2303 interrupt IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerSchedule(0,TWorkerTask(PL2303InterruptWorker),Request,nil)
end;

{==============================================================================}
{==============================================================================}
{PL2303 Helper Functions}
function PL2303CheckDevice(Device:PUSBDevice):LongWord;
{Check the Vendor and Device ID against the supported devices}
{Device: USB device to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Device IDs}
 for Count:=0 to PL2303_DEVICE_ID_COUNT - 1 do
  begin
   if (PL2303_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (PL2303_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function PL2303PatchDevice(Device:PUSBDevice;var Model,Quirks:LongWord):LongWord;
{Check the USB device for quirks and model information needed by the driver}
{Device: USB device to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Setup Defaults}
 Model:=PL2303_TYPE_01;
 Quirks:=PL2303_QUIRK_NONE;
 
 {Check Descriptor}
 if Device.Descriptor.bDeviceClass = $02 then
  begin
   Model:=PL2303_TYPE_01; {Type 0}
  end
 else if Device.Descriptor.bMaxPacketSize0 = $40 then
  begin
   Model:=PL2303_TYPE_HX; {Type HX}
  end
 else if Device.Descriptor.bDeviceClass = $00 then
  begin
   Model:=PL2303_TYPE_01; {Type 1}
  end
 else if Device.Descriptor.bDeviceClass = $FF then
  begin
   Model:=PL2303_TYPE_01; {Type 1}
  end;
  
 {Check Model}
 case Model of
  PL2303_TYPE_01:begin
    Quirks:=Quirks or PL2303_QUIRK_LEGACY;
   end;
 end;
 
 {Check Device ID}
 if (Device.Descriptor.idVendor = PL2303_SIEMENS_VENDOR_ID) and (Device.Descriptor.idProduct = PL2303_SIEMENS_PRODUCT_ID_SX1) then
  begin
   Quirks:=Quirks or PL2303_QUIRK_UART_STATE_IDX0;
  end
 else if (Device.Descriptor.idVendor = PL2303_SIEMENS_VENDOR_ID) and (Device.Descriptor.idProduct = PL2303_SIEMENS_PRODUCT_ID_X65) then
  begin
   Quirks:=Quirks or PL2303_QUIRK_UART_STATE_IDX0;
  end 
 else if (Device.Descriptor.idVendor = PL2303_SIEMENS_VENDOR_ID) and (Device.Descriptor.idProduct = PL2303_SIEMENS_PRODUCT_ID_X75) then
  begin
   Quirks:=Quirks or PL2303_QUIRK_UART_STATE_IDX0;
  end;
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function PL2303VendorRead(Device:PUSBDevice;Value:Word;Data:PByte):LongWord;
{Perform a vendor read request to a PL2303 device}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Send Vendor Read Request}
 Result:=USBControlRequestEx(Device,nil,PL2303_VENDOR_READ_REQUEST,PL2303_VENDOR_READ_REQUEST_TYPE,Value,0,Data,1,100,False);
end;

{==============================================================================}

function PL2303VendorWrite(Device:PUSBDevice;Value,Index:Word):LongWord;
{Perform a vendor write request to a PL2303 device}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Send Vendor Write Request}
 Result:=USBControlRequestEx(Device,nil,PL2303_VENDOR_WRITE_REQUEST,PL2303_VENDOR_WRITE_REQUEST_TYPE,Value,Index,nil,0,100,False);
end;

{==============================================================================}

function PL2303GetLineRequest(Device:PUSBDevice;Data:PByte):LongWord;
{Perform a get line request to a PL2303 device}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Send Get Line Request}
 Result:=USBControlRequestEx(Device,nil,PL2303_GET_LINE_REQUEST,PL2303_GET_LINE_REQUEST_TYPE,0,0,Data,7,100,False);
end;

{==============================================================================}

function PL2303SetLineRequest(Device:PUSBDevice;Data:PByte):LongWord;
{Perform a set line request to a PL2303 device}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Send Set Line Request}
 Result:=USBControlRequestEx(Device,nil,PL2303_SET_LINE_REQUEST,PL2303_SET_LINE_REQUEST_TYPE,0,0,Data,7,100,False);
end;

{==============================================================================}

function PL2303SetControlRequest(Device:PUSBDevice;Value:Word):LongWord;
{Perform a set control request to a PL2303 device}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Send Set Control Request}
 Result:=USBControlRequestEx(Device,nil,PL2303_SET_CONTROL_REQUEST,PL2303_SET_CONTROL_REQUEST_TYPE,Value,0,nil,0,100,False);
end;

{==============================================================================}

function PL2303EncodeBaudRate(Device:PUSBDevice;BaudRate:LongWord;Data:PByte):LongWord;
{Encode a baud rate value into the Line Request buffer for a PL2303 device}

 function PL2303GetSupportedRate(BaudRate:LongWord):LongWord;
 var
  Count:LongWord;
 begin
  {}
  Count:=0;
  while Count < PL2303_SUPPORTED_BAUD_RATE_COUNT do
   begin
    if PL2303_SUPPORTED_BAUD_RATES[Count] > BaudRate then Break;
    
    Inc(Count);
   end;
  
  if Count = PL2303_SUPPORTED_BAUD_RATE_COUNT then
   begin
    BaudRate:=PL2303_SUPPORTED_BAUD_RATES[Count - 1];
   end
  else if (Count > 0) and ((PL2303_SUPPORTED_BAUD_RATES[Count] - BaudRate) > (BaudRate - PL2303_SUPPORTED_BAUD_RATES[Count - 1])) then
   begin
    BaudRate:=PL2303_SUPPORTED_BAUD_RATES[Count - 1];
   end
  else
   begin
    BaudRate:=PL2303_SUPPORTED_BAUD_RATES[Count];
   end;
   
  Result:=BaudRate; 
 end;
 
var
 Baseline:LongWord;
 Mantissa:LongWord;
 Exponent:LongWord;
 SupportedRate:LongWord;
begin
 {}
 Result:=BaudRate;

 {Check Device}
 if Device = nil then Exit;

 {Check for Supported Rate}
 SupportedRate:=PL2303GetSupportedRate(BaudRate);
 if BaudRate = SupportedRate then
  begin
   {Use the supported rate directly}
   PLongWord(Data)^:=BaudRate;
  end
 else
  begin
   {Use a divisor}
   { Apparently the formula is:                     }
   {   baudrate = 12M * 32 / (mantissa * 4^exponent)}
   { where                                          }
   {   mantissa = buf[8:0]                          }
   {   exponent = buf[11:9]                         }
   Baseline:=12000000 * 32;
   Mantissa:=Baseline div BaudRate;
   if Mantissa = 0 then Mantissa:=1; {Avoid dividing by zero if baud > 32*12M}
   Exponent:=0;
   
   while Mantissa >= 512 do
    begin
     if Exponent < 7 then
      begin
       Mantissa:=Mantissa shr 2; {Divide by 4}
       Inc(Exponent);
      end 
     else
      begin
       {Exponent is maxed. Trim Mantissa and leave}
       Mantissa:=511;
       Break;
      end;
    end;
   
   Data[3]:=$80;
   Data[2]:=0;
   Data[1]:=(Exponent shl 1) or (Mantissa shr 8);
   Data[0]:=Mantissa and $ff;

   {Calculate and return the exact baud rate}
   Result:=(Baseline div Mantissa) shr (Exponent shl 1);
  end;  
end;

{==============================================================================}
{==============================================================================}

initialization
 PL2303Init;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
 