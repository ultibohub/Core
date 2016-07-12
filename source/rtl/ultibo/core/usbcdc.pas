{
Ultibo USB CDC interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

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

  Linux - /include/uapi/linux/usb/cdc.h
  
References
==========

  USB CDC - https://en.wikipedia.org/wiki/USB_communications_device_class
            http://www.usb.org/developers/docs/devclass_docs/CDC1.2_WMC1.1_012011.zip
            
USB CDC
=======

 The USB Communications Device Class (CDC) standard defines the protocol for communicating
 with a range of network adapters, modems and ISDN devices. This unit provides common
 defintions and structures used in conjunction with CDC devices.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit USBCDC; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,USB,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {CDC specific constants}
 {Class specific descriptor types (See: USB Class Definitions for Communications Devices 1.2)}
 USB_CDC_HEADER_TYPE             = $00;    {Header Functional Descriptor}
 USB_CDC_CALL_MANAGEMENT_TYPE    = $01;    {Call Management Functional Descriptor}
 USB_CDC_ACM_TYPE                = $02;    {Abstract Control Management Functional Descriptor}
 USB_CDC_DLM_TYPE                = $03;    {Direct Line Management Functional Descriptor}
 USB_CDC_RINGER_TYPE             = $04;    {Telephone Ringer Functional Descriptor}
 USB_CDC_CALL_LINE_TYPE          = $05;    {Telephone Call and Line State Reporting Capabilities Functional Descriptor}
 USB_CDC_UNION_TYPE              = $06;    {Union Functional Descriptor}
 USB_CDC_COUNTRY_TYPE            = $07;    {Country Selection Functional Descriptor}
 USB_CDC_TOM_TYPE                = $08;    {Telephone Operational Modes Functional Descriptor}
 USB_CDC_USB_TERMINAL_TYPE       = $09;    {USB Terminal Functional Descriptor}
 USB_CDC_NETWORK_TERMINAL_TYPE   = $0a;    {Network Channel Terminal Descriptor}
 USB_CDC_PROTOCOL_TYPE           = $0b;    {Protocol Unit Functional Descriptor}
 USB_CDC_EXTENSION_TYPE          = $0c;    {Extension Unit Functional Descriptor}
 USB_CDC_MCCM_TYPE               = $0d;    {Multi-Channel Management Functional Descriptor}
 USB_CDC_CCM_TYPE                = $0e;    {CAPI Control Management Functional Descriptor}
 USB_CDC_ETHERNET_TYPE           = $0f;    {Ethernet Networking Functional Descriptor}
 USB_CDC_ATM_TYPE                = $10;    {ATM Networking Functional Descriptor}
 USB_CDC_WHCM_TYPE               = $11;    {Wireless Handset Control Model Functional Descriptor}
 USB_CDC_MDLM_TYPE               = $12;    {Mobile Direct Line Model Functional Descriptor}
 USB_CDC_MDLM_DETAIL_TYPE        = $13;    {MDLM Detail Functional Descriptor}
 USB_CDC_DMM_TYPE                = $14;    {Device Management Model Functional Descriptor}
 USB_CDC_OBEX_TYPE               = $15;    {OBEX Functional Descriptor}
 USB_CDC_TCM_TYPE                = $18;    {Telephone Control Model Functional Descriptor}
 USB_CDC_NCM_TYPE                = $1a;    {NCM Functional Descriptor}
 USB_CDC_MBIM_TYPE               = $1b;
 USB_CDC_MBIM_EXTENDED_TYPE      = $1c;
 
 {Call Management Capabilities (See: Section 5.2.3.2 of USB Class Definitions for Communications Devices 1.2)}
 USB_CDC_CALL_MGMT_CAP_CALL_MGMT = $01;
 USB_CDC_CALL_MGMT_CAP_DATA_INTF = $02;
 
 {Abstract Control Model Capabilities (See: Section 5.2.3.3 of USB Class Definitions for Communications Devices 1.2)}
 USB_CDC_ACM_COMM_FEATURE = $01;
 USB_CDC_ACM_CAP_LINE     = $02;
 USB_CDC_ACM_CAP_BRK      = $04;
 USB_CDC_ACM_CAP_NOTIFY   = $08; 
 
 {Class specific Control Requests (See: Section 6.2 of USB Class Definitions for Communications Devices 1.2)}
 USB_CDC_SEND_ENCAPSULATED_COMMAND      = $00;
 USB_CDC_GET_ENCAPSULATED_RESPONSE      = $01;
 USB_CDC_REQ_SET_LINE_CODING            = $20;
 USB_CDC_REQ_GET_LINE_CODING            = $21;
 USB_CDC_REQ_SET_CONTROL_LINE_STATE     = $22;
 USB_CDC_REQ_SEND_BREAK                 = $23;
 USB_CDC_SET_ETHERNET_MULTICAST_FILTERS = $40;
 USB_CDC_SET_ETHERNET_PM_PATTERN_FILTER = $41;
 USB_CDC_GET_ETHERNET_PM_PATTERN_FILTER = $42;
 USB_CDC_SET_ETHERNET_PACKET_FILTER     = $43;
 USB_CDC_GET_ETHERNET_STATISTIC         = $44;
 USB_CDC_GET_NTB_PARAMETERS             = $80;
 USB_CDC_GET_NET_ADDRESS                = $81;
 USB_CDC_SET_NET_ADDRESS                = $82;
 USB_CDC_GET_NTB_FORMAT                 = $83;
 USB_CDC_SET_NTB_FORMAT                 = $84;
 USB_CDC_GET_NTB_INPUT_SIZE             = $85;
 USB_CDC_SET_NTB_INPUT_SIZE             = $86;
 USB_CDC_GET_MAX_DATAGRAM_SIZE          = $87;
 USB_CDC_SET_MAX_DATAGRAM_SIZE          = $88;
 USB_CDC_GET_CRC_MODE                   = $89;
 USB_CDC_SET_CRC_MODE                   = $8a;
 
 {Get/Set Line Coding bCharFormat (See: Section 6.3.11 of USB Communications Class Subclass Specification for PSTN Devices 1.2)}
 USB_CDC_1_STOP_BITS   = 0;
 USB_CDC_1_5_STOP_BITS = 1;
 USB_CDC_2_STOP_BITS   = 2;

 {Get/Set Line Coding bParityType (See: Section 6.3.11 of USB Communications Class Subclass Specification for PSTN Devices 1.2)}
 USB_CDC_NO_PARITY    = 0;
 USB_CDC_ODD_PARITY   = 1;
 USB_CDC_EVEN_PARITY  = 2;
 USB_CDC_MARK_PARITY  = 3;
 USB_CDC_SPACE_PARITY = 4;
 
 {Set Control Line State wValue (See: Section 6.3.12 of USB Communications Class Subclass Specification for PSTN Devices 1.2)}
 USB_CDC_ACM_CTRL_DTR = $01; {Indicates to DCE if DTE is present or not. This signal corresponds to V.24 signal 108/2 and RS-232 signal DTR}
 USB_CDC_ACM_CTRL_RTS = $02; {Carrier control for half duplex modems. This signal corresponds to V.24 signal 105 and RS-232 signal RTS}
 
 {Class Specific Notifications (See: Section 6.3 of USB Class Definitions for Communications Devices 1.2)}
 USB_CDC_NOTIFY_NETWORK_CONNECTION = $00;
 USB_CDC_NOTIFY_RESPONSE_AVAILABLE = $01;
 USB_CDC_NOTIFY_SERIAL_STATE       = $20;
 USB_CDC_NOTIFY_SPEED_CHANGE       = $2a;
 
 {Serial State Notification Data (See: Section 6.5.4 of USB Communications Class Subclass Specification for PSTN Devices 1.2)}
 USB_CDC_ACM_CTRL_DCD     = $01; {State of receiver carrier detection mechanism of device. This signal corresponds to V.24 signal 109 and RS-232 signal DCD}
 USB_CDC_ACM_CTRL_DSR     = $02; {State of transmission carrier. This signal corresponds to V.24 signal 106 and RS-232 signal DSR}
 USB_CDC_ACM_CTRL_BRK     = $04; {State of break detection mechanism of the device}
 USB_CDC_ACM_CTRL_RI      = $08; {State of ring signal detection of the device}
 USB_CDC_ACM_CTRL_FRAMING = $10; {A framing error has occurred}
 USB_CDC_ACM_CTRL_PARITY  = $20; {A parity error has occurred}
 USB_CDC_ACM_CTRL_OVERRUN = $40; {Received data has been discarded due to overrun in the device}
 
 USB_CDC_ACM_CTRL_CONTROL_MASK   = USB_CDC_ACM_CTRL_DCD or USB_CDC_ACM_CTRL_DSR or USB_CDC_ACM_CTRL_RI;
 USB_CDC_ACM_CTRL_TRANSIENT_MASK = USB_CDC_ACM_CTRL_BRK or USB_CDC_ACM_CTRL_FRAMING or USB_CDC_ACM_CTRL_PARITY or USB_CDC_ACM_CTRL_OVERRUN;
 
{==============================================================================}
type
 {CDC specific types}
 PUSBCDCDescriptor = ^TUSBCDCDescriptor;
 TUSBCDCDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 end;
 
 {Header Functional Descriptor (See: Section 5.2.3.1 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCHeaderDescriptor = ^TUSBCDCHeaderDescriptor;
 TUSBCDCHeaderDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
  
  bcdCDC:Word; {LE16}
 end;
 
 {Call Management Descriptor (See: Section 5.2.3.2 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCCallManagementDescriptor = ^TUSBCDCCallManagementDescriptor;
 TUSBCDCCallManagementDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;

  bmCapabilities:Byte;  {See USB_CDC_CALL_MGMT_CAP_*}
 end;

 {Abstract Control Management Descriptor (See: Section 5.2.3.3 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCACMDescriptor = ^TUSBCDCACMDescriptor;
 TUSBCDCACMDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  bmCapabilities:Byte;  {See USB_CDC_ACM_*}
 end;
 
 {Union Functional Descriptor (See: Section 5.2.3.8 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCUnionDescriptor = ^TUSBCDCUnionDescriptor;
 TUSBCDCUnionDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;

  bMasterInterface0:Byte;
  bSlaveInterface0:Byte;
  {Additional Slave interfaces dependant on bFunctionLength}
 end;
  
 {Country Selection Functional Descriptor (See: Section 5.2.3.9 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCCountryDescriptor = ^TUSBCDCCountryDescriptor;
 TUSBCDCCountryDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  iCountryCodeRelDate:Byte;
  wCountyCode0:Word; {LE16}
  {Additional Country codes dependant on bFunctionLength}
 end;

 {Network Channel Terminal Functional Descriptor (See: Section 5.2.3.11 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCNetworkTerminalDescriptor = ^TUSBCDCNetworkTerminalDescriptor;
 TUSBCDCNetworkTerminalDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  bEntityId:Byte;
  iName:Byte;
  bChannelIndex:Byte;
  bPhysicalInterface:Byte;
 end; 

 {Ethernet Networking Functional Descriptor (See: Section 5.4 of USB Communications Class Subclass Specification for Ethernet Control Model Devices 1.2)}
 PUSBCDCEthernetDescriptor = ^TUSBCDCEthernetDescriptor;
 TUSBCDCEthernetDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  iMACAddress:Byte;
  bmEthernetStatistics:LongWord; {LE32}
  wMaxSegmentSize:Word;          {LE16}
  wNumberMCFilters:Word;         {LE16}
  bNumberPowerFilters:Byte;
 end; 

 {Telephone Control Model Functional Descriptor (See: Section 6.3.2.3 of USB CDC Subclass Specification for Wireless Mobile Communications Devices 1.1)}
 PUSBCDCTCMDescriptor = ^TUSBCDCTCMDescriptor;
 TUSBCDCTCMDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  bcdVersion:Word;
  wMaxCommand:Word; {LE16}
 end;

 {MDLM Functional Descriptor (See: Section 6.7.2.3 of USB CDC Subclass Specification for Wireless Mobile Communications Devices 1.1)}
 PUSBCDCMDLMDescriptor = ^TUSBCDCMDLMDescriptor;
 TUSBCDCMDLMDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  bcdVersion:Word; {LE16}
  bGUID:array[0..15] of Byte;
 end; 

 {MDLM Detail Functional Descriptor (See: Section 6.7.2.4 of USB CDC Subclass Specification for Wireless Mobile Communications Devices 1.1)}
 PUSBCDCMDLMDetailDescriptor = ^TUSBCDCMDLMDetailDescriptor;
 TUSBCDCMDLMDetailDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  bGuidDescriptorType:Byte; {Associated with TUSBCDCMDLMDescriptor.bGUID}
  bDetailData:array[0..0] of Byte;
 end; 

 {OBEX Control Model Functional Descriptor (See: Section 6.5.2.3 of USB CDC Subclass Specification for Wireless Mobile Communications Devices 1.1)}
 PUSBCDCOBEXDescriptor = ^TUSBCDCOBEXDescriptor;
 TUSBCDCOBEXDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  bcdVersion:Word; {LE16}
 end; 
 
 {NCM Control Model Functional Descriptor}
 PUSBCDCNCMDescriptor = ^TUSBCDCNCMDescriptor;
 TUSBCDCNCMDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
  
  bcdNcmVersion:Word; {LE16}
  bmNetworkCapabilities:Byte;
 end; 

 {MBIM Control Model Functional Descriptor}
 PUSBCDCMBIMDescriptor = ^TUSBCDCMBIMDescriptor;
 TUSBCDCMBIMDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;
 
  bcdMBIMVersion:Word; {LE16}
  wMaxControlMessage:Word; {LE16}
  bNumberFilters:Byte;
  bMaxFilterSize:Byte;
  wMaxSegmentSize:Word; {LE16}
  bmNetworkCapabilities:Byte;
 end;

 {MBIM Extended Functional Descriptor}
 PUSBCDCMBIMExtendedDescriptor = ^TUSBCDCMBIMExtendedDescriptor;
 TUSBCDCMBIMExtendedDescriptor = packed record
  bFunctionLength:Byte;
  bDescriptorType:Byte;
  bDescriptorSubType:Byte;

  bcdMBIMExtendedVersion:Word; {LE16}
  bMaxOutstandingCommandMessages:Byte;
  wMTU:Word; {LE16}
 end;

 {Line Coding Structure (See: Section 6.3.11 of USB Communications Class Subclass Specification for PSTN Devices 1.2)}
 PUSBCDCLineCoding = ^TUSBCDCLineCoding;
 TUSBCDCLineCoding = packed record
  dwDTERate:LongWord; {LE32}
  bCharFormat:Byte;   {See USB_CDC_1_STOP_BITS etc}
  bParityType:Byte;   {See USB_CDC_NO_PARITY etc}
  bDataBits:Byte;
 end;
 
 {Notification Structure (See: Section 6.3 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCNotification = ^TUSBCDCNotification;
 TUSBCDCNotification = packed record
  bmRequestType:Byte;
  bNotificationType:Byte;
  wValue:Word; {LE16}
  wIndex:Word; {LE16}
  wLength:Word; {LE16}
 end;

 {Connection Speed Change Structure (See: Section 6.3.3 of USB Class Definitions for Communications Devices 1.2)}
 PUSBCDCSpeedChange = ^TUSBCDCSpeedChange;
 TUSBCDCSpeedChange = packed record
  DLBitRRate:LongWord; {LE32} {Contains the downlink bit rate, in bits per second, as sent on the IN pipe}
  ULBitRate:LongWord;  {LE32} {Contains the uplink bit rate, in bits per second, as sent on the OUT pipe}
 end; 
 
{==============================================================================}
{var}
 {CDC specific variables}
 
{==============================================================================}
{Initialization Functions}
  
{==============================================================================}
{CDC USB Functions}
  
{==============================================================================}
{CDC Helper Functions}
  
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {CDC specific variables}
  
{==============================================================================}
{==============================================================================}
{Initialization Functions}
  
{==============================================================================}
{==============================================================================}
{CDC USB Functions}
  
{==============================================================================}
{==============================================================================}
{CDC Helper Functions}
  
{==============================================================================}
{==============================================================================}
  
end.
  