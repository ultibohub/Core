{
Ultibo Network interface unit.

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

References
==========

Network Devices
===============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Network;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,Devices,SysUtils,Classes,Ultibo,UltiboClasses;

//To Do //For Ethernet Frame and VLAN tagging etc see: https://en.wikipedia.org/wiki/Ethernet_frame

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {Network specific constants}
 NETWORK_NAME_PREFIX = 'Network';   {Name prefix for Network Devices}
 
 {Network Device Types}
 NETWORK_TYPE_NONE      = 0;
 NETWORK_TYPE_ETHERNET  = 1;
 NETWORK_TYPE_TOKENRING = 2;
 NETWORK_TYPE_80211     = 3;  {IEEE 802.11 Wireless (WiFi) network}
 
 NETWORK_TYPE_MAX       = 3;
 
 {Network Type Names}
 NETWORK_TYPE_NAMES:array[NETWORK_TYPE_NONE..NETWORK_TYPE_MAX] of String = (
  'NETWORK_TYPE_NONE',
  'NETWORK_TYPE_ETHERNET',
  'NETWORK_TYPE_TOKENRING',
  'NETWORK_TYPE_80211');
 
 {Network Device States}
 NETWORK_STATE_CLOSED  = 0;
 NETWORK_STATE_CLOSING = 1;
 NETWORK_STATE_OPENING = 2;
 NETWORK_STATE_OPEN    = 3;
 
 NETWORK_STATE_MAX     = 3;
 
 {Network State Names}
 NETWORK_STATE_NAMES:array[NETWORK_STATE_CLOSED..NETWORK_STATE_MAX] of String = (
  'NETWORK_STATE_CLOSED',
  'NETWORK_STATE_CLOSING',
  'NETWORK_STATE_OPENING',
  'NETWORK_STATE_OPEN');
 
 {Network Device Status}
 NETWORK_STATUS_DOWN  = 0;
 NETWORK_STATUS_UP    = 1;
 
 NETWORK_STATUS_MAX   = 1;
 
 {Network Status Names}
 NETWORK_STATUS_NAMES:array[NETWORK_STATUS_DOWN..NETWORK_STATUS_MAX] of String = (
  'NETWORK_STATUS_DOWN',
  'NETWORK_STATUS_UP');
 
 {Network Device Flags}
 NETWORK_FLAG_NONE       = $00000000;
 NETWORK_FLAG_RX_BUFFER  = $00000001; {Device supports Receive Buffer (NetworkBufferReceive/NetworkBufferRelease)}
 NETWORK_FLAG_TX_BUFFER  = $00000002; {Device supports Transmit Buffer (NetworkBufferAllocate/NetworkBufferTransmit)}
 
 {Network Device Control Codes}
 NETWORK_CONTROL_CLEAR_STATS   = 1;  {Clear Statistics}
 NETWORK_CONTROL_SET_MAC       = 2;  {Set the MAC for this device}
 NETWORK_CONTROL_GET_MAC       = 3;  {Get the MAC for this device}
 NETWORK_CONTROL_SET_LOOPBACK  = 4;  {Set Loopback Mode}          
 NETWORK_CONTROL_RESET         = 5;  {Reset the device}  
 NETWORK_CONTROL_DISABLE       = 6;  {Disable the device}
 NETWORK_CONTROL_GET_HARDWARE  = 7;  {Get Hardware address for this device}
 NETWORK_CONTROL_GET_BROADCAST = 8;  {Get Broadcast address for this device}
 NETWORK_CONTROL_GET_MTU       = 9;  {Get MTU for this device}
 NETWORK_CONTROL_GET_HEADERLEN = 10; {Get Header length for this device}
 
 //To Do //Broadcast/Multicast/Promiscuous/Speed/Duplex/Link etc
 //To Do //Get DeviceId/VendorId/Stats etc
 //To Do //Get Overhead
 //To Do //Get/Set VLAN tags
 
 {Network Lock States}
 NETWORK_LOCK_NONE  = 0;
 NETWORK_LOCK_READ  = 1;
 NETWORK_LOCK_WRITE = 2;
  
 {Network Buffer Size}
 NETWORK_BUFFER_SIZE = 1024; 
 
 {Network Events}
 NETWORK_EVENT_NONE             = $00000000;
 NETWORK_EVENT_SYSTEM_START     = $00000001; {The network sub system is starting}
 NETWORK_EVENT_SYSTEM_STOP      = $00000002; {The network sub system has stopped}
 NETWORK_EVENT_ADAPTERS_START   = $00000004; {All network adapters are being started}
 NETWORK_EVENT_ADAPTERS_STOP    = $00000008; {All network adapters have been stopped}
 NETWORK_EVENT_TRANSPORTS_START = $00000010; {All network transports are being started}
 NETWORK_EVENT_TRANSPORTS_STOP  = $00000020; {All network transports have been stopped}
 NETWORK_EVENT_PROTOCOLS_START  = $00000040; {All network protocols are being started}
 NETWORK_EVENT_PROTOCOLS_STOP   = $00000080; {All network protocols have been stopped}

 {Network Event Signature}
 NETWORK_EVENT_SIGNATURE = $BE1D50C2;
 
 {Network Event States}
 NETWORK_EVENT_STATE_UNREGISTERED = 0;
 NETWORK_EVENT_STATE_REGISTERED   = 1;
 
 {Network Event Flags}
 NETWORK_EVENT_FLAG_NONE = $00000000;
 
const
 {Adapter Types}
 ADAPTER_TYPE_UNKNOWN  = 0;
 ADAPTER_TYPE_WIRED    = 1;
 ADAPTER_TYPE_LOOPBACK = 2; 
 ADAPTER_TYPE_WIRELESS = 3;
 
 {Adapter Threads}
 ADAPTER_THREAD_NAME     = 'Network Adapter';       {Thread name for Network adapter threads}
 ADAPTER_THREAD_PRIORITY = THREAD_PRIORITY_HIGHER;  {Thread priority for Network adapter threads}
 
 {Adapter State}
 ADAPTER_STATE_DISABLED = 0;
 ADAPTER_STATE_ENABLED  = 1;
 
 {Adapter Status}
 ADAPTER_STATUS_DOWN    = 0;
 ADAPTER_STATUS_UP      = 1;
 
 {Configuration Types}
 CONFIG_TYPE_AUTO     = 0; //To Do //Why aren't these in Transport ?
 CONFIG_TYPE_STATIC   = 1;
 CONFIG_TYPE_RARP     = 2;
 CONFIG_TYPE_BOOTP    = 3;
 CONFIG_TYPE_DHCP     = 4;
 CONFIG_TYPE_PSEUDO   = 5;
 CONFIG_TYPE_LOOPBACK = 6;
 
 CONFIG_TYPE_UNKNOWN  = Word(-1);
 
 {Authentication Types}
 AUTH_TYPE_UNKNOWN = 0;
 AUTH_TYPE_EAP     = 1;
 AUTH_TYPE_RSN     = 2;
 
const
 {Frame Types}             
 FRAME_TYPE_UNKNOWN       = 0;  
 FRAME_TYPE_ETHERNET_II   = 1;  {Blue Book}
 FRAME_TYPE_TOKEN_RING    = 3;  {IEEE 802.5}
 FRAME_TYPE_APPLETALK     = 5;  {LocalTalk}
 FRAME_TYPE_ETHERNET_8022 = 11; {IEEE 802.2 LLC}
 FRAME_TYPE_ETHERNET_SNAP = 98; {IEEE 802.2 LLC with SNAP Header}
 FRAME_TYPE_ETHERNET_8023 = 99; {802.3 RAW (Novell)}

 FRAME_START_ETHERNET_SNAP = $AAAA;
 FRAME_START_ETHERNET_8023 = $FFFF;

 ADAPTER_MODE_NONE            = 1;   //To Do //Remove ??  //Maybe not, seem to be for Get/SetReceiveMode ? //Use for Filtering 
 ADAPTER_MODE_LOCAL           = 2;
 ADAPTER_MODE_BROADCAST       = 3;  {This is the default}
 ADAPTER_MODE_LOCAL_MULTI     = 4;  {Mode 3 plus directed Multicast}
 ADAPTER_MODE_BROADCAST_MULTI = 5;  {Mode 3 plus all Multicast}
 ADAPTER_MODE_PROMISCUOUS     = 6;  {Promiscuous mode}

 {Configuration Commands}
 //To Do //Why aren't these in Transport ?
 //To Do //Change these to ADAPTER_CONFIG_ instead ? (or CONFIG_COMMAND_ ? //this one)
 CONFIG_ADAPTER_DISCOVER  = 0;   {Discover an Address from the ConfigHandler} 
 CONFIG_ADAPTER_REQUEST   = 1;   {Request an Address from the ConfigHandler} {Either configured or obtained from Discover}
 CONFIG_ADAPTER_RELEASE   = 2;   {Release the Address obtained from ConfigHandler}
 CONFIG_ADAPTER_RENEW     = 3;   {Renew the Address obtained from ConfigHandler}
 CONFIG_ADAPTER_REBIND    = 4;   {Rebind the Address obtained from ConfigHandler}
 CONFIG_ADAPTER_INFORM    = 5;   {Obtain information only from the ConfigHandler}
 CONFIG_ADAPTER_REBOOT    = 6;   {Request previous Address after a Reboot}

 CONFIG_RETRY_TIMEOUT     = 300000;  {5 min Config Retry Timeout}
 CONFIG_RENEW_TIMEOUT     = 60000;   {1 min Config Renew Timeout}
 CONFIG_REBIND_TIMEOUT    = 60000;   {1 min Config Rebind Timeout}

 {Authentication Commands}
 AUTH_COMMAND_ASSOCIATE      = 0;
 AUTH_COMMAND_DISASSOCIATE   = 1;
 AUTH_COMMAND_AUTHENTICATE   = 2;
 AUTH_COMMAND_UNAUTHENTICATE = 3;
 
 {Multicast Addressing}
 MAX_MULTICAST_ADDRESS = 8;   {Maximum number of addresses per adapter}

 MAX_PHYSICAL_PACKET = 2048;  {Large enough for all media types} //To Do //Change this to NETWORK_MAX_PACKET ?

 {Hardare Addressing}
 HARDWARE_ADDRESS_SIZE = 6;   {SizeOf(THardwareAddress)}

 {Media Types}
 MEDIA_TYPE_UNKNOWN  = $0000;
 
 MEDIA_TYPE_ETHERNET  = $0001;  {ARP type of Ethernet Hardware (These values must not change, they are the actual values used by ARP packets)}
 MEDIA_TYPE_TOKENRING = $0006;  {ARP type of Token-Ring Hardware (These values must not change, they are the actual values used by ARP packets)}

 {Packet Types}
 PACKET_MIN_TYPE   = $0600;  {If the value Ethernet header TypeLength field is greater than this the frame is Ethernet II}
 
 PACKET_TYPE_IP    = $0800;
 PACKET_TYPE_IP6   = $86DD;
 PACKET_TYPE_ARP   = $0806;
 PACKET_TYPE_RARP  = $8035;
 PACKET_TYPE_8021Q = $8100;  {802.1Q with extended Ethernet header}
 PACKET_TYPE_IPX   = $8137;  {IPX on EII}
 
 PACKET_TYPE_EAPOL = $888E;  {EAP-over-LAN (EAPOL)}
 PACKET_TYPE_RSN   = $88C7;  {RSN pre-authentication}
 PACKET_TYPE_TDLS  = $890D;  {Tunneled direct-link setup}
 
 PACKET_TYPE_RAW   = $FFFF;  {IPX on 802.3}  //To Do //$00     //See: https://en.wikipedia.org/wiki/Ethernet_frame
 PACKET_TYPE_LLC   = $0001;  {IPX on 802.2}  //To Do //$E0 ??  //See: https://en.wikipedia.org/wiki/Ethernet_frame
 
 {Ehternet Network} //To Do //Move to Ethernet constants below
 MIN_ETHERNET_PACKET = 60;
 MAX_ETHERNET_PACKET = 1514;

 //ETHERNET_HEADER_SIZE = 14; {SizeOf(TEthernetHeader);}

 {Ethernet 802.3 Network} {FRAME_TYPE_ETHERNET_8022}
 LLC_HEADER_SIZE = 3;  {SizeOf(TLLCHeader);} {Optionally can be 4 if Control is 2 octets}

 {Ethernet SNAP Network} {FRAME_TYPE_ETHERNET_SNAP}
 SNAP_HEADER_SIZE = 5;  {SizeOf(TSNAPHeader);} 

 {Token Ring Network} //To Do //Move to Token Ring constants below
 MIN_TOKENRING_PACKET = 60;  //To Do //
 MAX_TOKENRING_PACKET = 1514; //To Do //

 TOKENRING_HEADER_SIZE = 14; {SizeOf(TTokenRingHeader);} //To Do //
 
//To Do ////////////////////////////////////////////////////////////////
 
 {Service Sets}
 SERVICE_SET_UNKNOWN = 0;
 SERVICE_SET_BSS     = 1; {Basic Service Set}
 SERVICE_SET_ESS     = 2; {Extended Service Set}
 SERVICE_SET_IBSS    = 3; {Independent Basic Service Set}
 SERVICE_SET_MBSS    = 4; {Mesh Basic Service Set}
 
 {Network logging}
 NETWORK_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Network debugging messages}
 NETWORK_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Network informational messages, such as a device being attached or detached}
 NETWORK_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Network error messages}
 NETWORK_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Network messages}

var 
 NETWORK_DEFAULT_LOG_LEVEL:LongWord = NETWORK_LOG_LEVEL_DEBUG; {Minimum level for Network messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Network logging}
 NETWORK_LOG_ENABLED:Boolean; 
 
{==============================================================================}
const
 {Ethernet specific constants}
 ETHERNET_ADDRESS_SIZE = 6;   {SizeOf(TEthernetAddress)}
 ETHERNET_HEADER_SIZE  = 14;  {SizeOf(TEthernetHeader);}
 ETHERNET_VLAN_SIZE    = 4;   {Length of Ethernet VLAN tag}
 ETHERNET_CRC_SIZE     = 4;   {Length of Ethernet CRC}

 {Ethernet specific sizes}
 ETHERNET_MTU                  = 1500;
 ETHERNET_MAX_PACKET_SIZE      = ETHERNET_HEADER_SIZE + ETHERNET_VLAN_SIZE + ETHERNET_MTU; //To Do //Change this to ETHERNET_MAX_PACKET ?
 //ETHERNET_RECEIVE_BUFFER_SIZE  = ETHERNET_MAX_PACKET_SIZE + ETHERNET_CRC_SIZE + ?? //To Do //See: ETH_RX_BUF_SIZE in ether.h
 ETHERNET_TRANSMIT_BUFFER_SIZE = ETHERNET_MAX_PACKET_SIZE;
 
{==============================================================================}
type
 {Network specific types}
 PHardwareAddress = ^THardwareAddress;
 THardwareAddress = array[0..HARDWARE_ADDRESS_SIZE - 1] of Byte;

 PHardwareAddresses = ^THardwareAddresses;
 THardwareAddresses = array[0..0] of THardwareAddress;
 
 PMulticastAddresses = ^TMulticastAddresses;
 TMulticastAddresses = array[0..MAX_MULTICAST_ADDRESS - 1] of THardwareAddress;

 {Network Packet}
 PNetworkPacket = ^TNetworkPacket;
 TNetworkPacket = record
  Buffer:Pointer;  {Pointer to buffer (Set by driver that owns this packet)}
  Data:Pointer;    {Start of data within buffer (Set by driver that owns this packet)}
  Length:LongInt;  {Length of packet data (Set by driver on Receive / Set by caller on Transmit, contains maximum length on Allocate)}
  Flags:LongWord;  {Packet specific flags (eg Error, Broadcast etc) (Dependant on network type)}
 end;
 
 {Network Buffer}
 PNetworkBuffer = ^TNetworkBuffer;
 TNetworkBuffer = record
  Wait:TSemaphoreHandle;     {Packet ready semaphore}
  Start:LongWord;            {Index of first packet ready}
  Count:LongWord;            {Number of packets ready in buffer}
  Packets:array[0..(NETWORK_BUFFER_SIZE - 1)] of PNetworkPacket;
 end;
 
 {Network Entry}
 PNetworkEntry = ^TNetworkEntry;
 TNetworkEntry = record
  Buffer:Pointer;                  {Pointer to buffer (Allocated by driver that owns this entry)}
  Size:LongWord;                   {Size of the buffer (Total size, usable size is Size minus Offset)}
  Offset:LongWord;                 {Offset to start of data (Data for first packet will be at Buffer plus Offset, must be less than size)}
  Count:LongWord;                  {Number of packets in the entry (Set by driver that owns this entry, must be at least one)}
  DriverData:Pointer;              {Driver private data (Managed by driver that owns this entry)}
  Packets:array of TNetworkPacket; {Array of 0 to Count - 1 packets in this entry (Allocated by driver that owns this entry, must be at least one)}
 end;
 
 {Network Queue}
 PNetworkQueue = ^TNetworkQueue;
 TNetworkQueue = record
  Buffer:TBufferHandle;           {Handle for entry buffers}
  Wait:TSemaphoreHandle;          {Entry ready semaphore}
  Start:LongWord;                 {Index of first entry ready}
  Count:LongWord;                 {Number of entries ready in queue}
  Flags:LongWord;                 {Queue specific flags (eg Paused, Halted etc) (Managed by driver)}
  Entries:array of PNetworkEntry; {Array of 0 to Total - 1 entries in this queue (Allocated by driver that owns this queue)}
 end;
 
 {Network Device}
 PNetworkDevice = ^TNetworkDevice;
 
 {Network Enumeration Callback}
 TNetworkEnumerate = function(Network:PNetworkDevice;Data:Pointer):LongWord;
 {Network Notification Callback}
 TNetworkNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Network Device Methods}
 TNetworkDeviceOpen = function(Network:PNetworkDevice):LongWord;
 TNetworkDeviceClose = function(Network:PNetworkDevice):LongWord;
 TNetworkDeviceRead = function(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
 TNetworkDeviceWrite = function(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
 TNetworkDeviceControl = function(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
 
 TNetworkBufferAllocate = function(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
 TNetworkBufferRelease = function(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
 TNetworkBufferReceive = function(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
 TNetworkBufferTransmit = function(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
 
 TNetworkDevice = record
  {Device Properties}
  Device:TDevice;                        {The Device entry for this Network}
  {Network Properties}                   
  NetworkId:LongWord;                    {Unique Id of this Network in the Network table}
  NetworkState:LongWord;                 {Network state (eg NETWORK_STATE_OPEN)}
  NetworkStatus:LongWord;                {Network status (eg NETWORK_STATUS_UP)}
  DeviceOpen:TNetworkDeviceOpen;         {A Device specific DeviceOpen method implementing a standard Network device interface}
  DeviceClose:TNetworkDeviceClose;       {A Device specific DeviceClose method implementing a standard Network device interface}
  DeviceRead:TNetworkDeviceRead;         {A Device specific DeviceRead method implementing a standard Network device interface}
  DeviceWrite:TNetworkDeviceWrite;       {A Device specific DeviceWrite method implementing a standard Network device interface}
  DeviceControl:TNetworkDeviceControl;   {A Device specific DeviceControl method implementing a standard Network device interface}
  BufferAllocate:TNetworkBufferAllocate; {A Device specific BufferAllocate method implementing a standard Network device interface}
  BufferRelease:TNetworkBufferRelease;   {A Device specific BufferRelease method implementing a standard Network device interface}
  BufferReceive:TNetworkBufferReceive;   {A Device specific BufferReceive method implementing a standard Network device interface}
  BufferTransmit:TNetworkBufferTransmit; {A Device specific BufferTransmit method implementing a standard Network device interface}
  {Driver Properties}
  Lock:TMutexHandle;                     {Network lock}
  Buffer:TNetworkBuffer;                 {Network receive buffer}
  TransmitWait:TSemaphoreHandle;         {Transmit complete semaphore}
  ReceiveBuffer:TBufferHandle;           {Buffer for receive packets}
  TransmitBuffer:TBufferHandle;          {Buffer for transmit packets}
                                         
  ReceiveQueue:TNetworkQueue;            {Queue for receive packets}
  TransmitQueue:TNetworkQueue;           {Queue for transmit packets (If applicable)}
  {Statistics Properties}                
  ReceiveCount:LongWord;                 
  ReceiveErrors:LongWord;                
  TransmitCount:LongWord;                
  TransmitErrors:LongWord;               
  BufferOverruns:LongWord;               
  BufferUnavailable:LongWord;               
  {Internal Properties}                                                                          
  Prev:PNetworkDevice;                   {Previous entry in Network table}
  Next:PNetworkDevice;                   {Next entry in Network table}
 end; 
 
 {Network Event}
 PNetworkEvent = ^TNetworkEvent;
 
 {Network Event Callback}
 TNetworkEventCallback = function(Data:Pointer;Event:LongWord):LongWord;
 
 TNetworkEvent = record
  {Event Properties}
  Signature:LongWord;             {Signature for entry validation}
  EventState:LongWord;            {Event state (eg Registered/Unregistered)}
  EventFlags:LongWord;            {Event flags (eg NETWORK_EVENT_FLAG_NONE)}
  Callback:TNetworkEventCallback; {The callback for network events}
  Data:Pointer;                   {A pointer to callback specific data to be passed with events (Optional)}
  Event:LongWord;                 {The mask of events to notify on (eg NETWORK_EVENT_SYSTEM_START etc)}
  {Internal Properties}
  Prev:PNetworkEvent;             {Previous entry in Event table}
  Next:PNetworkEvent;             {Next entry in Event table}
 end; 
  
//To Do ///////////////////////////////////////////////////////////////////////////
 
type
 {Generic Network}
 //PHardwareAddress = ^THardwareAddress;
 //THardwareAddress = array[0..5] of Byte;

 //PMulticastAddresses = ^TMulticastAddresses;
 //TMulticastAddresses = array[0..MAX_MULTICAST_ADDRESS - 1] of THardwareAddress;

 PPacketFragment = ^TPacketFragment;
 TPacketFragment = packed record
  Size:Integer;
  Data:Pointer;
  Next:PPacketFragment;
 end;

 {Ethernet Network} {FRAME_TYPE_ETHERNET_II}
 //PEthernetAddress = ^TEthernetAddress;
 //TEthernetAddress = array[0..5] of Byte;

 //PEthernetHeader = ^TEthernetHeader;
 //TEthernetHeader = packed record {14 Bytes}
 // DestAddress:THardwareAddress;
 // SourceAddress:THardwareAddress;
 // TypeLength:Word;  {Length or Type (IEEE 802.3 or BlueBook/DIX Ethernet)}
 //end;

 {Ethernet 802.3 Network} {FRAME_TYPE_ETHERNET_8022}
 PLLCHeader = ^TLLCHeader;
 TLLCHeader = packed record
  DSAP:Byte;
  SSAP:Byte;
  Control:Byte;
  Information:array[0..0] of Byte;  {Optional (Control byte can be 1 or 2 octets)}
 end;

 {Ethernet SNAP Network} {FRAME_TYPE_ETHERNET_SNAP}
 PSNAPHeader = ^TSNAPHeader;
 TSNAPHeader = packed record
  OUI:array[0..2] of Byte;
  ProtocolID:Word;
  Data:array[0..0] of Byte;  {Not part of header, included to provide a pointer to the start of data}
 end;

 {Token Ring Network}
 PTokenRingAddress = ^TTokenRingAddress;
 TTokenRingAddress = array[0..5] of Byte;

 PTokenRingHeader = ^TTokenRingHeader;
 TTokenRingHeader = packed record
  //To Do //More
 end;

type
 {Generic Adapter}
 PAdapterParams = ^TAdapterParams; //To Do //Adjust for new model //Remove ?
 TAdapterParams = packed record
  Version:Word;       { Driver version   }
  FrameType:Byte;     { Driver class  }
  AdapterType:Word;   { Driver type   }
  Number:Byte;        { Driver number }
  Name:PChar;         { Driver name   }
  Functionality:Byte; { How good is this driver }
 end;

 PNetworkParams = ^TNetworkParams; //To Do //Adjust for new model //Remove ?
 TNetworkParams = packed record
  MajorRevision:Byte;      { Major revision ID of packet specs }
  MinorRevision:Byte;      { Minor revision ID of packet specs }
  ParamLength:Byte;        { Length of structure in Bytes      }
  AddressLength:Byte;      { Length of a MAC address           }
  MTU:Word;                { MTU, including MAC headers        }
  MulticastAvailable:Word; { buffer size for multicast addr.   }
  RxBuffers:Word;          { No of back-to-back MTU rcvs) - 1  }
  TxBuffers:Word;          { No of successive xmits) - 1       }
  IntNo:Word;              { Interrupt No to hook for post-EOI processing, 0 = none }
 end;

 PAdapterStatistics = ^TAdapterStatistics; 
 TAdapterStatistics = record
  PacketsIn:Int64;
  PacketsOut:Int64;
  BytesIn:Int64; 
  BytesOut:Int64;
  ErrorsIn:Int64;
  ErrorsOut:Int64;
  PacketsLost:Int64;
 end;
 
{==============================================================================}
const
 {Network specific constants}
 {Generic Network}
 HARDWARE_DEFAULT:THardwareAddress = ($00,$00,$00,$00,$00,$00);
 HARDWARE_LOOPBACK:THardwareAddress = ($00,$00,$00,$00,$00,$01);
 HARDWARE_BROADCAST:THardwareAddress = ($FF,$FF,$FF,$FF,$FF,$FF);
 
{==============================================================================}
type
 {Ethernet specific types}
 PEthernetAddress = ^TEthernetAddress;
 TEthernetAddress = array[0..ETHERNET_ADDRESS_SIZE - 1] of Byte;

 PEthernetHeader = ^TEthernetHeader;
 TEthernetHeader = packed record
  DestAddress:THardwareAddress;
  SourceAddress:THardwareAddress;
  TypeLength:Word;          {Length or Type (IEEE 802.3 or BlueBook/DIX Ethernet)}
  Data:array[0..0] of Byte; {Not part of header, included to provide a pointer to the start of data}
 end;  
 
 //To Do //PEthernetPacket ?
 
{==============================================================================}
const
 {Ethernet specific constants}
 ETHERNET_DEFAULT:THardwareAddress = ($00,$00,$00,$00,$00,$00);
 ETHERNET_LOOPBACK:THardwareAddress = ($00,$00,$00,$00,$00,$01);
 ETHERNET_BROADCAST:THardwareAddress = ($FF,$FF,$FF,$FF,$FF,$FF);
 ETHERNET_MULTICAST:THardwareAddress = ($01,$00,$5E,$00,$00,$00);
 
{==============================================================================}
type
 {Network specific classes}
 TNetworkList = class;
 TNetworkAdapter = class;
 TAdapterCallback = function(AAdapter:TNetworkAdapter):Boolean of object;

 TNetworkSettings = class;
 
 TAdapterManager = class(TObject)
   constructor Create(ASettings:TNetworkSettings);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
   FSettings:TNetworkSettings;
   
   {Status Variables}
   FAdapters:TNetworkList;    {List of TNetworkAdapter objects}

   {Event Variables}
   
   {Internal Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
  public
   {Public Properties}
   property Settings:TNetworkSettings read FSettings;

   {Public Methods}
   function AddAdapter(AAdapter:TNetworkAdapter):Boolean;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
   
   function GetAdapterByType(AAdapterType:Word;ALock:Boolean;AState:LongWord):TNetworkAdapter;
   function GetAdapterByDevice(ADevice:PNetworkDevice;ALock:Boolean;AState:LongWord):TNetworkAdapter;
   function GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TNetworkAdapter;
   function GetAdapterByNext(APrevious:TNetworkAdapter;ALock,AUnlock:Boolean;AState:LongWord):TNetworkAdapter;
   
   function StartAdapters:Boolean;
   function StopAdapters:Boolean;
   function ProcessStatus:Boolean;
   function ProcessAdapters:Boolean; //To do //Remove ?
   
   function EnumerateAdapters(ACallback:TAdapterCallback):Boolean;
 end;

 TAdapterPacketHandler = function(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean of object;
 TAdapterMonitorHandler = function(AHandle:THandle;APacket:Pointer;ASize:Integer;AStatus:Pointer):Boolean of object;
 TAdapterAuthenticatorHandler = function(AHandle:THandle;ACommand:Word):Boolean of object;
 
 TAdapterTransport = class(TListObject) {Upstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   FrameType:Word;          //To Do //Do these need lock protection ?
   PacketType:Word;
   PacketName:String;
   PacketHandler:TAdapterPacketHandler;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TAdapterBinding = class(TListObject)  {Upstream}
   constructor Create(ATransport:TAdapterTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   FrameType:Word;        //To Do //Do these need lock protection ?
   PacketType:Word;
   Transport:TAdapterTransport;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TAdapterMonitor = class(TListObject)  {Upstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   MonitorHandler:TAdapterMonitorHandler;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TAdapterAuthenticator = class(TListObject)  {Upstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   AuthType:Word;    {EAP/RSN}
   AuthenticatorHandler:TAdapterAuthenticatorHandler;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;
 
 TNetworkList = class(TLinkedObjList)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle; 
  public
   {Public Methods}
   procedure ClearList;

   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function ReaderConvert:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   function WriterConvert:Boolean;
 end;
 
 TAdapterThread = class;
 TNetworkAdapter = class(TListObject)
   constructor Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FManager:TAdapterManager;
   FDevice:PNetworkDevice;
   FName:String;
   
   {Status Variables}
   FState:Integer;
   FStatus:Integer;
   FMediaType:Word;                   {Physical Media type (Ethernet/Tokenring etc)}
   FAdapterType:Word;             
   FLastError:Integer;
   FThread:TAdapterThread;            {Thread for adapter receiving}
   FBindings:TNetworkList;            {List of TAdapterBinding objects}
   FTransports:TNetworkList;          {List of TAdapterTransport objects}
   FMonitors:TNetworkList;            {List of TAdapterMonitor objects}
   FAuthenticators:TNetworkList;      {List of TAdapterAuthenticator objects}
   
   FStatistics:TAdapterStatistics;
   
   {Event Methods}

   {Internal Methods}
   function GetThreadID:TThreadID;
   
   function GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterTransport;
   function GetTransportByType(APacketType,AFrameType:Word;ALock:Boolean;AState:LongWord):TAdapterTransport;
   function GetTransportByTransport(ATransport:TAdapterTransport;ALock:Boolean;AState:LongWord):TAdapterTransport;
   function GetTransportByNext(APrevious:TAdapterTransport;ALock,AUnlock:Boolean;AState:LongWord):TAdapterTransport;
   
   function GetBindingByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterBinding;
   function GetBindingByType(ATransport:TAdapterTransport;APacketType,AFrameType:Word;ALock:Boolean;AState:LongWord):TAdapterBinding;
   function GetBindingByTransport(ATransport:TAdapterTransport;ALock:Boolean;AState:LongWord):TAdapterBinding;
   function GetBindingByBinding(ABinding:TAdapterBinding;ALock:Boolean;AState:LongWord):TAdapterBinding;
   function GetBindingByNext(APrevious:TAdapterBinding;ALock,AUnlock:Boolean;AState:LongWord):TAdapterBinding;
   
   function GetMonitorByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterMonitor;
   function GetMonitorByMonitor(AMonitor:TAdapterMonitor;ALock:Boolean;AState:LongWord):TAdapterMonitor;
   function GetMonitorByNext(APrevious:TAdapterMonitor;ALock,AUnlock:Boolean;AState:LongWord):TAdapterMonitor;

   function GetAuthenticatorByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterAuthenticator;
   function GetAuthenticatorByType(AAuthType:Word;ALock:Boolean;AState:LongWord):TAdapterAuthenticator;
   function GetAuthenticatorByAuthenticator(AAuthenticator:TAdapterAuthenticator;ALock:Boolean;AState:LongWord):TAdapterAuthenticator;
   function GetAuthenticatorByNext(APrevious:TAdapterAuthenticator;ALock,AUnlock:Boolean;AState:LongWord):TAdapterAuthenticator;
   
   {Protected Methods}
   function AddBinding(ATransport:TAdapterTransport;APacketType,AFrameType:Word):THandle; virtual;
   function RemoveBinding(AHandle:THandle;APacketType:Word):Boolean; virtual;
  public
   {Public Properties}
   property Manager:TAdapterManager read FManager;
   property Device:PNetworkDevice read FDevice;
   property Name:String read FName;   //To Do //Does this need lock protection and UniqueString ? //Yes //LocalLock(Mutex)

   property State:Integer read FState;
   property Status:Integer read FStatus;
   property MediaType:Word read FMediaType;
   property AdapterType:Word read FAdapterType;
   property LastError:Integer read FLastError;
   property ThreadID:TThreadID read GetThreadID;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle; virtual;
   function RemoveTransport(AHandle:THandle;APacketType:Word):Boolean; virtual;

   function AddMonitor(AMonitorHandler:TAdapterMonitorHandler):THandle; virtual;
   function RemoveMonitor(AHandle:THandle):Boolean; virtual;

   function AddAuthenticator(AAuthType:Word;AAuthenticatorHandler:TAdapterAuthenticatorHandler):THandle; virtual;
   function RemoveAuthenticator(AHandle:THandle;AAuthType:Word):Boolean; virtual;
   
   function GetMTU(AHandle:THandle):Word; virtual;

   function SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean; virtual;

   function ResetInterface(AHandle:THandle):Boolean; virtual;
   function TerminateDriver(AHandle:THandle):Boolean; virtual;

   function GetReceiveMode(AHandle:THandle):Word; virtual;
   function SetReceiveMode(AHandle:THandle;AMode:Word):Boolean; virtual;

   function GetAdapterParams(AHandle:THandle):TAdapterParams; virtual;
   function GetNetworkParams(AHandle:THandle):TNetworkParams; virtual;
   function GetStatistics(AHandle:THandle):TAdapterStatistics; virtual;

   function GetDefaultAddress(AHandle:THandle):THardwareAddress; virtual;
   function GetHardwareAddress(AHandle:THandle):THardwareAddress; virtual;
   function SetHardwareAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; virtual;
   function GetBroadcastAddress(AHandle:THandle):THardwareAddress; virtual;
   function GetMulticastAddresses(AHandle:THandle):TMulticastAddresses; virtual;

   function AddMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; virtual;
   function RemoveMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; virtual;

   function StartAdapter:Boolean; virtual;
   function StopAdapter:Boolean; virtual;
   function ProcessStatus:Boolean; virtual;
   function ProcessAdapter:Boolean; virtual;

   function CompareAddress(const AAddress1,AAddress2:THardwareAddress):Boolean; virtual;
   function CompareDefault(AHandle:THandle;const AAddress:THardwareAddress):Boolean; virtual;
   function CompareHardware(AHandle:THandle;const AAddress:THardwareAddress):Boolean; virtual;
   function CompareBroadcast(AHandle:THandle;const AAddress:THardwareAddress):Boolean; virtual;
   function CompareMulticast(AHandle:THandle;const AAddress:THardwareAddress):Boolean; virtual;
 end;

 TAdapterThread = class(TThread)
   constructor Create(AAdapter:TNetworkAdapter);
  protected
   {Internal Variables}
   FAdapter:TNetworkAdapter;
   
   {Internal Methods}
   procedure Execute; override;
   
  public   
   {Public Methods}
   function SendHandle(AHandle:THandle):Boolean;
   function ReceiveHandle:THandle;
 end;
 
 TAdapterBuffer = class(TObject)  {Ring buffer for Packets}
   constructor Create; //To Do //Count (Size ?)
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   FSize:Integer;
   FCount:Integer;
   FNextRead:Integer;
   FNextWrite:Integer;

   {Status Variables}
   FList:TList;            //To Do //Should this be something better than a TList ?
   FMemory:TMemoryStream;  //To Do //Should this be a TMemoryStreamEx ?

   {Event Methods}

   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   procedure SetSize(ASize:Integer);
  public
   {}
   property Size:Integer read FSize write SetSize;
   
   function ReadNext:Pointer;
   function WriteNext:Pointer;
 end;
 
 TWiredAdapter = class(TNetworkAdapter)
   constructor Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
  private
   {Internal Variables}

   {Status Variables}
   FDefaultAddress:THardwareAddress;
   FHardwareAddress:THardwareAddress;
   FBroadcastAddress:THardwareAddress;
   FMulticastAddresses:TMulticastAddresses;
  protected
   {Inherited Methods}

  public
   {}
   function AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle; override;
   function RemoveTransport(AHandle:THandle;APacketType:Word):Boolean; override;

   function GetMTU(AHandle:THandle):Word; override;

   function SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean; override;

   function GetDefaultAddress(AHandle:THandle):THardwareAddress; override;
   function GetHardwareAddress(AHandle:THandle):THardwareAddress; override;
   function SetHardwareAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; override;
   function GetBroadcastAddress(AHandle:THandle):THardwareAddress; override;
   function GetMulticastAddresses(AHandle:THandle):TMulticastAddresses; override;

   function AddMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; override;
   function RemoveMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; override;
   
   function StartAdapter:Boolean; override;
   function StopAdapter:Boolean; override;
   function ProcessAdapter:Boolean; override;
 end;
 
 TNetworkSetting = class(TListObject)
  private
   {Internal Variables}
   FName:String;
   FValue:String;

   FHash:LongWord;
   
   {Internal Methods}
   procedure SetName(const AName:String);
  public
   {Public Properties}
   property Name:String read FName write SetName;
   property Value:String read FValue write FValue;
   
   property Hash:LongWord read FHash;
 end;
 
 TNetworkSettings = class(TObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
   FLocalLock:TMutexHandle;
   
   {Status Variables}
   FHostName:String; 
   FDomainName:String;
   FList:TLinkedObjList;
   
   {Event Variables}
   
   {Internal Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function GetHostName:String;
   procedure SetHostName(const AHostName:String);
   
   function GetDomainName:String;
   procedure SetDomainName(const ADomainName:String);
   
   function ExtractName(const AValue,AToken:String):String;
   function ExtractValue(const AValue,AToken:String):String;
   function ExtractPrefix(const AValue:String):String;
   
   function SplitName(const AName:String):String;
   function SplitPrefix(const AName:String):String;

   function MergePrefix(const APrefix,AName:String):String;
   
   function TranslateString(const AValue,ADefault:String):String;
   function TranslateInteger(const AValue:String;ADefault:Integer):Integer;
   function TranslateBoolean(const AValue:String;ADefault:Boolean):Boolean;
   
   function GetFromList(const AName:String):TNetworkSetting;
   
   function FindFromList(const AName:String):String;
   function FindFromEnvironment(const AName:String):String;
  public
   {Public Properties}
   property HostName:String read GetHostName write SetHostName;
   property DomainName:String read GetDomainName write SetDomainName;
   
   {Public Methods}
   function GetString(const AName:String):String;
   function GetStringDefault(const AName,ADefault:String):String;
   
   function GetInteger(const AName:String):Integer;
   function GetIntegerDefault(const AName:String;ADefault:Integer):Integer;
   
   function GetBoolean(const AName:String):Boolean;
   function GetBooleanDefault(const AName:String;ADefault:Boolean):Boolean;
   
   function AddString(const AName,AValue:String):Boolean;
   function AddInteger(const AName:String;AValue:Integer):Boolean;
   function AddBoolean(const AName:String;AValue:Boolean):Boolean;
   
   function Remove(const AName:String):Boolean;
   
   function LoadFromFile(const AFileName:String;AFlat:Boolean):Boolean;
   function LoadFromStream(AStream:TStream;AFlat:Boolean):Boolean;
   function LoadFromStrings(AStrings:TStrings;AFlat:Boolean):Boolean;
   
   function SaveToFile(const AFileName:String;AFlat:Boolean):Boolean;
   function SaveToStream(AStream:TStream;AFlat:Boolean):Boolean;
   function SaveToStrings(AStrings:TStrings;AFlat:Boolean):Boolean;
   
   function LoadFromEnvironment:Boolean;
 end;
  
 TServiceSet = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle; 
   FLocalLock:TMutexHandle;
   
   {Internal Methods}
   function GetName:String;
   procedure SetName(const AName:String);
   procedure SetServiceSetType(AServiceSetType:LongWord);
   procedure SetServiceSetTime(const AServiceSetTime:Int64);
   procedure SetAddress(const AAddress:THardwareAddress);
   procedure SetAdapter(AAdapter:TNetworkAdapter);
  protected
   {Status Variables}
   FName:String;
   FServiceSetType:LongWord;  {BSS/ESS/IBSS}
   FServiceSetTime:Int64;     {Flush time}
   FAddress:THardwareAddress; {Address of service set}
   FAdapter:TNetworkAdapter;
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Public Properties}
   property Name:String read GetName write SetName;
   property ServiceSetType:LongWord read FServiceSetType write SetServiceSetType;
   property ServiceSetTime:Int64 read FServiceSetTime write SetServiceSetTime;
   property Address:THardwareAddress read FAddress write SetAddress;
   property Adapter:TNetworkAdapter read FAdapter write SetAdapter;
 
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function ReaderConvert:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   function WriterConvert:Boolean;
 end;
 
{==============================================================================}
var
 {Network specific variables}
 AdapterManager:TAdapterManager;
 NetworkSettings:TNetworkSettings;
 
{==============================================================================}
{var}
 {Ethernet specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure NetworkInit;
function NetworkStart:LongWord;
function NetworkStop:LongWord;
 
function NetworkStartCompleted:Boolean;
 
{==============================================================================}
{Network Functions}
function NetworkDeviceOpen(Network:PNetworkDevice):LongWord;
function NetworkDeviceClose(Network:PNetworkDevice):LongWord;
function NetworkDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
function NetworkDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
function NetworkDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

function NetworkBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function NetworkBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
function NetworkBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function NetworkBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;

function NetworkDeviceSetState(Network:PNetworkDevice;State:LongWord):LongWord;
function NetworkDeviceSetStatus(Network:PNetworkDevice;Status:LongWord):LongWord;

function NetworkDeviceCreate:PNetworkDevice;
function NetworkDeviceCreateEx(Size:LongWord):PNetworkDevice;
function NetworkDeviceDestroy(Network:PNetworkDevice):LongWord;

function NetworkDeviceRegister(Network:PNetworkDevice):LongWord;
function NetworkDeviceDeregister(Network:PNetworkDevice):LongWord;

function NetworkDeviceFind(NetworkId:LongWord):PNetworkDevice;
function NetworkDeviceFindByName(const Name:String):PNetworkDevice; inline;
function NetworkDeviceFindByDescription(const Description:String):PNetworkDevice; inline;
function NetworkDeviceEnumerate(Callback:TNetworkEnumerate;Data:Pointer):LongWord;

function NetworkDeviceNotification(Network:PNetworkDevice;Callback:TNetworkNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

function NetworkEventAllocate(Callback:TNetworkEventCallback;Data:Pointer;Event:LongWord):PNetworkEvent;
function NetworkEventRelease(Event:PNetworkEvent):LongWord;

function NetworkEventRegister(Callback:TNetworkEventCallback;Data:Pointer;Event:LongWord):THandle;
function NetworkEventDeregister(Handle:THandle):LongWord;

function NetworkEventNotify(Event:LongWord):LongWord;

{==============================================================================}
{RTL Network Functions}
function SysHostGetName:String;
function SysHostSetName(const AName:String):Boolean; 

function SysHostGetDomain:String;
function SysHostSetDomain(const ADomain:String):Boolean; 

{==============================================================================}
{Network Helper Functions}
function NetworkGetCount:LongWord; inline;

function NetworkDeviceCheck(Network:PNetworkDevice):PNetworkDevice;

function NetworkDeviceTypeToString(NetworkType:LongWord):String;
function NetworkDeviceStateToString(NetworkState:LongWord):String;
function NetworkDeviceStatusToString(NetworkStatus:LongWord):String;

function NetworkDeviceStateToNotification(State:LongWord):LongWord;
function NetworkDeviceStatusToNotification(Status:LongWord):LongWord;

function NetworkEventCheck(Event:PNetworkEvent):PNetworkEvent;

procedure NetworkLog(Level:LongWord;Network:PNetworkDevice;const AText:String);
procedure NetworkLogInfo(Network:PNetworkDevice;const AText:String);
procedure NetworkLogError(Network:PNetworkDevice;const AText:String);
procedure NetworkLogDebug(Network:PNetworkDevice;const AText:String);

function HardwareAddressToString(const AAddress:THardwareAddress):String;
function StringToHardwareAddress(const AAddress:String):THardwareAddress;

function CompareHardwareAddress(const AAddress1,AAddress2:THardwareAddress):Boolean; 
function CompareHardwareDefault(const AAddress:THardwareAddress):Boolean; 
function CompareHardwareBroadcast(const AAddress:THardwareAddress):Boolean; 
function CompareHardwareMulticast(const AAddress:THardwareAddress):Boolean; 

function AdapterTypeToString(AType:Word):String;
function AdapterModeToString(AMode:Word):String;
function AdapterConfigToString(AConfig:Word):String;
function AdapterStateToString(AState:Integer):String;
function AdapterStatusToString(AStatus:Integer):String;

function FrameTypeToString(AType:Word):String;
function MediaTypeToString(AType:Word):String;
function PacketTypetoString(AType:Word):String;

function ConfigTypeToString(AType:Word):String;
function ConfigCommandToString(ACommand:Word):String;

function AuthTypeToString(AType:Word):String;
function AuthCommandToString(ACommand:Word):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Network specific variables}
 NetworkInitialized:Boolean;
 NetworkStarted:Boolean;
 
 NetworkTable:PNetworkDevice;
 NetworkTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 NetworkTableCount:LongWord;

 NetworkEventTable:PNetworkEvent;
 NetworkEventLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 NetworkEventCount:LongWord;
 
 
{==============================================================================}
{==============================================================================}
{var}
 {Ethernet specific variables}
 
{==============================================================================}
{==============================================================================}
{TAdapterManager}
constructor TAdapterManager.Create(ASettings:TNetworkSettings);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FSettings:=ASettings;
 
 FAdapters:=TNetworkList.Create;
end;

{==============================================================================}

destructor TAdapterManager.Destroy;
begin
 {}
 WriterLock;
 try
  FAdapters.Free;
  FSettings:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TAdapterManager.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterManager.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterManager.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterManager.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterManager.AddAdapter(AAdapter:TNetworkAdapter):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'AdapterManager: AddAdapter');
  {$ENDIF}

  {Acquire Lock}
  FAdapters.WriterLock;
  try
   {Add Adapter}
   Result:=FAdapters.Add(AAdapter);
  finally
   {Release Lock}
   FAdapters.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TAdapterManager.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'AdapterManager: RemoveAdapter');
  {$ENDIF}

  {Acquire Lock}
  FAdapters.WriterLock;
  try
   {Remove Adapter}
   Result:=FAdapters.Remove(AAdapter);
  finally
   {Release Lock}
   FAdapters.WriterUnlock;
  end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TAdapterManager.GetAdapterByType(AAdapterType:Word;ALock:Boolean;AState:LongWord):TNetworkAdapter;
var
 Adapter:TNetworkAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.AdapterType = AAdapterType then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
    
    {Get Next}
    Adapter:=TNetworkAdapter(Adapter.Next);
   end;
 finally
  FAdapters.ReaderUnlock;
 end;   
end;

{==============================================================================}

function TAdapterManager.GetAdapterByDevice(ADevice:PNetworkDevice;ALock:Boolean;AState:LongWord):TNetworkAdapter;
var
 Adapter:TNetworkAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.Device = ADevice then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
    
    {Get Next}
    Adapter:=TNetworkAdapter(Adapter.Next);
   end;
 finally
  FAdapters.ReaderUnlock;
 end;   
end;

{==============================================================================}

function TAdapterManager.GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TNetworkAdapter;
var
 Adapter:TNetworkAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter = AAdapter then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
    
    {Get Next}
    Adapter:=TNetworkAdapter(Adapter.Next);
   end;
 finally
  FAdapters.ReaderUnlock;
 end;   
end;

{==============================================================================}

function TAdapterManager.GetAdapterByNext(APrevious:TNetworkAdapter;ALock,AUnlock:Boolean;AState:LongWord):TNetworkAdapter;
var
 Adapter:TNetworkAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Adapter:=TNetworkAdapter(FAdapters.First);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;
   end
  else
   begin
    {Get Next}
    Adapter:=TNetworkAdapter(APrevious.Next);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TAdapterManager.StartAdapters:Boolean;
var
 Adapter:TNetworkAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'AdapterManager: StartAdapters');
  {$ENDIF}
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Check Adapter}
    if not Settings.GetBoolean(Adapter.Name + '_DISABLED') then
     begin
      {Start Adapter}
      if not(Adapter.StartAdapter) then Result:=False;
     end; 
    
    {Get Next}
    Adapter:=TNetworkAdapter(GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ));
   end;
 finally
  ReaderUnlock;
 end;   
end;

{==============================================================================}

function TAdapterManager.StopAdapters:Boolean;
var
 Adapter:TNetworkAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'AdapterManager: StopAdapters');
  {$ENDIF}
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Stop Adapter}
    if not(Adapter.StopAdapter) then Result:=False;
    
    {Get Next}
    Adapter:=TNetworkAdapter(GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ));
   end;
 finally
  ReaderUnlock;
 end;   
end;

{==============================================================================}

function TAdapterManager.ProcessStatus:Boolean;
var
 Adapter:TNetworkAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'AdapterManager: ProcessStatus');
  {$ENDIF}
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Process Status}
    if not(Adapter.ProcessStatus) then Result:=False;
    
    {Get Next}
    Adapter:=TNetworkAdapter(GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ));
   end;
 finally
  ReaderUnlock;
 end;   
end;

{==============================================================================}
//To do //Remove ?
function TAdapterManager.ProcessAdapters:Boolean;
var
 Adapter:TNetworkAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'AdapterManager: ProcessAdapters');
  {$ENDIF}
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Process Adapter}
    if not(Adapter.ProcessAdapter) then Result:=False;
    
    {Get Next}
    Adapter:=TNetworkAdapter(GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ));
   end;
 finally
  ReaderUnlock;
 end;   
end;

{==============================================================================}

function TAdapterManager.EnumerateAdapters(ACallback:TAdapterCallback):Boolean;
var
 Adapter:TNetworkAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Check Callback}
  if not Assigned(ACallback) then Exit;
 
  Result:=True;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'AdapterManager: EnumerateAdapters');
  {$ENDIF}
  
  {Get Adapter}
  Adapter:=TNetworkAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Enumerate Adapter}
    if not(ACallback(Adapter)) then Result:=False;
    
    {Get Next}
    Adapter:=TNetworkAdapter(GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ));
   end;
 finally
  ReaderUnlock;
 end;   
end;

{==============================================================================}
{==============================================================================}
{TAdapterTransport}
constructor TAdapterTransport.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FrameType:=FRAME_TYPE_UNKNOWN;
 PacketType:=PACKET_TYPE_RAW;
 PacketName:='';
 PacketHandler:=nil;
end;

{==============================================================================}

destructor TAdapterTransport.Destroy;
begin
 {}
 WriterLock;
 try
  PacketHandler:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TAdapterTransport.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterTransport.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterTransport.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterTransport.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TAdapterBinding}
constructor TAdapterBinding.Create(ATransport:TAdapterTransport);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FrameType:=FRAME_TYPE_UNKNOWN;
 PacketType:=PACKET_TYPE_RAW;
 Transport:=ATransport
end;

{==============================================================================}

destructor TAdapterBinding.Destroy;
begin
 {}
 WriterLock;
 try
  Transport:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TAdapterBinding.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterBinding.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterBinding.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterBinding.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TAdapterMonitor}
constructor TAdapterMonitor.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;

 MonitorHandler:=nil;
end;
 
{==============================================================================}

destructor TAdapterMonitor.Destroy; 
begin
 {}
 WriterLock;
 try
  MonitorHandler:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TAdapterMonitor.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterMonitor.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterMonitor.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterMonitor.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TAdapterAuthenticator}
constructor TAdapterAuthenticator.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;

 AuthType:=AUTH_TYPE_UNKNOWN;
 AuthenticatorHandler:=nil;
end;
 
{==============================================================================}

destructor TAdapterAuthenticator.Destroy; 
begin
 {}
 WriterLock;
 try
  AuthenticatorHandler:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TAdapterAuthenticator.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterAuthenticator.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterAuthenticator.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterAuthenticator.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkList}
constructor TNetworkList.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
end;

{==============================================================================}

destructor TNetworkList.Destroy;
begin
 {}
 WriterLock;
 try
  ClearList;
  inherited Destroy;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

procedure TNetworkList.ClearList;
var
 Next:TListObject;
 Current:TListObject;
begin
 {}
 WriterLock;
 try
  {Get Object}
  Next:=First;
  while Next <> nil do
   begin
    {Get Next}
    Current:=Next;
    Next:=Current.Next;
    
    {Free Object}
    Current.Free;
   end;
  
  {Reset Defaults}
  Clear;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNetworkList.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkList.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkList.ReaderConvert:Boolean; 
{Convert a Reader lock to a Writer lock}
begin
 {}
 Result:=(SynchronizerReaderConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkList.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkList.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkList.WriterConvert:Boolean;
{Convert a Writer lock to a Reader lock}
begin
 {}
 Result:=(SynchronizerWriterConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkAdapter}
constructor TNetworkAdapter.Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FManager:=AManager;
 FDevice:=ADevice;
 FName:=AName;

 FState:=ADAPTER_STATE_DISABLED;
 FStatus:=ADAPTER_STATUS_DOWN;
 FMediaType:=MEDIA_TYPE_UNKNOWN;
 FAdapterType:=ADAPTER_TYPE_UNKNOWN;
 FLastError:=0;
 FThread:=nil;
 FBindings:=TNetworkList.Create;
 FTransports:=TNetworkList.Create;
 FMonitors:=TNetworkList.Create;
 FAuthenticators:=TNetworkList.Create;
 FillChar(FStatistics,SizeOf(TAdapterStatistics),0);
 if FManager <> nil then FManager.AddAdapter(Self);
end;

{==============================================================================}

destructor TNetworkAdapter.Destroy;
begin
 {}
 WriterLock;
 try
  if FManager <> nil then FManager.RemoveAdapter(Self);
  FAuthenticators.Free;
  FMonitors.Free;
  FTransports.Free;
  FBindings.Free;
  FThread:=nil;
  FState:=ADAPTER_STATE_DISABLED;
  FStatus:=ADAPTER_STATUS_DOWN;
  FDevice:=nil;
  FManager:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkAdapter.GetThreadID:TThreadID;
begin
 {}
 //To Do //Lock
 Result:=INVALID_HANDLE_VALUE;
 
 if FThread = nil then Exit;
 
 Result:=FThread.ThreadID;
end;

{==============================================================================}

function TNetworkAdapter.GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterTransport;
var
 Transport:TAdapterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;

  {Get Transport}
  Transport:=TAdapterTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if THandle(Transport) = AHandle then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
      Exit;
     end;
    
    {Get Next}
    Transport:=TAdapterTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetTransportByType(APacketType,AFrameType:Word;ALock:Boolean;AState:LongWord):TAdapterTransport;
var
 Transport:TAdapterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TAdapterTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if (Transport.PacketType = APacketType) and (Transport.FrameType = AFrameType) then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TAdapterTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;
  
{==============================================================================}
  
function TNetworkAdapter.GetTransportByTransport(ATransport:TAdapterTransport;ALock:Boolean;AState:LongWord):TAdapterTransport;
var
 Transport:TAdapterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;

  {Get Transport}
  Transport:=TAdapterTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport = ATransport then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
      Exit;
     end;
    
    {Get Next}
    Transport:=TAdapterTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;
  
{==============================================================================}
  
function TNetworkAdapter.GetTransportByNext(APrevious:TAdapterTransport;ALock,AUnlock:Boolean;AState:LongWord):TAdapterTransport;
var
 Transport:TAdapterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Transport:=TAdapterTransport(FTransports.First);
    if Transport <> nil then
     begin
      {Lock Transport}
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
     end;
   end
  else
   begin
    {Get Next}
    Transport:=TAdapterTransport(APrevious.Next);
    if Transport <> nil then
     begin
      {Lock Transport}
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FTransports.ReaderUnlock;
 end; 
end;
  
{==============================================================================}

function TNetworkAdapter.GetBindingByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterBinding;
var
 Binding:TAdapterBinding;
begin
 {}
 FBindings.ReaderLock;
 try
  Result:=nil;
  
  {Get Binding}
  Binding:=TAdapterBinding(FBindings.First);
  while Binding <> nil do
   begin
    {Check Binding}   
    if THandle(Binding) = AHandle then
     begin
      {Lock Binding} 
      if ALock then if AState = NETWORK_LOCK_READ then Binding.ReaderLock else Binding.WriterLock;
      
      {Return Result}
      Result:=Binding;
      Exit;
     end;
     
    {Get Next} 
    Binding:=TAdapterBinding(Binding.Next);
   end;
 finally 
  FBindings.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetBindingByType(ATransport:TAdapterTransport;APacketType,AFrameType:Word;ALock:Boolean;AState:LongWord):TAdapterBinding;
var
 Binding:TAdapterBinding;
begin
 {}
 FBindings.ReaderLock;
 try
  Result:=nil;
  
  {Get Binding}
  Binding:=TAdapterBinding(FBindings.First);
  while Binding <> nil do
   begin
    {Check Binding}
    if (Binding.Transport = ATransport) then
     begin
      if (Binding.PacketType = APacketType) and (Binding.FrameType = AFrameType) then
       begin
        {Lock Binding} 
        if ALock then if AState = NETWORK_LOCK_READ then Binding.ReaderLock else Binding.WriterLock;
       
        {Return Result}
        Result:=Binding;
        Exit;
       end;
     end;
     
    {Get Next} 
    Binding:=TAdapterBinding(Binding.Next);
   end;
 finally 
  FBindings.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetBindingByTransport(ATransport:TAdapterTransport;ALock:Boolean;AState:LongWord):TAdapterBinding;
var
 Binding:TAdapterBinding;
begin
 {}
 FBindings.ReaderLock;
 try
  Result:=nil;
  
  {Get Binding}
  Binding:=TAdapterBinding(FBindings.First);
  while Binding <> nil do
   begin
    {Check Binding}
    if Binding.Transport = ATransport then
     begin
      {Lock Binding} 
      if ALock then if AState = NETWORK_LOCK_READ then Binding.ReaderLock else Binding.WriterLock;
      
      {Return Result}
      Result:=Binding;
      Exit;
     end;
    
    {Get Next} 
    Binding:=TAdapterBinding(Binding.Next);
   end;
 finally 
  FBindings.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetBindingByBinding(ABinding:TAdapterBinding;ALock:Boolean;AState:LongWord):TAdapterBinding;
var
 Binding:TAdapterBinding;
begin
 {}
 FBindings.ReaderLock;
 try
  Result:=nil;
  
  {Get Binding}
  Binding:=TAdapterBinding(FBindings.First);
  while Binding <> nil do
   begin
    {Check Binding}   
    if Binding = ABinding then
     begin
      {Lock Binding} 
      if ALock then if AState = NETWORK_LOCK_READ then Binding.ReaderLock else Binding.WriterLock;
      
      {Return Result}
      Result:=Binding;
      Exit;
     end;
     
    {Get Next} 
    Binding:=TAdapterBinding(Binding.Next);
   end;
 finally 
  FBindings.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetBindingByNext(APrevious:TAdapterBinding;ALock,AUnlock:Boolean;AState:LongWord):TAdapterBinding;
var
 Binding:TAdapterBinding;
begin
 {}
 FBindings.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Binding:=TAdapterBinding(FBindings.First);
    if Binding <> nil then
     begin
      {Lock Binding}
      if ALock then if AState = NETWORK_LOCK_READ then Binding.ReaderLock else Binding.WriterLock;
      
      {Return Result}
      Result:=Binding;
     end;
   end
  else
   begin
    {Get Next}
    Binding:=TAdapterBinding(APrevious.Next);
    if Binding <> nil then
     begin
      {Lock Binding}
      if ALock then if AState = NETWORK_LOCK_READ then Binding.ReaderLock else Binding.WriterLock;
      
      {Return Result}
      Result:=Binding;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FBindings.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetMonitorByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterMonitor;
var
 Monitor:TAdapterMonitor;
begin
 {}
 FMonitors.ReaderLock;
 try
  Result:=nil;

  {Get Monitor}
  Monitor:=TAdapterMonitor(FMonitors.First);
  while Monitor <> nil do
   begin
    {Check Monitor}
    if THandle(Monitor) = AHandle then
     begin
      {Lock Monitor} 
      if ALock then if AState = NETWORK_LOCK_READ then Monitor.ReaderLock else Monitor.WriterLock;
      
      {Return Result}
      Result:=Monitor;
      Exit;
     end;
    
    {Get Next}
    Monitor:=TAdapterMonitor(Monitor.Next);
   end;
 finally 
  FMonitors.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetMonitorByMonitor(AMonitor:TAdapterMonitor;ALock:Boolean;AState:LongWord):TAdapterMonitor;
var
 Monitor:TAdapterMonitor;
begin
 {}
 FMonitors.ReaderLock;
 try
  Result:=nil;

  {Get Monitor}
  Monitor:=TAdapterMonitor(FMonitors.First);
  while Monitor <> nil do
   begin
    {Check Monitor}
    if Monitor = AMonitor then
     begin
      {Lock Monitor} 
      if ALock then if AState = NETWORK_LOCK_READ then Monitor.ReaderLock else Monitor.WriterLock;
      
      {Return Result}
      Result:=Monitor;
      Exit;
     end;
    
    {Get Next}
    Monitor:=TAdapterMonitor(Monitor.Next);
   end;
 finally 
  FMonitors.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetMonitorByNext(APrevious:TAdapterMonitor;ALock,AUnlock:Boolean;AState:LongWord):TAdapterMonitor;
var
 Monitor:TAdapterMonitor;
begin
 {}
 FMonitors.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Monitor:=TAdapterMonitor(FMonitors.First);
    if Monitor <> nil then
     begin
      {Lock Monitor}
      if ALock then if AState = NETWORK_LOCK_READ then Monitor.ReaderLock else Monitor.WriterLock;
      
      {Return Result}
      Result:=Monitor;
     end;
   end
  else
   begin
    {Get Next}
    Monitor:=TAdapterMonitor(APrevious.Next);
    if Monitor <> nil then
     begin
      {Lock Monitor}
      if ALock then if AState = NETWORK_LOCK_READ then Monitor.ReaderLock else Monitor.WriterLock;
      
      {Return Result}
      Result:=Monitor;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FMonitors.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetAuthenticatorByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAdapterAuthenticator;
var
 Authenticator:TAdapterAuthenticator;
begin
 {}
 FAuthenticators.ReaderLock;
 try
  Result:=nil;

  {Get Authenticator}
  Authenticator:=TAdapterAuthenticator(FAuthenticators.First);
  while Authenticator <> nil do
   begin
    {Check Authenticator}
    if THandle(Authenticator) = AHandle then
     begin
      {Lock Authenticator} 
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
      Exit;
     end;
    
    {Get Next}
    Authenticator:=TAdapterAuthenticator(Authenticator.Next);
   end;
 finally 
  FAuthenticators.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetAuthenticatorByType(AAuthType:Word;ALock:Boolean;AState:LongWord):TAdapterAuthenticator;
var
 Authenticator:TAdapterAuthenticator;
begin
 {}
 FAuthenticators.ReaderLock;
 try
  Result:=nil;
  
  {Get Authenticator}
  Authenticator:=TAdapterAuthenticator(FAuthenticators.First);
  while Authenticator <> nil do
   begin
    {Check Authenticator}
    if Authenticator.AuthType = AAuthType then
     begin
      {Lock Authenticator} 
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
      Exit;
     end;
     
    {Get Next} 
    Authenticator:=TAdapterAuthenticator(Authenticator.Next);
   end;
 finally 
  FAuthenticators.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetAuthenticatorByAuthenticator(AAuthenticator:TAdapterAuthenticator;ALock:Boolean;AState:LongWord):TAdapterAuthenticator;
var
 Authenticator:TAdapterAuthenticator;
begin
 {}
 FAuthenticators.ReaderLock;
 try
  Result:=nil;

  {Get Authenticator}
  Authenticator:=TAdapterAuthenticator(FAuthenticators.First);
  while Authenticator <> nil do
   begin
    {Check Authenticator}
    if Authenticator = AAuthenticator then
     begin
      {Lock Authenticator} 
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
      Exit;
     end;
    
    {Get Next}
    Authenticator:=TAdapterAuthenticator(Authenticator.Next);
   end;
 finally 
  FAuthenticators.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.GetAuthenticatorByNext(APrevious:TAdapterAuthenticator;ALock,AUnlock:Boolean;AState:LongWord):TAdapterAuthenticator;
var
 Authenticator:TAdapterAuthenticator;
begin
 {}
 FAuthenticators.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Authenticator:=TAdapterAuthenticator(FAuthenticators.First);
    if Authenticator <> nil then
     begin
      {Lock Authenticator}
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
     end;
   end
  else
   begin
    {Get Next}
    Authenticator:=TAdapterAuthenticator(APrevious.Next);
    if Authenticator <> nil then
     begin
      {Lock Authenticator}
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FAuthenticators.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAdapter.AddBinding(ATransport:TAdapterTransport;APacketType,AFrameType:Word):THandle;
begin
 {Virtual Base Method}
 Result:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function TNetworkAdapter.RemoveBinding(AHandle:THandle;APacketType:Word):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAdapter.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAdapter.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAdapter.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAdapter.AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle;
begin
 {Virtual Base Method}
 Result:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function TNetworkAdapter.RemoveTransport(AHandle:THandle;APacketType:Word):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.AddMonitor(AMonitorHandler:TAdapterMonitorHandler):THandle; 
begin
 {Virtual Base Method}
 Result:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function TNetworkAdapter.RemoveMonitor(AHandle:THandle):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.AddAuthenticator(AAuthType:Word;AAuthenticatorHandler:TAdapterAuthenticatorHandler):THandle;
begin
 {Virtual Base Method}
 Result:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function TNetworkAdapter.RemoveAuthenticator(AHandle:THandle;AAuthType:Word):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.GetMTU(AHandle:THandle):Word;
begin
 {Virtual Base Method}
 Result:=0;
end;

{==============================================================================}

function TNetworkAdapter.SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.ResetInterface(AHandle:THandle):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.TerminateDriver(AHandle:THandle):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.GetReceiveMode(AHandle:THandle):Word;
begin
 {Virtual Base Method}
 Result:=ADAPTER_MODE_NONE;
end;

{==============================================================================}

function TNetworkAdapter.SetReceiveMode(AHandle:THandle;AMode:Word):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.GetAdapterParams(AHandle:THandle):TAdapterParams;
begin
 {Virtual Base Method}
 FillChar(Result,SizeOf(TAdapterParams),0);
end;

{==============================================================================}

function TNetworkAdapter.GetNetworkParams(AHandle:THandle):TNetworkParams;
begin
 {Virtual Base Method}
 FillChar(Result,SizeOf(TNetworkParams),0);
end;

{==============================================================================}

function TNetworkAdapter.GetStatistics(AHandle:THandle):TAdapterStatistics;
begin
 {Virtual Base Method}
 FillChar(Result,SizeOf(TAdapterStatistics),0);
end;

{==============================================================================}

function TNetworkAdapter.GetDefaultAddress(AHandle:THandle):THardwareAddress;
begin
 {Virtual Base Method}
 Result:=HARDWARE_DEFAULT;
end;

{==============================================================================}

function TNetworkAdapter.GetHardwareAddress(AHandle:THandle):THardwareAddress;
begin
 {Virtual Base Method}
 Result:=HARDWARE_DEFAULT;
end;

{==============================================================================}

function TNetworkAdapter.SetHardwareAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.GetBroadcastAddress(AHandle:THandle):THardwareAddress;
begin
 {Virtual Base Method}
 Result:=HARDWARE_BROADCAST;
end;

{==============================================================================}

function TNetworkAdapter.GetMulticastAddresses(AHandle:THandle):TMulticastAddresses;
begin
 {Virtual Base Method}
 FillChar(Result,SizeOf(TMulticastAddresses),0);
end;

{==============================================================================}

function TNetworkAdapter.AddMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.RemoveMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.StartAdapter:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.StopAdapter:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.ProcessStatus:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.ProcessAdapter:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAdapter.CompareAddress(const AAddress1,AAddress2:THardwareAddress):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 for Count:=0 to HARDWARE_ADDRESS_SIZE - 1 do
  begin
   if AAddress1[Count] <> AAddress2[Count] then Exit;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNetworkAdapter.CompareDefault(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
var
 DefaultAddress:THardwareAddress;
begin
 {}
 DefaultAddress:=GetDefaultAddress(AHandle);
 
 Result:=CompareAddress(AAddress,DefaultAddress);
end;

{==============================================================================}

function TNetworkAdapter.CompareHardware(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
var
 HardwareAddress:THardwareAddress;
begin
 {}
 HardwareAddress:=GetHardwareAddress(AHandle);
 
 Result:=CompareAddress(AAddress,HardwareAddress);
end;

{==============================================================================}

function TNetworkAdapter.CompareBroadcast(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
var
 BroadcastAddress:THardwareAddress;
begin
 {}
 BroadcastAddress:=GetBroadcastAddress(AHandle);
 
 Result:=CompareAddress(AAddress,BroadcastAddress);
end;

{==============================================================================}

function TNetworkAdapter.CompareMulticast(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
var
 Count:Integer;
 MulticastAddresses:TMulticastAddresses;
begin
 {}
 MulticastAddresses:=GetMulticastAddresses(AHandle);
 
 for Count:=0 to MAX_MULTICAST_ADDRESS - 1 do
  begin
   Result:=CompareAddress(AAddress,MulticastAddresses[Count]);
   if Result then Exit;
  end;
end;

{==============================================================================}
{==============================================================================}
{TAdapterThread}
constructor TAdapterThread.Create(AAdapter:TNetworkAdapter);
begin
 {}
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
 FAdapter:=AAdapter;
end;

{==============================================================================}

procedure TAdapterThread.Execute; 
var
 WorkBuffer:String;
begin
 {}
 try
  {Get Name}
  WorkBuffer:=ADAPTER_THREAD_NAME;
  if FAdapter <> nil then WorkBuffer:=WorkBuffer + ' (' + FAdapter.Name + ')';
  
  {Set Name}
  ThreadSetName(GetCurrentThreadID,WorkBuffer);
  
  {Set Priority}
  ThreadSetPriority(GetCurrentThreadID,ADAPTER_THREAD_PRIORITY);
  
  while not(Terminated) do
   begin
    {Check Adapter}
    if FAdapter <> nil then
     begin
      {Process Adapter}
      FAdapter.ProcessAdapter;
     end;
   end;  
 except
  on E: Exception do
   begin
    if NETWORK_LOG_ENABLED then NetworkLogError(nil,'AdapterThread: Exception: ' + E.Message + ' at ' + IntToHex(LongWord(ExceptAddr),8));
   end;
 end; 
end;

{==============================================================================}

function TAdapterThread.SendHandle(AHandle:THandle):Boolean;
var
 Message:TMessage;
begin
 {}
 Result:=False;
 
 FillChar(Message,SizeOf(TMessage),0);
 Message.Msg:=LongWord(AHandle);
 if ThreadSendMessage(FThreadID,Message) = ERROR_SUCCESS then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function TAdapterThread.ReceiveHandle:THandle;
var
 Message:TMessage;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 FillChar(Message,SizeOf(TMessage),0);
 if ThreadReceiveMessage(Message) = ERROR_SUCCESS then
  begin
   Result:=THandle(Message.Msg);
  end;
end;

{==============================================================================}
{==============================================================================}
{TAdapterBuffer}
constructor TAdapterBuffer.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FSize:=0;
 FCount:=0;
 FNextRead:=0;
 FNextWrite:=0;

 FList:=TList.Create;
 FMemory:=TMemoryStream.Create;
end;

{==============================================================================}

destructor TAdapterBuffer.Destroy;
begin
 {}
 AcquireLock;
 try
  FMemory.Free;
  FList.Free;
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock}
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TAdapterBuffer.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAdapterBuffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TAdapterBuffer.SetSize(ASize:Integer);
var
 Count:Integer;
begin
 {}
 if not AcquireLock then Exit;
 try
  if ASize > FSize then
   begin
    if FSize = 0 then
     begin
      {First Setting of Size}
      {Preallocate the Memory}
      FMemory.SetSize(MAX_PHYSICAL_PACKET * ASize);
      
      {Create List Pointers for each Buffer}
      for Count:=0 to ASize - 1 do
       begin
        FList.Add(Pointer(PtrUInt(FMemory.Memory) + PtrUInt(Count * MAX_PHYSICAL_PACKET)));
       end;
      FSize:=ASize;
     end
    else
     begin
      {Increasing Size}
      {Reallocate the Memory}
      FMemory.SetSize(MAX_PHYSICAL_PACKET * ASize);
      
      {Create List Pointers for each new Buffer}
      for Count:=FSize to ASize - 1 do
       begin
        FList.Add(Pointer(PtrUInt(FMemory.Memory) + PtrUInt(Count * MAX_PHYSICAL_PACKET)));
       end;
      FSize:=ASize;
     end;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TAdapterBuffer.ReadNext:Pointer;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  if FCount > 0 then
   begin
    Result:=FList.Items[FNextRead];
    
    Inc(FNextRead);
    
    if FNextRead = FSize then
     begin
      FNextRead:=0;
     end;
    
    Dec(FCount);
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TAdapterBuffer.WriteNext:Pointer;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  if FCount < FSize then
   begin
    Result:=FList.Items[FNextWrite];
    
    Inc(FNextWrite);
    
    if FNextWrite = FSize then
     begin
      FNextWrite:=0;
     end;
    
    Inc(FCount);
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TWiredAdapter}
constructor TWiredAdapter.Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
begin
 {}
 inherited Create(AManager,ADevice,AName);
 {Set Defaults}
 FAdapterType:=ADAPTER_TYPE_WIRED;
 FillChar(FDefaultAddress,SizeOf(THardwareAddress),0);
 FillChar(FHardwareAddress,SizeOf(THardwareAddress),0);
 FillChar(FBroadcastAddress,SizeOf(THardwareAddress),0);
 FillChar(FMulticastAddresses,SizeOf(TMulticastAddresses),0);
end;

{==============================================================================}

function TWiredAdapter.AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle;
var
 Transport:TAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: AddTransport (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Packet = ' + PacketTypeToString(APacketType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Frame = ' + FrameTypeToString(AFrameType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Name = ' + APacketName);
  {$ENDIF}
  
  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Get Transport}
  Transport:=TAdapterTransport(GetTransportByType(APacketType,AFrameType,False,NETWORK_LOCK_NONE)); {Do not lock}
  if Transport <> nil then Exit;
  
  {Check Frame Type}
  case AFrameType of 
   FRAME_TYPE_ETHERNET_II,FRAME_TYPE_ETHERNET_8022,FRAME_TYPE_ETHERNET_SNAP,FRAME_TYPE_ETHERNET_8023:begin
     {Check Media Type}
     if FMediaType <> MEDIA_TYPE_ETHERNET then Exit;
    end;
   FRAME_TYPE_TOKEN_RING:begin
     {Check Media Type}
     if FMediaType <> MEDIA_TYPE_TOKENRING then Exit;
    end;
   else
    begin
     {Invalid}
     Exit;
    end;
  end;
  
  {Create Transport}
  Transport:=TAdapterTransport.Create;
  Transport.FrameType:=AFrameType;
  Transport.PacketType:=APacketType;
  Transport.PacketName:=APacketName;
  Transport.PacketHandler:=APacketHandler;
 
  {Acquire Lock}
  FTransports.WriterLock;
  try
   {Add Transport}
   FTransports.Add(Transport);
 
   {Return Result}
   Result:=THandle(Transport);
  finally
   {Release Lock}
   FTransports.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.RemoveTransport(AHandle:THandle;APacketType:Word):Boolean;
var
 Transport:TAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: RemoveTransport (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Packet = ' + PacketTypeToString(APacketType));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;
  
  {Get Transport}
  Transport:=TAdapterTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Transport}
  if Transport.PacketType <> APacketType then
   begin
    {Unlock Transport}
    Transport.WriterUnlock;
    Exit;
   end; 
  
  {Acquire Lock}
  FTransports.WriterLock;
  try
   {Remove Transport}
   FTransports.Remove(Transport);
  
   {Unlock Transport}
   Transport.WriterUnlock;
  
   {Destroy Transport}
   Transport.Free;
 
   {Return Result}
   Result:=True;
  finally
   {Release Lock}
   FTransports.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.GetMTU(AHandle:THandle):Word;
var
 Value:LongWord;
begin
 {}
 ReaderLock;
 try
  Result:=0;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: GetMTU (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
 
  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;

  {Check Device}
  if FDevice = nil then Exit;

  {Get Device MTU}
  if FDevice.DeviceControl(FDevice,NETWORK_CONTROL_GET_MTU,0,Value) <> ERROR_SUCCESS then Exit;
 
  {Return Result}
  Result:=Value;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean;
var
 Size:Integer;
 Length:LongWord;
 Buffer:Pointer;
 LLC:PLLCHeader;
 SNAP:PSNAPHeader;
 Packet:PPacketFragment;
 Ethernet:PEthernetHeader;
 Transport:TAdapterTransport;
begin
 {}
 Result:=False;
  
 {$IFDEF NETWORK_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: SendPacket (' + Name + ')');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Size = ' + IntToStr(ASize));
 {$ENDIF}
  
 {Check Dest}
 if ADest = nil then Exit;
  
 {Check Packet}
 if APacket = nil then Exit;

 {Check State}
 if FState = ADAPTER_STATE_DISABLED then Exit;
 
 {Check Device}
 if FDevice = nil then Exit;
 
 {Get Transport}
 Transport:=TAdapterTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Media = ' + MediaTypeToString(FMediaType));
  {$ENDIF}
  {Check Media Type}
  case FMediaType of
   MEDIA_TYPE_ETHERNET:begin
     {$IFDEF NETWORK_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Frame = ' + FrameTypeToString(Transport.FrameType));
     {$ENDIF}
     {Check Frame Type}
     case Transport.FrameType of
      FRAME_TYPE_ETHERNET_II:begin
        {Check Size}
        if ASize > (MAX_ETHERNET_PACKET - ETHERNET_HEADER_SIZE) then Exit;
     
        {Get Buffer} //To Do //Call Device to Allocate for non copy //Device should return an Offset to allow for it's own header (if any, eg USB)
        Buffer:=GetMem(ASize + ETHERNET_HEADER_SIZE);
        if Buffer = nil then Exit;
     
        {Get Header}
        Ethernet:=PEthernetHeader(Buffer);
     
        {Build Header}
        Ethernet.DestAddress:=PHardwareAddress(ADest)^;
        Ethernet.SourceAddress:=FHardwareAddress;
        Ethernet.TypeLength:=WordNtoBE(Transport.PacketType);

        Size:=ETHERNET_HEADER_SIZE;
       end;
      FRAME_TYPE_ETHERNET_8022:begin
     
        Size:=0;
        Exit; //To Do
       end;
      FRAME_TYPE_ETHERNET_SNAP:begin
     
        Size:=0;
        Exit; //To Do
       end;
      FRAME_TYPE_ETHERNET_8023:begin
   
        Size:=0;
        Exit; //To Do
       end;
     end;
    end;
   MEDIA_TYPE_TOKENRING:begin
     {$IFDEF NETWORK_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Frame = ' + FrameTypeToString(Transport.FrameType));
     {$ENDIF}
     {Check Frame Type}
     case Transport.FrameType of
      FRAME_TYPE_TOKEN_RING:begin

        Size:=0;
        Exit; //To Do
       end;
     end;
    end;
  end;
  
  {Copy Packet Fragments to Buffer}
  Packet:=APacket;
  while Packet <> nil do
   begin
    System.Move(Packet.Data^,Pointer(PtrUInt(Buffer) + PtrUInt(Size))^,Packet.Size);
    Inc(Size,Packet.Size);
    Packet:=Packet.Next;
   end;
  
  {Send the Packet}
  if FDevice.DeviceWrite(FDevice,Buffer,Size,Length) = ERROR_SUCCESS then
   begin
    Result:=True;
   end;
   
  {Release Buffer} //To Do //Call Device to Release for non copy
  FreeMem(Buffer);
 finally 
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.GetDefaultAddress(AHandle:THandle):THardwareAddress; 
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(THardwareAddress),0);
 
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: GetDefaultAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  {Return Result}
  Result:=HARDWARE_DEFAULT;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.GetHardwareAddress(AHandle:THandle):THardwareAddress;
var
 Value:LongWord;
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(THardwareAddress),0);
 
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: GetHardwareAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Get Hardware Address}
  FDevice.DeviceControl(FDevice,NETWORK_CONTROL_GET_HARDWARE,LongWord(@Result),Value);
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.SetHardwareAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
var
 Value:LongWord;
begin
 {}
 WriterLock;
 try
  Result:=False;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: SetHardwareAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Address = ' + HardwareAddressToString(AAddress));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Set Hardware Address}
  if FDevice.DeviceControl(FDevice,NETWORK_CONTROL_SET_MAC,LongWord(@AAddress),Value) = ERROR_SUCCESS then
   begin
    FHardwareAddress:=AAddress;
   
    {Return Result}
    Result:=True;
   end;
 finally 
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.GetBroadcastAddress(AHandle:THandle):THardwareAddress;
var
 Value:LongWord;
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(THardwareAddress),0);
 
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: GetBroadcastAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  {Get Hardware Address}
  FDevice.DeviceControl(FDevice,NETWORK_CONTROL_GET_BROADCAST,LongWord(@Result),Value);
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.GetMulticastAddresses(AHandle:THandle):TMulticastAddresses;
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(TMulticastAddresses),0);
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: GetMulticastAddresses (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  //To Do
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.AddMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
begin
 {}
 WriterLock;
 try
  Result:=False;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: AddMulticastAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Address = ' + HardwareAddressToString(AAddress));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  //To Do
 finally 
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.RemoveMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
begin
 {}
 WriterLock;
 try
  Result:=False;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: RemoveMulticastAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Address = ' + HardwareAddressToString(AAddress));
  {$ENDIF}

  {Check State}
  if FState = ADAPTER_STATE_DISABLED then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  //To Do
 finally 
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.StartAdapter:Boolean;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: StartAdapter (' + Name + ')');
  {$ENDIF}

  {Check State}
  if FState <> ADAPTER_STATE_DISABLED then Exit;
 
  {Check Device}
  if FDevice = nil then Exit;

  {Check Media Type}
  case FDevice.Device.DeviceType of
   NETWORK_TYPE_ETHERNET:FMediaType:=MEDIA_TYPE_ETHERNET;
   NETWORK_TYPE_TOKENRING:FMediaType:=MEDIA_TYPE_TOKENRING;
  end;
  if FMediaType = MEDIA_TYPE_UNKNOWN then Exit;   
  
  {Open Device}
  if FDevice.DeviceOpen(FDevice) = ERROR_SUCCESS then
   begin
    {Set State}
    FState:=ADAPTER_STATE_ENABLED;
    
    {Get Status}
    FStatus:=ADAPTER_STATUS_UP; //To Do //Get status from Device (DeviceControl)
  
    {Get Properties}
    FDefaultAddress:=GetDefaultAddress(INVALID_HANDLE_VALUE);
    FHardwareAddress:=GetHardwareAddress(INVALID_HANDLE_VALUE);
    FBroadcastAddress:=GetBroadcastAddress(INVALID_HANDLE_VALUE); 
    FMulticastAddresses:=GetMulticastAddresses(INVALID_HANDLE_VALUE); 
    
    {Create Thread}
    FThread:=TAdapterThread.Create(Self);
    {FThread.FreeOnTerminate:=True;} {Freed by StopAdapter}
  
    {Start Thread}
    FThread.Start;
  
    {Return Result}
    Result:=True;
   end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.StopAdapter:Boolean;
var
 ResultCode:LongWord;
 Current:TAdapterTransport;
 Transport:TAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
   
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: StopAdapter (' + Name + ')');
  {$ENDIF}

  {Check State}
  if FState <> ADAPTER_STATE_ENABLED then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Check Thread}
  if FThread = nil then Exit;
   
  {Close Device}
  ResultCode:=FDevice.DeviceClose(FDevice);
  if (ResultCode = ERROR_SUCCESS) or (ResultCode = ERROR_NOT_OPEN) then
   begin
    {Terminate Thread}
    FThread.Terminate;
  
    {Wait For Thread}
    FThread.WaitFor;
  
    {Destroy Thread}
    FThread.Free;
    FThread:=nil;
  
    {Get Transport}
    Transport:=TAdapterTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
    while Transport <> nil do
     begin
      {Get Next}
      Current:=Transport;
      Transport:=TAdapterTransport(GetTransportByNext(Current,True,True,NETWORK_LOCK_READ));
    
      {Remove Transport}
      RemoveTransport(THandle(Current),Current.PacketType);
     end;
  
    {Reset State}
    FState:=ADAPTER_STATE_DISABLED;
    
    {Reset Status}
    FStatus:=ADAPTER_STATUS_DOWN;
    
    {Reset Properties}
    FillChar(FDefaultAddress,SizeOf(THardwareAddress),0);
    FillChar(FHardwareAddress,SizeOf(THardwareAddress),0);
    FillChar(FBroadcastAddress,SizeOf(THardwareAddress),0);
    FillChar(FMulticastAddresses,SizeOf(TMulticastAddresses),0);
    
    {Return Result}
    Result:=True;
   end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiredAdapter.ProcessAdapter:Boolean;
var
 Data:Pointer;
 Size:LongWord;
 Length:LongWord;
 Buffer:Pointer;
 FrameType:Word;
 PacketType:Word;
 LLC:PLLCHeader;
 SNAP:PSNAPHeader;
 Dest:PHardwareAddress;
 Source:PHardwareAddress;
 Ethernet:PEthernetHeader;
 Transport:TAdapterTransport;
begin
 {}
 Result:=False;
 
 {Check State}
 if FState = ADAPTER_STATE_DISABLED then Exit;

 {Check Device}
 if FDevice = nil then Exit;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Media Type}
 case FMediaType of
  MEDIA_TYPE_ETHERNET:begin
    {Get Size}
    Size:=ETHERNET_MAX_PACKET_SIZE;
   end;
  MEDIA_TYPE_TOKENRING:begin
    
    Size:=0;
    Exit; //To Do
   end;
 end; 
                      //Need to check device flags/capabilities for support of buffer allocate etc
 {Get Buffer} //To Do //Call Device to Allocate for non copy //Device should return an Offset to allow for it's own header (if any, eg USB)
 Buffer:=GetMem(Size);
 if Buffer = nil then Exit;
 try
  {Receive a Packet}
  if FDevice.DeviceRead(FDevice,Buffer,Size,Length) = ERROR_SUCCESS then
   begin
    {$IFDEF NETWORK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter: ProcessAdapter (' + Name + ')');
    {$ENDIF}
   
    {Check Media Type}
    case FMediaType of
     MEDIA_TYPE_ETHERNET:begin
       {Check Length}
       if Length < ETHERNET_HEADER_SIZE then Exit;
        
       {Get Header}
       Ethernet:=PEthernetHeader(Buffer);
        
       {Determine Frame Type}
       FrameType:=FRAME_TYPE_UNKNOWN;
        
       //To Do //Check for 802.1Q tag here, TypeLength = 0x8100 if tagged, real TypeLength is after the 4 byte 802.1Q tag
       
       {Check Type Length}
       if WordBEtoN(Ethernet.TypeLength) > PACKET_MIN_TYPE then
        begin
         {$IFDEF NETWORK_DEBUG}
         if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Frame = ' + FrameTypeToString(FRAME_TYPE_ETHERNET_II));
         {$ENDIF}
          
         {Get Frame Type}
         FrameType:=FRAME_TYPE_ETHERNET_II;
          
         {Get Packet Type}
         PacketType:=WordBEtoN(Ethernet.TypeLength);
          
         {Get Size}
         Size:=Length - ETHERNET_HEADER_SIZE;
          
         {Get Data}
         Data:=Pointer(PtrUInt(Buffer) + ETHERNET_HEADER_SIZE);
          
         {Get Dest Address}
         Dest:=@Ethernet.DestAddress;
         {Get Source Address}
         Source:=@Ethernet.SourceAddress;
        end
       else
        begin
         {Check Payload Start}
         if PWord(@Ethernet.Data)^ = FRAME_START_ETHERNET_SNAP then
          begin
           {$IFDEF NETWORK_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Frame = ' + FrameTypeToString(FRAME_TYPE_ETHERNET_SNAP));
           {$ENDIF}
            
           {Get Frame Type}
           FrameType:=FRAME_TYPE_ETHERNET_SNAP;
            
           {Get LLC}
           LLC:=PLLCHeader(@Ethernet.Data);
            
           Exit; //To Do
          end
         else if PWord(@Ethernet.Data)^ = FRAME_START_ETHERNET_8023 then 
          begin
           {$IFDEF NETWORK_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Frame = ' + FrameTypeToString(FRAME_TYPE_ETHERNET_8023));
           {$ENDIF}
           
           {Get Frame Type}
           FrameType:=FRAME_TYPE_ETHERNET_8023;
           
           Exit; //To Do
          end
         else
          begin
           {$IFDEF NETWORK_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Frame = ' + FrameTypeToString(FRAME_TYPE_ETHERNET_8022));
           {$ENDIF}
            
           {Get Frame Type}
           FrameType:=FRAME_TYPE_ETHERNET_8022;

           {Get LLC}
           LLC:=PLLCHeader(@Ethernet.Data);
            
           Exit; //To Do
          end;           
        end;         
      end; 
     MEDIA_TYPE_TOKENRING:begin
       Exit; //To Do
      end;
    end; 
     
    {$IFDEF NETWORK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Packet = ' + PacketTypeToString(PacketType));
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiredAdapter:  Size = ' + IntToStr(Size));
    {$ENDIF}
     
    {Get Transport}
    Transport:=TAdapterTransport(GetTransportByType(PacketType,FrameType,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;
    try
     {Check Handler}
     if not(Assigned(Transport.PacketHandler)) then Exit;
     
     {Call the Packet Handler}
     Transport.PacketHandler(THandle(Transport),Source,Dest,Data,Size,CompareBroadcast(THandle(Transport),Dest^));
     
     {Return Result}
     Result:=True;
    finally 
     Transport.ReaderUnlock;
    end; 
   end; 
 finally
  {Release Buffer} //To Do //Call Device to Release for non copy
  FreeMem(Buffer);
 end; 
end;

{==============================================================================}
{==============================================================================}
{TNetworkSetting}
procedure TNetworkSetting.SetName(const AName:String);
begin
 {}
 FName:=AName;
 FHash:=GenerateNameHash(FName,stringHashSize);
end;

{==============================================================================}
{==============================================================================}
{TNetworkSettings}
constructor TNetworkSettings.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FLocalLock:=MutexCreate;
 
 FList:=TLinkedObjList.Create;
end;
 
{==============================================================================}

destructor TNetworkSettings.Destroy;
begin
 {}
 WriterLock;
 try
  FList.Free;
  MutexDestroy(FLocalLock);
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkSettings.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkSettings.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkSettings.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkSettings.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkSettings.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkSettings.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkSettings.GetHostName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 {Check Host Name}
 if Length(FHostName) = 0 then
  begin
   {Get Host Name}
   FHostName:=GetString('HOST_NAME');
   UniqueString(FHostName);
  end;
  
 {Return Host Name} 
 Result:=FHostName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TNetworkSettings.SetHostName(const AHostName:String);
begin
 {}
 if Length(AHostName) = 0 then Exit;
 
 if not AcquireLock then Exit; 
 
 {Set Host Name}
 FHostName:=AHostName;
 UniqueString(FHostName);
 
 {Update Host Name}
 AddString('HOST_NAME',AHostName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNetworkSettings.GetDomainName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 {Check Domain Name}
 if Length(FDomainName) = 0 then
  begin
   {Get Domain Name}
   FDomainName:=GetString('HOST_DOMAIN');
   UniqueString(FDomainName);
  end;
  
 {Return Domain Name} 
 Result:=FDomainName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TNetworkSettings.SetDomainName(const ADomainName:String);
begin
 {}
 if Length(ADomainName) = 0 then Exit;
 
 if not AcquireLock then Exit; 
 
 {Set Host Name}
 FDomainName:=ADomainName;
 UniqueString(FDomainName);
 
 {Update Host Name}
 AddString('HOST_DOMAIN',ADomainName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNetworkSettings.ExtractName(const AValue,AToken:String):String;
{Extract the name from a name value pair}
var
 PosIdx:Integer;
begin
 {}
 PosIdx:=Pos(AToken,AValue);
 if PosIdx = 0 then
  begin
   Result:=AValue;
  end
 else
  begin
   Result:=Copy(AValue,1,PosIdx - 1);
  end;
end;

{==============================================================================}

function TNetworkSettings.ExtractValue(const AValue,AToken:String):String;
{Extract the value from a name value pair}
var
 PosIdx:Integer;
begin
 {}
 PosIdx:=Pos(AToken,AValue);
 if PosIdx = 0 then
  begin
   Result:='';
  end
 else
  begin
   Result:=Copy(AValue,PosIdx + 1,Length(AValue));
  end;  
end;

{==============================================================================}

function TNetworkSettings.ExtractPrefix(const AValue:String):String;
{Extract the prefix from the section header}
begin
 {}
 Result:=AValue;
 if Length(Result) <> 0 then
  begin
   if Result[1] = '[' then
    begin
     Delete(Result,1,1);
    end;
   if Length(Result) > 0 then
    begin
     if Result[Length(Result)] = ']' then
      begin
       Delete(Result,Length(Result),1);
      end;
    end;
  end;
end;

{==============================================================================}

function TNetworkSettings.SplitName(const AName:String):String;
{Split name only portion from the complete name}
var
 PosIdx:Integer;
begin
 {}
 PosIdx:=Pos('_',AName);
 if PosIdx = 0 then
  begin
   Result:=AName;
  end
 else
  begin
   Result:=Copy(AName,PosIdx + 1,Length(AName));
  end;  
end;

{==============================================================================}

function TNetworkSettings.SplitPrefix(const AName:String):String;
{Split prefix from the complete name}
var
 PosIdx:Integer;
begin
 {}
 PosIdx:=Pos('_',AName);
 if PosIdx = 0 then
  begin
   Result:='';
  end
 else
  begin
   Result:=Copy(AName,1,PosIdx - 1);
  end;
end;

{==============================================================================}

function TNetworkSettings.MergePrefix(const APrefix,AName:String):String;
{Add prefix to name to make a complete name}
begin
 {}
 if Length(APrefix) <> 0 then
  begin
   if Length(AName) <> 0 then
    begin
     Result:=APrefix;
     
     {Check Prefix}
     if APrefix[Length(APrefix)] = '_' then
      begin
       Delete(Result,Length(Result),1);
      end;
     
     {Check Name}
     if AName[1] <> '_' then
      begin
       Result:=Result + '_';
      end;
      
     Result:=Result + AName;
    end
   else
    begin
     Result:=APrefix;
    end;    
  end
 else
  begin
   Result:=AName;
  end;  
end;

{==============================================================================}

function TNetworkSettings.TranslateString(const AValue,ADefault:String):String;
begin
 {}
 if Length(AValue) <> 0 then
  begin
   Result:=AValue;
  end
 else
  begin
   Result:=ADefault;
  end;  
end;

{==============================================================================}

function TNetworkSettings.TranslateInteger(const AValue:String;ADefault:Integer):Integer;
begin
 {}
 if Length(AValue) <> 0 then
  begin
   Result:=StrToIntDef(AValue,ADefault);
  end
 else
  begin
   Result:=ADefault;
  end;  
end;

{==============================================================================}

function TNetworkSettings.TranslateBoolean(const AValue:String;ADefault:Boolean):Boolean;
var
 WorkInt:Integer;
begin
 {}
 if Length(AValue) <> 0 then
  begin
   if ADefault then
    begin
     WorkInt:=StrToIntDef(AValue,1);
    end
   else
    begin   
     WorkInt:=StrToIntDef(AValue,0);
    end; 
   if WorkInt = 0 then Result:=False else Result:=True;
  end
 else
  begin
   Result:=ADefault;
  end;  
end;

{==============================================================================}

function TNetworkSettings.GetFromList(const AName:String):TNetworkSetting;
var
 Hash:LongWord;
 Setting:TNetworkSetting;
begin
 {}
 Result:=nil;
 
 if Length(AName) = 0 then Exit;
 
 Hash:=GenerateNameHash(AName,stringHashSize);
 Setting:=TNetworkSetting(FList.First);
 while Setting <> nil do
  begin
   if Setting.Hash = Hash then
    begin
     if Setting.Name = AName then
      begin
       Result:=Setting;
       Exit;
      end;
    end;
   
   Setting:=TNetworkSetting(Setting.Next);
  end;
end;

{==============================================================================}

function TNetworkSettings.FindFromList(const AName:String):String;
var
 Setting:TNetworkSetting;
begin
 {}
 Setting:=GetFromList(AName);
 if Setting <> nil then
  begin
   Result:=Setting.Value;
  end
 else
  begin
   Result:='';
  end;  
end;

{==============================================================================}

function TNetworkSettings.FindFromEnvironment(const AName:String):String;
begin
 {}
 Result:=SysUtils.GetEnvironmentVariable(AName);
end;

{==============================================================================}

function TNetworkSettings.GetString(const AName:String):String;
begin
 {}
 Result:=GetStringDefault(AName,'');
end;

{==============================================================================}

function TNetworkSettings.GetStringDefault(const AName,ADefault:String):String;
var
 Value:String;
begin
 {}
 Result:=ADefault;
 
 if not ReaderLock then Exit;
 
 Value:=FindFromList(AName);
 if Length(Value) <> 0 then
  begin
   Result:=Value;
  end
 else
  begin 
   Result:=TranslateString(FindFromEnvironment(AName),ADefault);
  end;
 
 ReaderUnlock;
end;
   
{==============================================================================}

function TNetworkSettings.GetInteger(const AName:String):Integer;
begin
 {}
 Result:=GetIntegerDefault(AName,0);
end;

{==============================================================================}

function TNetworkSettings.GetIntegerDefault(const AName:String;ADefault:Integer):Integer;
var
 Value:String;
begin
 {}
 Result:=ADefault;
 
 if not ReaderLock then Exit;
 
 Value:=FindFromList(AName);
 if Length(Value) <> 0 then
  begin
   Result:=TranslateInteger(Value,ADefault);
  end
 else 
  begin
   Result:=TranslateInteger(FindFromEnvironment(AName),ADefault);
  end;
 
 ReaderUnlock;
end;

{==============================================================================}

function TNetworkSettings.GetBoolean(const AName:String):Boolean;
begin
 {}
 Result:=GetBooleanDefault(AName,False);
end;

{==============================================================================}

function TNetworkSettings.GetBooleanDefault(const AName:String;ADefault:Boolean):Boolean;
var
 Value:String;
begin
 {}
 Result:=ADefault;
 
 if not ReaderLock then Exit;
 
 Value:=FindFromList(AName);
 if Length(Value) <> 0 then
  begin
   Result:=TranslateBoolean(Value,ADefault);
  end
 else 
  begin
   Result:=TranslateBoolean(FindFromEnvironment(AName),ADefault);
  end;
 
 ReaderUnlock;
end;

{==============================================================================}

function TNetworkSettings.AddString(const AName,AValue:String):Boolean;
var 
 Setting:TNetworkSetting;
begin
 {}
 Result:=False;
 
 if Length(AName) = 0 then Exit;
 
 if not WriterLock then Exit;
 
 {Get Setting}
 Setting:=GetFromList(AName);
 if Setting = nil then
  begin
   {Add Setting}
   Setting:=TNetworkSetting.Create;
   Setting.Name:=Uppercase(AName);
   Setting.Value:=AValue;
   
   Result:=FList.Add(Setting);
  end
 else
  begin
   {Update Setting}
   Setting.Value:=AValue;
   
   Result:=True;
  end;  
 
 WriterUnlock;
end;

{==============================================================================}

function TNetworkSettings.AddInteger(const AName:String;AValue:Integer):Boolean;
begin
 {}
 Result:=AddString(AName,IntToStr(AValue));
end;

{==============================================================================}

function TNetworkSettings.AddBoolean(const AName:String;AValue:Boolean):Boolean;
begin
 {}
 if AValue then
  begin
   Result:=AddString(AName,'1');
  end
 else
  begin
   Result:=AddString(AName,'0');
  end;  
end;

{==============================================================================}

function TNetworkSettings.Remove(const AName:String):Boolean;
var 
 Setting:TNetworkSetting;
begin
 {}
 Result:=False;
 
 if Length(AName) = 0 then Exit;

 if not WriterLock then Exit;
 
 {Get Setting}
 Setting:=GetFromList(AName);
 if Setting <> nil then
  begin
   {Remove Setting}
   Result:=FList.Remove(Setting);
   
   {Destroy Setting}
   Setting.Free;
  end;
 
 WriterUnlock;
end;

{==============================================================================}

function TNetworkSettings.LoadFromFile(const AFileName:String;AFlat:Boolean):Boolean;
var
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  {Check Filename}
  if Length(AFileName) = 0 then Exit;
  
  {Check File}
  if FileExists(AFileName) then
   begin
    {Open File}
    FileStream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);
    try
     {Load from Stream}
     Result:=LoadFromStream(FileStream,AFlat);
    finally
     FileStream.Free;
    end;    
   end; 
 except
  {}
 end; 
end;

{==============================================================================}

function TNetworkSettings.LoadFromStream(AStream:TStream;AFlat:Boolean):Boolean;
var
 StringList:TStringList;
begin
 {}
 Result:=False;
 
 {Check Stream}
 if AStream = nil then Exit;
 
 {Create Strings}
 StringList:=TStringList.Create;
 try
  {Load Strings}
  StringList.LoadFromStream(AStream);
  
  {Load from Strings}
  Result:=LoadFromStrings(StringList,AFlat);
 finally
  StringList.Free;
 end; 
end;

{==============================================================================}

function TNetworkSettings.LoadFromStrings(AStrings:TStrings;AFlat:Boolean):Boolean;
var
 Count:Integer;
 Prefix:String;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Strings}
 if AStrings = nil then Exit;
 
 if not WriterLock then Exit;
 
 {Load Strings}
 Prefix:='';
 for Count:=0 to AStrings.Count - 1 do
  begin
   {Get String}
   WorkBuffer:=AStrings.Strings[Count];
   if (Length(WorkBuffer) <> 0) and (WorkBuffer[1] <> '#') then
    begin
     {Check Flat}
     if AFlat then
      begin
       {Flat file}
       AddString(ExtractName(WorkBuffer,' '),ExtractValue(WorkBuffer,' '));
      end
     else
      begin
       {INI file}
       if WorkBuffer[1] = '[' then
        begin
         Prefix:=ExtractPrefix(WorkBuffer);
        end
       else
        begin
         AddString(MergePrefix(Prefix,ExtractName(WorkBuffer,'=')),ExtractValue(WorkBuffer,' '));
        end;
      end;
    end;
  end; 
 
 WriterUnlock;
end;

{==============================================================================}

function TNetworkSettings.SaveToFile(const AFileName:String;AFlat:Boolean):Boolean;
var
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  {Check Filename}
  if Length(AFileName) = 0 then Exit;
  
  {Check File}
  if FileExists(AFileName) then
   begin
    {Delete File}
    SysUtils.DeleteFile(AFileName);
   end;
  
  {Check File}
  if not FileExists(AFileName) then
   begin  
    {Create File}
    FileStream:=TFileStream.Create(AFileName,fmCreate);
    try
     {Save to Stream}
     Result:=SaveToStream(FileStream,AFlat);
    finally
     FileStream.Free;
    end;    
   end; 
 except
  {}
 end; 
end;

{==============================================================================}

function TNetworkSettings.SaveToStream(AStream:TStream;AFlat:Boolean):Boolean;
var
 StringList:TStringList;
begin
 {}
 Result:=False;
 
 {Check Stream}
 if AStream = nil then Exit;
 
 {Create Strings}
 StringList:=TStringList.Create;
 try
  {Save to Strings}
  Result:=SaveToStrings(StringList,AFlat);
  
  {Save Strings}
  StringList.SaveToStream(AStream);
 finally
  StringList.Free;
 end; 
end;

{==============================================================================}

function TNetworkSettings.SaveToStrings(AStrings:TStrings;AFlat:Boolean):Boolean;
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Strings}
 if AStrings = nil then Exit;
 
 if not WriterLock then Exit;
 
 //To Do //Continuing
 
 WriterUnlock;
end;

{==============================================================================}

function TNetworkSettings.LoadFromEnvironment:Boolean;
begin
 {}
 Result:=False;
 
 {Host configuration}
 {HOST_NAME}
 HOST_NAME:=TranslateString(FindFromEnvironment('HOST_NAME'),HOST_NAME);
 AddString('HOST_NAME',HOST_NAME);
 
 {HOST_DOMAIN}
 HOST_DOMAIN:=TranslateString(FindFromEnvironment('HOST_DOMAIN'),HOST_DOMAIN);
 AddString('HOST_DOMAIN',HOST_DOMAIN);

 {Winsock configuration} 
 {WINSOCK_AUTOSTART}
 WINSOCK_AUTOSTART:=TranslateBoolean(FindFromEnvironment('WINSOCK_AUTOSTART'),WINSOCK_AUTOSTART);
 {WINSOCK_ASYNCSTART}
 WINSOCK_ASYNCSTART:=TranslateBoolean(FindFromEnvironment('WINSOCK_ASYNCSTART'),WINSOCK_ASYNCSTART);
 
 {Winsock2 configuration} 
 {WINSOCK2_AUTOSTART}
 WINSOCK2_AUTOSTART:=TranslateBoolean(FindFromEnvironment('WINSOCK2_AUTOSTART'),WINSOCK2_AUTOSTART);
 {WINSOCK2_ASYNCSTART}
 WINSOCK2_ASYNCSTART:=TranslateBoolean(FindFromEnvironment('WINSOCK2_ASYNCSTART'),WINSOCK2_ASYNCSTART);
 
 {Sockets configuration}
 {SOCKETS_AUTOSTART}
 SOCKETS_AUTOSTART:=TranslateBoolean(FindFromEnvironment('SOCKETS_AUTOSTART'),SOCKETS_AUTOSTART);
 {SOCKETS_ASYNCSTART}
 SOCKETS_ASYNCSTART:=TranslateBoolean(FindFromEnvironment('SOCKETS_ASYNCSTART'),SOCKETS_ASYNCSTART);
 
 {Client configuration}
 {DNS_CLIENT_ENABLED}
 DNS_CLIENT_ENABLED:=TranslateBoolean(FindFromEnvironment('DNS_CLIENT_ENABLED'),DNS_CLIENT_ENABLED);
 AddBoolean('DNS_CLIENT_ENABLED',DNS_CLIENT_ENABLED);
 
 {Protocol configuration}
 {RAW_PROTOCOL_ENABLED}
 RAW_PROTOCOL_ENABLED:=TranslateBoolean(FindFromEnvironment('RAW_PROTOCOL_ENABLED'),RAW_PROTOCOL_ENABLED);
 AddBoolean('RAW_PROTOCOL_ENABLED',RAW_PROTOCOL_ENABLED);
 
 {UDP_PROTOCOL_ENABLED}
 UDP_PROTOCOL_ENABLED:=TranslateBoolean(FindFromEnvironment('UDP_PROTOCOL_ENABLED'),UDP_PROTOCOL_ENABLED);
 AddBoolean('UDP_PROTOCOL_ENABLED',UDP_PROTOCOL_ENABLED);

 {TCP_PROTOCOL_ENABLED}
 TCP_PROTOCOL_ENABLED:=TranslateBoolean(FindFromEnvironment('TCP_PROTOCOL_ENABLED'),TCP_PROTOCOL_ENABLED);
 AddBoolean('TCP_PROTOCOL_ENABLED',TCP_PROTOCOL_ENABLED);

 {ICMP_PROTOCOL_ENABLED}
 ICMP_PROTOCOL_ENABLED:=TranslateBoolean(FindFromEnvironment('ICMP_PROTOCOL_ENABLED'),ICMP_PROTOCOL_ENABLED);
 AddBoolean('ICMP_PROTOCOL_ENABLED',ICMP_PROTOCOL_ENABLED);

 {ICMP6_PROTOCOL_ENABLED}
 ICMP6_PROTOCOL_ENABLED:=TranslateBoolean(FindFromEnvironment('ICMP6_PROTOCOL_ENABLED'),ICMP6_PROTOCOL_ENABLED);
 AddBoolean('ICMP6_PROTOCOL_ENABLED',ICMP6_PROTOCOL_ENABLED);

 {IGMP_PROTOCOL_ENABLED}
 IGMP_PROTOCOL_ENABLED:=TranslateBoolean(FindFromEnvironment('IGMP_PROTOCOL_ENABLED'),IGMP_PROTOCOL_ENABLED);
 AddBoolean('IGMP_PROTOCOL_ENABLED',IGMP_PROTOCOL_ENABLED);

 {ARP_CONFIG_ENABLED}
 ARP_CONFIG_ENABLED:=TranslateBoolean(FindFromEnvironment('ARP_CONFIG_ENABLED'),ARP_CONFIG_ENABLED);
 AddBoolean('ARP_CONFIG_ENABLED',ARP_CONFIG_ENABLED);

 {RARP_CONFIG_ENABLED}
 RARP_CONFIG_ENABLED:=TranslateBoolean(FindFromEnvironment('RARP_CONFIG_ENABLED'),RARP_CONFIG_ENABLED);
 AddBoolean('RARP_CONFIG_ENABLED',RARP_CONFIG_ENABLED);

 {BOOTP_CONFIG_ENABLED}
 BOOTP_CONFIG_ENABLED:=TranslateBoolean(FindFromEnvironment('BOOTP_CONFIG_ENABLED'),BOOTP_CONFIG_ENABLED);
 AddBoolean('BOOTP_CONFIG_ENABLED',BOOTP_CONFIG_ENABLED);

 {DHCP_CONFIG_ENABLED}
 DHCP_CONFIG_ENABLED:=TranslateBoolean(FindFromEnvironment('DHCP_CONFIG_ENABLED'),DHCP_CONFIG_ENABLED);
 AddBoolean('DHCP_CONFIG_ENABLED',DHCP_CONFIG_ENABLED);

 {STATIC_CONFIG_ENABLED}
 STATIC_CONFIG_ENABLED:=TranslateBoolean(FindFromEnvironment('STATIC_CONFIG_ENABLED'),STATIC_CONFIG_ENABLED);
 AddBoolean('STATIC_CONFIG_ENABLED',STATIC_CONFIG_ENABLED);

 {LOOPBACK_CONFIG_ENABLED}
 LOOPBACK_CONFIG_ENABLED:=TranslateBoolean(FindFromEnvironment('LOOPBACK_CONFIG_ENABLED'),LOOPBACK_CONFIG_ENABLED);
 AddBoolean('LOOPBACK_CONFIG_ENABLED',LOOPBACK_CONFIG_ENABLED);
 
 {Transport configuration}
 {IP_TRANSPORT_ENABLED}
 IP_TRANSPORT_ENABLED:=TranslateBoolean(FindFromEnvironment('IP_TRANSPORT_ENABLED'),IP_TRANSPORT_ENABLED);
 AddBoolean('IP_TRANSPORT_ENABLED',IP_TRANSPORT_ENABLED);

 {IP6_TRANSPORT_ENABLED}
 IP6_TRANSPORT_ENABLED:=TranslateBoolean(FindFromEnvironment('IP6_TRANSPORT_ENABLED'),IP6_TRANSPORT_ENABLED);
 AddBoolean('IP6_TRANSPORT_ENABLED',IP6_TRANSPORT_ENABLED);

 {ARP_TRANSPORT_ENABLED}
 ARP_TRANSPORT_ENABLED:=TranslateBoolean(FindFromEnvironment('ARP_TRANSPORT_ENABLED'),ARP_TRANSPORT_ENABLED);
 AddBoolean('ARP_TRANSPORT_ENABLED',ARP_TRANSPORT_ENABLED);

 {RARP_TRANSPORT_ENABLED}
 RARP_TRANSPORT_ENABLED:=TranslateBoolean(FindFromEnvironment('RARP_TRANSPORT_ENABLED'),RARP_TRANSPORT_ENABLED);
 AddBoolean('RARP_TRANSPORT_ENABLED',RARP_TRANSPORT_ENABLED);

 {RSN_TRANSPORT_ENABLED}
 RSN_TRANSPORT_ENABLED:=TranslateBoolean(FindFromEnvironment('RSN_TRANSPORT_ENABLED'),RSN_TRANSPORT_ENABLED);
 AddBoolean('RSN_TRANSPORT_ENABLED',RSN_TRANSPORT_ENABLED);

 {EAPOL_TRANSPORT_ENABLED}
 EAPOL_TRANSPORT_ENABLED:=TranslateBoolean(FindFromEnvironment('EAPOL_TRANSPORT_ENABLED'),EAPOL_TRANSPORT_ENABLED);
 AddBoolean('EAPOL_TRANSPORT_ENABLED',EAPOL_TRANSPORT_ENABLED);
 
 {Network configuration}
 {WIRED_NETWORK_ENABLED}
 WIRED_NETWORK_ENABLED:=TranslateBoolean(FindFromEnvironment('WIRED_NETWORK_ENABLED'),WIRED_NETWORK_ENABLED);
 AddBoolean('WIRED_NETWORK_ENABLED',WIRED_NETWORK_ENABLED);

 {LOOPBACK_NETWORK_ENABLED}
 LOOPBACK_NETWORK_ENABLED:=TranslateBoolean(FindFromEnvironment('LOOPBACK_NETWORK_ENABLED'),LOOPBACK_NETWORK_ENABLED);
 AddBoolean('LOOPBACK_NETWORK_ENABLED',LOOPBACK_NETWORK_ENABLED);

 {WIRELESS_NETWORK_ENABLED}
 WIRELESS_NETWORK_ENABLED:=TranslateBoolean(FindFromEnvironment('WIRELESS_NETWORK_ENABLED'),WIRELESS_NETWORK_ENABLED);
 AddBoolean('WIRELESS_NETWORK_ENABLED',WIRELESS_NETWORK_ENABLED);
 
 {TCP configuration}
 {TCP_RECEIVE_BACKLOG}
 TCP_RECEIVE_BACKLOG:=TranslateInteger(FindFromEnvironment('TCP_RECEIVE_BACKLOG'),TCP_RECEIVE_BACKLOG);
 AddInteger('TCP_RECEIVE_BACKLOG',TCP_RECEIVE_BACKLOG);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TServiceSet}
constructor TServiceSet.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FLocalLock:=MutexCreate;
 
 FServiceSetType:=SERVICE_SET_UNKNOWN;
 FServiceSetTime:=GetTickCount64;
 FillChar(FAddress,SizeOf(THardwareAddress),0);
 FAdapter:=nil;
end;

{==============================================================================}

destructor TServiceSet.Destroy; 
begin
 {}
 WriterLock;
 try
  MutexDestroy(FLocalLock);
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TServiceSet.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TServiceSet.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);

 ReleaseLock;
end;

{==============================================================================}

procedure TServiceSet.SetServiceSetType(AServiceSetType:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FServiceSetType:=AServiceSetType;

 ReleaseLock;
end;

{==============================================================================}

procedure TServiceSet.SetServiceSetTime(const AServiceSetTime:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FServiceSetTime:=AServiceSetTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TServiceSet.SetAddress(const AAddress:THardwareAddress);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}

procedure TServiceSet.SetAdapter(AAdapter:TNetworkAdapter);
begin
 {}
 if not AcquireLock then Exit;

 FAdapter:=AAdapter;

 ReleaseLock;
end;

{==============================================================================}

function TServiceSet.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServiceSet.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServiceSet.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServiceSet.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServiceSet.ReaderConvert:Boolean;
{Convert a Reader lock to a Writer lock}
begin
 {}
 Result:=(SynchronizerReaderConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServiceSet.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServiceSet.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServiceSet.WriterConvert:Boolean;
{Convert a Writer lock to a Reader lock}
begin
 {}
 Result:=(SynchronizerWriterConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure NetworkInit;
var 
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if NetworkInitialized then Exit;
 
 {Initialize Logging}
 NETWORK_LOG_ENABLED:=(NETWORK_DEFAULT_LOG_LEVEL <> NETWORK_LOG_LEVEL_NONE); 
 
 {Initialize Network Table}
 NetworkTable:=nil;
 NetworkTableLock:=CriticalSectionCreate; 
 NetworkTableCount:=0;
 if NetworkTableLock = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to create network table lock');
  end;

 {Initialize Event Table}
 NetworkEventTable:=nil;
 NetworkEventLock:=CriticalSectionCreate; 
 NetworkEventCount:=0;
 if NetworkEventLock = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to create event table lock');
  end;
 
 {Create Network Settings}
 NetworkSettings:=TNetworkSettings.Create;
 
 {Create Adapter Manager}
 AdapterManager:=TAdapterManager.Create(NetworkSettings);
 
 {Check Environment Variables (Network)}
  {Done by NetworkSettings.LoadFromEnvironment}
 
 {Load Environment Settings}
 NetworkSettings.LoadFromEnvironment;
 
 {Register Platform Network Handlers}
 HostGetNameHandler:=SysHostGetName;
 HostSetNameHandler:=SysHostSetName;
 HostGetDomainHandler:=SysHostGetDomain;
 HostSetDomainHandler:=SysHostSetDomain;
 
 NetworkInitialized:=True;
end;

{==============================================================================}

function NetworkStart:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if NetworkStarted then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if AdapterManager = nil then Exit;
 
 {Notify Event}
 NetworkEventNotify(NETWORK_EVENT_SYSTEM_START);
 
 {Start Adapters}
 if not AdapterManager.StartAdapters then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to start one or more network adapters');
  end;
 
 {Set Started} 
 NetworkStarted:=True;
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function NetworkStop:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if not(NetworkStarted) then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if AdapterManager = nil then Exit;
 
 {Stop Adapters}
 if not AdapterManager.StopAdapters then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to stop one or more network adapters');
  end;

 {Notify Event}
 NetworkEventNotify(NETWORK_EVENT_SYSTEM_STOP);
  
 {Set Started}
 NetworkStarted:=False;    
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function NetworkStartCompleted:Boolean;
{Returns True if the network sub system has been started}
begin
 {}
 Result:=NetworkStarted;
end;

{==============================================================================}
{==============================================================================}
{Network Functions}
function NetworkDeviceOpen(Network:PNetworkDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if not Assigned(Network.DeviceOpen) then Exit;
 
 {Call Open}
 Result:=Network.DeviceOpen(Network);
end;
 
{==============================================================================}

function NetworkDeviceClose(Network:PNetworkDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if not Assigned(Network.DeviceClose) then Exit;

 {Call Close}
 Result:=Network.DeviceClose(Network);
end;

{==============================================================================}

function NetworkDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
var
 Entry:PNetworkEntry;
 Packet:PNetworkPacket;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Length}
 Length:=0;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if Assigned(Network.DeviceRead) then
  begin
   {Call Read}
   Result:=Network.DeviceRead(Network,Buffer,Size,Length);
  end
 else
  begin
   {Default Method}
   {Check Buffer}
   if Buffer = nil then Exit;
   
   {Check Size}
   if Size = 0 then Exit;
   
   {Check State}
   Result:=ERROR_NOT_READY;
   if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
   
   {Receive Buffer}
   Result:=NetworkBufferReceive(Network,Entry);
   if Result <> ERROR_SUCCESS then Exit;
   
   {Get First Packet}
   Packet:=@Entry.Packets[0];
   
   {Get Size}
   if Packet.Length < Size then
    begin
     Size:=Packet.Length;
    end;
   
   {Copy Data}
   System.Move(Packet.Data^,Buffer^,Size);
   
   {Release Buffer}
   Result:=NetworkBufferRelease(Network,Entry);
   if Result = ERROR_SUCCESS then
    begin
     {Return Length}
     Length:=Size;
    end;
  end;
end;

{==============================================================================}

function NetworkDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
var
 Entry:PNetworkEntry;
 Packet:PNetworkPacket;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Length}
 Length:=0;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if Assigned(Network.DeviceWrite) then
  begin
   {Call Write}
   Result:=Network.DeviceWrite(Network,Buffer,Size,Length);
  end
 else
  begin
   {Default Method}
   {Check Buffer}
   if Buffer = nil then Exit;
   
   {Check Size}
   if Size = 0 then Exit;
   
   {Check State}
   Result:=ERROR_NOT_READY;
   if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
   
   {Allocate Buffer}
   Result:=NetworkBufferAllocate(Network,Entry);
   if Result <> ERROR_SUCCESS then Exit;
   
   {Get First Packet}
   Packet:=@Entry.Packets[0];

   {Get Size}
   if Packet.Length < Size then
    begin
     Size:=Packet.Length;
    end;
   
   {Update Packet}
   Packet.Length:=Size;
   
   {Copy Data}
   System.Move(Buffer^,Packet.Data^,Size);
   
   {Transmit Buffer}
   Result:=NetworkBufferTransmit(Network,Entry);
   if Result = ERROR_SUCCESS then
    begin
     {Return Length}
     Length:=Size;
    end;
  end;
end;

{==============================================================================}

function NetworkDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 Result:=ERROR_NOT_SUPPORTED;
 if not Assigned(Network.DeviceControl) then Exit;

 {Call Control}
 Result:=Network.DeviceControl(Network,Request,Argument1,Argument2);
end;

{==============================================================================}

function NetworkBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Allocate a transmit buffer from the specified network device, the returned entry will
 include a buffer for writing data to as well as an offfset value to allow the driver
 data to be written to the start of the buffer.
 
 When the data has been copied to the buffer, pass the entry to NetworkBufferTransmit}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 Result:=ERROR_NOT_SUPPORTED;
 if not Assigned(Network.BufferAllocate) then Exit;

 {Call Allocate}
 Result:=Network.BufferAllocate(Network,Entry);
end;
 
{==============================================================================}

function NetworkBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Release a receive buffer to the specified network device, the entry must have been
 returned from NetworkBufferReceive}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 Result:=ERROR_NOT_SUPPORTED;
 if not Assigned(Network.BufferRelease) then Exit;

 {Call Release}
 Result:=Network.BufferRelease(Network,Entry);
end;

{==============================================================================}

function NetworkBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Receive a completed receive buffer from the specified network device. The returned
 entry will contain a one or more packets of data to read from.
 
 When the data has been processed pass the returned buffer to NetworkBufferRelease}
 
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 Result:=ERROR_NOT_SUPPORTED;
 if not Assigned(Network.BufferReceive) then Exit;

 {Call Release}
 Result:=Network.BufferReceive(Network,Entry);
end;

{==============================================================================}

function NetworkBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Transmit a completed transmit buffer to the specified network device. The entry
 must have been allocated using NetworkBufferAllocate}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 Result:=ERROR_NOT_SUPPORTED;
 if not Assigned(Network.BufferTransmit) then Exit;

 {Call Release}
 Result:=Network.BufferTransmit(Network,Entry);
end;
 
{==============================================================================}

function NetworkDeviceSetState(Network:PNetworkDevice;State:LongWord):LongWord;
{Set the state of the specified network and send a notification}
{Network: The network to set the state for}
{State: The new state to set and notify}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > NETWORK_STATE_OPEN then Exit;
 
 {Check State}
 if Network.NetworkState = State then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Network.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set State}
      Network.NetworkState:=State;
  
      {Notify State}
      NotifierNotify(@Network.Device,NetworkDeviceStateToNotification(State));

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
end;

{==============================================================================}

function NetworkDeviceSetStatus(Network:PNetworkDevice;Status:LongWord):LongWord;
{Set the status of the specified network and send a notification}
{Network: The network to set the status for}
{Status: The new status to set and notify}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Status}
 if Status > NETWORK_STATUS_UP then Exit;
 
 {Check Status}
 if Network.NetworkStatus = Status then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Network.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set Status}
      Network.NetworkStatus:=Status;
  
      {Notify Status}
      NotifierNotify(@Network.Device,NetworkDeviceStatusToNotification(Status));

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
end;

{==============================================================================}

function NetworkDeviceCreate:PNetworkDevice;
{Create a new Network entry}
{Return: Pointer to new Network entry or nil if network could not be created}
begin
 {}
 Result:=NetworkDeviceCreateEx(SizeOf(TNetworkDevice));
end;

{==============================================================================}

function NetworkDeviceCreateEx(Size:LongWord):PNetworkDevice;
{Create a new Network entry}
{Size: Size in bytes to allocate for new network (Including the network entry)}
{Return: Pointer to new Network entry or nil if network could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TNetworkDevice) then Exit;
 
 {Create Network}
 Result:=PNetworkDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=NETWORK_TYPE_NONE;
 Result.Device.DeviceFlags:=NETWORK_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Network}
 Result.NetworkId:=DEVICE_ID_ANY;
 Result.NetworkState:=NETWORK_STATE_CLOSED;
 Result.NetworkStatus:=NETWORK_STATUS_DOWN;
 Result.DeviceOpen:=nil;
 Result.DeviceClose:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceControl:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Buffer.Wait:=INVALID_HANDLE_VALUE;
 Result.TransmitWait:=INVALID_HANDLE_VALUE;
 Result.ReceiveBuffer:=INVALID_HANDLE_VALUE;
 Result.TransmitBuffer:=INVALID_HANDLE_VALUE;
 Result.ReceiveQueue.Buffer:=INVALID_HANDLE_VALUE;
 Result.ReceiveQueue.Wait:=INVALID_HANDLE_VALUE;
 Result.TransmitQueue.Buffer:=INVALID_HANDLE_VALUE;
 Result.TransmitQueue.Wait:=INVALID_HANDLE_VALUE;
 
 {Create Lock} 
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to create lock for network');
   NetworkDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
 
 {Create Buffer Semaphore}
 Result.Buffer.Wait:=SemaphoreCreate(0);
 if Result.Buffer.Wait = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to create buffer semaphore for network');
   NetworkDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function NetworkDeviceDestroy(Network:PNetworkDevice):LongWord;
{Destroy an existing Network entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Network}
 Result:=ERROR_IN_USE;
 if NetworkDeviceCheck(Network) = Network then Exit;

 {Check State}
 if Network.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Buffer Semaphore} 
 if Network.Buffer.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(Network.Buffer.Wait);
  end;
  
 {Destroy Lock} 
 if Network.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Network.Lock);
  end;
 
 {Destroy Network} 
 Result:=DeviceDestroy(@Network.Device);
end;

{==============================================================================}

function NetworkDeviceRegister(Network:PNetworkDevice):LongWord;
{Register a new Network in the Network table}
var
 NetworkId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.NetworkId <> DEVICE_ID_ANY then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Functions}
 if not Assigned(Network.DeviceOpen) then Exit;
 if not Assigned(Network.DeviceClose) then Exit;
 if not Assigned(Network.DeviceRead) then
  begin
   if not Assigned(Network.BufferReceive) then Exit;
   if not Assigned(Network.BufferRelease) then Exit;
  end;
 if not Assigned(Network.DeviceWrite) then
  begin
   if not Assigned(Network.BufferAllocate) then Exit;
   if not Assigned(Network.BufferTransmit) then Exit;
  end;
  
 {Check Network}
 Result:=ERROR_ALREADY_EXISTS;
 if NetworkDeviceCheck(Network) = Network then Exit;
 
 {Check State}
 if Network.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Network}
 if CriticalSectionLock(NetworkTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Network}
    NetworkId:=0;
    while NetworkDeviceFind(NetworkId) <> nil do
     begin
      Inc(NetworkId);
     end;
    Network.NetworkId:=NetworkId;
    
    {Update Device}
    Network.Device.DeviceName:=NETWORK_NAME_PREFIX + IntToStr(Network.NetworkId);
    Network.Device.DeviceClass:=DEVICE_CLASS_NETWORK;
    
    {Register Device}
    Result:=DeviceRegister(@Network.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Network.NetworkId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Network}
    if NetworkTable = nil then
     begin
      NetworkTable:=Network;
     end
    else
     begin
      Network.Next:=NetworkTable;
      NetworkTable.Prev:=Network;
      NetworkTable:=Network;
     end;
 
    {Increment Count}
    Inc(NetworkTableCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(NetworkTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function NetworkDeviceDeregister(Network:PNetworkDevice):LongWord;
{Deregister a Network from the Network table}
var
 Prev:PNetworkDevice;
 Next:PNetworkDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.NetworkId = DEVICE_ID_ANY then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Network}
 Result:=ERROR_NOT_FOUND;
 if NetworkDeviceCheck(Network) <> Network then Exit;
 
 {Check State}
 if Network.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Network}
 if CriticalSectionLock(NetworkTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Network.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Network}
    Prev:=Network.Prev;
    Next:=Network.Next;
    if Prev = nil then
     begin
      NetworkTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
 
    {Decrement Count}
    Dec(NetworkTableCount);
 
    {Update Network}
    Network.NetworkId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(NetworkTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function NetworkDeviceFind(NetworkId:LongWord):PNetworkDevice;
var
 Network:PNetworkDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if NetworkId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(NetworkTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Network}
    Network:=NetworkTable;
    while Network <> nil do
     begin
      {Check State}
      if Network.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Network.NetworkId = NetworkId then
         begin
          Result:=Network;
          Exit;
         end;
       end;
       
      {Get Next}
      Network:=Network.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(NetworkTableLock);
   end;
  end;
end;
       
{==============================================================================}
       
function NetworkDeviceFindByName(const Name:String):PNetworkDevice; inline;
begin
 {}
 Result:=PNetworkDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function NetworkDeviceFindByDescription(const Description:String):PNetworkDevice; inline;
begin
 {}
 Result:=PNetworkDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function NetworkDeviceEnumerate(Callback:TNetworkEnumerate;Data:Pointer):LongWord;
var
 Network:PNetworkDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(NetworkTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Network}
    Network:=NetworkTable;
    while Network <> nil do
     begin
      {Check State}
      if Network.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Network,Data) <> ERROR_SUCCESS then Exit;
       end;
      
      {Get Next}
      Network:=Network.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(NetworkTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function NetworkDeviceNotification(Network:PNetworkDevice;Callback:TNetworkNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_NETWORK,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Network}
   if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Network.Device,DEVICE_CLASS_NETWORK,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}

function NetworkEventAllocate(Callback:TNetworkEventCallback;Data:Pointer;Event:LongWord):PNetworkEvent;
{Create and Register a new Event entry in the Event table}
var
 NetworkEvent:PNetworkEvent;
begin
 {}
 Result:=nil;

 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Create Event}
 NetworkEvent:=AllocMem(SizeOf(TNetworkEvent));
 if NetworkEvent = nil then Exit;

 {Update Event}
 NetworkEvent.Signature:=NETWORK_EVENT_SIGNATURE;
 NetworkEvent.EventState:=NETWORK_EVENT_STATE_UNREGISTERED;
 NetworkEvent.EventFlags:=NETWORK_EVENT_FLAG_NONE;
 NetworkEvent.Callback:=Callback;
 NetworkEvent.Data:=Data;
 NetworkEvent.Event:=Event;
 
 {Insert Event}
 if CriticalSectionLock(NetworkEventLock) = ERROR_SUCCESS then
  begin
   try
    {Link Event}
    if NetworkEventTable = nil then
     begin
      NetworkEventTable:=NetworkEvent;
     end
    else
     begin
      NetworkEvent.Next:=NetworkEventTable;
      NetworkEventTable.Prev:=NetworkEvent;
      NetworkEventTable:=NetworkEvent;
     end;
    
    {Increment Count}
    Inc(NetworkEventCount);

    {Return Result}
    Result:=NetworkEvent;
   finally
    CriticalSectionUnlock(NetworkEventLock);
   end;
  end;
end;
    
{==============================================================================}

function NetworkEventRelease(Event:PNetworkEvent):LongWord;
{Deregister and Destroy a Event from the Event table}
var
 Prev:PNetworkEvent;
 Next:PNetworkEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Event}
 if Event = nil then Exit;
 if Event.Signature <> NETWORK_EVENT_SIGNATURE then Exit;
 
 {Check Event}
 if NetworkEventCheck(Event) <> Event then Exit;

 {Check State}
 if Event.EventState <> NETWORK_EVENT_STATE_UNREGISTERED then Exit;
 
 {Remove Event}
 if CriticalSectionLock(NetworkEventLock) = ERROR_SUCCESS then
  begin
   try
    {Unlink Event}
    Prev:=Event.Prev;
    Next:=Event.Next;
    if Prev = nil then
     begin
      NetworkEventTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
    
    {Decrement Count}
    Dec(NetworkEventCount);

    {Invalidate Event}
    Event.Signature:=0;
 
    {Free Event}
    FreeMem(Event);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(NetworkEventLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function NetworkEventRegister(Callback:TNetworkEventCallback;Data:Pointer;Event:LongWord):THandle;
{Register a callback for one or more network events}
var
 NetworkEvent:PNetworkEvent;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Check Event}
 if Event = NETWORK_EVENT_NONE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(NetworkEventLock) = ERROR_SUCCESS then
  begin
   try
    {Allocate Event}
    NetworkEvent:=NetworkEventAllocate(Callback,Data,Event);
    if NetworkEvent = nil then Exit;
    
    {Register Event}
    NetworkEvent.EventState:=NETWORK_EVENT_STATE_REGISTERED;
 
    {Return Result}
    Result:=THandle(NetworkEvent);
   finally
    {Release the Lock}
    CriticalSectionUnlock(NetworkEventLock);
   end;
  end;  
end;

{==============================================================================}

function NetworkEventDeregister(Handle:THandle):LongWord;
{Deregister a network event callback}
var
 NetworkEvent:PNetworkEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Check Event}
 NetworkEvent:=PNetworkEvent(Handle);
 if NetworkEvent = nil then Exit;
 if NetworkEvent.Signature <> NETWORK_EVENT_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(NetworkEventLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Event}
    NetworkEvent.EventState:=NETWORK_EVENT_STATE_UNREGISTERED;
   
    {Release Event}
    Result:=NetworkEventRelease(NetworkEvent);
   finally
    {Release the Lock}
    CriticalSectionUnlock(NetworkEventLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;
{==============================================================================}

function NetworkEventNotify(Event:LongWord):LongWord;
var
 Status:LongWord;
 NetworkEvent:PNetworkEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Event}
 if Event = NETWORK_EVENT_NONE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(NetworkEventLock) = ERROR_SUCCESS then
  begin
   try
    Result:=ERROR_SUCCESS;
 
    {Get Event}
    NetworkEvent:=NetworkEventTable;
    while NetworkEvent <> nil do
     begin
      {Check State}
      if NetworkEvent.EventState = NETWORK_EVENT_STATE_REGISTERED then
       begin
        {Check Event}
        if (NetworkEvent.Event and Event) <> 0 then
         begin
          Status:=NetworkEvent.Callback(NetworkEvent.Data,Event);
          if Status <> ERROR_SUCCESS then Result:=Status;
         end;
       end;
       
      {Get Next}
      NetworkEvent:=NetworkEvent.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(NetworkEventLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}
{==============================================================================}
{RTL Network Functions}
function SysHostGetName:String;
begin
 {}
 Result:='';
 
 {Check Settings}
 if NetworkSettings = nil then Exit;
 
 Result:=NetworkSettings.HostName;
end;

{==============================================================================}

function SysHostSetName(const AName:String):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Name}
 if Length(AName) = 0 then Exit;
 
 {Check Settings}
 if NetworkSettings = nil then Exit;
 
 NetworkSettings.HostName:=AName;
 
 Result:=True;
end;

{==============================================================================}

function SysHostGetDomain:String;
begin
 {}
 Result:='';
 
 {Check Settings}
 if NetworkSettings = nil then Exit;
 
 Result:=NetworkSettings.DomainName;
end;

{==============================================================================}

function SysHostSetDomain(const ADomain:String):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Domain}
 if Length(ADomain) = 0 then Exit;
 
 {Check Settings}
 if NetworkSettings = nil then Exit;
 
 NetworkSettings.DomainName:=ADomain;
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{Network Helper Functions}
function NetworkGetCount:LongWord; inline;
{Get the current network count}
begin
 {}
 Result:=NetworkTableCount;
end;

{==============================================================================}

function NetworkDeviceCheck(Network:PNetworkDevice):PNetworkDevice;
{Check if the supplied Network is in the network table}
var
 Current:PNetworkDevice;
begin
 {}
 Result:=nil;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(NetworkTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Network}
    Current:=NetworkTable;
    while Current <> nil do
     begin
      {Check Network}
      if Current = Network then
       begin
        Result:=Network;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(NetworkTableLock);
   end;
  end;
end;

{==============================================================================}

function NetworkDeviceTypeToString(NetworkType:LongWord):String;
begin
 {}
 Result:='NETWORK_TYPE_UNKNOWN';
 
 if NetworkType <= NETWORK_TYPE_MAX then
  begin
   Result:=NETWORK_TYPE_NAMES[NetworkType];
  end;
end;

{==============================================================================}

function NetworkDeviceStateToString(NetworkState:LongWord):String;
begin
 {}
 Result:='NETWORK_STATE_UNKNOWN';
 
 if NetworkState <= NETWORK_STATE_MAX then
  begin
   Result:=NETWORK_STATE_NAMES[NetworkState];
  end;
end;

{==============================================================================}

function NetworkDeviceStatusToString(NetworkStatus:LongWord):String;
begin
 {}
 Result:='NETWORK_STATUS_UNKNOWN';
 
 if NetworkStatus <= NETWORK_STATUS_MAX then
  begin
   Result:=NETWORK_STATUS_NAMES[NetworkStatus];
  end;
end;

{==============================================================================}

function NetworkDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Network state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  NETWORK_STATE_CLOSED:Result:=DEVICE_NOTIFICATION_CLOSE;
  NETWORK_STATE_CLOSING:Result:=DEVICE_NOTIFICATION_CLOSING;
  NETWORK_STATE_OPENING:Result:=DEVICE_NOTIFICATION_OPENING;
  NETWORK_STATE_OPEN:Result:=DEVICE_NOTIFICATION_OPEN;
 end;
end;

{==============================================================================}

function NetworkDeviceStatusToNotification(Status:LongWord):LongWord;
{Convert a Network status value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check Status}
 case Status of
  NETWORK_STATUS_DOWN:Result:=DEVICE_NOTIFICATION_DOWN;
  NETWORK_STATUS_UP:Result:=DEVICE_NOTIFICATION_UP;
 end;
end;

{==============================================================================}

function NetworkEventCheck(Event:PNetworkEvent):PNetworkEvent;
{Check if the supplied Event is in the event table}
var
 Current:PNetworkEvent;
begin
 {}
 Result:=nil;
 
 {Check Event}
 if Event = nil then Exit;
 if Event.Signature <> NETWORK_EVENT_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(NetworkEventLock) = ERROR_SUCCESS then
  begin
   try
    {Get Event}
    Current:=NetworkEventTable;
    while Current <> nil do
     begin
      {Check Event}
      if Current = Event then
       begin
        Result:=Event;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(NetworkEventLock);
   end;
  end;
end;
 
{==============================================================================}

procedure NetworkLog(Level:LongWord;Network:PNetworkDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < NETWORK_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = NETWORK_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = NETWORK_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Network: ';
 
 {Check Network}
 if Network <> nil then
  begin
   WorkBuffer:=WorkBuffer + NETWORK_NAME_PREFIX + IntToStr(Network.NetworkId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_NETWORK,LogLevelToLoggingSeverity(Level),'Network',WorkBuffer + AText);
end;

{==============================================================================}

procedure NetworkLogInfo(Network:PNetworkDevice;const AText:String);
begin
 {}
 NetworkLog(NETWORK_LOG_LEVEL_INFO,Network,AText);
end;

{==============================================================================}

procedure NetworkLogError(Network:PNetworkDevice;const AText:String);
begin
 {}
 NetworkLog(NETWORK_LOG_LEVEL_ERROR,Network,AText);
end;

{==============================================================================}

procedure NetworkLogDebug(Network:PNetworkDevice;const AText:String);
begin
 {}
 NetworkLog(NETWORK_LOG_LEVEL_DEBUG,Network,AText);
end;

{==============================================================================}

function HardwareAddressToString(const AAddress:THardwareAddress):String;
var
 Count:Integer;
begin
 {}
 Result:='';
 for Count:=0 to HARDWARE_ADDRESS_SIZE - 1 do
  begin
   Result:=Result + IntToHex(AAddress[Count],2);
  end;
end;

{==============================================================================}

function StringToHardwareAddress(const AAddress:String):THardwareAddress;
var
 Count:Integer;
begin
 {}
 FillChar(Result,SizeOf(THardwareAddress),0);
 if Length(AAddress) = (2 * HARDWARE_ADDRESS_SIZE) then {Bare Address Form 1234567890AB}
  begin
   for Count:=0 to HARDWARE_ADDRESS_SIZE - 1 do
    begin
     Result[Count]:=StrToIntDef('$' + Copy(AAddress,(Count * 2) + 1,2),0);
    end;
  end
 else if Length(AAddress) = ((2 * HARDWARE_ADDRESS_SIZE) + 5) then {Colon separated Form 12:34:56:78:90:AB}
  begin
   for Count:=0 to HARDWARE_ADDRESS_SIZE - 1 do
    begin
     Result[Count]:=StrToIntDef('$' + Copy(AAddress,(Count * 3) + 1,2),0);
    end;
  end;
end;

{==============================================================================}

function CompareHardwareAddress(const AAddress1,AAddress2:THardwareAddress):Boolean; 
var
 Count:Integer;
begin
 {}
 Result:=False;
 for Count:=0 to HARDWARE_ADDRESS_SIZE - 1 do
  begin
   if AAddress1[Count] <> AAddress2[Count] then Exit;
  end;
 Result:=True;
end;

{==============================================================================}

function CompareHardwareDefault(const AAddress:THardwareAddress):Boolean; 
begin
 {}
 Result:=CompareHardwareAddress(AAddress,HARDWARE_DEFAULT);
end;

{==============================================================================}

function CompareHardwareBroadcast(const AAddress:THardwareAddress):Boolean; 
begin
 {}
 Result:=CompareHardwareAddress(AAddress,HARDWARE_BROADCAST);
end;

{==============================================================================}

function CompareHardwareMulticast(const AAddress:THardwareAddress):Boolean; 
begin
 {}
 Result:=(AAddress[0] and $01) <> 0;
end;

{==============================================================================}

function AdapterTypeToString(AType:Word):String;
begin
 {}
 Result:='ADAPTER_TYPE_UNKNOWN';
 
 {Check Type}
 case AType of
  ADAPTER_TYPE_WIRED:Result:='ADAPTER_TYPE_WIRED';
  ADAPTER_TYPE_LOOPBACK:Result:='ADAPTER_TYPE_LOOPBACK';
  ADAPTER_TYPE_WIRELESS:Result:='ADAPTER_TYPE_WIRELESS';
 end; 
end;

{==============================================================================}

function AdapterModeToString(AMode:Word):String;
begin
 {}
 Result:='';
 
 {Check Mode}
 case AMode of
  ADAPTER_MODE_NONE:Result:='ADAPTER_MODE_NONE';
  ADAPTER_MODE_LOCAL:Result:='ADAPTER_MODE_LOCAL';     
  ADAPTER_MODE_BROADCAST:Result:='ADAPTER_MODE_BROADCAST';  
  ADAPTER_MODE_LOCAL_MULTI:Result:='ADAPTER_MODE_LOCAL_MULTI'; 
  ADAPTER_MODE_BROADCAST_MULTI:Result:='ADAPTER_MODE_BROADCAST_MULTI';
  ADAPTER_MODE_PROMISCUOUS:Result:='ADAPTER_MODE_PROMISCUOUS';
 end; 
end;

{==============================================================================}

function AdapterConfigToString(AConfig:Word):String;
begin
 {}
 Result:='';
 
 {Check Config}
 case AConfig of
  CONFIG_ADAPTER_DISCOVER:Result:='CONFIG_ADAPTER_DISCOVER';
  CONFIG_ADAPTER_REQUEST:Result:='CONFIG_ADAPTER_REQUEST';
  CONFIG_ADAPTER_RELEASE:Result:='CONFIG_ADAPTER_RELEASE';
  CONFIG_ADAPTER_RENEW:Result:='CONFIG_ADAPTER_RENEW';
  CONFIG_ADAPTER_REBIND:Result:='CONFIG_ADAPTER_REBIND';
  CONFIG_ADAPTER_INFORM:Result:='CONFIG_ADAPTER_INFORM';
  CONFIG_ADAPTER_REBOOT:Result:='CONFIG_ADAPTER_REBOOT';
 end; 
end;

{==============================================================================}

function AdapterStateToString(AState:Integer):String;
begin
 {}
 Result:='';
 
 {Check State}
 case AState of
  ADAPTER_STATE_DISABLED:Result:='ADAPTER_STATE_DISABLED';
  ADAPTER_STATE_ENABLED:Result:='ADAPTER_STATE_ENABLED';
 end; 
end;

{==============================================================================}

function AdapterStatusToString(AStatus:Integer):String;
begin
 {}
 Result:='';
 
 {Check Status}
 case AStatus of
  ADAPTER_STATUS_DOWN:Result:='ADAPTER_STATUS_DOWN';
  ADAPTER_STATUS_UP:Result:='ADAPTER_STATUS_UP';
 end; 
end;

{==============================================================================}

function FrameTypeToString(AType:Word):String;
begin
 {}
 Result:='FRAME_TYPE_UNKNOWN';
 
 {Check Type}
 case AType of
  FRAME_TYPE_ETHERNET_II:Result:='FRAME_TYPE_ETHERNET_II';
  FRAME_TYPE_TOKEN_RING:Result:='FRAME_TYPE_TOKEN_RING';
  FRAME_TYPE_APPLETALK:Result:='FRAME_TYPE_APPLETALK';
  FRAME_TYPE_ETHERNET_8022:Result:='FRAME_TYPE_ETHERNET_8022';
  FRAME_TYPE_ETHERNET_SNAP:Result:='FRAME_TYPE_ETHERNET_SNAP';
  FRAME_TYPE_ETHERNET_8023:Result:='FRAME_TYPE_ETHERNET_8023';
 end; 
end;

{==============================================================================}

function MediaTypeToString(AType:Word):String;
begin
 {}
 Result:='';
 
 {Check Type}
 case AType of
  MEDIA_TYPE_ETHERNET:Result:='MEDIA_TYPE_ETHERNET';
  MEDIA_TYPE_TOKENRING:Result:='MEDIA_TYPE_TOKENRING';
 end; 
end;

{==============================================================================}

function PacketTypetoString(AType:Word):String;
begin
 {}
 Result:='';
 
 {Check Type}
 case AType of
  PACKET_TYPE_IP:Result:='PACKET_TYPE_IP';
  PACKET_TYPE_IP6:Result:='PACKET_TYPE_IP6';
  PACKET_TYPE_ARP:Result:='PACKET_TYPE_ARP';
  PACKET_TYPE_RARP:Result:='PACKET_TYPE_RARP';
  PACKET_TYPE_IPX:Result:='PACKET_TYPE_IPX';
  PACKET_TYPE_EAPOL:Result:='PACKET_TYPE_EAPOL';
  PACKET_TYPE_RSN:Result:='PACKET_TYPE_RSN';
 end; 
end;

{==============================================================================}

function ConfigTypeToString(AType:Word):String;
begin
 {}
 Result:='CONFIG_TYPE_UNKNOWN';
 
 {Check Type}
 case AType of
  CONFIG_TYPE_AUTO:Result:='CONFIG_TYPE_AUTO';
  CONFIG_TYPE_STATIC:Result:='CONFIG_TYPE_STATIC';
  CONFIG_TYPE_RARP:Result:='CONFIG_TYPE_RARP';
  CONFIG_TYPE_BOOTP:Result:='CONFIG_TYPE_BOOTP';
  CONFIG_TYPE_DHCP:Result:='CONFIG_TYPE_DHCP';
  CONFIG_TYPE_PSEUDO:Result:='CONFIG_TYPE_PSEUDO';
  CONFIG_TYPE_LOOPBACK:Result:='CONFIG_TYPE_LOOPBACK';
 end; 
end;

{==============================================================================}

function ConfigCommandToString(ACommand:Word):String;
begin
 {}
 Result:='';
 
 {Check Command}
 case ACommand of
  CONFIG_ADAPTER_DISCOVER:Result:='CONFIG_ADAPTER_DISCOVER';
  CONFIG_ADAPTER_REQUEST:Result:='CONFIG_ADAPTER_REQUEST';
  CONFIG_ADAPTER_RELEASE:Result:='CONFIG_ADAPTER_RELEASE';
  CONFIG_ADAPTER_RENEW:Result:='CONFIG_ADAPTER_RENEW';
  CONFIG_ADAPTER_REBIND:Result:='CONFIG_ADAPTER_REBIND';
  CONFIG_ADAPTER_INFORM:Result:='CONFIG_ADAPTER_INFORM';
  CONFIG_ADAPTER_REBOOT:Result:='CONFIG_ADAPTER_REBOOT';
 end; 
end;

{==============================================================================}

function AuthTypeToString(AType:Word):String;
begin
 {}
 Result:='AUTH_TYPE_UNKNOWN';
 
 {Check Type}
 case AType of
  AUTH_TYPE_EAP:Result:='AUTH_TYPE_EAP';
  AUTH_TYPE_RSN:Result:='AUTH_TYPE_RSN';
 end; 
end;
  
{==============================================================================}

function AuthCommandToString(ACommand:Word):String;
begin
 {}
 Result:='';
 
 {Check Command}
 case ACommand of
  AUTH_COMMAND_ASSOCIATE:Result:='AUTH_COMMAND_ASSOCIATE';
  AUTH_COMMAND_DISASSOCIATE:Result:='AUTH_COMMAND_DISASSOCIATE';
  AUTH_COMMAND_AUTHENTICATE:Result:='AUTH_COMMAND_AUTHENTICATE';
  AUTH_COMMAND_UNAUTHENTICATE:Result:='AUTH_COMMAND_UNAUTHENTICATE';
 end; 
end;
  
{==============================================================================}
{==============================================================================}

initialization
 NetworkInit;

{==============================================================================}
 
finalization
 NetworkStop;

{==============================================================================}
{==============================================================================}

end.
