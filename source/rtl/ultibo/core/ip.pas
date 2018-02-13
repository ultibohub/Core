{
Ultibo IP (Internet Protocol) unit.

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

 
Internet Protocol
=================

 Note: In spite of the name, IP is considered a transport rather than a 
       protocol.
       
 Note: The implementation of Addresses at present only allows one Address
       per Adapter plus Secondary Addresses on the same subnet.

       Should be changed to allow multiple Addresses per Adapter by
       separating the Address from the AdapterTransport and using a
       Binding mechanism (as per ODI etc) to allow multiple bindings
       to different Networks on the same Adapter

       Requires changes to:

         AddAdapter/RemoveAdapter  - Similar to ODI etc
         AddAddress/RemoveAddress/FlushAddresses - Simplify (No checks for Secondary)
         SendPacket/ForwardPacket - Get Binding then get Adapter
         IP_ADD_MEMBERSHIP/IP_DROP_MEMBERSHIP - Use AddBinding/RemoveBinding
         CompareDirected - Use Binding instead of Adapter
         ProcessTransport - Check Bindings for Renew/Rebind/Expire
         ConfigHandler - Pass Binding as well as Adapter

       Requires addition of:
         TTransportBinding etc  - FBindings, TIPTransportBinding (with pointer to Adapter)
         AddBinding/RemoveBinding - Do the actual Config currently in AddAdapter (Similar to ODI etc)
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit IP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,SysUtils,Classes,Network,Transport,Protocol,ARP,Ultibo,UltiboUtils,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {IP specific constants}
 IP_TRANSPORT_NAME = 'IP';
 
 {IP Constants}
 MIN_IP_PACKET = 20;    {Not Counting Adapter Header}
 MAX_IP_PACKET = 65536; {Not Counting Adapter Header}

 MAX_FRAG_LIFE = 32000; {Only wait 32 seconds for the rest of the packet}

 IP_HEADER_SIZE = 20;     {SizeOf(TIPHeader);} {Does Not Allow for Options}
 IP_OPTIONS_SIZE = 40;    {Maximum Allowed Options}
 IP_PSEUDO_SIZE = 12;     {SizeOf(TPseudoHeader);}

 IP_PACKET_SIZE = 56;     {SizeOf(TIPPacket);} {Previously 46}
 IP_FRAGMENT_SIZE = 12;   {SizeOf(TIPFragment);}

 {IP Header Flags / Fragment Offset}
 IP_CE      = $8000;      {Congestion Experienced}
 IP_DF      = $4000;      {1 = Don't Fragment 0 = May Fragment}
 IP_MF      = $2000;      {1 = More Fragments 0 = Last Fragment}
 IP_OFFMASK = $1FFF;      {Fragment Offset Mask}

 {IP Header Options}
 IPOPT_EOL       = 0;       // end-of-option list
 IPOPT_NOP       = 1;       // no-operation
 IPOPT_RR        = 7;       // record packet route
 IPOPT_TS        = 68;      // timestamp
 IPOPT_SECURITY  = 130;     // provide s,c,h,tcc
 IPOPT_LSRR      = 131;     // loose source route
 IPOPT_SATID     = 136;     // satnet id
 IPOPT_SSRR      = 137;     // strict source route
 IPOPT_RA        = 148;     // router alert

 {Offsets to fields in IP Options other than EOL and NOP.}
 IPOPT_OPTVAL    = 0;       // option ID
 IPOPT_OLEN      = 1;       // option length
 IPOPT_OFFSET    = 2;       // offset within option
 IPOPT_MINOFF    = 4;       // min value of above

 {Flags for IPOPT_TS Flags}
 IPOPT_TS_TSONLY     = 0;   // timestamps only
 IPOPT_TS_TSANDADDR  = 1;   // timestamps and addresses
 IPOPT_TS_PRESPEC    = 3;   // specified modules only

 {Flags for IPOPT_SECURITY Flags (Network Order)}
 IPOPT_SECUR_UNCLASS    = $0000;
 IPOPT_SECUR_CONFID     = $F135;
 IPOPT_SECUR_EFTO       = $789A;
 IPOPT_SECUR_MMMM       = $BC4D;
 IPOPT_SECUR_RESTR      = $AF13;
 IPOPT_SECUR_SECRET     = $D788;
 IPOPT_SECUR_TOPSECRET  = $6BC5;

 {IP Type of Service (TOS) Options}
 IPTOS_LOWDELAY       =   $10;
 IPTOS_THROUGHPUT     =   $08;
 IPTOS_RELIABILITY    =   $04;
 IPTOS_MINCOST        =   $02;
  
{==============================================================================}
type
 {IP specific types}
 PIPHeader = ^TIPHeader;
 TIPHeader = packed record   {20 Bytes unless IP Options are added}
  VersionLength:Byte;        { $45 for IPv4 and 20-byte header }
  TOS:Byte;                  {throughput type of service}
  TotalLength:Word;          {Total Size of the Packet (Max 65535) (Network Order)}
  Id:Word;                   {Incremented with each Packet (Network Order)}
  FragOffset:Word;           {and IP Header Flags DF, MF etc (Network Order)}
  TTL:Byte;                  {Time to Live Seconds / Hops}
  Protocol:Byte;             {IPPROTO_IP, IPPROTO_TCP etc}
  Checksum:Word;             {1s Compliment checksum}
  SourceIP:TInAddr;          {Source IP (Network Order)}
  DestIP:TInAddr;            {Destination IP (Network Order)}
 end;

 PIPTimestampAddress = ^TIPTimestampAddress;
 TIPTimestampAddress = packed record
  Address:TInAddr;
  Timestamp:LongWord;
 end;

 PIPTimestamp = ^TIPTimestamp;
 TIPTimestamp = packed record
  Code:Byte;               {IPOPT_TS}
  Length:Byte;             {Size of structure (Variable)}
  Index:Byte;              {Index of current entry}
  FlagsOverflow:Byte;      {Flags (4 Bits)}
                           {Overflow counter (4 Bits)}
  case Integer of
   0:(Timestamp:LongWord);
   1:(TimestampAddress:TIPTimestampAddress);
 end;

 PIPPseudo = ^TIPPseudo;
 TIPPseudo = packed record {12 Bytes} {Used by UDP/TCP Checksum}
  SourceIP:TInAddr;          {Source IP Network Order}
  DestIP:TInAddr;            {Destination IP Network Order}
  Mbz:Byte;                  {Must Be Zero}
  Protocol:Byte;             {IP Protocol Type IPPROTO_UDP, IPPROTO_TCP etc}
  Length:Word;               {Length of UDP/TCP Header and Data}
 end;

 PIPFragment = ^TIPFragment;
 TIPFragment = record {12 Bytes} {Used by Fragment Buffer}
  Offset:Word;       {Offset of this Fragment in the Packet}
  Size:Word;         {Size of this Fragment}
  
  Prev:PIPFragment;  {Pointer to Prev Fragment}
  Next:PIPFragment;  {Pointer to Next Fragment}
 end;

 PIPPacket = ^TIPPacket;
 TIPPacket = record {56 Bytes} {Used by Fragment Buffer}
  Lock:TMutexHandle; {Packet Lock}
  
  Id:Word;           {IP Id}
  Protocol:Word;     {IP Protocol}
  Dest:TInAddr;      {IP Dest}
  Source:TInAddr;    {IP Source}

  Data:Pointer;      {Pointer to Data}
  Timeout:Int64;     {Packet Timeout}
  Total:Word;        {IP Total Length}
  Length:Word;       {IP Header Length}
  Received:Word;     {Received Bytes}
  
  Prev:PIPPacket;    {Pointer to Prev Packet}
  Next:PIPPacket;    {Pointer to Next Packet}
  
  First:PIPFragment; {Pointer to First Fragment}
  Last:PIPFragment;  {Pointer to Last Fragment}
 end;
  
{==============================================================================}
type
 {IP specific classes}
 TIPBuffer = class(TTransportBufferEx)  {Different to Socket Buffer}
   constructor Create(ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   FFirst:PIPPacket;
   FLast:PIPPacket;

   {Internal Methods}
   function GetFragment(APacket:PIPPacket;AOffset,ASize:Word):PIPFragment;
   function AddFragment(APacket:PIPPacket;AOffset,ASize:Word):PIPFragment;
   function RemoveFragment(APacket:PIPPacket):Boolean;
   procedure FlushFragments(APacket:PIPPacket);
  public
   {Public Methods}
   function GetPacket(AId:Word;AProtocol:Byte;const ASource,ADest:TInAddr;ALock:Boolean):PIPPacket;
   function AddPacket(AId:Word;AProtocol:Byte;const ASource,ADest:TInAddr;ALock:Boolean):PIPPacket;
   function RemovePacket(APacket:PIPPacket):Boolean;
   function UnlockPacket(APacket:PIPPacket):Boolean;
   procedure FlushPackets(All:Boolean);

   function PutHeader(APacket:PIPPacket;ABuffer:Pointer;ALength:Word):Boolean;
   function PutFragment(APacket:PIPPacket;ABuffer:Pointer;AOffset,ASize,AFlags:Word):Boolean;
 end;

 TIPTransportAdapter = class(TTransportAdapter)
   constructor Create;
  private
   {Internal Variables}
   
  public
   {Status Variables}
   Address:TInAddr;
   Netmask:TInAddr;
   Gateway:TInAddr;
   Network:TInAddr;
   Directed:TInAddr;
   
   Server:TInAddr;       {DHCP Server}
   LeaseTime:LongWord;   {DHCP Lease Time}
   RetryTime:Int64;      {DHCP Retry Time}
   ExpiryTime:Int64;     {DHCP Expiry Time}
   RenewalTime:Int64;    {DHCP Renewal Time}
   RebindingTime:Int64;  {DHCP Rebinding Time}
   
   ConfigDefault:Word;   {BOOTP/DHCP/RARP/STATIC/PSEUDO/LOOPBACK}
   ConfigAddress:TInAddr;
   ConfigNetmask:TInAddr;
   ConfigGateway:TInAddr;
   ConfigServer:TInAddr;
 end;

 TIPTransportBinding = class(TTransportBinding)
   constructor Create;
  private
   {Internal Variables}
   
  public
   {Status Variables}
   Address:TInAddr;
   Netmask:TInAddr;
   Gateway:TInAddr;
   Network:TInAddr;
   Directed:TInAddr;
   
   Server:TInAddr;       {DHCP Server}
   LeaseTime:LongWord;   {DHCP Lease Time}
   RetryTime:Int64;      {DHCP Retry Time}
   ExpiryTime:Int64;     {DHCP Expiry Time}
   RenewalTime:Int64;    {DHCP Renewal Time}
   RebindingTime:Int64;  {DHCP Rebinding Time}
   
   ConfigDefault:Word;   {BOOTP/DHCP/RARP/STATIC/PSEUDO/LOOPBACK}
   ConfigAddress:TInAddr;
   ConfigNetmask:TInAddr;
   ConfigGateway:TInAddr;
   ConfigServer:TInAddr;
 end;

 TIPTransportProtocol = class(TTransportProtocol)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TIPTransportFilter = class(TTransportFilter)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TIPTransportConfig = class(TTransportConfig)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TIPHostEntry = class;
 TIPRouteEntry = class;
 TIPAddressEntry = class;
 TIPNetworkEntry = class;
 TIPServEntry = class;
 TIPProtoEntry = class;
 TIPTransport = class(TNetworkTransport)
   constructor Create(AManager:TTransportManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FNextIPId:Word;
   FNextIPLock:TMutexHandle; //To Do //Change this to LocalLock and share with other properties
   
   FARP:TARPTransport;
   FRARP:TRARPTransport;
   FFragments:TIPBuffer;

   FHosts:TNetworkList;
   FServs:TNetworkList;
   FProtos:TNetworkList;
   FRoutes:TNetworkList;
   FNetworks:TNetworkList;
   FAddresses:TNetworkList;

   {Status Variables}
   FNameservers:TIPNameservers; //To Do //Change Nameservers to an object type (eg Transport.TNameserverEntry and TIPNameserverEntry) (Part of TNetworkList)
   FNameserverLock:TMutexHandle;                      //Then do Add/Remove/GetNameserverByNext etc)
                                                      
   FForwarding:LongWord;
   FDefaultTTL:LongWord;
   FAutoRelease:Boolean;

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
   function FragmentHandler(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;

   function CheckFragment(ABuffer:Pointer):Boolean;

   function GetNextIPId(AIncrement:Boolean):Word;
 
   function GetIPNameserver(ACount:LongWord):TInAddr;
   
   function GetAdapterConfigType(const AName:String):Word;
   function GetAdapterConfigAddress(const AName:String):TInAddr;
   function GetAdapterConfigNetmask(const AName:String):TInAddr;
   function GetAdapterConfigGateway(const AName:String):TInAddr;
   function GetAdapterConfigServer(const AName:String):TInAddr;
  protected
   {Inherited Methods}
   function FilterPacket(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean; override;
   function ForwardPacket(AAdapter:TTransportAdapter;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean; override;
  public
   {Public Properties}
   property Nameservers:TIPNameservers read FNameservers;

   property Forwarding:LongWord read FForwarding write FForwarding;
   property DefaultTTL:LongWord read FDefaultTTL write FDefaultTTL;
   property AutoRelease:Boolean read FAutoRelease write FAutoRelease;

   {Public Methods}
   function AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean; override;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; override;

   function AddProtocol(AProtocol:Word;APacketHandler:TTransportPacketHandler;AControlHandler:TTransportControlHandler):THandle; override;
   function RemoveProtocol(AHandle:THandle;AProtocol:Word):Boolean; override;

   function AddFilter(AProtocol:Word;AFilterHandler:TTransportFilterHandler):THandle; override;
   function RemoveFilter(AHandle:THandle;AProtocol:Word):Boolean; override;

   function AddConfig(AConfigType:Word;AConfigAuto:Boolean;AConfigHandler:TTransportConfigHandler):THandle; override;
   function RemoveConfig(AHandle:THandle;AConfigType:Word):Boolean; override;

   function SendPacket(ASocket:TTransportSocket;ASource,ADest:Pointer;APacket:PPacketFragment;ASize,AFlags:Integer):Integer; override;
   function SendControl(ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean; override;

   {BSD Socket Methods}
   function GetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; override;
   function SetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; override;

   function StartTransport:Boolean; override;
   function StopTransport:Boolean; override;
   function ProcessTransport:Boolean; override;

   function BindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   function UnbindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   
   {IP Methods}
   function AddNameserver(const AAddress:TInAddr):Boolean;
   function RemoveNameserver(const AAddress:TInAddr):Boolean;

   function GetHostByName(const AName:String;ALock:Boolean):TIPHostEntry;
   function GetHostByAddress(const AAddress:TInAddr;ALock:Boolean):TIPHostEntry;
   function GetHostByNext(APrevious:TIPHostEntry;ALock,AUnlock:Boolean):TIPHostEntry;
   function AddHost(const AAddress:TInAddr;const AName:String;AType:Word;ALock:Boolean):TIPHostEntry;
   function RemoveHost(const AAddress:TInAddr):Boolean;
   procedure FlushHosts(All:Boolean);

   function GetRouteByAddress(const AAddress:TInAddr;ALock:Boolean;AState:LongWord):TIPRouteEntry;
   function GetRouteByNetwork(const ANetwork,AAddress:TInAddr;ALock:Boolean;AState:LongWord):TIPRouteEntry;
   function GetRouteByNext(APrevious:TIPRouteEntry;ALock,AUnlock:Boolean;AState:LongWord):TIPRouteEntry;
   function AddRoute(const ANetwork,ANetmask,AGateway,AAddress:TInAddr;AType:Word;ALock:Boolean;AState:LongWord):TIPRouteEntry;
   function RemoveRoute(const ANetwork,AAddress:TInAddr):Boolean;
   procedure FlushRoutes(All:Boolean);
   
   function GetAddressByAddress(const AAddress:TInAddr;ALock:Boolean;AState:LongWord):TIPAddressEntry;
   function GetAddressByNext(APrevious:TIPAddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TIPAddressEntry;
   function AddAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter;AType:Word;ALock:Boolean;AState:LongWord):TIPAddressEntry;
   function RemoveAddress(const AAddress:TInAddr):Boolean;
   procedure FlushAddresses(All:Boolean);
   
   function GetNetworkByName(const AName:String;ALock:Boolean):TIPNetworkEntry;
   function GetNetworkByAddress(const ANetwork:TInAddr;ALock:Boolean):TIPNetworkEntry;
   function AddNetwork(const AName:String;const ANetwork:TInAddr;ALock:Boolean):TIPNetworkEntry;
   function RemoveNetwork(const AName:String):Boolean;
   
   function GetServByName(const AName,AProtocol:String;ALock:Boolean):TIPServEntry;
   function GetServByPort(APort:Word;const AProtocol:String;ALock:Boolean):TIPServEntry;
   function AddServ(const AName,AProtocol:String;APort:Word;ALock:Boolean):TIPServEntry;
   function RemoveServ(const AName,AProtocol:String):Boolean;

   function GetProtoByName(const AName:String;ALock:Boolean):TIPProtoEntry;
   function GetProtoByNumber(ANumber:Word;ALock:Boolean):TIPProtoEntry;
   function AddProto(const AName:String;ANumber:Word;ALock:Boolean):TIPProtoEntry;
   function RemoveProto(const AName:String):Boolean;

   function CompareLocal(const AAddress:TInAddr):Boolean;
   function CompareDefault(const AAddress:TInAddr):Boolean;
   function CompareLoopback(const AAddress:TInAddr):Boolean;
   function CompareDirected(const AAddress:TInAddr):Boolean;
   function CompareBroadcast(const AAddress:TInAddr):Boolean;
   function CompareMulticast(const AAddress:TInAddr):Boolean;
   function CompareAddress(const AAddress1,AAddress2:TInAddr):Boolean;
   function CompareSubnet(const AAddress,ANetwork,ANetmask:TInAddr):Boolean;
 end;

 TIPState = class(TTransportState)
   constructor Create;
  private
   {Internal Variables}
   FLocalAddress:TInAddr;   {Host Order}
   FRemoteAddress:TInAddr;  {Host Order}
   
   {Internal Methods}
   procedure SetLocalAddress(const ALocalAddress:TInAddr);
   procedure SetRemoteAddress(const ARemoteAddress:TInAddr);
  public
   {Public Properties}
   property LocalAddress:TInAddr read FLocalAddress write SetLocalAddress;
   property RemoteAddress:TInAddr read FRemoteAddress write SetRemoteAddress;
 end;

 TIPOptions = class(TTransportOptions) {For Get/Set Options Level = IP_PROTO Option = ???}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FMemory:TMemoryStream; //To Do //Should this be a TMemoryStreamEx ?
   FOptions:Pointer;
   FLength:Integer;
   
   {IP Layer Variables}
   FTOS:Byte;               {IPTOS_LOWDELAY etc}
   FTTL:Byte;               {Time To Live}
   FFlags:Word;             {IP_DF etc}
   FHeader:Boolean;         {IP_HDRINCL}
   FMulticastIF:TInAddr;    {IP_MULTICAST_IF}
   FMulticastTTL:Byte;      {IP_MULTICAST_TTL}
   FMulticastLOOP:Boolean;  {IP_MULTICAST_LOOP}
   
   {Internal Methods}
   procedure SetTOS(ATOS:Byte);
   procedure SetTTL(ATTL:Byte); 
   procedure SetFlags(AFlags:Word); 
   procedure SetHeader(AHeader:Boolean);
   procedure SetMulticastIF(const AMulticastIF:TInAddr);
   procedure SetMulticastTTL(AMulticastTTL:Byte); 
   procedure SetMulticastLOOP(AMulticastLOOP:Boolean); 
   
   procedure SetLength(ALength:Integer);
  public
   {IP Layer Properties}
   property TOS:Byte read FTOS write SetTOS;
   property TTL:Byte read FTTL write SetTTL;
   property Flags:Word read FFlags write SetFlags;
   property Header:Boolean read FHeader write SetHeader;
   property MulticastIF:TInAddr read FMulticastIF write SetMulticastIF;
   property MulticastTTL:Byte read FMulticastTTL write SetMulticastTTL;
   property MulticastLOOP:Boolean read FMulticastLOOP write SetMulticastLOOP;

   {Public Properties}
   property Options:Pointer read FOptions;  {IP_OPTIONS} //To Do //Does this need lock protection after reading (Copy on read etc) ?
   property Length:Integer read FLength write SetLength;
 end;

 TIPHostEntry = class(THostEntry)
   constructor Create;
  private
   {Internal Variables}
   FAddresses:array[0..MAX_HOST_ALIASES - 1] of TInAddr;
   
   {Internal Methods}
   function GetAddress:TInAddr;
   procedure SetAddress(const AAddress:TInAddr);
   function GetAddresses(Index:Integer):TInAddr;
  public
   {Public Properties}
   property Address:TInAddr read GetAddress write SetAddress;
   property Addresses[Index:Integer]:TInAddr read GetAddresses;

   {Public Methods}
   function FindAddress(const AAddress:TInAddr):Boolean;
   
   function AddAddress(const AAddress:TInAddr):Boolean;
   function RemoveAddress(const AAddress:TInAddr):Boolean;
 end;

 TIPRouteEntry = class(TRouteEntry)
   constructor Create;
  private
   {Internal Variables}
   FTOS:Byte;             {Default TOS for this Route}
   FTTL:Byte;             {Default TTL for this Route}
   FNetwork:TInAddr;
   FNetmask:TInAddr;
   FGateway:TInAddr;
   FAddress:TInAddr;
   
   {Internal Methods}
   procedure SetTOS(ATOS:Byte);        
   procedure SetTTL(ATTL:Byte);       
   procedure SetNetwork(const ANetwork:TInAddr);
   procedure SetNetmask(const ANetmask:TInAddr);
   procedure SetGateway(const AGateway:TInAddr);
   procedure SetAddress(const AAddress:TInAddr);
  public
   {Status Variables}
   property TOS:Byte read FTOS write SetTOS;
   property TTL:Byte read FTTL write SetTTL;
   property Network:TInAddr read FNetwork write SetNetwork;
   property Netmask:TInAddr read FNetmask write SetNetmask;
   property Gateway:TInAddr read FGateway write SetGateway;
   property Address:TInAddr read FAddress write SetAddress;
 end;

 TIPAddressEntry = class(TAddressEntry) {Used for secondary addresses}
   constructor Create;
  private
   {Internal Variables}
   FAddress:TInAddr;
   
   {Internal Methods}
   procedure SetAddress(const AAddress:TInAddr);
  public
   {Status Variables}
   property Address:TInAddr read FAddress write SetAddress;
 end;

 TIPNetworkEntry = class(TNetworkEntry)
   constructor Create;
  private
   {Internal Variables}
   FNetwork:TInAddr;
   
   {Internal Methods}
   procedure SetNetwork(const ANetwork:TInAddr);
  public
   {Public Properties}
   property Network:TInAddr read FNetwork write SetNetwork;
 end;

 TIPServEntry = class(TServEntry)
   constructor Create;
  private
   {Internal Variables}
   
  public
   {Status Variables}
   
 end;

 TIPProtoEntry = class(TProtoEntry)
   constructor Create;
  private
   {Internal Variables}
   
  public
   {Status Variables}
   
 end;
  
{==============================================================================}
{var}
 {IP specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure IPInit;

{==============================================================================}
{IP Functions}
function CheckIP(ABuffer:Pointer):Boolean;

function GetIPHeaderOffset(ABuffer:Pointer):Word;
function GetIPHeaderLength(ABuffer:Pointer):Word;
function GetIPOptionsLength(ABuffer:Pointer):Word;
function GetIPDataOffset(ABuffer:Pointer):Word;
function GetIPDataLength(ABuffer:Pointer):Word;

function ChecksumIPRecv(ABuffer:Pointer;AOffset,ALength:Word):Word;
function ChecksumIPSend(AHeader,AOptions:Pointer;ALength:Word):Word;
  
{==============================================================================}
{IP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {IP specific variables}
 IPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TIPBuffer}
constructor TIPBuffer.Create(ATransport:TNetworkTransport);
begin
 {}
 inherited Create(ATransport);
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TIPBuffer.Destroy;
begin
 {}
 WriterLock;
 try
  FlushPackets(True);
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TIPBuffer.GetFragment(APacket:PIPPacket;AOffset,ASize:Word):PIPFragment;
{Gets the Fragment with matching Offset and Size if any}

{Note: Caller must hold the Packet lock}
var
 Fragment:PIPFragment;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {Check Packet}
  if APacket = nil then Exit;
  
  {Get Fragment}
  Fragment:=APacket.First;
  while Fragment <> nil do
   begin
    {Check Fragment}
    if (Fragment.Offset = AOffset) and (Fragment.Size = ASize) then
     begin
      {Return Result}
      Result:=Fragment;
      Exit;
     end;
    
    {Get Next}
    Fragment:=Fragment.Next;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPBuffer.AddFragment(APacket:PIPPacket;AOffset,ASize:Word):PIPFragment;
{Adds a new Fragment as the Last fragment in the Packet}

{Note: Caller must hold the Packet lock}
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {Check Packet}
  if APacket = nil then Exit;
  
  {Create Fragment}
  Result:=GetMem(SizeOf(TIPFragment)); {IP_FRAGMENT_SIZE}
  if Result = nil then Exit;
  
  {Update Fragment}
  Result.Offset:=AOffset;
  Result.Size:=ASize;
  Result.Prev:=nil;
  Result.Next:=nil;
  
  {Add Fragement}
  if APacket.Last = nil then
   begin
    {Is First Fragment}
    APacket.First:=Result;
    APacket.Last:=Result;
   end
  else
   begin
    {Not First Fragment}
    APacket.Last.Next:=Result;
    Result.Prev:=APacket.Last;
    APacket.Last:=Result;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPBuffer.RemoveFragment(APacket:PIPPacket):Boolean;
{Removes the First fragment from the Packet}

{Note: Caller must hold the Packet lock}
var
 Fragment:PIPFragment;
begin
 {}
 if not(WriterOwner) then ReaderLock else WriterLock;
 try
  Result:=False;
  
  {Check Packet}
  if APacket = nil then Exit;
  
  {Check for Fragments}
  if APacket.First = nil then Exit;
  
  {Remove Fragment}
  if APacket.First.Next <> nil then
   begin
    {Not Last Fragment}
    Fragment:=APacket.First;
    APacket.First:=APacket.First.Next;
    APacket.First.Prev:=nil;
   end
  else
   begin
    {Is Last Fragment}
    Fragment:=APacket.First;
    APacket.First:=nil;
    APacket.Last:=nil;
   end;
   
  {Free Fragment}
  FreeMem(Fragment,SizeOf(TIPFragment)); {IP_FRAGMENT_SIZE}
  
  {Return Result}
  Result:=True;
 finally 
  if not(WriterOwner) then ReaderUnlock else WriterUnlock;
 end; 
end;

{==============================================================================}

procedure TIPBuffer.FlushFragments(APacket:PIPPacket);
{Removes all fragments from the Packet}

{Note: Caller must hold the Packet lock}
begin
 {}
 if not(WriterOwner) then ReaderLock else WriterLock;
 try
  {Check Packet}
  if APacket = nil then Exit;
  
  {Get Fragment}
  while APacket.First <> nil do
   begin
    {Remove Fragment}
    RemoveFragment(APacket);
   end;
 finally 
  if not(WriterOwner) then ReaderUnlock else WriterUnlock;
 end; 
end;

{==============================================================================}

function TIPBuffer.GetPacket(AId:Word;AProtocol:Byte;const ASource,ADest:TInAddr;ALock:Boolean):PIPPacket;
{Gets the Packet with matching Id, Protocol, Source and Dest if any}
var
 Packet:PIPPacket;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetPacket - Id = ' + IntToStr(WordBEtoN(AId)));
  {$ENDIF}
  
  {Get Packet}
  Packet:=FFirst;
  while Packet <> nil do
   begin
    {Check Packet}
    if (Packet.Id = AId) and (Packet.Protocol = AProtocol) then
     begin
      if LongWord(Packet.Source.S_addr) = LongWord(ASource.S_addr) then
       begin
        if LongWord(Packet.Dest.S_addr) = LongWord(ADest.S_addr) then
         begin
          {Lock Packet}
          if ALock then MutexLock(Packet.Lock);
          
          {Return Result}
          Result:=Packet;
          Exit;
         end;
       end;
     end;
     
    {Get Next} 
    Packet:=Packet.Next;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPBuffer.AddPacket(AId:Word;AProtocol:Byte;const ASource,ADest:TInAddr;ALock:Boolean):PIPPacket;
{Adds a new Packet as the Last packet in the Buffer}
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddPacket - Id = ' + IntToStr(WordBEtoN(AId)));
 {$ENDIF}
  
 {Create Packet}
 Result:=GetMem(SizeOf(TIPPacket)); {IP_PACKET_SIZE}
 if Result = nil then Exit;
  
 {Update Packet}
 Result.Lock:=MutexCreate;
 Result.Id:=AId;
 Result.Protocol:=AProtocol;
 Result.Dest:=ADest;
 Result.Source:=ASource;
 Result.Data:=GetMem(MAX_IP_PACKET);
 Result.Timeout:=GetTickCount64;
 Result.Total:=0;
 Result.Length:=0;
 Result.Received:=0;
 Result.Prev:=nil;
 Result.Next:=nil;
 Result.First:=nil;
 Result.Last:=nil;

 {Acquire Lock} 
 WriterLock; {Acquire as Writer}
 try
  {Add Packet}
  if FLast = nil then
   begin
    {Is First Packet}
    FFirst:=Result;
    FLast:=Result;
   end
  else
   begin
    {Not First Packet}
    FLast.Next:=Result;
    Result.Prev:=FLast;
    FLast:=Result;
   end;
  
  {Convert Lock}
  WriterConvert; 
  
  {Lock Packet}
  if ALock then MutexLock(Result.Lock);
 finally 
  ReaderUnlock; {Converted to Reader}
 end; 
end;

{==============================================================================}

function TIPBuffer.RemovePacket(APacket:PIPPacket):Boolean;
{Removes the Packet from the Buffer and release the memory}
begin
 {}
 Result:=False;

 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemovePacket');
 {$ENDIF}

 {Check Packet}
 if APacket = nil then Exit;

 {Acquire the Lock}
 if MutexLock(APacket.Lock) <> ERROR_SUCCESS then Exit;
  
 {Remove any Fragments}
 FlushFragments(APacket);

 {Acquire Lock} 
 WriterLock;
 try
  {Remove Packet}
  if APacket.Prev <> nil then
   begin
    {Not First Packet}
    if APacket.Next <> nil then
     begin
      {Not Last Packet}
      APacket.Prev.Next:=APacket.Next;
      APacket.Next.Prev:=APacket.Prev;
     end
    else
     begin
      {Is Last Packet}
      APacket.Prev.Next:=nil;
      FLast:=APacket.Prev;
     end;
   end
  else
   begin
    {Is First Packet}
    if APacket.Next <> nil then
     begin
      {Not Last Packet}
      APacket.Next.Prev:=nil;
      FFirst:=APacket.Next;
     end
    else
     begin
      {Is Last Packet}
      FFirst:=nil;
      FLast:=nil;
     end;
   end;
  
  {Release the Lock}
  MutexUnlock(APacket.Lock);
  
  {Free Packet Memory}
  FreeMem(APacket.Data,MAX_IP_PACKET);
  
  {Free Packet Lock}
  MutexDestroy(APacket.Lock);
  
  {Free Packet}
  FreeMem(APacket,SizeOf(TIPPacket)); {IP_PACKET_SIZE}

  {Return Result}
  Result:=True;
 finally 
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TIPBuffer.UnlockPacket(APacket:PIPPacket):Boolean;
{Unlock the Packet previously locked by GetPacket or AddPacket}
begin
 {}
 Result:=False;
 
 {Check Packet}
 if APacket = nil then Exit;
 
 {Unlock Packet}
 Result:=(MutexUnlock(APacket.Lock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TIPBuffer.FlushPackets(All:Boolean);
{Removes either all packets or packets which have expired}
var
 Packet:PIPPacket;
 Current:PIPPacket;
 CurrentTime:Int64;
begin
 {}
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Acquire Lock}   
 if All then WriterLock else ReaderLock;
 try
  {Get Packet}
  Packet:=FFirst;
  while Packet <> nil do
   begin
    {Get Next}
    Current:=Packet;
    Packet:=Current.Next;
    
    {Check Packet}
    if ((Current.Timeout + MAX_FRAG_LIFE) < CurrentTime) or (All) then
     begin
      {Convert Lock}
      if not(All) then ReaderConvert; 
     
      {Remove Packet}
      RemovePacket(Current);
      
      {Convert Lock}
      if not(All) then WriterConvert; 
     end;
   end;
 finally 
  if All then WriterUnlock else ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPBuffer.PutHeader(APacket:PIPPacket;ABuffer:Pointer;ALength:Word):Boolean;
{Copies the Packet header from the Buffer to the Packet}

{Note: Caller must hold the Packet lock}
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: PutHeader - Length = ' + IntToStr(ALength));
  {$ENDIF}
  
  {Check Packet}
  if APacket = nil then Exit;
  
  {Check Buffer}
  if ABuffer = nil then Exit;
  
  {Update Packet}
  APacket.Length:=ALength;
  Inc(APacket.Received,ALength);
  
  {Copy Header}
  System.Move(ABuffer^,APacket.Data^,ALength);
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPBuffer.PutFragment(APacket:PIPPacket;ABuffer:Pointer;AOffset,ASize,AFlags:Word):Boolean;
{Creates a new Fragment and copies the data from the Buffer to the Packet}

{Note: Caller must hold the Packet lock}
var
 Fragment:PIPFragment;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: PutFragment - Size = ' + IntToStr(ASize));
  {$ENDIF}
  
  {Check Packet}
  if APacket = nil then Exit;
  
  {Check Buffer}
  if ABuffer = nil then Exit;
  
  {Get Fragment}
  Fragment:=GetFragment(APacket,AOffset,ASize);
  if Fragment = nil then
   begin
    {Add Fragment}
    Fragment:=AddFragment(APacket,AOffset,ASize);
    if Fragment = nil then Exit;
    
    {Update Packet}
    Inc(APacket.Received,ASize);
    
    {Copy Fragment}
    System.Move(ABuffer^,Pointer(LongWord(APacket.Data) + APacket.Length + AOffset)^,ASize);
    
    {Check Flags}
    if (AFlags and IP_MF) = 0 then
     begin
      {Last Fragment}
      APacket.Total:=APacket.Length + AOffset + ASize;
      
      {Update Header}
      PIPHeader(APacket.Data).TotalLength:=WordNtoBE(APacket.Total);
      PIPHeader(APacket.Data).FragOffset:=WordNtoBE(AFlags);
     end;
   end;
  
  {Return Result}  
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TIPTransportAdapter}
constructor TIPTransportAdapter.Create;
begin
 {}
 inherited Create;
 LongWord(Address.S_addr):=INADDR_ANY;
 LongWord(Netmask.S_addr):=INADDR_ANY;
 LongWord(Gateway.S_addr):=INADDR_ANY;
 LongWord(Network.S_addr):=INADDR_ANY;
 LongWord(Directed.S_addr):=INADDR_BROADCAST;
 
 LongWord(Server.S_addr):=INADDR_BROADCAST;
 LeaseTime:=0;
 RetryTime:=0;
 ExpiryTime:=0;
 RenewalTime:=0;
 RebindingTime:=0;
 
 ConfigDefault:=CONFIG_TYPE_AUTO;
 LongWord(ConfigAddress.S_addr):=INADDR_ANY;
 LongWord(ConfigNetmask.S_addr):=INADDR_ANY;
 LongWord(ConfigGateway.S_addr):=INADDR_ANY;
 LongWord(ConfigServer.S_addr):=INADDR_BROADCAST;
end;

{==============================================================================}
{==============================================================================}
{TIPTransportBinding}
constructor TIPTransportBinding.Create;
begin
 {}
 inherited Create;
 LongWord(Address.S_addr):=INADDR_ANY;
 LongWord(Netmask.S_addr):=INADDR_ANY;
 LongWord(Gateway.S_addr):=INADDR_ANY;
 LongWord(Network.S_addr):=INADDR_ANY;
 LongWord(Directed.S_addr):=INADDR_BROADCAST;
 
 LongWord(Server.S_addr):=INADDR_BROADCAST;
 LeaseTime:=0;
 RetryTime:=0;
 ExpiryTime:=0;
 RenewalTime:=0;
 RebindingTime:=0;
 
 ConfigDefault:=CONFIG_TYPE_AUTO;
 LongWord(ConfigAddress.S_addr):=INADDR_ANY;
 LongWord(ConfigNetmask.S_addr):=INADDR_ANY;
 LongWord(ConfigGateway.S_addr):=INADDR_ANY;
 LongWord(ConfigServer.S_addr):=INADDR_BROADCAST;
end;

{==============================================================================}
{==============================================================================}
{TIPTransport}
constructor TIPTransport.Create(AManager:TTransportManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FFamily:=AF_INET;
 FPacketType:=PACKET_TYPE_IP;

 FNextIPId:=1;
 FNextIPLock:=MutexCreate;
 
 FARP:=nil;
 FRARP:=nil;
 FFragments:=TIPBuffer.Create(Self);

 FHosts:=TNetworkList.Create;
 FServs:=TNetworkList.Create;
 FProtos:=TNetworkList.Create;
 FRoutes:=TNetworkList.Create;
 FNetworks:=TNetworkList.Create;
 FAddresses:=TNetworkList.Create;

 FillChar(FNameservers,SizeOf(TIPNameservers),0);
 FNameserverLock:=MutexCreate;

 FForwarding:=WSA_IP_NOT_FORWARDING;
 FDefaultTTL:=TTL_DEFAULT;
 FAutoRelease:=False;
end;

{==============================================================================}

destructor TIPTransport.Destroy;
begin
 {}
 WriterLock;
 try
  MutexDestroy(FNameserverLock);
  
  FAddresses.Free;
  FNetworks.Free;
  FRoutes.Free;
  FProtos.Free;
  FServs.Free;
  FHosts.Free;

  FFragments.Free;
  FRARP:=nil;
  FARP:=nil;
  
  MutexDestroy(FNextIPLock);
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TIPTransport.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by an Adapter}
{Handle: The Handle of the Transport Adapter the packet was received from}
{Source: The source hardware address of the received packet (Set by Adapter)}
{Dest: The destination hardware address of the received packet (Set by Adapter)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}

{Note: The first protocol handler to accept the packet causes processing to cease}
var
 IP:PIPHeader;
 Broadcast:Boolean;
 Adapter:TIPTransportAdapter;
 Protocol:TIPTransportProtocol;
begin
 {}
 Result:=False;
 
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Handle = ' + IntToHex(AHandle,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
  
 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}
 
 {Check Source}
 {if ASource = nil then Exit;} {Not Used} 
  
 {Check Packet}
 if APacket = nil then Exit;
  
 {Get Adapter}
 Adapter:=TIPTransportAdapter(GetAdapterByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try 
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  PacketType = ' + PacketTypeToString(Adapter.PacketType));
  {$ENDIF}
  
  {Check Packet Type}
  case Adapter.PacketType of
   PACKET_TYPE_IP:begin
     {Check IP Packet}
     if CheckIP(APacket) then
      begin
       {Get Header}
       IP:=PIPHeader(APacket);
     
       {$IFDEF IP_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + ProtocolToString(IP.Protocol));
       {$ENDIF}
     
       {Set the Addresses to Host order}
       IP.DestIP:=InAddrToHost(IP.DestIP);
       IP.SourceIP:=InAddrToHost(IP.SourceIP);
       
       {$IFDEF IP_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  DestIP = ' + InAddrToString(InAddrToNetwork(IP.DestIP)));
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  SourceIP = ' + InAddrToString(InAddrToNetwork(IP.SourceIP)));
       {$ENDIF}
       
       {Check for Broadcast}
       Broadcast:=False;
       if CompareBroadcast(IP.DestIP) or CompareDirected(IP.DestIP) then
        begin
         Broadcast:=True;
        end;
       
       {Check for Local or Loopback}
       if Broadcast or CompareLocal(IP.DestIP) or CompareLoopback(IP.DestIP) then
        begin
         {Check for Fragment}
         if not CheckFragment(IP) then
          begin
           {Check for Filter}
           if not FilterPacket(@IP.SourceIP,@IP.DestIP,IP,ASize,Broadcast) then
            begin
             {Get Protocol}
             Protocol:=TIPTransportProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
             while Protocol <> nil do
              begin
               {Note: IPPROTO_IP matches any Protocol}
               if (Protocol.Protocol = IP.Protocol) or (Protocol.Protocol = IPPROTO_IP) then
                begin
                 {Check Protocol}
                 if Assigned(Protocol.PacketHandler) then
                  begin
                   {Call Handler}
                   Result:=Protocol.PacketHandler(THandle(Protocol),@IP.SourceIP,@IP.DestIP,IP,ASize,Broadcast);
                   if Result then
                    begin
                     {Unlock Protocol}
                     Protocol.ReaderUnlock;
                     Exit;
                    end; 
                  end;
                end;
               
               {Get Next}
               Protocol:=TIPTransportProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
              end;
             {If no one handled the Packet send an ICMP_UNREACH/PROTOCOL}
             if not Broadcast then
              begin
               Result:=SendControl(@IP.DestIP,@IP.SourceIP,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_PROTOCOL,nil,IP,ASize);
              end;
            end
           else
            begin
             {If the Packet was filtered send an ICMP_UNREACH/FILTER}
             if not Broadcast then
              begin
               Result:=SendControl(@IP.DestIP,@IP.SourceIP,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_FILTER_PROHIB,nil,IP,ASize);
              end;
            end;
          end
         else
          begin
           {Call Fragment Handler}
           Result:=FragmentHandler(@IP.SourceIP,@IP.DestIP,IP,ASize,Broadcast);
          end;
        end
       else
        begin
         {Forward Packet} {Dont forward Hardware Broadcast or Loopback Source}
         if not(ABroadcast) and not(CompareLoopback(IP.SourceIP)) and (FForwarding = WSA_IP_FORWARDING) then
          begin
           Result:=ForwardPacket(Adapter,@IP.SourceIP,@IP.DestIP,IP,ASize,Broadcast);
          end;
        end;
      end
     else
      begin
       {Silently consume and discard a bad IP packet}
       Result:=True;
      end;
    end;
  end;
 finally 
  Adapter.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.FragmentHandler(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a received fragment by adding it to the buffer and if completed process the packet}
{Source: The source IP address of the received fragment (Set by Packet Handler)}
{Dest: The destination IP address of the received fragment (Set by Packet Handler)}
{Packet: The received fragment (The complete packet without Adapter header)}
{Size: The size of the received fragment in bytes}
{Broadcast: True if the destination address is a broadcast address}
var
 Size:Word;
 Flags:Word;
 Offset:Word;
 Length:Word;
 IP:PIPHeader;
 Packet:PIPPacket;
 Protocol:TIPTransportProtocol;
begin
 {}
 Result:=False;
  
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: FragmentHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
  
 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}
 
 {Check Source}
 {if ASource = nil then Exit;} {Not Used} 
  
 {Check Packet}
 if APacket = nil then Exit;
  
 {Get Header}
 IP:=PIPHeader(APacket);
  
 {Get Length}
 Length:=GetIPHeaderLength(IP);
  
 {Get Packet}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  GetPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   Id = ' + IntToStr(WordBEtoN(IP.Id)));
 {$ENDIF}
 
 Packet:=FFragments.GetPacket(IP.Id,IP.Protocol,IP.SourceIP,IP.DestIP,True);
 if Packet = nil then
  begin
   {Add the Packet}
   {$IFDEF IP_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  AddPacket');
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   Id = ' + IntToStr(WordBEtoN(IP.Id)));
   {$ENDIF}
   
   Packet:=FFragments.AddPacket(IP.Id,IP.Protocol,IP.SourceIP,IP.DestIP,True);
   if Packet = nil then Exit;

   {Add the Header}
   if not FFragments.PutHeader(Packet,IP,Length) then
    begin
     FFragments.UnlockPacket(Packet);
     Exit;
    end; 
  end;
 try
  {Add the Fragment}
  Size:=WordBEtoN(IP.TotalLength) - Length;
  Flags:=WordBEtoN(IP.FragOffset) and not(IP_OFFMASK);
  Offset:=(WordBEtoN(IP.FragOffset) and IP_OFFMASK) shl 3;
  if not FFragments.PutFragment(Packet,Pointer(LongWord(IP) + Length),Offset,Size,Flags) then Exit;

  {Check for Completed}
  if Packet.Received = Packet.Total then
   begin
    try
     {Check for Filter}
     if not FilterPacket(@Packet.Source,@Packet.Dest,Packet.Data,Packet.Total,ABroadcast) then
      begin
       {Get Protocol}
       Protocol:=TIPTransportProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
       while Protocol <> nil do
        begin
         {Note: IPPROTO_IP matches any Protocol}
         if (Protocol.Protocol = Packet.Protocol) or (Protocol.Protocol = IPPROTO_IP) then
          begin
           {Check Protocol}
           if Assigned(Protocol.PacketHandler) then
            begin
             {Call Handler}
             Result:=Protocol.PacketHandler(THandle(Protocol),@Packet.Source,@Packet.Dest,Packet.Data,Packet.Total,ABroadcast);
             if Result then
              begin
               {Unlock Protocol}
               Protocol.ReaderUnlock;
               Exit;
              end; 
            end;
          end;
         
         {Get Next}
         Protocol:=TIPTransportProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
        end;
       {If no one handled the Packet send an ICMP_UNREACH/PROTOCOL}
       if not ABroadcast then
        begin
         Result:=SendControl(@Packet.Dest,@Packet.Source,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_PROTOCOL,nil,Packet.Data,Packet.Total);
        end;
      end
     else
      begin
       {If the Packet was filtered send an ICMP_UNREACH/FILTER}
       if not ABroadcast then
        begin
         Result:=SendControl(@Packet.Dest,@Packet.Source,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_FILTER_PROHIB,nil,Packet.Data,Packet.Total);
        end;
      end;
    finally
     {Unlock Packet}
     FFragments.UnlockPacket(Packet);
     
     {Free the Packet}
     FFragments.RemovePacket(Packet);
     
     Packet:=nil;
    end;
   end;
 finally 
  {Unlock Packet}
  if Packet <> nil then FFragments.UnlockPacket(Packet);
 end; 
end;

{==============================================================================}

function TIPTransport.CheckFragment(ABuffer:Pointer):Boolean;
{Check if a packet is a fragment and return True if it is}
{Buffer: The packet to check (The complete packet without Adapter header)}
var
 Flags:Word;
 Offset:Word;
 IP:PIPHeader;
begin
 {}
 Result:=False;
  
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: CheckFragment');
 {$ENDIF}
  
 {Check Buffer}
 if ABuffer = nil then Exit;
  
 {Get Header} 
 IP:=PIPHeader(ABuffer);
 
 {Get Flags}
 Flags:=WordBEtoN(IP.FragOffset) and not(IP_OFFMASK);
 
 {Get Offset}
 Offset:=(WordBEtoN(IP.FragOffset) and IP_OFFMASK) shl 3;
  
 {Check for Dont Fragment Flag} {Dont check as Solaris sets it}
 {if (Flags and IP_DF) <> IP_DF then}
 { begin}
   {Check for More Fragments or Offset not zero}
   {Note: Last fragment will not have IP_MF but will have Offset}
   if ((Flags and IP_MF) = IP_MF) or (Offset <> 0) then
    begin
     Result:=True;
    end;
 { end;}
end;

{==============================================================================}

function TIPTransport.GetNextIPId(AIncrement:Boolean):Word;
{Get the next IP packet id number}
{Increment: If True increment the next id}
begin
 {}
 MutexLock(FNextIPLock);

 {Get Next Id}
 Result:=FNextIPId;
  
 {Increment Id}
 if AIncrement then Inc(FNextIPId,ID_INCREMENT);

 MutexUnlock(FNextIPLock);
end;

{==============================================================================}

function TIPTransport.GetIPNameserver(ACount:LongWord):TInAddr;
{Get the nameserver address from the network settings}
var
 Value:String;
begin
 {}
 LongWord(Result.S_addr):=INADDR_NONE;
 
 Value:=Uppercase(Manager.Settings.GetString('IP_NAMESERVER' + IntToStr(ACount)));
 if Length(Value) <> 0 then
  begin
   Result:=InAddrToHost(StringToInAddr(Value));
  end; 
end;
 
{==============================================================================}

function TIPTransport.GetAdapterConfigType(const AName:String):Word;
{Get the adapter config type from the network settings}
var
 Value:String;
begin
 {}
 Result:=CONFIG_TYPE_AUTO;
 
 Value:=Uppercase(Manager.Settings.GetString(AName + '_IP_CONFIG'));
 if Length(Value) <> 0 then
  begin
   Result:=StrToIntDef(Value,CONFIG_TYPE_UNKNOWN);
   if Result > CONFIG_TYPE_PSEUDO then {CONFIG_TYPE_LOOPBACK not allowed}
    begin
     if Value = 'STATIC' then
      begin
       Result:=CONFIG_TYPE_STATIC;
      end
     else if Value = 'RARP' then
      begin
       Result:=CONFIG_TYPE_RARP;
      end
     else if Value = 'BOOTP' then
      begin
       Result:=CONFIG_TYPE_BOOTP;
      end
     else if Value = 'DHCP' then
      begin
       Result:=CONFIG_TYPE_DHCP;
      end
     else if Value = 'PSEUDO' then
      begin
       Result:=CONFIG_TYPE_PSEUDO;
      end
     else
      begin
       Result:=CONFIG_TYPE_AUTO;
      end;      
    end; 
  end; 
end;

{==============================================================================}

function TIPTransport.GetAdapterConfigAddress(const AName:String):TInAddr;
{Get the adapter address from the network settings}
var
 Value:String;
begin
 {}
 LongWord(Result.S_addr):=INADDR_NONE;
 
 Value:=Uppercase(Manager.Settings.GetString(AName + '_IP_ADDRESS'));
 if Length(Value) <> 0 then
  begin
   Result:=InAddrToHost(StringToInAddr(Value));
  end; 
end;

{==============================================================================}

function TIPTransport.GetAdapterConfigNetmask(const AName:String):TInAddr;
{Get the adapter netmask from the network settings}
var
 Value:String;
begin
 {}
 LongWord(Result.S_addr):=INADDR_NONE;
 
 Value:=Uppercase(Manager.Settings.GetString(AName + '_IP_NETMASK'));
 if Length(Value) <> 0 then
  begin
   Result:=InAddrToHost(StringToInAddr(Value));
  end; 
end;

{==============================================================================}

function TIPTransport.GetAdapterConfigGateway(const AName:String):TInAddr;
{Get the adapter gateway from the network settings}
var
 Value:String;
begin
 {}
 LongWord(Result.S_addr):=INADDR_NONE;
 
 Value:=Uppercase(Manager.Settings.GetString(AName + '_IP_GATEWAY'));
 if Length(Value) <> 0 then
  begin
   Result:=InAddrToHost(StringToInAddr(Value));
  end; 
end;

{==============================================================================}

function TIPTransport.GetAdapterConfigServer(const AName:String):TInAddr;
{Get the adapter server from the network settings}
var
 Value:String;
begin
 {}
 LongWord(Result.S_addr):=INADDR_NONE;
 
 Value:=Uppercase(Manager.Settings.GetString(AName + '_IP_SERVER'));
 if Length(Value) <> 0 then
  begin
   Result:=InAddrToHost(StringToInAddr(Value));
  end; 
end;

{==============================================================================}

function TIPTransport.FilterPacket(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Filter a received packet}
{Source: The source IP address of the received fragment (Set by Packet or Fragment Handler)}
{Dest: The destination IP address of the received fragment (Set by Packet or Fragment Handler)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}

{Note: If Filter returns True the Packet should be discarded}
{Note: The first filter handler to reject the packet causes filtering to cease}
var
 IP:PIPHeader;
 Filter:TIPTransportFilter;
begin
 {}
 Result:=False;
  
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: FilterPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
  
 {Check Dest}
 if ADest = nil then Exit; 
 
 {Check Source}
 if ASource = nil then Exit; 
  
 {Check Packet}
 if APacket = nil then Exit;
  
 {Get Header}
 IP:=PIPHeader(APacket);
  
 {Get Filter}
 Filter:=TIPTransportFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
 while Filter <> nil do
  begin
   {Note: IPPROTO_IP matches any Protocol}
   if (Filter.Protocol = IP.Protocol) or (Filter.Protocol = IPPROTO_IP) then
    begin
     {Check Filter}
     if Assigned(Filter.FilterHandler) then
      begin
       {Call Handler}
       Result:=Filter.FilterHandler(THandle(Filter),ASource,ADest,APacket,ASize,ABroadcast);
       if Result then
        begin
         {Unlock Filter}
         Filter.ReaderUnlock;
         Exit;
        end; 
      end;
    end;
   
   {Get Next}    
   Filter:=TIPTransportFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
  end;
end;

{==============================================================================}

function TIPTransport.ForwardPacket(AAdapter:TTransportAdapter;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Forward a received packet}
{Adapter: The adapter the packet was received on}
{Source: The source IP address of the received fragment (Set by Packet Handler)}
{Dest: The destination IP address of the received fragment (Set by Packet Handler)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}

{Note: Caller must hold Adapter lock}
var
 Dest:TInAddr;
 Source:TInAddr;
 Route:TIPRouteEntry;           {Route to Dest Address (provides the Interface)}
 Packet:TPacketFragment;        {Actual Packet to Send}
 Address:TIPAddressEntry;       {Interface for Send (provides the Adapter)}
 Hardware:THardwareAddress;     {Hardware of Dest or Gateway (depends on Route)}
 Adapter:TIPTransportAdapter;   {Adapter information (provides Handle,MTU etc)}
begin
 {}
 Result:=False;
  
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: ForwardPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 if ADest = nil then Exit; 
 
 {Check Source}
 if ASource = nil then Exit; 

 {Check Packet}
 if APacket = nil then Exit;
  
 {Get Dest}
 Dest:=PInAddr(ADest)^;

 {Get Source}
 Source:=PInAddr(ASource)^;
 if CompareDefault(Source) then Exit;
  
 {Get the Route}
 Route:=GetRouteByAddress(Dest,True,NETWORK_LOCK_READ);
 if Route = nil then
  begin
   {If no route is available send an ICMP_UNREACH/NET}
   if not ABroadcast then
    begin
     Result:=SendControl(@TIPTransportAdapter(AAdapter).Address,@Source,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_NET,nil,APacket,ASize);
    end;
   Exit;
  end;
 try
  {Get the Address}
  Address:=GetAddressByAddress(Route.Address,True,NETWORK_LOCK_READ);
  if Address = nil then Exit;
  try
   {Get the Adapter}
   Adapter:=TIPTransportAdapter(GetAdapterByAdapter(Address.Adapter,True,NETWORK_LOCK_READ));
   if Adapter = nil then Exit;
   try
    {Check the Size}
    if ASize > Adapter.MTU then
     begin
      {If packet is too big send an ICMP_UNREACH/NEEDFRAG}
      if not ABroadcast then
       begin
        Result:=SendControl(@TIPTransportAdapter(AAdapter).Address,@Source,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_NEEDFRAG,nil,APacket,ASize);
       end;
      Exit;
     end;
  
    {Update the TTL}
    Dec(PIPHeader(APacket).TTL,TTL_DECREMENT);
   
    {Check the TTL}
    if PIPHeader(APacket).TTL < 1 then
     begin
      {If TTL has expired send an ICMP_TIMXCEED/INTRANS}
      if not ABroadcast then
       begin
        Result:=SendControl(@TIPTransportAdapter(AAdapter).Address,@Source,IPPROTO_ICMP,ICMP_TIMXCEED,ICMP_TIMXCEED_INTRANS,nil,APacket,ASize);
       end;
      Exit;
     end;
  
    {Set the Addresses to Network order}
    PIPHeader(APacket).DestIP:=InAddrToNetwork(PIPHeader(APacket).DestIP);
    PIPHeader(APacket).SourceIP:=InAddrToNetwork(PIPHeader(APacket).SourceIP);
  
    {Calculate the Checksum}
    PIPHeader(APacket).Checksum:=ChecksumIPRecv(APacket,0,GetIPHeaderLength(APacket));
  
    {Get the Hardware}
    if CompareAddress(Route.Gateway,Route.Address) then
     begin
      {Check ARP Transport}
      if FARP = nil then Exit;
      
      {Get the Dest Hardware}
      if not FARP.ResolveAddress(Address.Adapter,Address.Address,Dest,Hardware) then
       begin
        {If no host is available send an ICMP_UNREACH/HOST}
        if not ABroadcast then
         begin
          Result:=SendControl(@TIPTransportAdapter(AAdapter).Address,@Source,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_HOST,nil,APacket,ASize);
         end;
        Exit;
       end;
     end
    else
     begin
      {Check ARP Transport}
      if FARP = nil then Exit;

      {Get the Gateway Hardware}
      if not FARP.ResolveAddress(Address.Adapter,Address.Address,Route.Gateway,Hardware) then
       begin
        {If no host is available send an ICMP_UNREACH/HOST}
        if not ABroadcast then
         begin
          Result:=SendControl(@TIPTransportAdapter(AAdapter).Address,@Source,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_HOST,nil,APacket,ASize);
         end;
        Exit;
       end;
     end;
  
    {Create the Packet}
    Packet.Size:=ASize;
    Packet.Data:=APacket;
    Packet.Next:=nil;
  
    {Send the Packet}
    Result:=Adapter.Adapter.SendPacket(Adapter.Handle,@Hardware,@Packet,ASize);
  
    {Check Route}
    if AAdapter = Adapter then
     begin
      {If packet was sent and received on the same interface send an ICMP_REDIRECT}
      if CompareAddress(Route.Gateway,Route.Address) then
       begin
        {If host is on the local network send an ICMP_REDIRECT/ICMP_REDIRECT_HOST}
        Result:=SendControl(@TIPTransportAdapter(AAdapter).Address,@Source,IPPROTO_ICMP,ICMP_REDIRECT,ICMP_REDIRECT_HOST,@Dest,APacket,ASize);
       end
      else
       begin
        {If host is on a remote network send an ICMP_REDIRECT/ICMP_REDIRECT_NET}
        Result:=SendControl(@TIPTransportAdapter(AAdapter).Address,@Source,IPPROTO_ICMP,ICMP_REDIRECT,ICMP_REDIRECT_NET,@Route.Gateway,APacket,ASize);
       end;
     end;
   finally
    Adapter.ReaderUnlock;
   end;
  finally
   Address.ReaderUnlock;
  end;
 finally 
  Route.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean;
{Add an adapter to this transport}
{Adapter: The adapter to add}
{ConfigType: The configuration type to use for configuring the adapter (eg CONFIG_TYPE_AUTO)}
{Address: The transport address to use for this adapter (or nil if supplied during configuration)}
{Netmask: The transport netmask to use for this adapter (or nil if supplied during configuration)}
{Gateway: The transport default gateway to use for this adapter (or nil if supplied during configuration)}
{Server: The transport configuration server to use for this adapter (or nil if supplied during configuration)}
var
 Handle:THandle;
 Adapter:TIPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddAdapter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {Check State}
  if AAdapter.State <> ADAPTER_STATE_ENABLED then Exit;
  
  {Get Adapter}
  Adapter:=TIPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter = nil then
   begin
    {Check ARP Transport}
    if FARP = nil then Exit;

    {Check RARP Transport}
    if FRARP = nil then Exit;
    
    {Add IP Type}
    Handle:=AAdapter.AddTransport(PACKET_TYPE_IP,FRAME_TYPE_ETHERNET_II,IP_TRANSPORT_NAME,PacketHandler);
    if Handle <> INVALID_HANDLE_VALUE then
     begin
      {Add to ARP}
      FARP.AddAdapter(AAdapter,0,nil,nil,nil,nil);
      
      {Add to RARP}
      FRARP.AddAdapter(AAdapter,0,nil,nil,nil,nil);
      
      {Create Adapter}
      Adapter:=TIPTransportAdapter.Create;
      Adapter.Name:=AAdapter.Name;
      Adapter.Handle:=Handle;
      Adapter.PacketType:=PACKET_TYPE_IP;
      Adapter.Adapter:=AAdapter;
      Adapter.Hardware:=AAdapter.GetHardwareAddress(Handle);
      Adapter.Broadcast:=AAdapter.GetBroadcastAddress(Handle);
      Adapter.MTU:=AAdapter.GetMTU(Handle);
      Adapter.ConfigType:=AConfigType;
      Adapter.Configured:=False;
      Adapter.Configuring:=False;
      Adapter.ConfigDefault:=AConfigType;
      if AAddress <> nil then Adapter.ConfigAddress:=TInAddr(AAddress^);
      if ANetmask <> nil then Adapter.ConfigNetmask:=TInAddr(ANetmask^);
      if AGateway <> nil then Adapter.ConfigGateway:=TInAddr(AGateway^);
      if AServer <> nil then Adapter.ConfigServer:=TInAddr(AServer^);
      
      {Lock Adapter}
      Adapter.WriterLock;
      
      {Acquire Lock}
      FAdapters.WriterLock;
      try
       {Add Adapter}
       FAdapters.Add(Adapter);
      
       {Configure Adapter}
       case Adapter.ConfigType of
        CONFIG_TYPE_STATIC:begin
          {Configure using Static information}
          Adapter.RetryTime:=GetTickCount64; {Initial Configuration}
         end;
        CONFIG_TYPE_LOOPBACK:begin
          {Configure using Loopback information}
          Adapter.RetryTime:=GetTickCount64; {Initial Configuration}
         end;
        else
         begin
          {Configure using Config Handlers}
          Adapter.RetryTime:=GetTickCount64; {Initial Configuration}
         end;
       end;
      
       {Unlock Adapter}
       Adapter.WriterUnlock;

       {Return Result}
       Result:=True;
      finally
       {Release Lock}
       FAdapters.WriterUnlock;
      end;  
     end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
{Remove an adapter from this transport}
{Adapter: The adapter to remove}
var
 Config:TIPTransportConfig;
 Adapter:TIPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveAdapter');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {Check ARP Transport}
  if FARP = nil then Exit;

  {Check RARP Transport}
  if FRARP = nil then Exit;
  
  {Get Adapter}
  Adapter:=TIPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to configure}
  if Adapter = nil then Exit;
  
  {Notify Config of Remove}
  case Adapter.ConfigType of
   CONFIG_TYPE_STATIC:begin
     {Update Adapter}
     Adapter.Configured:=False;
     
     {Remove Default Route}
     if not CompareDefault(Adapter.Gateway) then
      begin
       RemoveRoute(IP_DEFAULT_NETWORK,Adapter.Address);
      end;
     
     {Remove Broadcast Route}
     RemoveRoute(IP_BROADCAST_NETWORK,Adapter.Address);
     
     {Remove Multicast Route}
     RemoveRoute(IP_MULTICAST_NETWORK,Adapter.Address);
     
     {Remove Routes (Network, Address, Directed)}
     RemoveRoute(Adapter.Directed,Adapter.Address);
     RemoveRoute(Adapter.Address,IP_LOOPBACK_ADDRESS);
     RemoveRoute(Adapter.Network,Adapter.Address);
     
     {Remove Address}
     RemoveAddress(Adapter.Address);
    end;
   CONFIG_TYPE_LOOPBACK:begin
     {Update Adapter}
     Adapter.Configured:=False;
     
     {Remove loopback Network}
     RemoveNetwork('loopback');
     
     {Remove loopback Address}
     RemoveAddress(IP_LOOPBACK_ADDRESS);
     
     {Remove loopback Route}
     RemoveRoute(IP_LOOPBACK_NETWORK,IP_LOOPBACK_ADDRESS);
     
     {Remove localhost Host}
     RemoveHost(IP_LOOPBACK_ADDRESS);
    end;
   else
    begin
     {Get Config}
     Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
     while Config <> nil do
      begin
       {Check Type}
       if (Config.ConfigType = Adapter.ConfigType) or ((Config.ConfigAuto) and (Adapter.ConfigType = CONFIG_TYPE_AUTO)) then
        begin
         {Check Handler}
         if Assigned(Config.ConfigHandler) then
          begin
           {Check for Release}
           if FAutoRelease then
            begin
             {Call Config Handler}
             if Config.ConfigHandler(THandle(Config),Adapter,CONFIG_ADAPTER_RELEASE) then
              begin
               {Update Adapter}
               Adapter.Configured:=False;
               
               {Remove Default Route}
               if not CompareDefault(Adapter.Gateway) then
                begin
                 RemoveRoute(IP_DEFAULT_NETWORK,Adapter.Address);
                end;
               
               {Remove Broadcast Route}
               RemoveRoute(IP_BROADCAST_NETWORK,Adapter.Address);
               
               {Remove Multicast Route}
               RemoveRoute(IP_MULTICAST_NETWORK,Adapter.Address);
               
               {Remove Routes (Network, Address, Directed)}
               RemoveRoute(Adapter.Directed,Adapter.Address);
               RemoveRoute(Adapter.Address,IP_LOOPBACK_ADDRESS);
               RemoveRoute(Adapter.Network,Adapter.Address);
               
               {Remove Address}
               RemoveAddress(Adapter.Address);
               
               {Unlock Config}
               Config.ReaderUnlock;
               
               Break;
              end;
            end
           else
            begin
             {Update Adapter}
             Adapter.Configured:=False;
             
             {Remove Default Route}
             if not CompareDefault(Adapter.Gateway) then
              begin
               RemoveRoute(IP_DEFAULT_NETWORK,Adapter.Address);
              end;
             
             {Remove Broadcast Route}
             RemoveRoute(IP_BROADCAST_NETWORK,Adapter.Address);
             
             {Remove Multicast Route}
             RemoveRoute(IP_MULTICAST_NETWORK,Adapter.Address);
             
             {Remove Routes (Network, Address, Directed)}
             RemoveRoute(Adapter.Directed,Adapter.Address);
             RemoveRoute(Adapter.Address,IP_LOOPBACK_ADDRESS);
             RemoveRoute(Adapter.Network,Adapter.Address);
             
             {Remove Address}
             RemoveAddress(Adapter.Address);
             
             {Unlock Config}
             Config.ReaderUnlock;
             
             Break;
            end;
          end;
        end;
        
       {Get Next} 
       Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
      end;
    end;
  end;
  
  {Remove RARP Address}
  FRARP.UnloadAddress(Adapter.Adapter,Adapter.Address);
  
  {Remove ARP Addresses}
  {FARP.UnloadAddress(Adapter.Adapter,IP_BROADCAST_ADDRESS);} {Removed in ARP.RemoveAdapter}
  FARP.UnloadAddress(Adapter.Adapter,Adapter.Directed);
  FARP.UnloadAddress(Adapter.Adapter,Adapter.Address);
  
  {Remove from RARP}
  FRARP.RemoveAdapter(AAdapter);
  
  {Remove from ARP}
  FARP.RemoveAdapter(AAdapter);
  
  {Remove IP Type}
  AAdapter.RemoveTransport(Adapter.Handle,Adapter.PacketType);
  
  {Acquire Lock}
  FAdapters.WriterLock;
  try
   {Remove Adapter}
   FAdapters.Remove(Adapter);
  
   {Unlock Adapter}
   Adapter.WriterUnlock;
  
   {Destroy Adapter}
   Adapter.Free;
  
   {Return Result}
   Result:=True;
  finally
   {Release Lock}
   FAdapters.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddProtocol(AProtocol:Word;APacketHandler:TTransportPacketHandler;AControlHandler:TTransportControlHandler):THandle;
{Add a protocol to this transport}
{Protocol: The protocol type to add}
{PacketHandler: Packet handler to call on matching protocol type}
{ControlHandler: Control handler to call on error condition}
var
 Protocol:TIPTransportProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddProtocol');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Get Protocol}
  {Protocol:=TIPTransportProtocol(GetProtocolByType(AProtocol,False,NETWORK_LOCK_NONE));} {Do not lock} {Allow multiple protocols with same type}
  {if Protocol <> nil then Exit;}
  
  {Create Protocol}
  Protocol:=TIPTransportProtocol.Create;
  Protocol.Protocol:=AProtocol;
  Protocol.PacketHandler:=APacketHandler;
  Protocol.ControlHandler:=AControlHandler;
  
  {Acquire Lock}
  FProtocols.WriterLock;
  try
   {Add Protocol}
   FProtocols.Add(Protocol);
  
   {Return Result}
   Result:=THandle(Protocol);
  finally
   {Release Lock}
   FProtocols.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveProtocol(AHandle:THandle;AProtocol:Word):Boolean;
{Remove a protocol from this transport}
{Handle: Handle of the protocol to remove}
{Protocol: The protocol type to remove}
var
 Protocol:TIPTransportProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveProtocol');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Get Protocol}
  Protocol:=TIPTransportProtocol(GetProtocolByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Protocol = nil then Exit;

  {Check Protocol}
  if Protocol.Protocol <> AProtocol then Exit;
   begin
    {Unlock Protocol}
    Protocol.WriterUnlock;
    Exit;
   end; 
  
  {Acquire Lock}
  FProtocols.WriterLock;
  try
   {Remove Protocol}
   FProtocols.Remove(Protocol);

   {Unlock Protocol}
   Protocol.WriterUnlock;
  
   {Destroy Protocol}
   Protocol.Free;
 
   {Return Result}
   Result:=True;
  finally
   {Release Lock}
   FProtocols.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddFilter(AProtocol:Word;AFilterHandler:TTransportFilterHandler):THandle;
{Add a filter to this transport}
{Protocol: The protocol type of the filter to add}
{FilterHandler: Filter handler to call on matching protocol type}
var
 Filter:TIPTransportFilter;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddFilter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Get Filter}
  Filter:=TIPTransportFilter(GetFilterByProtocol(AProtocol,False,NETWORK_LOCK_NONE)); {Do not lock}
  if Filter <> nil then Exit;
  
  {Create Filter}
  Filter:=TIPTransportFilter.Create;
  Filter.Protocol:=AProtocol;
  Filter.FilterHandler:=AFilterHandler;
  
  {Acquire Lock}
  FFilters.WriterLock;
  try
   {Add Filter}
   FFilters.Add(Filter);
  
   {Return Result}
   Result:=THandle(Filter);
  finally
   {Release Lock}
   FFilters.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveFilter(AHandle:THandle;AProtocol:Word):Boolean;
{Remove a filter from this transport}
{Handle: Handle of the filter to remove}
{Protocol: The protocol type of the filter to remove}
var
 Filter:TIPTransportFilter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveFilter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Get Filter}
  Filter:=TIPTransportFilter(GetFilterByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Filter = nil then Exit;
  
  {Check Filter}
  if Filter.Protocol <> AProtocol then
   begin
    {Unlock Filter}
    Filter.WriterUnlock;
    Exit;
   end; 
  
  {Acquire Lock}
  FFilters.WriterLock;
  try
   {Remove Filter}
   FFilters.Remove(Filter);

   {Unlock Filter}
   Filter.WriterUnlock;
  
   {Destroy Filter}
   Filter.Free;
  
   {Return Result}
   Result:=True;
  finally
   {Release Lock}
   FFilters.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddConfig(AConfigType:Word;AConfigAuto:Boolean;AConfigHandler:TTransportConfigHandler):THandle;
{Add a config to this transport}
{ConfigType: The config type to add}
{ConfigAuto: True if this config supports auto configuration type}
{ConfigHandler: Config handler to call on matching config type}
var
 Config:TIPTransportConfig;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddConfig');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}
  
  {Get Config}
  Config:=TIPTransportConfig(GetConfigByType(AConfigType,False,NETWORK_LOCK_NONE)); {Do not lock}
  if Config <> nil then Exit;
  
  {Create Config}
  Config:=TIPTransportConfig.Create;
  Config.ConfigType:=AConfigType;
  Config.ConfigAuto:=AConfigAuto;
  Config.ConfigHandler:=AConfigHandler;
  
  {Acquire Lock}
  FConfigs.WriterLock;
  try
   {Add Config}
   FConfigs.Add(Config);
  
   {Return Result}
   Result:=THandle(Config);
  finally
   {Release Lock}
   FConfigs.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveConfig(AHandle:THandle;AConfigType:Word):Boolean;
{Remove a config from this transport}
{Handle: Handle of the config to remove}
{ConfigType: The config type to remove}
var
 Config:TIPTransportConfig;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveConfig');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}
  
  {Get Config}
  Config:=TIPTransportConfig(GetConfigByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Config = nil then Exit;
  
  {Check Config}
  if Config.ConfigType <> AConfigType then
   begin
    {Unlock Config}
    Config.WriterUnlock;
    Exit;
   end; 
  
  {Acquire Lock}
  FConfigs.WriterLock;
  try
   {Remove Config}
   FConfigs.Remove(Config);

   {Unlock Config}
   Config.WriterUnlock;
  
   {Destroy Config}
   Config.Free;
  
   {Return Result}
   Result:=True;
  finally
   {Release Lock}
   FConfigs.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.SendPacket(ASocket:TTransportSocket;ASource,ADest:Pointer;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
{Send a Packet by adding the Transport Header and other details to the Data}
{Socket: The socket to use for sending the packet}
{Source: The source IP address of the packet}
{Dest: The destination IP address of the packet}
{Packet: The packet data to send}
{Size: The size of the packet data in bytes}
{Flags: Any transport specific flags for sending}

{Note: Caller must hold the Socket lock}
var
 Length:Word;                   {Length of Header (and Options)}
 Offset:Word;                   {Current Data Offset}
 Size:Integer;                  {Total Packet Size (including Headers) / Remaining Data when Fragmented}
 IP:TIPHeader;                  {IP Header of Packet}
 Source:TInAddr;                {Source Address in Packet (does not have to be on sending Interface}
 Route:TIPRouteEntry;           {Route to Dest Address (provides the Interface)}
 Packet:PPacketFragment;        {Temporary Pointer (used for copying data when Fragmented)}
 Buffer:TPacketFragment;        {Data Packet (mainly for Fragment else copied from passed Packet)}
 Header:TPacketFragment;        {Header Packet (unless IP_HDRINCL))}
 Options:TPacketFragment;       {Options Packet (optional)}
 Address:TIPAddressEntry;       {Interface for Send (provides the Adapter)}
 Hardware:THardwareAddress;     {Hardware of Dest or Gateway (depends on Route)}
 Adapter:TIPTransportAdapter;   {Adapter information (provides Handle,MTU etc)}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: SendPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Size = ' + IntToStr(ASize));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Dest = ' + InAddrToString(InAddrToNetwork(PInAddr(ADest)^)));
 {$ENDIF}
  
 {Check Dest}
 if ADest = nil then Exit;
  
 {Check Source}
 if ASource = nil then Exit;
  
 {Check Socket}
 if ASocket = nil then Exit;
  
 {Get the Route}
 NetworkSetLastError(WSAENETUNREACH);
 Route:=GetRouteByAddress(PInAddr(ADest)^,True,NETWORK_LOCK_READ);
 if Route = nil then Exit;
 try
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Route = ' + InAddrToString(InAddrToNetwork(Route.Network)) + '/' + InAddrToString(InAddrToNetwork(Route.Address)));
  {$ENDIF}
  
  {Get the Address}
  NetworkSetLastError(WSAEADDRNOTAVAIL);
  Address:=GetAddressByAddress(Route.Address,True,NETWORK_LOCK_READ);
  if Address = nil then Exit;
  try
   {$IFDEF IP_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(Address.Address)));
   {$ENDIF}
  
   {Get the Adapter}
   Adapter:=TIPTransportAdapter(GetAdapterByAdapter(Address.Adapter,True,NETWORK_LOCK_READ));
   if Adapter = nil then Exit;
   try
    {$IFDEF IP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Adapter = ' + InAddrToString(InAddrToNetwork(Adapter.Address)));
    {$ENDIF}
  
    {Get the Source}
    Source:=PInAddr(ASource)^;
    if CompareDefault(Source) then Source:=Route.Address;
    {Note: Dont check for Default Route Address to allow DHCP. GetRouteByAddress will fail if unconfigured}
    {$IFDEF IP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Source = ' + InAddrToString(InAddrToNetwork(Source)));
    {$ENDIF}
  
    {Get the Hardware}
    NetworkSetLastError(WSAEHOSTUNREACH);
    if CompareLoopback(Route.Address) then
     begin
      {Check ARP Transport}
      if FARP = nil then Exit;
      
      {Get the Loopback Hardware}
      {$IFDEF IP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Resolve Loopback Hardware');
      {$ENDIF}
      if not FARP.ResolveAddress(Address.Adapter,Address.Address,Route.Address,Hardware) then Exit;
     end
    else if CompareAddress(Route.Gateway,Route.Address) then
     begin
      {Check ARP Transport}
      if FARP = nil then Exit;

      {Get the Dest Hardware}
      {$IFDEF IP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Resolve Dest Hardware');
      {$ENDIF}
      if not FARP.ResolveAddress(Address.Adapter,Address.Address,PInAddr(ADest)^,Hardware) then Exit;
     end
    else
     begin
      {Check ARP Transport}
      if FARP = nil then Exit;

      {Get the Gateway Hardware}
      {$IFDEF IP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Resolve Gateway Hardware');
      {$ENDIF}
      if not FARP.ResolveAddress(Address.Adapter,Address.Address,Route.Gateway,Hardware) then Exit;
     end;
    {$IFDEF IP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Hardware = ' + HardwareAddressToString(Hardware));
    {$ENDIF}
 
    {Check for Header Include}
    if TIPOptions(ASocket.TransportOptions).Header then
     begin
      {Data only}
      Header.Size:=APacket.Size;
      Header.Data:=APacket.Data;
      Header.Next:=APacket.Next;
    
      {Get the Size}
      Size:=ASize;
     end
    else
     begin
      {Create the Header}
      IP.TOS:=TIPOptions(ASocket.TransportOptions).TOS;
      IP.Id:=WordNtoBE(GetNextIPId(True));
      IP.TTL:=TIPOptions(ASocket.TransportOptions).TTL;
      IP.Protocol:=ASocket.Proto;
      IP.SourceIP:=InAddrToNetwork(Source);
      IP.DestIP:=InAddrToNetwork(PInAddr(ADest)^);
    
      {Create the Fragments}
      Header.Size:=IP_HEADER_SIZE;
      Header.Data:=@IP;
      if TIPOptions(ASocket.TransportOptions).Length = 0 then
       begin
        {Header plus Data}
        Header.Next:=@Buffer;
        Options.Size:=0;
        Options.Data:=nil;
        Options.Next:=nil;
      
        {Get the Size}
        Size:=ASize + Header.Size;
      
        {Create the Header}
        IP.VersionLength:=$45;
        IP.TotalLength:=WordNtoBE(Size);
        IP.FragOffset:=WordNtoBE(AFlags or TIPOptions(ASocket.TransportOptions).Flags);
      
        {Calculate the Checksum} {Use Receive due to no Options}
        IP.Checksum:=ChecksumIPRecv(@IP,0,IP_HEADER_SIZE);
       end
      else
       begin
        {Header and Options plus Data}
        Header.Next:=@Options;
        Options.Size:=TIPOptions(ASocket.TransportOptions).Length;
        Options.Data:=TIPOptions(ASocket.TransportOptions).Options;
        Options.Next:=@Buffer;
      
        {Get the Size}
        Size:=ASize + Header.Size + Options.Size;
      
        {Create the Header}
        IP.VersionLength:=$40 + ((IP_HEADER_SIZE + Options.Size) shr 2); {Size div 4}
        IP.TotalLength:=WordNtoBE(Size);
        IP.FragOffset:=WordNtoBE(AFlags or TIPOptions(ASocket.TransportOptions).Flags); 
      
        {Calculate the Checksum}
        IP.Checksum:=ChecksumIPSend(@IP,Options.Data,Options.Size);
       end;
      Buffer.Size:=APacket.Size;
      Buffer.Data:=APacket.Data;
      Buffer.Next:=APacket.Next;
     end;
  
    {Check the Size}
    NetworkSetLastError(WSAEMSGSIZE);
    if Size > MAX_IP_PACKET then Exit;
    
    {Send the Packet}
    if Size <= Adapter.MTU then
     begin
      {Send a Single Packet}
      {$IFDEF IP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Packet is Non Fragmented');
      {$ENDIF}
    
      {Send the Packet}
      NetworkSetLastError(WSAENETDOWN);
      if Adapter.Adapter.SendPacket(Adapter.Handle,@Hardware,@Header,Size) then
       begin
        {Return passed size not sent size}
        Result:=ASize;
       end;
     end
    else
     begin
      {Send a Fragmented Packet}
      {$IFDEF IP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Packet is Fragmented');
      {$ENDIF}
    
      {Check for Header Include}
      if TIPOptions(ASocket.TransportOptions).Header then Exit;
    
      {Check for Dont Fragment}
      if ((AFlags and IP_DF) = IP_DF) or ((TIPOptions(ASocket.TransportOptions).Flags and IP_DF) = IP_DF) then Exit;
    
      {Set Offset and Length}
      Size:=ASize;
      Offset:=0;
      Length:=IP_HEADER_SIZE + Options.Size;
    
      {Set Data Fragment}
      Buffer.Size:=0;
      Buffer.Next:=nil;
      Buffer.Data:=GetMem(ASize);
      if Buffer.Data = nil then Exit;
      try
       {Copy the Packet Fragments}
       Packet:=APacket;
       while Packet <> nil do
        begin
         System.Move(Packet.Data^,Buffer.Data^,Packet.Size);
         Inc(Buffer.Size,Packet.Size);
         Packet:=Packet.Next;
        end;
     
       {Send an MTU size Packet for each Fragment}
       NetworkSetLastError(WSAENETDOWN);
       while Size > (Adapter.MTU - Length) do
        begin
         {Create the Header}
         IP.TotalLength:=WordNtoBE(Adapter.MTU);
         IP.FragOffset:=WordNtoBE((AFlags or IP_MF) or (Offset shr 3)); {Offset div 8} 

         {Calculate the Checksum}
         IP.Checksum:=ChecksumIPSend(@IP,Options.Data,Options.Size);
       
         {Set Data Fragment}
         Buffer.Size:=Adapter.MTU - Length;
       
         {Send the Fragment}
         if not Adapter.Adapter.SendPacket(Adapter.Handle,@Hardware,@Header,Adapter.MTU) then Exit;
       
         {Update Size and Offset}
         Dec(Size,Adapter.MTU - Length);
         Inc(Offset,Adapter.MTU - Length);
       
         {Update Data Fragment}
         Inc(PtrUInt(Buffer.Data),Adapter.MTU - Length);
        end;
     
       {Send the Last Fragment (either full size or partial)}
       if Size >= 0 then
        begin
         {Create the Header}
         IP.TotalLength:=WordNtoBE(Length + Size);
         IP.FragOffset:=WordNtoBE(AFlags or (Offset shr 3)); {Offset div 8} 
        
         {Calculate the Checksum}
         IP.Checksum:=ChecksumIPSend(@IP,Options.Data,Options.Size);
       
         {Set Data Fragment}
         Buffer.Size:=Size;
       
         {Send the Fragment}
         if not Adapter.Adapter.SendPacket(Adapter.Handle,@Hardware,@Header,Length + Size) then Exit;
        end;
      
       Result:=ASize;
      finally
       FreeMem(Pointer(PtrUInt(Buffer.Data) - Offset));
      end;
     end;
   finally
    Adapter.ReaderUnlock;
   end;
  finally
   Address.ReaderUnlock;
  end;  
 finally 
  Route.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.SendControl(ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;

{Note: The first control handler to accept the packet causes sending to cease}
var
 Protocol:TIPTransportProtocol;
begin
 {}
 Result:=False;
  
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: SendControl');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Protocol}
 Protocol:=TIPTransportProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
 while Protocol <> nil do
  begin
   {Check Protocol}
   if Protocol.Protocol = AProtocol then
    begin
     {Check Handler}
     if Assigned(Protocol.ControlHandler) then
      begin
       {Call Handler}
       Result:=Protocol.ControlHandler(THandle(Protocol),ASource,ADest,AProtocol,ACommand,ACode,AAddress,AData,ASize);
       if Result then
        begin
         {Unlock Protocol}
         Protocol.ReaderUnlock;
         Exit;
        end; 
      end;
    end;
    
   {Get Next} 
   Protocol:=TIPTransportProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
  end;
end;

{==============================================================================}

function TIPTransport.GetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
{BSD compatible Get Socket Option}
{Socket: The socket to get the option from}
{Level: The protocol level for the option}
{OptName: The name of the option to get}
{OptValue: The value of the option}
{OptLength: The length of the option}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetSockOpt');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Level}
 case ALevel of
  IPPROTO_IP:begin
    NetworkSetLastError(WSAENOPROTOOPT);
    
    {Check Option}
    case AOptName of
     IP_OPTIONS:begin
       NetworkSetLastError(WSAEFAULT);
        
       if AOptLength >= TIPOptions(ASocket.TransportOptions).Length then
        begin
         AOptLength:=TIPOptions(ASocket.TransportOptions).Length;
         System.Move(TIPOptions(ASocket.TransportOptions).Options^,AOptValue^,AOptLength);
         
         Result:=NO_ERROR;
        end;
      end;
     IP_MULTICAST_IF:begin
       {Note: MulticastIF only affects Sends}
       NetworkSetLastError(WSAEFAULT);
        
       if AOptLength >= SizeOf(TInAddr) then
        begin
         AOptLength:=SizeOf(TInAddr);
         PInAddr(AOptValue)^:=TIPOptions(ASocket.TransportOptions).MulticastIF;
         
         Result:=NO_ERROR;
        end;
      end;
     IP_MULTICAST_TTL:begin
       NetworkSetLastError(WSAEFAULT);
        
       if AOptLength >= SizeOf(Integer) then
        begin
         AOptLength:=SizeOf(Integer);
         PInteger(AOptValue)^:=TIPOptions(ASocket.TransportOptions).MulticastTTL;
          
         Result:=NO_ERROR;
        end;
      end;
     IP_MULTICAST_LOOP:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         AOptLength:=SizeOf(Integer);
         PInteger(AOptValue)^:=0;
         if TIPOptions(ASocket.TransportOptions).MulticastLOOP then PInteger(AOptValue)^:=1;

         Result:=NO_ERROR;
        end;
      end;
     IP_TTL:begin
       NetworkSetLastError(WSAEFAULT);
        
       if AOptLength >= SizeOf(Integer) then
        begin
         AOptLength:=SizeOf(Integer);
         PInteger(AOptValue)^:=TIPOptions(ASocket.TransportOptions).TTL;
         
         Result:=NO_ERROR;
        end;
      end;
     IP_TOS:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         AOptLength:=SizeOf(Integer);
         PInteger(AOptValue)^:=TIPOptions(ASocket.TransportOptions).TOS;
          
         Result:=NO_ERROR;
        end;
      end;
     IP_DONTFRAGMENT:begin
       NetworkSetLastError(WSAEFAULT);
        
       if AOptLength >= SizeOf(Integer) then
        begin
         AOptLength:=SizeOf(Integer);
         PInteger(AOptValue)^:=0;
         if (TIPOptions(ASocket.TransportOptions).Flags and IP_DF) <> 0 then PInteger(AOptValue)^:=1;
         
         Result:=NO_ERROR;
        end;
      end;
     IP_HDRINCL:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         AOptLength:=SizeOf(Integer);
         PInteger(AOptValue)^:=0;
         if TIPOptions(ASocket.TransportOptions).Header then PInteger(AOptValue)^:=1;
         
         Result:=NO_ERROR;
        end;
      end;
    end;
   end;
  else
   begin
    Result:=ASocket.GetOption(ALevel,AOptName,AOptValue,AOptLength);
   end;
 end;
end;

{==============================================================================}

function TIPTransport.SetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
{BSD compatible Set Socket Option}
{Socket: The socket to set the option for}
{Level: The protocol level for the option}
{OptName: The name of the option to set}
{OptValue: The value of the option}
{OptLength: The length of the option}

{Note: Caller must hold the Socket lock}
var
 Member:TInAddr;
 Address:TIPAddressEntry;
 Adapter:TIPTransportAdapter;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: SetSockOpt');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Level}
 case ALevel of
  IPPROTO_IP:begin
    NetworkSetLastError(WSAENOPROTOOPT);
    
    {Check Option}
    case AOptName of
     IP_OPTIONS:begin
       {Note: Modified behaviour from other options}
       NetworkSetLastError(WSAEFAULT);
       
       TIPOptions(ASocket.TransportOptions).Length:=AOptLength;
       if TIPOptions(ASocket.TransportOptions).Length >= AOptLength then
        begin
         Result:=NO_ERROR;
         
         if AOptLength > 0 then
          begin
           System.Move(AOptValue^,TIPOptions(ASocket.TransportOptions).Options^,AOptLength);
          end;
        end;
      end;
     IP_MULTICAST_IF:begin
       {Note: MulticastIF only affects Sends}
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(TInAddr) then
        begin
         TIPOptions(ASocket.TransportOptions).MulticastIF:=PInAddr(AOptValue)^;
         
         Result:=NO_ERROR;
        end;
      end;
     IP_MULTICAST_TTL:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         TIPOptions(ASocket.TransportOptions).MulticastTTL:=PInteger(AOptValue)^;
         
         Result:=NO_ERROR;
        end;
      end;
     IP_MULTICAST_LOOP:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         TIPOptions(ASocket.TransportOptions).MulticastLOOP:=(PInteger(AOptValue)^ <> 0);
         
         Result:=NO_ERROR;
        end;
      end;
     IP_TTL:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         TIPOptions(ASocket.TransportOptions).TTL:=PInteger(AOptValue)^;
         
         Result:=NO_ERROR;
        end;
      end;
     IP_TOS:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         TIPOptions(ASocket.TransportOptions).TOS:=PInteger(AOptValue)^;
         
         Result:=NO_ERROR;
        end;
      end;
     IP_DONTFRAGMENT:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         if PInteger(AOptValue)^ = 1 then
          begin
           TIPOptions(ASocket.TransportOptions).Flags:=TIPOptions(ASocket.TransportOptions).Flags or IP_DF;
          end
         else
          begin
           TIPOptions(ASocket.TransportOptions).Flags:=TIPOptions(ASocket.TransportOptions).Flags and not(IP_DF);
          end;
          
         Result:=NO_ERROR;
        end;
      end;
     IP_HDRINCL:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(Integer) then
        begin
         TIPOptions(ASocket.TransportOptions).Header:=(PInteger(AOptValue)^ <> 0);
         
         Result:=NO_ERROR;
        end;
      end;
     {Add/Drop Membership Options}
     IP_ADD_MEMBERSHIP:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(TMulticastRequest) then
        begin
         {Add the Hardware Address}
         //if CompareDefault(PMulticastRequest(AOptValue)^.IMRInterface) then
         //To Do //GetAdapterByAdapter/AddMulticastAddress/AddAddress(TYPE_MULTICAST) /SendControl
         //Note: The Membership is added to the supplied IF
         //See notes from Microsoft about the behaviour of this
         {Send an IGMP Membership Report}
         //
         
         Result:=NO_ERROR;
        end;
      end;
     IP_DROP_MEMBERSHIP:begin
       NetworkSetLastError(WSAEFAULT);
       
       if AOptLength >= SizeOf(TMulticastRequest) then
        begin
         {Get Member Address}
         Member:=PMulticastRequest(AOptValue)^.IMRMultiAddr;
         
         {Get Address}
         Address:=GetAddressByAddress(Member,True,NETWORK_LOCK_READ);
         if Address = nil then Exit;
         try
          {Get Adapter}
          Adapter:=TIPTransportAdapter(GetAdapterByAdapter(Address.Adapter,True,NETWORK_LOCK_READ));
          if Adapter = nil then Exit;
         
          //To Do //Multicast Memberships must be Reference counted - done by IGMP ?
         
          {Send an IGMP Leave Group}
          //To Do
          //SendControl(@Adapter.Address,@Adapter.Directed,IPPROTO_IGMP,IGMP_LEAVE,
         
          {Remove the Hardware Address}
          //To Do //Dont seem to have the Multicast Hardware address stored !!
          //Adapter.Adapter.RemoveMulticastAddress(Adapter.Handle,Address.Hardware);
         
          {Unlock Address}
          Address.ReaderUnlock;
          Address:=nil;
          
          {Remove the Address}
          RemoveAddress(Member);
          
          {Unlock Adapter}
          Adapter.ReaderUnlock;
          
          Result:=NO_ERROR;
         finally
          if Address <> nil then Address.ReaderUnlock;
         end;         
        end;
      end;
    end;
   end;
  else
   begin
    Result:=ASocket.SetOption(ALevel,AOptName,AOptValue,AOptLength);
   end;
 end;
end;

{==============================================================================}

function TIPTransport.StartTransport:Boolean;
{Start this transport ready for sending and receiving}
var
 Count:LongWord;
 Nameserver:TInAddr;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: StartTransport');
  {$ENDIF}
 
  {Check Manager}
  if Manager = nil then Exit;
 
  {Locate ARP Transport}
  FARP:=TARPTransport(Manager.GetTransportByType(AF_UNSPEC,PACKET_TYPE_ARP,False,NETWORK_LOCK_NONE)); {Do not lock} //To Do //AddTransport (TTransportTransport/TIPTransportTransport ?) //Client ?
  if FARP = nil then Exit;
  
  {Locate RARP Transport}
  FRARP:=TRARPTransport(Manager.GetTransportByType(AF_UNSPEC,PACKET_TYPE_RARP,False,NETWORK_LOCK_NONE)); {Do not lock} //To Do //AddTransport (TTransportTransport/TIPTransportTransport ?) //Client ?
  if FRARP = nil then Exit;
  
  {Add Nameservers}
  for Count:=1 to MAX_NAME_SERVERS do
   begin
    Nameserver:=GetIPNameserver(Count);
    if not CompareDefault(Nameserver) and not CompareBroadcast(Nameserver) then
     begin
      AddNameserver(Nameserver);
     end;
   end;  
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.StopTransport:Boolean;
{Stop this transport ready for removal}
var
 Current:TIPTransportAdapter;
 Adapter:TIPTransportAdapter; 
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: StopTransport');
  {$ENDIF}
 
  {Check Manager}
  if Manager = nil then Exit;
 
  {Get Adapter}
  Adapter:=TIPTransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Get Next}
    Current:=Adapter;
    Adapter:=TIPTransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));
    
    {Remove Adapter} 
    RemoveAdapter(Current.Adapter);
   end;
 
  {Flush all Addresses}
  FlushAddresses(True);
 
  {Flush all Routes}
  FlushRoutes(True);
 
  {Flush all Hosts}
  FlushHosts(True);
 
  {Flush all Fragments}
  FFragments.FlushPackets(True);
 
  {Remove RARP Transport} //To Do //RemoveTransport (TTransportTransport/TIPTransportTransport ?)
  FRARP:=nil;
 
  {Remove ARP Transport} //To Do //RemoveTransport (TTransportTransport/TIPTransportTransport ?)
  FARP:=nil;
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.ProcessTransport:Boolean;
{Process periodic tasks for this transport}
var
 CurrentTime:Int64;
 Config:TIPTransportConfig;
 Adapter:TIPTransportAdapter; {Note: Change to Binding when implemented}
 Current:TIPTransportAdapter;
begin
 {}
 {Flush old Hosts}
 FlushHosts(False);
  
 {Flush old Routes}
 FlushRoutes(False);
  
 {Flush old Addresses}
 FlushAddresses(False);
  
 {Flush old Fragments}
 FFragments.FlushPackets(False);
  
 {Check Adapters for Retry/Renew/Rebind/Expire}
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Get Adapter}
 Adapter:=TIPTransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
 while Adapter <> nil do
  begin
   {Get Current}
   Current:=Adapter;
   Adapter:=nil;
   
   {Note: No need to convert to write lock as this function is serialized by the caller}
   
   {Check for Unconfigured}
   if not(Current.Configured) and not(Current.Configuring) and (Current.Adapter.Status = ADAPTER_STATUS_UP) then
    begin
     {Check for Retry}
     if (Current.RetryTime <> 0) and (Current.RetryTime < CurrentTime) then
      begin
       {Configure Adapter}
       case Current.ConfigType of
        CONFIG_TYPE_STATIC:begin
          {Configure using Static information}
          Current.Address:=Current.ConfigAddress;
          Current.Netmask:=Current.ConfigNetmask;
          Current.Gateway:=Current.ConfigGateway;
          Current.Server:=Current.ConfigServer;

          {Get Config}
          Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
          while Config <> nil do
           begin
            {Check Type}
            if (Config.ConfigType = CONFIG_TYPE_STATIC) then
             begin
              {Check Handler}
              if Assigned(Config.ConfigHandler) then
               begin
                if NETWORK_LOG_ENABLED then NetworkLogInfo(nil,'IP: Configuring adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                
                {Call Request to confirm the Address}
                if Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_REQUEST) then
                 begin
                  {Set the Network and Directed}
                  Current.Network.S_addr:=(Current.Address.S_addr and Current.Netmask.S_addr);
                  Current.Directed.S_addr:=(Current.Address.S_addr or not(Current.Netmask.S_addr));
                 
                  {Add ARP Addresses (Address, Directed, Broadcast}
                  FARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  FARP.LoadAddress(Current.Adapter,Current.Directed,Current.Broadcast,ADDRESS_TYPE_BROADCAST);
                  {FARP.LoadAddress(Current.Adapter,IP_BROADCAST_ADDRESS,Current.Broadcast,ADDRESS_TYPE_BROADCAST);} {Added in ARP.AddAdapter}
                 
                  {Add RARP Address}
                  FRARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  
                  {Add Address}
                  AddAddress(Current.Address,Current.Adapter,ADDRESS_TYPE_PRIMARY,False,NETWORK_LOCK_NONE);
                  
                  {Add Routes (Network, Address, Directed)}
                  AddRoute(Current.Network,Current.Netmask,Current.Address,Current.Address,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  AddRoute(Current.Address,IP_BROADCAST_NETMASK,IP_LOOPBACK_ADDRESS,IP_LOOPBACK_ADDRESS,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  AddRoute(Current.Directed,IP_BROADCAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  
                  {Add Multicast Route}
                  AddRoute(IP_MULTICAST_NETWORK,IP_MULTICAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_MULTICAST,False,NETWORK_LOCK_NONE);
                  
                  {Add Broadcast Route}
                  AddRoute(IP_BROADCAST_NETWORK,IP_BROADCAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_BROADCAST,False,NETWORK_LOCK_NONE);
                 
                  {Add Default Route}
                  if not CompareDefault(Current.Gateway) then
                   begin
                    AddRoute(IP_DEFAULT_NETWORK,IP_DEFAULT_NETMASK,Current.Gateway,Current.Address,ROUTE_TYPE_GATEWAY,False,NETWORK_LOCK_NONE);
                   end;
                  
                  {Update Adapter}
                  Current.Configured:=True;
                  
                  {Unlock Config}
                  Config.ReaderUnlock;
                  
                  {Return Result}
                  Result:=True;
                  Break;
                 end;
               end;
             end;
             
            {Get Next} 
            Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
           end;
          
          {Check for Completed}
          if Result then
           begin
            if NETWORK_LOG_ENABLED then
             begin
              NetworkLogInfo(nil,'IP: Configuration completed for adapter ' + Current.Name);
              NetworkLogInfo(nil,'IP:  Config = ' + ConfigTypeToString(Current.ConfigType));
              NetworkLogInfo(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(Current.Address)));
              NetworkLogInfo(nil,'IP:  Netmask = ' + InAddrToString(InAddrToNetwork(Current.Netmask)));
              NetworkLogInfo(nil,'IP:  Gateway = ' + InAddrToString(InAddrToNetwork(Current.Gateway)));
              NetworkLogInfo(nil,'IP:  Server = ' + InAddrToString(InAddrToNetwork(Current.Server)));
             end;
           end
          else 
           begin
            {Reset Adapter if Config failed}
            Current.RetryTime:=GetTickCount64 + CONFIG_RETRY_TIMEOUT;
            LongWord(Current.Address.S_addr):=INADDR_ANY;
            LongWord(Current.Netmask.S_addr):=INADDR_ANY;
            LongWord(Current.Gateway.S_addr):=INADDR_ANY;
            LongWord(Current.Server.S_addr):=INADDR_BROADCAST;
           end;
         end;
        CONFIG_TYPE_LOOPBACK:begin
          {Configure using Loopback information}
          Current.Address:=IP_LOOPBACK_ADDRESS;
          Current.Netmask:=IP_LOOPBACK_NETMASK;
          Current.Gateway:=IP_DEFAULT_ADDRESS;
          Current.Server:=IP_DEFAULT_ADDRESS;
         
          {Call appropriate Config Handler}
          Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
          while Config <> nil do
           begin
            {Check Type}
            if (Config.ConfigType = CONFIG_TYPE_LOOPBACK) then
             begin
              {Check Handler}
              if Assigned(Config.ConfigHandler) then
               begin
                if NETWORK_LOG_ENABLED then NetworkLogInfo(nil,'IP: Configuring adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                
                {Call Request to confirm the Address}
                if Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_REQUEST) then
                 begin
                  {Set the Network and Directed}
                  Current.Network.S_addr:=(Current.Address.S_addr and Current.Netmask.S_addr);
                  Current.Directed.S_addr:=(Current.Address.S_addr or not(Current.Netmask.S_addr));
                  
                  {Add ARP Addresses (Address, Directed, Broadcast}
                  FARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  FARP.LoadAddress(Current.Adapter,Current.Directed,Current.Broadcast,ADDRESS_TYPE_BROADCAST);
                  {FARP.LoadAddress(Current.Adapter,IP_BROADCAST_ADDRESS,Current.Broadcast,ADDRESS_TYPE_BROADCAST);} {Added in ARP.AddAdapter}
                 
                  {Add RARP Address}
                  FRARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  
                  {Add localhost Host}
                  AddHost(IP_LOOPBACK_ADDRESS,'localhost',HOST_TYPE_LOOPBACK,False);
                  
                  {Add loopback Route}
                  AddRoute(IP_LOOPBACK_NETWORK,IP_LOOPBACK_NETMASK,IP_LOOPBACK_ADDRESS,IP_LOOPBACK_ADDRESS,ROUTE_TYPE_LOOPBACK,False,NETWORK_LOCK_NONE);
                  
                  {Add loopback Address}
                  AddAddress(IP_LOOPBACK_ADDRESS,Current.Adapter,ADDRESS_TYPE_LOOPBACK,False,NETWORK_LOCK_NONE);
                  
                  {Add loopback Network}
                  AddNetwork('loopback',IP_LOOPBACK_NETWORK,False);
                  
                  {Update Adapter}
                  Current.Configured:=True;
                  
                  {Unlock Config}
                  Config.ReaderUnlock;

                  {Return Result}
                  Result:=True;
                  Break;
                 end;
               end;
             end;
             
            {Get Next} 
            Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
           end;
           
          {Check for Completed}
          if Result then
           begin
            if NETWORK_LOG_ENABLED then
             begin
              NetworkLogInfo(nil,'IP: Configuration completed for adapter ' + Current.Name);
              NetworkLogInfo(nil,'IP:  Config = ' + ConfigTypeToString(Current.ConfigType));
              NetworkLogInfo(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(Current.Address)));
              NetworkLogInfo(nil,'IP:  Netmask = ' + InAddrToString(InAddrToNetwork(Current.Netmask)));
              NetworkLogInfo(nil,'IP:  Gateway = ' + InAddrToString(InAddrToNetwork(Current.Gateway)));
              NetworkLogInfo(nil,'IP:  Server = ' + InAddrToString(InAddrToNetwork(Current.Server)));
             end;
           end
          else 
           begin
            {Reset Adapter if Config failed}
            Current.RetryTime:=GetTickCount64 + CONFIG_RETRY_TIMEOUT;
            LongWord(Current.Address.S_addr):=INADDR_ANY;
            LongWord(Current.Netmask.S_addr):=INADDR_ANY;
            LongWord(Current.Gateway.S_addr):=INADDR_ANY;
            LongWord(Current.Server.S_addr):=INADDR_BROADCAST;
           end;
         end;
        else
         begin
          {Configure using Config Handlers}
          Current.Address:=Current.ConfigAddress;
          Current.Netmask:=Current.ConfigNetmask;
          Current.Gateway:=Current.ConfigGateway;
          Current.Server:=Current.ConfigServer;
         
          {Call appropriate Config Handler}
          Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
          while Config <> nil do
           begin
            {Check Type}
            if (Config.ConfigType = Current.ConfigType) or ((Config.ConfigAuto) and (Current.ConfigType = CONFIG_TYPE_AUTO)) then
             begin
              {Check Handler}
              if Assigned(Config.ConfigHandler) then
               begin
                if NETWORK_LOG_ENABLED then NetworkLogInfo(nil,'IP: Configuring adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                
                {Call Reboot to confirm the Address}
                if Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_REBOOT) then
                 begin
                  {Set the Type, Network and Directed}
                  Current.ConfigType:=Config.ConfigType;
                  Current.Network.S_addr:=(Current.Address.S_addr and Current.Netmask.S_addr);
                  Current.Directed.S_addr:=(Current.Address.S_addr or not(Current.Netmask.S_addr));
                  
                  {Add ARP Addresses (Address, Directed, Broadcast}
                  FARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  FARP.LoadAddress(Current.Adapter,Current.Directed,Current.Broadcast,ADDRESS_TYPE_BROADCAST);
                  {FARP.LoadAddress(Current.Adapter,IP_BROADCAST_ADDRESS,Current.Broadcast,ADDRESS_TYPE_BROADCAST);} {Added in ARP.AddAdapter}
                  
                  {Add RARP Address}
                  FRARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  
                  {Add Address}
                  AddAddress(Current.Address,Current.Adapter,ADDRESS_TYPE_PRIMARY,False,NETWORK_LOCK_NONE);
                  
                  {Add Routes (Network, Address, Directed)}
                  AddRoute(Current.Network,Current.Netmask,Current.Address,Current.Address,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  AddRoute(Current.Address,IP_BROADCAST_NETMASK,IP_LOOPBACK_ADDRESS,IP_LOOPBACK_ADDRESS,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  AddRoute(Current.Directed,IP_BROADCAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  
                  {Add Multicast Route}
                  AddRoute(IP_MULTICAST_NETWORK,IP_MULTICAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_MULTICAST,False,NETWORK_LOCK_NONE);
                  
                  {Add Broadcast Route}
                  AddRoute(IP_BROADCAST_NETWORK,IP_BROADCAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_BROADCAST,False,NETWORK_LOCK_NONE);
                  
                  {Add Default Route}
                  if not CompareDefault(Current.Gateway) then
                   begin
                    AddRoute(IP_DEFAULT_NETWORK,IP_DEFAULT_NETMASK,Current.Gateway,Current.Address,ROUTE_TYPE_GATEWAY,False,NETWORK_LOCK_NONE);
                   end;
                  
                  {Update Adapter}
                  Current.Configured:=True;
                  
                  {Unlock Config}
                  Config.ReaderUnlock;

                  {Return Result}
                  Result:=True;
                  Break;
                 end;
                {Call Discover to locate an Address}
                if Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_DISCOVER) then
                 begin
                  {Set the Type, Network and Directed}
                  Current.ConfigType:=Config.ConfigType;
                  Current.Network.S_addr:=(Current.Address.S_addr and Current.Netmask.S_addr);
                  Current.Directed.S_addr:=(Current.Address.S_addr or not(Current.Netmask.S_addr));
                  
                  {Add ARP Addresses (Address, Directed, Broadcast}
                  FARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  FARP.LoadAddress(Current.Adapter,Current.Directed,Current.Broadcast,ADDRESS_TYPE_BROADCAST);
                  {FARP.LoadAddress(Current.Adapter,IP_BROADCAST_ADDRESS,Current.Broadcast,ADDRESS_TYPE_BROADCAST);} {Added in ARP.AddAdapter}
                  
                  {Add RARP Address}
                  FRARP.LoadAddress(Current.Adapter,Current.Address,Current.Hardware,ADDRESS_TYPE_LOCAL);
                  
                  {Add Address}
                  AddAddress(Current.Address,Current.Adapter,ADDRESS_TYPE_PRIMARY,False,NETWORK_LOCK_NONE);
                  
                  {Add Routes (Network, Address, Directed)}
                  AddRoute(Current.Network,Current.Netmask,Current.Address,Current.Address,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  AddRoute(Current.Address,IP_BROADCAST_NETMASK,IP_LOOPBACK_ADDRESS,IP_LOOPBACK_ADDRESS,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  AddRoute(Current.Directed,IP_BROADCAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
                  
                  {Add Multicast Route}
                  AddRoute(IP_MULTICAST_NETWORK,IP_MULTICAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_MULTICAST,False,NETWORK_LOCK_NONE);
                  
                  {Add Broadcast Route}
                  AddRoute(IP_BROADCAST_NETWORK,IP_BROADCAST_NETMASK,Current.Address,Current.Address,ROUTE_TYPE_BROADCAST,False,NETWORK_LOCK_NONE);
                  
                  {Add Default Route}
                  if not CompareDefault(Current.Gateway) then
                   begin
                    AddRoute(IP_DEFAULT_NETWORK,IP_DEFAULT_NETMASK,Current.Gateway,Current.Address,ROUTE_TYPE_GATEWAY,False,NETWORK_LOCK_NONE);
                   end;
                  
                  {Update Adapter}
                  Current.Configured:=True;
                  
                  {Unlock Config}
                  Config.ReaderUnlock;

                  {Return Result}
                  Result:=True;
                  Break;
                 end;
               end;
             end;
            
            {Get Next}
            Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
           end;
           
          {Check for Completed}
          if Result then
           begin
            if NETWORK_LOG_ENABLED then
             begin
              NetworkLogInfo(nil,'IP: Configuration completed for adapter ' + Current.Name);
              NetworkLogInfo(nil,'IP:  Config = ' + ConfigTypeToString(Current.ConfigType));
              NetworkLogInfo(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(Current.Address)));
              NetworkLogInfo(nil,'IP:  Netmask = ' + InAddrToString(InAddrToNetwork(Current.Netmask)));
              NetworkLogInfo(nil,'IP:  Gateway = ' + InAddrToString(InAddrToNetwork(Current.Gateway)));
              NetworkLogInfo(nil,'IP:  Server = ' + InAddrToString(InAddrToNetwork(Current.Server)));
             end;
           end
          else 
           begin
            {Reset Adapter if Config failed}
            Current.RetryTime:=GetTickCount64 + CONFIG_RETRY_TIMEOUT;
            LongWord(Current.Address.S_addr):=INADDR_ANY;
            LongWord(Current.Netmask.S_addr):=INADDR_ANY;
            LongWord(Current.Gateway.S_addr):=INADDR_ANY;
            LongWord(Current.Server.S_addr):=INADDR_BROADCAST;
           end;
         end;
       end;
      end;
    end;
    
   {Check for Configured}
   if Current.Configured then
    begin
     {Check Status}
     if Current.Adapter.Status = ADAPTER_STATUS_UP then
      begin
       {Check for Lease}
       if Current.LeaseTime <> 0 then
        begin
         {Check for Expire}
         if (Current.ExpiryTime <> 0) and (Current.ExpiryTime < CurrentTime) then
          begin
           {Get Next}
           {Adapter:=TIPTransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));} {Not required}
            
           {Remove Adapter}
           {RemoveAdapter(Current.Adapter);} {Not required}
           
           {Call Config Handlers}
           Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
           while Config <> nil do
            begin
             {Check Type}
             if Config.ConfigType = Current.ConfigType then {Note: Cannot be Auto if Configured}
              begin
               {Check Handler}
               if Assigned(Config.ConfigHandler) then
                begin
                 if NETWORK_LOG_ENABLED then NetworkLogInfo(nil,'IP: Configuration expired, unconfiguring adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                 
                 {Check for Release}
                 if FAutoRelease then
                  begin
                   {Call Config Handler}
                   Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_RELEASE); {Do not check return}
                  end;
           
                 {Update Adapter}
                 Current.Configured:=False;
                 
                 {Remove Default Route}
                 if not CompareDefault(Current.Gateway) then
                  begin
                   RemoveRoute(IP_DEFAULT_NETWORK,Current.Address);
                  end;
                 
                 {Remove Broadcast Route}
                 RemoveRoute(IP_BROADCAST_NETWORK,Current.Address);
                 
                 {Remove Multicast Route}
                 RemoveRoute(IP_MULTICAST_NETWORK,Current.Address);
                 
                 {Remove Routes (Network, Address, Directed)}
                 RemoveRoute(Current.Directed,Current.Address);
                 RemoveRoute(Current.Address,IP_LOOPBACK_ADDRESS);
                 RemoveRoute(Current.Network,Current.Address);
                 
                 {Remove Address}
                 RemoveAddress(Current.Address);
                 
                 {Remove RARP Address}
                 FRARP.UnloadAddress(Current.Adapter,Current.Address);
                 
                 {Remove ARP Addresses}
                 {FARP.UnloadAddress(Current.Adapter,IP_BROADCAST_ADDRESS);} {Removed in ARP.RemoveAdapter}
                 FARP.UnloadAddress(Current.Adapter,Current.Directed);
                 FARP.UnloadAddress(Current.Adapter,Current.Address);
         
                 {Reset Adapter}
                 LongWord(Current.Address.S_addr):=INADDR_ANY;
                 LongWord(Current.Netmask.S_addr):=INADDR_ANY;
                 LongWord(Current.Gateway.S_addr):=INADDR_ANY;
                 LongWord(Current.Network.S_addr):=INADDR_ANY;
                 LongWord(Current.Directed.S_addr):=INADDR_BROADCAST;
                 
                 LongWord(Current.Server.S_addr):=INADDR_BROADCAST;
                 
                 Current.LeaseTime:=0;
                 Current.RetryTime:=GetTickCount64; {Initial Configuration}
                 Current.ExpiryTime:=0;
                 Current.RenewalTime:=0;
                 Current.RebindingTime:=0;
                 
                 Current.ConfigType:=Current.ConfigDefault;
                 
                 {Unlock Config}
                 Config.ReaderUnlock;
                 
                 Break;
                end;
              end;
              
             {Get Next} 
             Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
            end;
          end
         else
          begin
           {Check for Rebind}
           if (Current.RebindingTime <> 0) and (Current.RebindingTime < CurrentTime) then
            begin
             {Check for Retry}
             if (Current.RetryTime <> 0) and (Current.RetryTime < CurrentTime) then
              begin
               {Call Config Handlers}
               Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
               while Config <> nil do
                begin
                 {Check Type}
                 if Config.ConfigType = Current.ConfigType then {Note: Cannot be Auto if Configured}
                  begin
                   {Check Handler}
                   if Assigned(Config.ConfigHandler) then
                    begin
                     if NETWORK_LOG_ENABLED then NetworkLogInfo(nil,'IP: Rebinding adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                     
                     {Call Rebind to extend the Lease}
                     if not Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_REBIND) then
                      begin
                       if NETWORK_LOG_ENABLED then NetworkLogError(nil,'IP: Rebind failed for adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                       
                       {Set Retry if Config failed}
                       Current.RetryTime:=GetTickCount64 + CONFIG_REBIND_TIMEOUT;
                       
                       {Check Expiry (Expires immediately on NAK)}
                       if Current.ExpiryTime <= GetTickCount64 then Current.RetryTime:=GetTickCount64;
                      end;
                    end;
                  end;
                 
                 {Get Next} 
                 Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
                end;
              end;
            end
           else
            begin
             {Check for Renew}
             if (Current.RenewalTime <> 0) and (Current.RenewalTime < CurrentTime) then
              begin
               {Check for Retry}
               if (Current.RetryTime <> 0) and (Current.RetryTime < CurrentTime) then
                begin
                 {Call Config Handlers}
                 Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
                 while Config <> nil do
                  begin
                   {Check Type}
                   if Config.ConfigType = Current.ConfigType then {Note: Cannot be Auto if Configured}
                    begin
                     {Check Handler}
                     if Assigned(Config.ConfigHandler) then
                      begin
                       if NETWORK_LOG_ENABLED then NetworkLogInfo(nil,'IP: Renewing adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                        
                       {Call Renew to extend the Lease}
                       if not Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_RENEW) then
                        begin
                         if NETWORK_LOG_ENABLED then NetworkLogError(nil,'IP: Renew failed for adapter ' + Current.Name + ' with ' + ConfigTypeToString(Config.ConfigType));
                         
                         {Set Retry if Config failed}
                         Current.RetryTime:=GetTickCount64 + CONFIG_RENEW_TIMEOUT;
                         
                         {Check Expiry (Expires immediately on NAK)}
                         if Current.ExpiryTime <= GetTickCount64 then Current.RetryTime:=GetTickCount64;
                        end;
                      end;
                    end;
                    
                   {Get Next} 
                   Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
                  end;
                end;
              end;
            end;
          end;
        end;
      end
     else
      begin
       {Unconfigure on Status Down}
       if NETWORK_LOG_ENABLED then NetworkLogInfo(nil,'IP: Status change to down, unconfiguring adapter ' + Current.Name + ' with ' + ConfigTypeToString(Current.ConfigType));
       
       case Current.ConfigType of
        CONFIG_TYPE_STATIC:begin
          {Update Adapter}
          Current.Configured:=False;
      
          {Remove Default Route}
          if not CompareDefault(Current.Gateway) then
           begin
            RemoveRoute(IP_DEFAULT_NETWORK,Current.Address);
           end;
          
          {Remove Broadcast Route}
          RemoveRoute(IP_BROADCAST_NETWORK,Current.Address);
          
          {Remove Multicast Route}
          RemoveRoute(IP_MULTICAST_NETWORK,Current.Address);
          
          {Remove Routes (Network, Address, Directed)}
          RemoveRoute(Current.Directed,Current.Address);
          RemoveRoute(Current.Address,IP_LOOPBACK_ADDRESS);
          RemoveRoute(Current.Network,Current.Address);
          
          {Remove Address}
          RemoveAddress(Current.Address);
         end;
        CONFIG_TYPE_LOOPBACK:begin
          {Update Adapter}
          Current.Configured:=False;
          
          {Remove loopback Network}
          RemoveNetwork('loopback');
          
          {Remove loopback Address}
          RemoveAddress(IP_LOOPBACK_ADDRESS);
          
          {Remove loopback Route}
          RemoveRoute(IP_LOOPBACK_NETWORK,IP_LOOPBACK_ADDRESS);
          
          {Remove localhost Host}
          RemoveHost(IP_LOOPBACK_ADDRESS);
         end;
        else
         begin
          {Get Config}
          Config:=TIPTransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
          while Config <> nil do
           begin
            {Check Type}
            if Config.ConfigType = Current.ConfigType then {Note: Cannot be Auto if Configured}
             begin
              {Check Handler}
              if Assigned(Config.ConfigHandler) then
               begin
                {Check for Release}
                if FAutoRelease then
                 begin
                  {Call Config Handler}
                  Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_RELEASE); {Do not check return}
                 end; 

                {Update Adapter}
                Current.Configured:=False;
                
                {Remove Default Route}
                if not CompareDefault(Current.Gateway) then
                 begin
                  RemoveRoute(IP_DEFAULT_NETWORK,Current.Address);
                 end;
                
                {Remove Broadcast Route}
                RemoveRoute(IP_BROADCAST_NETWORK,Current.Address);
                
                {Remove Multicast Route}
                RemoveRoute(IP_MULTICAST_NETWORK,Current.Address);
                
                {Remove Routes (Network, Address, Directed)}
                RemoveRoute(Current.Directed,Current.Address);
                RemoveRoute(Current.Address,IP_LOOPBACK_ADDRESS);
                RemoveRoute(Current.Network,Current.Address);
                
                {Remove Address}
                RemoveAddress(Current.Address);
                 
                {Unlock Config}
                Config.ReaderUnlock;
                 
                Break;
               end;     
             end;
             
            {Get Next} 
            Config:=TIPTransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
           end;
         end;
       end;
          
       {Remove RARP Address}
       FRARP.UnloadAddress(Current.Adapter,Current.Address);
       
       {Remove ARP Addresses}
       {FARP.UnloadAddress(Current.Adapter,IP_BROADCAST_ADDRESS);} {Removed in ARP.RemoveAdapter}
       FARP.UnloadAddress(Current.Adapter,Current.Directed);
       FARP.UnloadAddress(Current.Adapter,Current.Address);
          
       {Reset Adapter}
       LongWord(Current.Address.S_addr):=INADDR_ANY;
       LongWord(Current.Netmask.S_addr):=INADDR_ANY;
       LongWord(Current.Gateway.S_addr):=INADDR_ANY;
       LongWord(Current.Network.S_addr):=INADDR_ANY;
       LongWord(Current.Directed.S_addr):=INADDR_BROADCAST;
       
       LongWord(Current.Server.S_addr):=INADDR_BROADCAST;
       
       Current.LeaseTime:=0;
       Current.RetryTime:=GetTickCount64; {Initial Configuration}
       Current.ExpiryTime:=0;
       Current.RenewalTime:=0;
       Current.RebindingTime:=0;
       
       Current.ConfigType:=Current.ConfigDefault;
      end;      
    end;
    
   {Get Next}
   if Adapter = nil then Adapter:=TIPTransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));
  end;
  
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TIPTransport.BindTransport(AAdapter:TNetworkAdapter):Boolean; 
{Bind this transport to an adapter if appropriate}
{Adapter: The adapter to bind to}
var
 Address:PInAddr;
 Netmask:PInAddr;
 Gateway:PInAddr;
 Server:PInAddr;
 
 ConfigType:Word;
 ConfigAddress:TInAddr;
 ConfigNetmask:TInAddr;
 ConfigGateway:TInAddr;
 ConfigServer:TInAddr;
 
 Adapter:TIPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False; 
 
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: BindTransport');
  {$ENDIF}
 
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Adapter = ' + AAdapter.Name);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   State = ' + AdapterStateToString(AAdapter.State));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   Status = ' + AdapterStatusToString(AAdapter.Status));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   Type = ' + AdapterTypeToString(AAdapter.AdapterType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   Media = ' + MediaTypeToString(AAdapter.MediaType));
  {$ENDIF}
  
  {Check State}
  if AAdapter.State <> ADAPTER_STATE_ENABLED then Exit;
  
  Result:=True;
  
  {Check Media}
  if AAdapter.MediaType <> MEDIA_TYPE_ETHERNET then Exit;
  
  {Check Type}
  if AAdapter.AdapterType = ADAPTER_TYPE_UNKNOWN then Exit;
  
  Result:=False; 
  
  {Get Adapter}
  Adapter:=TIPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter = nil then
   begin
    {Set Addresses}
    Address:=nil;
    Netmask:=nil;
    Gateway:=nil;
    Server:=nil;
    
    {Check Type}
    case AAdapter.AdapterType of
     ADAPTER_TYPE_LOOPBACK:begin
       {Add Adapter}
       Result:=AddAdapter(AAdapter,CONFIG_TYPE_LOOPBACK,Address,Netmask,Gateway,Server);
      end;
     ADAPTER_TYPE_WIRED:begin
       {Get Settings}
       {IP_CONFIG}
       ConfigType:=GetAdapterConfigType(AAdapter.Name);
       if ConfigType <> CONFIG_TYPE_AUTO then
        begin       
         {IP_ADDRESS}
         Address:=nil;
         ConfigAddress:=GetAdapterConfigAddress(AAdapter.Name);
         if not CompareDefault(ConfigAddress) and not CompareBroadcast(ConfigAddress) then
          begin
           Address:=@ConfigAddress;
          end;
         {IP_NETMASK}
         Netmask:=nil;
         ConfigNetmask:=GetAdapterConfigNetmask(AAdapter.Name);
         if not CompareDefault(ConfigNetmask) and not CompareBroadcast(ConfigNetmask) then
          begin
           Netmask:=@ConfigNetmask;
          end;
         {IP_GATEWAY}
         Gateway:=nil;
         ConfigGateway:=GetAdapterConfigGateway(AAdapter.Name);
         if not CompareDefault(ConfigGateway) and not CompareBroadcast(ConfigGateway) then
          begin
           Gateway:=@ConfigGateway;
          end;
         {IP_SERVER}
         Server:=nil;
         ConfigServer:=GetAdapterConfigServer(AAdapter.Name);
         if not CompareDefault(ConfigServer) and not CompareBroadcast(ConfigServer) then
          begin
           Server:=@ConfigServer;
          end;
        end; 
       
       {$IFDEF IP_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   ConfigType = ' + ConfigTypeToString(ConfigType));
       if NETWORK_LOG_ENABLED and (Address <> nil) then NetworkLogDebug(nil,'IP:   Address = ' + InAddrToString(InAddrToNetwork(Address^)));
       if NETWORK_LOG_ENABLED and (Netmask <> nil) then NetworkLogDebug(nil,'IP:   Netmask = ' + InAddrToString(InAddrToNetwork(Netmask^)));
       if NETWORK_LOG_ENABLED and (Gateway <> nil) then NetworkLogDebug(nil,'IP:   Gateway = ' + InAddrToString(InAddrToNetwork(Gateway^)));
       if NETWORK_LOG_ENABLED and (Server <> nil) then NetworkLogDebug(nil,'IP:   Server = ' + InAddrToString(InAddrToNetwork(Server^)));
       {$ENDIF}
  
       {Add Adapter}
       Result:=AddAdapter(AAdapter,ConfigType,Address,Netmask,Gateway,Server);
      end;     
     ADAPTER_TYPE_WIRELESS:begin 
       {Get Settings}
       {IP_CONFIG}
       ConfigType:=GetAdapterConfigType(AAdapter.Name);
       if ConfigType <> CONFIG_TYPE_AUTO then
        begin       
         {IP_ADDRESS}
         Address:=nil;
         ConfigAddress:=GetAdapterConfigAddress(AAdapter.Name);
         if not CompareDefault(ConfigAddress) and not CompareBroadcast(ConfigAddress) then
          begin
           Address:=@ConfigAddress;
          end;
         {IP_NETMASK}
         Netmask:=nil;
         ConfigNetmask:=GetAdapterConfigNetmask(AAdapter.Name);
         if not CompareDefault(ConfigNetmask) and not CompareBroadcast(ConfigNetmask) then
          begin
           Netmask:=@ConfigNetmask;
          end;
         {IP_GATEWAY}
         Gateway:=nil;
         ConfigGateway:=GetAdapterConfigGateway(AAdapter.Name);
         if not CompareDefault(ConfigGateway) and not CompareBroadcast(ConfigGateway) then
          begin
           Gateway:=@ConfigGateway;
          end;
         {IP_SERVER}
         Server:=nil;
         ConfigServer:=GetAdapterConfigServer(AAdapter.Name);
         if not CompareDefault(ConfigServer) and not CompareBroadcast(ConfigServer) then
          begin
           Server:=@ConfigServer;
          end;
        end; 

       {$IFDEF IP_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:   ConfigType = ' + ConfigTypeToString(ConfigType));
       if NETWORK_LOG_ENABLED and (Address <> nil) then NetworkLogDebug(nil,'IP:   Address = ' + InAddrToString(InAddrToNetwork(Address^)));
       if NETWORK_LOG_ENABLED and (Netmask <> nil) then NetworkLogDebug(nil,'IP:   Netmask = ' + InAddrToString(InAddrToNetwork(Netmask^)));
       if NETWORK_LOG_ENABLED and (Gateway <> nil) then NetworkLogDebug(nil,'IP:   Gateway = ' + InAddrToString(InAddrToNetwork(Gateway^)));
       if NETWORK_LOG_ENABLED and (Server <> nil) then NetworkLogDebug(nil,'IP:   Server = ' + InAddrToString(InAddrToNetwork(Server^)));
       {$ENDIF}
        
       {Add Adapter}
       Result:=AddAdapter(AAdapter,ConfigType,Address,Netmask,Gateway,Server);
      end;     
    end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.UnbindTransport(AAdapter:TNetworkAdapter):Boolean; 
{Unbind this transport from an adapter if appropriate}
{Adapter: The adapter to unbind from}
var
 Adapter:TIPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False; 
 
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: UnbindTransport');
  {$ENDIF}
 
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Adapter = ' + AAdapter.Name);
  {$ENDIF}

  {Get Adapter}
  Adapter:=TIPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter <> nil then
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;

    {Remove Adapter}
    Result:=RemoveAdapter(AAdapter);
   end
  else
   begin
    {Return Result}
    Result:=True;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddNameserver(const AAddress:TInAddr):Boolean;
{Add a nameserver IP address to the list of available nameservers}
{Address: The IP address to add}
var
 Count:Integer;
begin
 {}
 MutexLock(FNameserverLock);
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddNameserver');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  for Count:=0 to MAX_NAME_SERVERS - 1 do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + InAddrToString(InAddrToNetwork(FNameservers[Count])));
    {$ENDIF}
    {Check Nameserver}
    if CompareDefault(FNameservers[Count]) then
     begin
      {Add Nameserver}
      FNameservers[Count]:=AAddress;
      
      {Return Result}
      Result:=True;
      Exit;
     end;
   end;
 finally 
  MutexUnlock(FNameserverLock);
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveNameserver(const AAddress:TInAddr):Boolean;
{Remove a nameserver IP address from the list of available nameservers}
{Address: The IP address to remove}
var
 Count:Integer;
begin
 {}
 MutexLock(FNameserverLock);
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveNameserver');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  for Count:=0 to MAX_NAME_SERVERS - 1 do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + InAddrToString(InAddrToNetwork(FNameservers[Count])));
    {$ENDIF}
    {Check Nameserver}
    if CompareAddress(FNameservers[Count],AAddress) then
     begin
      {Remove Nameserver}
      LongWord(FNameservers[Count].S_addr):=INADDR_ANY;
      
      {Return Result}
      Result:=True;
      Exit;
     end;
   end;
 finally 
  MutexUnlock(FNameserverLock);
 end; 
end;

{==============================================================================}

function TIPTransport.GetHostByName(const AName:String;ALock:Boolean):TIPHostEntry;
{Find the name in the host cache}
{Name: The name to find}
{Lock: If True then lock the found entry before returning}
var
 Name:String;
 Host:TIPHostEntry;
begin
 {}
 FHosts.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetHostByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
  {$ENDIF}
  
  {Get Host}
  Host:=TIPHostEntry(FHosts.First);
  while Host <> nil do
   begin
    {Get Name}
    Name:=Host.Name;

    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + Name);
    {$ENDIF}
    
    {Check Name}
    if Lowercase(Name) = Lowercase(AName) then
     begin
      {Lock Host}
      if ALock then Host.AcquireLock;
     
      {Return Result}
      Result:=Host;
      Exit;
     end
    else if Lowercase(Name + AddLeadingDot(Manager.Settings.DomainName)) = Lowercase(AName) then
     begin
      {Lock Host}
      if ALock then Host.AcquireLock;
     
      {Return Result}
      Result:=Host;
      Exit;
     end
    else if Host.FindAlias(Lowercase(AName)) then
     begin
      {Lock Host}
      if ALock then Host.AcquireLock;
     
      {Return Result}
      Result:=Host;
      Exit;
     end;
    
    {Get Next} 
    Host:=TIPHostEntry(Host.Next);
   end;
 finally 
  FHosts.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetHostByAddress(const AAddress:TInAddr;ALock:Boolean):TIPHostEntry;
{Find the IP address in the host cache}
{Address: The IP address to find}
{Lock: If True then lock the found entry before returning}
var
 Host:TIPHostEntry;
begin
 {}
 FHosts.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetHostByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  {Get Host}
  Host:=TIPHostEntry(FHosts.First);
  while Host <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + InAddrToString(InAddrToNetwork(Host.Address)));
    {$ENDIF}
    
    {Check Address}
    if CompareAddress(Host.Address,AAddress) then
     begin
      {Lock Host}
      if ALock then Host.AcquireLock;
     
      {Return Result}
      Result:=Host;
      Exit;
     end
    else
     begin
      {Check Aliases}
      if Host.FindAddress(AAddress) then
       begin
        {Lock Host}
        if ALock then Host.AcquireLock;
         
        {Return Result}
        Result:=Host;
        Exit;
       end;
     end;
    
    {Get Next} 
    Host:=TIPHostEntry(Host.Next);
   end;
 finally 
  FHosts.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetHostByNext(APrevious:TIPHostEntry;ALock,AUnlock:Boolean):TIPHostEntry;
var
 Host:TIPHostEntry;
begin
 {}
 FHosts.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Host:=TIPHostEntry(FHosts.First);
    if Host <> nil then
     begin
      {Lock Host}
      if ALock then Host.AcquireLock;
      
      {Return Result}
      Result:=Host;
     end;
   end
  else
   begin
    {Get Next}
    Host:=TIPHostEntry(APrevious.Next);
    if Host <> nil then
     begin
      {Lock Host}
      if ALock then Host.AcquireLock;
      
      {Return Result}
      Result:=Host;
     end;

    {Unlock Previous}
    if AUnlock then APrevious.ReleaseLock;
   end;   
 finally 
  FHosts.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddHost(const AAddress:TInAddr;const AName:String;AType:Word;ALock:Boolean):TIPHostEntry;
{Add an IP address and name to the host cache}
{Address: The IP address of the entry to add}
{Name: The name of the entry to add}
{Type: The type of the added entry (eg HOST_TYPE_DYNAMIC)}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddHost');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Type = ' + HostTypeToString(AType));
 {$ENDIF}
  
 {Create Host}
 Result:=TIPHostEntry.Create;
 Result.Address:=AAddress;
 Result.Name:=Lowercase(AName);
 Result.HostType:=AType;

 {Acquire Lock} 
 FHosts.WriterLock; {Acquire as Writer}
 try
  {Add Host}
  if FHosts.Add(Result) then
   begin
    {Convert Lock}
    FHosts.WriterConvert; 

    {Lock Host}
    if ALock then Result.AcquireLock;
   end
  else 
   begin
    {Convert Lock}
    FHosts.WriterConvert; 

    {Free Host}
    Result.Free;
    Result:=nil;
   end;
 finally 
  FHosts.ReaderUnlock; {Converted to Reader}
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveHost(const AAddress:TInAddr):Boolean;
{Remove an IP address from the host cache}
{Address: The IP address to remove}
var
 Host:TIPHostEntry;
begin
 {}
 FHosts.ReaderLock; 
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveHost');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  {Get Host}
  Host:=TIPHostEntry(FHosts.First);
  while Host <> nil do
   begin
    {Check Address}
    if CompareAddress(Host.Address,AAddress) then
     begin
      {Lock Host}
      Host.AcquireLock;
          
      {Convert Lock}
      if FHosts.ReaderConvert then
       begin
        {Remove Host}
        Result:=FHosts.Remove(Host);
      
        {Convert Lock}
        FHosts.WriterConvert; 
      
        {Unlock Host}
        Host.ReleaseLock;
           
        {Free Host}
        Host.Free;
       end;
       
      Exit;
     end
    else
     begin
      {Check Aliases}
      if Host.FindAddress(AAddress) then
       begin
        {Lock Host}
        Host.AcquireLock;
          
        {Convert Lock}
        if FHosts.ReaderConvert then
         begin
          {Remove Host}
          Result:=FHosts.Remove(Host);
    
          {Convert Lock}
          FHosts.WriterConvert; 
          
          {Unlock Host}
          Host.ReleaseLock;
          
          {Free Host}
          Host.Free;
         end;
         
        Exit;
       end;
     end;
    
    {Get Next}
    Host:=TIPHostEntry(Host.Next);
   end;
 finally 
  FHosts.ReaderUnlock; 
 end; 
end;

{==============================================================================}

procedure TIPTransport.FlushHosts(All:Boolean);
{Flush hosts from the host cache}
{All: If True flush all hosts, otherwise flush expired hosts}
var
 CurrentTime:Int64;
 Host:TIPHostEntry;
 Current:TIPHostEntry;
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: FlushHosts');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  All = ' + BoolToStr(All));
 {$ENDIF}
  
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Get Host}
 Host:=TIPHostEntry(GetHostByNext(nil,True,False));
 while Host <> nil do
  begin
   {Get Next}
   Current:=Host;
   Host:=TIPHostEntry(GetHostByNext(Current,True,False));
    
   {Check Host Type}
   if (Current.HostType = HOST_TYPE_DYNAMIC) or (All) then
    begin
     {Check Host Expiry}
     if ((Current.HostTime + MAX_HOST_LIFE) < CurrentTime) or (All) then
      begin
       {Acquire Lock}
       FHosts.WriterLock;
        
       {Remove Host}
       FHosts.Remove(Current);
        
       {Release Lock}
       FHosts.WriterUnlock;
        
       {Unlock Host}
       Current.ReleaseLock;
        
       {Free Host}
       Current.Free;
       Current:=nil;
      end;
    end;
    
   {Unlock Host}
   if Current <> nil then Current.ReleaseLock;
  end;
end;

{==============================================================================}

function TIPTransport.GetRouteByAddress(const AAddress:TInAddr;ALock:Boolean;AState:LongWord):TIPRouteEntry;
{Find the IP route in the route cache}
{Address: The IP address to find the route for}
{Lock: If True then lock the found entry before returning}
var
 Route:TIPRouteEntry;
begin
 {}
 FRoutes.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetRouteByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  {Get Route}
  Route:=TIPRouteEntry(FRoutes.Last); {Search backwards from last (Broadcast)}
  while Route <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + InAddrToString(InAddrToNetwork(Route.Network)) + '/' + InAddrToString(InAddrToNetwork(Route.Netmask)));
    {$ENDIF}
    {Check Subnet}
    if CompareSubnet(AAddress,Route.Network,Route.Netmask) then
     begin
      {Lock Route}
      if ALock then if AState = NETWORK_LOCK_READ then Route.ReaderLock else Route.WriterLock;
     
      {Return Result}
      Result:=Route;
      Exit;
     end;
    
    {Get Next} 
    Route:=TIPRouteEntry(Route.Prev);
   end;
 finally 
  FRoutes.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetRouteByNetwork(const ANetwork,AAddress:TInAddr;ALock:Boolean;AState:LongWord):TIPRouteEntry;
{Find the IP network and address in the route cache}
{Network: The network address of the entry to find}
{Address: The IP address of the entry to find}
{Lock: If True then lock the found entry before returning}
var
 Route:TIPRouteEntry;
begin
 {}
 FRoutes.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetRouteByNetwork');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Network = ' + InAddrToString(InAddrToNetwork(ANetwork)));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  {Get Route}
  Route:=TIPRouteEntry(FRoutes.First);
  while Route <> nil do
   begin
    {Check Network}
    if CompareAddress(Route.Network,ANetwork) then
     begin
      {Check Address}
      if CompareAddress(Route.Address,AAddress) then
       begin
        {Lock Route}
        if ALock then if AState = NETWORK_LOCK_READ then Route.ReaderLock else Route.WriterLock;
       
        {Return Result}
        Result:=Route;
        Exit;
       end; 
     end;
    
    {Get Next} 
    Route:=TIPRouteEntry(Route.Next);
   end;
 finally 
  FRoutes.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetRouteByNext(APrevious:TIPRouteEntry;ALock,AUnlock:Boolean;AState:LongWord):TIPRouteEntry;
var
 Route:TIPRouteEntry;
begin
 {}
 FRoutes.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Route:=TIPRouteEntry(FRoutes.First);
    if Route <> nil then
     begin
      {Lock Route}
      if ALock then if AState = NETWORK_LOCK_READ then Route.ReaderLock else Route.WriterLock;
      
      {Return Result}
      Result:=Route;
     end;
   end
  else
   begin
    {Get Next}
    Route:=TIPRouteEntry(APrevious.Next);
    if Route <> nil then
     begin
      {Lock Route}
      if ALock then if AState = NETWORK_LOCK_READ then Route.ReaderLock else Route.WriterLock;
      
      {Return Result}
      Result:=Route;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FRoutes.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddRoute(const ANetwork,ANetmask,AGateway,AAddress:TInAddr;AType:Word;ALock:Boolean;AState:LongWord):TIPRouteEntry;
{Add an IP route to the route cache}
{Network: The network address of the entry to add}
{Netmask: The netmask of the entry to add}
{Gateway: The gateway of the entry to add}
{Address: The IP address of the entry to add}
{Type: The type of the added entry (eg ROUTE_TYPE_DYNAMIC)}
{Lock: If True then lock the added entry before returning}
var
 Route:TIPRouteEntry;
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddRoute');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Network = ' + InAddrToString(InAddrToNetwork(ANetwork)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Netmask = ' + InAddrToString(InAddrToNetwork(ANetmask)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Gateway = ' + InAddrToString(InAddrToNetwork(AGateway)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Type = ' + RouteTypeToString(AType));
 {$ENDIF}
  
 {Create Route}
 Result:=TIPRouteEntry.Create;
 Result.Network:=ANetwork;
 Result.Netmask:=ANetmask;
 Result.Gateway:=AGateway;
 Result.Address:=AAddress;
 Result.RouteType:=AType;

 {Acquire Lock} 
 FRoutes.ReaderLock; 
 try
  {Find the insertion point}
  Route:=TIPRouteEntry(FRoutes.Last);
  while Route <> nil do
   begin
    if LongWord(Result.Network.S_addr) >= LongWord(Route.Network.S_addr) then
     begin
      Break;
     end;
    
    {Get Previous}
    Route:=TIPRouteEntry(Route.Prev);
   end;
  
  {Convert Lock}
  if FRoutes.ReaderConvert then
   begin
    {Insert Route}
    if FRoutes.Insert(Route,Result) then
     begin
      {Convert Lock}
      FRoutes.WriterConvert; 
    
      {Lock Route}
      if ALock then if AState = NETWORK_LOCK_READ then Result.ReaderLock else Result.WriterLock;
     end
    else 
     begin
      {Convert Lock}
      FRoutes.WriterConvert; 
     
      {Free Route}
      Result.Free;
      Result:=nil;
     end;
   end;  
 finally 
  FRoutes.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveRoute(const ANetwork,AAddress:TInAddr):Boolean;
{Remove an IP route from the route cache}
{Network: The network address of the entry to remove}
{Address: The IP address of the entry to remove}
var
 Route:TIPRouteEntry;
begin
 {}
 FRoutes.ReaderLock; 
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveRoute');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Network = ' + InAddrToString(InAddrToNetwork(ANetwork)));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  {Get Route}
  Route:=TIPRouteEntry(FRoutes.First);
  while Route <> nil do
   begin
    {Check Network}
    if CompareAddress(Route.Network,ANetwork) then
     begin
      {Check Address}
      if CompareAddress(Route.Address,AAddress) then
       begin
        {Lock Route}
        Route.WriterLock;
        
        {Convert Lock}
        if FRoutes.ReaderConvert then
         begin
          {Remove Route}
          Result:=FRoutes.Remove(Route);
        
          {Convert Lock}
          FRoutes.WriterConvert; 
          
          {Unlock Route}
          Route.WriterUnlock;
          
          {Free Route}
          Route.Free;
         end
        else
         begin
          {Unlock Route}
          Route.WriterUnlock;
         end;         
         
        Exit;
       end;
     end;
    
    {Get Next}
    Route:=TIPRouteEntry(Route.Next);
   end;
 finally 
  FRoutes.ReaderUnlock;
 end; 
end;

{==============================================================================}

procedure TIPTransport.FlushRoutes(All:Boolean);
{Flush routes from the route cache}
{All: If True flush all routes, otherwise flush expired routes}
var
 CurrentTime:Int64;
 Route:TIPRouteEntry;
 Current:TIPRouteEntry;
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: FlushRoutes');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  All = ' + BoolToStr(All));
 {$ENDIF}
 
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Get Route}
 Route:=TIPRouteEntry(GetRouteByNext(nil,True,False,NETWORK_LOCK_READ));
 while Route <> nil do
  begin
   {Get Next}
   Current:=Route;
   Route:=TIPRouteEntry(GetRouteByNext(Current,True,False,NETWORK_LOCK_READ));
    
   {Check Route Type}
   if (Current.RouteType = ROUTE_TYPE_DYNAMIC) or (All) then
    begin
     {Check Route Expiry}
     if ((Current.RouteTime + MAX_ROUTE_LIFE) < CurrentTime) or (All) then
      begin
       {Convert Route}
       if Current.ReaderConvert then
        begin
         {Acquire Lock}
         FRoutes.WriterLock;
       
         {Remove Route}
         FRoutes.Remove(Current);

         {Release Lock}
         FRoutes.WriterUnlock;
       
         {Unlock Route}
         Current.WriterUnlock;
        
         {Free Route}
         Current.Free;
         Current:=nil;
        end; 
      end;
    end;
    
   {Unlock Route}
   if Current <> nil then Current.ReaderUnlock;
  end;
end;

{==============================================================================}

function TIPTransport.GetAddressByAddress(const AAddress:TInAddr;ALock:Boolean;AState:LongWord):TIPAddressEntry;
{Find the IP address in the address cache}
{Address: The IP address to find}
{Lock: If True then lock the found entry before returning}
var
 Address:TIPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetAddressByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  {Get Address}
  Address:=TIPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + InAddrToString(InAddrToNetwork(Address.Address)));
    {$ENDIF}
    {Check Address}
    if CompareAddress(Address.Address,AAddress) then
     begin
      {Lock Address}
      if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;
     
      {Return Result}
      Result:=Address;
      Exit;
     end;
     
    {Get Next} 
    Address:=TIPAddressEntry(Address.Next);
   end;
 finally 
  FAddresses.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetAddressByNext(APrevious:TIPAddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TIPAddressEntry;
var
 Address:TIPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Address:=TIPAddressEntry(FAddresses.First);
    if Address <> nil then
     begin
      {Lock Address}
      if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;
      
      {Return Result}
      Result:=Address;
     end;
   end
  else
   begin
    {Get Next}
    Address:=TIPAddressEntry(APrevious.Next);
    if Address <> nil then
     begin
      {Lock Address}
      if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;
      
      {Return Result}
      Result:=Address;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FAddresses.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter;AType:Word;ALock:Boolean;AState:LongWord):TIPAddressEntry;
{Add an IP address to the address cache}
{Address: The IP address to add}
{Adapter: The adapter the address is on}
{Type: The type of the added entry (eg ADDRESS_TYPE_PRIMARY)}
{Lock: If True then lock the added entry before returning}

{Note: The handling of Secondary addresses should probably change in future to use a Binding type mechanism (eg AddBinding/RemoveBinding)}
var
 Adapter:TIPTransportAdapter;
begin
 {}
 Result:=nil;
  
 {Check Adapter}
 if AAdapter = nil then Exit;
  
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Adapter = ' + AAdapter.Name);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Type = ' + AddressTypeToString(AType));
 {$ENDIF}
  
 {Check Type}
 case AType of
  ADDRESS_TYPE_SECONDARY:begin
    {Check ARP Transport}
    if FARP = nil then Exit;

    {Check RARP Transport}
    if FRARP = nil then Exit;
   
    {Get Adapter}
    Adapter:=TIPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
    if Adapter = nil then Exit;
    try
     if not CompareSubnet(AAddress,Adapter.Network,Adapter.Netmask) then Exit;
     
     {Add ARP Address}
     FARP.LoadAddress(Adapter.Adapter,AAddress,Adapter.Hardware,ADDRESS_TYPE_LOCAL);
     
     {Add RARP Address}
     FRARP.LoadAddress(Adapter.Adapter,AAddress,Adapter.Hardware,ADDRESS_TYPE_LOCAL);
     
     {Add Route}
     AddRoute(AAddress,IP_BROADCAST_NETMASK,IP_LOOPBACK_ADDRESS,IP_LOOPBACK_ADDRESS,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE);
      
     //To Do //See Binding
    finally
     Adapter.ReaderUnlock;
    end;     
   end;
 end;
  
 {Create Address}
 Result:=TIPAddressEntry.Create;
 Result.Address:=AAddress;
 Result.Adapter:=AAdapter;
 Result.AddressType:=AType;
 
 {Acquire Lock} 
 FAddresses.WriterLock; {Acquire as Writer}
 try
  {Add Address}
  if FAddresses.Add(Result) then
   begin
    {Convert Lock}
    FAddresses.WriterConvert; 

    {Lock Address}
    if ALock then if AState = NETWORK_LOCK_READ then Result.ReaderLock else Result.WriterLock;
   end
  else 
   begin
    {Convert Lock}
    FAddresses.WriterConvert; 
   
    {Free Address}
    Result.Free;
    Result:=nil;
   end;
 finally 
  FAddresses.ReaderUnlock; {Converted to Reader}
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveAddress(const AAddress:TInAddr):Boolean;
{Remove an IP address from the address cache}
{Address: The IP address to remove}
var
 Address:TIPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}
  
  {Get Address}
  Address:=TIPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {Check Address}
    if CompareAddress(Address.Address,AAddress) then
     begin
      {Check Type}
      case Address.AddressType of
       ADDRESS_TYPE_SECONDARY:begin
         {Check ARP Transport}
         if FARP = nil then Exit;

         {Check RARP Transport}
         if FRARP = nil then Exit;
         
         {Remove Route}
         RemoveRoute(Address.Address,IP_LOOPBACK_ADDRESS);
         
         {Remove RARP Address}
         FRARP.UnloadAddress(Address.Adapter,Address.Address);
         
         {Remove ARP Addresses}
         FARP.UnloadAddress(Address.Adapter,Address.Address);
         
         //To Do //See Binding
        end;
      end;
      
      {Lock Address}
      Address.WriterLock;
          
      {Convert Lock}
      if FAddresses.ReaderConvert then
       begin
        {Remove Address}
        Result:=FAddresses.Remove(Address);
      
        {Convert Lock}
        FAddresses.WriterConvert; 
      
        {Unlock Address}
        Address.WriterUnlock;
          
        {Free Address}
        Address.Free;
       end;
       
      Exit;
     end;
    
    {Get Next}
    Address:=TIPAddressEntry(Address.Next);
   end;
 finally 
  FAddresses.ReaderUnlock;
 end; 
end;

{==============================================================================}

procedure TIPTransport.FlushAddresses(All:Boolean);
{Flush addresses from the adresses cache}
{All: If True flush all addresses, otherwise flush invalid addresses}
var
 Address:TIPAddressEntry;
 Current:TIPAddressEntry;
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: FlushAddresses');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Address}
 Address:=TIPAddressEntry(GetAddressByNext(nil,True,False,NETWORK_LOCK_READ));
 while Address <> nil do
  begin
   {Get Next}
   Current:=Address;
   Address:=TIPAddressEntry(GetAddressByNext(Current,True,False,NETWORK_LOCK_READ));
    
   {Check Adapter}
   if (GetAdapterByAdapter(Current.Adapter,False,NETWORK_LOCK_NONE) = nil) or (All) then {Do not lock}
    begin
     {Check Type}
     case Current.AddressType of
      ADDRESS_TYPE_SECONDARY:begin
        {Check ARP Transport}
        if FARP = nil then Exit;

        {Check RARP Transport}
        if FRARP = nil then Exit;
        
        {Remove Route}
        RemoveRoute(Current.Address,IP_LOOPBACK_ADDRESS);
         
        {Remove RARP Address}
        FRARP.UnloadAddress(Current.Adapter,Current.Address);
         
        {Remove ARP Addresses}
        FARP.UnloadAddress(Current.Adapter,Current.Address);
         
        //To Do //See Binding
      end;
      //To Do //Special Checks for SECONDARY to make sure Subnet is still valid
                    //See Binding
     end;
      
     {Convert Address}
     if Current.ReaderConvert then
      begin
       {Acquire Lock}
       FAddresses.WriterLock;
        
       {Remove Address}
       FAddresses.Remove(Current);
        
       {Release Lock}
       FAddresses.WriterUnlock;
        
       {Unlock Address}
       Current.WriterUnlock;
        
       {Free Address}
       Current.Free;
       Current:=nil;
      end;
    end;  
    
   {Unlock Address}
   if Current <> nil then Current.ReaderUnlock;
  end;
end;

{==============================================================================}

function TIPTransport.GetNetworkByName(const AName:String;ALock:Boolean):TIPNetworkEntry;
{Find the name in the network cache}
{Name: The name to find}
{Lock: If True then lock the found entry before returning}
var
 Name:String;
 Network:TIPNetworkEntry;
begin
 {}
 FNetworks.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetNetworkByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
  {$ENDIF}
  
  {Get Network}
  Network:=TIPNetworkEntry(FNetworks.First);
  while Network <> nil do
   begin
    {Get Name}
    Name:=Network.Name;
    
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + Name);
    {$ENDIF}
    
    {Check Name}
    if Lowercase(Name) = Lowercase(AName) then
     begin
      {Lock Network}
      if ALock then Network.AcquireLock;
     
      {Return Result}
      Result:=Network;
      Exit;
     end
    else if Network.FindAlias(Lowercase(AName)) then
     begin
      {Lock Network}
      if ALock then Network.AcquireLock;
     
      {Return Result}
      Result:=Network;
      Exit;
     end;
    
    {Get Next} 
    Network:=TIPNetworkEntry(Network.Next);
   end;
 finally 
  FNetworks.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetNetworkByAddress(const ANetwork:TInAddr;ALock:Boolean):TIPNetworkEntry;
{Find the network address in the network cache}
{Network: The network address to find}
{Lock: If True then lock the found entry before returning}
var
 Network:TIPNetworkEntry;
begin
 {}
 FNetworks.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetNetworkByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Network = ' + InAddrToString(InAddrToNetwork(ANetwork)));
  {$ENDIF}
  
  {Get Network}
  Network:=TIPNetworkEntry(FNetworks.First);
  while Network <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + InAddrToString(InAddrToNetwork(Network.Network)));
    {$ENDIF}
    {Check Network}
    if CompareAddress(Network.Network,ANetwork) then
     begin
      {Lock Network}
      if ALock then Network.AcquireLock;
     
      {Return Result}
      Result:=Network;
      Exit;
     end;
    
    {Get Next} 
    Network:=TIPNetworkEntry(Network.Next);
   end;
 finally 
  FNetworks.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddNetwork(const AName:String;const ANetwork:TInAddr;ALock:Boolean):TIPNetworkEntry;
{Add a network address and name to the network cache}
{Name: The name of the entry to add}
{Network: The network address of the entry to add}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddNetwork');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Network = ' + InAddrToString(InAddrToNetwork(ANetwork)));
 {$ENDIF}
  
 {Create Network}
 Result:=TIPNetworkEntry.Create;
 Result.Name:=Lowercase(AName);
 Result.Network:=ANetwork;
  
 {Acquire Lock} 
 FNetworks.WriterLock; {Acquire as Writer}
 try
  {Add Network}
  if FNetworks.Add(Result) then
   begin
    {Convert Lock}
    FNetworks.WriterConvert; 

    {Lock Network}
    if ALock then Result.AcquireLock;
   end
  else 
   begin
    {Convert Lock}
    FNetworks.WriterConvert; 
   
    {Free Network}
    Result.Free;
    Result:=nil;
   end;
 finally 
  FNetworks.ReaderUnlock; {Converted to Reader}
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveNetwork(const AName:String):Boolean;
{Remove a network from the network cache}
{Name: The name of the network to remove}
var
 Name:String;
 Network:TIPNetworkEntry;
begin
 {}
 FNetworks.ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveNetwork');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
  {$ENDIF}
  
  {Get Network}
  Network:=TIPNetworkEntry(FNetworks.First);
  while Network <> nil do
   begin
    {Get Name}
    Name:=Network.Name;
    
    {Check Name}
    if Lowercase(Name) = Lowercase(AName) then
     begin
      {Lock Network}
      Network.AcquireLock;
          
      {Convert Lock}
      if FNetworks.ReaderConvert then
       begin
        {Remove Network}
        Result:=FNetworks.Remove(Network);
      
        {Convert Lock}
        FNetworks.WriterConvert; 
      
        {Unlock Network}
        Network.ReleaseLock;
          
        {Free Network}
        Network.Free;
       end;
       
      Exit;
     end
    else if Network.FindAlias(Lowercase(AName)) then
     begin
      {Lock Network}
      Network.AcquireLock;
          
      {Convert Lock}
      if FNetworks.ReaderConvert then
       begin
        {Remove Network}
        Result:=FNetworks.Remove(Network);
      
        {Convert Lock}
        FNetworks.WriterConvert; 
      
        {Unlock Network}
        Network.ReleaseLock;
          
        {Free Network}
        Network.Free;
       end;
       
      Exit;
     end;
    
    {Get Next}
    Network:=TIPNetworkEntry(Network.Next);
   end;
 finally 
  FNetworks.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetServByName(const AName,AProtocol:String;ALock:Boolean):TIPServEntry;
{Find the name in the service cache}
{Name: The name to find}
{Protocol: The protocol to find}
{Lock: If True then lock the found entry before returning}
var
 Serv:TIPServEntry;
begin
 {}
 FServs.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetServByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + AProtocol);
  {$ENDIF}
  
  {Get Service}
  Serv:=TIPServEntry(FServs.First);
  while Serv <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + Serv.Name);
    {$ENDIF}
    {Check Name}
    if Lowercase(Serv.Name) = Lowercase(AName) then
     begin
      {Check Protocol}
      if Lowercase(Serv.Protocol) = Lowercase(AProtocol) then
       begin
        {Lock Service}
        if ALock then Serv.AcquireLock;
     
        {Return Result}
        Result:=Serv;
        Exit;
       end;
     end;
    
    {Get Next} 
    Serv:=TIPServEntry(Serv.Next);
   end;
 finally 
  FServs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetServByPort(APort:Word;const AProtocol:String;ALock:Boolean):TIPServEntry;
{Find the port in the service cache}
{Port: The port to find}
{Protocol: The protocol to find}
{Lock: If True then lock the found entry before returning}
var
 Serv:TIPServEntry;
begin
 {}
 FServs.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetServByPort');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Port = ' + IntToStr(APort));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + AProtocol);
  {$ENDIF}
  
  {Get Service}
  Serv:=TIPServEntry(FServs.First);
  while Serv <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + IntToStr(Serv.Port));
    {$ENDIF}
    {Check Port}
    if Serv.Port = APort then
     begin
      {Check Protocol}
      if Lowercase(Serv.Protocol) = Lowercase(AProtocol) then
       begin
        {Lock Service}
        if ALock then Serv.AcquireLock;
     
        {Return Result}
        Result:=Serv;
        Exit;
       end;
     end;
    
    {Get Next} 
    Serv:=TIPServEntry(Serv.Next);
   end;
 finally 
  FServs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddServ(const AName,AProtocol:String;APort:Word;ALock:Boolean):TIPServEntry;
{Add a service to the service cache}
{Name: The name of the entry to add}
{Protocol: The protocol of the entry to add}
{Port: The port of the entry to add}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddServ');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + AProtocol);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Port = ' + IntToStr(APort));
 {$ENDIF}
  
 {Create Service}
 Result:=TIPServEntry.Create;
 Result.Name:=Lowercase(AName);
 Result.Protocol:=Lowercase(AProtocol);
 Result.Port:=APort;

 {Acquire Lock} 
 FServs.WriterLock; {Acquire as Writer}
 try
  {Add Service}
  if FServs.Add(Result) then
   begin
    {Convert Lock}
    FServs.WriterConvert; 

    {Lock Service}
    if ALock then Result.AcquireLock;
   end
  else 
   begin
    {Convert Lock}
    FServs.WriterConvert; 

    {Free Service}
    Result.Free;
    Result:=nil;
   end;
 finally 
  FServs.ReaderUnlock; {Converted to Reader}
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveServ(const AName,AProtocol:String):Boolean;
{Remove a service from the service cache}
{Name: The name of the entry to remove}
{Protocol: The protocol of the entry to remove}
var
 Serv:TIPServEntry;
begin
 {}
 FServs.ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveServ');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Protocol = ' + AProtocol);
  {$ENDIF}
  
  {Get Service}
  Serv:=TIPServEntry(FServs.First);
  while Serv <> nil do
   begin
    {Check Name}
    if Lowercase(Serv.Name) = Lowercase(AName) then
     begin
      {Check Protocol}
      if Lowercase(Serv.Protocol) = Lowercase(AProtocol) then
       begin
        {Lock Service}
        Serv.AcquireLock;
          
        {Convert Lock}
        if FServs.ReaderConvert then
         begin
          {Remove Service}
          Result:=FServs.Remove(Serv);
      
          {Convert Lock}
          FServs.WriterConvert; 
      
          {Unlock Service}
          Serv.ReleaseLock;
          
          {Free Service}
          Serv.Free;
         end;
         
        Exit;
       end;
     end;
    
    {Get Next}
    Serv:=TIPServEntry(Serv.Next);
   end;
 finally 
  FServs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetProtoByName(const AName:String;ALock:Boolean):TIPProtoEntry;
{Find the name in the protocol cache}
{Name: The name to find}
{Lock: If True then lock the found entry before returning}
var
 Proto:TIPProtoEntry;
begin
 {}
 FProtos.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetProtoByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
  {$ENDIF}
  
  {Get Protocol}
  Proto:=TIPProtoEntry(FProtos.First);
  while Proto <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + Proto.Name);
    {$ENDIF}
    {Check Name}
    if Lowercase(Proto.Name) = Lowercase(AName) then
     begin
      {Lock Protocol}
      if ALock then Proto.AcquireLock;
     
      {Return Result}
      Result:=Proto;
      Exit;
     end;
    
    {Get Next}     
    Proto:=TIPProtoEntry(Proto.Next);
   end;
 finally 
  FProtos.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.GetProtoByNumber(ANumber:Word;ALock:Boolean):TIPProtoEntry;
{Find the protocol number in the protocol cache}
{Number: The protocol number to find}
{Lock: If True then lock the found entry before returning}
var
 Proto:TIPProtoEntry;
begin
 {}
 FProtos.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: GetProtoByNumber');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Number = ' + IntToStr(ANumber));
  {$ENDIF}
  
  {Get Protocol}
  Proto:=TIPProtoEntry(FProtos.First);
  while Proto <> nil do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Compare = ' + IntToStr(Proto.Number));
    {$ENDIF}
    {Check Number}
    if Proto.Number = ANumber then
     begin
      {Lock Protocol}
      if ALock then Proto.AcquireLock;
     
      {Return Result}
      Result:=Proto;
      Exit;
     end;
     
    {Get Next}     
    Proto:=TIPProtoEntry(Proto.Next);
   end;
 finally 
  FProtos.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.AddProto(const AName:String;ANumber:Word;ALock:Boolean):TIPProtoEntry;
{Add a protocol to the protocol cache}
{Name: The name of the entry to add}
{Number: The protocol number of the entry to add}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: AddProto');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Number = ' + IntToStr(ANumber));
 {$ENDIF}
  
 {Create Protocol}
 Result:=TIPProtoEntry.Create;
 Result.Name:=Lowercase(AName);
 Result.Number:=ANumber;
 
 {Acquire Lock} 
 FProtos.WriterLock; {Acquire as Writer}
 try
  {Add Protocol}
  if FProtos.Add(Result) then
   begin
    {Convert Lock}
    FProtos.WriterConvert; 

    {Lock Protocol}
    if ALock then Result.AcquireLock;
   end
  else 
   begin
    {Convert Lock}
    FProtos.WriterConvert; 

    {Free Protocol}
    Result.Free;
    Result:=nil;
   end;
 finally 
  FProtos.ReaderUnlock; {Converted to Reader}
 end; 
end;

{==============================================================================}

function TIPTransport.RemoveProto(const AName:String):Boolean;
{Remove a protocol from the protocol cache}
{Name: The name of the entry to remove}
var
 Proto:TIPProtoEntry;
begin
 {}
 FProtos.ReaderLock;
 try
  Result:=False;
  
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: RemoveProto');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Name = ' + AName);
  {$ENDIF}
  
  {Get Protocol}
  Proto:=TIPProtoEntry(FProtos.First);
  while Proto <> nil do
   begin
    {Check Address}
    if Lowercase(Proto.Name) = Lowercase(AName) then
     begin
      {Lock Protocol}
      Proto.AcquireLock;
          
      {Convert Lock}
      if FProtos.ReaderConvert then
       begin
        {Remove Protocol}
        Result:=FProtos.Remove(Proto);
      
        {Convert Lock}
        FProtos.WriterConvert; 
      
        {Unlock Protocol}
        Proto.ReleaseLock;
          
        {Free Protocol}
        Proto.Free;
       end;
       
      Exit;
     end;
    
    {Get Next}
    Proto:=TIPProtoEntry(Proto.Next);
   end;
 finally 
  FProtos.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.CompareLocal(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the local IP addresses}
var
 Address:TIPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=True;
  
  Address:=TIPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {Check for Bound to Default Address}
    if LongWord(Address.Address.S_addr) = INADDR_ANY then Exit;
    
    {Check for Matching Address}
    if LongWord(Address.Address.S_addr) = LongWord(AAddress.S_addr) then Exit;
    
    Address:=TIPAddressEntry(Address.Next);
   end;
   
  Result:=False;
 finally 
  FAddresses.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TIPTransport.CompareDefault(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the default IP address}
begin
 {}
 Result:=(LongWord(AAddress.S_addr) = INADDR_ANY);
end;

{==============================================================================}

function TIPTransport.CompareLoopback(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the loopback IP address}
begin
 {}
 Result:=False;
 
 if LongWord(AAddress.S_addr) < LongWord(IP_LOOPBACK_NETWORK.S_addr) then Exit;
 if LongWord(AAddress.S_addr) > (LongWord(IP_LOOPBACK_NETWORK.S_addr) or not(LongWord(IP_LOOPBACK_NETMASK.S_addr))) then Exit;
 
 Result:=True;
end;

{==============================================================================}

function TIPTransport.CompareDirected(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the directed broadcast IP addresses}
var
 Adapter:TIPTransportAdapter;
begin
 {}
 Result:=True;
 
 {Get Adapter} 
 Adapter:=TIPTransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
 while Adapter <> nil do
  begin
   if LongWord(Adapter.Directed.S_addr) = LongWord(AAddress.S_addr) then
    begin
     {Unlock Adapter}
     Adapter.ReaderUnlock;
     Exit;
    end; 
    
   {Get Next} 
   Adapter:=TIPTransportAdapter(GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ));
  end;
   
 Result:=False;
end;

{==============================================================================}

function TIPTransport.CompareBroadcast(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the broadcast IP address}
begin
 {}
 Result:=True;
 
 if LongWord(AAddress.S_addr) = LongWord(IP_BROADCAST_ADDRESS.S_addr) then Exit;
 
 Result:=False;
end;

{==============================================================================}

function TIPTransport.CompareMulticast(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the multicast IP address}
begin
 {}
 Result:=False;
 
 if LongWord(AAddress.S_addr) < LongWord(IP_MULTICAST_NETWORK.S_addr) then Exit;
 if LongWord(AAddress.S_addr) > (LongWord(IP_MULTICAST_NETWORK.S_addr) or not(LongWord(IP_MULTICAST_NETMASK.S_addr))) then Exit;
 
 Result:=True;
end;

{==============================================================================}

function TIPTransport.CompareAddress(const AAddress1,AAddress2:TInAddr):Boolean;
{Compare the supplied addresses to see if they are the same}
begin
 {}
 Result:=(LongWord(AAddress1.S_addr) = LongWord(AAddress2.S_addr));
end;

{==============================================================================}

function TIPTransport.CompareSubnet(const AAddress,ANetwork,ANetmask:TInAddr):Boolean;
{Compare the supplied address to see if it is in the supplied subnet}
begin
 {}
 Result:=False;
 
 if LongWord(AAddress.S_addr) < LongWord(ANetwork.S_addr) then Exit;
 if LongWord(AAddress.S_addr) > (LongWord(ANetwork.S_addr) or not(LongWord(ANetmask.S_addr))) then Exit;
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TIPState}
constructor TIPState.Create;
begin
 {}
 inherited Create;
 FLocalAddress:=IP_DEFAULT_ADDRESS;
 FRemoteAddress:=IP_DEFAULT_ADDRESS;
end;

{==============================================================================}

procedure TIPState.SetLocalAddress(const ALocalAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FLocalAddress:=ALocalAddress;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPState.SetRemoteAddress(const ARemoteAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FRemoteAddress:=ARemoteAddress;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIPOptions}
constructor TIPOptions.Create;
begin
 {}
 inherited Create;
 FMemory:=TMemoryStream.Create;
 FOptions:=FMemory.Memory;
 FLength:=0;

 FTOS:=TOS_DEFAULT;
 FTTL:=TTL_DEFAULT;
 FFlags:=0;
 FHeader:=False;
 FMulticastIF.S_addr:=INADDR_ANY;
 FMulticastTTL:=1;      { normally limit m'casts to 1 hop  }
 FMulticastLOOP:=True;  { normally hear sends if a member  }
end;

{==============================================================================}

destructor TIPOptions.Destroy;
begin
 {}
 AcquireLock;
 try
  FMemory.Free;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

procedure TIPOptions.SetTOS(ATOS:Byte);
begin
 {}
 if not AcquireLock then Exit;

 FTOS:=ATOS;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPOptions.SetTTL(ATTL:Byte); 
begin
 {}
 if not AcquireLock then Exit;

 FTTL:=ATTL;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPOptions.SetFlags(AFlags:Word); 
begin
 {}
 if not AcquireLock then Exit;

 FFlags:=AFlags;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPOptions.SetHeader(AHeader:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FHeader:=AHeader;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPOptions.SetMulticastIF(const AMulticastIF:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FMulticastIF:=AMulticastIF;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPOptions.SetMulticastTTL(AMulticastTTL:Byte); 
begin
 {}
 if not AcquireLock then Exit;

 FMulticastTTL:=AMulticastTTL;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPOptions.SetMulticastLOOP(AMulticastLOOP:Boolean); 
begin
 {}
 if not AcquireLock then Exit;

 FMulticastLOOP:=AMulticastLOOP;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPOptions.SetLength(ALength:Integer);
var
 Remain:Integer;
begin
 {}
 if not AcquireLock then Exit;
 try
  if (ALength > 0) and (ALength <= IP_OPTIONS_SIZE) then
   begin
    {Round up to multiple of 4}
    Remain:=(ALength mod 4);
    if Remain > 0 then ALength:=ALength + (4 - Remain);
    
    {Allocate the Memory}
    FMemory.SetSize(ALength);
    FLength:=ALength;
    FOptions:=FMemory.Memory;
    
    {Zero the Memory (IPOPT_EOL)}
    FillChar(FOptions^,FLength,0);
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TIPHostEntry}
constructor TIPHostEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET;
 FLength:=SizeOf(TInAddr);
 
 FillChar(FAddresses,SizeOf(TInAddr) * MAX_HOST_ALIASES,0);
end;

{==============================================================================}

function TIPHostEntry.GetAddress:TInAddr;
begin
 {}
 Result:=FAddresses[0];
end;

{==============================================================================}

procedure TIPHostEntry.SetAddress(const AAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FAddresses[0]:=AAddress;

 ReleaseLock;
end;

{==============================================================================}

function TIPHostEntry.GetAddresses(Index:Integer):TInAddr;
begin
 {}
 Result:=FAddresses[Index];
end;

{==============================================================================}

function TIPHostEntry.FindAddress(const AAddress:TInAddr):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPHostEntry: FindAddress');
  {$ENDIF}
  
  {Get Addresses}
  for Count:=0 to MAX_HOST_ALIASES - 1 do
   begin
    {$IFDEF IP_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPHostEntry:  Compare = ' + InAddrToString(InAddrToNetwork(FAddresses[Count])));
    {$ENDIF}
    
    {Check Address}
    if LongWord(FAddresses[Count].S_addr) = LongWord(AAddress.S_addr) then
     begin
      {Return Result}
      Result:=Result;
      Exit;
     end;
   end;
 finally
  ReleaseLock;
 end; 
end;
  
{==============================================================================}

function TIPHostEntry.AddAddress(const AAddress:TInAddr):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPHostEntry: AddAddress');
  {$ENDIF}
 
  {Get Addresses}
  for Count:=0 to MAX_HOST_ALIASES - 1 do
   begin
    {Check Address}
    if LongWord(FAddresses[Count].S_addr) = INADDR_ANY then
     begin
      {Set Address}
      FAddresses[Count]:=AAddress;
     
      {Return Result}
      Result:=True;
      Exit;
     end;
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TIPHostEntry.RemoveAddress(const AAddress:TInAddr):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPHostEntry: RemoveAddress');
  {$ENDIF}
 
  {Get Addresses}
  for Count:=0 to MAX_HOST_ALIASES - 1 do
   begin
    {Check Address}
    if LongWord(FAddresses[Count].S_addr) = LongWord(AAddress.S_addr) then
     begin
      {Clear Address}
      LongWord(FAddresses[Count].S_addr):=INADDR_ANY;
      
      {Return Result}
      Result:=True;
      Exit;
     end;
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TIPRouteEntry}
constructor TIPRouteEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET;
 FLength:=SizeOf(TInAddr);
 FTOS:=TOS_DEFAULT;    {This allows Route selection based on TOS as per RFC 1122}
 FTTL:=TTL_DEFAULT;    {This allows Multicast Routes to have a default TTL of 1}
 FNetwork.S_addr:=INADDR_ANY;
 FNetmask.S_addr:=INADDR_ANY;
 FGateway.S_addr:=INADDR_ANY;
 FAddress.S_addr:=INADDR_ANY;
end;

{==============================================================================}

procedure TIPRouteEntry.SetTOS(ATOS:Byte);        
begin
 {}
 if not AcquireLock then Exit;

 FTOS:=ATOS;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPRouteEntry.SetTTL(ATTL:Byte);       
begin
 {}
 if not AcquireLock then Exit;

 FTTL:=ATTL;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPRouteEntry.SetNetwork(const ANetwork:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FNetwork:=ANetwork;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPRouteEntry.SetNetmask(const ANetmask:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FNetmask:=ANetmask;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPRouteEntry.SetGateway(const AGateway:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FGateway:=AGateway;

 ReleaseLock;
end;

{==============================================================================}

procedure TIPRouteEntry.SetAddress(const AAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIPAddressEntry}
constructor TIPAddressEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET;
 FLength:=SizeOf(TInAddr);
 
 FAddress.S_addr:=INADDR_ANY;
end;

{==============================================================================}

procedure TIPAddressEntry.SetAddress(const AAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIPNetworkEntry}
constructor TIPNetworkEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET;
 FLength:=SizeOf(TInAddr);
 
 FNetwork.S_addr:=INADDR_ANY;
end;

{==============================================================================}

procedure TIPNetworkEntry.SetNetwork(const ANetwork:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FNetwork:=ANetwork;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIPServEntry}
constructor TIPServEntry.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}
{==============================================================================}
{TIPProtoEntry}
constructor TIPProtoEntry.Create;
begin
 {}
 inherited Create;
end;
  
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure IPInit;
begin
 {}
 {Check Initialized}
 if IPInitialized then Exit;

 {Create IP Transport}
 if NetworkSettings.GetBooleanDefault('IP_TRANSPORT_ENABLED',IP_TRANSPORT_ENABLED) then 
  begin
   TIPTransport.Create(TransportManager,IP_TRANSPORT_NAME);
  end; 
 
 IPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{IP Functions}
function CheckIP(ABuffer:Pointer):Boolean;
{Verify that the packet is a valid IP packet}
{Buffer: Complete packet without Adapter header}
var
 Length:Word;
 IP:PIPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF IP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP: CheckIP');
 {$ENDIF}
 
 {Get Header}
 IP:=PIPHeader(ABuffer);
 
 {Check Version - Only Support IPv4}
 if (IP.VersionLength and $F0) = $40 then
  begin
   {Check Header Length}
   Length:=GetIPHeaderLength(IP);
   if Length >= IP_HEADER_SIZE then
    begin
     {Validate the Header Checksum}
     {$IFDEF IP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Original Checksum   = ' + IntToHex(WordBEtoN(IP.Checksum),4));
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP:  Calculated Checksum = ' + IntToHex(WordBEtoN(ChecksumIPRecv(IP,0,Length)),4));
     {$ENDIF}
     if IP.Checksum = $FFFF then IP.Checksum:=0; {Allow for all 1s case}
     if IP.Checksum = ChecksumIPRecv(IP,0,Length) then
      begin
       Result:=True;
      end;
    end;
  end;
end;

{==============================================================================}

function GetIPHeaderOffset(ABuffer:Pointer):Word;
{Return Start of the IP Header (Start of Packet)}
{Buffer: Complete packet including Transport header}
begin
 {}
 Result:=0;
end;

{==============================================================================}

function GetIPHeaderLength(ABuffer:Pointer):Word;
{Return Size of IP Header (Including Options)}
{Buffer: Complete packet including Transport header}
begin
 {} 
 Result:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
end;

{==============================================================================}

function GetIPOptionsLength(ABuffer:Pointer):Word;
{Return Size of IP Options (Header - IP_HEADER_SIZE)}
{Buffer: Complete packet including Transport header}
begin
 {} 
 Result:=((PIPHeader(ABuffer).VersionLength and $0F) shl 2) - IP_HEADER_SIZE;
end;

{==============================================================================}

function GetIPDataOffset(ABuffer:Pointer):Word;
{Return Start of IP Packet Data (Length of IP Header)}
{Buffer: Complete packet including Transport header}
begin
 {} 
 Result:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
end;

{==============================================================================}

function GetIPDataLength(ABuffer:Pointer):Word;
{Return Size of IP Packet Data (IP TotalLength - IP Header)}
{Buffer: Complete packet including Transport header}
var
 Length:Integer;
begin
 {} 
 {Get Header Length}
 Length:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
 
 {Get Data Length}
 Result:=WordBEtoN(PIPHeader(ABuffer).TotalLength) - Length;
end;

{==============================================================================}

function ChecksumIPRecv(ABuffer:Pointer;AOffset,ALength:Word):Word;
{Validate the IP Header and Options Checksum on Receive}
{Buffer: Complete packet including Transport header}
var
 IP:PIPHeader;
 Original:Word;
begin
 {}
 {Get Header}
 IP:=PIPHeader(PtrUInt(ABuffer) + AOffset);
 
 {Save Checksum}
 Original:=IP.Checksum;
 IP.Checksum:=0;
 
 {Calculate 1s Compliment Checksum on IP Header}
 Result:=GetChecksum(ABuffer,AOffset,ALength);
 
 {Restore Checksum}
 IP.Checksum:=Original;
end;

{==============================================================================}

function ChecksumIPSend(AHeader,AOptions:Pointer;ALength:Word):Word;
{Checksum the IP Header and Options on Send}
{Buffer: Complete packet including Transport header}
var
 IP:PIPHeader;
 Original:Word;
begin
 {}
 {Check Options}
 if (AOptions = nil) or (ALength = 0) then
  begin
   Result:=ChecksumIPRecv(AHeader,0,IP_HEADER_SIZE);
  end
 else
  begin
   {Get Header}
   IP:=PIPHeader(AHeader);
   
   {Save Checksum}
   Original:=IP.Checksum;
   IP.Checksum:=0;
   
   {Calculate 1s Compliment Checksum on IP Header and Options}
   Result:=GetChecksum2(AHeader,AOptions,IP_HEADER_SIZE,0,ALength);
   
   {Restore Checksum}
   IP.Checksum:=Original;
  end;
end;
  
{==============================================================================}
{==============================================================================}
{IP Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 IPInit;

{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
  