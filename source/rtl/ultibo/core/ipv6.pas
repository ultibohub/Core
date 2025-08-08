{
Ultibo IPv6 (Internet Protocol version 6) unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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


Internet Protocol version 6
===========================

 Note: In spite of the name, IPv6 is considered a transport rather than a
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

unit IPv6;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  GlobalSock,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Network,
  Transport,
  Protocol,
  Ultibo,
  UltiboUtils,
  UltiboClasses;

//To Do //IPv6 uses NDP (Neighbour Discovery Protocol) (via ICMPv6) instead of ARP
                      //See: http://keepingitclassless.net/2011/10/neighbor-solicitation-ipv6s-replacement-for-arp/

//To Do //Multicast on IPv6 is handled by MLD (Multicast Listener Discovery) (via ICMPv6) instead of IGMP
                      //See: https://en.wikipedia.org/wiki/Internet_Group_Management_Protocol
                      //     https://en.wikipedia.org/wiki/Multicast_Listener_Discovery

//To Do //Look for:

//--

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {IPv6 specific constants}
 IP6_TRANSPORT_NAME = 'IPV6';

 {IP6 Constants}
 MIN_IP6_PACKET = 40;   {Not Counting Adapter Header} //To Do
 MAX_IP6_PACKET = 1500; {Not Counting Adapter Header} //To Do

 MAX_FRAG_LIFE = 32000; {Only wait 32 seconds for the rest of the packet}

 IP6_HEADER_SIZE = 40;     {SizeOf(TIP6Header);} {Does Not Allow for Extensions}

 {IP6 Extension Headers (See: https://en.wikipedia.org/wiki/List_of_IP_protocol_numbers)}
 IP6_HEADER_HOP         = 0;    {Hop-by-hop option header}
 IP6_HEADER_TCP         = 6;    {Next header is a TCP segment (The payload)}
 IP6_HEADER_UDP         = 17;    {Next header is a UDP message (The payload)}
 IP6_HEADER_IPV6     = 41;    {Next header is a IPv6 packet (IPv6 Encapsulation) (The payload)}
 IP6_HEADER_ROUTING     = 43;    {Next header is a Routing header}
 IP6_HEADER_FRAGMENT = 44;    {Next header is a Fragmentation/reassembly header}
 IP6_HEADER_GRE         = 47;    {Next header is a GRE header}
 IP6_HEADER_ESP         = 50;    {Next header is a Encapsulating security payload}
 IP6_HEADER_AUTH     = 51;    {Next header is a Authentication header}
 IP6_HEADER_ICMP     = 58;    {Next header is a ICMP for IPv6 message}
 IP6_HEADER_NONE     = 59;    {There is no next header (Do not process payload)}
 IP6_HEADER_DEST     = 60;    {Next header is a Destination options header}
 IP6_HEADER_SCTP     = 132;    {Next header is a SCTP message (The payload)}
 IP6_HEADER_MOBILITY = 135; {Next header is a Mobility header}

 IP6_MTU = MAX_IP6_PACKET;   //To Do

{==============================================================================}
type
 {IPv6 specific types}
 PIP6Header = ^TIP6Header;
 TIP6Header = packed record   {40 Bytes}
  VersionClassLabel:LongWord; {Version (4 bits)(Always 6) / Traffic Class (8 bits) / Flow Label (20 bits)}
  PayloadLength:Word;         {The size of the payload in octets, including any extension headers (The length is set to zero when a Hop-by-Hop extension header carries a Jumbo Payload option)}
  NextHeader:Byte;            {Specifies the type of the next header (When extension headers are present in the packet this field indicates which extension header follows)}
  HopLimit:Byte;              {This value is decremented by one at each intermediate node visited by the packet. When the counter reaches 0 the packet is discarded (Replaces the time to live field of IPv4)}
  SourceIP:TIn6Addr;          {Source IP}
  DestIP:TIn6Addr;            {Destination IP}
 end;

 PIP6ExtensionHeader = ^TIP6ExtensionHeader;
 TIP6ExtensionHeader = packed record   {2 Bytes (Common Header of all Extension Headers}
  NextHeader:Byte;            {Specifies the type of the next header (If zero discard the packet)}
  ExtensionLength:Byte;
 end;

 //To Do Extension headers

 PIP6Pseudo = ^TIP6Pseudo;
 TIP6Pseudo = packed record {?? Bytes} {Used by UDP/TCP Checksum}
  SourceIP:TIn6Addr;         {Source IP}
  DestIP:TIn6Addr;           {Destination IP}
  Mbz:Byte;                  {Must Be Zero}
  Protocol:Byte;             {IP6 Protocol Type IPPROTO_UDP, IPPROTO_TCP etc}
  Length:Word;               {Length of UDP/TCP Header and Data}
 end;

 PIP6Fragment = ^TIP6Fragment;
 TIP6Fragment = record {?? Bytes} {Used by Fragment Buffer}
  Offset:LongWord;   {Offset of this Fragment in the Packet}
  Size:Word;         {Size of this Fragment}
  Prev:PIP6Fragment; {Pointer to Prev Fragment}
  Next:PIP6Fragment; {Pointer to Next Fragment}
 end;

 PIP6Packet = ^TIP6Packet;
 TIP6Packet = record {?? Bytes} {Used by Fragment Buffer}
  Lock:TMutexHandle;  {Packet Lock}

  Id:LongWord;        {IP6 Id}
  Dest:TIn6Addr;      {IP6 Dest}
  Source:TIn6Addr;    {IP6 Source}

  Data:Pointer;       {Pointer to Data}
  Timeout:Int64;      {Packet Timeout}
  Total:Word;         {IP Total Length}
  Length:Word;        {IP Header Length}
  Received:Word;      {Received Bytes}
  Prev:PIP6Packet;    {Pointer to Prev Packet}
  Next:PIP6Packet;    {Pointer to Next Packet}
  First:PIP6Fragment; {Pointer to First Fragment}
  Last:PIP6Fragment;  {Pointer to Last Fragment}
 end;

{==============================================================================}
type
 {IPv6 specific classes}
 TIP6Buffer = class(TTransportBufferEx)  {Different to Socket Buffer}
   constructor Create(ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Status Variables}
   FFirst:PIP6Packet;
   FLast:PIP6Packet;

   {Internal Methods}
   function GetFragment(APacket:PIP6Packet;AOffset:LongWord;ASize:Word):PIP6Fragment;
   function AddFragment(APacket:PIP6Packet;AOffset:LongWord;ASize:Word):PIP6Fragment;
   function RemoveFragment(APacket:PIP6Packet):Boolean;
   procedure FlushFragments(APacket:PIP6Packet);
  public
   {Public Methods}
   function GetPacket(AId:LongWord;const ASource,ADest:TIn6Addr;ALock:Boolean):PIP6Packet;
   function AddPacket(AId:LongWord;const ASource,ADest:TIn6Addr;ALock:Boolean):PIP6Packet;
   function RemovePacket(APacket:PIP6Packet):Boolean;
   function UnlockPacket(APacket:PIP6Packet):Boolean;
   procedure FlushPackets(All:Boolean);

   function PutHeader(APacket:PIP6Packet;ABuffer:Pointer;ALength:Word):Boolean;
   function PutFragment(APacket:PIP6Packet;ABuffer:Pointer;AOffset,ASize,AFlags:Word):Boolean;
 end;

 TIP6TransportAdapter = class(TTransportAdapter)
   constructor Create;
  private
   {Internal Variables}

  public
   {Status Variables}
   Address:TIn6Addr;
   Netmask:TIn6Addr;
   Gateway:TIn6Addr;
   Network:TIn6Addr;
   Directed:TIn6Addr;

   Server:TIn6Addr;      {DHCP Server}
   LeaseTime:LongWord;   {DHCP Lease Time}
   RetryTime:Int64;      {DHCP Retry Time}
   ExpiryTime:Int64;     {DHCP Expiry Time}
   RenewalTime:Int64;    {DHCP Renewal Time}
   RebindingTime:Int64;  {DHCP Rebinding Time}

   ConfigDefault:Word;   {BOOTP/DHCP/RARP/STATIC/PSEUDO/LOOPBACK}
   ConfigAddress:TIn6Addr;
   ConfigNetmask:TIn6Addr;
   ConfigGateway:TIn6Addr;
   ConfigServer:TIn6Addr;
 end;

 TIP6TransportBinding = class(TTransportBinding)
   constructor Create;
  private
   {Internal Variables}

  public
   {Status Variables}
   Address:TIn6Addr;
   Netmask:TIn6Addr;
   Gateway:TIn6Addr;
   Network:TIn6Addr;
   Directed:TIn6Addr;

   Server:TIn6Addr;      {DHCP Server}
   LeaseTime:LongWord;   {DHCP Lease Time}
   RetryTime:Int64;      {DHCP Retry Time}
   ExpiryTime:Int64;     {DHCP Expiry Time}
   RenewalTime:Int64;    {DHCP Renewal Time}
   RebindingTime:Int64;  {DHCP Rebinding Time}

   ConfigDefault:Word;   {BOOTP/DHCP/RARP/STATIC/PSEUDO/LOOPBACK}
   ConfigAddress:TIn6Addr;
   ConfigNetmask:TIn6Addr;
   ConfigGateway:TIn6Addr;
   ConfigServer:TIn6Addr;
 end;

 TIP6TransportProtocol = class(TTransportProtocol)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TIP6TransportFilter = class(TTransportFilter)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TIP6TransportConfig = class(TTransportConfig)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TIP6HostEntry = class;
 TIP6RouteEntry = class;
 TIP6AddressEntry = class;
 TIP6NetworkEntry = class;
 TIP6ServEntry = class;
 TIP6ProtoEntry = class;
 TIP6Transport = class(TNetworkTransport)
   constructor Create(AManager:TTransportManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FNextIP6Id:Word; //To do //LongWord ?
   FNextIP6Lock:TMutexHandle; //To Do //Change this to LocalLock and share with other properties

   FFragments:TIP6Buffer;

   FHosts:TNetworkList;
   FServs:TNetworkList;
   FProtos:TNetworkList;
   FRoutes:TNetworkList;
   FNetworks:TNetworkList;
   FAddresses:TNetworkList;

   {Status Variables}
   FDefaultAddress:TIn6Addr;
   FLoopbackAddress:TIn6Addr;
   FBroadcastAddress:TIn6Addr;
   FNameservers:TIP6Nameservers; //To Do //Change Nameservers to an object type (eg Transport.TNameserverEntry and TIPNameserverEntry) (Part of TNetworkList)
   FNameserverLock:TMutexHandle;                       //Then do Add/Remove/GetNameserverByNext etc)

   FDefaultHopLimit:LongWord;
   //To Do //Forwarding/ - See IP
   FAutoRelease:Boolean;

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
   function FragmentHandler(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;

   function CheckFragment(ABuffer:Pointer):Boolean;

   function GetNextIP6Id(AIncrement:Boolean):Word;

   function GetIP6Nameserver(ACount:LongWord):TIn6Addr;

   function GetAdapterConfigType(const AName:String):Word;
   function GetAdapterConfigAddress(const AName:String):TIn6Addr;
   function GetAdapterConfigNetmask(const AName:String):TIn6Addr;
   function GetAdapterConfigGateway(const AName:String):TIn6Addr;
   function GetAdapterConfigServer(const AName:String):TIn6Addr;
  protected
   {Inherited Methods}
   function FilterPacket(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean; override;
   function ForwardPacket(AAdapter:TTransportAdapter;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean; override;
  public
   {Public Properties}
   property Nameservers:TIP6Nameservers read FNameservers;

   property DefaultHopLimit:LongWord read FDefaultHopLimit write FDefaultHopLimit;

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

   {IP6 Methods}
   function AddNameserver(const AAddress:TIn6Addr):Boolean;
   function RemoveNameserver(const AAddress:TIn6Addr):Boolean;

   function GetHostByName(const AName:String;ALock:Boolean):TIP6HostEntry;
   function GetHostByAddress(const AAddress:TIn6Addr;ALock:Boolean):TIP6HostEntry;
   function GetHostByNext(APrevious:TIP6HostEntry;ALock,AUnlock:Boolean):TIP6HostEntry;
   function CheckHost(AHost:TIP6HostEntry;ALock:Boolean):Boolean;
   function AddHost(const AAddress:TIn6Addr;const AName:String;AType:Word;ALock:Boolean):TIP6HostEntry;
   function RemoveHost(const AAddress:TIn6Addr):Boolean;
   procedure FlushHosts(All:Boolean);

   function GetRouteByAddress(const AAddress:TIn6Addr;ALock:Boolean;AState:LongWord):TIP6RouteEntry;
   function GetRouteByNetwork(const ANetwork,AAddress:TIn6Addr;ALock:Boolean;AState:LongWord):TIP6RouteEntry;
   function GetRouteByNext(APrevious:TIP6RouteEntry;ALock,AUnlock:Boolean;AState:LongWord):TIP6RouteEntry;
   function CheckRoute(ARoute:TIP6RouteEntry;ALock:Boolean;AState:LongWord):Boolean;
   function AddRoute(const ANetwork,ANetmask,AGateway,AAddress:TIn6Addr;AType:Word;ALock:Boolean;AState:LongWord):TIP6RouteEntry;
   function RemoveRoute(const ANetwork,AAddress:TIn6Addr):Boolean;
   procedure FlushRoutes(All:Boolean);

   function GetAddressByAddress(const AAddress:TIn6Addr;ALock:Boolean;AState:LongWord):TIP6AddressEntry;
   function GetAddressByNext(APrevious:TIP6AddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TIP6AddressEntry;
   function CheckAddress(AAddress:TIP6AddressEntry;ALock:Boolean;AState:LongWord):Boolean;
   function AddAddress(const AAddress:TIn6Addr;AType:Word;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TIP6AddressEntry;
   function RemoveAddress(const AAddress:TIn6Addr):Boolean;
   procedure FlushAddresses(All:Boolean);

   function GetNetworkByName(const AName:String;ALock:Boolean):TIP6NetworkEntry;
   function GetNetworkByAddress(const ANetwork:TIn6Addr;ALock:Boolean):TIP6NetworkEntry;
   function AddNetwork(const AName:String;const ANetwork:TIn6Addr;ALock:Boolean):TIP6NetworkEntry;
   function RemoveNetwork(const AName:String):Boolean;

   function GetServByName(const AName,AProtocol:String;ALock:Boolean):TIP6ServEntry;
   function GetServByPort(APort:Word;const AProtocol:String;ALock:Boolean):TIP6ServEntry;
   function AddServ(const AName,AProtocol:String;APort:Word;ALock:Boolean):TIP6ServEntry;
   function RemoveServ(const AName,AProtocol:String):Boolean;

   function GetProtoByName(const AName:String;ALock:Boolean):TIP6ProtoEntry;
   function GetProtoByNumber(ANumber:Word;ALock:Boolean):TIP6ProtoEntry;
   function AddProto(const AName:String;ANumber:Word;ALock:Boolean):TIP6ProtoEntry;
   function RemoveProto(const AName:String):Boolean;

   function CompareLocal(const AAddress:TIn6Addr):Boolean;
   function CompareDefault(const AAddress:TIn6Addr):Boolean;
   function CompareLoopback(const AAddress:TIn6Addr):Boolean;
   function CompareDirected(const AAddress:TIn6Addr):Boolean;
   function CompareBroadcast(const AAddress:TIn6Addr):Boolean;
   function CompareMulticast(const AAddress:TIn6Addr):Boolean;
   function CompareAddress(const AAddress1,AAddress2:TIn6Addr):Boolean;
   function CompareSubnet(const AAddress,ANetwork,ANetmask:TIn6Addr):Boolean;
 end;

 TIP6State = class(TTransportState)
   constructor Create;
  private
   {Internal Variables}
   FLocalAddress:TIn6Addr;
   FRemoteAddress:TIn6Addr;

   {Internal Methods}
   procedure SetLocalAddress(const ALocalAddress:TIn6Addr);
   procedure SetRemoteAddress(const ARemoteAddress:TIn6Addr);
  public
   {Status Variables}
   property LocalAddress:TIn6Addr read FLocalAddress write SetLocalAddress;
   property RemoteAddress:TIn6Addr read FRemoteAddress write SetRemoteAddress;
 end;

 TIP6Options = class(TTransportOptions)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FHopLimit:Byte;               {Hop Limit}

   {IP6 Layer Variables}
   procedure SetHopLimit(AHopLimit:Byte);
  public
   {IP6 Layer Properties}
   property HopLimit:Byte read FHopLimit write SetHopLimit;

   {Public Properties}

 end;

 TIP6HostEntry = class(THostEntry)
   constructor Create;
  private
   {Internal Variables}
   FAddresses:array[0..MAX_HOST_ALIASES - 1] of TIn6Addr;

   {Internal Methods}
   function GetAddress:TIn6Addr;
   procedure SetAddress(const AAddress:TIn6Addr);
   function GetAddresses(Index:Integer):TIn6Addr;
  public
   {Public Properties}
   property Address:TIn6Addr read GetAddress write SetAddress;
   property Addresses[Index:Integer]:TIn6Addr read GetAddresses;

   {Public Methods}
   function FindAddress(const AAddress:TIn6Addr):Boolean;

   function AddAddress(const AAddress:TIn6Addr):Boolean;
   function RemoveAddress(const AAddress:TIn6Addr):Boolean;
 end;

 TIP6RouteEntry = class(TRouteEntry)
   constructor Create;
  private
   {Internal Variables}
   FNetwork:TIn6Addr;
   FNetmask:TIn6Addr;
   FGateway:TIn6Addr;
   FAddress:TIn6Addr;

   {Internal Methods}
   procedure SetNetwork(const ANetwork:TIn6Addr);
   procedure SetNetmask(const ANetmask:TIn6Addr);
   procedure SetGateway(const AGateway:TIn6Addr);
   procedure SetAddress(const AAddress:TIn6Addr);
  public
   {Status Variables}
   property Network:TIn6Addr read FNetwork write SetNetwork;
   property Netmask:TIn6Addr read FNetmask write SetNetmask;
   property Gateway:TIn6Addr read FGateway write SetGateway;
   property Address:TIn6Addr read FAddress write SetAddress;
 end;

 TIP6AddressEntry = class(TAddressEntry) {Used for secondary addresses}
   constructor Create;
  private
   {Internal Variables}
   FAddress:TIn6Addr;

   {Internal Methods}
   procedure SetAddress(const AAddress:TIn6Addr);
  public
   {Status Variables}
   property Address:TIn6Addr read FAddress write SetAddress;
 end;

 TIP6NetworkEntry = class(TNetworkEntry)
   constructor Create;
  private
   {Internal Variables}
   FNetwork:TIn6Addr;

   {Internal Methods}
   procedure SetNetwork(const ANetwork:TIn6Addr);
  public
   {Public Properties}
   property Network:TIn6Addr read FNetwork write SetNetwork;
 end;

 TIP6ServEntry = class(TServEntry)
   constructor Create;
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TIP6ProtoEntry = class(TProtoEntry)
   constructor Create;
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

{==============================================================================}
{var}
 {IPv6 specific variables}

{==============================================================================}
{Initialization Functions}
procedure IP6Init;

{==============================================================================}
{IPv6 Functions}
function CheckIP6(ABuffer:Pointer):Boolean;

function GetIP6Protocol(ABuffer:Pointer):Byte;

function GetIP6HeaderOffset(ABuffer:Pointer):Word;
function GetIP6HeaderLength(ABuffer:Pointer):Word;
function GetIP6DataOffset(ABuffer:Pointer):Word;
function GetIP6DataLength(ABuffer:Pointer):Word;

function ChecksumIP6(ABuffer:Pointer;AOffset,ALength:Word):Word; //To do //Remove //No checksums in IPv6

{==============================================================================}
{IPv6 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {IPv6 specific variables}
 IP6Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{TIP6Buffer}
constructor TIP6Buffer.Create(ATransport:TNetworkTransport);
begin
 {}
 inherited Create(ATransport);
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TIP6Buffer.Destroy;
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

function TIP6Buffer.GetFragment(APacket:PIP6Packet;AOffset:LongWord;ASize:Word):PIP6Fragment;
{Gets the Fragment with matching Offset and Size if any}

{Note: Caller must hold the Packet lock}
var
 Fragment:PIP6Fragment;
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

function TIP6Buffer.AddFragment(APacket:PIP6Packet;AOffset:LongWord;ASize:Word):PIP6Fragment;
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
  Result:=GetMem(SizeOf(TIP6Fragment)); {IP6_FRAGMENT_SIZE}
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

function TIP6Buffer.RemoveFragment(APacket:PIP6Packet):Boolean;
{Removes the First fragment from the Packet}

{Note: Caller must hold the Packet lock}
var
 Fragment:PIP6Fragment;
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
  FreeMem(Fragment,SizeOf(TIP6Fragment)); {IP6_FRAGMENT_SIZE}

  {Return Result}
  Result:=True;
 finally
  if not(WriterOwner) then ReaderUnlock else WriterUnlock;
 end;
end;

{==============================================================================}

procedure TIP6Buffer.FlushFragments(APacket:PIP6Packet);
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

function TIP6Buffer.GetPacket(AId:LongWord;const ASource,ADest:TIn6Addr;ALock:Boolean):PIP6Packet;
{Gets the Packet with matching Id, Source and Dest if any}
var
 Packet:PIP6Packet;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetPacket - Id = ' + IntToStr(LongWordBEtoN(AId)));
  {$ENDIF}

  //To Do
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Buffer.AddPacket(AId:LongWord;const ASource,ADest:TIn6Addr;ALock:Boolean):PIP6Packet;
{Adds a new Packet as the Last packet in the Buffer}
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddPacket - Id = ' + IntToStr(LongWordBEtoN(AId)));
 {$ENDIF}

 //To Do
 Result:=nil;

 {Acquire Lock}
 WriterLock; {Acquire as Writer}
 try

  //To Do

  {Convert Lock}
  WriterConvert;

  //To Do
 finally
  ReaderUnlock; {Converted to Reader}
 end;
end;

{==============================================================================}

function TIP6Buffer.RemovePacket(APacket:PIP6Packet):Boolean;
{Removes the Packet from the Buffer and release the memory}
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemovePacket');
 {$ENDIF}

 {Check Packet}
 if APacket = nil then Exit;

 {Acquire the Lock}
 MutexLock(APacket.Lock);

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
  FreeMem(APacket.Data,MAX_IP6_PACKET);

  {Free Packet Lock}
  MutexDestroy(APacket.Lock);

  {Free Packet}
  FreeMem(APacket,SizeOf(TIP6Packet)); {IP6_PACKET_SIZE}

  {Return Result}
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TIP6Buffer.UnlockPacket(APacket:PIP6Packet):Boolean;
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

procedure TIP6Buffer.FlushPackets(All:Boolean);
{Removes either all packets or packets which have expired}
var
 Packet:PIP6Packet;
 Current:PIP6Packet;
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

function TIP6Buffer.PutHeader(APacket:PIP6Packet;ABuffer:Pointer;ALength:Word):Boolean;
{Copies the Packet header from the Buffer to the Packet}

{Note: Caller must hold the Packet lock}
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: PutHeader - Length = ' + IntToStr(ALength));
  {$ENDIF}

  {Check Packet}
  if APacket = nil then Exit;

  {Check Buffer}
  if ABuffer = nil then Exit;

  //To Do
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Buffer.PutFragment(APacket:PIP6Packet;ABuffer:Pointer;AOffset,ASize,AFlags:Word):Boolean;
{Creates a new Fragment and copies the data from the Buffer to the Packet}

{Note: Caller must hold the Packet lock}
var
 Fragment:PIP6Fragment;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: PutFragment - Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check Packet}
  if APacket = nil then Exit;

  {Check Buffer}
  if ABuffer = nil then Exit;

  //To Do
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TIP6TransportAdapter}
constructor TIP6TransportAdapter.Create;
begin
 {}
 inherited Create;
 //To Do

 //LongWord(Address.S_addr):=INADDR_ANY;
 //LongWord(Netmask.S_addr):=INADDR_ANY;
 //LongWord(Gateway.S_addr):=INADDR_ANY;
 //LongWord(Network.S_addr):=INADDR_ANY;
 //LongWord(Directed.S_addr):=INADDR_BROADCAST;

 ConfigDefault:=CONFIG_TYPE_AUTO;
 //LongWord(ConfigAddress.S_addr):=INADDR_ANY;
 //LongWord(ConfigNetmask.S_addr):=INADDR_ANY;
 //LongWord(ConfigGateway.S_addr):=INADDR_ANY;
 //LongWord(ConfigServer.S_addr):=INADDR_BROADCAST;
end;

{==============================================================================}
{==============================================================================}
{TIP6TransportBinding}
constructor TIP6TransportBinding.Create;
begin
 {}
 inherited Create;
 //To Do

 //LongWord(Address.S_addr):=INADDR_ANY;
 //LongWord(Netmask.S_addr):=INADDR_ANY;
 //LongWord(Gateway.S_addr):=INADDR_ANY;
 //LongWord(Network.S_addr):=INADDR_ANY;
 //LongWord(Directed.S_addr):=INADDR_BROADCAST;

 ConfigDefault:=CONFIG_TYPE_AUTO;
 //LongWord(ConfigAddress.S_addr):=INADDR_ANY;
 //LongWord(ConfigNetmask.S_addr):=INADDR_ANY;
 //LongWord(ConfigGateway.S_addr):=INADDR_ANY;
 //LongWord(ConfigServer.S_addr):=INADDR_BROADCAST;
end;

{==============================================================================}
{==============================================================================}
{TIP6Transport}
constructor TIP6Transport.Create(AManager:TTransportManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FFamily:=AF_INET6;
 FPacketType:=PACKET_TYPE_IP6;

 FNextIP6Id:=1;
 FNextIP6Lock:=MutexCreate;

 FFragments:=TIP6Buffer.Create(Self);

 FHosts:=TNetworkList.Create;
 FServs:=TNetworkList.Create;
 FProtos:=TNetworkList.Create;
 FRoutes:=TNetworkList.Create;
 FNetworks:=TNetworkList.Create;
 FAddresses:=TNetworkList.Create;

 FillChar(FNameservers,SizeOf(TIP6Nameservers),0);
 FNameserverLock:=MutexCreate;

 FDefaultHopLimit:=HOPLIMIT_DEFAULT;

 //To Do
end;

{==============================================================================}

destructor TIP6Transport.Destroy;
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

  MutexDestroy(FNextIP6Lock);
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TIP6Transport.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by an Adapter}
{Handle: The Handle of the Transport Adapter the packet was received from}
{Source: The source hardware address of the received packet (Set by Adapter)}
{Dest: The destination hardware address of the received packet (Set by Adapter)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}

{Note: The first protocol handler to accept the packet causes processing to cease}
var
 Adapter:TIP6TransportAdapter;
 Protocol:TIP6TransportProtocol;
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}

 {Check Source}
 {if ASource = nil then Exit;} {Not Used}

 {Check Packet}
 if APacket = nil then Exit;

 {Get Adapter}
 Adapter:=TIP6TransportAdapter(GetAdapterByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  PacketType = ' + PacketTypeToString(Adapter.PacketType));
  {$ENDIF}

  {Check Packet Type}
  case Adapter.PacketType of
   PACKET_TYPE_IP6:begin
     {Check IP6 Packet}
     if CheckIP6(APacket) then
      begin
       //To Do

       //To Do //Extensions

      end
     else
      begin
       {Silently consume and discard a bad I6P packet}
       Result:=True;
      end;
    end;
  end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.FragmentHandler(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a received fragment by adding it to the buffer and if completed process the packet}
{Source: The source IP6 address of the received fragment (Set by Packet Handler)}
{Dest: The destination IP6 address of the received fragment (Set by Packet Handler)}
{Packet: The received fragment (The complete packet without Adapter header)}
{Size: The size of the received fragment in bytes}
{Broadcast: True if the destination address is a broadcast address}
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: FragmentHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}

 {Check Source}
 {if ASource = nil then Exit;} {Not Used}

 {Check Packet}
 if APacket = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIP6Transport.CheckFragment(ABuffer:Pointer):Boolean;
{Check if a packet is a fragment and return True if it is}
{Buffer: The packet to check (The complete packet without Adapter header)}
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: CheckFragment');
 {$ENDIF}

 //To do
end;

{==============================================================================}

function TIP6Transport.GetNextIP6Id(AIncrement:Boolean):Word;
{Get the next IP6 packet id number}
{Increment: If True increment the next id}
begin
 {}
 //To Do //Not required for IPv6 ?
 MutexLock(FNextIP6Lock);

 {Get Next Id}
 Result:=FNextIP6Id;

 {Increment Id}
 if AIncrement then Inc(FNextIP6Id,ID_INCREMENT);

 MutexUnlock(FNextIP6Lock);
end;

{==============================================================================}

function TIP6Transport.GetIP6Nameserver(ACount:LongWord):TIn6Addr;
{Get the nameserver address from the network settings}
begin
 {}
 Result:=IN6ADDR_ANY;

 //To Do
end;

{==============================================================================}

function TIP6Transport.GetAdapterConfigType(const AName:String):Word;
{Get the adapter config type from the network settings}
var
 Value:String;
begin
 {}
 Result:=CONFIG_TYPE_AUTO;

 Value:=Uppercase(Manager.Settings.GetString(AName + '_IP6_CONFIG'));
 if Length(Value) <> 0 then
  begin
   Result:=StrToIntDef(Value,CONFIG_TYPE_UNKNOWN);
   if (Result <> CONFIG_TYPE_STATIC) and (Result <> CONFIG_TYPE_DHCP) then
    begin
     if Value = 'STATIC' then
      begin
       Result:=CONFIG_TYPE_STATIC;
      end
     else if Value = 'DHCP' then
      begin
       Result:=CONFIG_TYPE_DHCP;
      end
     else
      begin
       Result:=CONFIG_TYPE_AUTO;
      end;
    end;
  end;
end;

{==============================================================================}

function TIP6Transport.GetAdapterConfigAddress(const AName:String):TIn6Addr;
{Get the adapter address from the network settings}
begin
 {}
 Result:=IN6ADDR_ANY;

 //To Do
end;

{==============================================================================}

function TIP6Transport.GetAdapterConfigNetmask(const AName:String):TIn6Addr;
{Get the adapter netmask from the network settings}
begin
 {}
 Result:=IN6ADDR_ANY;

 //To Do
end;

{==============================================================================}

function TIP6Transport.GetAdapterConfigGateway(const AName:String):TIn6Addr;
{Get the adapter gateway from the network settings}
begin
 {}
 Result:=IN6ADDR_ANY;

 //To Do
end;

{==============================================================================}

function TIP6Transport.GetAdapterConfigServer(const AName:String):TIn6Addr;
{Get the adapter server from the network settings}
begin
 {}
 Result:=IN6ADDR_ANY;

 //To Do
end;

{==============================================================================}

function TIP6Transport.FilterPacket(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Filter a received packet}
{Source: The source IP6 address of the received fragment (Set by Packet or Fragment Handler)}
{Dest: The destination IP6 address of the received fragment (Set by Packet or Fragment Handler)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}

{Note: If Filter returns True the Packet should be discarded}
{Note: The first filter handler to reject the packet causes filtering to cease}
var
 IP6:PIP6Header;
 Protocol:Byte;
 Filter:TIP6TransportFilter;
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: FilterPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 if ADest = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Check Packet}
 if APacket = nil then Exit;

 {Get Header}
 IP6:=PIP6Header(APacket);

 {Get Protocol}
 Protocol:=GetIP6Protocol(APacket);

 {Get Filter}
 Filter:=TIP6TransportFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
 while Filter <> nil do
  begin
   {Note: IPPROTO_IP matches any Protocol}
   if (Filter.Protocol = Protocol) or (Filter.Protocol = IPPROTO_IP) then
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
   Filter:=TIP6TransportFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
  end;
end;

{==============================================================================}

function TIP6Transport.ForwardPacket(AAdapter:TTransportAdapter;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Forward a received packet}
{Adapter: The adapter the packet was received on}
{Source: The source IP6 address of the received fragment (Set by Packet Handler)}
{Dest: The destination IP6 address of the received fragment (Set by Packet Handler)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}

{Note: Caller must hold Adapter lock}
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: ForwardPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 if ADest = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Check Packet}
 if APacket = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIP6Transport.AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean;
{Add an adapter to this transport}
{Adapter: The adapter to add}
{ConfigType: The configuration type to use for configuring the adapter (eg CONFIG_TYPE_AUTO)}
{Address: The transport address to use for this adapter (or nil if supplied during configuration)}
{Netmask: The transport netmask to use for this adapter (or nil if supplied during configuration)}
{Gateway: The transport default gateway to use for this adapter (or nil if supplied during configuration)}
{Server: The transport configuration server to use for this adapter (or nil if supplied during configuration)}
var
 Handle:THandle;
 Config:TIP6TransportConfig;
 Adapter:TIP6TransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddAdapter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Check State}
  if AAdapter.State <> ADAPTER_STATE_ENABLED then Exit;

  {Get Adapter}
  Adapter:=TIP6TransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter = nil then
   begin
    {Add IP6 Type}
    Handle:=AAdapter.AddTransport(PACKET_TYPE_IP6,FRAME_TYPE_ETHERNET_II,IP6_TRANSPORT_NAME,PacketHandler);
    if Handle <> INVALID_HANDLE_VALUE then
     begin
      {Create Adapter}
      Adapter:=TIP6TransportAdapter.Create;
      Adapter.Name:=AAdapter.Name;
      Adapter.Handle:=Handle;
      Adapter.PacketType:=PACKET_TYPE_IP6;
      Adapter.Adapter:=AAdapter;
      Adapter.Hardware:=AAdapter.GetHardwareAddress(Handle);
      Adapter.Broadcast:=AAdapter.GetBroadcastAddress(Handle);
      Adapter.MTU:=AAdapter.GetMTU(Handle);
      Adapter.ConfigType:=AConfigType;
      Adapter.Configured:=False;
      Adapter.Configuring:=False;
      Adapter.ConfigDefault:=AConfigType;
      if AAddress <> nil then Adapter.ConfigAddress:=TIn6Addr(AAddress^);
      if ANetmask <> nil then Adapter.ConfigNetmask:=TIn6Addr(ANetmask^);
      if AGateway <> nil then Adapter.ConfigGateway:=TIn6Addr(AGateway^);
      if AServer <> nil then Adapter.ConfigServer:=TIn6Addr(AServer^);

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

function TIP6Transport.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
{Remove an adapter from this transport}
{Adapter: The adapter to remove}
var
 Config:TIP6TransportConfig;
 Adapter:TIP6TransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveAdapter');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Adapter}
  Adapter:=TIP6TransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to configure}
  if Adapter = nil then Exit;

  {Notify Config of Remove}
  case Adapter.ConfigType of
   CONFIG_TYPE_STATIC:begin
     {Update Adapter}
     Adapter.Configured:=False;

     //To Do

    end;
   CONFIG_TYPE_LOOPBACK:begin
     {Update Adapter}
     Adapter.Configured:=False;

     //To Do

    end;
   else
    begin
     {Get Config}
     Config:=TIP6TransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
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

               //To Do

               Break;
              end;
            end
           else
            begin
             {Update Adapter}
             Adapter.Configured:=False;

             //To Do


             Break;
            end;
          end;
        end;

       {Get Next}
       Config:=TIP6TransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
      end;
    end;
  end;

  //To Do

  {Remove IP6 Type}
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

function TIP6Transport.AddProtocol(AProtocol:Word;APacketHandler:TTransportPacketHandler;AControlHandler:TTransportControlHandler):THandle;
{Add a protocol to this transport}
{Protocol: The protocol type to add}
{PacketHandler: Packet handler to call on matching protocol type}
{ControlHandler: Control handler to call on error condition}
var
 Protocol:TIP6TransportProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddProtocol');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Get Protocol}
  {Protocol:=TIP6TransportProtocol(GetProtocolByType(AProtocol,False,NETWORK_LOCK_NONE));} {Do not lock} {Allow multiple protocols with same type}
  {if Protocol <> nil then Exit;}

  {Create Protocol}
  Protocol:=TIP6TransportProtocol.Create;
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

function TIP6Transport.RemoveProtocol(AHandle:THandle;AProtocol:Word):Boolean;
{Remove a protocol from this transport}
{Handle: Handle of the protocol to remove}
{Protocol: The protocol type to remove}
var
 Protocol:TIP6TransportProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveProtocol');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Get Protocol}
  Protocol:=TIP6TransportProtocol(GetProtocolByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
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

function TIP6Transport.AddFilter(AProtocol:Word;AFilterHandler:TTransportFilterHandler):THandle;
{Add a filter to this transport}
{Protocol: The protocol type of the filter to add}
{FilterHandler: Filter handler to call on matching protocol type}
var
 Filter:TIP6TransportFilter;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddFilter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Get Filter}
  Filter:=TIP6TransportFilter(GetFilterByProtocol(AProtocol,False,NETWORK_LOCK_NONE)); {Do not lock}
  if Filter <> nil then Exit;

  {Create Filter}
  Filter:=TIP6TransportFilter.Create;
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

function TIP6Transport.RemoveFilter(AHandle:THandle;AProtocol:Word):Boolean;
{Remove a filter from this transport}
{Handle: Handle of the filter to remove}
{Protocol: The protocol type of the filter to remove}
var
 Filter:TIP6TransportFilter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveFilter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Get Filter}
  Filter:=TIP6TransportFilter(GetFilterByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
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

function TIP6Transport.AddConfig(AConfigType:Word;AConfigAuto:Boolean;AConfigHandler:TTransportConfigHandler):THandle;
{Add a config to this transport}
{ConfigType: The config type to add}
{ConfigAuto: True if this config supports auto configuration type}
{ConfigHandler: Config handler to call on matching config type}
var
 Config:TIP6TransportConfig;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddConfig');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}

  {Get Config}
  Config:=TIP6TransportConfig(GetConfigByType(AConfigType,False,NETWORK_LOCK_NONE)); {Do not lock}
  if Config <> nil then Exit;

  {Create Config}
  Config:=TIP6TransportConfig.Create;
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

function TIP6Transport.RemoveConfig(AHandle:THandle;AConfigType:Word):Boolean;
{Remove a config from this transport}
{Handle: Handle of the config to remove}
{ConfigType: The config type to remove}
var
 Config:TIP6TransportConfig;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveConfig');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}

  {Get Config}
  Config:=TIP6TransportConfig(GetConfigByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
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

function TIP6Transport.SendPacket(ASocket:TTransportSocket;ASource,ADest:Pointer;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
{Send a Packet by adding the Transport Header and other details to the Data}
{Socket: }
{Source: The source IP6 address of the packet}
{Dest: The destination IP6 address of the packet}
{Packet: The packet data to send}
{Size: }
{Flags: }

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: SendPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Size = ' + IntToStr(ASize));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Dest = ' + In6AddrToString(PIn6Addr(ADest)^));
 {$ENDIF}

 {Check Dest}
 if ADest = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Check Socket}
 if ASocket = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIP6Transport.SendControl(ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;

{Note: The first control handler to accept the packet causes sending to cease}
var
 Protocol:TIP6TransportProtocol;
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: SendControl');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Get Protocol}
 Protocol:=TIP6TransportProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
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
   Protocol:=TIP6TransportProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
  end;
end;

{==============================================================================}

function TIP6Transport.GetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
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

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetSockOpt');
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Level}
 case ALevel of
  IPPROTO_IPV6:begin
    NetworkSetLastError(WSAENOPROTOOPT);

    //To Do

   end;
  else
   begin
    Result:=ASocket.GetOption(ALevel,AOptName,AOptValue,AOptLength);
   end;
 end;
end;

{==============================================================================}

function TIP6Transport.SetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
{BSD compatible Set Socket Option}
{Socket: The socket to set the option for}
{Level: The protocol level for the option}
{OptName: The name of the option to set}
{OptValue: The value of the option}
{OptLength: The length of the option}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: SetSockOpt');
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Level}
 case ALevel of
  IPPROTO_IPV6:begin
    NetworkSetLastError(WSAENOPROTOOPT);

    //To Do

   end;
  else
   begin
    Result:=ASocket.SetOption(ALevel,AOptName,AOptValue,AOptLength);
   end;
 end;
end;

{==============================================================================}

function TIP6Transport.StartTransport:Boolean;
{Start this transport ready for sending and receiving}
var
 Count:LongWord;
 Nameserver:TIn6Addr;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: StartTransport');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Add Nameservers}
  for Count:=1 to MAX_NAME_SERVERS do
   begin
    //To Do
   end;

  {Return Result}
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.StopTransport:Boolean;
{Stop this transport ready for removal}
var
 Current:TIP6TransportAdapter;
 Adapter:TIP6TransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: StopTransport');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Get Adapter}
  Adapter:=TIP6TransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Get Next}
    Current:=Adapter;
    Adapter:=TIP6TransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));

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

  {Return Result}
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.ProcessTransport:Boolean;
{Process periodic tasks for this transport}
var
 CurrentTime:Int64;
 Config:TIP6TransportConfig;
 Adapter:TIP6TransportAdapter; {Note: Change to Binding when implemented}
 Current:TIP6TransportAdapter;
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
 Adapter:=TIP6TransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
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

       //To Do - See AddAdapter //See IP.ProcessTransport
       //How to get the Config params that were passed to AddAdapter ?? //Need to save them somewhere - Done (ConfigAddress/ConfigNetmask etc etc)

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
         {Check for Renew}
         if (Current.RenewalTime <> 0) and (Current.RenewalTime < CurrentTime) then
          begin
           {Call Config Handlers}
           Config:=TIP6TransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
           while Config <> nil do
            begin
             if Config.ConfigType = Current.ConfigType then {Note: Cannot be Auto if Configured}
              begin
               if Assigned(Config.ConfigHandler) then
                begin

                 //To Do //Must implement a Retry Count and/or a Timeout backoff

                 Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_RENEW);
                end;
              end;
             Config:=TIP6TransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
            end;
          end;

         {Check for Rebind}
         if (Current.RebindingTime <> 0) and (Current.RebindingTime < CurrentTime) then
          begin
           {Call Config Handlers}
           Config:=TIP6TransportConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
           while Config <> nil do
            begin
             if Config.ConfigType = Current.ConfigType then {Note: Cannot be Auto if Configured}
              begin
               if Assigned(Config.ConfigHandler) then
                begin

                 //To Do //Must implement a Retry Count and/or a Timeout backoff

                 Config.ConfigHandler(THandle(Config),Current,CONFIG_ADAPTER_REBIND);
                end;
              end;
             Config:=TIP6TransportConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
            end;
          end;

         {Check for Expire}
         if (Current.ExpiryTime <> 0) and (Current.ExpiryTime < CurrentTime) then
          begin
           {Get Next}
           Adapter:=TIP6TransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));

           {Remove Adapter}
           RemoveAdapter(Current.Adapter);
          end;
        end;
      end
     else
      begin
       //To do //Unconfigure

       //Current.ConfigType:=Current.ConfigDefault;
      end;
    end;

   {Get Next}
   if Adapter = nil then Adapter:=TIP6TransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));
  end;

 {Return Result}
 Result:=False;
end;

{==============================================================================}

function TIP6Transport.BindTransport(AAdapter:TNetworkAdapter):Boolean;
{Bind this transport to an adapter if appropriate}
{Adapter: The adapter to bind to}
var
 Address:PIn6Addr;
 Netmask:PIn6Addr;
 Gateway:PIn6Addr;
 Server:PIn6Addr;

 ConfigType:Word;
 ConfigAddress:TIn6Addr;
 ConfigNetmask:TIn6Addr;
 ConfigGateway:TIn6Addr;
 ConfigServer:TIn6Addr;

 Adapter:TIP6TransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: BindTransport');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Adapter = ' + AAdapter.Name);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:   State = ' + AdapterStateToString(AAdapter.State));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:   Status = ' + AdapterStatusToString(AAdapter.Status));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:   Type = ' + AdapterTypeToString(AAdapter.AdapterType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:   Media = ' + MediaTypeToString(AAdapter.MediaType));
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
  Adapter:=TIP6TransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
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
       {IP6_CONFIG}
       ConfigType:=GetAdapterConfigType(AAdapter.Name);
       if ConfigType <> CONFIG_TYPE_AUTO then
        begin
         //To Do

        end;

       {Add Adapter}
       Result:=AddAdapter(AAdapter,ConfigType,Address,Netmask,Gateway,Server);
      end;
     ADAPTER_TYPE_WIRELESS:begin
       {Get Settings}
       {IP6_CONFIG}
       ConfigType:=GetAdapterConfigType(AAdapter.Name);
       if ConfigType <> CONFIG_TYPE_AUTO then
        begin
         //To Do

        end;

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

function TIP6Transport.UnbindTransport(AAdapter:TNetworkAdapter):Boolean;
{Unbind this transport from an adapter if appropriate}
{Adapter: The adapter to unbind from}
var
 Adapter:TIP6TransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: UnbindTransport');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Adapter = ' + AAdapter.Name);
  {$ENDIF}

  {Get Adapter}
  Adapter:=TIP6TransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
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

function TIP6Transport.AddNameserver(const AAddress:TIn6Addr):Boolean;
{Add a nameserver IP6 address to the list of available nameservers}
{Address: The IP6 address to add}
var
 Count:Integer;
begin
 {}
 MutexLock(FNameserverLock);
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddNameserver');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  for Count:=0 to MAX_NAME_SERVERS - 1 do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + In6AddrToString(FNameservers[Count]));
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

function TIP6Transport.RemoveNameserver(const AAddress:TIn6Addr):Boolean;
{Remove a nameserver IP6 address from the list of available nameservers}
{Address: The IP6 address to remove}
var
 Count:Integer;
begin
 {}
 MutexLock(FNameserverLock);
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveNameserver');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  for Count:=0 to MAX_NAME_SERVERS - 1 do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + In6AddrToString(FNameservers[Count]));
    {$ENDIF}
    {Check Nameserver}
    if CompareAddress(FNameservers[Count],AAddress) then
     begin
      {Remove Nameserver}
      //LongWord(FNameservers[Count].S_addr):=INADDR_ANY; //To Do

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

function TIP6Transport.GetHostByName(const AName:String;ALock:Boolean):TIP6HostEntry;
{Find the name in the host cache}
{Name: The name to find}
{Lock: If True then lock the found entry before returning}
var
 Name:String;
 Host:TIP6HostEntry;
begin
 {}
 FHosts.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetHostByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
  {$ENDIF}

  {Get Host}
  Host:=TIP6HostEntry(FHosts.First);
  while Host <> nil do
   begin
    {Get Name}
    Name:=Host.Name;

    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + Name);
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
    Host:=TIP6HostEntry(Host.Next);
   end;
 finally
  FHosts.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetHostByAddress(const AAddress:TIn6Addr;ALock:Boolean):TIP6HostEntry;
{Find the IP6 address in the host cache}
{Address: The IP6 address to find}
{Lock: If True then lock the found entry before returning}
var
 Host:TIP6HostEntry;
begin
 {}
 FHosts.ReaderLock;
 try
  Result:=nil;
  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetHostByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  {Get Host}
  Host:=TIP6HostEntry(FHosts.First);
  while Host <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + In6AddrToString(Host.Address));
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
    Host:=TIP6HostEntry(Host.Next);
   end;
 finally
  FHosts.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetHostByNext(APrevious:TIP6HostEntry;ALock,AUnlock:Boolean):TIP6HostEntry;
var
 Host:TIP6HostEntry;
begin
 {}
 if not(FHosts.WriterOwner) then FHosts.ReaderLock else FHosts.WriterLock;
 try
  Result:=nil;

  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Host:=TIP6HostEntry(FHosts.First);
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
    Host:=TIP6HostEntry(APrevious.Next);
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
  if not(FHosts.WriterOwner) then FHosts.ReaderUnlock else FHosts.WriterUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.CheckHost(AHost:TIP6HostEntry;ALock:Boolean):Boolean;
{Check a host entry in the host cache}
{Host: The host entry to check}
{Lock: If True then lock the found entry before returning}

{Note: This allows safely obtaining a lock on an existing object in case it has been freed}
var
 Host:TIP6HostEntry;
begin
 {}
 if not(FHosts.WriterOwner) then FHosts.ReaderLock else FHosts.WriterLock;
 try
  Result:=False;

  {Check Host}
  if AHost = nil then Exit;

  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: CheckHost');
  {$ENDIF}

  {Get Host}
  Host:=TIP6HostEntry(FHosts.First);
  while Host <> nil do
   begin
    {Check Host}
    if Host = AHost then
     begin
      {Lock Host}
      if ALock then Host.AcquireLock;

      {Return Result}
      Result:=True;
      Exit;
     end;

    {Get Next}
    Host:=TIP6HostEntry(Host.Next);
   end;
 finally
  if not(FHosts.WriterOwner) then FHosts.ReaderUnlock else FHosts.WriterUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.AddHost(const AAddress:TIn6Addr;const AName:String;AType:Word;ALock:Boolean):TIP6HostEntry;
{Add an IP6 address and name to the host cache}
{Address: The IP6 address of the entry to add}
{Name: The name of the entry to add}
{Type: The type of the added entry (eg HOST_TYPE_DYNAMIC)}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddHost');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Type = ' + HostTypeToString(AType));
 {$ENDIF}

 {Create Host}
 Result:=TIP6HostEntry.Create;
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

function TIP6Transport.RemoveHost(const AAddress:TIn6Addr):Boolean;
{Remove an IP6 address from the host cache}
{Address: The IP6 address to remove}
var
 Host:TIP6HostEntry;
begin
 {}
 FHosts.ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveHost');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  {Get Host}
  Host:=TIP6HostEntry(FHosts.First);
  while Host <> nil do
   begin
    {Check Address}
    if CompareAddress(Host.Address,AAddress) then
     begin
      {Lock Host}
      {Host.AcquireLock;} {Must be after acquiring writer lock}

      {Convert Lock}
      if FHosts.ReaderConvert then
       begin
        {Lock Host}
        if CheckHost(Host,True) then
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
       end;

      Exit;
     end
    else
     begin
      {Check Aliases}
      if Host.FindAddress(AAddress) then
       begin
        {Lock Host}
        {Host.AcquireLock;} {Must be after acquiring writer lock}

        {Convert Lock}
        if FHosts.ReaderConvert then
         begin
          {Lock Host}
          if CheckHost(Host,True) then
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
         end;

        Exit;
       end;
     end;

    {Get Next}
    Host:=TIP6HostEntry(Host.Next);
   end;
 finally
  if not(FHosts.WriterOwner) then FHosts.ReaderUnlock else FHosts.WriterUnlock;
 end;
end;

{==============================================================================}

procedure TIP6Transport.FlushHosts(All:Boolean);
{Flush hosts from the host cache}
{All: If True flush all hosts, otherwise flush expired hosts}
var
 CurrentTime:Int64;
 Host:TIP6HostEntry;
 Current:TIP6HostEntry;
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: FlushHosts');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Tick Count}
 CurrentTime:=GetTickCount64;

 {Get Host}
 Host:=GetHostByNext(nil,True,False);
 while Host <> nil do
  begin
   {Check Host Type and Expiry}
   if ((Host.HostType = HOST_TYPE_DYNAMIC) and ((Host.HostTime + MAX_HOST_LIFE) < CurrentTime)) or (All) then
    begin
     {Unlock Host (While waiting for writer lock)}
     Host.ReleaseLock;

     {Acquire Lock}
     FHosts.WriterLock;

     {Lock Host}
     if CheckHost(Host,True) then
      begin
       {Save Host}
       Current:=Host;

       {Get Next}
       Host:=GetHostByNext(Current,True,False);

       {Remove Host}
       FHosts.Remove(Current);

       {Release Lock}
       FHosts.WriterUnlock;

       {Unlock Host}
       Current.ReleaseLock;

       {Free Host}
       Current.Free;
       Current:=nil;
      end
     else
      begin
       {Release Lock}
       FHosts.WriterUnlock;

       {Get Next}
       if All then Host:=GetHostByNext(nil,True,False);
      end;
    end
   else
    begin
     {Get Next}
     Host:=GetHostByNext(Host,True,True);
    end;
  end;
end;

{==============================================================================}

function TIP6Transport.GetRouteByAddress(const AAddress:TIn6Addr;ALock:Boolean;AState:LongWord):TIP6RouteEntry;
{Find the IP6 route in the route cache}
{Address: The IP6 address to find the route for}
{Lock: If True then lock the found entry before returning}
var
 Route:TIP6RouteEntry;
begin
 {}
 FRoutes.ReaderLock;
 try
  Result:=nil;
  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetRouteByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  {Get Route}
  Route:=TIP6RouteEntry(FRoutes.Last); {Search backwards from last (Broadcast)}
  while Route <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + In6AddrToString(Route.Network) + '/' + In6AddrToString(Route.Netmask));
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
    Route:=TIP6RouteEntry(Route.Prev);
   end;
 finally
  FRoutes.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetRouteByNetwork(const ANetwork,AAddress:TIn6Addr;ALock:Boolean;AState:LongWord):TIP6RouteEntry;
{Find the IP6 network and address in the route cache}
{Network: The network address of the entry to find}
{Address: The IP6 address of the entry to find}
{Lock: If True then lock the found entry before returning}
var
 Route:TIP6RouteEntry;
begin
 {}
 FRoutes.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetRouteByNetwork');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Network = ' + In6AddrToString(ANetwork));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  {Get Route}
  Route:=TIP6RouteEntry(FRoutes.First);
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
    Route:=TIP6RouteEntry(Route.Next);
   end;
 finally
  FRoutes.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetRouteByNext(APrevious:TIP6RouteEntry;ALock,AUnlock:Boolean;AState:LongWord):TIP6RouteEntry;
var
 Route:TIP6RouteEntry;
begin
 {}
 if not(FRoutes.WriterOwner) then FRoutes.ReaderLock else FRoutes.WriterLock;
 try
  Result:=nil;

  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Route:=TIP6RouteEntry(FRoutes.First);
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
    Route:=TIP6RouteEntry(APrevious.Next);
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
  if not(FRoutes.WriterOwner) then FRoutes.ReaderUnlock else FRoutes.WriterUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.CheckRoute(ARoute:TIP6RouteEntry;ALock:Boolean;AState:LongWord):Boolean;
{Check a route entry in the route cache}
{Route: The route entry to check}
{Lock: If True then lock the found entry before returning}
{State: The lock type if Lock is True (NETWORK_LOCK_READ or NETWORK_LOCK_WRITE)}

{Note: This allows safely obtaining a lock on an existing object in case it has been freed}
var
 Route:TIP6RouteEntry;
begin
 {}
 if not(FRoutes.WriterOwner) then FRoutes.ReaderLock else FRoutes.WriterLock;
 try
  Result:=False;

  {Check Route}
  if ARoute = nil then Exit;

  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: CheckRoute');
  {$ENDIF}

  {Get Route}
  Route:=TIP6RouteEntry(FRoutes.First);
  while Route <> nil do
   begin
    {Check Route}
    if Route = ARoute then
     begin
      {Lock Route}
      if ALock then if AState = NETWORK_LOCK_READ then Route.ReaderLock else Route.WriterLock;

      {Return Result}
      Result:=True;
      Exit;
     end;

    {Get Next}
    Route:=TIP6RouteEntry(Route.Next);
   end;
 finally
  if not(FRoutes.WriterOwner) then FRoutes.ReaderUnlock else FRoutes.WriterUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.AddRoute(const ANetwork,ANetmask,AGateway,AAddress:TIn6Addr;AType:Word;ALock:Boolean;AState:LongWord):TIP6RouteEntry;
{Add an IP6 route to the route cache}
{Network: The network address of the entry to add}
{Netmask: The netmask of the entry to add}
{Gateway: The gateway of the entry to add}
{Address: The IP6 address of the entry to add}
{Type: The type of the added entry (eg ROUTE_TYPE_DYNAMIC)}
{Lock: If True then lock the added entry before returning}
var
 Route:TIP6RouteEntry;
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddRoute');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Network = ' + In6AddrToString(ANetwork));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Netmask = ' + In6AddrToString(ANetmask));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Gateway = ' + In6AddrToString(AGateway));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Type = ' + RouteTypeToString(AType));
 {$ENDIF}

 {Create Route}
 Result:=TIP6RouteEntry.Create;
 Result.Network:=ANetwork;
 Result.Netmask:=ANetmask;
 Result.Gateway:=AGateway;
 Result.Address:=AAddress;
 Result.RouteType:=AType;

 {Acquire Lock}
 FRoutes.ReaderLock;
 try
  {Find the insertion point}
  Route:=TIP6RouteEntry(FRoutes.Last);
  while Route <> nil do
   begin
    //if LongWord(Result.Network.S_addr) >= LongWord(Route.Network.S_addr) then //To Do
    // begin
    //  Break;
    // end;

    {Get Previous}
    Route:=TIP6RouteEntry(Route.Prev);
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

function TIP6Transport.RemoveRoute(const ANetwork,AAddress:TIn6Addr):Boolean;
{Remove an IP6 route from the route cache}
{Network: The network address of the entry to remove}
{Address: The IP6 address of the entry to remove}
var
 Route:TIP6RouteEntry;
begin
 {}
 FRoutes.ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveRoute');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Network = ' + In6AddrToString(ANetwork));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  {Get Route}
  Route:=TIP6RouteEntry(FRoutes.First);
  while Route <> nil do
   begin
    {Check Network}
    if CompareAddress(Route.Network,ANetwork) then
     begin
      {Check Address}
      if CompareAddress(Route.Address,AAddress) then
       begin
        {Lock Route}
        {Route.WriterLock;} {Must be after acquiring writer lock}

        {Convert Lock}
        if FRoutes.ReaderConvert then
         begin
          {Lock Route}
          if CheckRoute(Route,True,NETWORK_LOCK_WRITE) then
           begin
            {Remove Route}
            Result:=FRoutes.Remove(Route);

            {Convert Lock}
            FRoutes.WriterConvert;

            {Unlock Route}
            Route.WriterUnlock;

            {Free Route}
            Route.Free;
           end;
         end;

        Exit;
       end;
     end;

    {Get Next}
    Route:=TIP6RouteEntry(Route.Next);
   end;
 finally
  if not(FRoutes.WriterOwner) then FRoutes.ReaderUnlock else FRoutes.WriterUnlock;
 end;
end;

{==============================================================================}

procedure TIP6Transport.FlushRoutes(All:Boolean);
{Flush routes from the route cache}
{All: If True flush all routes, otherwise flush expired routes}
var
 CurrentTime:Int64;
 Route:TIP6RouteEntry;
 Current:TIP6RouteEntry;
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: FlushRoutes');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Tick Count}
 CurrentTime:=GetTickCount64;

 {Get Route}
 Route:=GetRouteByNext(nil,True,False,NETWORK_LOCK_READ);
 while Route <> nil do
  begin
   {Check Route Type and Expiry}
   if ((Route.RouteType = ROUTE_TYPE_DYNAMIC) and ((Route.RouteTime + MAX_ROUTE_LIFE) < CurrentTime)) or (All) then
    begin
     {Unlock Route (While waiting for writer lock)}
     Route.ReaderUnlock;

     {Acquire Lock}
     FRoutes.WriterLock;

     {Lock Route}
     if CheckRoute(Route,True,NETWORK_LOCK_WRITE) then
      begin
       {Save Route}
       Current:=Route;

       {Get Next}
       Route:=GetRouteByNext(Current,True,False,NETWORK_LOCK_READ);

       {Remove Route}
       FRoutes.Remove(Current);

       {Release Lock}
       FRoutes.WriterUnlock;

       {Unlock Route}
       Current.WriterUnlock;

       {Free Route}
       Current.Free;
       Current:=nil;
      end
     else
      begin
       {Release Lock}
       FRoutes.WriterUnlock;

       {Get Next}
       if All then Route:=GetRouteByNext(nil,True,False,NETWORK_LOCK_READ);
      end;
    end
   else
    begin
     {Get Next}
     Route:=GetRouteByNext(Route,True,True,NETWORK_LOCK_READ);
    end;
  end;
end;

{==============================================================================}

function TIP6Transport.GetAddressByAddress(const AAddress:TIn6Addr;ALock:Boolean;AState:LongWord):TIP6AddressEntry;
{Find the IP6 address in the address cache}
{Address: The IP6 address to find}
{Lock: If True then lock the found entry before returning}
var
 Address:TIP6AddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetAddressByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  {Get Address}
  Address:=TIP6AddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + In6AddrToString(Address.Address));
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
    Address:=TIP6AddressEntry(Address.Next);
   end;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetAddressByNext(APrevious:TIP6AddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TIP6AddressEntry;
var
 Address:TIP6AddressEntry;
begin
 {}
 if not(FAddresses.WriterOwner) then FAddresses.ReaderLock else FAddresses.WriterLock;
 try
  Result:=nil;

  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Address:=TIP6AddressEntry(FAddresses.First);
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
    Address:=TIP6AddressEntry(APrevious.Next);
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
  if not(FAddresses.WriterOwner) then FAddresses.ReaderUnlock else FAddresses.WriterUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.CheckAddress(AAddress:TIP6AddressEntry;ALock:Boolean;AState:LongWord):Boolean;
{Check an address entry in the address cache}
{Address: The address entry to check}
{Lock: If True then lock the found entry before returning}
{State: The lock type if Lock is True (NETWORK_LOCK_READ or NETWORK_LOCK_WRITE)}

{Note: This allows safely obtaining a lock on an existing object in case it has been freed}
var
 Address:TIP6AddressEntry;
begin
 {}
 if not(FAddresses.WriterOwner) then FAddresses.ReaderLock else FAddresses.WriterLock;
 try
  Result:=False;

  {Check Address}
  if AAddress = nil then Exit;

  {$IFDEF IP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: CheckAddress');
  {$ENDIF}

  {Get Address}
  Address:=TIP6AddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {Check Address}
    if Address = AAddress then
     begin
      {Lock Address}
      if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;

      {Return Result}
      Result:=True;
      Exit;
     end;

    {Get Next}
    Address:=TIP6AddressEntry(Address.Next);
   end;
 finally
  if not(FAddresses.WriterOwner) then FAddresses.ReaderUnlock else FAddresses.WriterUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.AddAddress(const AAddress:TIn6Addr;AType:Word;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TIP6AddressEntry;
{Add an IP6 address to the address cache}
{Address: The IP6 address to add}
{Adapter: The adapter the address is on}
{Type: The type of the added entry (eg ADDRESS_TYPE_PRIMARY)}
{Lock: If True then lock the added entry before returning}

{Note: The handling of Secondary addresses should probably change in future to use a Binding type mechanism (eg AddBinding/RemoveBinding)}
var
 Adapter:TIP6TransportAdapter;
begin
 {}
 Result:=nil;

 {Check Adapter}
 if AAdapter = nil then Exit;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Adapter = ' + AAdapter.Name);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Type = ' + AddressTypeToString(AType));
 {$ENDIF}

 {Check Type}
 case AType of
  ADDRESS_TYPE_SECONDARY:begin
    Adapter:=TIP6TransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
    if Adapter = nil then Exit;
    try
     if not CompareSubnet(AAddress,Adapter.Network,Adapter.Netmask) then Exit;

     {Add Route}
     //AddRoute(AAddress,IP6_BROADCAST_NETMASK,IP6_LOOPBACK_ADDRESS,IP6_LOOPBACK_ADDRESS,ROUTE_TYPE_STATIC,False,NETWORK_LOCK_NONE); //To Do

     //To Do //See Binding
    finally
     Adapter.ReaderUnlock;
    end;
   end;
 end;

 {Create Address}
 Result:=TIP6AddressEntry.Create;
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

function TIP6Transport.RemoveAddress(const AAddress:TIn6Addr):Boolean;
{Remove an IP6 address from the address cache}
{Address: The IP6 address to remove}
var
 Address:TIP6AddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Address = ' + In6AddrToString(AAddress));
  {$ENDIF}

  {Get Address}
  Address:=TIP6AddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {Check Address}
    if CompareAddress(Address.Address,AAddress) then
     begin
      {Check Type}
      case Address.AddressType of
       ADDRESS_TYPE_SECONDARY:begin
         {Remove Route}
         //RemoveRoute(Address.Address,IP6_LOOPBACK_ADDRESS); //To Do

         //To Do //See Binding
        end;
      end;

      {Lock Address}
      {Address.WriterLock;} {Must be after acquiring writer lock}

      {Convert Lock}
      if FAddresses.ReaderConvert then
       begin
        {Lock Address}
        if CheckAddress(Address,True,NETWORK_LOCK_WRITE) then
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
       end;

      Exit;
     end;

    {Get Next}
    Address:=TIP6AddressEntry(Address.Next);
   end;
 finally
  if not(FAddresses.WriterOwner) then FAddresses.ReaderUnlock else FAddresses.WriterUnlock;
 end;
end;

{==============================================================================}

procedure TIP6Transport.FlushAddresses(All:Boolean);
{Flush addresses from the adresses cache}
{All: If True flush all addresses, otherwise flush invalid addresses}
var
 Address:TIP6AddressEntry;
 Current:TIP6AddressEntry;
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: FlushAddresses');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Address}
 Address:=GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
 while Address <> nil do
  begin
   {Check Adapter}
   if (GetAdapterByAdapter(Address.Adapter,False,NETWORK_LOCK_NONE) = nil) or (All) then {Do not lock}
    begin
     {Check Type}
     case Current.AddressType of
      ADDRESS_TYPE_SECONDARY:begin
        {Remove Route}
        //RemoveRoute(Address.Address,IP_LOOPBACK_ADDRESS); //To Do

        //To Do //See Binding
      end;
      //To Do //Special Checks for SECONDARY to make sure Subnet is still valid
              //See Binding
     end;

     {Unlock Address (While waiting for writer lock)}
     Address.ReaderUnlock;

     {Acquire Lock}
     FAddresses.WriterLock;

     {Lock Address}
     if CheckAddress(Address,True,NETWORK_LOCK_WRITE) then
      begin
       {Save Address}
       Current:=Address;

       {Get Next}
       Address:=GetAddressByNext(Current,True,False,NETWORK_LOCK_READ);

       {Remove Address}
       FAddresses.Remove(Current);

       {Release Lock}
       FAddresses.WriterUnlock;

       {Unlock Address}
       Current.WriterUnlock;

       {Free Address}
       Current.Free;
       Current:=nil;
      end
     else
      begin
       {Release Lock}
       FAddresses.WriterUnlock;

       {Get Next}
       if All then Address:=GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
      end;
    end
   else
    begin
     {Get Next}
     Address:=GetAddressByNext(Address,True,True,NETWORK_LOCK_READ);
    end;
  end;
end;

{==============================================================================}

function TIP6Transport.GetNetworkByName(const AName:String;ALock:Boolean):TIP6NetworkEntry;
{Find the name in the network cache}
{Name: The name to find}
{Lock: If True then lock the found entry before returning}
var
 Name:String;
 Network:TIP6NetworkEntry;
begin
 {}
 FNetworks.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetNetworkByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
  {$ENDIF}

  {Get Network}
  Network:=TIP6NetworkEntry(FNetworks.First);
  while Network <> nil do
   begin
    {Get Name}
    Name:=Network.Name;

    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + Name);
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
    Network:=TIP6NetworkEntry(Network.Next);
   end;
 finally
  FNetworks.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetNetworkByAddress(const ANetwork:TIn6Addr;ALock:Boolean):TIP6NetworkEntry;
{Find the network address in the network cache}
{Network: The network address to find}
{Lock: If True then lock the found entry before returning}
var
 Network:TIP6NetworkEntry;
begin
 {}
 FNetworks.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetNetworkByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Network = ' + In6AddrToString(ANetwork));
  {$ENDIF}

  {Get Network}
  Network:=TIP6NetworkEntry(FNetworks.First);
  while Network <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + In6AddrToString(Network.Network));
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
    Network:=TIP6NetworkEntry(Network.Next);
   end;
 finally
  FNetworks.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.AddNetwork(const AName:String;const ANetwork:TIn6Addr;ALock:Boolean):TIP6NetworkEntry;
{Add a network address and name to the network cache}
{Name: The name of the entry to add}
{Network: The network address of the entry to add}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddNetwork');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Network = ' + In6AddrToString(ANetwork));
 {$ENDIF}

 {Create Network}
 Result:=TIP6NetworkEntry.Create;
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

function TIP6Transport.RemoveNetwork(const AName:String):Boolean;
{Remove a network from the network cache}
{Name: The name of the network to remove}
var
 Name:String;
 Network:TIP6NetworkEntry;
begin
 {}
 FNetworks.ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveNetwork');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
  {$ENDIF}

  {Get Network}
  Network:=TIP6NetworkEntry(FNetworks.First);
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
    Network:=TIP6NetworkEntry(Network.Next);
   end;
 finally
  FNetworks.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetServByName(const AName,AProtocol:String;ALock:Boolean):TIP6ServEntry;
{Find the name in the service cache}
{Name: The name to find}
{Protocol: The protocol to find}
{Lock: If True then lock the found entry before returning}
var
 Serv:TIP6ServEntry;
begin
 {}
 FServs.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetServByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + AProtocol);
  {$ENDIF}

  {Get Service}
  Serv:=TIP6ServEntry(FServs.First);
  while Serv <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + Serv.Name);
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
    Serv:=TIP6ServEntry(Serv.Next);
   end;
 finally
  FServs.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetServByPort(APort:Word;const AProtocol:String;ALock:Boolean):TIP6ServEntry;
{Find the port in the service cache}
{Port: The port to find}
{Protocol: The protocol to find}
{Lock: If True then lock the found entry before returning}
var
 Serv:TIP6ServEntry;
begin
 {}
 FServs.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetServByPort');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Port = ' + IntToStr(APort));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + AProtocol);
  {$ENDIF}

  {Get Service}
  Serv:=TIP6ServEntry(FServs.First);
  while Serv <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + IntToStr(Serv.Port));
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
    Serv:=TIP6ServEntry(Serv.Next);
   end;
 finally
  FServs.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.AddServ(const AName,AProtocol:String;APort:Word;ALock:Boolean):TIP6ServEntry;
{Add a service to the service cache}
{Name: The name of the entry to add}
{Protocol: The protocol of the entry to add}
{Port: The port of the entry to add}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddServ');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + AProtocol);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Port = ' + IntToStr(APort));
 {$ENDIF}

 {Create Service}
 Result:=TIP6ServEntry.Create;
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

function TIP6Transport.RemoveServ(const AName,AProtocol:String):Boolean;
{Remove a service from the service cache}
{Name: The name of the entry to remove}
{Protocol: The protocol of the entry to remove}
var
 Serv:TIP6ServEntry;
begin
 {}
 FServs.ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveServ');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Protocol = ' + AProtocol);
  {$ENDIF}

  {Get Service}
  Serv:=TIP6ServEntry(FServs.First);
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
    Serv:=TIP6ServEntry(Serv.Next);
   end;
 finally
  FServs.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetProtoByName(const AName:String;ALock:Boolean):TIP6ProtoEntry;
{Find the name in the protocol cache}
{Name: The name to find}
{Lock: If True then lock the found entry before returning}
var
 Proto:TIP6ProtoEntry;
begin
 {}
 FProtos.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetProtoByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
  {$ENDIF}

  {Get Protocol}
  Proto:=TIP6ProtoEntry(FProtos.First);
  while Proto <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + Proto.Name);
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
    Proto:=TIP6ProtoEntry(Proto.Next);
   end;
 finally
  FProtos.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.GetProtoByNumber(ANumber:Word;ALock:Boolean):TIP6ProtoEntry;
{Find the protocol number in the protocol cache}
{Number: The protocol number to find}
{Lock: If True then lock the found entry before returning}
var
 Proto:TIP6ProtoEntry;
begin
 {}
 FProtos.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: GetProtoByNumber');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Number = ' + IntToStr(ANumber));
  {$ENDIF}

  {Get Protocol}
  Proto:=TIP6ProtoEntry(FProtos.First);
  while Proto <> nil do
   begin
    {$IFDEF IP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Compare = ' + IntToStr(Proto.Number));
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
    Proto:=TIP6ProtoEntry(Proto.Next);
   end;
 finally
  FProtos.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.AddProto(const AName:String;ANumber:Word;ALock:Boolean):TIP6ProtoEntry;
{Add a protocol to the protocol cache}
{Name: The name of the entry to add}
{Number: The protocol number of the entry to add}
{Lock: If True then lock the added entry before returning}
begin
 {}
 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: AddProto');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Number = ' + IntToStr(ANumber));
 {$ENDIF}

 {Create Protocol}
 Result:=TIP6ProtoEntry.Create;
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

function TIP6Transport.RemoveProto(const AName:String):Boolean;
{Remove a protocol from the protocol cache}
{Name: The name of the entry to remove}
var
 Proto:TIP6ProtoEntry;
begin
 {}
 FProtos.ReaderLock;
 try
  Result:=False;

  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: RemoveProto');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6:  Name = ' + AName);
  {$ENDIF}

  {Get Protocol}
  Proto:=TIP6ProtoEntry(FProtos.First);
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
    Proto:=TIP6ProtoEntry(Proto.Next);
   end;
 finally
  FProtos.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.CompareLocal(const AAddress:TIn6Addr):Boolean;
{Compare the supplied address with the local IP6 addresses}
var
 Address:TIP6AddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=True;

  Address:=TIP6AddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {Check for Bound to Default Address}
    //To Do //Not required for IPv6 ?

    {Check for Matching Address}
    //To Do

    Address:=TIP6AddressEntry(Address.Next);
   end;

  Result:=False;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIP6Transport.CompareDefault(const AAddress:TIn6Addr):Boolean;
{Compare the supplied address with the default IP6 address}
begin
 {}
 Result:=False; //To Do
 //To Do //For some routines see: \jwa\branches\2.3\Win32API\jwaws2tcpip.pas
end;

{==============================================================================}

function TIP6Transport.CompareLoopback(const AAddress:TIn6Addr):Boolean;
{Compare the supplied address with the loopback IP6 address}
begin
 {}
 Result:=False; //To Do
 //To Do //For some routines see: \jwa\branches\2.3\Win32API\jwaws2tcpip.pas
end;

{==============================================================================}

function TIP6Transport.CompareDirected(const AAddress:TIn6Addr):Boolean;
{Compare the supplied address with the directed broadcast IP6 addresses}
var
 Adapter:TIP6TransportAdapter;
begin
 {}
 //To Do //Should this change back to FAdapters.ReaderLock for speed ?
 Result:=True;

 {Get Adapter}
 Adapter:=TIP6TransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
 while Adapter <> nil do
  begin
   //if LongWord(Adapter.Directed.S_addr) = LongWord(AAddress.S_addr) then //To Do
   // begin
   //  {Unlock Adapter}
   //  Adapter.ReaderUnlock;
   //  Exit;
   // end;
   //To Do //For some routines see: \jwa\branches\2.3\Win32API\jwaws2tcpip.pas

   {Get Next}
   Adapter:=TIP6TransportAdapter(GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ));
  end;

 Result:=False;
end;

{==============================================================================}

function TIP6Transport.CompareBroadcast(const AAddress:TIn6Addr):Boolean;
{Compare the supplied address with the broadcast IP6 address}
begin
 {}
 Result:=False; //To Do //Not required for IPv6
 //To Do //For some routines see: \jwa\branches\2.3\Win32API\jwaws2tcpip.pas
end;

{==============================================================================}

function TIP6Transport.CompareMulticast(const AAddress:TIn6Addr):Boolean;
{Compare the supplied address with the multicast IP6 address}
begin
 {}
 Result:=False; //To Do
 //To Do //For some routines see: \jwa\branches\2.3\Win32API\jwaws2tcpip.pas
end;

{==============================================================================}

function TIP6Transport.CompareAddress(const AAddress1,AAddress2:TIn6Addr):Boolean;
{Compare the supplied addresses to see if they are the same}
begin
 {}
 Result:=False; //To Do
 //To Do //For some routines see: \jwa\branches\2.3\Win32API\jwaws2tcpip.pas
end;

{==============================================================================}

function TIP6Transport.CompareSubnet(const AAddress,ANetwork,ANetmask:TIn6Addr):Boolean;
{Compare the supplied address to see if it is in the supplied subnet}
begin
 {}
 Result:=False; //To Do
 //To Do //For some routines see: \jwa\branches\2.3\Win32API\jwaws2tcpip.pas
end;

{==============================================================================}
{==============================================================================}
{TIP6State}
constructor TIP6State.Create;
begin
 {}
 inherited Create;
 FLocalAddress:=IP6_DEFAULT_ADDRESS;
 FRemoteAddress:=IP6_DEFAULT_ADDRESS;
end;

{==============================================================================}

procedure TIP6State.SetLocalAddress(const ALocalAddress:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FLocalAddress:=ALocalAddress;

 ReleaseLock;
end;

{==============================================================================}

procedure TIP6State.SetRemoteAddress(const ARemoteAddress:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FRemoteAddress:=ARemoteAddress;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIP6Options}
constructor TIP6Options.Create;
begin
 {}
 inherited Create;
 FHopLimit:=HOPLIMIT_DEFAULT;
 //To do
end;

{==============================================================================}

destructor TIP6Options.Destroy;
begin
 {}
 AcquireLock;
 try
  //To do
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

procedure TIP6Options.SetHopLimit(AHopLimit:Byte);
begin
 {}
 if not AcquireLock then Exit;

 FHopLimit:=AHopLimit;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIP6HostEntry}
constructor TIP6HostEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET6;
 FLength:=SizeOf(TIn6Addr);

 FillChar(FAddresses,SizeOf(TIn6Addr) * MAX_HOST_ALIASES,0);
end;

{==============================================================================}

function TIP6HostEntry.GetAddress:TIn6Addr;
begin
 {}
 Result:=FAddresses[0];
end;

{==============================================================================}

procedure TIP6HostEntry.SetAddress(const AAddress:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FAddresses[0]:=AAddress;

 ReleaseLock;
end;

{==============================================================================}

function TIP6HostEntry.GetAddresses(Index:Integer):TIn6Addr;
begin
 {}
 Result:=FAddresses[Index];
end;

{==============================================================================}

function TIP6HostEntry.FindAddress(const AAddress:TIn6Addr):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  {$IFDEF IP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP6HostEntry: FindAddress');
  {$ENDIF}

  {Get Addresses}
  for Count:=0 to MAX_HOST_ALIASES - 1 do
   begin
    {$IFDEF IP6_DEBUG}
    //if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IP6HostEntry:  Compare = ' + InAddrToString(InAddrToNetwork(FAddresses[Count])));
    {$ENDIF}

    {Check Address}
    //if LongWord(FAddresses[Count].S_addr) = LongWord(AAddress.S_addr) then
    // begin
    //  {Return Result}
    //  Result:=Result;
    //  Exit;
    // end;
    //To do
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TIP6HostEntry.AddAddress(const AAddress:TIn6Addr):Boolean;
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
    //if LongWord(FAddresses[Count].S_addr) = INADDR_ANY then
    // begin
    //  {Set Address}
    //  FAddresses[Count]:=AAddress;

    //  {Return Result}
    //  Result:=True;
    //  Exit;
    // end;
    //To do
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TIP6HostEntry.RemoveAddress(const AAddress:TIn6Addr):Boolean;
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
    //if LongWord(FAddresses[Count].S_addr) = LongWord(AAddress.S_addr) then
    // begin
    //  {Clear Address}
    //  LongWord(FAddresses[Count].S_addr):=INADDR_ANY;

    //  {Return Result}
    //  Result:=True;
    //  Exit;
    // end;
    //To do
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TIP6RouteEntry}
constructor TIP6RouteEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET6;
 FLength:=SizeOf(TIn6Addr);

 //To do
end;

{==============================================================================}

procedure TIP6RouteEntry.SetNetwork(const ANetwork:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FNetwork:=ANetwork;

 ReleaseLock;
end;

{==============================================================================}

procedure TIP6RouteEntry.SetNetmask(const ANetmask:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FNetmask:=ANetmask;

 ReleaseLock;
end;

{==============================================================================}

procedure TIP6RouteEntry.SetGateway(const AGateway:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FGateway:=AGateway;

 ReleaseLock;
end;

{==============================================================================}

procedure TIP6RouteEntry.SetAddress(const AAddress:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIP6AddressEntry}
constructor TIP6AddressEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET6;
 FLength:=SizeOf(TIn6Addr);

 //To do
end;

{==============================================================================}

procedure TIP6AddressEntry.SetAddress(const AAddress:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIP6NetworkEntry}
constructor TIP6NetworkEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET6;
 FLength:=SizeOf(TIn6Addr);

 //FNetwork.S_addr:=INADDR_ANY; //To do
end;

{==============================================================================}

procedure TIP6NetworkEntry.SetNetwork(const ANetwork:TIn6Addr);
begin
 {}
 if not AcquireLock then Exit;

 FNetwork:=ANetwork;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIP6ServEntry}
constructor TIP6ServEntry.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}
{==============================================================================}
{TIP6ProtoEntry}
constructor TIP6ProtoEntry.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure IP6Init;
begin
 {}
 {Check Initialized}
 if IP6Initialized then Exit;

 {Create IPv6 Transport}
 if NetworkSettings.GetBooleanDefault('IP6_TRANSPORT_ENABLED',IP6_TRANSPORT_ENABLED) then
  begin
   TIP6Transport.Create(TransportManager,IP6_TRANSPORT_NAME);
  end;

 IP6Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{IPv6 Functions}
function CheckIP6(ABuffer:Pointer):Boolean;
{Verify that the packet is a valid IP6 packet}
{Buffer: Complete packet without Adapter header}
var
 Length:Word;
 IP6:PIP6Header;
begin
 {}
 Result:=False;

 {$IFDEF IP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IPv6: CheckIP6');
 {$ENDIF}

 {Get Header}
 IP6:=PIP6Header(ABuffer);

 {Check Version - Only Support IPv6}
 if (IP6.VersionClassLabel and $F0000000) = $60000000 then
  begin
   {Check Header Length}
   Length:=GetIP6HeaderLength(IP6);
   if Length >= IP6_HEADER_SIZE then
    begin

     //To do

    end;
  end;
end;

{==============================================================================}

function GetIP6Protocol(ABuffer:Pointer):Byte;

{Buffer: Complete packet including Transport header}
begin
 {}
 Result:=0;
 //To do
end;

{==============================================================================}

function GetIP6HeaderOffset(ABuffer:Pointer):Word;
{Return Start of the IP6 Header (Start of Packet)}
{Buffer: Complete packet including Transport header}
begin
 {}
 Result:=0;
 //To do
end;

{==============================================================================}

function GetIP6HeaderLength(ABuffer:Pointer):Word;
{Return Size of IP6 Header (Including Options)}
{Buffer: Complete packet including Transport header}
begin
 {}
 Result:=0;
 //To do
end;

{==============================================================================}

function GetIP6DataOffset(ABuffer:Pointer):Word;
{Return Start of IP6 Packet Data (Length of IP6 Header)}
{Buffer: Complete packet including Transport header}
begin
 {}
 Result:=0;
 //To do
end;

{==============================================================================}

function GetIP6DataLength(ABuffer:Pointer):Word;
{Return Size of IP6 Packet Data (IP6 TotalLength - IP6 Header)}
{Buffer: Complete packet including Transport header}
begin
 {}
 Result:=0;
 //To do
end;

{==============================================================================}

function ChecksumIP6(ABuffer:Pointer;AOffset,ALength:Word):Word;
{Validate the IP6 Header and Options Checksum on Receive}
{Buffer: Complete packet including Transport header}
begin
 {}
 Result:=0;
 //To Do //Not required for IPv6 ?
end;

{==============================================================================}
{==============================================================================}
{IPv6 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 IP6Init;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

