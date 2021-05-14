{
Ultibo ICMP (Internet Control Message Protocol) unit.

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
 
 
References
==========

 
Internet Control Message Protocol
=================================

 Notes: ICMP Checksum includes both Header and Data (if any)
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ICMP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Threads,SysUtils,Classes,Network,Transport,Protocol,IP,Ultibo,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {ICMP specific constants}
 {Note: Some ICMP definitions are in the Protocol or IP modules}
 ICMP_PROTOCOL_NAME = 'ICMP';
 
 {ICMP constants}
 ICMP_TIMEOUT = 0;         {Wait forever on a ICMP Read}
 ICMP_BUFFER_SIZE = 65536; {ICMP Receive Buffer Size}

 ICMP_HEADER_SIZE = 8;     {SizeOf(TICMPHeader);} {ICMP.Solicit}

 ICMP_PACKET_SIZE = 8;     {SizeOf(TICMPPacket)}

 ICMP_ROUTER_TIMEOUT = 60000; //To Do - See RFC 1256

{==============================================================================}
type
 {ICMP specific types}
 {Note: Some ICMP definitions are in the Protocol or IP modules}
 PICMPUnusedHeader = ^TICMPUnusedHeader;
 TICMPUnusedHeader = packed record  {Unreachable, TimeExceeded, SourceQuench etc}
  ICMPType:Byte;             {ICMP_UNREACH etc}
  Code:Byte;                 {ICMP_UNREACH_NET,ICMP_UNREACH_PORT etc}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Unused:LongWord;           {4 Bytes Unused Must be Zero}
  IP:TIPHeader;              {IP Header from Original IP Packet}
  Data:array[0..7] of Byte;  {8 Bytes of Original IP Packet Data}
 end;

 PICMPUnreachHeader = ^TICMPUnreachHeader;
 TICMPUnreachHeader = TICMPUnusedHeader; {ICMP_UNREACH}

 PICMPExpireHeader = ^TICMPExpireHeader;
 TICMPExpireHeader = TICMPUnusedHeader;  {ICMP_TIMXCEED}

 PICMPQuenchHeader = ^TICMPQuenchHeader;
 TICMPQuenchHeader = TICMPUnusedHeader;  {ICMP_SOURCEQUENCH}

 PICMPPointerHeader = ^TICMPPointerHeader;
 TICMPPointerHeader = packed record  {Param Problem}
  ICMPType:Byte;             {ICMP_PARAMPROB etc}
  Code:Byte;                 {ICMP_PARAMPROB_OPTABSENT etc}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Pointer:Byte;              {If Code = 0 Pointer to Byte where Error Occured}
  Unused:array[0..2] of Byte;{3 Bytes Unused Must be Zero}
  IP:TIPHeader;              {IP Header from Original IP Packet}
  Data:array[0..7] of Byte;  {8 Bytes of Original IP Packet Data}
 end;

 PICMPParamHeader = ^TICMPParamHeader;
 TICMPParamHeader = TICMPPointerHeader;  {ICMP_PARAMPROB}

 PICMPIpHeader = ^TICMPIpHeader;
 TICMPIpHeader = packed record  {Redirect}
  ICMPType:Byte;             {ICMP_REDIRECT etc}
  Code:Byte;                 {ICMP_REDIRECT_NET,ICMP_REDIRECT_HOST etc}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Gateway:TInAddr;           {Redirect Gateway Address}
  IP:TIPHeader;              {IP Header from Original IP Packet}
  Data:array[0..7] of Byte;  {8 Bytes of Original IP Packet Data}
 end;

 PICMPRedirectHeader = ^TICMPRedirectHeader;
 TICMPRedirectHeader = TICMPIpHeader; {ICMP_REDIRECT}

 PICMPEchoHeader = ^TICMPEchoHeader;
 TICMPEchoHeader = packed record   {Echo Request/Reply}
  ICMPType:Byte;             {ICMP_ECHO,ICMP_ECHOREPLY}
  Code:Byte;                 {Always 0}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Identifier:Word;           {Increments with each ICMP_ECHO}
  Sequence:Word;             {Sequence within an ICMP_ECHO set}
  {Data:array[0..((MAX_IP_PACKET - 1) - SizeOf(TIPHeader)) - 8] of Byte;}
 end;

 PICMPTimestampHeader = ^TICMPTimestampHeader;
 TICMPTimestampHeader = packed record {Timestamp Request/Reply}
  ICMPType:Byte;             {ICMP_TSTAMP,ICMP_TSTAMPREPLY}
  Code:Byte;                 {Always 0}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Identifier:Word;           {Increments with each ICMP_TSTAMP}
  Sequence:Word;             {Sequence within an ICMP_TSTAMP set}
  Original:LongWord;         {Original Timestamp}
  Receive:LongWord;          {Receive Timestamp}
  Transmit:LongWord;         {Transmit Timestamp}
 end; {A sort of Round Trip Time Echo}

 PICMPInfoHeader = ^TICMPInfoHeader;
 TICMPInfoHeader = packed record  {Info Request/Reply}
  ICMPType:Byte;             {ICMP_IREQ,ICMP_IREQREPLY}
  Code:Byte;                 {Always 0}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Identifier:Word;           {Increments with each ICMP_IREQ}
  Sequence:Word;             {Sequence within an ICMP_IREQ set}
 end; {Used to determine Network Number of Local Subnet}

 PICMPMaskHeader = ^TICMPMaskHeader;
 TICMPMaskHeader = packed record  {Mask Request/Reply}
  ICMPType:Byte;             {ICMP_MASKREQ,ICMP_MASKREPLY}
  Code:Byte;                 {Always 0}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Identifier:Word;           {Increments with each ICMP_MASKREQ}
  Sequence:Word;             {Sequence within an ICMP_MASKREQ set}
  Mask:TInAddr;              {Subnet Mask of Local Subnet}
 end; {Used to determine Subnet Mask of Local Subnet}

 PICMPRouterAddress = ^TICMPRouterAddress;
 TICMPRouterAddress = packed record
  Address:TInAddr;   {The address of the Router on this subnet}
  Level:LongInt;     {The level of preference, higher is better}
 end;

 PICMPAdvertHeader = ^TICMPAdvertHeader;
 TICMPAdvertHeader = packed record
  ICMPType:Byte;             {ICMP_ROUTERADVERT}
  Code:Byte;                 {Always 0}
  Checksum:Word;             {1s Compliment checksum of Structure}
  EntryCount:Byte;           {Number of Router Address entries}
  EntrySize:Byte;            {Size of each Router Address entry}
  Lifetime:Word;             {Lifetime of entries in seconds}
  {Routers:array[1..EntryCount] of TICMPRouterAddress;}
 end; {Used to advertise routers}

 PICMPSolicitHeader = ^TICMPSolicitHeader;
 TICMPSolicitHeader = packed record
  ICMPType:Byte;             {ICMP_ROUTERSOLICIT}
  Code:Byte;                 {Always 0}
  Checksum:Word;             {1s Compliment checksum of Structure}
  Reserved:LongWord;         {Always 0}
 end; {Used to solicit routers}

 PICMPHeader = ^TICMPHeader;
 TICMPHeader = packed record
  case Integer of
   0:(Unused:TICMPUnusedHeader);
   1:(Unreach:TICMPUnreachHeader);
   2:(Expire:TICMPExpireHeader);
   3:(Quench:TICMPQuenchHeader);
   4:(Pointer:TICMPPointerHeader);
   5:(Param:TICMPParamHeader);
   6:(Ip:TICMPIpHeader);
   7:(Redirect:TICMPRedirectHeader);
   8:(Echo:TICMPEchoHeader);
   9:(Timestamp:TICMPTimestampHeader);
   10:(Info:TICMPInfoHeader);
   11:(Mask:TICMPMaskHeader);
   12:(Advert:TICMPAdvertHeader);
   13:(Solicit:TICMPSolicitHeader);
 end;

 PICMPPacket = ^TICMPPacket;
 TICMPPacket = record   {8 Bytes} {Used by ICMPBuffer} 
  Size:LongWord;        {LongWord to keep size even}
  Next:PICMPPacket;
 end; {Followed by RemoteAddress (4 or 16 Bytes)}
 
{==============================================================================}
type
 {ICMP specific classes}
 TICMPSocket = class;
 TICMPProtocolTransport = class(TProtocolTransport)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}

  public
   {Status Variables}
   Socket:TICMPSocket;   {Socket for sending replies}
 end;

 TICMPProtocol = class(TNetworkProtocol)
   constructor Create(AManager:TProtocolManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Status Variables}
   FNextICMPId:Word;
   FNextICMPLock:TMutexHandle; //To Do //Change this to LocalLock and share with other properties ?

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
   function ControlHandler(AHandle:THandle;ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;

   function GetNextICMPId(AIncrement:Boolean):Word;

   function SendICMPUnreach(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode:Byte;AData:Pointer;ASize:Integer):Boolean;
   function SendICMPRedirect(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode:Byte;AAddress,AData:Pointer;ASize:Integer):Boolean;
   function SendICMPTimeExceeded(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode:Byte;AData:Pointer;ASize:Integer):Boolean;
   function SendICMPSourceQuench(ASocket:TICMPSocket;ASource,ADest:Pointer;AData:Pointer;ASize:Integer):Boolean;
   function SendICMPParamProblem(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode,APointer:Byte;AData:Pointer;ASize:Integer):Boolean;

   function SendICMPEchoReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;
   function SendICMPInfoReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;
   function SendICMPMaskReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;
   function SendICMPTimestampReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;

   function SendICMPEchoRequest(ASocket:TICMPSocket;ASource,ADest:Pointer;ASize:Integer):Boolean;
   function SendICMPInfoRequest(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;
   function SendICMPMaskRequest(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;
   function SendICMPTimestampRequest(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;

   function SendICMPRouterAdvert(ASocket:TICMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
   function SendICMPRouterSolicit(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;
  protected
   {Inherited Methods}
   function SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer; override;
   function SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; override;

   function SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer; override;
  public
   {Public Properties}

   {BSD Socket Methods}
   function Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket; override;
   function Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer; override;
   function CloseSocket(ASocket:TProtocolSocket):Integer; override;
   function Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer; override;
   function IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer; override;
   function GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer; override;
   function GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer; override;
   function GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; override;
   function Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer; override;
   function Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer; override;
   function RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer; override;
   {function Select(ANfds:Integer;AReadfds,AWritefds,AExceptfds:PFDSet;ATimeout:PTimeVal):LongInt; override;}
   function Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer; override;
   function SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer; override;
   function SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; override;
   function Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer; override;
   function Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket; override;

   {Public Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;

   function FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket; override;
   procedure FlushSockets(All:Boolean); override;
   
   function StartProtocol:Boolean; override;
   function StopProtocol:Boolean; override;
   function ProcessProtocol:Boolean; override;
 end;

 TICMPBuffer = class;
 TICMPSocket = class(TProtocolSocket)  {SOCK_RAW}
   constructor Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Data Layer Variables}
   FRecvData:TICMPBuffer;
  public
   {Data Layer Properties}
   property RecvData:TICMPBuffer read FRecvData;

   {Public Methods}
   function IoCtl(ACommand:DWORD;var AArgument:u_long):Integer; override;

   function IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
   function IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
 end;

 TICMPBuffer = class(TSocketBuffer)
   constructor Create(ASocket:TTransportSocket);
   destructor Destroy; override;
  private
   {Internal Variables}
   FOffset:Word;            {Offset to RemoteAddress}
   FLength:Word;            {Length of RemoteAddress}

   FRead:Pointer;           {Pointer to Next Read from Buffer}
   FWrite:Pointer;          {Pointer to Next Write to Buffer}

   {Status Variables}
   FCount:LongWord;         {Number of Packets in Buffer}

   FFirst:PICMPPacket;      {Pointer to First Packet}
   FLast:PICMPPacket;       {Pointer to Last Packet}

   {Internal Methods}
   function AddPacket(ASize:Integer):Boolean;
   function RemovePacket:Boolean;
   procedure FlushPackets;
  protected
   {Inherited Methods}
   procedure SetSize(ASize:LongWord); override;
  public
   {Public Methods}
   function GetNext:Integer;
   function GetCount:LongWord;

   function ReadBuffer(var ABuffer;var ASize:Integer;ARemoteAddress:Pointer;AFlags:Integer):Boolean;
   function WriteBuffer(var ABuffer;ASize:Integer;ARemoteAddress:Pointer):Boolean;
 end;
 
{==============================================================================}
{var}
 {ICMP specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure ICMPInit;

{==============================================================================}
{ICMP Functions}
function CheckICMP(AFamily:Word;ABuffer:Pointer):Boolean;

function GetICMPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetICMPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
function GetICMPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetICMPDataLength(AFamily:Word;ABuffer:Pointer):Word;

function ChecksumICMP(AFamily:Word;ABuffer:Pointer;AOffset,ALength:Word):Word;
  
{==============================================================================}
{ICMP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {ICMP specific variables}
 ICMPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TICMPProtocolTransport}
constructor TICMPProtocolTransport.Create;
begin
 {}
 inherited Create;
 Socket:=nil;
end;

{==============================================================================}

destructor TICMPProtocolTransport.Destroy;
begin
 {}
 WriterLock;
 try
  Socket:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TICMPProtocol}
constructor TICMPProtocol.Create(AManager:TProtocolManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FProtocol:=IPPROTO_ICMP;
 FSocketType:=SOCK_RAW;
 FNextICMPId:=1;
 FNextICMPLock:=MutexCreate;
end;

{==============================================================================}

destructor TICMPProtocol.Destroy;
begin
 {}
 WriterLock;
 try
  MutexDestroy(FNextICMPLock);
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TICMPProtocol.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by a Transport}
{Handle: The Handle of the Protocol Transport the packet was received from}
{Source: The source address of the received packet (Set by Transport)}
{Dest: The destination address of the received packet (Set by Transport)}
{Packet: The received packet (The complete packet including Transport header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}
var
 Offset:Word;
 Length:Word;
 IP:PIPHeader;
 ICMP:PICMPHeader;
 Socket:TICMPSocket;
 Transport:TICMPProtocolTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}
 
 {Check Source}
 {if ASource = nil then Exit;} {Not Used} 
  
 {Check Packet}
 if APacket = nil then Exit;
 
 {Get Transport}
 Transport:=TICMPProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Family = ' + AddressFamilyToString(Transport.Transport.Family));
  {$ENDIF}
 
  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Get Header}  
     IP:=PIPHeader(APacket);
    
     {$IFDEF ICMP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Protocol = ' + ProtocolToString(Transport.Protocol));
     {$ENDIF}
    
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_ICMP:begin
        {Check ICMP Packet}
        if CheckICMP(AF_INET,IP) then
         begin
          {Get Header}
          ICMP:=PICMPHeader(PtrUInt(IP) + GetICMPHeaderOffset(AF_INET,IP));
         
          {Check for a Type that we handle}
          case ICMP.Unused.ICMPType of
           ICMP_REDIRECT:begin
             Result:=True;
            
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Redirect');
             {$ENDIF}
            
             {Check that we sent the original packet}
             if TIPTransport(Transport.Transport).GetAddressByAddress(InAddrToHost(ICMP.Redirect.IP.SourceIP),False,NETWORK_LOCK_NONE) = nil then Exit;
            
             {Check that the Dest is not Broadcast}
             if ABroadcast then Exit;
             
             {Check that the Gateway is not Broadcast}
             if TIPTransport(Transport.Transport).CompareBroadcast(InAddrToHost(ICMP.Redirect.Gateway)) or TIPTransport(Transport.Transport).CompareDirected(InAddrToHost(ICMP.Redirect.Gateway)) then Exit;
             
             {Add the new Route}
             Result:=(TIPTransport(Transport.Transport).AddRoute(InAddrToHost(ICMP.Redirect.IP.DestIP),IP_BROADCAST_NETMASK,InAddrToHost(ICMP.Redirect.Gateway),IP.DestIP,ROUTE_TYPE_DYNAMIC,False,NETWORK_LOCK_NONE) <> nil);
            end;
           ICMP_ECHO:begin
             Result:=True;
             
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Echo');
             {$ENDIF}
             
             {Check that the Dest is not Broadcast}
             if ABroadcast then Exit;
            
             {Check that the Source is not Broadcast}
             if TIPTransport(Transport.Transport).CompareBroadcast(IP.SourceIP) or TIPTransport(Transport.Transport).CompareDirected(IP.SourceIP) then Exit;
            
             {Lock Socket}
             Transport.Socket.ReaderLock;

             {Pass to SendICMPEchoReply (Source and Dest reversed)} //To Do //Use Min to avoid jumbo packet ??
             Result:=SendICMPEchoReply(Transport.Socket,@IP.DestIP,@IP.SourceIP,ICMP,GetIPDataLength(IP));

             {Unlock Socket}
             Transport.Socket.ReaderUnlock;
            end;
           ICMP_ROUTERADVERT:begin
             Result:=True;
            
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Advert');
             {$ENDIF}
             
             {Ignore this - See RFC 1256}
            end;
           ICMP_ROUTERSOLICIT:begin
             Result:=True;
            
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Solicit');
             {$ENDIF}
             
             {Ignore this - See RFC 1256}
            end;
           ICMP_TSTAMP:begin
             Result:=True;
             
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Timestamp');
             {$ENDIF}
            
             {Check that the Source is not Broadcast}
             if TIPTransport(Transport.Transport).CompareBroadcast(IP.SourceIP) or TIPTransport(Transport.Transport).CompareDirected(IP.SourceIP) then Exit;
             
             {Lock Socket}
             Transport.Socket.ReaderLock;
             
             {Pass to SendICMPTimestampReply (Source and Dest reversed)}
             Result:=SendICMPTimestampReply(Transport.Socket,@IP.DestIP,@IP.SourceIP,ICMP,SizeOf(TICMPTimestampHeader));
             
             {Unlock Socket}
             Transport.Socket.ReaderUnlock;
            end;
           ICMP_IREQ:begin
             Result:=True;
             
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Info');
             {$ENDIF}
            
             {Check that the Source is not Broadcast}
             if TIPTransport(Transport.Transport).CompareBroadcast(IP.SourceIP) or TIPTransport(Transport.Transport).CompareDirected(IP.SourceIP) then Exit;
             
             {Lock Socket}
             Transport.Socket.ReaderLock;
             
             {Pass to SendICMPInfoReply (Source and Dest reversed)}
             Result:=SendICMPInfoReply(Transport.Socket,@IP.DestIP,@IP.SourceIP,ICMP,SizeOf(TICMPInfoHeader));
             
             {Unlock Socket}
             Transport.Socket.ReaderUnlock;
            end;
           ICMP_MASKREQ:begin
             Result:=True;
             
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Mask');
             {$ENDIF}
            
             {Check that the Source is not Broadcast}
             if TIPTransport(Transport.Transport).CompareBroadcast(IP.SourceIP) or TIPTransport(Transport.Transport).CompareDirected(IP.SourceIP) then Exit;
            
             {Lock Socket}
             Transport.Socket.ReaderLock;
            
             {Pass to SendICMPMaskReply (Source and Dest reversed)}
             Result:=SendICMPMaskReply(Transport.Socket,@IP.DestIP,@IP.SourceIP,ICMP,SizeOf(TICMPInfoHeader));
             
             {Unlock Socket}
             Transport.Socket.ReaderUnlock;
            end;
           else
            begin
             {Check for a Connected Socket}
             Socket:=TICMPSocket(FindSocket(AF_INET,SOCK_RAW,IPPROTO_ICMP,@IP.DestIP,@IP.SourceIP,0,0,ABroadcast,False,True,NETWORK_LOCK_READ));
             if Socket = nil then
              begin
               {Check for a Listening Socket}
               Socket:=TICMPSocket(FindSocket(AF_INET,SOCK_RAW,IPPROTO_ICMP,@IP.DestIP,@IP.SourceIP,0,0,ABroadcast,True,True,NETWORK_LOCK_READ));
              end;
             if Socket = nil then Exit;
             
             {Get the Data Offset}
             Offset:=GetIPDataOffset(IP);
             Length:=GetIPDataLength(IP);
             
             {Account for IP_HDRINCL}
             if TIPOptions(Socket.TransportOptions).Header then
              begin
               Offset:=0;
               Length:=ASize;
              end;

             {Write the Data into the Receive Buffer}              
             {$IFDEF ICMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Offset = ' + IntToStr(Offset) + ' Length = ' + IntToStr(Length));
             {$ENDIF}
             Socket.RecvData.WriteBuffer(Pointer(PtrUInt(IP) + Offset)^,Length,@IP.SourceIP);
             
             {Signal the Event}
             Socket.SignalChange;
             
             {Return True even if the Write failed (eg Buffer is Full, Socket Shutdown etc)}
             Result:=True;

             {Unlock Socket}
             Socket.ReaderUnlock;
            end;
          end;
         end;
       end;
     end;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.ControlHandler(AHandle:THandle;ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;
{Process a network control request from a Transport}
{Handle: The Handle of the Protocol Transport the control request is from}
{Source: The source address of the control request (Set by Transport)}
{Dest: The destination address of the control request (Set by Transport)}
{Protocol: The control protocol requested}
{Command: The control command to send}
{Code: The control code to send}
{Address: The control address to send}
{Data: The control data to send}
{Size: The size of the control data in bytes}
var
 Transport:TICMPProtocolTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: ControlHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Dest}
 {if ADest = nil then Exit;} {May be nil}
 
 {Check Source}
 {if ASource = nil then Exit;} {May be nil}
  
 {Check Data}
 {if AData = nil then Exit;} {May be nil}
 
 {Get Transport}
 Transport:=TICMPProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_ICMP:begin
        {Check Command}
        case ACommand of
         ICMP_ECHOREPLY:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Echo Reply');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Echo Reply}
           Result:=SendICMPEchoReply(Transport.Socket,ASource,ADest,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_UNREACH:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Unreach');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Unreach}
           Result:=SendICMPUnreach(Transport.Socket,ASource,ADest,ACode,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_SOURCEQUENCH:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Source Quench');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Source Quench}
           Result:=SendICMPSourceQuench(Transport.Socket,ASource,ADest,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_REDIRECT:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Redirect');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Redirect}
           Result:=SendICMPRedirect(Transport.Socket,ASource,ADest,ACode,AAddress,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_ECHO:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Echo');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Echo Request}
           Result:=SendICMPEchoRequest(Transport.Socket,ASource,ADest,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_ROUTERADVERT:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Router Advert');
           {$ENDIF}
           {Ignore this - See RFC 1256}
          end;
         ICMP_ROUTERSOLICIT:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Router Solicit');
           {$ENDIF}
           {Ignore this - See RFC 1256}
          end;
         ICMP_TIMXCEED:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Time Exceeded');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Time Exceeded}
           Result:=SendICMPTimeExceeded(Transport.Socket,ASource,ADest,ACode,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_PARAMPROB:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Param Problem');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Param Problem}
           Result:=SendICMPParamProblem(Transport.Socket,ASource,ADest,ACode,PByte(AAddress)^,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_TSTAMP:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Timestamp');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Timestamp Request}
           Result:=SendICMPTimestampRequest(Transport.Socket,ASource,ADest);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_TSTAMPREPLY:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Timestamp Reply');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Timestamp Reply}
           Result:=SendICMPTimestampReply(Transport.Socket,ASource,ADest,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_IREQ:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Info');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Info Request}
           Result:=SendICMPInfoRequest(Transport.Socket,ASource,ADest);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_IREQREPLY:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Info Reply');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Info Reply}
           Result:=SendICMPInfoReply(Transport.Socket,ASource,ADest,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_MASKREQ:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Mask');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Mask Request}
           Result:=SendICMPMaskRequest(Transport.Socket,ASource,ADest);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
         ICMP_MASKREPLY:begin
           {$IFDEF ICMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  ICMP Mask Reply');
           {$ENDIF}
           {Lock Socket}
           Transport.Socket.ReaderLock;
           
           {Send Mask Reply}
           Result:=SendICMPMaskReply(Transport.Socket,ASource,ADest,AData,ASize);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;
          end;
        end;
       end;
     end;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.GetNextICMPId(AIncrement:Boolean):Word;
{Get the next ICMP packet id number}
{Increment: If True increment the next id}
begin
 {}
 MutexLock(FNextICMPLock);

 {Get Next Id}
 Result:=FNextICMPId;
  
 {Increment Id}
 if AIncrement then Inc(FNextICMPId,ID_INCREMENT);

 MutexUnlock(FNextICMPLock);
end;

{==============================================================================}

function TICMPProtocol.SendICMPUnreach(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode:Byte;AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP unreachable message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Code: The unreachable message code}
{Data: The data to send for the message (The original packet including Transport Header)}
{Size: The size of the data in bytes}

{Note: Caller must ensure Source and Dest are not Broadcast}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Offset:Word;
 Packet:TPacketFragment;
 Unreach:TICMPUnreachHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPUnreach');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Fill in the ICMP fields}
 Unreach.ICMPType:=ICMP_UNREACH;
 Unreach.Code:=ACode; {ICMP_UNREACH_PROTOCOL or ICMP_UNREACH_PORT only}
 Unreach.Unused:=0;
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPUnreachHeader);
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Header and Data Offsets}
    Offset:=GetIPDataOffset(AData);
    
    {Copy the IP Header and first 64 bits of Data}
    System.Move(AData^,Unreach.IP,SizeOf(TIPHeader));
    System.Move(Pointer(PtrUInt(AData) + Offset)^,Unreach.Data,8);
    
    {Set the Addresses to Network order}
    Unreach.IP.DestIP:=InAddrToNetwork(Unreach.IP.DestIP);
    Unreach.IP.SourceIP:=InAddrToNetwork(Unreach.IP.SourceIP);
    
    {Calculate the Checksum}
    Unreach.Checksum:=ChecksumICMP(AF_INET,@Unreach,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Unreach;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPUnreach Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPRedirect(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode:Byte;AAddress,AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP redirect message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Code: The redirect message code}
{Address: The address to send for the message}
{Data: The data to send for the message (The original packet including Transport Header)}
{Size: The size of the data in bytes}

{Note: Caller must ensure Source and Dest are not Broadcast}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Offset:Word;
 Packet:TPacketFragment;
 Redirect:TICMPRedirectHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPRedirect');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Address}
 if AAddress = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Fill in the ICMP fields}
 Redirect.ICMPType:=ICMP_REDIRECT;
 Redirect.Code:=ACode; {ICMP_REDIRECT_NET or ICMP_REDIRECT_HOST only}
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPRedirectHeader);
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Fill in the ICMP fields}
    Redirect.Gateway:=InAddrToNetwork(PInAddr(AAddress)^);
    
    {Calculate the Header and Data Offsets}
    Offset:=GetIPDataOffset(AData);
    
    {Copy the IP Header and first 64 bits of Data}
    System.Move(AData^,Redirect.IP,SizeOf(TIPHeader));
    System.Move(Pointer(PtrUInt(AData) + Offset)^,Redirect.Data,8);
    
    {Set the Addresses to Network order}
    Redirect.IP.DestIP:=InAddrToNetwork(Redirect.IP.DestIP);
    Redirect.IP.SourceIP:=InAddrToNetwork(Redirect.IP.SourceIP);
    
    {Calculate the Checksum}
    Redirect.Checksum:=ChecksumICMP(AF_INET,@Redirect,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Redirect;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPRedirect Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPTimeExceeded(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode:Byte;AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP time exceeded message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Code: The time exceeded message code}
{Data: The data to send for the message (The original packet including Transport Header)}
{Size: The size of the data in bytes}

{Note: Caller must ensure Source and Dest are not Broadcast}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Offset:Word;
 Packet:TPacketFragment;
 Expire:TICMPExpireHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPTimeExceeded');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Fill in the ICMP fields}
 Expire.ICMPType:=ICMP_TIMXCEED;
 Expire.Code:=ACode; {ICMP_TIMXCEED_INTRANS or ICMP_TIMXCEED_REASS}
 Expire.Unused:=0;
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPExpireHeader);
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Header and Data Offsets}
    Offset:=GetIPDataOffset(AData);
    
    {Copy the IP Header and first 64 bits of Data}
    System.Move(AData^,Expire.IP,SizeOf(TIPHeader));
    System.Move(Pointer(PtrUInt(AData) + Offset)^,Expire.Data,8);
    
    {Set the Addresses to Network order}
    Expire.IP.DestIP:=InAddrToNetwork(Expire.IP.DestIP);
    Expire.IP.SourceIP:=InAddrToNetwork(Expire.IP.SourceIP);
    
    {Calculate the Checksum}
    Expire.Checksum:=ChecksumICMP(AF_INET,@Expire,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Expire;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPTimeExceeded Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPSourceQuench(ASocket:TICMPSocket;ASource,ADest:Pointer;AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP source quench message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Data: The data to send for the message (The original packet including Transport Header)}
{Size: The size of the data in bytes}

{Note: Caller must ensure Source and Dest are not Broadcast}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Offset:Word;
 Packet:TPacketFragment;
 Quench:TICMPQuenchHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPSourceQuench');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Fill in the ICMP fields}
 Quench.ICMPType:=ICMP_SOURCEQUENCH;
 Quench.Code:=0;
 Quench.Unused:=0;
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPQuenchHeader);
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Header and Data Offsets}
    Offset:=GetIPDataOffset(AData);
    
    {Copy the IP Header and first 64 bits of Data}
    System.Move(AData^,Quench.IP,SizeOf(TIPHeader));
    System.Move(Pointer(PtrUInt(AData) + Offset)^,Quench.Data,8);
    
    {Set the Addresses to Network order}
    Quench.IP.DestIP:=InAddrToNetwork(Quench.IP.DestIP);
    Quench.IP.SourceIP:=InAddrToNetwork(Quench.IP.SourceIP);
    
    {Calculate the Checksum}
    Quench.Checksum:=ChecksumICMP(AF_INET,@Quench,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Quench;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPSourceQuench Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPParamProblem(ASocket:TICMPSocket;ASource,ADest:Pointer;ACode,APointer:Byte;AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP param problem message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Code: The param problem message code}
{Data: The data to send for the message (The original packet including Transport Header)}
{Size: The size of the data in bytes}

{Note: Caller must ensure Source and Dest are not Broadcast}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Offset:Word;
 Packet:TPacketFragment;
 Param:TICMPParamHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPParamProblem');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Fill in the ICMP fields}
 Param.ICMPType:=ICMP_PARAMPROB;
 Param.Code:=ACode; {ICMP_PARAMPROB_OPTABSENT Only}
 Param.Pointer:=APointer;
 FillChar(Param.Unused,3,0);
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPParamHeader);
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Header and Data Offsets}
    Offset:=GetIPDataOffset(AData);
    
    {Copy the IP Header and first 64 bits of Data}
    System.Move(AData^,Param.IP,SizeOf(TIPHeader));
    System.Move(Pointer(PtrUInt(AData) + Offset)^,Param.Data,8);
    
    {Set the Addresses to Network order}
    Param.IP.DestIP:=InAddrToNetwork(Param.IP.DestIP);
    Param.IP.SourceIP:=InAddrToNetwork(Param.IP.SourceIP);
    
    {Calculate the Checksum}
    Param.Checksum:=ChecksumICMP(AF_INET,@Param,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Param;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPParamProblem Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPEchoReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP echo reply message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Data: The data to send for the message (The ICMP packet without Transport Header)}
{Size: The size of the data in bytes}

{Note: Caller must ensure Source and Dest are not Broadcast}
{Note: Caller must hold the Socket lock}
var
 Echo:PICMPEchoHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPEchoReply');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Get the Header}
 Echo:=PICMPEchoHeader(AData);
 
 {Adjust the Original Packet}
 Echo.ICMPType:=ICMP_ECHOREPLY;
 Echo.Code:=0;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Checksum}
    Echo.Checksum:=ChecksumICMP(AF_INET,Echo,0,ASize);
    
    {Create the Fragment}
    Packet.Size:=ASize;
    Packet.Data:=Echo;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,ASize,0) = ASize);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPEchoReply Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPInfoReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP info reply message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Data: The data to send for the message (The ICMP packet without Transport Header)}
{Size: The size of the data in bytes}

{Note: Source and Dest may be Default and Source may be Broadcast}
{Note: Caller must hold the Socket lock}
var
 Length:Word;
 Info:PICMPInfoHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPInfoReply');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Get the Header}
 Info:=PICMPInfoHeader(AData);
 
 {Adjust the Original Packet}
 Info.ICMPType:=ICMP_IREQREPLY;
 Info.Code:=0;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Header Length}
    Length:=GetICMPHeaderLength(AF_INET,AData);
    
    {Calculate the Checksum}
    Info.Checksum:=ChecksumICMP(AF_INET,Info,0,Length);
    
    {Get the Dest Address (Network)}
    //To Do //GetRouteByAddress to LocalAddress/CalcMask/GetRouteByAddress to NetworkAddress
    
    {Create the Fragment}
    Packet.Size:=ASize;
    Packet.Data:=Info;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,ADest,0,0,@Packet,ASize,0) = ASize);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPInfoReply Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPMaskReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP mask reply message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Data: The data to send for the message (The ICMP packet without Transport Header)}
{Size: The size of the data in bytes}

{Note: Source will usually be Broadcast, Dest should be valid}
{Note: Caller must hold the Socket lock}
var
 Length:Word;
 Mask:PICMPMaskHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPMaskReply');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Get the Header}
 Mask:=PICMPMaskHeader(AData);
 
 {Adjust the Original Packet}
 Mask.ICMPType:=ICMP_MASKREPLY;
 Mask.Code:=0;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Header Length}
    Length:=GetICMPHeaderLength(AF_INET,AData);
    
    {Calculate the Checksum}
    Mask.Checksum:=ChecksumICMP(AF_INET,Mask,0,Length);
    
    {Fill in the ICMP fields}
    //Mask.Mask:= //Netmask //To Do //GetRouteByAddress/Calc Mask ??
    
    {Create the Fragment}
    Packet.Size:=ASize;
    Packet.Data:=Mask;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,ADest,0,0,@Packet,ASize,0) = ASize);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPMaskReply Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPTimestampReply(ASocket:TICMPSocket;ASource,ADest,AData:Pointer;ASize:Integer):Boolean;
{Send an ICMP timestamp reply message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Data: The data to send for the message (The ICMP packet without Transport Header)}
{Size: The size of the data in bytes}

{Note: Source will usually be Broadcast, Dest should be valid}
{Note: Caller must hold the Socket lock}
var
 Length:Word;
 Packet:TPacketFragment;
 Timestamp:PICMPTimestampHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPTimestampReply');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Data}
 if AData = nil then Exit;
 
 {Get the Header}
 Timestamp:=PICMPTimestampHeader(AData);
 
 {Adjust the Original Packet}
 Timestamp.ICMPType:=ICMP_TSTAMPREPLY;
 Timestamp.Code:=0;
 Timestamp.Receive:=(GetTickCount or $80000000);
 Timestamp.Transmit:=(GetTickCount or $80000000);
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Header Length}
    Length:=GetICMPHeaderLength(AF_INET,AData);
    
    {Calculate the Checksum}
    Timestamp.Checksum:=ChecksumICMP(AF_INET,Timestamp,0,Length);
    
    {Create the Fragment}
    Packet.Size:=ASize;
    Packet.Data:=Timestamp;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,ADest,0,0,@Packet,ASize,0) = ASize);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPTimestampReply Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPEchoRequest(ASocket:TICMPSocket;ASource,ADest:Pointer;ASize:Integer):Boolean;
{Send an ICMP echo request message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Size: The Size of the Data to Send not including the ICMP Header}

{Note: It is assumed Source and Dest are not Broadcast / Default}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Length:Word;
 Echo:PICMPEchoHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPEchoRequest');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Size}
 if ASize < 0 then Exit;
 if ASize > 65500 then Exit;
 
 {Get the Length of the Header}
 Length:=SizeOf(TICMPEchoHeader);
 
 {Get the Size of the Packet}
 Size:=Length + ASize;
 
 {Allocate the Packet}
 Echo:=GetMem(Size);
 if Echo = nil then Exit;
 try
  {Fill in the ICMP fields}
  Echo.ICMPType:=ICMP_ECHO;
  Echo.Code:=0;
  Echo.Checksum:=0;
  Echo.Identifier:=WordNtoBE(GetNextICMPId(True));
  Echo.Sequence:=0;
  
  {Create the Data}
  if ASize > 0 then
   begin
    FillChar(Pointer(PtrUInt(Echo) + Length)^,ASize,#65);
   end; 
   
  {Check Address Family} 
  case ASocket.Family of
   AF_INET:begin
     {Calculate the Checksum}
     Echo.Checksum:=ChecksumICMP(AF_INET,Echo,0,Size);
     
     {Create the Fragment}
     Packet.Size:=Size;
     Packet.Data:=Echo;
     Packet.Next:=nil;
     
     {Send the Packet}
     Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
     
     {$IFDEF ICMP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPEchoRequest Completed');
     {$ENDIF}
    end;
  end;
 finally
  FreeMem(Echo);
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPInfoRequest(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;
{Send an ICMP info request message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}

{Note: It is assumed Source and Dest are not Broadcast / Default}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Info:TICMPInfoHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPInfoRequest');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPInfoHeader);
 
 {Fill in the ICMP fields}
 Info.ICMPType:=ICMP_IREQ;
 Info.Code:=0;
 Info.Checksum:=0;
 Info.Identifier:=WordNtoBE(GetNextICMPId(True));
 Info.Sequence:=0;
 
 {Check Address Family} 
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Checksum}
    Info.Checksum:=ChecksumICMP(AF_INET,@Info,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Info;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPInfoRequest Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPMaskRequest(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;
{Send an ICMP mask request message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}

{Note: It is assumed Source and Dest are not Broadcast / Default}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Mask:TICMPMaskHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPMaskRequest');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPMaskHeader);
 
 {Fill in the ICMP fields}
 Mask.ICMPType:=ICMP_MASKREQ;
 Mask.Code:=0;
 Mask.Checksum:=0;
 Mask.Identifier:=WordNtoBE(GetNextICMPId(True));
 Mask.Sequence:=0;
 Mask.Mask.S_addr:=INADDR_ANY;
 
 {Check Address Family} 
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Checksum}
    Mask.Checksum:=ChecksumICMP(AF_INET,@Mask,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Mask;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPMaskRequest Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPTimestampRequest(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;
{Send an ICMP timestamp request message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}

{Note: It is assumed Source and Dest are not Broadcast / Default}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Packet:TPacketFragment;
 Timestamp:TICMPTimestampHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPTimestampRequest');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPTimestampHeader);
 
 {Fill in the ICMP fields}
 Timestamp.ICMPType:=ICMP_TSTAMP;
 Timestamp.Code:=0;
 Timestamp.Checksum:=0;
 Timestamp.Identifier:=WordNtoBE(GetNextICMPId(True));
 Timestamp.Sequence:=0;
 Timestamp.Original:=(GetTickCount or $80000000);
 Timestamp.Receive:=0;
 Timestamp.Transmit:=0;
 
 {Check Address Family} 
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Checksum}
    Timestamp.Checksum:=ChecksumICMP(AF_INET,@Timestamp,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Timestamp;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPTimestampRequest Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPRouterAdvert(ASocket:TICMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
{Send an ICMP router advert message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Address: The address to send for the message}

{Note: Source will usually be Default and Dest will usually be Broadcast}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Length:Word;
 Packet:TPacketFragment;
 Advert:PICMPAdvertHeader;
 Router:PICMPRouterAddress;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPRouterAdvert');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Address}
 if AAddress = nil then Exit;
 
 {Get the Length of the Header}
 Length:=SizeOf(TICMPAdvertHeader);
 
 {Get the Size of the Packet}
 Size:=Length + SizeOf(TICMPRouterAddress);
 
 {Allocate the Packet}
 Advert:=GetMem(Size);
 if Advert = nil then Exit;
 try
  {Fill in the ICMP fields}
  Advert.ICMPType:=ICMP_ROUTERADVERT;
  Advert.Code:=0;
  Advert.Checksum:=0;
  Advert.EntryCount:=1;
  Advert.EntrySize:=SizeOf(TICMPRouterAddress);
  Advert.Lifetime:=ICMP_ROUTER_TIMEOUT;
  
  {Check Address Family} 
  case ASocket.Family of
   AF_INET:begin
     {Fill in the ICMP fields}
     Router:=PICMPRouterAddress(PtrUInt(Advert) + Length);
     Router.Address:=PInAddr(AAddress)^;
     Router.Level:=LongInt($80000000);
     
     {Calculate the Checksum}
     Advert.Checksum:=ChecksumICMP(AF_INET,Advert,0,Size);
     
     {Create the Fragment}
     Packet.Size:=Size;
     Packet.Data:=Advert;
     Packet.Next:=nil;
     
     {Send the Packet}
     Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
     
     {$IFDEF ICMP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPRouterAdvert Completed');
     {$ENDIF}
    end;
  end;
 finally
  FreeMem(Advert);
 end;
end;

{==============================================================================}

function TICMPProtocol.SendICMPRouterSolicit(ASocket:TICMPSocket;ASource,ADest:Pointer):Boolean;
{Send an ICMP router solicit message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}

{Note: Source will usually be Default and Dest will usually be Broadcast}
{Note: Caller must hold the Socket lock}
var
 Size:Word;
 Packet:TPacketFragment;
 Solicit:TICMPSolicitHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPRouterSolicit');
 {$ENDIF}
 
 if ASocket = nil then Exit;
 
 {Get the Size of the Packet}
 Size:=SizeOf(TICMPSolicitHeader);
 
 {Fill in the ICMP fields}
 Solicit.ICMPType:=ICMP_ROUTERSOLICIT;
 Solicit.Code:=0;
 Solicit.Checksum:=0;
 Solicit.Reserved:=0;
 
 {Check Address Family} 
 case ASocket.Family of
  AF_INET:begin
    {Calculate the Checksum}
    Solicit.Checksum:=ChecksumICMP(AF_INET,@Solicit,0,Size);
    
    {Create the Fragment}
    Packet.Size:=Size;
    Packet.Data:=@Solicit;
    Packet.Next:=nil;
    
    {Send the Packet}
    Result:=(SendPacket(ASocket,ASource,ADest,0,0,@Packet,Size,0) = Size);
    
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendICMPRouterSolicit Completed');
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer;
{Source is the working set to check, Dest is the set passed to Select}
var
 Count:Integer;
 Socket:TICMPSocket;
begin
 {}
 Result:=0;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SelectCheck');
 {$ENDIF}
 
 {Check Dest}
 if ADest = nil then Exit;
 
 {Check Source}
 if ASource = nil then Exit;
 
 {Check Code}
 case ACode of
  SELECT_READ:begin
    Result:=SOCKET_ERROR;
    
    {Get Sockets}
    for Count:=ASource.fd_count - 1 downto 0 do
     begin
      {Get Socket}
      Socket:=TICMPSocket(ASource.fd_array[Count]);
      
      {Check Socket}
      if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
      
      {Check Receive Count}
      if Socket.RecvData.GetCount > 0 then
       begin
        {Check Set}
        if not FD_ISSET(TSocket(Socket),ADest^) then
         begin
          FD_SET(TSocket(Socket),ADest^);
         end;
       end;
       
      {Unlock Socket}
      Socket.ReaderUnlock;
     end;
    
    {Return Result}
    Result:=ADest.fd_count;
   end;
  SELECT_WRITE:begin
    Result:=SOCKET_ERROR;
    
    {Get Sockets}
    for Count:=ASource.fd_count - 1 downto 0 do
     begin
      {Get Socket}
      Socket:=TICMPSocket(ASource.fd_array[Count]);
      
      {Check Socket}
      if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
      
      {Check Set}
      if not FD_ISSET(TSocket(Socket),ADest^) then
       begin
        FD_SET(TSocket(Socket),ADest^);
       end;

      {Unlock Socket}
      Socket.ReaderUnlock;
     end;

    {Return Result}
    Result:=ADest.fd_count;
   end;
  SELECT_ERROR:begin
    {Return Result}
    Result:=0;
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; 
{Socket is the single socket to check, Code is the type of check, Timeout is how long to wait}
var
 StartTime:Int64;
 Socket:TICMPSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SelectWait');
 {$ENDIF}

 {Get Socket}
 Socket:=TICMPSocket(ASocket);
 
 {Check Socket}
 if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
 try
  {Check Code}
  case ACode of
   SELECT_READ:begin
     {Wait for Data}
     StartTime:=GetTickCount64;
     while Socket.RecvData.GetCount = 0 do
      begin
       {Check Timeout}
       if ATimeout = 0 then
        begin
         {Return Zero}
         Result:=0;
         Exit;
        end
       else if ATimeout = INFINITE then
        begin
         {Wait for Event}
         if not Socket.WaitChange then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;
        end
       else 
        begin
         {Wait for Event}
         if not Socket.WaitChangeEx(ATimeout) then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;

         {Check for Timeout}
         if GetTickCount64 > (StartTime + ATimeout) then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;
        end;           
      end;
      
     {Return One}
     Result:=1; 
    end;
   SELECT_WRITE:begin
     {Return One}
     Result:=1; 
    end;
   SELECT_ERROR:begin
     {Return Zero}
     Result:=0;
    end;
  end;
 
 finally
  {Unlock Socket} 
  Socket.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
{Send a Packet by adding the Protocol Header and other details to the Data}
{Socket: The socket to use for sending the packet}
{Source: The source address of the packet (Host Order)}
{Dest: The destination address of the packet (Host Order)}
{SourcePort: The source port of the packet (Host Order)}
{DestPort: The destination port of the packet (Host Order)}
{Packet: The packet data to send}
{Size: The size of the packet data in bytes}
{Flags: Any protocol specific flags for sending}

{Note: For ICMP the Data is the Header so the packet is not changed}
{Note: Caller must hold the Socket lock}
var
 Transport:TICMPProtocolTransport;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Dest = ' + InAddrToString(InAddrToNetwork(PInAddr(ADest)^)));
    {$ENDIF}
    
    {Get Transport}
    Transport:=TICMPProtocolTransport(GetTransportByTransport(ASocket.Transport,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;
        
    {Send the Packet}
    Result:=TIPTransport(ASocket.Transport).SendPacket(ASocket,ASource,ADest,APacket,ASize,AFlags);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end;
 end;
end;

{==============================================================================}

function TICMPProtocol.Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket;
{BSD compatible Accept}
{Socket: The socket to accept from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=TProtocolSocket(INVALID_SOCKET);
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Accept');
 {$ENDIF}

 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Not supported}
   NetworkSetLastError(WSAEOPNOTSUPP);
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Bind}
{Sets the LocalAddress for future Sends and Receives, Address can be specified as INADDR_ANY which allows Listening or auto assignment}
{Socket: The socket to bind}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Bind');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected or Bound}
   NetworkSetLastError(WSAEINVAL);
   if ASocket.SocketState.Connected then Exit;
   if ASocket.SocketState.LocalAddress then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   if ASocket.Family <> ASockAddr.sin_family then Exit;
   
   {Check Address Family}
   case ASocket.Family of
    AF_INET:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Check LocalAddress}
      if not TIPTransport(ASocket.Transport).CompareDefault(InAddrToHost(ASockAddr.sin_addr)) then
       begin
        NetworkSetLastError(WSAEADDRNOTAVAIL);
        if TIPTransport(ASocket.Transport).GetAddressByAddress(InAddrToHost(ASockAddr.sin_addr),False,NETWORK_LOCK_NONE) = nil then Exit;
       end;
      
      {Bind the Socket}
      ASocket.SocketState.LocalAddress:=True;
      TIPState(ASocket.TransportState).LocalAddress:=InAddrToHost(ASockAddr.sin_addr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.CloseSocket(ASocket:TProtocolSocket):Integer;
{BSD compatible Close Socket}
{Closes and removes the socket, does not perform Linger}
{Socket: The socket to close}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: CloseSocket');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Close Socket}
   ASocket.SocketState.Closed:=True;
   ASocket.CloseTime:=GetTickCount64;
   
   {Signal the Event}
   ASocket.SignalChange;
   
   {Return Result}
   NetworkSetLastError(ERROR_SUCCESS);
   Result:=NO_ERROR;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Connect}
{Sets the RemoteAddress of future Sends and Receives, if Bind has not been called then
 the LocalAddress will be set appropriately as well based on the route to the RemoteAddress}
{Socket: The socket to connect}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
var
 Route:TRouteEntry;
 Address:TAddressEntry;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Connect');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAEISCONN);
   if ASocket.SocketState.Connected then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   if ASocket.Family <> ASockAddr.sin_family then Exit;
   
   {Check Address Family}
   case ASocket.Family of
    AF_INET:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIPTransport(ASocket.Transport).CompareDefault(InAddrToHost(ASockAddr.sin_addr)) then Exit;
      
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIPTransport(ASocket.Transport).CompareBroadcast(InAddrToHost(ASockAddr.sin_addr)) or TIPTransport(ASocket.Transport).CompareDirected(InAddrToHost(ASockAddr.sin_addr)) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Check the Route}
      NetworkSetLastError(WSAENETUNREACH);
      Route:=TIPTransport(ASocket.Transport).GetRouteByAddress(InAddrToHost(ASockAddr.sin_addr),True,NETWORK_LOCK_READ);
      if Route = nil then Exit;
      try
       {Check the LocalAddress}
       NetworkSetLastError(WSAEADDRNOTAVAIL);
       Address:=TIPTransport(ASocket.Transport).GetAddressByAddress(TIPRouteEntry(Route).Address,True,NETWORK_LOCK_READ);
       if Address = nil then Exit;
      
       {Check the Binding}
       if not ASocket.SocketState.LocalAddress then
        begin
         {Bind the Socket}
         ASocket.SocketState.LocalAddress:=True;
         TIPState(ASocket.TransportState).LocalAddress:=TIPAddressEntry(Address).Address;
        end;
      
       {Check for Default Binding}
       if TIPTransport(ASocket.Transport).CompareDefault(TIPState(ASocket.TransportState).LocalAddress) then
        begin
         {Set the LocalAddress}
         TIPState(ASocket.TransportState).LocalAddress:=TIPAddressEntry(Address).Address;
        end;
      
       {Connect the Socket}
       ASocket.SocketState.Connected:=True;
       ASocket.SocketState.RemoteAddress:=True;
       TIPState(ASocket.TransportState).RemoteAddress:=InAddrToHost(ASockAddr.sin_addr);
      
       {Unlock Address}
       Address.ReaderUnlock;
       
       {Signal the Event}
       ASocket.SignalChange;
       
       {Return Result}
       NetworkSetLastError(ERROR_SUCCESS);
       Result:=NO_ERROR;
      finally
       Route.ReaderUnlock;
      end;      
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer;
{BSD compatible IO Control Socket}
{Socket: The socket to control}
{Cmd: The socket command}
{Arg: The command argument}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: IoctlSocket');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Pass the call to the socket}
   Result:=ASocket.IoCtl(ACmd,AArg);
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Peer Name (Remote)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: GetPeerName');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Return the Peer Details}
      ASockAddr.sin_family:=ASocket.Family;
      ASockAddr.sin_port:=WordNtoBE(IPPORT_ANY);
      ASockAddr.sin_addr:=InAddrToNetwork(TIPState(ASocket.TransportState).RemoteAddress);
      AAddrLength:=SizeOf(TSockAddr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Sock Name (Local)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: GetSockName');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Return the Socket Details}
      ASockAddr.sin_family:=ASocket.Family;
      ASockAddr.sin_port:=WordNtoBE(IPPORT_ANY);
      ASockAddr.sin_addr:=InAddrToNetwork(TIPState(ASocket.TransportState).LocalAddress);
      AAddrLength:=SizeOf(TSockAddr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
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
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: GetSockOpt');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Level}
   case ALevel of
    IPPROTO_IP:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      case ASocket.Family of
       AF_INET:begin
         {Pass the call to the transport}
         Result:=TIPTransport(ASocket.Transport).GetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
        end;
      end;
     end;
    else
     begin
      {Pass the call to the socket}
      Result:=ASocket.GetOption(ALevel,AOptName,AOptValue,AOptLength);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer;
{BSD compatible Listen}
{Socket: The socket to listen on}
{Backlog: Queue depth for accepted connections}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Listen');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Not supported}
   NetworkSetLastError(WSAEOPNOTSUPP);
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
{BSD compatible Receive}
{Socket: The socket to receive from}
{Buffer: Buffer for received data}
{Length: Length of buffer in bytes}
{Flags: Protocol specific receive flags}

{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 StartTime:Int64;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Recv');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then Exit;
   
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Check for Shutdown}
   NetworkSetLastError(WSAESHUTDOWN);
   if ASocket.SocketState.CantRecvMore then Exit;
   
   {Check for Flag MSG_OOB}
   NetworkSetLastError(WSAEOPNOTSUPP);
   if (AFlags and MSG_OOB) = MSG_OOB then Exit;
   
   {Wait for Data}
   StartTime:=GetTickCount64;
   while TICMPSocket(ASocket).RecvData.GetCount = 0 do
    begin
     {Check for Timeout}
     if ASocket.SocketOptions.RecvTimeout > 0 then
      begin
       {Wait for Event}
       if not ASocket.WaitChangeEx(ASocket.SocketOptions.RecvTimeout) then
        begin
         NetworkSetLastError(WSAECONNABORTED);
         Exit;
        end; 
       
       {Check for Timeout}
       if GetTickCount64 > (StartTime + ASocket.SocketOptions.RecvTimeout) then
        begin
         NetworkSetLastError(WSAECONNABORTED);
         Exit;
        end;
      end
     else
      begin
       {Wait for Event}
       if not ASocket.WaitChange then
        begin
         NetworkSetLastError(WSAECONNABORTED);
         Exit;
        end; 
      end;      
     
     {Check for Closed}
     if ASocket.SocketState.Closed then
      begin
       Result:=0;
       Exit;
      end;
    end;
   
   {Check Size}
   NetworkSetLastError(ERROR_SUCCESS);
   Size:=TICMPSocket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;
    
   {Read Data}
   if TICMPSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,nil,AFlags) then
    begin
     {Return Size}
     Result:=Size;
    end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer;
{BSD compatible Receive From}
{Socket: The socket to receive from}
{Buffer: Buffer for received data}
{Length: Length of buffer in bytes}
{Flags: Protocol specific receive flags}
{FromAddr: The address the data was received from (Network Order)}
{FromLength: The length of the address}

{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 StartTime:Int64;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: RecvFrom');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Check for Shutdown}
   NetworkSetLastError(WSAESHUTDOWN);
   if ASocket.SocketState.CantRecvMore then Exit;
   
   {Check for Flag MSG_OOB}
   NetworkSetLastError(WSAEOPNOTSUPP);
   if (AFlags and MSG_OOB) = MSG_OOB then Exit;
   
   {Check size of FromAddr}
   NetworkSetLastError(WSAEFAULT);
   if AFromLength < SizeOf(TSockAddr) then Exit;
   
   {Wait for Data}
   StartTime:=GetTickCount64;
   while TICMPSocket(ASocket).RecvData.GetCount = 0 do
    begin
     {Check for Timeout}
     if ASocket.SocketOptions.RecvTimeout > 0 then
      begin
       {Wait for Event}
       if not ASocket.WaitChangeEx(ASocket.SocketOptions.RecvTimeout) then
        begin
         NetworkSetLastError(WSAECONNABORTED);
         Exit;
        end; 

       {Check for Timeout}
       if GetTickCount64 > (StartTime + ASocket.SocketOptions.RecvTimeout) then
        begin
         NetworkSetLastError(WSAECONNABORTED);
         Exit;
        end;
      end
     else
      begin
       {Wait for Event}
       if not ASocket.WaitChange then
        begin
         NetworkSetLastError(WSAECONNABORTED);
         Exit;
        end; 
      end;      
     
     {Check for Closed}
     if ASocket.SocketState.Closed then
      begin
       Result:=0;
       Exit;
      end;
    end;
    
   {Check Size}
   NetworkSetLastError(ERROR_SUCCESS);
   Size:=TICMPSocket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;
   
   {Get Address}
   AFromAddr.sin_family:=ASocket.Family;
   AFromAddr.sin_port:=WordNtoBE(IPPORT_ANY);
   
   {Read Data}
   if TICMPSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,@AFromAddr.sin_addr,AFlags) then
    begin
     {Get Address}
     AFromAddr.sin_addr:=InAddrToNetwork(AFromAddr.sin_addr);
     
     {Return Size}
     Result:=Size;
    end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
{BSD compatible Send}
{Socket: The socket to send to}
{Buffer: Buffer for data to send}
{Length: Length of buffer in bytes}
{Flags: Protocol specific send flags}

{Note: Caller must hold the Socket lock}
var
 Packet:TPacketFragment;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Send');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then Exit;
   
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Check for Shutdown}
   NetworkSetLastError(WSAESHUTDOWN);
   if ASocket.SocketState.CantSendMore then Exit;
   
   {Check for Flag MSG_OOB}
   NetworkSetLastError(WSAEOPNOTSUPP);
   if (AFlags and MSG_OOB) = MSG_OOB then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIPTransport(ASocket.Transport).CompareBroadcast(TIPState(ASocket.TransportState).RemoteAddress) or TIPTransport(ASocket.Transport).CompareDirected(TIPState(ASocket.TransportState).RemoteAddress) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Create the Fragment}
      Packet.Size:=ALength;
      Packet.Data:=@ABuffer;
      Packet.Next:=nil;
      
      {Send the Packet}
      Result:=SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,@TIPState(ASocket.TransportState).RemoteAddress,0,0,@Packet,ALength,AFlags);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer;
{BSD compatible Send To}
{Socket: The socket to send to}
{Buffer: Buffer for data to send}
{Length: Length of buffer in bytes}
{Flags: Protocol specific send flags}
{ToAddr: The socket address to send to (Network Order)}
{ToLength: The length of the socket address}

{Note: Caller must hold the Socket lock}
var
 InAddr:TInAddr;
 Route:TRouteEntry;
 Address:TAddressEntry;
 Packet:TPacketFragment;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SendTo');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Bound} {Moved Below}
   {NetworkSetLastError(WSAEINVAL);}
   {if not ASocket.SocketState.LocalAddress then Exit;}
   
   {Check for Shutdown}
   NetworkSetLastError(WSAESHUTDOWN);
   if ASocket.SocketState.CantSendMore then Exit;
   
   {Check for Flag MSG_OOB}
   NetworkSetLastError(WSAEOPNOTSUPP);
   if (AFlags and MSG_OOB) = MSG_OOB then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   if ASocket.Family <> AToAddr.sin_family then Exit;
   
   {Check Address Family}
   case ASocket.Family of
    AF_INET:begin
      {Check size of ToAddr}
      NetworkSetLastError(WSAEFAULT);
      if AToLength < SizeOf(TSockAddr) then Exit;
      
      {Get the RemoteAddress}
      InAddr:=InAddrToHost(AToAddr.sin_addr);
      
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIPTransport(ASocket.Transport).CompareDefault(InAddr) then Exit;
      
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIPTransport(ASocket.Transport).CompareBroadcast(InAddr) or TIPTransport(ASocket.Transport).CompareDirected(InAddr) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Check the Binding}
      if not(ASocket.SocketState.LocalAddress) or TIPTransport(ASocket.Transport).CompareDefault(TIPState(ASocket.TransportState).LocalAddress) then
       begin
        {Check the Route}
        NetworkSetLastError(WSAENETUNREACH);
        Route:=TIPTransport(ASocket.Transport).GetRouteByAddress(InAddr,True,NETWORK_LOCK_READ);
        if Route = nil then Exit;
        try
         {Check the LocalAddress}
         NetworkSetLastError(WSAEADDRNOTAVAIL);
         Address:=TIPTransport(ASocket.Transport).GetAddressByAddress(TIPRouteEntry(Route).Address,True,NETWORK_LOCK_READ);
         if Address = nil then Exit;
      
         {Bind the Socket}
         ASocket.SocketState.LocalAddress:=True;
         TIPState(ASocket.TransportState).LocalAddress:=TIPAddressEntry(Address).Address;
         
         {Unlock Address}
         Address.ReaderUnlock;
        finally
         {Unlock Route}
         Route.ReaderUnlock;
        end;      
       end; 
      
      {Create the Fragment}
      Packet.Size:=ALength;
      Packet.Data:=@ABuffer;
      Packet.Next:=nil;
      
      {Send the Packet}
      Result:=SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,@InAddr,0,0,@Packet,ALength,AFlags);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
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
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: SetSockOpt');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Level}
   case ALevel of
    SOL_SOCKET:begin
      {Socket Options Handled on Set}
      {Check Option}
      case AOptName of
       SO_RCVBUF:begin
         NetworkSetLastError(WSAEFAULT);
         
         if AOptLength >= SizeOf(Integer) then
          begin
           TICMPSocket(ASocket).RecvData.Size:=PInteger(AOptValue)^;
          end;
        end;
      end;
      
      {Pass the call to the socket}
      Result:=ASocket.SetOption(ALevel,AOptName,AOptValue,AOptLength);
     end;
    IPPROTO_IP:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      case ASocket.Family of
       AF_INET:begin
         {Pass the call to the transport}
         Result:=TIPTransport(ASocket.Transport).SetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
        end;
      end;
     end;
    else
     begin
      {Pass the call to the socket}
      Result:=ASocket.SetOption(ALevel,AOptName,AOptValue,AOptLength);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer;
{BSD compatible Shutdown}
{Socket: The socket to shutdown}
{How: The direction to shutdown the socket}

{Note: Shutdown does not result in CloseSocket so Closed must not get set}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Shutdown');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Direction}
   case AHow of
    SHUTDOWN_RECV:begin
      {Shutdown Receive}
      ASocket.SocketState.CantRecvMore:=True;
      
      {Signal the Event}
      ASocket.SignalChange;
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    SHUTDOWN_SEND:begin
      {Shutdown Send}
      ASocket.SocketState.CantSendMore:=True;
      
      {Signal the Event}
      ASocket.SignalChange;
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    SHUTDOWN_BOTH:begin
      {Shutdown Both}
      ASocket.SocketState.CantRecvMore:=True;
      ASocket.SocketState.CantSendMore:=True;
      
      {Signal the Event}
      ASocket.SignalChange;
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    else
     begin
      NetworkSetLastError(WSAEINVAL);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TICMPProtocol.Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket;
{BSD compatible Socket (Create a new socket)}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}
var
 Transport:TICMPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=TProtocolSocket(INVALID_SOCKET);
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: Socket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Check Socket Type}
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if AStruct <> SOCK_RAW then Exit;
  
  {Check Address Family}
  NetworkSetLastError(WSAEAFNOSUPPORT);
  if (AFamily = AF_UNSPEC) and (AProtocol <> IPPROTO_IP) then AFamily:=AF_INET;

  {Check Protocol}
  NetworkSetLastError(WSAEPROTOTYPE);
  if (AProtocol <> IPPROTO_ICMP) and (AProtocol <> IPPROTO_IP) then Exit;
  
  {Get Transport}
  Transport:=TICMPProtocolTransport(GetTransportByFamily(AFamily,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
    
  {Create Socket}
  Result:=TICMPSocket.Create(Self,Transport.Transport);
  
  {Unlock Transport}
  Transport.ReaderUnlock;

  {Acquire Lock}
  FSockets.WriterLock;
  try
   {Add Socket}
   FSockets.Add(Result);
  finally 
   {Release Lock}
   FSockets.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this protocol}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TICMPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: AddTransport');
  {$ENDIF}
 
  {Check Transport}
  if ATransport = nil then Exit;
 
  {Get Transport} 
  Transport:=TICMPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add ICMP Protocol}
       Handle:=TIPTransport(ATransport).AddProtocol(IPPROTO_ICMP,PacketHandler,ControlHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TICMPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_ICMP;
         Transport.Transport:=ATransport;
       
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
        
          {Add Control Socket}
          Transport.Socket:=TICMPSocket.Create(Self,ATransport);
          {FSockets.Add(Transport.Socket);} {Dont add this one to the list}
        
          {Add Proto Entry}
          TIPTransport(ATransport).AddProto(ICMP_PROTOCOL_NAME,IPPROTO_ICMP,False);
       
          {Return Result}
          Result:=True;
         finally
          {Release Lock}
          FTransports.WriterUnlock;
         end;  
        end;
      end;
    end;
   end
  else
   begin
    {Unlock Transport}
    Transport.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this protocol}
{Transport: The transport to remove}
var
 Transport:TICMPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: RemoveTransport');
  {$ENDIF}
 
  {Check Transport}
  if ATransport = nil then Exit;
 
  {Get Transport}
  Transport:=TICMPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
 
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove ICMP Protocol}
     if TIPTransport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
      begin
       {Remove Proto Entry}
       TIPTransport(ATransport).RemoveProto(ICMP_PROTOCOL_NAME);
       
       {Remove Control Socket}
       {FSockets.Remove(Transport.Socket);} {This one is not on the list}
       Transport.Socket.Free;
       
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
      end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket;
{Find a protocol socket based on all relevant parameters}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}
{LocalAddress: Local transport address to match (Host Order)}
{RemoteAddress: Remote transport address to match (Host Order)}
{LocalPort: Local port to match (Host Order)}
{RemotePort: Remote port to match (Host Order)}
{Broadcast: If True then match broadcast addresses}
{Listen: If True then match only listening sockets}
{Lock: If True then lock the found entry before returning}
var
 Socket:TICMPSocket;
begin
 {}
 FSockets.ReaderLock;
 try
  Result:=nil;
 
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: FindSocket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
 
  {Get Socket}
  Socket:=TICMPSocket(FSockets.First);
  while Socket <> nil do
   begin
    {Check for Closed}
    if not Socket.SocketState.Closed then
     begin
      {Check for Match}
      if (Socket.Family = AFamily) and (Socket.Struct = AStruct) and (Socket.Proto = AProtocol) then
       begin
        if not AListen then
         begin
          {Check for a Connected Socket}
          if Socket.IsConnected(ALocalAddress,ARemoteAddress,ALocalPort,ARemotePort,ABroadcast) then
           begin
            {Lock Socket}
            if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
           
            {Return Result}
            Result:=Socket;
            Exit;
           end;
         end
        else
         begin
          {Check for a Listening Socket}
          if Socket.IsListening(ALocalAddress,ARemoteAddress,ALocalPort,ARemotePort,ABroadcast) then
           begin
            {Lock Socket}
            if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
            
            {Return Result}
            Result:=Socket;
            Exit;
           end;
         end;
       end;
     end;  
    
    {Get Next}
    Socket:=TICMPSocket(Socket.Next);
   end;
 finally 
  FSockets.ReaderUnlock;
 end; 
end;

{==============================================================================}

procedure TICMPProtocol.FlushSockets(All:Boolean);
{Flush sockets from the socket cache}
{All: If True flush all sockets, otherwise flush expired sockets}
var
 CurrentTime:Int64;
 Socket:TICMPSocket;
 Current:TICMPSocket;
begin
 {}
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: FlushSockets');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  All = ' + BoolToStr(All));
 {$ENDIF}
  
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Get Socket}
 Socket:=TICMPSocket(GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
 while Socket <> nil do
  begin
   {Get Next}
   Current:=Socket;
   Socket:=TICMPSocket(GetSocketByNext(Current,True,False,NETWORK_LOCK_READ));
    
   {Check Socket State}
   if (Current.SocketState.Closed) or (All) then
    begin
     {Check Socket Expired}
     if ((Current.CloseTime + CLOSE_TIMEOUT) < CurrentTime) or (All) then
      begin
       {Convert Socket}
       if Current.ReaderConvert then
        begin
         {Acquire Lock}
         FSockets.WriterLock;
       
         {Remove Socket}
         FSockets.Remove(Current);
       
         {Release Lock}
         FSockets.WriterUnlock;
       
         {Unlock Socket}
         Current.WriterUnlock;
        
         {Free Socket}
         Current.Free;
         Current:=nil;
        end; 
      end; 
    end;
    
   {Unlock Socket}
   if Current <> nil then Current.ReaderUnlock;
  end;
end;

{==============================================================================}

function TICMPProtocol.StartProtocol:Boolean;
{Start this protocol ready for sending and receiving}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: StartProtocol');
  {$ENDIF}
 
  {Check Manager}
  if Manager = nil then Exit;
 
  {Register with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.StopProtocol:Boolean;
{Stop this protocol ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: StopProtocol');
  {$ENDIF}
 
  {Check Manager}
  if Manager = nil then Exit;
 
  {Close all Sockets}
  FlushSockets(True);
 
  {Deregister with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPProtocol.ProcessProtocol:Boolean;
{Process periodic tasks for this protocol}
begin
 {}
 {Close old Sockets}
 FlushSockets(False);
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TICMPSocket}
constructor TICMPSocket.Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
begin
 {}
 inherited Create(AProtocol,ATransport);
 
 {Check Address Family}
 case Family of
  AF_INET:begin
    {Create IP Transport State}
    FTransportState:=TIPState.Create;
    {Create IP Transport Options}
    FTransportOptions:=TIPOptions.Create;
    
    {Set IP Defaults}
    TIPOptions(FTransportOptions).TTL:=TIPTransport(ATransport).DefaultTTL;
   end;
  else
   begin
    {Create Transport State}
    FTransportState:=TTransportState.Create;
    {Create Transport Options}
    FTransportOptions:=TTransportOptions.Create;
   end;
 end;

 {Create Protocol State}
 FProtocolState:=TProtocolState.Create;
 {Create Protocol Options}
 FProtocolOptions:=TProtocolOptions.Create;

 {Create Receive Buffer}
 FRecvData:=TICMPBuffer.Create(Self);
 FRecvData.Size:=ICMP_BUFFER_SIZE;

 {Set Socket Defaults}
 FSocketOptions.SendTimeout:=ICMP_TIMEOUT;
 FSocketOptions.RecvTimeout:=ICMP_TIMEOUT;
end;

{==============================================================================}

destructor TICMPSocket.Destroy;
begin
 {}
 WriterLock;
 try
  {Free Receive Buffer}
  FRecvData.Free;

  {Free Protocol Options}
  FProtocolOptions.Free;
  {Free Protocol State}
  FProtocolState.Free;

  {Free Transport Options}
  FTransportOptions.Free;
  {Free Transport State}
  FTransportState.Free;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TICMPSocket.IoCtl(ACommand:DWORD;var AArgument:u_long):Integer;
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Socket: IoCtl');
  {$ENDIF}
  
  {Check Commmand}
  NetworkSetLastError(WSAEINVAL);
  case ACommand of
   FIONREAD:begin
     AArgument:=RecvData.GetNext;
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
    end;
   FIONBIO:begin
     SocketState.NonBlocking:=(AArgument <> 0);
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPSocket.IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this ICMP Socket is Connected to the Host specified by RemoteAddress}
{A connected Socket will have a Bound LocalAddress and a Connected RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Socket: IsConnected');
  {$ENDIF}
  
  {Check Local}
  if ALocalAddress = nil then Exit;
  
  {Check Remote}
  if ARemoteAddress = nil then Exit;
  
  {Check for Bound}
  if not SocketState.LocalAddress then Exit;
  
  {Check for Connected}
  if not SocketState.Connected then Exit; {Could use RemoteAddress instead}
  
  {Check Address Family}
  case Family of
   AF_INET:begin
     {Check the LocalAddress}
     if not ABroadcast then
      begin
       {Check the Bound LocalAddress}
       if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).LocalAddress,PInAddr(ALocalAddress)^) then Exit;
      end
     else
      begin
       {Check the Broadcast LocalAddress}
       if not TIPTransport(Transport).CompareBroadcast(PInAddr(ALocalAddress)^) then
        begin
         {If not global Broadcast then check for Directed Broadcast}
         Route:=TIPTransport(Transport).GetRouteByAddress(PInAddr(ALocalAddress)^,True,NETWORK_LOCK_READ);
         if Route = nil then Exit;
         try
          if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).LocalAddress,TIPRouteEntry(Route).Address) then Exit;
         finally
          Route.ReaderUnlock;
         end; 
        end;
      end;
     
     {Check the Connected RemoteAddress}
     if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).RemoteAddress,PInAddr(ARemoteAddress)^) then Exit;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TICMPSocket.IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this ICMP Socket is Listening on the LocalAddress specified}
{A listening Socket may or may not have a Bound LocalAddress and will have a default (INADDR_ANY) RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Socket: IsListening');
  {$ENDIF}
  
  {Check Local}
  if ALocalAddress = nil then Exit;
  
  {Check Remote}
  {if ARemoteAddress = nil then Exit;} {Not Used}
  
  {Check for Bound}
  if not SocketState.LocalAddress then Exit;
  
  {Check for Connected}
  if SocketState.Connected then Exit; {Could use RemoteAddress instead}
  
  {Check Address Family}
  case Family of
   AF_INET:begin
     {Check the LocalAddress}
     if not TIPTransport(Transport).CompareDefault(TIPState(TransportState).LocalAddress) then
      begin
       if not ABroadcast then
        begin
         {Check the Bound LocalAddress}
         if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).LocalAddress,PInAddr(ALocalAddress)^) then Exit;
        end
       else
        begin
         {Check the Broadcast LocalAddress}
         if not TIPTransport(Transport).CompareBroadcast(PInAddr(ALocalAddress)^) then
          begin
           {If not global Broadcast then check for Directed Broadcast}
           Route:=TIPTransport(Transport).GetRouteByAddress(PInAddr(ALocalAddress)^,True,NETWORK_LOCK_READ);
           if Route = nil then Exit;
           try
            if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).LocalAddress,TIPRouteEntry(Route).Address) then Exit;
           finally
            Route.ReaderUnlock;
           end; 
          end;
        end;
      end;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TICMPBuffer}
constructor TICMPBuffer.Create(ASocket:TTransportSocket);
begin
 {}
 inherited Create(ASocket);
 
 FOffset:=SizeOf(TICMPPacket); {ICMP_PACKET_SIZE}
 
 {Check Address Family}
 case FSocket.Family of
  AF_INET:begin
    FLength:=IP_ADDRESS_SIZE;
   end;
  else
   begin
    FLength:=IP_ADDRESS_SIZE;  {Default to TInAddr for safety}
   end;
 end;

 FRead:=nil;
 FWrite:=nil;

 FCount:=0;

 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TICMPBuffer.Destroy;
begin
 {}
 AcquireLock;
 try
  FlushPackets;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TICMPBuffer.AddPacket(ASize:Integer):Boolean;
{Adds a new Packet as the Last packet in the Buffer}
var
 Packet:PICMPPacket;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: AddPacket');
  {$ENDIF}
  
  {Check Data Size}
  if ASize = 0 then Exit;
  
  {Check Buffer Free}
  if LongWord(ASize) > FFree then Exit;
  
  {Create a new Packet}
  Packet:=GetMem(FOffset + FLength);
  if Packet = nil then Exit;
  Packet.Size:=ASize;
  Packet.Next:=nil;
  
  {Add to List}
  if FLast = nil then
   begin
    {Is First Packet}
    FFirst:=Packet;
    FLast:=Packet;
    Dec(FFree,ASize);
    Inc(FUsed,ASize);
    Inc(FCount);
   end
  else
   begin
    {Not First Packet}
    FLast.Next:=Packet;
    FLast:=Packet;
    Dec(FFree,ASize);
    Inc(FUsed,ASize);
    Inc(FCount);
   end;
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TICMPBuffer.RemovePacket:Boolean;
{Removes the First packet from the Buffer}
var
 Packet:PICMPPacket;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: RemovePacket');
  {$ENDIF}
  
  {Check for Packets}
  if FFirst = nil then Exit;
  
  {Remove from List}
  if FFirst.Next <> nil then
   begin
    {Not Last Packet}
    Packet:=FFirst;
    FFirst:=FFirst.Next;
    Dec(FUsed,Packet.Size);
    Inc(FFree,Packet.Size);
    Dec(FCount);
   end
  else
   begin
    {Is Last Packet}
    Packet:=FFirst;
    FFirst:=nil;
    FLast:=nil;
    Dec(FUsed,Packet.Size);
    Inc(FFree,Packet.Size);
    Dec(FCount);
   end;
  
  {Free the Packet}
  FreeMem(Packet,FOffset + FLength);
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TICMPBuffer.FlushPackets;
begin
 {}
 if not AcquireLock then Exit;
 try
  {Get Packet}
  while FFirst <> nil do
   begin
    {Remove Packet}
    RemovePacket;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TICMPBuffer.SetSize(ASize:LongWord);
{Setting the Size clears any current Packets}
begin
 {}
 if not AcquireLock then Exit;
 try
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: SetSize: Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check Size}
  if ASize = 0 then Exit;
  
  {Get Size}
  ASize:=Max(ASize,ICMP_BUFFER_SIZE);
  
  {Clear any Packets}
  FlushPackets;
  
  {Allocate the Memory}
  FBuffer.SetSize(ASize);
  
  {Set the Buffer Values}
  FSize:=ASize;
  FStart:=FBuffer.Memory;
  
  {End actually points to byte beyond last for simpler calculations}
  FEnd:=Pointer(PtrUInt(FStart) + FSize);
  
  {Set the Data Values}
  FUsed:=0;
  FFree:=FSize;
  FRead:=FStart;
  FWrite:=FStart;
  
  {Set the Packet Values}
  FCount:=0;
  FFirst:=nil;
  FLast:=nil;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TICMPBuffer.GetNext:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 try
  {Check First}
  if FFirst = nil then Exit;
  
  {Get First Size}
  Result:=FFirst.Size;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TICMPBuffer.GetCount:LongWord;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TICMPBuffer.ReadBuffer(var ABuffer;var ASize:Integer;ARemoteAddress:Pointer;AFlags:Integer):Boolean;
{Stored Packet contains the Data and if IP_HDRINCL the Transport Header}
{Passed Size contains size of Buffer and should return the size of Packet}
{Flags is for MSG_PEEK and MSG_OOB when applicable}
var
 ReadNext:Pointer;
 ReadSize:LongWord;
 BlockSize:LongWord;
 BufferSize:LongWord;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check the Buffer Size}
  if ASize = 0 then Exit;
  
  {Check there is Data}
  if FFirst = nil then Exit;
  
  {Get the Start and Size}
  ReadNext:=FRead;
  ReadSize:=FFirst.Size;
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
  {$ENDIF}
  
  {Get the Return Size}
  BufferSize:=Min(ASize,ReadSize);
  ASize:=BufferSize;
  
  {Since we Guarantee ReadNext to be at least 1 byte from the End of the Buffer, we can start reading}
  {Check for Single or Double Read}
  if (PtrUInt(ReadNext) + ReadSize) <= PtrUInt(FEnd) then
   begin
    {Single Read with no wrap around}
    {Read the Data}
    if BufferSize < ReadSize then
     begin
      {$IFDEF ICMP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: Short Read');
      {$ENDIF}
      System.Move(ReadNext^,ABuffer,BufferSize);
     end
    else
     begin
      System.Move(ReadNext^,ABuffer,ReadSize);
     end;
    
    Inc(PtrUInt(ReadNext),ReadSize);
    Dec(ReadSize,ReadSize);
   end
  else
   begin
    {Double Read with wrap around}
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: Double Read');
    {$ENDIF}
    
    {Read the First Block of the Data}
    BlockSize:=(PtrUInt(FEnd) - PtrUInt(ReadNext));
    if BufferSize < BlockSize then
     begin
      {$IFDEF ICMP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: Short First Read');
      {$ENDIF}
      System.Move(ReadNext^,ABuffer,BufferSize);
      
      Dec(BufferSize,BufferSize);
     end
    else
     begin
      System.Move(ReadNext^,ABuffer,BlockSize);
      
      Dec(BufferSize,BlockSize);
     end;
    
    {Wrap to Start of Buffer}
    ReadNext:=FStart;
    Dec(ReadSize,BlockSize);
    
    {Read the Second Block of the Data}
    if BufferSize > 0 then
     begin
      if BufferSize < ReadSize then
       begin
        {$IFDEF ICMP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: Short Second Read');
        {$ENDIF}
        System.Move(ReadNext^,Pointer(PtrUInt(@ABuffer) + BlockSize)^,BufferSize);
       end
      else
       begin
        System.Move(ReadNext^,Pointer(PtrUInt(@ABuffer) + BlockSize)^,ReadSize);
       end;
     end;
    
    Inc(PtrUInt(ReadNext),ReadSize);
    Dec(ReadSize,ReadSize);
   end;
  
  {Check for Wrap around}
  if PtrUInt(ReadNext) = PtrUInt(FEnd) then ReadNext:=FStart;
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
  {$ENDIF}
  
  {Get the Remote Address}
  if ARemoteAddress <> nil then
   begin
    System.Move(Pointer(PtrUInt(FFirst) + FOffset)^,ARemoteAddress^,FLength);
   end;
  
  {Check for Peek Flag}
  if (AFlags and MSG_PEEK) = 0 then
   begin
    {Update the Next Read}
    FRead:=ReadNext;
    
    {Remove the Packet}
    RemovePacket;
   end;
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: ReadBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
  {$ENDIF}
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TICMPBuffer.WriteBuffer(var ABuffer;ASize:Integer;ARemoteAddress:Pointer):Boolean;
{Supplied Buffer contains the Data and if IP_HDRINCL the Transport Header}
{Data that will not fit in the Buffer is simply discarded}
var
 WriteNext:Pointer;
 WriteSize:LongWord;
 BlockSize:LongWord;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check the Data Size}
  if ASize = 0 then Exit;
  
  {Add the Packet}
  if not AddPacket(ASize) then Exit;
  
  {Get the Start and Size}
  WriteNext:=FWrite;
  WriteSize:=ASize;
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: WriteBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
  {$ENDIF}
  
  {Since we guarantee WriteNext to be at least 1 byte from the End of the Buffer, we can start writing}
  if (PtrUInt(WriteNext) + WriteSize) <= PtrUInt(FEnd) then
   begin
    {Single Write with no wrap around}
    {Write the Packet Data}
    System.Move(ABuffer,WriteNext^,WriteSize);
    
    Inc(PtrUInt(WriteNext),WriteSize);
    Dec(WriteSize,WriteSize);
   end
  else
   begin
    {Double Write with wrap around}
    {$IFDEF ICMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: WriteBuffer: Double Write');
    {$ENDIF}
    
    {Write the First Block of the Packet Data}
    BlockSize:=(PtrUInt(FEnd) - PtrUInt(WriteNext));
    System.Move(ABuffer,WriteNext^,BlockSize);
    
    {Wrap to Start of Buffer}
    WriteNext:=FStart;
    Dec(WriteSize,BlockSize);
    
    {Write the Second Block of the Packet Data}
    System.Move(Pointer(PtrUInt(@ABuffer) + BlockSize)^,WriteNext^,WriteSize);
    
    Inc(PtrUInt(WriteNext),WriteSize);
    Dec(WriteSize,WriteSize);
   end;
  
  {Check for Wrap around}
  if PtrUInt(WriteNext) = PtrUInt(FEnd) then WriteNext:=FStart;
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
  {$ENDIF}
  
  {Set the RemoteAddress}
  if ARemoteAddress <> nil then
   begin
    System.Move(ARemoteAddress^,Pointer(PtrUInt(FLast) + FOffset)^,FLength);
   end;
  
  {Update the Next Write}
  FWrite:=WriteNext;
  
  {$IFDEF ICMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP Buffer: WriteBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
  {$ENDIF}
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ICMPInit;
begin
 {}
 {Check Initialized}
 if ICMPInitialized then Exit;

 {Setup ICMP Protocol}
 if NetworkSettings.GetBoolean('IP_TRANSPORT_ENABLED') then NetworkSettings.AddBoolean('ICMP_PROTOCOL_ENABLED',True);
 
 {Create ICMP Protocol}
 if NetworkSettings.GetBooleanDefault('ICMP_PROTOCOL_ENABLED',ICMP_PROTOCOL_ENABLED) then 
  begin
   TICMPProtocol.Create(ProtocolManager,ICMP_PROTOCOL_NAME);
  end; 
 
 ICMPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ICMP Functions}
function CheckICMP(AFamily:Word;ABuffer:Pointer):Boolean;
{Verify that the packet is a valid ICMP packet}
{Buffer: The complete packet including Transport header}
var
 Length:Word;
 ICMP:PICMPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF ICMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP: CheckICMP');
 {$ENDIF}
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    ICMP:=PICMPHeader(PtrUInt(ABuffer) + GetICMPHeaderOffset(AF_INET,ABuffer));
        
    {Check Data Length}
    Length:=GetIPDataLength(ABuffer);
    if Length >= ICMP_HEADER_SIZE then
     begin
      {Validate the Header Checksum}
      {$IFDEF ICMP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Data Length = ' + IntToStr(Length));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Original Checksum   = ' + IntToHex(WordBEtoN(ICMP.Unused.Checksum),4));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMP:  Calculated Checksum = ' + IntToHex(WordBEtoN(ChecksumICMP(AF_INET,ICMP,0,Length)),4));
      {$ENDIF}
      if ICMP.Unused.Checksum = $FFFF then ICMP.Unused.Checksum:=0; {Allow for all 1s case}
      if ICMP.Unused.Checksum = ChecksumICMP(AF_INET,ICMP,0,Length) then
       begin
        Result:=True;
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function GetICMPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Start of ICMP Header (Length of IP Header)}
    Result:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
   end;
 end;
end;

{==============================================================================}

function GetICMPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Start of the ICMP Header (Length of IP Header)}
    Offset:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
    
    {Return Size of ICMP Header based on ICMPType Field}
    case PICMPHeader(PtrUInt(ABuffer) + Offset).Unused.ICMPType of
     ICMP_UNREACH,ICMP_TIMXCEED,ICMP_SOURCEQUENCH:begin
       Result:=SizeOf(TICMPUnusedHeader);
      end;
     ICMP_PARAMPROB:begin
       Result:=SizeOf(TICMPPointerHeader);
      end;
     ICMP_REDIRECT:begin
       Result:=SizeOf(TICMPIpHeader);
      end;
     ICMP_ECHO,ICMP_ECHOREPLY:begin
       Result:=SizeOf(TICMPEchoHeader);
      end;
     ICMP_TSTAMP,ICMP_TSTAMPREPLY:begin
       Result:=SizeOf(TICMPTimestampHeader);
      end;
     ICMP_IREQ,ICMP_IREQREPLY:begin
       Result:=SizeOf(TICMPInfoHeader);
      end;
     ICMP_MASKREQ,ICMP_MASKREPLY:begin
       Result:=SizeOf(TICMPMaskHeader);
      end;
     ICMP_ROUTERADVERT:begin
       Result:=SizeOf(TICMPAdvertHeader);
      end;
     ICMP_ROUTERSOLICIT:begin
       Result:=SizeOf(TICMPSolicitHeader);
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function GetICMPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Start of ICMP Data (If Any) (IP Header + ICMP Header)}
    Result:=GetICMPHeaderOffset(AFamily,ABuffer) + GetICMPHeaderLength(AFamily,ABuffer);
   end;
 end;
end;

{==============================================================================}

function GetICMPDataLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Size of ICMP Data (If Any) based on IP Total Length Field - IP Header - ICMP Header}
    Result:=WordBEtoN(PIPHeader(ABuffer).TotalLength);
    Result:=(Result - GetICMPHeaderOffset(AFamily,ABuffer)) - GetICMPHeaderLength(AFamily,ABuffer);
   end;
 end;
end;

{==============================================================================}

function ChecksumICMP(AFamily:Word;ABuffer:Pointer;AOffset,ALength:Word):Word;
{Checksum the ICMP Header on Send / Validate the Checksum on Receive}
var
 Original:Word;
 ICMP:PICMPHeader;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    ICMP:=PICMPHeader(PtrUInt(ABuffer) + AOffset);
    
    {Save Checksum}
    Original:=ICMP.Unused.Checksum;
    ICMP.Unused.Checksum:=0;
    
    {Calculate 1s Compliment Checksum on ICMP Header and Data}
    Result:=GetChecksum(ABuffer,AOffset,ALength);
    
    {Restore Checksum}
    ICMP.Unused.Checksum:=Original;
   end;
 end;
end;
  
{==============================================================================}
{==============================================================================}
{ICMP Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 ICMPInit;

{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 