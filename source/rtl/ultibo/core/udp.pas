{
Ultibo UDP (User Datagram Protocol) unit.

Copyright (C) 2021 - SoftOz Pty Ltd.

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

 
User Datagram Protocol
======================

 Notes: UDP Checksum includes both Header and Data
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit UDP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,SysUtils,Classes,Network,Transport,Protocol,IP,IPv6,ICMP,ICMPv6,Ultibo,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {UDP specific constants}
 {Note: Some UDP definitions are in the Protocol or IP modules}
 UDP_PROTOCOL_NAME = 'UDP';
 
 {UDP Constants}
 MIN_UDP_PACKET =  8;      {Not Counting Adapter and Transport Header}
 MAX_UDP_PACKET =  8192;   {Not Counting Adapter and Transport Header}

 UDP_TIMEOUT = 0;          {Wait forever on a UDP Read}
 UDP_BUFFER_SIZE = 65536;  {UDP Receive Buffer Size}

 UDP_MAX_PORT = 65536;

 UDP_HEADER_SIZE = 8;      {SizeOf(TUDPHeader);}

 UDP_DATAGRAM_SIZE = 8;    {SizeOf(TUDPDatagram)}

 {UDP Socket Options}
 {See Sockets.pas}

 UDP_PORT_START = 49152;    {First dynamic port (Previously 1024)} {As per IANA assignment}
 UDP_PORT_STOP  = 65534;    {Last dynamic port (Previously 5000)}  {Short of IANA assignment to allow for rollover}
  
{==============================================================================}
type
 {UDP specific types}
 {Note: Some UDP definitions are in the Protocol or IP modules}
 PUDPHeader = ^TUDPHeader;
 TUDPHeader = packed record   {8 Bytes}
  SourcePort:Word; {Network Order}
  DestPort:Word;   {Network Order}
  Length:Word;     {Network Order}
  Checksum:Word;
 end;

 PUDPDatagram = ^TUDPDatagram;
 TUDPDatagram = record   {8 Bytes} {Used by UDPBuffer} 
  Size:Word;             {Word to keep size even}
  RemotePort:Word;
  Next:PUDPDatagram;
 end; {Followed by RemoteAddress (4 or 16 Bytes)}
  
{==============================================================================}
type
 {UDP specific classes}
 TUDPSocket = class;
 TUDPProtocolTransport = class(TProtocolTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TUDPProtocol = class(TNetworkProtocol)
   constructor Create(AManager:TProtocolManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FNextPort:Word;

   {Status Variables}

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
  protected
   {Inherited Methods}
   function OpenPort(ASocket:TProtocolSocket;APort:Word):Boolean; override;
   function ClosePort(ASocket:TProtocolSocket):Boolean; override;
   function FindPort(APort:Word;AWrite,ALock:Boolean):TProtocolPort; override;

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

 TUDPState = class;
 TUDPBuffer = class;
 TUDPOptions = class;
 TUDPSocket = class(TProtocolSocket)  {SOCK_DGRAM}
   constructor Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   
   {UDP Layer Variables}
   
   {Data Layer Variables}
   FRecvData:TUDPBuffer;
  public
   {UDP Layer Properties}

   {Data Layer Properties}
   property RecvData:TUDPBuffer read FRecvData;

   {Public Methods}
   function GetOption(ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; override;
   function SetOption(ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; override;
   function IoCtl(ACommand:DWORD;var AArgument:u_long):Integer; override;

   function IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
   function IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
 end;

 TUDPState = class(TProtocolState)
   constructor Create;
  private
   {Internal Variables}
   
   
  public
   {Status Variables}
   
 end;

 TUDPBuffer = class(TSocketBuffer)
   constructor Create(ASocket:TTransportSocket);
   destructor Destroy; override;
  private
   {Internal Variables}
   FOffset:Word;            {Offset to RemoteAddress}
   FLength:Word;            {Length of RemoteAddress}

   FRead:Pointer;           {Pointer to Next Read from Buffer}
   FWrite:Pointer;          {Pointer to Next Write to Buffer}

   {Status Variables}
   FCount:LongWord;         {Number of Datagrams in Buffer}

   FFirst:PUDPDatagram;     {Pointer to First Datagram}
   FLast:PUDPDatagram;      {Pointer to Last Datagram}

   {Internal Methods}
   function AddDatagram(ASize:Integer):Boolean;
   function RemoveDatagram:Boolean;
   procedure FlushDatagrams;
  protected
   {Inherited Methods}
   procedure SetSize(ASize:LongWord); override;
  public
   {Public Methods}
   function GetNext:Integer;
   function GetCount:LongWord;

   function ReadBuffer(var ABuffer;var ASize:Integer;ARemoteAddress:Pointer;ARemotePort:PWORD;AFlags:Integer):Boolean;
   function WriteBuffer(var ABuffer;ASize:Integer;ARemoteAddress:Pointer;ARemotePort:PWORD):Boolean;
 end;

 TUDPOptions = class(TProtocolOptions)  {For Get/Set Options Level = IPPROTO_UDP Option = ????}
   constructor Create;
  private
   {Internal Variables}
   FOptions:LongWord;

   {Internal Methods}
   function GetNoChecksum:Boolean;
   procedure SetNoChecksum(ANoChecksum:Boolean);
  public
   {Public Properties}
   property NoChecksum:Boolean read GetNoChecksum write SetNoChecksum;
 end;
  
{==============================================================================}
{var}
 {UDP specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure UDPInit;

{==============================================================================}
{UDP Functions}
function CheckUDP(AFamily:Word;ABuffer:Pointer):Boolean;

function GetUDPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetUDPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
function GetUDPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetUDPDataLength(AFamily:Word;ABuffer:Pointer):Word;

function ChecksumUDPRecv(AFamily:Word;APseudo:PIPPseudo;ABuffer:Pointer;AOffset,ALength:Word):Word;
function ChecksumUDPSend(AFamily:Word;APseudo:PIPPseudo;AHeader:PUDPHeader;AData:Pointer;ALength:Word):Word;
  
{==============================================================================}
{UDP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {UDP specific variables}
 UDPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TUDPProtocol}
constructor TUDPProtocol.Create(AManager:TProtocolManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FProtocol:=IPPROTO_UDP;
 FSocketType:=SOCK_DGRAM;

 FNextPort:=UDP_PORT_START;
end;

{==============================================================================}

destructor TUDPProtocol.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TUDPProtocol.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
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
 IP6:PIP6Header;
 UDP:PUDPHeader;
 ICMP:PICMPHeader;
 ICMP6:PICMP6Header;
 Socket:TUDPSocket;
 Transport:TUDPProtocolTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}
 
 {Check Source}
 {if ASource = nil then Exit;} {Not Used} 
  
 {Check Packet}
 if APacket = nil then Exit;
 
 {Get Transport}
 Transport:=TUDPProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Family = ' + AddressFamilyToString(Transport.Transport.Family));
  {$ENDIF}

  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Get Header}  
     IP:=PIPHeader(APacket);
    
     {$IFDEF UDP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Protocol = ' + ProtocolToString(Transport.Protocol));
     {$ENDIF}
    
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_UDP:begin
        {Check for a valid UDP Packet}
        if CheckUDP(AF_INET,IP) then
         begin
          {Get Header}
          UDP:=PUDPHeader(PtrUInt(IP) + GetUDPHeaderOffset(AF_INET,IP));
         
          {Set the Ports to Host order}
          UDP.DestPort:=WordBEtoN(UDP.DestPort);
          UDP.SourcePort:=WordBEtoN(UDP.SourcePort);
         
          {$IFDEF UDP_DEBUG}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  DestPort = ' + IntToStr(UDP.DestPort));
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  SourcePort = ' + IntToStr(UDP.SourcePort));
          {$ENDIF}
         
          {Check for a Connected Socket}
          Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.DestIP,@IP.SourceIP,UDP.DestPort,UDP.SourcePort,ABroadcast,False,True,NETWORK_LOCK_READ));
          if Socket = nil then
           begin
            {Check for a Listening Socket}
            Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.DestIP,@IP.SourceIP,UDP.DestPort,UDP.SourcePort,ABroadcast,True,True,NETWORK_LOCK_READ));
           end;
          if Socket <> nil then
           begin
            {Get the Data Offset}
            Offset:=GetUDPDataOffset(AF_INET,IP);
            Length:=GetUDPDataLength(AF_INET,IP);
            
            {$IFDEF UDP_DEBUG}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Offset = ' + IntToStr(Offset) + ' Length = ' + IntToStr(Length));
            {$ENDIF}
            
            {Write the Data into the Receive Buffer}
            Socket.RecvData.WriteBuffer(Pointer(PtrUInt(IP) + Offset)^,Length,@IP.SourceIP,@UDP.SourcePort);
            
            {Signal the Event}
            Socket.SignalChange;
            
            {Return True even if the Write failed (eg Buffer is Full, Socket Shutdown etc)}
            Result:=True;
           
            {Unlock Socket}
            Socket.ReaderUnlock;
           end
          else
           begin
            {If there was no Socket send an ICMP_UNREACH/PORT}
            if not ABroadcast then
             begin
              {Set the Ports to Network order}
              UDP.DestPort:=WordNtoBE(UDP.DestPort);
              UDP.SourcePort:=WordNtoBE(UDP.SourcePort);
              
              {Send Control}
              Result:=TIPTransport(Transport.Transport).SendControl(@IP.DestIP,@IP.SourceIP,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_PORT,nil,IP,ASize);
             end;
           end;
         end
        else
         begin
          {Silently consume and discard a bad UDP packet}
          Result:=True;
         end;
       end;
      IPPROTO_ICMP:begin
        {Check for a valid ICMP Packet}
        if CheckICMP(AF_INET,IP) then
         begin
          {Get Header}
          ICMP:=PICMPHeader(PtrUInt(IP) + GetICMPHeaderOffset(AF_INET,IP));
          
          {Check for a Type that we handle}
          case ICMP.Unused.ICMPType of
           ICMP_UNREACH:begin
             {$IFDEF UDP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  ICMP Unreach');
             {$ENDIF}
             
             {Check the Protocol of the original packet}
             if ICMP.Unreach.IP.Protocol <> IPPROTO_UDP then Exit;
             
             {Check that we sent the original packet}
             if TIPTransport(Transport.Transport).GetAddressByAddress(InAddrToHost(ICMP.Unreach.IP.SourceIP),False,NETWORK_LOCK_NONE) = nil then Exit;
             
             {Check that the Dest is not Broadcast}
             if ABroadcast then Exit;
             
             {Find the Socket}
             Result:=True;
             
             {Set the Addresses to Host order}
             IP:=@ICMP.Unreach.IP;
             IP.DestIP:=InAddrToHost(IP.DestIP);
             IP.SourceIP:=InAddrToHost(IP.SourceIP);
             
             {Set the Ports to Host order}
             UDP:=@ICMP.Unreach.Data;
             UDP.DestPort:=WordBEtoN(UDP.DestPort);
             UDP.SourcePort:=WordBEtoN(UDP.SourcePort);
             
             {Check for a Connected Socket}
             Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,False,True,NETWORK_LOCK_READ));
             if Socket = nil then
              begin
               {Check for a Listening Socket}
               Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,True,True,NETWORK_LOCK_READ));
              end;
             if Socket = nil then Exit;
             
             {Set the Socket Error}
             case ICMP.Unreach.Code of
              ICMP_UNREACH_NET:Socket.SocketError:=WSAENETUNREACH;
              ICMP_UNREACH_HOST:Socket.SocketError:=WSAEHOSTUNREACH;
              ICMP_UNREACH_PROTOCOL,ICMP_UNREACH_PORT:Socket.SocketError:=WSAECONNREFUSED;
             end;
             
             {Unlock Socket}
             Socket.ReaderUnlock;
            end;
           ICMP_SOURCEQUENCH:begin
             {$IFDEF UDP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  ICMP Source Quench');
             {$ENDIF}
             
             {Check the Protocol of the original packet}
             if ICMP.Quench.IP.Protocol <> IPPROTO_UDP then Exit;
             
             {Check that we sent the original packet}
             if TIPTransport(Transport.Transport).GetAddressByAddress(InAddrToHost(ICMP.Quench.IP.SourceIP),False,NETWORK_LOCK_NONE) = nil then Exit;
             
             {Check that the Dest is not Broadcast}
             if ABroadcast then Exit;
            
             {Find the Socket}
             Result:=True;
            
             {Set the Addresses to Host order}
             IP:=@ICMP.Quench.IP;
             IP.DestIP:=InAddrToHost(IP.DestIP);
             IP.SourceIP:=InAddrToHost(IP.SourceIP);
            
             {Set the Ports to Host order}
             UDP:=@ICMP.Quench.Data;
             UDP.DestPort:=WordBEtoN(UDP.DestPort);
             UDP.SourcePort:=WordBEtoN(UDP.SourcePort);
             
             {Check for a Connected Socket}
             Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,False,True,NETWORK_LOCK_READ));
             if Socket = nil then
              begin
               {Check for a Listening Socket}
               Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,True,True,NETWORK_LOCK_READ));
              end;
             if Socket = nil then Exit;
            
             {Set the Socket Error}
             Socket.SocketError:=WSAECONNRESET; {Not Really}
             
             {Unlock Socket}
             Socket.ReaderUnlock;
            end;
           ICMP_TIMXCEED:begin
             {$IFDEF UDP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  ICMP Expire');
             {$ENDIF}
             
             {Check the Protocol of the original packet}
             if ICMP.Expire.IP.Protocol <> IPPROTO_UDP then Exit;
            
             {Check that we sent the original packet}
             if TIPTransport(Transport.Transport).GetAddressByAddress(InAddrToHost(ICMP.Expire.IP.SourceIP),False,NETWORK_LOCK_NONE) = nil then Exit;
             
             {Check that the Dest is not Broadcast}
             if ABroadcast then Exit;
             
             {Find the Socket}
             Result:=True;
             
             {Set the Addresses to Host order}
             IP:=@ICMP.Expire.IP;
             IP.DestIP:=InAddrToHost(IP.DestIP);
             IP.SourceIP:=InAddrToHost(IP.SourceIP);
            
             {Set the Ports to Host order}
             UDP:=@ICMP.Expire.Data;
             UDP.DestPort:=WordBEtoN(UDP.DestPort);
             UDP.SourcePort:=WordBEtoN(UDP.SourcePort);
             
             {Check for a Connected Socket}
             Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,False,True,NETWORK_LOCK_READ));
             if Socket = nil then
              begin
               {Check for a Listening Socket}
               Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,True,True,NETWORK_LOCK_READ));
              end;
             if Socket = nil then Exit;
            
             {Set the Socket Error}
             Socket.SocketError:=WSAETIMEDOUT; {Not Really}
            
             {Unlock Socket}
             Socket.ReaderUnlock;
            end;
           ICMP_PARAMPROB:begin
             {$IFDEF UDP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  ICMP Param Problem');
             {$ENDIF}
             
             {Check the Protocol of the original packet}
             if ICMP.Param.IP.Protocol <> IPPROTO_UDP then Exit;
            
             {Check that we sent the original packet}
             if TIPTransport(Transport.Transport).GetAddressByAddress(InAddrToHost(ICMP.Param.IP.SourceIP),False,NETWORK_LOCK_NONE) = nil then Exit;
             
             {Check that the Dest is not Broadcast}
             if ABroadcast then Exit;
             
             {Find the Socket}
             Result:=True;
             
             {Set the Addresses to Host order}
             IP:=@ICMP.Param.IP;
             IP.DestIP:=InAddrToHost(IP.DestIP);
             IP.SourceIP:=InAddrToHost(IP.SourceIP);
             
             {Set the Ports to Host order}
             UDP:=@ICMP.Param.Data;
             UDP.DestPort:=WordBEtoN(UDP.DestPort);
             UDP.SourcePort:=WordBEtoN(UDP.SourcePort);
             
             {Check for a Connected Socket}
             Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,False,True,NETWORK_LOCK_READ));
             if Socket = nil then
              begin
               {Check for a Listening Socket}
               Socket:=TUDPSocket(FindSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,@IP.SourceIP,@IP.DestIP,UDP.SourcePort,UDP.DestPort,ABroadcast,True,True,NETWORK_LOCK_READ));
              end;
             if Socket = nil then Exit;
             
             {Set the Socket Error}
             Socket.SocketError:=WSAECONNABORTED; {Not Really}
             
             {Unlock Socket}
             Socket.ReaderUnlock;
            end;
          end;
         end;
       end;
     end;
    end;
   AF_INET6:begin
     {Get Header}  
     IP6:=PIP6Header(APacket);
   
     //To Do //Extensions
     
     {$IFDEF UDP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Protocol = ' + ProtocolToString(Transport.Protocol));
     {$ENDIF}
   
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_UDP:begin
        {Check for a valid UDP Packet}
        if CheckUDP(AF_INET6,IP6) then
         begin
          {Get Header}
          UDP:=PUDPHeader(PtrUInt(IP6) + GetUDPHeaderOffset(AF_INET6,IP6));
         
          {Set the Ports to Host order}
          UDP.DestPort:=WordBEtoN(UDP.DestPort);
          UDP.SourcePort:=WordBEtoN(UDP.SourcePort);
      
          {Check for a Connected Socket}
          Socket:=TUDPSocket(FindSocket(AF_INET6,SOCK_DGRAM,IPPROTO_UDP,@IP6.DestIP,@IP6.SourceIP,UDP.DestPort,UDP.SourcePort,ABroadcast,False,True,NETWORK_LOCK_READ));
          if Socket = nil then
           begin
            {Check for a Listening Socket}
            Socket:=TUDPSocket(FindSocket(AF_INET6,SOCK_DGRAM,IPPROTO_UDP,@IP6.DestIP,@IP6.SourceIP,UDP.DestPort,UDP.SourcePort,ABroadcast,True,True,NETWORK_LOCK_READ));
           end;
          if Socket <> nil then
           begin
            {Get the Data Offset}
            Offset:=GetUDPDataOffset(AF_INET6,IP6);
            Length:=GetUDPDataLength(AF_INET6,IP6);
            
            {$IFDEF UDP_DEBUG}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Offset = ' + IntToStr(Offset) + ' Length = ' + IntToStr(Length));
            {$ENDIF}
            
            {Write the Data into the Receive Buffer}
            Socket.RecvData.WriteBuffer(Pointer(PtrUInt(IP6) + Offset)^,Length,@IP6.SourceIP,@UDP.SourcePort);
            
            {Signal the Event}
            Socket.SignalChange;
            
            {Return True even if the Write failed (eg Buffer is Full, Socket Shutdown etc)}
            Result:=True;
           
            {Unlock Socket}
            Socket.ReaderUnlock;
           end
          else
           begin
            {If there was no Socket send an ICMP_UNREACH/PORT}
            if not ABroadcast then
             begin
              {Set the Ports to Network order}
              UDP.DestPort:=WordNtoBE(UDP.DestPort);
              UDP.SourcePort:=WordNtoBE(UDP.SourcePort);
              
              {Send Control}
              Result:=TIP6Transport(Transport.Transport).SendControl(@IP6.DestIP,@IP6.SourceIP,IPPROTO_ICMP,ICMP_UNREACH,ICMP_UNREACH_PORT,nil,IP6,ASize);
             end;
           end;
         end
        else
         begin
          {Silently consume and discard a bad UDP packet}
          Result:=True;
         end;
       end;
      IPPROTO_ICMPV6:begin
        {Check for a valid ICMP Packet}
        if CheckICMP(AF_INET6,IP6) then
         begin
          {Get Header}
          ICMP6:=PICMP6Header(PtrUInt(IP6) + GetICMP6HeaderOffset(AF_INET6,IP6));
        
          {Check for a Type that we handle}
          
          //To do
          
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

function TUDPProtocol.OpenPort(ASocket:TProtocolSocket;APort:Word):Boolean;

{Note: Caller must hold the Socket lock}
var
 Start:Word;
 Port:TProtocolPort;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: OpenPort');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Port = '  + IntToStr(APort));
  {$ENDIF}
  
  {Check Socket}
  if ASocket = nil then Exit;
  
  {Check for Any Port}
  if APort <> IPPORT_ANY then
   begin
    {Acquire Lock}
    FPorts.WriterLock;
    try
     {Use the specified port if available}
     Port:=FindPort(APort,True,True); {Writer Lock / Lock Return}
     if Port <> nil then
      begin
       //To Do //Check the LocalAddress as well as Port
       {If already in use check ReuseAddress option}
       if ASocket.SocketOptions.ReuseAddress then
        begin
         {Add Socket}
         Port.Sockets.Add(ASocket);
         
         {Set Local Port}
         ASocket.ProtocolState.LocalPort:=APort;
         
         {Return Result}
         Result:=True;
        end;
        
       {Unlock Port}
       Port.ReleaseLock;      
      end
     else
      begin
       {Create Port}
       Port:=TProtocolPort.Create;
       Port.Port:=APort;
       
       {Add Socket}
       Port.Sockets.Add(ASocket);
       
       {Add Port}
       FPorts.Add(Port);
       
       {Set Local Port}
       ASocket.ProtocolState.LocalPort:=APort;
       
       {Return Result}
       Result:=True;
      end;
    finally
     {Release Lock}
     FPorts.WriterUnlock;
    end;  
   end
  else
   begin
    {Acquire Lock}
    FPorts.WriterLock;
    try
     {Auto assign a dynamic port}
     Start:=FNextPort;
     while FindPort(FNextPort,True,False) <> nil do {Writer Lock / Do not lock Return}
      begin
       //To Do //Check the LocalAddress as well as Port
       {Increment Port}
       Inc(FNextPort);
       
       {Check for wrap around}
       if FNextPort > UDP_PORT_STOP then FNextPort:=UDP_PORT_START;
       
       {Check for a complete cycle}
       if FNextPort = Start then Exit;
      end;
     
     {Create Port}
     Port:=TProtocolPort.Create;
     Port.Port:=FNextPort;
     
     {Add Socket}
     Port.Sockets.Add(ASocket);
    
     {Add Port}
     FPorts.Add(Port);
    
     {Set Local Port}
     ASocket.ProtocolState.LocalPort:=FNextPort;
    
     {Return Result}
     Result:=True;
    finally
     {Release Lock}
     FPorts.WriterUnlock;
    end;  
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TUDPProtocol.ClosePort(ASocket:TProtocolSocket):Boolean;

{Note: Caller must hold the Socket lock}
var
 Port:TProtocolPort;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: ClosePort');
  {$ENDIF}
  
  {Check Socket}
  if ASocket = nil then Exit;
  
  {Get Port}
  Port:=FindPort(ASocket.ProtocolState.LocalPort,False,True); {Reader Lock / Lock Return}
  if Port = nil then Exit;
  
  {Remove Socket}
  Port.Sockets.Remove(ASocket);
  
  {Check Count}
  if Port.Sockets.Count = 0 then
   begin
    {Acquire Lock}
    FPorts.WriterLock;
    try
     {Remove Port}
     FPorts.Remove(Port);
    
     {Unlock Port}
     Port.ReleaseLock;
    
     {Destroy Port}
     Port.Free;
    finally
     {Release Lock}
     FPorts.WriterUnlock;
    end;  
   end
  else
   begin
    {Unlock Port}
    Port.ReleaseLock;
   end;   
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TUDPProtocol.FindPort(APort:Word;AWrite,ALock:Boolean):TProtocolPort;
{Find a protocol port for the specified port number}
{Port: The port number to find}
{Write: If True then use the writer lock}
{Lock: If True then lock the found entry before returning}
var
 Port:TProtocolPort;
begin
 {}
 Result:=nil;
 
 if AWrite then
  begin
   if not FPorts.WriterLock then Exit;
  end
 else
  begin 
   if not FPorts.ReaderLock then Exit;
  end; 
 try
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: FindPort');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Port = '  + IntToStr(APort));
  {$ENDIF}
  
  {Check Port}
  if APort = IPPORT_ANY then Exit;
  
  {Get Port}
  Port:=TProtocolPort(FPorts.First);
  while Port <> nil do
   begin
    {Check Port}
    if Port.Port = APort then
     begin
      {Lock Port}
      if ALock then Port.AcquireLock;
      
      {Return Result}
      Result:=Port;
      Exit;
     end;
     
    {Get Next}
    Port:=TProtocolPort(Port.Next);
   end;
 finally 
  if AWrite then
   begin
    FPorts.WriterUnlock;
   end
  else
   begin  
    FPorts.ReaderUnlock;
   end; 
 end; 
end;

{==============================================================================}

function TUDPProtocol.SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer;
{Source is the working set to check, Dest is the set passed to Select}
var
 Count:Integer;
 Socket:TUDPSocket;
begin
 {}
 Result:=0;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: SelectCheck');
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
      Socket:=TUDPSocket(ASource.fd_array[Count]);
      
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
      Socket:=TUDPSocket(ASource.fd_array[Count]);
      
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
    Result:=SOCKET_ERROR;
    
    {Get Sockets}
    for Count:=ASource.fd_count - 1 downto 0 do
     begin
      {Get Socket}
      Socket:=TUDPSocket(ASource.fd_array[Count]);
      
      {Check Socket}
      if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
      
      {Check Error}
      if Socket.SocketError <> ERROR_SUCCESS then
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
 end;
end;

{==============================================================================}

function TUDPProtocol.SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; 
{Socket is the single socket to check, Code is the type of check, Timeout is how long to wait}
var
 StartTime:Int64;
 Socket:TUDPSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: SelectWait');
 {$ENDIF}

 {Get Socket}
 Socket:=TUDPSocket(ASocket);
 
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
     {Wait for Error}
     StartTime:=GetTickCount64;
     while Socket.SocketError = ERROR_SUCCESS do
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
  end;
 
 finally
  {Unlock Socket} 
  Socket.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TUDPProtocol.SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
{Send a Packet by adding the Protocol Header and other details to the Data}
{Socket: The socket to use for sending the packet}
{Source: The source address of the packet (Host Order)}
{Dest: The destination address of the packet (Host Order)}
{SourcePort: The source port of the packet (Host Order)}
{DestPort: The destination port of the packet (Host Order)}
{Packet: The packet data to send}
{Size: The size of the packet data in bytes}
{Flags: Any protocol specific flags for sending}

{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 UDP:TUDPHeader;
 Pseudo:TIPPseudo;
 Route:TRouteEntry;
 Packet:TPacketFragment;
 Transport:TUDPProtocolTransport;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: SendPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check the Packet Size}
 NetworkSetLastError(WSAEMSGSIZE);
 if ASize > MAX_UDP_PACKET then Exit;
 
 {Get the Size of the Packet}
 Size:=ASize + UDP_HEADER_SIZE;
 
 {Fill in the UDP fields}
 UDP.SourcePort:=WordNtoBE(ASourcePort);
 UDP.DestPort:=WordNtoBE(ADestPort);
 UDP.Length:=WordNtoBE(Size); 
 UDP.Checksum:=0;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Check Options}
    if not TUDPOptions(ASocket.ProtocolOptions).NoChecksum then
     begin
      {Check for Default}
      if TIPTransport(ASocket.Transport).CompareDefault(PInAddr(ASource)^) then
       begin
        {Get the Route}
        NetworkSetLastError(WSAENETUNREACH);
        Route:=TIPTransport(ASocket.Transport).GetRouteByAddress(PInAddr(ADest)^,True,NETWORK_LOCK_READ);
        if Route = nil then Exit;
        
        {Fill in the Pseudo Header}
        Pseudo.SourceIP:=InAddrToNetwork(TIPRouteEntry(Route).Address);
        
        {Unlock Route}
        Route.ReaderUnlock;
       end
      else
       begin
        {Fill in the Pseudo Header}
        Pseudo.SourceIP:=InAddrToNetwork(PInAddr(ASource)^);
       end;
      
      {Fill in the Pseudo Header}
      Pseudo.DestIP:=InAddrToNetwork(PInAddr(ADest)^);
      Pseudo.Mbz:=0;
      Pseudo.Protocol:=IPPROTO_UDP;
      Pseudo.Length:=WordNtoBE(Size); 
      
      {Calculate the Checksum}
      UDP.Checksum:=ChecksumUDPSend(AF_INET,@Pseudo,@UDP,APacket.Data,ASize);
     end;
    
    {Create the Fragment}
    Packet.Size:=UDP_HEADER_SIZE;
    Packet.Data:=@UDP;
    Packet.Next:=APacket;
    
    {Get Transport}
    Transport:=TUDPProtocolTransport(GetTransportByTransport(ASocket.Transport,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;
    
    {Send the Packet}
    if TIPTransport(ASocket.Transport).SendPacket(ASocket,ASource,ADest,@Packet,Size,AFlags) = Size then
     begin
      {Return passed size not sent size}
      Result:=ASize;
     end;

    {Unlock Transport}
    Transport.ReaderUnlock;
   end;
  AF_INET6:begin
    
    //To Do
    
   end;
 end;
end;

{==============================================================================}

function TUDPProtocol.Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket;
{BSD compatible Accept}
{Socket: The socket to accept from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=TProtocolSocket(INVALID_SOCKET);
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Accept');
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

function TUDPProtocol.Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Bind}
{Sets the LocalAddress/Port for future Sends and Receives, Address can be specified as INADDR_ANY which allows Listening or auto assignment}
{If Port is IPPORT_ANY then a dynamic Port will be assigned}
{Socket: The socket to bind}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Bind');
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
   case ASocket.Family of
    AF_INET:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> ASockAddr.sin_family then Exit;
    
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Check LocalAddress}
      if not TIPTransport(ASocket.Transport).CompareDefault(InAddrToHost(ASockAddr.sin_addr)) then
       begin
        NetworkSetLastError(WSAEADDRNOTAVAIL);
        if TIPTransport(ASocket.Transport).GetAddressByAddress(InAddrToHost(ASockAddr.sin_addr),False,NETWORK_LOCK_NONE) = nil then Exit;
       end;
      
      {Bind the Port}
      NetworkSetLastError(WSAEADDRINUSE);
      if not OpenPort(ASocket,WordBEtoN(ASockAddr.sin_port)) then Exit;
      
      {Bind the Address}
      ASocket.SocketState.LocalAddress:=True;
      TIPState(ASocket.TransportState).LocalAddress:=InAddrToHost(ASockAddr.sin_addr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> SockAddr6.sin6_family then Exit;
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Check LocalAddress}
      if not TIP6Transport(ASocket.Transport).CompareDefault(SockAddr6.sin6_addr) then
       begin
        NetworkSetLastError(WSAEADDRNOTAVAIL);
        if TIP6Transport(ASocket.Transport).GetAddressByAddress(SockAddr6.sin6_addr,False,NETWORK_LOCK_NONE) = nil then Exit;
       end;
      
      {Bind the Port}
      NetworkSetLastError(WSAEADDRINUSE);
      if not OpenPort(ASocket,WordBEtoN(SockAddr6.sin6_port)) then Exit;
      
      {Bind the Socket}
      ASocket.SocketState.LocalAddress:=True;
      TIP6State(ASocket.TransportState).LocalAddress:=SockAddr6.sin6_addr;
      
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

function TUDPProtocol.CloseSocket(ASocket:TProtocolSocket):Integer;
{BSD compatible Close Socket}
{Closes and removes the socket, does not perform Linger}
{Socket: The socket to close}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: CloseSocket');
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

function TUDPProtocol.Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Connect}
{Sets the RemoteAddress/Port of future Sends and Receives, if Bind has not been called then the
 LocalAddress/Port will be set appropriately as well based on the route to the RemoteAddress}
{Socket: The socket to connect}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
var
 Route:TRouteEntry;
 Address:TAddressEntry;
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Connect');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAEISCONN);
   if ASocket.SocketState.Connected then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> ASockAddr.sin_family then Exit;
    
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Check for Default Remote Port}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if WordBEtoN(ASockAddr.sin_port) = IPPORT_ANY then Exit;
      
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
       try
        {Check the Binding}
        if not ASocket.SocketState.LocalAddress then
         begin
          {Bind the Port}
          NetworkSetLastError(WSAEADDRINUSE);
          if not OpenPort(ASocket,WordBEtoN(IPPORT_ANY)) then Exit;
          
          {Bind the Address}
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
        ASocket.ProtocolState.RemotePort:=WordBEtoN(ASockAddr.sin_port);
        TIPState(ASocket.TransportState).RemoteAddress:=InAddrToHost(ASockAddr.sin_addr);
       
        {Signal the Event}
        ASocket.SignalChange;
       
        {Return Result}
        NetworkSetLastError(ERROR_SUCCESS);
        Result:=NO_ERROR;
       finally
        Address.ReaderUnlock;
       end;
      finally
       Route.ReaderUnlock;
      end;      
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> SockAddr6.sin6_family then Exit;
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Check for Default Remote Port}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if WordBEtoN(SockAddr6.sin6_port) = IPPORT_ANY then Exit;
      
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIP6Transport(ASocket.Transport).CompareDefault(SockAddr6.sin6_addr) then Exit;
      
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIP6Transport(ASocket.Transport).CompareBroadcast(SockAddr6.sin6_addr) or TIP6Transport(ASocket.Transport).CompareDirected(SockAddr6.sin6_addr) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Check the Route}
      NetworkSetLastError(WSAENETUNREACH);
      Route:=TIP6Transport(ASocket.Transport).GetRouteByAddress(SockAddr6.sin6_addr,True,NETWORK_LOCK_READ);
      if Route = nil then Exit;
      try
       {Check the LocalAddress}
       NetworkSetLastError(WSAEADDRNOTAVAIL);
       Address:=TIP6Transport(ASocket.Transport).GetAddressByAddress(TIP6RouteEntry(Route).Address,True,NETWORK_LOCK_READ);
       if Address = nil then Exit;
       try
        {Check the Binding}
        if not ASocket.SocketState.LocalAddress then
         begin
          {Bind the Port}
          NetworkSetLastError(WSAEADDRINUSE);
          if not OpenPort(ASocket,WordBEtoN(IPPORT_ANY)) then Exit;
          
          {Bind the Address}
          ASocket.SocketState.LocalAddress:=True;
          TIP6State(ASocket.TransportState).LocalAddress:=TIP6AddressEntry(Address).Address;
         end;
         
        {Check for Default Binding}
        if TIP6Transport(ASocket.Transport).CompareDefault(TIP6State(ASocket.TransportState).LocalAddress) then
         begin
          {Set the LocalAddress}
          TIP6State(ASocket.TransportState).LocalAddress:=TIP6AddressEntry(Address).Address;
         end;
         
        {Connect the Socket}
        ASocket.SocketState.Connected:=True;
        ASocket.SocketState.RemoteAddress:=True;
        ASocket.ProtocolState.RemotePort:=WordBEtoN(SockAddr6.sin6_port);
        TIP6State(ASocket.TransportState).RemoteAddress:=SockAddr6.sin6_addr;
      
        {Signal the Event}
        ASocket.SignalChange;
      
        {Return Result}
        NetworkSetLastError(ERROR_SUCCESS);
        Result:=NO_ERROR;
       finally
        Address.ReaderUnlock;
       end;
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

function TUDPProtocol.IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer;
{BSD compatible IO Control Socket}
{Socket: The socket to control}
{Cmd: The socket command}
{Arg: The command argument}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: IoctlSocket');
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

function TUDPProtocol.GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Peer Name (Remote)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: GetPeerName');
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
      ASockAddr.sin_port:=WordNtoBE(ASocket.ProtocolState.RemotePort);
      ASockAddr.sin_addr:=InAddrToNetwork(TIPState(ASocket.TransportState).RemoteAddress);
      AAddrLength:=SizeOf(TSockAddr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Return the Peer Details}
      SockAddr6.sin6_family:=ASocket.Family;
      SockAddr6.sin6_port:=WordNtoBE(ASocket.ProtocolState.RemotePort);
      SockAddr6.sin6_addr:=TIP6State(ASocket.TransportState).RemoteAddress;
      AAddrLength:=SizeOf(TSockAddr6);
      
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

function TUDPProtocol.GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Sock Name (Local)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: GetSockName');
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
      ASockAddr.sin_port:=WordNtoBE(ASocket.ProtocolState.LocalPort);
      ASockAddr.sin_addr:=InAddrToNetwork(TIPState(ASocket.TransportState).LocalAddress);
      AAddrLength:=SizeOf(TSockAddr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Return the Peer Details}
      SockAddr6.sin6_family:=ASocket.Family;
      SockAddr6.sin6_port:=WordNtoBE(ASocket.ProtocolState.LocalPort);
      SockAddr6.sin6_addr:=TIP6State(ASocket.TransportState).LocalAddress;
      AAddrLength:=SizeOf(TSockAddr6);
      
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

function TUDPProtocol.GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
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
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: GetSockOpt');
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
       AF_INET6:begin
         {Pass the call to the transport}
         Result:=TIP6Transport(ASocket.Transport).GetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
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

function TUDPProtocol.Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer;
{BSD compatible Listen}
{Socket: The socket to listen on}
{Backlog: Queue depth for accepted connections}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Listen');
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

function TUDPProtocol.Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Recv');
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
   while TUDPSocket(ASocket).RecvData.GetCount = 0 do
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
         NetworkSetLastError(WSAETIMEDOUT);
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
       NetworkSetLastError(WSAEINTR);
       Exit;
      end;
    end;
   
   {Check Size}
   NetworkSetLastError(ERROR_SUCCESS);
   Size:=TUDPSocket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;
   
   {Read Data}
   if TUDPSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,nil,nil,AFlags) then
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

function TUDPProtocol.RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer;
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
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: RecvFrom');
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
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check size of FromAddr}
      NetworkSetLastError(WSAEFAULT);
      if AFromLength < SizeOf(TSockAddr) then Exit;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@AFromAddr);
      
      {Check size of FromAddr}
      NetworkSetLastError(WSAEFAULT);
      if AFromLength < SizeOf(TSockAddr6) then Exit;
     end;
    else
     begin
      Exit;
     end;     
   end;
   
   {Wait for Data}
   StartTime:=GetTickCount64;
   while TUDPSocket(ASocket).RecvData.GetCount = 0 do
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
         NetworkSetLastError(WSAETIMEDOUT);
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
   Size:=TUDPSocket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;
    
   {Check Address Family}
   case ASocket.Family of
    AF_INET:begin
      {Get Address}
      AFromAddr.sin_family:=ASocket.Family;
   
      {Read Data}
      if TUDPSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,@AFromAddr.sin_addr,@AFromAddr.sin_port,AFlags) then
       begin
        {Get Address}
        AFromAddr.sin_addr:=InAddrToNetwork(AFromAddr.sin_addr);
        AFromAddr.sin_port:=WordNtoBE(AFromAddr.sin_port);
        
        {Return Size}
        Result:=Size;
       end;
     end;
    AF_INET6:begin
      {Get Address}
      SockAddr6.sin6_family:=ASocket.Family;
      
      {Read Data}
      if TUDPSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,@SockAddr6.sin6_addr,@SockAddr6.sin6_port,AFlags) then
       begin
        {Get Address}
        SockAddr6.sin6_port:=WordNtoBE(SockAddr6.sin6_port);
         
        {Return Size}
        Result:=Size;
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

function TUDPProtocol.Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Send');
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
      Result:=SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,@TIPState(ASocket.TransportState).RemoteAddress,ASocket.ProtocolState.LocalPort,ASocket.ProtocolState.RemotePort,@Packet,ALength,AFlags);
     end;
    AF_INET6:begin
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIP6Transport(ASocket.Transport).CompareBroadcast(TIP6State(ASocket.TransportState).RemoteAddress) or TIP6Transport(ASocket.Transport).CompareDirected(TIP6State(ASocket.TransportState).RemoteAddress) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Create the Fragment}
      Packet.Size:=ALength;
      Packet.Data:=@ABuffer;
      Packet.Next:=nil;
      
      {Send the Packet}
      Result:=SendPacket(ASocket,@TIP6State(ASocket.TransportState).LocalAddress,@TIP6State(ASocket.TransportState).RemoteAddress,ASocket.ProtocolState.LocalPort,ASocket.ProtocolState.RemotePort,@Packet,ALength,AFlags);
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

function TUDPProtocol.SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer;
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
 In6Addr:TIn6Addr;
 Route:TRouteEntry;
 Address:TAddressEntry;
 SockAddr6:PSockAddr6;
 Packet:TPacketFragment;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: SendTo');
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
   case ASocket.Family of
    AF_INET:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> AToAddr.sin_family then Exit;
    
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
         try
          {Bind the Port}
          if not ASocket.SocketState.LocalAddress then
           begin
            NetworkSetLastError(WSAEADDRINUSE);
            if not OpenPort(ASocket,WordBEtoN(IPPORT_ANY)) then Exit;
           end; 
          
          {Bind the Address}
          ASocket.SocketState.LocalAddress:=True;
          TIPState(ASocket.TransportState).LocalAddress:=TIPAddressEntry(Address).Address;
         finally
          {Unlock Address}
          Address.ReaderUnlock;
         end;
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
      Result:=SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,@InAddr,ASocket.ProtocolState.LocalPort,WordBEtoN(AToAddr.sin_port),@Packet,ALength,AFlags);
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@AToAddr);
      
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> SockAddr6.sin6_family then Exit;
      
      {Check size of ToAddr}
      NetworkSetLastError(WSAEFAULT);
      if AToLength < SizeOf(TSockAddr6) then Exit;
      
      {Get the RemoteAddress}
      In6Addr:=SockAddr6.sin6_addr;
      
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIP6Transport(ASocket.Transport).CompareDefault(In6Addr) then Exit;
      
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIP6Transport(ASocket.Transport).CompareBroadcast(In6Addr) or TIP6Transport(ASocket.Transport).CompareDirected(In6Addr) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Check the Binding}
      if not(ASocket.SocketState.LocalAddress) or TIP6Transport(ASocket.Transport).CompareDefault(TIP6State(ASocket.TransportState).LocalAddress) then
       begin
        {Check the Route}
        NetworkSetLastError(WSAENETUNREACH);
        Route:=TIP6Transport(ASocket.Transport).GetRouteByAddress(In6Addr,True,NETWORK_LOCK_READ);
        if Route = nil then Exit;
        try
         {Check the LocalAddress}
         NetworkSetLastError(WSAEADDRNOTAVAIL);
         Address:=TIP6Transport(ASocket.Transport).GetAddressByAddress(TIP6RouteEntry(Route).Address,True,NETWORK_LOCK_READ);
         if Address = nil then Exit;
         try
          {Bind the Port}
          if not ASocket.SocketState.LocalAddress then
           begin
            NetworkSetLastError(WSAEADDRINUSE);
            if not OpenPort(ASocket,WordBEtoN(IPPORT_ANY)) then Exit;
           end; 
          
          {Bind the Address}
          ASocket.SocketState.LocalAddress:=True;
          TIP6State(ASocket.TransportState).LocalAddress:=TIP6AddressEntry(Address).Address;
         finally
          {Unlock Address}
          Address.ReaderUnlock;
         end;
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
      Result:=SendPacket(ASocket,@TIP6State(ASocket.TransportState).LocalAddress,@In6Addr,ASocket.ProtocolState.LocalPort,WordBEtoN(SockAddr6.sin6_port),@Packet,ALength,AFlags);
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

function TUDPProtocol.SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
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
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: SetSockOpt');
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
           TUDPSocket(ASocket).RecvData.Size:=PInteger(AOptValue)^;
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
       AF_INET6:begin
         {Pass the call to the transport}
         Result:=TIP6Transport(ASocket.Transport).SetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
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

function TUDPProtocol.Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer;
{BSD compatible Shutdown}
{Socket: The socket to shutdown}
{How: The direction to shutdown the socket}

{Note: Shutdown does not result in CloseSocket so Closed must not get set}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Shutdown');
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

function TUDPProtocol.Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket;
{BSD compatible Socket (Create a new socket)}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}
var
 Transport:TUDPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=TProtocolSocket(INVALID_SOCKET);
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: Socket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Check Socket Type}
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if AStruct <> SOCK_DGRAM then Exit;
  
  {Check Address Family}
  NetworkSetLastError(WSAEAFNOSUPPORT);
  if (AFamily = AF_UNSPEC) and (AProtocol <> IPPROTO_IP) then AFamily:=AF_INET; 

  {Check Protocol}
  NetworkSetLastError(WSAEPROTOTYPE);
  if (AProtocol <> IPPROTO_UDP) and (AProtocol <> IPPROTO_IP) then Exit;
  
  {Get Transport}
  Transport:=TUDPProtocolTransport(GetTransportByFamily(AFamily,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  
  {Create Socket}
  Result:=TUDPSocket.Create(Self,Transport.Transport);
  
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

function TUDPProtocol.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this protocol}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TUDPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: AddTransport');
  {$ENDIF}
 
  {Check Transport}
  if ATransport = nil then Exit;
 
  {Get Transport} 
  Transport:=TUDPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add UDP Protocol}
       Handle:=TIPTransport(ATransport).AddProtocol(IPPROTO_UDP,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TUDPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_UDP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Add Proto Entry}
          TIPTransport(ATransport).AddProto(UDP_PROTOCOL_NAME,IPPROTO_UDP,False);
         
          {Add Serv Entries}
          TIPTransport(ATransport).AddServ('ECHO',UDP_PROTOCOL_NAME,7,False);
          TIPTransport(ATransport).AddServ('DISCARD',UDP_PROTOCOL_NAME,9,False);
          TIPTransport(ATransport).AddServ('DAYTIME',UDP_PROTOCOL_NAME,13,False);
          TIPTransport(ATransport).AddServ('QOTD',UDP_PROTOCOL_NAME,17,False);
          TIPTransport(ATransport).AddServ('CHARGEN',UDP_PROTOCOL_NAME,19,False);
          TIPTransport(ATransport).AddServ('TIME',UDP_PROTOCOL_NAME,37,False);
          TIPTransport(ATransport).AddServ('NAME',UDP_PROTOCOL_NAME,42,False);
          TIPTransport(ATransport).AddServ('DOMAIN',UDP_PROTOCOL_NAME,53,False);
          TIPTransport(ATransport).AddServ('NAMESERVER',UDP_PROTOCOL_NAME,53,False);
          TIPTransport(ATransport).AddServ('BOOTP',UDP_PROTOCOL_NAME,67,False);
          TIPTransport(ATransport).AddServ('BOOTPS',UDP_PROTOCOL_NAME,67,False);
          TIPTransport(ATransport).AddServ('BOOTPC',UDP_PROTOCOL_NAME,68,False);
          TIPTransport(ATransport).AddServ('TFTP',UDP_PROTOCOL_NAME,69,False);
          TIPTransport(ATransport).AddServ('NTP',UDP_PROTOCOL_NAME,123,False);
          TIPTransport(ATransport).AddServ('NBNAME',UDP_PROTOCOL_NAME,137,False);
          TIPTransport(ATransport).AddServ('NBDATAGRAM',UDP_PROTOCOL_NAME,138,False);
          TIPTransport(ATransport).AddServ('SNMP',UDP_PROTOCOL_NAME,161,False);
          
          {Return Result}
          Result:=True;
         finally
          {Release Lock}
          FTransports.WriterUnlock;
         end;  
        end;
        
       {Add ICMP Protocol}
       Handle:=TIPTransport(ATransport).AddProtocol(IPPROTO_ICMP,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TUDPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_ICMP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally
          {Release Lock}
          FTransports.WriterUnlock;
         end;  
        end;
      end;
     AF_INET6:begin
       {Add UDP Protocol}
       Handle:=TIP6Transport(ATransport).AddProtocol(IPPROTO_UDP,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TUDPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_UDP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Add Proto Entry}
          TIP6Transport(ATransport).AddProto(UDP_PROTOCOL_NAME,IPPROTO_UDP,False);
          
          {Add Serv Entries}
          TIP6Transport(ATransport).AddServ('ECHO',UDP_PROTOCOL_NAME,7,False);
          TIP6Transport(ATransport).AddServ('DISCARD',UDP_PROTOCOL_NAME,9,False);
          TIP6Transport(ATransport).AddServ('DAYTIME',UDP_PROTOCOL_NAME,13,False);
          TIP6Transport(ATransport).AddServ('QOTD',UDP_PROTOCOL_NAME,17,False);
          TIP6Transport(ATransport).AddServ('CHARGEN',UDP_PROTOCOL_NAME,19,False);
          TIP6Transport(ATransport).AddServ('TIME',UDP_PROTOCOL_NAME,37,False);
          TIP6Transport(ATransport).AddServ('NAME',UDP_PROTOCOL_NAME,42,False);
          TIP6Transport(ATransport).AddServ('DOMAIN',UDP_PROTOCOL_NAME,53,False);
          TIP6Transport(ATransport).AddServ('NAMESERVER',UDP_PROTOCOL_NAME,53,False);
          TIP6Transport(ATransport).AddServ('BOOTP',UDP_PROTOCOL_NAME,67,False);
          TIP6Transport(ATransport).AddServ('BOOTPS',UDP_PROTOCOL_NAME,67,False);
          TIP6Transport(ATransport).AddServ('BOOTPC',UDP_PROTOCOL_NAME,68,False);
          TIP6Transport(ATransport).AddServ('TFTP',UDP_PROTOCOL_NAME,69,False);
          TIP6Transport(ATransport).AddServ('NTP',UDP_PROTOCOL_NAME,123,False);
          TIP6Transport(ATransport).AddServ('NBNAME',UDP_PROTOCOL_NAME,137,False);
          TIP6Transport(ATransport).AddServ('NBDATAGRAM',UDP_PROTOCOL_NAME,138,False);
          TIP6Transport(ATransport).AddServ('SNMP',UDP_PROTOCOL_NAME,161,False);
         
          {Return Result}
          Result:=True;
         finally
          {Release Lock}
          FTransports.WriterUnlock;
         end;  
        end;
        
       {Add ICMP6 Protocol}
       Handle:=TIP6Transport(ATransport).AddProtocol(IPPROTO_ICMPV6,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TUDPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_ICMPV6;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
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

function TUDPProtocol.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this protocol}
{Transport: The transport to remove}
var
 Transport:TUDPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
 
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Get Transport}
     Transport:=TUDPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
     while Transport <> nil do
      begin
       {Check Protocol}
       case Transport.Protocol of
        IPPROTO_UDP:begin
          {Remove UDP Protocol}
          if TIPTransport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
           begin
            {Remove Serv Entries}
            TIPTransport(ATransport).RemoveServ('ECHO',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('DISCARD',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('DAYTIME',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('QOTD',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('CHARGEN',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('TIME',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NAME',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('DOMAIN',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NAMESERVER',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('BOOTP',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('BOOTPS',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('BOOTPC',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('TFTP',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NTP',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NBNAME',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NBDATAGRAM',UDP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('SNMP',UDP_PROTOCOL_NAME);
            
            {Remove Proto Entry}
            TIPTransport(ATransport).RemoveProto(UDP_PROTOCOL_NAME);
            
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
        IPPROTO_ICMP:begin
          {Remove ICMP Protocol}
          if TIPTransport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
           begin
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
        else
         begin
          {Unlock Transport}
          Transport.WriterUnlock;
         end;         
       end;
    
       {Get Transport}
       Transport:=TUDPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
      end; 
    end;
   AF_INET6:begin
     {Get Transport}
     Transport:=TUDPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
     while Transport <> nil do
      begin
       {Check Protocol}
       case Transport.Protocol of
        IPPROTO_UDP:begin
          {Remove UDP Protocol}
          if TIP6Transport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
           begin
            {Remove Serv Entries}
            TIP6Transport(ATransport).RemoveServ('ECHO',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('DISCARD',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('DAYTIME',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('QOTD',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('CHARGEN',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('TIME',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NAME',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('DOMAIN',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NAMESERVER',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('BOOTP',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('BOOTPS',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('BOOTPC',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('TFTP',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NTP',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NBNAME',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NBDATAGRAM',UDP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('SNMP',UDP_PROTOCOL_NAME);
            
            {Remove Proto Entry}
            TIP6Transport(ATransport).RemoveProto(UDP_PROTOCOL_NAME);
            
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
        IPPROTO_ICMPV6:begin
          {Remove ICMP6 Protocol}
          if TIP6Transport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
           begin
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
        else
         begin
          {Unlock Transport}
          Transport.WriterUnlock;
         end;         
       end;
   
       {Get Transport}
       Transport:=TUDPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
      end; 
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TUDPProtocol.FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket;
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
 Socket:TUDPSocket;
begin
 {}
 FSockets.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: FindSocket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Get Socket}
  Socket:=TUDPSocket(FSockets.First);
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
    Socket:=TUDPSocket(Socket.Next);
   end;
 finally 
  FSockets.ReaderUnlock;
 end; 
end;

{==============================================================================}

procedure TUDPProtocol.FlushSockets(All:Boolean);
{Flush sockets from the socket cache}
{All: If True flush all sockets, otherwise flush expired sockets}
var
 CurrentTime:Int64;
 Socket:TUDPSocket;
 Current:TUDPSocket;
begin
 {}
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: FlushSockets');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  All = ' + BoolToStr(All));
 {$ENDIF}
  
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Get Socket}
 Socket:=TUDPSocket(GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
 while Socket <> nil do
  begin
   {Get Next}
   Current:=Socket;
   Socket:=TUDPSocket(GetSocketByNext(Current,True,False,NETWORK_LOCK_READ));
    
   {Check Socket State}
   if (Current.SocketState.Closed) or (All) then
    begin
     {Check Socket Expired}
     if ((Current.CloseTime + CLOSE_TIMEOUT) < CurrentTime) or (All) then
      begin
       {Convert Socket}
       if Current.ReaderConvert then
        begin
         {Close Port}
         ClosePort(Current);
        
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

function TUDPProtocol.StartProtocol:Boolean;
{Start this protocol ready for sending and receiving}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: StartProtocol');
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

  {Register with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ);
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

function TUDPProtocol.StopProtocol:Boolean;
{Stop this protocol ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: StopProtocol');
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

  {Deregister with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ);
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

function TUDPProtocol.ProcessProtocol:Boolean;
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
{TUDPSocket}
constructor TUDPSocket.Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
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
  AF_INET6:begin
    {Create IP6 Transport State}
    FTransportState:=TIP6State.Create;
    {Create IP6 Transport Options}
    FTransportOptions:=TIP6Options.Create;
    
    {Set IP6 Defaults}
    TIP6Options(FTransportOptions).HopLimit:=TIP6Transport(ATransport).DefaultHopLimit;
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
 FProtocolState:=TUDPState.Create;
 {Create Protocol Options}
 FProtocolOptions:=TUDPOptions.Create;

 {Create Receive Buffer}
 FRecvData:=TUDPBuffer.Create(Self);
 FRecvData.Size:=UDP_BUFFER_SIZE;

 {Set Socket Defaults}
 FSocketOptions.SendTimeout:=UDP_TIMEOUT;
 FSocketOptions.RecvTimeout:=UDP_TIMEOUT;
end;

{==============================================================================}

destructor TUDPSocket.Destroy;
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

function TUDPSocket.GetOption(ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
{Note: With Socket Options that are Boolean 0 is False and > 0 is True}
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
 
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Socket: GetOption');
  {$ENDIF}
 
  {Check Level}
  case ALevel of
   IPPROTO_UDP:begin
     NetworkSetLastError(WSAENOPROTOOPT);
     
     {Check Option}
     case AOptName of
      UDP_NOCHECKSUM:begin
        NetworkSetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          PInteger(AOptValue)^:=0;
          AOptLength:=SizeOf(Integer);
          if TUDPOptions(ProtocolOptions).NoChecksum then PInteger(AOptValue)^:=1;
          
          {Return Result}
          NetworkSetLastError(ERROR_SUCCESS);
          Result:=NO_ERROR;
         end;
       end;
     end;
    end;
   else
    begin
     Result:=inherited GetOption(ALevel,AOptName,AOptValue,AOptLength);
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TUDPSocket.SetOption(ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
{Note: With Socket Options that are Boolean 0 is False and > 0 is True}
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Socket: SetOption');
  {$ENDIF}
  
  {Check Level}
  case ALevel of
   IPPROTO_UDP:begin
     NetworkSetLastError(WSAENOPROTOOPT);
     
     {Check Option}
     case AOptName of
      UDP_NOCHECKSUM:begin
        NetworkSetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          TUDPOptions(ProtocolOptions).NoChecksum:=(PInteger(AOptValue)^ <> 0);
          
          {Return Result}
          NetworkSetLastError(ERROR_SUCCESS);
          Result:=NO_ERROR;
         end;
       end;
     end;
    end;
   else
    begin
     Result:=inherited SetOption(ALevel,AOptName,AOptValue,AOptLength);
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TUDPSocket.IoCtl(ACommand:DWORD;var AArgument:u_long):Integer;
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Socket: IoCtl');
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

function TUDPSocket.IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this UDP Socket is Connected to the Host specified by RemoteAddress/Port}
{A connected Socket will have a Bound LocalAddress/Port and a Connected RemoteAddress/Port}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Socket: IsConnected');
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
     
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
     {Check the Connected RemoteAddress}
     if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).RemoteAddress,PInAddr(ARemoteAddress)^) then Exit;
     
     {Check the Connected RemotePort}
     if ProtocolState.RemotePort <> ARemotePort then Exit;
     
     {Return Result}
     Result:=True;
    end;
   AF_INET6:begin
     {Check the LocalAddress}
     if not ABroadcast then
      begin
       {Check the Bound LocalAddress}
       if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).LocalAddress,PIn6Addr(ALocalAddress)^) then Exit;
      end
     else
      begin
       {Check the Broadcast LocalAddress}
       if not TIP6Transport(Transport).CompareBroadcast(PIn6Addr(ALocalAddress)^) then
        begin
         {If not global Broadcast then check for Directed Broadcast}
         Route:=TIP6Transport(Transport).GetRouteByAddress(PIn6Addr(ALocalAddress)^,True,NETWORK_LOCK_READ);
         if Route = nil then Exit;
         try
          if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).LocalAddress,TIP6RouteEntry(Route).Address) then Exit;
         finally
          Route.ReaderUnlock;
         end; 
        end;
      end;
     
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
     {Check the Connected RemoteAddress}
     if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).RemoteAddress,PIn6Addr(ARemoteAddress)^) then Exit;
     
     {Check the Connected RemotePort}
     if ProtocolState.RemotePort <> ARemotePort then Exit;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TUDPSocket.IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this UDP Socket is Listening on the LocalAddress/Port specified}
{A listening Socket may or may not have a Bound LocalAddress/Port and will have a default (INADDR_ANY/IPPORT_ANY) RemoteAddress/Port}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Socket: IsListening');
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
      
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
     {Return Result}
     Result:=True;
    end;
   AF_INET6:begin
     {Check the LocalAddress}
     if not TIP6Transport(Transport).CompareDefault(TIP6State(TransportState).LocalAddress) then
      begin
       if not ABroadcast then
        begin
         {Check the Bound LocalAddress}
         if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).LocalAddress,PIn6Addr(ALocalAddress)^) then Exit;
        end
       else
        begin
         {Check the Broadcast LocalAddress}
         if not TIP6Transport(Transport).CompareBroadcast(PIn6Addr(ALocalAddress)^) then
          begin
           {If not global Broadcast then check for Directed Broadcast}
           Route:=TIP6Transport(Transport).GetRouteByAddress(PIn6Addr(ALocalAddress)^,True,NETWORK_LOCK_READ); 
           if Route = nil then Exit;
           try
            if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).LocalAddress,TIP6RouteEntry(Route).Address) then Exit;
           finally
            Route.ReaderUnlock;
           end; 
          end;
        end;
      end;
     
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
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
{TUDPState}
constructor TUDPState.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}
{==============================================================================}
{TUDPBuffer}
constructor TUDPBuffer.Create(ASocket:TTransportSocket);
begin
 {}
 inherited Create(ASocket);
 
 FOffset:=SizeOf(TUDPDatagram); {UDP_DATAGRAM_SIZE}
 
 {Check Address Family}
 case FSocket.Family of
  AF_INET:begin
    FLength:=IP_ADDRESS_SIZE;
   end;
  AF_INET6:begin
    FLength:=IP6_ADDRESS_SIZE;
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

destructor TUDPBuffer.Destroy;
begin
 {}
 AcquireLock;
 try
  FlushDatagrams;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TUDPBuffer.AddDatagram(ASize:Integer):Boolean;
{Adds a new Datagram as the Last datagram in the Buffer}
var
 Datagram:PUDPDatagram;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: AddDatagram');
  {$ENDIF}
  
  {Check Data Size}
  if ASize = 0 then Exit;
  
  {Check Buffer Free}
  if LongWord(ASize) > FFree then Exit;
  
  {Create a new Datagram}
  Datagram:=GetMem(FOffset + FLength);
  if Datagram = nil then Exit;
  Datagram.Size:=ASize;
  Datagram.RemotePort:=IPPORT_ANY;
  Datagram.Next:=nil;
  
  {Add to List}
  if FLast = nil then
   begin
    {Is First Datagram}
    FFirst:=Datagram;
    FLast:=Datagram;
    Dec(FFree,ASize);
    Inc(FUsed,ASize);
    Inc(FCount);
   end
  else
   begin
    {Not First Datagram}
    FLast.Next:=Datagram;
    FLast:=Datagram;
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

function TUDPBuffer.RemoveDatagram:Boolean;
{Removes the First datagram from the Buffer}
var
 Datagram:PUDPDatagram;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: RemoveDatagram');
  {$ENDIF}
  
  {Check for Datagrams}
  if FFirst = nil then Exit;
  
  {Remove from List}
  if FFirst.Next <> nil then
   begin
    {Not Last Datagram}
    Datagram:=FFirst;
    FFirst:=FFirst.Next;
    Dec(FUsed,Datagram.Size);
    Inc(FFree,Datagram.Size);
    Dec(FCount);
   end
  else
   begin
    {Is Last Datagram}
    Datagram:=FFirst;
    FFirst:=nil;
    FLast:=nil;
    Dec(FUsed,Datagram.Size);
    Inc(FFree,Datagram.Size);
    Dec(FCount);
   end;
   
  {Free the Datagram}
  FreeMem(Datagram,FOffset + FLength);
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TUDPBuffer.FlushDatagrams;
begin
 {}
 if not AcquireLock then Exit;
 try
  {Get Datagram}
  while FFirst <> nil do
   begin
    {Remove Datagram}
    RemoveDatagram;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TUDPBuffer.SetSize(ASize:LongWord);
{Setting the Size clears any current Datagrams}
begin
 {}
 if not AcquireLock then Exit;
 try
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: SetSize: Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check Size}
  if ASize = 0 then Exit;
  
  {Get Size}
  ASize:=Max(ASize,UDP_BUFFER_SIZE);
  
  {Clear any Datagrams}
  FlushDatagrams;
  
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
  
  {Set the Datagram Values}
  FCount:=0;
  FFirst:=nil;
  FLast:=nil;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TUDPBuffer.GetNext:Integer;
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

function TUDPBuffer.GetCount:LongWord;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TUDPBuffer.ReadBuffer(var ABuffer;var ASize:Integer;ARemoteAddress:Pointer;ARemotePort:PWORD;AFlags:Integer):Boolean;
{Stored Datagram contains the Data without the Headers}
{Passed Size contains size of Buffer and should return the size of Datagram}
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
  {Check Buffer Size}
  if ASize = 0 then Exit;
  
  {Check there is Data}
  if FFirst = nil then Exit;
  
  {Get the Start and Size}
  ReadNext:=FRead;
  ReadSize:=FFirst.Size;
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
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
      {$IFDEF UDP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: Short Read');
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
    {$IFDEF UDP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: Double Read');
    {$ENDIF}
    {Read the First Block of the Data}
    BlockSize:=(PtrUInt(FEnd) - PtrUInt(ReadNext));
    if BufferSize < BlockSize then
     begin
      {$IFDEF UDP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: Short First Read');
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
        {$IFDEF UDP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: Short Second Read');
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
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
  {$ENDIF}
  
  {Get the Remote Address}
  if ARemoteAddress <> nil then
   begin
    System.Move(Pointer(PtrUInt(FFirst) + FOffset)^,ARemoteAddress^,FLength);
   end;
  
  {Get the Remote Port}
  if ARemotePort <> nil then
   begin
    ARemotePort^:=FFirst.RemotePort;
   end;
  
  {Check for Peek Flag}
  if (AFlags and MSG_PEEK) = 0 then
   begin
    {Update the Next Read}
    FRead:=ReadNext;
    
    {Remove the Datagram}
    RemoveDatagram;
   end;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: ReadBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
  {$ENDIF}
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TUDPBuffer.WriteBuffer(var ABuffer;ASize:Integer;ARemoteAddress:Pointer;ARemotePort:PWORD):Boolean;
{Supplied Buffer contains the Data without the Headers}
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
  {Check Data Size}
  if ASize = 0 then Exit;
  
  {Add the Datagram}
  if not AddDatagram(ASize) then Exit;
  
  {Get the Start and Size}
  WriteNext:=FWrite;
  WriteSize:=ASize;
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: WriteBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
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
    {$IFDEF UDP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: WriteBuffer: Double Write');
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
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
  {$ENDIF}
  
  {Set the Remote Address}
  if ARemoteAddress <> nil then
   begin
    System.Move(ARemoteAddress^,Pointer(PtrUInt(FLast) + FOffset)^,FLength);
   end;
  
  {Set the Remote Port}
  if ARemotePort <> nil then
   begin
    FLast.RemotePort:=ARemotePort^;
   end;
  
  {Update the Next Write}
  FWrite:=WriteNext;
  
  {$IFDEF UDP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP Buffer: WriteBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
  {$ENDIF}
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TUDPOptions}
constructor TUDPOptions.Create;
begin
 {}
 inherited Create;
 FOptions:=0; 
end;

{==============================================================================}

function TUDPOptions.GetNoChecksum:Boolean;
begin
 {}
 Result:=(FOptions and UDP_NOCHECKSUM) = UDP_NOCHECKSUM;
end;

{==============================================================================}

procedure TUDPOptions.SetNoChecksum(ANoChecksum:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ANoChecksum then
  FOptions:=FOptions or UDP_NOCHECKSUM
 else
  FOptions:=FOptions and not(UDP_NOCHECKSUM);
  
 ReleaseLock;
end;
  
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure UDPInit;
begin
 {}
 {Check Initialized}
 if UDPInitialized then Exit;

 {Setup UDP Protocol}
 if NetworkSettings.GetBoolean('DNS_CLIENT_ENABLED') then NetworkSettings.AddBoolean('UDP_PROTOCOL_ENABLED',True);
 if NetworkSettings.GetBoolean('DHCP_CONFIG_ENABLED') then NetworkSettings.AddBoolean('UDP_PROTOCOL_ENABLED',True);
 if NetworkSettings.GetBoolean('BOOTP_CONFIG_ENABLED') then NetworkSettings.AddBoolean('UDP_PROTOCOL_ENABLED',True);
 
 {Create UDP Protocol}
 if NetworkSettings.GetBooleanDefault('UDP_PROTOCOL_ENABLED',UDP_PROTOCOL_ENABLED) then 
  begin
   TUDPProtocol.Create(ProtocolManager,UDP_PROTOCOL_NAME);
  end; 
 
 UDPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{UDP Functions}
function CheckUDP(AFamily:Word;ABuffer:Pointer):Boolean;
{Verify that the packet is a valid UDP packet}
{Buffer: The complete packet including Transport header}

{Note: If checksum is zero then no checksum was added, return True}
var
 Length:Word;
 IP:PIPHeader;
 IP6:PIP6Header;
 UDP:PUDPHeader;
 Pseudo:TIPPseudo;
 Pseudo6:TIP6Pseudo;
begin
 {}
 Result:=False;
 
 {$IFDEF UDP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP: CheckUDP');
 {$ENDIF}
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    IP:=PIPHeader(ABuffer);        {GetIPHeaderLength}
    
    {Get Header}
    UDP:=PUDPHeader(PtrUInt(IP) + GetUDPHeaderOffset(AF_INET,ABuffer));
    
    {Check Header Length}
    Length:=GetUDPHeaderLength(AF_INET,ABuffer); {GetIPDataLength} //To Do //Same for ICMP/IGMP/TCP etc ?
    if Length >= UDP_HEADER_SIZE then
     begin
      {Check No Checksum}
      if UDP.Checksum > 0 then
       begin
        Length:=Length + GetUDPDataLength(AF_INET,ABuffer); {Not Needed if above}
        
        {Get the Pseudo Header}
        Pseudo.SourceIP:=InAddrToNetwork(IP.SourceIP);
        Pseudo.DestIP:=InAddrToNetwork(IP.DestIP);
        Pseudo.Mbz:=0;
        Pseudo.Protocol:=IP.Protocol;
        Pseudo.Length:=WordNtoBE(Length);
        
        {Validate the Checksum}
        {$IFDEF UDP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Original Checksum   = ' + IntToHex(WordBEtoN(UDP.Checksum),4));
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'UDP:  Calculated Checksum = ' + IntToHex(WordBEtoN(ChecksumUDPRecv(AF_INET,@Pseudo,UDP,0,Length)),4));
        {$ENDIF}
        if UDP.Checksum = $FFFF then UDP.Checksum:=0; {Allow for all 1s case}
        if UDP.Checksum = ChecksumUDPRecv(AF_INET,@Pseudo,UDP,0,Length) then
         begin
          Result:=True;
         end;
       end
      else
       begin
        Result:=True;
       end;
     end;
   end;
  AF_INET6:begin
    {Get Header}
    IP6:=PIP6Header(ABuffer);        {GetIP6HeaderLength}
    
    {Get Header}
    UDP:=PUDPHeader(PtrUInt(IP6) + GetUDPHeaderOffset(AF_INET6,ABuffer));
    
    //To Do
    
   end;
 end;
end;

{==============================================================================}

function GetUDPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Start of UDP Header (Length of IP Header)}
    Result:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
   end;
  AF_INET6:begin
    
    //To Do
    
   end;
 end;
end;

{==============================================================================}

function GetUDPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Size of UDP Header}
    Result:=UDP_HEADER_SIZE;
   end;
  AF_INET6:begin
    
    //To Do
    
   end;
 end;
end;

{==============================================================================}

function GetUDPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Start of UDP Data (IP Header + UDP Header)}
    Result:=((PIPHeader(ABuffer).VersionLength and $0F) shl 2) + UDP_HEADER_SIZE;
   end;
  AF_INET6:begin
    
    //To Do
    
   end;
 end;
end;

{==============================================================================}

function GetUDPDataLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Start of UDP Header (Length of IP Header)}
    Offset:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
    {Return UDP Length Field - Size of UDP Header}
    Result:=WordBEtoN(PUDPHeader(PtrUInt(ABuffer) + Offset).Length) - UDP_HEADER_SIZE;
   end;
  AF_INET6:begin
    
    //To Do
    
   end;
 end;
end;

{==============================================================================}

function ChecksumUDPRecv(AFamily:Word;APseudo:PIPPseudo;ABuffer:Pointer;AOffset,ALength:Word):Word;
{Validate the Checksum of UDP Pseudo, Header and Data on Receive}
var
 Original:Word;
 UDP:PUDPHeader;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    UDP:=PUDPHeader(PtrUInt(ABuffer) + AOffset);
    
    {Save Checksum}
    Original:=UDP.Checksum;
    UDP.Checksum:=0;
    
    {Calculate 1s Compliment Checksum on UDP Pseudo, Header and Data}
    Result:=GetChecksum2(APseudo,ABuffer,IP_PSEUDO_SIZE,AOffset,ALength);
    
    {Restore Checksum}
    UDP.Checksum:=Original;
   end;
  AF_INET6:begin
    
    //To Do //Pass APseudo6 ?
    
   end;
 end;
end;

{==============================================================================}

function ChecksumUDPSend(AFamily:Word;APseudo:PIPPseudo;AHeader:PUDPHeader;AData:Pointer;ALength:Word):Word;
{Checksum the UDP Pseudo, Header and Data on Send}
var
 Original:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Save Checksum}
    Original:=AHeader.Checksum;
    AHeader.Checksum:=0;
    
    {Calculate 1s Compliment Checksum on UDP Pseudo, Header and Data}
    Result:=GetChecksum3(APseudo,AHeader,AData,IP_PSEUDO_SIZE,UDP_HEADER_SIZE,0,ALength);
    if Result = 0 then Result:=$FFFF; {Allow for all 1s case on Send}
    
    {Restore Checksum}
    AHeader.Checksum:=Original;
   end;
  AF_INET6:begin
    
    //To Do //Pass APseudo6 ?
    
   end;
 end;
end;
  
{==============================================================================}
{==============================================================================}
{UDP Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 UDPInit;

{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
  