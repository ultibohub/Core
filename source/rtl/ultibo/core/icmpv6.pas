{
Ultibo ICMPv6 (Internet Control Message Protocol version 6) unit.

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


References
==========


Internet Control Message Protocol version 6
===========================================

 Notes: ICMP6 Checksum includes both Header and Data (if any)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ICMPv6;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  GlobalSock,
  Threads,
  SysUtils,
  Classes,
  Network,
  Transport,
  Protocol,
  IPv6,
  Ultibo,
  UltiboClasses;

//To Do //IPv6 uses NDP (via ICMPv6) instead of ARP
                      //See: http://keepingitclassless.net/2011/10/neighbor-solicitation-ipv6s-replacement-for-arp/

//To Do //Multicast on IPv6 is handled by MLD (Multicast Listener Discovery) (via ICMPv6) instead of IGMP
                      //See: https://en.wikipedia.org/wiki/Internet_Group_Management_Protocol
                      //     https://en.wikipedia.org/wiki/Multicast_Listener_Discovery


{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {ICMPv6 specific constants}
 {Note: Some ICMPV6 definitions are in the Protocol or IPv6 modules}
 ICMP6_PROTOCOL_NAME = 'ICMPV6';

 {ICMPv6  constants}
 ICMP6_TIMEOUT = 0;          {Wait forever on a ICMP6 Read}
 ICMP6_BUFFER_SIZE = 65536;  {ICMP6 Receive Buffer Size}

 ICMP6_HEADER_SIZE = 5;      {SizeOf(TICMP6Header);} //????? Which Message ?????

 ICMP6_PACKET_SIZE = 8;      {SizeOf(TICMP6Packet)}

{==============================================================================}
type
 {ICMPv6 specific types}
 {Note: Some ICMPv6 definitions are in the Protocol or IPv6 modules}
 PICMP6Header = ^TICMP6Header;
 TICMP6Header = packed record
  ICMP6Type:Byte;            {ICMP_UNREACH etc}
  Code:Byte;                 {ICMP_UNREACH_NET,ICMP_UNREACH_PORT etc}
  Checksum:Word;             {1s Compliment checksum of Structure}
  CurrentHopLimit:Byte;
  //To Do
 end;

 PICMP6Packet = ^TICMP6Packet;
 TICMP6Packet = record   {8 Bytes} {Used by ICMP6Buffer}
  Size:LongWord;         {LongWord to keep size even}
  Next:PICMP6Packet;
 end; {Followed by RemoteAddress (4 or 16 Bytes)}

{==============================================================================}
type
 {ICMPv6 specific classes}
 TICMP6Socket = class;
 TICMP6ProtocolTransport = class(TProtocolTransport)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}

  public
   {Status Variables}
   Socket:TICMP6Socket;   {Socket for sending replies}
 end;

 TICMP6Protocol = class(TNetworkProtocol)
   constructor Create(AManager:TProtocolManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Status Variables}
   FNextICMP6Id:Word;
   FNextICMP6Lock:TMutexHandle; //To Do //Change this to LocalLock and share with other properties ?

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
   function ControlHandler(AHandle:THandle;ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;

   function GetNextICMP6Id(AIncrement:Boolean):Word;
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

 TICMP6Buffer = class;
 TICMP6Socket = class(TProtocolSocket)  {SOCK_RAW}
   constructor Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Data Layer Variables}
   FRecvData:TICMP6Buffer;
  public
   {Data Layer Properties}
   property RecvData:TICMP6Buffer read FRecvData;

   {Public Methods}
   function IoCtl(ACommand:DWORD;var AArgument:u_long):Integer; override;

   function IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
   function IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
 end;

 TICMP6Buffer = class(TSocketBuffer)
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

   FFirst:PICMP6Packet;     {Pointer to First Packet}
   FLast:PICMP6Packet;      {Pointer to Last Packet}

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
 {ICMPv6 specific variables}

{==============================================================================}
{Initialization Functions}
procedure ICMP6Init;

{==============================================================================}
{ICMPv6 Functions}
function CheckICMP6(AFamily:Word;ABuffer:Pointer):Boolean;

function GetICMP6HeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetICMP6HeaderLength(AFamily:Word;ABuffer:Pointer):Word;
function GetICMP6DataOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetICMP6DataLength(AFamily:Word;ABuffer:Pointer):Word;

function ChecksumICMP6(AFamily:Word;ABuffer:Pointer;AOffset,ALength:Word):Word;

{==============================================================================}
{ICMPv6 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {ICMPv6 specific variables}
 ICMP6Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{TICMP6ProtocolTransport}
constructor TICMP6ProtocolTransport.Create;
begin
 {}
 inherited Create;
 Socket:=nil;
end;

{==============================================================================}

destructor TICMP6ProtocolTransport.Destroy;
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
{TICMP6Protocol}
constructor TICMP6Protocol.Create(AManager:TProtocolManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FProtocol:=IPPROTO_ICMPV6;
 FSocketType:=SOCK_RAW;
 FNextICMP6Id:=1;
 FNextICMP6Lock:=MutexCreate;
end;

{==============================================================================}

destructor TICMP6Protocol.Destroy;
begin
 {}
 WriterLock;
 try
  MutexDestroy(FNextICMP6Lock);
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TICMP6Protocol.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
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
 IP6:PIP6Header;
 ICMP6:PICMP6Header;
 Socket:TICMP6Socket;
 Transport:TICMP6ProtocolTransport;
begin
 {}
 Result:=False;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}

 {Check Source}
 {if ASource = nil then Exit;} {Not Used}

 {Check Packet}
 if APacket = nil then Exit;

 {Get Transport}
 Transport:=TICMP6ProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Family = ' + AddressFamilyToString(Transport.Transport.Family));
  {$ENDIF}

  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET6:begin
     {Get Header}
     IP6:=PIP6Header(APacket);

     //To Do //Extensions

     {$IFDEF ICMP6_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Protocol = ' + ProtocolToString(Transport.Protocol));
     {$ENDIF}

     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_ICMPV6:begin
        {Check ICMP Packet}
        if CheckICMP6(AF_INET6,IP6) then
         begin
          {Get Header}
          ICMP6:=PICMP6Header(PtrUInt(IP6) + GetICMP6HeaderOffset(AF_INET6,IP6));

          //To do

          //   {Signal the Event}
          //   Socket.SignalChange;

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

function TICMP6Protocol.ControlHandler(AHandle:THandle;ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;
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
 Transport:TICMP6ProtocolTransport;
begin
 {}
 Result:=False;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: ControlHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 {if ADest = nil then Exit;} {May be nil}

 {Check Source}
 {if ASource = nil then Exit;} {May be nil}

 {Check Data}
 {if AData = nil then Exit;} {May be nil}

 {Get Transport}
 Transport:=TICMP6ProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET6:begin
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_ICMPV6:begin
        {Check Command}

        //To Do
       end;
     end;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TICMP6Protocol.GetNextICMP6Id(AIncrement:Boolean):Word;
{Get the next ICMP6 packet id number}
{Increment: If True increment the next id}
begin
 {}
 MutexLock(FNextICMP6Lock);

 {Get Next Id}
 Result:=FNextICMP6Id;

 {Increment Id}
 if AIncrement then Inc(FNextICMP6Id,ID_INCREMENT);

 MutexUnlock(FNextICMP6Lock);
end;

{==============================================================================}

function TICMP6Protocol.SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer;
{Source is the working set to check, Dest is the set passed to Select}
var
 Count:Integer;
 Socket:TICMP6Socket;
begin
 {}
 Result:=0;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: SelectCheck');
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
      Socket:=TICMP6Socket(ASource.fd_array[Count]);

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
      Socket:=TICMP6Socket(ASource.fd_array[Count]);

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

function TICMP6Protocol.SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer;
{Socket is the single socket to check, Code is the type of check, Timeout is how long to wait}
var
 StartTime:Int64;
 Socket:TICMP6Socket;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: SelectWait');
 {$ENDIF}

 {Get Socket}
 Socket:=TICMP6Socket(ASocket);

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
         if GetTickCount64 >= (StartTime + ATimeout) then
          begin
           {Return Zero}
           Result:=0;
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

function TICMP6Protocol.SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
{Send a Packet by adding the Protocol Header and other details to the Data}
{Socket: The socket to use for sending the packet}
{Source: The source address of the packet (Host Order)}
{Dest: The destination address of the packet (Host Order)}
{SourcePort: The source port of the packet (Host Order)}
{DestPort: The destination port of the packet (Host Order)}
{Packet: The packet data to send}
{Size: The size of the packet data in bytes}
{Flags: Any protocol specific flags for sending}

{Note: For ICMP6 the Data is the Header so the packet is not changed}
{Note: Caller must hold the Socket lock}
var
 Transport:TICMP6ProtocolTransport;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: SendPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Address Family}
 case ASocket.Family of
  AF_INET6:begin
    {$IFDEF ICMP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Dest = ' + In6AddrToString(PIn6Addr(ADest)^));
    {$ENDIF}

    {Get Transport}
    Transport:=TICMP6ProtocolTransport(GetTransportByTransport(ASocket.Transport,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;

    {Send the Packet}
    Result:=TIP6Transport(ASocket.Transport).SendPacket(ASocket,ASource,ADest,APacket,ASize,AFlags);

    {Unlock Transport}
    Transport.ReaderUnlock;
   end;
 end;
end;

{==============================================================================}

function TICMP6Protocol.Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket;
{BSD compatible Accept}
{Socket: The socket to accept from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=TProtocolSocket(INVALID_SOCKET);

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Accept');
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

function TICMP6Protocol.Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Bind}
{Sets the LocalAddress for future Sends and Receives, Address can be specified as INADDR_ANY which allows Listening or auto assignment}
{Socket: The socket to bind}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Bind');
 {$ENDIF}

 {Check Socket}
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected or Bound}
   NetworkSetLastError(WSAEINVAL);
   if ASocket.SocketState.Connected then Exit;
   if ASocket.SocketState.LocalAddress then Exit;

   {Get Socket Address}
   SockAddr6:=PSockAddr6(@ASockAddr);

   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   if ASocket.Family <> SockAddr6.sin6_family then Exit;

   {Check Address Family}
   case ASocket.Family of
    AF_INET6:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;

      {Check LocalAddress}
      if not TIP6Transport(ASocket.Transport).CompareDefault(SockAddr6.sin6_addr) then
       begin
        NetworkSetLastError(WSAEADDRNOTAVAIL);
        if TIP6Transport(ASocket.Transport).GetAddressByAddress(SockAddr6.sin6_addr,False,NETWORK_LOCK_NONE) = nil then Exit;
       end;

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

function TICMP6Protocol.CloseSocket(ASocket:TProtocolSocket):Integer;
{BSD compatible Close Socket}
{Closes and removes the socket, does not perform Linger}
{Socket: The socket to close}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: CloseSocket');
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

function TICMP6Protocol.Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
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
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Connect');
 {$ENDIF}

 {Check Socket}
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAEISCONN);
   if ASocket.SocketState.Connected then Exit;

   {Get Socket Address}
   SockAddr6:=PSockAddr6(@ASockAddr);

   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   if ASocket.Family <> SockAddr6.sin6_family then Exit;

   {Check Address Family}
   case ASocket.Family of
    AF_INET6:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;

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

       {Check the Binding}
       if not ASocket.SocketState.LocalAddress then
        begin
         {Bind the Socket}
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
       TIP6State(ASocket.TransportState).RemoteAddress:=SockAddr6.sin6_addr;

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

function TICMP6Protocol.IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer;
{BSD compatible IO Control Socket}
{Socket: The socket to control}
{Cmd: The socket command}
{Arg: The command argument}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: IoctlSocket');
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

function TICMP6Protocol.GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
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

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: GetPeerName');
 {$ENDIF}

 {Check Socket}
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then Exit;

   {Get Socket Address}
   SockAddr6:=PSockAddr6(@ASockAddr);

   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET6:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;

      {Return the Peer Details}
      SockAddr6.sin6_family:=ASocket.Family;
      SockAddr6.sin6_port:=WordNtoBE(IPPORT_ANY);
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

function TICMP6Protocol.GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
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

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: GetSockName');
 {$ENDIF}

 {Check Socket}
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;

   {Get Socket Address}
   SockAddr6:=PSockAddr6(@ASockAddr);

   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET6:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;

      {Return the Peer Details}
      SockAddr6.sin6_family:=ASocket.Family;
      SockAddr6.sin6_port:=WordNtoBE(IPPORT_ANY);
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

function TICMP6Protocol.GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
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

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: GetSockOpt');
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

function TICMP6Protocol.Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer;
{BSD compatible Listen}
{Socket: The socket to listen on}
{Backlog: Queue depth for accepted connections}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Listen');
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

function TICMP6Protocol.Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Recv');
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
   while TICMP6Socket(ASocket).RecvData.GetCount = 0 do
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
       if GetTickCount64 >= (StartTime + ASocket.SocketOptions.RecvTimeout) then
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
   Size:=TICMP6Socket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;

   {Read Data}
   if TICMP6Socket(ASocket).RecvData.ReadBuffer(ABuffer,Size,nil,AFlags) then
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

function TICMP6Protocol.RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer;
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

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: RecvFrom');
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
   if AFromLength < SizeOf(TSockAddr6) then Exit;

   {Wait for Data}
   StartTime:=GetTickCount64;
   while TICMP6Socket(ASocket).RecvData.GetCount = 0 do
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
       if GetTickCount64 >= (StartTime + ASocket.SocketOptions.RecvTimeout) then
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
   Size:=TICMP6Socket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;

   {Get Socket Address}
   SockAddr6:=PSockAddr6(@AFromAddr);

   {Get Address}
   SockAddr6.sin6_family:=ASocket.Family;
   SockAddr6.sin6_port:=WordNtoBE(IPPORT_ANY);

   {Read Data}
   if TICMP6Socket(ASocket).RecvData.ReadBuffer(ABuffer,Size,@SockAddr6.sin6_addr,AFlags) then
    begin
     {Get Address Length}
     AFromLength:=SizeOf(TSockAddr6);

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

function TICMP6Protocol.Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Send');
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
      Result:=SendPacket(ASocket,@TIP6State(ASocket.TransportState).LocalAddress,@TIP6State(ASocket.TransportState).RemoteAddress,0,0,@Packet,ALength,AFlags);
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

function TICMP6Protocol.SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer;
{BSD compatible Send To}
{Socket: The socket to send to}
{Buffer: Buffer for data to send}
{Length: Length of buffer in bytes}
{Flags: Protocol specific send flags}
{ToAddr: The socket address to send to (Network Order)}
{ToLength: The length of the socket address}

{Note: Caller must hold the Socket lock}
var
 In6Addr:TIn6Addr;
 Route:TRouteEntry;
 Address:TAddressEntry;
 SockAddr6:PSockAddr6;
 Packet:TPacketFragment;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: SendTo');
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

   {Get Socket Address}
   SockAddr6:=PSockAddr6(@AToAddr);

   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   if ASocket.Family <> SockAddr6.sin6_family then Exit;

   {Check Address Family}
   case ASocket.Family of
    AF_INET6:begin
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

         {Bind the Socket}
         ASocket.SocketState.LocalAddress:=True;
         TIP6State(ASocket.TransportState).LocalAddress:=TIP6AddressEntry(Address).Address;

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
      Result:=SendPacket(ASocket,@TIP6State(ASocket.TransportState).LocalAddress,@In6Addr,0,0,@Packet,ALength,AFlags);
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

function TICMP6Protocol.SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
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

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: SetSockOpt');
 {$ENDIF}

 {Check Socket}
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Level}
   case ALevel of
    SOL_SOCKET:begin
      {Socket Options Handled on Set}
      case AOptName of
       SO_RCVBUF:begin
         NetworkSetLastError(WSAEFAULT);
         if AOptLength >= SizeOf(Integer) then
          begin
           TICMP6Socket(ASocket).RecvData.Size:=PInteger(AOptValue)^;
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

function TICMP6Protocol.Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer;
{BSD compatible Shutdown}
{Socket: The socket to shutdown}
{How: The direction to shutdown the socket}

{Note: Shutdown does not result in CloseSocket so Closed must not get set}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Shutdown');
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

function TICMP6Protocol.Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket;
{BSD compatible Socket (Create a new socket)}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}
var
 Transport:TICMP6ProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=TProtocolSocket(INVALID_SOCKET);

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: Socket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Check Socket Type}
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if AStruct <> SOCK_RAW then Exit;

  {Check Address Family}
  NetworkSetLastError(WSAEAFNOSUPPORT);
  if (AFamily = AF_UNSPEC) and (AProtocol <> IPPROTO_IP) then AFamily:=AF_INET6;

  {Check Protocol}
  NetworkSetLastError(WSAEPROTOTYPE);
  if (AProtocol <> IPPROTO_ICMPV6) and (AProtocol <> IPPROTO_IP) then Exit;

  {Get Transport}
  Transport:=TICMP6ProtocolTransport(GetTransportByFamily(AFamily,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;

  {Create Socket}
  Result:=TICMP6Socket.Create(Self,Transport.Transport);

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

function TICMP6Protocol.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this protocol}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TICMP6ProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: AddTransport');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then Exit;

  {Get Transport}
  Transport:=TICMP6ProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET6:begin
       {Add ICMP6 Protocol}
       Handle:=TIP6Transport(ATransport).AddProtocol(IPPROTO_ICMPV6,PacketHandler,ControlHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TICMP6ProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_ICMPV6;
         Transport.Transport:=ATransport;

         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);

          {Add Control Socket}
          Transport.Socket:=TICMP6Socket.Create(Self,ATransport);
          {FSockets.Add(Transport.Socket);} {Dont add this one to the list}

          {Add Proto Entry}
          TIP6Transport(ATransport).AddProto(ICMP6_PROTOCOL_NAME,IPPROTO_ICMPV6,False);

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

function TICMP6Protocol.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this protocol}
{Transport: The transport to remove}
var
 Transport:TICMP6ProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: RemoveTransport');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then Exit;

  {Get Transport}
  Transport:=TICMP6ProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;

  {Check Address Family}
  case ATransport.Family of
   AF_INET6:begin
     {Remove ICMP6 Protocol}
     if TIP6Transport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
      begin
       {Remove Proto Entry}
       TIP6Transport(ATransport).RemoveProto(ICMP6_PROTOCOL_NAME);

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

function TICMP6Protocol.FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket;
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
 Socket:TICMP6Socket;
begin
 {}
 FSockets.ReaderLock;
 try
  Result:=nil;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: FindSocket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Get Socket}
  Socket:=TICMP6Socket(FSockets.First);
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
    Socket:=TICMP6Socket(Socket.Next);
   end;
 finally
  FSockets.ReaderUnlock;
 end;
end;

{==============================================================================}

procedure TICMP6Protocol.FlushSockets(All:Boolean);
{Flush sockets from the socket cache}
{All: If True flush all sockets, otherwise flush expired sockets}
var
 CurrentTime:Int64;
 Socket:TICMP6Socket;
 Current:TICMP6Socket;
begin
 {}
 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: FlushSockets');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Tick Count}
 CurrentTime:=GetTickCount64;

 {Get Socket}
 Socket:=TICMP6Socket(GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
 while Socket <> nil do
  begin
   {Get Next}
   Current:=Socket;
   Socket:=TICMP6Socket(GetSocketByNext(Current,True,False,NETWORK_LOCK_READ));

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

function TICMP6Protocol.StartProtocol:Boolean;
{Start this protocol ready for sending and receiving}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: StartProtocol');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

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

function TICMP6Protocol.StopProtocol:Boolean;
{Stop this protocol ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: StopProtocol');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Close all Sockets}
  FlushSockets(True);

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

function TICMP6Protocol.ProcessProtocol:Boolean;
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
{TICMP6Socket}
constructor TICMP6Socket.Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
begin
 {}
 inherited Create(AProtocol,ATransport);

 {Check Address Family}
 case Family of
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
 FProtocolState:=TProtocolState.Create;
 {Create Protocol Options}
 FProtocolOptions:=TProtocolOptions.Create;

 {Create Receive Buffer}
 FRecvData:=TICMP6Buffer.Create(Self);
 FRecvData.Size:=ICMP6_BUFFER_SIZE;

 {Set Socket Defaults}
 FSocketOptions.SendBuffer:=ICMP6_BUFFER_SIZE;
 FSocketOptions.RecvBuffer:=ICMP6_BUFFER_SIZE;
 FSocketOptions.SendTimeout:=ICMP6_TIMEOUT;
 FSocketOptions.RecvTimeout:=ICMP6_TIMEOUT;
end;

{==============================================================================}

destructor TICMP6Socket.Destroy;
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

function TICMP6Socket.IoCtl(ACommand:DWORD;var AArgument:u_long):Integer;
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Socket: IoCtl');
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

function TICMP6Socket.IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this ICMP6 Socket is Connected to the Host specified by RemoteAddress}
{A connected Socket will have a Bound LocalAddress and a Connected RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Socket: IsConnected');
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

     {Check the Connected RemoteAddress}
     if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).RemoteAddress,PIn6Addr(ARemoteAddress)^) then Exit;

     {Return Result}
     Result:=True;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TICMP6Socket.IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this ICMP6 Socket is Listening on the LocalAddress specified}
{A listening Socket may or may not have a Bound LocalAddress and will have a default (INADDR_ANY) RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Socket: IsListening');
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
{TICMP6Buffer}
constructor TICMP6Buffer.Create(ASocket:TTransportSocket);
begin
 {}
 inherited Create(ASocket);

 FOffset:=SizeOf(TICMP6Packet); {ICMP6_PACKET_SIZE}

 {Check Address Family}
 case FSocket.Family of
  AF_INET6:begin
    FLength:=IP6_ADDRESS_SIZE;
   end;
  else
   begin
    FLength:=IP6_ADDRESS_SIZE;  {Default to TIn6Addr for safety}
   end;
 end;

 FRead:=nil;
 FWrite:=nil;

 FCount:=0;

 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TICMP6Buffer.Destroy;
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

function TICMP6Buffer.AddPacket(ASize:Integer):Boolean;
{Adds a new Packet as the Last packet in the Buffer}
var
 Packet:PICMP6Packet;
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: AddPacket');
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

function TICMP6Buffer.RemovePacket:Boolean;
{Removes the First packet from the Buffer}
var
 Packet:PICMP6Packet;
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: RemovePacket');
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

procedure TICMP6Buffer.FlushPackets;
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

procedure TICMP6Buffer.SetSize(ASize:LongWord);
{Setting the Size clears any current Packets}
begin
 {}
 if not AcquireLock then Exit;
 try
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: SetSize: Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check Size}
  if ASize = 0 then Exit;

  {Get Size}
  ASize:=Max(ASize,ICMP6_BUFFER_SIZE);

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

function TICMP6Buffer.GetNext:Integer;
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

function TICMP6Buffer.GetCount:LongWord;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TICMP6Buffer.ReadBuffer(var ABuffer;var ASize:Integer;ARemoteAddress:Pointer;AFlags:Integer):Boolean;
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
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
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
      {$IFDEF ICMP6_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: Short Read');
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
    {$IFDEF ICMP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: Double Read');
    {$ENDIF}
    {Read the First Block of the Data}
    BlockSize:=(PtrUInt(FEnd) - PtrUInt(ReadNext));
    if BufferSize < BlockSize then
     begin
      {$IFDEF ICMP6_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: Short First Read');
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
        {$IFDEF ICMP6_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: Short Second Read');
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
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
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

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: ReadBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
  {$ENDIF}

  {Return Result}
  Result:=True;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TICMP6Buffer.WriteBuffer(var ABuffer;ASize:Integer;ARemoteAddress:Pointer):Boolean;
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
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: WriteBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
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
    {$IFDEF ICMP6_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: WriteBuffer: Double Write');
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
  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
  {$ENDIF}

  {Set the RemoteAddress}
  if ARemoteAddress <> nil then
   begin
    System.Move(ARemoteAddress^,Pointer(PtrUInt(FLast) + FOffset)^,FLength);
   end;

  {Update the Next Write}
  FWrite:=WriteNext;

  {$IFDEF ICMP6_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6 Buffer: WriteBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
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
procedure ICMP6Init;
begin
 {}
 {Check Initialized}
 if ICMP6Initialized then Exit;

 {Setup ICMP Protocol}
 if NetworkSettings.GetBoolean('IP6_TRANSPORT_ENABLED') then NetworkSettings.AddBoolean('ICMP6_PROTOCOL_ENABLED',True);

 {Create ICMPv6 Protocol}
 if NetworkSettings.GetBooleanDefault('ICMP6_PROTOCOL_ENABLED',ICMP6_PROTOCOL_ENABLED) then
  begin
   TICMP6Protocol.Create(ProtocolManager,ICMP6_PROTOCOL_NAME);
  end;

 ICMP6Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ICMPv6 Functions}
function CheckICMP6(AFamily:Word;ABuffer:Pointer):Boolean;
{Verify that the packet is a valid ICMP6 packet}
{Buffer: The complete packet including Transport header}
var
 Length:Word;
 ICMP6:PICMP6Header;
begin
 {}
 Result:=False;

 {$IFDEF ICMP6_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ICMPv6: CheckICMP6');
 {$ENDIF}

 {Check Address Family}
 case AFamily of
  AF_INET6:begin
    {Get Header}
    ICMP6:=PICMP6Header(PtrUInt(ABuffer) + GetICMP6HeaderOffset(AF_INET6,ABuffer));

    {Check Data Length}
    Length:=GetIP6DataLength(ABuffer);
    if Length >= ICMP6_HEADER_SIZE then
     begin
      {Validate the Header Checksum}

      //To do

     end;
   end;
 end;
end;

{==============================================================================}

function GetICMP6HeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET6:begin
    //To Do
   end;
 end;
end;

{==============================================================================}

function GetICMP6HeaderLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET6:begin
    //To Do
   end;
 end;
end;

{==============================================================================}

function GetICMP6DataOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET6:begin
    {Return Start of ICMP6 Data (If Any) (IP6 Header + ICMP6 Header)}
    Result:=GetICMP6HeaderOffset(AFamily,ABuffer) + GetICMP6HeaderLength(AFamily,ABuffer);
   end;
 end;
end;

{==============================================================================}

function GetICMP6DataLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET6:begin
    //To Do
   end;
 end;
end;

{==============================================================================}

function ChecksumICMP6(AFamily:Word;ABuffer:Pointer;AOffset,ALength:Word):Word;
{Checksum the ICMP6 Header on Send / Validate the Checksum on Receive}
var
 Original:Word;
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET6:begin
    //To Do
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{ICMPv6 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 ICMP6Init;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

