{
Ultibo Raw Socket Protocol unit.

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

 
Raw Socket Protocol
===================


 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Raw;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,SysUtils,Classes,Network,Transport,Protocol,IP,IPv6,Ultibo,UltiboClasses;

//To Do //Look for:

//--

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {Raw specific constants}
 {Note: Some RAW definitions are in the Protocol or IP modules}
 IP_PROTOCOL_NAME = 'IP';
 RAW_PROTOCOL_NAME = 'RAW';
 
 {Raw constants}
 RAW_TIMEOUT = 0;          {Wait forever on a RAW Read}
 RAW_BUFFER_SIZE = 65536;  {RAW Receive Buffer Size}

 RAW_HEADER_SIZE = 0;      {No Header for RAW}

 RAW_PACKET_SIZE = 8;      {SizeOf(TRAWPacket)}
  
{==============================================================================}
type
 {Raw specific types}
 {Note: Some RAW definitions are in the Protocol or IP modules}
 PRAWPacket = ^TRAWPacket;
 TRAWPacket = record   {8 Bytes} {Used by RAWBuffer}
  Size:LongWord;       {LongWord to keep size even}
  Next:PRAWPacket;
 end; {Followed by RemoteAddress (4 or 16 Bytes)}
  
{==============================================================================}
type
 {Raw specific classes}
 TRAWSocket = class;
 TRAWProtocolTransport = class(TProtocolTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TRAWProtocol = class(TNetworkProtocol)
   constructor Create(AManager:TProtocolManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Status Variables}

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
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

 TRAWBuffer = class;
 TRAWSocket = class(TProtocolSocket)  {SOCK_RAW}
   constructor Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   
   {Data Layer Variables}
   FRecvData:TRAWBuffer;
  public
   {Data Layer Properties}
   property RecvData:TRAWBuffer read FRecvData;

   {Public Methods}
   function IoCtl(ACommand:DWORD;var AArgument:u_long):Integer; override;

   function IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
   function IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
 end;

 TRAWBuffer = class(TSocketBuffer)
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

   FFirst:PRAWPacket;       {Pointer to First Packet}
   FLast:PRAWPacket;        {Pointer to Last Packet}

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
 {Raw specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure RAWInit;

{==============================================================================}
{Raw Functions}
  
{==============================================================================}
{Raw Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Raw specific variables}
 RAWInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TRAWProtocol}
constructor TRAWProtocol.Create(AManager:TProtocolManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FProtocol:=IPPROTO_IP;
 FSocketType:=SOCK_RAW;
end;

{==============================================================================}

destructor TRAWProtocol.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TRAWProtocol.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by a Transport}
{Handle: The Handle of the Protocol Transport the packet was received from}
{Source: The source address of the received packet (Set by Transport)}
{Dest: The destination address of the received packet (Set by Transport)}
{Packet: The received packet (The complete packet including Transport header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}

{Note: RAW Accepts any protocol type when creating a Socket}
var
 Offset:Word;
 Length:Word;
 IP:PIPHeader;
 IP6:PIP6Header;
 Socket:TRAWSocket;
 Transport:TRAWProtocolTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF RAW_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}
 
 {Check Source}
 {if ASource = nil then Exit;} {Not Used} 
  
 {Check Packet}
 if APacket = nil then Exit;
 
 {Get Transport}
 Transport:=TRAWProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try 
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Family = ' + AddressFamilyToString(Transport.Transport.Family));
  {$ENDIF}
 
  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Get Header}  
     IP:=PIPHeader(APacket);
     
     {Check for a Connected Socket}
     Socket:=TRAWSocket(FindSocket(AF_INET,SOCK_RAW,IP.Protocol,@IP.DestIP,@IP.SourceIP,0,0,ABroadcast,False,True,NETWORK_LOCK_READ));
     if Socket = nil then
      begin
       {Check for a Listening Socket}
       Socket:=TRAWSocket(FindSocket(AF_INET,SOCK_RAW,IP.Protocol,@IP.DestIP,@IP.SourceIP,0,0,ABroadcast,True,True,NETWORK_LOCK_READ));
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
     
     {$IFDEF RAW_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Offset = ' + IntToStr(Offset) + ' Length = ' + IntToStr(Length));
     {$ENDIF}
     
     {Write the Data into the Receive Buffer}
     Socket.RecvData.WriteBuffer(Pointer(LongWord(IP) + Offset)^,Length,@IP.SourceIP);
     
     {Signal the Event}
     Socket.SignalChange;
     
     {Return True even if the Write failed (eg Buffer is Full, Socket Shutdown etc)}
     Result:=True;
     
     {Unlock Socket}
     Socket.ReaderUnlock;
    end;
   AF_INET6:begin
     {Get Header}  
     IP6:=PIP6Header(APacket);
     
     //To Do //Extensions
     
     {Check for a Connected Socket}
     Socket:=TRAWSocket(FindSocket(AF_INET,SOCK_RAW,IP6.NextHeader,@IP6.DestIP,@IP6.SourceIP,0,0,ABroadcast,False,True,NETWORK_LOCK_READ));
     if Socket = nil then
      begin
       {Check for a Listening Socket}
       Socket:=TRAWSocket(FindSocket(AF_INET,SOCK_RAW,IP6.NextHeader,@IP6.DestIP,@IP6.SourceIP,0,0,ABroadcast,True,True,NETWORK_LOCK_READ));
      end;
     if Socket = nil then Exit;
     
     {Get the Data Offset}
     Offset:=GetIP6DataOffset(IP6);
     Length:=GetIP6DataLength(IP6);
     
     {Account for IP_HDRINCL}
     //if TIP6Options(Socket.TransportOptions).Header then //To Do
     // begin
     //  Offset:=0;
     //  Length:=ASize;
     // end;
     
     {$IFDEF RAW_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Offset = ' + IntToStr(Offset) + ' Length = ' + IntToStr(Length));
     {$ENDIF}
     
     {Write the Data into the Receive Buffer}
     Socket.RecvData.WriteBuffer(Pointer(LongWord(IP6) + Offset)^,Length,@IP6.SourceIP);

     {Signal the Event}
     Socket.SignalChange;
     
     {Return True even if the Write failed (eg Buffer is Full, Socket Shutdown etc)}
     Result:=True;
     
     {Unlock Socket}
     Socket.ReaderUnlock;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRAWProtocol.SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer;
{Source is the working set to check, Dest is the set passed to Select}
var
 Count:Integer;
 Socket:TRAWSocket;
begin
 {}
 Result:=0;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: SelectCheck');
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
      Socket:=TRAWSocket(ASource.fd_array[Count]);
      
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
      Socket:=TRAWSocket(ASource.fd_array[Count]);
      
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

function TRAWProtocol.SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; 
{Socket is the single socket to check, Code is the type of check, Timeout is how long to wait}
var
 StartTime:Int64;
 Socket:TRAWSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: SelectWait');
 {$ENDIF}

 {Get Socket}
 Socket:=TRAWSocket(ASocket);
 
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

function TRAWProtocol.SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
{Send a Packet by adding the Protocol Header and other details to the Data}
{Socket: The socket to use for sending the packet}
{Source: The source address of the packet (Host Order)}
{Dest: The destination address of the packet (Host Order)}
{SourcePort: The source port of the packet (Host Order)}
{DestPort: The destination port of the packet (Host Order)}
{Packet: The packet data to send}
{Size: The size of the packet data in bytes}
{Flags: Any protocol specific flags for sending}

{Note: RAW has no Protocol Header so the packet is passed unchanged}
{Note: Caller must hold the Socket lock}
var
 Transport:TRAWProtocolTransport;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: SendPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Get Transport}
    Transport:=TRAWProtocolTransport(GetTransportByTransport(ASocket.Transport,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;
  
    {Send the Packet}
    Result:=TIPTransport(ASocket.Transport).SendPacket(ASocket,ASource,ADest,APacket,ASize,AFlags);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end;
  AF_INET6:begin
    {Get Transport}
    Transport:=TRAWProtocolTransport(GetTransportByTransport(ASocket.Transport,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;

    {Send the Packet}
    Result:=TIP6Transport(ASocket.Transport).SendPacket(ASocket,ASource,ADest,APacket,ASize,AFlags);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end;
 end;
end;

{==============================================================================}

function TRAWProtocol.Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket;
{BSD compatible Accept}
{Socket: The socket to accept from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=TProtocolSocket(INVALID_SOCKET);
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Accept');
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

function TRAWProtocol.Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Bind}
{Sets the LocalAddress for future Sends and Receives, Address can be specified as INADDR_ANY which allows Listening or auto assignment}
{If Port is IPPORT_ANY then a dynamic Port will be assigned}
{Socket: The socket to bind}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is passed in Network order}
{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Bind');
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
      
      {Bind the Socket}
      ASocket.SocketState.LocalAddress:=True;
      TIPState(ASocket.TransportState).LocalAddress:=InAddrToHost(ASockAddr.sin_addr);
      
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

function TRAWProtocol.CloseSocket(ASocket:TProtocolSocket):Integer;
{BSD compatible Close Socket}
{Closes and removes the socket, does not perform Linger}
{Socket: The socket to close}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: CloseSocket');
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

function TRAWProtocol.Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Connect}
{Sets the RemoteAddress of future Sends and Receives, if Bind has not been called then the
 LocalAddress will be set appropriately as well based on the route to the RemoteAddress}
{Socket: The socket to connect}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is passed in Network order}
{Note: Caller must hold the Socket lock}
var
 Route:TRouteEntry;
 Address:TAddressEntry;
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Connect');
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

function TRAWProtocol.IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer;
{BSD compatible IO Control Socket}
{Socket: The socket to control}
{Cmd: The socket command}
{Arg: The command argument}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: IoctlSocket');
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

function TRAWProtocol.GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Peer Name (Remote)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: GetPeerName');
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
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
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

function TRAWProtocol.GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Sock Name (Local)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: GetSockName');
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
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
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

function TRAWProtocol.GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
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
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: GetSockOpt');
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

function TRAWProtocol.Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer;
{BSD compatible Listen}
{Socket: The socket to listen on}
{Backlog: Queue depth for accepted connections}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Listen');
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

function TRAWProtocol.Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Recv');
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
   while TRAWSocket(ASocket).RecvData.GetCount = 0 do
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
   Size:=TRAWSocket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;
   
   {Read Data}
   if TRAWSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,nil,AFlags) then
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

function TRAWProtocol.RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer;
{BSD compatible Receive From}
{Socket: The socket to receive from}
{Buffer: Buffer for received data}
{Length: Length of buffer in bytes}
{Flags: Protocol specific receive flags}
{FromAddr: The address the data was received from (Network Order)}
{FromLength: The length of the address}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 StartTime:Int64;
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: RecvFrom');
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
   while TRAWSocket(ASocket).RecvData.GetCount = 0 do
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
   Size:=TRAWSocket(ASocket).RecvData.GetNext;
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
      AFromAddr.sin_port:=WordNtoBE(IPPORT_ANY);
   
      {Read Data}
      if TRAWSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,@AFromAddr.sin_addr,AFlags) then
       begin
        {Get Address}
        AFromAddr.sin_addr:=InAddrToNetwork(AFromAddr.sin_addr);
        
        {Return Size}
        Result:=Size;
       end;
     end;
    AF_INET6:begin
      {Get Address}
      SockAddr6.sin6_family:=ASocket.Family;
      SockAddr6.sin6_port:=WordNtoBE(IPPORT_ANY);
      
      {Read Data}
      if TRAWSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,@SockAddr6.sin6_addr,AFlags) then
       begin
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

function TRAWProtocol.Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Send');
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

function TRAWProtocol.SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer;
{BSD compatible Send To}
{Socket: The socket to send to}
{Buffer: Buffer for data to send}
{Length: Length of buffer in bytes}
{Flags: Protocol specific send flags}
{ToAddr: The socket address to send to (Network Order)}
{ToLength: The length of the socket address}

{Note: SockAddr is passed in Network order}
{Note: Caller must hold the Socket lock}
var
 Address:TInAddr;
 Address6:TIn6Addr;
 SockAddr6:PSockAddr6;
 Packet:TPacketFragment;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: SendTo');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
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
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> AToAddr.sin_family then Exit;
      
      {Check size of ToAddr}
      NetworkSetLastError(WSAEFAULT);
      if AToLength < SizeOf(TSockAddr) then Exit;
      
      {Get the RemoteAddress}
      Address:=InAddrToHost(AToAddr.sin_addr);
      
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIPTransport(ASocket.Transport).CompareDefault(Address) then Exit;
      
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIPTransport(ASocket.Transport).CompareBroadcast(Address) or TIPTransport(ASocket.Transport).CompareDirected(Address) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Create the Fragment}
      Packet.Size:=ALength;
      Packet.Data:=@ABuffer;
      Packet.Next:=nil;
      
      {Send the Packet}
      Result:=SendPacket(ASocket,@TIPState(ASocket.TransportState).LocalAddress,@Address,0,0,@Packet,ALength,AFlags);
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
      Address6:=SockAddr6.sin6_addr;
      
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIP6Transport(ASocket.Transport).CompareDefault(Address6) then Exit;
      
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEACCES);
      if TIP6Transport(ASocket.Transport).CompareBroadcast(Address6) or TIP6Transport(ASocket.Transport).CompareDirected(Address6) then
       begin
        if not ASocket.SocketOptions.Broadcast then Exit;
       end;
      
      {Create the Fragment}
      Packet.Size:=ALength;
      Packet.Data:=@ABuffer;
      Packet.Next:=nil;
      
      {Send the Packet}
      Result:=SendPacket(ASocket,@TIP6State(ASocket.TransportState).LocalAddress,@Address6,0,0,@Packet,ALength,AFlags);
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

function TRAWProtocol.SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
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
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: SetSockOpt');
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
           TRAWSocket(ASocket).RecvData.Size:=PInteger(AOptValue)^;
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

function TRAWProtocol.Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer;
{BSD compatible Shutdown}
{Socket: The socket to shutdown}
{How: The direction to shutdown the socket}

{Note: Shutdown does not result in CloseSocket so Closed must not get set}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF RAW_DEBUG} 
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Shutdown');
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

function TRAWProtocol.Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket;
{BSD compatible Socket (Create a new socket)}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}
var
 Transport:TRAWProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=TProtocolSocket(INVALID_SOCKET);
 
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: Socket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Check Socket Type}
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if AStruct <> SOCK_RAW then Exit;
  
  {Check Address Family}
  NetworkSetLastError(WSAEAFNOSUPPORT);
  if (AFamily = AF_UNSPEC) and (AProtocol <> IPPROTO_IP) then AFamily:=AF_INET;

  {Get Transport}
  Transport:=TRAWProtocolTransport(GetTransportByFamily(AFamily,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  
  {Create Socket}
  Result:=TRAWSocket.Create(Self,Transport.Transport);
  Result.Proto:=AProtocol; {SOCK_RAW accepts any Protocol}
  
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

function TRAWProtocol.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this protocol}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TRAWProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport} 
  Transport:=TRAWProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
      AF_INET:begin
        {Add RAW Protocol}
        Handle:=TIPTransport(ATransport).AddProtocol(IPPROTO_IP,PacketHandler,nil);
        if Handle <> INVALID_HANDLE_VALUE then
         begin
          {Create Transport}
          Transport:=TRAWProtocolTransport.Create;
          Transport.Handle:=Handle;
          Transport.Protocol:=IPPROTO_IP;
          Transport.Transport:=ATransport;
          
          {Acquire Lock}
          FTransports.WriterLock;
          try
           {Add Transport}
           FTransports.Add(Transport);
          
           {Add Proto Entries}
           TIPTransport(ATransport).AddProto(IP_PROTOCOL_NAME,IPPROTO_IP,False);
           TIPTransport(ATransport).AddProto(RAW_PROTOCOL_NAME,IPPROTO_RAW,False);
           
           {Return Result}
           Result:=True;
          finally
           {Release Lock}
           FTransports.WriterUnlock;
          end;  
         end;
       end;
      AF_INET6:begin
        {Add RAW Protocol}
        Handle:=TIP6Transport(ATransport).AddProtocol(IPPROTO_IP,PacketHandler,nil);
        if Handle <> INVALID_HANDLE_VALUE then
         begin
          {Create Transport}
          Transport:=TRAWProtocolTransport.Create;
          Transport.Handle:=Handle;
          Transport.Protocol:=IPPROTO_IP;
          Transport.Transport:=ATransport;
          
          {Acquire Lock}
          FTransports.WriterLock;
          try
           {Add Transport}
           FTransports.Add(Transport);
           
           {Add Proto Entries}
           TIP6Transport(ATransport).AddProto(IP_PROTOCOL_NAME,IPPROTO_IP,False);
           TIP6Transport(ATransport).AddProto(RAW_PROTOCOL_NAME,IPPROTO_RAW,False);
          
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

function TRAWProtocol.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this protocol}
{Transport: The transport to remove}
var
 Transport:TRAWProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport}
  Transport:=TRAWProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
 
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove RAW Protocol}
     if TIPTransport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
      begin
       {Remove Proto Entries}
       TIPTransport(ATransport).RemoveProto(RAW_PROTOCOL_NAME);
       TIPTransport(ATransport).RemoveProto(IP_PROTOCOL_NAME);
       
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
   AF_INET6:begin
     {Remove RAW Protocol}
     if TIP6Transport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
      begin
       {Remove Proto Entries}
       TIP6Transport(ATransport).RemoveProto(RAW_PROTOCOL_NAME);
       TIP6Transport(ATransport).RemoveProto(IP_PROTOCOL_NAME);
       
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

function TRAWProtocol.FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket;
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
 Socket:TRAWSocket;
begin
 {}
 FSockets.ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: FindSocket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Get Socket}
  Socket:=TRAWSocket(FSockets.First);
  while Socket <> nil do
   begin
    {Check for Closed}
    if not Socket.SocketState.Closed then
     begin
      {Check for Match}
      if (Socket.Family = AFamily) and (Socket.Struct = AStruct) then
       begin
        {Note: IPPROTO_IP matches any Protocol}
        if (Socket.Proto = AProtocol) or (Socket.Proto = IPPROTO_IP) then
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
     end;
     
    {Get Next}
    Socket:=TRAWSocket(Socket.Next);
   end;
 finally 
  FSockets.ReaderUnlock;
 end; 
end;

{==============================================================================}

procedure TRAWProtocol.FlushSockets(All:Boolean);
{Flush sockets from the socket cache}
{All: If True flush all sockets, otherwise flush expired sockets}
var
 CurrentTime:Int64;
 Socket:TRAWSocket;
 Current:TRAWSocket;
begin
 {}
 {$IFDEF RAW_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: FlushSockets');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW:  All = ' + BoolToStr(All));
 {$ENDIF}
  
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Get Socket}
 Socket:=TRAWSocket(GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
 while Socket <> nil do
  begin
   {Get Next}
   Current:=Socket;
   Socket:=TRAWSocket(GetSocketByNext(Current,True,False,NETWORK_LOCK_READ));
    
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

function TRAWProtocol.StartProtocol:Boolean;
{Start this protocol ready for sending and receiving}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: StartProtocol');
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

function TRAWProtocol.StopProtocol:Boolean;
{Stop this protocol ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW: StopProtocol');
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

function TRAWProtocol.ProcessProtocol:Boolean;
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
{TRAWSocket}
constructor TRAWSocket.Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
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
 FProtocolState:=TProtocolState.Create;
 {Create Protocol Options}
 FProtocolOptions:=TProtocolOptions.Create;

 {Create Receive Buffer}
 FRecvData:=TRAWBuffer.Create(Self);
 FRecvData.Size:=RAW_BUFFER_SIZE;

 {Set Socket Defaults}
 FSocketOptions.SendTimeout:=RAW_TIMEOUT;
 FSocketOptions.RecvTimeout:=RAW_TIMEOUT;
end;

{==============================================================================}

destructor TRAWSocket.Destroy;
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

function TRAWSocket.IoCtl(ACommand:DWORD;var AArgument:u_long):Integer;
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
  
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Socket: IoCtl');
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

function TRAWSocket.IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this RAW Socket is Connected to the Host specified by RemoteAddress}
{A connected Socket will have a Bound LocalAddress and a Connected RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Socket: IsConnected');
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

function TRAWSocket.IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this RAW Socket is Listening on the LocalAddress specified}
{A listening Socket may or may not have a Bound LocalAddress and will have a default (INADDR_ANY) RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Socket: IsListening');
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
{TRAWBuffer}
constructor TRAWBuffer.Create(ASocket:TTransportSocket);
begin
 {}
 inherited Create(ASocket);
 
 FOffset:=SizeOf(TRAWPacket); {RAW_PACKET_SIZE}
 
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

destructor TRAWBuffer.Destroy;
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

function TRAWBuffer.AddPacket(ASize:Integer):Boolean;
{Adds a new Packet as the Last packet in the Buffer}
var
 Packet:PRAWPacket;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: AddPacket');
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

function TRAWBuffer.RemovePacket:Boolean;
{Removes the First packet from the Buffer}
var
 Packet:PRAWPacket;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: RemovePacket');
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

procedure TRAWBuffer.FlushPackets;
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

procedure TRAWBuffer.SetSize(ASize:LongWord);
{Setting the Size clears any current Packets}
begin
 {}
 if not AcquireLock then Exit;
 try
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: SetSize: Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check Size}
  if ASize = 0 then Exit;
  
  {Get Size}
  ASize:=Max(ASize,RAW_BUFFER_SIZE);
  
  {Clear any Packets}
  FlushPackets;
  
  {Allocate the Memory}
  FBuffer.SetSize(ASize);
  
  {Set the Buffer Values}
  FSize:=ASize;
  FStart:=FBuffer.Memory;
  
  {End actually points to byte beyond last for simpler calculations}
  FEnd:=Pointer(LongWord(FStart) + FSize);
  
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

function TRAWBuffer.GetNext:Integer;
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

function TRAWBuffer.GetCount:LongWord;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TRAWBuffer.ReadBuffer(var ABuffer;var ASize:Integer;ARemoteAddress:Pointer;AFlags:Integer):Boolean;
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
  {$IFDEF RAW_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: FStart = ' + IntToHex(LongWord(FStart),8) + ' FEnd = ' + IntToHex(LongWord(FEnd),8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: ReadNext = ' + IntToStr(LongWord(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
  {$ENDIF}
  
  {Get the Return Size}
  BufferSize:=Min(ASize,ReadSize);
  ASize:=BufferSize;
  
  {Since we Guarantee ReadNext to be at least 1 byte from the End of the Buffer, we can start reading}
  {Check for Single or Double Read}
  if (LongWord(ReadNext) + ReadSize) <= LongWord(FEnd) then
   begin
    {Single Read with no wrap around}
    {Read the Data}
    if BufferSize < ReadSize then
     begin
      {$IFDEF RAW_DEBUG} 
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: Short Read');
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
    {$IFDEF RAW_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: Double Read');
    {$ENDIF}
    {Read the First Block of the Data}
    BlockSize:=(LongWord(FEnd) - LongWord(ReadNext));
    if BufferSize < BlockSize then
     begin
      {$IFDEF RAW_DEBUG} 
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: Short First Read');
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
        {$IFDEF RAW_DEBUG} 
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: Short Second Read');
        {$ENDIF}
        System.Move(ReadNext^,Pointer(LongWord(@ABuffer) + BlockSize)^,BufferSize);
       end
      else
       begin
        System.Move(ReadNext^,Pointer(LongWord(@ABuffer) + BlockSize)^,ReadSize);
       end;
     end;
    
    Inc(PtrUInt(ReadNext),ReadSize);
    Dec(ReadSize,ReadSize);
   end;
  
  {Check for Wrap around}
  if LongWord(ReadNext) = LongWord(FEnd) then ReadNext:=FStart;
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: ReadNext = ' + IntToStr(LongWord(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
  {$ENDIF}
  
  {Get the Remote Address}
  if ARemoteAddress <> nil then
   begin
    System.Move(Pointer(LongWord(FFirst) + FOffset)^,ARemoteAddress^,FLength);
   end;
  
  {Check for Peek Flag}
  if (AFlags and MSG_PEEK) = 0 then
   begin
    {Update the Next Read}
    FRead:=ReadNext;
    
    {Remove the Packet}
    RemovePacket;
   end;
  
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: ReadBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
  {$ENDIF}
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TRAWBuffer.WriteBuffer(var ABuffer;ASize:Integer;ARemoteAddress:Pointer):Boolean;
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
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: WriteBuffer: FStart = ' + IntToHex(LongWord(FStart),8) + ' FEnd = ' + IntToHex(LongWord(FEnd),8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: WriteBuffer: WriteNext = ' + IntToStr(LongWord(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
  {$ENDIF}
  
  {Since we guarantee WriteNext to be at least 1 byte from the End of the Buffer, we can start writing}
  if (LongWord(WriteNext) + WriteSize) <= LongWord(FEnd) then
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
    {$IFDEF RAW_DEBUG} 
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: WriteBuffer: Double Write');
    {$ENDIF}
    {Write the First Block of the Packet Data}
    BlockSize:=(LongWord(FEnd) - LongWord(WriteNext));
    System.Move(ABuffer,WriteNext^,BlockSize);
    
    {Wrap to Start of Buffer}
    WriteNext:=FStart;
    Dec(WriteSize,BlockSize);
    
    {Write the Second Block of the Packet Data}
    System.Move(Pointer(LongWord(@ABuffer) + BlockSize)^,WriteNext^,WriteSize);
    
    Inc(PtrUInt(WriteNext),WriteSize);
    Dec(WriteSize,WriteSize);
   end;
   
  {Check for Wrap around}
  if LongWord(WriteNext) = LongWord(FEnd) then WriteNext:=FStart;
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: WriteBuffer: WriteNext = ' + IntToStr(LongWord(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
  {$ENDIF}
  
  {Set the RemoteAddress}
  if ARemoteAddress <> nil then
   begin
    System.Move(ARemoteAddress^,Pointer(LongWord(FLast) + FOffset)^,FLength);
   end;
  
  {Update the Next Write}
  FWrite:=WriteNext;
  {$IFDEF RAW_DEBUG} 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RAW Buffer: WriteBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
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
procedure RAWInit;
begin
 {}
 {Check Initialized}
 if RAWInitialized then Exit;

 {Create RAW Protocol}
 if NetworkSettings.GetBooleanDefault('RAW_PROTOCOL_ENABLED',RAW_PROTOCOL_ENABLED) then 
  begin
   TRAWProtocol.Create(ProtocolManager,RAW_PROTOCOL_NAME);
  end; 
 
 RAWInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Raw Functions}
  
{==============================================================================}
{==============================================================================}
{Raw Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 RAWInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
  