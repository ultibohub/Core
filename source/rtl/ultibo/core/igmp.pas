{
Ultibo IGMP (Internet Group Management Protocol) unit.

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

 Multicast Address Assignments

  http://www.iana.org/assignments/multicast-addresses/multicast-addresses.xhtml

Internet Group Management Protocol
==================================

 Notes: Supports IGMP Version 1 and 2

 Notes: IGMP Checksum includes both Header and Data (if any)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit IGMP;

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
  IP,
  Ultibo,
  UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {IGMP specific constants}
 {Note: Some IGMP definitions are in the Protocol or IP modules}
 IGMP_PROTOCOL_NAME = 'IGMP';

 {IGMP constants}
 IGMP_TIMEOUT = 0;          {Wait forever on a IGMP Read}
 IGMP_BUFFER_SIZE = 65536;  {IGMP Receive Buffer Size}

 IGMP_HEADER_SIZE = 8;      {SizeOf(TIGMPHeader);}

 IGMP_PACKET_SIZE = 8;      {SizeOf(TIGMPPacket)}

 IGMP_QUERY_TIMEOUT = 5000;  //To Do - See RFC
 IGMP_REPORT_TIMEOUT = 5000; //To Do - See RFC

 IGMP_TRANSMIT_COUNT = 3;    //To Do - See RFC

{==============================================================================}
type
 {IGMP specific types}
 {Note: Some IGMP definitions are in the Protocol or IP modules}
 PIGMPHeader = ^TIGMPHeader;
 TIGMPHeader = packed record
  IGMPType:Byte;          {type of IGMP message}
  RespTime:Byte;          {Query only otherwise 0} {Always zero in Version 1 (Assume 100)}
  Checksum:Word;          {1s Compliment IP-style checksum}
  Address:TInAddr;        {group address being reported or queried}
 end;                     {zero for "General" queries}

 PIGMPPacket = ^TIGMPPacket;
 TIGMPPacket = record   {8 Bytes} {Used by IGMPBuffer}
  Size:LongWord;        {LongWord to keep size even}
  Next:PIGMPPacket;
 end; {Followed by RemoteAddress (4 or 16 Bytes)}

{==============================================================================}
type
 {IGMP specific classes}
 TIGMPSocket = class;
 TIGMPProtocolTransport = class(TProtocolTransport)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}

  public
   {Status Variables}
   Socket:TIGMPSocket;   {Socket for sending replies}
 end;

 TIGMPGroup = class;
 TIGMPProtocol = class(TNetworkProtocol)
   constructor Create(AManager:TProtocolManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Status Variables}
   FGroups:TNetworkList;    {List of TIGMPGroup objects}

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
   function ControlHandler(AHandle:THandle;ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;

   function SendIGMP1Report(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
   function SendIGMP2Report(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
   function SendIGMP2Leave(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
   function SendIGMP1Query(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
   function SendIGMP2Query(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;

   function GetGroup(ATransport:TIGMPProtocolTransport;const AAddress:TInAddr;ALock:Boolean):TIGMPGroup;
   function GetGroupByNext(APrevious:TIGMPGroup;ALock,AUnlock:Boolean):TIGMPGroup;
   function AddGroup(ATransport:TIGMPProtocolTransport;const AAddress:TInAddr;const AHardware:THardwareAddress;ALock:Boolean):TIGMPGroup;
   function RemoveGroup(ATransport:TIGMPProtocolTransport;const AAddress:TInAddr):Boolean;
   procedure FlushGroups(All:Boolean);
   function ProcessGroups:Boolean;
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

 TIGMPGroup = class(TListObject) //To Do //Make a base TGroupEntry class in Transport ? (To hold the lock etc)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;

   {Status Variables}
   FCount:Word;                     {Number of memberships}
   FQueryTime:Int64;                {IGMP query reply timer}
   FReportTime:Int64;               {IGMP report reply/transmit timer}
   FTransmitCount:Word;             {IGMP report transmit counter}
   FAddress:TInAddr;                {IP address of group}
   FHardware:THardwareAddress;      {Hardware address of group}
   FTransport:TNetworkTransport;    {Transport provider for group}

   {Internal Methods}
   procedure SetCount(ACount:Word);
   function GetActive:Boolean;
   procedure SetQueryTime(const AQueryTime:Int64);
   procedure SetReportTime(const AReportTime:Int64);
   procedure SetTransmitCount(ATransmitCount:Word);
   procedure SetAddress(const AAddress:TInAddr);
   procedure SetHardware(const AHardware:THardwareAddress);
   procedure SetTransport(ATransport:TNetworkTransport);
  public
   {Public Properties}
   property Count:Word read FCount write SetCount;
   property Active:Boolean read GetActive;
   property QueryTime:Int64 read FQueryTime write SetQueryTime;
   property ReportTime:Int64 read FReportTime write SetReportTime;
   property TransmitCount:Word read FTransmitCount write SetTransmitCount;
   property Address:TInAddr read FAddress write SetAddress;
   property Hardware:THardwareAddress read FHardware write SetHardware;
   property Transport:TNetworkTransport read FTransport write SetTransport;

   {Public Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   procedure IncrementCount;
   procedure DecrementCount;
   procedure IncrementTransmitCount;
   procedure DecrementTransmitCount;
 end;

 TIGMPBuffer = class;
 TIGMPSocket = class(TProtocolSocket)  {SOCK_RAW}
   constructor Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Data Layer Variables}
   FRecvData:TIGMPBuffer;
  public
   {Data Layer Properties}
   property RecvData:TIGMPBuffer read FRecvData;

   {Public Methods}
   function IoCtl(ACommand:DWORD;var AArgument:u_long):Integer; override;

   function IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
   function IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
 end;

 TIGMPBuffer = class(TSocketBuffer)
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

   FFirst:PIGMPPacket;      {Pointer to First Packet}
   FLast:PIGMPPacket;       {Pointer to Last Packet}

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
 {IGMP specific variables}

{==============================================================================}
{Initialization Functions}
procedure IGMPInit;

{==============================================================================}
{IGMP Functions}
function CheckIGMP(AFamily:Word;ABuffer:Pointer):Boolean;

function GetIGMPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetIGMPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
function GetIGMPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetIGMPDataLength(AFamily:Word;ABuffer:Pointer):Word;

function ChecksumIGMP(AFamily:Word;ABuffer:Pointer;AOffset,ALength:Word):Word;

{==============================================================================}
{IGMP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {IGMP specific variables}
 IGMPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TIGMPProtocolTransport}
constructor TIGMPProtocolTransport.Create;
begin
 {}
 inherited Create;
 Socket:=nil;
end;

{==============================================================================}

destructor TIGMPProtocolTransport.Destroy;
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
{TIGMPProtocol}
constructor TIGMPProtocol.Create(AManager:TProtocolManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FProtocol:=IPPROTO_IGMP;
 FSocketType:=SOCK_RAW;
 FGroups:=TNetworkList.Create;
end;

{==============================================================================}

destructor TIGMPProtocol.Destroy;
begin
 {}
 WriterLock;
 try
  FGroups.Free;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TIGMPProtocol.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
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
 IGMP:PIGMPHeader;
 Group:TIGMPGroup;
 Socket:TIGMPSocket;
 Transport:TIGMPProtocolTransport;
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}

 {Check Source}
 {if ASource = nil then Exit;} {Not Used}

 {Check Packet}
 if APacket = nil then Exit;

 {Get Transport}
 Transport:=TIGMPProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Family = ' + AddressFamilyToString(Transport.Transport.Family));
  {$ENDIF}

  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Get Header}
     IP:=PIPHeader(APacket);

     {$IFDEF IGMP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Protocol = ' + ProtocolToString(Transport.Protocol));
     {$ENDIF}

     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_IGMP:begin
        {Check for a valid IGMP Packet}
        if CheckIGMP(AF_INET,IP) then
         begin
          {Get Header}
          IGMP:=PIGMPHeader(PtrUInt(IP) + GetIGMPHeaderOffset(AF_INET,IP));

          {Check for a Type that we handle}
          case IGMP.IGMPType of
           IGMP_QUERY:begin
             {Return Result}
             Result:=True;

             {$IFDEF IGMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  IGMP 1/2 Query');
             {$ENDIF}

             if IGMP.RespTime = 0 then IGMP.RespTime:=100;

             if TIPTransport(Transport.Transport).CompareDefault(InAddrToHost(IGMP.Address)) then
              begin
               {General Query}
               Group:=TIGMPGroup(GetGroupByNext(nil,True,False));
               while Group <> nil do
                begin
                 {Check Active}
                 if Group.Active then
                  begin
                   {Set the Report Timeout}
                   Group.ReportTime:=GetTickCount64 + IGMP.RespTime;

                   {Set the Transmit Counter}
                   Group.TransmitCount:=1;
                  end;

                 {Get Next}
                 Group:=TIGMPGroup(GetGroupByNext(Group,True,True));
                end;
              end
             else
              begin
               {Specific Query}
               Group:=GetGroup(Transport,InAddrToHost(IGMP.Address),True);
               if Group = nil then Exit;

               {Check Active}
               if Group.Active then
                begin
                 {Set the Report Timeout}
                 Group.ReportTime:=GetTickCount64 + IGMP.RespTime;

                 {Set the Transmit Counter}
                 Group.TransmitCount:=1;
                end;

               {Unlock Group}
               Group.ReleaseLock;
              end;
            end;
           IGMP_REPORTV1,IGMP_REPORTV2:begin
             {Return Result}
             Result:=True;

             {$IFDEF IGMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  IGMP 1/2 Report');
             {$ENDIF}

             {Get the Group}
             Group:=GetGroup(Transport,InAddrToHost(IGMP.Address),True);
             if Group = nil then Exit;

             {Reset the Query Timeout}
             Group.QueryTime:=0;

             {Reset the Report Timeout}
             Group.ReportTime:=0;

             {Reset the Transmit Counter}
             Group.TransmitCount:=0;

             {Unlock Group}
             Group.ReleaseLock;
            end;
           IGMP_LEAVE:begin
             {Return Result}
             Result:=True;

             {$IFDEF IGMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  IGMP 2 Leave');
             {$ENDIF}

             {Note: Only a Router should process this}
            end;
           else
            begin
             {Check for a Connected Socket}
             Socket:=TIGMPSocket(FindSocket(AF_INET,SOCK_RAW,IPPROTO_IGMP,@IP.DestIP,@IP.SourceIP,0,0,ABroadcast,False,True,NETWORK_LOCK_READ));
             if Socket = nil then
              begin
               {Check for a Listening Socket}
               Socket:=TIGMPSocket(FindSocket(AF_INET,SOCK_RAW,IPPROTO_IGMP,@IP.DestIP,@IP.SourceIP,0,0,ABroadcast,True,True,NETWORK_LOCK_READ));
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

             {$IFDEF IGMP_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Offset = ' + IntToStr(Offset) + ' Length = ' + IntToStr(Length));
             {$ENDIF}

             {Write the Data into the Receive Buffer}
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

function TIGMPProtocol.ControlHandler(AHandle:THandle;ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;
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

{Note: Addresses are passed in Host order}
var
 Group:TIGMPGroup;
 Transport:TIGMPProtocolTransport;
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: ControlHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Dest}
 {if ADest = nil then Exit;} {May be nil}

 {Check Source}
 {if ASource = nil then Exit;} {May be nil}

 {Check Data}
 {if AData = nil then Exit;} {May be nil}

 {Get Transport}
 Transport:=TIGMPProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_IGMP:begin
        {Check Command}
        case ACommand of
         IGMP_QUERY:begin
           {Note: Only a Router should send this}
           {$IFDEF IGMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  IGMP 1/2 Query');
           {$ENDIF}

           {Get the Group}
           Group:=GetGroup(Transport,PInAddr(AAddress)^,True);
           if Group = nil then Exit;

           {Set the Query Timeout}
           Group.QueryTime:=GetTickCount64 + IGMP_QUERY_TIMEOUT;

           {Set the Transmit Counter}
           Group.TransmitCount:=IGMP_TRANSMIT_COUNT - 1;

           {Lock Socket}
           Transport.Socket.ReaderLock;

           {Send the Query}
           Result:=SendIGMP2Query(Transport.Socket,ASource,ADest,AAddress);

           {Unlock Socket}
           Transport.Socket.ReaderUnlock;

           {Unlock Group}
           Group.ReleaseLock;
          end;
         IGMP_REPORTV1:begin
           {$IFDEF IGMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  IGMP 1 Report');
           {$ENDIF}

           {Check the Group}
           Group:=GetGroup(Transport,PInAddr(AAddress)^,True);
           if Group <> nil then
            begin
             {Increment Count}
             Group.IncrementCount;

             {Return Result}
             Result:=True;
            end
           else
            begin
             {Add the Group}
             Group:=AddGroup(Transport,PInAddr(AAddress)^,PHardwareAddress(AData)^,True);
             if Group = nil then Exit;

             {Set the Report (Retransmit) Timeout}
             Group.ReportTime:=GetTickCount64 + IGMP_REPORT_TIMEOUT;

             {Set the Transmit Counter}
             Group.TransmitCount:=IGMP_TRANSMIT_COUNT - 1;

             {Lock Socket}
             Transport.Socket.ReaderLock;

             {Send the Report}
             Result:=SendIGMP1Report(Transport.Socket,ASource,ADest,AAddress);

             {Unlock Socket}
             Transport.Socket.ReaderUnlock;
            end;

           {Unlock Group}
           Group.ReleaseLock;
          end;
         IGMP_REPORTV2:begin
           {$IFDEF IGMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  IGMP 2 Report');
           {$ENDIF}

           {Check the Group}
           Group:=GetGroup(Transport,PInAddr(AAddress)^,True);
           if Group <> nil then
            begin
             {Increment Count}
             Group.IncrementCount;

             {Return Result}
             Result:=True;
            end
           else
            begin
             {Add the Group}
             Group:=AddGroup(Transport,PInAddr(AAddress)^,PHardwareAddress(AData)^,True);
             if Group = nil then Exit;

             {Set the Report (Retransmit) Timeout}
             Group.ReportTime:=GetTickCount64 + IGMP_REPORT_TIMEOUT;

             {Set the Transmit Counter}
             Group.TransmitCount:=IGMP_TRANSMIT_COUNT - 1;

             {Lock Socket}
             Transport.Socket.ReaderLock;

             {Send the Report}
             Result:=SendIGMP2Report(Transport.Socket,ASource,ADest,AAddress);

             {Unlock Socket}
             Transport.Socket.ReaderUnlock;
            end;

           {Unlock Group}
           Group.ReleaseLock;
          end;
         IGMP_LEAVE:begin
           {$IFDEF IGMP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  IGMP 2 Leave');
           {$ENDIF}

           {Check the Group}
           Group:=GetGroup(Transport,PInAddr(AAddress)^,True);
           if Group = nil then Exit;

           {Decrement Count}
           Group.DecrementCount;

           {Check for Active}
           if not Group.Active then
            begin
             {Lock Socket}
             Transport.Socket.ReaderLock;

             {Send the Leave}
             Result:=SendIGMP2Leave(Transport.Socket,ASource,ADest,AAddress);

             {Unlock Socket}
             Transport.Socket.ReaderUnlock;
            end
           else
            begin
             {Return Result}
             Result:=True;
            end;

           {Unlock Group}
           Group.ReleaseLock;
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

function TIGMPProtocol.SendIGMP1Report(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
{Send an IGMP V1 report message (Join Group)}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Address: The address to send for the message}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SendIGMP1Report');
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Address}
 if AAddress = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIGMPProtocol.SendIGMP2Report(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
{Send an IGMP V2 report message (Join Group)}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Address: The address to send for the message}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SendIGMP2Report');
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Address}
 if AAddress = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIGMPProtocol.SendIGMP2Leave(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
{Send an IGMP leave message (Leave Group)}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Address: The address to send for the message}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SendIGMP2Leave');
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Address}
 if AAddress = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIGMPProtocol.SendIGMP1Query(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
{Send an IGMP V1 query message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Address: The address to send for the message}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SendIGMP1Query');
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Address}
 if AAddress = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIGMPProtocol.SendIGMP2Query(ASocket:TIGMPSocket;ASource,ADest,AAddress:Pointer):Boolean;
{Send an IGMP V2 query message}
{Socket: The socket to use for sending the message}
{Source: The source address to use for the message}
{Dest: The destination address to use for the message}
{Address: The address to send for the message}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SendIGMP2Query');
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Address}
 if AAddress = nil then Exit;

 //To Do
end;

{==============================================================================}

function TIGMPProtocol.GetGroup(ATransport:TIGMPProtocolTransport;const AAddress:TInAddr;ALock:Boolean):TIGMPGroup;
{Find the address in the group cache}
{Transport: The transport for the group to find}
{Address: The address for the group to find}
{Lock: If True then lock the found entry before returning}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Transport lock}
var
 Group:TIGMPGroup;
begin
 {}
 FGroups.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: GetGroup');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then Exit;

  {Get Group}
  Group:=TIGMPGroup(FGroups.First);
  while Group <> nil do
   begin
    {Check Transport}
    if Group.Transport = ATransport.Transport then
     begin
      {$IFDEF IGMP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Compare = ' + InAddrToString(InAddrToNetwork(Group.Address)));
      {$ENDIF}

      {Check Address Family}
      case Group.Transport.Family of
       AF_INET:begin
         {Check Address}
         if TIPTransport(Group.Transport).CompareAddress(Group.Address,AAddress) then
          begin
           {Lock Group}
           if ALock then Group.AcquireLock;

           {Return Result}
           Result:=Group;
           Exit;
          end;
        end;
      end;
     end;

    {Get Next}
    Group:=TIGMPGroup(Group.Next);
   end;
 finally
  FGroups.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIGMPProtocol.GetGroupByNext(APrevious:TIGMPGroup;ALock,AUnlock:Boolean):TIGMPGroup;
var
 Group:TIGMPGroup;
begin
 {}
 FGroups.ReaderLock;
 try
  Result:=nil;

  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Group:=TIGMPGroup(FGroups.First);
    if Group <> nil then
     begin
      {Lock Group}
      if ALock then Group.AcquireLock;

      {Return Result}
      Result:=Group;
     end;
   end
  else
   begin
    {Get Next}
    Group:=TIGMPGroup(APrevious.Next);
    if Group <> nil then
     begin
      {Lock Group}
      if ALock then Group.AcquireLock;

      {Return Result}
      Result:=Group;
     end;

    {Unlock Previous}
    if AUnlock then APrevious.ReleaseLock;
   end;
 finally
  FGroups.ReaderUnlock;
 end;
end;

{==============================================================================}

function TIGMPProtocol.AddGroup(ATransport:TIGMPProtocolTransport;const AAddress:TInAddr;const AHardware:THardwareAddress;ALock:Boolean):TIGMPGroup;
{Add an IP address and hardware address to the group cache}
{Transport: The transport for the group to add}
{Address: The IP address of the entry to add}
{Hardware: The hardware address of the entry to add}
{Lock: If True then lock the added entry before returning}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Transport lock}
begin
 {}
 Result:=nil;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: AddGroup');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Hardware = ' + HardwareAddressToString(AHardware));
 {$ENDIF}

 {Check Transport}
 if ATransport = nil then Exit;

 {Create Group}
 Result:=TIGMPGroup.Create;
 Result.Count:=1;
 Result.QueryTime:=0;
 Result.ReportTime:=0;
 Result.TransmitCount:=0;
 Result.Address:=AAddress;
 Result.Hardware:=AHardware;
 Result.Transport:=ATransport.Transport;

 {Acquire Lock}
 FGroups.WriterLock; {Acquire as Writer}
 try
  {Add Group}
  if FGroups.Add(Result) then
   begin
    {Convert Lock}
    FGroups.WriterConvert;

    {Lock Group}
    if ALock then Result.AcquireLock;
   end
  else
   begin
    {Convert Lock}
    FGroups.WriterConvert;

    {Free Group}
    Result.Free;
    Result:=nil;
   end;
 finally
  FGroups.ReaderUnlock; {Converted to Reader}
 end;
end;

{==============================================================================}

function TIGMPProtocol.RemoveGroup(ATransport:TIGMPProtocolTransport;const AAddress:TInAddr):Boolean;
{Remove an IP address from the group cache}
{Transport: The transport for the group to remove}
{Address: The IP address to remove}

{Note: Addresses are passed in Host order}
{Note: Caller must hold the Transport lock}
var
 Group:TIGMPGroup;
begin
 {}
 FGroups.ReaderLock;
 try
  Result:=False;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: RemoveGroup');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then Exit;

  {Get Group}
  Group:=TIGMPGroup(FGroups.First);
  while Group <> nil do
   begin
    {Check Transport}
    if Group.Transport = ATransport.Transport then
     begin
      {Check Transport Family}
      case Group.Transport.Family of
       AF_INET:begin
         {Check Address}
         if TIPTransport(Group.Transport).CompareAddress(Group.Address,AAddress) then
          begin
           {Lock Group}
           Group.AcquireLock;

           {Convert Lock}
           if FGroups.ReaderConvert then
            begin
             {Remove Group}
             Result:=FGroups.Remove(Group);

             {Convert Lock}
             FGroups.WriterConvert;

             {Unlock Group}
             Group.ReleaseLock;

             {Free Group}
             Group.Free;
            end;

           Exit;
          end;
        end;
      end;
     end;

    {Get Next}
    Group:=TIGMPGroup(Group.Next);
   end;
 finally
  FGroups.ReaderUnlock;
 end;
end;

{==============================================================================}

procedure TIGMPProtocol.FlushGroups(All:Boolean);
{Flush groups from the group cache}
{All: If True flush all groups, otherwise flush expired groups}
var
 Group:TIGMPGroup;
 Current:TIGMPGroup;
begin
 {}
 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: FlushGroups');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Group}
 Group:=TIGMPGroup(GetGroupByNext(nil,True,False));
 while Group <> nil do
  begin
   {Get Next}
   Current:=Group;
   Group:=TIGMPGroup(GetGroupByNext(Current,True,False));

   {Check Active}
   if (Group.Active = False) or (All) then
    begin
     {Acquire Lock}
     FGroups.WriterLock;

     {Remove Group}
     FGroups.Remove(Current);

     {Release Lock}
     FGroups.WriterUnlock;

     {Unlock Group}
     Current.ReleaseLock;

     {Free Group}
     Current.Free;
     Current:=nil;
    end;

   {Unlock Group}
   if Current <> nil then Current.ReleaseLock;
  end;
end;

{==============================================================================}

function TIGMPProtocol.ProcessGroups:Boolean;
{Process periodic tasks for IGMP groups}
var
 Group:TIGMPGroup;
 Transport:TIGMPProtocolTransport;
begin
 {}
 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: ProcessGroups');
 {$ENDIF}

 Result:=False;

 {Get Group}
 Group:=TIGMPGroup(GetGroupByNext(nil,True,False));
 while Group <> nil do
  begin
   {Check Status}
   if (Group.Active) and ((Group.QueryTime <> 0) or (Group.ReportTime <> 0)) then
    begin
     {Check Transport Family}
     case Group.Transport.Family of
      AF_INET:begin
        {Get Transport}
        Transport:=TIGMPProtocolTransport(GetTransportByTransport(Group.Transport,True,NETWORK_LOCK_READ));
        if Transport <> nil then
         begin
          {Check Query Time}
          if (Group.QueryTime <> 0) and (Group.QueryTime < GetTickCount64) then
           begin
            {Lock Socket}
            Transport.Socket.ReaderLock;

            {Send Query}
            //Result:=SendIGMP2Query(Transport.Socket,MULTICAST_HOST //To Do

            {Unlock Socket}
            Transport.Socket.ReaderUnlock;

            Group.DecrementTransmitCount;

            Group.QueryTime:=GetTickCount64 + IGMP_QUERY_TIMEOUT;
            if Group.TransmitCount < 1 then
             begin
              {Remove Group}
              Group.Count:=0;
              Group.QueryTime:=0;
              Group.TransmitCount:=0;
             end;
           end;

          {Check Report Time}
          if (Group.ReportTime <> 0) and (Group.ReportTime < GetTickCount64) then
           begin
            {Lock Socket}
            Transport.Socket.ReaderLock;

            {Send Report}
            //Result:=SendIGMP2Report(Transport.Socket,MUTLICAST_ROUTER //To Do

            {Unlock Socket}
            Transport.Socket.ReaderUnlock;

            Group.DecrementTransmitCount;

            Group.ReportTime:=GetTickCount64 + IGMP_REPORT_TIMEOUT;
            if Group.TransmitCount < 1 then
             begin
              {Stop Reporting}
              Group.ReportTime:=0;
              Group.TransmitCount:=0;
             end;
           end;

          {Unlock Transport}
          Transport.ReaderUnlock;
         end;
       end;
     end;
    end;

   {Get Next}
   Group:=TIGMPGroup(GetGroupByNext(Group,True,True));
  end;
end;

{==============================================================================}

function TIGMPProtocol.SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer;
{Source is the working set to check, Dest is the set passed to Select}
var
 Count:Integer;
 Socket:TIGMPSocket;
begin
 {}
 Result:=0;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SelectCheck');
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
      Socket:=TIGMPSocket(ASource.fd_array[Count]);

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
      Socket:=TIGMPSocket(ASource.fd_array[Count]);

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

function TIGMPProtocol.SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer;
{Socket is the single socket to check, Code is the type of check, Timeout is how long to wait}
var
 StartTime:Int64;
 Socket:TIGMPSocket;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SelectWait');
 {$ENDIF}

 {Get Socket}
 Socket:=TIGMPSocket(ASocket);

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

function TIGMPProtocol.SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
{Send a Packet by adding the Protocol Header and other details to the Data}
{Socket: The socket to use for sending the packet}
{Source: The source address of the packet (Host Order)}
{Dest: The destination address of the packet (Host Order)}
{SourcePort: The source port of the packet (Host Order)}
{DestPort: The destination port of the packet (Host Order)}
{Packet: The packet data to send}
{Size: The size of the packet data in bytes}
{Flags: Any protocol specific flags for sending}

{Note: For IGMP the Data is the Header so the packet is not changed}
{Note: Addresses are passed in Host order}
{Note: Caller must hold the Socket lock}
var
 Transport:TIGMPProtocolTransport;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SendPacket');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check Socket}
 if ASocket = nil then Exit;

 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {$IFDEF IGMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Dest = ' + InAddrToString(InAddrToNetwork(PInAddr(ADest)^)));
    {$ENDIF}

    {Get Transport}
    Transport:=TIGMPProtocolTransport(GetTransportByTransport(ASocket.Transport,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;

    {Send the Packet}
    Result:=TIPTransport(ASocket.Transport).SendPacket(ASocket,ASource,ADest,APacket,ASize,AFlags);

    {Unlock Transport}
    Transport.ReaderUnlock;
   end;
 end;
end;

{==============================================================================}

function TIGMPProtocol.Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket;
{BSD compatible Accept}
{Socket: The socket to accept from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is passed in Network order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=TProtocolSocket(INVALID_SOCKET);

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Accept');
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

function TIGMPProtocol.Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Bind}
{Sets the LocalAddress for future Sends and Receives, Address can be specified as INADDR_ANY which allows Listening or auto assignment}
{Socket: The socket to bind}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is passed in Network order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Bind');
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

function TIGMPProtocol.CloseSocket(ASocket:TProtocolSocket):Integer;
{BSD compatible Close Socket}
{Closes and removes the socket, does not perform Linger}
{Socket: The socket to close}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: CloseSocket');
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

function TIGMPProtocol.Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Connect}
{Sets the RemoteAddress of future Sends and Receives, if Bind has not been called then
 the LocalAddress will be set appropriately as well based on the route to the RemoteAddress}
{Socket: The socket to connect}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is passed in Network order}
{Note: Caller must hold the Socket lock}
var
 Route:TRouteEntry;
 Address:TAddressEntry;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Connect');
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

function TIGMPProtocol.IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer;
{BSD compatible IO Control Socket}
{Socket: The socket to control}
{Cmd: The socket command}
{Arg: The command argument}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: IoctlSocket');
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

function TIGMPProtocol.GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Peer Name (Remote)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: GetPeerName');
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

function TIGMPProtocol.GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Sock Name (Local)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: GetSockName');
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

function TIGMPProtocol.GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
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

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: GetSockOpt');
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

function TIGMPProtocol.Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer;
{BSD compatible Listen}
{Socket: The socket to listen on}
{Backlog: Queue depth for accepted connections}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Listen');
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

function TIGMPProtocol.Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Recv');
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
   while TIGMPSocket(ASocket).RecvData.GetCount = 0 do
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
   Size:=TIGMPSocket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;

   {Read Data}
   if TIGMPSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,nil,AFlags) then
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

function TIGMPProtocol.RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer;
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
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: RecvFrom');
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
   while TIGMPSocket(ASocket).RecvData.GetCount = 0 do
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
   Size:=TIGMPSocket(ASocket).RecvData.GetNext;
   if Size > ALength then
    begin
     NetworkSetLastError(WSAEMSGSIZE);
     Size:=ALength;
    end;

   {Get Address}
   AFromAddr.sin_family:=ASocket.Family;
   AFromAddr.sin_port:=WordNtoBE(IPPORT_ANY);

   {Read Data}
   if TIGMPSocket(ASocket).RecvData.ReadBuffer(ABuffer,Size,@AFromAddr.sin_addr,AFlags) then
    begin
     {Get Address}
     AFromAddr.sin_addr:=InAddrToNetwork(AFromAddr.sin_addr);

     {Get Address Length}
     AFromLength:=SizeOf(TSockAddr);

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

function TIGMPProtocol.Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
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

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Send');
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

function TIGMPProtocol.SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer;
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
 Packet:TPacketFragment;
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SendTo');
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
   if ASocket.Family <> AToAddr.sin_family then Exit;

   {Check Address Family}
   case ASocket.Family of
    AF_INET:begin
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
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TIGMPProtocol.SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
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

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: SetSockOpt');
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
           TIGMPSocket(ASocket).RecvData.Size:=PInteger(AOptValue)^;
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

function TIGMPProtocol.Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer;
{BSD compatible Shutdown}
{Socket: The socket to shutdown}
{How: The direction to shutdown the socket}

{Note: Shutdown does not result in CloseSocket so Closed must not get set}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Shutdown');
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

function TIGMPProtocol.Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket;
{BSD compatible Socket (Create a new socket)}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}
var
 Transport:TIGMPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=TProtocolSocket(INVALID_SOCKET);

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: Socket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Check Socket Type}
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if AStruct <> SOCK_RAW then Exit;

  {Check Address Family}
  NetworkSetLastError(WSAEAFNOSUPPORT);
  if (AFamily = AF_UNSPEC) and (AProtocol <> IPPROTO_IP) then AFamily:=AF_INET;

  {Check Protocol}
  NetworkSetLastError(WSAEPROTOTYPE);
  if (AProtocol <> IPPROTO_IGMP) and (AProtocol <> IPPROTO_IP) then Exit;

  {Get Transport}
  Transport:=TIGMPProtocolTransport(GetTransportByFamily(AFamily,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;

  {Create Socket}
  Result:=TIGMPSocket.Create(Self,Transport.Transport);

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

function TIGMPProtocol.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this protocol}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TIGMPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: AddTransport');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then Exit;

  {Get Transport}
  Transport:=TIGMPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add IGMP Protocol}
       Handle:=TIPTransport(ATransport).AddProtocol(IPPROTO_IGMP,PacketHandler,ControlHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TIGMPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_IGMP;
         Transport.Transport:=ATransport;

         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);

          {Add Control Socket}
          Transport.Socket:=TIGMPSocket.Create(Self,ATransport);
          {FSockets.Add(Transport.Socket);} {Dont add this one to the list}

          {Add Proto Entry}
          TIPTransport(ATransport).AddProto(IGMP_PROTOCOL_NAME,IPPROTO_IGMP,False);

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

function TIGMPProtocol.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this protocol}
{Transport: The transport to remove}
var
 Transport:TIGMPProtocolTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: RemoveTransport');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then Exit;

  {Get Transport}
  Transport:=TIGMPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;

  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove IGMP Protocol}
     if TIPTransport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
      begin
       {Remove Proto Entry}
       TIPTransport(ATransport).RemoveProto(IGMP_PROTOCOL_NAME);

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

function TIGMPProtocol.FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket;
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
 Socket:TIGMPSocket;
begin
 {}
 FSockets.ReaderLock;
 try
  Result:=nil;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: FindSocket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}

  {Get Socket}
  Socket:=TIGMPSocket(FSockets.First);
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
    Socket:=TIGMPSocket(Socket.Next);
   end;
 finally
  FSockets.ReaderUnlock;
 end;
end;

{==============================================================================}

procedure TIGMPProtocol.FlushSockets(All:Boolean);
{Flush sockets from the socket cache}
{All: If True flush all sockets, otherwise flush expired sockets}
var
 CurrentTime:Int64;
 Socket:TIGMPSocket;
 Current:TIGMPSocket;
begin
 {}
 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: FlushSockets');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Tick Count}
 CurrentTime:=GetTickCount64;

 {Get Socket}
 Socket:=TIGMPSocket(GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
 while Socket <> nil do
  begin
   {Get Next}
   Current:=Socket;
   Socket:=TIGMPSocket(GetSocketByNext(Current,True,False,NETWORK_LOCK_READ));

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

function TIGMPProtocol.StartProtocol:Boolean;
{Start this protocol ready for sending and receiving}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: StartProtocol');
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

function TIGMPProtocol.StopProtocol:Boolean;
{Stop this protocol ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: StopProtocol');
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

function TIGMPProtocol.ProcessProtocol:Boolean;
{Process periodic tasks for this protocol}
begin
 {}
 {Close old Sockets}
 FlushSockets(False);

 {Process Groups}
 ProcessGroups;

 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TIGMPGroup}
constructor TIGMPGroup.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;

 FCount:=0;
 FQueryTime:=0;
 FReportTime:=0;
 LongWord(FAddress.S_addr):=INADDR_ANY;
 FillChar(FHardware,SizeOf(THardwareAddress),0);
 FTransport:=nil;
end;

{==============================================================================}

destructor TIGMPGroup.Destroy;
begin
 {}
 AcquireLock;
 try
  FTransport:=nil;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock}
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

procedure TIGMPGroup.SetCount(ACount:Word);
begin
 {}
 if not AcquireLock then Exit;

 FCount:=ACount;

 ReleaseLock;
end;

{==============================================================================}

function TIGMPGroup.GetActive:Boolean;
begin
 {}
 Result:=(FCount > 0);
end;

{==============================================================================}

procedure TIGMPGroup.SetQueryTime(const AQueryTime:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FQueryTime:=AQueryTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.SetReportTime(const AReportTime:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FReportTime:=AReportTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.SetTransmitCount(ATransmitCount:Word);
begin
 {}
 if not AcquireLock then Exit;

 FTransmitCount:=ATransmitCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.SetAddress(const AAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.SetHardware(const AHardware:THardwareAddress);
begin
 {}
 if not AcquireLock then Exit;

 FHardware:=AHardware;

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.SetTransport(ATransport:TNetworkTransport);
begin
 {}
 if not AcquireLock then Exit;

 FTransport:=ATransport;

 ReleaseLock;
end;

{==============================================================================}

function TIGMPGroup.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TIGMPGroup.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TIGMPGroup.IncrementCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.DecrementCount;
begin
 {}
 if not AcquireLock then Exit;

 Dec(FCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.IncrementTransmitCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FTransmitCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TIGMPGroup.DecrementTransmitCount;
begin
 {}
 if not AcquireLock then Exit;

 Dec(FTransmitCount);

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TIGMPSocket}
constructor TIGMPSocket.Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
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
 FRecvData:=TIGMPBuffer.Create(Self);
 FRecvData.Size:=IGMP_BUFFER_SIZE;

 {Set Socket Defaults}
 FSocketOptions.SendBuffer:=IGMP_BUFFER_SIZE;
 FSocketOptions.RecvBuffer:=IGMP_BUFFER_SIZE;
 FSocketOptions.SendTimeout:=IGMP_TIMEOUT;
 FSocketOptions.RecvTimeout:=IGMP_TIMEOUT;
end;

{==============================================================================}

destructor TIGMPSocket.Destroy;
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

function TIGMPSocket.IoCtl(ACommand:DWORD;var AArgument:u_long):Integer;
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Socket: IoCtl');
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

function TIGMPSocket.IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this IGMP Socket is Connected to the Host specified by RemoteAddress}
{A connected Socket will have a Bound LocalAddress and a Connected RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Socket: IsConnected');
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

function TIGMPSocket.IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this IGMP Socket is Listening on the LocalAddress specified}
{A listening Socket may or may not have a Bound LocalAddress and will have a default (INADDR_ANY) RemoteAddress}
var
 Route:TRouteEntry;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Socket: IsListening');
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
{TIGMPBuffer}
constructor TIGMPBuffer.Create(ASocket:TTransportSocket);
begin
 {}
 inherited Create(ASocket);

 FOffset:=SizeOf(TIGMPPacket); {IGMP_PACKET_SIZE}

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

destructor TIGMPBuffer.Destroy;
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

function TIGMPBuffer.AddPacket(ASize:Integer):Boolean;
{Adds a new Packet as the Last packet in the Buffer}
var
 Packet:PIGMPPacket;
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: AddPacket');
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

function TIGMPBuffer.RemovePacket:Boolean;
{Removes the First packet from the Buffer}
var
 Packet:PIGMPPacket;
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: RemovePacket');
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

procedure TIGMPBuffer.FlushPackets;
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

procedure TIGMPBuffer.SetSize(ASize:LongWord);
{Setting the Size clears any current Packets}
begin
 {}
 if not AcquireLock then Exit;
 try
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: SetSize: Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check Size}
  if ASize = 0 then Exit;

  {Get Size}
  ASize:=Max(ASize,IGMP_BUFFER_SIZE);

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

function TIGMPBuffer.GetNext:Integer;
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

function TIGMPBuffer.GetCount:LongWord;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TIGMPBuffer.ReadBuffer(var ABuffer;var ASize:Integer;ARemoteAddress:Pointer;AFlags:Integer):Boolean;
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
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
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
      {$IFDEF IGMP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: Short Read');
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
    {$IFDEF IGMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: Double Read');
    {$ENDIF}

    {Read the First Block of the Data}
    BlockSize:=(PtrUInt(FEnd) - PtrUInt(ReadNext));
    if BufferSize < BlockSize then
     begin
      {$IFDEF IGMP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: Short First Read');
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
        {$IFDEF IGMP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: Short Second Read');
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
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: ReadNext = ' + IntToStr(PtrUInt(ReadNext)) + ' ReadSize = ' + IntToStr(ReadSize));
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

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: ReadBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
  {$ENDIF}

  {Return Result}
  Result:=True;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TIGMPBuffer.WriteBuffer(var ABuffer;ASize:Integer;ARemoteAddress:Pointer):Boolean;
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
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: WriteBuffer: FStart = ' + PtrToHex(FStart) + ' FEnd = ' + PtrToHex(FEnd));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
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
    {$IFDEF IGMP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: WriteBuffer: Double Write');
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
  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: WriteBuffer: WriteNext = ' + IntToStr(PtrUInt(WriteNext)) + ' WriteSize = ' + IntToStr(WriteSize));
  {$ENDIF}

  {Set the RemoteAddress}
  if ARemoteAddress <> nil then
   begin
    System.Move(ARemoteAddress^,Pointer(PtrUInt(FLast) + FOffset)^,FLength);
   end;

  {Update the Next Write}
  FWrite:=WriteNext;

  {$IFDEF IGMP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP Buffer: WriteBuffer: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed) + ' Count = ' + IntToStr(FCount));
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
procedure IGMPInit;
begin
 {}
 {Check Initialized}
 if IGMPInitialized then Exit;

 {Create IGMP Protocol}
 if NetworkSettings.GetBooleanDefault('IGMP_PROTOCOL_ENABLED',IGMP_PROTOCOL_ENABLED) then
  begin
   TIGMPProtocol.Create(ProtocolManager,IGMP_PROTOCOL_NAME);
  end;

 IGMPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{IGMP Functions}
function CheckIGMP(AFamily:Word;ABuffer:Pointer):Boolean;
{Verify that the packet is a valid IGMP packet}
{Buffer: The complete packet including Transport header}
var
 Length:Word;
 IGMP:PIGMPHeader;
begin
 {}
 Result:=False;

 {$IFDEF IGMP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP: CheckIGMP');
 {$ENDIF}

 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    IGMP:=PIGMPHeader(PtrUInt(ABuffer) + GetIGMPHeaderOffset(AF_INET,ABuffer));

    {Check Header Length}
    Length:=GetIGMPHeaderLength(AF_INET,ABuffer);
    if Length >= IGMP_HEADER_SIZE then
     begin
      {Validate the Header Checksum}
      {$IFDEF IGMP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Original Checksum   = ' + IntToHex(WordBEtoN(IGMP.Checksum),4));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'IGMP:  Calculated Checksum = ' + IntToHex(WordBEtoN(ChecksumIGMP(AF_INET,IGMP,0,Length)),4));
      {$ENDIF}
      if IGMP.Checksum = $FFFF then IGMP.Checksum:=0; {Allow for all 1s case}
      if IGMP.Checksum = ChecksumIGMP(AF_INET,IGMP,0,Length) then
       begin
        Result:=True;
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function GetIGMPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Start of IGMP Header (Length of IP Header)}
    Result:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
   end;
 end;
end;

{==============================================================================}

function GetIGMPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Start of the IGMP Header (Length of IP Header)}
    Offset:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;

    {Return Size of IGMP Header based on IGMPType Field}
    case PIGMPHeader(PtrUInt(ABuffer) + Offset).IGMPType of
     IGMP_QUERY,IGMP_REPORTV1,IGMP_REPORTV2,IGMP_LEAVE:begin
       Result:=SizeOf(TIGMPHeader);
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function GetIGMPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Start of ICMP Data (If Any) (IP Header + IGMP Header)}
    Result:=GetIGMPHeaderOffset(AFamily,ABuffer) + GetIGMPHeaderLength(AFamily,ABuffer);
   end;
 end;
end;

{==============================================================================}

function GetIGMPDataLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Size of IGMP Data (If Any) based on IP Total Length Field - IP Header - IGMP Header}
    Result:=WordBEtoN(PIPHeader(ABuffer).TotalLength);
    Result:=(Result - GetIGMPHeaderOffset(AFamily,ABuffer)) - GetIGMPHeaderLength(AFamily,ABuffer);
   end;
 end;
end;

{==============================================================================}

function ChecksumIGMP(AFamily:Word;ABuffer:Pointer;AOffset,ALength:Word):Word;
{Checksum the IGMP Header on Send / Validate the Checksum on Receive}
{Note: Checksum is calculated on the whole packet (Header and Data)}
var
 Original:Word;
 IGMP:PIGMPHeader;
begin
 {}
 Result:=0;

 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    IGMP:=PIGMPHeader(PtrUInt(ABuffer) + AOffset);

    {Save Checksum}
    Original:=IGMP.Checksum;
    IGMP.Checksum:=0;

    {Calculate 1s Compliment Checksum on IGMP Header and Data}
    Result:=GetChecksum(ABuffer,AOffset,ALength);

    {Restore Checksum}
    IGMP.Checksum:=Original;
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{IGMP Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 IGMPInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
