{
Ultibo ARP (Address Resolution Protocol) unit.

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

 General

  https://en.wikipedia.org/wiki/Address_Resolution_Protocol

Address Resolution Protocol
===========================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ARP;

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
  Ultibo,
  UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {ARP specific constants}
 ARP_TRANSPORT_NAME = 'ARP';
 RARP_TRANSPORT_NAME = 'RARP';

 {ARP and RARP Constants}
 MIN_ARP_PACKET = 46;  {Not Counting Adapter Header}
 MAX_ARP_PACKET = 46;  {Not Counting Adapter Header}

 ARP_TIMEOUT = 1000;   {We wait for 1 second approx for reply}
 ARP_RETRIES = 4;      {We try the request 4 times}

 RARP_TIMEOUT = 4000;  {We wait for 4 seconds for a RARP reply}
 RARP_RETRIES = 4;     {We try the request 4 times}

 ARP_HEADER_SIZE  = 46;  {SizeOf(TARPHeader);}
 RARP_HEADER_SIZE = 46;  {SizeOf(TRARPHeader);}

 {ARP and RARP Messages}
 ARP_REQUEST   = $0001;    {ARP/RARP op codes, Request}
 ARP_REPLY     = $0002;    {                     Reply}
 RARP_REQUEST  = $0003;
 RARP_REPLY    = $0004;
 INARP_REQUEST = $0008;    {Inverse ARP see RFC 1293}
 INARP_REPLY   = $0009;

{==============================================================================}
type
 {ARP specific types}
 PARPHeader = ^TARPHeader;
 TARPHeader = packed record       {46 Bytes}
  HardwareType:Word;              {Hardware address space (Network Order)}
  ProtocolType:Word;              {Protocol address space (Network Order)}
  HardwareLength:Byte;            {Byte length of hardware address}
  ProtocolLength:Byte;            {Byte length of each protocol address}
  Opcode:Word;                    {Op code (eg ARP_REQUEST or ARP_REPLY) (Network Order)}
  SourceHardware:THardwareAddress;{Source hardware address (of sender)}
  SourceIP:TInAddr;               {Source protocol address (of sender) (Network Order)}
  TargetHardware:THardwareAddress;{Target hardware address (if known)}
  TargetIP:TInAddr;               {Target protocol address (Network Order)}
  Reserved:array[0..17] of Word;
 end;

 PRARPHeader = ^TRARPHeader;
 TRARPHeader = TARPHeader;    {46 Bytes}

{==============================================================================}
type
 {ARP specific classes}
 TARPTransportAdapter = class(TTransportAdapter)
   constructor Create;
  private
   {Internal Variables}
   UseCount:Integer;
  public
   {Status Variables}

 end;

 TARPAddressEntry = class;
 TARPTransport = class(TNetworkTransport)
   constructor Create(AManager:TTransportManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FAddresses:TNetworkList;
   FAddressAdd:TEventHandle;
   FAddressRemove:TEventHandle;

   {Status Variables}

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;

   function SendARPRequest(AAdapter:TARPTransportAdapter;ASource:TARPAddressEntry;const ATarget:TInAddr):Boolean;
   function SendARPReply(AAdapter:TARPTransportAdapter;ASource,ATarget:TARPAddressEntry):Boolean;

   function SendINARPRequest(AAdapter:TARPTransportAdapter;ASource:TARPAddressEntry;const ATarget:THardwareAddress):Boolean;
   function SendINARPReply(AAdapter:TARPTransportAdapter;ASource,ATarget:TARPAddressEntry):Boolean;
  protected
   {Inherited Methods}

   {Internal Methods}

  public
   {Public Methods}
   function AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean; override;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; override;

   function StartTransport:Boolean; override;
   function StopTransport:Boolean; override;
   function ProcessTransport:Boolean; override;

   function BindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   function UnbindTransport(AAdapter:TNetworkAdapter):Boolean; override;

   {ARP Methods}
   function GetAddressByAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TARPAddressEntry;
   function GetAddressByHardware(const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TARPAddressEntry;
   function GetAddressByNext(APrevious:TARPAddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TARPAddressEntry;
   function AddAddress(const AAddress:TInAddr;const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;AType:Word;ALock:Boolean;AState:LongWord):TARPAddressEntry;
   function RemoveAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter):Boolean;
   procedure FlushAddresses(All:Boolean);

   function LoadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr;const AHardware:THardwareAddress;AType:Word):Boolean;
   function UnloadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr):Boolean;

   function ResolveAddress(AAdapter:TNetworkAdapter;const ASource,AAddress:TInAddr;var AHardware:THardwareAddress):Boolean;
   function ResolveHardware(AAdapter:TNetworkAdapter;const ASource:TInAddr;const AHardware:THardwareAddress;var AAddress:TInAddr):Boolean;

   function ConfirmAddress(AAdapter:TNetworkAdapter;const ASource,AAddress:TInAddr):Boolean;
   function AdvertiseAddress(AAdapter:TNetworkAdapter;const ASource:TInAddr):Boolean;

   function CompareDefault(const AAddress:TInAddr):Boolean;
   function CompareAddress(const AAddress1,AAddress2:TInAddr):Boolean;
 end;

 TARPAddressEntry = class(TAddressEntry)
   constructor Create;
  private
   {Internal Variables}
   FAddress:TInAddr;
   FProtocolType:Word;
   FHardware:THardwareAddress;

   {Internal Methods}
   procedure SetAddress(const AAddress:TInAddr);
   procedure SetProtocolType(AProtocolType:Word);
   procedure SetHardware(const AHardware:THardwareAddress);
  public
   {Status Variables}
   property Address:TInAddr read FAddress write SetAddress;
   property ProtocolType:Word read FProtocolType write SetProtocolType;
   property Hardware:THardwareAddress read FHardware write SetHardware;
 end;

 TRARPTransportAdapter = class(TTransportAdapter)
   constructor Create;
  private
   {Internal Variables}
   UseCount:Integer;
  public
   {Status Variables}

 end;

 TRARPAddressEntry = class;
 TRARPTransport = class(TNetworkTransport)
   constructor Create(AManager:TTransportManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FAddresses:TNetworkList;
   FAddressAdd:TEventHandle;
   FAddressRemove:TEventHandle;

   {Status Variables}

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;

   function SendRARPRequest(AAdapter:TRARPTransportAdapter):Boolean;
   function SendRARPReply(AAdapter:TRARPTransportAdapter;ASource,ATarget:TRARPAddressEntry):Boolean;
  protected
   {Inherited Methods}

   {Internal Methods}

  public
   {Public Methods}
   function AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean; override;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; override;

   function StartTransport:Boolean; override;
   function StopTransport:Boolean; override;
   function ProcessTransport:Boolean; override;

   function BindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   function UnbindTransport(AAdapter:TNetworkAdapter):Boolean; override;

   {RARP Methods}
   function GetAddressByAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TRARPAddressEntry;
   function GetAddressByHardware(const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TRARPAddressEntry;
   function GetAddressByNext(APrevious:TRARPAddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TRARPAddressEntry;
   function AddAddress(const AAddress:TInAddr;const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;AType:Word;ALock:Boolean;AState:LongWord):TRARPAddressEntry;
   function RemoveAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter):Boolean;
   procedure FlushAddresses(All:Boolean);

   function LoadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr;const AHardware:THardwareAddress;AType:Word):Boolean;
   function UnloadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr):Boolean;

   function ResolveHardware(AAdapter:TNetworkAdapter;var AAddress:TInAddr):Boolean;

   function CompareDefault(const AAddress:TInAddr):Boolean;
   function CompareAddress(const AAddress1,AAddress2:TInAddr):Boolean;
 end;

 TRARPAddressEntry = class(TAddressEntry)
   constructor Create;
  private
   {Internal Variables}
   FAddress:TInAddr;
   FProtocolType:Word;
   FHardware:THardwareAddress;

   {Internal Methods}
   procedure SetAddress(const AAddress:TInAddr);
   procedure SetProtocolType(AProtocolType:Word);
   procedure SetHardware(const AHardware:THardwareAddress);
  public
   {Status Variables}
   property Address:TInAddr read FAddress write SetAddress;
   property ProtocolType:Word read FProtocolType write SetProtocolType;
   property Hardware:THardwareAddress read FHardware write SetHardware;
 end;

{==============================================================================}
{var}
 {ARP specific variables}

{==============================================================================}
{Initialization Functions}
procedure ARPInit;

{==============================================================================}
{ARP Functions}
function CheckARP(ABuffer:Pointer):Boolean;
function CheckRARP(ABuffer:Pointer):Boolean;

{==============================================================================}
{ARP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {ARP specific variables}
 ARPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TARPTransportAdapter}
constructor TARPTransportAdapter.Create;
begin
 {}
 inherited Create;
 UseCount:=0;
end;

{==============================================================================}
{==============================================================================}
{TARPTransport}
constructor TARPTransport.Create(AManager:TTransportManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FFamily:=AF_UNSPEC;
 FPacketType:=PACKET_TYPE_ARP;

 FAddresses:=TNetworkList.Create;
 FAddressAdd:=EventCreate(True,False);
 FAddressRemove:=EventCreate(True,False);
end;

{==============================================================================}

destructor TARPTransport.Destroy;
begin
 {}
 WriterLock;
 try
  EventDestroy(FAddressRemove);
  EventDestroy(FAddressAdd);
  FAddresses.Free;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TARPTransport.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by an Adapter}
{Handle: The Handle of the Transport Adapter the packet was received from}
{Source: The source hardware address of the received packet (Set by Adapter)}
{Dest: The destination hardware address of the received packet (Set by Adapter)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}
var
 ARP:PARPHeader;
 Local:TARPAddressEntry;
 Remote:TARPAddressEntry;
 Adapter:TARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Get Adapter}
 Adapter:=TARPTransportAdapter(GetAdapterByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  PacketType = ' + PacketTypeToString(Adapter.PacketType));
  {$ENDIF}

  {Check Packet Type}
  case Adapter.PacketType of
   PACKET_TYPE_ARP:begin
     {Check ARP Packet}
     if CheckARP(APacket) then
      begin
       {Get Header}
       ARP:=PARPHeader(APacket);

       {Set the Addresses to Host order}
       ARP.TargetIP:=InAddrToHost(ARP.TargetIP);
       ARP.SourceIP:=InAddrToHost(ARP.SourceIP);

       {Check Opcode}
       case WordBEtoN(ARP.Opcode) of
        ARP_REQUEST:begin
          {Check Protocol Type}
          case WordBEtoN(ARP.ProtocolType) of
           PACKET_TYPE_IP:begin
             {Get Local Address}
             Local:=GetAddressByAddress(ARP.TargetIP,Adapter.Adapter,True,NETWORK_LOCK_READ);
             if Local <> nil then
              begin
               {Check Address Type}
               if Local.AddressType = ADDRESS_TYPE_LOCAL then
                begin
                 {Get Remote Address}
                 Remote:=GetAddressByAddress(ARP.SourceIP,Adapter.Adapter,True,NETWORK_LOCK_READ);
                 if Remote = nil then
                  begin
                   {Add Remote Address}
                   Remote:=AddAddress(ARP.SourceIP,ARP.SourceHardware,Adapter.Adapter,ADDRESS_TYPE_DYNAMIC,True,NETWORK_LOCK_READ);
                  end
                 else
                  begin
                   {Update Remote Address}
                   Remote.Hardware:=ARP.SourceHardware;

                   {Update timeout on the Remote}
                   if Remote.AddressType = ADDRESS_TYPE_DYNAMIC then
                    begin
                     Remote.AddressTime:=GetTickCount64;
                    end;
                  end;

                 {Check Remote Address}
                 if Remote <> nil then
                  begin
                   {Safety Check for INADDR_ANY}
                   if not CompareDefault(Local.Address) then
                    begin
                     {Send ARP Reply}
                     Result:=SendARPReply(Adapter,Local,Remote);
                   end;

                   {Unlock Remote}
                   Remote.ReaderUnlock;
                  end;
                end;

               {Unlock Local}
               Local.ReaderUnlock;
              end;
            end;
          end;
         end;
        ARP_REPLY:begin
          {Check Protocol Type}
          case WordBEtoN(ARP.ProtocolType) of
           PACKET_TYPE_IP:begin
             {Get Local Address}
             Local:=GetAddressByAddress(ARP.TargetIP,Adapter.Adapter,True,NETWORK_LOCK_READ);
             if Local <> nil then
              begin
               {Check Address Type}
               if (Local.AddressType = ADDRESS_TYPE_LOCAL) or (Local.AddressType = ADDRESS_TYPE_BROADCAST) then
                begin
                 {Get Remote Address}
                 Remote:=GetAddressByAddress(ARP.SourceIP,Adapter.Adapter,True,NETWORK_LOCK_READ);
                 if Remote = nil then
                  begin
                   {Add Remote Address}
                   AddAddress(ARP.SourceIP,ARP.SourceHardware,Adapter.Adapter,ADDRESS_TYPE_DYNAMIC,False,NETWORK_LOCK_NONE);
                  end
                 else
                  begin
                   {Update Remote Address}
                   Remote.Hardware:=ARP.SourceHardware;

                   {Update timeout on the Remote}
                   if Remote.AddressType = ADDRESS_TYPE_DYNAMIC then
                    begin
                     Remote.AddressTime:=GetTickCount64;
                    end;

                   {Unlock Remote}
                   Remote.ReaderUnlock;
                  end;

                 {Return Result}
                 Result:=True;
                end;

               {Unlock Local}
               Local.ReaderUnlock;
              end;
            end;
          end;
         end;
        INARP_REQUEST:begin
          {Check Protocol Type}
          case WordBEtoN(ARP.ProtocolType) of
           PACKET_TYPE_IP:begin
             {Get Local Address}
             Local:=GetAddressByHardware(ARP.TargetHardware,Adapter.Adapter,True,NETWORK_LOCK_READ);
             if Local <> nil then
              begin
               {Check Address Type}
               if Local.AddressType = ADDRESS_TYPE_LOCAL then
                begin
                 {Get Remote Address}
                 Remote:=GetAddressByAddress(ARP.SourceIP,Adapter.Adapter,True,NETWORK_LOCK_READ);
                 if Remote = nil then
                  begin
                   {Add Remote Address}
                   Remote:=AddAddress(ARP.SourceIP,ARP.SourceHardware,Adapter.Adapter,ADDRESS_TYPE_DYNAMIC,True,NETWORK_LOCK_READ);
                  end
                 else
                  begin
                   {Update Remote Address}
                   Remote.Hardware:=ARP.SourceHardware;

                   {Update timeout on the Remote}
                   if Remote.AddressType = ADDRESS_TYPE_DYNAMIC then
                    begin
                     Remote.AddressTime:=GetTickCount64;
                    end;
                  end;

                 {Check Remote Address}
                 if Remote <> nil then
                  begin
                   {Safety Check for INADDR_ANY}
                   if not CompareDefault(Local.Address) then
                    begin
                     {Send INARP Reply}
                     Result:=SendINARPReply(Adapter,Local,Remote);
                    end;

                   {Unlock Remote}
                   Remote.ReaderUnlock;
                  end;
                end;

               {Unlock Local}
               Local.ReaderUnlock;
              end;
            end;
          end;
         end;
        INARP_REPLY:begin
          {Check Protocol Type}
          case WordBEtoN(ARP.ProtocolType) of
           PACKET_TYPE_IP:begin
             {Get Local Address}
             Local:=GetAddressByAddress(ARP.TargetIP,Adapter.Adapter,True,NETWORK_LOCK_READ);
             if Local <> nil then
              begin
               {Check Address Type}
               if (Local.AddressType = ADDRESS_TYPE_LOCAL) or (Local.AddressType = ADDRESS_TYPE_BROADCAST) then
                begin
                 {Get Remote Address}
                 Remote:=GetAddressByAddress(ARP.SourceIP,Adapter.Adapter,True,NETWORK_LOCK_READ);
                 if Remote = nil then
                  begin
                   {Add Remote Address}
                   AddAddress(ARP.SourceIP,ARP.SourceHardware,Adapter.Adapter,ADDRESS_TYPE_DYNAMIC,False,NETWORK_LOCK_NONE);
                  end
                 else
                  begin
                   {Update Remote Address}
                   Remote.Hardware:=ARP.SourceHardware;

                   {Update timeout on the Remote}
                   if Remote.AddressType = ADDRESS_TYPE_DYNAMIC then
                    begin
                     Remote.AddressTime:=GetTickCount64;
                    end;

                   {Unlock Remote}
                   Remote.ReaderUnlock;
                  end;

                 {Return Result}
                 Result:=True;
                end;

               {Unlock Local}
               Local.ReaderUnlock;
              end;
            end;
          end;
         end;
       end;
      end;
    end;
  end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.SendARPRequest(AAdapter:TARPTransportAdapter;ASource:TARPAddressEntry;const ATarget:TInAddr):Boolean;
{Send an ARP request packet}
{Adapter: The transport adapter to send the request on}
{Source: The source hardware and IP address to send the request with}
{Target: The target IP address to send the request to}

{Note: Caller must hold Adapter lock and Source lock}
var
 ARP:TARPHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendARPRequest');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Create ARP Request}
 ARP.HardwareType:=WordNtoBE(AAdapter.Adapter.MediaType);
 ARP.ProtocolType:=WordNtoBE(PACKET_TYPE_IP);
 ARP.HardwareLength:=SizeOf(THardwareAddress);
 ARP.ProtocolLength:=SizeOf(TInAddr);
 ARP.Opcode:=WordNtoBE(ARP_REQUEST);
 ARP.SourceHardware:=ASource.Hardware;
 ARP.SourceIP:=InAddrToNetwork(ASource.Address);
 ARP.TargetHardware:=HARDWARE_DEFAULT;
 ARP.TargetIP:=InAddrToNetwork(ATarget);

 {Create Packet}
 Packet.Size:=ARP_HEADER_SIZE;
 Packet.Data:=@ARP;
 Packet.Next:=nil;

 {Send Packet}
 Result:=AAdapter.Adapter.SendPacket(AAdapter.Handle,@AAdapter.Broadcast,@Packet,ARP_HEADER_SIZE);

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendARPRequest Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TARPTransport.SendARPReply(AAdapter:TARPTransportAdapter;ASource,ATarget:TARPAddressEntry):Boolean;
{Send an ARP reply packet}
{Adapter: The transport adapter to send the reply on}
{Source: The source hardware and IP address to send the reply with}
{Target: The target harware and IP address to send the reply to}

{Note: Caller must hold Adapter lock, Source and Target locks}
var
 ARP:TARPHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendARPReply');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Check Target}
 if ATarget = nil then Exit;

 {Create ARP Reply}
 ARP.HardwareType:=WordNtoBE(AAdapter.Adapter.MediaType);
 ARP.ProtocolType:=WordNtoBE(PACKET_TYPE_IP);
 ARP.HardwareLength:=SizeOf(THardwareAddress);
 ARP.ProtocolLength:=SizeOf(TInAddr);
 ARP.Opcode:=WordNtoBE(ARP_REPLY);
 ARP.SourceHardware:=ASource.Hardware;
 ARP.SourceIP:=InAddrToNetwork(ASource.Address);
 ARP.TargetHardware:=ATarget.Hardware;
 ARP.TargetIP:=InAddrToNetwork(ATarget.Address);

 {Create Packet}
 Packet.Size:=ARP_HEADER_SIZE;
 Packet.Data:=@ARP;
 Packet.Next:=nil;

 {Send Packet}
 Result:=AAdapter.Adapter.SendPacket(AAdapter.Handle,@ATarget.Hardware,@Packet,ARP_HEADER_SIZE);

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendARPReply Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TARPTransport.SendINARPRequest(AAdapter:TARPTransportAdapter;ASource:TARPAddressEntry;const ATarget:THardwareAddress):Boolean;
{Send an Inverse ARP request packet}
{Adapter: The transport adapter to send the request on}
{Source: The source hardware and IP address to send the request with}
{Target: The target hardware address to send the request to}

{Note: Caller must hold Adapter lock and Source lock}
var
 ARP:TARPHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendINARPRequest');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Create INARP Request}
 ARP.HardwareType:=WordNtoBE(AAdapter.Adapter.MediaType);
 ARP.ProtocolType:=WordNtoBE(PACKET_TYPE_IP);
 ARP.HardwareLength:=SizeOf(THardwareAddress);
 ARP.ProtocolLength:=SizeOf(TInAddr);
 ARP.Opcode:=WordNtoBE(INARP_REQUEST);
 ARP.SourceHardware:=ASource.Hardware;
 ARP.SourceIP:=InAddrToNetwork(ASource.Address);
 ARP.TargetHardware:=ATarget;
 ARP.TargetIP:=InAddrToNetwork(IP_DEFAULT_ADDRESS);

 {Create Packet}
 Packet.Size:=ARP_HEADER_SIZE;
 Packet.Data:=@ARP;
 Packet.Next:=nil;

 {Send Packet}
 Result:=AAdapter.Adapter.SendPacket(AAdapter.Handle,@ATarget,@Packet,ARP_HEADER_SIZE);

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendINARPRequest Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TARPTransport.SendINARPReply(AAdapter:TARPTransportAdapter;ASource,ATarget:TARPAddressEntry):Boolean;
{Send an Inverse ARP reply packet}
{Adapter: The transport adapter to send the reply on}
{Source: The source hardware and IP address to send the reply with}
{Target: The target harware and IP address to send the reply to}

{Note: Caller must hold Adapter lock, Source and Target locks}
var
 ARP:TARPHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendINARPReply');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Check Target}
 if ATarget = nil then Exit;

 {Create INARP Reply}
 ARP.HardwareType:=WordNtoBE(AAdapter.Adapter.MediaType);
 ARP.ProtocolType:=WordNtoBE(PACKET_TYPE_IP);
 ARP.HardwareLength:=SizeOf(THardwareAddress);
 ARP.ProtocolLength:=SizeOf(TInAddr);
 ARP.Opcode:=WordNtoBE(INARP_REPLY);
 ARP.SourceHardware:=ASource.Hardware;
 ARP.SourceIP:=InAddrToNetwork(ASource.Address);
 ARP.TargetHardware:=ATarget.Hardware;
 ARP.TargetIP:=InAddrToNetwork(ATarget.Address);

 {Create Packet}
 Packet.Size:=ARP_HEADER_SIZE;
 Packet.Data:=@ARP;
 Packet.Next:=nil;

 {Send Packet}
 Result:=AAdapter.Adapter.SendPacket(AAdapter.Handle,@ATarget.Hardware,@Packet,ARP_HEADER_SIZE);

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: SendINARPReply Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TARPTransport.AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean;
{Add an adapter to this transport}
{Adapter: The adapter to add}
{ConfigType: The configuration type to use for configuring the adapter (eg CONFIG_TYPE_AUTO)}
{Address: The transport address to use for this adapter (or nil if supplied during configuration)}
{Netmask: The transport netmask to use for this adapter (or nil if supplied during configuration)}
{Gateway: The transport default gateway to use for this adapter (or nil if supplied during configuration)}
{Server: The transport configuration server to use for this adapter (or nil if supplied during configuration)}
var
 Handle:THandle;
 Adapter:TARPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: AddAdapter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Check State}
  if AAdapter.State <> ADAPTER_STATE_ENABLED then Exit;

  {Get Adapter}
  Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to use count}
  if Adapter = nil then
   begin
    {Add ARP Type}
    Handle:=AAdapter.AddTransport(PACKET_TYPE_ARP,FRAME_TYPE_ETHERNET_II,ARP_TRANSPORT_NAME,PacketHandler);
    if Handle <> INVALID_HANDLE_VALUE then
     begin
      {Create Adapter}
      Adapter:=TARPTransportAdapter.Create;
      Adapter.UseCount:=1;
      Adapter.Name:=AAdapter.Name;
      Adapter.Handle:=Handle;
      Adapter.PacketType:=PACKET_TYPE_ARP;
      Adapter.Adapter:=AAdapter;
      Adapter.Hardware:=AAdapter.GetHardwareAddress(Handle);
      Adapter.Broadcast:=AAdapter.GetBroadcastAddress(Handle);
      Adapter.MTU:=AAdapter.GetMTU(Handle);
      Adapter.ConfigType:=CONFIG_TYPE_AUTO;
      Adapter.Configured:=True;
      Adapter.Configuring:=False;

      {Acquire Lock}
      FAdapters.WriterLock;
      try
       {Add Adapter}
       FAdapters.Add(Adapter);

       {Add Broadcast Address}
       AddAddress(IP_BROADCAST_ADDRESS,Adapter.Broadcast,Adapter.Adapter,ADDRESS_TYPE_BROADCAST,False,NETWORK_LOCK_NONE);

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
    {Increment Count}
    Inc(Adapter.UseCount);

    {Unlock Adapter}
    Adapter.WriterUnlock;

    {Return Result}
    Result:=True;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
{Remove an adapter from this transport}
{Adapter: The adapter to remove}
var
 Adapter:TARPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: RemoveAdapter');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Adapter}
  Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to use count}
  if Adapter = nil then Exit;

  {Check Count}
  if Adapter.UseCount = 0 then
   begin
    {Unlock Adapter}
    Adapter.WriterUnlock;
    Exit;
   end;

  {Decrement Count}
  Dec(Adapter.UseCount);

  if Adapter.UseCount < 1 then
   begin
    {Remove ARP Type}
    if AAdapter.RemoveTransport(Adapter.Handle,Adapter.PacketType) then
     begin
      {Remove Broadcast Address}
      RemoveAddress(IP_BROADCAST_ADDRESS,AAdapter);

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
     end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.WriterUnlock;

    {Return Result}
    Result:=True;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.StartTransport:Boolean;
{Start this transport ready for sending and receiving}
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: StartTransport');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Return Result}
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.StopTransport:Boolean;
{Stop this transport ready for removal}
var
 Current:TARPTransportAdapter;
 Adapter:TARPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: StopTransport');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Get Adapter}
  Adapter:=TARPTransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Get Next}
    Current:=Adapter;
    Adapter:=TARPTransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));

    {Remove Adapter}
    RemoveAdapter(Adapter.Adapter);
   end;

  {Remove all Addresses}
  FlushAddresses(True);

  {Return Result}
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.ProcessTransport:Boolean;
{Process periodic tasks for this transport}
begin
 {}
 {Remove old Addresses}
 FlushAddresses(False);

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TARPTransport.BindTransport(AAdapter:TNetworkAdapter):Boolean;
{Bind this transport to an adapter if appropriate}
{Adapter: The adapter to bind to}

{Note: ARP binds to adapters only on request from other transports (eg IP)}
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: BindTransport');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:   State = ' + AdapterStateToString(AAdapter.State));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:   Status = ' + AdapterStatusToString(AAdapter.Status));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:   Type = ' + AdapterTypeToString(AAdapter.AdapterType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:   Media = ' + MediaTypeToString(AAdapter.MediaType));
  {$ENDIF}

  {Check State}
  if AAdapter.State <> ADAPTER_STATE_ENABLED then Exit;

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.UnbindTransport(AAdapter:TNetworkAdapter):Boolean;
{Unbind this transport from an adapter if appropriate}
{Adapter: The adapter to unbind from}

{Note: ARP unbinds from adapters only on request from other transports (eg IP)}
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: UnbindTransport');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
  {$ENDIF}

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.GetAddressByAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TARPAddressEntry;
{Find the IP address entry in the address cache}
{Address: The IP address to find}
{Adapter: The adapter which the address should be on}
{Lock: If True then lock the found entry before returning}
var
 Address:TARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: GetAddressByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Address}
  Address:=TARPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {$IFDEF ARP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Compare = ' + InAddrToString(InAddrToNetwork(Address.Address)));
    {$ENDIF}
    {Check Adatper}
    if Address.Adapter = AAdapter then
     begin
      {Check Address}
      if CompareAddress(Address.Address,AAddress) then
       begin
        {Lock Address}
        if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;

        {Return Result}
        Result:=Address;
        Exit;
       end;
     end;

    {Get Next}
    Address:=TARPAddressEntry(Address.Next);
   end;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.GetAddressByHardware(const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TARPAddressEntry;
{Find the hardware address entry in the address cache}
{Address: The hardware address to find}
{Adapter: The adapter which the address should be on}
{Lock: If True then lock the found entry before returning}
var
 Address:TARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: GetAddressByHardware');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Hardware = ' + HardwareAddressToString(AHardware));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Address}
  Address:=TARPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {$IFDEF ARP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Compare = ' + HardwareAddressToString(Address.Hardware));
    {$ENDIF}
    {Check Adatper}
    if Address.Adapter = AAdapter then
     begin
      {Check Hardware}
      if Address.Adapter.CompareAddress(Address.Hardware,AHardware) then
       begin
        {Lock Address}
        if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;

        {Return Result}
        Result:=Address;
        Exit;
       end;
     end;

    {Get Next}
    Address:=TARPAddressEntry(Address.Next);
   end;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.GetAddressByNext(APrevious:TARPAddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TARPAddressEntry;
var
 Address:TARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;

  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Address:=TARPAddressEntry(FAddresses.First);
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
    Address:=TARPAddressEntry(APrevious.Next);
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

function TARPTransport.AddAddress(const AAddress:TInAddr;const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;AType:Word;ALock:Boolean;AState:LongWord):TARPAddressEntry;
{Add an IP and hardware address pair to the address cache}
{Address: The IP address to add}
{Hardware: The hardware address to add}
{Adapter: The adapter the address is on}
{Type: The type of the added entry (eg ADDRESS_TYPE_DYNAMIC)}
{Lock: If True then lock the added entry before returning}
begin
 {}
 Result:=nil;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: AddAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Hardware = ' + HardwareAddressToString(AHardware));
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Create Address}
 Result:=TARPAddressEntry.Create;
 Result.Address:=AAddress;
 Result.Hardware:=AHardware;
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

    {Signal Event}
    EventPulse(FAddressAdd);

    {Reset Event (Manual Reset)}
    {EventReset(FAddressAdd);} {Not required, reset by EventPulse}

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

function TARPTransport.RemoveAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter):Boolean;
{Remove an IP and hardware address pair from the address cache}
{Address: The IP address to remove}
{Adapter: The adapter the address is on}
var
 Address:TARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: RemoveAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Address}
  Address:=TARPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {Check Adapter}
    if Address.Adapter = AAdapter then
     begin
      {Check Address}
      if CompareAddress(Address.Address,AAddress) then
       begin
        {Lock Address}
        Address.WriterLock;

        {Convert Lock}
        if FAddresses.ReaderConvert then
         begin
          {Remove Address}
          Result:=FAddresses.Remove(Address);

          {Convert Lock}
          FAddresses.WriterConvert;

          {Signal Event}
          EventPulse(FAddressRemove);

          {Reset Event (Manual Reset)}
          {EventReset(FAddressRemove);} {Not required, reset by EventPulse}

          {Unlock Address}
          Address.WriterUnlock;

          {Free Address}
          Address.Free;
         end;

        Exit;
       end;
     end;

    {Get Next}
    Address:=TARPAddressEntry(Address.Next);
   end;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

procedure TARPTransport.FlushAddresses(All:Boolean);
{Flush addresses from the address cache}
{All: If True flush all addresses, otherwise flush expired addresses}
var
 CurrentTime:Int64;
 Address:TARPAddressEntry;
 Current:TARPAddressEntry;
begin
 {}
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: FlushAddresses');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Tick Count}
 CurrentTime:=GetTickCount64;

 {Get Address}
 Address:=TARPAddressEntry(GetAddressByNext(nil,True,False,NETWORK_LOCK_READ));
 while Address <> nil do
  begin
   {Get Next}
   Current:=Address;
   Address:=TARPAddressEntry(GetAddressByNext(Current,True,False,NETWORK_LOCK_READ));

   {Check Address Type}
   if (Current.AddressType = ADDRESS_TYPE_DYNAMIC) or (All) then
    begin
     {Check Address Expiry}
     if ((Current.AddressTime + MAX_ADDRESS_LIFE) < CurrentTime) or (All) then
      begin
       {Convert Address}
       if Current.ReaderConvert then
        begin
         {Recheck Expiry (After Convert Lock)}
         if ((Current.AddressTime + MAX_ADDRESS_LIFE) < CurrentTime) or (All) then
          begin
           {$IFDEF ARP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Flush = ' + InAddrToString(InAddrToNetwork(Current.Address)) + ' / ' + HardwareAddressToString(Current.Hardware));
           {$ENDIF}

           {Acquire Lock}
           FAddresses.WriterLock;

           {Remove Address}
           FAddresses.Remove(Current);

           {Release Lock}
           FAddresses.WriterUnlock;

           {Signal Event}
           EventPulse(FAddressRemove);

           {Reset Event (Manual Reset)}
           {EventReset(FAddressRemove);} {Not required, reset by EventPulse}

           {Unlock Address}
           Current.WriterUnlock;

           {Free the Address}
           Current.Free;
           Current:=nil;
          end
         else
          begin
           {Convert Address}
           Current.WriterConvert;
          end;
        end;
      end;
    end;

   {Unlock Address}
   if Current <> nil then Current.ReaderUnlock;
  end;
end;

{==============================================================================}

function TARPTransport.LoadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr;const AHardware:THardwareAddress;AType:Word):Boolean;
{Add an IP and hardware address pair to the address cache}
{Adapter: The adapter the address is on}
{Address: The IP address to add}
{Hardware: The hardware address to add}
{Type: The type of the added entry (eg ADDRESS_TYPE_LOCAL)}
var
 Adapter:TARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: LoadAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Hardware = ' + HardwareAddressToString(AHardware));
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Check Address}
  if GetAddressByAddress(AAddress,AAdapter,False,NETWORK_LOCK_NONE) = nil then
   begin
    {Add Address}
    Result:=(AddAddress(AAddress,AHardware,AAdapter,AType,False,NETWORK_LOCK_NONE) <> nil); {Do not lock}
   end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.UnloadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr):Boolean;
{Remove an IP and hardware address pair from the address cache}
{Adapter: The adapter the address is on}
{Address: The IP address to remove}
var
 Adapter:TARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: UnloadAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Check Address}
  if GetAddressByAddress(AAddress,AAdapter,False,NETWORK_LOCK_NONE) <> nil then
   begin
    {Remove Address}
    Result:=RemoveAddress(AAddress,AAdapter);
   end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.ResolveAddress(AAdapter:TNetworkAdapter;const ASource,AAddress:TInAddr;var AHardware:THardwareAddress):Boolean;
{Resolve the hardware address for the given IP address either from cache or by sending an ARP request}
{Adapter: The adapter to use for resolving the hardware address}
{Source: The source IP address to use for resolving the hardware address}
{Address: The IP address to resolve}
{Hardware: The returned hardware address}
var
 Retries:Integer;
 StartTime:Int64;
 Source:TARPAddressEntry;
 Target:TARPAddressEntry;
 Adapter:TARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: ResolveAddress');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Get Source}
  Source:=GetAddressByAddress(ASource,AAdapter,True,NETWORK_LOCK_READ);
  if Source = nil then Exit;
  try
   {$IFDEF ARP_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Source = ' + InAddrToString(InAddrToNetwork(Source.Address)) + ' / ' + HardwareAddressToString(Source.Hardware));
   {$ENDIF}

   {Get Target}
   Target:=GetAddressByAddress(AAddress,AAdapter,True,NETWORK_LOCK_READ);
   if Target = nil then
    begin
     {$IFDEF ARP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Target not in cache');
     {$ENDIF}

     {Send an ARP Request}
     Retries:=ARP_RETRIES;
     while Retries > 0 do
      begin
       if SendARPRequest(Adapter,Source,AAddress) then
        begin
         {Check for Adapter Thread}
         if GetCurrentThreadId = AAdapter.ThreadID then
          begin
           if NETWORK_LOG_ENABLED then NetworkLogWarn(nil,'ARP:  Wait requested on adapter thread');
           Exit;
          end;

         {Wait for the Reply}
         StartTime:=GetTickCount64;
         while (StartTime + ARP_TIMEOUT) > GetTickCount64 do
          begin
           if EventWaitEx(FAddressAdd,ARP_TIMEOUT) = ERROR_SUCCESS then
            begin
             {Get Target}
             Target:=GetAddressByAddress(AAddress,AAdapter,True,NETWORK_LOCK_READ);
             if Target <> nil then
              begin
               {$IFDEF ARP_DEBUG}
               if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Target = ' + InAddrToString(InAddrToNetwork(Target.Address)) + ' / ' + HardwareAddressToString(Target.Hardware));
               {$ENDIF}

               {Get Hardware}
               AHardware:=Target.Hardware;

               {Unlock Target}
               Target.ReaderUnlock;

               {Return Result}
               Result:=True;
               Exit;
              end;
            end;
          end;
        end;

       Dec(Retries);
      end;
    end
   else
    begin
     {$IFDEF ARP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Target = ' + InAddrToString(InAddrToNetwork(Target.Address)) + ' / ' + HardwareAddressToString(Target.Hardware));
     {$ENDIF}

     {Update timeout on the Address}
     if Target.AddressType = ADDRESS_TYPE_DYNAMIC then
      begin
       Target.AddressTime:=GetTickCount64;
      end;

     {Get Hardware}
     AHardware:=Target.Hardware;

     {Unlock Target}
     Target.ReaderUnlock;

     {Return Result}
     Result:=True;
    end;
  finally
   Source.ReaderUnlock;
  end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.ResolveHardware(AAdapter:TNetworkAdapter;const ASource:TInAddr;const AHardware:THardwareAddress;var AAddress:TInAddr):Boolean;
{Resolve the IP address for the given hardware address either from cache or by sending an ARP request}
{Adapter: The adapter to use for resolving the IP address}
{Source: The source IP address to use for resolving the IP address}
{Hardware: The hardware address to resolve}
{Address: The returned IP address}
var
 Retries:Integer;
 StartTime:Int64;
 Source:TARPAddressEntry;
 Target:TARPAddressEntry;
 Adapter:TARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: ResolveHardware');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Get Source}
  Source:=GetAddressByAddress(ASource,AAdapter,True,NETWORK_LOCK_READ);
  if Source = nil then Exit;
  try
   {$IFDEF ARP_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Source = ' + InAddrToString(InAddrToNetwork(Source.Address)) + ' / ' + HardwareAddressToString(Source.Hardware));
   {$ENDIF}

   {Get Target}
   Target:=GetAddressByHardware(AHardware,AAdapter,True,NETWORK_LOCK_READ);
   if Target = nil then
    begin
     {$IFDEF ARP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Target not in cache');
     {$ENDIF}

     {Send an INARP Request}
     Retries:=ARP_RETRIES;
     while Retries > 0 do
      begin
       if SendINARPRequest(Adapter,Source,AHardware) then
        begin
         {Check for Adapter Thread}
         if GetCurrentThreadId = AAdapter.ThreadID then
          begin
           if NETWORK_LOG_ENABLED then NetworkLogWarn(nil,'ARP:  Wait requested on adapter thread');
           Exit;
          end;

         {Wait for the Reply}
         StartTime:=GetTickCount64;
         while (StartTime + ARP_TIMEOUT) > GetTickCount64 do
          begin
           if EventWaitEx(FAddressAdd,ARP_TIMEOUT) = ERROR_SUCCESS then
            begin
             {Get Target}
             Target:=GetAddressByHardware(AHardware,AAdapter,True,NETWORK_LOCK_READ);
             if Target <> nil then
              begin
               {$IFDEF ARP_DEBUG}
               if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Target = ' + InAddrToString(InAddrToNetwork(Target.Address)) + ' / ' + HardwareAddressToString(Target.Hardware));
               {$ENDIF}

               {Get Address}
               AAddress:=Target.Address;

               {Unlock Target}
               Target.ReaderUnlock;

               {Return Result}
               Result:=True;
               Exit;
              end;
            end;
          end;
        end;

       Dec(Retries);
      end;
    end
   else
    begin
     {$IFDEF ARP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Target = ' + InAddrToString(InAddrToNetwork(Target.Address)) + ' / ' + HardwareAddressToString(Target.Hardware));
     {$ENDIF}

     {Update timeout on the Address}
     if Target.AddressType = ADDRESS_TYPE_DYNAMIC then
      begin
       Target.AddressTime:=GetTickCount64;
      end;

     {Get Address}
     AAddress:=Target.Address;

     {Unlock Target}
     Target.ReaderUnlock;

     {Return Result}
     Result:=True;
    end;
  finally
   Source.ReaderUnlock;
  end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.ConfirmAddress(AAdapter:TNetworkAdapter;const ASource,AAddress:TInAddr):Boolean;
{Confirm that the supplied Address is not in use on this network}
{Adapter: The adapter to use for confirming the IP address}
{Source: The source IP address to use for confirming the IP address}
{Address: The IP address to confirm}

{Note: Confirm only tries once to resolve the Address}
var
 StartTime:Int64;
 Source:TARPAddressEntry;
 Target:TARPAddressEntry;
 Adapter:TARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: ConfirmAddress');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Get Source}
  Source:=GetAddressByAddress(ASource,AAdapter,True,NETWORK_LOCK_READ);
  if Source = nil then Exit;
  try
   {$IFDEF ARP_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Source = ' + InAddrToString(InAddrToNetwork(Source.Address)) + ' / ' + HardwareAddressToString(Source.Hardware));
   {$ENDIF}

   {Get Target}
   Target:=GetAddressByAddress(AAddress,AAdapter,True,NETWORK_LOCK_READ);
   if Target = nil then
    begin
     {$IFDEF ARP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Target not in cache');
     {$ENDIF}

     {Send an ARP Request}
     if SendARPRequest(Adapter,Source,AAddress) then
      begin
       {Check for Adapter Thread}
       if GetCurrentThreadId = AAdapter.ThreadID then
        begin
         if NETWORK_LOG_ENABLED then NetworkLogWarn(nil,'ARP:  Wait requested on adapter thread');
         Exit;
        end;

       {Wait for the Reply}
       StartTime:=GetTickCount64;
       while (StartTime + ARP_TIMEOUT) > GetTickCount64 do
        begin
         if EventWaitEx(FAddressAdd,ARP_TIMEOUT) = ERROR_SUCCESS then
          begin
           {Get Target}
           Target:=GetAddressByAddress(AAddress,AAdapter,False,NETWORK_LOCK_NONE);
           if Target <> nil then Exit;
          end;
        end;
      end;

     {Return Result}
     Result:=True;
    end;

   //To Do //Need to add use of this to Config - DHCP/BOOTP/RARP/STATIC etc
           //Note: for DHCP the Server will most likely already check this address by ARP
  finally
   Source.ReaderUnlock;
  end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.AdvertiseAddress(AAdapter:TNetworkAdapter;const ASource:TInAddr):Boolean;
{Send a Broadcast ARP reply to allow others to update their cache}
{Adapter: The adapter to use for advertising the IP address}
{Source: The IP address to advertise}
var
 Source:TARPAddressEntry;
 Target:TARPAddressEntry;
 Adapter:TARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: AdvertiseAddress');
 {$ENDIF}

 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Get Source}
  Source:=GetAddressByAddress(ASource,AAdapter,True,NETWORK_LOCK_READ);
  if Source = nil then Exit;
  try
   {$IFDEF ARP_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP:  Source = ' + InAddrToString(InAddrToNetwork(Source.Address)) + ' / ' + HardwareAddressToString(Source.Hardware));
   {$ENDIF}

   {Get Target}
   Target:=GetAddressByAddress(IP_BROADCAST_ADDRESS,AAdapter,True,NETWORK_LOCK_READ);
   if Target = nil then Exit;

   {Send an ARP Reply (Gratutitous)}
   Result:=SendARPReply(Adapter,Source,Target);

   {Unlock Target}
   Target.ReaderUnlock;

   //To Do //Need to add use of this to Config - DHCP/BOOTP/RARP/STATIC etc
           //Note: No one seems to listen to this when we send it anyway ? (tried with DHCP)
  finally
   Source.ReaderUnlock;
  end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TARPTransport.CompareDefault(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the IP default address}
begin
 {}
 Result:=(LongWord(AAddress.S_addr) = INADDR_ANY);
end;

{==============================================================================}

function TARPTransport.CompareAddress(const AAddress1,AAddress2:TInAddr):Boolean;
{Compare the supplied addresses to see if they are the same}
begin
 {}
 Result:=(LongWord(AAddress1.S_addr) = LongWord(AAddress2.S_addr));
end;

{==============================================================================}
{==============================================================================}
{TARPAddressEntry}
constructor TARPAddressEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET;
 FLength:=SizeOf(TInAddr);

 FAddress.S_addr:=INADDR_ANY;
 FProtocolType:=PACKET_TYPE_IP;
 FillChar(FHardware,SizeOf(THardwareAddress),0);
end;

{==============================================================================}

procedure TARPAddressEntry.SetAddress(const AAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}

procedure TARPAddressEntry.SetProtocolType(AProtocolType:Word);
begin
 {}
 if not AcquireLock then Exit;

 FProtocolType:=AProtocolType;

 ReleaseLock;
end;

{==============================================================================}

procedure TARPAddressEntry.SetHardware(const AHardware:THardwareAddress);
begin
 {}
 if not AcquireLock then Exit;

 FHardware:=AHardware;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TRARPTransportAdapter}
constructor TRARPTransportAdapter.Create;
begin
 {}
 inherited Create;
 UseCount:=0;
end;

{==============================================================================}
{==============================================================================}
{TRARPTransport}
constructor TRARPTransport.Create(AManager:TTransportManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FFamily:=AF_UNSPEC;
 FPacketType:=PACKET_TYPE_RARP;

 FAddresses:=TNetworkList.Create;
 FAddressAdd:=EventCreate(True,False);
 FAddressRemove:=EventCreate(True,False);
end;

{==============================================================================}

destructor TRARPTransport.Destroy;
begin
 {}
 WriterLock;
 try
  EventDestroy(FAddressRemove);
  EventDestroy(FAddressAdd);
  FAddresses.Free;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TRARPTransport.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by an Adapter}
{Handle: The Handle of the Transport Adapter the packet was received from}
{Source: The source hardware address of the received packet (Set by Adapter)}
{Dest: The destination hardware address of the received packet (Set by Adapter)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}
var
 RARP:PRARPHeader;
 Local:TRARPAddressEntry;
 Remote:TRARPAddressEntry;
 Adapter:TRARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: PacketHandler - Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Get Adapter}
 Adapter:=TRARPTransportAdapter(GetAdapterByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Check Packet Type}
  case Adapter.PacketType of
   PACKET_TYPE_RARP:begin
     {Check RARP Packet}
     if CheckRARP(APacket) then
      begin
       {Get Header}
       RARP:=PRARPHeader(APacket);

       {Set the Addresses to Host order}
       RARP.TargetIP:=InAddrToHost(RARP.TargetIP);
       RARP.SourceIP:=InAddrToHost(RARP.SourceIP);

       {Check Opcode}
       case WordBEtoN(RARP.Opcode) of
        RARP_REQUEST:begin
          {Check Protocol Type}
          case WordBEtoN(RARP.ProtocolType) of
           PACKET_TYPE_IP:begin
             {Get Remote Address}
             Remote:=GetAddressByHardware(RARP.TargetHardware,Adapter.Adapter,True,NETWORK_LOCK_READ);
             if Remote <> nil then
              begin
               {Check Address Type}
               if (Remote.AddressType = ADDRESS_TYPE_STATIC) or (Remote.AddressType = ADDRESS_TYPE_DYNAMIC) then
                begin
                 {Get Local Address}
                 Local:=GetAddressByHardware(Adapter.Hardware,Adapter.Adapter,True,NETWORK_LOCK_READ);
                 if Local <> nil then
                  begin
                   {Safety Check for INADDR_ANY}
                   if not CompareDefault(Local.Address) then
                    begin
                     {Send RARP Reply}
                     Result:=SendRARPReply(Adapter,Local,Remote);
                    end;

                   {Unlock Local}
                   Local.ReaderUnlock;
                  end;
                end;

               {Unlock Remote}
               Remote.ReaderUnlock;
              end;
            end;
          end;
         end;
        RARP_REPLY:begin
          {Check Protocol Type}
          case WordBEtoN(RARP.ProtocolType) of
           PACKET_TYPE_IP:begin
             {Compare Local Address}
             if Adapter.Adapter.CompareAddress(RARP.TargetHardware,Adapter.Hardware) then
              begin
               {Get Local Address}
               Local:=GetAddressByHardware(Adapter.Hardware,Adapter.Adapter,True,NETWORK_LOCK_READ);
               if Local = nil then
                begin
                 {Add Local Address}
                 AddAddress(RARP.TargetIP,RARP.TargetHardware,Adapter.Adapter,ADDRESS_TYPE_LOCAL,False,NETWORK_LOCK_NONE);
                end
               else
                begin
                 {Update Local Address}
                 Local.Address:=RARP.TargetIP;

                 {Unlock Local}
                 Local.ReaderUnlock;
                end;

               {Return Result}
               Result:=True;
              end;
            end;
          end;
         end;
       end;
      end;
    end;
  end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.SendRARPRequest(AAdapter:TRARPTransportAdapter):Boolean;
{Send a RARP request packet}
{Adapter: The transport adapter to send the request on}

{Note: Caller must hold Adapter lock}
var
 RARP:TRARPHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: SendRARPRequest');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Create RARP Request}
 RARP.HardwareType:=WordNtoBE(AAdapter.Adapter.MediaType);
 RARP.ProtocolType:=WordNtoBE(PACKET_TYPE_IP);
 RARP.HardwareLength:=SizeOf(THardwareAddress);
 RARP.ProtocolLength:=SizeOf(TInAddr);
 RARP.Opcode:=WordNtoBE(RARP_REQUEST);
 RARP.SourceHardware:=AAdapter.Hardware;
 RARP.SourceIP:=InAddrToNetwork(IP_DEFAULT_ADDRESS);
 RARP.TargetHardware:=AAdapter.Hardware;
 RARP.TargetIP:=InAddrToNetwork(IP_DEFAULT_ADDRESS);

 {Create Packet}
 Packet.Size:=RARP_HEADER_SIZE;
 Packet.Data:=@RARP;
 Packet.Next:=nil;

 {Send Packet}
 Result:=AAdapter.Adapter.SendPacket(AAdapter.Handle,@AAdapter.Broadcast,@Packet,RARP_HEADER_SIZE);

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: SendRARPRequest Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TRARPTransport.SendRARPReply(AAdapter:TRARPTransportAdapter;ASource,ATarget:TRARPAddressEntry):Boolean;
{Send a RARP reply packet}
{Adapter: The transport adapter to send the reply on}
{Source: The source hardware and IP address to send the reply with}
{Target: The target harware and IP address to send the reply to}

{Note: Caller must hold Adapter lock, Source and Target locks}
var
 RARP:TRARPHeader;
 Packet:TPacketFragment;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: SendRARPReply');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Check Source}
 if ASource = nil then Exit;

 {Check Target}
 if ATarget = nil then Exit;

 {Create RARP Reply}
 RARP.HardwareType:=WordNtoBE(AAdapter.Adapter.MediaType);
 RARP.ProtocolType:=WordNtoBE(PACKET_TYPE_IP);
 RARP.HardwareLength:=SizeOf(THardwareAddress);
 RARP.ProtocolLength:=SizeOf(TInAddr);
 RARP.Opcode:=WordNtoBE(RARP_REPLY);
 RARP.SourceHardware:=ASource.Hardware;
 RARP.SourceIP:=InAddrToNetwork(ASource.Address);
 RARP.TargetHardware:=ATarget.Hardware;
 RARP.TargetIP:=InAddrToNetwork(ATarget.Address);

 {Create Packet}
 Packet.Size:=RARP_HEADER_SIZE;
 Packet.Data:=@RARP;
 Packet.Next:=nil;

 {Send Packet}
 Result:=AAdapter.Adapter.SendPacket(AAdapter.Handle,@ATarget.Hardware,@Packet,RARP_HEADER_SIZE);

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: SendRARPReply Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TRARPTransport.AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean;
{Add an adapter to this transport}
{Adapter: The adapter to add}
{ConfigType: The configuration type to use for configuring the adapter (eg CONFIG_TYPE_AUTO)}
{Address: The transport address to use for this adapter (or nil if supplied during configuration)}
{Netmask: The transport netmask to use for this adapter (or nil if supplied during configuration)}
{Gateway: The transport default gateway to use for this adapter (or nil if supplied during configuration)}
{Server: The transport configuration server to use for this adapter (or nil if supplied during configuration)}
var
 Handle:THandle;
 Adapter:TRARPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: AddAdapter');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Check State}
  if AAdapter.State <> ADAPTER_STATE_ENABLED then Exit;

  {Get Adapter}
  Adapter:=TRARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to use count}
  if Adapter = nil then
   begin
    {Add RARP Type}
    Handle:=AAdapter.AddTransport(PACKET_TYPE_RARP,FRAME_TYPE_ETHERNET_II,RARP_TRANSPORT_NAME,PacketHandler);
    if Handle <> INVALID_HANDLE_VALUE then
     begin
      {Create Adapter}
      Adapter:=TRARPTransportAdapter.Create;
      Adapter.UseCount:=1;
      Adapter.Name:=AAdapter.Name;
      Adapter.Handle:=Handle;
      Adapter.PacketType:=PACKET_TYPE_RARP;
      Adapter.Adapter:=AAdapter;
      Adapter.Hardware:=AAdapter.GetHardwareAddress(Handle);
      Adapter.Broadcast:=AAdapter.GetBroadcastAddress(Handle);
      Adapter.MTU:=AAdapter.GetMTU(Handle);
      Adapter.ConfigType:=CONFIG_TYPE_AUTO;
      Adapter.Configured:=True;
      Adapter.Configuring:=False;

      {Acquire Lock}
      FAdapters.WriterLock;
      try
       {Add Adapter}
       FAdapters.Add(Adapter);

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
    {Increment Count}
    Inc(Adapter.UseCount);

    {Unlock Adapter}
    Adapter.WriterUnlock;

    {Return Result}
    Result:=True;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
{Remove an adapter from this transport}
{Adapter: The adapter to remove}
var
 Adapter:TRARPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: RemoveAdapter');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Adapter}
  Adapter:=TRARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to use count}
  if Adapter = nil then Exit;

  {Check Count}
  if Adapter.UseCount = 0 then
   begin
    {Unlock Adapter}
    Adapter.WriterUnlock;
    Exit;
   end;

  {Decrement Count}
  Dec(Adapter.UseCount);

  if Adapter.UseCount < 1 then
   begin
    {Remove RARP Type}
    if AAdapter.RemoveTransport(Adapter.Handle,Adapter.PacketType) then
     begin
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
     end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.WriterUnlock;

    {Return Result}
    Result:=True;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.StartTransport:Boolean;
{Start this transport ready for sending and receiving}
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: StartTransport');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Return Result}
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.StopTransport:Boolean;
{Stop this transport ready for removal}
var
 Current:TRARPTransportAdapter;
 Adapter:TRARPTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: StopTransport');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Get Adapter}
  Adapter:=TRARPTransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Get Next}
    Current:=Adapter;
    Adapter:=TRARPTransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));

    {Remove Adapter}
    RemoveAdapter(Adapter.Adapter);
   end;

  {Remove all Addresses}
  FlushAddresses(True);

  {Return Result}
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.ProcessTransport:Boolean;
{Process periodic tasks for this transport}
begin
 {}
 {Remove old Addresses}
 FlushAddresses(False);

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TRARPTransport.BindTransport(AAdapter:TNetworkAdapter):Boolean;
{Bind this transport to an adapter if appropriate}
{Adapter: The adapter to bind to}

{Note: RARP binds to adapters only on request from other transports (eg IP)}
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: BindTransport');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Adapter = ' + AAdapter.Name);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:   State = ' + AdapterStateToString(AAdapter.State));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:   Status = ' + AdapterStatusToString(AAdapter.Status));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:   Type = ' + AdapterTypeToString(AAdapter.AdapterType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:   Media = ' + MediaTypeToString(AAdapter.MediaType));
  {$ENDIF}

  {Check State}
  if AAdapter.State <> ADAPTER_STATE_ENABLED then Exit;

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.UnbindTransport(AAdapter:TNetworkAdapter):Boolean;
{Unbind this transport from an adapter if appropriate}
{Adapter: The adapter to unbind from}

{Note: RARP unbinds from adapters only on request from other transports (eg IP)}
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: UnbindTransport');
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Adapter = ' + AAdapter.Name);
  {$ENDIF}

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.GetAddressByAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TRARPAddressEntry;
{Find the IP address entry in the address cache}
{Address: The IP address to find}
{Adapter: The adapter which the address should be on}
{Lock: If True then lock the found entry before returning}
var
 Address:TRARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: GetAddressByAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Address}
  Address:=TRARPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {$IFDEF ARP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Compare = ' + InAddrToString(InAddrToNetwork(Address.Address)));
    {$ENDIF}
    {Check Adatper}
    if Address.Adapter = AAdapter then
     begin
      {Check Address}
      if CompareAddress(Address.Address,AAddress) then
       begin
        {Lock Address}
        if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;

        {Return Result}
        Result:=Address;
        Exit;
       end;
     end;

    {Get Next}
    Address:=TRARPAddressEntry(Address.Next);
   end;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.GetAddressByHardware(const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TRARPAddressEntry;
{Find the hardware address entry in the address cache}
{Address: The hardware address to find}
{Adapter: The adapter which the address should be on}
{Lock: If True then lock the found entry before returning}
var
 Address:TRARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: GetAddressByHardware');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Hardware = ' + HardwareAddressToString(AHardware));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Address}
  Address:=TRARPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {$IFDEF ARP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Compare = ' + HardwareAddressToString(Address.Hardware));
    {$ENDIF}
    {Check Adatper}
    if Address.Adapter = AAdapter then
     begin
      {Check Hardware}
      if Address.Adapter.CompareAddress(Address.Hardware,AHardware) then
       begin
        {Lock Address}
        if ALock then if AState = NETWORK_LOCK_READ then Address.ReaderLock else Address.WriterLock;

        {Return Result}
        Result:=Address;
        Exit;
       end;
     end;

    {Get Next}
    Address:=TRARPAddressEntry(Address.Next);
   end;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.GetAddressByNext(APrevious:TRARPAddressEntry;ALock,AUnlock:Boolean;AState:LongWord):TRARPAddressEntry;
var
 Address:TRARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=nil;

  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Address:=TRARPAddressEntry(FAddresses.First);
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
    Address:=TRARPAddressEntry(APrevious.Next);
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

function TRARPTransport.AddAddress(const AAddress:TInAddr;const AHardware:THardwareAddress;AAdapter:TNetworkAdapter;AType:Word;ALock:Boolean;AState:LongWord):TRARPAddressEntry;
{Add an IP and hardware address pair to the address cache}
{Address: The IP address to add}
{Hardware: The hardware address to add}
{Adapter: The adapter the address is on}
{Type: The type of the added entry (eg ADDRESS_TYPE_DYNAMIC)}
{Lock: If True then lock the added entry before returning}
begin
 {}
 Result:=nil;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: AddAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Hardware = ' + HardwareAddressToString(AHardware));
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;

 {Create Address}
 Result:=TRARPAddressEntry.Create;
 Result.Address:=AAddress;
 Result.Hardware:=AHardware;
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

    {Signal Event}
    EventPulse(FAddressAdd);

    {Reset Event (Manual Reset)}
    {EventReset(FAddressAdd);} {Not required, reset by EventPulse}

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

function TRARPTransport.RemoveAddress(const AAddress:TInAddr;AAdapter:TNetworkAdapter):Boolean;
{Remove an IP and hardware address pair from the address cache}
{Address: The IP address to remove}
{Adapter: The adapter the address is on}
var
 Address:TRARPAddressEntry;
begin
 {}
 FAddresses.ReaderLock;
 try
  Result:=False;

  {$IFDEF ARP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: RemoveAddress');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
  {$ENDIF}

  {Check Adapter}
  if AAdapter = nil then Exit;

  {Get Address}
  Address:=TRARPAddressEntry(FAddresses.First);
  while Address <> nil do
   begin
    {Check Adapter}
    if Address.Adapter = AAdapter then
     begin
      {Check Address}
      if CompareAddress(Address.Address,AAddress) then
       begin
        {Lock Address}
        Address.WriterLock;

        {Convert Lock}
        if FAddresses.ReaderConvert then
         begin
          {Remove Address}
          Result:=FAddresses.Remove(Address);

          {Convert Lock}
          FAddresses.WriterConvert;

          {Signal Event}
          EventPulse(FAddressRemove);

          {Reset Event (Manual Reset)}
          {EventReset(FAddressRemove);} {Not required, reset by EventPulse}

          {Unlock Address}
          Address.WriterUnlock;

          {Free Address}
          Address.Free;
         end;

        Exit;
       end;
     end;

    {Get Next}
    Address:=TRARPAddressEntry(Address.Next);
   end;
 finally
  FAddresses.ReaderUnlock;
 end;
end;

{==============================================================================}

procedure TRARPTransport.FlushAddresses(All:Boolean);
{Flush addresses from the address cache}
{All: If True flush all addresses, otherwise flush expired addresses}
var
 CurrentTime:Int64;
 Address:TRARPAddressEntry;
 Current:TRARPAddressEntry;
begin
 {}
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: FlushAddresses');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  All = ' + BoolToStr(All));
 {$ENDIF}

 {Get Tick Count}
 CurrentTime:=GetTickCount64;

 {Get Address}
 Address:=TRARPAddressEntry(GetAddressByNext(nil,True,False,NETWORK_LOCK_READ));
 while Address <> nil do
  begin
   {Get Next}
   Current:=Address;
   Address:=TRARPAddressEntry(GetAddressByNext(Current,True,False,NETWORK_LOCK_READ));

   {Check Address Type}
   if (Current.AddressType = ADDRESS_TYPE_DYNAMIC) or (All) then
    begin
     {Check Address Expiry}
     if ((Current.AddressTime + MAX_ADDRESS_LIFE) < CurrentTime) or (All) then
      begin
       {Convert Address}
       if Current.ReaderConvert then
        begin
         {Recheck Expiry (After Convert Lock)}
         if ((Current.AddressTime + MAX_ADDRESS_LIFE) < CurrentTime) or (All) then
          begin
           {$IFDEF ARP_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Flush = ' + InAddrToString(InAddrToNetwork(Current.Address)) + ' / ' + HardwareAddressToString(Current.Hardware));
           {$ENDIF}

           {Acquire Lock}
           FAddresses.WriterLock;

           {Remove Address}
           FAddresses.Remove(Current);

           {Release Lock}
           FAddresses.WriterUnlock;

           {Signal Event}
           EventPulse(FAddressRemove);

           {Reset Event (Manual Reset)}
           {EventReset(FAddressRemove);} {Not required, reset by EventPulse}

           {Unlock Address}
           Current.WriterUnlock;

           {Free the Address}
           Current.Free;
           Current:=nil;
          end
         else
          begin
           {Convert Address}
           Current.WriterConvert;
          end;
        end;
      end;
    end;

   {Unlock Address}
   if Current <> nil then Current.ReaderUnlock;
  end;
end;

{==============================================================================}

function TRARPTransport.LoadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr;const AHardware:THardwareAddress;AType:Word):Boolean;
{Add an IP and hardware address pair to the address cache}
{Adapter: The adapter the address is on}
{Address: The IP address to add}
{Hardware: The hardware address to add}
{Type: The type of the added entry (eg ADDRESS_TYPE_LOCAL)}
var
 Adapter:TRARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: LoadAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Hardware = ' + HardwareAddressToString(AHardware));
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TRARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Check Address}
  if GetAddressByAddress(AAddress,AAdapter,False,NETWORK_LOCK_NONE) = nil then
   begin
    {Add Address}
    Result:=(AddAddress(AAddress,AHardware,AAdapter,AType,False,NETWORK_LOCK_NONE) <> nil); {Do not lock}
   end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.UnloadAddress(AAdapter:TNetworkAdapter;const AAddress:TInAddr):Boolean;
{Remove an IP and hardware address pair from the address cache}
{Adapter: The adapter the address is on}
{Address: The IP address to remove}
var
 Adapter:TRARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: UnloadAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address = ' + InAddrToString(InAddrToNetwork(AAddress)));
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TRARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Check Address}
  if GetAddressByAddress(AAddress,AAdapter,False,NETWORK_LOCK_NONE) <> nil then
   begin
    {Remove Address}
    Result:=RemoveAddress(AAddress,AAdapter);
   end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.ResolveHardware(AAdapter:TNetworkAdapter;var AAddress:TInAddr):Boolean;
{Resolve the hardware address for the given IP address either from cache or by sending a RARP request}
{Adapter: The adapter to use for resolving the hardware address}
{Source: The source IP address to use for resolving the hardware address}
{Address: The IP address to resolve}
{Hardware: The returned hardware address}
var
 Retries:Integer;
 StartTime:Int64;
 Address:TRARPAddressEntry;
 Adapter:TRARPTransportAdapter;
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: ResolveHardware');
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Adapter = ' + AAdapter.Name);
 {$ENDIF}

 {Get Adapter}
 Adapter:=TRARPTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {Get Address}
  Address:=GetAddressByHardware(Adapter.Hardware,AAdapter,True,NETWORK_LOCK_READ);
  if Address = nil then
   begin
    {$IFDEF ARP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address not in cache');
    {$ENDIF}

    {Send a RARP Request}
    Retries:=RARP_RETRIES;
    while Retries > 0 do
     begin
      if SendRARPRequest(Adapter) then
       begin
        {Wait for the Reply}
        StartTime:=GetTickCount64;
        while (StartTime + RARP_TIMEOUT) > GetTickCount64 do
         begin
          if EventWaitEx(FAddressAdd,RARP_TIMEOUT) = ERROR_SUCCESS then
           begin
            {Get Address}
            Address:=GetAddressByHardware(Adapter.Hardware,AAdapter,True,NETWORK_LOCK_READ);
            if Address <> nil then
             begin
              {$IFDEF ARP_DEBUG}
              if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address = ' + InAddrToString(InAddrToNetwork(Address.Address)) + ' / ' + HardwareAddressToString(Address.Hardware));
              {$ENDIF}

              {Get Address}
              AAddress:=Address.Address;

              {Unlock Address}
              Address.ReaderUnlock;

              {Return Result}
              Result:=True;
              Exit;
             end;
           end;
         end;
       end;

      Dec(Retries);
     end;
   end
  else
   begin
    {$IFDEF ARP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP:  Address = ' + InAddrToString(InAddrToNetwork(Address.Address)) + ' / ' + HardwareAddressToString(Address.Hardware));
    {$ENDIF}

    {Get Address}
    AAddress:=Address.Address;

    {Unlock Address}
    Address.ReaderUnlock;

    {Return Result}
    Result:=True;
   end;
 finally
  Adapter.ReaderUnlock;
 end;
end;

{==============================================================================}

function TRARPTransport.CompareDefault(const AAddress:TInAddr):Boolean;
{Compare the supplied address with the IP default address}
begin
 {}
 Result:=(LongWord(AAddress.S_addr) = INADDR_ANY);
end;

{==============================================================================}

function TRARPTransport.CompareAddress(const AAddress1,AAddress2:TInAddr):Boolean;
{Compare the supplied addresses to see if they are the same}
begin
 {}
 Result:=(LongWord(AAddress1.S_addr) = LongWord(AAddress2.S_addr));
end;

{==============================================================================}
{==============================================================================}
{TRARPAddressEntry}
constructor TRARPAddressEntry.Create;
begin
 {}
 inherited Create;
 FFamily:=AF_INET;
 FLength:=SizeOf(TInAddr);

 FAddress.S_addr:=INADDR_ANY;
 FProtocolType:=PACKET_TYPE_IP;
 FillChar(FHardware,SizeOf(THardwareAddress),0);
end;

{==============================================================================}

procedure TRARPAddressEntry.SetAddress(const AAddress:TInAddr);
begin
 {}
 if not AcquireLock then Exit;

 FAddress:=AAddress;

 ReleaseLock;
end;

{==============================================================================}

procedure TRARPAddressEntry.SetProtocolType(AProtocolType:Word);
begin
 {}
 if not AcquireLock then Exit;

 FProtocolType:=AProtocolType;

 ReleaseLock;
end;

{==============================================================================}

procedure TRARPAddressEntry.SetHardware(const AHardware:THardwareAddress);
begin
 {}
 if not AcquireLock then Exit;

 FHardware:=AHardware;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ARPInit;
begin
 {}
 {Check Initialized}
 if ARPInitialized then Exit;

 {Setup ARP Transport}
 if NetworkSettings.GetBoolean('IP_TRANSPORT_ENABLED') then NetworkSettings.AddBoolean('ARP_TRANSPORT_ENABLED',True);
 if NetworkSettings.GetBoolean('ARP_CONFIG_ENABLED') then NetworkSettings.AddBoolean('ARP_TRANSPORT_ENABLED',True);

 {Setup RARP Transport}
 if NetworkSettings.GetBoolean('IP_TRANSPORT_ENABLED') then NetworkSettings.AddBoolean('RARP_TRANSPORT_ENABLED',True);
 if NetworkSettings.GetBoolean('RARP_CONFIG_ENABLED') then NetworkSettings.AddBoolean('RARP_TRANSPORT_ENABLED',True);

 {Create ARP Transport}
 if NetworkSettings.GetBooleanDefault('ARP_TRANSPORT_ENABLED',ARP_TRANSPORT_ENABLED) then
  begin
   TARPTransport.Create(TransportManager,ARP_TRANSPORT_NAME);
  end;

 {Create RARP Transport}
 if NetworkSettings.GetBooleanDefault('RARP_TRANSPORT_ENABLED',RARP_TRANSPORT_ENABLED) then
  begin
   TRARPTransport.Create(TransportManager,RARP_TRANSPORT_NAME);
  end;

 ARPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ARP Functions}
function CheckARP(ABuffer:Pointer):Boolean;
{Buffer points to the complete packet without Adapter header}
{Verify that the packet is a valid ARP packet}
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARP: CheckARP');
 {$ENDIF}

 {Check Hardware Type}
 case WordBEtoN(PARPHeader(ABuffer).HardwareType) of
  MEDIA_TYPE_ETHERNET,MEDIA_TYPE_TOKENRING:begin
    {Check Hardware Length}
    if PARPHeader(ABuffer).HardwareLength <> SizeOf(THardwareAddress) then Exit;

    {Check Protocol Type}
    case WordBEtoN(PARPHeader(ABuffer).ProtocolType) of
     PACKET_TYPE_IP:begin
       {Check Protocol Length}
       if PARPHeader(ABuffer).ProtocolLength <> SizeOf(TInAddr) then Exit;

       {Return Result}
       Result:=True;
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function CheckRARP(ABuffer:Pointer):Boolean;
{Buffer points to the complete packet without Adapter header}
{Verify that the packet is a valid RARP packet}
begin
 {}
 Result:=False;

 {$IFDEF ARP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARP: CheckRARP');
 {$ENDIF}

 {Check Hardware Type}
 case WordBEtoN(PRARPHeader(ABuffer).HardwareType) of
  MEDIA_TYPE_ETHERNET,MEDIA_TYPE_TOKENRING:begin
    {Check Hardware Length}
    if PRARPHeader(ABuffer).HardwareLength <> SizeOf(THardwareAddress) then Exit;

    {Check Protocol Type}
    case WordBEtoN(PRARPHeader(ABuffer).ProtocolType) of
     PACKET_TYPE_IP:begin
       {Check Protocol Length}
       if PRARPHeader(ABuffer).ProtocolLength <> SizeOf(TInAddr) then Exit;

       {Return Result}
       Result:=True;
      end;
    end;
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{ARP Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 ARPInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

