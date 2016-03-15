{
Ultibo Loopback Network adapter unit.

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

 
Loopback Network
================

 Notes: Implements a simple Loopback that appears like an Adapter

 Notes: Loopback Driver does not use Bindings as per Network.pas
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Loopback;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,SysUtils,Classes,Network,Ultibo,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {Loopback specific constants}
 MAX_LOOPBACK_BUFFERS = 256;     {Used for Recv Queues (Increased to Handle Higher Data Rate)}
 MIN_LOOPBACK_BUFFERS = 4;       {Used for Send Queues}
  
{==============================================================================}
type
 {Loopback specific types}
 PLoopbackBuffer = ^TLoopbackBuffer;
 TLoopbackBuffer = packed record
  Size:Word;  {Size of Data}
  Data:array[0..MAX_PHYSICAL_PACKET - 3] of Byte; {MAX_PHYSICAL_PACKET - Word}
 end;
  
{==============================================================================}
type
 {Loopback specific classes}
 TLoopbackAdapter = class(TNetworkAdapter)
   constructor Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
  private
   {Internal Variables}

   {Status Variables}
   FHardwareAddress:THardwareAddress;
  protected
   {Inherited Methods}

  public
   {}
   function AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle; override;
   function RemoveTransport(AHandle:THandle;APacketType:Word):Boolean; override;

   function GetMTU(AHandle:THandle):Word; override;

   function SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean; override;

   function GetHardwareAddress(AHandle:THandle):THardwareAddress; override;

   function StartAdapter:Boolean; override;
   function StopAdapter:Boolean; override;
   function ProcessAdapter:Boolean; override;
 end;
 
 TLoopbackAdapterTransport = class(TAdapterTransport)
   constructor Create;
   destructor Destroy; override;
  private
   {}
  public
   {}
   Buffer:TAdapterBuffer; //To Do //Does this need lock protection ?
 end;
  
{==============================================================================}
{var}
 {Loopback specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure LoopbackInit;

{==============================================================================}
{Loopback Functions}
  
{==============================================================================}
{Loopback Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Loopback specific variables}
 LoopbackInitialized:Boolean;
 
{==============================================================================}
{==============================================================================}
{TLoopbackAdapter}
constructor TLoopbackAdapter.Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
begin
 {}
 inherited Create(AManager,ADevice,AName);
 {Set Defaults}
 FMediaType:=MEDIA_TYPE_ETHERNET;
 FAdapterType:=ADAPTER_TYPE_LOOPBACK;
 FillChar(FHardwareAddress,SizeOf(THardwareAddress),0);
end;

{==============================================================================}

function TLoopbackAdapter.AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle;
var
 Transport:TLoopbackAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;
  
  {$IFDEF LOOPBACK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: AddTransport (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Packet = ' + PacketTypeToString(APacketType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Frame = ' + FrameTypeToString(AFrameType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Name = ' + APacketName);
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
 
  {Get Transport}
  Transport:=TLoopbackAdapterTransport(GetTransportByType(APacketType,AFrameType,False,NETWORK_LOCK_NONE)); {Do not lock}
  if Transport <> nil then Exit;
 
  {Create Transport}
  Transport:=TLoopbackAdapterTransport.Create;
  Transport.FrameType:=AFrameType;
  Transport.PacketType:=APacketType;
  Transport.PacketName:=APacketName;
  Transport.PacketHandler:=APacketHandler;
  Transport.Buffer.Size:=MAX_LOOPBACK_BUFFERS;
 
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

function TLoopbackAdapter.RemoveTransport(AHandle:THandle;APacketType:Word):Boolean;
var
 Transport:TLoopbackAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF LOOPBACK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: RemoveTransport (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Packet = ' + PacketTypeToString(APacketType));
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

  {Get Transport}
  Transport:=TLoopbackAdapterTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
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

function TLoopbackAdapter.GetMTU(AHandle:THandle):Word;
begin
 {}
 ReaderLock;
 try
  Result:=0;
  
  {$IFDEF LOOPBACK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: GetMTU (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
 
  {Return Result}
  Result:=MAX_ETHERNET_PACKET - ETHERNET_HEADER_SIZE;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackAdapter.SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean;
{Write the Packet to the Transport Buffer}
var
 Size:Integer;
 Packet:PPacketFragment;
 Buffer:PLoopbackBuffer;
 Transport:TLoopbackAdapterTransport;
begin
 {}
 Result:=False;
  
 {$IFDEF LOOPBACK_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: SendPacket (' + Name + ')');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Handle = ' + IntToHex(AHandle,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Dest}
 if ADest = nil then Exit;
  
 {Check Packet}
 if APacket = nil then Exit;
 
 {Check Status}
 if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Size}
 if ASize > (MAX_ETHERNET_PACKET - ETHERNET_HEADER_SIZE) then Exit;
  
 {Get Transport}
 Transport:=TLoopbackAdapterTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Get Next Buffer}
  Buffer:=Transport.Buffer.WriteNext;
  if Buffer = nil then Exit;
 
  {Copy Packet Fragments to Buffer}
  Size:=0;
  Packet:=APacket;
  while Packet <> nil do
   begin
    System.Move(Packet.Data^,Buffer.Data[Size],Packet.Size);
    Inc(Size,Packet.Size);
    Packet:=Packet.Next;
   end;
 
  {Save the Size}
  Buffer.Size:=Size;
 
  {Notify the Thread}
  Result:=FThread.SendHandle(AHandle);
 finally 
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackAdapter.GetHardwareAddress(AHandle:THandle):THardwareAddress;
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(THardwareAddress),0);
 
  {$IFDEF NETWORK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: GetHardwareAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
 
  {Return Result}
  Result:=FHardwareAddress;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackAdapter.StartAdapter:Boolean;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF LOOPBACK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: StartAdapter (' + Name + ')');
  {$ENDIF}
 
  {Check Status} 
  if FStatus <> ADAPTER_STATUS_UNKNOWN then Exit;
 
  {Set Status}
  FStatus:=ADAPTER_STATUS_READY;
 
  {Set Hardware Address}
  FHardwareAddress:=HARDWARE_LOOPBACK;
 
  {Create Thread}
  FThread:=TAdapterThread.Create(Self);
  
  {Start Thread}
  FThread.Start;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackAdapter.StopAdapter:Boolean;
var
 Current:TLoopbackAdapterTransport;
 Transport:TLoopbackAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF LOOPBACK_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: StopAdapter (' + Name + ')');
  {$ENDIF}

  {Check Status} 
  if FStatus <> ADAPTER_STATUS_READY then Exit;
  
  {Check Thread}
  if FThread = nil then Exit;

  {Terminate Thread}
  FThread.Terminate;
  
  {Release Thread}
  FThread.SendHandle(INVALID_HANDLE_VALUE);
  
  {Wait For Thread}
  FThread.WaitFor;
  
  {Destroy Thread}
  FThread.Free;
  FThread:=nil;
  
  {Get Transport}
  Transport:=TLoopbackAdapterTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
  while Transport <> nil do
   begin
    {Get Next}
    Current:=Transport;
    Transport:=TLoopbackAdapterTransport(GetTransportByNext(Current,True,True,NETWORK_LOCK_READ));
    
    {Remove Transport}
    RemoveTransport(THandle(Transport),Transport.PacketType);
   end;
 
  {Reset Status}
  FStatus:=ADAPTER_STATUS_UNKNOWN;
  
  {Reset Hardware Address}
  FillChar(FHardwareAddress,SizeOf(THardwareAddress),0);
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackAdapter.ProcessAdapter:Boolean;
{Read the Packets from the Transport Buffer and send to Handler}
var
 Handle:THandle;
 Buffer:PLoopbackBuffer;
 Transport:TLoopbackAdapterTransport;
begin
 {}
 Result:=False;
 
 {Check Status}
 if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

 {Check Thread}
 if FThread = nil then Exit;
 
 {Wait for Handle}
 Handle:=FThread.ReceiveHandle;
 if Handle <> INVALID_HANDLE_VALUE then
  begin
   {$IFDEF LOOPBACK_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackAdapter: ProcessAdapter (' + Name + ')');
   {$ENDIF}
   
   {Get Transport}
   Transport:=TLoopbackAdapterTransport(GetTransportByHandle(Handle,True,NETWORK_LOCK_READ));
   if Transport = nil then Exit;
   try
    {Check Handler}
    if not(Assigned(Transport.PacketHandler)) then Exit;
    
    {Get Next Buffer}
    Buffer:=PLoopbackBuffer(Transport.Buffer.ReadNext);
    if Buffer = nil then Exit;

    {Call the Packet Handler}
    Transport.PacketHandler(THandle(Transport),@FHardwareAddress,@FHardwareAddress,@Buffer.Data,Buffer.Size,False); {No need to check for Broadcast}
          
    {Return Result}
    Result:=True;
   finally 
    Transport.ReaderUnlock;
   end; 
  end;
end;

{==============================================================================}
{==============================================================================}
{TLoopbackAdapterTransport}
constructor TLoopbackAdapterTransport.Create;
begin
 {}
 inherited Create;
 Buffer:=TAdapterBuffer.Create;
end;

{==============================================================================}

destructor TLoopbackAdapterTransport.Destroy;
begin
 {}
 WriterLock;
 try
  Buffer.Free;
 finally 
  WriterUnlock;
  inherited Destroy;
 end; 
end;
  
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure LoopbackInit;
begin
 {}
 {Check Initialized}
 if LoopbackInitialized then Exit;

 {Create Loopback Adapter}
 if LOOPBACK_NETWORK_ENABLED then
  begin
   TLoopbackAdapter.Create(AdapterManager,nil,'Loopback Adapter');
  end; 
 
 LoopbackInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{Loopback Functions}
  
{==============================================================================}
{==============================================================================}
{Loopback Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 LoopbackInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
  