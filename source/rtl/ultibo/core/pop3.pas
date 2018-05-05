{
Ultibo POP3 interface unit.

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

 RFC1939 - Post Office Protocol (Version 3) - https://www.ietf.org/rfc/rfc1939.txt
 
POP3
====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit POP3;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,UltiboClasses,Winsock2,Crypto,Authentication;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {POP3 specific constants}
 POP3_LINE_END = Chr(13) + Chr(10); {CR LF}
 
 POP3_BUFFER_SIZE = SIZE_4K;
 
 {POP3 Status constants}
 POP3_STATUS_NONE = 0;
 POP3_STATUS_CONN = 1;
 POP3_STATUS_USER = 2;
 POP3_STATUS_PASS = 3;
 POP3_STATUS_APOP = 4;
 POP3_STATUS_DATA = 5;
 POP3_STATUS_QUIT = 6;
 //To Do //Align these with states in RFC1939 (TRANSACTION / AUTHORIZATION etc)
 
 POP3_MAX_STATUS = 6;
 
 POP3_STATUS_STRINGS:array[0..POP3_MAX_STATUS] of String = (
  'None',
  'Connect',
  'Username',
  'Password',
  'Authenticate',
  'Data',
  'Disconnect');
 
 {POP3 Command constants}
 POP3_COMMAND_STAT = 'STAT';
 POP3_COMMAND_LIST = 'LIST';
 POP3_COMMAND_RETR = 'RETR';
 POP3_COMMAND_DELE = 'DELE';
 POP3_COMMAND_NOOP = 'NOOP';
 POP3_COMMAND_RSET = 'RSET';
 POP3_COMMAND_QUIT = 'QUIT';
 
 POP3_COMMAND_TOP  = 'TOP';
 POP3_COMMAND_UIDL = 'UIDL';
 POP3_COMMAND_USER = 'USER';
 POP3_COMMAND_PASS = 'PASS';
 POP3_COMMAND_APOP = 'APOP';
 
 {POP3 String constants}
 POP3_STRING_SUCCESS  = '+OK ';
 POP3_STRING_FAILURE  = '-ERR ';
 POP3_STRING_COMPLETE = '.';
 
 POP3_STRING_SIGNON = 'Ultibo POP3 server (version ' + ULTIBO_RELEASE_VERSION + ')';
 POP3_STRING_SIGNOFF = 'Ultibo POP3 server closing connection';
 POP3_STRING_EMPTY_STAT = '0 0';
 POP3_STRING_EMPTY_LIST = '0 messages (0 octets)';
 POP3_STRING_BAD_COMMAND = 'Bad command';
 POP3_STRING_BAD_MESSAGE = 'No such message';
 POP3_STRING_BAD_USERNAME = 'Invalid username or password';
 POP3_STRING_BAD_PASSWORD = 'Invalid username or password';
 POP3_STRING_NO_PERMISSION = 'Permission denied';
 
 {POP3 logging}
 POP3_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {POP3 debugging messages}
 POP3_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {POP3 informational messages}
 POP3_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {POP3 warning messages}
 POP3_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {POP3 error messages}
 POP3_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No POP3 messages}

var 
 POP3_DEFAULT_LOG_LEVEL:LongWord = POP3_LOG_LEVEL_DEBUG; {Minimum level for POP3 messages.  Only messages with level greater than or equal to this will be printed} 
 
var 
 {POP3 logging}
 POP3_LOG_ENABLED:Boolean; 
              
{==============================================================================}
{type}
 {POP3 specific types}

{==============================================================================}
type
 {POP3 specific classes}

 {Helper classes}
 TPOP3Buffer = class(TObject)
 public
  constructor Create(ASize:LongWord);
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle;
  
  FData:Pointer;
  FSize:LongWord;
  FCount:LongWord;
  FStart:LongWord;
  
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;

  function GetCount:LongWord;  
 public
  {Public Properties}
  property Count:LongWord read GetCount;
  
  {Public Methods}
  function ReadData:Char;
  function WriteData(AChar:Char):Boolean;
  
  function WriteLock(var ASize:LongWord):Pointer;
  function WriteUnlock(ACount:LongWord):Boolean;
 end;
 
 {Client classes}
 TPOP3ClientNotifyEvent = procedure(const ARequest:String) of Object;

 TPOP3Client = class(TWinsock2TCPClient)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FOnRequest:TPOP3ClientNotifyEvent;
  FOnReply:TPOP3ClientNotifyEvent;

  FOnRequestStart:TNotifyEvent;
  FOnRequestEnd:TNotifyEvent;

  FBuffer:TPOP3Buffer;        {Buffer for received data}
 protected
  {Internal Methods}
  function GetReply(var AReply:String):Boolean;
  function SendRequest(const ARequest:String):Boolean;
 public
  {Public Properties}
  property OnRequest:TPOP3ClientNotifyEvent read FOnRequest write FOnRequest;
  property OnReply:TPOP3ClientNotifyEvent read FOnReply write FOnReply;

  property OnRequestStart:TNotifyEvent read FOnRequestStart write FOnRequestStart;
  property OnRequestEnd:TNotifyEvent read FOnRequestEnd write FOnRequestEnd;
 
  property Buffer:TPOP3Buffer read FBuffer;
 
  {Public Methods}
  function DoConn(const AHost,APort:String;var AReply:String):Boolean;
  function DoStat(var AReply:String):Boolean;
  function DoList(const AMessage:String;var AReply:String):Boolean;
  function DoRetr(const AMessage:String;var AReply:String):Boolean;
  function DoDele(const AMessage:String;var AReply:String):Boolean;
  function DoNoop(var AReply:String):Boolean;
  function DoRset(var AReply:String):Boolean;
  function DoQuit(var AReply:String):Boolean;
  
  function DoTop(const AMessage,ACount:String;var AReply:String):Boolean;
  function DoUidl(const AMessage:String;var AReply:String):Boolean;
  function DoUser(const AUsername:String;var AReply:String):Boolean;
  function DoPass(const APassword:String;var AReply:String):Boolean;
  function DoApop(const AUsername,ADigest:String;var AReply:String):Boolean;
 end;
 
 {Server classes}
 TPOP3Connection = class(TListObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle;
  
  FHandle:LongWord;
  FRxByteCount:Int64;         {Bytes Recv Count from Connection}
  FTxByteCount:Int64;         {Bytes Sent Count to Connection}
  FRequestCount:Int64;        {Requests Recv Count from Connection}
  FReplyCount:Int64;          {Replies Sent Count to Connection}
  FRequestTime:TDateTime;     {Last Request Time}
  FReplyTime:TDateTime;       {Last Reply Time}
  FRemoteAddress:String;      {Address of Remote Client}
  FStatus:LongWord;           {None,Conn,User,Data etc}
  FUsername:String;           {Current Username}
  FPassword:String;           {Current Password}
  FTimestamp:String;          {Timestamp for APOP authentication}

  FThread:TThread;            {TWinsock2TCPServerThread}
  FBuffer:TPOP3Buffer;        {Buffer for received data}
  
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  procedure SetHandle(AHandle:LongWord);
  function GetRxByteCount:Int64;
  procedure SetRxByteCount(const ARxByteCount:Int64);
  function GetTxByteCount:Int64;
  procedure SetTxByteCount(const ATxByteCount:Int64);
  function GetRequestCount:Int64;
  procedure SetRequestCount(const ARequestCount:Int64);
  function GetReplyCount:Int64;
  procedure SetReplyCount(const AReplyCount:Int64);
  function GetRequestTime:TDateTime;
  procedure SetRequestTime(const ARequestTime:TDateTime);
  function GetReplyTime:TDateTime;
  procedure SetReplyTime(const AReplyTime:TDateTime);
  function GetRemoteAddress:String;
  procedure SetRemoteAddress(const ARemoteAddress:String);
  procedure SetStatus(AStatus:LongWord);
  function GetUsername:String;
  procedure SetUsername(const AUsername:String);
  function GetPassword:String;
  procedure SetPassword(const APassword:String);
  function GetTimestamp:String;
  procedure SetTimestamp(const ATimestamp:String);
  
  procedure SetThread(AThread:TThread);
 public
  {Public Properties}
  property Handle:LongWord read FHandle write SetHandle;
  property RxByteCount:Int64 read GetRxByteCount write SetRxByteCount;
  property TxByteCount:Int64 read GetTxByteCount write SetTxByteCount;
  property RequestCount:Int64 read GetRequestCount write SetRequestCount;
  property ReplyCount:Int64 read GetReplyCount write SetReplyCount;
  property RequestTime:TDateTime read GetRequestTime write SetRequestTime;
  property ReplyTime:TDateTime read GetReplyTime write SetReplyTime;
  property RemoteAddress:String read GetRemoteAddress write SetRemoteAddress;
  property Status:LongWord read FStatus write SetStatus;
  property Username:String read GetUsername write SetUsername;
  property Password:String read GetPassword write SetPassword;
  property Timestamp:String read GetTimestamp write SetTimestamp;
  
  property Thread:TThread read FThread  write SetThread;
  property Buffer:TPOP3Buffer read FBuffer;
  
  {Public Methods}
  procedure IncrementRxByteCount(const ARxByteCount:Int64);
  procedure IncrementTxByteCount(const ATxByteCount:Int64);
  procedure IncrementRequestCount;
  procedure IncrementReplyCount;
 end;
  
 TPOP3HostEvent = function(AConnection:TPOP3Connection):Boolean of Object;
 TPOP3CountEvent = function(AConnection:TPOP3Connection):Boolean of Object;
 TPOP3NotifyEvent = procedure(AConnection:TPOP3Connection;const ARequest:String) of Object;
 TPOP3RequestEvent = function(AConnection:TPOP3Connection;const ARequest:String;var AReply:String):Boolean of Object;
 TPOP3RequestExEvent = function(AConnection:TPOP3Connection;const ARequest,AData:String;var AReply:String):Boolean of Object;
 TPOP3ConnectionEvent = procedure(AConnection:TPOP3Connection) of Object;
 
 TPOP3Listener = class(TWinsock2TCPListener)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
  FOnConnected:TPOP3ConnectionEvent;
  FOnDisconnected:TPOP3ConnectionEvent;

  FOnCheckHost:TPOP3HostEvent;
  FOnCheckCount:TPOP3CountEvent;

  FOnRequest:TPOP3NotifyEvent;
  FOnReply:TPOP3NotifyEvent;

  FOnConn:TPOP3RequestEvent;
  FOnStat:TPOP3RequestEvent;
  FOnList:TPOP3RequestEvent;
  FOnRetr:TPOP3RequestEvent;
  FOnDele:TPOP3RequestEvent;
  FOnNoop:TPOP3RequestEvent;
  FOnRset:TPOP3RequestEvent;
  FOnQuit:TPOP3RequestEvent;
  
  FOnTop:TPOP3RequestExEvent;
  FOnUidl:TPOP3RequestEvent;
  FOnUser:TPOP3RequestEvent;
  FOnPass:TPOP3RequestEvent;
  FOnApop:TPOP3RequestExEvent;
 protected
  {Internal Methods}
  procedure DoConnect(AThread:TWinsock2TCPServerThread); override;
  procedure DoDisconnect(AThread:TWinsock2TCPServerThread); override;

  function DoCheckHost(AThread:TWinsock2TCPServerThread):Boolean; virtual;
  function DoCheckCount(AThread:TWinsock2TCPServerThread):Boolean; virtual;
  
  function DoExecute(AThread:TWinsock2TCPServerThread):Boolean; override;

  procedure DoConn(AThread:TWinsock2TCPServerThread);
  procedure DoStat(AThread:TWinsock2TCPServerThread);
  procedure DoList(AThread:TWinsock2TCPServerThread;const AMessage:String);
  procedure DoRetr(AThread:TWinsock2TCPServerThread;const AMessage:String);
  procedure DoDele(AThread:TWinsock2TCPServerThread;const AMessage:String);
  procedure DoNoop(AThread:TWinsock2TCPServerThread);
  procedure DoRset(AThread:TWinsock2TCPServerThread);
  procedure DoQuit(AThread:TWinsock2TCPServerThread);

  procedure DoTop(AThread:TWinsock2TCPServerThread;const AMessage,ACount:String);
  procedure DoUidl(AThread:TWinsock2TCPServerThread;const AMessage:String);
  procedure DoUser(AThread:TWinsock2TCPServerThread;const AUsername:String);
  procedure DoPass(AThread:TWinsock2TCPServerThread;const APassword:String);
  procedure DoApop(AThread:TWinsock2TCPServerThread;const AUsername,ADigest:String);
  
  function GetRequest(AThread:TWinsock2TCPServerThread;var ARequest:String):Boolean;
  function SendReply(AThread:TWinsock2TCPServerThread;const AReply:String):Boolean;
 public
  {Public Properties}
  property OnConnected:TPOP3ConnectionEvent read FOnConnected write FOnConnected;
  property OnDisconnected:TPOP3ConnectionEvent read FOnDisconnected write FOnDisconnected;

  property OnCheckHost:TPOP3HostEvent read FOnCheckHost write FOnCheckHost;
  property OnCheckCount:TPOP3CountEvent read FOnCheckCount write FOnCheckCount;

  property OnRequest:TPOP3NotifyEvent read FOnRequest write FOnRequest;
  property OnReply:TPOP3NotifyEvent read FOnReply write FOnReply;

  property OnConn:TPOP3RequestEvent read FOnConn write FOnConn;
  property OnStat:TPOP3RequestEvent read FOnStat write FOnStat;
  property OnList:TPOP3RequestEvent read FOnList write FOnList;
  property OnRetr:TPOP3RequestEvent read FOnRetr write FOnRetr;
  property OnDele:TPOP3RequestEvent read FOnDele write FOnDele;
  property OnNoop:TPOP3RequestEvent read FOnNoop write FOnNoop;
  property OnRset:TPOP3RequestEvent read FOnRset write FOnRset;
  property OnQuit:TPOP3RequestEvent read FOnQuit write FOnQuit;

  property OnTop:TPOP3RequestExEvent read FOnTop write FOnTop;
  property OnUidl:TPOP3RequestEvent read FOnUidl write FOnUidl;
  property OnUser:TPOP3RequestEvent read FOnUser write FOnUser;
  property OnPass:TPOP3RequestEvent read FOnPass write FOnPass;
  property OnApop:TPOP3RequestExEvent read FOnApop write FOnApop;
 end;
 
{==============================================================================}
{var}
 {POP3 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure POP3Init;

{==============================================================================}
{POP3 Functions}

{==============================================================================}
{POP3 Helper Functions}
procedure POP3Log(Level:LongWord;const AText:String);
procedure POP3LogInfo(const AText:String); inline;
procedure POP3LogWarn(const AText:String); inline;
procedure POP3LogError(const AText:String); inline;
procedure POP3LogDebug(const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {POP3 specific variables}
 POP3Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{TPOP3Buffer}
constructor TPOP3Buffer.Create(ASize:LongWord);
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FData:=nil;
 FSize:=ASize;
 FCount:=0;
 FStart:=0;
 
 if FSize <> 0 then FData:=GetMem(FSize);
end;

{==============================================================================}

destructor TPOP3Buffer.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FData <> nil then FreeMem(FData);
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TPOP3Buffer.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TPOP3Buffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TPOP3Buffer.GetCount:LongWord;  
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=FCount;
 
 ReleaseLock; 
end;

{==============================================================================}

function TPOP3Buffer.ReadData:Char;
begin
 {}
 Result:=#0;
 
 if not AcquireLock then Exit;
 try
  if FCount > 0 then
   begin
    {Read Char}
    Result:=Char(Pointer(PtrUInt(FData) + PtrUInt(FStart))^);
    
    {Update Start}
    FStart:=(FStart + 1) mod FSize;

    {Update Count}
    Dec(FCount);
   end; 
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TPOP3Buffer.WriteData(AChar:Char):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if FCount < FSize then
   begin
    {Write Char}
    Char(Pointer(PtrUInt(FData) + PtrUInt((FStart + FCount) mod FSize))^):=AChar;
    
    {Update Count}
    Inc(FCount);
   end; 
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TPOP3Buffer.WriteLock(var ASize:LongWord):Pointer;
{Lock the buffer and return a pointer to the next write}
begin
 {}
 {Setup Result}
 ASize:=0;
 Result:=nil;

 if not AcquireLock then Exit;
 
 if FCount < FSize then
  begin
   {Check Wraparound}
   if (FStart + FCount) >= FSize then
    begin
     {Get Size}
     ASize:=FStart - ((FStart + FCount) mod FSize);
     
     {Get Data}
     Result:=Pointer(PtrUInt(FData) + PtrUInt((FStart + FCount) mod FSize));
    end
   else
    begin
     {Get Size}
     ASize:=FSize - (FStart + FCount);
     
     {Get Data}
     Result:=Pointer(PtrUInt(FData) + PtrUInt((FStart + FCount) mod FSize));
    end;    
  end
 else
  begin
   ReleaseLock;
  end;  
end;

{==============================================================================}

function TPOP3Buffer.WriteUnlock(ACount:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if (FCount + ACount) <= FSize then
  begin
   {Update Count}
   Inc(FCount,ACount);
  end;
  
 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TPOP3Client}
constructor TPOP3Client.Create;
begin
 {}
 inherited Create;
 FBuffer:=TPOP3Buffer.Create(POP3_BUFFER_SIZE);
end;

{==============================================================================}

destructor TPOP3Client.Destroy; 
begin
 {}
 FBuffer.Free;
 inherited Destroy;
end;

{==============================================================================}

function TPOP3Client.GetReply(var AReply:String):Boolean;
var
 Value:Char;
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
begin
 {}
 Result:=False;
 AReply:='';

 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Client: GetReply');
 {$ENDIF}
 
 {Get Request}
 Completed:=False;
 while not(Completed) do
  begin
   {$IFDEF POP3_DEBUG}
   if POP3_LOG_ENABLED then POP3LogDebug('Client:  Buffer Count = ' + IntToStr(FBuffer.Count));
   {$ENDIF}
   
   {Read from Buffer}
   while FBuffer.Count > 0 do
    begin
     {Read Value}
     Value:=FBuffer.ReadData;
     
     {Check for CR LF}
     if not(Value in [#10,#13]) then
      begin
       AReply:=AReply + Value;
      end
     else
      begin
       {Check for LF}
       if Value = #10 then
        begin
         Completed:=True;
         Break;
        end;
      end;      
    end;
   
   {Check Completed}
   if Completed then Break;
   
   {Read from Socket}
   Data:=FBuffer.WriteLock(Size);
   if Data = nil then Exit;
   try
    {$IFDEF POP3_DEBUG}
    if POP3_LOG_ENABLED then POP3LogDebug('Client:  Buffer Write Size = ' + IntToStr(Size));
    {$ENDIF}

    Count:=0;
      
    {Read Available}
    if not ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;
    
    {$IFDEF POP3_DEBUG}
    if POP3_LOG_ENABLED then POP3LogDebug('Client:  Buffer Write Count = ' + IntToStr(Count));
    {$ENDIF}
   finally
    FBuffer.WriteUnlock(Count);
   end; 
  end;  
 
 {Reply Event}
 if Assigned(FOnReply) then
  begin
   FOnReply(AReply);
  end; 
   
 {Return Result}
 Result:=True; 
   
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Client:  Reply = ' + AReply);
 {$ENDIF}
end;
 
{==============================================================================}

function TPOP3Client.SendRequest(const ARequest:String):Boolean;
begin
 {}
 Result:=False;

 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Client: SendRequest');
 if POP3_LOG_ENABLED then POP3LogDebug('Client:  Request = ' + ARequest);
 {$ENDIF}
 
 {Send Request}
 if not WriteData(PChar(ARequest),Length(ARequest)) then Exit;
 
 {Send Line End}
 if not WriteData(PChar(POP3_LINE_END),Length(POP3_LINE_END)) then Exit;
 
 {Request Event}
 if Assigned(FOnRequest) then
  begin
   FOnRequest(ARequest);
  end;
  
 {Return Result}
 Result:=True; 
end;
 
{==============================================================================}

function TPOP3Client.DoConn(const AHost,APort:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoConn');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Host = ' + AHost);
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Port = ' + APort);
  {$ENDIF}
 
  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if Connected then Exit;
   
   {Set Host and Port}
   RemoteHost:=AHost;
   RemotePort:=StrToInt(APort);
   
   {Connect}
   if not Connect then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end
   else
    begin
     {Disconnect}
     Disconnect;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoStat(var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoStat');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_STAT) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoList(const AMessage:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoList');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Message = ' + AMessage);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_LIST + ' ' + AMessage) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoRetr(const AMessage:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoRetr');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Message = ' + AMessage);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_RETR + ' ' + AMessage) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoDele(const AMessage:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoDele');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Message = ' + AMessage);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_DELE + ' ' + AMessage) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoNoop(var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoNoop');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_NOOP) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoRset(var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoRset');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_RSET) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoQuit(var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoQuit');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_QUIT) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
    
   {Disconnect}
   Disconnect;   
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}
  
function TPOP3Client.DoTop(const AMessage,ACount:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoTop');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Message = ' + AMessage);
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Count = ' + ACount);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_TOP + ' ' + AMessage + ' ' + ACount) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoUidl(const AMessage:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoUidl');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Message = ' + AMessage);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_UIDL + ' ' + AMessage) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoUser(const AUsername:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoUser');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Username = ' + AUsername);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_USER + ' ' + AUsername) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoPass(const APassword:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoPass');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Password = ' + APassword);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_PASS + ' ' + APassword) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TPOP3Client.DoApop(const AUsername,ADigest:String;var AReply:String):Boolean;
{POP3 server will return +OK for success and -ERR for failure}
begin
 {}
 Result:=False;
 try
  {$IFDEF POP3_DEBUG}
  if POP3_LOG_ENABLED then POP3LogDebug('Client: DoApop');
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Username = ' + AUsername);
  if POP3_LOG_ENABLED then POP3LogDebug('Client:  Digest = ' + ADigest);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';
   
   {Check Connected}
   if not Connected then Exit;
   
   {Send Request}
   if not SendRequest(POP3_COMMAND_APOP + ' ' + AUsername + ' ' + ADigest) then Exit;
   
   {Get Reply}
   if not GetReply(AReply) then Exit;
   
   {Check Reply}
   if Uppercase(Copy(Trim(AReply),1,Length(Trim(POP3_STRING_SUCCESS)))) = Uppercase(Trim(POP3_STRING_SUCCESS)) then
    begin
     {Return Result}
     Result:=True;
    end;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{TPOP3Connection}
constructor TPOP3Connection.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FHandle:=LongWord(Self);
 FRxByteCount:=0;
 FTxByteCount:=0;
 FRequestCount:=0;
 FReplyCount:=0;
 FRequestTime:=Now;
 FReplyTime:=Now;
 FRemoteAddress:='';
 FStatus:=POP3_STATUS_NONE;
 FUsername:='';
 FPassword:='';
 FTimestamp:='';
 
 FThread:=nil;
 FBuffer:=TPOP3Buffer.Create(POP3_BUFFER_SIZE);
end;

{==============================================================================}

destructor TPOP3Connection.Destroy; 
begin
 {}
 AcquireLock;
 try
  FThread:=nil;
  FBuffer.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TPOP3Connection.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TPOP3Connection.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TPOP3Connection.SetHandle(AHandle:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FHandle:=AHandle;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetRxByteCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FRxByteCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetRxByteCount(const ARxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FRxByteCount:=ARxByteCount;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetTxByteCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FTxByteCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetTxByteCount(const ATxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FTxByteCount:=ATxByteCount;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetRequestCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FRequestCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetRequestCount(const ARequestCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FRequestCount:=ARequestCount;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetReplyCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FReplyCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetReplyCount(const AReplyCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FReplyCount:=AReplyCount;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetRequestTime:TDateTime;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FRequestTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetRequestTime(const ARequestTime:TDateTime);
begin
 {}
 if not AcquireLock then Exit;

 FRequestTime:=ARequestTime;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetReplyTime:TDateTime;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FReplyTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetReplyTime(const AReplyTime:TDateTime);
begin
 {}
 if not AcquireLock then Exit;

 FReplyTime:=AReplyTime;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetRemoteAddress:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FRemoteAddress;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetRemoteAddress(const ARemoteAddress:String);
begin
 {}
 if not AcquireLock then Exit;

 FRemoteAddress:=ARemoteAddress;
 UniqueString(FRemoteAddress);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetStatus(AStatus:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FStatus:=AStatus;

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetUsername:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FUsername;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetUsername(const AUsername:String);
begin
 {}
 if not AcquireLock then Exit;

 FUsername:=AUsername;
 UniqueString(FUsername);

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetPassword:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FPassword;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetPassword(const APassword:String);
begin
 {}
 if not AcquireLock then Exit;

 FPassword:=APassword;
 UniqueString(FPassword);

 ReleaseLock;
end;

{==============================================================================}

function TPOP3Connection.GetTimestamp:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FTimestamp;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.SetTimestamp(const ATimestamp:String);
begin
 {}
 if not AcquireLock then Exit;

 FTimestamp:=ATimestamp;
 UniqueString(FPassword);

 ReleaseLock;
end;

{==============================================================================}
 
procedure TPOP3Connection.SetThread(AThread:TThread);
begin
 {}
 if not AcquireLock then Exit;

 FThread:=AThread;

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.IncrementRxByteCount(const ARxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 Inc(FRxByteCount,ARxByteCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.IncrementTxByteCount(const ATxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 Inc(FTxByteCount,ATxByteCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.IncrementRequestCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FRequestCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TPOP3Connection.IncrementReplyCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FReplyCount);

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TPOP3Listener}
constructor TPOP3Listener.Create;
begin
 {}
 inherited Create;
 BoundPort:=POP3_PORT_DEFAULT;
 UseNagle:=False; {Nagle is not recommended for POP3 (Due to strict request reply nature of the protocol)}
end;

{==============================================================================}

procedure TPOP3Listener.DoConnect(AThread:TWinsock2TCPServerThread); 
var
 Connection:TPOP3Connection;
begin
 {}
 inherited DoConnect(AThread);
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoConnect');
 {$ENDIF}
 
 {Create Connection}
 Connection:=TPOP3Connection.Create;
 Connection.RemoteAddress:=AThread.Server.PeerAddress;
 Connection.Thread:=AThread;
 Connection.RequestTime:=Now;
 Connection.ReplyTime:=Now;
 Connection.Timestamp:=IntToStr(GetCurrentThreadId) + '.' + IntToStr(GetTickCount64) + '@' + LocalHost;
 
 {Update Thread}
 AThread.Data:=Connection;
 
 {Connected Event}
 if Assigned(FOnConnected) then
  begin
   FOnConnected(Connection);
  end;
 
 {Check Host Event}
 if not DoCheckHost(AThread) then
  begin
   {Refuse Connection}
   AThread.Server.Disconnect;
   Exit;
  end;
  
 {Check Count Event}
 if not DoCheckCount(AThread) then
  begin
   {Refuse Connection}
   AThread.Server.Disconnect;
   Exit;
  end;

 {Conn Event}  
 DoConn(AThread);
end;

{==============================================================================}

procedure TPOP3Listener.DoDisconnect(AThread:TWinsock2TCPServerThread); 
begin
 {}
 inherited DoDisconnect(AThread);
 
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoDisconnect');
 {$ENDIF}
 
 {Disconnected Event}
 if Assigned(FOnDisconnected) then
  begin
   FOnDisconnected(TPOP3Connection(AThread.Data));
  end;
end;

{==============================================================================}

function TPOP3Listener.DoCheckHost(AThread:TWinsock2TCPServerThread):Boolean; 
begin
 {}
 Result:=True;
 
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoCheckHost');
 {$ENDIF}
 
 {Check Host Event}
 if Assigned(FOnCheckHost) then
  begin
   Result:=FOnCheckHost(TPOP3Connection(AThread.Data));
  end;
end;

{==============================================================================}

function TPOP3Listener.DoCheckCount(AThread:TWinsock2TCPServerThread):Boolean; 
begin
 {}
 Result:=True;
 
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoCheckCount');
 {$ENDIF}
 
 {Check Count Event}
 if Assigned(FOnCheckCount) then
  begin
   Result:=FOnCheckCount(TPOP3Connection(AThread.Data));
  end;
end;

{==============================================================================}
  
function TPOP3Listener.DoExecute(AThread:TWinsock2TCPServerThread):Boolean; 
var
 Reply:String;
 Request:String;
 PosIdx:Integer;
 POP3Data:String;
 POP3Extra:String;
 POP3Command:String;
begin
 {}
 Result:=inherited DoExecute(AThread);
 if not Result then Exit;
 
 Result:=False;

 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoExecute');
 {$ENDIF}
 
 if AThread.Server.Connected then
  begin
   {Get Request}
   if not GetRequest(AThread,Request) then Exit;
   
   {Set Default Reply}
   Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_COMMAND;
 
   {Process Request}
   if Trim(Request) <> '' then
    begin
     if Length(Trim(Request)) >= 4 then  {TOP command is 3 characters but must be followed by two arguments}
      begin
       {Get POP3 Command}
       POP3Command:=Trim(Uppercase(Copy(Trim(Request),1,4)));
     
       {Get POP3 Data}
       POP3Data:=Trim(Copy(Trim(Request),Length(POP3Command) + 1,Length(Request)));
       POP3Extra:='';
       
       {Check POP3 Commands (Mandatory)}
       if POP3Command = POP3_COMMAND_STAT then
        begin
         {Stat Event}
         DoStat(AThread);
        end
       else if POP3Command = POP3_COMMAND_LIST then
        begin
         {List Event}
         DoList(AThread,POP3Data);
        end
       else if POP3Command = POP3_COMMAND_RETR then
        begin
         {Retr Event}
         DoRetr(AThread,POP3Data);
        end
       else if POP3Command = POP3_COMMAND_DELE then
        begin
         {Dele Event}
         DoDele(AThread,POP3Data);
        end
       else if POP3Command = POP3_COMMAND_NOOP then
        begin
         {Noop Event}
         DoNoop(AThread);
        end
       else if POP3Command = POP3_COMMAND_RSET then
        begin
         {Rset Event}
         DoRset(AThread);
        end
       else if POP3Command = POP3_COMMAND_QUIT then
        begin
         {Quit Event}
         DoQuit(AThread);
        end
       {Check POP3 Commands (Optional)}
       else if POP3Command = POP3_COMMAND_TOP then
        begin
         {Get POP3 Extra}
         PosIdx:=Pos(' ',POP3Data);
         if PosIdx > 0 then
          begin
           POP3Extra:=Copy(POP3Data,PosIdx + 1,Length(POP3Data));
           POP3Data:=Copy(POP3Data,1,PosIdx - 1);
          end;
         
         {Top Event}
         DoTop(AThread,POP3Data,POP3Extra);
        end
       else if POP3Command = POP3_COMMAND_UIDL then
        begin
         {Uidl Event}
         DoUidl(AThread,POP3Data);
        end
       else if POP3Command = POP3_COMMAND_USER then
        begin
         {User Event}
         DoUser(AThread,POP3Data);
        end
       else if POP3Command = POP3_COMMAND_PASS then
        begin
         {Pass Event}
         DoPass(AThread,POP3Data);
        end
       else if POP3Command = POP3_COMMAND_APOP then
        begin
         {Get POP3 Extra}
         PosIdx:=Pos(' ',POP3Data);
         if PosIdx > 0 then
          begin
           POP3Extra:=Copy(POP3Data,PosIdx + 1,Length(POP3Data));
           POP3Data:=Copy(POP3Data,1,PosIdx - 1);
          end;
        
         {Apop Event}
         DoApop(AThread,POP3Data,POP3Extra);
        end
       else
        begin
         {Unknown Command}
         if not SendReply(AThread,Reply) then Exit;
        end;
      end
     else
      begin
       {Invalid Command}
       if not SendReply(AThread,Reply) then Exit;
      end;
    end
   else
    begin
     {Invalid Command}
     if not SendReply(AThread,Reply) then Exit;
    end;
  
   Result:=True;
  end;
end;
  
{==============================================================================}

procedure TPOP3Listener.DoConn(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoConn');
 {$ENDIF}
 
 Request:='';
 
 {Set Default Reply}
 Reply:=POP3_STRING_SUCCESS + POP3_STRING_SIGNON;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);
 if Connection <> nil then 
  begin
   Connection.Status:=POP3_STATUS_CONN;
   
   {Add Timestamp}
   if Assigned(FOnApop) then Reply:=Reply + ' <' + Connection.Timestamp + '>';
  end; 
 
 {Conn Event}
 if Assigned(FOnConn) then
  begin
   if FOnConn(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Refuse Connection}
     AThread.Server.Disconnect;
    end;
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoStat(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoStat');
 {$ENDIF}
 
 Request:='';
 
 {Set Default Reply}
 Reply:=POP3_STRING_SUCCESS + POP3_STRING_EMPTY_STAT;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {Stat Event}
 if Assigned(FOnStat) then
  begin
   if FOnStat(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;
 
{==============================================================================}

procedure TPOP3Listener.DoList(AThread:TWinsock2TCPServerThread;const AMessage:String);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoList');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Message = ' + AMessage);
 {$ENDIF}
 
 Request:=AMessage;
 
 {Set Default Reply}
 Reply:=POP3_STRING_SUCCESS + POP3_STRING_EMPTY_LIST;
 if Length(Request) <> 0 then Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_MESSAGE;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {List Event}
 if Assigned(FOnList) then
  begin
   if FOnList(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoRetr(AThread:TWinsock2TCPServerThread;const AMessage:String);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoRetr');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Message = ' + AMessage);
 {$ENDIF}
 
 Request:=AMessage;
 
 {Set Default Reply}
 Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_MESSAGE;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {Retr Event}
 if Assigned(FOnRetr) then
  begin
   if FOnRetr(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoDele(AThread:TWinsock2TCPServerThread;const AMessage:String);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoDele');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Message = ' + AMessage);
 {$ENDIF}
 
 Request:=AMessage;
 
 {Set Default Reply}
 Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_MESSAGE;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {Dele Event}
 if Assigned(FOnDele) then
  begin
   if FOnDele(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoNoop(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoNoop');
 {$ENDIF}
 
 Request:='';
 
 {Set Default Reply}
 Reply:=POP3_STRING_SUCCESS;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {Noop Event}
 if Assigned(FOnNoop) then
  begin
   if FOnNoop(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoRset(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoRset');
 {$ENDIF}
 
 Request:='';
 
 {Set Default Reply}
 Reply:=POP3_STRING_SUCCESS;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {Rset Event}
 if Assigned(FOnRset) then
  begin
   if FOnRset(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoQuit(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoQuit');
 {$ENDIF}
 
 Request:='';
 
 {Set Default Reply}
 Reply:=POP3_STRING_SUCCESS + POP3_STRING_SIGNOFF;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=POP3_STATUS_QUIT;
  end;
  
 {Quit Event}  
 if Assigned(FOnQuit) then
  begin
   FOnQuit(Connection,Request,Reply);
   
   {Send Reply}
   if not SendReply(AThread,Reply) then Exit;
   
   {Close Connection}
   AThread.Server.Disconnect;
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
   
   {Close Connection}
   AThread.Server.Disconnect;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoTop(AThread:TWinsock2TCPServerThread;const AMessage,ACount:String);
var
 Data:String;
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoTop');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Message = ' + AMessage);
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Count = ' + ACount);
 {$ENDIF}
 
 Request:=AMessage;
 Data:=ACount;
 
 {Set Default Reply}
 Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_MESSAGE;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {Top Event}
 if Assigned(FOnTop) then
  begin
   if FOnTop(Connection,Request,Data,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoUidl(AThread:TWinsock2TCPServerThread;const AMessage:String);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoUidl');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Message = ' + AMessage);
 {$ENDIF}
 
 Request:=AMessage;
 
 {Set Default Reply}
 Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_MESSAGE;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);

 {Uidl Event}
 if Assigned(FOnUidl) then
  begin
   if FOnUidl(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;


{==============================================================================}

procedure TPOP3Listener.DoUser(AThread:TWinsock2TCPServerThread;const AUsername:String);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoUser');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Username = ' + AUsername);
 {$ENDIF}
 
 Request:=AUsername;
 
 {Set Default Reply}
 Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_USERNAME;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=POP3_STATUS_USER;
   Connection.Username:=AUsername;
  end; 
 
 {User Event}
 if Assigned(FOnUser) then
  begin
   if FOnUser(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoPass(AThread:TWinsock2TCPServerThread;const APassword:String);
var
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoPass');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Password = ' + APassword);
 {$ENDIF}
 
 Request:=APassword;
 
 {Set Default Reply}
 Reply:=POP3_STRING_FAILURE + POP3_STRING_BAD_PASSWORD;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=POP3_STATUS_PASS;
   Connection.Password:=APassword;
  end; 

 {Pass Event}
 if Assigned(FOnPass) then
  begin
   if FOnPass(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
     
     {Update Connection}
     if Connection <> nil then
      begin
       Connection.Status:=POP3_STATUS_DATA;
      end;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TPOP3Listener.DoApop(AThread:TWinsock2TCPServerThread;const AUsername,ADigest:String);
var
 Data:String;
 Reply:String;
 Request:String;
 Connection:TPOP3Connection;
begin
 {}
 if AThread = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: DoApop');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Username = ' + AUsername);
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Digest = ' + ADigest);
 {$ENDIF}
 
 Request:=AUsername;
 Data:=ADigest;
 
 {Set Default Reply}
 Reply:=POP3_STRING_FAILURE + POP3_STRING_NO_PERMISSION;
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=POP3_STATUS_APOP;
   Connection.Username:=AUsername;
  end; 

 {Apop Event}
 if Assigned(FOnApop) then
  begin
   if FOnApop(Connection,Request,Data,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;
    end;    
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

function TPOP3Listener.GetRequest(AThread:TWinsock2TCPServerThread;var ARequest:String):Boolean;
var
 Value:Char;
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
 Connection:TPOP3Connection;
begin
 {}
 Result:=False;
 ARequest:='';
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: GetRequest');
 {$ENDIF}
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);
 if Connection = nil then Exit;
 
 {Get Request}
 Completed:=False;
 while not(Completed) do
  begin
   {$IFDEF POP3_DEBUG}
   if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Buffer Count = ' + IntToStr(Connection.Buffer.Count));
   {$ENDIF}
   
   {Read from Buffer}
   while Connection.Buffer.Count > 0 do
    begin
     {Read Value}
     Value:=Connection.Buffer.ReadData;
     
     {Check for CR LF}
     if not(Value in [#10,#13]) then
      begin
       ARequest:=ARequest + Value;
      end
     else
      begin
       {Check for LF}
       if Value = #10 then
        begin
         Completed:=True;
         Break;
        end;
      end;      
    end;
   
   {Check Completed}
   if Completed then Break;
   
   {Read from Socket}
   Data:=Connection.Buffer.WriteLock(Size);
   if Data = nil then Exit;
   try
    {$IFDEF POP3_DEBUG}
    if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Buffer Write Size = ' + IntToStr(Size));
    {$ENDIF}

    Count:=0;
      
    {Read Available}
    if not AThread.Server.ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;
    
    {$IFDEF POP3_DEBUG}
    if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Buffer Write Count = ' + IntToStr(Count));
    {$ENDIF}
   finally
    Connection.Buffer.WriteUnlock(Count);
   end; 
  end;  
 
 {Update Connection}
 Connection.RequestTime:=Now;
 Connection.IncrementRequestCount;
 Connection.IncrementRxByteCount(Length(ARequest));
 
 {Request Event}
 if Assigned(FOnRequest) then
  begin
   FOnRequest(Connection,ARequest);
  end;
  
 {Return Result}
 Result:=True; 
  
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Request = ' + ARequest);
 {$ENDIF}
end;

{==============================================================================}

function TPOP3Listener.SendReply(AThread:TWinsock2TCPServerThread;const AReply:String):Boolean;
var
 Connection:TPOP3Connection;
begin
 {}
 Result:=False;
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF POP3_DEBUG}
 if POP3_LOG_ENABLED then POP3LogDebug('Listener: SendReply');
 if POP3_LOG_ENABLED then POP3LogDebug('Listener:  Reply = ' + AReply);
 {$ENDIF}
 
 {Get Connection}
 Connection:=TPOP3Connection(AThread.Data);
 if Connection = nil then Exit;

 {Send Reply}
 if not AThread.Server.WriteData(PChar(AReply),Length(AReply)) then Exit;
 
 {Send Line End}
 if not AThread.Server.WriteData(PChar(POP3_LINE_END),Length(POP3_LINE_END)) then Exit;
 
 {Update Connection}
 Connection.ReplyTime:=Now;
 Connection.IncrementReplyCount;
 Connection.IncrementTxByteCount(Length(AReply));
 
 {Reply Event}
 if Assigned(FOnReply) then
  begin
   FOnReply(Connection,AReply);
  end;
  
 {Return Result}
 Result:=True; 
end;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure POP3Init;
begin
 {}
 {Check Initialized}
 if POP3Initialized then Exit;
 
 {Initialize Logging}
 POP3_LOG_ENABLED:=(POP3_DEFAULT_LOG_LEVEL <> POP3_LOG_LEVEL_NONE); 
 
 POP3Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{POP3 Functions}

{==============================================================================}
{==============================================================================}
{POP3 Helper Functions}
procedure POP3Log(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < POP3_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = POP3_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = POP3_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = POP3_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'POP3: ';
 
 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_POP,LogLevelToLoggingSeverity(Level),'POP3',WorkBuffer + AText);
end;

{==============================================================================}

procedure POP3LogInfo(const AText:String); inline;
begin
 {}
 POP3Log(POP3_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure POP3LogWarn(const AText:String); inline;
begin
 {}
 POP3Log(POP3_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure POP3LogError(const AText:String); inline;
begin
 {}
 POP3Log(POP3_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure POP3LogDebug(const AText:String); inline;
begin
 {}
 POP3Log(POP3_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 POP3Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.


