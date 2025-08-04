{
Ultibo SMTP interface unit.

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


SMTP
====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SMTP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,UltiboClasses,UltiboUtils,Winsock2,Crypto,Authentication;

//To Do //Look for:

//Remove

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {SMTP specific constants}
 SMTP_LINE_END = Chr(13) + Chr(10); {CR LF}

 SMTP_BUFFER_SIZE = SIZE_4K;

 {SMTP Status constants}
 SMTP_STATUS_NONE = 0;
 SMTP_STATUS_CONN = 1;
 SMTP_STATUS_HELO = 2;
 SMTP_STATUS_AUTH = 3;
 SMTP_STATUS_MAIL = 4;
 SMTP_STATUS_RCPT = 5;
 SMTP_STATUS_DATA = 6;
 SMTP_STATUS_QUIT = 7;

 SMTP_MAX_STATUS = 7;

 SMTP_STATUS_STRINGS:array[0..SMTP_MAX_STATUS] of String = (
  'None',
  'Connect',
  'Hello',
  'Authenticate',
  'Mail From',
  'Rcpt To',
  'Data',
  'Quit');

 {SMTP Command constants}
 SMTP_COMMAND_HELO       = 'HELO';
 SMTP_COMMAND_EHLO       = 'EHLO';
 SMTP_COMMAND_TIME       = 'TIME';
 SMTP_COMMAND_NOOP       = 'NOOP';
 SMTP_COMMAND_AUTH       = 'AUTH';
 SMTP_COMMAND_MAIL       = 'MAIL';
 SMTP_COMMAND_RCPT       = 'RCPT';
 SMTP_COMMAND_VRFY       = 'VRFY';
 SMTP_COMMAND_BEGIN_DATA = 'DATA';
 SMTP_COMMAND_END_DATA   = '.';
 SMTP_COMMAND_RSET       = 'RSET';
 SMTP_COMMAND_QUIT       = 'QUIT';

 {SMTP Sub Command constants}
 SMTP_SUB_COMMAND_MAIL_FROM  = 'FROM:';
 SMTP_SUB_COMMAND_RCPT_TO    = 'TO:';

 {SMTP String constants}
 SMTP_STRING_CONTINUE           = '-';

 SMTP_STRING_CONN_SUCCESS       = '220 ';
 SMTP_STRING_QUIT_SUCCESS       = '221 ';
 SMTP_STRING_HELO_SUCCESS       = '250 ';
 SMTP_STRING_EHLO_SUCCESS       = '250 ';
 SMTP_STRING_TIME_SUCCESS       = '250 ';
 SMTP_STRING_NOOP_SUCCESS       = '250 ';
 SMTP_STRING_CONT_SUCCESS       = '250-';
 SMTP_STRING_AUTH_SUCCESS       = '235 ';
 SMTP_STRING_AUTH_CHALLENGE     = '334 ';
 SMTP_STRING_MAIL_SUCCESS       = '250 ';
 SMTP_STRING_RCPT_SUCCESS       = '250 ';
 SMTP_STRING_VRFY_SUCCESS       = '250 ';
 SMTP_STRING_BEGIN_DATA_SUCCESS = '354 ';
 SMTP_STRING_END_DATA_SUCCESS   = '250 ';
 SMTP_STRING_RSET_SUCCESS       = '250 ';

 SMTP_STRING_WRITE_FAILURE      = '451 ';
 SMTP_STRING_COMMAND_FAILURE    = '500 ';
 SMTP_STRING_SYNTAX_FAILURE     = '501 ';
 SMTP_STRING_LOGON_FAILURE      = '501 ';
 SMTP_STRING_SEQUENCE_FAILURE   = '503 ';
 SMTP_STRING_AUTH_FAILURE       = '504 ';
 SMTP_STRING_RCPT_FAILURE       = '553 ';
 SMTP_STRING_VRFY_FAILURE       = '550 ';
 SMTP_STRING_SIZE_FAILURE       = '552 ';
 SMTP_STRING_FORWARD_FAILURE    = '551 ';
 SMTP_STRING_PERMISSION_FAILURE = '571 ';

 SMTP_STRING_CONN_OK           = 'Ultibo SMTP server (version ' + ULTIBO_RELEASE_VERSION + ')';
 SMTP_STRING_HELO_OK           = 'Ultibo SMTP server - Hello, ';
 SMTP_STRING_EHLO_HEAD_OK      = 'Ultibo SMTP server - Hello ';
 SMTP_STRING_EHLO_TAIL_OK      = '; ESMTPs are: ';
 SMTP_STRING_AUTH_OK           = 'Authenticated OK';
 SMTP_STRING_MAIL_OK           = 'Sender OK - send RCPTs';
 SMTP_STRING_MAIL_SIZE_HEAD_OK = 'Sender and size (';
 SMTP_STRING_MAIL_SIZE_TAIL_OK = ') OK - send RCPTs';
 SMTP_STRING_RCPT_OK           = 'Recipient OK - send RCPT or DATA';
 SMTP_STRING_VRFY_OK           = 'OK - ';
 SMTP_STRING_BEGIN_DATA_OK     = 'OK, send data, end with CRLF.CRLF';
 SMTP_STRING_END_DATA_OK       = 'Data received OK';
 SMTP_STRING_RSET_OK           = 'Command processed OK';
 SMTP_STRING_QUIT_OK           = 'Ultibo SMTP server closing connection';
 SMTP_STRING_TIME_OK           = 'Time is ';
 SMTP_STRING_NOOP_OK           = 'OK';

 SMTP_STRING_BAD_COMMAND       = 'Command unrecognized';
 SMTP_STRING_BAD_SYNTAX        = 'Bad syntax in command';
 SMTP_STRING_BAD_SEQUENCE      = 'Bad sequence of commands';
 SMTP_STRING_BAD_AUTH          = 'Unrecognized authentication type';
 SMTP_STRING_BAD_LOGON         = 'Authentication failed';
 SMTP_STRING_BAD_MAIL          = 'Unknown sender';
 SMTP_STRING_BAD_RCPT          = 'Unknown recipient';
 SMTP_STRING_BAD_VRFY          = 'Mailbox not found';

 {SMTP logging}
 SMTP_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {SMTP debugging messages}
 SMTP_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {SMTP informational messages}
 SMTP_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {SMTP warning messages}
 SMTP_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {SMTP error messages}
 SMTP_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No SMTP messages}

var
 SMTP_DEFAULT_LOG_LEVEL:LongWord = SMTP_LOG_LEVEL_DEBUG; {Minimum level for SMTP messages.  Only messages with level greater than or equal to this will be printed}

var
 {SMTP logging}
 SMTP_LOG_ENABLED:Boolean;

{==============================================================================}
{type}
 {SMTP specific types}

{==============================================================================}
type
 {SMTP specific classes}

 {Helper classes}
 TSMTPBuffer = class(TObject)
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
 TSMTPClientNotifyEvent = procedure(const ARequest:String) of Object;

 TSMTPClient = class(TWinsock2TCPClient)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FOnRequest:TSMTPClientNotifyEvent;
  FOnReply:TSMTPClientNotifyEvent;

  FOnRequestStart:TNotifyEvent;
  FOnRequestEnd:TNotifyEvent;

  FAuthenticated:Boolean;
  FBuffer:TSMTPBuffer;        {Buffer for received data}
 protected
  {Internal Methods}
  function GetReply(var AReply:String):Boolean;
  function SendRequest(const ARequest:String):Boolean;
 public
  {Public Properties}
  property OnRequest:TSMTPClientNotifyEvent read FOnRequest write FOnRequest;
  property OnReply:TSMTPClientNotifyEvent read FOnReply write FOnReply;

  property OnRequestStart:TNotifyEvent read FOnRequestStart write FOnRequestStart;
  property OnRequestEnd:TNotifyEvent read FOnRequestEnd write FOnRequestEnd;

  property Authenticated:Boolean read FAuthenticated;
  property Buffer:TSMTPBuffer read FBuffer;

  {Public Methods}
  function DoConn(const AHost,APort:String;var AReply:String):Boolean;
  function DoHelo(const AHost:String;var AReply:String):Boolean;
  function DoEhlo(const AHost:String;var AReply:String):Boolean;
  function DoAuth(const AProtocol,AUsername,APassword:String;var AReply:String):Boolean;
  function DoMail(const ASender:String;var AReply:String):Boolean;
  function DoRcpt(const AReceiver:String;var AReply:String):Boolean;
  function DoVrfy(const AAddress:String;var AReply:String):Boolean;
  function DoBeginData(var AReply:String):Boolean;
  function DoSendData(const AData:String):Boolean;
  function DoEndData(var AReply:String):Boolean;
  function DoQuit(var AReply:String):Boolean;
  function DoRset(var AReply:String):Boolean;
  function DoTime(var AReply:String):Boolean;
  function DoNoop(var AReply:String):Boolean;
 end;

 {Server classes}
 TSMTPConnection = class(TListObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle;

  FHandle:THandle;
  FRxByteCount:Int64;         {Bytes Recv Count from Connection}
  FTxByteCount:Int64;         {Bytes Sent Count to Connection}
  FRequestCount:Int64;        {Requests Recv Count from Connection}
  FReplyCount:Int64;          {Replies Sent Count to Connection}
  FRequestTime:TDateTime;     {Last Request Time}
  FReplyTime:TDateTime;       {Last Reply Time}
  FRemoteAddress:String;      {Address of Remote Client}
  FStatus:LongWord;           {None,Conn,Auth,Helo,Mail,Rcpt,Data etc}

  FAuthenticated:Boolean;     {Current authentication state}
  FUsername:String;           {Current Username}
  FPassword:String;           {Current Password}

  FThread:TThread;            {TWinsock2TCPServerThread}
  FBuffer:TSMTPBuffer;        {Buffer for received data}

  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;

  procedure SetHandle(AHandle:THandle);
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

  procedure SetAuthenticated(AAuthenticated:Boolean);
  function GetUsername:String;
  procedure SetUsername(const AUsername:String);
  function GetPassword:String;
  procedure SetPassword(const APassword:String);

  procedure SetThread(AThread:TThread);
 public
  {Public Properties}
  property Handle:THandle read FHandle write SetHandle;
  property RxByteCount:Int64 read GetRxByteCount write SetRxByteCount;
  property TxByteCount:Int64 read GetTxByteCount write SetTxByteCount;
  property RequestCount:Int64 read GetRequestCount write SetRequestCount;
  property ReplyCount:Int64 read GetReplyCount write SetReplyCount;
  property RequestTime:TDateTime read GetRequestTime write SetRequestTime;
  property ReplyTime:TDateTime read GetReplyTime write SetReplyTime;
  property RemoteAddress:String read GetRemoteAddress write SetRemoteAddress;
  property Status:LongWord read FStatus write SetStatus;

  property Authenticated:Boolean read FAuthenticated write SetAuthenticated;
  property Username:String read GetUsername write SetUsername;
  property Password:String read GetPassword write SetPassword;

  property Thread:TThread read FThread  write SetThread;
  property Buffer:TSMTPBuffer read FBuffer;

  {Public Methods}
  procedure IncrementRxByteCount(const ARxByteCount:Int64);
  procedure IncrementTxByteCount(const ATxByteCount:Int64);
  procedure IncrementRequestCount;
  procedure IncrementReplyCount;
 end;

 TSMTPHostEvent = function(AConnection:TSMTPConnection):Boolean of Object;
 TSMTPCountEvent = function(AConnection:TSMTPConnection):Boolean of Object;
 TSMTPNotifyEvent = procedure(AConnection:TSMTPConnection;const ARequest:String) of Object;
 TSMTPRequestEvent = function(AConnection:TSMTPConnection;const ARequest:String;var AReply:String):Boolean of Object;
 TSMTPAuthenticateEvent = function(AConnection:TSMTPConnection;const AMethod,AUsername,APassword:String;var AReply:String):Boolean of Object;
 TSMTPConnectionEvent = procedure(AConnection:TSMTPConnection) of Object;

 TSMTPListener = class(TWinsock2TCPListener)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
  FOnConnected:TSMTPConnectionEvent;
  FOnDisconnected:TSMTPConnectionEvent;

  FOnCheckHost:TSMTPHostEvent;
  FOnCheckCount:TSMTPCountEvent;

  FOnRequest:TSMTPNotifyEvent;
  FOnReply:TSMTPNotifyEvent;

  FOnConn:TSMTPRequestEvent;
  FOnHelo:TSMTPRequestEvent;
  FOnEhlo:TSMTPRequestEvent;
  FOnAuth:TSMTPAuthenticateEvent;
  FOnMail:TSMTPRequestEvent;
  FOnRcpt:TSMTPRequestEvent;
  FOnVrfy:TSMTPRequestEvent;
  FOnBeginData:TSMTPRequestEvent;
  FOnData:TSMTPRequestEvent;
  FOnEndData:TSMTPRequestEvent;
  FOnQuit:TSMTPRequestEvent;
  FOnRset:TSMTPRequestEvent;
  FOnTime:TSMTPRequestEvent;
  FOnNoop:TSMTPRequestEvent;
 protected
  {Internal Methods}
  procedure DoConnect(AThread:TWinsock2TCPServerThread); override;
  procedure DoDisconnect(AThread:TWinsock2TCPServerThread); override;

  function DoCheckHost(AThread:TWinsock2TCPServerThread):Boolean; virtual;
  function DoCheckCount(AThread:TWinsock2TCPServerThread):Boolean; virtual;

  function DoExecute(AThread:TWinsock2TCPServerThread):Boolean; override;

  procedure DoConn(AThread:TWinsock2TCPServerThread);
  procedure DoHelo(AThread:TWinsock2TCPServerThread;const AHost:String);
  procedure DoEhlo(AThread:TWinsock2TCPServerThread;const AHost:String);
  procedure DoAuth(AThread:TWinsock2TCPServerThread;const AParams:String);
  procedure DoMail(AThread:TWinsock2TCPServerThread;const ASender:String);
  procedure DoRcpt(AThread:TWinsock2TCPServerThread;const ARecipient:String);
  procedure DoVrfy(AThread:TWinsock2TCPServerThread;const AAddress:String);
  procedure DoBeginData(AThread:TWinsock2TCPServerThread);
  procedure DoData(AThread:TWinsock2TCPServerThread;const AData:String);
  procedure DoEndData(AThread:TWinsock2TCPServerThread);
  procedure DoQuit(AThread:TWinsock2TCPServerThread);
  procedure DoRset(AThread:TWinsock2TCPServerThread);
  procedure DoTime(AThread:TWinsock2TCPServerThread);
  procedure DoNoop(AThread:TWinsock2TCPServerThread);

  function GetRequest(AThread:TWinsock2TCPServerThread;var ARequest:String):Boolean;
  function SendReply(AThread:TWinsock2TCPServerThread;const AReply:String):Boolean;
 public
  {Public Properties}
  property OnConnected:TSMTPConnectionEvent read FOnConnected write FOnConnected;
  property OnDisconnected:TSMTPConnectionEvent read FOnDisconnected write FOnDisconnected;

  property OnCheckHost:TSMTPHostEvent read FOnCheckHost write FOnCheckHost;
  property OnCheckCount:TSMTPCountEvent read FOnCheckCount write FOnCheckCount;

  property OnRequest:TSMTPNotifyEvent read FOnRequest write FOnRequest;
  property OnReply:TSMTPNotifyEvent read FOnReply write FOnReply;

  property OnConn:TSMTPRequestEvent read FOnConn write FOnConn;
  property OnHelo:TSMTPRequestEvent read FOnHelo write FOnHelo;
  property OnEhlo:TSMTPRequestEvent read FOnEhlo write FOnEhlo;
  property OnAuth:TSMTPAuthenticateEvent read FOnAuth write FOnAuth;
  property OnMail:TSMTPRequestEvent read FOnMail write FOnMail;
  property OnRcpt:TSMTPRequestEvent read FOnRcpt write FOnRcpt;
  property OnVrfy:TSMTPRequestEvent read FOnVrfy write FOnVrfy;
  property OnBeginData:TSMTPRequestEvent read FOnBeginData write FOnBeginData;
  property OnData:TSMTPRequestEvent read FOnData write FOnData;
  property OnEndData:TSMTPRequestEvent read FOnEndData write FOnEndData;
  property OnQuit:TSMTPRequestEvent read FOnQuit write FOnQuit;
  property OnRset:TSMTPRequestEvent read FOnRset write FOnRset;
  property OnTime:TSMTPRequestEvent read FOnTime write FOnTime;
  property OnNoop:TSMTPRequestEvent read FOnNoop write FOnNoop;
 end;

{==============================================================================}
{var}
 {SMTP specific variables}

{==============================================================================}
{Initialization Functions}
procedure SMTPInit;

{==============================================================================}
{SMTP Functions}

{==============================================================================}
{SMTP Helper Functions}
procedure SMTPLog(Level:LongWord;const AText:String);
procedure SMTPLogInfo(const AText:String); inline;
procedure SMTPLogWarn(const AText:String); inline;
procedure SMTPLogError(const AText:String); inline;
procedure SMTPLogDebug(const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {SMTP specific variables}
 SMTPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TSMTPBuffer}
constructor TSMTPBuffer.Create(ASize:LongWord);
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

destructor TSMTPBuffer.Destroy;
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

function TSMTPBuffer.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSMTPBuffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSMTPBuffer.GetCount:LongWord;
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;

 Result:=FCount;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPBuffer.ReadData:Char;
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

function TSMTPBuffer.WriteData(AChar:Char):Boolean;
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

function TSMTPBuffer.WriteLock(var ASize:LongWord):Pointer;
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

function TSMTPBuffer.WriteUnlock(ACount:LongWord):Boolean;
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
{TSMTPClient}
constructor TSMTPClient.Create;
begin
 {}
 inherited Create;
 FBuffer:=TSMTPBuffer.Create(SMTP_BUFFER_SIZE);
end;

{==============================================================================}

destructor TSMTPClient.Destroy;
begin
 {}
 FBuffer.Free;
 inherited Destroy;
end;

{==============================================================================}

function TSMTPClient.GetReply(var AReply:String):Boolean;
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

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Client: GetReply');
 {$ENDIF}

 {Get Request}
 Completed:=False;
 while not(Completed) do
  begin
   {$IFDEF SMTP_DEBUG}
   if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Buffer Count = ' + IntToStr(FBuffer.Count));
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
    {$IFDEF SMTP_DEBUG}
    if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Buffer Write Size = ' + IntToStr(Size));
    {$ENDIF}

    Count:=0;

    {Read Available}
    if not ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;

    {$IFDEF SMTP_DEBUG}
    if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Buffer Write Count = ' + IntToStr(Count));
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

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Reply = ' + AReply);
 {$ENDIF}
end;

{==============================================================================}

function TSMTPClient.SendRequest(const ARequest:String):Boolean;
begin
 {}
 Result:=False;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Client: SendRequest');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Request = ' + ARequest);
 {$ENDIF}

 {Send Request}
 if not WriteData(PChar(ARequest),Length(ARequest)) then Exit;

 {Send Line End}
 if not WriteData(PChar(SMTP_LINE_END),Length(SMTP_LINE_END)) then Exit;

 {Request Event}
 if Assigned(FOnRequest) then
  begin
   FOnRequest(ARequest);
  end;

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TSMTPClient.DoConn(const AHost,APort:String;var AReply:String):Boolean;
{SMTP server will normally return 220 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoConn');
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Host = ' + AHost);
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Port = ' + APort);
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

   FAuthenticated:=False;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_CONN_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_CONN_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end
     else
      begin
       {Disconnect}
       Disconnect;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_CONN_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_CONN_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end
     else
      begin
       {Disconnect}
       Disconnect;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoHelo(const AHost:String;var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoHelo');
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Host = ' + AHost);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_HELO + ' ' + AHost) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_HELO_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_HELO_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_HELO_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_HELO_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoEhlo(const AHost:String;var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoEhlo');
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Host = ' + AHost);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_EHLO + ' ' + AHost) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_EHLO_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_EHLO_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_EHLO_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_EHLO_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoAuth(const AProtocol,AUsername,APassword:String;var AReply:String):Boolean;
{SMTP server will normally return 235 for success and other for failure (334 for challenge during handshake)}
begin
 {}
 //To do //Does this need line continuation support ? - Yes Probably
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoAuth');
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Protocol = ' + AProtocol);
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Username = ' + AUsername);
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Password = ' + APassword);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Check Authenticated}
   if not FAuthenticated then
    begin
     {Send Request}
     if not SendRequest(SMTP_COMMAND_AUTH + ' ' + Uppercase(AProtocol)) then Exit;

     {Get Reply}
     if not GetReply(AReply) then Exit;

     {Check Reply}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_AUTH_CHALLENGE)))) = Uppercase(Trim(SMTP_STRING_AUTH_CHALLENGE)) then
      begin
       {Send Username}
       if not SendRequest(Base64EncodeString(AUsername)) then Exit;

       {Get Reply}
       if not GetReply(AReply) then Exit;

       {Check Reply}
       if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_AUTH_CHALLENGE)))) = Uppercase(Trim(SMTP_STRING_AUTH_CHALLENGE)) then
        begin
         {Send Password}
         if not SendRequest(Base64EncodeString(APassword)) then Exit;

         {Get Reply}
         if not GetReply(AReply) then Exit;

         {Check Reply}
         if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_AUTH_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_AUTH_SUCCESS)) then
          begin
           FAuthenticated:=True;

           {Return Result}
           Result:=True;
          end;
        end;
      end;
    end
   else
    begin
     {Already Authenticated}
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

function TSMTPClient.DoMail(const ASender:String;var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoMail');
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Sender = ' + ASender);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_MAIL + ' ' + SMTP_SUB_COMMAND_MAIL_FROM + ' ' + ASender) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_MAIL_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_MAIL_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_MAIL_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_MAIL_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoRcpt(const AReceiver:String;var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoRcpt');
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Receiver = ' + AReceiver);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_RCPT + ' ' + SMTP_SUB_COMMAND_RCPT_TO + ' ' + AReceiver) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_RCPT_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_RCPT_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_RCPT_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_RCPT_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoVrfy(const AAddress:String;var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoVrfy');
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client:  Address = ' + AAddress);
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_VRFY + ' ' + AAddress) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_VRFY_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_VRFY_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_VRFY_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_VRFY_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoBeginData(var AReply:String):Boolean;
{SMTP server will normally return 354 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoBeginData');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_BEGIN_DATA) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_BEGIN_DATA_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_BEGIN_DATA_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_BEGIN_DATA_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_BEGIN_DATA_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoSendData(const AData:String):Boolean;
{SMTP server will not return anything while until data end}
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoSendData');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(AData) then Exit;

   {Return Result}
   Result:=True;
  finally
   {Request End Event}
   if Assigned(FOnRequestEnd) then FOnRequestEnd(Self);
  end;
 except
  {}
 end;
end;

{==============================================================================}

function TSMTPClient.DoEndData(var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoEndData');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_END_DATA) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_END_DATA_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_END_DATA_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_END_DATA_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_END_DATA_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoQuit(var AReply:String):Boolean;
{SMTP server will normally return 221 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoQuit');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_QUIT) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_QUIT_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_QUIT_SUCCESS)) then
      begin
       FAuthenticated:=False;

       {Return Result}
       Result:=True;
      end;

     {Disconnect}
     Disconnect;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_QUIT_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_QUIT_SUCCESS)) then
      begin
       FAuthenticated:=False;

       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;

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

function TSMTPClient.DoRset(var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoRset');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_RSET) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_RSET_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_RSET_SUCCESS)) then
      begin
       FAuthenticated:=False;

       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_RSET_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_RSET_SUCCESS)) then
      begin
       FAuthenticated:=False;

       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoTime(var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoTime');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_TIME) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_TIME_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_TIME_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_TIME_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_TIME_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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

function TSMTPClient.DoNoop(var AReply:String):Boolean;
{SMTP server will normally return 250 for success and other for failure}
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  {$IFDEF SMTP_DEBUG}
  if SMTP_LOG_ENABLED then SMTPLogDebug('Client: DoNoop');
  {$ENDIF}

  {Request Start Event}
  if Assigned(FOnRequestStart) then FOnRequestStart(Self);
  try
   AReply:='';

   {Check Connected}
   if not Connected then Exit;

   {Send Request}
   if not SendRequest(SMTP_COMMAND_NOOP) then Exit;

   {Get Reply}
   if not GetReply(AReply) then Exit;

   {Check Continuation}
   if Uppercase(Copy(Trim(AReply),4,Length(Trim(SMTP_STRING_CONTINUE)))) <> Uppercase(Trim(SMTP_STRING_CONTINUE)) then
    begin
     {No Continuation}
     {Check for Success}
     if Uppercase(Copy(Trim(AReply),1,Length(Trim(SMTP_STRING_NOOP_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_NOOP_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;
    end
   else
    begin
     {Line Continuation}
     {Get Response}
     if not GetReply(WorkBuffer) then Exit;

     {Check Response}
     while Uppercase(Copy(Trim(WorkBuffer),4,Length(Trim(SMTP_STRING_CONTINUE)))) = Uppercase(Trim(SMTP_STRING_CONTINUE)) do
      begin
       {Add Line}
       AReply:=AReply + SMTP_LINE_END + WorkBuffer;

       {Get Response}
       if not GetReply(WorkBuffer) then Exit;
      end;

     {Check for Success}
     if Uppercase(Copy(Trim(WorkBuffer),1,Length(Trim(SMTP_STRING_NOOP_SUCCESS)))) = Uppercase(Trim(SMTP_STRING_NOOP_SUCCESS)) then
      begin
       {Return Result}
       Result:=True;
      end;

     {Add Line}
     AReply:=AReply + SMTP_LINE_END + WorkBuffer;
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
{TSMTPConnection}
constructor TSMTPConnection.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;

 FHandle:=THandle(Self);
 FRxByteCount:=0;
 FTxByteCount:=0;
 FRequestCount:=0;
 FReplyCount:=0;
 FRequestTime:=Now;
 FReplyTime:=Now;
 FRemoteAddress:='';
 FStatus:=SMTP_STATUS_NONE;

 FAuthenticated:=False;
 FUsername:='';
 FPassword:='';

 FThread:=nil;
 FBuffer:=TSMTPBuffer.Create(SMTP_BUFFER_SIZE);
end;

{==============================================================================}

destructor TSMTPConnection.Destroy;
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

function TSMTPConnection.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSMTPConnection.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TSMTPConnection.SetHandle(AHandle:THandle);
begin
 {}
 if not AcquireLock then Exit;

 FHandle:=AHandle;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetRxByteCount:Int64;
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;

 Result:=FRxByteCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetRxByteCount(const ARxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FRxByteCount:=ARxByteCount;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetTxByteCount:Int64;
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;

 Result:=FTxByteCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetTxByteCount(const ATxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FTxByteCount:=ATxByteCount;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetRequestCount:Int64;
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;

 Result:=FRequestCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetRequestCount(const ARequestCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FRequestCount:=ARequestCount;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetReplyCount:Int64;
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;

 Result:=FReplyCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetReplyCount(const AReplyCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FReplyCount:=AReplyCount;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetRequestTime:TDateTime;
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;

 Result:=FRequestTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetRequestTime(const ARequestTime:TDateTime);
begin
 {}
 if not AcquireLock then Exit;

 FRequestTime:=ARequestTime;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetReplyTime:TDateTime;
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;

 Result:=FReplyTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetReplyTime(const AReplyTime:TDateTime);
begin
 {}
 if not AcquireLock then Exit;

 FReplyTime:=AReplyTime;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetRemoteAddress:String;
begin
 {}
 Result:='';

 if not AcquireLock then Exit;

 Result:=FRemoteAddress;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetRemoteAddress(const ARemoteAddress:String);
begin
 {}
 if not AcquireLock then Exit;

 FRemoteAddress:=ARemoteAddress;
 UniqueString(FRemoteAddress);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetStatus(AStatus:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FStatus:=AStatus;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetAuthenticated(AAuthenticated:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FAuthenticated:=AAuthenticated;

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetUsername:String;
begin
 {}
 Result:='';

 if not AcquireLock then Exit;

 Result:=FUsername;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetUsername(const AUsername:String);
begin
 {}
 if not AcquireLock then Exit;

 FUsername:=AUsername;
 UniqueString(FUsername);

 ReleaseLock;
end;

{==============================================================================}

function TSMTPConnection.GetPassword:String;
begin
 {}
 Result:='';

 if not AcquireLock then Exit;

 Result:=FPassword;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetPassword(const APassword:String);
begin
 {}
 if not AcquireLock then Exit;

 FPassword:=APassword;
 UniqueString(FPassword);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.SetThread(AThread:TThread);
begin
 {}
 if not AcquireLock then Exit;

 FThread:=AThread;

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.IncrementRxByteCount(const ARxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 Inc(FRxByteCount,ARxByteCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.IncrementTxByteCount(const ATxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 Inc(FTxByteCount,ATxByteCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.IncrementRequestCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FRequestCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TSMTPConnection.IncrementReplyCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FReplyCount);

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TSMTPListener}
constructor TSMTPListener.Create;
begin
 {}
 inherited Create;
 BoundPort:=SMTP_PORT_DEFAULT;
 UseNagle:=False; {Nagle is not recommended for SMTP (Due to strict request reply nature of the protocol)}
end;

{==============================================================================}

procedure TSMTPListener.DoConnect(AThread:TWinsock2TCPServerThread);
var
 Connection:TSMTPConnection;
begin
 {}
 inherited DoConnect(AThread);

 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoConnect');
 {$ENDIF}

 {Create Connection}
 Connection:=TSMTPConnection.Create;
 Connection.RemoteAddress:=AThread.Server.PeerAddress;
 Connection.Thread:=AThread;
 Connection.RequestTime:=Now;
 Connection.ReplyTime:=Now;

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

procedure TSMTPListener.DoDisconnect(AThread:TWinsock2TCPServerThread);
begin
 {}
 inherited DoDisconnect(AThread);

 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoDisconnect');
 {$ENDIF}

 {Disconnected Event}
 if Assigned(FOnDisconnected) then
  begin
   FOnDisconnected(TSMTPConnection(AThread.Data));
  end;
end;

{==============================================================================}

function TSMTPListener.DoCheckHost(AThread:TWinsock2TCPServerThread):Boolean;
begin
 {}
 Result:=True;

 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoCheckHost');
 {$ENDIF}

 {Check Host Event}
 if Assigned(FOnCheckHost) then
  begin
   Result:=FOnCheckHost(TSMTPConnection(AThread.Data));
  end;
end;

{==============================================================================}

function TSMTPListener.DoCheckCount(AThread:TWinsock2TCPServerThread):Boolean;
begin
 {}
 Result:=True;

 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoCheckCount');
 {$ENDIF}

 {Check Count Event}
 if Assigned(FOnCheckCount) then
  begin
   Result:=FOnCheckCount(TSMTPConnection(AThread.Data));
  end;
end;

{==============================================================================}

function TSMTPListener.DoExecute(AThread:TWinsock2TCPServerThread):Boolean;
var
 Reply:String;
 Request:String;
 SMTPData:String;
 SMTPCommand:String;
 SMTPSubCommand:String;
 Connection:TSMTPConnection;
begin
 {}
 Result:=inherited DoExecute(AThread);
 if not Result then Exit;

 Result:=False;

 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoExecute');
 {$ENDIF}

 if AThread.Server.Connected then
  begin
   {Get Connection}
   Connection:=TSMTPConnection(AThread.Data);
   if Connection = nil then Exit;

   {Check Status}
   case Connection.Status of
    SMTP_STATUS_NONE,SMTP_STATUS_CONN,SMTP_STATUS_HELO,SMTP_STATUS_AUTH,SMTP_STATUS_MAIL,SMTP_STATUS_RCPT,SMTP_STATUS_QUIT:begin
      {Get Request}
      if not GetRequest(AThread,Request) then Exit;

      {Set Default Reply}
      Reply:=SMTP_STRING_COMMAND_FAILURE + SMTP_STRING_BAD_COMMAND;

      {Process Request}
      if Trim(Request) <> '' then
       begin
        if Length(Trim(Request)) >= 4 then
         begin
          {Get SMTP Command}
          SMTPCommand:=Uppercase(Copy(Trim(Request),1,4));

          {Get SMTP Data}
          SMTPData:=Trim(Copy(Trim(Request),Length(SMTPCommand) + 1,Length(Request)));

          {Check SMTP Commands}
          if SMTPCommand = SMTP_COMMAND_HELO then
           begin
            {Helo Event}
            DoHelo(AThread,SMTPData);
           end
          else if SMTPCommand = SMTP_COMMAND_EHLO then
           begin
            {Ehlo Event}
            DoEhlo(AThread,SMTPData);
           end
          else if SMTPCommand = SMTP_COMMAND_AUTH then
           begin
            {Auth Event}
            DoAuth(AThread,SMTPData);
           end
          else if SMTPCommand = SMTP_COMMAND_MAIL then
           begin
            {Get SMTP Sub Command}
            SMTPSubCommand:=Uppercase(Copy(Trim(SMTPData),1,5));

            {Get SMTP Data}
            SMTPData:=Trim(Copy(Trim(SMTPData),Length(SMTPSubCommand) + 1,Length(SMTPData)));

            {Check SMTP Sub Command}
            if SMTPSubCommand = SMTP_SUB_COMMAND_MAIL_FROM then
             begin
              {Mail Event}
              DoMail(AThread,SMTPData);
             end
            else
             begin
              {Unknown Command}
              if not SendReply(AThread,Reply) then Exit;
             end;
           end
          else if SMTPCommand = SMTP_COMMAND_RCPT then
           begin
            {Get SMTP Sub Command}
            SMTPSubCommand:=Uppercase(Copy(Trim(SMTPData),1,3));

            {Get SMTP Data}
            SMTPData:=Trim(Copy(Trim(SMTPData),Length(SMTPSubCommand) + 1,Length(SMTPData)));

            {Check SMTP Sub Command}
            if SMTPSubCommand = SMTP_SUB_COMMAND_RCPT_TO then
             begin
              {Rcpt Event}
              DoRcpt(AThread,SMTPData);
             end
            else
             begin
              {Unknown Command}
              if not SendReply(AThread,Reply) then Exit;
             end;
           end
          else if SMTPCommand = SMTP_COMMAND_VRFY then
           begin
            {Vrfy Event}
            DoVrfy(AThread,SMTPData);
           end
          else if SMTPCommand = SMTP_COMMAND_BEGIN_DATA then
           begin
            {Begin Data Event}
            DoBeginData(AThread);
           end
          else if SMTPCommand = SMTP_COMMAND_QUIT then
           begin
            {Quit EVent}
            DoQuit(AThread);
           end
          else if SMTPCommand = SMTP_COMMAND_RSET then
           begin
            {Rset Event}
            DoRset(AThread);
           end
          else if SMTPCommand = SMTP_COMMAND_TIME then
           begin
            {Time Event}
            DoTime(AThread);
           end
          else if SMTPCommand = SMTP_COMMAND_NOOP then
           begin
            {Noop Event}
            DoNoop(AThread);
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
     end;
    SMTP_STATUS_DATA:begin
      {Get Request}
      if not GetRequest(AThread,Request) then Exit;

      {Set Default Reply}
      Reply:='';

      {Process Request}
      if Request = SMTP_COMMAND_END_DATA then
       begin
        {End Data Event}
        DoEndData(AThread);
       end
      else
       begin
        {Data Event}
        DoData(AThread,Request);
       end;
     end;
   end;

   Result:=True;
  end;
end;

{==============================================================================}

procedure TSMTPListener.DoConn(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoConn');
 {$ENDIF}

 Request:='';

 {Set Default Reply}
 Reply:=SMTP_STRING_CONN_SUCCESS + SMTP_STRING_CONN_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_CONN;
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

procedure TSMTPListener.DoHelo(AThread:TWinsock2TCPServerThread;const AHost:String);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoHelo');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Host = ' + AHost);
 {$ENDIF}

 Request:=AHost;

 {Set Default Reply}
 Reply:=SMTP_STRING_HELO_SUCCESS + SMTP_STRING_HELO_OK + AHost;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_HELO;
  end;

 {Helo Event}
 if Assigned(FOnHelo) then
  begin
   if FOnHelo(Connection,Request,Reply) then
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

procedure TSMTPListener.DoEhlo(AThread:TWinsock2TCPServerThread;const AHost:String);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoEhlo');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Host = ' + AHost);
 {$ENDIF}

 Request:=AHost;

 {Set Default Reply}
 Reply:=SMTP_STRING_EHLO_SUCCESS + SMTP_STRING_EHLO_HEAD_OK + AHost + SMTP_STRING_EHLO_TAIL_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_HELO;
  end;

 {Ehlo Event}
 if Assigned(FOnEhlo) then
  begin
   if FOnEhlo(Connection,Request,Reply) then
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

procedure TSMTPListener.DoAuth(AThread:TWinsock2TCPServerThread;const AParams:String);
var
 Reply:String;
 Method:String;
 Username:String;
 Password:String;
 Params:TStringList;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoAuth');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Params = ' + AParams);
 {$ENDIF}

 {Set Default Reply}
 Reply:=SMTP_STRING_COMMAND_FAILURE + SMTP_STRING_BAD_COMMAND;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_AUTH;
  end;

 {Auth Event}
 if Assigned(FOnAuth) then
  begin
   Params:=TStringList.Create;
   try
    {Extract Params}
    UndelimitString(AParams,Params,' ');

    {Check Params}
    case Params.Count of
     1:begin
       {Method supplied}
       {Get Method}
       Method:=Params.Strings[0];

       {Send Reply}   {Base64 Encoded}
       Reply:=SMTP_STRING_AUTH_CHALLENGE + Base64EncodeString('Username:');
       if not SendReply(AThread,Reply) then Exit;

       {Get Username} {Base64 Encoded}
       if not GetRequest(AThread,Username) then Exit;
       Username:=Base64DecodeString(Username);

       {Send Reply}   {Base64 Encoded}
       Reply:=SMTP_STRING_AUTH_CHALLENGE + Base64EncodeString('Password:');
       if not SendReply(AThread,Reply) then Exit;

       {Get Password} {Base64 Encoded}
       if not GetRequest(AThread,Password) then Exit;
       Password:=Base64DecodeString(Password);

       if FOnAuth(Connection,Method,Username,Password,Reply) then
        begin
         {Send Reply}
         if not SendReply(AThread,Reply) then Exit;
        end
       else
        begin
         {Send Reply}
         if not SendReply(AThread,Reply) then Exit;
        end;
      end;
     2:begin
       {Method and Username supplied}
       {Get Method}
       Method:=Params.Strings[0];

       {Get Username} {Base64 Encoded}
       Username:=Base64DecodeString(Params.Strings[1]);

       {Send Reply}   {Base64 Encoded}
       Reply:=SMTP_STRING_AUTH_CHALLENGE + Base64EncodeString('Password:');
       if not SendReply(AThread,Reply) then Exit;

       {Get Password} {Base64 Encoded}
       if not GetRequest(AThread,Password) then Exit;
       Password:=Base64DecodeString(Password);

       if FOnAuth(Connection,Method,Username,Password,Reply) then
        begin
         {Send Reply}
         if not SendReply(AThread,Reply) then Exit;
        end
       else
        begin
         {Send Reply}
         if not SendReply(AThread,Reply) then Exit;
        end;
      end;
     else
      begin
       {Set Default Reply}
       Reply:=SMTP_STRING_AUTH_FAILURE + SMTP_STRING_BAD_AUTH;

       {Send Reply}
       if not SendReply(AThread,Reply) then Exit;
      end;
    end;
   finally
    Params.Free;
   end;
  end
 else
  begin
   {Default Reply}
   if not SendReply(AThread,Reply) then Exit;
  end;
end;

{==============================================================================}

procedure TSMTPListener.DoMail(AThread:TWinsock2TCPServerThread;const ASender:String);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoMail');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Sender = ' + ASender);
 {$ENDIF}

 Request:=ASender;

 {Set Default Reply}
 Reply:=SMTP_STRING_MAIL_SUCCESS + SMTP_STRING_MAIL_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_MAIL;
  end;

 {Mail Event}
 if Assigned(FOnMail) then
  begin
   if FOnMail(Connection,Request,Reply) then
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

procedure TSMTPListener.DoRcpt(AThread:TWinsock2TCPServerThread;const ARecipient:String);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoRcpt');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Recipient = ' + ARecipient);
 {$ENDIF}

 Request:=ARecipient;

 {Set Default Reply}
 Reply:=SMTP_STRING_RCPT_SUCCESS + SMTP_STRING_RCPT_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_RCPT;
  end;

 {Rcpt Event}
 if Assigned(FOnRcpt) then
  begin
   if FOnRcpt(Connection,Request,Reply) then
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

procedure TSMTPListener.DoVrfy(AThread:TWinsock2TCPServerThread;const AAddress:String);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoVrfy');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Address = ' + AAddress);
 {$ENDIF}

 Request:=AAddress;

 {Set Default Reply}
 Reply:=SMTP_STRING_VRFY_FAILURE + SMTP_STRING_BAD_VRFY;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=Connection.Status;
  end;

 {Vrfy Event}
 if Assigned(FOnVrfy) then
  begin
   if FOnVrfy(Connection,Request,Reply) then
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

procedure TSMTPListener.DoBeginData(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoBeginData');
 {$ENDIF}

 Request:='';

 {Set Default Reply}
 Reply:=SMTP_STRING_BEGIN_DATA_SUCCESS + SMTP_STRING_BEGIN_DATA_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_DATA;
  end;

 {Begin Data Event}
 if Assigned(FOnBeginData) then
  begin
   if FOnBeginData(Connection,Request,Reply) then
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

procedure TSMTPListener.DoData(AThread:TWinsock2TCPServerThread;const AData:String);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoData');
 {$ENDIF}

 Request:=AData;

 {Set Default Reply}
 Reply:='';

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_DATA;
  end;

 {Data Event}
 if Assigned(FOnData) then
  begin
   if FOnData(Connection,Request,Reply) then
    begin
     {Nothing}
    end
   else
    begin
     {Nothing}
    end;
  end
 else
  begin
   {Discard Data}
  end;
end;

{==============================================================================}

procedure TSMTPListener.DoEndData(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoEndData');
 {$ENDIF}

 Request:='';

 {Set Default Reply}
 Reply:=SMTP_STRING_END_DATA_SUCCESS + SMTP_STRING_END_DATA_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_HELO;
  end;

 {End Data Event}
 if Assigned(FOnEndData) then
  begin
   if FOnEndData(Connection,Request,Reply) then
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

procedure TSMTPListener.DoQuit(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoQuit');
 {$ENDIF}

 Request:='';

 {Set Default Reply}
 Reply:=SMTP_STRING_QUIT_SUCCESS + SMTP_STRING_QUIT_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_QUIT;
  end;

 {Quit Event}
 if Assigned(FOnQuit) then
  begin
   if FOnQuit(Connection,Request,Reply) then
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;

     {Close Connection}
     AThread.Server.Disconnect;
    end
   else
    begin
     {Send Reply}
     if not SendReply(AThread,Reply) then Exit;

     {Close Connection}
     AThread.Server.Disconnect;
    end;
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

procedure TSMTPListener.DoRset(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoRset');
 {$ENDIF}

 Request:='';

 {Set Default Reply}
 Reply:=SMTP_STRING_RSET_SUCCESS + SMTP_STRING_RSET_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=SMTP_STATUS_HELO;
  end;

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

procedure TSMTPListener.DoTime(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoTime');
 {$ENDIF}

 Request:='';

 {Set Default Reply}
 Reply:=SMTP_STRING_TIME_SUCCESS + SMTP_STRING_TIME_OK + SystemDateTimeToString(Now);

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=Connection.Status;
  end;

 {Time Event}
 if Assigned(FOnTime) then
  begin
   if FOnTime(Connection,Request,Reply) then
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

procedure TSMTPListener.DoNoop(AThread:TWinsock2TCPServerThread);
var
 Reply:String;
 Request:String;
 Connection:TSMTPConnection;
begin
 {}
 if AThread = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: DoNoop');
 {$ENDIF}

 Request:='';

 {Set Default Reply}
 Reply:=SMTP_STRING_NOOP_SUCCESS + SMTP_STRING_NOOP_OK;

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection <> nil then
  begin
   Connection.Status:=Connection.Status;
  end;

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

function TSMTPListener.GetRequest(AThread:TWinsock2TCPServerThread;var ARequest:String):Boolean;
var
 Value:Char;
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
 Connection:TSMTPConnection;
begin
 {}
 Result:=False;
 ARequest:='';

 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: GetRequest');
 {$ENDIF}

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection = nil then Exit;

 {Get Request}
 Completed:=False;
 while not(Completed) do
  begin
   {$IFDEF SMTP_DEBUG}
   if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Buffer Count = ' + IntToStr(Connection.Buffer.Count));
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
    {$IFDEF SMTP_DEBUG}
    if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Buffer Write Size = ' + IntToStr(Size));
    {$ENDIF}

    Count:=0;

    {Read Available}
    if not AThread.Server.ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;

    {$IFDEF SMTP_DEBUG}
    if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Buffer Write Count = ' + IntToStr(Count));
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

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Request = ' + ARequest);
 {$ENDIF}
end;

{==============================================================================}

function TSMTPListener.SendReply(AThread:TWinsock2TCPServerThread;const AReply:String):Boolean;
var
 Connection:TSMTPConnection;
begin
 {}
 Result:=False;

 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF SMTP_DEBUG}
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener: SendReply');
 if SMTP_LOG_ENABLED then SMTPLogDebug('Listener:  Reply = ' + AReply);
 {$ENDIF}

 {Get Connection}
 Connection:=TSMTPConnection(AThread.Data);
 if Connection = nil then Exit;

 {Send Reply}
 if not AThread.Server.WriteData(PChar(AReply),Length(AReply)) then Exit;

 {Send Line End}
 if not AThread.Server.WriteData(PChar(SMTP_LINE_END),Length(SMTP_LINE_END)) then Exit;

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
procedure SMTPInit;
begin
 {}
 {Check Initialized}
 if SMTPInitialized then Exit;

 {Initialize Logging}
 SMTP_LOG_ENABLED:=(SMTP_DEFAULT_LOG_LEVEL <> SMTP_LOG_LEVEL_NONE);

 SMTPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{SMTP Functions}

{==============================================================================}
{==============================================================================}
{SMTP Helper Functions}
procedure SMTPLog(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SMTP_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = SMTP_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SMTP_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = SMTP_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'SMTP: ';

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_SMTP,LogLevelToLoggingSeverity(Level),'SMTP',WorkBuffer + AText);
end;

{==============================================================================}

procedure SMTPLogInfo(const AText:String); inline;
begin
 {}
 SMTPLog(SMTP_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure SMTPLogWarn(const AText:String); inline;
begin
 {}
 SMTPLog(SMTP_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure SMTPLogError(const AText:String); inline;
begin
 {}
 SMTPLog(SMTP_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure SMTPLogDebug(const AText:String); inline;
begin
 {}
 SMTPLog(SMTP_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 SMTPInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.



