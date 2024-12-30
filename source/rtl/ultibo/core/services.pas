{
Ultibo Services interface unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

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
 
 Ping
 
  RFC792 - Internet Control Message Protocol - https://www.rfc-editor.org/rfc/rfc792
 
 NTP
  
  RFC1305 - Network Time Protocol (Version 3) - https://tools.ietf.org/html/rfc1305
  RFC5905 - Network Time Protocol (Version 4) - https://tools.ietf.org/html/rfc5905
  RFC4330 - Simple Network Time Protocol (SNTP) Version 4 - https://tools.ietf.org/html/rfc4330
  
 Telnet
 
  RFC????
  
  ANSI Escape Sequences - http://ascii-table.com/ansi-escape-sequences.php
  VT100 Escape Sequences - http://ascii-table.com/ansi-escape-sequences-vt-100.php
  
 SSH
 
  RFC????
  
 UPnP
 
  RFC????
 
 SysLog
 
  RFC3164 - The BSD syslog Protocol - https://tools.ietf.org/html/rfc3164
  RFC5424 - The Syslog Protocol - https://tools.ietf.org/html/rfc5424
  RFC5426 - Transmission of Syslog Messages over UDP - https://tools.ietf.org/html/rfc5426
  RFC6587 - Transmission of Syslog Messages over TCP - https://tools.ietf.org/html/rfc6587
 
Services
========

 Ping
 
 NTP

 Telnet
 
 SSH
 
 UPnP
 
 SysLog
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Services;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Logging,SysUtils,
     Classes,Ultibo,UltiboClasses,Winsock2,Protocol,ICMP,Crypto,Authentication;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Services specific constants}
 
 {Ping constants}
 PING_DEFAULT_SIZE = 32;      {Default number of bytes of data to send with echo (Ping) request} 
 PING_MAXIMUM_SIZE = 65500;   {Maximum number of bytes of data to send with echo (Ping) request} 
 PING_DEFAULT_COUNT = 4;      {Default number of echo (ping) requests to send in a sequence}
 PING_DEFAULT_TIMEOUT = 3000; {Default time to wait for echo (Ping) response (Milliseconds)} 
 
 {NTP constants}
 NTP_VERSION_1 = 1;
 NTP_VERSION_2 = 2;
 NTP_VERSION_3 = 3;
 NTP_VERSION_4 = 4;
 
 NTP_VERSION_MASK = $07; {shl 3}
 
 NTP_VERSION = NTP_VERSION_4;      {Current NTP/SNTP version}
 NTP_MIN_VERSION = NTP_VERSION_2;  {Minimum acceptable NTP/SNTP version}
 NTP_MAX_VERSION = NTP_VERSION_4;  {Maximum acceptable NTP/SNTP version}
 
 NTP_PACKET_SIZE = 48; {SizeOf(TNTPPacket)}

 NTP_TIMESTAMP_START = 94354848000000000; {Offset between 1/1/1601 (Ultibo) and 1/1/1900 (NTP Timestamp)}
                       
 {NTP Leap Indicator}
 NTP_LEAP_NONE    = 0;
 NTP_LEAP_LAST_61 = 1;
 NTP_LEAP_LAST_59 = 2;
 NTP_LEAP_ALARM   = 3;
 
 NTP_LEAP_MASK    = $03; {shl 6}
 
 {NTP Mode}
 NTP_MODE_RESERVED          = 0; {Reserved}
 NTP_MODE_SYMMETRIC_ACTIVE  = 1; {Symmetric active}
 NTP_MODE_SYMMETRIC_PASSIVE = 2; {Symmetric passive}
 NTP_MODE_CLIENT            = 3; {Client}
 NTP_MODE_SERVER            = 4; {Server}
 NTP_MODE_BROADCAST         = 5; {Broadcast}
 NTP_MODE_CONTROL           = 6; {Reserved for NTP control message}
 NTP_MODE_PRIVATE           = 7; {Reserved for private use}

 NTP_MODE_MASK              = $07; {shl 0}
 
 {NTP Stratum}
 NTP_STRATUM_INVALID       = 0;  {unspecified or invalid}
 NTP_STRATUM_PRIMARY       = 1;  {primary server (e.g., equipped with a GPS receiver)}
 NTP_STRATUM_SECONDARY2    = 2;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY3    = 3;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY4    = 4;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY5    = 5;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY6    = 6;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY7    = 7;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY8    = 8;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY9    = 9;  {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY10   = 10; {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY11   = 11; {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY12   = 12; {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY13   = 13; {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY14   = 14; {secondary server (via NTP)}
 NTP_STRATUM_SECONDARY15   = 15; {secondary server (via NTP)}
 NTP_STRATUM_UNSYNCRONIZED = 16; {unsynchronized}
 NTP_STRATUM_RESERVED      = 17; {reserved}
 
 {Telnet character constants}
 TELNET_CHAR_NUL       = #0;
 TELNET_CHAR_CR        = #13;
 TELNET_CHAR_LF        = #10;
 TELNET_CHAR_BELL      = #7;
 TELNET_CHAR_TAB       = #9;
 TELNET_CHAR_ESC       = #27;
 TELNET_CHAR_BACKSPACE = #127;
 
 {Telnet escape sequences}
 TELNET_SEQUENCE_UP_ARROW    = #27#91#65; {ESC[A}
 TELNET_SEQUENCE_DOWN_ARROW  = #27#91#66; {ESC[B}
 TELNET_SEQUENCE_RIGHT_ARROW = #27#91#67; {ESC[C}
 TELNET_SEQUENCE_LEFT_ARROW  = #27#91#68; {ESC[D}
 
 TELNET_SEQUENCE_HOME        = #27#91#49#126; {ESC[1~}
 TELNET_SEQUENCE_INSERT      = #27#91#50#126; {ESC[2~}
 TELNET_SEQUENCE_DELETE      = #27#91#51#126; {ESC[3~}
 TELNET_SEQUENCE_END         = #27#91#52#126; {ESC[4~}
 TELNET_SEQUENCE_PGUP        = #27#91#53#126; {ESC[5~}
 TELNET_SEQUENCE_PGDN        = #27#91#54#126; {ESC[6~}
 
 TELNET_SEQUENCE_F1          = #27#91#49#49#126; {ESC[11~} 
 TELNET_SEQUENCE_F2          = #27#91#49#50#126; {ESC[12~} 
 TELNET_SEQUENCE_F3          = #27#91#49#51#126; {ESC[13~} 
 TELNET_SEQUENCE_F4          = #27#91#49#52#126; {ESC[14~} 
 TELNET_SEQUENCE_F5          = #27#91#49#53#126; {ESC[15~} 
 TELNET_SEQUENCE_F6          = #27#91#49#55#126; {ESC[17~} 
 TELNET_SEQUENCE_F7          = #27#91#49#56#126; {ESC[18~} 
 TELNET_SEQUENCE_F8          = #27#91#49#57#126; {ESC[19~} 
 TELNET_SEQUENCE_F9          = #27#91#50#48#126; {ESC[20~} 
 TELNET_SEQUENCE_F10         = #27#91#50#49#126; {ESC[21~} 
 
 TELNET_BUFFER_SIZE = SIZE_2K;
 
 {Telnet State constants}
 TELNET_STATE_NONE            = $00000000;
 TELNET_STATE_TRANSMIT_BINARY = $00000001;
 TELNET_STATE_ECHO            = $00000002;
 TELNET_STATE_SUPPRESS_GA     = $00000004;
 TELNET_STATE_TERMINAL_TYPE   = $00000008;
 TELNET_STATE_WINDOW_SIZE     = $00000010;
 TELNET_STATE_TERMINAL_SPEED  = $00000020;
 TELNET_STATE_NEW_ENVIRONMENT = $00000040;
 
 {Telnet Commands}
 TELNET_COMMAND_EOR    =  239; {0xEF end of record command}
 TELNET_COMMAND_SE     =  240; {0xF0 end of subnegotiations command}
 TELNET_COMMAND_NOP    =  241; {0xF1 no operation command}
 TELNET_COMMAND_DM     =  242; {0xF2 data mark command}
 TELNET_COMMAND_BRK    =  243; {0xF3 break NVT charater}
 TELNET_COMMAND_IP     =  244; {0xF4 interrupt process command}
 TELNET_COMMAND_AO     =  245; {0xF5 abort output command}
 TELNET_COMMAND_AYT    =  246; {0xF6 are you there command}
 TELNET_COMMAND_EC     =  247; {0xF7 erase character command}
 TELNET_COMMAND_EL     =  248; {0xF8 erase line command}
 TELNET_COMMAND_GA     =  249; {0xF9 go ahead command}
 TELNET_COMMAND_SB     =  250; {0xFA begin option subnegotiations command}
 TELNET_COMMAND_WILL   =  251; {0xFB will enable option}
 TELNET_COMMAND_WONT   =  252; {0xFC won't enable option}
 TELNET_COMMAND_DO     =  253; {0xFD request other party enables option}
 TELNET_COMMAND_DONT   =  254; {0xFE request other party doesn't enable option}
 TELNET_COMMAND_IAC    =  255; {0xFF interpret as command}
 
 {Telnet Options}
 TELNET_OPTION_TRANSMIT_BINARY  = 0;  {0x00 transmit binary option}
 TELNET_OPTION_ECHO             = 1;  {0x01 echo option}
 TELNET_OPTION_SUPPRESS_GA      = 3;  {0x03 suppress go ahead option}
 TELNET_OPTION_TERMINAL_TYPE    = 24; {0x18 terminal type}
 TELNET_OPTION_WINDOW_SIZE      = 31; {0x1F negotiate window size}
 TELNET_OPTION_TERMINAL_SPEED   = 32; {0x20 terminal speed}
 TELNET_OPTION_NEW_ENVIRONMENT  = 39; {0x27 new environment}
 
 {SSH constants}
 //To do
 
 {UPnP constants}
 
 {SysLog constants}
 SYSLOG_LOGGING_DESCRIPTION = 'SysLog Logging';
 
 {SysLog Facility codes}
 SYSLOG_FACILITY_KERNEL   = 0;  {kernel messages}
 SYSLOG_FACILITY_USER     = 1;  {user-level messages}
 SYSLOG_FACILITY_MAIL     = 2;  {mail system}
 SYSLOG_FACILITY_SYSTEM   = 3;  {system daemons}
 SYSLOG_FACILITY_SECURITY = 4;  {security/authorization messages (note 1)}
 SYSLOG_FACILITY_SYSLOG   = 5;  {messages generated internally by syslogd}
 SYSLOG_FACILITY_PRINTER  = 6;  {line printer subsystem}
 SYSLOG_FACILITY_NEWS     = 7;  {network news subsystem}
 SYSLOG_FACILITY_UUCP     = 8;  {UUCP subsystem}
 SYSLOG_FACILITY_CLOCK    = 9;  {clock daemon (note 2)}
 SYSLOG_FACILITY_AUTH     = 10; {security/authorization messages (note 1)}
 SYSLOG_FACILITY_FTP      = 11; {FTP daemon}
 SYSLOG_FACILITY_NTP      = 12; {NTP subsystem}
 SYSLOG_FACILITY_AUDIT    = 13; {log audit (note 1)}
 SYSLOG_FACILITY_ALERT    = 14; {log alert (note 1)}
 SYSLOG_FACILITY_CLOCK2   = 15; {clock daemon (note 2)}
 SYSLOG_FACILITY_LOCAL0   = 16; {local use 0 (local0)}
 SYSLOG_FACILITY_LOCAL1   = 17; {local use 1 (local1)}
 SYSLOG_FACILITY_LOCAL2   = 18; {local use 2 (local2)}
 SYSLOG_FACILITY_LOCAL3   = 19; {local use 3 (local3)}
 SYSLOG_FACILITY_LOCAL4   = 20; {local use 4 (local4)}
 SYSLOG_FACILITY_LOCAL5   = 21; {local use 5 (local5)}
 SYSLOG_FACILITY_LOCAL6   = 22; {local use 6 (local6)}
 SYSLOG_FACILITY_LOCAL7   = 23; {local use 7 (local7)}
 
 {SysLog Severity codes}
 SYSLOG_SEVERITY_EMERGENCY   = 0; {Emergency: system is unusable}
 SYSLOG_SEVERITY_ALERT       = 1; {Alert: action must be taken immediately}
 SYSLOG_SEVERITY_CRITICAL    = 2; {Critical: critical conditions}
 SYSLOG_SEVERITY_ERROR       = 3; {Error: error conditions}
 SYSLOG_SEVERITY_WARNING     = 4; {Warning: warning conditions}
 SYSLOG_SEVERITY_NOTICE      = 5; {Notice: normal but significant condition}
 SYSLOG_SEVERITY_INFORMATION = 6; {Informational: informational messages}
 SYSLOG_SEVERITY_DEBUG       = 7; {Debug: debug-level messages}
 
 {Service logging}
 SERVICE_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Service debugging messages}
 SERVICE_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Service informational messages, such as a service being created or destroyed}
 SERVICE_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Service warning messages}
 SERVICE_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Service error messages}
 SERVICE_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Service messages}

var 
 SERVICE_DEFAULT_LOG_LEVEL:LongWord = SERVICE_LOG_LEVEL_DEBUG; {Minimum level for Service messages.  Only messages with level greater than or equal to this will be printed} 
 
var 
 {Service logging}
 SERVICE_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {Services specific types}
 
 {Ping types}
 
 {NTP types}
 PNTPShort = ^TNTPShort;
 TNTPShort = packed record {NTP Short Format}
  Seconds:Word;       {Seconds}
  Fraction:Word;      {Fraction}
 end;
 
 PNTPTimestamp = ^TNTPTimestamp;
 TNTPTimestamp = packed record {NTP Timestamp Format}
  Seconds:LongWord;   {Seconds}
  Fraction:LongWord;  {Seconds Fraction (0-padded)}
 end;
 
 {NTP Date Format}
 PNTPDate = ^TNTPDate;
 TNTPDate = packed record
  EraNumber:LongWord;  {Era Number}
  EraOffset:LongWord;  {Era Offset}
  Fraction:Int64;      {Fraction}
 end;
 
 PNTPPacket = ^TNTPPacket;
 TNTPPacket = packed record
  LeapVersionMode:Byte;                    {Leap Indicator (2 bits) / Version (3 bits) / Mode (3 bits)}
  Stratum:Byte;                            {This is an eight-bit unsigned integer indicating the stratum}
  PollInterval:Byte;                       {This is an eight-bit unsigned integer used as an exponent of two, where the resulting value is the maximum interval between successive messages in seconds}
  Precision:ShortInt;                      {This is an eight-bit signed integer used as an exponent of two, where the resulting value is the precision of the system clock in seconds}
  RootDelay:LongInt;                       {This is a 32-bit signed fixed-point number indicating the total roundtrip delay to the primary reference source, in seconds with the fraction point between bits 15 and 16}
  RootDispersion:LongWord;                 {This is a 32-bit unsigned fixed-point number indicating the maximum error due to the clock frequency tolerance, in seconds with the fraction point between bits 15 and 16}
  ReferenceIdentifier:array[0..3] of Byte; {This is a 32-bit bitstring identifying the particular reference source}
  ReferenceTimestamp:TNTPTimestamp;        {This field is the time the system clock was last set or corrected, in 64-bit timestamp format}
  OriginateTimestamp:TNTPTimestamp;        {This is the time at which the request departed the client for the server, in 64-bit timestamp format}
  ReceiveTimestamp:TNTPTimestamp;          {This is the time at which the request arrived at the server or the reply arrived at the client, in 64-bit timestamp format}
  TransmitTimestamp:TNTPTimestamp;         {This is the time at which the request departed the client or the reply departed the server, in 64-bit timestamp format} 
  {KeyIdentifier:LongWord;}                {Optional for NTP authentication}
  {MessageDigest:array[0..15] of Byte;}    {Optional for NTP authentication}
 end;

 {Telnet types}
 
 {SSH types}
 //To do
 
 {UPnP types}
 
 {SysLog types}
 
{==============================================================================}
type
 {Services specific classes}
 
 {Ping classes}
 TPingClient = class(TWinsock2RAWClient)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
  FSize:LongWord;
  FCount:LongWord;
  FTimeout:LongWord;

  FTimeToLive:Byte;
  FNoFragment:Boolean;

  FLastHost:String;
  FLastAddress:String;

  FLastTime:LongWord;
  FReplyAddress:String;

  FLastSequence:Word;
  FLastIdentifier:Word;

  FSendCount:LongWord;
  FReceiveCount:LongWord;
  FLostCount:LongWord;

  {Internal Methods}
  procedure SetSize(ASize:LongWord);
  procedure SetCount(ACount:LongWord);
  procedure SetTimeout(ATimeout:LongWord);

  procedure SetTimeToLive(ATimeToLive:Byte);
  procedure SetNoFragment(ANoFragment:Boolean);

  function GetLastCount:LongWord;

  function GetErrorCode:LongInt;
  function GetErrorText:String;
 protected
  {Internal Methods}
  procedure ResetPing; virtual;
 public
  {Public Properties}
  property Size:LongWord read FSize write SetSize;
  property Count:LongWord read FCount write SetCount;
  property Timeout:LongWord read FTimeout write SetTimeout;

  property TimeToLive:Byte read FTimeToLive write SetTimeToLive;
  property NoFragment:Boolean read FNoFragment write SetNoFragment;

  property LastHost:String read FLastHost;
  property LastAddress:String read FLastAddress;

  property LastTime:LongWord read FLastTime;
  property LastCount:LongWord read GetLastCount;
  property ReplyAddress:String read FReplyAddress;

  property LastSequence:Word read FLastSequence;
  property LastIdentifier:Word read FLastIdentifier;

  property SendCount:LongWord read FSendCount;
  property ReceiveCount:LongWord read FReceiveCount;
  property LostCount:LongWord read FLostCount;

  property ErrorCode:LongInt read GetErrorCode;
  property ErrorText:String read GetErrorText;

  {Public Methods}
  function FirstPing(const AHost:String):Boolean;
  function NextPing:Boolean;
 end;

 {NTP classes}
 TNTPClient = class(TWinsock2UDPClient)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TMutexHandle;
  
  FPollInterval:LongWord;        {How often to poll the server (in seconds)}
  FPollTimeout:LongWord;         {How long before receive or send timeout occurs (in milliseconds)}
  FPollRetries:LongWord;         {How many times to retry a poll}
  
  FRetryTimeout:LongWord;        {How long to wait between poll retries (in milliseconds)}
  
  FUseClockOffset:Boolean;       {Use the calculated NTP clock offset to update the local time}
  FClockTolerance:LongWord;      {Milliseconds difference between local and remote to trigger a clock set}
  
  FInitialClockGet:Boolean;      {Has the time been obtained at least once}
  FInitialClockCount:LongWord;   {How many times have we tried to obtain the initial clock}
  FInitialClockRetry:Boolean;    {Should the client setup a worker thread to retry until the initial clock has been set (Default: True)}
  
  FTimerHandle:TTimerHandle;     {Handle for the NTP update timer}
  
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;  
  
  procedure SetPollInterval(APollInterval:LongWord);
  procedure SetPollTimeout(APollTimeout:LongWord);
  procedure SetPollRetries(APollRetries:LongWord);
  
  procedure SetRetryTimeout(ARetryTimeout:LongWord);

  procedure SetUseClockOffset(AUseClockOffset:Boolean);
  procedure SetClockTolerance(AClockTolerance:LongWord);
  
  procedure SetInitialClockGet(AInitialClockGet:Boolean);
  procedure SetInitialClockCount(AInitialClockCount:LongWord);
  procedure SetInitialClockRetry(AInitialClockRetry:Boolean);
  
  procedure SetTimerHandle(ATimerHandle:TTimerHandle);
 public
  {Public Properties}
  property PollInterval:LongWord read FPollInterval write SetPollInterval;
  property PollTimeout:LongWord read FPollTimeout write SetPollTimeout;
  property PollRetries:LongWord read FPollRetries write SetPollRetries;
  
  property RetryTimeout:LongWord read FRetryTimeout write SetRetryTimeout;

  property UseClockOffset:Boolean read FUseClockOffset write SetUseClockOffset;
  property ClockTolerance:LongWord read FClockTolerance write SetClockTolerance;

  property InitialClockGet:Boolean read FInitialClockGet write SetInitialClockGet;
  property InitialClockCount:LongWord read FInitialClockCount write SetInitialClockCount;
  property InitialClockRetry:Boolean read FInitialClockRetry write SetInitialClockRetry;
  
  property TimerHandle:TTimerHandle read FTimerHandle write SetTimerHandle;
  
  {Public Methods}
  function GetTime:Int64;
  
  procedure IncrementInitialClockCount;
  
  function FormatTime(Time:Int64):String;
  function FormatOffset(Offset:Int64):String;
  
  function CalculateClockOffset(T1,T2,T3,T4:Int64):Int64;
  function CalculateRoundtripDelay(T1,T2,T3,T4:Int64):Int64;
 end;
 
 {Telnet classes}
 TTelnetBuffer = class(TObject)
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
 
 TTelnetListener = class;
 TTelnetConnection = class(TListObject)
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
  FResponseCount:Int64;       {Responses Sent Count to Connection}
  FRequestTime:TDateTime;     {Last Request Time}
  FResponseTime:TDateTime;    {Last Response Time}
  FRemoteAddress:String;      {Address of Remote Client}
  FLocalState:LongWord;       {Local connection state (eg TELNET_STATE_ECHO)}
  FRemoteState:LongWord;      {Remote connection state (eg TELNET_STATE_ECHO)}
  
  FData:Pointer;              {Private data for application}
  FThread:TThread;            {TWinsock2TCPServerThread}
  FBuffer:TTelnetBuffer;      {Buffer for received data}
  FListener:TTelnetListener;  {Listener for Connection}
  
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
  function GetResponseCount:Int64;
  procedure SetResponseCount(const AResponseCount:Int64);
  function GetRequestTime:TDateTime;
  procedure SetRequestTime(const ARequestTime:TDateTime);
  function GetResponseTime:TDateTime;
  procedure SetResponseTime(const AResponseTime:TDateTime);
  function GetRemoteAddress:String;
  procedure SetRemoteAddress(const ARemoteAddress:String);
  procedure SetLocalState(ALocalState:LongWord);
  procedure SetRemoteState(ARemoteState:LongWord);
  
  procedure SetData(AData:Pointer);
  procedure SetThread(AThread:TThread);
  procedure SetListener(AListener:TTelnetListener);
 public
  {Public Properties}
  property Handle:THandle read FHandle write SetHandle;
  property RxByteCount:Int64 read GetRxByteCount write SetRxByteCount;
  property TxByteCount:Int64 read GetTxByteCount write SetTxByteCount;
  property RequestCount:Int64 read GetRequestCount write SetRequestCount;
  property ResponseCount:Int64 read GetResponseCount write SetResponseCount;
  property RequestTime:TDateTime read GetRequestTime write SetRequestTime;
  property ResponseTime:TDateTime read GetResponseTime write SetResponseTime;
  property RemoteAddress:String read GetRemoteAddress write SetRemoteAddress;
  property LocalState:LongWord read FLocalState write SetLocalState;
  property RemoteState:LongWord read FRemoteState write SetRemoteState;
 
  property Data:Pointer read FData write SetData;
  property Thread:TThread read FThread  write SetThread;
  property Buffer:TTelnetBuffer read FBuffer;
  property Listener:TTelnetListener read FListener write SetListener;
  
  {Public Methods}
  procedure IncrementRxByteCount(const ARxByteCount:Int64);
  procedure IncrementTxByteCount(const ATxByteCount:Int64);
  procedure IncrementRequestCount;
  procedure IncrementResponseCount;
 end;
 
 TTelnetHostEvent = function(AConnection:TTelnetConnection):Boolean of Object;
 TTelnetCountEvent = function(AConnection:TTelnetConnection):Boolean of Object;
 TTelnetInitEvent = function(AConnection:TTelnetConnection):Boolean of Object;
 TTelnetCharEvent = function(AConnection:TTelnetConnection;AChar:Char):Boolean of Object;
 TTelnetCommandEvent = function(AConnection:TTelnetConnection;ACommand,AOption:Byte;AData:Pointer;ASize:LongWord):Boolean of Object;
 TTelnetConnectionEvent = procedure(AConnection:TTelnetConnection) of Object;
 
 TTelnetListener = class(TWinsock2TCPListener)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
  FOnConnected:TTelnetConnectionEvent;
  FOnDisconnected:TTelnetConnectionEvent;

  FOnCheckHost:TTelnetHostEvent;
  FOnCheckCount:TTelnetCountEvent;
  
  FOnInit:TTelnetInitEvent;
  FOnChar:TTelnetCharEvent;
  FOnCommand:TTelnetCommandEvent;
 protected
  {Internal Methods}
  procedure DoConnect(AThread:TWinsock2TCPServerThread); override;
  procedure DoDisconnect(AThread:TWinsock2TCPServerThread); override;
  
  function DoCheckHost(AThread:TWinsock2TCPServerThread):Boolean; virtual;
  function DoCheckCount(AThread:TWinsock2TCPServerThread):Boolean; virtual;
  
  function DoExecute(AThread:TWinsock2TCPServerThread):Boolean; override;
 
  procedure DoInit(AThread:TWinsock2TCPServerThread);
  procedure DoChar(AThread:TWinsock2TCPServerThread;AChar:Char);
  procedure DoCommand(AThread:TWinsock2TCPServerThread;ACommand,AOption:Byte;AData:Pointer;ASize:LongWord);
  
  function SendEcho(AThread:TWinsock2TCPServerThread;AChar:Char):Boolean;
 public
  {Public Properties}
  property OnConnected:TTelnetConnectionEvent read FOnConnected write FOnConnected;
  property OnDisconnected:TTelnetConnectionEvent read FOnDisconnected write FOnDisconnected;
 
  property OnCheckHost:TTelnetHostEvent read FOnCheckHost write FOnCheckHost;
  property OnCheckCount:TTelnetCountEvent read FOnCheckCount write FOnCheckCount;
  
  property OnInit:TTelnetInitEvent read FOnInit write FOnInit;
  property OnChar:TTelnetCharEvent read FOnChar write FOnChar;
  property OnCommand:TTelnetCommandEvent read FOnCommand write FOnCommand;
  
  {Public Methods}
  function GetChar(AThread:TWinsock2TCPServerThread;var AChar:Char):Boolean;
  
  function SendChar(AThread:TWinsock2TCPServerThread;AChar:Char):Boolean;
  function SendText(AThread:TWinsock2TCPServerThread;const AText:String):Boolean;
  function SendCommand(AThread:TWinsock2TCPServerThread;ACommand,AOption:Byte;AData:Pointer;ASize:LongWord):Boolean;
 end;
 
 {SSH classes}
 //To do
 
 {UPnP classes}
 
 {SysLog classes}
 TSyslogClient = class(TObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TMutexHandle;
 
  FProtocol:LongWord;
  FBoundPort:Word;
  FRemoteHost:String;
  FRemotePort:Word;
  FOctetCounting:Boolean;
  FBroadcastEnabled:Boolean;
  
  FUDPClient:TWinsock2UDPClient;
  FTCPClient:TWinsock2TCPClient;
  
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  procedure SetProtocol(AProtocol:LongWord);
  procedure SetBoundPort(ABoundPort:Word);
  function GetRemoteHost:String;
  procedure SetRemoteHost(const ARemoteHost:String);
  procedure SetRemotePort(ARemotePort:Word);
  procedure SetOctetCounting(AOctetCounting:Boolean);
  procedure SetBroadcastEnabled(ABroadcastEnabled:Boolean);
 protected
  {Protected Variables}
  
  {Protected Methods}
  function GetPriority(AFacility,ASeverity:LongWord):String;
  function GetMessage(const APriority,AAddress,ATag,AContent:String):String;
 public
  {Public Properties}
  property Protocol:LongWord read FProtocol write SetProtocol;
  property BoundPort:Word read FBoundPort write SetBoundPort;
  property RemoteHost:String read GetRemoteHost write SetRemoteHost;
  property RemotePort:Word read FRemotePort write SetRemotePort;
  property OctetCounting:Boolean read FOctetCounting write SetOctetCounting;
  property BroadcastEnabled:Boolean read FBroadcastEnabled write SetBroadcastEnabled;
  
  {Public Methods}
  function SendMessage(AFacility,ASeverity:LongWord;const ATag,AContent:String):LongWord;
 end;
 
{==============================================================================}
type
 {Syslog Logging specific types}
 PSyslogLogging = ^TSyslogLogging;
 
 {Syslog Logging}
 TSyslogLogging = record
  {Logging Properties}
  Logging:TLoggingDevice;
  {Syslog Properties}
  Client:TSyslogClient;
 end;
 
{==============================================================================}
{var}
 {Services specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure ServicesInit;

{==============================================================================}
{Service Functions}

{==============================================================================}
{Ping Functions}

{==============================================================================}
{NTP Functions}
procedure NTPUpdateTime(Client:TNTPClient);

{==============================================================================}
{Telnet Functions}

{==============================================================================}
{SSH Functions}

{==============================================================================}
{UPnP Functions}

{==============================================================================}
{SysLog Functions}
function SysLogLoggingStart(Logging:PLoggingDevice):LongWord;
function SysLogLoggingStop(Logging:PLoggingDevice):LongWord;

function SysLogLoggingOutput(Logging:PLoggingDevice;const Data:String):LongWord;
function SysLogLoggingOutputEx(Logging:PLoggingDevice;Facility,Severity:LongWord;const Tag,Content:String):LongWord;

function SysLogLoggingGetTarget(Logging:PLoggingDevice):String;
function SysLogLoggingSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;

{==============================================================================}
{Service Helper Functions}
procedure ServiceLog(Level:LongWord;const AText:String);
procedure ServiceLogInfo(const AText:String); inline;
procedure ServiceLogWarn(const AText:String); inline;
procedure ServiceLogError(const AText:String); inline;
procedure ServiceLogDebug(const AText:String); inline;

{==============================================================================}
{Ping Helper Functions}

{==============================================================================}
{NTP Helper Functions}
function NTPTimestampToHost(const Timestamp:TNTPTimestamp):TNTPTimestamp;
function NTPTimestampToNetwork(const Timestamp:TNTPTimestamp):TNTPTimestamp;

function NTPTimestampAdd(const Timestamp1,Timestamp2:TNTPTimestamp):TNTPTimestamp;
function NTPTimestampSubtract(const Timestamp1,Timestamp2:TNTPTimestamp):TNTPTimestamp;
function NTPTimestampDivide(const Timestamp:TNTPTimestamp;Divisor:LongWord):TNTPTimestamp;

function ClockTimeToNTPTimestamp(const Time:Int64):TNTPTimestamp;
function NTPTimestampToClockTime(const Timestamp:TNTPTimestamp):Int64;

function NTPTimestampToString(const Timestamp:TNTPTimestamp):String;

{==============================================================================}
{Telnet Helper Functions}
function TelnetCommandToString(Command:Byte):String;
function TelnetCommandHasOption(Command:Byte):Boolean;

function TelnetOptionToString(Option:Byte):String;

{==============================================================================}
{SSH Helper Functions}

{==============================================================================}
{UPnP Helper Functions}

{==============================================================================}
{SysLog Helper Functions}
function FileTimeToSysLogDateTime(const AFileTime:TFileTime):String;

function LoggingFacilityToSysLogFacility(Facility:LongWord):LongWord;
function LoggingSeverityToSysLogSeverity(Severity:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Services specific variables}
 ServicesInitialized:Boolean;
 
{==============================================================================}
{==============================================================================}
{TPingClient}
constructor TPingClient.Create;
{Constructor for TPingClient class}
begin
 {}
 inherited Create;
 
 FSize:=PING_DEFAULT_SIZE;
 FCount:=PING_DEFAULT_COUNT;
 FTimeout:=PING_DEFAULT_TIMEOUT;
end;

{==============================================================================}

procedure TPingClient.SetSize(ASize:LongWord);
{Set the size in bytes of the data for the ping request}
begin
 {}
 if FSize <> ASize then
  begin
   FSize:=ASize;
   if FSize > PING_MAXIMUM_SIZE then FSize:=PING_DEFAULT_SIZE;

   ResetPing;
  end; 
end;

{==============================================================================}

procedure TPingClient.SetCount(ACount:LongWord);
{Set the number of ping requests to perform}
begin
 {}
 if FCount <> ACount then
  begin
   FCount:=ACount;
   if FCount = 0 then FCount:=PING_DEFAULT_COUNT;

   ResetPing;
  end; 
end;

{==============================================================================}

procedure TPingClient.SetTimeout(ATimeout:LongWord);
{Set the time to wait for a reply to each request (Milliseconds)}
begin
 {}
 if FTimeout <> ATimeout then
  begin
   FTimeout:=ATimeout;
   if FTimeout = 0 then FTimeout:=PING_DEFAULT_TIMEOUT;

   ResetPing;
  end; 
end;

{==============================================================================}

procedure TPingClient.SetTimeToLive(ATimeToLive:Byte);
{Set the time to live in hops for each request}
begin
 {}
 if FTimeToLive <> ATimeToLive then
  begin
   FTimeToLive:=ATimeToLive;

   ResetPing;
  end; 
end;

{==============================================================================}

procedure TPingClient.SetNoFragment(ANoFragment:Boolean);
{Set the don't fragment bit in the header of each request}
begin
 {}
 if FNoFragment <> ANoFragment then
  begin
   FNoFragment:=ANoFragment;

   ResetPing;
  end; 
end;

{==============================================================================}

function TPingClient.GetLastCount:LongWord;
{Get the current count after the latest request}
begin
 {}
 Result:=FLastSequence;
end;

{==============================================================================}

function TPingClient.GetErrorCode:LongInt;
{Get the error code from the latest request}
begin
 {}
 Result:=FLastError;
end;

{==============================================================================}

function TPingClient.GetErrorText:String;
{Get the error message from the latest request}
begin
 {}
 Result:=Winsock2ErrorToString(FLastError);
end;

{==============================================================================}

procedure TPingClient.ResetPing;
{Reset the state and clear the current ping request}
begin
 {}
 {Disconnect}
 Disconnect;

 {Reset State}
 FLastHost:='';
 FLastAddress:='';
 
 FLastTime:=0;
 FLastError:=0;
 FReplyAddress:='';

 FLastSequence:=0;

 FSendCount:=0;
 FReceiveCount:=0;
 FLostCount:=0;
end;

{==============================================================================}

function TPingClient.FirstPing(const AHost:String):Boolean;
{Start a new ping sequence to the specified host or address}
{Host: The host name or address to ping}

{Note: After the first ping, NextPing should be called repeatedly until completed}
var
 Start:Int64;
 Len:LongWord;
 Count:LongInt;
 Reply:PByte;
 Request:PByte;
 WorkInt:LongInt;
begin
 {}
 Result:=False;

 {Check Host}
 if Length(AHost) = 0 then Exit;

 {Reset}
 ResetPing;

 {Set Protocol}
 Protocol:=IPPROTO_ICMP;

 {Set Connect}
 UseConnect:=False;

 {Update Sequence}
 Inc(FLastSequence);

 {Update Identifier}
 Inc(FLastIdentifier);

 {Connect}
 if not Connect then Exit;
 try
  {Set Host}
  FLastHost:=AHost;

  {Set Address}
  FLastAddress:=ResolveHost(FLastHost);
  if Length(FLastAddress) = 0 then Exit;

  {Set Timeouts}
  SendTimeout:=FTimeout;
  ReceiveTimeout:=FTimeout;

  {Set TTL}
  if FTimeToLive > 0 then
   begin
    WorkInt:=FTimeToLive;
    if Winsock2.setsockopt(Handle,IPPROTO_IP,IP_TTL,PChar(@WorkInt),SizeOf(LongInt)) <> ERROR_SUCCESS then Exit;
   end;

  {Set Dont Fragment}
  if FNoFragment then
   begin
    WorkInt:=1;
    if Winsock2.setsockopt(Handle,IPPROTO_IP,IP_DONTFRAGMENT,PChar(@WorkInt),SizeOf(LongInt)) <> ERROR_SUCCESS then Exit;
   end;

  {Get Length}
  Len:=SizeOf(TICMPEchoHeader) + FSize;

  {Allocate Request}
  Request:=AllocMem(Len);
  try
   {Setup Echo Request}
   PICMPEchoHeader(Request).ICMPType:=ICMP_ECHO;
   PICMPEchoHeader(Request).Code:=0;
   PICMPEchoHeader(Request).Checksum:=0;
   PICMPEchoHeader(Request).Identifier:=WordNtoBE(FLastIdentifier);
   PICMPEchoHeader(Request).Sequence:=WordNtoBE(FLastSequence);

   {Setup Echo Data}
   if FSize > 0 then
    begin
     FillChar(Pointer(Request + SizeOf(TICMPEchoHeader))^,FSize,#65);
    end;

   {Calculate the Checksum}
   PICMPEchoHeader(Request).Checksum:=ChecksumICMP(ResolveFamily(FLastAddress),Request,0,Len);

   {Get Time}
   Start:=GetTickCount64;

   {Send Echo Request}
   Count:=SendDataTo(FLastAddress,Request,Len);
   if Count = Len then
    begin
     {Update Count}
     Inc(FSendCount);

     {Allocate Reply}
     Reply:=AllocMem($FFFF);
     try
      {Receive Echo Reply}
      Count:=RecvDataFrom(FReplyAddress,Reply,$FFFF);
      if Count >= Len then
       begin
        {Save Time}
        FLastTime:=GetTickCount64 - Start;

        {Update Count}
        Inc(FReceiveCount);

        {Check Reply}
        if PICMPEchoHeader(Reply).ICMPType = ICMP_ECHOREPLY then
         begin
          {Check Sequence}
          if PICMPEchoHeader(Reply).Sequence = PICMPEchoHeader(Request).Sequence then
           begin
            Result:=True;
           end
          else
           begin
            FLastError:=WSAEINVAL;
           end; 
         end
        else if PICMPUnreachHeader(Reply).ICMPType = ICMP_UNREACH then
         begin
          {Check Code}
          if PICMPUnreachHeader(Reply).Code = ICMP_UNREACH_NET then
           begin
            FLastError:=WSAENETUNREACH;
           end
          else if PICMPUnreachHeader(Reply).Code = ICMP_UNREACH_HOST then 
           begin
            FLastError:=WSAEHOSTUNREACH;
           end
          else if PICMPUnreachHeader(Reply).Code = ICMP_UNREACH_NEEDFRAG then 
           begin
            FLastError:=WSAEOPNOTSUPP;
           end
          else
           begin
            FLastError:=WSAENETUNREACH; {WSAEINVAL}
           end;
         end
        else if PICMPExpireHeader(Reply).ICMPType = ICMP_TIMXCEED then
         begin
          FLastError:=WSAETIMEDOUT;
         end
        else
         begin
          FLastError:=WSAEINVAL;
         end;
       end
      else
       begin
        if FLastError = ERROR_SUCCESS then FLastError:=WSAEINVAL;

        {Update Count}
        Inc(FLostCount);
       end;
     finally
      {Free Reply}
      FreeMem(Reply);
     end; 
    end
   else
    begin
     if FLastError = ERROR_SUCCESS then FLastError:=WSAEINVAL;
    end;
  finally
   {Free Request}
   FreeMem(Request);
  end;
 finally
  {Disconnect}
  WorkInt:=FLastError;

  Disconnect;
  FLastError:=WorkInt;
 end; 
end;

{==============================================================================}

function TPingClient.NextPing:Boolean;
{Continue pinging the current address with the current parameters}

{Note: Once LastCount equals Count the sequence is complete and this method will return False}
var
 Start:Int64;
 Len:LongWord;
 Count:LongInt;
 Reply:PByte;
 Request:PByte;
 WorkInt:LongInt;
begin
 {}
 Result:=False;
 
 {Check Sequence}
 if FLastSequence = 0 then Exit;

 {Check Sequence}
 if FLastSequence = FCount then Exit;

 {Update Sequence}
 Inc(FLastSequence);

 {Connect}
 if not Connect then Exit;
 try
  {Set Timeouts}
  SendTimeout:=FTimeout;
  ReceiveTimeout:=FTimeout;

  {Set TTL}
  if FTimeToLive > 0 then
   begin
    WorkInt:=FTimeToLive;
    if Winsock2.setsockopt(Handle,IPPROTO_IP,IP_TTL,PChar(@WorkInt),SizeOf(LongInt)) <> ERROR_SUCCESS then Exit;
   end;

  {Set Dont Fragment}
  if FNoFragment then
   begin
    WorkInt:=1;
    if Winsock2.setsockopt(Handle,IPPROTO_IP,IP_DONTFRAGMENT,PChar(@WorkInt),SizeOf(LongInt)) <> ERROR_SUCCESS then Exit;
   end;

  {Get Length}
  Len:=SizeOf(TICMPEchoHeader) + FSize;

  {Allocate Request}
  Request:=AllocMem(Len);
  try
   {Setup Echo Request}
   PICMPEchoHeader(Request).ICMPType:=ICMP_ECHO;
   PICMPEchoHeader(Request).Code:=0;
   PICMPEchoHeader(Request).Checksum:=0;
   PICMPEchoHeader(Request).Identifier:=WordNtoBE(FLastIdentifier);
   PICMPEchoHeader(Request).Sequence:=WordNtoBE(FLastSequence);

   {Setup Echo Data}
   if FSize > 0 then
    begin
     FillChar(Pointer(Request + SizeOf(TICMPEchoHeader))^,FSize,#65);
    end;

   {Calculate the Checksum}
   PICMPEchoHeader(Request).Checksum:=ChecksumICMP(ResolveFamily(FLastAddress),Request,0,Len);

   {Get Start Count}
   Start:=GetTickCount64;

   {Send Echo Request}
   Count:=SendDataTo(FLastAddress,Request,Len);
   if Count = Len then
    begin
     {Update Count}
     Inc(FSendCount);

     {Allocate Reply}
     Reply:=AllocMem($FFFF);
     try
      {Receive Echo Reply}
      Count:=RecvDataFrom(FReplyAddress,Reply,$FFFF);
      if Count >= Len then
       begin
        {Save Time}
        FLastTime:=GetTickCount64 - Start;

        {Update Count}
        Inc(FReceiveCount);

        {Check Reply}
        if PICMPEchoHeader(Reply).ICMPType = ICMP_ECHOREPLY then
         begin
          {Check Sequence}
          if PICMPEchoHeader(Reply).Sequence = PICMPEchoHeader(Request).Sequence then
           begin
            Result:=True;
           end
          else
           begin
            FLastError:=WSAEINVAL;
           end; 
         end
        else if PICMPUnreachHeader(Reply).ICMPType = ICMP_UNREACH then
         begin
          {Check Code}
          if PICMPUnreachHeader(Reply).Code = ICMP_UNREACH_NET then
           begin
            FLastError:=WSAENETUNREACH;
           end
          else if PICMPUnreachHeader(Reply).Code = ICMP_UNREACH_HOST then 
           begin
            FLastError:=WSAEHOSTUNREACH;
           end
          else if PICMPUnreachHeader(Reply).Code = ICMP_UNREACH_NEEDFRAG then 
           begin
            FLastError:=WSAEOPNOTSUPP;
           end
          else
           begin
            FLastError:=WSAENETUNREACH; {WSAEINVAL}
           end;
         end
        else if PICMPExpireHeader(Reply).ICMPType = ICMP_TIMXCEED then
         begin
          FLastError:=WSAETIMEDOUT;
         end
        else
         begin
          FLastError:=WSAEINVAL;
         end;
       end
      else
       begin
        if FLastError = ERROR_SUCCESS then FLastError:=WSAEINVAL;

        {Update Count}
        Inc(FLostCount);
       end;
     finally
      {Free Reply}
      FreeMem(Reply);
     end; 
    end
   else
    begin
     if FLastError = ERROR_SUCCESS then FLastError:=WSAEINVAL;
    end;
  finally
   {Free Request}
   FreeMem(Request);
  end; 
 finally
  {Disconnect}
  WorkInt:=FLastError;

  Disconnect;
  FLastError:=WorkInt;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TNTPClient}
constructor TNTPClient.Create;
begin
 {}
 inherited Create;
 FLock:=MutexCreate;
 
 FPollInterval:=NTP_POLLING_INTERVAL;
 FPollTimeout:=NTP_POLLING_TIMEOUT;
 FPollRetries:=NTP_POLLING_RETRIES;
 
 FRetryTimeout:=NTP_RETRY_TIMEOUT;

 FUseClockOffset:=NTP_USE_CLOCK_OFFSET;
 FClockTolerance:=NTP_CLOCK_TOLERANCE;

 FInitialClockGet:=False;
 FInitialClockCount:=0;
 FInitialClockRetry:=True;
 
 FTimerHandle:=INVALID_HANDLE_VALUE;
 
 RemoteHost:=NTP_SERVER_DEFAULT;
 RemotePort:=NTP_PORT_DEFAULT;
end;

{==============================================================================}

destructor TNTPClient.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FTimerHandle <> INVALID_HANDLE_VALUE then TimerDestroy(FTimerHandle);
  FTimerHandle:=INVALID_HANDLE_VALUE;
  
  inherited Destroy;
 finally
  ReleaseLock;
  MutexDestroy(FLock);
 end; 
end;

{==============================================================================}

function TNTPClient.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTPClient.ReleaseLock:Boolean;  
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TNTPClient.SetPollInterval(APollInterval:LongWord);
begin
 {}
 if not AcquireLock then Exit;
 
 FPollInterval:=APollInterval;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetPollTimeout(APollTimeout:LongWord);
begin
 {}
 if not AcquireLock then Exit;
 
 FPollTimeout:=APollTimeout;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetPollRetries(APollRetries:LongWord);
begin
 {}
 if not AcquireLock then Exit;
 
 FPollRetries:=APollRetries;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetRetryTimeout(ARetryTimeout:LongWord);
begin
 {}
 if not AcquireLock then Exit;
 
 FRetryTimeout:=ARetryTimeout;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetUseClockOffset(AUseClockOffset:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FUseClockOffset:=AUseClockOffset;

 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetClockTolerance(AClockTolerance:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FClockTolerance:=AClockTolerance;

 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetInitialClockGet(AInitialClockGet:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 FInitialClockGet:=AInitialClockGet;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetInitialClockCount(AInitialClockCount:LongWord);
begin
 {}
 if not AcquireLock then Exit;
 
 FInitialClockCount:=AInitialClockCount;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetInitialClockRetry(AInitialClockRetry:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 FInitialClockRetry:=AInitialClockRetry;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTPClient.SetTimerHandle(ATimerHandle:TTimerHandle);
begin
 {}
 if not AcquireLock then Exit;
 
 FTimerHandle:=ATimerHandle;
 
 ReleaseLock;
end;

{==============================================================================}

function TNTPClient.GetTime:Int64;
var
 Leap:Byte;
 Mode:Byte;
 Version:Byte;
 Count:LongWord;
 NTPReply:PNTPPacket;
 NTPRequest:PNTPPacket;
 ReceiveTime:Int64;
 TransmitTime:Int64;
 ReferenceTime:Int64;
 OriginateTime:Int64;
 DestinationTime:Int64;
 ClockOffset:Int64;
 RoundtripDelay:Int64;
 DestinationTimestamp:TNTPTimestamp;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF NTP_DEBUG}
  if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: GetTime');
  {$ENDIF}
 
  {Connect}
  if not Connect then Exit;
  try
   {$IFDEF NTP_DEBUG}
   if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Connected');
   {$ENDIF}
  
   {Set Polling Send Timeout}
   SendTimeout:=PollTimeout;
  
   {Set Polling Receive Timeout}
   ReceiveTimeout:=PollTimeout;
 
   {Create NTP Request}
   NTPRequest:=AllocMem(SizeOf(TNTPPacket));
   if NTPRequest = nil then Exit;
   try
    {Set Polling Retries}
    Count:=PollRetries;
    while Count > 0 do
     begin
      {Connect}
      if not Connected then
       begin
        if not Connect then Exit;
        {$IFDEF NTP_DEBUG}
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Connected');
        {$ENDIF}
        
        {Set Polling Send Timeout}
        SendTimeout:=PollTimeout;
        
        {Set Polling Receive Timeout}
        ReceiveTimeout:=PollTimeout;
       end;
     
      {$IFDEF NTP_DEBUG}
      if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Sending Request');
      {$ENDIF}
      
      {Setup NTP Request}
      TransmitTime:=ClockGetTime;
      NTPRequest.LeapVersionMode:=(NTP_LEAP_NONE shl 6) or (NTP_VERSION shl 3) or (NTP_MODE_CLIENT shl 0);
      NTPRequest.TransmitTimestamp:=ClockTimeToNTPTimestamp(TransmitTime);
      
      {Send NTP Request}
      if SendData(NTPRequest,SizeOf(TNTPPacket)) = SizeOf(TNTPPacket) then
       begin
        {$IFDEF NTP_DEBUG}
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Request Sent to ' + RemoteHost + ' on port ' + IntToStr(RemotePort));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Leap = ' + IntToHex((NTPRequest.LeapVersionMode shr 6) and NTP_LEAP_MASK,2));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Version = ' + IntToHex((NTPRequest.LeapVersionMode shr 3) and NTP_VERSION_MASK,2));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Mode = ' + IntToHex((NTPRequest.LeapVersionMode shr 0) and NTP_MODE_MASK,2));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Stratum = ' + IntToHex(NTPRequest.Stratum,2));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Poll Interval = ' + IntToHex(NTPRequest.PollInterval,2));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Precision = ' + IntToHex(NTPRequest.Precision,2));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Root Delay = ' + IntToHex(NTPRequest.RootDelay,8));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Root Dispersion = ' + IntToHex(NTPRequest.RootDispersion,8));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Reference Timestamp = ' + NTPTimestampToString(NTPRequest.ReferenceTimestamp));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Originate Timestamp = ' + NTPTimestampToString(NTPRequest.OriginateTimestamp));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Receive Timestamp = ' + NTPTimestampToString(NTPRequest.ReceiveTimestamp));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Transmit Timestamp = ' + NTPTimestampToString(NTPRequest.TransmitTimestamp));
        if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:                       ' + FormatTime(TransmitTime));
        {$ENDIF}

        {Create NTP Reply}
        NTPReply:=AllocMem(SizeOf(TNTPPacket));
        if NTPReply = nil then Exit;
        try
         {$IFDEF NTP_DEBUG}
         if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Receiving Reply');
         {$ENDIF}

         {Receive NTP Reply}
         if RecvData(NTPReply,SizeOf(TNTPPacket)) > 0 then
          begin
           {Get Destination Timestamp}
           DestinationTime:=ClockGetTime;
           DestinationTimestamp:=ClockTimeToNTPTimestamp(DestinationTime);

           {Get Leap}
           Leap:=(NTPReply.LeapVersionMode shr 6) and NTP_LEAP_MASK;

           {Get Version}
           Version:=(NTPReply.LeapVersionMode shr 3) and NTP_VERSION_MASK;

           {Get Mode}
           Mode:=(NTPReply.LeapVersionMode shr 0) and NTP_MODE_MASK;

           {Get Reference Time}
           ReferenceTime:=NTPTimestampToClockTime(NTPReply.ReferenceTimestamp);

           {Get Originate Time}
           OriginateTime:=NTPTimestampToClockTime(NTPReply.OriginateTimestamp);

           {Get Receive Time}
           ReceiveTime:=NTPTimestampToClockTime(NTPReply.ReceiveTimestamp);

           {Get Transmit Time}
           TransmitTime:=NTPTimestampToClockTime(NTPReply.TransmitTimestamp);

           {$IFDEF NTP_DEBUG}
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Reply Received');
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Leap = ' + IntToHex(Leap,2));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Version = ' + IntToHex(Version,2));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Mode = ' + IntToHex(Mode,2));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Stratum = ' + IntToHex(NTPReply.Stratum,2));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Poll Interval = ' + IntToHex(NTPReply.PollInterval,2));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Precision = ' + IntToHex(NTPReply.Precision,2));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Root Delay = ' + IntToHex(NTPReply.RootDelay,8));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Root Dispersion = ' + IntToHex(NTPReply.RootDispersion,8));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Reference Timestamp = ' + NTPTimestampToString(NTPReply.ReferenceTimestamp));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:                        ' + FormatTime(ReferenceTime));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Originate Timestamp = ' + NTPTimestampToString(NTPReply.OriginateTimestamp));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:                        ' + FormatTime(OriginateTime));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Receive Timestamp = ' + NTPTimestampToString(NTPReply.ReceiveTimestamp));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:                      ' + FormatTime(ReceiveTime));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Transmit Timestamp = ' + NTPTimestampToString(NTPReply.TransmitTimestamp));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:                       ' + FormatTime(TransmitTime));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Destination Timestamp = ' + NTPTimestampToString(DestinationTimestamp));
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:                          ' + FormatTime(DestinationTime));
           {$ENDIF}

           {Check NTP Reply}
           {Leap}
           if Leap = NTP_LEAP_ALARM then
            begin
             if SERVICE_LOG_ENABLED then ServiceLogError('NTP Client: Leap indicator set to NTP_LEAP_ALARM in reply');
             Exit;
            end; 
           {Version}
           if (Version <> NTP_VERSION) and ((Version < NTP_MIN_VERSION) or (Version > NTP_MAX_VERSION)) then
            begin
             if SERVICE_LOG_ENABLED then ServiceLogError('NTP Client: Version incorrect in reply (Version=' + IntToStr(Version) + ' NTP_VERSION=' + IntToStr(NTP_VERSION) + ')');
             Exit;
            end; 
           {Mode}
           if (Mode <> NTP_MODE_SERVER) and (Mode <> NTP_MODE_BROADCAST) then
            begin
             if SERVICE_LOG_ENABLED then ServiceLogError('NTP Client: Mode not equal to server or broadcast in reply (Mode=' + IntToStr(Mode) + ')');
             Exit;
            end; 
           {Stratum}
           if NTPReply.Stratum  = NTP_STRATUM_INVALID then
            begin
             if SERVICE_LOG_ENABLED then ServiceLogError('NTP Client: Stratum set to invalid in reply');
             Exit;
            end; 
           {Timestamp}
           if (NTPReply.TransmitTimestamp.Seconds = 0) and (NTPReply.TransmitTimestamp.Fraction = 0) then
            begin
             if SERVICE_LOG_ENABLED then ServiceLogError('NTP Client: Transmit timestamp not valid in reply');
             Exit;
            end; 

           {$IFDEF NTP_DEBUG}
           if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Reply Validated');
           {$ENDIF}

           {Check Time and Offset}
           if (DestinationTime > TIME_TICKS_TO_2001) and UseClockOffset then
            begin
             {T1 = Originate Timestamp (time request sent by client)
              T2 = Receive Timestamp (time request received by server)
              T3 = Transmit Timestamp (time reply sent by server)
              T4 = Destination Timestamp (time reply received by client)}

             {Calculate Roudtrip Delay} {RoundtripDelay = (T4 - T1) - (T3 - T2)}
             {RoundtripDelay:=NTPTimestampSubtract(NTPTimestampSubtract(DestinationTimestamp,NTPReply.OriginateTimestamp),NTPTimestampSubtract(NTPReply.TransmitTimestamp,NTPReply.ReceiveTimestamp));}
             RoundtripDelay:=CalculateRoundtripDelay(OriginateTime,ReceiveTime,TransmitTime,DestinationTime);

             {$IFDEF NTP_DEBUG}
             if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Roundtrip Delay = ' + FormatOffset(RoundtripDelay));
             {$ENDIF}

             {Calculate Clock  Offset} {ClockOffset = ((T2 - T1) + (T3 - T4)) / 2}
             {ClockOffset:=NTPTimestampDivide(NTPTimestampAdd(NTPTimestampSubtract(NTPReply.ReceiveTimestamp,NTPReply.OriginateTimestamp),NTPTimestampSubtract(NTPReply.TransmitTimestamp,DestinationTimestamp)),2);}
             ClockOffset:=CalculateClockOffset(OriginateTime,ReceiveTime,TransmitTime,DestinationTime);

             {$IFDEF NTP_DEBUG}
             if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Clock Offset = ' + FormatOffset(ClockOffset));
             {$ENDIF}

             {Get Time}
             Result:=DestinationTime + ClockOffset;
            end
           else
            begin
             {$IFDEF NTP_DEBUG}
             if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client:  Local time not set or clock offset disabled');
             {$ENDIF}

             {Get Time}
             Result:=TransmitTime;
            end;
           Exit;
          end;          
        finally
         FreeMem(NTPReply);
        end;     
       end;       

      Dec(Count);
      Sleep(RetryTimeout);

      {$IFDEF NTP_DEBUG}
      if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Client: Retrying Request (Retries remaining=' + IntToStr(Count) + ')');
      {$ENDIF}
     end;
   finally
    FreeMem(NTPRequest);
   end;  
  finally 
   {Disconnect}
   Disconnect;
  end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TNTPClient.IncrementInitialClockCount;
begin
 {}
 if not AcquireLock then Exit;
 
 Inc(FInitialClockCount);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTPClient.FormatTime(Time:Int64):String;
begin
 {}
 Result:=SystemDateTimeToString(SystemFileTimeToDateTime(TFileTime(Time)));
end;

{==============================================================================}

function TNTPClient.FormatOffset(Offset:Int64):String;
var
 Seconds:Int64;
 Microseconds:Int64;
begin
 {}
 Seconds:=Offset div TIME_TICKS_PER_SECOND;
 Microseconds:=(Offset mod TIME_TICKS_PER_SECOND) div TIME_TICKS_PER_MICROSECOND;
 
 Result:=IntToStr(Seconds) + ' seconds ' + IntToStr(Microseconds) + ' microseconds';
end;

{==============================================================================}

function TNTPClient.CalculateClockOffset(T1,T2,T3,T4:Int64):Int64;
{T1 = Originate Timestamp (time request sent by client)
 T2 = Receive Timestamp (time request received by server)
 T3 = Transmit Timestamp (time reply sent by server)
 T4 = Destination Timestamp (time reply received by client)}
{ClockOffset = ((T2 - T1) + (T3 - T4)) / 2} 
begin
 {}
 Result:=((T2 - T1) + (T3 - T4)) div 2;
end;

{==============================================================================}

function TNTPClient.CalculateRoundtripDelay(T1,T2,T3,T4:Int64):Int64;
{T1 = Originate Timestamp (time request sent by client)
 T2 = Receive Timestamp (time request received by server)
 T3 = Transmit Timestamp (time reply sent by server)
 T4 = Destination Timestamp (time reply received by client)}
{RoundtripDelay = (T4 - T1) - (T3 - T2)}
begin
 {}
 Result:=(T4 - T1) - (T3 - T2);
end;

{==============================================================================}
{==============================================================================}
{TTelnetBuffer}
constructor TTelnetBuffer.Create(ASize:LongWord);
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

destructor TTelnetBuffer.Destroy; 
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

function TTelnetBuffer.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTelnetBuffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTelnetBuffer.GetCount:LongWord;  
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=FCount;
 
 ReleaseLock; 
end;

{==============================================================================}

function TTelnetBuffer.ReadData:Char;
begin
 {}
 Result:=TELNET_CHAR_NUL;
 
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

function TTelnetBuffer.WriteData(AChar:Char):Boolean;
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

function TTelnetBuffer.WriteLock(var ASize:LongWord):Pointer;
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

function TTelnetBuffer.WriteUnlock(ACount:LongWord):Boolean;
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
{TTelnetConnection}
constructor TTelnetConnection.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FHandle:=THandle(Self);
 FRxByteCount:=0;
 FTxByteCount:=0;
 FRequestCount:=0;
 FResponseCount:=0;
 FRequestTime:=Now;
 FResponseTime:=Now;
 FRemoteAddress:='';
 FLocalState:=TELNET_STATE_NONE; 
 FRemoteState:=TELNET_STATE_NONE;
 
 FData:=nil;
 FThread:=nil;
 FBuffer:=TTelnetBuffer.Create(TELNET_BUFFER_SIZE);
 FListener:=nil;
end;

{==============================================================================}

destructor TTelnetConnection.Destroy; 
begin
 {}
 AcquireLock;
 try
  FData:=nil;
  FThread:=nil;
  FBuffer.Free;
  FListener:=nil;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TTelnetConnection.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTelnetConnection.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TTelnetConnection.SetHandle(AHandle:THandle);
begin
 {}
 if not AcquireLock then Exit;

 FHandle:=AHandle;

 ReleaseLock;
end;

{==============================================================================}

function TTelnetConnection.GetRxByteCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FRxByteCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetRxByteCount(const ARxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FRxByteCount:=ARxByteCount;

 ReleaseLock;
end;

{==============================================================================}

function TTelnetConnection.GetTxByteCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FTxByteCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetTxByteCount(const ATxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FTxByteCount:=ATxByteCount;

 ReleaseLock;
end;

{==============================================================================}

function TTelnetConnection.GetRequestCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FRequestCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetRequestCount(const ARequestCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FRequestCount:=ARequestCount;

 ReleaseLock;
end;

{==============================================================================}

function TTelnetConnection.GetResponseCount:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FResponseCount;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetResponseCount(const AResponseCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FResponseCount:=AResponseCount;

 ReleaseLock;
end;

{==============================================================================}

function TTelnetConnection.GetRequestTime:TDateTime;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FRequestTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetRequestTime(const ARequestTime:TDateTime);
begin
 {}
 if not AcquireLock then Exit;

 FRequestTime:=ARequestTime;

 ReleaseLock;
end;

{==============================================================================}

function TTelnetConnection.GetResponseTime:TDateTime;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FResponseTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetResponseTime(const AResponseTime:TDateTime);
begin
 {}
 if not AcquireLock then Exit;

 FResponseTime:=AResponseTime;

 ReleaseLock;
end;

{==============================================================================}

function TTelnetConnection.GetRemoteAddress:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FRemoteAddress;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetRemoteAddress(const ARemoteAddress:String);
begin
 {}
 if not AcquireLock then Exit;

 FRemoteAddress:=ARemoteAddress;
 UniqueString(FRemoteAddress);

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetLocalState(ALocalState:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FLocalState:=ALocalState;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetRemoteState(ARemoteState:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FRemoteState:=ARemoteState;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetData(AData:Pointer);
begin
 {}
 if not AcquireLock then Exit;

 FData:=AData;

 ReleaseLock;
end;

{==============================================================================}
 
procedure TTelnetConnection.SetThread(AThread:TThread);
begin
 {}
 if not AcquireLock then Exit;

 FThread:=AThread;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.SetListener(AListener:TTelnetListener);
begin
 {}
 if not AcquireLock then Exit;

 FListener:=AListener;

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.IncrementRxByteCount(const ARxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 Inc(FRxByteCount,ARxByteCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.IncrementTxByteCount(const ATxByteCount:Int64);
begin
 {}
 if not AcquireLock then Exit;

 Inc(FTxByteCount,ATxByteCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.IncrementRequestCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FRequestCount);

 ReleaseLock;
end;

{==============================================================================}

procedure TTelnetConnection.IncrementResponseCount;
begin
 {}
 if not AcquireLock then Exit;

 Inc(FResponseCount);

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TTelnetListener}
constructor TTelnetListener.Create;
begin
 {}
 inherited Create;
 BoundPort:=TELNET_PORT_DEFAULT;
 UseNagle:=False; {Note: Nagle is recommended for Telnet to reduce small packets}
end;

{==============================================================================}

procedure TTelnetListener.DoConnect(AThread:TWinsock2TCPServerThread); 
var
 Connection:TTelnetConnection;
begin
 {}
 inherited DoConnect(AThread);
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoConnect');
 {$ENDIF}
 
 {Create Connection}
 Connection:=TTelnetConnection.Create;
 Connection.RemoteAddress:=AThread.Server.PeerAddress;
 Connection.Thread:=AThread;
 Connection.Listener:=Self;
 Connection.RequestTime:=Now;
 Connection.ResponseTime:=Now;
 
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
 
 {Send Command (WILL ECHO)}
 if not SendCommand(AThread,TELNET_COMMAND_WILL,TELNET_OPTION_ECHO,nil,0) then
  begin
   {Terminate Connection}
   AThread.Server.Disconnect;
   Exit;
  end;
  
 {Send Command (DO SUPPRESS GA)}
 if not SendCommand(AThread,TELNET_COMMAND_DO,TELNET_OPTION_SUPPRESS_GA,nil,0) then
  begin
   {Terminate Connection}
   AThread.Server.Disconnect;
   Exit;
  end;
  
 {Init Event}
 DoInit(AThread);
end;

{==============================================================================}

procedure TTelnetListener.DoDisconnect(AThread:TWinsock2TCPServerThread); 
begin
 {}
 inherited DoDisconnect(AThread);
 
 if AThread = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoDisconnect');
 {$ENDIF}
 
 {Disconnected Event}
 if Assigned(FOnDisconnected) then
  begin
   FOnDisconnected(TTelnetConnection(AThread.Data));
  end;
end;

{==============================================================================}

function TTelnetListener.DoCheckHost(AThread:TWinsock2TCPServerThread):Boolean;
begin
 {}
 Result:=True;
 
 if AThread = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoCheckHost');
 {$ENDIF}
 
 {Check Host Event}
 if Assigned(FOnCheckHost) then
  begin
   Result:=FOnCheckHost(TTelnetConnection(AThread.Data));
  end;
end;

{==============================================================================}

function TTelnetListener.DoCheckCount(AThread:TWinsock2TCPServerThread):Boolean; 
begin
 {}
 Result:=True;
 
 if AThread = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoCheckCount');
 {$ENDIF}
 
 {Check Count Event}
 if Assigned(FOnCheckCount) then
  begin
   Result:=FOnCheckCount(TTelnetConnection(AThread.Data));
  end;
end;

{==============================================================================}
  
function TTelnetListener.DoExecute(AThread:TWinsock2TCPServerThread):Boolean; 
var
 Value:Char;
begin
 {}
 Result:=inherited DoExecute(AThread);
 if not Result then Exit;
 
 Result:=False;

 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoExecute');
 {$ENDIF}
 
 {Check Connected}
 if AThread.Server.Connected then
  begin
   {Get Char}
   if not GetChar(AThread,Value) then Exit;

   {Check for TELNET_COMMAND_IAC}
   if Ord(Value) = TELNET_COMMAND_IAC then
    begin
     {Get Command}
     if not GetChar(AThread,Value) then Exit;
     
     {Check Command}
     case Ord(Value) of
      TELNET_COMMAND_IAC:begin
        {Char Event}
        DoChar(AThread,Value);
       end;
      TELNET_COMMAND_DONT:begin
        {Get Option}
        if not GetChar(AThread,Value) then Exit;
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_DONT,Ord(Value),nil,0);
       end;
      TELNET_COMMAND_DO:begin
        {Get Option}
        if not GetChar(AThread,Value) then Exit;

        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_DO,Ord(Value),nil,0);
       end;
      TELNET_COMMAND_WONT:begin
        {Get Option}
        if not GetChar(AThread,Value) then Exit;
      
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_WONT,Ord(Value),nil,0);
       end;
      TELNET_COMMAND_WILL:begin
        {Get Option}
        if not GetChar(AThread,Value) then Exit;
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_WILL,Ord(Value),nil,0);
       end;
      TELNET_COMMAND_SB:begin
        {Begin Subnegotiations}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_SB,0,nil,0);
       end;
      TELNET_COMMAND_GA:begin
        {Go Ahead}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_GA,0,nil,0);
       end;
      TELNET_COMMAND_EL:begin
        {Erase Line}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_EL,0,nil,0);
       end;        
      TELNET_COMMAND_EC:begin
        {Erase Character}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_EC,0,nil,0);
       end;
      TELNET_COMMAND_AYT:begin
        {Are you there}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_AYT,0,nil,0);
       end;
      TELNET_COMMAND_AO:begin
        {Abort Output}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_AO,0,nil,0);
       end;
      TELNET_COMMAND_IP:begin
        {Interrupt Process}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_IP,0,nil,0);
       end;
      TELNET_COMMAND_BRK:begin
        {Break}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_BRK,0,nil,0);
       end;
      TELNET_COMMAND_DM:begin
        {Data Mark}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_DM,0,nil,0);
       end;
      TELNET_COMMAND_NOP:begin
        {No Operation}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_NOP,0,nil,0);
       end;
      TELNET_COMMAND_SE:begin
        {End Subnegotiations}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_SE,0,nil,0);
       end;
      TELNET_COMMAND_EOR:begin
        {End of Record}
        
        {Command Event}
        DoCommand(AThread,TELNET_COMMAND_EOR,0,nil,0);
       end;
     end;
    end
   else
    begin     
     {Check for CR}
     if Value = TELNET_CHAR_CR then
      begin
       {Send Echo (Unconditional)}
       if not SendEcho(AThread,Value) then Exit;

       {Char Event}
       DoChar(AThread,Value);
      
       {Get Char}
       if not GetChar(AThread,Value) then Exit;
       
       {Check for NUL or LF}
       if (Value = TELNET_CHAR_NUL) or (Value = TELNET_CHAR_LF) then
        begin
         {Send Echo (Unconditional)}
         if not SendEcho(AThread,Value) then Exit;

         {Char Event}
         DoChar(AThread,TELNET_CHAR_LF);
        end
       else
        begin
         {Char Event}
         DoChar(AThread,Value);

         {Send Echo}
         if not SendEcho(AThread,Value) then Exit;
        end;        
      end
     {Check for LF} 
     else if Value = TELNET_CHAR_LF then
      begin
       {Send Echo (Unconditional)}
       if not SendEcho(AThread,Value) then Exit;

       {Char Event}
       DoChar(AThread,Value);
      end      
     {All other characters} 
     else 
      begin
       {Char Event}
       DoChar(AThread,Value);
       
       {Send Echo}
       if not SendEcho(AThread,Value) then Exit;
      end;
    end;
   
   Result:=True;
  end;
end;

{==============================================================================}

procedure TTelnetListener.DoInit(AThread:TWinsock2TCPServerThread);
var
 Connection:TTelnetConnection;
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoInit');
 {$ENDIF}
 
 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;
 
 {Init Event}
 if Assigned(FOnInit) then
  begin
   if not FOnInit(Connection) then
    begin
     {Terminate Connection}
     AThread.Server.Disconnect;
    end;
  end;
  
 {Init Default} 
 {Nothing}
end;
 
{==============================================================================}
 
procedure TTelnetListener.DoChar(AThread:TWinsock2TCPServerThread;AChar:Char);
var
 Connection:TTelnetConnection;
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoChar');
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Char = ' + AChar + ' (' + IntToStr(Ord(AChar)) + ' / 0x' + IntToHex(Ord(AChar),2) + ')');
 {$ENDIF}
 
 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;
 
 {Update Connection}
 Connection.RequestTime:=Now;
 Connection.IncrementRequestCount;
 Connection.IncrementRxByteCount(1);
 
 {Char Event}
 if Assigned(FOnChar) then
  begin
   if FOnChar(Connection,AChar) then
    begin
     Exit;
    end; 
  end;
  
 {Char Default} 
 {Nothing}
end;
 
{==============================================================================}

procedure TTelnetListener.DoCommand(AThread:TWinsock2TCPServerThread;ACommand,AOption:Byte;AData:Pointer;ASize:LongWord);
var
 Size:LongWord;
 State:LongWord;
 Connection:TTelnetConnection;
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: DoCommand');
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Command = ' + TelnetCommandToString(ACommand));
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Option = ' + TelnetOptionToString(AOption));
 {$ENDIF}
 
 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;
 
 {Get Size}
 Size:=2;
 if TelnetCommandHasOption(ACommand) then Size:=3;
 if (AData <> nil) and (ASize > 0) then
  begin
   {Check Size}
   if ASize > 253 then Exit;
   
   {Get Size}
   Size:=Size + ASize;
  end;
 
 {Update Connection}
 Connection.RequestTime:=Now;
 Connection.IncrementRequestCount;
 Connection.IncrementRxByteCount(Size);
 
 {Command Event}
 if Assigned(FOnCommand) then
  begin
   if FOnCommand(Connection,ACommand,AOption,AData,ASize) then
    begin
     Exit;
    end; 
  end;
  
 {Command Default} 
 {Check Option}
 case AOption of
  TELNET_OPTION_TRANSMIT_BINARY:begin
    State:=TELNET_STATE_TRANSMIT_BINARY;
   end;
  TELNET_OPTION_ECHO:begin
    State:=TELNET_STATE_ECHO;
   end;
  TELNET_OPTION_SUPPRESS_GA:begin
    State:=TELNET_STATE_SUPPRESS_GA;
   end;
  TELNET_OPTION_TERMINAL_TYPE:begin
    State:=TELNET_STATE_TERMINAL_TYPE;
   end;
  TELNET_OPTION_WINDOW_SIZE:begin
    State:=TELNET_STATE_WINDOW_SIZE;
   end;
  TELNET_OPTION_TERMINAL_SPEED:begin
    State:=TELNET_STATE_TERMINAL_SPEED;
   end;
  TELNET_OPTION_NEW_ENVIRONMENT:begin
    State:=TELNET_STATE_NEW_ENVIRONMENT;
   end;
  else
   begin
    State:=TELNET_STATE_NONE;
   end;   
 end;
 
 {Check Command}
 case ACommand of
  TELNET_COMMAND_EOR:begin
    {Ignore}
   end;
  TELNET_COMMAND_SE:begin
    {Ignore}
   end;
  TELNET_COMMAND_NOP:begin
    {Ignore}
   end;
  TELNET_COMMAND_DM:begin
    {Ignore}
   end;
  TELNET_COMMAND_BRK:begin
    {Ignore}
   end;
  TELNET_COMMAND_IP:begin 
    {Ignore}
   end;
  TELNET_COMMAND_AO:begin 
    {Ignore}
   end;
  TELNET_COMMAND_AYT:begin 
    {Are You There}
    {Send Text}
    SendText(AThread,'Ultibo/' + ULTIBO_RELEASE_VERSION);
   end;
  TELNET_COMMAND_EC:begin
    {Ignore}
   end;
  TELNET_COMMAND_EL:begin 
    {Ignore}
   end;
  TELNET_COMMAND_GA:begin
    {Ignore}
   end;
  TELNET_COMMAND_SB:begin  
    {Ignore}
   end;
  TELNET_COMMAND_WILL:begin
    {Will}
    {Update State}
    if State <> TELNET_STATE_NONE then
     begin
      Connection.RemoteState:=Connection.RemoteState or State;
     end;    
   end;
  TELNET_COMMAND_WONT:begin
    {Won't}
    {Update State}
    if State <> TELNET_STATE_NONE then
     begin
      Connection.RemoteState:=Connection.RemoteState and not(State);
     end;    
   end;
  TELNET_COMMAND_DO:begin 
    {Do}
    {Send Command}
    if State <> TELNET_STATE_NONE then
     begin
      SendCommand(AThread,TELNET_COMMAND_WILL,AOption,nil,0);
     end;
   end;
  TELNET_COMMAND_DONT:begin 
    {Don't}
    {Send Command}
    if State <> TELNET_STATE_NONE then
     begin
      SendCommand(AThread,TELNET_COMMAND_WONT,AOption,nil,0);
     end;
   end;
  TELNET_COMMAND_IAC:begin  
    {Ignore (Handled by DoChar}
   end;
 end;
end;

{==============================================================================}

function TTelnetListener.GetChar(AThread:TWinsock2TCPServerThread;var AChar:Char):Boolean;
var
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 AChar:=TELNET_CHAR_NUL;
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: GetChar');
 {$ENDIF}
 
 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;
 
 {Get Next}
 Completed:=False;
 while not(Completed) do
  begin
   {$IFDEF TELNET_DEBUG}
   if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Buffer Count = ' + IntToStr(Connection.Buffer.Count));
   {$ENDIF}
   
   {Read from Buffer}
   if Connection.Buffer.Count > 0 then
    begin
     {Read Value}
     AChar:=Connection.Buffer.ReadData;
     
     {Mark Completed}
     Completed:=True;
    end;
    
   {Check Completed}
   if Completed then Break;
   
   {Read from Socket}
   Data:=Connection.Buffer.WriteLock(Size);
   if Data = nil then Exit;
   try
    {$IFDEF TELNET_DEBUG}
    if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Buffer Write Size = ' + IntToStr(Size));
    {$ENDIF}

    Count:=0;
      
    {Read Available}
    if not AThread.Server.ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;
    
    {$IFDEF TELNET_DEBUG}
    if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Buffer Write Count = ' + IntToStr(Count));
    {$ENDIF}
   finally
    Connection.Buffer.WriteUnlock(Count);
   end; 
  end;  
  
 {Return Result}
 Result:=True; 
  
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Char = ' + AChar + ' (' + IntToStr(Ord(AChar)) + ' / 0x' + IntToHex(Ord(AChar),2) + ')');
 {$ENDIF}
end;

{==============================================================================}

function TTelnetListener.SendEcho(AThread:TWinsock2TCPServerThread;AChar:Char):Boolean;
var
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: SendEcho');
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Char = ' + AChar + ' (' + IntToStr(Ord(AChar)) + ' / 0x' + IntToHex(Ord(AChar),2) + ')');
 {$ENDIF}

 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;
 
 {Check Echo}
 if (Connection.LocalState and TELNET_STATE_ECHO) <> 0 then
  begin
   {Send Echo}
   if not AThread.Server.WriteData(@AChar,1) then Exit;
   
   {Update Connection}
   Connection.ResponseTime:=Now;
   Connection.IncrementResponseCount;
   Connection.IncrementTxByteCount(1);
  end; 
  
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function TTelnetListener.SendChar(AThread:TWinsock2TCPServerThread;AChar:Char):Boolean;
var
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: SendChar');
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Char = ' + AChar + ' (' + IntToStr(Ord(AChar)) + ' / 0x' + IntToHex(Ord(AChar),2) + ')');
 {$ENDIF}

 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;
 
 {Send Char}
 if not AThread.Server.WriteData(@AChar,1) then Exit;
   
 {Update Connection}
 Connection.ResponseTime:=Now;
 Connection.IncrementResponseCount;
 Connection.IncrementTxByteCount(1);
  
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function TTelnetListener.SendText(AThread:TWinsock2TCPServerThread;const AText:String):Boolean;
var
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: SendText');
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Text = ' + AText);
 {$ENDIF}
 
 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;

 {Send Text}
 if not AThread.Server.WriteData(PChar(AText),Length(AText)) then Exit;
 
 {Update Connection}
 Connection.ResponseTime:=Now;
 Connection.IncrementResponseCount;
 Connection.IncrementTxByteCount(Length(AText));
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function TTelnetListener.SendCommand(AThread:TWinsock2TCPServerThread;ACommand,AOption:Byte;AData:Pointer;ASize:LongWord):Boolean;
var
 Size:LongWord;
 State:LongWord;
 Data:array[0..255] of Byte;
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {$IFDEF TELNET_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener: SendCommand');
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Command = ' + TelnetCommandToString(ACommand));
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Option = ' + TelnetOptionToString(AOption));
 if SERVICE_LOG_ENABLED then ServiceLogDebug('Telnet Listener:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Connection}
 Connection:=TTelnetConnection(AThread.Data);
 if Connection = nil then Exit;
 
 {Get Size}
 Size:=2;
 if TelnetCommandHasOption(ACommand) then Size:=3;
 if (AData <> nil) and (ASize > 0) then
  begin
   {Check Size}
   if ASize > 253 then Exit;
   
   {Get Size}
   Size:=Size + ASize;
  end;
 
 {Get Data}
 Data[0]:=TELNET_COMMAND_IAC;
 Data[1]:=ACommand;
 Data[2]:=AOption;
 if (AData <> nil) and (ASize > 0) then
  begin
   {Copy Data}
   System.Move(AData^,Data[3],ASize);
  end;
  
 {Send Command}
 if not AThread.Server.WriteData(@Data,Size) then Exit;
 
 {Check Option}
 case AOption of
  TELNET_OPTION_TRANSMIT_BINARY:begin
    State:=TELNET_STATE_TRANSMIT_BINARY;
   end;
  TELNET_OPTION_ECHO:begin
    State:=TELNET_STATE_ECHO;
   end;
  TELNET_OPTION_SUPPRESS_GA:begin
    State:=TELNET_STATE_SUPPRESS_GA;
   end;
  TELNET_OPTION_TERMINAL_TYPE:begin
    State:=TELNET_STATE_TERMINAL_TYPE;
   end;
  TELNET_OPTION_WINDOW_SIZE:begin
    State:=TELNET_STATE_WINDOW_SIZE;
   end;
  TELNET_OPTION_TERMINAL_SPEED:begin
    State:=TELNET_STATE_TERMINAL_SPEED;
   end;
  TELNET_OPTION_NEW_ENVIRONMENT:begin
    State:=TELNET_STATE_NEW_ENVIRONMENT;
   end;
  else
   begin
    State:=TELNET_STATE_NONE;
   end;   
 end;
 
 {Check Comand}
 if ACommand = TELNET_COMMAND_WILL then
  begin
   {Update State}
   if State <> TELNET_STATE_NONE then
    begin
     Connection.LocalState:=Connection.LocalState or State;
    end;    
  end
 else if ACommand = TELNET_COMMAND_WONT then
  begin
   {Update State}
   if State <> TELNET_STATE_NONE then
    begin
     Connection.LocalState:=Connection.LocalState and not(State);
    end;    
  end;
 
 {Update Connection}
 Connection.ResponseTime:=Now;
 Connection.IncrementResponseCount;
 Connection.IncrementTxByteCount(Size);
  
 {Return Result}
 Result:=True; 
end;

{==============================================================================}
{==============================================================================}
{TSSHListener}

{==============================================================================}
{==============================================================================}
{TSyslogClient}
constructor TSyslogClient.Create;
begin
 {}
 inherited Create;
 FLock:=MutexCreate;

 FProtocol:=SYSLOG_PROTOCOL_DEFAULT;
 FBoundPort:=SYSLOG_BOUND_PORT;
 FRemoteHost:=SYSLOG_SERVER_DEFAULT;
 FRemotePort:=SYSLOG_PORT_DEFAULT;
 FOctetCounting:=SYSLOG_OCTET_COUNTING;
 FBroadcastEnabled:=SYSLOG_BROADCAST_ENABLED;
 
 FUDPClient:=nil;
 FTCPClient:=nil;
end;
 
{==============================================================================}

destructor TSyslogClient.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FUDPClient <> nil then FUDPClient.Free;
  if FTCPClient <> nil then FTCPClient.Free;
  
  FUDPClient:=nil;
  FTCPClient:=nil;
  
  inherited Destroy;
 finally
  ReleaseLock;
  MutexDestroy(FLock);
 end; 
end;

{==============================================================================}

function TSyslogClient.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSyslogClient.ReleaseLock:Boolean;  
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TSyslogClient.SetProtocol(AProtocol:LongWord);
begin
 {}
 if AProtocol = FProtocol then Exit;
 
 if not AcquireLock then Exit;
 try
  {Check Protocol} 
  case AProtocol of
   LOGGING_PROTOCOL_UDP:begin
     {Close TCP Client}
     if FTCPClient <> nil then FTCPClient.Free;
     FTCPClient:=nil;
     
     FProtocol:=AProtocol;
    end;
   LOGGING_PROTOCOL_TCP:begin
     {Close UDP Client}
     if FUDPClient <> nil then FUDPClient.Free;
     FUDPClient:=nil;
     
     FProtocol:=AProtocol;
    end;
  end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TSyslogClient.SetBoundPort(ABoundPort:Word);
begin
 {}
 if ABoundPort = FBoundPort then Exit;

 if not AcquireLock then Exit;
 try
  {Check Protocol} 
  case FProtocol of
   LOGGING_PROTOCOL_UDP:begin
     FBoundPort:=ABoundPort;
     
     {Disconnect UDP Client}
     if FUDPClient <> nil then
      begin
       FUDPClient.Disconnect;
       FUDPClient.BoundPort:=FBoundPort;
      end; 
    end;
   LOGGING_PROTOCOL_TCP:begin
     FBoundPort:=ABoundPort;
     
     {Disconnect TCP Client}
     if FTCPClient <> nil then
      begin
       FTCPClient.Disconnect;
       FTCPClient.BoundPort:=FBoundPort;
      end; 
    end;
  end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSyslogClient.GetRemoteHost:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FRemoteHost;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TSyslogClient.SetRemoteHost(const ARemoteHost:String);
begin
 {}
 if ARemoteHost = FRemoteHost then Exit;

 if not AcquireLock then Exit;
 try
  {Check Protocol} 
  case FProtocol of
   LOGGING_PROTOCOL_UDP:begin
     FRemoteHost:=ARemoteHost;
     UniqueString(FRemoteHost);   

     {Disconnect UDP Client}
     if FUDPClient <> nil then
      begin
       FUDPClient.Disconnect;
       FUDPClient.RemoteHost:=FRemoteHost;
      end; 
    end;
   LOGGING_PROTOCOL_TCP:begin
     FRemoteHost:=ARemoteHost;
     UniqueString(FRemoteHost);   

     {Disconnect TCP Client}
     if FTCPClient <> nil then
      begin
       FTCPClient.Disconnect;
       FTCPClient.RemoteHost:=FRemoteHost;
      end; 
    end;
  end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TSyslogClient.SetRemotePort(ARemotePort:Word);
begin
 {}
 if ARemotePort = FRemotePort then Exit;

 if not AcquireLock then Exit;
 try
  {Check Protocol} 
  case FProtocol of
   LOGGING_PROTOCOL_UDP:begin
     FRemotePort:=ARemotePort;
     
     {Disconnect UDP Client}
     if FUDPClient <> nil then
      begin
       FUDPClient.Disconnect;
       FUDPClient.RemotePort:=FRemotePort;
      end; 
    end;
   LOGGING_PROTOCOL_TCP:begin
     FRemotePort:=ARemotePort;
     
     {Disconnect TCP Client}
     if FTCPClient <> nil then
      begin
       FTCPClient.Disconnect;
       FTCPClient.RemotePort:=FRemotePort;
      end; 
    end;
  end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TSyslogClient.SetOctetCounting(AOctetCounting:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 FOctetCounting:=AOctetCounting;
 
 ReleaseLock;
end;

{==============================================================================}

procedure TSyslogClient.SetBroadcastEnabled(ABroadcastEnabled:Boolean);
begin
 {}
 if ABroadcastEnabled = FBroadcastEnabled then Exit;

 if not AcquireLock then Exit;
 try
  {Check Protocol} 
  case FProtocol of
   LOGGING_PROTOCOL_UDP:begin
     FBroadcastEnabled:=ABroadcastEnabled;
     
     {Disconnect UDP Client}
     if FUDPClient <> nil then
      begin
       FUDPClient.Disconnect;
       FUDPClient.BroadcastEnabled:=FBroadcastEnabled;
      end; 
    end;
   LOGGING_PROTOCOL_TCP:begin
     FBroadcastEnabled:=ABroadcastEnabled;
     
     {Broadcast not applicable to TCP}
    end;
  end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSyslogClient.GetPriority(AFacility,ASeverity:LongWord):String;
var
 Facility:LongWord;
 Severity:LongWord;
begin
 {}
 Result:='';
 
 {Get Facility}
 Facility:=LoggingFacilityToSysLogFacility(AFacility);
 
 {Get Severity}
 Severity:=LoggingSeverityToSysLogSeverity(ASeverity);
 
 {Return Result}
 Result:='<' + IntToStr((Facility * 8) + Severity) + '>';
end;

{==============================================================================}

function TSyslogClient.GetMessage(const APriority,AAddress,ATag,AContent:String):String;
begin
 {}
 {Add Priority}
 Result:=APriority;
 
 {Add Date Time}
 Result:=Result + FileTimeToSysLogDateTime(GetCurrentTime) + ' ';
 
 {Add Local Address}
 Result:=Result + AAddress + ' ';
 
 {Add Tag}
 if Length(ATag) > 0 then Result:=Result + ATag + ': ';
 
 {Add Content}
 Result:=Result + AContent;
end;

{==============================================================================}

function TSyslogClient.SendMessage(AFacility,ASeverity:LongWord;const ATag,AContent:String):LongWord;
var
 WorkBuffer:String;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if not AcquireLock then Exit;
 try 
  {Check Port}
  if FRemotePort = 0 then Exit;
  
  {Check Host}
  if Length(FRemoteHost) = 0 then Exit;
  
  {Check Protocol}
  case FProtocol of
   LOGGING_PROTOCOL_UDP:begin
     {Check UDP Client}
     if FUDPClient = nil then
      begin
       {Create UDP Client}
       FUDPClient:=TWinsock2UDPClient.Create;
       FUDPClient.BoundPort:=FBoundPort;
       FUDPClient.RemoteHost:=FRemoteHost;
       FUDPClient.RemotePort:=FRemotePort;
       FUDPClient.BroadcastEnabled:=FBroadcastEnabled;
      end;

     Result:=ERROR_OPERATION_FAILED;
     
     {Connect UDP Client}
     if not FUDPClient.Connected then
      begin
       if not FUDPClient.Connect then Exit;
      end;
     
     {Get Message}
     WorkBuffer:=GetMessage(GetPriority(AFacility,ASeverity),FUDPClient.LocalAddress,ATag,AContent);
     
     {Send UDP Message}
     if FUDPClient.SendData(PChar(WorkBuffer),Length(WorkBuffer)) <> Length(WorkBuffer) then Exit;
     
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
   LOGGING_PROTOCOL_TCP:begin
     {Check TCP Client}
     if FTCPClient = nil then
      begin
       {Create TCP Client}
       FTCPClient:=TWinsock2TCPClient.Create;
       FTCPClient.BoundPort:=FBoundPort;
       FTCPClient.RemoteHost:=FRemoteHost;
       FTCPClient.RemotePort:=FRemotePort;
      end;
      
     Result:=ERROR_OPERATION_FAILED;
      
     {Connect TCP Client}
     if not FTCPClient.Connected then
      begin
       if not FTCPClient.Connect then Exit;
      end;
     
     {Get Message}
     WorkBuffer:=GetMessage(GetPriority(AFacility,ASeverity),FTCPClient.LocalAddress,ATag,AContent);
     
     if FOctetCounting then
      begin
       {Octet Counting (See: RFC6587)}
       {Add Message Length}
       WorkBuffer:=IntToStr(Length(WorkBuffer)) + ' ' + WorkBuffer;
       
       {Send TCP Message}
       if not FTCPClient.WriteData(PChar(WorkBuffer),Length(WorkBuffer)) then Exit;
      end
     else
      begin     
       {Non-Transparent-Framing (See: RFC6587)}
       {Add Trailer (LF)}
       WorkBuffer:=WorkBuffer + #10;
       
       {Send TCP Message}
       if not FTCPClient.WriteData(PChar(WorkBuffer),Length(WorkBuffer)) then Exit;
      end; 
     
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
  end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ServicesInit;
var
 Status:LongWord;
 WorkInt:LongWord;
 WorkBool:LongBool;
 WorkBuffer:String;
 WSAData:TWSAData;
 NTPClient:TNTPClient;
 Logging:PSysLogLogging;
begin
 {}
 {Check Initialized}
 if ServicesInitialized then Exit;
 
 {Initialize Logging}
 SERVICE_LOG_ENABLED:=(SERVICE_DEFAULT_LOG_LEVEL <> SERVICE_LOG_LEVEL_NONE); 
 
 {Check Environment Variables (NTP)}
 {NTP_SERVER_DEFAULT}
 WorkBuffer:=EnvironmentGet('NTP_SERVER_DEFAULT');
 if Length(WorkBuffer) <> 0 then NTP_SERVER_DEFAULT:=WorkBuffer;
 
 {NTP_PORT_DEFAULT}
 WorkInt:=StrToIntDef(EnvironmentGet('NTP_PORT_DEFAULT'),0);
 if WorkInt > 0 then NTP_PORT_DEFAULT:=WorkInt;
 
 {NTP_POLLING_INTERVAL}
 WorkInt:=StrToIntDef(EnvironmentGet('NTP_POLLING_INTERVAL'),0);
 if WorkInt > 0 then NTP_POLLING_INTERVAL:=WorkInt;
 
 {NTP_POLLING_TIMEOUT}
 WorkInt:=StrToIntDef(EnvironmentGet('NTP_POLLING_TIMEOUT'),0);
 if WorkInt > 0 then NTP_POLLING_TIMEOUT:=WorkInt;
 
 {NTP_POLLING_RETRIES}
 WorkInt:=StrToIntDef(EnvironmentGet('NTP_POLLING_RETRIES'),0);
 if WorkInt > 0 then NTP_POLLING_RETRIES:=WorkInt;
 
 {NTP_RETRY_TIMEOUT}
 WorkInt:=StrToIntDef(EnvironmentGet('NTP_RETRY_TIMEOUT'),0);
 if WorkInt > 0 then NTP_RETRY_TIMEOUT:=WorkInt;
 
 {NTP_AUTOSTART}
 WorkInt:=StrToIntDef(EnvironmentGet('NTP_AUTOSTART'),0);
 if WorkInt <> 0 then NTP_AUTOSTART:=True;
 
 {Check Environment Variables (SYSLOG)}
 {SYSLOG_BOUND_PORT}
 WorkInt:=StrToIntDef(EnvironmentGet('SYSLOG_BOUND_PORT'),0);
 if WorkInt > 0 then SYSLOG_BOUND_PORT:=WorkInt;
 
 {SYSLOG_SERVER_DEFAULT}
 WorkBuffer:=EnvironmentGet('SYSLOG_SERVER_DEFAULT');
 if Length(WorkBuffer) <> 0 then SYSLOG_SERVER_DEFAULT:=WorkBuffer;
 
 {SYSLOG_PORT_DEFAULT}
 WorkInt:=StrToIntDef(EnvironmentGet('SYSLOG_PORT_DEFAULT'),0);
 if WorkInt > 0 then SYSLOG_PORT_DEFAULT:=WorkInt;
 
 {SYSLOG_PROTOCOL_DEFAULT}
 WorkInt:=StrToIntDef(EnvironmentGet('SYSLOG_PROTOCOL_DEFAULT'),0);
 if WorkInt > 0 then SYSLOG_PROTOCOL_DEFAULT:=WorkInt;
 
 {SYSLOG_OCTET_COUNTING}
 WorkInt:=StrToIntDef(EnvironmentGet('SYSLOG_OCTET_COUNTING'),0);
 if WorkInt <> 0 then SYSLOG_OCTET_COUNTING:=True;
 
 {SYSLOG_BROADCAST_ENABLED}
 WorkBool:=StrToBoolDef(EnvironmentGet('SYSLOG_BROADCAST_ENABLED'),SYSLOG_BROADCAST_ENABLED);
 if WorkBool <> SYSLOG_BROADCAST_ENABLED then SYSLOG_BROADCAST_ENABLED:=WorkBool;
 
 {SYSLOG_REGISTER_LOGGING}
 WorkInt:=StrToIntDef(EnvironmentGet('SYSLOG_REGISTER_LOGGING'),0);
 if WorkInt <> 0 then SYSLOG_REGISTER_LOGGING:=True;
 
 {SYSLOG_LOGGING_DEFAULT}
 WorkInt:=StrToIntDef(EnvironmentGet('SYSLOG_LOGGING_DEFAULT'),0);
 if WorkInt <> 0 then SYSLOG_LOGGING_DEFAULT:=True;
 
 {SYSLOG_AUTOSTART}
 WorkInt:=StrToIntDef(EnvironmentGet('SYSLOG_AUTOSTART'),0);
 if WorkInt <> 0 then SYSLOG_AUTOSTART:=True;
 
 {Check NTP Auto Start}
 if NTP_AUTOSTART then
  begin
   {Start Winsock}
   FillChar(WSAData,SizeOf(TWSAData),0);
   if WSAStartup(WINSOCK_VERSION,WSAData) = ERROR_SUCCESS then
    begin
     {Create Client}
     NTPClient:=TNTPClient.Create;
     
     {Create Timer}
     NTPClient.TimerHandle:=TimerCreateEx(NTPClient.PollInterval * MILLISECONDS_PER_SECOND,TIMER_STATE_ENABLED,TIMER_FLAG_IMMEDIATE or TIMER_FLAG_WORKER,TTimerEvent(NTPUpdateTime),NTPClient); {Rescheduled by Timer Event}
    end; 
  end;
 
 {Check Syslog Auto Start}
 if SYSLOG_AUTOSTART then
  begin
   {Start Winsock}
   FillChar(WSAData,SizeOf(TWSAData),0);
   if WSAStartup(WINSOCK_VERSION,WSAData) = ERROR_SUCCESS then
    begin
     {Create Logging}
     if SYSLOG_REGISTER_LOGGING then
      begin
       Logging:=PSysLogLogging(LoggingDeviceCreateEx(SizeOf(TSysLogLogging),SYSLOG_LOGGING_DEFAULT));
       if Logging <> nil then
        begin
         {Update Logging}
         {Device}
         Logging.Logging.Device.DeviceBus:=DEVICE_BUS_NONE; 
         Logging.Logging.Device.DeviceType:=LOGGING_TYPE_SYSLOG;
         Logging.Logging.Device.DeviceFlags:=LOGGING_FLAG_NONE;
         Logging.Logging.Device.DeviceData:=nil;
         Logging.Logging.Device.DeviceDescription:=SYSLOG_LOGGING_DESCRIPTION;
         {Logging}
         Logging.Logging.LoggingState:=LOGGING_STATE_DISABLED;
         Logging.Logging.DeviceStart:=SysLogLoggingStart;
         Logging.Logging.DeviceStop:=SysLogLoggingStop;
         Logging.Logging.DeviceOutput:=SysLogLoggingOutput;
         Logging.Logging.DeviceOutputEx:=SysLogLoggingOutputEx;
         Logging.Logging.DeviceGetTarget:=SysLogLoggingGetTarget;
         Logging.Logging.DeviceSetTarget:=SysLogLoggingSetTarget;
         Logging.Logging.Target:=SYSLOG_SERVER_DEFAULT;
         {SysLog}
         Logging.Client:=nil;
         
         {Register Logging}
         Status:=LoggingDeviceRegister(@Logging.Logging);
         if Status = ERROR_SUCCESS then
          begin
           {Start Logging}
           Status:=LoggingDeviceStart(@Logging.Logging);
           if Status <> ERROR_SUCCESS then
            begin
             if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to start new syslog logging device: ' + ErrorToString(Status));

             {Deregister Logging}
             LoggingDeviceDeregister(@Logging.Logging);

             {Destroy Logging}
             LoggingDeviceDestroy(@Logging.Logging);
            end;
          end
         else 
          begin
           if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to register new syslog logging device: ' + ErrorToString(Status));

           {Destroy Logging}
           LoggingDeviceDestroy(@Logging.Logging);
          end;
        end
       else 
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to create new syslog logging device');
        end;
      end;
    end; 
  end; 
 
 ServicesInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Service Functions}

{==============================================================================}
{==============================================================================}
{Ping Functions}

{==============================================================================}
{==============================================================================}
{NTP Functions}
procedure NTPUpdateTime(Client:TNTPClient);

 function WholeSeconds(Time:Int64):Int64;
 {Truncate a time value in 100ns intervals to whole seconds only}
 begin
  {}
  Result:=(Time div TIME_TICKS_PER_SECOND) * TIME_TICKS_PER_SECOND;
 end;

 function MillisecondsDifference(Time1,Time2:Int64):Int64;
 {Determine how many milliseconds difference between two time values in 100ns intervals}
 var
  Milliseconds1:Int64;
  Milliseconds2:Int64;
 begin
  {}
  {Get Milliseconds}
  Milliseconds1:=(Time1 div TIME_TICKS_PER_MILLISECOND);
  Milliseconds2:=(Time2 div TIME_TICKS_PER_MILLISECOND);

  {Return Difference}
  if Milliseconds1 > Milliseconds2 then
   begin
    Result:=Milliseconds1 - Milliseconds2;
   end
  else
   begin
    Result:=Milliseconds2 - Milliseconds1;
   end;
 end;

var
 Current:Int64;
 Previous:Int64;
begin
 {}
 {Check Client}
 if Client = nil then Exit;
 
 {$IFDEF NTP_DEBUG}
 if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time');
 {$ENDIF}
 
 {Get Remote Time}
 Current:=Client.GetTime;
 if Current <> 0 then
  begin
   {$IFDEF NTP_DEBUG}
   if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time: Get Time success');
   if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time:  Remote time is ' + Client.FormatTime(Current));
   {$ENDIF}

   {Get Local Time}
   Previous:=ClockGetTime;

   {$IFDEF NTP_DEBUG}
   if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time:  Local time is ' + Client.FormatTime(Previous));
   {$ENDIF}

   {Compare Time}
   if MillisecondsDifference(Previous,Current) >= Client.ClockTolerance then
    begin
     {Set Time}
     ClockSetTime(Current,True);
     
     if SERVICE_LOG_ENABLED then ServiceLogInfo('NTP: Setting time to ' + Client.FormatTime(Current) + ' (from ' + Client.FormatTime(Previous) + ')');
    end; 
   
   {Set Initial Clock}
   Client.InitialClockGet:=True;
   
   {$IFDEF NTP_DEBUG}
   if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time: Scheduling Update in ' + IntToStr(Client.PollInterval) + ' seconds');
   {$ENDIF}
   
   {Enable Timer}
   TimerEnable(Client.TimerHandle);
  end
 else
  begin
   {$IFDEF NTP_DEBUG}
   if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time: Get Time failure');
   {$ENDIF}
   
   {Check Initial Clock}
   if Client.InitialClockGet then
    begin
     {$IFDEF NTP_DEBUG}
     if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time: Scheduling Update in ' + IntToStr(Client.PollInterval) + ' seconds');
     {$ENDIF}
    
     {Enable Timer}
     TimerEnable(Client.TimerHandle);
    end
   else 
    begin
     {$IFDEF NTP_DEBUG}
     if SERVICE_LOG_ENABLED then ServiceLogDebug('NTP Update Time: Scheduling Retry in ' + IntToStr(Client.RetryTimeout * Min(Client.InitialClockCount + 1,10)) + ' milliseconds');
     {$ENDIF}
     
     {Increment Clock Count}
     Client.IncrementInitialClockCount;
     
     {Check Retry}
     if Client.InitialClockRetry then
      begin
       {Schedule Worker}
       WorkerSchedule(Client.RetryTimeout * Min(Client.InitialClockCount,10),TWorkerTask(NTPUpdateTime),Client,nil);
      end; 
    end;
  end;  
end; 

{==============================================================================}
{==============================================================================}
{Telnet Functions}

{==============================================================================}
{==============================================================================}
{SSH Functions}

{==============================================================================}
{==============================================================================}
{UPnP Functions}

{==============================================================================}
{==============================================================================}
{SysLog Functions}
function SysLogLoggingStart(Logging:PLoggingDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

    {Create Client}
    PSysLogLogging(Logging).Client:=TSysLogClient.Create;
    if PSysLogLogging(Logging).Client = nil then Exit;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SysLogLoggingStop(Logging:PLoggingDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

    {Check Client}
    if PSysLogLogging(Logging).Client = nil then Exit; 
    
    {Destroy Client}
    PSysLogLogging(Logging).Client.Free;
    PSysLogLogging(Logging).Client:=nil;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SysLogLoggingOutput(Logging:PLoggingDevice;const Data:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

    {Check Client}
    if PSysLogLogging(Logging).Client = nil then Exit; 
    
    {Send Message}
    Result:=PSysLogLogging(Logging).Client.SendMessage(LOGGING_FACILITY_USER,LOGGING_SEVERITY_INFO,'',Data);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Update Statistics}
    Inc(Logging.OutputCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SysLogLoggingOutputEx(Logging:PLoggingDevice;Facility,Severity:LongWord;const Tag,Content:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

    {Check Client}
    if PSysLogLogging(Logging).Client = nil then Exit; 
    
    {Send Message}
    Result:=PSysLogLogging(Logging).Client.SendMessage(Facility,Severity,Tag,Content);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Update Statistics}
    Inc(Logging.OutputCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SysLogLoggingGetTarget(Logging:PLoggingDevice):String;
begin
 {}
 Result:='';
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

    {Check Client}
    if PSysLogLogging(Logging).Client = nil then Exit; 
    
    {Return Result}
    Result:=PSysLogLogging(Logging).Client.RemoteHost;
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end;
end;

{==============================================================================}

function SysLogLoggingSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

    {Check Client}
    if PSysLogLogging(Logging).Client = nil then Exit; 
    
    {Set Target}
    PSysLogLogging(Logging).Client.RemoteHost:=Target;
    Logging.Target:=Target;
    UniqueString(Logging.Target);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{Service Helper Functions}
procedure ServiceLog(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SERVICE_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = SERVICE_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SERVICE_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = SERVICE_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Services: ';
 
 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_SERVICES,LogLevelToLoggingSeverity(Level),'Services',WorkBuffer + AText);
end;

{==============================================================================}

procedure ServiceLogInfo(const AText:String); inline;
begin
 {}
 ServiceLog(SERVICE_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure ServiceLogWarn(const AText:String); inline;
begin
 {}
 ServiceLog(SERVICE_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure ServiceLogError(const AText:String); inline;
begin
 {}
 ServiceLog(SERVICE_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure ServiceLogDebug(const AText:String); inline;
begin
 {}
 ServiceLog(SERVICE_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}
{Ping Helper Functions}

{==============================================================================}
{==============================================================================}
{NTP Helper Functions}
function NTPTimestampToHost(const Timestamp:TNTPTimestamp):TNTPTimestamp;
{Convert an NTP Timestamp to Host order}
begin
 {}
 Result.Seconds:=BEToN(Timestamp.Seconds);
 Result.Fraction:=BEToN(Timestamp.Fraction);
end;

{==============================================================================}

function NTPTimestampToNetwork(const Timestamp:TNTPTimestamp):TNTPTimestamp;
{Convert an NTP Timestamp to Network order}
begin
 {}
 Result.Seconds:=NToBE(Timestamp.Seconds);
 Result.Fraction:=NToBE(Timestamp.Fraction);
end;

{==============================================================================}

function NTPTimestampAdd(const Timestamp1,Timestamp2:TNTPTimestamp):TNTPTimestamp;
{Note: Expects Timestamp to be in Network order}
{Note: Returns Timestamp in Network order}
begin
 {}
 Result.Seconds:=BEToN(Timestamp1.Seconds) + BEToN(Timestamp2.Seconds);
 Result.Fraction:=BEToN(Timestamp1.Fraction) + BEToN(Timestamp2.Fraction);
 
 {Swap Endian}
 Result.Seconds:=NToBE(Result.Seconds);
 Result.Fraction:=NToBE(Result.Fraction);
end;

{==============================================================================}

function NTPTimestampSubtract(const Timestamp1,Timestamp2:TNTPTimestamp):TNTPTimestamp;
{Note: Expects Timestamp to be in Network order}
{Note: Returns Timestamp in Network order}
begin
 {}
 Result.Seconds:=BEToN(Timestamp1.Seconds) - BEToN(Timestamp2.Seconds);
 Result.Fraction:=BEToN(Timestamp1.Fraction) - BEToN(Timestamp2.Fraction);
 
 {Swap Endian}
 Result.Seconds:=NToBE(Result.Seconds);
 Result.Fraction:=NToBE(Result.Fraction);
end;

{==============================================================================}

function NTPTimestampDivide(const Timestamp:TNTPTimestamp;Divisor:LongWord):TNTPTimestamp;
{Note: Expects Timestamp to be in Network order}
{Note: Returns Timestamp in Network order}
begin
 {}
 Result.Seconds:=0;
 Result.Fraction:=0;
 
 {Check Divisor}
 if Divisor = 0 then Exit;
 
 Result.Seconds:=BEToN(Timestamp.Seconds) div Divisor;
 Result.Fraction:=BEToN(Timestamp.Fraction) div Divisor;
 
 {Swap Endian}
 Result.Seconds:=NToBE(Result.Seconds);
 Result.Fraction:=NToBE(Result.Fraction);
end;
 
{==============================================================================}

function ClockTimeToNTPTimestamp(const Time:Int64):TNTPTimestamp;
{Note: Returns Timestamp in Network order}
var
 Microseconds:Int64;
begin
 {}
 Result.Seconds:=0;
 Result.Fraction:=0;
 
 {Check Time}
 if Time < NTP_TIMESTAMP_START then Exit;
 
 {Calculate Timestamp Seconds}
 Result.Seconds:=(Time - NTP_TIMESTAMP_START) div TIME_TICKS_PER_SECOND;
 
 {Calculate Timestamp Fraction}
 Microseconds:=((Time - NTP_TIMESTAMP_START) mod TIME_TICKS_PER_SECOND) div TIME_TICKS_PER_MICROSECOND;
 Result.Fraction:=(Microseconds shl 32) div MICROSECONDS_PER_SECOND; {Fraction is units of 1/2^32 of a second}
 
 {Change to Network order}
 Result.Seconds:=NToBE(Result.Seconds);
 Result.Fraction:=NToBE(Result.Fraction);
end;

{==============================================================================}

function NTPTimestampToClockTime(const Timestamp:TNTPTimestamp):Int64;
{Note: Expects Timestamp to be in Network order}
var
 Seconds:Int64;
 Microseconds:Int64;
begin
 {}
 {Get Seconds}
 Seconds:=BEToN(Timestamp.Seconds); {Avoid 32 bit overflow}
 
 {Get Microseconds}
 Microseconds:=BEToN(Timestamp.Fraction); {Avoid 32 bit overflow}
 Microseconds:=(Microseconds * MICROSECONDS_PER_SECOND) shr 32; {Fraction is units of 1/2^32 of a second}
 
 {Calculate Time}
 Result:=(Seconds * TIME_TICKS_PER_SECOND) + (Microseconds * TIME_TICKS_PER_MICROSECOND) + NTP_TIMESTAMP_START;
end;

{==============================================================================}

function NTPTimestampToString(const Timestamp:TNTPTimestamp):String;
{Note: Expects Timestamp to be in Network order}
var
 Seconds:Int64;
 Microseconds:Int64;
begin
 {}
 {Get Seconds}
 Seconds:=BEToN(Timestamp.Seconds); {Avoid 32 bit overflow}
 
 {Get Microseconds}
 Microseconds:=BEToN(Timestamp.Fraction); {Avoid 32 bit overflow}
 Microseconds:=(Microseconds * MICROSECONDS_PER_SECOND) shr 32; {Fraction is units of 1/2^32 of a second}
 
 Result:=IntToHex(BEToN(Timestamp.Seconds),8) + ' / ' + IntToHex(BEToN(Timestamp.Fraction),8) + ' (Seconds = ' + IntToStr(Seconds) + ' / Microseconds = ' + IntToStr(Microseconds) + ')';
end;

{==============================================================================}
{==============================================================================}
{Telnet Helper Functions}
function TelnetCommandToString(Command:Byte):String;
begin
 {}
 case Command of
  TELNET_COMMAND_EOR:Result:='TELNET_COMMAND_EOR';
  TELNET_COMMAND_SE:Result:='TELNET_COMMAND_SE';
  TELNET_COMMAND_NOP:Result:='TELNET_COMMAND_NOP';
  TELNET_COMMAND_DM:Result:='TELNET_COMMAND_DM';
  TELNET_COMMAND_BRK:Result:='TELNET_COMMAND_BRK'; 
  TELNET_COMMAND_IP:Result:='TELNET_COMMAND_IP'; 
  TELNET_COMMAND_AO:Result:='TELNET_COMMAND_AO'; 
  TELNET_COMMAND_AYT:Result:='TELNET_COMMAND_AYT'; 
  TELNET_COMMAND_EC:Result:='TELNET_COMMAND_EC';
  TELNET_COMMAND_EL:Result:='TELNET_COMMAND_EL'; 
  TELNET_COMMAND_GA:Result:='TELNET_COMMAND_GA'; 
  TELNET_COMMAND_SB:Result:='TELNET_COMMAND_SB';
  TELNET_COMMAND_WILL:Result:='TELNET_COMMAND_WILL';
  TELNET_COMMAND_WONT:Result:='TELNET_COMMAND_WONT';
  TELNET_COMMAND_DO:Result:='TELNET_COMMAND_DO'; 
  TELNET_COMMAND_DONT:Result:='TELNET_COMMAND_DONT';
  TELNET_COMMAND_IAC:Result:='TELNET_COMMAND_IAC';
  else
   begin
    Result:=IntToHex(Command,2);
   end;
 end;
end;

{==============================================================================}

function TelnetCommandHasOption(Command:Byte):Boolean;
begin
 {}
 Result:=False;
 
 case Command of
  TELNET_COMMAND_WILL:Result:=True;
  TELNET_COMMAND_WONT:Result:=True;
  TELNET_COMMAND_DO:Result:=True; 
  TELNET_COMMAND_DONT:Result:=True;
 end;
end;

{==============================================================================}

function TelnetOptionToString(Option:Byte):String;
begin
 {}
 case Option of
  TELNET_OPTION_TRANSMIT_BINARY:Result:='TELNET_OPTION_TRANSMIT_BINARY';
  TELNET_OPTION_ECHO:Result:='TELNET_OPTION_ECHO'; 
  TELNET_OPTION_SUPPRESS_GA:Result:='TELNET_OPTION_SUPPRESS_GA';
  TELNET_OPTION_TERMINAL_TYPE:Result:='TELNET_OPTION_TERMINAL_TYPE';
  TELNET_OPTION_WINDOW_SIZE:Result:='TELNET_OPTION_WINDOW_SIZE';
  TELNET_OPTION_TERMINAL_SPEED:Result:='TELNET_OPTION_TERMINAL_SPEED';
  TELNET_OPTION_NEW_ENVIRONMENT:Result:='TELNET_OPTION_NEW_ENVIRONMENT';
  else
   begin
    Result:=IntToHex(Option,2);
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{SSH Helper Functions}

{==============================================================================}
{==============================================================================}
{UPnP Helper Functions}

{==============================================================================}
{==============================================================================}
{SysLog Helper Functions}
function FileTimeToSysLogDateTime(const AFileTime:TFileTime):String;
var
 DateTime:TDateTime;
begin
 {}
 DateTime:=FileTimeToDateTime(AFileTime); {Converted to Local}
 
 Result:=FormatDateTime('mmm dd hh:nn:ss',DateTime);
end;

{==============================================================================}

function LoggingFacilityToSysLogFacility(Facility:LongWord):LongWord;
begin
 {}
 Result:=SYSLOG_FACILITY_USER;
 
 case Facility of
  LOGGING_FACILITY_KERNEL,
  LOGGING_FACILITY_PLATFORM,
  LOGGING_FACILITY_THREADS:Result:=SYSLOG_FACILITY_KERNEL;
  LOGGING_FACILITY_DEVICES,
  LOGGING_FACILITY_NETWORK,
  LOGGING_FACILITY_STORAGE,
  LOGGING_FACILITY_FILESYSTEM,
  LOGGING_FACILITY_KEYBOARD,
  LOGGING_FACILITY_MOUSE,
  LOGGING_FACILITY_SCSI,
  LOGGING_FACILITY_DMA,
  LOGGING_FACILITY_GPIO,
  LOGGING_FACILITY_MMC,
  LOGGING_FACILITY_USB,
  LOGGING_FACILITY_SERVICES,
  LOGGING_FACILITY_HTTP:Result:=SYSLOG_FACILITY_SYSTEM;
  LOGGING_FACILITY_IMAP,
  LOGGING_FACILITY_POP,
  LOGGING_FACILITY_SMTP:Result:=SYSLOG_FACILITY_MAIL; 
  LOGGING_FACILITY_TELNET,
  LOGGING_FACILITY_SSH,
  LOGGING_FACILITY_SHELL:Result:=SYSLOG_FACILITY_SYSTEM;
  LOGGING_FACILITY_NTP:Result:=SYSLOG_FACILITY_NTP;
  LOGGING_FACILITY_FTP:Result:=SYSLOG_FACILITY_FTP;
  LOGGING_FACILITY_RTC,
  LOGGING_FACILITY_I2C,
  LOGGING_FACILITY_I2S,
  LOGGING_FACILITY_PWM,
  LOGGING_FACILITY_SERIAL,
  LOGGING_FACILITY_SPI,
  LOGGING_FACILITY_UART,
  LOGGING_FACILITY_AUDIO,
  LOGGING_FACILITY_1WIRE,
  LOGGING_FACILITY_TOUCH,
  LOGGING_FACILITY_VIDEO,
  LOGGING_FACILITY_CODEC,
  LOGGING_FACILITY_NFS,
  LOGGING_FACILITY_RPC,
  LOGGING_FACILITY_PCI,
  LOGGING_FACILITY_VIRTIO,
  LOGGING_FACILITY_BLUETOOTH,
  LOGGING_FACILITY_JOYSTICK,
  LOGGING_FACILITY_HID:Result:=SYSLOG_FACILITY_SYSTEM;
  
  LOGGING_FACILITY_USER:Result:=SYSLOG_FACILITY_USER; 
 end;
end;

{==============================================================================}

function LoggingSeverityToSysLogSeverity(Severity:LongWord):LongWord;
begin
 {}
 Result:=SYSLOG_SEVERITY_INFORMATION;
 
 case Severity of
  LOGGING_SEVERITY_ERROR:Result:=SYSLOG_SEVERITY_ERROR;
  LOGGING_SEVERITY_WARN:Result:=SYSLOG_SEVERITY_WARNING;
  LOGGING_SEVERITY_INFO:Result:=SYSLOG_SEVERITY_INFORMATION;
  LOGGING_SEVERITY_DEBUG:Result:=SYSLOG_SEVERITY_DEBUG;
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 ServicesInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
