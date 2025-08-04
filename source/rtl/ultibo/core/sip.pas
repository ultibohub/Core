{
Ultibo SIP interface unit.

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


SIP
===

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SIP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,UltiboClasses,Winsock2;

//To Do //A generic SIP interface (Client, Server, Call, Session, Router and Proxy)

        //This unit will include Winsock2 (Like HTTP etc) as well as Crypto for SSL/TLS etc
        //It will also include the Device, Audio and Video units for A/V device interface and streams etc

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {SIP specific constants}

 //To Do //See: POP3 for framework

 {SIP logging}
 SIP_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {SIP debugging messages}
 SIP_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {SIP informational messages}
 SIP_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {SIP warning messages}
 SIP_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {SIP error messages}
 SIP_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No SIP messages}

var
 SIP_DEFAULT_LOG_LEVEL:LongWord = SIP_LOG_LEVEL_DEBUG; {Minimum level for SIP messages.  Only messages with level greater than or equal to this will be printed}

var
 {SIP logging}
 SIP_LOG_ENABLED:Boolean;


{==============================================================================}
//type
 {SIP specific types}
//To Do

{==============================================================================}
//type
 {SIP specific classes}

 {Helper classes}

 {Client classes}

 {Server classes}

{==============================================================================}
{var}
 {SIP specific variables}

{==============================================================================}
{Initialization Functions}
procedure SIPInit;

{==============================================================================}
{SIP Functions}

{==============================================================================}
{SIP Helper Functions}
procedure SIPLog(Level:LongWord;const AText:String);
procedure SIPLogInfo(const AText:String); inline;
procedure SIPLogWarn(const AText:String); inline;
procedure SIPLogError(const AText:String); inline;
procedure SIPLogDebug(const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {SIP specific variables}
 SIPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SIPInit;
begin
 {}
 {Check Initialized}
 if SIPInitialized then Exit;

 {Initialize Logging}
 SIP_LOG_ENABLED:=(SIP_DEFAULT_LOG_LEVEL <> SIP_LOG_LEVEL_NONE);

 SIPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{SIP Functions}

{==============================================================================}
{==============================================================================}
{SIP Helper Functions}
procedure SIPLog(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SIP_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = SIP_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SIP_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = SIP_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'SIP: ';

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_IMAP,LogLevelToLoggingSeverity(Level),'SIP',WorkBuffer + AText);
end;

{==============================================================================}

procedure SIPLogInfo(const AText:String); inline;
begin
 {}
 SIPLog(SIP_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure SIPLogWarn(const AText:String); inline;
begin
 {}
 SIPLog(SIP_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure SIPLogError(const AText:String); inline;
begin
 {}
 SIPLog(SIP_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure SIPLogDebug(const AText:String); inline;
begin
 {}
 SIPLog(SIP_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 SIPInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
