{
Ultibo IMAP4 interface unit.

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


IMAP4
=====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit IMAP4;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  UltiboClasses,
  Winsock2;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {IMAP4 specific constants}

 //To Do //See: POP3 etc for framework

 {IMAP4 logging}
 IMAP4_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {IMAP4 debugging messages}
 IMAP4_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {IMAP4 informational messages}
 IMAP4_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {IMAP4 warning messages}
 IMAP4_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {IMAP4 error messages}
 IMAP4_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No IMAP4 messages}

var
 IMAP4_DEFAULT_LOG_LEVEL:LongWord = IMAP4_LOG_LEVEL_DEBUG; {Minimum level for IMAP4 messages.  Only messages with level greater than or equal to this will be printed}

var
 {IMAP4 logging}
 IMAP4_LOG_ENABLED:Boolean;


{==============================================================================}
//type
 {IMAP4 specific types}
//To Do

{==============================================================================}
//type
 {IMAP4 specific classes}

 {Helper classes}

 {Client classes}

 {Server classes}

{==============================================================================}
{var}
 {IMAP4 specific variables}

{==============================================================================}
{Initialization Functions}
procedure IMAP4Init;

{==============================================================================}
{IMAP4 Functions}

{==============================================================================}
{IMAP4 Helper Functions}
procedure IMAP4Log(Level:LongWord;const AText:String);
procedure IMAP4LogInfo(const AText:String); inline;
procedure IMAP4LogWarn(const AText:String); inline;
procedure IMAP4LogError(const AText:String); inline;
procedure IMAP4LogDebug(const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {IMAP4 specific variables}
 IMAP4Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{TIMAP4Buffer}

{==============================================================================}
{==============================================================================}
{TIMAP4Client}

{==============================================================================}
{==============================================================================}
{TIMAP4Connection}

{==============================================================================}
{==============================================================================}
{TIMAP4Listener}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure IMAP4Init;
begin
 {}
 {Check Initialized}
 if IMAP4Initialized then Exit;

 {Initialize Logging}
 IMAP4_LOG_ENABLED:=(IMAP4_DEFAULT_LOG_LEVEL <> IMAP4_LOG_LEVEL_NONE);

 IMAP4Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{IMAP4 Functions}

{==============================================================================}
{==============================================================================}
{IMAP4 Helper Functions}
procedure IMAP4Log(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < IMAP4_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = IMAP4_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = IMAP4_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = IMAP4_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'IMAP4: ';

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_IMAP,LogLevelToLoggingSeverity(Level),'IMAP4',WorkBuffer + AText);
end;

{==============================================================================}

procedure IMAP4LogInfo(const AText:String); inline;
begin
 {}
 IMAP4Log(IMAP4_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure IMAP4LogWarn(const AText:String); inline;
begin
 {}
 IMAP4Log(IMAP4_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure IMAP4LogError(const AText:String); inline;
begin
 {}
 IMAP4Log(IMAP4_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure IMAP4LogDebug(const AText:String); inline;
begin
 {}
 IMAP4Log(IMAP4_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 IMAP4Init;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

