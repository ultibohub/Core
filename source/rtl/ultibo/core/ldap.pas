{
Ultibo LDAP interface unit.

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


LDAP
====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit LDAP;

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

//To Do //A generic LDAP interface (Client and Server)


{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {LDAP specific constants}

 //To Do //See: POP3 for framework

 {LDAP logging}
 LDAP_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {LDAP debugging messages}
 LDAP_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {LDAP informational messages}
 LDAP_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {LDAP warning messages}
 LDAP_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {LDAP error messages}
 LDAP_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No LDAP messages}

var
 LDAP_DEFAULT_LOG_LEVEL:LongWord = LDAP_LOG_LEVEL_DEBUG; {Minimum level for LDAP messages.  Only messages with level greater than or equal to this will be printed}

var
 {LDAP logging}
 LDAP_LOG_ENABLED:Boolean;

{==============================================================================}
//type
 {LDAP specific types}
//To Do

{==============================================================================}
//type
 {LDAP specific classes}

 {Helper classes}

 {Client classes}

 {Server classes}

{==============================================================================}
{var}
 {LDAP specific variables}

{==============================================================================}
{Initialization Functions}
procedure LDAPInit;

{==============================================================================}
{LDAP Functions}

{==============================================================================}
{LDAP Helper Functions}
procedure LDAPLog(Level:LongWord;const AText:String);
procedure LDAPLogInfo(const AText:String); inline;
procedure LDAPLogWarn(const AText:String); inline;
procedure LDAPLogError(const AText:String); inline;
procedure LDAPLogDebug(const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {LDAP specific variables}
 LDAPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TLDAPBuffer}

{==============================================================================}
{==============================================================================}
{TLDAPClient}

{==============================================================================}
{==============================================================================}
{TLDAPConnection}

{==============================================================================}
{==============================================================================}
{TLDAPListener}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure LDAPInit;
begin
 {}
 {Check Initialized}
 if LDAPInitialized then Exit;

 {Initialize Logging}
 LDAP_LOG_ENABLED:=(LDAP_DEFAULT_LOG_LEVEL <> LDAP_LOG_LEVEL_NONE);

 LDAPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{LDAP Functions}

{==============================================================================}
{==============================================================================}
{LDAP Helper Functions}
procedure LDAPLog(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < LDAP_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = LDAP_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = LDAP_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = LDAP_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'LDAP: ';

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_IMAP,LogLevelToLoggingSeverity(Level),'LDAP',WorkBuffer + AText);
end;

{==============================================================================}

procedure LDAPLogInfo(const AText:String); inline;
begin
 {}
 LDAPLog(LDAP_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure LDAPLogWarn(const AText:String); inline;
begin
 {}
 LDAPLog(LDAP_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure LDAPLogError(const AText:String); inline;
begin
 {}
 LDAPLog(LDAP_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure LDAPLogDebug(const AText:String); inline;
begin
 {}
 LDAPLog(LDAP_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 LDAPInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
