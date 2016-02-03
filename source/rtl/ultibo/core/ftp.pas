{
Ultibo FTP interface unit.

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

 
FTP
===

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit FTP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,UltiboClasses,Winsock2;

//To Do //A generic FTP interface (Client and Server)

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {FTP specific constants}
 
 //To Do //See: POP3 etc for framework
        
 {FTP logging}
 FTP_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {FTP debugging messages}
 FTP_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {FTP informational messages,}
 FTP_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {FTP error messages}
 FTP_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No FTP messages}

var 
 FTP_DEFAULT_LOG_LEVEL:LongWord = FTP_LOG_LEVEL_DEBUG; //FTP_LOG_LEVEL_INFO; {Minimum level for FTP messages.  Only messages with level greater than or equal to this will be printed} 
 
var 
 {FTP logging}
 FTP_LOG_ENABLED:Boolean; 
        
              
{==============================================================================}
//type
 {FTP specific types}
//To Do

{==============================================================================}
//type
 {FTP specific classes}

 {Helper classes}

 {Client classes}
 
 {Server classes}
 
{==============================================================================}
{var}
 {FTP specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure FTPInit;

{==============================================================================}
{FTP Functions}

{==============================================================================}
{FTP Helper Functions}
procedure FTPLog(Level:LongWord;const AText:String);
procedure FTPLogInfo(const AText:String);
procedure FTPLogError(const AText:String);
procedure FTPLogDebug(const AText:String);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {FTP specific variables}
 FTPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TFTPBuffer}

{==============================================================================}
{==============================================================================}
{TFTPClient}

{==============================================================================}
{==============================================================================}
{TFTPConnection}

{==============================================================================}
{==============================================================================}
{TFTPListener}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure FTPInit;
begin
 {}
 {Check Initialized}
 if FTPInitialized then Exit;
 
 {Initialize Logging}
 FTP_LOG_ENABLED:=(FTP_DEFAULT_LOG_LEVEL <> FTP_LOG_LEVEL_NONE); 
 
 FTPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{FTP Functions}

{==============================================================================}
{==============================================================================}
{FTP Helper Functions}
procedure FTPLog(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < FTP_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = FTP_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = FTP_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'FTP: ';

 {Output Logging} 
 LoggingOutputEx(LOGGING_FACILITY_IMAP,LogLevelToLoggingSeverity(Level),'FTP',WorkBuffer + AText);
end;

{==============================================================================}

procedure FTPLogInfo(const AText:String);
begin
 {}
 FTPLog(FTP_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure FTPLogError(const AText:String);
begin
 {}
 FTPLog(FTP_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure FTPLogDebug(const AText:String);
begin
 {}
 FTPLog(FTP_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 FTPInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
