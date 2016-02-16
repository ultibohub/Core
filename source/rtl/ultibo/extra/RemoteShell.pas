{
Ultibo Remote Shell unit.

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

 

Remote Shell
============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RemoteShell;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,UltiboClasses,Winsock2,Services,Shell;

//To Do //Look for:

//--

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Remote Shell specific constants}
 REMOTE_SHELL_DEFAULT_WELCOME = ' (Type HELP for a list of available commands)';
 
 {Telnet Shell constants}
 TELNET_SHELL_NAME = 'Telnet Shell';

 {Telnet Shell Command constants}
 TELNET_SHELL_COMMAND_LOGOUT = 'LOGOUT';

 {Telnet Shell Alias constants}
 TELNET_SHELL_ALIAS_EXIT = 'EXIT';
 
 {SSH Shell constants}
 SSH_SHELL_NAME = 'SSH Shell';
 
 {SSH Shell Command constants}
 SSH_SHELL_COMMAND_LOGOUT = 'LOGOUT';
 
 {SSH Shell Alias constants}
 SSH_SHELL_ALIAS_EXIT = 'EXIT';
 
{==============================================================================}
{type}
 {Remote Shell specific types}

{==============================================================================}
type
 {Remote Shell specific clases}
 TTelnetShell = class(TShell)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FListener:TTelnetListener;
  
  {Internal Methods}
  
 protected
  {Internal Variables}

  {Internal Methods}
  procedure TelnetConnected(AConnection:TTelnetConnection);
  procedure TelnetDisconnected(AConnection:TTelnetConnection);
  function TelnetInit(AConnection:TTelnetConnection):Boolean;
  function TelnetChar(AConnection:TTelnetConnection;AChar:Char):Boolean;
  function TelnetCommand(AConnection:TTelnetConnection;ACommand,AOption:Byte;AData:Pointer;ASize:LongWord):Boolean;
 public
  {Public Properties}
  property Listener:TTelnetListener read FListener;
  
  {Public Methods}
  function DoClear(ASession:TShellSession):Boolean; override;
  
  function DoInput(ASession:TShellSession;var AInput:String):Boolean; override;
  
  function DoOutputEx(ASession:TShellSession;const AOutput:String;AReturn:Boolean):Boolean; override;
  
  function MatchSequence(const ASequence:String;var AContinue:Boolean):Boolean;
  function ProcessSequence(const ASequence:String):Boolean;
 end;
  
 TTelnetSession = class(TShellSession)
 private
  {Internal Variables}

 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}
  Command:String;
  Sequence:String;
  Position:LongWord;
  
  {Public Methods}
  
 end;
  
 TSSHShell = class(TShell)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  //FListener:TSSHListener; //To Do
  
  {Internal Methods}
  
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  //To Do
 end;
  
 TTelnetShellLogout = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TSSHShellLogout = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  //To Do
 end;
 
{==============================================================================}
{var}
 {Remote Shell specific variables}

{==============================================================================}
{Initialization Functions}
procedure RemoteShellInit;

{==============================================================================}
{Remote Shell Functions}

{==============================================================================}
{Remote Shell Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Remote Shell specific variables}
 RemoteShellInitialized:Boolean;
 
{==============================================================================}
{==============================================================================}
{TTelnetShell}
constructor TTelnetShell.Create;
begin
 {}
 inherited Create;
 Name:=TELNET_SHELL_NAME;
 Flags:=SHELL_FLAG_CLEAR;
 
 FListener:=TTelnetListener.Create;
 FListener.OnConnected:=TelnetConnected;
 FListener.OnDisconnected:=TelnetDisconnected;
 FListener.OnInit:=TelnetInit;
 FListener.OnChar:=TelnetChar;
 FListener.OnCommand:=TelnetCommand;
 FListener.Active:=True;
end;

{==============================================================================}

destructor TTelnetShell.Destroy; 
begin
 {}
 AcquireLock;
 try
  FListener.Active:=False;
  FListener.Threads.TerminateAll;
 finally
  ReleaseLock;
  inherited Destroy;
 end;
end;

{==============================================================================}

procedure TTelnetShell.TelnetConnected(AConnection:TTelnetConnection);
var
 Session:TShellSession;
begin
 {}
 {Check Connection}
 if AConnection = nil then Exit;
 
 {Create Session}
 Session:=TTelnetSession.Create(Self,AConnection.Handle);
 Session.Data:=AConnection;

 {Register Session}
 if not RegisterSession(Session) then
  begin
   Session.Free;
   Exit;
  end;
 
 {Update Connection}
 AConnection.Data:=Session;
end;

{==============================================================================}

procedure TTelnetShell.TelnetDisconnected(AConnection:TTelnetConnection);
var
 Session:TShellSession;
begin
 {}
 {Check Connection}
 if AConnection = nil then Exit;
 
 {Get Session}
 Session:=TShellSession(AConnection.Data);
 if Session = nil then Exit;
 
 {Deregister Session}
 if not DeregisterSession(Session) then Exit;
 
 {Destroy Session}
 Session.Free;
 
 {Update Connection}
 AConnection.Data:=nil;
end;

{==============================================================================}

function TTelnetShell.TelnetInit(AConnection:TTelnetConnection):Boolean;
var
 Session:TTelnetSession;
begin
 {}
 Result:=False;
 
 {Check Connection}
 if AConnection = nil then Exit;
 
 {Get Session}
 Session:=TTelnetSession(AConnection.Data);
 if Session = nil then Exit;
 
 {Check Authenticator}
 if Authenticator <> nil then
  begin
   {Perform Authentication}
   //To Do
  end;
 
 {Clear Screen}
 if not DoClear(Session) then Exit;
 
 {Send Banner}
 if not DoBanner(Session) then Exit;
 
 {Send Welcome}
 if not DoOutput(Session,REMOTE_SHELL_DEFAULT_WELCOME) then Exit;
 
 {Send Prompt}
 if not DoPrompt(Session) then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TTelnetShell.TelnetChar(AConnection:TTelnetConnection;AChar:Char):Boolean;
var
 Session:TTelnetSession;
begin
 {}
 Result:=False;
 
 {Check Connection}
 if AConnection = nil then Exit;
 
 {Get Session}
 Session:=TTelnetSession(AConnection.Data);
 if Session = nil then Exit;
 
 {Check for ESC}
 if AChar = TELNET_CHAR_ESC then
  begin
   {Check Sequence}
   if Length(Session.Sequence) > 0 then
    begin
     {Add Command}
     Session.Command:=Session.Command + Session.Sequence;
     Inc(Session.Position,Length(Session.Sequence));
    end;
    
   {Start Sequence}
   Session.Sequence:=AChar;
  end
 else
  begin
   {Check Sequence}
   if Length(Session.Sequence) > 0 then
    begin
     {Check for CR}
     if AChar = TELNET_CHAR_CR then
      begin
       {Add Command}
       Session.Command:=Session.Command + Session.Sequence;
       Inc(Session.Position,Length(Session.Sequence));
       
       {Clear Sequence}
       SetLength(Session.Sequence,0);
      end
     {Check for LF}
     else if AChar = TELNET_CHAR_LF then
      begin
       {Add Command}
       Session.Command:=Session.Command + Session.Sequence;
       Inc(Session.Position,Length(Session.Sequence));
       
       {Clear Sequence}
       SetLength(Session.Sequence,0);
      
       {Check Command}
       if Length(Session.Command) > 0 then
        begin
         {Process Command}
         ProcessCommand(Session,Session.Command);
        
         {Clear Command}
         SetLength(Session.Command,0);
         Session.Position:=0;
        end;
        
       {Send Prompt} 
       DoPrompt(Session);
      end
     else 
      begin
       {Add Sequence}
       Session.Sequence:=Session.Sequence + AChar;
     
       {Match Sequence}
       //To Do 
      end; 
    end
   else
    begin
     {Check for CR}
     if AChar = TELNET_CHAR_CR then
      begin
       {Nothing}
      end
     {Check for LF}
     else if AChar = TELNET_CHAR_LF then
      begin
       {Check Command}
       if Length(Session.Command) > 0 then
        begin
         {Process Command}
         ProcessCommand(Session,Session.Command);
         
         {Clear Command}
         SetLength(Session.Command,0);
         Session.Position:=0;
        end;
        
       {Send Prompt} 
       DoPrompt(Session);
      end
     {Check for Tab}
     else if AChar = TELNET_CHAR_TAB then
      begin
       {Match Command}
       //To Do 
      end
     {Check for Backspace}
     else if AChar = TELNET_CHAR_BACKSPACE then
      begin
       if Length(Session.Command) > 0 then
        begin
         {Remove Character}
         SetLength(Session.Command,Length(Session.Command) - 1);
         Dec(Session.Position);
        end; 
      end
     else
      begin     
       {Add Command}
       Session.Command:=Session.Command + AChar;
       Inc(Session.Position);
      end; 
    end;    
  end;  
end;

{==============================================================================}

function TTelnetShell.TelnetCommand(AConnection:TTelnetConnection;ACommand,AOption:Byte;AData:Pointer;ASize:LongWord):Boolean;
var
 Session:TTelnetSession;
begin
 {}
 Result:=False;
 
 {Check Connection}
 if AConnection = nil then Exit;
 
 {Get Session}
 Session:=TTelnetSession(AConnection.Data);
 if Session = nil then Exit;
 
 {Check Command}
 case ACommand of
  TELNET_COMMAND_EC:begin
    {Erase Character}
    if Length(Session.Command) > 0 then
     begin
      SetLength(Session.Command,Length(Session.Command) - 1);
      Dec(Session.Position);
     end;
     
    {Return Result}
    Result:=True;
   end;
  TELNET_COMMAND_EL:begin 
    {Erase Line}
    SetLength(Session.Command,0);
    Session.Position:=0;
    
    {Return Result}
    Result:=True;
   end;
 end; 
end;

{==============================================================================}

function TTelnetShell.DoClear(ASession:TShellSession):Boolean; 
var
 WorkBuffer:String;
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;
 
 {Get Connection}
 Connection:=TTelnetConnection(ASession.Data);
 if Connection = nil then Exit;
 
 {Create Text}
 WorkBuffer:=TELNET_CHAR_ESC + '[2J'; {Esc[2J - Clear entire screen}
 
 {Send Text}
 if not FListener.SendText(TWinsock2TCPServerThread(Connection.Thread),WorkBuffer) then Exit;

 {Create Text}
 WorkBuffer:=TELNET_CHAR_ESC + '[H'; {Esc[H - Move cursor to upper left corner}
 
 {Send Text}
 Result:=FListener.SendText(TWinsock2TCPServerThread(Connection.Thread),WorkBuffer);
end;

{==============================================================================}

function TTelnetShell.DoInput(ASession:TShellSession;var AInput:String):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 //To Do
end;
 
{==============================================================================}

function TTelnetShell.DoOutputEx(ASession:TShellSession;const AOutput:String;AReturn:Boolean):Boolean; 
var
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;
 
 {Get Connection}
 Connection:=TTelnetConnection(ASession.Data);
 if Connection = nil then Exit;
 
 {Check Return}
 if AReturn then
  begin
   {Send Text}
   Result:=FListener.SendText(TWinsock2TCPServerThread(Connection.Thread),AOutput + SHELL_LINE_END);
  end
 else
  begin
   {Send Text}
   Result:=FListener.SendText(TWinsock2TCPServerThread(Connection.Thread),AOutput);
  end;
end;

{==============================================================================}

function TTelnetShell.MatchSequence(const ASequence:String;var AContinue:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 //To Do
end;

{==============================================================================}

function TTelnetShell.ProcessSequence(const ASequence:String):Boolean;
begin
 {}
 Result:=False;
 
 //To Do
end;

{==============================================================================}
{==============================================================================}
{TSSHShell}
constructor TSSHShell.Create;
begin
 {}
 inherited Create;
 Name:=SSH_SHELL_NAME;
 Flags:=SHELL_FLAG_CLEAR;
 
 //FListener:=TSSHListener.Create;
 //To Do
 //FListener.Active:=True;
end;

{==============================================================================}

destructor TSSHShell.Destroy;
begin
 {}
 AcquireLock;
 try
  //FListener. //To Do
 finally
  ReleaseLock;
  inherited Destroy;
 end;
end;
 
{==============================================================================}
{==============================================================================}
{TTelnetShellLogout}
constructor TTelnetShellLogout.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=TELNET_SHELL_COMMAND_LOGOUT;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
 
 {Create Alias}
 Alias:=TShellAlias.Create;
 Alias.Name:=TELNET_SHELL_ALIAS_EXIT;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TTelnetShellLogout.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias}
 Alias:=FindAlias(TELNET_SHELL_ALIAS_EXIT);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;

{==============================================================================}

function TTelnetShellLogout.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 Result:=AShell.DoOutput(ASession,'Logout from the current shell session');
end;

{==============================================================================}

function TTelnetShellLogout.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Logout from the current shell session');
end;

{==============================================================================}

function TTelnetShellLogout.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Connection:TTelnetConnection;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Check Session}
 if ASession = nil then Exit;
 
 {Get Connection}
 Connection:=TTelnetConnection(ASession.Data);
 if Connection = nil then Exit;
 if Connection.Thread = nil then Exit;
 
 {Signoff}
 if not AShell.DoOutput(ASession,'Goodbye!') then Exit;
 
 {Wait}
 Sleep(500);
 
 {Disconnect}
 TWinsock2TCPServerThread(Connection.Thread).Server.Disconnect;
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TSSHShellLogout}
constructor TSSHShellLogout.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=SSH_SHELL_COMMAND_LOGOUT;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
 
 {Create Alias}
 Alias:=TShellAlias.Create;
 Alias.Name:=SSH_SHELL_ALIAS_EXIT;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TSSHShellLogout.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias}
 Alias:=FindAlias(SSH_SHELL_ALIAS_EXIT);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RemoteShellInit;
var
 WSAData:TWSAData;
 SSHShell:TSSHShell;
 TelnetShell:TTelnetShell;
begin
 {}
 {Check Initialized}
 if RemoteShellInitialized then Exit;
 
 {Check Telnet Auto Start}
 if TELNET_AUTOSTART then
  begin
   {Start Winsock}
   FillChar(WSAData,SizeOf(TWSAData),0);
   if WSAStartup(WINSOCK_VERSION,WSAData) = ERROR_SUCCESS then
    begin
     {Create Telnet Shell}
     TelnetShell:=TTelnetShell.Create;
     
     {Register Shell}
     if ShellRegisterShell(TelnetShell) then
      begin
       {Register Logout Command}
       TelnetShell.RegisterCommand(TTelnetShellLogout.Create);
      end
     else
      begin
       {Destroy Shell}
       TelnetShell.Free;
      end;      
    end; 
  end;
 
 {Check SSH Auto Start}
 if SSH_AUTOSTART then
  begin
   {Start Winsock}
   FillChar(WSAData,SizeOf(TWSAData),0);
   if WSAStartup(WINSOCK_VERSION,WSAData) = ERROR_SUCCESS then
    begin
     {Create SSH Shell}
     SSHShell:=TSSHShell.Create;
     
     {Register Shell}
     if ShellRegisterShell(SSHShell) then
      begin
       {Register Logout Command}
       //To Do 
      end
     else
      begin
       {Destroy Shell}
       SSHShell.Free;
      end;      
    end; 
  end;
  
 RemoteShellInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Remote Shell Functions}
 
{==============================================================================}
{==============================================================================}
{Remote Shell Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 RemoteShellInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}
 
end.
  