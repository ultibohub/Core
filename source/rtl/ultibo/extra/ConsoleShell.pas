{
Ultibo Console Shell unit.

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

 

Console Shell
=============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ConsoleShell;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils,Classes,UltiboClasses,Console,Shell;

//To Do //Look for:

//--

{==============================================================================}
const
 {Console Shell specific constants}
 CONSOLE_SHELL_DEFAULT_WELCOME = ' (Type HELP for a list of available commands)';

 CONSOLE_SHELL_THREAD_NAME = 'Console Shell Keyboard';  {Thread name for Console shell keyboard threads}
 
 {Console Shell constants}
 CONSOLE_SHELL_NAME = 'Console Shell';

 {Console Shell Command constants}
 CONSOLE_SHELL_COMMAND_EXIT = 'EXIT';
 
{==============================================================================}
{type}
 {Console Shell specific types}
 
{==============================================================================}
type
 {Console Shell specific clases}
 TConsoleShellThread = class;
 TConsoleShell = class(TShell)
 public
  {}
  constructor Create(AConsole:PConsoleDevice);
  destructor Destroy; override;
 private
  {Internal Variables}
  FConsole:PConsoleDevice;
  FThread:TConsoleShellThread;
  FDefaultSession:TShellSession;
  
  {Internal Methods}
  
 protected
  {Internal Variables}

  {Internal Methods}
  function CreateWindow:TShellSession;
  function DestroyWindow(ASession:TShellSession):Boolean;
 public
  {Public Properties}
  property Console:PConsoleDevice read FConsole;
  property DefaultSession:TShellSession read FDefaultSession;
  
  {Public Methods}
  function DoClear(ASession:TShellSession):Boolean; override;
  
  function DoInput(ASession:TShellSession;var AInput:String):Boolean; override;
  
  function DoOutputEx(ASession:TShellSession;const AOutput:String;AReturn:Boolean):Boolean; override;
  
  function DoGetSize(ASession:TShellSession;var ARows,ACols:LongWord):Boolean; override;
  function DoSetSize(ASession:TShellSession;ARows,ACols:LongWord):Boolean; override;
  
  function DoGetCursor(ASession:TShellSession;var ARow,ACol:LongWord):Boolean; override;
  function DoSetCursor(ASession:TShellSession;ARow,ACol:LongWord):Boolean; override;

  function DoGetColors(ASession:TShellSession;var AForecolor,ABackcolor:LongWord):Boolean; override;
  function DoSetColors(ASession:TShellSession;AForecolor,ABackcolor:LongWord):Boolean; override;
  
  function DoGetCoordinates(ASession:TShellSession;var ARow,ACol:LongWord):Boolean; override;
  function DoSetCoordinates(ASession:TShellSession;ARow,ACol:LongWord):Boolean; override;
  
  function ConsoleChar(ASession:TShellSession;AChar:Char):Boolean;
 end;
 
 TConsoleSession = class(TShellSession)
 public
  {}
  constructor Create(AShell:TShell;AIdentifier:LongWord);
 private
  {Internal Variables}

 protected
  {Internal Variables}
  
  {Internal Methods}
  
 public
  {Public Properties}
  Command:String;
  Position:LongWord;
  Window:TWindowHandle;
  
  {Public Methods}
  
 end;
 
 TConsoleShellThread = class(TThread)
 public
  {}
  constructor Create(AShell:TConsoleShell);
 protected
  {Internal Variables}
  FShell:TConsoleShell;
   
  {Internal Methods}
  procedure Execute; override;
 public  
  {Public Properties}
  
  {Public Methods}
 end;
 
 TConsoleShellExit = class(TShellCommand)
 public
  {}
  constructor Create;
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
  
{==============================================================================}
{var}
 {Console Shell specific variables}

{==============================================================================}
{Initialization Functions}
procedure ConsoleShellInit;

{==============================================================================}
{Console Shell Functions}
function ConsoleShellFindByDevice(Console:PConsoleDevice):TConsoleShell;

{==============================================================================}
{Console Shell Helper Functions}
function ConsoleShellDeviceAdd(Console:PConsoleDevice):LongWord;
function ConsoleShellDeviceRemove(Console:PConsoleDevice):LongWord;

function ConsoleShellDeviceEnum(Console:PConsoleDevice;Data:Pointer):LongWord;
function ConsoleShellDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Console Shell specific variables}
 ConsoleShellInitialized:Boolean;
 
{==============================================================================}
{==============================================================================}
{TConsoleShell}
constructor TConsoleShell.Create(AConsole:PConsoleDevice);
begin
 {}
 inherited Create;
 FConsole:=AConsole;
 
 Name:=CONSOLE_SHELL_NAME;
 Flags:=SHELL_FLAG_CLEAR or SHELL_FLAG_SIZE or SHELL_FLAG_CURSOR or SHELL_FLAG_COLORS or SHELL_FLAG_COORDINATES;

 {Create Thread}
 FThread:=TConsoleShellThread.Create(Self);
 FThread.FreeOnTerminate:=True;
  
 {Start Thread}
 FThread.Start;

 {Create Window}
 FDefaultSession:=CreateWindow;
end;

{==============================================================================}

destructor TConsoleShell.Destroy; 
begin
 {}
 AcquireLock;
 try
  {Terminate Thread}
  FThread.Terminate;

  {Destroy Window}
  DestroyWindow(FDefaultSession);
 finally
  ReleaseLock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TConsoleShell.CreateWindow:TShellSession;
var
 Handle:TWindowHandle;
 Session:TConsoleSession;
begin
 {}
 Result:=nil;
 
 {Check Console}
 if FConsole = nil then Exit;
 
 //To Do //Lock
 
 {Create Window (Default position)}
 Handle:=ConsoleWindowCreate(FConsole,CONSOLE_SHELL_POSITION,False);
 
 {Create Window (Any position)}
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_FULL,False);
  end; 
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_TOP,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_BOTTOM,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_LEFT,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_RIGHT,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_TOPLEFT,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_TOPRIGHT,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_BOTTOMLEFT,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then
  begin
   Handle:=ConsoleWindowCreate(FConsole,CONSOLE_POSITION_BOTTOMRIGHT,False);
  end;
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Create Session}
 Session:=TConsoleSession.Create(Self,Handle);
 Session.Window:=Handle;
 
 {Register Session}
 if not RegisterSession(Session) then
  begin
   {Destroy Window}
   ConsoleWindowDestroy(Handle);

   {Destroy Session}
   Session.Free;
   
   Exit;
  end;
  
 {Clear Screen}
 if not DoClear(Session) then Exit;
 
 {Send Banner}
 if not DoBanner(Session) then Exit;
 
 {Send Welcome}
 if not DoOutput(Session,CONSOLE_SHELL_DEFAULT_WELCOME) then Exit;
 
 {Send Prompt}
 if not DoPrompt(Session) then Exit;

 {Check Default}
 if FDefaultSession = nil then
  begin
   FDefaultSession:=Session;
  end;
 
 {Return Result}
 Result:=Session;
end;

{==============================================================================}

function TConsoleShell.DestroyWindow(ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 //To Do //Lock
 
 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;
 
 {Deregister Session}
 if not DeregisterSession(ASession) then Exit;
 
 {Destroy Window}
 ConsoleWindowDestroy(TConsoleSession(ASession).Window);
 
 {Check Default}
 if FDefaultSession = ASession then
  begin
   FDefaultSession:=nil;
  end;
 
 {Destroy Session}
 ASession.Free;
 
 Result:=True;
end;

{==============================================================================}

function TConsoleShell.DoClear(ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Clear Screen}
 Result:=(ConsoleWindowClear(TConsoleSession(ASession).Window) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConsoleShell.DoInput(ASession:TShellSession;var AInput:String):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 //To Do
end;

{==============================================================================}

function TConsoleShell.DoOutputEx(ASession:TShellSession;const AOutput:String;AReturn:Boolean):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Check Return}
 if AReturn then
  begin
   {Write Text}
   Result:=(ConsoleWindowWriteLn(TConsoleSession(ASession).Window,AOutput) = ERROR_SUCCESS);
  end
 else
  begin
   {Write Text}
   Result:=(ConsoleWindowWrite(TConsoleSession(ASession).Window,AOutput) = ERROR_SUCCESS);
  end;  
end;

{==============================================================================}

function TConsoleShell.DoGetSize(ASession:TShellSession;var ARows,ACols:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Get Size}
 ACols:=ConsoleWindowGetCols(TConsoleSession(ASession).Window);
 ARows:=ConsoleWindowGetRows(TConsoleSession(ASession).Window);
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TConsoleShell.DoSetSize(ASession:TShellSession;ARows,ACols:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Set Size}
 {Nothing}
end;

{==============================================================================}

function TConsoleShell.DoGetCursor(ASession:TShellSession;var ARow,ACol:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Get Cursor}
 Result:=(ConsoleWindowGetCursorXY(TConsoleSession(ASession).Window,ACol,ARow) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConsoleShell.DoSetCursor(ASession:TShellSession;ARow,ACol:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Set Cursor}
 Result:=(ConsoleWindowSetCursorXY(TConsoleSession(ASession).Window,ACol,ARow) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConsoleShell.DoGetColors(ASession:TShellSession;var AForecolor,ABackcolor:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Get Colors}
 AForecolor:=ConsoleWindowGetForecolor(TConsoleSession(ASession).Window);
 ABackcolor:=ConsoleWindowGetBackcolor(TConsoleSession(ASession).Window);
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TConsoleShell.DoSetColors(ASession:TShellSession;AForecolor,ABackcolor:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Set Colors}
 Result:=(ConsoleWindowSetForecolor(TConsoleSession(ASession).Window,AForecolor) = ERROR_SUCCESS);
 if Result then
  begin
   Result:=(ConsoleWindowSetBackcolor(TConsoleSession(ASession).Window,ABackcolor) = ERROR_SUCCESS);
  end;
end;

{==============================================================================}

function TConsoleShell.DoGetCoordinates(ASession:TShellSession;var ARow,ACol:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Get Coordinates}
 Result:=(ConsoleWindowGetXY(TConsoleSession(ASession).Window,ACol,ARow) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConsoleShell.DoSetCoordinates(ASession:TShellSession;ARow,ACol:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Set Coordinates}
 Result:=(ConsoleWindowSetXY(TConsoleSession(ASession).Window,ACol,ARow) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConsoleShell.ConsoleChar(ASession:TShellSession;AChar:Char):Boolean;
var
 Session:TConsoleSession;
begin
 {}
 Result:=False;
 
 {Check Session}
 if ASession = nil then Exit;
 
 {Get Session}
 Session:=TConsoleSession(ASession);
 
 {Check for ENTER}
 if AChar  = #13 then //To Do
  begin
   {Send Char}
   DoOutput(Session,'');

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
 {Check for TAB}
 else if AChar = #9 then //To Do
  begin
   {Match Command}
   //To Do 
  end
 {Check for Backspace}
 else if AChar = #8 then //To Do
  begin
   if Length(Session.Command) > 0 then
    begin
     //To Do //Remove from screen
     
     {Remove Character}
     SetLength(Session.Command,Length(Session.Command) - 1);
     Dec(Session.Position);
    end; 
  end
 else
  begin     
   {Send Char}
   DoOutputEx(Session,AChar,False);

   {Add Command}
   Session.Command:=Session.Command + AChar;
   Inc(Session.Position);
  end; 
end;

{==============================================================================}
{==============================================================================}
{TConsoleSession}
constructor TConsoleSession.Create(AShell:TShell;AIdentifier:LongWord);
begin
 {}
 inherited Create(AShell,AIdentifier);
 
 Window:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}
{==============================================================================}
{TConsoleShellThread}
constructor TConsoleShellThread.Create(AShell:TConsoleShell);
begin
 {}
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
 FShell:=AShell;
end;

{==============================================================================}

procedure TConsoleShellThread.Execute; 
var
 Character:Char;
begin
 {}
 try
 {Set Name}
 ThreadSetName(GetCurrentThreadID,CONSOLE_SHELL_THREAD_NAME);
 
 while not(Terminated) do
  begin
   {Check Shell}
   if FShell <> nil then
    begin
     {Read Console}
     if ConsoleReadChar(Character,nil) then
      begin
       {Write Console Shell}
       FShell.ConsoleChar(FShell.DefaultSession,Character);
      end; 
    end;
  end;  
 except
  on E: Exception do
   begin
    if SHELL_LOG_ENABLED then ShellLogError('ConsoleShellThread: Exception: ' + E.Message + ' at ' + IntToHex(LongWord(ExceptAddr),8));
   end;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TConsoleShellExit}
constructor TConsoleShellExit.Create;
begin
 {}
 inherited Create;

 Name:=CONSOLE_SHELL_COMMAND_EXIT;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TConsoleShellExit.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 Result:=AShell.DoOutput(ASession,'Close the current console window and end this session');
end;

{==============================================================================}

function TConsoleShellExit.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Close the current console window and end this session');
end;

{==============================================================================}

function TConsoleShellExit.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Exit}
 Result:=TConsoleShell(AShell).DestroyWindow(ASession);
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ConsoleShellInit;
begin
 {}
 {Check Initialized}
 if ConsoleShellInitialized then Exit;
 
 {Enumerate Consoles}
 ConsoleDeviceEnumerate(ConsoleShellDeviceEnum,nil);
 
 {Register Notification}
 ConsoleDeviceNotification(nil,ConsoleShellDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER or DEVICE_NOTIFICATION_OPEN or DEVICE_NOTIFICATION_CLOSE,NOTIFIER_FLAG_NONE);
 
 ConsoleShellInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{Console Shell Functions}
function ConsoleShellFindByDevice(Console:PConsoleDevice):TConsoleShell;
var
 Shell:TShell;
begin
 {}
 Result:=nil;
 
 {Get Shell}
 Shell:=ShellGetShell(nil,True,False);
 while Shell <> nil do
  begin
   {Check Shell}
   if Shell is TConsoleShell then
    begin
     {Check Console}
     if TConsoleShell(Shell).Console = Console then
      begin
       {Unlock Shell}
       ShellGetShell(Shell,False,True);
       
       Result:=TConsoleShell(Shell);
       Exit;
      end;
    end; 
   
   {Get Next}
   Shell:=ShellGetShell(Shell,True,True);
  end; 
end;

{==============================================================================}
{==============================================================================}
{Console Shell Helper Functions}
function ConsoleShellDeviceAdd(Console:PConsoleDevice):LongWord;
var
 ConsoleShell:TConsoleShell;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Console Shell}
 if ConsoleShellFindByDevice(Console) = nil then
  begin
   {Create Console Shell}
   if CONSOLE_SHELL_ENABLED then
    begin
     ConsoleShell:=TConsoleShell.Create(Console);
     ConsoleShell.Name:=CONSOLE_SHELL_NAME + ' (' + DeviceGetName(@Console.Device) + ')';
     
     {Register Shell}
     if ShellRegisterShell(ConsoleShell) then
      begin
       {Register Exit Command}
       ConsoleShell.RegisterCommand(TConsoleShellExit.Create);
      end
     else
      begin
       {Destroy Shell}
       ConsoleShell.Free;
      end;
    end;  
  end; 
end;

{==============================================================================}

function ConsoleShellDeviceRemove(Console:PConsoleDevice):LongWord;
var
 ConsoleShell:TConsoleShell;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Console Shell}
 ConsoleShell:=ConsoleShellFindByDevice(Console);
 if ConsoleShell <> nil then
  begin
   {Deregister Shell}
   if ShellDeregisterShell(ConsoleShell) then
    begin
     {Deregister Exit Command}
     //To Do
     
     {Destroy Shell}
     ConsoleShell.Free;
    end;
  end; 
end;

{==============================================================================}

function ConsoleShellDeviceEnum(Console:PConsoleDevice;Data:Pointer):LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IFDEF SHELL_DEBUG}
 if SHELL_LOG_ENABLED then ShellLogDebug('Console Shell: Console device enumeration');
 {$ENDIF}
 
 {Check Console}
 if Console = nil then Exit;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit; 
 
 {Add Console}
 Result:=ConsoleShellDeviceAdd(Console);
end;

{==============================================================================}

function ConsoleShellDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
var
 Console:PConsoleDevice;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IFDEF SHELL_DEBUG}
 if SHELL_LOG_ENABLED then ShellLogDebug('Console Shell: Console device notification (Notification=' + NotificationToString(Notification) + ')');
 {$ENDIF}
 
 {Check Device}
 if Device = nil then Exit;
 
 {Get Console}
 Console:=PConsoleDevice(Device);

 {Check Notification}
 if (Notification and DEVICE_NOTIFICATION_REGISTER) <> 0 then
  begin
   {Check Console}
   if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit; 
   
   {Add Console}
   Result:=ConsoleShellDeviceAdd(Console);
  end
 else if (Notification and DEVICE_NOTIFICATION_OPEN) <> 0 then
  begin
   {Check Console}
   if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit; 

   {Add Console}
   Result:=ConsoleShellDeviceAdd(Console);
  end
 else if (Notification and DEVICE_NOTIFICATION_DEREGISTER) <> 0 then
  begin
   {Remove Console}
   Result:=ConsoleShellDeviceRemove(Console);
  end
 else if (Notification and DEVICE_NOTIFICATION_CLOSE) <> 0 then
  begin
   {Remove Console}
   Result:=ConsoleShellDeviceRemove(Console);
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 ConsoleShellInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}
 
end.
