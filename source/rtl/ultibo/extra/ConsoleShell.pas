{
Ultibo Console Shell unit.

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



Console Shell
=============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ConsoleShell;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils,Classes,UltiboClasses,Console,Shell;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

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
 {Console Shell specific classes}
 TConsoleSession = class;
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
  procedure Reset(ASession:TConsoleSession);

  procedure MoveFirst(ASession:TConsoleSession);
  procedure MoveLast(ASession:TConsoleSession);
  procedure MoveLeft(ASession:TConsoleSession);
  procedure MoveRight(ASession:TConsoleSession);

  procedure EraseLine(ASession:TConsoleSession);
  procedure OutputLine(ASession:TConsoleSession;const AValue:String);
  function ExpandLine(ASession:TConsoleSession):Boolean;

  procedure EraseCharacter(ASession:TConsoleSession);
  procedure DeleteCharacter(ASession:TConsoleSession);
  procedure InsertCharacter(ASession:TConsoleSession;ACh:Char);
  procedure OverwriteCharacter(ASession:TConsoleSession;ACh:Char);

  procedure PrevHistory(ASession:TConsoleSession);
  procedure NextHistory(ASession:TConsoleSession);
  procedure FirstHistory(ASession:TConsoleSession);
  procedure LastHistory(ASession:TConsoleSession);
  procedure CurrentHistory(ASession:TConsoleSession);
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
  function DoReset(ASession:TShellSession):Boolean; override;

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

  function DoGetCursorMode(ASession:TShellSession;var AMode:LongWord):Boolean; override;
  function DoSetCursorMode(ASession:TShellSession;AMode:LongWord):Boolean; override;

  function DoGetCursorShape(ASession:TShellSession;var AShape:LongWord):Boolean; override;
  function DoSetCursorShape(ASession:TShellSession;AShape:LongWord):Boolean; override;

  function ConsoleChar(ASession:TShellSession;AChar:Char):Boolean;
  function ConsoleExtended(ASession:TShellSession;AChar:Char):Boolean;
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
  MaxX:LongWord;
  MaxY:LongWord;
  LastX:LongWord;
  LastY:LongWord;
  FirstX:LongWord;
  FirstY:LongWord;
  CurrentX:LongWord;
  CurrentY:LongWord;
  Mode:LongWord;
  Shape:LongWord;
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
function ConsoleShellDeviceAdd(Console:PConsoleDevice;Force:Boolean):LongWord;
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

procedure TConsoleShell.Reset(ASession:TConsoleSession);
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Setup Start}
 ASession.Command:='';
 ASession.MaxX:=ConsoleWindowGetCols(TConsoleSession(ASession).Window);
 ASession.MaxY:=ConsoleWindowGetRows(TConsoleSession(ASession).Window);
 ASession.CurrentX:=ConsoleWindowGetX(TConsoleSession(ASession).Window);
 ASession.CurrentY:=ConsoleWindowGetY(TConsoleSession(ASession).Window);
 ASession.FirstX:=ASession.CurrentX;
 ASession.FirstY:=ASession.CurrentY;
 ASession.LastX:=ASession.CurrentX;
 ASession.LastY:=ASession.CurrentY;
 ASession.Mode:=SHELL_CURSOR_MODE_INSERT;
 ASession.Shape:=SHELL_CURSOR_SHAPE_LINE;

 {Setup Cursor}
 ConsoleWindowSetCursorMode(ASession.Window,CURSOR_MODE_INSERT);
 ConsoleWindowSetCursorShape(ASession.Window,CURSOR_SHAPE_LINE);
 ConsoleWindowSetCursorBlink(ASession.Window,True);
 ConsoleWindowSetCursorState(ASession.Window,CURSOR_STATE_ON);
end;

{==============================================================================}

procedure TConsoleShell.MoveFirst(ASession:TConsoleSession);
{Move the cursor to the starting position of the line}
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 ASession.CurrentX:=ASession.FirstX;
 ASession.CurrentY:=ASession.FirstY;
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);
end;

{==============================================================================}

procedure TConsoleShell.MoveLast(ASession:TConsoleSession);
{Move the cursor to the ending position of the line}
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 ASession.CurrentX:=ASession.LastX;
 ASession.CurrentY:=ASession.LastY;
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);
end;

{==============================================================================}

procedure TConsoleShell.MoveLeft(ASession:TConsoleSession);
{Move the cursor one position to the left in the line}
var
 First:LongWord;
 Current:LongWord;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get Offsets}
 First:=(ASession.FirstY * ASession.MaxX) + ASession.FirstX;
 Current:=(ASession.CurrentY * ASession.MaxX) + ASession.CurrentX;

 {Check Offset}
 if Current = First then Exit;

 {Update Position}
 Dec(ASession.CurrentX);
 if (ASession.CurrentY > ASession.FirstY) and (ASession.CurrentX < 1) then
  begin
   ASession.CurrentX:=ASession.MaxX;
   Dec(ASession.CurrentY);
  end;
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);
end;

{==============================================================================}

procedure TConsoleShell.MoveRight(ASession:TConsoleSession);
{Move the cursor one position to the right in the line}
var
 Last:LongWord;
 Current:LongWord;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get Offsets}
 Last:=(ASession.LastY * ASession.MaxX) + ASession.LastX;
 Current:=(ASession.CurrentY * ASession.MaxX) + ASession.CurrentX;

 {Check Offset}
 if Current = Last then Exit;

 {Update Position}
 Inc(ASession.CurrentX);
 if (ASession.CurrentY < ASession.LastY) and (ASession.CurrentX > ASession.MaxX) then
  begin
   ASession.CurrentX:=1;
   Inc(ASession.CurrentY);
  end;
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);
end;

{==============================================================================}

procedure TConsoleShell.EraseLine(ASession:TConsoleSession);
{Erase all characters in the line and reset the cursor position}
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Update Position}
 ASession.CurrentX:=ASession.FirstX;
 ASession.CurrentY:=ASession.FirstY;
 ASession.LastX:=ASession.FirstX;
 ASession.LastY:=ASession.FirstY;
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);

 {Update Line}
 ConsoleWindowWrite(ASession.Window,StringOfChar(' ',Length(ASession.Command)));
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);

 {Update Command}
 ASession.Command:='';
end;

{==============================================================================}

procedure TConsoleShell.OutputLine(ASession:TConsoleSession;const AValue:String);
{Output all characters in value and update the cursor position}
var
 Count:Integer;
 Value:String;
begin
 {}
 {Check Value}
 if Length(AValue) = 0 then Exit;

 {Move Last}
 MoveLast(ASession);

 {Output Line}
 Value:='';
 for Count:=1 to Length(AValue) do
  begin
   {Update Position}
   Inc(ASession.LastX);
   if ASession.LastX > ASession.MaxX then
    begin
     if ASession.LastY < ASession.MaxY then
      begin
       ASession.LastX:=1;
       Inc(ASession.LastY);
      end
     else
      begin
       if ASession.FirstY > 1 then
        begin
         ASession.LastX:=1;
         Dec(ASession.FirstY);
        end
       else
        begin
         Dec(ASession.LastX);
         Break;
        end;
      end;
    end;

   {Get Character}
   Value:=Value + AValue[Count];
  end;

 {Update Position}
 ASession.CurrentX:=ASession.LastX;
 ASession.CurrentY:=ASession.LastY;

 {Update Line}
 ConsoleWindowWrite(ASession.Window,Value);

 {Update Command}
 ASession.Command:=Value;
end;

{==============================================================================}

function TConsoleShell.ExpandLine(ASession:TConsoleSession):Boolean;
{Expand the tab key to a command completion if available}
var
 Value:String;
 Error:Boolean;
begin
 {}
 Result:=False;

 {Check Session}
 if ASession = nil then Exit;

 {Complete Command}
 Value:=CompleteCommand(ASession,ASession.Command,Error);
 if Value <> ASession.Command then
  begin
   {Erase Current}
   EraseLine(ASession);

   {Output Line}
   OutputLine(ASession,Value);

   {Update Command}
   ASession.Command:=Value;
  end;

 {Return True (Prevent Tab Character)}
 Result:=True;
end;

{==============================================================================}

procedure TConsoleShell.EraseCharacter(ASession:TConsoleSession);
{Erase the character to the left of the cursor position}
var
 Head:String;
 Tail:String;
 Last:LongWord;
 First:LongWord;
 Current:LongWord;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get Offsets}
 First:=(ASession.FirstY * ASession.MaxX) + ASession.FirstX;
 Last:=(ASession.LastY * ASession.MaxX) + ASession.LastX;
 Current:=(ASession.CurrentY * ASession.MaxX) + ASession.CurrentX;

 {Check Offset}
 if Current = First then Exit;

 {Get Head and Tail}
 Head:=Copy(ASession.Command,1,(Current - First) - 1);
 Tail:=Copy(ASession.Command,(Current - First) + 1,Length(ASession.Command));

 {Update Position}
 Dec(ASession.CurrentX);
 if (ASession.CurrentY > ASession.FirstY) and (ASession.CurrentX < 1) then
  begin
   ASession.CurrentX:=ASession.MaxX;
   Dec(ASession.CurrentY);
  end;

 Dec(ASession.LastX);
 if (ASession.LastY > ASession.FirstY) and (ASession.LastX < 1) then
  begin
   ASession.LastX:=ASession.MaxX;
   Dec(ASession.LastY);
  end;
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);

 {Update Line}
 ConsoleWindowWrite(ASession.Window,Tail + ' ');
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);

 {Update Command}
 ASession.Command:=Head + Tail;
end;

{==============================================================================}

procedure TConsoleShell.DeleteCharacter(ASession:TConsoleSession);
{Delete the character to the right of the cursor position}
var
 Head:String;
 Tail:String;
 Last:LongWord;
 First:LongWord;
 Current:LongWord;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get Offsets}
 First:=(ASession.FirstY * ASession.MaxX) + ASession.FirstX;
 Last:=(ASession.LastY * ASession.MaxX) + ASession.LastX;
 Current:=(ASession.CurrentY * ASession.MaxX) + ASession.CurrentX;

 {Check Offset}
 if Current = Last then Exit;

 {Get Head and Tail}
 Head:=Copy(ASession.Command,1,Current - First);
 Tail:=Copy(ASession.Command,(Current - First) + 2,Length(ASession.Command));

 {Update Position}
 Dec(ASession.LastX);
 if (ASession.LastY > ASession.FirstY) and (ASession.LastX < 1) then
  begin
   ASession.LastX:=ASession.MaxX;
   Dec(ASession.LastY);
  end;

 {Update Line}
 ConsoleWindowWrite(ASession.Window,Tail + ' ');
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);

 {Update Command}
 ASession.Command:=Head + Tail;
end;

{==============================================================================}

procedure TConsoleShell.InsertCharacter(ASession:TConsoleSession;ACh:Char);
{Insert a character at the cursor position}
var
 Head:String;
 Tail:String;
 Last:LongWord;
 First:LongWord;
 Current:LongWord;
 Scroll:Boolean;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get Offsets}
 First:=(ASession.FirstY * ASession.MaxX) + ASession.FirstX;
 Last:=(ASession.LastY * ASession.MaxX) + ASession.LastX;
 Current:=(ASession.CurrentY * ASession.MaxX) + ASession.CurrentX;

 {Get Head and Tail}
 Head:=Copy(ASession.Command,1,Current - First);
 Tail:=Copy(ASession.Command,(Current - First) + 1,Length(ASession.Command));

 Scroll:=False;

 {Update Position}
 Inc(ASession.LastX);
 if ASession.LastX > ASession.MaxX then
  begin
   if ASession.LastY < ASession.MaxY then
    begin
     ASession.LastX:=1;
     Inc(ASession.LastY);
    end
   else
    begin
     if ASession.FirstY > 1 then
      begin
       ASession.LastX:=1;
       Dec(ASession.FirstY);
       Dec(ASession.CurrentY);
       Scroll:=True;
      end
     else
      begin
       Dec(ASession.LastX);
       Exit;
      end;
    end;
  end;

 Inc(ASession.CurrentX);
 if ASession.CurrentX > ASession.MaxX then
  begin
   if ASession.CurrentY < ASession.MaxY then
    begin
     ASession.CurrentX:=1;
     Inc(ASession.CurrentY);
    end
   else
    begin
     if Scroll then
      begin
       ASession.CurrentX:=1;
      end
     else
      begin
       Dec(ASession.CurrentX);
       Exit;
      end;
    end;
  end;

 {Update Line}
 ConsoleWindowWrite(ASession.Window,ACh + Tail);
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);

 {Update Command}
 ASession.Command:=Head + ACh + Tail;
end;

{==============================================================================}

procedure TConsoleShell.OverwriteCharacter(ASession:TConsoleSession;ACh:Char);
{Overwrite the character at the cursor position}
var
 Head:String;
 Tail:String;
 Last:LongWord;
 First:LongWord;
 Current:LongWord;
 Scroll:Boolean;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get Offsets}
 First:=(ASession.FirstY * ASession.MaxX) + ASession.FirstX;
 Last:=(ASession.LastY * ASession.MaxX) + ASession.LastX;
 Current:=(ASession.CurrentY * ASession.MaxX) + ASession.CurrentX;

 {Get Head and Tail}
 Head:=Copy(ASession.Command,1,Current - First);
 Tail:=Copy(ASession.Command,(Current - First) + 2,Length(ASession.Command));

 Scroll:=False;

 {Update Position}
 if (ASession.CurrentX = ASession.LastX) and (ASession.CurrentY = ASession.LastY) then
  begin
   Inc(ASession.LastX);
  end;

 Inc(ASession.CurrentX);
 if ASession.CurrentX > ASession.MaxX then
  begin
   if (ASession.CurrentX = ASession.LastX) and (ASession.CurrentY = ASession.LastY) then
    begin
     if ASession.LastY < ASession.MaxY then
      begin
       ASession.LastX:=1;
       Inc(ASession.LastY);
      end
     else
      begin
       if ASession.FirstY > 1 then
        begin
         ASession.LastX:=1;
         Dec(ASession.FirstY);
         Dec(ASession.CurrentY);
         Scroll:=True;
        end
       else
        begin
         Dec(ASession.LastX);
        end;
      end;
    end;

   if ASession.CurrentY < ASession.MaxY then
    begin
     ASession.CurrentX:=1;
     Inc(ASession.CurrentY);
    end
   else
    begin
     if Scroll then
      begin
       ASession.CurrentX:=1;
      end
     else
      begin
       Dec(ASession.CurrentX);
       Exit;
      end;
    end;
  end;

 {Update Line}
 ConsoleWindowWrite(ASession.Window,ACh + Tail);
 ConsoleWindowSetXY(ASession.Window,ASession.CurrentX,ASession.CurrentY);

 {Update Command}
 ASession.Command:=Head + ACh + Tail;
end;

{==============================================================================}

procedure TConsoleShell.PrevHistory(ASession:TConsoleSession);
{Get the previous command history value}
var
 Value:String;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get History}
 Value:=ASession.PrevHistory;
 if Length(Value) = 0 then Exit;

 {Erase Current}
 EraseLine(ASession);

 {Output Line}
 OutputLine(ASession,Value);
end;

{==============================================================================}

procedure TConsoleShell.NextHistory(ASession:TConsoleSession);
{Get the next command history value}
var
 Value:String;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get History}
 Value:=ASession.NextHistory;
 if Length(Value) = 0 then Exit;

 {Erase Current}
 EraseLine(ASession);

 {Output Line}
 OutputLine(ASession,Value);
end;

{==============================================================================}

procedure TConsoleShell.FirstHistory(ASession:TConsoleSession);
{Get the first command history value}
var
 Value:String;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get History}
 Value:=ASession.FirstHistory;
 if Length(Value) = 0 then Exit;

 {Erase Current}
 EraseLine(ASession);

 {Output Line}
 OutputLine(ASession,Value);
end;

{==============================================================================}

procedure TConsoleShell.LastHistory(ASession:TConsoleSession);
{Get the last command history value}
var
 Value:String;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get History}
 Value:=ASession.LastHistory;
 if Length(Value) = 0 then Exit;

 {Erase Current}
 EraseLine(ASession);

 {Output Line}
 OutputLine(ASession,Value);
end;

{==============================================================================}

procedure TConsoleShell.CurrentHistory(ASession:TConsoleSession);
{Get the current command history value}
var
 Value:String;
begin
 {}
 {Check Session}
 if ASession = nil then Exit;

 {Get History}
 Value:=ASession.CurrentHistory;
 if Length(Value) = 0 then Exit;

 {Erase Current}
 EraseLine(ASession);

 {Output Line}
 OutputLine(ASession,Value);
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

 if not AcquireLock then Exit;
 try
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

  {Reset Session}
  Reset(Session);

  {Check Default}
  if FDefaultSession = nil then
   begin
    FDefaultSession:=Session;
   end;

  {Return Result}
  Result:=Session;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TConsoleShell.DestroyWindow(ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;

 {Check Session}
 if ASession = nil then Exit;

 if not AcquireLock then Exit;
 try
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
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TConsoleShell.DoReset(ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;

 {Check Session}
 if ASession = nil then Exit;

 {Reset Session}
 Reset(TConsoleSession(ASession));

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
var
 Session:TConsoleSession;
begin
 {}
 Result:=False;

 {Check Session}
 if ASession = nil then Exit;

 {Get Session}
 Session:=TConsoleSession(ASession);

 //To Do //ConsoleWindowReadLnEx - No History / No Completion
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

function TConsoleShell.DoGetCursorMode(ASession:TShellSession;var AMode:LongWord):Boolean;
var
 Mode:TCursorMode;
begin
 {}
 Result:=False;

 {Set Default}
 AMode:=SHELL_CURSOR_MODE_INSERT;

 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Get Mode}
 Mode:=ConsoleWindowGetCursorMode(TConsoleSession(ASession).Window);
 if Mode = CURSOR_MODE_OVERWRITE then AMode:=SHELL_CURSOR_MODE_OVERWRITE;

 Result:=True;
end;

{==============================================================================}

function TConsoleShell.DoSetCursorMode(ASession:TShellSession;AMode:LongWord):Boolean;
const
 Modes:array[SHELL_CURSOR_MODE_INSERT..SHELL_CURSOR_MODE_OVERWRITE] of TCursorMode =
  (CURSOR_MODE_INSERT, CURSOR_MODE_OVERWRITE);
begin
 {}
 Result:=False;

 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Check Mode}
 if AMode > SHELL_CURSOR_MODE_OVERWRITE then Exit;

 {Set Mode}
 Result:=(ConsoleWindowSetCursorMode(TConsoleSession(ASession).Window,Modes[AMode]) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConsoleShell.DoGetCursorShape(ASession:TShellSession;var AShape:LongWord):Boolean;
var
 Shape:TCursorShape;
begin
 {}
 Result:=False;

 {Set Default}
 AShape:=SHELL_CURSOR_SHAPE_LINE;

 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Get Shape}
 Shape:=ConsoleWindowGetCursorShape(TConsoleSession(ASession).Window);
 if Shape = CURSOR_SHAPE_BAR then
  begin
   AShape:=SHELL_CURSOR_SHAPE_BAR;
  end
 else if Shape = CURSOR_SHAPE_BLOCK then
  begin
   AShape:=SHELL_CURSOR_SHAPE_BLOCK;
  end;

 Result:=True;
end;

{==============================================================================}

function TConsoleShell.DoSetCursorShape(ASession:TShellSession;AShape:LongWord):Boolean;
const
 Shapes:array[SHELL_CURSOR_SHAPE_LINE..SHELL_CURSOR_SHAPE_BLOCK] of TCursorShape =
  (CURSOR_SHAPE_LINE, CURSOR_SHAPE_BAR, CURSOR_SHAPE_BLOCK);
begin
 {}
 Result:=False;

 {Check Session}
 if ASession = nil then Exit;

 {Check Window}
 if TConsoleSession(ASession).Window = INVALID_HANDLE_VALUE then Exit;

 {Check Shape}
 if AShape > SHELL_CURSOR_SHAPE_BLOCK then Exit;

 {Set Shape}
 Result:=(ConsoleWindowSetCursorShape(TConsoleSession(ASession).Window,Shapes[AShape]) = ERROR_SUCCESS);
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

 {Check Key}
 case AChar of
  #8:begin  {Backspace}
    EraseCharacter(Session);
   end;
  #9:begin  {Tab}
    if not ExpandLine(Session) then
     begin
      if Session.Mode = SHELL_CURSOR_MODE_INSERT then
       begin
        InsertCharacter(Session,AChar);
       end
      else
       begin
        OverwriteCharacter(Session,AChar);
       end;
     end;
   end;
  #13:begin {Return}
    {End}
    MoveLast(Session);

    {Send Char}
    DoOutput(Session,'');

    {Check Command}
    if Length(Session.Command) > 0 then
     begin
      {Process Command}
      ProcessCommand(Session,Session.Command);

      {Add History}
      Session.AddHistory(Session.Command);
     end;

    {Send Prompt}
    DoPrompt(Session);

    {Reset State}
    Reset(Session);
   end;
  #27:begin {Escape}
    {Clear Command}
    EraseLine(Session);

    {Send Char}
    DoOutput(Session,'');

    {Send Prompt}
    DoPrompt(Session);

    {Reset State}
    Reset(Session);
   end;
  else
   begin    {Any Other Key}
    if Session.Mode = SHELL_CURSOR_MODE_INSERT then
     begin
      InsertCharacter(Session,AChar);
     end
    else
     begin
      OverwriteCharacter(Session,AChar);
     end;
   end;
 end;
end;

{==============================================================================}

function TConsoleShell.ConsoleExtended(ASession:TShellSession;AChar:Char):Boolean;
var
 Session:TConsoleSession;
begin
 {}
 Result:=False;

 {Check Session}
 if ASession = nil then Exit;

 {Get Session}
 Session:=TConsoleSession(ASession);

 {Check Key}
 case AChar of
  #75:begin {Left Arrow}
    MoveLeft(Session);
   end;
  #77:begin {Right Arrow}
    MoveRight(Session);
   end;
  #72:begin {Up Arrow}
    PrevHistory(Session);
   end;
  #80:begin {Down Arrow}
    NextHistory(Session);
   end;
  #82:begin {Insert}
    {Check Mode}
    if Session.Mode = SHELL_CURSOR_MODE_INSERT then
     begin
      Session.Mode:=SHELL_CURSOR_MODE_OVERWRITE;
      Session.Shape:=SHELL_CURSOR_SHAPE_BAR;
     end
    else
     begin
      Session.Mode:=SHELL_CURSOR_MODE_INSERT;
      Session.Shape:=SHELL_CURSOR_SHAPE_LINE;
     end;

    {Set Mode}
    DoSetCursorMode(Session,Session.Mode);
    DoSetCursorShape(Session,Session.Shape);
   end;
  #83:begin {Delete}
    DeleteCharacter(Session);
   end;
  #71:begin {Home}
    MoveFirst(Session);
   end;
  #79:begin {End}
    MoveLast(Session);
   end;
  #73:begin {Page Up}
    FirstHistory(Session);
   end;
  #81:begin {Page Down}
    LastHistory(Session);
   end;
  #59:begin {F1}
    {Nothing}
   end;
  #60:begin {F2}
    {Nothing}
   end;
  #61:begin {F3}
    CurrentHistory(Session);
   end;
  #62:begin {F4}
    {Nothing}
   end;
  #63:begin {F5}
    {Nothing}
   end;
  #64:begin {F6}
    {Nothing}
   end;
  #65:begin {F7}
    {Nothing}
   end;
  #66:begin {F8}
    {Nothing}
   end;
  #67:begin {F9}
    {Nothing}
   end;
  #68:begin {F10}
    {Nothing}
   end;
 end;

 Result:=True;
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
     {Get Key}
     if ConsoleGetKey(Character,nil) then
      begin
       if Character = #0 then
        begin
         {Extended Key}
         if ConsoleGetKey(Character,nil) then
          begin
           {Write Console Shell}
           FShell.ConsoleExtended(FShell.DefaultSession,Character);
          end;
        end
       else
        begin
         {Write Console Shell}
         FShell.ConsoleChar(FShell.DefaultSession,Character);
        end;
      end;
    end;
  end;
 except
  on E: Exception do
   begin
    if SHELL_LOG_ENABLED then ShellLogError('ConsoleShellThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
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
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if ConsoleShellInitialized then Exit;

 {Check Environment Variables}
 {CONSOLE_SHELL_ENABLED}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_SHELL_ENABLED'),0);
 if WorkInt <> 0 then CONSOLE_SHELL_ENABLED:=True;

 {CONSOLE_SHELL_POSITION}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_SHELL_POSITION'),0);
 if WorkInt > 0 then CONSOLE_SHELL_POSITION:=WorkInt;

 {CONSOLE_SHELL_DEVICE}
 WorkBuffer:=EnvironmentGet('CONSOLE_SHELL_DEVICE');
 if Length(WorkBuffer) <> 0 then CONSOLE_SHELL_DEVICE:=WorkBuffer;

 {Enumerate Consoles}
 ConsoleDeviceEnumerate(ConsoleShellDeviceEnum,nil);

 {Register Notification}
 ConsoleDeviceNotification(nil,ConsoleShellDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER or DEVICE_NOTIFICATION_OPEN or DEVICE_NOTIFICATION_CLOSE,NOTIFIER_FLAG_WORKER);

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
function ConsoleShellDeviceAdd(Console:PConsoleDevice;Force:Boolean):LongWord;
var
 ConsoleShell:TConsoleShell;
begin
 {}
 Result:=ERROR_SUCCESS;

 {Check Console Shell}
 if ConsoleShellFindByDevice(Console) = nil then
  begin
   {Check Enabled}
   if (CONSOLE_SHELL_ENABLED and not(ConsoleDeviceCheckFlag(Console,CONSOLE_FLAG_SINGLE_WINDOW))) or Force then
    begin
     {Check Device}
     if not(Force) then
      begin
       if Length(CONSOLE_SHELL_DEVICE) <> 0 then
        begin
         {Check Name}
         if ConsoleDeviceFindByName(CONSOLE_SHELL_DEVICE) <> Console then
          begin
           {Check Description}
           if ConsoleDeviceFindByDescription(CONSOLE_SHELL_DEVICE) <> Console then Exit;
          end;
        end
       else
        begin
         {Check Default}
         if ConsoleDeviceGetDefault <> Console then Exit;
        end;
      end;

     {Create Console Shell}
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
 Result:=ConsoleShellDeviceAdd(Console,False);
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
   Result:=ConsoleShellDeviceAdd(Console,False);
  end
 else if (Notification and DEVICE_NOTIFICATION_OPEN) <> 0 then
  begin
   {Check Console}
   if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

   {Add Console}
   Result:=ConsoleShellDeviceAdd(Console,False);
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
