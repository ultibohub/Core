{
Ultibo Authentication interface unit.

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


Authentication
==============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Authentication;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  System.SysUtils,
  System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Authentication specific constants}

 {Authenticator Flag constants}
 AUTHENTICATOR_FLAG_NONE = $00000000;

 //To Do

{==============================================================================}
{type}
 {Authentication specific types}
 //To Do

{==============================================================================}
type
 {Authentication specific classes}
 TAuthenticator = class(TObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle;

  FFlags:LongWord;

  {Internal Methods}
  procedure SetFlags(AFlags:LongWord);
 protected
  {Internal Variables}

  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 public
  {Public Properties}
  property Flags:LongWord read FFlags write SetFlags;

  {Public Methods}
  function UsernamePrompt:String; virtual;
  function PasswordPrompt(const AUsername:String):String; virtual;

  function CheckUsername(const AUsername:String):LongWord; virtual;
  function CheckPassword(const AUsername,APassword:String):LongWord; virtual;
 end;

 TNullAuthenticator = class(TAuthenticator) {An authenticator module that accepts any username and password}
 private
  {Internal Variables}

  {Internal Methods}
 protected
  {Internal Variables}

  {Internal Methods}
 public
  {Public Properties}

  {Public Methods}
  function UsernamePrompt:String; override;
  function PasswordPrompt(const AUsername:String):String; override;

  function CheckUsername(const AUsername:String):LongWord; override;
  function CheckPassword(const AUsername,APassword:String):LongWord; override;
 end;

{==============================================================================}
{var}
 {Authentication specific variables}

{==============================================================================}
{Initialization Functions}
procedure AuthInit;

{==============================================================================}
{Authentication Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Authentication specific variables}
 AuthInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TAuthenticator}
constructor TAuthenticator.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;

 FFlags:=AUTHENTICATOR_FLAG_NONE;
end;

{==============================================================================}

destructor TAuthenticator.Destroy;
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock}
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

procedure TAuthenticator.SetFlags(AFlags:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FFlags:=AFlags;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAuthenticator.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAuthenticator.UsernamePrompt:String;
begin
 {Virtual Base}
 Result:='Username:';
end;

{==============================================================================}

function TAuthenticator.PasswordPrompt(const AUsername:String):String;
begin
 {Virtual Base}
 Result:='Password:';
end;

{==============================================================================}

function TAuthenticator.CheckUsername(const AUsername:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_FOUND;
end;

{==============================================================================}

function TAuthenticator.CheckPassword(const AUsername,APassword:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_OPERATION_FAILED;
end;

{==============================================================================}
{==============================================================================}
{TNullAuthenticator}
function TNullAuthenticator.UsernamePrompt:String;
begin
 {}
 Result:='Username:';
end;

{==============================================================================}

function TNullAuthenticator.PasswordPrompt(const AUsername:String):String;
begin
 {}
 Result:='Password:';
end;

{==============================================================================}

function TNullAuthenticator.CheckUsername(const AUsername:String):LongWord;
begin
 {}
 Result:=ERROR_NOT_FOUND;

 {Check Username}
 if Length(AUsername) = 0 then Exit;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TNullAuthenticator.CheckPassword(const AUsername,APassword:String):LongWord;
begin
 {}
 Result:=ERROR_NOT_FOUND;

 {Check Username}
 if Length(AUsername) = 0 then Exit;

 Result:=ERROR_OPERATION_FAILED;

 {Check Password}
 if Length(APassword) = 0 then Exit;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure AuthInit;
begin
 {}
 {Check Initialized}
 if AuthInitialized then Exit;

 //To Do

 AuthInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Authentication Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 AuthInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

