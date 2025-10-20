{
Ultibo Authentication interface unit.

Copyright (C) 2025 - SoftOz Pty Ltd.

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
  System.Classes,
  Core.Crypto,
  Core.UltiboClasses;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Crypto,
  UltiboClasses;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Authentication specific constants}

 {Authenticator Flag constants}
 AUTHENTICATOR_FLAG_NONE     = $00000000;
 AUTHENTICATOR_FLAG_USERNAME = $00000001; {Authenticator supports usernames}
 AUTHENTICATOR_FLAG_PASSWORD = $00000002; {Authenticator supports passwords}
 AUTHENTICATOR_FLAG_TOKEN    = $00000004; {Authenticator supports tokens}
 AUTHENTICATOR_FLAG_KEY      = $00000008; {Authenticator supports key pairs}
 AUTHENTICATOR_FLAG_CACHE    = $00000010; {Authenticator supports token and value caching (eg session caching)}
 AUTHENTICATOR_FLAG_LOCAL    = $00000020; {Authenticator maintains a local list of usernames, passwords or tokens}

 {Authenticator Mode constants}
 AUTHENTICATOR_MODE_UNKNOWN  = 0;
 AUTHENTICATOR_MODE_BASIC    = 1; {Password only authentication}
 AUTHENTICATOR_MODE_USER     = 2; {Username and password authentication}
 AUTHENTICATOR_MODE_SESSION  = 3; {Session authentication}

{==============================================================================}
{type}
 {Authentication specific types}

{==============================================================================}
type
 {Authentication specific methods}
 TAuthenticator = class;

 TCheckPasswordEvent = function(ASource:TAuthenticator;const APassword:String):LongWord of Object;

 TCheckUsernameEvent = function(ASource:TAuthenticator;const AUsername:String):LongWord of Object;
 TCheckUserPasswordEvent = function(ASource:TAuthenticator;const AUsername,APassword:String):LongWord of Object;

 FCheckTokenEvent = function(ASource:TAuthenticator;const AToken,AValue:String):LongWord of Object;

 TCreateTokenEvent = function(ASource:TAuthenticator;const AValue:String;var AToken:String;ATimeout:LongWord):LongWord of Object;
 TDeleteTokenEvent = function(ASource:TAuthenticator;const AToken,AValue:String):LongWord of Object;

 TCheckKeyEvent = function(ASource:TAuthenticator;AData:Pointer;ASize:LongWord):LongWord of Object;

 {Authentication specific classes}
 {The base authenticator class, not intended to be used directly}
 TAuthenticator = class(TObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle;

  {Internal Methods}

 protected
  {Protected Variables}
  FMode:LongWord;
  FFlags:LongWord;

  {Password Only}
  FPasswordPrompt:String;  {The text prompt for password only entry (Default: 'Password: ')}

  FOnCheckPassword:TCheckPasswordEvent;

  {Username and Password}
  FUsernamePrompt:String;      {The text prompt for username entry (Default: 'Username: ')}
  FUserPasswordPrompt:String;  {The text prompt for user password entry (Default: 'Password: ')}

  FCaseSensitiveUsername:Boolean;  {True if usernames should be compared case sensitively (Default: False)}
  FCaseSensitivePassword:Boolean;  {True if passwords should be compared case sensitively (Default: True)}

  FOnCheckUsername:TCheckUsernameEvent;
  FOnCheckUserPassword:TCheckUserPasswordEvent;

  {Token}
  FTokenName:String;       {The parameter name to use for the token in URLs (Default: 'token')}
  FCookieName:String;      {The cookie name to use for the token (Default: 'sessionid')}
  FUseCookie:Boolean;      {True if cookies should be used for token exchange (eg HTTP cookies) or False otherwise, protocol dependent (Default: True)}

  FTokenTimeout:LongWord;  {Timeout value in seconds for token expiry (Default: INFINITE)}

  FReturnURLName:String;   {The parameter name to use for the return URL in request URLs (Default: 'returnurl')}

  FAuthenticateURL:String;    {The URL to redirect to for authentication (Default: <None>)}
  FDeauthenticateURL:String;  {The URL to redirect to after deauthentication (Default: <None>)}

  FOnCheckToken:FCheckTokenEvent;

  FOnCreateToken:TCreateTokenEvent;
  FOnDeleteToken:TDeleteTokenEvent;

  {Key}
  FOnCheckKey:TCheckKeyEvent;

  {Protected Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;

  procedure SetMode(AMode:LongWord); virtual;
  procedure SetFlags(AFlags:LongWord); virtual;

  function FixUsername(const AUsername:String):String; virtual;
  function FixPassword(const APassword:String):String; virtual;

  {Password Only}
  function GetPasswordPrompt:String; virtual;
  procedure SetPasswordPrompt(const APrompt:String); virtual;

  {Username and Password}
  function GetUsernamePrompt:String; virtual;
  procedure SetUsernamePrompt(const APrompt:String); virtual;
  function GetUserPasswordPrompt(const AUsername:String):String; virtual;
  procedure SetUserPasswordPrompt(const AUsername,APrompt:String); virtual;

  {Token}
  function GetTokenName:String; virtual;
  procedure SetTokenName(const AName:String); virtual;
  function GetCookieName:String; virtual;
  procedure SetCookieName(const AName:String); virtual;

  function GetReturnURLName:String; virtual;
  procedure SetReturnURLName(const AName:String); virtual;

  function GetAuthenticateURL:String; virtual;
  procedure SetAuthenticateURL(const AURL:String); virtual;
  function GetDeauthenticateURL:String; virtual;
  procedure SetDeauthenticateURL(const AURL:String); virtual;
 public
  {Public Properties}
  property Mode:LongWord read FMode write SetMode;
  property Flags:LongWord read FFlags write SetFlags;

  {Password Only}
  property PasswordPrompt:String read GetPasswordPrompt write SetPasswordPrompt;

  property OnCheckPassword:TCheckPasswordEvent read FOnCheckPassword write FOnCheckPassword;

  {Username and Password}
  property UsernamePrompt:String read GetUsernamePrompt write SetUsernamePrompt;
  property UserPasswordPrompt[const Username:String]:String read GetUserPasswordPrompt write SetUserPasswordPrompt;

  property CaseSensitiveUsername:Boolean read FCaseSensitiveUsername write FCaseSensitiveUsername;
  property CaseSensitivePassword:Boolean read FCaseSensitivePassword write FCaseSensitivePassword;

  property OnCheckUsername:TCheckUsernameEvent read FOnCheckUsername write FOnCheckUsername;
  property OnCheckUserPassword:TCheckUserPasswordEvent read FOnCheckUserPassword write FOnCheckUserPassword;

  {Token}
  property TokenName:String read GetTokenName write SetTokenName;
  property CookieName:String read GetCookieName write SetCookieName;
  property UseCookie:Boolean read FUseCookie write FUseCookie;

  property TokenTimeout:LongWord read FTokenTimeout write FTokenTimeout;

  property ReturnURLName:String read GetReturnURLName write SetReturnURLName;

  property AuthenticateURL:String read GetAuthenticateURL write SetAuthenticateURL;
  property DeauthenticateURL:String read GetDeauthenticateURL write SetDeauthenticateURL;

  property OnCheckToken:FCheckTokenEvent read FOnCheckToken write FOnCheckToken;

  property OnCreateToken:TCreateTokenEvent read FOnCreateToken write FOnCreateToken;
  property OnDeleteToken:TDeleteTokenEvent read FOnDeleteToken write FOnDeleteToken;

  {Key}
  property OnCheckKey:TCheckKeyEvent read FOnCheckKey write FOnCheckKey;

  {Public Methods}
  {Password Only}
  function CheckPassword(const APassword:String):LongWord; virtual;

  function AddPassword(const APassword:String):LongWord; virtual;
  function DeletePassword(const APassword:String):LongWord; virtual;

  {Username and Password}
  function CheckUsername(const AUsername:String):LongWord; virtual;
  function CheckUserPassword(const AUsername,APassword:String):LongWord; virtual;

  function AddUsername(const AUsername,APassword:String):LongWord; virtual;
  function DeleteUsername(const AUsername,APassword:String):LongWord; virtual;

  {Token}
  function CheckToken(const AToken,AValue:String):LongWord; virtual;

  function CreateToken(const AValue:String;var AToken:String;ATimeout:LongWord):LongWord; virtual;
  function DeleteToken(const AToken,AValue:String):LongWord; virtual;

  {Key}
  function CheckKey(AData:Pointer;ASize:LongWord):LongWord; virtual;
 end;

 {An authenticator module that accepts any non blank password or username and password}
 TNullAuthenticator = class(TAuthenticator)
 public
  {}
  constructor Create;
 protected
  {Protected Variables}

  {Protected Methods}
  procedure SetMode(AMode:LongWord); override;
 public
  {Public Properties}

  {Public Methods}
  {Password Only}
  function CheckPassword(const APassword:String):LongWord; override;

  {Username and Password}
  function CheckUsername(const AUsername:String):LongWord; override;
  function CheckUserPassword(const AUsername,APassword:String):LongWord; override;
 end;

 {An authenticator module supporting password only authentication from an external source}
 TBasicAuthenticator = class(TAuthenticator)
 public
  {}
  constructor Create;
 public
  {Public Properties}
  {Password Only}
  property OnCheckPassword;

  {Public Methods}
  {Password Only}
  function CheckPassword(const APassword:String):LongWord; override;
 end;

 {An authenticator module supporting password only authentication from a local plain text source}
 TLocalBasicAuthenticator = class(TAuthenticator)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FPasswords:TLinkedObjList;

  {Internal Methods}

 public
  {Public Properties}

  {Public Methods}
  {Password Only}
  function CheckPassword(const APassword:String):LongWord; override;

  function AddPassword(const APassword:String):LongWord; override;
  function DeletePassword(const APassword:String):LongWord; override;
 end;

 {An authenticator module supporting username and password authentication from an external source}
 TUserAuthenticator = class(TAuthenticator)
 public
  {}
  constructor Create;
 public
  {Public Properties}
  {Username and Password}
  property OnCheckUsername;
  property OnCheckUserPassword;

  {Public Methods}
  {Username and Password}
  function CheckUsername(const AUsername:String):LongWord; override;
  function CheckUserPassword(const AUsername,APassword:String):LongWord; override;
 end;

 {An authenticator module supporting username and password authentication from a local plain text source}
 TLocalUserAuthenticator = class(TAuthenticator)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FUsernames:TLinkedObjList;

  {Internal Methods}

 public
  {Public Properties}

  {Public Methods}
  {Username and Password}
  function CheckUsername(const AUsername:String):LongWord; override;
  function CheckUserPassword(const AUsername,APassword:String):LongWord; override;

  function AddUsername(const AUsername,APassword:String):LongWord; override;
  function DeleteUsername(const AUsername,APassword:String):LongWord; override;
 end;

 {An authenticator module supporting token authentication and caching from an external source}
 TSessionAuthenticator = class(TAuthenticator)
 public
  {}
  constructor Create;
 public
  {Public Properties}

  {Public Methods}
  {Token}
  function CheckToken(const AToken,AValue:String):LongWord; override;

  function CreateToken(const AValue:String;var AToken:String;ATimeout:LongWord):LongWord; override;
  function DeleteToken(const AToken,AValue:String):LongWord; override;
 end;

 {An authenticator module supporting token authentication and caching from a local plain text source}
 TLocalSessionAuthenticator = class(TAuthenticator)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FSessions:TLinkedObjList;

  {Internal Methods}

 protected
  {Protected Variables}

  {Protected Methods}
  function FindToken(const AValue:String;var AToken:String):LongWord; virtual;
  function GenerateToken(const AValue:String;var AToken:String):LongWord; virtual;
 public
  {Public Properties}

  {Public Methods}
  {Token}
  function CheckToken(const AToken,AValue:String):LongWord; override;

  function CreateToken(const AValue:String;var AToken:String;ATimeout:LongWord):LongWord; override;
  function DeleteToken(const AToken,AValue:String):LongWord; override;
 end;

{==============================================================================}
{var}
 {Authentication specific variables}

{==============================================================================}
{Initialization Functions}
procedure AuthInit;

{==============================================================================}
{Authentication Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
type
 {Authentication specific types}
 TLocalPassword = class(TListObject)
 public
  {Public Variables}
  Password:String;
  PasswordHash:LongWord;
 end;

 TLocalUsername = class(TListObject)
 public
  {Public Variables}
  Username:String;
  Password:String;
  UsernameHash:LongWord;
  PasswordHash:LongWord;
 end;

 TLocalSession = class(TListObject)
 public
  {Public Variables}
  Token:String;
  Value:String;
  TokenHash:LongWord;
  ValueHash:LongWord;

  ExpiryTime:Int64;
 end;

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

 FMode:=AUTHENTICATOR_MODE_UNKNOWN;
 FFlags:=AUTHENTICATOR_FLAG_NONE;

 FPasswordPrompt:='Password: ';

 FUsernamePrompt:='Username: ';
 FUserPasswordPrompt:='Password: ';

 FCaseSensitiveUsername:=False;
 FCaseSensitivePassword:=True;

 FTokenName:='token';
 FCookieName:='sessionid';
 FUseCookie:=True;

 FTokenTimeout:=INFINITE;

 FReturnURLName:='returnurl';
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

procedure TAuthenticator.SetMode(AMode:LongWord);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 {FMode:=AMode;} {Don't update in base method}

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetFlags(AFlags:LongWord);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 {FFlags:=AFlags;} {Don't update in base method}

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.FixUsername(const AUsername:String):String;
begin
 {}
 if CaseSensitiveUsername then Result:=AUsername else Result:=Uppercase(AUsername);
end;

{==============================================================================}

function TAuthenticator.FixPassword(const APassword:String):String;
begin
 {}
 if CaseSensitivePassword then Result:=APassword else Result:=Uppercase(APassword);
end;

{==============================================================================}

function TAuthenticator.GetPasswordPrompt:String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FPasswordPrompt;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetPasswordPrompt(const APrompt:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FPasswordPrompt:=APrompt;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.GetUsernamePrompt:String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FUsernamePrompt;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetUsernamePrompt(const APrompt:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FUsernamePrompt:=APrompt;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.GetUserPasswordPrompt(const AUsername:String):String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FUserPasswordPrompt;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetUserPasswordPrompt(const AUsername,APrompt:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FUserPasswordPrompt:=APrompt;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.GetTokenName:String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FTokenName;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetTokenName(const AName:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FTokenName:=AName;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.GetCookieName:String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FCookieName;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetCookieName(const AName:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FCookieName:=AName;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.GetReturnURLName:String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FReturnURLName;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetReturnURLName(const AName:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FReturnURLName:=AName;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.GetAuthenticateURL:String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FAuthenticateURL;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetAuthenticateURL(const AURL:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FAuthenticateURL:=AURL;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.GetDeauthenticateURL:String;
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 Result:=FDeauthenticateURL;

 ReleaseLock;
end;

{==============================================================================}

procedure TAuthenticator.SetDeauthenticateURL(const AURL:String);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 FDeauthenticateURL:=AURL;

 ReleaseLock;
end;

{==============================================================================}

function TAuthenticator.CheckPassword(const APassword:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.AddPassword(const APassword:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.DeletePassword(const APassword:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.CheckUsername(const AUsername:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.CheckUserPassword(const AUsername,APassword:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.AddUsername(const AUsername,APassword:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.DeleteUsername(const AUsername,APassword:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.CheckToken(const AToken,AValue:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.CreateToken(const AValue:String;var AToken:String;ATimeout:LongWord):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.DeleteToken(const AToken,AValue:String):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function TAuthenticator.CheckKey(AData:Pointer;ASize:LongWord):LongWord;
begin
 {Virtual Base}
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}
{==============================================================================}
{TNullAuthenticator}
constructor TNullAuthenticator.Create;
begin
 {}
 inherited Create;

 FMode:=AUTHENTICATOR_MODE_USER;
 FFlags:=AUTHENTICATOR_FLAG_USERNAME or AUTHENTICATOR_FLAG_PASSWORD;

 FPasswordPrompt:='';

 FUsernamePrompt:='Username: ';
 FUserPasswordPrompt:='Password: ';
end;

{==============================================================================}

procedure TNullAuthenticator.SetMode(AMode:LongWord);
begin
 {}
 if AMode = AUTHENTICATOR_MODE_BASIC then
  begin
   FMode:=AMode;

   FFlags:=AUTHENTICATOR_FLAG_PASSWORD;

   FPasswordPrompt:='Password: ';
   FUsernamePrompt:='';
   FUserPasswordPrompt:='';
  end
 else if AMode = AUTHENTICATOR_MODE_USER then
  begin
   FMode:=AMode;

   FFlags:=AUTHENTICATOR_FLAG_USERNAME or AUTHENTICATOR_FLAG_PASSWORD;

   FPasswordPrompt:='';
   FUsernamePrompt:='Username: ';
   FUserPasswordPrompt:='Password: ';
  end;
end;

{==============================================================================}

function TNullAuthenticator.CheckPassword(const APassword:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_FUNCTION;

 {Check Mode}
 if Mode <> AUTHENTICATOR_MODE_BASIC then Exit;

 Result:=ERROR_INVALID_PASSWORD;

 {Check Password}
 if Length(APassword) = 0 then Exit;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TNullAuthenticator.CheckUsername(const AUsername:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_FUNCTION;

 {Check Mode}
 if Mode <> AUTHENTICATOR_MODE_USER then Exit;

 Result:=ERROR_NOT_FOUND;

 {Check Username}
 if Length(AUsername) = 0 then Exit;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TNullAuthenticator.CheckUserPassword(const AUsername,APassword:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_FUNCTION;

 {Check Mode}
 if Mode <> AUTHENTICATOR_MODE_USER then Exit;

 Result:=ERROR_NOT_FOUND;

 {Check Username}
 if Length(AUsername) = 0 then Exit;

 Result:=ERROR_INVALID_PASSWORD;

 {Check Password}
 if Length(APassword) = 0 then Exit;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{TBasicAuthenticator}
constructor TBasicAuthenticator.Create;
begin
 {}
 inherited Create;

 FMode:=AUTHENTICATOR_MODE_BASIC;
 FFlags:=AUTHENTICATOR_FLAG_PASSWORD;

 FUsernamePrompt:='';
 FUserPasswordPrompt:='';
end;

{==============================================================================}

function TBasicAuthenticator.CheckPassword(const APassword:String):LongWord;
begin
 {}
 Result:=ERROR_NOT_SUPPORTED;

 if Assigned(FOnCheckPassword) then
  begin
   Result:=FOnCheckPassword(Self,APassword);
  end;
end;

{==============================================================================}
{==============================================================================}
{TLocalBasicAuthenticator}
constructor TLocalBasicAuthenticator.Create;
begin
 {}
 inherited Create;

 FMode:=AUTHENTICATOR_MODE_BASIC;
 FFlags:=AUTHENTICATOR_FLAG_PASSWORD or AUTHENTICATOR_FLAG_LOCAL;

 FUsernamePrompt:='';
 FUserPasswordPrompt:='';

 FPasswords:=TLinkedObjList.Create;
end;

{==============================================================================}

destructor TLocalBasicAuthenticator.Destroy;
begin
 {}
 AcquireLock;
 FPasswords.Free;
 {ReleaseLock;} {Can destroy Critical Section while holding lock}
 inherited Destroy;
end;

{==============================================================================}

function TLocalBasicAuthenticator.CheckPassword(const APassword:String):LongWord;
var
 Hash:LongWord;
 Password:TLocalPassword;
begin
 {}
 Result:=ERROR_INVALID_PASSWORD;

 {Get Hash}
 Hash:=StringHash(APassword);

 AcquireLock;
 try
  {Find Password}
  Password:=TLocalPassword(FPasswords.First);
  while Password <> nil do
   begin
    if (Password.PasswordHash = Hash) and (FixPassword(Password.Password) = FixPassword(APassword)) then
     begin
      Result:=ERROR_SUCCESS;
      Exit;
     end;

    {Get Next}
    Password:=TLocalPassword(Password.Next);
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TLocalBasicAuthenticator.AddPassword(const APassword:String):LongWord;
var
 Password:TLocalPassword;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Check Existing Password}
 if CheckPassword(APassword) <> ERROR_SUCCESS then
  begin
   AcquireLock;

   {Create Password}
   Password:=TLocalPassword.Create;
   Password.Password:=APassword;
   Password.PasswordHash:=StringHash(APassword);

   {Add to List}
   FPasswords.Add(Password);

   Result:=ERROR_SUCCESS;

   ReleaseLock;
  end;
end;

{==============================================================================}

function TLocalBasicAuthenticator.DeletePassword(const APassword:String):LongWord;
var
 Hash:LongWord;
 Password:TLocalPassword;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Get Hash}
 Hash:=StringHash(APassword);

 AcquireLock;
 try
  {Find Password}
  Password:=TLocalPassword(FPasswords.First);
  while Password <> nil do
   begin
    if (Password.PasswordHash = Hash) and (Password.Password = APassword) then
     begin
      {Remove from List}
      FPasswords.Remove(Password);

      {Destroy Password}
      Password.Free;

      Result:=ERROR_SUCCESS;
      Exit;
     end;

    {Get Next}
    Password:=TLocalPassword(Password.Next);
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TUserAuthenticator}
constructor TUserAuthenticator.Create;
begin
 {}
 inherited Create;

 FMode:=AUTHENTICATOR_MODE_USER;
 FFlags:=AUTHENTICATOR_FLAG_USERNAME or AUTHENTICATOR_FLAG_PASSWORD;

 FPasswordPrompt:='';
end;

{==============================================================================}

function TUserAuthenticator.CheckUsername(const AUsername:String):LongWord;
begin
 {}
 Result:=ERROR_NOT_SUPPORTED;

 if Assigned(FOnCheckUsername) then
  begin
   Result:=FOnCheckUsername(Self,AUsername);
  end;
end;

{==============================================================================}

function TUserAuthenticator.CheckUserPassword(const AUsername,APassword:String):LongWord;
begin
 {}
 Result:=ERROR_NOT_SUPPORTED;

 if Assigned(FOnCheckUserPassword) then
  begin
   Result:=FOnCheckUserPassword(Self,AUsername,APassword);
  end;
end;

{==============================================================================}
{==============================================================================}
{TLocalUserAuthenticator}
constructor TLocalUserAuthenticator.Create;
begin
 {}
 inherited Create;

 FMode:=AUTHENTICATOR_MODE_USER;
 FFlags:=AUTHENTICATOR_FLAG_USERNAME or AUTHENTICATOR_FLAG_PASSWORD or AUTHENTICATOR_FLAG_LOCAL;

 FPasswordPrompt:='';

 FUsernames:=TLinkedObjList.Create;
end;

{==============================================================================}

destructor TLocalUserAuthenticator.Destroy;
begin
 {}
 AcquireLock;
 FUsernames.Free;
 {ReleaseLock;} {Can destroy Critical Section while holding lock}
 inherited Destroy;
end;

{==============================================================================}

function TLocalUserAuthenticator.CheckUsername(const AUsername:String):LongWord;
var
 Hash:LongWord;
 Username:TLocalUsername;
begin
 {}
 Result:=ERROR_NOT_FOUND;

 {Get Hash}
 Hash:=StringHash(AUsername);

 AcquireLock;
 try
  {Find Username}
  Username:=TLocalUsername(FUsernames.First);
  while Username <> nil do
   begin
    if (Username.UsernameHash = Hash) and (FixUsername(Username.Username) = FixUsername(AUsername)) then
     begin
      Result:=ERROR_SUCCESS;
      Exit;
     end;

    {Get Next}
    Username:=TLocalUsername(Username.Next);
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TLocalUserAuthenticator.CheckUserPassword(const AUsername,APassword:String):LongWord;
var
 UsernameHash:LongWord;
 PasswordHash:LongWord;
 Username:TLocalUsername;
begin
 {}
 Result:=ERROR_INVALID_PASSWORD;

 {Get Hash}
 UsernameHash:=StringHash(AUsername);
 PasswordHash:=StringHash(APassword);

 AcquireLock;
 try
  {Find Username and Password}
  Username:=TLocalUsername(FUsernames.First);
  while Username <> nil do
   begin
    if (Username.UsernameHash = UsernameHash) and (FixUsername(Username.Username) = FixUsername(AUsername)) then
     begin
      if (Username.PasswordHash = PasswordHash) and (FixPassword(Username.Password) = FixPassword(APassword)) then
       begin
        Result:=ERROR_SUCCESS;
        Exit;
       end;
     end;

    {Get Next}
    Username:=TLocalUsername(Username.Next);
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TLocalUserAuthenticator.AddUsername(const AUsername,APassword:String):LongWord;
var
 Username:TLocalUsername;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Check Existing Username}
 if CheckUsername(AUsername) <> ERROR_SUCCESS then
  begin
   {Check Existing Password}
   if CheckUserPassword(AUsername,APassword) <> ERROR_SUCCESS then
    begin
     AcquireLock;

     {Create Username}
     Username:=TLocalUsername.Create;
     Username.Username:=AUsername;
     Username.Password:=APassword;
     Username.UsernameHash:=StringHash(AUsername);
     Username.PasswordHash:=StringHash(APassword);

     {Add to List}
     FUsernames.Add(Username);

     Result:=ERROR_SUCCESS;

     ReleaseLock;
    end;
  end;
end;

{==============================================================================}

function TLocalUserAuthenticator.DeleteUsername(const AUsername,APassword:String):LongWord;
var
 UsernameHash:LongWord;
 PasswordHash:LongWord;
 Username:TLocalUsername;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Get Hash}
 UsernameHash:=StringHash(AUsername);
 PasswordHash:=StringHash(APassword);

 AcquireLock;
 try
  {Find Username and Password}
  Username:=TLocalUsername(FUsernames.First);
  while Username <> nil do
   begin
    if (Username.UsernameHash = UsernameHash) and (Username.Username = AUsername) then
     begin
      if (Username.PasswordHash = PasswordHash) and (Username.Password = APassword) then
       begin
        {Remove from List}
        FUsernames.Remove(Username);

        {Destroy Username}
        Username.Free;

        Result:=ERROR_SUCCESS;
        Exit;
       end;
     end;

    {Get Next}
    Username:=TLocalUsername(Username.Next);
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TSessionAuthenticator}
constructor TSessionAuthenticator.Create;
begin
 {}
 inherited Create;

 FMode:=AUTHENTICATOR_MODE_SESSION;
 FFlags:=AUTHENTICATOR_FLAG_TOKEN or AUTHENTICATOR_FLAG_CACHE;

 FPasswordPrompt:='';

 FUsernamePrompt:='';
 FUserPasswordPrompt:='';
end;

{==============================================================================}

function TSessionAuthenticator.CheckToken(const AToken,AValue:String):LongWord;
begin
 {}
 Result:=ERROR_NOT_SUPPORTED;

 if Assigned(FOnCheckToken) then
  begin
   Result:=FOnCheckToken(Self,AToken,AValue);
  end;
end;

{==============================================================================}

function TSessionAuthenticator.CreateToken(const AValue:String;var AToken:String;ATimeout:LongWord):LongWord;
begin
 {}
 Result:=ERROR_NOT_SUPPORTED;

 if Assigned(FOnCreateToken) then
  begin
   Result:=FOnCreateToken(Self,AValue,AToken,ATimeout);
  end;
end;

{==============================================================================}

function TSessionAuthenticator.DeleteToken(const AToken,AValue:String):LongWord;
begin
 {}
 Result:=ERROR_NOT_SUPPORTED;

 if Assigned(FOnDeleteToken) then
  begin
   Result:=FOnDeleteToken(Self,AToken,AValue);
  end;
end;

{==============================================================================}
{==============================================================================}
{TLocalSessionAuthenticator}
constructor TLocalSessionAuthenticator.Create;
begin
 {}
 inherited Create;

 FMode:=AUTHENTICATOR_MODE_SESSION;
 FFlags:=AUTHENTICATOR_FLAG_TOKEN or AUTHENTICATOR_FLAG_CACHE or AUTHENTICATOR_FLAG_LOCAL;

 FPasswordPrompt:='';

 FUsernamePrompt:='';
 FUserPasswordPrompt:='';

 FSessions:=TLinkedObjList.Create;
end;

{==============================================================================}

destructor TLocalSessionAuthenticator.Destroy;
begin
 {}
 AcquireLock;
 FSessions.Free;
 {ReleaseLock;} {Can destroy Critical Section while holding lock}
 inherited Destroy;
end;

{==============================================================================}

function TLocalSessionAuthenticator.FindToken(const AValue:String;var AToken:String):LongWord;
var
 Value:String;
 ValueHash:LongWord;
 Next:TLocalSession;
 Session:TLocalSession;
begin
 {}
 Result:=ERROR_NOT_FOUND;

 {Set Default}
 AToken:='';

 {Get Hash}
 ValueHash:=StringHash(AValue);

 {Get String}
 Value:=Uppercase(AValue);

 AcquireLock;
 try
  {Find Session}
  Session:=TLocalSession(FSessions.First);
  while Session <> nil do
   begin
    if (Session.ValueHash = ValueHash) and (Session.Value = Value) then
     begin
      {Return Token}
      AToken:=Lowercase(Session.Token);

      Result:=ERROR_SUCCESS;
      Exit;
     end;

    {Get Next}
    Session:=TLocalSession(Session.Next);
   end;
 finally
  ReleaseLock;
 end;

end;

{==============================================================================}

function TLocalSessionAuthenticator.GenerateToken(const AValue:String;var AToken:String):LongWord;
var
 Value:String;
 Digest:TSHA1Digest;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Set Default}
 AToken:='';

 if Length(AValue) = 0 then Exit;

 {Get Value}
 Randomize;
 Value:=IntToStr(ClockGetTime) + AValue + IntToStr(Random($7FFFFFFFFFFFFFFF));

 {Concatenate Value}
 while Length(Value) < 512 do
  begin
   Value:=Value + IntToStr(ClockGetTime) + AValue + IntToStr(Random($7FFFFFFFFFFFFFFF));
  end;

 {Create SHA1 Digest}
 if not SHA1DigestString(Value,@Digest) then Exit;

 {Create Token}
 AToken:=SHA1DigestToString(@Digest);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TLocalSessionAuthenticator.CheckToken(const AToken,AValue:String):LongWord;
var
 Token:String;
 Value:String;
 TokenHash:LongWord;
 ValueHash:LongWord;
 Next:TLocalSession;
 Session:TLocalSession;
begin
 {}
 Result:=ERROR_NOT_FOUND;

 {Get Hash}
 TokenHash:=StringHash(AToken);
 ValueHash:=StringHash(AValue);

 {Get Strings}
 Token:=Uppercase(AToken);
 Value:=Uppercase(AValue);

 AcquireLock;
 try
  {Find Session}
  Session:=TLocalSession(FSessions.First);
  while Session <> nil do
   begin
    {Check Expiry}
    if (Session.ExpiryTime = 0) or (ClockGetTime < Session.ExpiryTime) then
     begin
      if (Session.TokenHash = TokenHash) and (Session.Token = Token) then
       begin
        if (Session.ValueHash = ValueHash) and (Session.Value = Value) then
         begin
          Result:=ERROR_SUCCESS;
          Exit;
         end;
       end;

      {Get Next}
      Session:=TLocalSession(Session.Next);
     end
    else
     begin
      {Remove Expired Session}
      {Get Next}
      Next:=TLocalSession(Session.Next);

      {Remove from List}
      FSessions.Remove(Session);

      {Destroy Session}
      Session.Free;

      {Get Next}
      Session:=Next;
     end;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TLocalSessionAuthenticator.CreateToken(const AValue:String;var AToken:String;ATimeout:LongWord):LongWord;
var
 Timeout:Int64;
 Session:TLocalSession;
begin
 {}
 {Find Token}
 Result:=FindToken(AValue,AToken);
 if Result = ERROR_SUCCESS then Exit;

 {Generate Token}
 Result:=GenerateToken(AValue,AToken);
 if Result <> ERROR_SUCCESS then Exit;

 {Check Timeout}
 Timeout:=ATimeout; {Avoid 32 bit overflow}
 if Timeout = 0 then Timeout:=TokenTimeout;
 if Timeout = 0 then Timeout:=INFINITE;

 AcquireLock;

 {Create Session}
 Session:=TLocalSession.Create;
 Session.Token:=Uppercase(AToken);
 Session.Value:=Uppercase(AValue);
 Session.TokenHash:=StringHash(AToken);
 Session.ValueHash:=StringHash(AValue);
 Session.ExpiryTime:=0;
 if Timeout <> INFINITE then Session.ExpiryTime:=ClockGetTime + (Timeout * TIME_TICKS_PER_SECOND);

 {Add to List}
 FSessions.Add(Session);

 Result:=ERROR_SUCCESS;

 ReleaseLock;
end;

{==============================================================================}

function TLocalSessionAuthenticator.DeleteToken(const AToken,AValue:String):LongWord;
var
 Token:String;
 Value:String;
 TokenHash:LongWord;
 ValueHash:LongWord;
 Session:TLocalSession;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Get Hash}
 TokenHash:=StringHash(AToken);
 ValueHash:=StringHash(AValue);

 {Get Strings}
 Token:=Uppercase(AToken);
 Value:=Uppercase(AValue);

 AcquireLock;
 try
  {Find Session}
  Session:=TLocalSession(FSessions.First);
  while Session <> nil do
   begin
    if (Session.TokenHash = TokenHash) and (Session.Token = Token) then
     begin
      if (Session.ValueHash = ValueHash) and (Session.Value = Value) then
       begin
        {Remove from List}
        FSessions.Remove(Session);

        {Destroy Session}
        Session.Free;

        Result:=ERROR_SUCCESS;
        Exit;
       end;
     end;

    {Get Next}
    Session:=TLocalSession(Session.Next);
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure AuthInit;
begin
 {}
 {Check Initialized}
 if AuthInitialized then Exit;

 {Check Environment Variables}
 {Nothing}

 AuthInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Authentication Functions}

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

