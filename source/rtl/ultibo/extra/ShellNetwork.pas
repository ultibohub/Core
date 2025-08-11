{
Ultibo Network Shell extension unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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



Shell Network
=============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit ShellNetwork;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.Network,
  System.SysUtils,
  System.Classes,
  Core.Ultibo,
  Core.UltiboClasses,
  Core.UltiboUtils,
  Core.Shell,
  Core.Winsock2,
  Core.Iphlpapi,
  Core.Services;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Network,
  SysUtils,
  Classes,
  Ultibo,
  UltiboClasses,
  UltiboUtils,
  Shell,
  Winsock2,
  Iphlpapi,
  Services;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Shell Network specific constants}

 {Shell Network Command constants}
 SHELL_NET_COMMAND_NET      = 'NET';
 SHELL_NET_COMMAND_PING     = 'PING';
 SHELL_NET_COMMAND_IPCONFIG = 'IPCONFIG';
 SHELL_NET_COMMAND_IFCONFIG = 'IFCONFIG';
 SHELL_NET_COMMAND_IFUP     = 'IFUP';
 SHELL_NET_COMMAND_IFDOWN   = 'IFDOWN';

 {Shell Network Action constants}
 SHELL_NET_ACTION_LIST    = 'LIST';
 SHELL_NET_ACTION_SHOW    = 'SHOW';

 SHELL_NET_ACTION_START   = 'START';   {Adapter only}
 SHELL_NET_ACTION_STOP    = 'STOP';    {Adapter only}

 SHELL_NET_ACTION_ADD     = 'ADD';     {ARP/IP/ROUTE only}
 SHELL_NET_ACTION_DELETE  = 'DELETE';  {ARP/IP/ROUTE only}

 SHELL_NET_ACTION_FLUSH   = 'FLUSH';   {ARP only}
 SHELL_NET_ACTION_SEND    = 'SEND';    {ARP only}

 SHELL_NET_ACTION_RELEASE = 'RELEASE'; {IP only}
 SHELL_NET_ACTION_RENEW   = 'RENEW';   {IP only}
 SHELL_NET_ACTION_CONFIG  = 'CONFIG';  {IP only}

 {Shell Network Item constants}
 SHELL_NET_ITEM_ALL       = 'ALL';
 SHELL_NET_ITEM_ARP       = 'ARP';
 SHELL_NET_ITEM_IP        = 'IP';
 SHELL_NET_ITEM_ROUTE     = 'ROUTE';
 SHELL_NET_ITEM_ADAPTER   = 'ADAPTER';
 SHELL_NET_ITEM_TRANSPORT = 'TRANSPORT';
 SHELL_NET_ITEM_PROTOCOL  = 'PROTOCOL';

{==============================================================================}
{type}
 {Shell Network specific types}

{==============================================================================}
type
 {Shell Network specific classes}
 TShellPing = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}

  {Internal Methods}
  function PingErrorToDescription(AErrorCode:LongInt):String;

  procedure PingSuccess(AShell:TShell;ASession:TShellSession;AClient:TPingClient);
  procedure PingFailure(AShell:TShell;ASession:TShellSession;AClient:TPingClient);
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
 {Shell Network specific variables}

{==============================================================================}
{Initialization Functions}
procedure ShellNetworkInit;

{==============================================================================}
{Shell Network Functions}

{==============================================================================}
{Shell Network Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Shell Network specific variables}
 ShellNetworkInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TShellPing}
constructor TShellPing.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_NET_COMMAND_PING;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellPing.PingErrorToDescription(AErrorCode:LongInt):String;
begin
 {}
 Result:='Unknown error occurred';

 case AErrorCode of
  WSAENETUNREACH:Result:='Destination network unreachable';
  WSAEHOSTUNREACH:Result:='Destination host unreachable';
  WSAETIMEDOUT:Result:='Request timed out';
  WSAEOPNOTSUPP:Result:='Operation not supported';
  WSAEINVAL:Result:='Invalid response received';
 end;

 {$IFDEF SHELL_DEBUG}
 Result:=Result + ' (' + Winsock2ErrorToString(AErrorCode) + ')';
 {$ENDIF}
end;

{==============================================================================}

procedure TShellPing.PingSuccess(AShell:TShell;ASession:TShellSession;AClient:TPingClient);
begin
 {}
 {Check Shell}
 if AShell = nil then Exit;

 {Check Client}
 if AClient = nil then Exit;

 AShell.DoOutput(ASession,'Reply from ' + AClient.ReplyAddress + ': bytes=' + IntToStr(AClient.Size) + ' time=' + IntToStr(AClient.LastTime) + 'ms');
end;

{==============================================================================}

procedure TShellPing.PingFailure(AShell:TShell;ASession:TShellSession;AClient:TPingClient);
var
 WorkBuffer:String;
begin
 {}
 {Check Shell}
 if AShell = nil then Exit;

 {Check Client}
 if AClient = nil then Exit;

 {Check Address}
 if Length(AClient.ReplyAddress) > 0 then
  begin
   WorkBuffer:='Reply from ' + AClient.ReplyAddress + ': ';
  end
 else
  begin
   WorkBuffer:='';
  end;

 {Check Error}
 WorkBuffer:=WorkBuffer + PingErrorToDescription(AClient.ErrorCode) + '.';

 AShell.DoOutput(ASession,WorkBuffer);
end;

{==============================================================================}

function TShellPing.DoHelp(AShell:TShell;ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;

 {Check Shell}
 if AShell = nil then Exit;

 {Do Help}
 AShell.DoOutput(ASession,'Send ICMP Echo (Ping) requests to a specified address');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' PING [/SIZE=bytes] [/COUNT=count] [/TIMEOUT=ms] [/TTL=hops] [/DF] <Name or address>');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  /SIZE     (or /S)   Number of bytes to be sent');
 AShell.DoOutput(ASession,'  /COUNT    (or /C)   Number of echo requests to send');
 AShell.DoOutput(ASession,'  /TIMEOUT  (or /T)   Timeout to wait for each reply (Milliseconds)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  /TTL                The Time To Live value for each request');
 AShell.DoOutput(ASession,'  /DF                 Set the don''t fragment flag');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'   Examples:');
 AShell.DoOutput(ASession,'    ' + Name + ' 127.0.0.1');
 AShell.DoOutput(ASession,'    ' + Name + ' www.google.com');
 AShell.DoOutput(ASession,'    ' + Name + ' /SIZE=64 /COUNT=4 192.168.0.1');
 AShell.DoOutput(ASession,'    ' + Name + ' /S=256 /C=20 /T=5000 192.168.0.1');
 AShell.DoOutput(ASession,'');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellPing.DoInfo(AShell:TShell;ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;

 {Check Shell}
 if AShell = nil then Exit;

 {Do Info}
 Result:=AShell.DoOutput(ASession,'Send ICMP Echo (Ping) requests to a specified address');
end;

{==============================================================================}

function TShellPing.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;
var
 Host:String;
 Address:String;
 Size:LongWord;
 Count:LongWord;
 Timeout:LongWord;
 TimeToLive:Byte;
 NoFragment:Boolean;
 PingClient:TPingClient;
begin
 {}
 Result:=False;

 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;

 {Check Parameter Count}
 if AParameters.Count = 0 then
  begin
   Result:=DoHelp(AShell,ASession);
   Exit;
  end;

 {Get Size}
 Size:=0;
 if AShell.ParameterExists('SIZE',AParameters) then
  begin
   Size:=StrToIntDef(AShell.ParameterValue('SIZE',AParameters),0);
  end
 else if AShell.ParameterExists('S',AParameters) then
  begin
   Size:=StrToIntDef(AShell.ParameterValue('S',AParameters),0);
  end;

 {Get Count}
 Count:=0;
 if AShell.ParameterExists('COUNT',AParameters) then
  begin
   Count:=StrToIntDef(AShell.ParameterValue('COUNT',AParameters),0);
  end
 else if AShell.ParameterExists('C',AParameters) then
  begin
   Count:=StrToIntDef(AShell.ParameterValue('C',AParameters),0);
  end;

 {Get Timeout}
 Timeout:=0;
 if AShell.ParameterExists('TIMEOUT',AParameters) then
  begin
   Timeout:=StrToIntDef(AShell.ParameterValue('TIMEOUT',AParameters),0);
  end
 else if AShell.ParameterExists('T',AParameters) then
  begin
   Timeout:=StrToIntDef(AShell.ParameterValue('T',AParameters),0);
  end;

 {Get TTL}
 TimeToLive:=0;
 if AShell.ParameterExists('TTL',AParameters) then
  begin
   TimeToLive:=StrToIntDef(AShell.ParameterValue('TTL',AParameters),0);
  end;

 {Get DF}
 NoFragment:=False;
 if AShell.ParameterExists('DF',AParameters) then
  begin
   NoFragment:=True;
  end;

 {Get Host}
 Host:=AShell.ParameterIndex(AParameters.Count - 1,AParameters);

 {Check Host}
 if (Pos('=',Host) <> 0) or (Pos('/',Host) <> 0) or (Pos('-',Host) <> 0) then
  begin
   AShell.DoOutput(ASession,'Target name or address not supplied');
   AShell.DoOutput(ASession,'');
  end
 else
  begin
   {Create Client}
   PingClient:=TPingClient.Create;
   try
    {Get Address}
    Address:=PingClient.ResolveHost(Host);
    if Length(Address) = 0 then
     begin
      AShell.DoOutput(ASession,'Could not resolve name ' + Host + ' to a valid address');
      AShell.DoOutput(ASession,'');
     end
    else
     begin
      {Set Parameters}
      if Size > 0 then PingClient.Size:=Size;
      if Count > 0 then PingClient.Count:=Count;
      if Timeout > 0 then PingClient.Timeout:=Timeout;
      if TimeToLive > 0 then PingClient.TimeToLive:=TimeToLive;
      if NoFragment then PingClient.NoFragment:=True;

      AShell.DoOutput(ASession,'Pinging ' + Host + ' [' + Address + '] with ' + IntToStr(PingClient.Size) + ' bytes of data:');

      {Send First Ping}
      if PingClient.FirstPing(Host) then
       begin
        PingSuccess(AShell,ASession,PingClient);
       end
      else
       begin
        PingFailure(AShell,ASession,PingClient);
       end;

      {Send Next Ping}
      while PingClient.LastCount < PingClient.Count do
       begin
        Sleep(1000);

        if PingClient.NextPing then
         begin
          PingSuccess(AShell,ASession,PingClient);
         end
        else
         begin
          PingFailure(AShell,ASession,PingClient);
         end;
       end;

      AShell.DoOutput(ASession,'');

      {Add Statistics}
      AShell.DoOutput(ASession,'Statistics:');
      AShell.DoOutput(ASession,' Sent: ' + IntToStr(PingClient.SendCount) + ' Received: ' + IntToStr(PingClient.ReceiveCount) + ' Lost: ' + IntToStr(PingClient.LostCount));
      AShell.DoOutput(ASession,'');
     end;
   finally
    {Free Client}
    PingClient.Free;
   end;
  end;

 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ShellNetworkInit;
var
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if ShellNetworkInitialized then Exit;

 {Register Network Commands}
 ShellRegisterCommand(TShellPing.Create);

 ShellNetworkInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Shell Network Functions}

{==============================================================================}
{==============================================================================}
{Shell Network Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 ShellNetworkInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
