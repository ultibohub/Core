{
Ultibo SSL/TLS interface unit.

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

  \wpa_supplicant-2.5\src\tls


References
==========

 Based on information from sources of OpenSSL, WPA supplicant, the Linux kernel and others

SSL
===


TLS
===


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SSL;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

//To Do //SSL/TLS functionality
        //See: \source\packages\fcl-net\src\sslsockets.pp
        //For starting point including FPC interface for OpenSSL etc

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {SSL specific constants}

{==============================================================================}
//type
 {SSL specific types}

{==============================================================================}
var
 {SSL specific variables}
 SSLInitialized:Boolean;

{==============================================================================}
{Initialization Functions}
procedure SSLInit;

{==============================================================================}
{SSL Functions}

{==============================================================================}
{SSL Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SSLInit;
begin
 {}
 {Check Initialized}
 if SSLInitialized then Exit;

 //To Do

 SSLInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{SSL Functions}

{==============================================================================}
{==============================================================================}
{SSL Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 SSLInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

