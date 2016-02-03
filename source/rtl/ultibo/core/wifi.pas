{
Ultibo WiFi (WPA) interface unit.

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


WiFi
====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit WiFi;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Network,Crypto;

//To Do //See: http://w1.fi/wpa_supplicant/
                      //See: http://w1.fi/wpa_supplicant/devel/
                      //See: http://w1.fi/wpa_supplicant/devel/porting.html
                      //See: http://w1.fi/wpa_supplicant/devel/driver_wrapper.html
                      //See: http://w1.fi/wpa_supplicant/devel/code_structure.html
                      //See: http://w1.fi/cgit
                      
//To Do //In general terms WPA appears to the network stack as a Transport layer
                      //sending and receiving 2 specific packet types:
                      //EAP-over-LAN (EAPOL) $888E and RSN pre-authentication $88C7
                      
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Wifi specific constants}
//To Do

{==============================================================================}
//type
 {Wifi specific types}
//To Do

{==============================================================================}
var
 {Wifi specific variables}
 WifiInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure WifiInit;

{==============================================================================}
{Wifi Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure WifiInit;
begin
 {}
 {Check Initialized}
 if WifiInitialized then Exit;
 
 //To Do
 
 WifiInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Wifi Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 WifiInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
