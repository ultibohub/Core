{
Ultibo Serial interface unit.

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


Serial Devices
==============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Serial;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Serial specific constants}
//To Do

{==============================================================================}
//type
 {Serial specific types}
//To Do

{==============================================================================}
var
 {Serial specific variables}
 SerialInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure SerialInit;

{==============================================================================}
{Serial Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SerialInit;
begin
 {}
 {Check Initialized}
 if SerialInitialized then Exit;
 
 //To Do
 
 SerialInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Serial Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 SerialInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
