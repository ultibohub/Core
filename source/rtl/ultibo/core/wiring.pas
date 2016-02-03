{
Ultibo implementation of the Wiring API.

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


Wiring API
==========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Wiring;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Wiring specific constants}

//To Do //A generic Wiring unit 
                           
              
{==============================================================================}
//type
 {Wiring specific types}
//To Do

{==============================================================================}
var
 {Wiring specific variables}
 WiringInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure WiringInit;

{==============================================================================}
{Wiring Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure WiringInit;
begin
 {}
 {Check Initialized}
 if WiringInitialized then Exit;
 
 //To Do
 
 WiringInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Wiring Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 WiringInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
