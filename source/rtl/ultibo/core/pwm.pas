{
Ultibo PWM interface unit.

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


PWM Hosts
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PWM; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

//To Do //A generic PWM driver

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {PWM specific constants}
 
{==============================================================================}
//type
 {PWM specific types}
 
{==============================================================================}
var
 {PWM specific variables}
 PWMInitialized:Boolean;

{==============================================================================}
{Initialization Functions}
procedure PWMInit;
 
{==============================================================================}
{PWM Functions}
 
{==============================================================================}
{PWM Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PWMInit;
begin
 {}
 {Check Initialized}
 if PWMInitialized then Exit;
 
 //To Do
 
 PWMInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{PWM Functions}

{==============================================================================}
{==============================================================================}
{PWM Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 PWMInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
