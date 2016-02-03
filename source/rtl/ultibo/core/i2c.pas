{
Ultibo I2C interface unit.

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


I2C Hosts
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit I2C; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

//To Do //A generic I2C driver

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {I2C specific constants}

{==============================================================================}
//type
 {I2C specific types}
 
{==============================================================================}
var
 {I2C specific variables}
 I2CInitialized:Boolean;

{==============================================================================}
{Initialization Functions}
procedure I2CInit;
 
{==============================================================================}
{I2C Functions}
 
{==============================================================================}
{I2C Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure I2CInit;
begin
 {}
 {Check Initialized}
 if I2CInitialized then Exit;
 
 //To Do
 
 I2CInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{I2C Functions}

{==============================================================================}
{==============================================================================}
{I2C Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 I2CInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.