{
Ultibo I2S/PCM interface unit.

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


I2S Hosts
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit I2S; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {I2S specific constants}
//To Do //A generic I2S/PCM Audio driver

{==============================================================================}
//type
 {I2S specific types}
 
{==============================================================================}
var
 {I2S specific variables}
 I2SInitialized:Boolean;

{==============================================================================}
{Initialization Functions}
procedure I2SInit;
 
{==============================================================================}
{I2S Functions}
 
{==============================================================================}
{I2S Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure I2SInit;
begin
 {}
 {Check Initialized}
 if I2SInitialized then Exit;
 
 //To Do
 
 I2SInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{I2S Functions}

{==============================================================================}
{==============================================================================}
{I2S Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 I2SInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
