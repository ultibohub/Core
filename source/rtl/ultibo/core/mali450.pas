{
Ultibo ARM Mali450 interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A7)

Boards
======


Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


ARM Mali450
===========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Mali450;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Audio,Video;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Mali450 specific constants}

//To Do //A generic Mali450 unit 
                           
              
{==============================================================================}
//type
 {Mali450 specific types}
//To Do

{==============================================================================}
var
 {Mali450 specific variables}
 Mali450Initialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure Mali450Init;

{==============================================================================}
{Mali450 Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure Mali450Init;
begin
 {}
 {Check Initialized}
 if Mali450Initialized then Exit;
 
 //To Do
 
 Mali450Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Mali450 Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 Mali450Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

