{
Ultibo ARM Mali400 interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A7)

Boards
======

 Banana Pi
 Banana Pro

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


ARM Mali400
===========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Mali400;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Audio,Video;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Mali400 specific constants}

//To Do //A generic Mali400 unit 
                           
              
{==============================================================================}
//type
 {Mali400 specific types}
//To Do

{==============================================================================}
var
 {Mali400 specific variables}
 Mali400Initialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure Mali400Init;

{==============================================================================}
{Mali400 Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure Mali400Init;
begin
 {}
 {Check Initialized}
 if Mali400Initialized then Exit;
 
 //To Do
 
 Mali400Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Mali400 Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 Mali400Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

