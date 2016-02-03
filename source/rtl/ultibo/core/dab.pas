{
Ultibo Digital Audio Broadcast (DAB) interface unit.

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


Digital Audio Broadcast (DAB)
=============================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DAB;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Audio,USB;

//To Do

 //Useful references:

 // \linux-rpi-3.12.y\drivers\media\tuners
 
 //Drivers
 // \linux-rpi-3.12.y\drivers\media\tuners\r820t.c

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
 
{==============================================================================}
//const
 {DAB specific constants}

//To Do //A generic DAB unit 
                           
              
{==============================================================================}
//type
 {DAB specific types}
//To Do

{==============================================================================}
var
 {DAB specific variables}
 DABInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure DABInit;

{==============================================================================}
{DAB Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DABInit;
begin
 {}
 {Check Initialized}
 if DABInitialized then Exit;
 
 //To Do
 
 DABInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{DAB Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 DABInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
 
 