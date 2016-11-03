{
Ultibo NVidia GEForce interface unit

Copyright (C) 2016 - Rob Judd <judd@ob-wan.com>

Arch
====

 ARMv7 (Cortex A9)

Boards
======

 Nexus 7 (2012)

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


NVidia GEForce
==============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GEForce;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Audio,Video;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {GEForce specific constants}

//To Do
                           
              
{==============================================================================}
//type
 {GEForce specific types}
//To Do

{==============================================================================}
var
 {GEForce specific variables}
 GEForceInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure GEForceInit;

{==============================================================================}
{GEForce Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure GEForceInit;
begin
 {}
 {Check Initialized}
 if Mali400Initialized then Exit;
 
 //To Do
 
 GEForceInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{GEForce Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 GEForceInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

