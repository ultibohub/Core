{
Ultibo Vivante GC2000 interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A9)

Boards
======

 Cubox-i2Ex
 Cubox-i4Pro
 Hummingboard
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


Vivante GC2000
==============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GC2000;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Audio,Video;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {GC2000 specific constants}

//To Do //A generic GC2000 unit 
                           
              
{==============================================================================}
//type
 {GC2000 specific types}
//To Do

{==============================================================================}
var
 {GC2000 specific variables}
 GC2000Initialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure GC2000Init;

{==============================================================================}
{GC2000 Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure GC2000Init;
begin
 {}
 {Check Initialized}
 if GC2000Initialized then Exit;
 
 //To Do
 
 GC2000Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{GC2000 Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 GC2000Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

