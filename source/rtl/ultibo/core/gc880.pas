{
Ultibo Vivante GC880 interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A9)

Boards
======

 Cubox-i2

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


Vivante GC880
=============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GC880;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Audio,Video;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {GC880 specific constants}

//To Do //A generic GC880 unit 
                           
              
{==============================================================================}
//type
 {GC880 specific types}
//To Do

{==============================================================================}
var
 {GC880 specific variables}
 GC880Initialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure GC880Init;

{==============================================================================}
{GC880 Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure GC880Init;
begin
 {}
 {Check Initialized}
 if GC880Initialized then Exit;
 
 //To Do
 
 GC880Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{GC880 Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 GC880Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
