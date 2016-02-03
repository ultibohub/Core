{
Ultibo Device Firmware Update (DFU) interface unit.

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

 
Device Firmware Update (DFU)
============================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DFU;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {DFU specific constants}
//To Do //A generic DFU unit 
                           
              
{==============================================================================}
//type
 {DFU specific types}
//To Do

{==============================================================================}
var
 {DFU specific variables}
 DFUInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure DFUInit;

{==============================================================================}
{DFU Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DFUInit;
begin
 {}
 {Check Initialized}
 if DFUInitialized then Exit;
 
 //To Do
 
 DFUInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{DFU Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 DFUInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
