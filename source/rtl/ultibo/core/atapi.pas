{
Ultibo ATA/ATAPI interface unit.

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


ATA/ATAPI
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ATAPI;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Storage;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {ATAPI specific constants}

//To Do //A generic ATAPI unit


{==============================================================================}
//type
 {ATAPI specific types}
//To Do

{==============================================================================}
var
 {ATAPI specific variables}
 ATAPIInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure ATAPIInit;

{==============================================================================}
{ATAPI Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ATAPIInit;
begin
 {}
 {Check Initialized}
 if ATAPIInitialized then Exit;

 //To Do

 ATAPIInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ATAPI Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 ATAPIInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
