{
Ultibo SPI interface unit.

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


SPI Hosts
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SPI; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {SPI specific constants}
 //To Do

{==============================================================================}
//type
 {SPI specific types}
 //To Do
 
{==============================================================================}
var
 {SPI specific variables}
 SPIInitialized:Boolean;

{==============================================================================}
{Initialization Functions}
procedure SPIInit;
 
{==============================================================================}
{SPI Functions}
//To Do

{==============================================================================}
{SPI Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SPIInit;
begin
 {}
 {Check Initialized}
 if SPIInitialized then Exit;
 
 //To Do
 
 SPIInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{SPI Functions}
//To Do

{==============================================================================}
{==============================================================================}
{SPI Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 SPIInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.