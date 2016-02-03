{
Ultibo MMC SPI interface unit.

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


MMC SPI Hosts
=============

This driver implements a generic MMC SPI host controller. It relies on the MMC core unit as well as the SPI core.

There needs to be an SPI driver registered which provides the SPI interface to support this module.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit MMCSPI;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,MMC,SPI,SysUtils;

//To Do

//See: \u-boot-HEAD-5745f8c\drivers\mmc\mmc_spi.c
//See: \linux-rpi-3.18.y\drivers\mmc\host\mmc_spi.c

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {MMC SPI specific constants}
 //To Do
 
{==============================================================================}
//type
 {MMC SPI specific types}
 //To Do
 
{==============================================================================}
var
 {MMC SPI specific variables}
 MMCSPIInitialized:Boolean;
 
 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure MMCSPIInit;

{==============================================================================}
{MMC SPI Functions}
//To Do

{==============================================================================}
{MMC SPI Helper Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure MMCSPIInit;
begin
 {}
 {Check Initialized}
 if MMCSPIInitialized then Exit;

 //To Do
 
 MMCSPIInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{MMC SPI Functions}
//To Do

{==============================================================================}
{==============================================================================}
{MMC SPI Helper Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 MMCSPIInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
