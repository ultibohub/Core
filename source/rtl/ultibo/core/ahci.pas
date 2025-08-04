{
Ultibo Advanced Host Controller Interface (AHCI) interface unit.

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


Advanced Host Controller Interface (AHCI)
=========================================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit AHCI;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Storage;

//To Do //See: \u-boot-HEAD-5745f8c\drivers\block

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {AHCI specific constants}


//To Do //A generic AHCI unit


{==============================================================================}
//type
 {AHCI specific types}
//To Do

{==============================================================================}
var
 {AHCI specific variables}
 AHCIInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure AHCIInit;

{==============================================================================}
{AHCI Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure AHCIInit;
begin
 {}
 {Check Initialized}
 if AHCIInitialized then Exit;

 //To Do

 AHCIInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{AHCI Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 AHCIInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

