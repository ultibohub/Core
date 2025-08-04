{
Ultibo iSCSI interface unit.

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


iSCSI
=====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit iSCSI;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Storage,Winsock2;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {iSCSI specific constants}

//To Do //A generic iSCSI unit


{==============================================================================}
//type
 {iSCSI specific types}
//To Do

{==============================================================================}
var
 {iSCSI specific variables}
 iSCSIInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure iSCSIInit;

{==============================================================================}
{iSCSI Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure iSCSIInit;
begin
 {}
 {Check Initialized}
 if iSCSIInitialized then Exit;

 //To Do

 iSCSIInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{iSCSI Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 iSCSIInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
