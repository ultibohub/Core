{
Ultibo Video interface unit.

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


Video Devices
=============

This unit provides both the Video device interface and the generic USB video device driver.

USB Video Devices
=================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Video;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;
     //To Do //Add MPEG etc

//To Do //This unit should also include the generic USB Video driver ?
     
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Video specific constants}
//To Do

{==============================================================================}
//type
 {Video specific types}
//To Do

{==============================================================================}
var
 {Video specific variables}
 VideoInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure VideoInit;

{==============================================================================}
{Video Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure VideoInit;
begin
 {}
 {Check Initialized}
 if VideoInitialized then Exit;
 
 //To Do
 
 VideoInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Video Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 VideoInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
