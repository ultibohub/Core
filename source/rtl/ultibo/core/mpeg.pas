{
Ultibo MPEG interface unit.

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

 Based on sources from ffmpeg and others.

MPEG
====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit MPEG;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {MPEG specific constants}

//To Do //A generic MPEG unit


{==============================================================================}
//type
 {MPEG specific types}
//To Do

{==============================================================================}
var
 {MPEG specific variables}
 MPEGInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure MPEGInit;

{==============================================================================}
{MPEG Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure MPEGInit;
begin
 {}
 {Check Initialized}
 if MPEGInitialized then Exit;

 //To Do

 MPEGInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{MPEG Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 MPEGInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
