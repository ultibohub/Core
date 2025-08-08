{
Ultibo Platform interface unit for PC.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 x86  (i386)
 x86_64

Boards
======

 PC

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:


References
==========


Platform PC
===========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformPC;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Platformx86,
  Platformx86_64,
  HeapManager,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}

//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}


{==============================================================================}

end.
