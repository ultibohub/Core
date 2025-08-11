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

{$IFNDEF FPC_DOTTEDUNITS}
unit PlatformPC;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Platforms.Platformx86,
  Platforms.Platformx86_64,
  Core.HeapManager,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Platformx86,
  Platformx86_64,
  HeapManager,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

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
