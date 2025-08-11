{
Ultibo Initialization code for x86 PC.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 x86  (i386)

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


PC
==

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit BootPCx86;
{$ENDIF FPC_DOTTEDUNITS}

interface

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Platforms.PlatformPC,
  Platforms.Platformx86,
  Core.Threads,
  {$IFDEF CONSOLE_EARLY_INIT}
  Core.Devices,
  Core.Framebuffer,
  Core.Console,
  {$ENDIF}
  {$IFDEF LOGGING_EARLY_INIT}
  Core.Logging,
  {$ENDIF}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  PlatformPC,
  Platformx86,
  Threads,
  {$IFDEF CONSOLE_EARLY_INIT}
  Devices,
  Framebuffer,
  Console,
  {$ENDIF}
  {$IFDEF LOGGING_EARLY_INIT}
  Logging,
  {$ENDIF}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

//To Do //See: \u-boot-HEAD-5745f8c\arch\x86\cpu

{==============================================================================}
{Boot Functions}
procedure Startup;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Boot Functions}
procedure Startup; assembler; nostackframe; [public, alias: '_START'];
asm
 //To Do
end;

{==============================================================================}
{==============================================================================}

end.
