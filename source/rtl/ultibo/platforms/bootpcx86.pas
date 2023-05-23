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

unit BootPCx86;

interface

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformPC,Platformx86,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

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
