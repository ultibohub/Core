{
Realtek 8xxx Wireless Driver library.

Copyright (C) 2016 - SoftOz Pty Ltd.

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

  Linux - \drivers\net\wireless\rtlwifi\* - Copyright(c) 2009-2012  Realtek Corporation.

References
==========


Realtek RTLWIFI
===============

 This unit provides functionality and definitions common to multiple implementations of the RTL8xxx
 chipset series PCI, USB or other.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit RTLWIFI;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.Network,
  Core.WiFi,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Network,
  WiFi,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
//const
 {RTLWIFI specific constants}

{==============================================================================}
{type}
 {RTLWIFI specific types}

{==============================================================================}
{var}
 {RTLWIFI specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{RTLWIFI Functions}

{==============================================================================}
{RTLWIFI Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{RTLWIFI Functions}


{==============================================================================}
{==============================================================================}
{RTLWIFI Helper Functions}

{==============================================================================}
{==============================================================================}

end.

