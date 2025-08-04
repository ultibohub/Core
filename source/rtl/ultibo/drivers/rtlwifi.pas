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

unit RTLWIFI;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Network,WiFi,SysUtils;

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

