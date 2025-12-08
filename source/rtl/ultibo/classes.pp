{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Free Pascal development team

    Classes unit for Ultibo target.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$H+}
{$modeswitch advancedrecords}
{$IF FPC_FULLVERSION>=30301}
{$modeswitch FUNCTIONREFERENCES}
{$define FPC_HAS_REFERENCE_PROCEDURE}
{$endif}

{$IFNDEF FPC_DOTTEDUNITS}
unit Classes;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.RtlConsts,
  System.Types,
  System.SortBase,
{$ifdef FPC_TESTGENERICS}
  System.FGL,
{$endif}
{$IF DEFINED(CPUARM) or DEFINED(CPUAARCH64) }
{$IF NOT DEFINED(FPC_STABLE) AND NOT DEFINED(FPC_FIXES) AND NOT DEFINED(FPC_LEGACY)}
  System.Intrinsics,
{$ENDIF}
{$ENDIF}
  System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils,
  rtlconsts,
  types,
  sortbase,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
{$IF DEFINED(CPUARM) or DEFINED(CPUAARCH64) }
{$IF NOT DEFINED(FPC_STABLE) AND NOT DEFINED(FPC_FIXES) AND NOT DEFINED(FPC_LEGACY)}
  intrinsics,
{$ENDIF}
{$ENDIF}
  typinfo;
{$ENDIF FPC_DOTTEDUNITS}

{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;

end.
