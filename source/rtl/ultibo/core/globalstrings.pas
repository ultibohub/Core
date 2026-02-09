{
Ultibo Global String Definitions.

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


Global Strings
==============


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit GlobalStrings;
{$ENDIF FPC_DOTTEDUNITS}

interface

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{Global strings}
const
 {Exception Strings}
 STRING_DATA_ABORT = 'Data abort';
 STRING_PREFETCH_ABORT = 'Prefetch abort';
 STRING_UNDEFINED_INSTRUCTION = 'Undefined Instruction';

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
