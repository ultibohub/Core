{
Ultibo Definitions specific to the NVidia Tegra 3 System on chip.

Copyright (C) 2016 - Rob Judd <judd@ob-wan.com>

Arch
====

 ARMv7 (Cortex A9)

Boards
======

 Asus Transformer TF300T
 Lenovo Idea A2109
 Nexus 7 (2012)
 Microsoft Surface
 Sony Xperia S
 Toshiba AT300 (Excite 10)
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 https://en.wikipedia.org/wiki/Tegra
 
 
References
==========

 https://android.googlesource.com/kernel/tegra/
 
 
NVidia Tegra 3
==============



}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit TEGRA3; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
{Tegra 3 specific constants}
 TEGRA3_CPU_COUNT = 4; {Ignore power saving CPU}

 
end;
 
{==============================================================================}
{Tegra 3 specific structures}

type 


end;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
 