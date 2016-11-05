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

http://ecee.colorado.edu/ecen5653/ecen5653/doc/JETSON/Tegra_Linux_Driver_Package_Developers_Guide.pdf

 
NVidia Tegra 3
==============



}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit TEGRA3; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes;

//To Do

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{NVidia Tegra 3 specific constants}

 const
 TEGRA3_CPU_COUNT = 4; {Ignore power saving CPU}
 
end;
 
{==============================================================================}
{NVidia Tegra 3 specific structures}

 type 

end;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
 