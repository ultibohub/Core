{
Ultibo Initialization code for Banana Pro.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A7)

Boards
======

 Banana Pi

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 ????
 
References
==========

 Allwinner A20 Datasheet V1.41 20131230.pdf
 
 Allwinner A20 user manual v1.3 20141010.pdf
 
 Linux Device Tree files in /arch/arm/boot/dts

 ????
 
Banana Pro
==========

 SoC: Allwinner A20
 
 CPU: ARM Cortex A7 (ARMv7) (2 @ 1GHz)
 
 GPU: Mali400 MP2
 
 RAM: ??
 
 USB: 2 x EHCI/OHCI
 
 LAN: ??
  
 SD/MMC: 
 
 WiFi: 
 
 Bluetooth: 

 Other: AHCI / RTC / VIC
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootBPro;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformBPro,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF};  

//To Do //See: \u-boot-HEAD-5745f8c\arch\arm\cpu\armv7\sunxi

//See: \linux-sunxi-lemaker-3.4\arch\arm\plat-sunxi
//See: \linux-sunxi-lemaker-3.4\arch\arm\plat-sunxi\include\plat\platform.h

//See: \linux-sunxi-lemaker-3.4\arch\arm\mach-sun7i

{==============================================================================}
{Boot Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Boot Functions}

{==============================================================================}
{==============================================================================}
 
end.
