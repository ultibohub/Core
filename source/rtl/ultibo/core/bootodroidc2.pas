{
Ultibo Initialization code for Odroid-C2

Copyright (C) 2016 - Rob Judd <judd@ob-wan.com>

Arch
====

 ARMv8 (Cortex A53)

Boards
======

 Odroid C2

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 http://www.hardkernel.com/main/products/prdt_info.php
 http://dn.odroid.com/S905/DataSheet/S905_Public_Datasheet_V1.1.4.pdf
 
 
References
==========

 Linux Device Tree files in

 ????


Odroid-C2
=========

 SoC: Amlogic S905
 
 CPU: ARMv8 Cortex-A53 (4 @ 1.5GHz)
 
 Cache: L1 32KB I/D, L2 512KB (Shared)
 
 GPU: Mali-450 (3 pixel processors + 2 vertex shaders) OpenGL ES2 750MHz + AVE H264/H265/VC1
 
 FPU: Crypto engine, 
 
 RAM: 2GB (Samsung K4B4G1646D x 4)
 
 USB: GeneSys GL852G Hub (4 ports) (with OnSemi NCP380 VBus controller) + Micro-B OTG
 
 LAN: RealTek RTL8211F Gigabit PHY
  
 SD/MMC: eMMC5.0 HS400 Flash Storage slot / UHS-1 SDR50 MicroSD Card slot
 
 WiFi: Nil
 
 Bluetooth: Nil
 
 Display: HDMI 2.0
 
 Other: GPIO (4), I2S (7), IR sensor, OnSemi NCP372 Power Protection IC.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootOdroidC2;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,amlogics905,Platform,PlatformOdroidC2,PlatformARM,PlatformARMv8,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

{==============================================================================}
{Boot Functions}
//procedure Startup;

//procedure Vectors; 
//procedure SecureVectors;

//procedure SecureMonitor;

//procedure StartupSwitch;
//procedure StartupSecure;
//procedure StartupHandler;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Boot Functions}

{==============================================================================}
{==============================================================================}
 
end.

