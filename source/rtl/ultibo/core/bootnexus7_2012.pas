{
Ultibo Initialization code for Nexus 7 (2012)

Copyright (C) 2016 - Rob Judd <judd@ob-wan.com>

Arch
====

 ARMv7 (Cortex A9)

Boards
======

 Nexus 7 (2012)

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 https://wikidevi.com/wiki/Google_Nexus_7_(Gen_1,_Wi-Fi_only,_16_GB)
 
 
References
==========

 Full Source Tree files in ...

 https://android.googlesource.com/


Nexus 7 (2012)
==============

 Manufacturer: ASUS
 
 Model: ME370T
 
 Stock OS: Android 4.1.1 (Jelly Bean)
 
 SoC: NVidia Tegra 3 T30L (Kal-El)
 
 CPU: Cortex A8 (ARM7) (4 @ 1.2GHz, plus 5th "power monitor" CPU)
 
 Cache: L1 32KB instruction plus 32KB data, L2 1MB (Shared by all cores)
 
 FPU: VFPv3 plus NEON SIMD
 
 GPU: NVidia GEForce ULP (12 @ 416MHz)
 
 RAM: 1GB (Hynix H5TC2G83CFR-H9R × 4)
 
 USB: Micro-B OTG
 
 LAN: Nil
  
 SD/MMC: 8/16/32GB internal
 
 WiFi: AzureWave AW-NH665 802.11 b/g/n (SDIO I/F)
 
 Bluetooth: Broadcom BCM4330 BLE 4.0
 
 Mobile: HSPA+ (optional)
 
 Audio: RealTek ALC5642 (Stereo with inbuilt speakers and 3.5mm jack)
 
 Camera: Lite-On 10P2SF130J 1.2MP (front facing)
 
 NFC: NXP PN65N / 65N04 Secured
 
 GPS: Broadcom BCM 47511
 
 Gyro/Accelerometer: InvenSense MPU-6050
 
 Display: Hydis HV070WX2-1E0 (7" diagonal, 1280x800)
 
 Capacitive Touchscreen: Elan eKTH1036BWS / eKTF3624BWS
 
 Other: Mic, Magnetometer, Maxim Power Management, 3.7V 4325mAh (16WH) Battery

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootNexus7_2012;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformNexus7_2012,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

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

