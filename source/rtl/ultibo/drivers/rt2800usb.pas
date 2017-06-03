{
Ralink RT2800 USB Wireless Driver.

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

  Linux - \drivers\net\wireless\rt2x00\* - Copyright (C) 2010 Willow Garage and others.
  
References
==========

 RT2x00 - http://ralink.rapla.net/ (Contains some patchy information about Ralink chipsets)
 
Ralink RT2800
=============

 The Ralink RT2800 is a WLAN controller with USB2.0 interface, Ralink is now part of MediaTek.
 
 The list of USB supported device IDs shown below for this driver is taken from the equivalent Linux driver
 but not all have been tested. In general if your device works with the rt2800usb kernel module under Linux
 then it should also work with this driver.
 
 Only USB devices are currently supported, PCI and other forms of the same chipset will require a separate 
 driver unit for support.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RT2800USB; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,Network,WiFi,RT2800LIB,RT2X00USB,RT2X00LIB,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RT2800USB specific constants}
 RT2800USB_NETWORK_DESCRIPTION = 'Ralink RT2800 USB Wireless'; {Description of RT2800USB device}
 
 RT2800USB_DRIVER_NAME = 'Ralink RT2800 USB Wireless Driver';  {Name of RT2800USB driver}
 
 RT2800USB_DEVICE_ID_COUNT = 330; {Number of supported Device IDs}

 RT2800USB_DEVICE_ID:array[0..RT2800USB_DEVICE_ID_COUNT - 1] of TUSBDeviceId = (
  (idVendor:$07b8;idProduct:$2870),  {Abocom}
  (idVendor:$07b8;idProduct:$2770),  
  (idVendor:$07b8;idProduct:$3070),  
  (idVendor:$07b8;idProduct:$3071),  
  (idVendor:$07b8;idProduct:$3072),  
  (idVendor:$1482;idProduct:$3c09),  
  (idVendor:$1eda;idProduct:$2012),  {AirTies}
  (idVendor:$1eda;idProduct:$2210), 
  (idVendor:$1eda;idProduct:$2310), 
  (idVendor:$8516;idProduct:$2070),  {Allwin}
  (idVendor:$8516;idProduct:$2770), 
  (idVendor:$8516;idProduct:$2870), 
  (idVendor:$8516;idProduct:$3070), 
  (idVendor:$8516;idProduct:$3071), 
  (idVendor:$8516;idProduct:$3072), 
  (idVendor:$14b2;idProduct:$3c06),  {Alpha Networks}
  (idVendor:$14b2;idProduct:$3c07), 
  (idVendor:$14b2;idProduct:$3c09), 
  (idVendor:$14b2;idProduct:$3c12), 
  (idVendor:$14b2;idProduct:$3c23), 
  (idVendor:$14b2;idProduct:$3c25), 
  (idVendor:$14b2;idProduct:$3c27), 
  (idVendor:$14b2;idProduct:$3c28), 
  (idVendor:$14b2;idProduct:$3c2c), 
  (idVendor:$15c5;idProduct:$0008),  {Amit}
  (idVendor:$1690;idProduct:$0740),  {Askey} 
  (idVendor:$0b05;idProduct:$1731),  {ASUS} 
  (idVendor:$0b05;idProduct:$1732), 
  (idVendor:$0b05;idProduct:$1742), 
  (idVendor:$0b05;idProduct:$1784), 
  (idVendor:$1761;idProduct:$0b05), 
  (idVendor:$13d3;idProduct:$3247),  {AzureWave} 
  (idVendor:$13d3;idProduct:$3273), 
  (idVendor:$13d3;idProduct:$3305), 
  (idVendor:$13d3;idProduct:$3307), 
  (idVendor:$13d3;idProduct:$3321), 
  (idVendor:$050d;idProduct:$8053),  {Belkin}
  (idVendor:$050d;idProduct:$805c), 
  (idVendor:$050d;idProduct:$815c), 
  (idVendor:$050d;idProduct:$825a), 
  (idVendor:$050d;idProduct:$825b), 
  (idVendor:$050d;idProduct:$935a), 
  (idVendor:$050d;idProduct:$935b), 
  (idVendor:$0411;idProduct:$00e8),  {Buffalo}
  (idVendor:$0411;idProduct:$0158), 
  (idVendor:$0411;idProduct:$015d), 
  (idVendor:$0411;idProduct:$016f), 
  (idVendor:$0411;idProduct:$01a2), 
  (idVendor:$0411;idProduct:$01ee), 
  (idVendor:$0411;idProduct:$01a8), 
  (idVendor:$07aa;idProduct:$002f),  {Corega}
  (idVendor:$07aa;idProduct:$003c), 
  (idVendor:$07aa;idProduct:$003f), 
  (idVendor:$18c5;idProduct:$0012), 
  (idVendor:$07d1;idProduct:$3c09),  {D-Link} 
  (idVendor:$07d1;idProduct:$3c0a), 
  (idVendor:$07d1;idProduct:$3c0d), 
  (idVendor:$07d1;idProduct:$3c0e), 
  (idVendor:$07d1;idProduct:$3c0f), 
  (idVendor:$07d1;idProduct:$3c11), 
  (idVendor:$07d1;idProduct:$3c13), 
  (idVendor:$07d1;idProduct:$3c15), 
  (idVendor:$07d1;idProduct:$3c16), 
  (idVendor:$07d1;idProduct:$3c17), 
  (idVendor:$2001;idProduct:$3317), 
  (idVendor:$2001;idProduct:$3c1b), 
  (idVendor:$2001;idProduct:$3c25), 
  (idVendor:$07fa;idProduct:$7712),  {Draytek}
  (idVendor:$0fe9;idProduct:$b307),  {DVICO}
  (idVendor:$7392;idProduct:$4085),  {Edimax}
  (idVendor:$7392;idProduct:$7711), 
  (idVendor:$7392;idProduct:$7717), 
  (idVendor:$7392;idProduct:$7718), 
  (idVendor:$7392;idProduct:$7722), 
  (idVendor:$203d;idProduct:$1480),  {Encore}
  (idVendor:$203d;idProduct:$14a9), 
  (idVendor:$1740;idProduct:$9701),  {EnGenius}
  (idVendor:$1740;idProduct:$9702), 
  (idVendor:$1740;idProduct:$9703), 
  (idVendor:$1740;idProduct:$9705), 
  (idVendor:$1740;idProduct:$9706), 
  (idVendor:$1740;idProduct:$9707), 
  (idVendor:$1740;idProduct:$9708), 
  (idVendor:$1740;idProduct:$9709), 
  (idVendor:$15a9;idProduct:$0012),  {Gemtek}
  (idVendor:$1044;idProduct:$800b),  {Gigabyte}
  (idVendor:$1044;idProduct:$800d), 
  (idVendor:$0e66;idProduct:$0001),  {Hawking}
  (idVendor:$0e66;idProduct:$0003), 
  (idVendor:$0e66;idProduct:$0009), 
  (idVendor:$0e66;idProduct:$000b), 
  (idVendor:$0e66;idProduct:$0013), 
  (idVendor:$0e66;idProduct:$0017), 
  (idVendor:$0e66;idProduct:$0018), 
  (idVendor:$04bb;idProduct:$0945),  {I-O DATA} 
  (idVendor:$04bb;idProduct:$0947), 
  (idVendor:$04bb;idProduct:$0948), 
  (idVendor:$13b1;idProduct:$0031), {Linksys} 
  (idVendor:$1737;idProduct:$0070), 
  (idVendor:$1737;idProduct:$0071), 
  (idVendor:$1737;idProduct:$0077), 
  (idVendor:$1737;idProduct:$0078), 
  (idVendor:$0789;idProduct:$0162),  {Logitec} 
  (idVendor:$0789;idProduct:$0163), 
  (idVendor:$0789;idProduct:$0164), 
  (idVendor:$0789;idProduct:$0166), 
  (idVendor:$100d;idProduct:$9031),  {Motorola}
  (idVendor:$0db0;idProduct:$3820),  {MSI} 
  (idVendor:$0db0;idProduct:$3821), 
  (idVendor:$0db0;idProduct:$3822), 
  (idVendor:$0db0;idProduct:$3870), 
  (idVendor:$0db0;idProduct:$3871), 
  (idVendor:$0db0;idProduct:$6899), 
  (idVendor:$0db0;idProduct:$821a), 
  (idVendor:$0db0;idProduct:$822a), 
  (idVendor:$0db0;idProduct:$822b), 
  (idVendor:$0db0;idProduct:$822c), 
  (idVendor:$0db0;idProduct:$870a), 
  (idVendor:$0db0;idProduct:$871a), 
  (idVendor:$0db0;idProduct:$871b), 
  (idVendor:$0db0;idProduct:$871c), 
  (idVendor:$0db0;idProduct:$899a), 
  (idVendor:$1b75;idProduct:$3071),  {Ovislink} 
  (idVendor:$1b75;idProduct:$3072), 
  (idVendor:$1b75;idProduct:$a200), 
  (idVendor:$20b8;idProduct:$8888),  {Para}
  (idVendor:$1d4d;idProduct:$0002),  {Pegatron} 
  (idVendor:$1d4d;idProduct:$000c), 
  (idVendor:$1d4d;idProduct:$000e), 
  (idVendor:$1d4d;idProduct:$0011), 
  (idVendor:$0471;idProduct:$200f),  {Philips} 
  (idVendor:$2019;idProduct:$5201),  {Planex} 
  (idVendor:$2019;idProduct:$ab25), 
  (idVendor:$2019;idProduct:$ed06), 
  (idVendor:$1a32;idProduct:$0304),  {Quanta}
  (idVendor:$148f;idProduct:$2070),  {Ralink}
  (idVendor:$148f;idProduct:$2770), 
  (idVendor:$148f;idProduct:$2870), 
  (idVendor:$148f;idProduct:$3070), 
  (idVendor:$148f;idProduct:$3071), 
  (idVendor:$148f;idProduct:$3072), 
  (idVendor:$04e8;idProduct:$2018),  {Samsung} 
  (idVendor:$129b;idProduct:$1828),  {Siemens}
  (idVendor:$0df6;idProduct:$0017),  {Sitecom} 
  (idVendor:$0df6;idProduct:$002b), 
  (idVendor:$0df6;idProduct:$002c), 
  (idVendor:$0df6;idProduct:$002d), 
  (idVendor:$0df6;idProduct:$0039), 
  (idVendor:$0df6;idProduct:$003b), 
  (idVendor:$0df6;idProduct:$003d), 
  (idVendor:$0df6;idProduct:$003e), 
  (idVendor:$0df6;idProduct:$003f), 
  (idVendor:$0df6;idProduct:$0040), 
  (idVendor:$0df6;idProduct:$0042), 
  (idVendor:$0df6;idProduct:$0047), 
  (idVendor:$0df6;idProduct:$0048), 
  (idVendor:$0df6;idProduct:$0051), 
  (idVendor:$0df6;idProduct:$005f), 
  (idVendor:$0df6;idProduct:$0060), 
  (idVendor:$083a;idProduct:$6618),  {SMC}
  (idVendor:$083a;idProduct:$7511), 
  (idVendor:$083a;idProduct:$7512), 
  (idVendor:$083a;idProduct:$7522), 
  (idVendor:$083a;idProduct:$8522), 
  (idVendor:$083a;idProduct:$a618), 
  (idVendor:$083a;idProduct:$a701), 
  (idVendor:$083a;idProduct:$a702), 
  (idVendor:$083a;idProduct:$a703), 
  (idVendor:$083a;idProduct:$b522), 
  (idVendor:$15a9;idProduct:$0006),  {Sparklan}
  (idVendor:$177f;idProduct:$0153),  {Sweex} 
  (idVendor:$177f;idProduct:$0164), 
  (idVendor:$177f;idProduct:$0302), 
  (idVendor:$177f;idProduct:$0313), 
  (idVendor:$177f;idProduct:$0323), 
  (idVendor:$177f;idProduct:$0324), 
  (idVendor:$157e;idProduct:$300e),  {U-Media}
  (idVendor:$157e;idProduct:$3013), 
  (idVendor:$0cde;idProduct:$0022),  {ZCOM} 
  (idVendor:$0cde;idProduct:$0025), 
  (idVendor:$5a57;idProduct:$0280),  {Zinwell}
  (idVendor:$5a57;idProduct:$0282), 
  (idVendor:$5a57;idProduct:$0283), 
  (idVendor:$5a57;idProduct:$5257), 
  (idVendor:$0586;idProduct:$3416),  {Zyxel}
  (idVendor:$0586;idProduct:$3418), 
  (idVendor:$0586;idProduct:$341a), 
  (idVendor:$0586;idProduct:$341e), 
  (idVendor:$0586;idProduct:$343e), 
  {RT2800USB_RT33XX devices}
  (idVendor:$050d;idProduct:$945b),  {Belkin}
  (idVendor:$2001;idProduct:$3c17),  {D-Link} 
  (idVendor:$083a;idProduct:$b511),  {Panasonic}
  (idVendor:$0471;idProduct:$20dd),  {Philips}
  (idVendor:$148f;idProduct:$3370),  {Ralink}
  (idVendor:$148f;idProduct:$8070), 
  (idVendor:$0df6;idProduct:$0050),  {Sitecom}
  (idVendor:$177f;idProduct:$0163),  {Sweex}
  (idVendor:$177f;idProduct:$0165), 
  {RT2800USB_RT35XX devices}
  (idVendor:$8516;idProduct:$3572),  {Allwin}
  (idVendor:$1690;idProduct:$0744),  {Askey}
  (idVendor:$1690;idProduct:$0761), 
  (idVendor:$1690;idProduct:$0764), 
  (idVendor:$0b05;idProduct:$179d),  {ASUS}
  (idVendor:$167b;idProduct:$4001),  {Cisco} 
  (idVendor:$1740;idProduct:$9801),  {EnGenius} 
  (idVendor:$04bb;idProduct:$0944),  {I-O DATA}
  (idVendor:$13b1;idProduct:$002f),  {Linksys}
  (idVendor:$1737;idProduct:$0079), 
  (idVendor:$0789;idProduct:$0170),  {Logitec}
  (idVendor:$148f;idProduct:$3572),  {Ralink} 
  (idVendor:$0df6;idProduct:$0041),  {Sitecom} 
  (idVendor:$0df6;idProduct:$0062), 
  (idVendor:$0df6;idProduct:$0065), 
  (idVendor:$0df6;idProduct:$0066), 
  (idVendor:$0df6;idProduct:$0068), 
  (idVendor:$0930;idProduct:$0a07),  {Toshiba} 
  (idVendor:$5a57;idProduct:$0284),  {Zinwell} 
  {RT2800USB_RT3573 devices}
  (idVendor:$1b75;idProduct:$7733),  {AirLive}
  (idVendor:$0b05;idProduct:$17bc),  {ASUS}
  (idVendor:$0b05;idProduct:$17ad), 
  (idVendor:$050d;idProduct:$1103),  {Belkin}
  (idVendor:$148f;idProduct:$f301),  {Cameo}
  (idVendor:$2001;idProduct:$3c1f),  {D-Link} 
  (idVendor:$7392;idProduct:$7733),  {Edimax}
  (idVendor:$0e66;idProduct:$0020),  {Hawking}
  (idVendor:$0e66;idProduct:$0021), 
  (idVendor:$04bb;idProduct:$094e),  {I-O DATA}
  (idVendor:$13b1;idProduct:$003b),  {Linksys} 
  (idVendor:$0789;idProduct:$016b),  {Logitec}
  (idVendor:$0846;idProduct:$9012),  {NETGEAR} 
  (idVendor:$0846;idProduct:$9013), 
  (idVendor:$0846;idProduct:$9019), 
  (idVendor:$2019;idProduct:$ed19),  {Planex}
  (idVendor:$148f;idProduct:$3573),  {Ralink}
  (idVendor:$0df6;idProduct:$0067),  {Sitecom}
  (idVendor:$0df6;idProduct:$006a), 
  (idVendor:$0df6;idProduct:$006e), 
  (idVendor:$0586;idProduct:$3421),  {ZyXEL}
  {RT2800USB_RT53XX devices}
  (idVendor:$043e;idProduct:$7a12),  {Arcadyan}
  (idVendor:$043e;idProduct:$7a32), 
  (idVendor:$0b05;idProduct:$17e8),  {ASUS}
  (idVendor:$13d3;idProduct:$3329),  {Azurewave}
  (idVendor:$13d3;idProduct:$3365), 
  (idVendor:$2001;idProduct:$3c15),  {D-Link}
  (idVendor:$2001;idProduct:$3c19), 
  (idVendor:$2001;idProduct:$3c1c), 
  (idVendor:$2001;idProduct:$3c1d), 
  (idVendor:$2001;idProduct:$3c1e), 
  (idVendor:$2001;idProduct:$3c20), 
  (idVendor:$2001;idProduct:$3c22), 
  (idVendor:$2001;idProduct:$3c23), 
  (idVendor:$043e;idProduct:$7a22),  {LG innotek} 
  (idVendor:$043e;idProduct:$7a42), 
  (idVendor:$04da;idProduct:$1801),  {Panasonic} 
  (idVendor:$04da;idProduct:$1800), 
  (idVendor:$04da;idProduct:$23f6), 
  (idVendor:$0471;idProduct:$2104),  {Philips} 
  (idVendor:$0471;idProduct:$2126), 
  (idVendor:$0471;idProduct:$2180), 
  (idVendor:$0471;idProduct:$2181), 
  (idVendor:$0471;idProduct:$2182), 
  (idVendor:$148f;idProduct:$5370),  {Ralink} 
  (idVendor:$148f;idProduct:$5372), 
  {RT2800USB_RT55XX devices}
  (idVendor:$043e;idProduct:$7a32),  {Arcadyan}
  (idVendor:$057c;idProduct:$8501),  {AVM GmbH} 
  (idVendor:$0411;idProduct:$0241),  {Buffalo}
  (idVendor:$0411;idProduct:$0253), 
  (idVendor:$2001;idProduct:$3c1a),  {D-Link}
  (idVendor:$2001;idProduct:$3c21), 
  (idVendor:$043e;idProduct:$7a13),  {Proware}
  (idVendor:$148f;idProduct:$5572),  {Ralink}
  (idVendor:$20f4;idProduct:$724a),  {TRENDnet}
  {RT2800USB_UNKNOWN devices}
  (idVendor:$07b8;idProduct:$3073),  {Abocom}
  (idVendor:$07b8;idProduct:$3074), 
  (idVendor:$14b2;idProduct:$3c08),  {Alpha Networks}
  (idVendor:$14b2;idProduct:$3c11), 
  (idVendor:$0e0b;idProduct:$9031),  {Amigo}
  (idVendor:$0e0b;idProduct:$9041), 
  (idVendor:$0b05;idProduct:$166a),  {ASUS}
  (idVendor:$0b05;idProduct:$1760), 
  (idVendor:$0b05;idProduct:$1761), 
  (idVendor:$0b05;idProduct:$1790), 
  (idVendor:$0b05;idProduct:$17a7), 
  (idVendor:$13d3;idProduct:$3262),  {AzureWave}
  (idVendor:$13d3;idProduct:$3284), 
  (idVendor:$13d3;idProduct:$3322), 
  (idVendor:$13d3;idProduct:$3340), 
  (idVendor:$13d3;idProduct:$3399), 
  (idVendor:$13d3;idProduct:$3400), 
  (idVendor:$13d3;idProduct:$3401), 
  (idVendor:$050d;idProduct:$1003),  {Belkin}
  (idVendor:$0411;idProduct:$012e),  {Buffalo}
  (idVendor:$0411;idProduct:$0148), 
  (idVendor:$0411;idProduct:$0150), 
  (idVendor:$07aa;idProduct:$0041),  {Corega}
  (idVendor:$07aa;idProduct:$0042), 
  (idVendor:$18c5;idProduct:$0008), 
  (idVendor:$07d1;idProduct:$3c0b),  {D-Link}
  (idVendor:$203d;idProduct:$14a1),  {Encore}
  (idVendor:$1740;idProduct:$0600),  {EnGenius}
  (idVendor:$1740;idProduct:$0602), 
  (idVendor:$15a9;idProduct:$0010),  {Gemtek}
  (idVendor:$1044;idProduct:$800c),  {Gigabyte}
  (idVendor:$06f8;idProduct:$e036),  {Hercules}
  (idVendor:$148f;idProduct:$f101),  {Huawei}
  (idVendor:$04bb;idProduct:$094b),  {I-O DATA} 
  (idVendor:$1740;idProduct:$0605),  {LevelOne}
  (idVendor:$1740;idProduct:$0615), 
  (idVendor:$0789;idProduct:$0168),  {Logitec}
  (idVendor:$0789;idProduct:$0169), 
  (idVendor:$100d;idProduct:$9032),  {Motorola}
  (idVendor:$05a6;idProduct:$0101),  {Pegatron}
  (idVendor:$1d4d;idProduct:$0010), 
  (idVendor:$2019;idProduct:$ab24),  {Planex}
  (idVendor:$2019;idProduct:$ab29), 
  (idVendor:$18e8;idProduct:$6259),  {Qcom}
  (idVendor:$08b9;idProduct:$1197),  {RadioShack}
  (idVendor:$0df6;idProduct:$003c),  {Sitecom}
  (idVendor:$0df6;idProduct:$004a), 
  (idVendor:$0df6;idProduct:$004d), 
  (idVendor:$0df6;idProduct:$0053), 
  (idVendor:$0df6;idProduct:$0069), 
  (idVendor:$0df6;idProduct:$006f), 
  (idVendor:$0df6;idProduct:$0078), 
  (idVendor:$083a;idProduct:$a512),  {SMC}
  (idVendor:$083a;idProduct:$c522), 
  (idVendor:$083a;idProduct:$d522), 
  (idVendor:$083a;idProduct:$f511), 
  (idVendor:$177f;idProduct:$0254),  {Sweex}
  (idVendor:$f201;idProduct:$5370));  {TP-LINK}
 
 {Firmware image}
 RT2800USB_FIRMWARE_IMAGEBASE = $3000;
  
 {DMA descriptor defines}
 RT2800USB_TXINFO_DESC_SIZE = (1 * SizeOf(LongWord));
 RT2800USB_RXINFO_DESC_SIZE = (1 * SizeOf(LongWord));
 
 {TXINFO structure}
 {Word0}
 RT2800USB_TXINFO_W0_USB_DMA_TX_PKT_LEN = $0000ffff;
 RT2800USB_TXINFO_W0_WIV		        = $01000000; {WIV: Wireless Info Valid. 1: Driver filled WI,  0: DMA needs to copy WI}
 RT2800USB_TXINFO_W0_QSEL		        = $06000000; {QSEL: Select on-chip FIFO ID for 2nd-stage output scheduler. 0:MGMT, 1:HCCA 2:EDCA}
 RT2800USB_TXINFO_W0_SW_USE_LAST_ROUND  = $08000000; 
 RT2800USB_TXINFO_W0_USB_DMA_NEXT_VALID = $40000000; {USB_DMA_NEXT_VALID: Used ONLY in USB bulk Aggregation, NextValid}
 RT2800USB_TXINFO_W0_USB_DMA_TX_BURST   = $80000000; {DMA_TX_BURST: Used ONLY in USB bulk Aggregation. Force USB DMA transmit frame from current selected endpoint}
 
 {RXINFO structure}
 {Word 0}
 RT2800USB_RXINFO_W0_USB_DMA_RX_PKT_LEN	= $0000ffff;
 
 {RXD structure}
 {Word0}
 RT2800USB_RXD_W0_BA             = $00000001;
 RT2800USB_RXD_W0_DATA           = $00000002;
 RT2800USB_RXD_W0_NULLDATA       = $00000004;
 RT2800USB_RXD_W0_FRAG           = $00000008;
 RT2800USB_RXD_W0_UNICAST_TO_ME  = $00000010; {UNICAST_TO_ME: This RX frame is unicast to me.}
 RT2800USB_RXD_W0_MULTICAST      = $00000020; {MULTICAST: This is a multicast frame.}
 RT2800USB_RXD_W0_BROADCAST      = $00000040; {BROADCAST: This is a broadcast frame.}
 RT2800USB_RXD_W0_MY_BSS         = $00000080; {MY_BSS: this frame belongs to the same BSSID.}
 RT2800USB_RXD_W0_CRC_ERROR      = $00000100; {CRC_ERROR: CRC error.}
 RT2800USB_RXD_W0_CIPHER_ERROR   = $00000600; {CIPHER_ERROR: 0: decryption okay, 1:ICV error, 2:MIC error, 3:KEY not valid}
 RT2800USB_RXD_W0_AMSDU          = $00000800; {AMSDU: rx with 802.3 header, not 802.11 header}
 RT2800USB_RXD_W0_HTC            = $00001000;
 RT2800USB_RXD_W0_RSSI           = $00002000;
 RT2800USB_RXD_W0_L2PAD          = $00004000;
 RT2800USB_RXD_W0_AMPDU          = $00008000;
 RT2800USB_RXD_W0_DECRYPTED      = $00010000;
 RT2800USB_RXD_W0_PLCP_RSSI      = $00020000;
 RT2800USB_RXD_W0_CIPHER_ALG     = $00040000;
 RT2800USB_RXD_W0_LAST_AMSDU     = $00080000;
 RT2800USB_RXD_W0_PLCP_SIGNAL    = $fff00000;
 
 RT2800USB_MAX_TX_ENTRIES = SIZE_16;
 RT2800USB_MAX_RX_ENTRIES = SIZE_256; 
 
{==============================================================================}
type
 {RT2800USB specific types}
 
 {RT2800USB Request}
 PRT2800USBRequest = ^TRT2800USBRequest;
 TRT2800USBRequest = record
  Index:LongWord;                     {The index of this request in the receive or transmit requests array}
  Entry:PNetworkEntry;                {The network queue entry currently allocated to this endpoint}
  Request:PUSBRequest;                {The USB request allocated for this endpoint}
  Endpoint:PUSBEndpointDescriptor;    {The USB endpoint descriptor found during bind for this endpoint}
 end;
 
 {RT2800USB Device}
 PRT2800USBWiFiDevice = ^TRT2800USBWiFiDevice;
 TRT2800USBWiFiDevice = record
  {RT2X00 Properties}
  RT2X00:TRT2X00WiFiDevice;
  {USB Properties}
  ReceiveMask:LongWord;                          {Bit mask of Receive requests}
  ReceiveFree:LongWord;                          {Map of free Receive requests}
  ReceiveRequests:array of PRT2800USBRequest;    {Receive requests, 1 per Bulk IN Endpoint}
  TransmitMask:LongWord;                         {Bit mask of Transmit requests}
  TransmitFree:LongWord;                         {Map of free Transmit requests}
  TransmitRequests:array of PRT2800USBRequest;   {Transmit requests, 1 per Bulk OUT Endpoint}
  PendingCount:LongWord;                         {Number of USB requests pending for this network}
  WaiterThread:TThreadId;                        {Thread waiting for pending requests to complete (for network close)}
 end;
 
{==============================================================================}
var
 {RT2800USB specific variables}
 RT2800USB_FIRMWARE_FILENAME:String = 'rt2870.bin';  {The name of the firmware image file to load}
 RT2800USB_FIRMWARE_INTERNAL:LongBool = True;        {If True then load the internal firmware not a firmware file}
 
 RT2800USB_HARDWARE_ENCRYPTION_DISABLED:LongBool;    {If True then disable hardware encryption in the device}
 
{==============================================================================}
{Initialization Functions}
procedure RT2800USBInit;

{==============================================================================}
{RT2800USB Network Functions}
function RT2800USBDeviceOpen(Network:PNetworkDevice):LongWord;
function RT2800USBDeviceClose(Network:PNetworkDevice):LongWord;
function RT2800USBDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function RT2800USBBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function RT2800USBBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
function RT2800USBBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function RT2800USBBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;

{==============================================================================}
{RT2800USB WiFi Functions}
function RT2800USBDeviceConfigure(WiFi:PWiFiDevice;Flags:LongWord):LongWord;
function RT2800USBDeviceConfigureFilter(WiFi:PWiFiDevice;var Filter:LongWord):LongWord;
function RT2800USBDeviceConfigureInterface(WiFi:PWiFiDevice;Interrface:PWiFiInterface):LongWord; 

{==============================================================================}
{RT2800USB USB Functions}
function RT2800USBDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function RT2800USBDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure RT2800USBReceiveWorker(Request:PUSBRequest); 
procedure RT2800USBReceiveComplete(Request:PUSBRequest); 

procedure RT2800USBTransmitWorker(Request:PUSBRequest); 
procedure RT2800USBTransmitComplete(Request:PUSBRequest); 

{==============================================================================}
{RT2800USB Helper Functions}
function RT2800USBCheckDevice(Device:PUSBDevice):LongWord;

function RT2800USBDriverInit(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2800USBEepromLoad(RT2X00:PRT2X00WiFiDevice;Data:PWord;Size:LongWord):LongWord;

function RT2800USBSetState(RT2X00:PRT2X00WiFiDevice;State:LongWord):LongWord;

function RT2800USBEnableRX(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2800USBDisableRX(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2800USBEnableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2800USBDisableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2800USBGetFirmware(RT2X00:PRT2X00WiFiDevice;var Name:String;var Address:Pointer;var Size:LongWord):Boolean;
function RT2800USBWriteFirmware(RT2X00:PRT2X00WiFiDevice;Data:PByte;Size:LongWord):Boolean;

function RT2800USBInitializeRegisters(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2800USBHardwareEncryptionDisabled(RT2X00:PRT2X00WiFiDevice):Boolean;

function RT2800USBDetectEfuse(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2800USBDetectAutorun(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2800USBEnableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2800USBDisableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2800USBReceiveProcessRXD(RT2X00:PRT2X00WiFiDevice;Descriptor:PRT2X00RXDescriptor;var Data:Pointer;var Size:LongWord;PacketLength:LongWord):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RT2800USB specific variables}
 RT2800USBInitialized:Boolean; 
 
 RT2800USBDriver:PUSBDriver;  {RT2800USB Driver interface (Set by RT2800USBInit)}

 {RT2800USB firmware (rt2870.bin)}
 RT2800USBFirmwareOld:array[0..8191] of Byte = ( //To Do //rt2870.bin from ????
  $FF,$FF,$FF,$02,$10,$28,$02,$10,$32,$02,$10,$7D,$02,$15,$02,$02,
  $15,$03,$02,$15,$22,$02,$15,$27,$12,$15,$23,$22,$02,$1A,$69,$02,
  $1B,$8C,$02,$16,$B6,$02,$15,$F1,$30,$05,$06,$20,$0D,$03,$12,$1C,
  $71,$22,$90,$01,$8C,$E0,$30,$E3,$20,$E5,$58,$30,$E0,$10,$E5,$4C,
  $30,$E0,$04,$7F,$40,$80,$02,$7F,$00,$90,$10,$2F,$EF,$F0,$90,$01,
  $8C,$74,$08,$F0,$E4,$90,$01,$A7,$F0,$90,$01,$8C,$E0,$30,$E0,$1C,
  $90,$01,$80,$E0,$B4,$02,$15,$A3,$E0,$B4,$01,$10,$90,$01,$84,$E0,
  $B4,$81,$09,$90,$01,$8C,$74,$01,$F0,$12,$0D,$C8,$22,$90,$04,$14,
  $E0,$20,$E7,$03,$02,$15,$01,$90,$70,$12,$E0,$F5,$56,$90,$04,$04,
  $E0,$12,$0A,$9D,$10,$E7,$31,$10,$CB,$36,$11,$10,$50,$11,$47,$51,
  $11,$50,$52,$11,$50,$53,$11,$50,$54,$11,$91,$55,$11,$E0,$56,$12,
  $3E,$70,$12,$69,$71,$12,$97,$72,$13,$4B,$73,$13,$70,$74,$14,$67,
  $80,$14,$8A,$90,$14,$BF,$91,$00,$00,$15,$01,$90,$70,$11,$E0,$F5,
  $3C,$E4,$90,$70,$13,$F0,$90,$04,$14,$74,$80,$F0,$E5,$56,$F4,$70,
  $03,$02,$15,$01,$02,$14,$FA,$20,$02,$03,$30,$03,$1D,$7D,$02,$AF,
  $56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,
  $56,$F4,$70,$03,$02,$15,$01,$02,$14,$FA,$85,$56,$41,$D2,$02,$22,
  $90,$70,$10,$E0,$54,$7F,$FF,$BF,$0A,$0D,$90,$70,$11,$E0,$B4,$08,
  $06,$75,$4E,$01,$75,$4F,$84,$90,$70,$10,$E0,$54,$7F,$FF,$BF,$02,
  $12,$90,$70,$11,$E0,$64,$08,$60,$04,$E0,$B4,$20,$06,$75,$4E,$03,
  $75,$4F,$20,$E4,$F5,$27,$22,$90,$70,$11,$E0,$24,$FF,$92,$47,$22,
  $90,$04,$04,$E0,$25,$E0,$24,$5D,$F5,$57,$90,$70,$10,$E0,$FF,$74,
  $47,$25,$57,$F8,$C6,$EF,$C6,$90,$70,$11,$E0,$FF,$74,$48,$25,$57,
  $F8,$C6,$EF,$C6,$E4,$FD,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,
  $F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$01,$02,$14,
  $FA,$E5,$47,$64,$07,$60,$1D,$E5,$47,$64,$08,$60,$17,$E5,$47,$64,
  $09,$60,$11,$E5,$47,$64,$0A,$60,$0B,$E5,$47,$64,$0B,$60,$05,$E5,
  $47,$B4,$0C,$08,$90,$70,$11,$E0,$54,$0F,$F5,$3A,$E5,$47,$B4,$09,
  $08,$E5,$3A,$B4,$03,$03,$E4,$F5,$46,$E5,$47,$B4,$0A,$08,$E5,$3A,
  $B4,$01,$03,$E4,$F5,$46,$E4,$FD,$AF,$56,$12,$0B,$91,$D2,$04,$22,
  $90,$70,$11,$E0,$F4,$FF,$90,$70,$10,$E0,$5F,$FF,$90,$70,$11,$E0,
  $55,$27,$4F,$90,$70,$18,$F0,$90,$70,$11,$E0,$90,$70,$19,$F0,$E4,
  $FD,$AF,$56,$12,$0B,$91,$30,$15,$04,$D2,$14,$80,$26,$90,$70,$18,
  $E0,$F5,$27,$90,$02,$29,$E0,$FF,$90,$70,$19,$E0,$FE,$EF,$5E,$90,
  $02,$29,$F0,$30,$47,$04,$AF,$27,$80,$04,$E5,$27,$F4,$FF,$90,$02,
  $28,$EF,$F0,$E5,$56,$F4,$70,$03,$02,$15,$01,$02,$14,$FA,$90,$70,
  $10,$E0,$FE,$90,$70,$11,$E0,$FD,$ED,$F8,$E6,$F5,$57,$FD,$AF,$56,
  $12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,
  $F4,$70,$03,$02,$15,$01,$02,$14,$FA,$90,$70,$10,$E0,$FE,$90,$70,
  $11,$E0,$FD,$ED,$F5,$82,$8E,$83,$E0,$F5,$57,$FD,$AF,$56,$12,$0B,
  $91,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,
  $03,$02,$15,$01,$02,$14,$FA,$90,$10,$00,$E0,$F5,$57,$E4,$F5,$58,
  $F5,$59,$90,$10,$03,$E0,$B4,$28,$05,$75,$58,$01,$80,$3C,$90,$10,
  $03,$E0,$B4,$30,$05,$75,$58,$02,$80,$30,$90,$10,$03,$E0,$B4,$33,
  $05,$75,$58,$04,$80,$24,$90,$10,$03,$E0,$B4,$35,$0C,$90,$10,$02,
  $E0,$B4,$72,$05,$75,$58,$08,$80,$11,$90,$10,$03,$E0,$B4,$35,$0A,
  $90,$10,$02,$E0,$B4,$93,$03,$75,$58,$10,$E5,$58,$30,$E1,$19,$90,
  $05,$08,$E0,$44,$01,$F0,$FD,$90,$05,$05,$E0,$54,$FB,$F0,$44,$04,
  $F0,$ED,$54,$FE,$90,$05,$08,$F0,$E4,$F5,$4E,$F5,$4F,$75,$3A,$FF,
  $75,$3C,$FF,$AD,$57,$AF,$56,$12,$0B,$91,$90,$70,$34,$74,$31,$F0,
  $A3,$74,$26,$F0,$C2,$16,$E4,$F5,$5C,$F5,$5A,$F5,$5B,$90,$70,$30,
  $F0,$A3,$F0,$C2,$17,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,
  $E5,$56,$F4,$70,$03,$02,$15,$01,$02,$14,$FA,$90,$70,$10,$E0,$24,
  $FF,$92,$93,$E4,$FD,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,
  $E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$01,$02,$14,$FA,
  $90,$10,$00,$E0,$90,$10,$2C,$F0,$90,$10,$2F,$74,$40,$F0,$90,$70,
  $11,$E0,$54,$7F,$F5,$57,$E0,$54,$80,$90,$70,$32,$F0,$90,$70,$10,
  $E0,$FF,$E5,$57,$D3,$9F,$40,$42,$90,$70,$33,$E5,$57,$F0,$90,$70,
  $10,$E0,$FF,$90,$70,$33,$E0,$D3,$9F,$40,$71,$E0,$FF,$90,$70,$32,
  $E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,$17,$80,$02,
  $7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$FF,$12,
  $0D,$BA,$90,$70,$33,$E0,$14,$F0,$80,$C4,$90,$70,$33,$E5,$57,$F0,
  $90,$70,$10,$E0,$FF,$90,$70,$33,$E0,$C3,$9F,$50,$2F,$E0,$FF,$90,
  $70,$32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,$17,
  $80,$02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,$F0,
  $FF,$12,$0D,$BA,$90,$70,$33,$E0,$04,$F0,$80,$C4,$90,$70,$10,$E0,
  $FF,$90,$70,$32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,
  $7F,$17,$80,$02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,
  $03,$F0,$FF,$12,$0D,$BA,$90,$10,$00,$E0,$90,$10,$2C,$F0,$90,$10,
  $2F,$74,$7F,$F0,$E4,$FD,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,
  $F0,$E4,$90,$70,$13,$F0,$22,$90,$70,$10,$E0,$24,$FF,$92,$4A,$D2,
  $05,$AD,$57,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,
  $70,$13,$F0,$E5,$56,$F4,$60,$79,$80,$70,$90,$70,$10,$E0,$24,$FF,
  $92,$16,$90,$70,$11,$E0,$F5,$5C,$AD,$57,$AF,$56,$12,$0B,$91,$90,
  $04,$14,$74,$80,$F0,$90,$70,$30,$E5,$5A,$F0,$A3,$E5,$5B,$F0,$E4,
  $F5,$5A,$F5,$5B,$90,$70,$13,$F0,$E5,$56,$F4,$60,$44,$80,$3B,$90,
  $70,$11,$E0,$24,$FF,$92,$17,$90,$70,$10,$E0,$F5,$5D,$AD,$57,$AF,
  $56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$30,$17,$13,$90,$10,$00,
  $E0,$90,$10,$2C,$F0,$90,$10,$2F,$E0,$54,$F0,$F5,$57,$45,$5D,$F0,
  $E4,$90,$70,$13,$F0,$E5,$56,$F4,$60,$07,$90,$70,$25,$E0,$44,$01,
  $F0,$22,$22,$E5,$53,$70,$1A,$30,$60,$09,$B2,$4D,$30,$4D,$04,$05,
  $46,$C2,$04,$E5,$4F,$45,$4E,$60,$08,$E5,$4F,$15,$4F,$70,$02,$15,
  $4E,$22,$22,$C2,$42,$D3,$22,$30,$14,$30,$90,$70,$19,$E0,$55,$27,
  $FF,$90,$70,$18,$E0,$4F,$F5,$27,$90,$02,$29,$E0,$FF,$90,$70,$19,
  $E0,$FE,$EF,$5E,$90,$02,$29,$F0,$30,$47,$04,$AF,$27,$80,$04,$E5,
  $27,$F4,$FF,$90,$02,$28,$EF,$F0,$C2,$14,$30,$16,$60,$C2,$AF,$90,
  $10,$04,$E0,$F5,$57,$90,$02,$28,$E0,$54,$05,$F5,$57,$E5,$5C,$64,
  $01,$70,$21,$E5,$57,$90,$10,$04,$30,$E0,$06,$E0,$54,$FB,$F0,$80,
  $04,$E0,$44,$04,$F0,$E5,$57,$30,$E2,$31,$05,$5B,$E5,$5B,$70,$2B,
  $05,$5A,$80,$27,$E5,$57,$30,$E0,$1B,$E5,$5C,$90,$10,$04,$70,$06,
  $E0,$54,$FB,$F0,$80,$04,$E0,$44,$04,$F0,$05,$5B,$E5,$5B,$70,$0B,
  $05,$5A,$80,$07,$90,$10,$04,$E0,$44,$04,$F0,$D2,$AF,$22,$90,$10,
  $1C,$ED,$F0,$A3,$EF,$F0,$A3,$74,$0A,$F0,$90,$10,$1C,$E0,$F5,$58,
  $90,$10,$1E,$E0,$20,$E1,$F3,$22,$90,$10,$1D,$EF,$F0,$A3,$74,$0B,
  $F0,$90,$10,$1C,$E0,$F5,$58,$90,$10,$1E,$E0,$20,$E1,$F3,$AF,$58,
  $22,$C2,$4B,$C2,$4C,$E5,$44,$12,$0A,$9D,$16,$13,$00,$16,$A1,$04,
  $16,$9D,$08,$16,$7D,$10,$16,$27,$20,$16,$47,$60,$16,$58,$A0,$00,
  $00,$16,$A3,$85,$48,$43,$85,$4A,$42,$85,$4C,$5E,$E5,$47,$64,$06,
  $60,$03,$02,$16,$A3,$80,$1B,$E5,$48,$C4,$54,$0F,$F5,$43,$E5,$4A,
  $C4,$54,$0F,$F5,$42,$E5,$4C,$C4,$54,$0F,$F5,$5E,$E5,$47,$64,$06,
  $70,$61,$53,$43,$0F,$80,$5C,$85,$49,$43,$85,$4B,$42,$85,$4D,$5E,
  $E5,$47,$64,$06,$70,$4D,$80,$1B,$E5,$49,$C4,$54,$0F,$F5,$43,$E5,
  $4B,$C4,$54,$0F,$F5,$42,$E5,$4D,$C4,$54,$0F,$F5,$5E,$E5,$47,$64,
  $06,$70,$30,$E5,$43,$54,$0F,$44,$10,$F5,$43,$80,$26,$E5,$47,$64,
  $04,$60,$05,$E5,$47,$B4,$05,$06,$43,$5E,$04,$75,$42,$09,$E5,$47,
  $B4,$06,$10,$E5,$43,$54,$0F,$44,$30,$F5,$43,$80,$06,$D2,$4B,$80,
  $02,$D2,$4C,$E4,$F5,$25,$E5,$42,$C4,$54,$F0,$FF,$E5,$43,$54,$0F,
  $4F,$F5,$5F,$D2,$60,$22,$D2,$15,$E5,$47,$24,$F5,$60,$0B,$24,$CB,
  $60,$07,$24,$40,$70,$06,$C2,$15,$22,$12,$1A,$34,$12,$16,$D8,$C2,
  $15,$C2,$AF,$C2,$04,$D2,$AF,$22,$C2,$AF,$90,$04,$14,$E0,$54,$0E,
  $60,$04,$D2,$18,$80,$08,$E5,$4E,$45,$4F,$24,$FF,$92,$18,$D2,$AF,
  $90,$04,$14,$E0,$A2,$E4,$92,$19,$74,$1E,$F0,$E5,$5F,$54,$0F,$F5,
  $2D,$E5,$25,$70,$13,$30,$18,$05,$E5,$5F,$20,$E5,$0B,$30,$19,$19,
  $E5,$5F,$54,$30,$FF,$BF,$30,$11,$E5,$25,$70,$05,$75,$25,$0C,$80,
  $02,$15,$25,$D2,$6C,$D2,$6D,$80,$0F,$E5,$5F,$30,$E6,$06,$C2,$6C,
  $D2,$6D,$80,$04,$D2,$6C,$C2,$6D,$E5,$47,$64,$03,$70,$21,$30,$4B,
  $06,$C2,$6C,$D2,$6D,$80,$18,$E5,$25,$70,$03,$30,$4C,$11,$C2,$4C,
  $E5,$25,$70,$05,$75,$25,$07,$80,$02,$15,$25,$D2,$6C,$D2,$6D,$E5,
  $47,$B4,$09,$14,$E5,$44,$20,$E3,$0B,$E5,$3A,$64,$02,$60,$05,$E5,
  $3A,$B4,$03,$04,$C2,$6C,$D2,$6D,$E5,$47,$B4,$0A,$13,$E5,$3A,$B4,
  $01,$06,$C2,$6C,$D2,$6D,$80,$08,$E5,$3A,$70,$04,$D2,$6C,$C2,$6D,
  $20,$69,$07,$E5,$5E,$20,$E0,$02,$B2,$68,$20,$6B,$07,$E5,$5E,$20,
  $E1,$02,$B2,$6A,$20,$6D,$07,$E5,$5E,$20,$E2,$02,$B2,$6C,$75,$2E,
  $40,$20,$69,$04,$A2,$68,$80,$26,$30,$68,$06,$E5,$46,$A2,$E2,$80,
  $1D,$E5,$5E,$20,$E0,$04,$7F,$01,$80,$02,$7F,$00,$E5,$46,$54,$F0,
  $FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,$EE,$6F,$24,$FF,$92,$73,
  $92,$72,$20,$6B,$04,$A2,$6A,$80,$26,$30,$6A,$06,$E5,$46,$A2,$E2,
  $80,$1D,$E5,$5E,$20,$E1,$04,$7F,$01,$80,$02,$7F,$00,$E5,$46,$54,
  $F0,$FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,$EE,$6F,$24,$FF,$92,
  $75,$92,$74,$20,$6D,$04,$A2,$6C,$80,$26,$E5,$47,$64,$0A,$70,$22,
  $30,$6C,$06,$E5,$46,$A2,$E3,$80,$17,$E5,$3A,$B4,$01,$06,$E5,$46,
  $A2,$E3,$80,$34,$E5,$46,$20,$E4,$03,$30,$E5,$03,$D3,$80,$01,$C3,
  $80,$26,$30,$6C,$06,$E5,$46,$A2,$E2,$80,$1D,$E5,$5E,$20,$E2,$04,
  $7F,$01,$80,$02,$7F,$00,$E5,$46,$54,$F0,$FE,$BE,$F0,$04,$7E,$01,
  $80,$02,$7E,$00,$EE,$6F,$24,$FF,$92,$71,$92,$70,$90,$10,$00,$E0,
  $90,$10,$2C,$F0,$90,$10,$03,$E0,$C3,$94,$30,$40,$19,$E0,$64,$32,
  $60,$14,$A2,$71,$92,$77,$A2,$70,$92,$76,$E5,$2E,$13,$13,$54,$3F,
  $F5,$2E,$C2,$77,$D2,$76,$30,$17,$0D,$53,$2E,$F0,$E5,$2E,$45,$5D,
  $90,$10,$2F,$F0,$80,$06,$90,$10,$2F,$E5,$2E,$F0,$E5,$47,$64,$06,
  $70,$47,$90,$02,$28,$E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,$8F,$27,
  $90,$02,$29,$E0,$54,$FE,$F0,$E5,$43,$C4,$54,$0F,$14,$60,$0C,$24,
  $FE,$60,$0C,$24,$03,$70,$13,$C2,$38,$80,$0F,$D2,$38,$80,$0B,$E5,
  $46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$38,$30,$47,$05,$AF,$27,$02,
  $1A,$2E,$E5,$27,$F4,$FF,$02,$1A,$2E,$E5,$47,$64,$07,$60,$0F,$E5,
  $47,$64,$08,$60,$09,$E5,$47,$64,$09,$60,$03,$02,$19,$9C,$90,$02,
  $28,$E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,$8F,$27,$90,$02,$29,$E0,
  $54,$FC,$F0,$E5,$3A,$14,$60,$22,$14,$60,$25,$14,$60,$2D,$24,$FC,
  $60,$49,$24,$F9,$60,$14,$24,$0E,$70,$50,$E5,$46,$13,$13,$54,$3F,
  $75,$F0,$03,$84,$E5,$F0,$24,$FF,$80,$3A,$D2,$39,$C2,$38,$80,$3E,
  $E5,$46,$30,$E2,$03,$D3,$80,$1D,$C3,$80,$1A,$E5,$46,$30,$E2,$0D,
  $54,$38,$C3,$94,$30,$50,$06,$7E,$00,$7F,$01,$80,$04,$7E,$00,$7F,
  $00,$EE,$4F,$24,$FF,$92,$38,$C2,$39,$80,$13,$E5,$46,$30,$E2,$03,
  $D3,$80,$01,$C3,$92,$39,$C2,$38,$80,$04,$C2,$38,$C2,$39,$30,$47,
  $04,$AF,$27,$80,$04,$E5,$27,$F4,$FF,$02,$1A,$2E,$E5,$47,$64,$0C,
  $60,$09,$E5,$47,$64,$0B,$60,$03,$02,$1A,$33,$90,$02,$28,$E0,$30,
  $47,$03,$FF,$80,$02,$F4,$FF,$8F,$27,$90,$02,$29,$E0,$54,$FD,$F0,
  $E5,$3A,$14,$60,$20,$14,$60,$21,$14,$60,$2B,$24,$FC,$60,$45,$24,
  $F9,$60,$12,$24,$0E,$70,$4A,$E5,$46,$13,$13,$54,$3F,$75,$F0,$03,
  $84,$E5,$F0,$80,$29,$D2,$39,$80,$3A,$E5,$46,$30,$E2,$03,$D3,$80,
  $01,$C3,$92,$39,$80,$2D,$E5,$46,$30,$E2,$0D,$54,$38,$C3,$94,$30,
  $50,$06,$7E,$00,$7F,$01,$80,$04,$7E,$00,$7F,$00,$EE,$4F,$24,$FF,
  $92,$39,$80,$0F,$E5,$46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$39,$80,
  $02,$C2,$39,$30,$47,$04,$AF,$27,$80,$04,$E5,$27,$F4,$FF,$90,$02,
  $28,$EF,$F0,$22,$E5,$47,$B4,$0B,$10,$90,$02,$29,$E0,$54,$EB,$F0,
  $E5,$27,$54,$EB,$45,$45,$F5,$27,$22,$E4,$90,$02,$29,$F0,$30,$47,
  $04,$AF,$45,$80,$04,$E5,$45,$F4,$FF,$90,$02,$28,$EF,$F0,$22,$8F,
  $50,$D2,$59,$22,$8F,$54,$D2,$58,$22,$E4,$F5,$62,$C2,$AF,$E5,$51,
  $14,$60,$48,$14,$60,$66,$24,$02,$60,$03,$02,$1B,$70,$D2,$59,$75,
  $55,$01,$90,$02,$A2,$E0,$54,$7F,$F0,$A3,$E0,$20,$E7,$23,$90,$04,
  $34,$E0,$B4,$02,$1C,$A3,$E0,$B4,$02,$17,$A3,$E0,$B4,$02,$12,$7F,
  $20,$12,$1A,$5F,$90,$10,$04,$E0,$54,$F3,$F0,$75,$51,$01,$02,$1B,
  $70,$E5,$50,$70,$06,$75,$62,$03,$02,$1B,$70,$90,$12,$00,$E0,$54,
  $03,$70,$12,$7F,$20,$12,$1A,$5F,$90,$02,$A2,$E0,$54,$BF,$F0,$75,
  $51,$02,$02,$1B,$70,$E5,$50,$70,$03,$02,$1B,$6B,$90,$02,$A3,$E0,
  $30,$E6,$03,$02,$1B,$67,$90,$04,$37,$E0,$64,$22,$70,$79,$90,$01,
  $8A,$74,$7E,$F0,$90,$01,$96,$F0,$90,$12,$04,$74,$0A,$F0,$90,$13,
  $28,$E0,$90,$70,$1A,$F0,$90,$13,$29,$E0,$90,$70,$1B,$F0,$90,$13,
  $2B,$E0,$90,$70,$22,$F0,$90,$13,$28,$E0,$54,$F0,$F0,$A3,$E0,$54,
  $F0,$F0,$90,$13,$2B,$E0,$54,$CC,$F0,$E5,$58,$30,$E3,$13,$E5,$3C,
  $F4,$90,$13,$2A,$60,$05,$E0,$54,$F3,$80,$11,$E0,$54,$FB,$F0,$80,
  $14,$E5,$3C,$F4,$90,$13,$2A,$60,$08,$E0,$54,$F2,$45,$3C,$F0,$80,
  $04,$E0,$54,$FA,$F0,$90,$04,$01,$E0,$54,$FD,$F0,$75,$62,$01,$75,
  $55,$02,$E4,$F5,$51,$80,$09,$E5,$50,$70,$05,$75,$62,$03,$F5,$51,
  $E5,$62,$60,$15,$C2,$01,$E4,$F5,$51,$C2,$59,$AD,$62,$AF,$40,$12,
  $1C,$3D,$E5,$62,$B4,$03,$02,$D2,$03,$D2,$AF,$22,$C2,$AF,$30,$01,
  $12,$E4,$90,$01,$96,$F0,$F5,$51,$C2,$59,$C2,$01,$7D,$02,$AF,$40,
  $12,$1C,$3D,$E5,$52,$14,$60,$0C,$04,$60,$03,$02,$1C,$3A,$75,$52,
  $01,$75,$55,$03,$90,$04,$01,$E0,$44,$0E,$F0,$E5,$58,$54,$18,$60,
  $1E,$90,$70,$1A,$E0,$90,$13,$28,$F0,$90,$70,$1B,$E0,$90,$13,$29,
  $F0,$A3,$74,$05,$F0,$90,$70,$22,$E0,$90,$13,$2B,$F0,$80,$11,$90,
  $13,$28,$E0,$44,$0F,$F0,$A3,$E0,$44,$0F,$F0,$A3,$E0,$44,$05,$F0,
  $90,$12,$04,$74,$03,$F0,$E5,$58,$30,$E3,$16,$90,$05,$00,$74,$E2,
  $F0,$A3,$74,$08,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$7F,$01,$12,$0D,
  $2A,$90,$02,$A2,$E0,$44,$C0,$F0,$90,$10,$04,$E0,$44,$0C,$F0,$E4,
  $F5,$52,$F5,$55,$30,$02,$09,$C2,$02,$7D,$01,$AF,$41,$12,$1C,$3D,
  $30,$03,$02,$C2,$03,$E4,$90,$01,$96,$F0,$D2,$AF,$22,$EF,$F4,$60,
  $2D,$E4,$FE,$74,$14,$2E,$F5,$82,$E4,$34,$70,$F5,$83,$E0,$B4,$FF,
  $19,$74,$14,$2E,$F5,$82,$E4,$34,$70,$F5,$83,$EF,$F0,$74,$1C,$2E,
  $F5,$82,$E4,$34,$70,$F5,$83,$ED,$F0,$22,$0E,$BE,$04,$D5,$22,$22,
  $22,$90,$70,$2A,$E0,$30,$E1,$4D,$C2,$AF,$90,$70,$28,$E0,$90,$10,
  $1C,$F0,$90,$70,$29,$E0,$90,$10,$1D,$F0,$90,$70,$2A,$E0,$90,$10,
  $1E,$F0,$90,$10,$1C,$E0,$F5,$62,$90,$10,$1E,$E0,$20,$E1,$F3,$90,
  $10,$1C,$E0,$90,$70,$28,$F0,$90,$10,$1D,$E0,$90,$70,$29,$F0,$90,
  $10,$1E,$E0,$90,$70,$2A,$F0,$30,$4A,$07,$90,$70,$24,$E0,$44,$01,
  $F0,$C2,$05,$D2,$AF,$22,$22,$22,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1D,$C2,$38,
  $FF,$FF,$FF,$02,$10,$28,$02,$10,$32,$02,$10,$7D,$02,$15,$33,$02,
  $15,$34,$02,$16,$01,$02,$16,$06,$12,$16,$02,$22,$02,$1B,$48,$02,
  $1C,$6B,$02,$17,$95,$02,$16,$D0,$30,$05,$06,$20,$0D,$03,$12,$1D,
  $50,$22,$90,$01,$8C,$E0,$30,$E3,$20,$E5,$58,$30,$E0,$10,$E5,$4C,
  $30,$E0,$04,$7F,$40,$80,$02,$7F,$00,$90,$10,$2F,$EF,$F0,$90,$01,
  $8C,$74,$08,$F0,$E4,$90,$01,$A7,$F0,$90,$01,$8C,$E0,$30,$E0,$1C,
  $90,$01,$80,$E0,$B4,$02,$15,$A3,$E0,$B4,$01,$10,$90,$01,$84,$E0,
  $B4,$81,$09,$90,$01,$8C,$74,$01,$F0,$12,$0D,$DD,$22,$90,$04,$14,
  $E0,$20,$E7,$03,$02,$15,$32,$90,$70,$12,$E0,$F5,$56,$90,$04,$04,
  $E0,$12,$0A,$B6,$10,$EA,$31,$10,$CE,$36,$11,$13,$50,$11,$4A,$51,
  $11,$53,$52,$11,$53,$53,$11,$53,$54,$11,$94,$55,$11,$E3,$56,$12,
  $41,$64,$12,$6D,$70,$12,$98,$71,$12,$C6,$72,$13,$7C,$73,$13,$A1,
  $74,$14,$98,$80,$14,$BB,$90,$14,$F0,$91,$00,$00,$15,$32,$90,$70,
  $11,$E0,$F5,$3C,$E4,$90,$70,$13,$F0,$90,$04,$14,$74,$80,$F0,$E5,
  $56,$F4,$70,$03,$02,$15,$32,$02,$15,$2B,$20,$02,$03,$30,$03,$1D,
  $7D,$02,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,
  $13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$32,$02,$15,$2B,$85,$56,$41,
  $D2,$02,$22,$90,$70,$10,$E0,$54,$7F,$FF,$BF,$0A,$0D,$90,$70,$11,
  $E0,$B4,$08,$06,$75,$4E,$01,$75,$4F,$84,$90,$70,$10,$E0,$54,$7F,
  $FF,$BF,$02,$12,$90,$70,$11,$E0,$64,$08,$60,$04,$E0,$B4,$20,$06,
  $75,$4E,$03,$75,$4F,$20,$E4,$F5,$27,$22,$90,$70,$11,$E0,$24,$FF,
  $92,$47,$22,$90,$04,$04,$E0,$25,$E0,$24,$5D,$F5,$57,$90,$70,$10,
  $E0,$FF,$74,$47,$25,$57,$F8,$C6,$EF,$C6,$90,$70,$11,$E0,$FF,$74,
  $48,$25,$57,$F8,$C6,$EF,$C6,$E4,$FD,$AF,$56,$12,$0B,$AA,$90,$04,
  $14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,
  $32,$02,$15,$2B,$E5,$47,$64,$07,$60,$1D,$E5,$47,$64,$08,$60,$17,
  $E5,$47,$64,$09,$60,$11,$E5,$47,$64,$0A,$60,$0B,$E5,$47,$64,$0B,
  $60,$05,$E5,$47,$B4,$0C,$08,$90,$70,$11,$E0,$54,$0F,$F5,$3A,$E5,
  $47,$B4,$09,$08,$E5,$3A,$B4,$03,$03,$E4,$F5,$46,$E5,$47,$B4,$0A,
  $08,$E5,$3A,$B4,$01,$03,$E4,$F5,$46,$E4,$FD,$AF,$56,$12,$0B,$AA,
  $D2,$04,$22,$90,$70,$11,$E0,$F4,$FF,$90,$70,$10,$E0,$5F,$FF,$90,
  $70,$11,$E0,$55,$27,$4F,$90,$70,$18,$F0,$90,$70,$11,$E0,$90,$70,
  $19,$F0,$E4,$FD,$AF,$56,$12,$0B,$AA,$30,$15,$04,$D2,$14,$80,$26,
  $90,$70,$18,$E0,$F5,$27,$90,$02,$29,$E0,$FF,$90,$70,$19,$E0,$FE,
  $EF,$5E,$90,$02,$29,$F0,$30,$47,$04,$AF,$27,$80,$04,$E5,$27,$F4,
  $FF,$90,$02,$28,$EF,$F0,$E5,$56,$F4,$70,$03,$02,$15,$32,$02,$15,
  $2B,$90,$70,$10,$E0,$F5,$2A,$75,$3D,$03,$75,$3E,$1F,$E4,$F5,$2F,
  $AD,$57,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,
  $13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$32,$02,$15,$2B,$90,$70,$10,
  $E0,$FE,$90,$70,$11,$E0,$FD,$ED,$F8,$E6,$F5,$57,$FD,$AF,$56,$12,
  $0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,
  $70,$03,$02,$15,$32,$02,$15,$2B,$90,$70,$10,$E0,$FE,$90,$70,$11,
  $E0,$FD,$ED,$F5,$82,$8E,$83,$E0,$F5,$57,$FD,$AF,$56,$12,$0B,$AA,
  $90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,
  $02,$15,$32,$02,$15,$2B,$90,$10,$00,$E0,$F5,$57,$E4,$F5,$58,$F5,
  $59,$90,$10,$03,$E0,$B4,$28,$05,$75,$58,$01,$80,$3C,$90,$10,$03,
  $E0,$B4,$30,$05,$75,$58,$02,$80,$30,$90,$10,$03,$E0,$B4,$33,$05,
  $75,$58,$04,$80,$24,$90,$10,$03,$E0,$B4,$35,$0C,$90,$10,$02,$E0,
  $B4,$72,$05,$75,$58,$08,$80,$11,$90,$10,$03,$E0,$B4,$35,$0A,$90,
  $10,$02,$E0,$B4,$93,$03,$75,$58,$10,$E5,$58,$30,$E1,$19,$90,$05,
  $08,$E0,$44,$01,$F0,$FD,$90,$05,$05,$E0,$54,$FB,$F0,$44,$04,$F0,
  $ED,$54,$FE,$90,$05,$08,$F0,$E4,$F5,$4E,$F5,$4F,$75,$3A,$FF,$75,
  $3C,$FF,$AD,$57,$AF,$56,$12,$0B,$AA,$90,$70,$34,$74,$31,$F0,$A3,
  $74,$26,$F0,$C2,$16,$E4,$F5,$5C,$F5,$5A,$F5,$5B,$90,$70,$30,$F0,
  $A3,$F0,$F5,$2A,$C2,$17,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,
  $F0,$E5,$56,$F4,$70,$03,$02,$15,$32,$02,$15,$2B,$90,$70,$10,$E0,
  $24,$FF,$92,$93,$E4,$FD,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,
  $F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$32,$02,$15,
  $2B,$90,$10,$00,$E0,$90,$10,$2C,$F0,$90,$10,$2F,$74,$40,$F0,$90,
  $70,$11,$E0,$54,$7F,$F5,$57,$E0,$54,$80,$90,$70,$32,$F0,$90,$70,
  $10,$E0,$FF,$E5,$57,$D3,$9F,$40,$42,$90,$70,$33,$E5,$57,$F0,$90,
  $70,$10,$E0,$FF,$90,$70,$33,$E0,$D3,$9F,$40,$71,$E0,$FF,$90,$70,
  $32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,$17,$80,
  $02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$FF,
  $12,$0D,$CF,$90,$70,$33,$E0,$14,$F0,$80,$C4,$90,$70,$33,$E5,$57,
  $F0,$90,$70,$10,$E0,$FF,$90,$70,$33,$E0,$C3,$9F,$50,$2F,$E0,$FF,
  $90,$70,$32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,
  $17,$80,$02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,
  $F0,$FF,$12,$0D,$CF,$90,$70,$33,$E0,$04,$F0,$80,$C4,$90,$70,$10,
  $E0,$FF,$90,$70,$32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,
  $04,$7F,$17,$80,$02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,
  $74,$03,$F0,$FF,$12,$0D,$CF,$90,$10,$00,$E0,$90,$10,$2C,$F0,$90,
  $10,$2F,$74,$7F,$F0,$E4,$FD,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,
  $80,$F0,$E4,$90,$70,$13,$F0,$22,$90,$70,$10,$E0,$24,$FF,$92,$4A,
  $D2,$05,$AD,$57,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,
  $90,$70,$13,$F0,$E5,$56,$F4,$60,$79,$80,$70,$90,$70,$10,$E0,$24,
  $FF,$92,$16,$90,$70,$11,$E0,$F5,$5C,$AD,$57,$AF,$56,$12,$0B,$AA,
  $90,$04,$14,$74,$80,$F0,$90,$70,$30,$E5,$5A,$F0,$A3,$E5,$5B,$F0,
  $E4,$F5,$5A,$F5,$5B,$90,$70,$13,$F0,$E5,$56,$F4,$60,$44,$80,$3B,
  $90,$70,$11,$E0,$24,$FF,$92,$17,$90,$70,$10,$E0,$F5,$5D,$AD,$57,
  $AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$30,$17,$13,$90,$10,
  $00,$E0,$90,$10,$2C,$F0,$90,$10,$2F,$E0,$54,$F0,$F5,$57,$45,$5D,
  $F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$60,$07,$90,$70,$25,$E0,$44,
  $01,$F0,$22,$22,$E5,$3E,$45,$3D,$60,$0A,$E5,$3E,$15,$3E,$70,$0A,
  $15,$3D,$80,$06,$75,$3D,$03,$75,$3E,$1F,$E5,$3E,$45,$3D,$60,$03,
  $02,$15,$E2,$E5,$2A,$70,$03,$02,$15,$E2,$74,$A0,$25,$2F,$F5,$82,
  $E4,$34,$4C,$F5,$83,$E0,$60,$7A,$7F,$7E,$12,$16,$B7,$EF,$54,$FE,
  $44,$02,$FD,$7F,$7E,$12,$16,$9D,$E5,$2F,$7F,$00,$25,$E0,$FE,$EF,
  $24,$00,$F5,$82,$74,$4D,$3E,$AF,$82,$90,$4C,$A8,$F0,$A3,$EF,$F0,
  $E4,$F5,$56,$F5,$57,$7F,$7F,$12,$16,$B7,$90,$4C,$A8,$E0,$FA,$A3,
  $E0,$25,$57,$F5,$82,$EA,$35,$56,$F5,$83,$EF,$F0,$05,$57,$E5,$57,
  $70,$02,$05,$56,$C3,$94,$80,$E5,$56,$94,$01,$40,$D8,$7F,$7E,$12,
  $16,$B7,$EF,$44,$03,$FD,$7F,$7E,$12,$16,$9D,$74,$A0,$25,$2F,$F5,
  $82,$E4,$34,$4C,$F5,$83,$E4,$F0,$05,$2F,$E5,$2F,$B4,$08,$03,$E4,
  $F5,$2F,$E5,$53,$70,$1A,$30,$60,$09,$B2,$4D,$30,$4D,$04,$05,$46,
  $C2,$04,$E5,$4F,$45,$4E,$60,$08,$E5,$4F,$15,$4F,$70,$02,$15,$4E,
  $22,$22,$C2,$42,$D3,$22,$30,$14,$30,$90,$70,$19,$E0,$55,$27,$FF,
  $90,$70,$18,$E0,$4F,$F5,$27,$90,$02,$29,$E0,$FF,$90,$70,$19,$E0,
  $FE,$EF,$5E,$90,$02,$29,$F0,$30,$47,$04,$AF,$27,$80,$04,$E5,$27,
  $F4,$FF,$90,$02,$28,$EF,$F0,$C2,$14,$30,$16,$60,$C2,$AF,$90,$10,
  $04,$E0,$F5,$57,$90,$02,$28,$E0,$54,$05,$F5,$57,$E5,$5C,$64,$01,
  $70,$21,$E5,$57,$90,$10,$04,$30,$E0,$06,$E0,$54,$FB,$F0,$80,$04,
  $E0,$44,$04,$F0,$E5,$57,$30,$E2,$31,$05,$5B,$E5,$5B,$70,$2B,$05,
  $5A,$80,$27,$E5,$57,$30,$E0,$1B,$E5,$5C,$90,$10,$04,$70,$06,$E0,
  $54,$FB,$F0,$80,$04,$E0,$44,$04,$F0,$05,$5B,$E5,$5B,$70,$0B,$05,
  $5A,$80,$07,$90,$10,$04,$E0,$44,$04,$F0,$D2,$AF,$22,$90,$10,$1C,
  $ED,$F0,$A3,$EF,$F0,$A3,$74,$0A,$F0,$90,$10,$1C,$E0,$F5,$58,$90,
  $10,$1E,$E0,$20,$E1,$F3,$22,$90,$10,$1D,$EF,$F0,$A3,$74,$0B,$F0,
  $90,$10,$1C,$E0,$F5,$58,$90,$10,$1E,$E0,$20,$E1,$F3,$AF,$58,$22,
  $C2,$4B,$C2,$4C,$E5,$44,$12,$0A,$B6,$16,$F2,$00,$17,$80,$04,$17,
  $7C,$08,$17,$5C,$10,$17,$06,$20,$17,$26,$60,$17,$37,$A0,$00,$00,
  $17,$82,$85,$48,$43,$85,$4A,$42,$85,$4C,$5E,$E5,$47,$64,$06,$60,
  $03,$02,$17,$82,$80,$1B,$E5,$48,$C4,$54,$0F,$F5,$43,$E5,$4A,$C4,
  $54,$0F,$F5,$42,$E5,$4C,$C4,$54,$0F,$F5,$5E,$E5,$47,$64,$06,$70,
  $61,$53,$43,$0F,$80,$5C,$85,$49,$43,$85,$4B,$42,$85,$4D,$5E,$E5,
  $47,$64,$06,$70,$4D,$80,$1B,$E5,$49,$C4,$54,$0F,$F5,$43,$E5,$4B,
  $C4,$54,$0F,$F5,$42,$E5,$4D,$C4,$54,$0F,$F5,$5E,$E5,$47,$64,$06,
  $70,$30,$E5,$43,$54,$0F,$44,$10,$F5,$43,$80,$26,$E5,$47,$64,$04,
  $60,$05,$E5,$47,$B4,$05,$06,$43,$5E,$04,$75,$42,$09,$E5,$47,$B4,
  $06,$10,$E5,$43,$54,$0F,$44,$30,$F5,$43,$80,$06,$D2,$4B,$80,$02,
  $D2,$4C,$E4,$F5,$25,$E5,$42,$C4,$54,$F0,$FF,$E5,$43,$54,$0F,$4F,
  $F5,$5F,$D2,$60,$22,$D2,$15,$E5,$47,$24,$F5,$60,$0B,$24,$CB,$60,
  $07,$24,$40,$70,$06,$C2,$15,$22,$12,$1B,$13,$12,$17,$B7,$C2,$15,
  $C2,$AF,$C2,$04,$D2,$AF,$22,$C2,$AF,$90,$04,$14,$E0,$54,$0E,$60,
  $04,$D2,$18,$80,$08,$E5,$4E,$45,$4F,$24,$FF,$92,$18,$D2,$AF,$90,
  $04,$14,$E0,$A2,$E4,$92,$19,$74,$1E,$F0,$E5,$5F,$54,$0F,$F5,$2D,
  $E5,$25,$70,$13,$30,$18,$05,$E5,$5F,$20,$E5,$0B,$30,$19,$19,$E5,
  $5F,$54,$30,$FF,$BF,$30,$11,$E5,$25,$70,$05,$75,$25,$0C,$80,$02,
  $15,$25,$D2,$6C,$D2,$6D,$80,$0F,$E5,$5F,$30,$E6,$06,$C2,$6C,$D2,
  $6D,$80,$04,$D2,$6C,$C2,$6D,$E5,$47,$64,$03,$70,$21,$30,$4B,$06,
  $C2,$6C,$D2,$6D,$80,$18,$E5,$25,$70,$03,$30,$4C,$11,$C2,$4C,$E5,
  $25,$70,$05,$75,$25,$07,$80,$02,$15,$25,$D2,$6C,$D2,$6D,$E5,$47,
  $B4,$09,$14,$E5,$44,$20,$E3,$0B,$E5,$3A,$64,$02,$60,$05,$E5,$3A,
  $B4,$03,$04,$C2,$6C,$D2,$6D,$E5,$47,$B4,$0A,$13,$E5,$3A,$B4,$01,
  $06,$C2,$6C,$D2,$6D,$80,$08,$E5,$3A,$70,$04,$D2,$6C,$C2,$6D,$20,
  $69,$07,$E5,$5E,$20,$E0,$02,$B2,$68,$20,$6B,$07,$E5,$5E,$20,$E1,
  $02,$B2,$6A,$20,$6D,$07,$E5,$5E,$20,$E2,$02,$B2,$6C,$75,$2E,$40,
  $20,$69,$04,$A2,$68,$80,$26,$30,$68,$06,$E5,$46,$A2,$E2,$80,$1D,
  $E5,$5E,$20,$E0,$04,$7F,$01,$80,$02,$7F,$00,$E5,$46,$54,$F0,$FE,
  $BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,$EE,$6F,$24,$FF,$92,$73,$92,
  $72,$20,$6B,$04,$A2,$6A,$80,$26,$30,$6A,$06,$E5,$46,$A2,$E2,$80,
  $1D,$E5,$5E,$20,$E1,$04,$7F,$01,$80,$02,$7F,$00,$E5,$46,$54,$F0,
  $FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,$EE,$6F,$24,$FF,$92,$75,
  $92,$74,$20,$6D,$04,$A2,$6C,$80,$26,$E5,$47,$64,$0A,$70,$22,$30,
  $6C,$06,$E5,$46,$A2,$E3,$80,$17,$E5,$3A,$B4,$01,$06,$E5,$46,$A2,
  $E3,$80,$34,$E5,$46,$20,$E4,$03,$30,$E5,$03,$D3,$80,$01,$C3,$80,
  $26,$30,$6C,$06,$E5,$46,$A2,$E2,$80,$1D,$E5,$5E,$20,$E2,$04,$7F,
  $01,$80,$02,$7F,$00,$E5,$46,$54,$F0,$FE,$BE,$F0,$04,$7E,$01,$80,
  $02,$7E,$00,$EE,$6F,$24,$FF,$92,$71,$92,$70,$90,$10,$00,$E0,$90,
  $10,$2C,$F0,$90,$10,$03,$E0,$C3,$94,$30,$40,$19,$E0,$64,$32,$60,
  $14,$A2,$71,$92,$77,$A2,$70,$92,$76,$E5,$2E,$13,$13,$54,$3F,$F5,
  $2E,$C2,$77,$D2,$76,$30,$17,$0D,$53,$2E,$F0,$E5,$2E,$45,$5D,$90,
  $10,$2F,$F0,$80,$06,$90,$10,$2F,$E5,$2E,$F0,$E5,$47,$64,$06,$70,
  $47,$90,$02,$28,$E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,$8F,$27,$90,
  $02,$29,$E0,$54,$FE,$F0,$E5,$43,$C4,$54,$0F,$14,$60,$0C,$24,$FE,
  $60,$0C,$24,$03,$70,$13,$C2,$38,$80,$0F,$D2,$38,$80,$0B,$E5,$46,
  $30,$E2,$03,$D3,$80,$01,$C3,$92,$38,$30,$47,$05,$AF,$27,$02,$1B,
  $0D,$E5,$27,$F4,$FF,$02,$1B,$0D,$E5,$47,$64,$07,$60,$0F,$E5,$47,
  $64,$08,$60,$09,$E5,$47,$64,$09,$60,$03,$02,$1A,$7B,$90,$02,$28,
  $E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,$8F,$27,$90,$02,$29,$E0,$54,
  $FC,$F0,$E5,$3A,$14,$60,$22,$14,$60,$25,$14,$60,$2D,$24,$FC,$60,
  $49,$24,$F9,$60,$14,$24,$0E,$70,$50,$E5,$46,$13,$13,$54,$3F,$75,
  $F0,$03,$84,$E5,$F0,$24,$FF,$80,$3A,$D2,$39,$C2,$38,$80,$3E,$E5,
  $46,$30,$E2,$03,$D3,$80,$1D,$C3,$80,$1A,$E5,$46,$30,$E2,$0D,$54,
  $38,$C3,$94,$30,$50,$06,$7E,$00,$7F,$01,$80,$04,$7E,$00,$7F,$00,
  $EE,$4F,$24,$FF,$92,$38,$C2,$39,$80,$13,$E5,$46,$30,$E2,$03,$D3,
  $80,$01,$C3,$92,$39,$C2,$38,$80,$04,$C2,$38,$C2,$39,$30,$47,$04,
  $AF,$27,$80,$04,$E5,$27,$F4,$FF,$02,$1B,$0D,$E5,$47,$64,$0C,$60,
  $09,$E5,$47,$64,$0B,$60,$03,$02,$1B,$12,$90,$02,$28,$E0,$30,$47,
  $03,$FF,$80,$02,$F4,$FF,$8F,$27,$90,$02,$29,$E0,$54,$FD,$F0,$E5,
  $3A,$14,$60,$20,$14,$60,$21,$14,$60,$2B,$24,$FC,$60,$45,$24,$F9,
  $60,$12,$24,$0E,$70,$4A,$E5,$46,$13,$13,$54,$3F,$75,$F0,$03,$84,
  $E5,$F0,$80,$29,$D2,$39,$80,$3A,$E5,$46,$30,$E2,$03,$D3,$80,$01,
  $C3,$92,$39,$80,$2D,$E5,$46,$30,$E2,$0D,$54,$38,$C3,$94,$30,$50,
  $06,$7E,$00,$7F,$01,$80,$04,$7E,$00,$7F,$00,$EE,$4F,$24,$FF,$92,
  $39,$80,$0F,$E5,$46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$39,$80,$02,
  $C2,$39,$30,$47,$04,$AF,$27,$80,$04,$E5,$27,$F4,$FF,$90,$02,$28,
  $EF,$F0,$22,$E5,$47,$B4,$0B,$10,$90,$02,$29,$E0,$54,$EB,$F0,$E5,
  $27,$54,$EB,$45,$45,$F5,$27,$22,$E4,$90,$02,$29,$F0,$30,$47,$04,
  $AF,$45,$80,$04,$E5,$45,$F4,$FF,$90,$02,$28,$EF,$F0,$22,$8F,$50,
  $D2,$59,$22,$8F,$54,$D2,$58,$22,$E4,$F5,$62,$C2,$AF,$E5,$51,$14,
  $60,$48,$14,$60,$66,$24,$02,$60,$03,$02,$1C,$4F,$D2,$59,$75,$55,
  $01,$90,$02,$A2,$E0,$54,$7F,$F0,$A3,$E0,$20,$E7,$23,$90,$04,$34,
  $E0,$B4,$02,$1C,$A3,$E0,$B4,$02,$17,$A3,$E0,$B4,$02,$12,$7F,$20,
  $12,$1B,$3E,$90,$10,$04,$E0,$54,$F3,$F0,$75,$51,$01,$02,$1C,$4F,
  $E5,$50,$70,$06,$75,$62,$03,$02,$1C,$4F,$90,$12,$00,$E0,$54,$03,
  $70,$12,$7F,$20,$12,$1B,$3E,$90,$02,$A2,$E0,$54,$BF,$F0,$75,$51,
  $02,$02,$1C,$4F,$E5,$50,$70,$03,$02,$1C,$4A,$90,$02,$A3,$E0,$30,
  $E6,$03,$02,$1C,$46,$90,$04,$37,$E0,$64,$22,$70,$79,$90,$01,$8A,
  $74,$7E,$F0,$90,$01,$96,$F0,$90,$12,$04,$74,$0A,$F0,$90,$13,$28,
  $E0,$90,$70,$1A,$F0,$90,$13,$29,$E0,$90,$70,$1B,$F0,$90,$13,$2B,
  $E0,$90,$70,$22,$F0,$90,$13,$28,$E0,$54,$F0,$F0,$A3,$E0,$54,$F0,
  $F0,$90,$13,$2B,$E0,$54,$CC,$F0,$E5,$58,$30,$E3,$13,$E5,$3C,$F4,
  $90,$13,$2A,$60,$05,$E0,$54,$F3,$80,$11,$E0,$54,$FB,$F0,$80,$14,
  $E5,$3C,$F4,$90,$13,$2A,$60,$08,$E0,$54,$F2,$45,$3C,$F0,$80,$04,
  $E0,$54,$FA,$F0,$90,$04,$01,$E0,$54,$FD,$F0,$75,$62,$01,$75,$55,
  $02,$E4,$F5,$51,$80,$09,$E5,$50,$70,$05,$75,$62,$03,$F5,$51,$E5,
  $62,$60,$15,$C2,$01,$E4,$F5,$51,$C2,$59,$AD,$62,$AF,$40,$12,$1D,
  $1C,$E5,$62,$B4,$03,$02,$D2,$03,$D2,$AF,$22,$C2,$AF,$30,$01,$12,
  $E4,$90,$01,$96,$F0,$F5,$51,$C2,$59,$C2,$01,$7D,$02,$AF,$40,$12,
  $1D,$1C,$E5,$52,$14,$60,$0C,$04,$60,$03,$02,$1D,$19,$75,$52,$01,
  $75,$55,$03,$90,$04,$01,$E0,$44,$0E,$F0,$E5,$58,$54,$18,$60,$1E,
  $90,$70,$1A,$E0,$90,$13,$28,$F0,$90,$70,$1B,$E0,$90,$13,$29,$F0,
  $A3,$74,$05,$F0,$90,$70,$22,$E0,$90,$13,$2B,$F0,$80,$11,$90,$13,
  $28,$E0,$44,$0F,$F0,$A3,$E0,$44,$0F,$F0,$A3,$E0,$44,$05,$F0,$90,
  $12,$04,$74,$03,$F0,$E5,$58,$30,$E3,$16,$90,$05,$00,$74,$E2,$F0,
  $A3,$74,$08,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$7F,$01,$12,$0D,$48,
  $90,$02,$A2,$E0,$44,$C0,$F0,$90,$10,$04,$E0,$44,$0C,$F0,$E4,$F5,
  $52,$F5,$55,$30,$02,$09,$C2,$02,$7D,$01,$AF,$41,$12,$1D,$1C,$30,
  $03,$02,$C2,$03,$E4,$90,$01,$96,$F0,$D2,$AF,$22,$EF,$F4,$60,$2D,
  $E4,$FE,$74,$14,$2E,$F5,$82,$E4,$34,$70,$F5,$83,$E0,$B4,$FF,$19,
  $74,$14,$2E,$F5,$82,$E4,$34,$70,$F5,$83,$EF,$F0,$74,$1C,$2E,$F5,
  $82,$E4,$34,$70,$F5,$83,$ED,$F0,$22,$0E,$BE,$04,$D5,$22,$22,$22,
  $90,$70,$2A,$E0,$30,$E1,$4D,$C2,$AF,$90,$70,$28,$E0,$90,$10,$1C,
  $F0,$90,$70,$29,$E0,$90,$10,$1D,$F0,$90,$70,$2A,$E0,$90,$10,$1E,
  $F0,$90,$10,$1C,$E0,$F5,$62,$90,$10,$1E,$E0,$20,$E1,$F3,$90,$10,
  $1C,$E0,$90,$70,$28,$F0,$90,$10,$1D,$E0,$90,$70,$29,$F0,$90,$10,
  $1E,$E0,$90,$70,$2A,$F0,$30,$4A,$07,$90,$70,$24,$E0,$44,$01,$F0,
  $C2,$05,$D2,$AF,$22,$22,$22,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1D,$84,$C2
 );
 
 RT2800USBFirmware:array[0..8191] of Byte = ( //To Do //rt2870.bin from DPO_RT5572_LinuxSTA_2.6.1.3_20121022 (Ralink download)
  $FF,$FF,$FF,$02,$10,$28,$02,$10,$3B,$02,$10,$86,$02,$15,$5F,$02,
  $15,$60,$02,$16,$44,$02,$16,$8D,$12,$16,$45,$22,$02,$1B,$CF,$02,
  $1C,$F8,$02,$18,$1C,$02,$17,$57,$30,$05,$06,$20,$0D,$03,$12,$1D,
  $DD,$30,$07,$06,$20,$0F,$03,$12,$1E,$32,$22,$90,$01,$8C,$E0,$30,
  $E3,$20,$E5,$58,$30,$E0,$10,$E5,$4C,$30,$E0,$04,$7F,$40,$80,$02,
  $7F,$00,$90,$10,$2F,$EF,$F0,$90,$01,$8C,$74,$08,$F0,$E4,$90,$01,
  $A7,$F0,$90,$01,$8C,$E0,$30,$E0,$1C,$90,$01,$80,$E0,$B4,$02,$15,
  $A3,$E0,$B4,$01,$10,$90,$01,$84,$E0,$B4,$81,$09,$90,$01,$8C,$74,
  $01,$F0,$12,$0D,$C8,$22,$90,$04,$14,$E0,$20,$E7,$03,$02,$15,$5E,
  $90,$70,$12,$E0,$F5,$56,$90,$04,$04,$E0,$12,$0A,$9D,$10,$F3,$31,
  $10,$D7,$36,$11,$1C,$50,$11,$53,$51,$11,$5C,$52,$11,$5C,$53,$11,
  $5C,$54,$11,$9D,$55,$11,$EC,$56,$12,$AF,$64,$12,$4A,$65,$12,$75,
  $66,$12,$DB,$72,$13,$A3,$73,$13,$C8,$74,$14,$C4,$80,$14,$E7,$90,
  $15,$1C,$91,$00,$00,$15,$5E,$90,$70,$11,$E0,$F5,$3C,$E4,$90,$70,
  $13,$F0,$90,$04,$14,$74,$80,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,
  $02,$15,$57,$20,$02,$03,$30,$03,$1D,$7D,$02,$AF,$56,$12,$0B,$91,
  $90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,
  $02,$15,$5E,$02,$15,$57,$85,$56,$41,$D2,$02,$22,$90,$70,$10,$E0,
  $54,$7F,$FF,$BF,$0A,$0D,$90,$70,$11,$E0,$B4,$08,$06,$75,$4E,$01,
  $75,$4F,$84,$90,$70,$10,$E0,$54,$7F,$FF,$BF,$02,$12,$90,$70,$11,
  $E0,$64,$08,$60,$04,$E0,$B4,$20,$06,$75,$4E,$03,$75,$4F,$20,$E4,
  $F5,$3F,$22,$90,$70,$11,$E0,$24,$FF,$92,$47,$22,$90,$04,$04,$E0,
  $25,$E0,$24,$5D,$F5,$57,$90,$70,$10,$E0,$FF,$74,$47,$25,$57,$F8,
  $C6,$EF,$C6,$90,$70,$11,$E0,$FF,$74,$48,$25,$57,$F8,$C6,$EF,$C6,
  $E4,$FD,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,
  $13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$E5,$47,$64,
  $07,$60,$1D,$E5,$47,$64,$08,$60,$17,$E5,$47,$64,$09,$60,$11,$E5,
  $47,$64,$0A,$60,$0B,$E5,$47,$64,$0B,$60,$05,$E5,$47,$B4,$0C,$08,
  $90,$70,$11,$E0,$54,$0F,$F5,$3A,$E5,$47,$B4,$09,$08,$E5,$3A,$B4,
  $03,$03,$E4,$F5,$46,$E5,$47,$B4,$0A,$08,$E5,$3A,$B4,$01,$03,$E4,
  $F5,$46,$E4,$FD,$AF,$56,$12,$0B,$91,$D2,$04,$22,$90,$70,$11,$E0,
  $F4,$FF,$90,$70,$10,$E0,$5F,$FF,$90,$70,$11,$E0,$55,$3F,$4F,$90,
  $70,$18,$F0,$90,$70,$11,$E0,$90,$70,$19,$F0,$E4,$FD,$AF,$56,$12,
  $0B,$91,$30,$15,$04,$D2,$14,$80,$26,$90,$70,$18,$E0,$F5,$3F,$90,
  $02,$29,$E0,$FF,$90,$70,$19,$E0,$FE,$EF,$5E,$90,$02,$29,$F0,$30,
  $47,$04,$AF,$3F,$80,$04,$E5,$3F,$F4,$FF,$90,$02,$28,$EF,$F0,$E5,
  $56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$90,$70,$10,$E0,$13,$92,
  $1B,$C2,$1C,$AD,$57,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,
  $30,$1B,$02,$D2,$07,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,
  $15,$5E,$02,$15,$57,$90,$4C,$B1,$E0,$90,$4C,$BE,$F0,$90,$4C,$B8,
  $E0,$90,$4C,$BF,$F0,$E4,$90,$4C,$B1,$F0,$90,$4C,$B9,$E0,$90,$4C,
  $B8,$F0,$AD,$57,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,
  $90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$90,
  $70,$10,$E0,$F5,$2A,$75,$3D,$03,$75,$3E,$1F,$E4,$F5,$2F,$AD,$57,
  $AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,
  $E5,$56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$90,$10,$00,$E0,$F5,
  $57,$E4,$F5,$58,$F5,$59,$90,$10,$03,$E0,$B4,$28,$05,$75,$58,$01,
  $80,$3C,$90,$10,$03,$E0,$B4,$30,$05,$75,$58,$02,$80,$30,$90,$10,
  $03,$E0,$B4,$33,$05,$75,$58,$04,$80,$24,$90,$10,$03,$E0,$B4,$35,
  $0C,$90,$10,$02,$E0,$B4,$72,$05,$75,$58,$08,$80,$11,$90,$10,$03,
  $E0,$B4,$35,$0A,$90,$10,$02,$E0,$B4,$93,$03,$75,$58,$10,$E5,$58,
  $30,$E1,$19,$90,$05,$08,$E0,$44,$01,$F0,$FD,$90,$05,$05,$E0,$54,
  $FB,$F0,$44,$04,$F0,$ED,$54,$FE,$90,$05,$08,$F0,$75,$3B,$18,$E4,
  $F5,$26,$F5,$27,$C2,$1C,$C2,$1B,$F5,$4E,$F5,$4F,$75,$3A,$FF,$75,
  $3C,$FF,$AD,$57,$AF,$56,$12,$0B,$91,$90,$70,$36,$74,$37,$F0,$A3,
  $74,$32,$F0,$90,$04,$01,$E0,$44,$01,$F0,$C2,$16,$E4,$F5,$5C,$F5,
  $5A,$F5,$5B,$90,$70,$30,$F0,$A3,$F0,$F5,$2A,$C2,$17,$90,$04,$14,
  $74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,
  $02,$15,$57,$90,$70,$10,$E0,$24,$FF,$92,$93,$E4,$FD,$AF,$56,$12,
  $0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,
  $70,$03,$02,$15,$5E,$02,$15,$57,$90,$10,$00,$E0,$90,$10,$2C,$F0,
  $90,$10,$2F,$74,$40,$F0,$90,$70,$11,$E0,$54,$7F,$F5,$57,$E0,$54,
  $80,$90,$70,$32,$F0,$90,$70,$10,$E0,$FF,$E5,$57,$D3,$9F,$40,$43,
  $90,$70,$33,$E5,$57,$F0,$90,$70,$10,$E0,$FF,$90,$70,$33,$E0,$C3,
  $9F,$D3,$94,$04,$40,$73,$E0,$24,$FC,$F0,$E0,$FF,$90,$70,$32,$E0,
  $4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,$17,$80,$02,$7F,
  $11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$FF,$12,$0D,
  $2A,$80,$C3,$90,$70,$33,$E5,$57,$F0,$90,$70,$33,$E0,$FF,$90,$70,
  $10,$E0,$C3,$9F,$D3,$94,$04,$40,$30,$90,$70,$33,$E0,$24,$04,$F0,
  $E0,$FF,$90,$70,$32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,
  $04,$7F,$17,$80,$02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,
  $74,$03,$F0,$FF,$12,$0D,$2A,$80,$C0,$90,$70,$10,$E0,$FF,$90,$70,
  $32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,$17,$80,
  $02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$FF,
  $12,$0D,$2A,$90,$10,$00,$E0,$90,$10,$2C,$F0,$90,$10,$2F,$74,$7F,
  $F0,$E4,$FD,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,
  $70,$13,$F0,$22,$90,$70,$10,$E0,$24,$FF,$92,$4A,$D2,$05,$AD,$57,
  $AF,$56,$12,$0B,$91,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,
  $E5,$56,$F4,$60,$79,$80,$70,$90,$70,$10,$E0,$24,$FF,$92,$16,$90,
  $70,$11,$E0,$F5,$5C,$AD,$57,$AF,$56,$12,$0B,$91,$90,$04,$14,$74,
  $80,$F0,$90,$70,$30,$E5,$5A,$F0,$A3,$E5,$5B,$F0,$E4,$F5,$5A,$F5,
  $5B,$90,$70,$13,$F0,$E5,$56,$F4,$60,$44,$80,$3B,$90,$70,$11,$E0,
  $24,$FF,$92,$17,$90,$70,$10,$E0,$F5,$5D,$AD,$57,$AF,$56,$12,$0B,
  $91,$90,$04,$14,$74,$80,$F0,$30,$17,$13,$90,$10,$00,$E0,$90,$10,
  $2C,$F0,$90,$10,$2F,$E0,$54,$F0,$F5,$57,$45,$5D,$F0,$E4,$90,$70,
  $13,$F0,$E5,$56,$F4,$60,$07,$90,$70,$25,$E0,$44,$01,$F0,$22,$22,
  $E5,$3E,$45,$3D,$60,$0A,$E5,$3E,$15,$3E,$70,$0A,$15,$3D,$80,$06,
  $75,$3D,$03,$75,$3E,$1F,$E5,$3E,$45,$3D,$60,$03,$02,$16,$0E,$E5,
  $2A,$70,$03,$02,$16,$0E,$74,$A0,$25,$2F,$F5,$82,$E4,$34,$4C,$F5,
  $83,$E0,$60,$7A,$7F,$7E,$12,$17,$3E,$EF,$54,$FE,$44,$02,$FD,$7F,
  $7E,$12,$17,$24,$E5,$2F,$7F,$00,$25,$E0,$FE,$EF,$24,$00,$F5,$82,
  $74,$4D,$3E,$AF,$82,$90,$4C,$A8,$F0,$A3,$EF,$F0,$E4,$F5,$56,$F5,
  $57,$7F,$7F,$12,$17,$3E,$90,$4C,$A8,$E0,$FA,$A3,$E0,$25,$57,$F5,
  $82,$EA,$35,$56,$F5,$83,$EF,$F0,$05,$57,$E5,$57,$70,$02,$05,$56,
  $C3,$94,$80,$E5,$56,$94,$01,$40,$D8,$7F,$7E,$12,$17,$3E,$EF,$44,
  $03,$FD,$7F,$7E,$12,$17,$24,$74,$A0,$25,$2F,$F5,$82,$E4,$34,$4C,
  $F5,$83,$E4,$F0,$05,$2F,$E5,$2F,$B4,$08,$03,$E4,$F5,$2F,$E5,$3B,
  $60,$04,$15,$3B,$80,$03,$75,$3B,$FE,$E5,$3B,$70,$08,$20,$07,$05,
  $30,$1B,$02,$D2,$07,$E5,$53,$70,$1A,$30,$60,$09,$B2,$4D,$30,$4D,
  $04,$05,$46,$C2,$04,$E5,$4F,$45,$4E,$60,$08,$E5,$4F,$15,$4F,$70,
  $02,$15,$4E,$22,$22,$E5,$31,$24,$EE,$60,$0F,$14,$60,$10,$14,$60,
  $12,$24,$04,$70,$18,$12,$16,$71,$80,$15,$C2,$45,$80,$11,$E4,$F5,
  $21,$80,$0C,$D2,$09,$D2,$0A,$D2,$0B,$D2,$0C,$80,$02,$D3,$22,$C3,
  $22,$90,$04,$02,$E0,$44,$08,$F0,$85,$39,$82,$85,$38,$83,$E5,$33,
  $F0,$E5,$32,$A3,$F0,$90,$04,$02,$E0,$54,$F7,$F0,$22,$30,$14,$30,
  $90,$70,$19,$E0,$55,$3F,$FF,$90,$70,$18,$E0,$4F,$F5,$3F,$90,$02,
  $29,$E0,$FF,$90,$70,$19,$E0,$FE,$EF,$5E,$90,$02,$29,$F0,$30,$47,
  $04,$AF,$3F,$80,$04,$E5,$3F,$F4,$FF,$90,$02,$28,$EF,$F0,$C2,$14,
  $30,$16,$60,$C2,$AF,$90,$10,$04,$E0,$F5,$57,$90,$02,$28,$E0,$54,
  $05,$F5,$57,$E5,$5C,$64,$01,$70,$21,$E5,$57,$90,$10,$04,$30,$E0,
  $06,$E0,$54,$FB,$F0,$80,$04,$E0,$44,$04,$F0,$E5,$57,$30,$E2,$31,
  $05,$5B,$E5,$5B,$70,$2B,$05,$5A,$80,$27,$E5,$57,$30,$E0,$1B,$E5,
  $5C,$90,$10,$04,$70,$06,$E0,$54,$FB,$F0,$80,$04,$E0,$44,$04,$F0,
  $05,$5B,$E5,$5B,$70,$0B,$05,$5A,$80,$07,$90,$10,$04,$E0,$44,$04,
  $F0,$D2,$AF,$22,$90,$10,$1C,$ED,$F0,$A3,$EF,$F0,$A3,$74,$0A,$F0,
  $90,$10,$1C,$E0,$F5,$58,$90,$10,$1E,$E0,$20,$E1,$F3,$22,$90,$10,
  $1D,$EF,$F0,$A3,$74,$0B,$F0,$90,$10,$1C,$E0,$F5,$58,$90,$10,$1E,
  $E0,$20,$E1,$F3,$AF,$58,$22,$C2,$4B,$C2,$4C,$E5,$44,$12,$0A,$9D,
  $17,$79,$00,$18,$07,$04,$18,$03,$08,$17,$E3,$10,$17,$8D,$20,$17,
  $AD,$60,$17,$BE,$A0,$00,$00,$18,$09,$85,$48,$43,$85,$4A,$42,$85,
  $4C,$5E,$E5,$47,$64,$06,$60,$03,$02,$18,$09,$80,$1B,$E5,$48,$C4,
  $54,$0F,$F5,$43,$E5,$4A,$C4,$54,$0F,$F5,$42,$E5,$4C,$C4,$54,$0F,
  $F5,$5E,$E5,$47,$64,$06,$70,$61,$53,$43,$0F,$80,$5C,$85,$49,$43,
  $85,$4B,$42,$85,$4D,$5E,$E5,$47,$64,$06,$70,$4D,$80,$1B,$E5,$49,
  $C4,$54,$0F,$F5,$43,$E5,$4B,$C4,$54,$0F,$F5,$42,$E5,$4D,$C4,$54,
  $0F,$F5,$5E,$E5,$47,$64,$06,$70,$30,$E5,$43,$54,$0F,$44,$10,$F5,
  $43,$80,$26,$E5,$47,$64,$04,$60,$05,$E5,$47,$B4,$05,$06,$43,$5E,
  $04,$75,$42,$09,$E5,$47,$B4,$06,$10,$E5,$43,$54,$0F,$44,$30,$F5,
  $43,$80,$06,$D2,$4B,$80,$02,$D2,$4C,$E4,$F5,$25,$E5,$42,$C4,$54,
  $F0,$FF,$E5,$43,$54,$0F,$4F,$F5,$5F,$D2,$60,$22,$D2,$15,$E5,$47,
  $24,$F5,$60,$0B,$24,$CB,$60,$07,$24,$40,$70,$06,$C2,$15,$22,$12,
  $1B,$9A,$12,$18,$3E,$C2,$15,$C2,$AF,$C2,$04,$D2,$AF,$22,$C2,$AF,
  $90,$04,$14,$E0,$54,$0E,$60,$04,$D2,$18,$80,$08,$E5,$4E,$45,$4F,
  $24,$FF,$92,$18,$D2,$AF,$90,$04,$14,$E0,$A2,$E4,$92,$19,$74,$1E,
  $F0,$E5,$5F,$54,$0F,$F5,$2D,$E5,$25,$70,$13,$30,$18,$05,$E5,$5F,
  $20,$E5,$0B,$30,$19,$19,$E5,$5F,$54,$30,$FF,$BF,$30,$11,$E5,$25,
  $70,$05,$75,$25,$0C,$80,$02,$15,$25,$D2,$6C,$D2,$6D,$80,$0F,$E5,
  $5F,$30,$E6,$06,$C2,$6C,$D2,$6D,$80,$04,$D2,$6C,$C2,$6D,$E5,$47,
  $64,$03,$70,$21,$30,$4B,$06,$C2,$6C,$D2,$6D,$80,$18,$E5,$25,$70,
  $03,$30,$4C,$11,$C2,$4C,$E5,$25,$70,$05,$75,$25,$07,$80,$02,$15,
  $25,$D2,$6C,$D2,$6D,$E5,$47,$B4,$09,$14,$E5,$44,$20,$E3,$0B,$E5,
  $3A,$64,$02,$60,$05,$E5,$3A,$B4,$03,$04,$C2,$6C,$D2,$6D,$E5,$47,
  $B4,$0A,$13,$E5,$3A,$B4,$01,$06,$C2,$6C,$D2,$6D,$80,$08,$E5,$3A,
  $70,$04,$D2,$6C,$C2,$6D,$20,$69,$07,$E5,$5E,$20,$E0,$02,$B2,$68,
  $20,$6B,$07,$E5,$5E,$20,$E1,$02,$B2,$6A,$20,$6D,$07,$E5,$5E,$20,
  $E2,$02,$B2,$6C,$75,$2E,$40,$20,$69,$04,$A2,$68,$80,$26,$30,$68,
  $06,$E5,$46,$A2,$E2,$80,$1D,$E5,$5E,$20,$E0,$04,$7F,$01,$80,$02,
  $7F,$00,$E5,$46,$54,$F0,$FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,
  $EE,$6F,$24,$FF,$92,$73,$92,$72,$20,$6B,$04,$A2,$6A,$80,$26,$30,
  $6A,$06,$E5,$46,$A2,$E2,$80,$1D,$E5,$5E,$20,$E1,$04,$7F,$01,$80,
  $02,$7F,$00,$E5,$46,$54,$F0,$FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,
  $00,$EE,$6F,$24,$FF,$92,$75,$92,$74,$20,$6D,$04,$A2,$6C,$80,$26,
  $E5,$47,$64,$0A,$70,$22,$30,$6C,$06,$E5,$46,$A2,$E3,$80,$17,$E5,
  $3A,$B4,$01,$06,$E5,$46,$A2,$E3,$80,$34,$E5,$46,$20,$E4,$03,$30,
  $E5,$03,$D3,$80,$01,$C3,$80,$26,$30,$6C,$06,$E5,$46,$A2,$E2,$80,
  $1D,$E5,$5E,$20,$E2,$04,$7F,$01,$80,$02,$7F,$00,$E5,$46,$54,$F0,
  $FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,$EE,$6F,$24,$FF,$92,$71,
  $92,$70,$90,$10,$00,$E0,$90,$10,$2C,$F0,$90,$10,$03,$E0,$C3,$94,
  $30,$40,$19,$E0,$64,$32,$60,$14,$A2,$71,$92,$77,$A2,$70,$92,$76,
  $E5,$2E,$13,$13,$54,$3F,$F5,$2E,$C2,$77,$D2,$76,$30,$17,$0D,$53,
  $2E,$F0,$E5,$2E,$45,$5D,$90,$10,$2F,$F0,$80,$06,$90,$10,$2F,$E5,
  $2E,$F0,$E5,$47,$64,$06,$70,$47,$90,$02,$28,$E0,$30,$47,$03,$FF,
  $80,$02,$F4,$FF,$8F,$3F,$90,$02,$29,$E0,$54,$FE,$F0,$E5,$43,$C4,
  $54,$0F,$14,$60,$0C,$24,$FE,$60,$0C,$24,$03,$70,$13,$C2,$F8,$80,
  $0F,$D2,$F8,$80,$0B,$E5,$46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$F8,
  $30,$47,$05,$AF,$3F,$02,$1B,$94,$E5,$3F,$F4,$FF,$02,$1B,$94,$E5,
  $47,$64,$07,$60,$0F,$E5,$47,$64,$08,$60,$09,$E5,$47,$64,$09,$60,
  $03,$02,$1B,$02,$90,$02,$28,$E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,
  $8F,$3F,$90,$02,$29,$E0,$54,$FC,$F0,$E5,$3A,$14,$60,$22,$14,$60,
  $25,$14,$60,$2D,$24,$FC,$60,$49,$24,$F9,$60,$14,$24,$0E,$70,$50,
  $E5,$46,$13,$13,$54,$3F,$75,$F0,$03,$84,$E5,$F0,$24,$FF,$80,$3A,
  $D2,$F9,$C2,$F8,$80,$3E,$E5,$46,$30,$E2,$03,$D3,$80,$1D,$C3,$80,
  $1A,$E5,$46,$30,$E2,$0D,$54,$38,$C3,$94,$30,$50,$06,$7E,$00,$7F,
  $01,$80,$04,$7E,$00,$7F,$00,$EE,$4F,$24,$FF,$92,$F8,$C2,$F9,$80,
  $13,$E5,$46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$F9,$C2,$F8,$80,$04,
  $C2,$F8,$C2,$F9,$30,$47,$04,$AF,$3F,$80,$04,$E5,$3F,$F4,$FF,$02,
  $1B,$94,$E5,$47,$64,$0C,$60,$09,$E5,$47,$64,$0B,$60,$03,$02,$1B,
  $99,$90,$02,$28,$E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,$8F,$3F,$90,
  $02,$29,$E0,$54,$FD,$F0,$E5,$3A,$14,$60,$20,$14,$60,$21,$14,$60,
  $2B,$24,$FC,$60,$45,$24,$F9,$60,$12,$24,$0E,$70,$4A,$E5,$46,$13,
  $13,$54,$3F,$75,$F0,$03,$84,$E5,$F0,$80,$29,$D2,$F9,$80,$3A,$E5,
  $46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$F9,$80,$2D,$E5,$46,$30,$E2,
  $0D,$54,$38,$C3,$94,$30,$50,$06,$7E,$00,$7F,$01,$80,$04,$7E,$00,
  $7F,$00,$EE,$4F,$24,$FF,$92,$F9,$80,$0F,$E5,$46,$30,$E2,$03,$D3,
  $80,$01,$C3,$92,$F9,$80,$02,$C2,$F9,$30,$47,$04,$AF,$3F,$80,$04,
  $E5,$3F,$F4,$FF,$90,$02,$28,$EF,$F0,$22,$E5,$47,$B4,$0B,$10,$90,
  $02,$29,$E0,$54,$EB,$F0,$E5,$3F,$54,$EB,$45,$45,$F5,$3F,$22,$E4,
  $90,$02,$29,$F0,$30,$47,$04,$AF,$45,$80,$04,$E5,$45,$F4,$FF,$90,
  $02,$28,$EF,$F0,$22,$8F,$50,$D2,$59,$22,$8F,$54,$D2,$58,$22,$E4,
  $F5,$62,$C2,$AF,$E5,$51,$14,$60,$4B,$14,$60,$6C,$24,$02,$60,$03,
  $02,$1C,$DC,$D2,$59,$75,$55,$01,$90,$02,$A2,$E0,$54,$7F,$F0,$A3,
  $E0,$20,$E7,$23,$90,$04,$34,$E0,$B4,$02,$1C,$A3,$E0,$B4,$02,$17,
  $A3,$E0,$B4,$02,$12,$7F,$20,$12,$1B,$C5,$90,$10,$04,$E0,$54,$F3,
  $F0,$75,$51,$01,$02,$1C,$DC,$E5,$50,$60,$03,$02,$1C,$DC,$75,$62,
  $03,$02,$1C,$DC,$90,$12,$00,$E0,$54,$03,$70,$12,$7F,$20,$12,$1B,
  $C5,$90,$02,$A2,$E0,$54,$BF,$F0,$75,$51,$02,$02,$1C,$DC,$E5,$50,
  $60,$03,$02,$1C,$DC,$02,$1C,$D7,$90,$02,$A3,$E0,$30,$E6,$03,$02,
  $1C,$D3,$90,$04,$37,$E0,$64,$22,$70,$79,$90,$01,$8A,$74,$7E,$F0,
  $90,$01,$96,$F0,$90,$12,$04,$74,$0A,$F0,$90,$13,$28,$E0,$90,$70,
  $1A,$F0,$90,$13,$29,$E0,$90,$70,$1B,$F0,$90,$13,$2B,$E0,$90,$70,
  $22,$F0,$90,$13,$28,$E0,$54,$F0,$F0,$A3,$E0,$54,$F0,$F0,$90,$13,
  $2B,$E0,$54,$CC,$F0,$E5,$58,$30,$E3,$13,$E5,$3C,$F4,$90,$13,$2A,
  $60,$05,$E0,$54,$F3,$80,$11,$E0,$54,$FB,$F0,$80,$14,$E5,$3C,$F4,
  $90,$13,$2A,$60,$08,$E0,$54,$F2,$45,$3C,$F0,$80,$04,$E0,$54,$FA,
  $F0,$90,$04,$01,$E0,$54,$FD,$F0,$75,$62,$01,$75,$55,$02,$E4,$F5,
  $51,$80,$09,$E5,$50,$70,$05,$75,$62,$03,$F5,$51,$E5,$62,$60,$15,
  $C2,$01,$E4,$F5,$51,$C2,$59,$AD,$62,$AF,$40,$12,$1D,$A9,$E5,$62,
  $B4,$03,$02,$D2,$03,$D2,$AF,$22,$C2,$AF,$30,$01,$12,$E4,$90,$01,
  $96,$F0,$F5,$51,$C2,$59,$C2,$01,$7D,$02,$AF,$40,$12,$1D,$A9,$E5,
  $52,$14,$60,$0C,$04,$60,$03,$02,$1D,$A6,$75,$52,$01,$75,$55,$03,
  $90,$04,$01,$E0,$44,$0E,$F0,$E5,$58,$54,$18,$60,$1E,$90,$70,$1A,
  $E0,$90,$13,$28,$F0,$90,$70,$1B,$E0,$90,$13,$29,$F0,$A3,$74,$05,
  $F0,$90,$70,$22,$E0,$90,$13,$2B,$F0,$80,$11,$90,$13,$28,$E0,$44,
  $0F,$F0,$A3,$E0,$44,$0F,$F0,$A3,$E0,$44,$05,$F0,$90,$12,$04,$74,
  $03,$F0,$E5,$58,$30,$E3,$16,$90,$05,$00,$74,$80,$F0,$A3,$74,$08,
  $F0,$A3,$74,$01,$F0,$74,$03,$F0,$7F,$01,$12,$0D,$2A,$90,$02,$A2,
  $E0,$44,$C0,$F0,$90,$10,$04,$E0,$44,$0C,$F0,$E4,$F5,$52,$F5,$55,
  $30,$02,$09,$C2,$02,$7D,$01,$AF,$41,$12,$1D,$A9,$30,$03,$02,$C2,
  $03,$E4,$90,$01,$96,$F0,$D2,$AF,$22,$EF,$F4,$60,$2D,$E4,$FE,$74,
  $14,$2E,$F5,$82,$E4,$34,$70,$F5,$83,$E0,$B4,$FF,$19,$74,$14,$2E,
  $F5,$82,$E4,$34,$70,$F5,$83,$EF,$F0,$74,$1C,$2E,$F5,$82,$E4,$34,
  $70,$F5,$83,$ED,$F0,$22,$0E,$BE,$04,$D5,$22,$22,$22,$90,$70,$2A,
  $E0,$30,$E1,$4D,$C2,$AF,$90,$70,$28,$E0,$90,$10,$1C,$F0,$90,$70,
  $29,$E0,$90,$10,$1D,$F0,$90,$70,$2A,$E0,$90,$10,$1E,$F0,$90,$10,
  $1C,$E0,$F5,$62,$90,$10,$1E,$E0,$20,$E1,$F3,$90,$10,$1C,$E0,$90,
  $70,$28,$F0,$90,$10,$1D,$E0,$90,$70,$29,$F0,$90,$10,$1E,$E0,$90,
  $70,$2A,$F0,$30,$4A,$07,$90,$70,$24,$E0,$44,$01,$F0,$C2,$05,$D2,
  $AF,$22,$20,$1C,$5A,$7D,$05,$12,$1F,$73,$90,$4C,$B0,$12,$1F,$64,
  $90,$4C,$BB,$EF,$F0,$7D,$03,$12,$1F,$73,$90,$4C,$BC,$12,$1F,$64,
  $90,$4C,$BD,$EF,$F0,$7D,$09,$12,$1F,$7E,$90,$4C,$B7,$EF,$F0,$7D,
  $08,$12,$1F,$7E,$90,$4C,$B6,$EF,$F0,$7D,$07,$12,$1F,$99,$90,$4C,
  $B5,$EF,$F0,$7D,$06,$12,$1F,$99,$90,$4C,$B4,$EF,$F0,$E4,$90,$4C,
  $B1,$F0,$90,$4C,$B9,$E0,$90,$4C,$B8,$F0,$D2,$1C,$C2,$07,$22,$7D,
  $01,$7F,$B8,$12,$17,$24,$7F,$B9,$12,$17,$3E,$EF,$64,$01,$60,$03,
  $02,$1F,$59,$D3,$90,$4C,$B3,$E0,$95,$27,$90,$4C,$B2,$E0,$95,$26,
  $40,$11,$90,$4C,$B1,$E0,$04,$F0,$90,$4C,$B8,$E0,$60,$1A,$E0,$14,
  $F0,$80,$15,$90,$4C,$B9,$E0,$FF,$90,$4C,$B8,$E0,$C3,$9F,$50,$08,
  $90,$4C,$B9,$E0,$90,$4C,$B8,$F0,$E4,$F5,$26,$F5,$27,$7D,$05,$7F,
  $B8,$12,$17,$24,$90,$4C,$B0,$12,$1F,$89,$7D,$02,$7F,$B8,$12,$17,
  $24,$90,$4C,$BB,$12,$1F,$89,$7D,$03,$7F,$B8,$12,$17,$24,$90,$4C,
  $BC,$12,$1F,$91,$7D,$04,$7F,$B8,$12,$17,$24,$90,$4C,$BD,$12,$1F,
  $91,$7D,$09,$7F,$B8,$12,$17,$24,$90,$4C,$B7,$12,$1F,$A4,$7D,$08,
  $7F,$B8,$12,$17,$24,$90,$4C,$B6,$12,$1F,$A4,$7D,$07,$7F,$B8,$12,
  $17,$24,$90,$4C,$B5,$E0,$FD,$7F,$B9,$12,$17,$24,$7D,$06,$7F,$B8,
  $12,$17,$24,$90,$4C,$B4,$E0,$FD,$7F,$B9,$12,$17,$24,$7D,$01,$12,
  $1F,$AC,$E4,$FD,$12,$1F,$AC,$80,$08,$05,$27,$E5,$27,$70,$02,$05,
  $26,$C2,$07,$22,$EF,$F0,$7D,$02,$7F,$B8,$12,$17,$24,$7F,$B9,$12,
  $17,$3E,$22,$7F,$B8,$12,$17,$24,$7F,$B9,$12,$17,$3E,$22,$7F,$B8,
  $12,$17,$24,$7F,$B9,$12,$17,$3E,$22,$E0,$FD,$7F,$B9,$12,$17,$24,
  $22,$E0,$FD,$7F,$B9,$12,$17,$24,$22,$7F,$B8,$12,$17,$24,$7F,$B9,
  $12,$17,$3E,$22,$E0,$FD,$7F,$B9,$12,$17,$24,$22,$7F,$B8,$12,$17,
  $24,$7D,$01,$7F,$B9,$12,$17,$24,$22,$22,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$21,$61,$80,
  $FF,$FF,$FF,$02,$10,$28,$02,$10,$3B,$02,$10,$86,$02,$15,$5F,$02,
  $15,$60,$02,$16,$44,$02,$16,$8D,$12,$16,$45,$22,$02,$1B,$CF,$02,
  $1C,$F8,$02,$18,$1C,$02,$17,$57,$30,$05,$06,$20,$0D,$03,$12,$1D,
  $DD,$30,$07,$06,$20,$0F,$03,$12,$1E,$32,$22,$90,$01,$8C,$E0,$30,
  $E3,$20,$E5,$58,$30,$E0,$10,$E5,$4C,$30,$E0,$04,$7F,$40,$80,$02,
  $7F,$00,$90,$10,$2F,$EF,$F0,$90,$01,$8C,$74,$08,$F0,$E4,$90,$01,
  $A7,$F0,$90,$01,$8C,$E0,$30,$E0,$1C,$90,$01,$80,$E0,$B4,$02,$15,
  $A3,$E0,$B4,$01,$10,$90,$01,$84,$E0,$B4,$81,$09,$90,$01,$8C,$74,
  $01,$F0,$12,$0D,$DD,$22,$90,$04,$14,$E0,$20,$E7,$03,$02,$15,$5E,
  $90,$70,$12,$E0,$F5,$56,$90,$04,$04,$E0,$12,$0A,$B6,$10,$F3,$31,
  $10,$D7,$36,$11,$1C,$50,$11,$53,$51,$11,$5C,$52,$11,$5C,$53,$11,
  $5C,$54,$11,$9D,$55,$11,$EC,$56,$12,$AF,$64,$12,$4A,$65,$12,$75,
  $66,$12,$DB,$72,$13,$A3,$73,$13,$C8,$74,$14,$C4,$80,$14,$E7,$90,
  $15,$1C,$91,$00,$00,$15,$5E,$90,$70,$11,$E0,$F5,$3C,$E4,$90,$70,
  $13,$F0,$90,$04,$14,$74,$80,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,
  $02,$15,$57,$20,$02,$03,$30,$03,$1D,$7D,$02,$AF,$56,$12,$0B,$AA,
  $90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,
  $02,$15,$5E,$02,$15,$57,$85,$56,$41,$D2,$02,$22,$90,$70,$10,$E0,
  $54,$7F,$FF,$BF,$0A,$0D,$90,$70,$11,$E0,$B4,$08,$06,$75,$4E,$01,
  $75,$4F,$84,$90,$70,$10,$E0,$54,$7F,$FF,$BF,$02,$12,$90,$70,$11,
  $E0,$64,$08,$60,$04,$E0,$B4,$20,$06,$75,$4E,$03,$75,$4F,$20,$E4,
  $F5,$3F,$22,$90,$70,$11,$E0,$24,$FF,$92,$47,$22,$90,$04,$04,$E0,
  $25,$E0,$24,$5D,$F5,$57,$90,$70,$10,$E0,$FF,$74,$47,$25,$57,$F8,
  $C6,$EF,$C6,$90,$70,$11,$E0,$FF,$74,$48,$25,$57,$F8,$C6,$EF,$C6,
  $E4,$FD,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,
  $13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$E5,$47,$64,
  $07,$60,$1D,$E5,$47,$64,$08,$60,$17,$E5,$47,$64,$09,$60,$11,$E5,
  $47,$64,$0A,$60,$0B,$E5,$47,$64,$0B,$60,$05,$E5,$47,$B4,$0C,$08,
  $90,$70,$11,$E0,$54,$0F,$F5,$3A,$E5,$47,$B4,$09,$08,$E5,$3A,$B4,
  $03,$03,$E4,$F5,$46,$E5,$47,$B4,$0A,$08,$E5,$3A,$B4,$01,$03,$E4,
  $F5,$46,$E4,$FD,$AF,$56,$12,$0B,$AA,$D2,$04,$22,$90,$70,$11,$E0,
  $F4,$FF,$90,$70,$10,$E0,$5F,$FF,$90,$70,$11,$E0,$55,$3F,$4F,$90,
  $70,$18,$F0,$90,$70,$11,$E0,$90,$70,$19,$F0,$E4,$FD,$AF,$56,$12,
  $0B,$AA,$30,$15,$04,$D2,$14,$80,$26,$90,$70,$18,$E0,$F5,$3F,$90,
  $02,$29,$E0,$FF,$90,$70,$19,$E0,$FE,$EF,$5E,$90,$02,$29,$F0,$30,
  $47,$04,$AF,$3F,$80,$04,$E5,$3F,$F4,$FF,$90,$02,$28,$EF,$F0,$E5,
  $56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$90,$70,$10,$E0,$13,$92,
  $1B,$C2,$1C,$AD,$57,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,
  $30,$1B,$02,$D2,$07,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,
  $15,$5E,$02,$15,$57,$90,$4C,$B1,$E0,$90,$4C,$BE,$F0,$90,$4C,$B8,
  $E0,$90,$4C,$BF,$F0,$E4,$90,$4C,$B1,$F0,$90,$4C,$B9,$E0,$90,$4C,
  $B8,$F0,$AD,$57,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,
  $90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$90,
  $70,$10,$E0,$F5,$2A,$75,$3D,$03,$75,$3E,$1F,$E4,$F5,$2F,$AD,$57,
  $AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,
  $E5,$56,$F4,$70,$03,$02,$15,$5E,$02,$15,$57,$90,$10,$00,$E0,$F5,
  $57,$E4,$F5,$58,$F5,$59,$90,$10,$03,$E0,$B4,$28,$05,$75,$58,$01,
  $80,$3C,$90,$10,$03,$E0,$B4,$30,$05,$75,$58,$02,$80,$30,$90,$10,
  $03,$E0,$B4,$33,$05,$75,$58,$04,$80,$24,$90,$10,$03,$E0,$B4,$35,
  $0C,$90,$10,$02,$E0,$B4,$72,$05,$75,$58,$08,$80,$11,$90,$10,$03,
  $E0,$B4,$35,$0A,$90,$10,$02,$E0,$B4,$93,$03,$75,$58,$10,$E5,$58,
  $30,$E1,$19,$90,$05,$08,$E0,$44,$01,$F0,$FD,$90,$05,$05,$E0,$54,
  $FB,$F0,$44,$04,$F0,$ED,$54,$FE,$90,$05,$08,$F0,$75,$3B,$18,$E4,
  $F5,$26,$F5,$27,$C2,$1C,$C2,$1B,$F5,$4E,$F5,$4F,$75,$3A,$FF,$75,
  $3C,$FF,$AD,$57,$AF,$56,$12,$0B,$AA,$90,$70,$36,$74,$37,$F0,$A3,
  $74,$32,$F0,$90,$04,$01,$E0,$44,$01,$F0,$C2,$16,$E4,$F5,$5C,$F5,
  $5A,$F5,$5B,$90,$70,$30,$F0,$A3,$F0,$F5,$2A,$C2,$17,$90,$04,$14,
  $74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,$70,$03,$02,$15,$5E,
  $02,$15,$57,$90,$70,$10,$E0,$24,$FF,$92,$93,$E4,$FD,$AF,$56,$12,
  $0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,$E5,$56,$F4,
  $70,$03,$02,$15,$5E,$02,$15,$57,$90,$10,$00,$E0,$90,$10,$2C,$F0,
  $90,$10,$2F,$74,$40,$F0,$90,$70,$11,$E0,$54,$7F,$F5,$57,$E0,$54,
  $80,$90,$70,$32,$F0,$90,$70,$10,$E0,$FF,$E5,$57,$D3,$9F,$40,$43,
  $90,$70,$33,$E5,$57,$F0,$90,$70,$10,$E0,$FF,$90,$70,$33,$E0,$C3,
  $9F,$D3,$94,$04,$40,$73,$E0,$24,$FC,$F0,$E0,$FF,$90,$70,$32,$E0,
  $4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,$17,$80,$02,$7F,
  $11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$FF,$12,$0D,
  $48,$80,$C3,$90,$70,$33,$E5,$57,$F0,$90,$70,$33,$E0,$FF,$90,$70,
  $10,$E0,$C3,$9F,$D3,$94,$04,$40,$30,$90,$70,$33,$E0,$24,$04,$F0,
  $E0,$FF,$90,$70,$32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,
  $04,$7F,$17,$80,$02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,
  $74,$03,$F0,$FF,$12,$0D,$48,$80,$C0,$90,$70,$10,$E0,$FF,$90,$70,
  $32,$E0,$4F,$90,$05,$00,$F0,$E5,$58,$54,$0F,$60,$04,$7F,$17,$80,
  $02,$7F,$11,$90,$05,$01,$EF,$F0,$A3,$74,$01,$F0,$74,$03,$F0,$FF,
  $12,$0D,$48,$90,$10,$00,$E0,$90,$10,$2C,$F0,$90,$10,$2F,$74,$7F,
  $F0,$E4,$FD,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,
  $70,$13,$F0,$22,$90,$70,$10,$E0,$24,$FF,$92,$4A,$D2,$05,$AD,$57,
  $AF,$56,$12,$0B,$AA,$90,$04,$14,$74,$80,$F0,$E4,$90,$70,$13,$F0,
  $E5,$56,$F4,$60,$79,$80,$70,$90,$70,$10,$E0,$24,$FF,$92,$16,$90,
  $70,$11,$E0,$F5,$5C,$AD,$57,$AF,$56,$12,$0B,$AA,$90,$04,$14,$74,
  $80,$F0,$90,$70,$30,$E5,$5A,$F0,$A3,$E5,$5B,$F0,$E4,$F5,$5A,$F5,
  $5B,$90,$70,$13,$F0,$E5,$56,$F4,$60,$44,$80,$3B,$90,$70,$11,$E0,
  $24,$FF,$92,$17,$90,$70,$10,$E0,$F5,$5D,$AD,$57,$AF,$56,$12,$0B,
  $AA,$90,$04,$14,$74,$80,$F0,$30,$17,$13,$90,$10,$00,$E0,$90,$10,
  $2C,$F0,$90,$10,$2F,$E0,$54,$F0,$F5,$57,$45,$5D,$F0,$E4,$90,$70,
  $13,$F0,$E5,$56,$F4,$60,$07,$90,$70,$25,$E0,$44,$01,$F0,$22,$22,
  $E5,$3E,$45,$3D,$60,$0A,$E5,$3E,$15,$3E,$70,$0A,$15,$3D,$80,$06,
  $75,$3D,$03,$75,$3E,$1F,$E5,$3E,$45,$3D,$60,$03,$02,$16,$0E,$E5,
  $2A,$70,$03,$02,$16,$0E,$74,$A0,$25,$2F,$F5,$82,$E4,$34,$4C,$F5,
  $83,$E0,$60,$7A,$7F,$7E,$12,$17,$3E,$EF,$54,$FE,$44,$02,$FD,$7F,
  $7E,$12,$17,$24,$E5,$2F,$7F,$00,$25,$E0,$FE,$EF,$24,$00,$F5,$82,
  $74,$4D,$3E,$AF,$82,$90,$4C,$A8,$F0,$A3,$EF,$F0,$E4,$F5,$56,$F5,
  $57,$7F,$7F,$12,$17,$3E,$90,$4C,$A8,$E0,$FA,$A3,$E0,$25,$57,$F5,
  $82,$EA,$35,$56,$F5,$83,$EF,$F0,$05,$57,$E5,$57,$70,$02,$05,$56,
  $C3,$94,$80,$E5,$56,$94,$01,$40,$D8,$7F,$7E,$12,$17,$3E,$EF,$44,
  $03,$FD,$7F,$7E,$12,$17,$24,$74,$A0,$25,$2F,$F5,$82,$E4,$34,$4C,
  $F5,$83,$E4,$F0,$05,$2F,$E5,$2F,$B4,$08,$03,$E4,$F5,$2F,$E5,$3B,
  $60,$04,$15,$3B,$80,$03,$75,$3B,$FE,$E5,$3B,$70,$08,$20,$07,$05,
  $30,$1B,$02,$D2,$07,$E5,$53,$70,$1A,$30,$60,$09,$B2,$4D,$30,$4D,
  $04,$05,$46,$C2,$04,$E5,$4F,$45,$4E,$60,$08,$E5,$4F,$15,$4F,$70,
  $02,$15,$4E,$22,$22,$E5,$31,$24,$EE,$60,$0F,$14,$60,$10,$14,$60,
  $12,$24,$04,$70,$18,$12,$16,$71,$80,$15,$C2,$45,$80,$11,$E4,$F5,
  $21,$80,$0C,$D2,$09,$D2,$0A,$D2,$0B,$D2,$0C,$80,$02,$D3,$22,$C3,
  $22,$90,$04,$02,$E0,$44,$08,$F0,$85,$39,$82,$85,$38,$83,$E5,$33,
  $F0,$E5,$32,$A3,$F0,$90,$04,$02,$E0,$54,$F7,$F0,$22,$30,$14,$30,
  $90,$70,$19,$E0,$55,$3F,$FF,$90,$70,$18,$E0,$4F,$F5,$3F,$90,$02,
  $29,$E0,$FF,$90,$70,$19,$E0,$FE,$EF,$5E,$90,$02,$29,$F0,$30,$47,
  $04,$AF,$3F,$80,$04,$E5,$3F,$F4,$FF,$90,$02,$28,$EF,$F0,$C2,$14,
  $30,$16,$60,$C2,$AF,$90,$10,$04,$E0,$F5,$57,$90,$02,$28,$E0,$54,
  $05,$F5,$57,$E5,$5C,$64,$01,$70,$21,$E5,$57,$90,$10,$04,$30,$E0,
  $06,$E0,$54,$FB,$F0,$80,$04,$E0,$44,$04,$F0,$E5,$57,$30,$E2,$31,
  $05,$5B,$E5,$5B,$70,$2B,$05,$5A,$80,$27,$E5,$57,$30,$E0,$1B,$E5,
  $5C,$90,$10,$04,$70,$06,$E0,$54,$FB,$F0,$80,$04,$E0,$44,$04,$F0,
  $05,$5B,$E5,$5B,$70,$0B,$05,$5A,$80,$07,$90,$10,$04,$E0,$44,$04,
  $F0,$D2,$AF,$22,$90,$10,$1C,$ED,$F0,$A3,$EF,$F0,$A3,$74,$0A,$F0,
  $90,$10,$1C,$E0,$F5,$58,$90,$10,$1E,$E0,$20,$E1,$F3,$22,$90,$10,
  $1D,$EF,$F0,$A3,$74,$0B,$F0,$90,$10,$1C,$E0,$F5,$58,$90,$10,$1E,
  $E0,$20,$E1,$F3,$AF,$58,$22,$C2,$4B,$C2,$4C,$E5,$44,$12,$0A,$B6,
  $17,$79,$00,$18,$07,$04,$18,$03,$08,$17,$E3,$10,$17,$8D,$20,$17,
  $AD,$60,$17,$BE,$A0,$00,$00,$18,$09,$85,$48,$43,$85,$4A,$42,$85,
  $4C,$5E,$E5,$47,$64,$06,$60,$03,$02,$18,$09,$80,$1B,$E5,$48,$C4,
  $54,$0F,$F5,$43,$E5,$4A,$C4,$54,$0F,$F5,$42,$E5,$4C,$C4,$54,$0F,
  $F5,$5E,$E5,$47,$64,$06,$70,$61,$53,$43,$0F,$80,$5C,$85,$49,$43,
  $85,$4B,$42,$85,$4D,$5E,$E5,$47,$64,$06,$70,$4D,$80,$1B,$E5,$49,
  $C4,$54,$0F,$F5,$43,$E5,$4B,$C4,$54,$0F,$F5,$42,$E5,$4D,$C4,$54,
  $0F,$F5,$5E,$E5,$47,$64,$06,$70,$30,$E5,$43,$54,$0F,$44,$10,$F5,
  $43,$80,$26,$E5,$47,$64,$04,$60,$05,$E5,$47,$B4,$05,$06,$43,$5E,
  $04,$75,$42,$09,$E5,$47,$B4,$06,$10,$E5,$43,$54,$0F,$44,$30,$F5,
  $43,$80,$06,$D2,$4B,$80,$02,$D2,$4C,$E4,$F5,$25,$E5,$42,$C4,$54,
  $F0,$FF,$E5,$43,$54,$0F,$4F,$F5,$5F,$D2,$60,$22,$D2,$15,$E5,$47,
  $24,$F5,$60,$0B,$24,$CB,$60,$07,$24,$40,$70,$06,$C2,$15,$22,$12,
  $1B,$9A,$12,$18,$3E,$C2,$15,$C2,$AF,$C2,$04,$D2,$AF,$22,$C2,$AF,
  $90,$04,$14,$E0,$54,$0E,$60,$04,$D2,$18,$80,$08,$E5,$4E,$45,$4F,
  $24,$FF,$92,$18,$D2,$AF,$90,$04,$14,$E0,$A2,$E4,$92,$19,$74,$1E,
  $F0,$E5,$5F,$54,$0F,$F5,$2D,$E5,$25,$70,$13,$30,$18,$05,$E5,$5F,
  $20,$E5,$0B,$30,$19,$19,$E5,$5F,$54,$30,$FF,$BF,$30,$11,$E5,$25,
  $70,$05,$75,$25,$0C,$80,$02,$15,$25,$D2,$6C,$D2,$6D,$80,$0F,$E5,
  $5F,$30,$E6,$06,$C2,$6C,$D2,$6D,$80,$04,$D2,$6C,$C2,$6D,$E5,$47,
  $64,$03,$70,$21,$30,$4B,$06,$C2,$6C,$D2,$6D,$80,$18,$E5,$25,$70,
  $03,$30,$4C,$11,$C2,$4C,$E5,$25,$70,$05,$75,$25,$07,$80,$02,$15,
  $25,$D2,$6C,$D2,$6D,$E5,$47,$B4,$09,$14,$E5,$44,$20,$E3,$0B,$E5,
  $3A,$64,$02,$60,$05,$E5,$3A,$B4,$03,$04,$C2,$6C,$D2,$6D,$E5,$47,
  $B4,$0A,$13,$E5,$3A,$B4,$01,$06,$C2,$6C,$D2,$6D,$80,$08,$E5,$3A,
  $70,$04,$D2,$6C,$C2,$6D,$20,$69,$07,$E5,$5E,$20,$E0,$02,$B2,$68,
  $20,$6B,$07,$E5,$5E,$20,$E1,$02,$B2,$6A,$20,$6D,$07,$E5,$5E,$20,
  $E2,$02,$B2,$6C,$75,$2E,$40,$20,$69,$04,$A2,$68,$80,$26,$30,$68,
  $06,$E5,$46,$A2,$E2,$80,$1D,$E5,$5E,$20,$E0,$04,$7F,$01,$80,$02,
  $7F,$00,$E5,$46,$54,$F0,$FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,
  $EE,$6F,$24,$FF,$92,$73,$92,$72,$20,$6B,$04,$A2,$6A,$80,$26,$30,
  $6A,$06,$E5,$46,$A2,$E2,$80,$1D,$E5,$5E,$20,$E1,$04,$7F,$01,$80,
  $02,$7F,$00,$E5,$46,$54,$F0,$FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,
  $00,$EE,$6F,$24,$FF,$92,$75,$92,$74,$20,$6D,$04,$A2,$6C,$80,$26,
  $E5,$47,$64,$0A,$70,$22,$30,$6C,$06,$E5,$46,$A2,$E3,$80,$17,$E5,
  $3A,$B4,$01,$06,$E5,$46,$A2,$E3,$80,$34,$E5,$46,$20,$E4,$03,$30,
  $E5,$03,$D3,$80,$01,$C3,$80,$26,$30,$6C,$06,$E5,$46,$A2,$E2,$80,
  $1D,$E5,$5E,$20,$E2,$04,$7F,$01,$80,$02,$7F,$00,$E5,$46,$54,$F0,
  $FE,$BE,$F0,$04,$7E,$01,$80,$02,$7E,$00,$EE,$6F,$24,$FF,$92,$71,
  $92,$70,$90,$10,$00,$E0,$90,$10,$2C,$F0,$90,$10,$03,$E0,$C3,$94,
  $30,$40,$19,$E0,$64,$32,$60,$14,$A2,$71,$92,$77,$A2,$70,$92,$76,
  $E5,$2E,$13,$13,$54,$3F,$F5,$2E,$C2,$77,$D2,$76,$30,$17,$0D,$53,
  $2E,$F0,$E5,$2E,$45,$5D,$90,$10,$2F,$F0,$80,$06,$90,$10,$2F,$E5,
  $2E,$F0,$E5,$47,$64,$06,$70,$47,$90,$02,$28,$E0,$30,$47,$03,$FF,
  $80,$02,$F4,$FF,$8F,$3F,$90,$02,$29,$E0,$54,$FE,$F0,$E5,$43,$C4,
  $54,$0F,$14,$60,$0C,$24,$FE,$60,$0C,$24,$03,$70,$13,$C2,$F8,$80,
  $0F,$D2,$F8,$80,$0B,$E5,$46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$F8,
  $30,$47,$05,$AF,$3F,$02,$1B,$94,$E5,$3F,$F4,$FF,$02,$1B,$94,$E5,
  $47,$64,$07,$60,$0F,$E5,$47,$64,$08,$60,$09,$E5,$47,$64,$09,$60,
  $03,$02,$1B,$02,$90,$02,$28,$E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,
  $8F,$3F,$90,$02,$29,$E0,$54,$FC,$F0,$E5,$3A,$14,$60,$22,$14,$60,
  $25,$14,$60,$2D,$24,$FC,$60,$49,$24,$F9,$60,$14,$24,$0E,$70,$50,
  $E5,$46,$13,$13,$54,$3F,$75,$F0,$03,$84,$E5,$F0,$24,$FF,$80,$3A,
  $D2,$F9,$C2,$F8,$80,$3E,$E5,$46,$30,$E2,$03,$D3,$80,$1D,$C3,$80,
  $1A,$E5,$46,$30,$E2,$0D,$54,$38,$C3,$94,$30,$50,$06,$7E,$00,$7F,
  $01,$80,$04,$7E,$00,$7F,$00,$EE,$4F,$24,$FF,$92,$F8,$C2,$F9,$80,
  $13,$E5,$46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$F9,$C2,$F8,$80,$04,
  $C2,$F8,$C2,$F9,$30,$47,$04,$AF,$3F,$80,$04,$E5,$3F,$F4,$FF,$02,
  $1B,$94,$E5,$47,$64,$0C,$60,$09,$E5,$47,$64,$0B,$60,$03,$02,$1B,
  $99,$90,$02,$28,$E0,$30,$47,$03,$FF,$80,$02,$F4,$FF,$8F,$3F,$90,
  $02,$29,$E0,$54,$FD,$F0,$E5,$3A,$14,$60,$20,$14,$60,$21,$14,$60,
  $2B,$24,$FC,$60,$45,$24,$F9,$60,$12,$24,$0E,$70,$4A,$E5,$46,$13,
  $13,$54,$3F,$75,$F0,$03,$84,$E5,$F0,$80,$29,$D2,$F9,$80,$3A,$E5,
  $46,$30,$E2,$03,$D3,$80,$01,$C3,$92,$F9,$80,$2D,$E5,$46,$30,$E2,
  $0D,$54,$38,$C3,$94,$30,$50,$06,$7E,$00,$7F,$01,$80,$04,$7E,$00,
  $7F,$00,$EE,$4F,$24,$FF,$92,$F9,$80,$0F,$E5,$46,$30,$E2,$03,$D3,
  $80,$01,$C3,$92,$F9,$80,$02,$C2,$F9,$30,$47,$04,$AF,$3F,$80,$04,
  $E5,$3F,$F4,$FF,$90,$02,$28,$EF,$F0,$22,$E5,$47,$B4,$0B,$10,$90,
  $02,$29,$E0,$54,$EB,$F0,$E5,$3F,$54,$EB,$45,$45,$F5,$3F,$22,$E4,
  $90,$02,$29,$F0,$30,$47,$04,$AF,$45,$80,$04,$E5,$45,$F4,$FF,$90,
  $02,$28,$EF,$F0,$22,$8F,$50,$D2,$59,$22,$8F,$54,$D2,$58,$22,$E4,
  $F5,$62,$C2,$AF,$E5,$51,$14,$60,$4B,$14,$60,$6C,$24,$02,$60,$03,
  $02,$1C,$DC,$D2,$59,$75,$55,$01,$90,$02,$A2,$E0,$54,$7F,$F0,$A3,
  $E0,$20,$E7,$23,$90,$04,$34,$E0,$B4,$02,$1C,$A3,$E0,$B4,$02,$17,
  $A3,$E0,$B4,$02,$12,$7F,$20,$12,$1B,$C5,$90,$10,$04,$E0,$54,$F3,
  $F0,$75,$51,$01,$02,$1C,$DC,$E5,$50,$60,$03,$02,$1C,$DC,$75,$62,
  $03,$02,$1C,$DC,$90,$12,$00,$E0,$54,$03,$70,$12,$7F,$20,$12,$1B,
  $C5,$90,$02,$A2,$E0,$54,$BF,$F0,$75,$51,$02,$02,$1C,$DC,$E5,$50,
  $60,$03,$02,$1C,$DC,$02,$1C,$D7,$90,$02,$A3,$E0,$30,$E6,$03,$02,
  $1C,$D3,$90,$04,$37,$E0,$64,$22,$70,$79,$90,$01,$8A,$74,$7E,$F0,
  $90,$01,$96,$F0,$90,$12,$04,$74,$0A,$F0,$90,$13,$28,$E0,$90,$70,
  $1A,$F0,$90,$13,$29,$E0,$90,$70,$1B,$F0,$90,$13,$2B,$E0,$90,$70,
  $22,$F0,$90,$13,$28,$E0,$54,$F0,$F0,$A3,$E0,$54,$F0,$F0,$90,$13,
  $2B,$E0,$54,$CC,$F0,$E5,$58,$30,$E3,$13,$E5,$3C,$F4,$90,$13,$2A,
  $60,$05,$E0,$54,$F3,$80,$11,$E0,$54,$FB,$F0,$80,$14,$E5,$3C,$F4,
  $90,$13,$2A,$60,$08,$E0,$54,$F2,$45,$3C,$F0,$80,$04,$E0,$54,$FA,
  $F0,$90,$04,$01,$E0,$54,$FD,$F0,$75,$62,$01,$75,$55,$02,$E4,$F5,
  $51,$80,$09,$E5,$50,$70,$05,$75,$62,$03,$F5,$51,$E5,$62,$60,$15,
  $C2,$01,$E4,$F5,$51,$C2,$59,$AD,$62,$AF,$40,$12,$1D,$A9,$E5,$62,
  $B4,$03,$02,$D2,$03,$D2,$AF,$22,$C2,$AF,$30,$01,$12,$E4,$90,$01,
  $96,$F0,$F5,$51,$C2,$59,$C2,$01,$7D,$02,$AF,$40,$12,$1D,$A9,$E5,
  $52,$14,$60,$0C,$04,$60,$03,$02,$1D,$A6,$75,$52,$01,$75,$55,$03,
  $90,$04,$01,$E0,$44,$0E,$F0,$E5,$58,$54,$18,$60,$1E,$90,$70,$1A,
  $E0,$90,$13,$28,$F0,$90,$70,$1B,$E0,$90,$13,$29,$F0,$A3,$74,$05,
  $F0,$90,$70,$22,$E0,$90,$13,$2B,$F0,$80,$11,$90,$13,$28,$E0,$44,
  $0F,$F0,$A3,$E0,$44,$0F,$F0,$A3,$E0,$44,$05,$F0,$90,$12,$04,$74,
  $03,$F0,$E5,$58,$30,$E3,$16,$90,$05,$00,$74,$80,$F0,$A3,$74,$08,
  $F0,$A3,$74,$01,$F0,$74,$03,$F0,$7F,$01,$12,$0D,$48,$90,$02,$A2,
  $E0,$44,$C0,$F0,$90,$10,$04,$E0,$44,$0C,$F0,$E4,$F5,$52,$F5,$55,
  $30,$02,$09,$C2,$02,$7D,$01,$AF,$41,$12,$1D,$A9,$30,$03,$02,$C2,
  $03,$E4,$90,$01,$96,$F0,$D2,$AF,$22,$EF,$F4,$60,$2D,$E4,$FE,$74,
  $14,$2E,$F5,$82,$E4,$34,$70,$F5,$83,$E0,$B4,$FF,$19,$74,$14,$2E,
  $F5,$82,$E4,$34,$70,$F5,$83,$EF,$F0,$74,$1C,$2E,$F5,$82,$E4,$34,
  $70,$F5,$83,$ED,$F0,$22,$0E,$BE,$04,$D5,$22,$22,$22,$90,$70,$2A,
  $E0,$30,$E1,$4D,$C2,$AF,$90,$70,$28,$E0,$90,$10,$1C,$F0,$90,$70,
  $29,$E0,$90,$10,$1D,$F0,$90,$70,$2A,$E0,$90,$10,$1E,$F0,$90,$10,
  $1C,$E0,$F5,$62,$90,$10,$1E,$E0,$20,$E1,$F3,$90,$10,$1C,$E0,$90,
  $70,$28,$F0,$90,$10,$1D,$E0,$90,$70,$29,$F0,$90,$10,$1E,$E0,$90,
  $70,$2A,$F0,$30,$4A,$07,$90,$70,$24,$E0,$44,$01,$F0,$C2,$05,$D2,
  $AF,$22,$20,$1C,$5A,$7D,$05,$12,$1F,$73,$90,$4C,$B0,$12,$1F,$64,
  $90,$4C,$BB,$EF,$F0,$7D,$03,$12,$1F,$73,$90,$4C,$BC,$12,$1F,$64,
  $90,$4C,$BD,$EF,$F0,$7D,$09,$12,$1F,$7E,$90,$4C,$B7,$EF,$F0,$7D,
  $08,$12,$1F,$7E,$90,$4C,$B6,$EF,$F0,$7D,$07,$12,$1F,$99,$90,$4C,
  $B5,$EF,$F0,$7D,$06,$12,$1F,$99,$90,$4C,$B4,$EF,$F0,$E4,$90,$4C,
  $B1,$F0,$90,$4C,$B9,$E0,$90,$4C,$B8,$F0,$D2,$1C,$C2,$07,$22,$7D,
  $01,$7F,$B8,$12,$17,$24,$7F,$B9,$12,$17,$3E,$EF,$64,$01,$60,$03,
  $02,$1F,$59,$D3,$90,$4C,$B3,$E0,$95,$27,$90,$4C,$B2,$E0,$95,$26,
  $40,$11,$90,$4C,$B1,$E0,$04,$F0,$90,$4C,$B8,$E0,$60,$1A,$E0,$14,
  $F0,$80,$15,$90,$4C,$B9,$E0,$FF,$90,$4C,$B8,$E0,$C3,$9F,$50,$08,
  $90,$4C,$B9,$E0,$90,$4C,$B8,$F0,$E4,$F5,$26,$F5,$27,$7D,$05,$7F,
  $B8,$12,$17,$24,$90,$4C,$B0,$12,$1F,$89,$7D,$02,$7F,$B8,$12,$17,
  $24,$90,$4C,$BB,$12,$1F,$89,$7D,$03,$7F,$B8,$12,$17,$24,$90,$4C,
  $BC,$12,$1F,$91,$7D,$04,$7F,$B8,$12,$17,$24,$90,$4C,$BD,$12,$1F,
  $91,$7D,$09,$7F,$B8,$12,$17,$24,$90,$4C,$B7,$12,$1F,$A4,$7D,$08,
  $7F,$B8,$12,$17,$24,$90,$4C,$B6,$12,$1F,$A4,$7D,$07,$7F,$B8,$12,
  $17,$24,$90,$4C,$B5,$E0,$FD,$7F,$B9,$12,$17,$24,$7D,$06,$7F,$B8,
  $12,$17,$24,$90,$4C,$B4,$E0,$FD,$7F,$B9,$12,$17,$24,$7D,$01,$12,
  $1F,$AC,$E4,$FD,$12,$1F,$AC,$80,$08,$05,$27,$E5,$27,$70,$02,$05,
  $26,$C2,$07,$22,$EF,$F0,$7D,$02,$7F,$B8,$12,$17,$24,$7F,$B9,$12,
  $17,$3E,$22,$7F,$B8,$12,$17,$24,$7F,$B9,$12,$17,$3E,$22,$7F,$B8,
  $12,$17,$24,$7F,$B9,$12,$17,$3E,$22,$E0,$FD,$7F,$B9,$12,$17,$24,
  $22,$E0,$FD,$7F,$B9,$12,$17,$24,$22,$7F,$B8,$12,$17,$24,$7F,$B9,
  $12,$17,$3E,$22,$E0,$FD,$7F,$B9,$12,$17,$24,$22,$7F,$B8,$12,$17,
  $24,$7D,$01,$7F,$B9,$12,$17,$24,$22,$22,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$21,$F7,$CD
 );
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RT2800USBInit;
var
 Status:LongWord;
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if RT2800USBInitialized then Exit;

 {Check Environment Variables}
 {RT2800USB_FIRMWARE_FILENAME}
 WorkBuffer:=SysUtils.GetEnvironmentVariable('RT2800USB_FIRMWARE_FILENAME');
 if Length(WorkBuffer) <> 0 then RT2800USB_FIRMWARE_FILENAME:=WorkBuffer;
 
 {RT2800USB_FIRMWARE_INTERNAL}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('RT2800USB_FIRMWARE_INTERNAL'),1);
 if WorkInt = 0 then RT2800USB_FIRMWARE_INTERNAL:=False;
 
 {RT2800USB_HARDWARE_ENCRYPTION_DISABLED}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('RT2800USB_HARDWARE_ENCRYPTION_DISABLED'),0);
 if WorkInt <> 0 then RT2800USB_HARDWARE_ENCRYPTION_DISABLED:=True;
 
 {Create RT2800USB Wireless Driver}
 RT2800USBDriver:=USBDriverCreate;
 if RT2800USBDriver <> nil then
  begin
   {Update RT2800USB Wireless Driver}
   {Driver}
   RT2800USBDriver.Driver.DriverName:=RT2800USB_DRIVER_NAME; 
   {USB}
   RT2800USBDriver.DriverBind:=RT2800USBDriverBind;
   RT2800USBDriver.DriverUnbind:=RT2800USBDriverUnbind;

   {Register RT2800USB Wireless Driver}
   Status:=USBDriverRegister(RT2800USBDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'RT2800USB: Failed to register RT2800USB driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'RT2800USB: Failed to create RT2800USB driver');
  end;
 
 RT2800USBInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{RT2800USB Network Functions}
function RT2800USBDeviceOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen for the RT2800USB device}
var
 Count:LongWord;
 Status:LongWord;
 Device:PUSBDevice;
 Entry:PNetworkEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'RT2800USB: Device Open');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    Result:=ERROR_ALREADY_OPEN;
    if Network.NetworkState <> NETWORK_STATE_CLOSED then Exit;
 
    {Set Result}
    Result:=ERROR_OPERATION_FAILED;
 
    //To Do //Set State to OPENING ? //Need to return to closed on fail as well
    
    {Load Firmware}
    Status:=RT2X00LoadFirmware(PRT2X00WiFiDevice(Network));
    if Status <> ERROR_SUCCESS then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to load firmware (Status=' + ErrorToString(Status) + ')');
      Result:=Status;
      Exit;
     end;
    
    {Initialize Queues}
    //To Do //rt2x00lib_start
    
    {Enable Radio}
    Status:=RT2X00EnableRadio(PRT2X00WiFiDevice(Network));
    if Status <> ERROR_SUCCESS then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to enable radio (Status=' + ErrorToString(Status) + ')');
      Result:=Status;
      Exit;
     end;
     
    try 
     {Allocate Receive Queue Buffer}
     Network.ReceiveQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry) + SizeOf(TIEEE80211RXStatus),RT2800USB_MAX_RX_ENTRIES);
     if Network.ReceiveQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to create receive queue buffer');
       
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end;
     
     {Allocate Receive Queue Semaphore}
     Network.ReceiveQueue.Wait:=SemaphoreCreate(0);
     if Network.ReceiveQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to create receive queue semaphore');
       
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end;
       
     {Allocate Receive Queue Buffers}
     Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=RT2X00GetRXBufferSize(PRT2X00WiFiDevice(Network));
       Entry.Offset:=0;     //To Do //Offset //See drivers
       Entry.Count:=1;
       
       {Allocate USB Request Buffer}
       Entry.Buffer:=USBBufferAllocate(Device,Entry.Size);
       if Entry.Buffer = nil then
        begin
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to allocate USB receive buffer');
         
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
       
       {Initialize Packets}
       SetLength(Entry.Packets,Entry.Count);
       
       {Initialize Packet}
       Entry.Packets[0].Buffer:=Entry.Buffer;
       Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
       Entry.Packets[0].Length:=Entry.Size - Entry.Offset;
       
       Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
      end;
     
     {Allocate Receive Queue Entries}
     SetLength(Network.ReceiveQueue.Entries,RT2800USB_MAX_RX_ENTRIES);
     
     {Allocate Transmit Queue Buffer}
     Network.TransmitQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry) + SizeOf(TIEEE80211TXInfo),RT2800USB_MAX_TX_ENTRIES);
     if Network.TransmitQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to create transmit queue buffer');
       
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end;
     
     {Allocate Transmit Queue Buffers}
     Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=SIZE_4K; //To Do //Size per buffer //See drivers
       Entry.Offset:=0;     //To Do //Offset //See drivers
       Entry.Count:=1;
       
       {Allocate USB Request Buffer}
       Entry.Buffer:=USBBufferAllocate(Device,Entry.Size);
       if Entry.Buffer = nil then
        begin
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to allocate USB transmit buffer');
         
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
        
       {Initialize Packets}
       SetLength(Entry.Packets,Entry.Count);
      
       {Initialize Packet}
       Entry.Packets[0].Buffer:=Entry.Buffer;
       Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
       Entry.Packets[0].Length:=Entry.Size - Entry.Offset;
       
       Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
      end;
     
     {Allocate Receive Requests}
     for Count:=0 to Length(PRT2800USBWiFiDevice(Network).ReceiveRequests) - 1 do
      begin
       PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Request:=USBRequestAllocate(Device,PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Endpoint,RT2800USBReceiveComplete,0,PRT2800USBWiFiDevice(Network).ReceiveRequests[Count]);
       if PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Request = nil then
        begin
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to allocate USB receive request');
         
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
      end;
     
     {Allocate Transmit Requests}
     for Count:=0 to Length(PRT2800USBWiFiDevice(Network).TransmitRequests) - 1 do
      begin
       PRT2800USBWiFiDevice(Network).TransmitRequests[Count].Request:=USBRequestAllocate(Device,PRT2800USBWiFiDevice(Network).TransmitRequests[Count].Endpoint,RT2800USBTransmitComplete,0,PRT2800USBWiFiDevice(Network).TransmitRequests[Count]);
       if PRT2800USBWiFiDevice(Network).TransmitRequests[Count].Request = nil then
        begin
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to allocate USB transmit request');
         
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
      end;
     
     {Submit Receive Requests}
     for Count:=0 to Length(PRT2800USBWiFiDevice(Network).ReceiveRequests) - 1 do
      begin
       {Get Entry}
       Entry:=BufferGet(Network.ReceiveQueue.Buffer);
       if Entry <> nil then
        begin
         {Update Pending}
         Inc(PRT2800USBWiFiDevice(Network).PendingCount);
         
         {Update Free}
         PRT2800USBWiFiDevice(Network).ReceiveFree:=PRT2800USBWiFiDevice(Network).ReceiveFree xor (1 shl Count);
         
         {Update Entry}
         Entry.DriverData:=Network;
         
         {Update Request}
         PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Entry:=Entry;
         
         {Reinitialize Request}
         USBRequestInitialize(PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Request,RT2800USBReceiveComplete,Entry.Buffer,Entry.Size,PRT2800USBWiFiDevice(Network).ReceiveRequests[Count]);
         
         {Submit Request}
         Status:=USBRequestSubmit(PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Request);
         if Status <> USB_STATUS_SUCCESS then
          begin
           if NETWORK_LOG_ENABLED then NetworkLogError(Network,'RT2800USB: Failed to submit USB receive request: ' + USBStatusToString(Status));
         
           {Update Pending}
           Dec(PRT2800USBWiFiDevice(Network).PendingCount);
           
           {Update Free}
           PRT2800USBWiFiDevice(Network).ReceiveFree:=PRT2800USBWiFiDevice(Network).ReceiveFree or (1 shl Count);
           
           {Update Entry}
           Entry.DriverData:=nil;
           
           {Update Request}
           PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Entry:=nil;
           
           {Free Entry}
           BufferFree(Entry);
          end;
        end;
      end;
     
     {Check Pending Count}
     if PRT2800USBWiFiDevice(Network).PendingCount = 0 then
      begin
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end;
     
     
     /////////////////////////////////////////////////////////////////////////////////////////////////
     
     //To Do //Continuing
     //DoBulkIn //F:\Download\Ralink\DPO_RT5572_LinuxSTA_2.6.1.3_20121022\common\rtusb_bulk.c
     //RTUSBInitRxDesc
     //RTUSBBulkReceive
     //RTUSBBulkRxComplete
     
     //RTMPAllocUsbBulkBufStruct
     //RTMPAllocTxRxRingMemory
     
     //To Do //rt2x00lib_enable_radio //Enable/Start Queues //Start watchdog monitoring
     
     /////////////////////////////////////////////////////////////////////////////////////////////////
     
     {Set State to Open}
     Network.NetworkState:=NETWORK_STATE_OPEN;
     
     {Notify the State}
     NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_OPEN); 
     
     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Check Result}
     if Result <> ERROR_SUCCESS then
      begin
       {Release Transmit Requests}
       for Count:=0 to Length(PRT2800USBWiFiDevice(Network).TransmitRequests) - 1 do
        begin
         USBRequestRelease(PRT2800USBWiFiDevice(Network).TransmitRequests[Count].Request);
        end;
       
       {Release Receive Requests}
       for Count:=0 to Length(PRT2800USBWiFiDevice(Network).ReceiveRequests) - 1 do
        begin
         //To Do //Cancel the request first ?
         USBRequestRelease(PRT2800USBWiFiDevice(Network).ReceiveRequests[Count].Request);
        end;
       
       {Release Transmit Queue Buffers}
       Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
       while Entry <> nil do
        begin
         {Release USB Buffer}
         USBBufferRelease(Entry.Buffer);
         
         {Release Packets}
         SetLength(Entry.Packets,0);
         
         Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
        end; 

       {Destroy Transmit Queue Buffer}
       BufferDestroy(Network.TransmitQueue.Buffer);
       
       {Release Receive Queue Entries}
       SetLength(Network.ReceiveQueue.Entries,0);
       
       {Release Receive Queue Buffers}
       Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
       while Entry <> nil do
        begin
         {Release USB Buffer}
         USBBufferRelease(Entry.Buffer);
         
         {Release Packets}
         SetLength(Entry.Packets,0);
         
         Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
        end; 
       
       {Destroy Receive Queue Semaphore}
       SemaphoreDestroy(Network.ReceiveQueue.Wait);
       
       {Destroy Receive Queue Buffer}
       BufferDestroy(Network.ReceiveQueue.Buffer);
       
       {Disable Radio}
       //To Do 
      end;
    end;    
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;
 
{==============================================================================}

function RT2800USBDeviceClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose for the RT2800USB device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'RT2800USB: Device Close');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_OPEN;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Set State to Closing}
 Result:=ERROR_OPERATION_FAILED;
 if NetworkDeviceSetState(Network,NETWORK_STATE_CLOSING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
 
    //To Do //rt2x00lib_stop
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;
 
{==============================================================================}

function RT2800USBDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of NetworkDeviceControl for the RT2800USB device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'RT2800USB: Device Control');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
 
    //To Do
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;
 
{==============================================================================}
 
function RT2800USBBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferAllocate for the RT2800USB device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'RT2800USB: Buffer Allocate');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Set Result}
 Result:=ERROR_OPERATION_FAILED;

 {Wait for Entry}
 Entry:=BufferGet(Network.TransmitQueue.Buffer);
 if Entry <> nil then
  begin
   //To Do //Continuing //Update Packet ?
   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function RT2800USBBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferRelease for the RT2800USB device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Entry}
 if Entry = nil then Exit;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'RT2800USB: Buffer Release');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Free Entry}
    Result:=BufferFree(Entry);
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function RT2800USBBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferReceive for the RT2800USB device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'RT2800USB: Buffer Receive');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Wait for Entry}
 if SemaphoreWait(Network.ReceiveQueue.Wait) = ERROR_SUCCESS then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Remove Entry}
      Entry:=Network.ReceiveQueue.Entries[Network.ReceiveQueue.Start];
      
      {Update Start}
      Network.ReceiveQueue.Start:=(Network.ReceiveQueue.Start + 1) mod RT2800USB_MAX_RX_ENTRIES;
      
      {Update Count}
      Dec(Network.ReceiveQueue.Count);
      
      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Network.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;  
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function RT2800USBBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferTransmit for the RT2800USB device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Entry}
 if Entry = nil then Exit;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'RT2800USB: Buffer Transmit');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
 
    //To Do //Continuing //Use the TransmitQueue ? and Wait for completion ?
                         //Need to check empty and do Submit ?
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}
{==============================================================================}
{RT2800USB WiFi Functions}
function RT2800USBDeviceConfigure(WiFi:PWiFiDevice;Flags:LongWord):LongWord;
{Implementation of WiFiDeviceConfigure for the RT2800USB device}

{rt2x00mac_config}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check WiFi}
 if WiFi = nil then Exit;
 if WiFi.Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@WiFi.Network,'RT2800USB: Device Configure (Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(WiFi.Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Some configuration parameters (e.g. channel and antenna values) can only be set when the radio is enabled, but do require the RX to be off}
    Status:=RT2X00DisableRX(PRT2X00WiFiDevice(WiFi));
    if Status <> ERROR_SUCCESS then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(@WiFi.Network,'RT2800USB: Failed to disable RX (Status=' + ErrorToString(Status) + ')');
      Result:=Status;
      Exit;
     end;
 
    {If we've just turned on the radio, we want to reprogram everything to ensure a consistent state}
    Status:=RT2X00Configure(PRT2X00WiFiDevice(WiFi),Flags);
    if Status <> ERROR_SUCCESS then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(@WiFi.Network,'RT2800USB: Failed to configure device (Status=' + ErrorToString(Status) + ')');
      Result:=Status;
      Exit;
     end;
    
    {After the radio has been enabled we need to configure the antenna to the default settings}
    {RT2X00ConfigureAntenna should determine if any action should be taken based on checking if diversity has been enabled or no antenna changes have been made since the last configuration change}
    Status:=RT2X00ConfigureAntenna(PRT2X00WiFiDevice(WiFi),@PRT2X00WiFiDevice(WiFi).Antenna);
    if Status <> ERROR_SUCCESS then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(@WiFi.Network,'RT2800USB: Failed to configure antenna (Status=' + ErrorToString(Status) + ')');
      Result:=Status;
      Exit;
     end;
    
    {Turn RX back on}
    Status:=RT2X00EnableRX(PRT2X00WiFiDevice(WiFi));
    if Status <> ERROR_SUCCESS then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(@WiFi.Network,'RT2800USB: Failed to enable RX (Status=' + ErrorToString(Status) + ')');
      Result:=Status;
      Exit;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(WiFi.Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function RT2800USBDeviceConfigureFilter(WiFi:PWiFiDevice;var Filter:LongWord):LongWord;
{Implementation of WiFiDeviceConfigureFilter for the RT2800USB device}

{rt2x00mac_configure_filter}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check WiFi}
 if WiFi = nil then Exit;
 if WiFi.Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@WiFi.Network,'RT2800USB: Device Configure Filter (Filter=' + IntToHex(Filter,8) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(WiFi.Network.Lock) = ERROR_SUCCESS then
  begin
   try
    //To Do //rt2x00mac_configure_filter
 
    //
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(WiFi.Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;
 
{==============================================================================}
 
function RT2800USBDeviceConfigureInterface(WiFi:PWiFiDevice;Interrface:PWiFiInterface):LongWord; 
{Implementation of WiFiDeviceConfigureInterface for the RT2800USB device}

{rt2x00mac_add_interface / rt2x00mac_remove_interface}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check WiFi}
 if WiFi = nil then Exit;
 if WiFi.Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@WiFi.Network,'RT2800USB: Device Configure Interface');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(WiFi.Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Type}
    if Interrface.InterfaceType = WIFI_IFTYPE_AP then
     begin
      Inc(PRT2X00WiFiDevice(WiFi).APInterfaceCount);
     end
    else
     begin
      Inc(PRT2X00WiFiDevice(WiFi).STAInterfaceCount);
     end;     
 
    //To Do //rt2x00mac_add_interface
    //To Do //rt2x00mac_remove_interface -> rt2x00lib_config_intf / rt2800_config_intf
    
    {The MAC address must be configured after the device has been initialized. Otherwise the device can reset the MAC registers}
    {The BSSID address must only be configured in AP mode, however we should not send an empty BSSID address for STA interfaces at this time, since this can cause invalid behavior in the device}
    Status:=RT2X00ConfigureInterface(PRT2X00WiFiDevice(WiFi),Interrface.InterfaceType,@Interrface.Address,nil);
    if Status <> ERROR_SUCCESS then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(@WiFi.Network,'RT2800USB: Failed to configure interface (Status=' + ErrorToString(Status) + ')');
      Result:=Status;
      Exit;
     end;
    
    {Some filters depend on the current working mode. We can force an update during the next DeviceConfigureFilter by resetting the current PacketFilter state}
    PRT2X00WiFiDevice(WiFi).PacketFilter:=0;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(WiFi.Network.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;
 
{==============================================================================}
{==============================================================================}
{RT2800USB USB Functions}
function RT2800USBDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the RT2800USB driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Index:Byte;
 Count:LongWord;
 Status:LongWord;
 RT2800USB:PRT2800USBWiFiDevice;
 NetworkInterface:PUSBInterface;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}
 
 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Interface bind not supported by driver');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check RT2800USB Device}
 if RT2800USBCheckDevice(Device) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Device not found in supported device list');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device Speed}
 if Device.Speed <> USB_SPEED_HIGH then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Device speed is not USB_SPEED_HIGH');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Interface}
 NetworkInterface:=USBDeviceFindInterfaceByIndex(Device,0);
 if NetworkInterface = nil then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Device has no available interface');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF RT2800USB_DEBUG}
  if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Interface.bInterfaceNumber=' + IntToStr(NetworkInterface.Descriptor.bInterfaceNumber));
 {$ENDIF}
 
 {Check Bulk IN Endpoint}
 ReceiveEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
 if ReceiveEndpoint = nil then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Device has no BULK IN endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: BULK IN Endpoint Count=' + IntToStr(USBDeviceCountEndpointsByType(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK)));
 {$ENDIF}
 
 {Check Bulk OUT Endpoint}
 TransmitEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
 if TransmitEndpoint = nil then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Device has no BULK OUT endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: BULK OUT Endpoint Count=' + IntToStr(USBDeviceCountEndpointsByType(Device,NetworkInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK)));
 {$ENDIF}
 
 {Check Configuration}
 if Device.ConfigurationValue = 0 then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Assigning configuration ' + IntToStr(Device.Configuration.Descriptor.bConfigurationValue) + ' (' + IntToStr(Device.Configuration.Descriptor.bNumInterfaces) + ' interfaces available)');
   {$ENDIF}
   
   {Set Configuration}
   Status:=USBDeviceSetConfiguration(Device,Device.Configuration.Descriptor.bConfigurationValue);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to set device configuration: ' + USBStatusToString(Status));
     
     {Return Result}
     Result:=Status;
     Exit;
    end;
  end;
 
 {USB device reset not required because the USB core already did a reset on the port during attach}
 
 {Create WiFi}
 RT2800USB:=PRT2800USBWiFiDevice(WiFiDeviceCreateEx(SizeOf(TRT2800USBWiFiDevice)));
 if RT2800USB = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to create new wifi device');
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update WiFi} 
 {Device}
 RT2800USB.RT2X00.WiFi.Network.Device.DeviceBus:=DEVICE_BUS_USB;
 RT2800USB.RT2X00.WiFi.Network.Device.DeviceType:=NETWORK_TYPE_80211;
 RT2800USB.RT2X00.WiFi.Network.Device.DeviceFlags:=NETWORK_FLAG_RX_BUFFER or NETWORK_FLAG_TX_BUFFER or NETWORK_FLAG_RX_MULTIPACKET;     
 RT2800USB.RT2X00.WiFi.Network.Device.DeviceData:=Device;
 RT2800USB.RT2X00.WiFi.Network.Device.DeviceDescription:=RT2800USB_NETWORK_DESCRIPTION;
 {Network}
 RT2800USB.RT2X00.WiFi.Network.NetworkState:=NETWORK_STATE_CLOSED;
 RT2800USB.RT2X00.WiFi.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
 RT2800USB.RT2X00.WiFi.Network.DeviceOpen:=RT2800USBDeviceOpen;
 RT2800USB.RT2X00.WiFi.Network.DeviceClose:=RT2800USBDeviceClose;
 RT2800USB.RT2X00.WiFi.Network.DeviceControl:=RT2800USBDeviceControl;
 RT2800USB.RT2X00.WiFi.Network.BufferAllocate:=RT2800USBBufferAllocate;
 RT2800USB.RT2X00.WiFi.Network.BufferRelease:=RT2800USBBufferRelease;
 RT2800USB.RT2X00.WiFi.Network.BufferReceive:=RT2800USBBufferReceive;
 RT2800USB.RT2X00.WiFi.Network.BufferTransmit:=RT2800USBBufferTransmit;
 {WiFi}
 //To Do //State/Status/Flags etc
 RT2800USB.RT2X00.WiFi.DeviceConfigure:=RT2800USBDeviceConfigure;
 RT2800USB.RT2X00.WiFi.DeviceConfigureFilter:=RT2800USBDeviceConfigureFilter;
 RT2800USB.RT2X00.WiFi.DeviceConfigureInterface:=RT2800USBDeviceConfigureInterface;
 //To Do
 {Driver}
 {RT2X00}
 RT2800USB.RT2X00.DriverInit:=RT2800USBDriverInit;
 RT2800USB.RT2X00.EepromLoad:=RT2800USBEepromLoad;
 RT2800USB.RT2X00.RegisterRead:=RT2X00USBRegisterRead;
 RT2800USB.RT2X00.RegisterWrite:=RT2X00USBRegisterWrite;
 RT2800USB.RT2X00.RegisterMultiRead:=RT2X00USBRegisterMultiRead;
 RT2800USB.RT2X00.RegisterMultiWrite:=RT2X00USBRegisterMultiWrite;
 RT2800USB.RT2X00.RegisterBusyRead:=RT2X00USBRegisterBusyRead;
 RT2800USB.RT2X00.SetLED:=RT2800SetLED;
 RT2800USB.RT2X00.SetState:=RT2800USBSetState;
 RT2800USB.RT2X00.EnableRX:=RT2800USBEnableRX;
 RT2800USB.RT2X00.DisableRX:=RT2800USBDisableRX;
 RT2800USB.RT2X00.EnableBeacon:=RT2800USBEnableBeacon;
 RT2800USB.RT2X00.DisableBeacon:=RT2800USBDisableBeacon;
 RT2800USB.RT2X00.GetFirmware:=RT2800USBGetFirmware;
 RT2800USB.RT2X00.CheckFirmware:=RT2800CheckFirmware;
 RT2800USB.RT2X00.LoadFirmware:=RT2800LoadFirmware;
 RT2800USB.RT2X00.WriteFirmware:=RT2800USBWriteFirmware;
 RT2800USB.RT2X00.Configure:=RT2800Configure;
 RT2800USB.RT2X00.ConfigureFilter:=RT2800ConfigureFilter;
 RT2800USB.RT2X00.ConfigureAntenna:=RT2800ConfigureAntenna;
 RT2800USB.RT2X00.ConfigureInterface:=RT2800ConfigureInterface;
 RT2800USB.RT2X00.InitializeRegisters:=RT2800USBInitializeRegisters;
 RT2800USB.RT2X00.HardwareEncryptionDisabled:=RT2800USBHardwareEncryptionDisabled;
 {RT2X00}
 RT2800USB.RT2X00.DataSize:=SizeOf(TRT2800Data);
 RT2800USB.RT2X00.RFSize:=RT2800_RF_SIZE;
 RT2800USB.RT2X00.EepromSize:=RT2800_EEPROM_SIZE;
 RT2800USB.RT2X00.CSRLock:=INVALID_HANDLE_VALUE;
 //To Do
 {USB}
 SetLength(RT2800USB.ReceiveRequests,USBDeviceCountEndpointsByType(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK));
 SetLength(RT2800USB.TransmitRequests,USBDeviceCountEndpointsByType(Device,NetworkInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK));
 RT2800USB.WaiterThread:=INVALID_HANDLE_VALUE;

 try
  {Create Data}
  RT2800USB.RT2X00.Data:=AllocMem(RT2800USB.RT2X00.DataSize);
  if RT2800USB.RT2X00.Data = nil then
   begin
    if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to allocate chipset data for wifi');
    
    {Return Result}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED;
    Exit;
   end;
  {$IFDEF RT2800USB_DEBUG}
  if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB:  (Chipset Data=' + IntToHex(LongWord(RT2800USB.RT2X00.Data),8) + ')');
  {$ENDIF}
  
  {Create RF}
  RT2800USB.RT2X00.RFData:=AllocMem(RT2800USB.RT2X00.RFSize);
  if RT2800USB.RT2X00.RFData = nil then
   begin
    if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to allocate RF data for wifi');
    
    {Return Result}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED;
    Exit;
   end;
  {$IFDEF RT2800USB_DEBUG}
  if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB:  (RF Data=' + IntToHex(LongWord(RT2800USB.RT2X00.RFData),8) + ')');
  {$ENDIF}
   
  {Create Eeprom}
  RT2800USB.RT2X00.EepromData:=AllocMem(RT2800USB.RT2X00.EepromSize);
  if RT2800USB.RT2X00.EepromData = nil then
   begin
    if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to allocate EEPROM data for wifi');
    
    {Return Result}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED;
    Exit;
   end;
  {$IFDEF RT2800USB_DEBUG}
  if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB:  (EEPROM Data=' + IntToHex(LongWord(RT2800USB.RT2X00.EepromData),8) + ')');
  {$ENDIF}
  
  {Create Lock} 
  RT2800USB.RT2X00.CSRLock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
  if RT2800USB.RT2X00.CSRLock = INVALID_HANDLE_VALUE then
   begin
    if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to create CSR lock for wifi');
    
    {Return Result}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED;
    Exit;
   end;
  
  {Create Receive Requests / Assign IN Endpoints}
  Index:=0;
  for Count:=0 to Length(RT2800USB.ReceiveRequests) - 1 do
   begin
    RT2800USB.ReceiveRequests[Count]:=AllocMem(SizeOf(TRT2800USBRequest));
    if RT2800USB.ReceiveRequests[Count] = nil then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to allocate receive request for wifi');
      
      {Return Result}
      Result:=USB_STATUS_DEVICE_UNSUPPORTED;
      Exit;
     end;

    RT2800USB.ReceiveRequests[Count].Index:=Count;
    RT2800USB.ReceiveRequests[Count].Endpoint:=USBDeviceFindEndpointByTypeEx(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK,Index);
    {Note: USB Request allocated by DeviceOpen}
    {      Network Entry set by Receive (Submit)}
    RT2800USB.ReceiveMask:=RT2800USB.ReceiveMask or (1 shl Count);
   end;
  RT2800USB.ReceiveFree:=RT2800USB.ReceiveMask;
  
  {Create Transmit Requests / Assign OUT Endpoints}
  Index:=0;
  for Count:=0 to Length(RT2800USB.TransmitRequests) - 1 do
   begin
    RT2800USB.TransmitRequests[Count]:=AllocMem(SizeOf(TRT2800USBRequest));
    if RT2800USB.TransmitRequests[Count] = nil then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to allocate transmit request for wifi');
      
      {Return Result}
      Result:=USB_STATUS_DEVICE_UNSUPPORTED;
      Exit;
     end;
     
    RT2800USB.TransmitRequests[Count].Index:=Count;
    RT2800USB.TransmitRequests[Count].Endpoint:=USBDeviceFindEndpointByTypeEx(Device,NetworkInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK,Index);
    {Note: USB Request allocated by DeviceOpen}
    {      Network Entry set by Transmit (Submit)}
    RT2800USB.TransmitMask:=RT2800USB.TransmitMask or (1 shl Count);
   end;
  RT2800USB.TransmitFree:=RT2800USB.TransmitMask;
  
  {Initialize Driver}
  if RT2X00DriverInit(@RT2800USB.RT2X00) <> ERROR_SUCCESS then
   begin
    if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to initialize driver for wifi');
    
    {Return Result}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED;
    Exit;
   end;
   
  {Register WiFi} 
  if WiFiDeviceRegister(@RT2800USB.RT2X00.WiFi) <> ERROR_SUCCESS then
   begin
    if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to register new wifi device');
    
    {Return Result}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED;
    Exit;
   end;
  
  {Update Device}
  Device.DriverData:=RT2800USB;
  
  {Return Result}
  Result:=USB_STATUS_SUCCESS;
 finally
  {Check Result}
  if Result <> USB_STATUS_SUCCESS then
   begin
    {Free Data}
    FreeMem(RT2800USB.RT2X00.Data);
    
    {Free RF}
    FreeMem(RT2800USB.RT2X00.RFData);
  
    {Free Eeprom}
    FreeMem(RT2800USB.RT2X00.EepromData);
    
    {Destroy Lock}
    MutexDestroy(RT2800USB.RT2X00.CSRLock);
  
    {Release Receive Requests}
    for Count:=0 to Length(RT2800USB.ReceiveRequests) - 1 do
     begin
      FreeMem(RT2800USB.ReceiveRequests[Count]);
     end;
    SetLength(RT2800USB.ReceiveRequests,0);
    
    {Release Transmit Requests}
    for Count:=0 to Length(RT2800USB.TransmitRequests) - 1 do
     begin
      FreeMem(RT2800USB.TransmitRequests[Count]);
     end;
    SetLength(RT2800USB.TransmitRequests,0);
  
    {Destroy WiFi}
    WiFiDeviceDestroy(@RT2800USB.RT2X00.WiFi);
   end;
 end; 
end;
  
{==============================================================================}

function RT2800USBDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the RT2800USB driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var 
 Count:LongWord;
 RT2800USB:PRT2800USBWiFiDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then Exit;
 
 {Check Driver}
 if Device.Driver <> RT2800USBDriver then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Get WiFi}
 RT2800USB:=PRT2800USBWiFiDevice(Device.DriverData);
 if RT2800USB = nil then Exit;
 
 {Close Network}
 RT2800USBDeviceClose(@RT2800USB.RT2X00.WiFi.Network);
 
 {Update Device}
 Device.DriverData:=nil;
 
 {Deregister WiFi}
 if WiFiDeviceDeregister(@RT2800USB.RT2X00.WiFi) <> ERROR_SUCCESS then Exit;
 
 {Terminate Driver}
 RT2X00USBDriverQuit(@RT2800USB.RT2X00);
 
 {Free Data}
 FreeMem(RT2800USB.RT2X00.Data);
 
 {Free RF}
 FreeMem(RT2800USB.RT2X00.RFData);
 
 {Free Eeprom}
 FreeMem(RT2800USB.RT2X00.EepromData);
 
 {Destroy Lock}
 MutexDestroy(RT2800USB.RT2X00.CSRLock);
 
 {Release Receive Requests}
 for Count:=0 to Length(RT2800USB.ReceiveRequests) - 1 do
  begin
   FreeMem(RT2800USB.ReceiveRequests[Count]);
  end;
 SetLength(RT2800USB.ReceiveRequests,0);
 
 {Release Transmit Requests}
 for Count:=0 to Length(RT2800USB.TransmitRequests) - 1 do
  begin
   FreeMem(RT2800USB.TransmitRequests[Count]);
  end;
 SetLength(RT2800USB.TransmitRequests,0);
 
 {Destroy WiFi}
 WiFiDeviceDestroy(@RT2800USB.RT2X00.WiFi);
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure RT2800USBReceiveWorker(Request:PUSBRequest); 
{Called (by a Worker thread) to process a completed USB request from the RT2800USB bulk IN endpoint}
{Request: The USB request which has completed}
var
 Data:Pointer;
 Size:LongWord;
 RXINFO:Pointer;
 Value:LongWord;
 Status:LongWord;
 Message:TMessage;
 RateIndex:LongWord;
 PacketLength:LongWord;
 HeaderLength:LongWord;
 Next:PNetworkEntry;
 Entry:PNetworkEntry;
 RXStatus:PIEEE80211RXStatus;
 Descriptor:TRT2X00RXDescriptor;
 RT2800USB:PRT2800USBWiFiDevice;
 RT2800USBRequest:PRT2800USBRequest;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Request}
 RT2800USBRequest:=PRT2800USBRequest(Request.DriverData);
 if (RT2800USBRequest <> nil) and (RT2800USBRequest.Request = Request) then
  begin
   {Get Entry}
   Entry:=RT2800USBRequest.Entry;
   if Entry <> nil then
    begin
     {Get WiFi}
     RT2800USB:=PRT2800USBWiFiDevice(Entry.DriverData);
     if RT2800USB <> nil then 
      begin
       {Acquire the Lock}
       if MutexLock(RT2800USB.RT2X00.WiFi.Network.Lock) = ERROR_SUCCESS then
        begin
         try
          {Update Statistics}
          Inc(RT2800USB.RT2X00.WiFi.Network.ReceiveCount); 
     
          {Check State}
          if RT2800USB.RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_CLOSING then
           begin
            {$IFDEF RT2800USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
            {$ENDIF}
          
            {Update Request}
            Request.Status:=USB_STATUS_DEVICE_DETACHED;
           end;
     
          {Check Result} 
          if Request.Status = USB_STATUS_SUCCESS then
           begin
            {$IFDEF RT2800USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: Receive complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
            {$ENDIF}
            {RX frame format is :  | RXINFO | RXWI | header | L2 pad | payload | pad | RXD | USB pad |
                                            |<----------- PacketLength ------------->|                  }
            {Get Data and Size}
            Data:=Request.Data;
            Size:=Request.ActualSize;
            
            {Get RXINFO}
            RXINFO:=Data;
            Value:=RT2X00ReadDescriptor(RXINFO,0);
            
            {$IFDEF RT2800USB_DEBUG}
            //if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: (RXINFO Value=' + IntToHex(Value,8) + ')'); //To Do
            {$ENDIF}
            
            {Get Packet Length}
            PacketLength:=RT2X00GetRegister32(Value,RT2800USB_RXINFO_W0_USB_DMA_RX_PKT_LEN,0);
            
            {$IFDEF RT2800USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: (PacketLength=' + IntToStr(PacketLength) + ')');
            {$ENDIF}
            
            {Check Packet Length}
            if (PacketLength > 0) and (PacketLength <= RT2X00_AGGREGATION_SIZE) then
             begin
              {Update Data and Size (Remove the RXINFO from start of buffer)}
              Inc(Data,RT2800USB_RXINFO_DESC_SIZE);
              Dec(Size,RT2800USB_RXINFO_DESC_SIZE);
              
              {Get Next}
              Next:=nil;
              if BufferAvailable(RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Buffer) > 0 then
               begin
                Next:=BufferGet(RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Buffer);
               end;
               
              {Check Next} 
              if Next <> nil then
               begin
                if RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Count < RT2800USB_MAX_RX_ENTRIES then
                 begin
                  {Setup Descriptor}
                  FillChar(Descriptor,SizeOf(TRT2X00RXDescriptor),0);
                  
                  {Process RXD}
                  RT2800USBReceiveProcessRXD(@RT2800USB.RT2X00,@Descriptor,Data,Size,PacketLength);
                  
                  {Process RXWI}
                  RT2800ReceiveProcessRXWI(@RT2800USB.RT2X00,@Descriptor,Data,Size);
                  
                  {$IFDEF RT2800USB_DEBUG}
                  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: (Descriptor.Size=' + IntToStr(Descriptor.Size) + ')');
                  {$ENDIF}
                  
                  {Check Descriptor Size}
                  if (Descriptor.Size > 0) and (Descriptor.Size <= RT2X00_AGGREGATION_SIZE) then
                   begin
                    {The data behind the IEEE80211 header must be aligned on a 4 byte boundary}
                    HeaderLength:=IEEE80211HeaderLengthFromBuffer(Data,Size);
                    
                    {$IFDEF RT2800USB_DEBUG}
                    if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: (HeaderLength=' + IntToStr(HeaderLength) + ')');
                    {$ENDIF}
                    
                    {Hardware might have stripped the IV/EIV/ICV data, it is possible that the data was provided
                     separately (through hardware descriptor) in which case we should reinsert the data into the frame}
                    if ((Descriptor.RXFlags and RT2X00_RXDONE_CRYPTO_IV) <> 0) and ((Descriptor.Flags and WIFI_RX_FLAG_IV_STRIPPED) <> 0) then
                     begin
                      RT2X00ReceiveInsertIV(@Descriptor,Data,Size,HeaderLength);
                     end
                    else if (HeaderLength <> 0) and (Descriptor.Size > HeaderLength) and ((Descriptor.RXFlags and RT2X00_RXDONE_L2PAD) <> 0) then
                     begin
                      RT2X00RemoveL2PAD(Data,Size,HeaderLength);
                     end;
                    
                    {Update Size (Actual size from descriptor)}
                    Size:=Descriptor.Size;
                    
                    {Translate the signal to the correct bitrate index}
                    RateIndex:=RT2X00ReceiveReadSignal(@RT2800USB.RT2X00,@Descriptor);
                    if (Descriptor.RateMode = RT2X00_RATE_MODE_HT_MIX) or (Descriptor.RateMode = RT2X00_RATE_MODE_HT_GREENFIELD) then
                     begin
                      Descriptor.Flags:=Descriptor.Flags or WIFI_RX_FLAG_HT;
                     end;
                    
                    //To Do //Continuing //rt2x00lib_rxdone_check_ps
                    
                    //To Do //Continuing //rt2x00lib_rxdone_check_ba
                    
                    {Update extra components}
                    //To Do //Continuing //rt2x00link_update_stats
                    //To Do //Continuing //rt2x00debug_update_crypto
                    //To Do //Continuing //rt2x00debug_dump_frame
                    
                    {Setup RX Status}
                    RXStatus:=PIEEE80211RXStatus(PtrUInt(Entry) + SizeOf(TNetworkEntry));
                    FillChar(RXStatus^,SizeOf(TIEEE80211RXStatus),0);
                    
                    {Update RX Status}
                    RXStatus.MACTime:=Descriptor.Timestamp;
                    RXStatus.Band:=RT2800USB.RT2X00.CurrentBand;
                    RXStatus.Frequency:=RT2800USB.RT2X00.CurrentFrequency;
                    RXStatus.RateIndex:=RateIndex;
                    RXStatus.Signal:=Descriptor.RSSI;
                    RXStatus.Flags:=Descriptor.Flags;
                    RXStatus.Antenna:=RT2800USB.RT2X00.Link.Antenna.ActiveAntenna.RX;
                 
                    {Update Packet}
                    Entry.Packets[0].Buffer:=Entry.Buffer;
                    Entry.Packets[0].Data:=Data;
                    Entry.Packets[0].Length:=Size;
                    
                    {Update Entry}
                    Entry.DriverData:=RXStatus;
                    
                    {Add Entry}
                    RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Entries[(RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Start + RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Count) mod RT2800USB_MAX_RX_ENTRIES]:=Entry;
                    
                    {Update Count}
                    Inc(RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Count);
                    
                    {Signal Packet Received}
                    SemaphoreSignal(RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Wait);
                    
                    {$IFDEF RT2800USB_DEBUG}
                    //if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: (Queue Count=' + IntToStr(RT2800USB.RT2X00.WiFi.Network.ReceiveQueue.Count) + ')'); //To Do
                    {$ENDIF}
                   end
                  else
                   begin
                    if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Receive error (Descriptor.Size=' + IntToStr(Descriptor.Size) + ')');
                   
                    {Free Entry}
                    BufferFree(Entry);
                    
                    {Update Statistics}
                    Inc(RT2800USB.RT2X00.WiFi.Network.ReceiveErrors); 
                   end;
                 end
                else
                 begin
                  if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Receive queue overrun, packet discarded');
                  
                  {Free Entry}
                  BufferFree(Entry);
                  
                  {Update Statistics}
                  Inc(RT2800USB.RT2X00.WiFi.Network.BufferOverruns); 
                 end;
               end
              else
               begin
                if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: No receive buffer available, packet discarded');
                
                {Get Next}
                Next:=Entry;
                
                {Update Statistics}
                Inc(RT2800USB.RT2X00.WiFi.Network.BufferUnavailable); 
               end;
             end
            else
             begin
              if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Receive error (PacketLength=' + IntToStr(PacketLength) + ')');
    
              {Get Next}
              Next:=Entry;
    
              {Update Statistics}
              Inc(RT2800USB.RT2X00.WiFi.Network.ReceiveErrors); 
             end;         
           end
          else 
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Failed receive request (Status=' + USBStatusToString(Request.Status) + ')');
       
            {Get Next}
            Next:=Entry;
            
            {Update Statistics}
            Inc(RT2800USB.RT2X00.WiFi.Network.ReceiveErrors); 
           end;
     
          {Update Pending}
          Dec(RT2800USB.PendingCount); 
          
          {Update Free}
          RT2800USB.ReceiveFree:=RT2800USB.ReceiveFree or (1 shl RT2800USBRequest.Index);
          
          {Update Next}
          Next.DriverData:=nil;
          
          {Update Request}
          RT2800USBRequest.Entry:=nil;
          
          {Check State}
          if RT2800USB.RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_CLOSING then
           begin
            {Free Next}
            BufferFree(Next);
            
            {Check Pending}
            if RT2800USB.PendingCount = 0 then
             begin
              {Check Waiter}
              if RT2800USB.WaiterThread <> INVALID_HANDLE_VALUE then
               begin
                {$IFDEF RT2800USB_DEBUG}
                if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: Close pending, sending message to waiter thread (Thread=' + IntToHex(RT2800USB.WaiterThread,8) + ')');
                {$ENDIF}
                
                {Send Message}
                FillChar(Message,SizeOf(TMessage),0);
                ThreadSendMessage(RT2800USB.WaiterThread,Message);
                RT2800USB.WaiterThread:=INVALID_HANDLE_VALUE;
               end; 
             end;
           end
          else
           begin
            {Check Next} 
            if Next <> nil then
             begin
              {Update Pending}
              Inc(RT2800USB.PendingCount);
              
              {Update Free}
              RT2800USB.ReceiveFree:=RT2800USB.ReceiveFree xor (1 shl RT2800USBRequest.Index);
              
              {Update Next}
              Next.DriverData:=RT2800USB;
              
              {Update Request}
              RT2800USBRequest.Entry:=Next;
              
              {Reinitialize Request}
              USBRequestInitialize(RT2800USBRequest.Request,RT2800USBReceiveComplete,Next.Buffer,Next.Size,RT2800USBRequest);
              
              {$IFDEF RT2800USB_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: Resubmitting receive request');
              {$ENDIF}
              
              {Resubmit Request}
              Status:=USBRequestSubmit(RT2800USBRequest.Request);
              if Status <> USB_STATUS_SUCCESS then
               begin
                if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Failed to resubmit receive request: ' + USBStatusToString(Status));
              
                {Update Pending}
                Dec(RT2800USB.PendingCount);
                
                {Update Free}
                RT2800USB.ReceiveFree:=RT2800USB.ReceiveFree or (1 shl RT2800USBRequest.Index);
                
                {Update Next}
                Next.DriverData:=nil;
                
                {Update Request}
                RT2800USBRequest.Entry:=nil;
                
                {Free Next}
                BufferFree(Next);
               end;
             end
            else
             begin
              if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: No receive buffer available, cannot resubmit receive request');
              
              {Update Statistics}
              Inc(RT2800USB.RT2X00.WiFi.Network.BufferUnavailable); 
             end;             
           end;  
         finally
          {Release the Lock}
          MutexUnlock(RT2800USB.RT2X00.WiFi.Network.Lock);
         end;
        end
       else
        begin
         if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Failed to acquire lock');
        end;
      end
     else
      begin
       if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Receive request invalid (WiFi)');
      end;    
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Receive request invalid (Entry)');
    end;    
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Receive request invalid (Request)');
  end;    
end;

{==============================================================================}
 
procedure RT2800USBReceiveComplete(Request:PUSBRequest); 
{Called when a USB request from the RT2800USB bulk IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerSchedule(0,TWorkerTask(RT2800USBReceiveWorker),Request,nil)
end;

{==============================================================================}

procedure RT2800USBTransmitWorker(Request:PUSBRequest); 
{Called (by a Worker thread) to process a completed USB request to the RT2800USB bulk OUT endpoint}
{Request: The USB request which has completed}
var
 Message:TMessage;
 Next:PNetworkEntry;
 Entry:PNetworkEntry;
 RT2800USB:PRT2800USBWiFiDevice;
 RT2800USBRequest:PRT2800USBRequest;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Request}
 RT2800USBRequest:=PRT2800USBRequest(Request.DriverData);
 if (RT2800USBRequest <> nil) and (RT2800USBRequest.Request = Request) then
  begin
   {Get Entry}
   Entry:=RT2800USBRequest.Entry;
   if Entry <> nil then
    begin
     {Get WiFi}
     RT2800USB:=PRT2800USBWiFiDevice(Entry.DriverData);
     if RT2800USB <> nil then 
      begin
       {Acquire the Lock}
       if MutexLock(RT2800USB.RT2X00.WiFi.Network.Lock) = ERROR_SUCCESS then
        begin
         try
          {Update Statistics}
          Inc(RT2800USB.RT2X00.WiFi.Network.TransmitCount); 
     
          {Check State}
          if RT2800USB.RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_CLOSING then
           begin
            {$IFDEF RT2800USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: Close pending, setting transmit request status to USB_STATUS_DEVICE_DETACHED');
            {$ENDIF}
          
            {Update Request}
            Request.Status:=USB_STATUS_DEVICE_DETACHED;
           end;
          
          {Check Result} 
          if Request.Status = USB_STATUS_SUCCESS then
           begin
            {$IFDEF RT2800USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: Transmit complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
            {$ENDIF}
          
            //To Do //Continuing
            
            //Signal waiting thread (Semaphore)
            //Get Next from Queue
            //What about status of result ?
            
           end
          else 
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Failed transmit request (Status=' + USBStatusToString(Request.Status) + ')');
            
            {Update Statistics}
            Inc(RT2800USB.RT2X00.WiFi.Network.TransmitErrors); 
           end;
          
          //To Do //Continuing
          
          {Update Pending}
          Dec(RT2800USB.PendingCount); 
          
          {Update Free}
          RT2800USB.TransmitFree:=RT2800USB.TransmitFree or (1 shl RT2800USBRequest.Index);
          
          {Check State}
          if RT2800USB.RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_CLOSING then
           begin
            {Check Pending}
            if RT2800USB.PendingCount = 0 then
             begin
              {Check Waiter}
              if RT2800USB.WaiterThread <> INVALID_HANDLE_VALUE then
               begin
                {$IFDEF RT2800USB_DEBUG}
                if USB_LOG_ENABLED then USBLogDebug(Request.Device,'RT2800USB: Close pending, sending message to waiter thread (Thread=' + IntToHex(RT2800USB.WaiterThread,8) + ')');
                {$ENDIF}
                
                {Send Message}
                FillChar(Message,SizeOf(TMessage),0);
                ThreadSendMessage(RT2800USB.WaiterThread,Message);
                RT2800USB.WaiterThread:=INVALID_HANDLE_VALUE;
               end; 
             end;
           end;
         finally
          {Release the Lock}
          MutexUnlock(RT2800USB.RT2X00.WiFi.Network.Lock);
         end;
        end
       else
        begin
         if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Failed to acquire lock');
        end;
      end
     else
      begin
       if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Transmit request invalid (WiFi)');
      end;    
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Transmit request invalid (Entry)');
    end;    
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'RT2800USB: Transmit request invalid (Request)');
  end;    
end;

{==============================================================================}

procedure RT2800USBTransmitComplete(Request:PUSBRequest); 
{Called when a USB request to the RT2800USB bulk OUT endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerSchedule(0,TWorkerTask(RT2800USBTransmitWorker),Request,nil)
end;

{==============================================================================}
{==============================================================================}
{RT2800USB Helper Functions}
function RT2800USBCheckDevice(Device:PUSBDevice):LongWord;
{Check the Vendor and Device ID against the supported devices}
{Device: USB device to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Device IDs}
 for Count:=0 to RT2800USB_DEVICE_ID_COUNT - 1 do
  begin
   if (RT2800USB_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (RT2800USB_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function RT2800USBDriverInit(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Driver init');
 {$ENDIF}

 {Set TXINFO/RXINFO size}
 RT2X00.TXINFOSize:=RT2800USB_TXINFO_DESC_SIZE;
 RT2X00.RXINFOSize:=RT2800USB_RXINFO_DESC_SIZE;

 {Call RT2800 initialization}
 Result:=RT2800DriverInit(RT2X00);
end;

{==============================================================================}

function RT2800USBEepromLoad(RT2X00:PRT2X00WiFiDevice;Data:PWord;Size:LongWord):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: EEPROM load');
 {$ENDIF}

 {Check eFuse}
 Status:=RT2800USBDetectEfuse(RT2X00);
 if (Status = ERROR_SUCCESS) or (Status = ERROR_NOT_SUPPORTED) then
  begin
   if Status = ERROR_SUCCESS then
    begin
     Result:=RT2800LoadEfuse(RT2X00,Data,Size);
    end
   else
    begin   
     Result:=RT2X00USBEepromLoad(RT2X00,Data,Size);
    end;
  end
 else
  begin
   Result:=Status;
  end;
end;

{==============================================================================}

function RT2800USBSetState(RT2X00:PRT2X00WiFiDevice;State:LongWord):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Set State=' + IntToStr(State));
 {$ENDIF}

 {Check State}
 case State of
  RT2X00_STATE_RADIO_ON:begin
    {Before the radio can be enabled, the device first has to be woken up. After that it needs a bit of time to be fully awake and then the radio can be enabled}
    RT2800MCURequest(RT2X00,RT2800_MCU_WAKEUP,$ff,0,2);
    Sleep(1);
    
    Status:=RT2800USBEnableRadio(RT2X00);
   end;
  RT2X00_STATE_RADIO_OFF:begin
    {After the radio has been disabled, the device should be put to sleep for powersaving}
    Status:=RT2800USBDisableRadio(RT2X00);
    
    RT2800MCURequest(RT2X00,RT2800_MCU_SLEEP,$ff,$ff,2);
   end;
  RT2X00_STATE_RADIO_IRQ_ON,RT2X00_STATE_RADIO_IRQ_OFF:begin
    {Not supported for USB, return success}
    Status:=ERROR_SUCCESS;
   end;
  RT2X00_STATE_DEEP_SLEEP,RT2X00_STATE_SLEEP,RT2X00_STATE_STANDBY:begin
    if RT2800MCURequest(RT2X00,RT2800_MCU_SLEEP,$ff,$ff,2) then
     begin
      Status:=ERROR_SUCCESS;
     end
    else
     begin    
      Status:=ERROR_OPERATION_FAILED;
     end;
   end;
  RT2X00_STATE_AWAKE:begin
    if RT2800MCURequest(RT2X00,RT2800_MCU_WAKEUP,$ff,0,2) then
     begin
      Status:=ERROR_SUCCESS;
     end
    else
     begin    
      Status:=ERROR_OPERATION_FAILED;
     end;
   end;
  else
   begin
    Status:=ERROR_NOT_SUPPORTED;
   end;   
 end;
 
 if Status <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Device failed to enter state (Status=' + ErrorToString(Status) + ')');
  end;
  
 Result:=Status;
end;

{==============================================================================}

function RT2800USBEnableRX(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2800usb_start_queue}
var
 Reg:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Enable RX');
 {$ENDIF}

 RT2X00USBRegisterRead(RT2X00,RT2800_MAC_SYS_CTRL,@Reg);
 RT2X00SetRegister32(Reg,RT2800_MAC_SYS_CTRL_ENABLE_RX,3,1);
 RT2X00USBRegisterWrite(RT2X00,RT2800_MAC_SYS_CTRL,Reg);
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function RT2800USBDisableRX(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2800usb_stop_queue}
var
 Reg:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Disable RX');
 {$ENDIF}

 RT2X00USBRegisterRead(RT2X00,RT2800_MAC_SYS_CTRL,@Reg);
 RT2X00SetRegister32(Reg,RT2800_MAC_SYS_CTRL_ENABLE_RX,3,0);
 RT2X00USBRegisterWrite(RT2X00,RT2800_MAC_SYS_CTRL,Reg);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RT2800USBEnableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2800usb_start_queue}
var
 Reg:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Enable Beacon');
 {$ENDIF}

 RT2X00USBRegisterRead(RT2X00,RT2800_BCN_TIME_CFG,@Reg);
 RT2X00SetRegister32(Reg,RT2800_BCN_TIME_CFG_TSF_TICKING,16,1);
 RT2X00SetRegister32(Reg,RT2800_BCN_TIME_CFG_TBTT_ENABLE,19,1);
 RT2X00SetRegister32(Reg,RT2800_BCN_TIME_CFG_BEACON_GEN,20,1);
 RT2X00USBRegisterWrite(RT2X00,RT2800_BCN_TIME_CFG,Reg);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RT2800USBDisableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2800usb_stop_queue}
var
 Reg:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Disable Beacon');
 {$ENDIF}

 RT2X00USBRegisterRead(RT2X00,RT2800_BCN_TIME_CFG,@Reg);
 RT2X00SetRegister32(Reg,RT2800_BCN_TIME_CFG_TSF_TICKING,16,0);
 RT2X00SetRegister32(Reg,RT2800_BCN_TIME_CFG_TBTT_ENABLE,19,0);
 RT2X00SetRegister32(Reg,RT2800_BCN_TIME_CFG_BEACON_GEN,20,0);
 RT2X00USBRegisterWrite(RT2X00,RT2800_BCN_TIME_CFG,Reg);
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function RT2800USBGetFirmware(RT2X00:PRT2X00WiFiDevice;var Name:String;var Address:Pointer;var Size:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Get firmware');
 {$ENDIF}

 {Check Internal}
 if RT2800USB_FIRMWARE_INTERNAL then
  begin
   {Update Name}
   Name:='';
   
   {Update Address and Size}
   Address:=@RT2800USBFirmware;
   Size:=SizeOf(RT2800USBFirmware);

   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Internal firmware (Address=' + IntToHex(PtrUInt(Address),SizeOf(PtrUInt)) + ' Size=' + IntToStr(Size) + ')');
   {$ENDIF}
  end
 else
  begin
   {Update Name}
   Name:=RT2800USB_FIRMWARE_FILENAME;
   
   {Update Address and Size}
   Address:=nil;
   Size:=0;

   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: External firmware (Name=' + Name + ')');
   {$ENDIF}
  end; 
  
 Result:=True; 
end;

{==============================================================================}

function RT2800USBWriteFirmware(RT2X00:PRT2X00WiFiDevice;Data:PByte;Size:LongWord):Boolean;
{rt2800usb_write_firmware}
var
 Count:PtrUInt;
 Offset:PtrUInt;
 Status:LongWord;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Write firmware (Data=' + IntToHex(LongWord(Data),8) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Firmware}
 if Data = nil then Exit;
 if Size <> SIZE_8K then Exit;
 
 {Check Chipset}
 if (RT2X00GetRTChip(RT2X00) = RT2X00_RT2860) or (RT2X00GetRTChip(RT2X00) = RT2X00_RT2872) or (RT2X00GetRTChip(RT2X00) = RT2X00_RT3070) then
  begin
   {Setup Offset and Count}
   Offset:=0;
   Count:=SIZE_4K;
  end
 else
  begin
   {Setup Offset and Count}
   Offset:=SIZE_4K;
   Count:=SIZE_4K;
  end;  
  
 {Check Autorun} 
 Status:=RT2800USBDetectAutorun(RT2X00);
 if (Status <> ERROR_SUCCESS) and (Status <> ERROR_NOT_SUPPORTED) then
  begin
   if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Failed detect autorun mode');
   Exit;
  end
 else
  begin 
   if Status = ERROR_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogInfo(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Device in autorun mode, firmware not required');
     
     {Firmware not required}
     RT2X00ClearRequirement(RT2X00,RT2X00_REQUIRE_FIRMWARE);
    end
   else
    begin 
     {$IFDEF RT2800USB_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB:  (Offset=' + IntToStr(Offset) + ' Count=' + IntToStr(Count) + ')');
     {$ENDIF}
    
     {Write the firmware}
     Status:=RT2X00USBRegisterMultiWrite(RT2X00,RT2800USB_FIRMWARE_IMAGEBASE,Data + Offset,Count);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Failed register multiwrite for RT2800USB_FIRMWARE_IMAGEBASE (Status=' + USBStatusToString(Status) + ')');
       Exit;
      end;
     
     {Setup mailbox}
     RT2X00USBRegisterWrite(RT2X00,RT2800_H2M_MAILBOX_CID,not(0));
     RT2X00USBRegisterWrite(RT2X00,RT2800_H2M_MAILBOX_STATUS,not(0));
     
     {Send firmware request}
     Status:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_DEVICE_MODE,RT2X00USB_VENDOR_REQUEST_OUT,RT2X00USB_MODE_FIRMWARE,0,nil,0,RT2X00USB_REGISTER_TIMEOUT_FIRMWARE);
     if Status <> USB_STATUS_SUCCESS then //To Do //Change to ERROR_ returns  //Dont use USB_STATUS_ codes
      begin
       if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Failed vendor request for RT2X00USB_MODE_FIRMWARE (Status=' + USBStatusToString(Status) + ')');
       Exit;
      end;
      
     {Delay} 
     MillisecondDelay(10);
     
     {Update mailbox}
     RT2X00USBRegisterWrite(RT2X00,RT2800_H2M_MAILBOX_CSR,0);
    end; 
   
   Result:=True;
  end;  
end;

{==============================================================================}

function RT2800USBInitializeRegisters(RT2X00:PRT2X00WiFiDevice):LongWord;
var
 Reg:LongWord;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Initialize Registers');
 {$ENDIF}

 {Wait for BBP and RF ready}
 if not RT2800WaitCSRReady(RT2X00) then
  begin
   if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Wait CSR ready failed');
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 RT2X00USBRegisterRead(RT2X00,RT2800_PBF_SYS_CTRL,@Reg);
 RT2X00USBRegisterWrite(RT2X00,RT2800_PBF_SYS_CTRL,Reg and not($00002000));
    
 Reg:=0;
 RT2X00SetRegister32(Reg,RT2800_MAC_SYS_CTRL_RESET_CSR,0,1);
 RT2X00SetRegister32(Reg,RT2800_MAC_SYS_CTRL_RESET_BBP,1,1);
 RT2X00USBRegisterWrite(RT2X00,RT2800_MAC_SYS_CTRL,Reg);
    
 RT2X00USBRegisterWrite(RT2X00,RT2800_USB_DMA_CFG,$00000000);
    
 Status:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_DEVICE_MODE,RT2X00USB_VENDOR_REQUEST_OUT,RT2X00USB_MODE_RESET,0,nil,0,RT2X00USB_REGISTER_TIMEOUT);
 if Status <> USB_STATUS_SUCCESS then //To Do //Change to ERROR_ returns  //Dont use USB_STATUS_ codes
  begin
   if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Failed vendor request for RT2X00USB_MODE_RESET (Status=' + USBStatusToString(Status) + ')');
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
    
 RT2X00USBRegisterWrite(RT2X00,RT2800_MAC_SYS_CTRL,$00000000);
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function RT2800USBHardwareEncryptionDisabled(RT2X00:PRT2X00WiFiDevice):Boolean;
begin
 {}
 Result:=RT2800USB_HARDWARE_ENCRYPTION_DISABLED;
end;

{==============================================================================}

function RT2800USBDetectEfuse(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Detect eFuse EEPROM');
 {$ENDIF}
 
 {Check Autorun}
 Result:=RT2800USBDetectAutorun(RT2X00);
 if Result = ERROR_NOT_SUPPORTED then
  begin
   {Check eFuse}
   Result:=RT2800DetectEfuse(RT2X00);
  end;
end;

{==============================================================================}

function RT2800USBDetectAutorun(RT2X00:PRT2X00WiFiDevice):LongWord;
var
 Reg:LongWord;
 Value:LongWord;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Detect RT2X00USB_MODE_AUTORUN');
 {$ENDIF}
 
 {Setup Value}
 Reg:=0;
 
 {Vendor Request for MODE_AUTORUN}
 Status:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_DEVICE_MODE,RT2X00USB_VENDOR_REQUEST_IN,RT2X00USB_MODE_AUTORUN,0,@Reg,SizeOf(LongWord),RT2X00USB_REGISTER_TIMEOUT_FIRMWARE);
 if Status <> USB_STATUS_SUCCESS then //To Do //Change to ERROR_ returns  //Dont use USB_STATUS_ codes
  begin
   if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Failed vendor request for RT2X00USB_MODE_AUTORUN (Status=' + USBStatusToString(Status) + ')');
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Get Value}
 Value:=LongWordLEtoN(Reg);
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB:  (Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}
 
 {Check Value}
 if (Value and $00000003) = 2 then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;
  
 Result:=ERROR_NOT_SUPPORTED;
end;

{==============================================================================}

function RT2800USBEnableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;
var
 Reg:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Enable Radio');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_ALREADY_OPEN;
 if RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_OPEN then Exit;

 {Wait DMA Ready}
 if not RT2800WaitWPDMAReady(RT2X00) then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 RT2X00USBRegisterRead(RT2X00,RT2800_USB_DMA_CFG,@Reg);
 RT2X00SetRegister32(Reg,RT2800_USB_DMA_CFG_PHY_CLEAR,20,0);
 RT2X00SetRegister32(Reg,RT2800_USB_DMA_CFG_RX_BULK_AGG_EN,21,0);
 RT2X00SetRegister32(Reg,RT2800_USB_DMA_CFG_RX_BULK_AGG_TIMEOUT,0,128);
 
 RT2X00SetRegister32(Reg,RT2800_USB_DMA_CFG_RX_BULK_AGG_LIMIT,8,((128 * RT2X00_DATA_FRAME_SIZE) div 1024) - 3); //To Do //128 = rx->limit //See: rt2800usb_enable_radio/rt2800usb_queue_init {Total room for RX frames in kilobytes, PBF might still exceed this limit so reduce the number to prevent errors}
 RT2X00SetRegister32(Reg,RT2800_USB_DMA_CFG_RX_BULK_EN,22,1);
 RT2X00SetRegister32(Reg,RT2800_USB_DMA_CFG_TX_BULK_EN,23,1);
 RT2800RegisterWrite(RT2X00,RT2800_USB_DMA_CFG,Reg);    
 
 {Enable Radio}
 Result:=RT2800EnableRadio(RT2X00);
end;

{==============================================================================}

function RT2800USBDisableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2800USB: Disable Radio');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_CLOSED then Exit;

 {Disable Radio}
 Result:=RT2800DisableRadio(RT2X00);
end;

{==============================================================================}

function RT2800USBReceiveProcessRXD(RT2X00:PRT2X00WiFiDevice;Descriptor:PRT2X00RXDescriptor;var Data:Pointer;var Size:LongWord;PacketLength:LongWord):Boolean;
{rt2800usb_fill_rxdone}
var
 RXD:Pointer;
 Value:LongWord;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;
 
 {Check Data and Size}
 if Data = nil then Exit;
 if Size = 0 then Exit;

 {Get RXD}
 RXD:=Data + PacketLength;
 Value:=RT2X00ReadDescriptor(RXD,0);
 {$IFDEF RT2800USB_DEBUG}
 //if NETWORK_LOG_ENABLED then NetworkLogDebug(@RT2X00.WiFi.Network,'RT2800USB: (RXD Value=' + IntToHex(Value,8) + ')'); //To Do
 {$ENDIF}
 
 {CRC Error}
 if RT2X00GetRegister32(Value,RT2800USB_RXD_W0_CRC_ERROR,8) <> 0 then
  begin
   Descriptor.Flags:=Descriptor.Flags or WIFI_RX_FLAG_FAILED_FCS_CRC;
  end;
  
 {Cipher Status}
 Descriptor.CipherStatus:=RT2X00GetRegister32(Value,RT2800USB_RXD_W0_CIPHER_ERROR,9);
 
 {Decrypted}
 if RT2X00GetRegister32(Value,RT2800USB_RXD_W0_DECRYPTED,16) <> 0 then
  begin
   {Hardware has stripped IV/EIV data from 802.11 frame during decryption. Unfortunately the
    descriptor doesn't contain any fields with the EIV/IV data either, so they can't be restored}
   Descriptor.Flags:=Descriptor.Flags or WIFI_RX_FLAG_IV_STRIPPED;
   
   {The hardware has already checked the Michael Mic and has stripped it from the frame}
   Descriptor.Flags:=Descriptor.Flags or WIFI_RX_FLAG_MMIC_STRIPPED;
   
   if Descriptor.CipherStatus = RT2X00_RX_CRYPTO_SUCCESS then
    begin
     Descriptor.Flags:=Descriptor.Flags or WIFI_RX_FLAG_DECRYPTED;
    end
   else if Descriptor.CipherStatus = RT2X00_RX_CRYPTO_FAIL_MIC then
    begin
     Descriptor.Flags:=Descriptor.Flags or WIFI_RX_FLAG_MMIC_ERROR;
    end;
  end;
  
 {My BSS}
 if RT2X00GetRegister32(Value,RT2800USB_RXD_W0_MY_BSS,7) <> 0 then  
  begin
   Descriptor.RXFlags:=Descriptor.RXFlags or RT2X00_RXDONE_MY_BSS;
  end;
 
 {L2 Pad}
 if RT2X00GetRegister32(Value,RT2800USB_RXD_W0_L2PAD,14) <> 0 then  
  begin
   Descriptor.RXFlags:=Descriptor.RXFlags or RT2X00_RXDONE_L2PAD;
  end;
  
 {Update Size (Remove RXD from end of buffer)}
 Size:=PacketLength;
  
 Result:=True;
end;

{==============================================================================}
{==============================================================================}

initialization
 RT2800USBInit;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 