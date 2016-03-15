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
 RT2800USB_DRIVER_NAME = 'Ralink RT2800 USB Wireless Driver'; {Name of RT2800USB driver}
 
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
 
 {TX Info structure}
 {Word0}
 RT2800USB_TXINFO_W0_USB_DMA_TX_PKT_LEN = $0000ffff;
 RT2800USB_TXINFO_W0_WIV		        = $01000000; {WIV: Wireless Info Valid. 1: Driver filled WI,  0: DMA needs to copy WI}
 RT2800USB_TXINFO_W0_QSEL		        = $06000000; {QSEL: Select on-chip FIFO ID for 2nd-stage output scheduler. 0:MGMT, 1:HCCA 2:EDCA}
 RT2800USB_TXINFO_W0_SW_USE_LAST_ROUND  = $08000000; 
 RT2800USB_TXINFO_W0_USB_DMA_NEXT_VALID = $40000000; {USB_DMA_NEXT_VALID: Used ONLY in USB bulk Aggregation, NextValid}
 RT2800USB_TXINFO_W0_USB_DMA_TX_BURST   = $80000000; {DMA_TX_BURST: used ONLY in USB bulk Aggregation. Force USB DMA transmit frame from current selected endpoint}
 
 {RX Info structure}
 {Word 0}
 RT2800USB_RXINFO_W0_USB_DMA_RX_PKT_LEN	= $0000ffff;
 
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
 
{==============================================================================}
type
 {RT2800USB specific types}
 
 {RT2800USB Device}
 PRT2800USBNetworkDevice = ^TRT2800USBNetworkDevice;
 TRT2800USBNetworkDevice = record
  {RT2X00 Properties}
  RT2X00:TRT2X00NetworkDevice;
  {USB Properties}
  //To Do
  PendingCount:LongWord;                                                  {Number of USB requests pending for this network}
  WaiterThread:TThreadId;                                                 {Thread waiting for pending requests to complete (for network close)}
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
function RT2800USBDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function RT2800USBDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function RT2800USBDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

//To Do //BufferAllocate/BufferRelease/BufferReceive/BufferTransmit

{==============================================================================}
{RT2800USB USB Functions}
function RT2800USBDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function RT2800USBDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

{==============================================================================}
{RT2800USB Helper Functions}
function RT2800USBCheckDevice(Device:PUSBDevice):LongWord;

function RT2800USBDriverInit(RT2X00:PRT2X00NetworkDevice):LongWord;
function RT2800USBEepromLoad(RT2X00:PRT2X00NetworkDevice;Data:PWord;Size:LongWord):LongWord;

//To Do //EnableRadio/InitRegisters etc

function RT2800USBGetFirmware(RT2X00:PRT2X00NetworkDevice;var Name:String;var Address:Pointer;var Size:LongWord):Boolean;
function RT2800USBWriteFirmware(RT2X00:PRT2X00NetworkDevice;Data:PByte;Size:LongWord):Boolean;

function RT2800USBDetectEfuse(RT2X00:PRT2X00NetworkDevice):LongWord;
function RT2800USBDetectAutorun(RT2X00:PRT2X00NetworkDevice):LongWord;
 
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
 RT2800USBFirmware:array[0..8191] of Byte = (
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

 //To Do
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
 
 {Check State}
 Result:=ERROR_NOT_OPEN;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Set State to Closing}
 Result:=ERROR_OPERATION_FAILED;
 if NetworkDeviceSetState(Network,NETWORK_STATE_CLOSING) <> ERROR_SUCCESS then Exit;

 //To Do
end;
 
{==============================================================================}

function RT2800USBDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of NetworkDeviceRead for the RT2800USB device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Length}
 Length:=0;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size = 0 then Exit;
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 //To Do
end;
 
{==============================================================================}

function RT2800USBDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of NetworkDeviceWrite for the RT2800USB device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Length}
 Length:=0;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 //To Do
end;
 
{==============================================================================}

function RT2800USBDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
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

 //To Do
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
 Status:LongWord;
 Network:PRT2800USBNetworkDevice;
 NetworkInterface:PUSBInterface;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
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
 
 {Check Configuration}
 if Device.ConfigurationValue = 0 then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Assigning configuration ' + IntToStr(Device.Configuration.Descriptor.bConfigurationValue) + ' (' + IntToStr(Device.Configuration.Descriptor.bNumInterfaces) + ' interfaces available)');
   {$ENDIF}
   
   {Set Configuration}
   Status:=USBDeviceSetConfiguration(Device,Device.Configuration.Descriptor.bConfigurationValue);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Failed to set device configuration: ' + USBStatusToString(Status));
     
     {Return Result}
     Result:=Status;
     Exit;
    end;
  end;
 
 {USB device reset not required because the USB core already did a reset on the port during attach}
 
 {Create Network}
 Network:=PRT2800USBNetworkDevice(NetworkDeviceCreateEx(SizeOf(TRT2800USBNetworkDevice)));
 if Network = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to create new network device');
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update Network} 
 {Device}
 Network.RT2X00.Network.Device.DeviceBus:=DEVICE_BUS_USB;
 Network.RT2X00.Network.Device.DeviceType:=NETWORK_TYPE_80211;
 Network.RT2X00.Network.Device.DeviceFlags:=NETWORK_FLAG_NONE;     
 Network.RT2X00.Network.Device.DeviceData:=Device;
 {Network}
 Network.RT2X00.Network.NetworkState:=NETWORK_STATE_CLOSED;
 Network.RT2X00.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
 Network.RT2X00.Network.DeviceOpen:=RT2800USBDeviceOpen;
 Network.RT2X00.Network.DeviceClose:=RT2800USBDeviceClose;
 Network.RT2X00.Network.DeviceRead:=RT2800USBDeviceRead;
 Network.RT2X00.Network.DeviceWrite:=RT2800USBDeviceWrite;
 Network.RT2X00.Network.DeviceControl:=RT2800USBDeviceControl;
 {WiFi}
 //To Do
 {Driver}
 {RT2X00}
 Network.RT2X00.DriverInit:=RT2800USBDriverInit;
 Network.RT2X00.EepromLoad:=RT2800USBEepromLoad;
 Network.RT2X00.RegisterRead:=RT2X00USBRegisterRead;
 Network.RT2X00.RegisterWrite:=RT2X00USBRegisterWrite;
 Network.RT2X00.RegisterMultiRead:=RT2X00USBRegisterMultiRead;
 Network.RT2X00.RegisterMultiWrite:=RT2X00USBRegisterMultiWrite;
 Network.RT2X00.RegisterBusyRead:=RT2X00USBRegisterBusyRead;
 Network.RT2X00.GetFirmware:=RT2800USBGetFirmware;
 Network.RT2X00.CheckFirmware:=RT2800CheckFirmware;
 Network.RT2X00.LoadFirmware:=RT2800LoadFirmware;
 Network.RT2X00.WriteFirmware:=RT2800USBWriteFirmware;
 //To Do
 {RT2X00}
 Network.RT2X00.RFSize:=RT2800_RF_SIZE;
 Network.RT2X00.EepromSize:=RT2800_EEPROM_SIZE;
 Network.RT2X00.CSRLock:=INVALID_HANDLE_VALUE;
 //To Do
 {USB}
 //Network.ReceiveEndpoint:=ReceiveEndpoint; //To Do
 //Network.TransmitEndpoint:=TransmitEndpoint; 
 Network.WaiterThread:=INVALID_HANDLE_VALUE;
 //To Do
 
 {Create RF}
 Network.RT2X00.RFData:=AllocMem(Network.RT2X00.RFSize);
 if Network.RT2X00.RFData = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to allocate RF data for network');
   
   {Destroy Network}
   NetworkDeviceDestroy(@Network.RT2X00.Network);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Create Eeprom}
 Network.RT2X00.EepromData:=AllocMem(Network.RT2X00.EepromSize);
 if Network.RT2X00.EepromData = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to allocate EEPROM data for network');
   
   {Free RF}
   FreeMem(Network.RT2X00.RFData);
   
   {Destroy Network}
   NetworkDeviceDestroy(@Network.RT2X00.Network);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Create Lock} 
 Network.RT2X00.CSRLock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Network.RT2X00.CSRLock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to create CSR lock for network');
   
   {Free RF}
   FreeMem(Network.RT2X00.RFData);

   {Free Eeprom}
   FreeMem(Network.RT2X00.EepromData);
   
   {Destroy Network}
   NetworkDeviceDestroy(@Network.RT2X00.Network);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Initialize Driver}
 if RT2X00DriverInit(@Network.RT2X00) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to initialize driver for network');
   
   {Free RF}
   FreeMem(Network.RT2X00.RFData);

   {Free Eeprom}
   FreeMem(Network.RT2X00.EepromData);
   
   {Destroy Lock}
   MutexDestroy(Network.RT2X00.CSRLock);
   
   {Destroy Network}
   NetworkDeviceDestroy(@Network.RT2X00.Network);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Register Network} 
 if NetworkDeviceRegister(@Network.RT2X00.Network) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'RT2800USB: Failed to register new network device');
   
   {Free RF}
   FreeMem(Network.RT2X00.RFData);

   {Free Eeprom}
   FreeMem(Network.RT2X00.EepromData);
   
   {Destroy Lock}
   MutexDestroy(Network.RT2X00.CSRLock);

   {Destroy Network}
   NetworkDeviceDestroy(@Network.RT2X00.Network);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update Device}
 Device.DriverData:=Network;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
  
{==============================================================================}

function RT2800USBDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the RT2800USB driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var 
 Network:PRT2800USBNetworkDevice;
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
 if USB_LOG_ENABLED then USBLogDebug(Device,'RT2800USB: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}

 {Get Network}
 Network:=PRT2800USBNetworkDevice(Device.DriverData);
 if Network = nil then Exit;
 
 {Close Network}
 RT2800USBDeviceClose(@Network.RT2X00.Network);
 
 {Update Device}
 Device.DriverData:=nil;
 
 {Deregister Network}
 if NetworkDeviceDeregister(@Network.RT2X00.Network) <> ERROR_SUCCESS then Exit;
 
 {Free RF}
 FreeMem(Network.RT2X00.RFData);
 
 {Free Eeprom}
 FreeMem(Network.RT2X00.EepromData);
 
 {Destroy Lock}
 MutexDestroy(Network.RT2X00.CSRLock);
 
 {Destroy Network}
 NetworkDeviceDestroy(@Network.RT2X00.Network);
 
 Result:=USB_STATUS_SUCCESS;
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

function RT2800USBDriverInit(RT2X00:PRT2X00NetworkDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Driver init');
 {$ENDIF}

 {Nothing RT2800USB specific}

 {Call RT2800 initialization}
 Result:=RT2800DriverInit(RT2X00);
end;

{==============================================================================}

function RT2800USBEepromLoad(RT2X00:PRT2X00NetworkDevice;Data:PWord;Size:LongWord):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: EEPROM load');
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

function RT2800USBGetFirmware(RT2X00:PRT2X00NetworkDevice;var Name:String;var Address:Pointer;var Size:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Get firmware');
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
   if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Internal firmware (Address=' + IntToHex(PtrUInt(Address),SizeOf(PtrUInt)) + ' Size=' + IntToStr(Size) + ')');
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
   if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: External firmware (Name=' + Name + ')');
   {$ENDIF}
  end; 
  
 Result:=True; 
end;

{==============================================================================}

function RT2800USBWriteFirmware(RT2X00:PRT2X00NetworkDevice;Data:PByte;Size:LongWord):Boolean;
var
 Count:PtrUInt;
 Offset:PtrUInt;
 Status:LongWord;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {Check Firmware}
 if Data = nil then Exit;
 if Size <> SIZE_8K then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Write firmware');
 {$ENDIF}
 
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
   if Status = ERROR_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogInfo(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Device in autorun mode, firmware not required');
     
     {Firmware not required}
     RT2X00ClearRequirement(RT2X00,RT2X00_REQUIRE_FIRMWARE);
    end
   else
    begin 
     {Write the firmware}
     Status:=RT2X00USBRegisterMultiWrite(RT2X00,RT2800USB_FIRMWARE_IMAGEBASE,Data + Offset,Count);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Failed register multiwrite for RT2800USB_FIRMWARE_IMAGEBASE: ' + USBStatusToString(Status));
       Exit;
      end;
     
     {Setup mailbox}
     RT2X00USBRegisterWrite(RT2X00,RT2800_H2M_MAILBOX_CID,not(0));
     RT2X00USBRegisterWrite(RT2X00,RT2800_H2M_MAILBOX_STATUS,not(0));
     
     {Send firmware request}
     Status:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_DEVICE_MODE,RT2X00USB_VENDOR_REQUEST_OUT,0,RT2X00USB_MODE_FIRMWARE,nil,0);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Failed vendor request for RT2X00USB_MODE_FIRMWARE: ' + USBStatusToString(Status));
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

function RT2800USBDetectEfuse(RT2X00:PRT2X00NetworkDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Detect eFuse EEPROM');
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

function RT2800USBDetectAutorun(RT2X00:PRT2X00NetworkDevice):LongWord;
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
 if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Detect RT2X00USB_MODE_AUTORUN');
 {$ENDIF}
 
 Result:=ERROR_NOT_SUPPORTED; //To Do //Testing //Need USBControlRequest timeout option 
 Exit;                        //To Do //Testing //Need USBControlRequest timeout option 
 
 {Vendor Request for MODE_AUTORUN}
 Status:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_DEVICE_MODE,RT2X00USB_VENDOR_REQUEST_IN,0,RT2X00USB_MODE_AUTORUN,@Reg,SizeOf(LongWord));
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.Network.Device.DeviceData),'RT2800USB: Failed vendor request for RT2X00USB_MODE_AUTORUN: ' + USBStatusToString(Status));
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Get Value}
 Value:=LongWordLEtoN(Reg);
 
 {Check Value}
 if (Value and $00000003) = 2 then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;
  
 Result:=ERROR_NOT_SUPPORTED;
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
 