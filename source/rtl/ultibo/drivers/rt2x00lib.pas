{
Ralink RT2x00 Wireless Driver library.

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
 
Ralink RT2x00
=============

 This unit provides functionality and definitions common to all implementations of the RT2x00
 chipset series PCI, USB or other.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RT2X00LIB; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Network,WiFi,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RT2X00LIB specific constants}

 {Device flags / Driver configuration flags}
 RT2X00_DEVICE_STATE_PRESENT       = (1 shl 0);  //To Do //Remove ?
 RT2X00_DEVICE_STATE_REGISTERED_HW = (1 shl 1);  //To Do //Remove ?
 RT2X00_DEVICE_STATE_INITIALIZED   = (1 shl 2);  //To Do //Remove ? 
 RT2X00_DEVICE_STATE_STARTED       = (1 shl 3);  //To Do //Remove ? 
 RT2X00_DEVICE_STATE_ENABLED_RADIO = (1 shl 4);  //To Do //Remove ? 
 RT2X00_DEVICE_STATE_SCANNING      = (1 shl 5);  //To Do //Remove ? 
    
 RT2X00_CONFIG_CHANNEL_HT40        = (1 shl 6); 
 RT2X00_CONFIG_POWERSAVING         = (1 shl 7); 
 RT2X00_CONFIG_HT_DISABLED         = (1 shl 8); 
 RT2X00_CONFIG_QOS_DISABLED        = (1 shl 9); 
    
 RT2X00_TX_STATUS_READING          = (1 shl 10); 
 
 {Standard timings and sizes} {These values should follow the ieee80211 specifications}
 RT2X00_ACK_SIZE         = 14;
 RT2X00_IEEE80211_HEADER = 24;
 RT2X00_PLCP             = 48;
 RT2X00_BEACON           = 100;
 RT2X00_PREAMBLE         = 144;
 RT2X00_SHORT_PREAMBLE   = 72;
 RT2X00_SLOT_TIME        = 20;
 RT2X00_SHORT_SLOT_TIME  = 9;
 RT2X00_SIFS             = 10;
 RT2X00_PIFS             = RT2X00_SIFS + RT2X00_SLOT_TIME;
 RT2X00_SHORT_PIFS       = RT2X00_SIFS + RT2X00_SHORT_SLOT_TIME;
 RT2X00_DIFS             = RT2X00_PIFS + RT2X00_SLOT_TIME;
 RT2X00_SHORT_DIFS       = RT2X00_SHORT_PIFS + RT2X00_SHORT_SLOT_TIME;
 //RT2X00_EIFS             = RT2X00_SIFS + RT2X00_DIFS +  RT2X00_GET_DURATION(RT2X00_IEEE80211_HEADER + RT2X00_ACK_SIZE,10); //To Do //rt2x00.h
 //RT2X00_SHORT_EIFS       = RT2X00_SIFS + RT2X00_SHORT_DIFS +  RT2X00_GET_DURATION(RT2X00_IEEE80211_HEADER + RT2X00_ACK_SIZE,10); //To Do //rt2x00.h
 
 {Extra TX headroom for alignment purposes}
 RT2X00_ALIGN_EXTRA  = 4; {RT2X00_ALIGN_SIZE}{Only whole frame needs alignment}
 RT2X00_L2PAD_EXTRA  = 8; {RT2X00_L2PAD_SIZE}{Both header & payload need alignment}
 
 {RT chip constants (The chipset on the device is composed of an RT and RF chip)}
 RT2X00_RT2460     = $2460;
 RT2X00_RT2560     = $2560;
 RT2X00_RT2570     = $2570;
 RT2X00_RT2661     = $2661;
 RT2X00_RT2573     = $2573;
 RT2X00_RT2860     = $2860; {2.4GHz}
 RT2X00_RT2872     = $2872; {WSOC}
 RT2X00_RT2883     = $2883; {WSOC}
 RT2X00_RT3070     = $3070;
 RT2X00_RT3071     = $3071;
 RT2X00_RT3090     = $3090; {2.4GHz PCIe}
 RT2X00_RT3290     = $3290;
 RT2X00_RT3352     = $3352; {WSOC}
 RT2X00_RT3390     = $3390;
 RT2X00_RT3572     = $3572;
 RT2X00_RT3593     = $3593;
 RT2X00_RT3883     = $3883; {WSOC}
 RT2X00_RT5390     = $5390; {2.4GHz}
 RT2X00_RT5392     = $5392; {2.4GHz}
 RT2X00_RT5592     = $5592;
 
 {Requirements constants}
 RT2X00_REQUIRE_FIRMWARE      = 1 shl 0;
 RT2X00_REQUIRE_BEACON_GUARD  = 1 shl 1;
 RT2X00_REQUIRE_ATIM_QUEUE    = 1 shl 2;
 RT2X00_REQUIRE_DMA           = 1 shl 3;
 RT2X00_REQUIRE_COPY_IV       = 1 shl 4;
 RT2X00_REQUIRE_L2PAD         = 1 shl 5;
 RT2X00_REQUIRE_TXSTATUS_FIFO = 1 shl 6;
 RT2X00_REQUIRE_SW_SEQNO      = 1 shl 7;
 RT2X00_REQUIRE_HT_TX_DESC    = 1 shl 8;
 RT2X00_REQUIRE_PS_AUTOWAKE   = 1 shl 9;
 
 {Capabilities constants}
 RT2X00_CAPABILITY_HW_BUTTON             = 1 shl 0;
 RT2X00_CAPABILITY_HW_CRYPTO             = 1 shl 1;
 RT2X00_CAPABILITY_POWER_LIMIT           = 1 shl 2;
 RT2X00_CAPABILITY_CONTROL_FILTERS       = 1 shl 3;
 RT2X00_CAPABILITY_CONTROL_FILTER_PSPOLL = 1 shl 4;
 RT2X00_CAPABILITY_PRE_TBTT_INTERRUPT    = 1 shl 5;
 RT2X00_CAPABILITY_LINK_TUNING           = 1 shl 6;
 RT2X00_CAPABILITY_FRAME_TYPE            = 1 shl 7;
 RT2X00_CAPABILITY_RF_SEQUENCE           = 1 shl 8;
 RT2X00_CAPABILITY_EXTERNAL_LNA_A        = 1 shl 9;
 RT2X00_CAPABILITY_EXTERNAL_LNA_BG       = 1 shl 10;
 RT2X00_CAPABILITY_DOUBLE_ANTENNA        = 1 shl 11;
 RT2X00_CAPABILITY_BT_COEXIST            = 1 shl 12;
 RT2X00_CAPABILITY_VCO_RECALIBRATION     = 1 shl 13;
 
 {Busy delay constants}
 RT2X00_REGISTER_BUSY_COUNT = 100;
 RT2X00_REGISTER_USB_BUSY_COUNT = 20;
 RT2X00_REGISTER_BUSY_DELAY = 100;
 
 {RX crypto status constants}
 RT2X00_RX_CRYPTO_SUCCESS  = 0;
 RT2X00_RX_CRYPTO_FAIL_ICV = 1;
 RT2X00_RX_CRYPTO_FAIL_MIC = 2;
 RT2X00_RX_CRYPTO_FAIL_KEY = 3;

 {RX descriptor flags}
 RT2X00_RXDONE_SIGNAL_PLCP    = (1 shl 0); {Signal field contains the plcp value}
 RT2X00_RXDONE_SIGNAL_BITRATE = (1 shl 1); {Signal field contains the bitrate value}
 RT2X00_RXDONE_SIGNAL_MCS     = (1 shl 2); {Signal field contains the mcs value}
 RT2X00_RXDONE_MY_BSS         = (1 shl 3); {Does this frame originate from device's BSS}
 RT2X00_RXDONE_CRYPTO_IV      = (1 shl 4); {Driver provided IV/EIV data}
 RT2X00_RXDONE_CRYPTO_ICV     = (1 shl 5); {Driver provided ICV data}
 RT2X00_RXDONE_L2PAD          = (1 shl 6); {802.11 payload has been padded to 4-byte boundary}
 
 RT2X00_RXDONE_SIGNAL_MASK    = (RT2X00_RXDONE_SIGNAL_PLCP or RT2X00_RXDONE_SIGNAL_BITRATE or RT2X00_RXDONE_SIGNAL_MCS);
 
 {TX descriptor flags}
 RT2X00_ENTRY_TXD_RTS_FRAME        = (1 shl 0);  {This frame is a RTS frame}
 RT2X00_ENTRY_TXD_CTS_FRAME        = (1 shl 1);  {This frame is a CTS-to-self frame}
 RT2X00_ENTRY_TXD_GENERATE_SEQ     = (1 shl 2);  {This frame requires sequence counter}
 RT2X00_ENTRY_TXD_FIRST_FRAGMENT   = (1 shl 3);  {This is the first frame}
 RT2X00_ENTRY_TXD_MORE_FRAG        = (1 shl 4);  {This frame is followed by another fragment}
 RT2X00_ENTRY_TXD_REQ_TIMESTAMP    = (1 shl 5);  {Require timestamp to be inserted}
 RT2X00_ENTRY_TXD_BURST            = (1 shl 6);  {This frame belongs to the same burst event}
 RT2X00_ENTRY_TXD_ACK              = (1 shl 7);  {An ACK is required for this frame}
 RT2X00_ENTRY_TXD_RETRY_MODE       = (1 shl 8);  {When set, the long retry count is used}
 RT2X00_ENTRY_TXD_ENCRYPT          = (1 shl 9);  {This frame should be encrypted}
 RT2X00_ENTRY_TXD_ENCRYPT_PAIRWISE = (1 shl 10); {Use pairwise key table (instead of shared)}
 RT2X00_ENTRY_TXD_ENCRYPT_IV       = (1 shl 11); {Generate IV/EIV in hardware}
 RT2X00_ENTRY_TXD_ENCRYPT_MMIC     = (1 shl 12); {Generate MIC in hardware}
 RT2X00_ENTRY_TXD_HT_AMPDU         = (1 shl 13); {This frame is part of an AMPDU}
 RT2X00_ENTRY_TXD_HT_BW_40         = (1 shl 14); {Use 40MHz Bandwidth}
 RT2X00_ENTRY_TXD_HT_SHORT_GI      = (1 shl 15); {Use short GI}
 RT2X00_ENTRY_TXD_HT_MIMO_PS       = (1 shl 16); {The receiving STA is in dynamic SM PS mode}
 
 {TX complete flags}
 RT2X00_TXDONE_UNKNOWN         = (1 shl 0); {Hardware could not determine success of transmission}
 RT2X00_TXDONE_SUCCESS         = (1 shl 1); {Frame was successfully sent}
 RT2X00_TXDONE_FALLBACK        = (1 shl 2); {Hardware used fallback rates for retries}
 RT2X00_TXDONE_FAILURE         = (1 shl 3); {Frame was not successfully sent}
 RT2X00_TXDONE_EXCESSIVE_RETRY = (1 shl 4); {In addition to TXDONE_FAILURE, the frame transmission failed due to excessive retries}
 RT2X00_TXDONE_AMPDU           = (1 shl 5);
 
 {Antenna constants}
 RT2X00_ANTENNA_SW_DIVERSITY = 0;
 RT2X00_ANTENNA_A            = 1;
 RT2X00_ANTENNA_B            = 2;
 RT2X00_ANTENNA_HW_DIVERSITY = 3;

 {Antenna flags}
 RT2X00_ANTENNA_RX_DIVERSITY = $00000001;
 RT2X00_ANTENNA_TX_DIVERSITY = $00000002;
 RT2X00_ANTENNA_MODE_SAMPLE  = $00000004;
 
 {LED type constants}
 RT2X00_LED_TYPE_RADIO    = 0;
 RT2X00_LED_TYPE_ASSOC    = 1;
 RT2X00_LED_TYPE_ACTIVITY = 2;
 RT2X00_LED_TYPE_QUALITY  = 3;
 
 {LED mode constants}
 RT2X00_LED_MODE_DEFAULT         = 0;
 RT2X00_LED_MODE_TXRX_ACTIVITY   = 1;
 RT2X00_LED_MODE_SIGNAL_STRENGTH = 2;
 RT2X00_LED_MODE_ASUS            = 3;
 RT2X00_LED_MODE_ALPHA           = 4;

 {TSF sync constants}
 RT2X00_TSF_SYNC_NONE    = 0;
 RT2X00_TSF_SYNC_INFRA   = 1;
 RT2X00_TSF_SYNC_ADHOC   = 2;
 RT2X00_TSF_SYNC_AP_NONE = 3;
 
 {Device state constants}
 RT2X00_STATE_DEEP_SLEEP    = 0;
 RT2X00_STATE_SLEEP         = 1;
 RT2X00_STATE_STANDBY       = 2;
 RT2X00_STATE_AWAKE         = 3;
 {Additional device states, these values are not directly passed into the device}
 RT2X00_STATE_RADIO_ON      = 4; 
 RT2X00_STATE_RADIO_OFF     = 5;
 RT2X00_STATE_RADIO_IRQ_ON  = 6;
 RT2X00_STATE_RADIO_IRQ_OFF = 7;
 
 {IFS backoff constants}
 RT2X00_IFS_BACKOFF     = 0;
 RT2X00_IFS_SIFS        = 1;
 RT2X00_IFS_NEW_BACKOFF = 2;
 RT2X00_IFS_NONE        = 3;
 
 {IFS backoff constants for HT devices}
 RT2X00_TXOP_HTTXOP  = 0;
 RT2X00_TXOP_PIFS    = 1;
 RT2X00_TXOP_SIFS    = 2;
 RT2X00_TXOP_BACKOFF = 3;
 
 {Cipher constants for hardware encryption}
 RT2X00_CIPHER_NONE        = 0;
 RT2X00_CIPHER_WEP64       = 1;
 RT2X00_CIPHER_WEP128      = 2;
 RT2X00_CIPHER_TKIP        = 3;
 RT2X00_CIPHER_AES         = 4;
 {The following were added by rt61pci and rt73usb}
 RT2X00_CIPHER_CKIP64      = 5;
 RT2X00_CIPHER_CKIP128     = 6;
 RT2X00_CIPHER_TKIP_NO_MIC = 7; {Don't send to device}
 
 RT2X00_CIPHER_MAX         = 4; {Note that CIPHER_NONE isn't counted, and CKIP64 and CKIP128 are excluded}
 
 {Rate modulation constants}
 RT2X00_RATE_MODE_CCK           = 0;
 RT2X00_RATE_MODE_OFDM          = 1;
 RT2X00_RATE_MODE_HT_MIX        = 2;
 RT2X00_RATE_MODE_HT_GREENFIELD = 3;
 
 {Size constants}
 {Ralink PCI devices demand the Frame size to be a multiple of 128 bytes, for USB devices this restriction does not apply,
  but the value of 2432 makes sense since it is big enough to contain the maximum fragment size according to the IEEE 802.11 specs.
  The aggregation size depends on support from the driver, but should be something around 3840 bytes}
 RT2X00_DATA_FRAME_SIZE  = 2432;
 RT2X00_MGMT_FRAME_SIZE  = 256;
 RT2X00_AGGREGATION_SIZE = 3840;
 
 {Channel constants}
 RT2X00_CHANNEL_GEOGRAPHY_ALLOWED = $00000001;
 
 {Specifications constants}
 {Supported Bands}
 RT2X00_SUPPORT_BAND_2GHZ  = $00000001;
 RT2X00_SUPPORT_BAND_5GHZ  = $00000002;
 
 {Supported Rates}
 RT2X00_SUPPORT_RATE_CCK   = $00000001;
 RT2X00_SUPPORT_RATE_OFDM  = $00000002;
 
 {Device Supported Rates}
 RT2X00_DEV_RATE_CCK            = $0001;
 RT2X00_DEV_RATE_OFDM           = $0002;
 RT2X00_DEV_RATE_SHORT_PREAMBLE = $0004;
 
 {Configuration constants}
 RT2X00_CONFIG_UPDATE_TYPE  = (1 shl 1);
 RT2X00_CONFIG_UPDATE_MAC   = (1 shl 2);
 RT2X00_CONFIG_UPDATE_BSSID = (1 shl 3);
 
 //To Do //See rt2x00reg.h
 
{==============================================================================}
type
 {RT2X00LIB specific types}
 PRT2X00Chipset = ^TRT2X00Chipset; {rt2x00_chip}
 TRT2X00Chipset = record
  RTChip:Word;    {RT2X00_RT2460 etc}
  RFChip:Word;    {RT2800_RF2820 etc}
  Revision:Word;  {RT2800_REV_RT2860C etc}
 end;

 PRT2X00Rate = ^TRT2X00Rate;
 TRT2X00Rate = record
  Flags:Word;
  BitRate:Word;  {In 100kbit/s}
  RateMask:Word;
  PLCP:Word;
  MCS:Word;
 end;
  
 PRT2X00Antenna = ^TRT2X00Antenna; {antenna_setup}
 TRT2X00Antenna = record
  RX:Byte;        {RT2X00_ANTENNA_A etc}
  TX:Byte;        {RT2X00_ANTENNA_A etc}
  RXChainNo:Byte;
  TXChainNo:Byte;
 end;
 
 PRT2X00Channel = ^TRT2X00Channel; {channel_info}
 TRT2X00Channel = record
  Flags:LongWord;   {RT2X00_CHANNEL_GEOGRAPHY_ALLOWED etc}
  MaxPower:ShortInt;
  DefaultPower1:ShortInt;
  DefaultPower2:ShortInt;
  DefaultPower3:ShortInt;
 end;

 PRT2X00Channels = ^TRT2X00Channels;
 TRT2X00Channels = array[0..0] of TRT2X00Channel;
 
 PRT2X00RFChannel = ^TRT2X00RFChannel; {rf_channel}
 TRT2X00RFChannel = record
  Channel:LongWord;
  RF1:LongWord;
  RF2:LongWord;
  RF3:LongWord;
  RF4:LongWord;
 end;
 
 PRT2X00RFChannels = ^TRT2X00RFChannels;
 TRT2X00RFChannels = array[0..0] of TRT2X00RFChannel;
 
 PRT2X00Specifications = ^TRT2X00Specifications; {hw_mode_spec}
 TRT2X00Specifications = record
  SupportedBands:LongWord;  {RT2X00_SUPPORT_BAND_2GHZ etc}
  SupportedRates:LongWord;  {RT2X00_SUPPORT_RATE_CCK etc}
  ChannelCount:LongWord;
  Channels:PRT2X00Channels;
  RFChannels:PRT2X00RFChannels;
  HTCapabilities:TIEEE80211StationHTCap;
 end;
 
 //To Do //See: rt2x00.h //rt2x00_sta //rt2x00lib_erp //rt2x00lib_crypto
 
 PRT2X00EWMA = ^TRT2X00EWMA; {Exponentially weighted moving average (EWMA)}
 TRT2X00EWMA = record
  Internal:LongWord;
  Factor:LongWord;
  Weight:LongWord;
 end;
 
 PRT2X00LinkQuality = ^TRT2X00LinkQuality; {Quality statistics about the currently active link} {link_qual}
 TRT2X00LinkQuality = record
  RSSI:LongInt;            {Statistics required for Link tuning by driver}
  FalseCCA:LongInt;
  
  {VGC levels}
  VGCLevel:Byte;           {Driver will tune the VGC level during each call to the link tuner callback function}
  VGCLevelRegister:Byte;   {The vgc_level is determined based on the link quality statistics like average RSSI and the false CCA count}
  
  RXSuccess:LongWord;      {Statistics required for Signal quality calculation}
  RXFailed:LongWord; 
  TXSuccess:LongWord; 
  TXFailed:LongWord; 
 end;
 
 PRT2X00LinkAntenna = ^TRT2X00LinkAntenna; {Antenna settings about the currently active link} {link_ant}
 TRT2X00LinkAntenna = record
  Flags:LongWord;               {Antenna flags (RT2X00_ANTENNA_RX_DIVERSITY etc)}
  ActiveAntenna:TRT2X00Antenna; {Currently active TX/RX antenna setup}
  RSSIHistory:LongInt;          {RSSI history information for the antenna}
  RSSIAntenna:TRT2X00EWMA;      {Current RSSI average of the currently active antenna}
 end;
 
 PRT2X00Link = ^TRT2X00Link; {link}
 TRT2X00Link = record
  Count:LongWord;             {Link tuner counter}
  Quality:TRT2X00LinkQuality; {Quality measurement values}
  Antenna:TRT2X00LinkAntenna; {TX/RX antenna setup} 
  RSSIAverage:TRT2X00EWMA;    {Currently active average RSSI value}
  //To Do //link
 end;
 
 PRT2X00Interface = ^TRT2X00Interface; {rt2x00_intf}
 TRT2X00Interface = record
 
  EnableBeacon:LongBool; 
  SequenceNo:LongWord;   {Software sequence counter (only required for hardware which doesn't support hardware sequence counting)} 
  //To Do //rt2x00_intf
 end;

 PRT2X00ChannelConfiguration = ^TRT2X00ChannelConfiguration; {rt2x00lib_conf}
 TRT2X00ChannelConfiguration = record
  Channel:TRT2X00Channel;
  RFChannel:TRT2X00RFChannel;
  //To Do //rt2x00lib_conf
 end;
 
 PRT2X00InterfaceConfiguration = ^TRT2X00InterfaceConfiguration; {rt2x00intf_conf}
 TRT2X00InterfaceConfiguration = record
  InterfaceType:LongWord;        {Interface type (WIFI_IFTYPE_STATION etc)}
  TSFSync:LongWord;              {TSF sync value (dependent on the interface type) (RT2X00_TSF_SYNC_NONE etc)}
  MAC:array[0..1] of LongWord;   {All devices (except RT2500USB) have 32 bits register word sizes}
  BSSID:array[0..1] of LongWord; {Which means these variables must be a multiple of 32 bits}
  //To Do //rt2x00intf_conf
 end;
 
 PRT2X00RXDescriptor = ^TRT2X00RXDescriptor; {rxdone_entry_desc}
 TRT2X00RXDescriptor = record
  Timestamp:Int64;                {RX Timestamp}
  Signal:LongInt;                 {Signal of the received frame}
  RSSI:LongInt;                   {RSSI of the received frame}
  Size:LongWord;                  {Data size of the received frame}
  Flags:LongWord;                 {Receive flags (WIFI_RX_FLAG_*)}
  RXFlags:LongWord;               {Receive flags (RT2X00_RXDONE_*)}
  RateMode:Word;                  {Rate mode (RT2X00_RATE_MODE_*)}
  Cipher:Byte;                    {Cipher type used during decryption}
  CipherStatus:Byte;              {Decryption status}
  IV:array[0..1] of LongWord;     {IV/EIV data used during decryption}
  ICV:LongWord;                   {ICV data used during decryption}
 end;
 
 PRT2X00TXDescriptor = ^TRT2X00TXDescriptor; {txentry_desc}
 TRT2X00TXDescriptor = record
  Flags:LongWord;
  Length:Word;
  HeaderLength:Word;
  //To Do //txentry_desc
 end;
 
 PRT2X00TXComplete = ^TRT2X00TXComplete; {txdone_entry_desc}
 TRT2X00TXComplete = record
  Flags:LongWord; {Transmit flags (RT2X00_TXDONE_*)}
  Retry:LongWord; {Retry count}
 end;
 
 {RT2X00 Device}
 PRT2X00WiFiDevice = ^TRT2X00WiFiDevice;
 
 {RT2X00 Device Methods}
 TRT2X00DriverInit = function(RT2X00:PRT2X00WiFiDevice):LongWord;
 
 TRT2X00EepromLoad = function(RT2X00:PRT2X00WiFiDevice;Data:PWord;Size:LongWord):LongWord;
 
 TRT2X00RegisterRead = function(RT2X00:PRT2X00WiFiDevice;Offset:Word;Value:PLongWord):LongWord;
 TRT2X00RegisterWrite = function(RT2X00:PRT2X00WiFiDevice;Offset:Word;Value:LongWord):LongWord;
 
 TRT2X00RegisterMultiRead = function(RT2X00:PRT2X00WiFiDevice;Offset:Word;Data:Pointer;Size:LongWord):LongWord;
 TRT2X00RegisterMultiWrite = function(RT2X00:PRT2X00WiFiDevice;Offset:Word;Data:Pointer;Size:LongWord):LongWord;
 
 TRT2X00RegisterBusyRead = function(RT2X00:PRT2X00WiFiDevice;Offset,Mask:LongWord;Reg:PLongWord):Boolean; 

 TRT2X00SetLED = function(RT2X00:PRT2X00WiFiDevice;ID,Level:LongWord):LongWord;
 TRT2X00SetState = function(RT2X00:PRT2X00WiFiDevice;State:LongWord):LongWord;
 
 TRT2X00EnableRX = function(RT2X00:PRT2X00WiFiDevice):LongWord;
 TRT2X00DisableRX = function(RT2X00:PRT2X00WiFiDevice):LongWord;
 
 TRT2X00EnableBeacon = function(RT2X00:PRT2X00WiFiDevice):LongWord;
 TRT2X00DisableBeacon = function(RT2X00:PRT2X00WiFiDevice):LongWord;
  
 TRT2X00GetFirmware = function(RT2X00:PRT2X00WiFiDevice;var Name:String;var Address:Pointer;var Size:LongWord):Boolean;
 TRT2X00CheckFirmware = function(RT2X00:PRT2X00WiFiDevice;Data:PByte;Size:LongWord):Boolean;
 TRT2X00LoadFirmware = function(RT2X00:PRT2X00WiFiDevice;Data:PByte;Size:LongWord):Boolean;
 TRT2X00WriteFirmware = function(RT2X00:PRT2X00WiFiDevice;Data:PByte;Size:LongWord):Boolean;

 TRT2X00Configure = function(RT2X00:PRT2X00WiFiDevice;Configuration:PRT2X00ChannelConfiguration;Flags:LongWord):LongWord; 
 TRT2X00ConfigureFilter = function(RT2X00:PRT2X00WiFiDevice;Filter:LongWord):LongWord;
 TRT2X00ConfigureAntenna = function(RT2X00:PRT2X00WiFiDevice;Antenna:PRT2X00Antenna):LongWord;
 TRT2X00ConfigureInterface = function(RT2X00:PRT2X00WiFiDevice;Configuration:PRT2X00InterfaceConfiguration;Flags:LongWord):LongWord;
 
 TRT2X00InitializeRegisters = function(RT2X00:PRT2X00WiFiDevice):LongWord;
 
 TRT2X00HardwareEncryptionDisabled = function(RT2X00:PRT2X00WiFiDevice):Boolean;
 
 TRT2X00WiFiDevice = record
  {WiFi Properties}
  WiFi:TWiFiDevice;
  {RT2X00 Methods}
  DriverInit:TRT2X00DriverInit;
  EepromLoad:TRT2X00EepromLoad;
  RegisterRead:TRT2X00RegisterRead;
  RegisterWrite:TRT2X00RegisterWrite;
  RegisterMultiRead:TRT2X00RegisterMultiRead;
  RegisterMultiWrite:TRT2X00RegisterMultiWrite;
  RegisterBusyRead:TRT2X00RegisterBusyRead;
  SetLED:TRT2X00SetLED;
  SetState:TRT2X00SetState;
  EnableRX:TRT2X00EnableRX;
  DisableRX:TRT2X00DisableRX;
  EnableBeacon:TRT2X00EnableBeacon;
  DisableBeacon:TRT2X00DisableBeacon;
  GetFirmware:TRT2X00GetFirmware;
  CheckFirmware:TRT2X00CheckFirmware;
  LoadFirmware:TRT2X00LoadFirmware;
  WriteFirmware:TRT2X00WriteFirmware;
  Configure:TRT2X00Configure;
  ConfigureFilter:TRT2X00ConfigureFilter;
  ConfigureAntenna:TRT2X00ConfigureAntenna;
  ConfigureInterface:TRT2X00ConfigureInterface;
  InitializeRegisters:TRT2X00InitializeRegisters;
  HardwareEncryptionDisabled:TRT2X00HardwareEncryptionDisabled;
  {RT2X00 Properties}
  Flags:LongWord;                          {Driver configuration flags (RT2X00_CONFIG_*)}
  Chipset:TRT2X00Chipset;                  {Detected RT/RF chipset and revision}
  Capabilities:LongWord;                   {Detected capabilities flags}
  Requirements:LongWord;                   {Detected requirements flags}
  Antenna:TRT2X00Antenna;                  {Default Antenna configuration}
  Specifications:TRT2X00Specifications;    {Detected hardware specifications (Channels, Bands, Rates etc)}  
  
  LNAGain:SmallInt;                        {LNA gain}
  TXPower:Word;                            {Current TX power value}
  ShortRetry:Byte;                         {Current retry values}
  LongRetry:Byte;
  RSSIOffset:Byte;                         {RSSI <-> Dbm offset}
  FrequencyOffset:Byte;                    {Frequency offset}
  APInterfaceCount:LongWord;               {AP interface count}
  STAInterfaceCount:LongWord;              {STA interface count}
  AssociationCount:LongWord;               {Association count}
  BeaconingCount:LongWord;                 {Beaconing enabled count}
  PacketFilter:LongWord;                   {Current packet filter configuration for the device (IEEE80211_FIF_*)}

  Link:TRT2X00Link;                        {Link information}
  
  //To Do
  RFChannel:LongWord;                      {Used for VCO periodic calibration}
  
  //To Do
  LEDRadio:LongBool;                       {If True then chipset has a radio LED}
  LEDAssoc:LongBool;                       {If True then chipset has an association LED}
  LEDActivity:LongBool;                    {If True then chipset has an activity LED}
  LEDQuality:LongBool;                     {If True then chipset has a quality LED}
  LEDMCURegister:Word;                     {The MCU value read from EEPROM during initialization}
  
  //To Do
  CurrentBand:LongWord;                    {Current RF band (eg IEEE80211_BAND_2GHZ)}
  CurrentFrequency:LongWord;               {Current RF frequency} 
  
  Data:Pointer;                            {Chipset Driver specific data (eg RT2800)}
  DataSize:LongWord;                       {Size of the chipset driver data}
  RFData:PLongWord;                        {RF chip data}
  RFSize:LongWord;                         {RF data size}
  EepromData:PWord;                        {EEPROM data}
  EepromSize:LongWord;                     {EEPROM size}
  CSRLock:TMutexHandle;                    {Protect against concurrent indirect register access (BBP, RF, MCU)}
  //To Do
  
  TXWISize:LongWord;                       {Size of the TXWI field in the TX frame}
  RXWISize:LongWord;                       {Size of the RXWI field in the RX frame}
  TXINFOSize:LongWord;                     {Size of the TXINFO field in the TX frame}
  RXINFOSize:LongWord;                     {Size of the RXINFO field in the RX frame}
  
  ExtraTXHeadroom:LongWord;                {Extra headroom required for TX frames}
 end; 
  
 
{==============================================================================}
{var}
 {RT2X00LIB specific variables}
 
const
 RT2X00SupportedRates:array[0..11] of TRT2X00Rate = (
  (Flags: RT2X00_DEV_RATE_CCK; BitRate: 10; RateMask: (1 shl 0); PLCP: $00; MCS: ((RT2X00_RATE_MODE_CCK and $00ff) shl 8) or (0 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_CCK,0)}),
  (Flags: RT2X00_DEV_RATE_CCK or RT2X00_DEV_RATE_SHORT_PREAMBLE; BitRate: 20; RateMask: (1 shl 1); PLCP: $01; MCS: ((RT2X00_RATE_MODE_CCK and $00ff) shl 8) or (1 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_CCK, 1)}),
  (Flags: RT2X00_DEV_RATE_CCK or RT2X00_DEV_RATE_SHORT_PREAMBLE; BitRate: 55; RateMask: (1 shl 2); PLCP: $02; MCS: ((RT2X00_RATE_MODE_CCK and $00ff) shl 8) or (2 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_CCK, 2)}),
  (Flags: RT2X00_DEV_RATE_CCK or RT2X00_DEV_RATE_SHORT_PREAMBLE; BitRate: 110; RateMask: (1 shl 3); PLCP: $03; MCS: ((RT2X00_RATE_MODE_CCK and $00ff) shl 8) or (3 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_CCK, 3)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 60; RateMask: (1 shl 4); PLCP: $0b; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (0 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 0)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 90; RateMask: (1 shl 5); PLCP: $0f; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (1 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 1)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 120; RateMask: (1 shl 6); PLCP: $0a; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (2 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 2)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 180; RateMask: (1 shl 7); PLCP: $0e; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (3 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 3)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 240; RateMask: (1 shl 8); PLCP: $09; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (4 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 4)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 360; RateMask: (1 shl 9); PLCP: $0d; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (5 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 5)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 480; RateMask: (1 shl 10); PLCP: $08; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (6 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 6)}),
  (Flags: RT2X00_DEV_RATE_OFDM; BitRate: 540; RateMask: (1 shl 11); PLCP: $0c; MCS: ((RT2X00_RATE_MODE_OFDM and $00ff) shl 8) or (7 and $00ff) {RATE_MCS(RT2X00_RATE_MODE_OFDM, 7)}) 
 );
  
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{RT2X00LIB Functions}
function RT2X00DriverInit(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2X00DriverQuit(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00InitializeModes(RT2X00:PRT2X00WiFiDevice):LongWord;
 
function RT2X00LoadFirmware(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00EnableRX(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2X00DisableRX(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00EnableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2X00DisableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;
 
function RT2X00EnableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2X00DisableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00SetLED(RT2X00:PRT2X00WiFiDevice;ID:LongWord;Enabled:Boolean):LongWord;
function RT2X00RadioLED(RT2X00:PRT2X00WiFiDevice;Enabled:Boolean):LongWord;
function RT2X00AssocLED(RT2X00:PRT2X00WiFiDevice;Enabled:Boolean):LongWord;
function RT2X00ActivityLED(RT2X00:PRT2X00WiFiDevice;Enabled:Boolean):LongWord;
function RT2X00QualityLED(RT2X00:PRT2X00WiFiDevice;RSSI:LongInt):LongWord;

procedure RT2X00InitializeRate(Entry:PIEEE80211Rate;Index:Word;Rate:PRT2X00Rate);
procedure RT2X00InitializeChannel(Entry:PIEEE80211Channel;Channel,TXPower,Value:LongWord);

function RT2X00GetHTCenterChannel(RT2X00:PRT2X00WiFiDevice):Word;

function RT2X00Configure(RT2X00:PRT2X00WiFiDevice;Flags:LongWord):LongWord;
function RT2X00ConfigureAntenna(RT2X00:PRT2X00WiFiDevice;Antenna:PRT2X00Antenna):LongWord;
function RT2X00ConfigureInterface(RT2X00:PRT2X00WiFiDevice;InterfaceType:LongWord;Address,BSSID:PHardwareAddress):LongWord;

//To Do //rt2x00link_tuner//rt2x00link_watchdog//rt2x00link_agc//rt2x00link_vcocal in rt2x00link.c

{==============================================================================}
{RT2X00LIB Helper Functions}
function RT2X00IsPCI(RT2X00:PRT2X00WiFiDevice):Boolean;
function RT2X00IsPCIe(RT2X00:PRT2X00WiFiDevice):Boolean;
function RT2X00IsUSB(RT2X00:PRT2X00WiFiDevice):Boolean;
function RT2X00IsMMIO(RT2X00:PRT2X00WiFiDevice):Boolean;

function RT2X00GetRTChip(RT2X00:PRT2X00WiFiDevice):Word;
procedure RT2X00SetRTChip(RT2X00:PRT2X00WiFiDevice;RTChip:Word);

function RT2X00GetRFChip(RT2X00:PRT2X00WiFiDevice):Word;
procedure RT2X00SetRFChip(RT2X00:PRT2X00WiFiDevice;RFChip:Word);

function RT2X00GetRevision(RT2X00:PRT2X00WiFiDevice):Word;
procedure RT2X00SetRevision(RT2X00:PRT2X00WiFiDevice;Revision:Word);

function RT2X00IsRTChipRevision(RT2X00:PRT2X00WiFiDevice;RTChip:Word;Revision:Word):Boolean;
function RT2X00IsRTChipRevisionLT(RT2X00:PRT2X00WiFiDevice;RTChip:Word;Revision:Word):Boolean;
function RT2X00IsRTChipRevisionGTE(RT2X00:PRT2X00WiFiDevice;RTChip:Word;Revision:Word):Boolean;

function RT2X00GetCapability(RT2X00:PRT2X00WiFiDevice;Capability:LongWord):Boolean;
procedure RT2X00SetCapability(RT2X00:PRT2X00WiFiDevice;Capability:LongWord);
procedure RT2X00ClearCapability(RT2X00:PRT2X00WiFiDevice;Capability:LongWord);

function RT2X00GetRequirement(RT2X00:PRT2X00WiFiDevice;Requirement:LongWord):Boolean;
procedure RT2X00SetRequirement(RT2X00:PRT2X00WiFiDevice;Requirement:LongWord);
procedure RT2X00ClearRequirement(RT2X00:PRT2X00WiFiDevice;Requirement:LongWord);

function RT2X00RFRead(RT2X00:PRT2X00WiFiDevice;Index:LongWord;Data:PLongWord):Boolean;
function RT2X00RFWrite(RT2X00:PRT2X00WiFiDevice;Index,Data:LongWord):Boolean;

function RT2X00GetEeprom8(RT2X00:PRT2X00WiFiDevice;Offset:Word):Byte;

function RT2X00GetEeprom16(RT2X00:PRT2X00WiFiDevice;Offset:Word):Word;
procedure RT2X00SetEeprom16(RT2X00:PRT2X00WiFiDevice;Offset,Value:Word);

function RT2X00GetEepromAddress(RT2X00:PRT2X00WiFiDevice;Offset:Word):Pointer;

function RT2X00GetRegister8(Reg,Mask,Shift:Byte):Byte; inline;
procedure RT2X00SetRegister8(var Reg:Byte;Mask,Shift,Value:Byte); inline;

function RT2X00GetRegister16(Reg,Mask,Shift:Word):Word; inline;
procedure RT2X00SetRegister16(var Reg:Word;Mask,Shift,Value:Word); inline;

function RT2X00GetRegister32(Reg,Mask,Shift:LongWord):LongWord; inline;
procedure RT2X00SetRegister32(var Reg:LongWord;Mask,Shift,Value:LongWord); {inline;} {Don't inline, can cause a compiler fault}

function RT2X00ReadDescriptor(Descriptor:PLongWord;Index:Byte):LongWord; inline;
procedure RT2X00WriteDescriptor(Descriptor:PLongWord;Index:Byte;Value:LongWord); inline;

function RT2X00GetRate(Value:Word):PRT2X00Rate; inline;
function RT2X00GetRateMCS(Value:Word):LongInt; inline;

function RT2X00GetTXBufferSize(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2X00GetTXBufferOffset(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00GetRXBufferSize(RT2X00:PRT2X00WiFiDevice):LongWord;
function RT2X00GetRXBufferOffset(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00InsertL2PAD(var Data:Pointer;var Size:LongWord;HeaderLength:LongWord):Boolean;
function RT2X00RemoveL2PAD(var Data:Pointer;var Size:LongWord;HeaderLength:LongWord):Boolean;

function RT2X00ReceiveReadSignal(RT2X00:PRT2X00WiFiDevice;Descriptor:PRT2X00RXDescriptor):LongWord;
function RT2X00ReceiveInsertIV(Descriptor:PRT2X00RXDescriptor;var Data:Pointer;var Size:LongWord;HeaderLength:LongWord):Boolean;

function RT2X00_RATE_MCS(Mode,MCS:Word):Word; inline;
function RT2X00_L2PAD_SIZE(HeaderLength:LongWord):LongWord; inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{RT2X00LIB Functions}
function RT2X00DriverInit(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RT2X00: Driver init');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_ALREADY_OPEN;
 if RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_OPEN then Exit;
 
 {Initialize Driver}
 if Assigned(RT2X00.DriverInit) then
  begin
   Result:=RT2X00.DriverInit(RT2X00);
   if Result <> ERROR_SUCCESS then Exit;
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
   Exit;
  end;  
  
 {Get TX Headroom value}
 if RT2X00IsUSB(RT2X00) then
  begin
   RT2X00.ExtraTXHeadroom:=RT2X00.TXWISize + RT2X00.TXINFOSize;
  end
 else
  begin
   RT2X00.ExtraTXHeadroom:=RT2X00.TXWISize;
  end;  
 
 {Determine which operating modes are supported}
 RT2X00.WiFi.InterfaceModes:=(1 shl WIFI_IFTYPE_STATION);
 //To Do //rt2x00lib_probe_dev
 
 {Initialize Modes}
 Result:=RT2X00InitializeModes(RT2X00);
 if Result <> ERROR_SUCCESS then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'RT2X00: Failed to initialize hardware modes');
   Exit;
  end;
  
 {Initialize HW fields}
 //To Do //rt2x00lib_probe_hw
 
 {Initialize extra TX headroom required}
 RT2X00.WiFi.Hardware.ExtraTXHeadroom:=Max(IEEE80211_TX_STATUS_HEADROOM,RT2X00.ExtraTXHeadroom);
 
 {Take TX headroom required for alignment into account}
 if RT2X00GetRequirement(RT2X00,RT2X00_REQUIRE_L2PAD) then
  begin
   Inc(RT2X00.WiFi.Hardware.ExtraTXHeadroom,RT2X00_L2PAD_EXTRA);
  end
 else if RT2X00GetRequirement(RT2X00,RT2X00_REQUIRE_DMA) then
  begin
   Inc(RT2X00.WiFi.Hardware.ExtraTXHeadroom,RT2X00_ALIGN_EXTRA);
  end;
  
 {Allocate tx status FIFO}
 //To Do //rt2x00lib_probe_hw
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RT2X00DriverQuit(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RT2X00: Driver quit');
 {$ENDIF}
 
 //To Do //rt2x00lib_remove_dev

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RT2X00InitializeModes(RT2X00:PRT2X00WiFiDevice):LongWord;
var
 Count:LongWord;
 RateCount:LongWord;
 Rates:PIEEE80211Rates;
 Channels:PIEEE80211Channels;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RT2X00: Initialize Modes');
 {$ENDIF}

 {Check State}
 Result:=ERROR_ALREADY_OPEN;
 if RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_OPEN then Exit;
 
 {Number of rates}
 RateCount:=0;
 if (RT2X00.Specifications.SupportedRates and RT2X00_SUPPORT_RATE_CCK) <> 0 then
  begin
   Inc(RateCount,4);
  end;
 if (RT2X00.Specifications.SupportedRates and RT2X00_SUPPORT_RATE_OFDM) <> 0 then
  begin
   Inc(RateCount,8);
  end;
  
 {Allocate Rates}
 Rates:=AllocMem(RateCount * SizeOf(TIEEE80211Rate)); 
 if Rates = nil then 
  begin
   Result:=ERROR_OUTOFMEMORY;
   Exit;
  end;
  
 {Allocate Channels}
 Channels:=AllocMem(RT2X00.Specifications.ChannelCount * SizeOf(TIEEE80211Channel));
 if Channels = nil then 
  begin
   FreeMem(Rates);
   
   Result:=ERROR_OUTOFMEMORY;
   Exit;
  end;
 try
  {Initialize Rate list}
  for Count:=0 to RateCount - 1 do
   begin
    RT2X00InitializeRate(@Rates[Count],Count,RT2X00GetRate(Count));
   end;
 
  {Initialize Channel list}
  for Count:=0 to RT2X00.Specifications.ChannelCount - 1 do
   begin
    RT2X00InitializeChannel(@Channels[Count],RT2X00.Specifications.RFChannels[Count].Channel,RT2X00.Specifications.Channels[Count].MaxPower,Count);
   end;
  
  {Intitialize 802.11b, 802.11g} {Rates: CCK, OFDM. Channels: 2.4 GHz}
  if (RT2X00.Specifications.SupportedBands and RT2X00_SUPPORT_BAND_2GHZ) <> 0 then
   begin
    {Create Band}
    RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ]:=AllocMem(SizeOf(TIEEE80211SupportedBand));
    
    {Update Band}
    RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].ChannelCount:=14;
    RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].RateCount:=RateCount;
    RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].Channels:=AllocMem(RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].ChannelCount * SizeOf(TIEEE80211Channel));
    RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].Rates:=AllocMem(RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].RateCount * SizeOf(TIEEE80211Rate));
    
    {$IFDEF RT2800USB_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RT2X00:  (Band=IEEE80211_BAND_2GHZ Channels=' + IntToStr(RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].ChannelCount) + ' Rates=' + IntToStr(RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].RateCount) + ')');
    {$ENDIF}
    
    {Copy Channels}
    for Count:=0 to RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].ChannelCount - 1 do
     begin
      System.Move(Channels[Count],RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].Channels[Count],SizeOf(TIEEE80211Channel));
     end;
     
    {Copy Rates}
    for Count:=0 to RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].RateCount - 1 do
     begin
      System.Move(Rates[Count],RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].Rates[Count],SizeOf(TIEEE80211Rate));
     end;
     
    {Copy HT Capabilities}
    System.Move(RT2X00.Specifications.HTCapabilities,RT2X00.WiFi.Bands[IEEE80211_BAND_2GHZ].HTCapabilities,SizeOf(TIEEE80211StationHTCap));
   end;
  
  {Intitialize 802.11a} {Rates: OFDM. Channels: OFDM, UNII, HiperLAN2}
  if (RT2X00.Specifications.SupportedBands and RT2X00_SUPPORT_BAND_5GHZ) <> 0 then
   begin
    {Create Band}
    RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ]:=AllocMem(SizeOf(TIEEE80211SupportedBand));
    
    {Update Band}
    RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].ChannelCount:=RT2X00.Specifications.ChannelCount - 14;
    RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].RateCount:=RateCount - 4;
    RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].Channels:=AllocMem(RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].ChannelCount * SizeOf(TIEEE80211Channel));
    RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].Rates:=AllocMem(RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].RateCount * SizeOf(TIEEE80211Rate));
   
    {$IFDEF RT2800USB_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RT2X00:  (Band=IEEE80211_BAND_5GHZ Channels=' + IntToStr(RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].ChannelCount) + ' Rates=' + IntToStr(RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].RateCount) + ')');
    {$ENDIF}
   
    {Copy Channels}
    for Count:=0 to RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].ChannelCount - 1 do
     begin
      System.Move(Channels[Count + 14],RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].Channels[Count],SizeOf(TIEEE80211Channel));
     end;
    
    {Copy Rates}
    for Count:=0 to RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].RateCount - 1 do
     begin
      System.Move(Rates[Count + 4],RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].Rates[Count],SizeOf(TIEEE80211Rate));
     end;
    
    {Copy HT Capabilities}
    System.Move(RT2X00.Specifications.HTCapabilities,RT2X00.WiFi.Bands[IEEE80211_BAND_5GHZ].HTCapabilities,SizeOf(TIEEE80211StationHTCap));
   end;
  
  Result:=ERROR_SUCCESS;
 finally
  {Free Rates and Channels}
  FreeMem(Rates);
  FreeMem(Channels);
 end; 
end;
 
{==============================================================================}
 
function RT2X00LoadFirmware(RT2X00:PRT2X00WiFiDevice):LongWord;
var
 Name:String;
 Data:Pointer;
 Size:LongWord;
 Handle:THandle; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Load firmware');
 {$ENDIF}

 {Check State}
 Result:=ERROR_ALREADY_OPEN;
 if RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_OPEN then Exit;
 
 {Check Requirements}
 if not RT2X00GetRequirement(RT2X00,RT2X00_REQUIRE_FIRMWARE) then
  begin
   {$IFDEF RT2800USB_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Device does not require firmware loading');
   {$ENDIF}
   Result:=ERROR_SUCCESS;
   Exit;
  end;
  
 {Get Firmware} 
 if Assigned(RT2X00.GetFirmware) then
  begin
   if not RT2X00.GetFirmware(RT2X00,Name,Data,Size) then
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Failed to get device firmware');
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
    
   {Check Data} 
   if (Data = nil) or (Size = 0) then
    begin
     {Check Name}
     if Name <> '' then
      begin
       {Open File}
       Handle:=FileOpen(Name,fmOpenRead or fmShareDenyNone);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         try
          {Get Size}
          Size:=FileSeek(Handle,0,fsFromEnd);
          FileSeek(Handle,0,fsFromBeginning);
          if Size > 0 then
           begin
            {Allocate Data}
            Data:=GetMem(Size);
            if Data <> nil then
             begin
              {Read Data}
              if FileRead(Handle,Data^,Size) <> Size then
               begin
                {Free Data}
                FreeMem(Data);
                Data:=nil;
               end;
             end;
           end;
         finally
          FileClose(Handle);
         end;
        end;
      end;
      
     {Check Data}
     if (Data = nil) or (Size = 0) then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Invalid firmware filename, data or size');
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end;      
    end;
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
   Exit;
  end;  
 
 {Check Firmware}
 if Assigned(RT2X00.CheckFirmware) then
  begin
   if not RT2X00.CheckFirmware(RT2X00,Data,Size) then
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Failed to check device firmware');
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end; 
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
   Exit;
  end;  

 {Load Firmware}
 if Assigned(RT2X00.LoadFirmware) then
  begin
   if not RT2X00.LoadFirmware(RT2X00,Data,Size) then
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Failed to load device firmware');
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end; 
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
   Exit;
  end;  
 
 //To Do //free Data if allocated above //Maybe GetFirmware should allocate even if returning internal firmware ?
                                        //Then free always
 
 {When the firmware is uploaded to the hardware the LED association status might have been triggered, for correct LED handling it should now be reset}
 RT2X00AssocLED(RT2X00,False);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RT2X00EnableRX(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2x00queue_start_queue}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Enable RX');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Enable RX}
 if Assigned(RT2X00.EnableRX) then
  begin
   Result:=RT2X00.EnableRX(RT2X00);
  end 
 else 
  begin
   Result:=ERROR_NOT_ASSIGNED;
  end;
end;
 
{==============================================================================}

function RT2X00DisableRX(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2x00queue_stop_queue}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Disable RX');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Disable RX}
 if Assigned(RT2X00.DisableRX) then
  begin
   Result:=RT2X00.DisableRX(RT2X00);
  end 
 else 
  begin
   Result:=ERROR_NOT_ASSIGNED;
  end;
end;

{==============================================================================}

function RT2X00EnableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2x00queue_start_queue}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Enable beacon');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Enable Beacon}
 if Assigned(RT2X00.EnableBeacon) then
  begin
   Result:=RT2X00.EnableBeacon(RT2X00);
  end 
 else 
  begin
   Result:=ERROR_NOT_ASSIGNED;
  end;
end;

{==============================================================================}

function RT2X00DisableBeacon(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2x00queue_stop_queue}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Disable beacon');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Disable Beacon}
 if Assigned(RT2X00.DisableBeacon) then
  begin
   Result:=RT2X00.DisableBeacon(RT2X00);
  end 
 else 
  begin
   Result:=ERROR_NOT_ASSIGNED;
  end;
end;

{==============================================================================}
 
function RT2X00EnableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Enable radio');
 {$ENDIF}

 {Check State}
 Result:=ERROR_ALREADY_OPEN;
 if RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_OPEN then Exit;
 
 //To Do //Init/Start queues ? //DeviceOpen ?
 
 {Enable Radio}
 if Assigned(RT2X00.SetState) then
  begin
   Status:=RT2X00.SetState(RT2X00,RT2X00_STATE_RADIO_ON);
   if Status <> ERROR_SUCCESS then
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Failed to enable radio');
     Result:=Status;
     Exit;
    end;
    
   Status:=RT2X00.SetState(RT2X00,RT2X00_STATE_RADIO_IRQ_ON); 
   if Status <> ERROR_SUCCESS then
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Failed to enable IRQ');
     Result:=Status;
     Exit;
    end;
  end 
 else 
  begin
   Result:=ERROR_NOT_ASSIGNED;
   Exit;
  end;
 
 {Enable Radio and Activity LEDs} 
 RT2X00RadioLED(RT2X00,True);
 RT2X00ActivityLED(RT2X00,True);
 
 //To Do //rt2x00lib_enable_radio
 
 //To Do //Start Watchdog ? //DeviceOpen ?
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RT2X00DisableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Disable radio');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState = NETWORK_STATE_CLOSED then Exit;
 
 //To Do //Stop queues ? //DeviceClose ?
 
 {Disable Radio}
 //To Do //rt2x00lib_disable_radio
 
 {Disable Radio and Activity LEDs} 
 RT2X00ActivityLED(RT2X00,False);
 RT2X00RadioLED(RT2X00,False);
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}
 
function RT2X00SetLED(RT2X00:PRT2X00WiFiDevice;ID:LongWord;Enabled:Boolean):LongWord;
var
 Level:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Set LED (ID=' + IntToStr(ID) + ')');
 {$ENDIF}

 {Get Level}
 if Enabled then Level:=WIFI_LED_FULL else Level:=WIFI_LED_OFF;
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00:  (Level=' + IntToStr(Level) + ')');
 {$ENDIF}
 
 {Check ID}
 case ID of
  RT2X00_LED_TYPE_RADIO:begin
    if not RT2X00.LEDRadio then
     begin
      Result:=ERROR_NOT_SUPPORTED;
      Exit;
     end;
   end;
  RT2X00_LED_TYPE_ASSOC:begin
    if not RT2X00.LEDAssoc then
     begin
      Result:=ERROR_NOT_SUPPORTED;
      Exit;
     end;
   end;
  RT2X00_LED_TYPE_ACTIVITY:begin
    if not RT2X00.LEDActivity then
     begin
      Result:=ERROR_NOT_SUPPORTED;
      Exit;
     end;
   end;
 end;
 
 if Assigned(RT2X00.SetLED) then
  begin
   Result:=RT2X00.SetLED(RT2X00,ID,Level);
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
  end;  
end;

{==============================================================================}

function RT2X00RadioLED(RT2X00:PRT2X00WiFiDevice;Enabled:Boolean):LongWord;
begin
 {}
 Result:=RT2X00SetLED(RT2X00,RT2X00_LED_TYPE_RADIO,Enabled);
end;

{==============================================================================}

function RT2X00AssocLED(RT2X00:PRT2X00WiFiDevice;Enabled:Boolean):LongWord;
begin
 {}
 Result:=RT2X00SetLED(RT2X00,RT2X00_LED_TYPE_ASSOC,Enabled);
end;

{==============================================================================}

function RT2X00ActivityLED(RT2X00:PRT2X00WiFiDevice;Enabled:Boolean):LongWord;
begin
 {}
 Result:=RT2X00SetLED(RT2X00,RT2X00_LED_TYPE_ACTIVITY,Enabled);
end;

{==============================================================================}

function RT2X00QualityLED(RT2X00:PRT2X00WiFiDevice;RSSI:LongInt):LongWord;
var
 Level:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Quality LED RSSI=' + IntToStr(RSSI));
 {$ENDIF}

 if not RT2X00.LEDQuality then
  begin
   Result:=ERROR_NOT_SUPPORTED;
   Exit;
  end;
 
 if Assigned(RT2X00.SetLED) then
  begin
   {LED handling requires a positive value for the RSSI, to do that correctly we need to add the correction}
   RSSI:=RSSI + RT2X00.RSSIOffset;
   
   {Get the RSSI level, this is used to convert the RSSI to a LED value inside the range LED_OFF - LED_FULL}
   if RSSI <= 30 then
    begin
     RSSI:=0;
    end 
   else if RSSI <= 39 then
    begin
     RSSI:=1;
    end 
   else if RSSI <= 49 then
    begin
     RSSI:=2; 
    end 
   else if RSSI <= 53 then
    begin
     RSSI:=3;
    end 
   else if RSSI <= 63 then
    begin
     RSSI:=4;
    end 
   else
    begin
     RSSI:=5;
    end; 

   {Must not send LED_OFF since the driver is going to calculate the value and might use it in a division}
   Level:=((WIFI_LED_FULL div 6) * RSSI) + 1;
  
   Result:=RT2X00.SetLED(RT2X00,RT2X00_LED_TYPE_QUALITY,Level);
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
  end;  
end; 

{==============================================================================}

procedure RT2X00InitializeRate(Entry:PIEEE80211Rate;Index:Word;Rate:PRT2X00Rate);
begin
 {}
 if Entry = nil then Exit;
 if Rate = nil then Exit;
 
 Entry.Flags:=0;
 Entry.BitRate:=Rate.BitRate;
 Entry.HardwareRate:=Index;
 Entry.HardwareRateShort:=Index;
 
 if (Rate.Flags and RT2X00_DEV_RATE_SHORT_PREAMBLE) <> 0 then
  begin
   Entry.Flags:=Entry.Flags or IEEE80211_RATE_SHORT_PREAMBLE;
  end;
end;

{==============================================================================}

procedure RT2X00InitializeChannel(Entry:PIEEE80211Channel;Channel,TXPower,Value:LongWord);
begin
 {}
 if Entry = nil then Exit;
 
 {This assumption about the band is wrong for 802.11j}
 if Channel <= 14 then Entry.Band:=IEEE80211_BAND_2GHZ else Entry.Band:=IEEE80211_BAND_5GHZ;
 Entry.CenterFrequency:=IEEE80211ChannelToFrequency(Channel,Entry.Band);
 Entry.HardwareChannel:=Value;
 Entry.MaxPower:=TXPower;
 Entry.MaxAntennaGain:=$ff;
end;

{==============================================================================}

function RT2X00GetHTCenterChannel(RT2X00:PRT2X00WiFiDevice):Word;
var
 Count:LongWord;
 CenterChannel:LongWord;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {Initialize center channel to current channel}
 CenterChannel:=RT2X00.Specifications.RFChannels[RT2X00.WiFi.Configuration.ChannelDefinition.Channel.HardwareChannel].Channel;
 
 {Adjust center channel to HT40+ and HT40- operation}
 if WiFiConfigurationIsHT40Plus(@RT2X00.WiFi.Configuration) then
  begin
   Inc(CenterChannel,2);
  end
 else if WiFiConfigurationIsHT40Minus(@RT2X00.WiFi.Configuration) then 
  begin
   if CenterChannel = 14 then Dec(CenterChannel,1) else Dec(CenterChannel,2);
  end;
 
 {Find center channel in specifications}
 for Count:=0 to RT2X00.Specifications.ChannelCount - 1 do
  begin
   if RT2X00.Specifications.RFChannels[Count].Channel = CenterChannel then
    begin
     Result:=Count;
     Exit;
    end;
  end;
 
 {Default to current channel} 
 Result:=RT2X00.WiFi.Configuration.ChannelDefinition.Channel.HardwareChannel;
end;

{==============================================================================}

function RT2X00Configure(RT2X00:PRT2X00WiFiDevice;Flags:LongWord):LongWord;
{rt2x00lib_config}
var
 HardwareChannel:Word;
 Configuration:TRT2X00ChannelConfiguration;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Configure (Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Setup Configuration}
 FillChar(Configuration,SizeOf(TRT2X00ChannelConfiguration),0);
 
 {Check Flags}
 if (Flags and IEEE80211_CONF_CHANGE_CHANNEL) <> 0 then
  begin
   {Check HT}
   if not WiFiConfigurationIsHT(@RT2X00.WiFi.Configuration) then
    begin
     RT2X00.Flags:=RT2X00.Flags or RT2X00_CONFIG_HT_DISABLED;
    end
   else
    begin
     RT2X00.Flags:=RT2X00.Flags and not(RT2X00_CONFIG_HT_DISABLED);
    end;
    
   {Check HT40}
   if WiFiConfigurationIsHT40(@RT2X00.WiFi.Configuration) then
    begin
     RT2X00.Flags:=RT2X00.Flags or RT2X00_CONFIG_CHANNEL_HT40;
     HardwareChannel:=RT2X00GetHTCenterChannel(RT2X00);
    end
   else
    begin
     RT2X00.Flags:=RT2X00.Flags and not(RT2X00_CONFIG_CHANNEL_HT40);
     HardwareChannel:=RT2X00.WiFi.Configuration.ChannelDefinition.Channel.HardwareChannel;
    end;    
    
   {$IFDEF RT2800USB_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: (Hardware Channel=' + IntToStr(HardwareChannel) + ')');
   {$ENDIF}
    
   {Copy RF Channel} 
   System.Move(RT2X00.Specifications.RFChannels[HardwareChannel],Configuration.RFChannel,SizeOf(TRT2X00RFChannel));
   
   {Copy Channel}
   System.Move(RT2X00.Specifications.Channels[HardwareChannel],Configuration.Channel,SizeOf(TRT2X00Channel));
   
   {Store RF Channel for VCO periodic calibration}
   RT2X00.RFChannel:=Configuration.RFChannel.Channel;
  end;
 
 {Check Flags}
 if ((Flags and IEEE80211_CONF_CHANGE_PS) <> 0) and (RT2X00GetRequirement(RT2X00,RT2X00_REQUIRE_PS_AUTOWAKE)) then
  begin
   //To Do //rt2x00lib_config
  end;
  
 {Start configuration}
 if Assigned(RT2X00.Configure) then
  begin
   Result:=RT2X00.Configure(RT2X00,@Configuration,Flags);
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
   Exit;
  end;  
 
 {Some configuration changes affect the link quality which means we need to reset the link tuner}
 if (Flags and IEEE80211_CONF_CHANGE_CHANNEL) <> 0 then
  begin
   //To Do //rt2x00link_reset_tuner(rt2x00dev, false); (Plus others rt2x00link_start_tuner etc)
  end;
  
 if ((Flags and IEEE80211_CONF_CHANGE_PS) <> 0) and ((RT2X00.WiFi.Configuration.Flags and IEEE80211_CONF_PS) <> 0) and (RT2X00GetRequirement(RT2X00,RT2X00_REQUIRE_PS_AUTOWAKE)) then
  begin
   //To Do //rt2x00lib_config
  end;
  
 if (RT2X00.WiFi.Configuration.Flags and IEEE80211_CONF_PS) <> 0 then
  begin
   RT2X00.Flags:=RT2X00.Flags or RT2X00_CONFIG_POWERSAVING;
  end
 else
  begin
   RT2X00.Flags:=RT2X00.Flags and not(RT2X00_CONFIG_POWERSAVING);
  end;
  
 RT2X00.CurrentBand:=RT2X00.WiFi.Configuration.ChannelDefinition.Channel.Band;
 RT2X00.CurrentFrequency:=RT2X00.WiFi.Configuration.ChannelDefinition.Channel.CenterFrequency;
 RT2X00.TXPower:=RT2X00.WiFi.Configuration.PowerLevel;
 RT2X00.ShortRetry:=RT2X00.WiFi.Configuration.ShortFrameMaxTXCount;
 RT2X00.LongRetry:=RT2X00.WiFi.Configuration.LongFrameMaxTXCount;

 Result:=ERROR_SUCCESS; 
end;
 
{==============================================================================}
 
function RT2X00ConfigureAntenna(RT2X00:PRT2X00WiFiDevice;Antenna:PRT2X00Antenna):LongWord;
{rt2x00lib_config_antenna}
var
 ActiveAntenna:PRT2X00Antenna;
 DefaultAntenna:PRT2X00Antenna;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Configure Antenna');
 {$ENDIF}
 
 {Check Antenna}
 if Antenna = nil then Exit;
 
 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 ActiveAntenna:=@RT2X00.Link.Antenna.ActiveAntenna;
 DefaultAntenna:=@RT2X00.Antenna;
 
 {When the caller tries to send the SW diversity, we must update the ANTENNA_RX_DIVERSITY flag to enable the antenna diversity in the link tuner}
 {Secondly, we must guarentee we never send the software antenna diversity command to the driver}
 if (RT2X00.Link.Antenna.Flags and RT2X00_ANTENNA_RX_DIVERSITY) = 0 then
  begin
   if Antenna.RX = RT2X00_ANTENNA_SW_DIVERSITY then
    begin
     RT2X00.Link.Antenna.Flags:=RT2X00.Link.Antenna.Flags or RT2X00_ANTENNA_RX_DIVERSITY;
     
     if DefaultAntenna.RX = RT2X00_ANTENNA_SW_DIVERSITY then
      begin
       Antenna.RX:=RT2X00_ANTENNA_B;
      end
     else
      begin
       Antenna.RX:=DefaultAntenna.RX;
      end;      
    end;
  end
 else if Antenna.RX = RT2X00_ANTENNA_SW_DIVERSITY then
  begin
   Antenna.RX:=ActiveAntenna.RX;
  end;  
 
 if (RT2X00.Link.Antenna.Flags and RT2X00_ANTENNA_TX_DIVERSITY) = 0 then
  begin
   if Antenna.TX = RT2X00_ANTENNA_SW_DIVERSITY then
    begin
     RT2X00.Link.Antenna.Flags:=RT2X00.Link.Antenna.Flags or RT2X00_ANTENNA_TX_DIVERSITY;
     
     if DefaultAntenna.TX = RT2X00_ANTENNA_SW_DIVERSITY then
      begin
       Antenna.TX:=RT2X00_ANTENNA_B;
      end
     else
      begin
       Antenna.TX:=DefaultAntenna.TX;
      end;      
    end;
  
  end
 else if Antenna.TX = RT2X00_ANTENNA_SW_DIVERSITY then
  begin
   Antenna.TX:=ActiveAntenna.TX;
  end;
  
 {Antenna setup changes require the RX to be disabled, else the changes will be ignored by the device}
 RT2X00DisableRX(RT2X00);
 
 {Write new antenna setup to the device}
 if Assigned(RT2X00.ConfigureAntenna) then
  begin
   Result:=RT2X00.ConfigureAntenna(RT2X00,Antenna);
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
   Exit;
  end;  
 
 {Reset the link tuner to recalibrate the noise-sensitivity ratio for the new setup}
 //To Do //rt2x00link_reset_tuner(rt2x00dev, true);
 
 {Enable RX}
 RT2X00EnableRX(RT2X00);
end;
 
{==============================================================================}

function RT2X00ConfigureInterface(RT2X00:PRT2X00WiFiDevice;InterfaceType:LongWord;Address,BSSID:PHardwareAddress):LongWord;
{rt2x00lib_config_intf}
var
 Flags:LongWord;
 Configuration:TRT2X00InterfaceConfiguration;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00: Configure interface');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00:  (Type=' + IntToStr(InterfaceType) + ')');
 if Address <> nil then if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00:  (Address=' + HardwareAddressToString(Address^) + ')');
 if BSSID <> nil then if NETWORK_LOG_ENABLED then NetworkLogDebug(PNetworkDevice(@RT2X00.WiFi.Network),'RT2X00:  (BSSID=' + HardwareAddressToString(BSSID^) + ')');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_OPEN;
 if RT2X00.WiFi.Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Setup Configuration}
 FillChar(Configuration,SizeOf(TRT2X00InterfaceConfiguration),0);
 Configuration.InterfaceType:=InterfaceType;
 
 {Check Type}
 case InterfaceType of
  WIFI_IFTYPE_ADHOC:begin
    Configuration.TSFSync:=RT2X00_TSF_SYNC_ADHOC;
   end;
  WIFI_IFTYPE_AP,WIFI_IFTYPE_MESH_POINT,WIFI_IFTYPE_WDS:begin
    Configuration.TSFSync:=RT2X00_TSF_SYNC_AP_NONE;
   end;
  WIFI_IFTYPE_STATION:begin
    Configuration.TSFSync:=RT2X00_TSF_SYNC_INFRA;
   end;
  else
   begin
    Configuration.TSFSync:=RT2X00_TSF_SYNC_NONE;
   end;   
 end;
 
 {When the address is nil we send 00:00:00:00:00:00 to the device to clear the address. This will prevent the device being confused when it wants to ACK frames or considers itself associated}
 if Address <> nil then
  begin
   System.Move(Address^,Configuration.MAC,SizeOf(THardwareAddress));
  end;
 
 if BSSID <> nil then
  begin
   System.Move(BSSID^,Configuration.BSSID,SizeOf(THardwareAddress));
  end;  
  
 Flags:=RT2X00_CONFIG_UPDATE_TYPE;
 if (Address <> nil) or ((RT2X00.APInterfaceCount = 0) and (RT2X00.STAInterfaceCount = 0)) then Flags:=Flags or RT2X00_CONFIG_UPDATE_MAC; 
 if (BSSID <> nil) or ((RT2X00.APInterfaceCount = 0) and (RT2X00.STAInterfaceCount = 0)) then Flags:=Flags or RT2X00_CONFIG_UPDATE_BSSID;
 
 if Assigned(RT2X00.ConfigureInterface) then
  begin
   Result:=RT2X00.ConfigureInterface(RT2X00,@Configuration,Flags);
  end
 else
  begin
   Result:=ERROR_NOT_ASSIGNED;
  end;  
end;

{==============================================================================}
{==============================================================================}
{RT2X00LIB Helper Functions}
function RT2X00IsPCI(RT2X00:PRT2X00WiFiDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.WiFi.Network.Device.DeviceBus = DEVICE_BUS_PCI);
end;

{==============================================================================}

function RT2X00IsPCIe(RT2X00:PRT2X00WiFiDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.WiFi.Network.Device.DeviceBus = DEVICE_BUS_PCIE);
end;

{==============================================================================}

function RT2X00IsUSB(RT2X00:PRT2X00WiFiDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.WiFi.Network.Device.DeviceBus = DEVICE_BUS_USB);
end;

{==============================================================================}

function RT2X00IsMMIO(RT2X00:PRT2X00WiFiDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.WiFi.Network.Device.DeviceBus = DEVICE_BUS_MMIO);
end;
  
{==============================================================================}
  
function RT2X00GetRTChip(RT2X00:PRT2X00WiFiDevice):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=RT2X00.Chipset.RTChip;
end;

{==============================================================================}

procedure RT2X00SetRTChip(RT2X00:PRT2X00WiFiDevice;RTChip:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Chipset.RTChip:=RTChip;
end;

{==============================================================================}

function RT2X00GetRFChip(RT2X00:PRT2X00WiFiDevice):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=RT2X00.Chipset.RFChip;
end;

{==============================================================================}

procedure RT2X00SetRFChip(RT2X00:PRT2X00WiFiDevice;RFChip:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Chipset.RFChip:=RFChip;
end;

{==============================================================================}

function RT2X00GetRevision(RT2X00:PRT2X00WiFiDevice):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=RT2X00.Chipset.Revision;
end;

{==============================================================================}

procedure RT2X00SetRevision(RT2X00:PRT2X00WiFiDevice;Revision:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Chipset.Revision:=Revision;
end;

{==============================================================================}

function RT2X00IsRTChipRevision(RT2X00:PRT2X00WiFiDevice;RTChip:Word;Revision:Word):Boolean;
begin
 {}
 Result:=(RT2X00GetRTChip(RT2X00) = RTChip) and (RT2X00GetRevision(RT2X00) = Revision);
end;

{==============================================================================}

function RT2X00IsRTChipRevisionLT(RT2X00:PRT2X00WiFiDevice;RTChip:Word;Revision:Word):Boolean;
begin
 {}
 Result:=(RT2X00GetRTChip(RT2X00) = RTChip) and (RT2X00GetRevision(RT2X00) < Revision);
end;

{==============================================================================}

function RT2X00IsRTChipRevisionGTE(RT2X00:PRT2X00WiFiDevice;RTChip:Word;Revision:Word):Boolean;
{rt2x00_rt_rev_gte}
begin
 {}
 Result:=(RT2X00GetRTChip(RT2X00) = RTChip) and (RT2X00GetRevision(RT2X00) >= Revision);
end;

{==============================================================================}

function RT2X00GetCapability(RT2X00:PRT2X00WiFiDevice;Capability:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=(RT2X00.Capabilities and Capability) <> 0;
end;

{==============================================================================}

procedure RT2X00SetCapability(RT2X00:PRT2X00WiFiDevice;Capability:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Capabilities:=(RT2X00.Capabilities or Capability);
end;

{==============================================================================}

procedure RT2X00ClearCapability(RT2X00:PRT2X00WiFiDevice;Capability:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Capabilities:=(RT2X00.Capabilities and not(Capability));
end;

{==============================================================================}

function RT2X00GetRequirement(RT2X00:PRT2X00WiFiDevice;Requirement:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=(RT2X00.Requirements and Requirement) <> 0;
end;

{==============================================================================}

procedure RT2X00SetRequirement(RT2X00:PRT2X00WiFiDevice;Requirement:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Requirements:=(RT2X00.Requirements or Requirement);
end;

{==============================================================================}

procedure RT2X00ClearRequirement(RT2X00:PRT2X00WiFiDevice;Requirement:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Requirements:=(RT2X00.Requirements and not(Requirement));
end;

{==============================================================================}

function RT2X00RFRead(RT2X00:PRT2X00WiFiDevice;Index:LongWord;Data:PLongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {Check Data}
 if Data = nil then Exit;
 
 {Check Index}
 if (Index < 1) or (Index > (RT2X00.RFSize div SizeOf(LongWord))) then Exit;
 
 Data^:=RT2X00.RFData[Index - 1];
 
 Result:=True;
end; 
 
{==============================================================================}

function RT2X00RFWrite(RT2X00:PRT2X00WiFiDevice;Index,Data:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {Check Index}
 if (Index < 1) or (Index > (RT2X00.RFSize div SizeOf(LongWord))) then Exit;

 RT2X00.RFData[Index - 1]:=Data;
 
 Result:=True;
end; 
 
{==============================================================================}

function RT2X00GetEeprom8(RT2X00:PRT2X00WiFiDevice;Offset:Word):Byte;
{rt2x00_eeprom_byte}
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=PByte(PtrUInt(RT2X00.EepromData) + Offset)^;
end;

{==============================================================================}

function RT2X00GetEeprom16(RT2X00:PRT2X00WiFiDevice;Offset:Word):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=WordLEtoN(RT2X00.EepromData[Offset]);
end;

{==============================================================================}

procedure RT2X00SetEeprom16(RT2X00:PRT2X00WiFiDevice;Offset,Value:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.EepromData[Offset]:=WordNtoLE(Value);
end;

{==============================================================================}

function RT2X00GetEepromAddress(RT2X00:PRT2X00WiFiDevice;Offset:Word):Pointer;
{Return a pointer to the EEPROM value at Offset}
begin
 {}
 Result:=nil;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=@RT2X00.EepromData[Offset];
end;
  
{==============================================================================}
  
function RT2X00GetRegister8(Reg,Mask,Shift:Byte):Byte; inline;
{Shift is the number of bits to shift the result right (SHR)}
begin
 {}
 {Get}
 Result:=(Reg and Mask) shr Shift;
end;

{==============================================================================}

procedure RT2X00SetRegister8(var Reg:Byte;Mask,Shift,Value:Byte); inline;
{Shift is the number of bits to shift the value left (SHL)}
begin
 {}
 {Mask}
 Reg:=Reg and not(Mask);
 {Set}
 Reg:=Reg or ((Value shl Shift) and Mask);
end;

{==============================================================================}

function RT2X00GetRegister16(Reg,Mask,Shift:Word):Word; inline;
{Shift is the number of bits to shift the result right (SHR)}
begin
 {}
 {Get}
 Result:=(Reg and Mask) shr Shift;
end;

{==============================================================================}

procedure RT2X00SetRegister16(var Reg:Word;Mask,Shift,Value:Word); inline;
{Shift is the number of bits to shift the value left (SHL)}
begin
 {}
 {Mask}
 Reg:=Reg and not(Mask);
 {Set}
 Reg:=Reg or ((Value shl Shift) and Mask);
end;

{==============================================================================}

function RT2X00GetRegister32(Reg,Mask,Shift:LongWord):LongWord; inline;
{Shift is the number of bits to shift the result right (SHR)}
begin
 {}
 {Get}
 Result:=(Reg and Mask) shr Shift;
end;

{==============================================================================}

procedure RT2X00SetRegister32(var Reg:LongWord;Mask,Shift,Value:LongWord); {inline;} {Don't inline, can cause a compiler fault}
{Shift is the number of bits to shift the value left (SHL)}
begin
 {}
 {Mask}
 Reg:=Reg and not(Mask);
 {Set}
 Reg:=Reg or ((Value shl Shift) and Mask);
end;

{==============================================================================}

function RT2X00ReadDescriptor(Descriptor:PLongWord;Index:Byte):LongWord; inline;
var
 Temp:LongWord;
begin
 {}
 Result:=0;
 
 if Descriptor = nil then Exit;
 
 Temp:=Descriptor[Index];
 
 Result:=LongWordLEToN(Temp);
end;

{==============================================================================}

procedure RT2X00WriteDescriptor(Descriptor:PLongWord;Index:Byte;Value:LongWord); inline;
begin
 {}
 if Descriptor = nil then Exit;

 Descriptor[Index]:=LongWordNToLE(Value);
end;

{==============================================================================}
  
function RT2X00GetRate(Value:Word):PRT2X00Rate; inline;
{rt2x00_get_rate}
begin
 {}
 Result:=@RT2X00SupportedRates[Value and $ff];
end;
  
{==============================================================================}
  
function RT2X00GetRateMCS(Value:Word):LongInt; inline;
{rt2x00_get_rate_mcs}
begin
 {}
 Result:=Value and $00ff;
end;

{==============================================================================}
  
function RT2X00GetTXBufferSize(RT2X00:PRT2X00WiFiDevice):LongWord;
{}
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 //To Do //Continuing 
end;

{==============================================================================}

function RT2X00GetTXBufferOffset(RT2X00:PRT2X00WiFiDevice):LongWord;
{}
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 //To Do //Continuing 
 //See RT2X00DriverInit / ExtraTXHeadroom
end;

{==============================================================================}
  
function RT2X00GetRXBufferSize(RT2X00:PRT2X00WiFiDevice):LongWord;
{rt2x00queue_alloc_rxskb}
var
 HeadSize:LongWord;
 TailSize:LongWord;
 FrameSize:LongWord;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 {The frame size includes descriptor size, because the hardware directly receives the frame into the buffer}
 FrameSize:=RT2X00_AGGREGATION_SIZE + RT2X00.RXINFOSize + RT2X00.RXWISize;
 
 {The payload should be aligned to a 4-byte boundary, this means we need at least 3 bytes for moving the frame into the correct offset}
 HeadSize:=4;
 
 {For IV/EIV/ICV assembly we must make sure there is at least 8 bytes bytes available in headroom for IV/EIV and 8 bytes for ICV data as tailroom}
 TailSize:=0;
 if RT2X00GetCapability(RT2X00,RT2X00_CAPABILITY_HW_CRYPTO) then
  begin
   HeadSize:=HeadSize + 8;
   TailSize:=TailSize + 8;
  end;
 
 {Return Result}
 Result:=FrameSize + HeadSize + TailSize;
end;

{==============================================================================}

function RT2X00GetRXBufferOffset(RT2X00:PRT2X00WiFiDevice):LongWord;
{}
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 //To Do //Continuing 
 //See above HeadSize
end;

{==============================================================================}

function RT2X00InsertL2PAD(var Data:Pointer;var Size:LongWord;HeaderLength:LongWord):Boolean;
{rt2x00queue_insert_l2pad}
var
 L2PAD:LongWord;
begin
 {}
 Result:=False;
 
 if Data = nil then Exit;
 if Size = 0 then Exit;
 
 if Size > HeaderLength then L2PAD:=RT2X00_L2PAD_SIZE(HeaderLength) else L2PAD:=0;
 
 if L2PAD > 0 then
  begin
   {Update Data and Size (Add bytes to start of buffer)}
   Dec(Data,L2PAD);
   Inc(Size,L2PAD);
   
   {Move Header Back}
   System.Move(Pointer(Data + L2PAD)^,Data^,HeaderLength);
  end; 
 
 Result:=True;
end;

{==============================================================================}

function RT2X00RemoveL2PAD(var Data:Pointer;var Size:LongWord;HeaderLength:LongWord):Boolean;
{rt2x00queue_remove_l2pad}
var
 L2PAD:LongWord;
begin
 {}
 Result:=False;
 
 if Data = nil then Exit;
 if Size = 0 then Exit;
 
 if Size > HeaderLength then L2PAD:=RT2X00_L2PAD_SIZE(HeaderLength) else L2PAD:=0;
 
 if L2PAD > 0 then
  begin
   {Move Header Forward}
   System.Move(Data^,Pointer(Data + L2PAD)^,HeaderLength);
   
   {Update Data and Size (Remove bytes from start of buffer)}
   Inc(Data,L2PAD);
   Dec(Size,L2PAD);
  end; 
 
 Result:=True;
end;

{==============================================================================}

function RT2X00ReceiveReadSignal(RT2X00:PRT2X00WiFiDevice;Descriptor:PRT2X00RXDescriptor):LongWord;
{rt2x00lib_rxdone_read_signal}
var
 Count:LongWord;
 Signal:LongInt;
 Rate:PRT2X00Rate;
 SignalType:LongWord;
 SupportedBand:PIEEE80211SupportedBand;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {Check Descriptor}
 if Descriptor = nil then Exit;
 
 {Get Signal}
 Signal:=Descriptor.Signal;
 SignalType:=(Descriptor.RXFlags and RT2X00_RXDONE_SIGNAL_MASK);
 
 {Check Rate Mode}
 case Descriptor.RateMode of
  RT2X00_RATE_MODE_CCK,RT2X00_RATE_MODE_OFDM:begin
    {For non-HT rates the MCS value needs to contain the actually used rate modulation (CCK or OFDM)}
    if (Descriptor.RXFlags and RT2X00_RXDONE_SIGNAL_MCS) <> 0 then
     begin
      Signal:=RT2X00_RATE_MCS(Descriptor.RateMode,Signal);
     end;
     
    SupportedBand:=RT2X00.WiFi.Bands[RT2X00.CurrentBand];
    for Count:=0 to SupportedBand.RateCount - 1 do
     begin
      Rate:=RT2X00GetRate(SupportedBand.Rates[Count].HardwareRate);
      
      if ((SignalType = RT2X00_RXDONE_SIGNAL_PLCP) and (Rate.PLCP = Signal)) or ((SignalType = RT2X00_RXDONE_SIGNAL_BITRATE) and (Rate.BitRate = Signal)) or ((SignalType = RT2X00_RXDONE_SIGNAL_MCS) and (Rate.MCS = Signal)) then
       begin
        Result:=Count;
       end;
     end;
   end;
  RT2X00_RATE_MODE_HT_MIX,RT2X00_RATE_MODE_HT_GREENFIELD:begin
    if (Signal >= 0) and (Signal <= 76) then
     begin
      Result:=Signal;
     end;
   end;
 end;
end;

{==============================================================================}

function RT2X00ReceiveInsertIV(Descriptor:PRT2X00RXDescriptor;var Data:Pointer;var Size:LongWord;HeaderLength:LongWord):Boolean;
{rt2x00crypto_rx_insert_iv}
begin
 {}
 Result:=False;
 
 if Descriptor = nil then Exit;
 if Data = nil then Exit;
 if Size = 0 then Exit;
 
 //To Do //Continuing //See rt2x00crypto_rx_insert_iv not used by RT2800USB
 
 Result:=True;
end;

{==============================================================================}
  
function RT2X00_RATE_MCS(Mode,MCS:Word):Word; inline;
{RATE_MCS}
begin
 {}
 Result:=((Mode and $00ff) shl 8) or (MCS and $00ff);
end;
  
{==============================================================================}
  
function RT2X00_L2PAD_SIZE(HeaderLength:LongWord):LongWord; inline;
{L2PAD_SIZE}
begin
 {}
 Result:=(-HeaderLength and 3);
end;
 
{==============================================================================}
{==============================================================================}

end.
 