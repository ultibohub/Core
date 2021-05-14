{
SMSC LAN95xx USB Ethernet Driver.

Copyright (C) 2018 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi - Model A/B/A+/B+
 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  U-Boot - \drivers\usb\eth\smsc95xx.c - Copyright The Chromium OS Authors, NVIDIA Corporation, SMSC
  Linux - \drivers\net\usb\smsc95xx.c - Copyright SMSC
  Embedded XINU - \device\smsc9512\smsc9512.c - Copyright, Douglas Comer and Dennis Brylow
  
References
==========

 9512.pdf (http://ww1.microchip.com/downloads/en/DeviceDoc/9512.pdf)
 9514.pdf (http://ww1.microchip.com/downloads/en/DeviceDoc/9514.pdf)
 
SMSC LAN95xx
============

The SMSC LAN95xx has an integrated USB Hub and is technically a compound
device.  Here we instead use the vendor ID and product ID of the
vendor specific class device attached to the hub that is the device we
actually need to communicate with. The hub will be detected and bound by
the default USB Hub driver when it is enumerated by the USB core.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SMSC95XX; 
  
interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,Network,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {SMSC95XX specific constants}
 SMSC95XX_NETWORK_DESCRIPTION = 'SMSC LAN95XX USB Ethernet Adapter';  {Description of SMSC95XX device}
 
 SMSC95XX_DRIVER_NAME = 'SMSC LAN95XX USB Ethernet Adapter Driver'; {Name of SMSC95XX driver}

 SMSC95XX_DEVICE_ID_COUNT = 5; {Number of supported Device IDs}
 
 SMSC95XX_DEVICE_ID:array[0..SMSC95XX_DEVICE_ID_COUNT - 1] of TUSBDeviceId = (
  (idVendor:$0424;idProduct:$ec00),  {LAN9512/LAN9514 Ethernet}
  (idVendor:$0424;idProduct:$9500),  {LAN9500 Ethernet}
  (idVendor:$0424;idProduct:$9730),  {LAN9730 Ethernet (HSIC)}
  (idVendor:$0424;idProduct:$9900),  {SMSC9500 USB Ethernet Device (SAL10)}
  (idVendor:$0424;idProduct:$9e00)); {LAN9500A Ethernet}
 
 SMSC95XX_INTERNAL_PHY_ID = 1;
 
 SMSC95XX_PHY_ID_MASK = $1F;
 SMSC95XX_REG_INDEX_MASK = $1F;
 
 SMSC95XX_TX_OVERHEAD = 8;
 SMSC95XX_RX_OVERHEAD = 4;
 
 SMSC95XX_HS_USB_PKT_SIZE = 512;
 SMSC95XX_FS_USB_PKT_SIZE = 64;
 
 SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE = (16 * 1024) + (5 * SMSC95XX_HS_USB_PKT_SIZE); 
 SMSC95XX_DEFAULT_FS_BURST_CAP_SIZE = (6 * 1024) + (33 * SMSC95XX_FS_USB_PKT_SIZE); 
 
 SMSC95XX_MAX_SINGLE_PACKET_SIZE = 2048; {Maximum size of a bulk IN receive when burst cap and multiple frames is not in use}
 
 SMSC95XX_DEFAULT_BULK_IN_DELAY = $2000;

 SMSC95XX_RX_MAX_QUEUE_MEMORY = 512 * (ETHERNET_MAX_PACKET_SIZE + SMSC95XX_RX_OVERHEAD); {Originally 60 * 1518} 
 SMSC95XX_TX_MAX_QUEUE_MEMORY = 64 * (ETHERNET_MAX_PACKET_SIZE + SMSC95XX_TX_OVERHEAD); {Originally 60 * 1518} 
 
 {Transmitted Ethernet frames (To the Bulk OUT endpoint) must be prefixed with an 8-byte header containing the "Tx command word A" followed by the "Tx command word B"}
 {TX Command word A} 
 SMSC95XX_TX_COMMAND_A_DATA_OFFSET = $001F0000;
 SMSC95XX_TX_COMMAND_A_FIRST_SEG   = $00002000;
 SMSC95XX_TX_COMMAND_A_LAST_SEG    = $00001000;
 SMSC95XX_TX_COMMAND_A_BUF_SIZE    = $000007FF;
 
 {TX Command word B}
 SMSC95XX_TX_COMMAND_B_CSUM_ENABLE     = $00004000;
 SMSC95XX_TX_COMMAND_B_ADD_CRC_DISABLE = $00002000;
 SMSC95XX_TX_COMMAND_B_DISABLE_PADDING = $00001000;
 SMSC95XX_TX_COMMAND_B_PKT_BYTE_LENGTH = $000007FF;
 
 {Received Ethernet frames (From the Bulk IN endpoint) are prefixed with a 4-byte Rx Status word containing the flags below.  A single USB Bulk IN transfer may
  contain multiple Ethernet frames (provided that HW_CONFIG_MEF is set in HW_CONFIG), each of which is prepended by a Rx Status word and padded to a 4-byte boundary}
 SMSC95XX_RX_STATUS_FF  = $40000000;    {Filter Fail}
 SMSC95XX_RX_STATUS_FL  = $3FFF0000;    {Frame Length} 
 SMSC95XX_RX_STATUS_ES  = $00008000;    {Error Summary} 
 SMSC95XX_RX_STATUS_BF  = $00002000;    {Broadcast Frame} 
 SMSC95XX_RX_STATUS_LE  = $00001000;    {Length Error} 
 SMSC95XX_RX_STATUS_RF  = $00000800;    {Runt Frame} 
 SMSC95XX_RX_STATUS_MF  = $00000400;    {Multicast Frame}
 SMSC95XX_RX_STATUS_TL  = $00000080;    {Frame too long} 
 SMSC95XX_RX_STATUS_CS  = $00000040;    {Collision Seen} 
 SMSC95XX_RX_STATUS_FT  = $00000020;    {Frame Type}
 SMSC95XX_RX_STATUS_RW  = $00000010;    {Receive Watchdog}
 SMSC95XX_RX_STATUS_ME  = $00000008;    {Mii Error}
 SMSC95XX_RX_STATUS_DB  = $00000004;    {Dribbling}
 SMSC95XX_RX_STATUS_CRC = $00000002;    {CRC Error}
 
 {Read/Write Register Definitions} 
 {Device ID / Revision Register} 
 SMSC95XX_ID_REVISION                = $00;
 SMSC95XX_ID_REVISION_CHIP_ID_MASK   = $FFFF0000;
 SMSC95XX_ID_REVISION_CHIP_REV_MASK  = $0000FFFF;
 SMSC95XX_ID_REVISION_CHIP_ID_9500   = $9500;
 SMSC95XX_ID_REVISION_CHIP_ID_9500A  = $9E00;
 SMSC95XX_ID_REVISION_CHIP_ID_9512   = $EC00;
 SMSC95XX_ID_REVISION_CHIP_ID_9530   = $9530;
 SMSC95XX_ID_REVISION_CHIP_ID_89530  = $9E08;
 SMSC95XX_ID_REVISION_CHIP_ID_9730   = $9730;
 
 {Interrupt Status Register} 
 SMSC95XX_INTERRUPT_STATUS            = $08;
 SMSC95XX_INTERRUPT_STATUS_TX_STOP    = $00020000;
 SMSC95XX_INTERRUPT_STATUS_RX_STOP    = $00010000;
 SMSC95XX_INTERRUPT_STATUS_PHY_INT    = $00008000;
 SMSC95XX_INTERRUPT_STATUS_TXE        = $00004000;
 SMSC95XX_INTERRUPT_STATUS_TDFU       = $00002000;
 SMSC95XX_INTERRUPT_STATUS_TDFO       = $00001000;
 SMSC95XX_INTERRUPT_STATUS_RXDF       = $00000800;
 SMSC95XX_INTERRUPT_STATUS_GPIOS      = $000007FF;
 SMSC95XX_INTERRUPT_STATUS_CLEAR_ALL  = $FFFFFFFF;
 
 {Receive Configuration Register}
 SMSC95XX_RX_CONFIG                   = $0C;
 {Most likely, software can write 1 to this flag discard all the Rx packets currently buffered by the device}
 SMSC95XX_RX_CONFIG_FIFO_FLUSH        = $00000001;
 
 {Transmit Configuration Register}
 SMSC95XX_TX_CONFIG                   = $10;
 {Transmit On flag.  Software can write 1 here to enable transmit functionality (at the PHY layer?).  Writing 0 is ignored.  Reads as current
  on (1) / off (0) state.  However, to actually allow packets to be transmitted, software also must set the MAC_CONTROL_TXEN flag in the MAC_CONTROL register}
 SMSC95XX_TX_CONFIG_ON                = $00000004;
 {Transmit Stop flag.  Software can write 1 here to turn transmit functionality off. Writing 0 is ignored. Always reads as 0}
 SMSC95XX_TX_CONFIG_STOP              = $00000002;
 {Most likely, software can write 1 to this flag to discard all the Tx packets currently buffered by the device}
 SMSC95XX_TX_CONFIG_FIFO_FLUSH        = $00000001; 
 
 {Hardware Configuration Register.  This contains a number of flags that software can modify to configure the Ethernet Adapter (Default 0)}
 SMSC95XX_HW_CONFIG                   = $14; 
 {Bulk In Request: When set the device will respond to a Bulk In request with
  a NAK when the RX FIFO is empty instead of a zero length packet} 
 SMSC95XX_HW_CONFIG_BIR               = $00001000;
 SMSC95XX_HW_CONFIG_LEDB              = $00000800;
 {Rx packet offset:  Software can modify this 2-bit field to cause Rx packets
  to be offset by the specified number of bytes.  This is apparently intended
  to allow software to align the IP header on a 4 byte boundary}
 SMSC95XX_HW_CONFIG_RXDOFF            = $00000600;
 SMSC95XX_HW_CONFIG_DRP               = $00000040;
 {Multiple Ethernet Frames:  Software can set this flag in HW_CONFIG to allow
  multiple Ethernet frames to be received in a single USB Bulk In transfer.
  The default value after reset is 0, meaning that the hardware will by default
  provide each received Ethernet frame in a separate USB Bulk In transfer}
 SMSC95XX_HW_CONFIG_MEF               = $00000020;
 {"Lite" Reset flag.  Software can write 1 to this flag in HW_CONFIG to start a
  "lite" reset on the device, whatever that means.  The hardware will
  automatically clear this flag when the device has finished resetting, which
  should take no longer than 1 second}
 SMSC95XX_HW_CONFIG_LRST              = $00000008; 
 SMSC95XX_HW_CONFIG_PSEL              = $00000004;
 {Burst Cap Enable: Enable use of the value in the burst cap register when using
  the device is configured to receive multiple Ethernet frames in a single transfer}
 SMSC95XX_HW_CONFIG_BCE               = $00000002;
 SMSC95XX_HW_CONFIG_SRST              = $00000001;
 
 {RX FIFO Register}
 SMSC95XX_RX_FIFO_INF                 = $18;
 
 {Power Management Control Register}
 SMSC95XX_PM_CONTROL                  = $20;
 SMSC95XX_PM_CONTROL_RES_CLR_WKP_STS  = $00000200;
 SMSC95XX_PM_CONTROL_DEV_RDY          = $00000080;
 SMSC95XX_PM_CONTROL_SUS_MODE         = $00000060;
 SMSC95XX_PM_CONTROL_SUS_MODE_0       = $00000000;
 SMSC95XX_PM_CONTROL_SUS_MODE_1       = $00000020;
 SMSC95XX_PM_CONTROL_SUS_MODE_2       = $00000040;
 SMSC95XX_PM_CONTROL_SUS_MODE_3       = $00000060;
 
 SMSC95XX_PM_CONTROL_PHY_RST          = $00000010; {PHY Reset flag:  Write 1 to start a PHY reset on the device. Automatically cleared when the PHY has reset (less than 1 second)}
 SMSC95XX_PM_CONTROL_WOL_EN           = $00000008;
 SMSC95XX_PM_CONTROL_ED_EN            = $00000004;
 SMSC95XX_PM_CONTROL_WUPS             = $00000003;
 SMSC95XX_PM_CONTROL_WUPS_NO          = $00000000;
 SMSC95XX_PM_CONTROL_WUPS_ED          = $00000001;
 SMSC95XX_PM_CONTROL_WUPS_WOL         = $00000002;
 SMSC95XX_PM_CONTROL_WUPS_MULTI       = $00000003;
 
 {LED General Purpose I/O Configuration Register}
 SMSC95XX_LED_GPIO_CONFIG             = $24;
 SMSC95XX_LED_GPIO_CONFIG_SPD_LED     = $01000000;
 SMSC95XX_LED_GPIO_CONFIG_LNK_LED     = $00100000;
 SMSC95XX_LED_GPIO_CONFIG_FDX_LED     = $00010000;
 
 {General Purpose I/O Configuration Register}
 SMSC95XX_GPIO_CONFIG                 = $28;

 {Advanced Flow Control Configuration Register. (Default 0)}
 SMSC95XX_AFC_CONFIG                  = $2C;
 {Value written to AFC_CFG by the Linux driver, with the following explanation:
     Hi watermark = 15.5Kb (~10 mtu pkts)
     low watermark = 3k (~2 mtu pkts)
     backpressure duration = ~ 350us
     Apply FC on any frame}
 SMSC95XX_AFC_CONFIG_DEFAULT          = $00F830A1;    
 
 {EEPROM Command Register}
 SMSC95XX_E2P_COMMAND                 = $30;
 SMSC95XX_E2P_COMMAND_BUSY            = $80000000;
 SMSC95XX_E2P_COMMAND_MASK            = $70000000;
 SMSC95XX_E2P_COMMAND_READ            = $00000000;
 SMSC95XX_E2P_COMMAND_EWDS            = $10000000;
 SMSC95XX_E2P_COMMAND_EWEN            = $20000000;
 SMSC95XX_E2P_COMMAND_WRITE           = $30000000;
 SMSC95XX_E2P_COMMAND_WRAL            = $40000000;
 SMSC95XX_E2P_COMMAND_ERASE           = $50000000;
 SMSC95XX_E2P_COMMAND_ERAL            = $60000000;
 SMSC95XX_E2P_COMMAND_RELOAD          = $70000000;
 SMSC95XX_E2P_COMMAND_TIMEOUT         = $00000400;
 SMSC95XX_E2P_COMMAND_LOADED          = $00000200;
 SMSC95XX_E2P_COMMAND_ADDR            = $000001FF;
 
 SMSC95XX_MAX_EEPROM_SIZE             = 512;
 
 {EEPROM Data Register}
 SMSC95XX_E2P_DATA                    = $34;
 SMSC95XX_E2P_DATA_MASK               = $000000FF;
 
 {Burst Cap Register}
 {When multiple Ethernet frames per USB bulk transfer are enabled, this
  register must be set by software to specify the maximum number of USB (not
  networking!) packets the hardware will provide in a single Bulk In transfer.
 
  This register is ignored if HW_CONFIG_MEF is not set.  Otherwise, this must be
  set to at least 5, which represents a maximum of 5 * 512 = 2560 bytes of data
  per transfer from the high speed Bulk In endpoint}
 SMSC95XX_BURST_CAP                   = $38;

 {GPIO Wake Register}
 SMSC95XX_GPIO_WAKE                   = $64; 

 {Interrupt Endpoint Register}
 SMSC95XX_INT_EP_CONTROL              = $68;
 SMSC95XX_INT_EP_CONTROL_INTEP        = $80000000;
 SMSC95XX_INT_EP_CONTROL_MACRTO       = $00080000;
 SMSC95XX_INT_EP_CONTROL_TX_STOP      = $00020000;
 SMSC95XX_INT_EP_CONTROL_RX_STOP      = $00010000;
 SMSC95XX_INT_EP_CONTROL_PHY_INT      = $00008000;
 SMSC95XX_INT_EP_CONTROL_TXE          = $00004000;
 SMSC95XX_INT_EP_CONTROL_TDFU         = $00002000;
 SMSC95XX_INT_EP_CONTROL_TDFO         = $00001000;
 SMSC95XX_INT_EP_CONTROL_RXDF         = $00000800;
 SMSC95XX_INT_EP_CONTROL_GPIOS        = $000007FF;
 
 {Bulk In Delay Register}
 {The low 16 bits of this register contain a value that indicates the maximum
  amount of time the hardware waits for additional packets before responding to
  a Bulk In request once a packet has been received.  From experiment, the time
  is specified on a linear scale where each unit is approximately 17
  nanoseconds.  The default value in this register after reset is 0x800 which
  indicates a delay of about 34.8 microseconds, assuming that the scale is
  0-based.  SMSC's Linux driver changes this to 0x2000, or a delay of about 139
  microseconds.
 
  The high 16 bits of this register are ignored, as far as I can tell.
 
  The value in this register no effect if HW_CONFIG_MEF is not set in the
  HW_CONFIG register}
 SMSC95XX_BULK_IN_DELAY                = $6C;
 
 {Media Access Control Control Register}
 SMSC95XX_MAC_CONTROL                  = $100;

 SMSC95XX_MAC_CONTROL_RXALL            = $80000000; {???}
 SMSC95XX_MAC_CONTROL_RCVOWN           = $00800000; {Half duplex mode}
 SMSC95XX_MAC_CONTROL_LOOPBK           = $00200000; {Loopback mode}
 SMSC95XX_MAC_CONTROL_FDPX             = $00100000; {Full duplex mode}
 SMSC95XX_MAC_CONTROL_MCPAS            = $00080000; {Multicast pass: receive all multicast packets}
 SMSC95XX_MAC_CONTROL_PRMS             = $00040000; {Promiscuous mode}
 SMSC95XX_MAC_CONTROL_INVFILT          = $00020000; {Inverse filtering}
 SMSC95XX_MAC_CONTROL_PASSBAD          = $00010000; {Pass on bad frames}
 SMSC95XX_MAC_CONTROL_HFILT            = $00008000; {???}
 SMSC95XX_MAC_CONTROL_HPFILT           = $00002000; {Filter received multicast packets by the set of addresses specified by HASH_HIGH and HASH_LOW}
 SMSC95XX_MAC_CONTROL_LCOLL            = $00001000; {???}
 SMSC95XX_MAC_CONTROL_BCAST            = $00000800; {Receive broadcast packets}
 SMSC95XX_MAC_CONTROL_DISRTY           = $00000400; {???}
 SMSC95XX_MAC_CONTROL_PADSTR           = $00000100; {???}
 SMSC95XX_MAC_CONTROL_BOLMT_MASK       = $000000C0; {???}
 SMSC95XX_MAC_CONTROL_DFCHK            = $00000020; {???}
 SMSC95XX_MAC_CONTROL_TX_ENABLE        = $00000008; {Transmit enabled (MAC layer).  1 to enable / 0 to disable (Must also set the SMSC95XX_TX_CONFIG_ON flag in the SMSC95XX_TX_CONFIG register)}
 SMSC95XX_MAC_CONTROL_RX_ENABLE        = $00000004; {Receive enabled. 1 to enable / 0 to disable}
 
 {Address High Register}
 {This contains the high 2 bytes of the MAC address used by the device, stored in little endian order.
 
  As they are not part of the MAC address, the hardware ignores the values
  written to the upper 2 bytes of this register and always reads them as 0.
 
  Software can change the MAC address used by the device by writing to the
  MAC_ADDRESS_HIGH and MAC_ADDRESS_LOW registers, and it can retrieve the current MAC address by
  reading them.  On reset, the device will read its MAC address from the EEPROM
  if one is attached; otherwise it will set its MAC address to 0xFFFFFFFFFFFF}
 SMSC95XX_MAC_ADDRESS_HIGH             = $104;
 
 {Address Low Register}
 {This contains the low 4 bytes of the MAC address used by the device, stored in little endian order.  See MAC_ADDRESS_HIGH}
 SMSC95XX_MAC_ADDRESS_LOW              = $108;
 
 {Hash High register}
 {Used together with HASH_LOW to filter specific multicast packets}
 SMSC95XX_HASH_HIGH                    = $10C;
 
 {Hash Low register}
 {used together with HASH_HIGH to filter specific multicast packets}
 SMSC95XX_HASH_LOW                     = $110;
 
 {MII Access Register}
 SMSC95XX_MII_ADDR                     = $114;
 SMSC95XX_MII_WRITE                    = $02;
 SMSC95XX_MII_BUSY                     = $01;
 SMSC95XX_MII_READ                     = $00; {~of MII Write bit}

 {MII Data Register}
 SMSC95XX_MII_DATA                     = $118;
 
 {Unknown Register,(Default 0)}
 SMSC95XX_FLOW                         = $11C;
 SMSC95XX_FLOW_FCPT                    = $FFFF0000;
 SMSC95XX_FLOW_FCPASS                  = $00000004;
 SMSC95XX_FLOW_FCEN                    = $00000002;
 SMSC95XX_FLOW_FCBSY                   = $00000001;

 {Unknown Register}
 SMSC95XX_VLAN1                        = $120;

 {Unknown Register}
 SMSC95XX_VLAN2                        = $124;
 
 {Unknown Register}
 SMSC95XX_WUFF                         = $128;
 SMSC95XX_LAN9500_WUFF_NUM             = 4;
 SMSC95XX_LAN9500A_WUFF_NUM            = 8;
 
 {Wakeup Control and Status Register}
 SMSC95XX_WUCSR                        = $12C;
 SMSC95XX_WUCSR_WFF_PTR_RST            = $80000000;
 SMSC95XX_WUCSR_GUE                    = $00000200;
 SMSC95XX_WUCSR_WUFR                   = $00000040;
 SMSC95XX_WUCSR_MPR                    = $00000020;
 SMSC95XX_WUCSR_WAKE_EN                = $00000004;
 SMSC95XX_WUCSR_MPEN                   = $00000002;
 
 {Checksum Offload Engine/Enable Control Register}
 SMSC95XX_COE_CONTROL                  = $130;
 SMSC95XX_TX_COE_ENABLE                = $00010000; {Transmit checksum offload enabled. 1 to enable / 0 to disable (Default 0)}
 SMSC95XX_RX_COE_MODE                  = $00000002; {Unknown (Default 0)}
 SMSC95XX_RX_COE_ENABLE                = $00000001; {Receive checksum offload enabled. 1 to enable / 0 to disable (Default 0)}
 
 {Vendor-specific PHY Definitions}
 {EDPD NLP / crossover time configuration (LAN9500A only)}
 SMSC95XX_PHY_EDPD_CONFIG               = 16;
 SMSC95XX_PHY_EDPD_CONFIG_TX_NLP_EN     = $8000;
 SMSC95XX_PHY_EDPD_CONFIG_TX_NLP_1000   = $0000;
 SMSC95XX_PHY_EDPD_CONFIG_TX_NLP_768    = $2000;
 SMSC95XX_PHY_EDPD_CONFIG_TX_NLP_512    = $4000;
 SMSC95XX_PHY_EDPD_CONFIG_TX_NLP_256    = $6000;
 SMSC95XX_PHY_EDPD_CONFIG_RX_1_NLP      = $1000;
 SMSC95XX_PHY_EDPD_CONFIG_RX_NLP_64     = $0000;
 SMSC95XX_PHY_EDPD_CONFIG_RX_NLP_256    = $0400;
 SMSC95XX_PHY_EDPD_CONFIG_RX_NLP_512    = $0800;
 SMSC95XX_PHY_EDPD_CONFIG_RX_NLP_1000   = $0C00;
 SMSC95XX_PHY_EDPD_CONFIG_EXT_CROSSOVER = $0001;
 SMSC95XX_PHY_EDPD_CONFIG_DEFAULT       = (SMSC95XX_PHY_EDPD_CONFIG_TX_NLP_EN or SMSC95XX_PHY_EDPD_CONFIG_TX_NLP_768 or SMSC95XX_PHY_EDPD_CONFIG_RX_1_NLP);
 
 {Mode Control/Status Register}
 SMSC95XX_PHY_MODE_CTRL_STS             = 17;
 SMSC95XX_MODE_CTRL_STS_EDPWRDOWN       = $2000;
 SMSC95XX_MODE_CTRL_STS_ENERGYON        = $0002;

 SMSC95XX_SPECIAL_CTRL_STS              = 27;
 SMSC95XX_SPECIAL_CTRL_STS_OVRRD_AMDIX  = $8000;
 SMSC95XX_SPECIAL_CTRL_STS_AMDIX_ENABLE = $4000;
 SMSC95XX_SPECIAL_CTRL_STS_AMDIX_STATE  = $2000;

 SMSC95XX_PHY_INT_SRC                   = 29;
 SMSC95XX_PHY_INT_SRC_ENERGY_ON         = $0080;
 SMSC95XX_PHY_INT_SRC_ANEG_COMP         = $0040;
 SMSC95XX_PHY_INT_SRC_REMOTE_FAULT      = $0020;
 SMSC95XX_PHY_INT_SRC_LINK_DOWN         = $0010;

 SMSC95XX_PHY_INT_MASK                  = 30;
 SMSC95XX_PHY_INT_MASK_ENERGY_ON        = $0080;
 SMSC95XX_PHY_INT_MASK_ANEG_COMP        = $0040;
 SMSC95XX_PHY_INT_MASK_REMOTE_FAULT     = $0020;
 SMSC95XX_PHY_INT_MASK_LINK_DOWN        = $0010;
 SMSC95XX_PHY_INT_MASK_DEFAULT          = (SMSC95XX_PHY_INT_MASK_ANEG_COMP or SMSC95XX_PHY_INT_MASK_LINK_DOWN);
 
 SMSC95XX_PHY_SPECIAL                   = 31;
 SMSC95XX_PHY_SPECIAL_SPD               = $001C;
 SMSC95XX_PHY_SPECIAL_SPD_10HALF        = $0004;
 SMSC95XX_PHY_SPECIAL_SPD_10FULL        = $0014;
 SMSC95XX_PHY_SPECIAL_SPD_100HALF       = $0008;
 SMSC95XX_PHY_SPECIAL_SPD_100FULL       = $0018;
 
 {SMSC LAN95XX USB Vendor Requests}
 {Write Register:  Specify as bRequest of a USB control message to write a register on the SMSC LAN95XX.  bmRequestType must specify a vendor-specific
  request in the host-to-device direction, wIndex must specify the register, and the transfer data must be 4 bytes containing the value to write}
 SMSC95XX_VENDOR_REQUEST_WRITE_REGISTER  = $A0;

 {Read Register:  Specify as bRequest of a USB control message to read a register from the SMSC LAN95XX.  bmRequestType must specify a vendor-specific
  request in the device-to-host direction, wIndex must specify the register, and the transfer data must be a 4 byte location to read the value}
 SMSC95XX_VENDOR_REQUEST_READ_REGISTER   = $A1;
 
 {Get Statistics}
 SMSC95XX_VENDOR_REQUEST_GET_STATS       = $A2;
 
 {Interrupt Endpoint status word bitfields}
 SMSC95XX_INT_ENP_TX_STOP              = $00000001 shl 17; 
 SMSC95XX_INT_ENP_RX_STOP              = $00000001 shl 16; 
 SMSC95XX_INT_ENP_PHY_INT              = $00000001 shl 15; 
 SMSC95XX_INT_ENP_TXE                  = $00000001 shl 14; 
 SMSC95XX_INT_ENP_TDFU                 = $00000001 shl 13; 
 SMSC95XX_INT_ENP_TDFO                 = $00000001 shl 12; 
 SMSC95XX_INT_ENP_RXDF                 = $00000001 shl 11; 
 
{==============================================================================}
type
 {SMSC95XX specific types}
 PSMSC95XXNetwork = ^TSMSC95XXNetwork;
 TSMSC95XXNetwork = record
  {Network Properties}
  Network:TNetworkDevice;
  {Driver Properties}
  ChipID:LongWord;
  ChipRevision:LongWord;
  PHYLock:TMutexHandle;
  ReceiveRequestSize:LongWord;                  {Size of each USB receive request buffer}
  TransmitRequestSize:LongWord;                 {Size of each USB transmit request buffer}
  ReceiveEntryCount:LongWord;                   {Number of entries in the receive queue}
  TransmitEntryCount:LongWord;                  {Number of entries in the transmit queue}
  ReceivePacketCount:LongWord;                  {Maximum number of packets per receive entry}
  TransmitPacketCount:LongWord;                 {Maximum number of packets per transmit entry}
  {USB Properties}
  ReceiveRequest:PUSBRequest;                   {USB request for packet receive data}
  TransmitRequest:PUSBRequest;                  {USB request for packet transmit data}
  InterruptRequest:PUSBRequest;                 {USB request for interrupt data}
  ReceiveEndpoint:PUSBEndpointDescriptor;       {Bulk IN Endpoint}
  TransmitEndpoint:PUSBEndpointDescriptor;      {Bulk OUT Endpoint}
  InterruptEndpoint:PUSBEndpointDescriptor;     {Interrupt IN Endpoint}
  PendingCount:LongWord;                        {Number of USB requests pending for this network}
  WaiterThread:TThreadId;                       {Thread waiting for pending requests to complete (for network close)}
 end; 
 
{==============================================================================}
{var}
 {SMSC95XX specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure SMSC95XXInit;
 
{==============================================================================}
{SMSC95XX Network Functions}
function SMSC95XXNetworkOpen(Network:PNetworkDevice):LongWord;
function SMSC95XXNetworkClose(Network:PNetworkDevice):LongWord;
function SMSC95XXNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function SMSC95XXBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function SMSC95XXBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
function SMSC95XXBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function SMSC95XXBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;

procedure SMSC95XXTransmitStart(Network:PSMSC95XXNetwork);

{==============================================================================}
{SMSC95XX USB Functions}
function SMSC95XXDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function SMSC95XXDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
 
procedure SMSC95XXReceiveWorker(Request:PUSBRequest); 
procedure SMSC95XXReceiveComplete(Request:PUSBRequest); 

procedure SMSC95XXTransmitWorker(Request:PUSBRequest); 
procedure SMSC95XXTransmitComplete(Request:PUSBRequest); 

procedure SMSC95XXInterruptWorker(Request:PUSBRequest);
procedure SMSC95XXInterruptComplete(Request:PUSBRequest);
 
{==============================================================================}
{SMSC95XX Helper Functions}
function SMSC95XXCheckDevice(Device:PUSBDevice):LongWord;

function SMSC95XXReadRegister(Device:PUSBDevice;Index:LongWord;var Data:LongWord):LongWord;
function SMSC95XXWriteRegister(Device:PUSBDevice;Index,Data:LongWord):LongWord;
function SMSC95XXModifyRegister(Device:PUSBDevice;Index,Mask,Value:LongWord):LongWord;

function SMSC95XXSetRegisterBits(Device:PUSBDevice;Index,Value:LongWord):LongWord;
function SMSC95XXClearRegisterBits(Device:PUSBDevice;Index,Value:LongWord):LongWord;

function SMSC95XXPHYRead(Device:PUSBDevice;Index:LongWord;var Value:Word):LongWord;
function SMSC95XXPHYWrite(Device:PUSBDevice;Index:LongWord;Value:Word):LongWord;
function SMSC95XXPHYInitialize(Device:PUSBDevice):LongWord;
function SMSC95XXPHYWaitNotBusy(Device:PUSBDevice):LongWord;

function SMSC95XXWaitEEPROM(Device:PUSBDevice):LongWord;
function SMSC95XXReadEEPROM(Device:PUSBDevice;Offset,Length:LongWord;Data:PByte):LongWord;
function SMSC95XXWriteEEPROM(Device:PUSBDevice;Offset,Length:LongWord;Data:PByte):LongWord;

function SMSC95XXSetMacAddress(Device:PUSBDevice;Address:PHardwareAddress):LongWord;
function SMSC95XXGetMacAddress(Device:PUSBDevice;Address:PHardwareAddress):LongWord;
              
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {SMSC95XX specific variables}
 SMSC95XXInitialized:Boolean; 
 
 SMSC95XXDriver:PUSBDriver;  {SMSC95XX Driver interface (Set by SMSC95XXInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SMSC95XXInit;
{Initialize the SMSC95XX unit, create and register the driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if SMSC95XXInitialized then Exit;

 {Create SMSC95XX Network Driver}
 SMSC95XXDriver:=USBDriverCreate;
 if SMSC95XXDriver <> nil then
  begin
   {Update SMSC95XX Network Driver}
   {Driver}
   SMSC95XXDriver.Driver.DriverName:=SMSC95XX_DRIVER_NAME; 
   {USB}
   SMSC95XXDriver.DriverBind:=SMSC95XXDriverBind;
   SMSC95XXDriver.DriverUnbind:=SMSC95XXDriverUnbind;

   {Register SMSC95XX Network Driver}
   Status:=USBDriverRegister(SMSC95XXDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'SMSC95XX: Failed to register SMSC95XX driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'SMSC95XX: Failed to create SMSC95XX driver');
  end;
 
 SMSC95XXInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{SMSC95XX Network Functions}
function SMSC95XXNetworkOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen for the SMSC95XX device}
{Note: Not intended to be called directly by applications, use NetworkDeviceOpen instead}
var
 Value:Word;
 Current:Int64;
 Status:LongWord;
 Buffer:LongWord;
 Device:PUSBDevice;
 Entry:PNetworkEntry;
 Address:THardwareAddress;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    Result:=ERROR_ALREADY_OPEN;
    if Network.NetworkState <> NETWORK_STATE_CLOSED then Exit;

    {Set Result}
    Result:=ERROR_OPERATION_FAILED;

    {Reset Device}
    Status:=SMSC95XXSetRegisterBits(Device,SMSC95XX_HW_CONFIG,SMSC95XX_HW_CONFIG_LRST);
    if Status <> USB_STATUS_SUCCESS then Exit;
    Current:=GetTickCount64;
    repeat
     MillisecondDelay(1);
     Status:=SMSC95XXReadRegister(Device,SMSC95XX_HW_CONFIG,Buffer);
     if (Status <> USB_STATUS_SUCCESS) or (GetTickCount64 > (Current + MILLISECONDS_PER_SECOND)) then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to reset device: ' + USBStatusToString(Status));
       Exit;
      end;
    until (Buffer and SMSC95XX_HW_CONFIG_LRST) = 0;
    
    {Reset PHY}
    Status:=SMSC95XXSetRegisterBits(Device,SMSC95XX_PM_CONTROL,SMSC95XX_PM_CONTROL_PHY_RST);
    if Status <> USB_STATUS_SUCCESS then Exit;
    Current:=GetTickCount64;
    repeat
     MillisecondDelay(1);
     Status:=SMSC95XXReadRegister(Device,SMSC95XX_PM_CONTROL,Buffer);
     if (Status <> USB_STATUS_SUCCESS) or (GetTickCount64 > (Current + MILLISECONDS_PER_SECOND)) then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to reset PHY: ' + USBStatusToString(Status));
       Exit;
      end;
    until (Buffer and SMSC95XX_PM_CONTROL_PHY_RST) = 0;
    
    {Get MAC Address}
    FillChar(Address,SizeOf(THardwareAddress),0);
    SMSC95XXGetMacAddress(Device,@Address);
    {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Default Address = ' + HardwareAddressToString(Address));
    {$ENDIF}
    
    {Check MAC Address}
    if not ValidHardwareAddress(Address) then
     begin
      {Convert MAC address}
      Address:=StringToHardwareAddress(SMSC95XX_MAC_ADDRESS); 
      {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Hardware Address = ' + HardwareAddressToString(Address));
      {$ENDIF}
      
      {Check MAC Address}
      if not ValidHardwareAddress(Address) then
       begin
        {Random MAC Address}
        Address:=RandomHardwareAddress;
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Random Address = ' + HardwareAddressToString(Address));
        {$ENDIF}
       end;
      
      {Set MAC Address} 
      Status:=SMSC95XXSetMacAddress(Device,@Address);
      if Status <> USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set mac address: ' + USBStatusToString(Status));
        Exit;
       end;
     end; 
    
    {Allow multiple Ethernet frames to be received in a single USB transfer. Also set Bulk In Request and Burst Cap Enable options}
    Status:=SMSC95XXSetRegisterBits(Device,SMSC95XX_HW_CONFIG,SMSC95XX_HW_CONFIG_MEF or SMSC95XX_HW_CONFIG_BIR or SMSC95XX_HW_CONFIG_BCE);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set hardware config: ' + USBStatusToString(Status));
      Exit;
     end;
    
    {Check the Speed}
    if Device.Speed = USB_SPEED_HIGH then 
     begin
      Buffer:=SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE div SMSC95XX_HS_USB_PKT_SIZE;
      
      {Set RX and TX Sizes}
      PSMSC95XXNetwork(Network).ReceiveRequestSize:=SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE;
      PSMSC95XXNetwork(Network).TransmitRequestSize:=ETHERNET_MAX_PACKET_SIZE + SMSC95XX_TX_OVERHEAD;      
      PSMSC95XXNetwork(Network).ReceiveEntryCount:=SMSC95XX_RX_MAX_QUEUE_MEMORY div PSMSC95XXNetwork(Network).ReceiveRequestSize;
      PSMSC95XXNetwork(Network).TransmitEntryCount:=SMSC95XX_TX_MAX_QUEUE_MEMORY div PSMSC95XXNetwork(Network).TransmitRequestSize;
      PSMSC95XXNetwork(Network).ReceivePacketCount:=SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE div (ETHERNET_MIN_PACKET_SIZE + SMSC95XX_RX_OVERHEAD);
      PSMSC95XXNetwork(Network).TransmitPacketCount:=1;
     end
    else
     begin
      Buffer:=SMSC95XX_DEFAULT_FS_BURST_CAP_SIZE div SMSC95XX_FS_USB_PKT_SIZE;
      
      {Set RX and TX Sizes}
      PSMSC95XXNetwork(Network).ReceiveRequestSize:=SMSC95XX_DEFAULT_FS_BURST_CAP_SIZE;
      PSMSC95XXNetwork(Network).TransmitRequestSize:=ETHERNET_MAX_PACKET_SIZE + SMSC95XX_TX_OVERHEAD;      
      PSMSC95XXNetwork(Network).ReceiveEntryCount:=4;
      PSMSC95XXNetwork(Network).TransmitEntryCount:=4;
      PSMSC95XXNetwork(Network).ReceivePacketCount:=SMSC95XX_DEFAULT_FS_BURST_CAP_SIZE div (ETHERNET_MIN_PACKET_SIZE + SMSC95XX_RX_OVERHEAD);
      PSMSC95XXNetwork(Network).TransmitPacketCount:=1;
     end;
    {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if USB_LOG_ENABLED then
     begin
      USBLogDebug(Device,'SMSC95XX: BurstCap=' + IntToStr(Buffer));
      USBLogDebug(Device,'SMSC95XX: ReceiveRequestSize=' + IntToStr(PSMSC95XXNetwork(Network).ReceiveRequestSize));
      USBLogDebug(Device,'SMSC95XX: TransmitRequestSize=' + IntToStr(PSMSC95XXNetwork(Network).TransmitRequestSize));
      USBLogDebug(Device,'SMSC95XX: ReceiveEntryCount=' + IntToStr(PSMSC95XXNetwork(Network).ReceiveEntryCount));
      USBLogDebug(Device,'SMSC95XX: TransmitEntryCount=' + IntToStr(PSMSC95XXNetwork(Network).TransmitEntryCount));
      USBLogDebug(Device,'SMSC95XX: ReceivePacketCount=' + IntToStr(PSMSC95XXNetwork(Network).ReceivePacketCount));
      USBLogDebug(Device,'SMSC95XX: TransmitPacketCount=' + IntToStr(PSMSC95XXNetwork(Network).TransmitPacketCount));
     end; 
    {$ENDIF}
     
    {Set the maximum USB (not network) packets per USB Receive transfer}
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_BURST_CAP,Buffer);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set burst cap size: ' + USBStatusToString(Status));
      Exit;
     end;
    
    {Set the USB Bulk IN delay (How long to delay a bulk IN request once a packet has been received)}
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_BULK_IN_DELAY,SMSC95XX_DEFAULT_BULK_IN_DELAY);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set bulk IN delay: ' + USBStatusToString(Status));
      Exit;
     end;
    
    //To Do //SMSC95XX_HW_CONFIG_RXDOFF to align received frames ?
    
    {Clear Interrupt Status}
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_INTERRUPT_STATUS,SMSC95XX_INTERRUPT_STATUS_CLEAR_ALL);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to clear interrupt status: ' + USBStatusToString(Status));
      Exit;
     end; 
    
    {Get Revision ID}
    Status:=SMSC95XXReadRegister(Device,SMSC95XX_ID_REVISION,Buffer);
    if Status <> USB_STATUS_SUCCESS then Exit;
    
    {Get Chip ID and Revision}
    PSMSC95XXNetwork(Network).ChipID:=(Buffer and SMSC95XX_ID_REVISION_CHIP_ID_MASK) shr 16;
    PSMSC95XXNetwork(Network).ChipRevision:=Buffer and SMSC95XX_ID_REVISION_CHIP_REV_MASK;
    {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: ID=' + IntToHex(PSMSC95XXNetwork(Network).ChipID,4) + ' Revision=' + IntToHex(PSMSC95XXNetwork(Network).ChipRevision,4));
    {$ENDIF}
    
    {Clear the Flow register}
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_FLOW,0);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to clear flow register: ' + USBStatusToString(Status));
      Exit;
     end;
    
    {Set the Advanced flow control register with a default value}
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_AFC_CONFIG,SMSC95XX_AFC_CONFIG_DEFAULT);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set AFC config: ' + USBStatusToString(Status));
      Exit;
     end;
    
    //To Do //smsc95xx_set_multicast
    
    {Initialize PHY (Reset, Interrupts)}
    Status:=SMSC95XXPHYInitialize(Device);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to initialize PHY: ' + USBStatusToString(Status));
      {Exit;} {Do not fail}
     end; 
    
    try 
     {Allocate Receive Queue Buffer}
     Network.ReceiveQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),PSMSC95XXNetwork(Network).ReceiveEntryCount);
     if Network.ReceiveQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMSC95XX: Failed to create receive queue buffer');
       
       Exit;
      end;
     
     {Allocate Receive Queue Semaphore}
     Network.ReceiveQueue.Wait:=SemaphoreCreate(0);
     if Network.ReceiveQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMSC95XX: Failed to create receive queue semaphore');
       
       Exit;
      end;
     
     {Allocate Receive Queue Buffers}
     Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=PSMSC95XXNetwork(Network).ReceiveRequestSize;
       Entry.Offset:=SMSC95XX_RX_OVERHEAD;
       Entry.Count:=0; {PSMSC95XXNetwork(Network).ReceivePacketCount}
       
       {Allocate USB Request Buffer}
       Entry.Buffer:=USBBufferAllocate(Device,Entry.Size);
       if Entry.Buffer = nil then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to allocate USB receive buffer');
         
         Exit;
        end;
       
       {Initialize Packets}
       SetLength(Entry.Packets,PSMSC95XXNetwork(Network).ReceivePacketCount);
       
       {Initialize First Packet}
       Entry.Packets[0].Buffer:=Entry.Buffer;
       Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
       Entry.Packets[0].Length:=Entry.Size - Entry.Offset;
       
       Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
      end;
     
     {Allocate Receive Queue Entries}
     SetLength(Network.ReceiveQueue.Entries,PSMSC95XXNetwork(Network).ReceiveEntryCount);
     
     {Allocate Transmit Queue Buffer}
     Network.TransmitQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),PSMSC95XXNetwork(Network).TransmitEntryCount);
     if Network.TransmitQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to create transmit queue buffer');
       
       Exit;
      end;
     
     {Allocate Transmit Queue Semaphore}
     Network.TransmitQueue.Wait:=SemaphoreCreate(PSMSC95XXNetwork(Network).TransmitEntryCount);
     if Network.TransmitQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to create transmit queue semaphore');
       
       Exit;
      end;

     {Allocate Transmit Queue Buffers}
     Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=PSMSC95XXNetwork(Network).TransmitRequestSize;
       Entry.Offset:=SMSC95XX_TX_OVERHEAD;
       Entry.Count:=PSMSC95XXNetwork(Network).TransmitPacketCount;
       
       {Allocate USB Request Buffer}
       Entry.Buffer:=USBBufferAllocate(Device,Entry.Size);
       if Entry.Buffer = nil then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to allocate USB transmit buffer');
         
         Exit;
        end;
        
       {Initialize Packets}
       SetLength(Entry.Packets,PSMSC95XXNetwork(Network).TransmitPacketCount);
      
       {Initialize First Packet}
       Entry.Packets[0].Buffer:=Entry.Buffer;
       Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
       Entry.Packets[0].Length:=Entry.Size - Entry.Offset;
       
       Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
      end;
      
     {Allocate Transmit Queue Entries}
     SetLength(Network.TransmitQueue.Entries,PSMSC95XXNetwork(Network).TransmitEntryCount);
     
     {Allocate Transmit Request}
     PSMSC95XXNetwork(Network).TransmitRequest:=USBRequestAllocate(Device,PSMSC95XXNetwork(Network).TransmitEndpoint,SMSC95XXTransmitComplete,0,nil);
     if PSMSC95XXNetwork(Network).TransmitRequest = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to allocate transmit request');
       
       Exit;
      end;

     {Allocate Receive Request}
     PSMSC95XXNetwork(Network).ReceiveRequest:=USBRequestAllocate(Device,PSMSC95XXNetwork(Network).ReceiveEndpoint,SMSC95XXReceiveComplete,0,nil);
     if PSMSC95XXNetwork(Network).ReceiveRequest = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to allocate receive request');
       
       Exit;
      end;
      
     {Submit Receive Request}
     {Get Entry}
     Entry:=BufferGet(Network.ReceiveQueue.Buffer);
     if Entry <> nil then
      begin
       {Update Pending}
       Inc(PSMSC95XXNetwork(Network).PendingCount);
       
       {Update Entry}
       Entry.DriverData:=Network;
       
       {Initialize Request}
       USBRequestInitialize(PSMSC95XXNetwork(Network).ReceiveRequest,SMSC95XXReceiveComplete,Entry.Buffer,Entry.Size,Entry);
       
       {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
       if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Submitting receive request');
       {$ENDIF}
       
       {Submit Request}
       Status:=USBRequestSubmit(PSMSC95XXNetwork(Network).ReceiveRequest);
       if Status <> USB_STATUS_SUCCESS then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to submit receive request: ' + USBStatusToString(Status));
         
         {Update Pending}
         Dec(PSMSC95XXNetwork(Network).PendingCount);
         
         {Update Entry}
         Entry.DriverData:=nil;
         
         {Free Entry}
         BufferFree(Entry);
         Exit;
        end; 
      end
     else
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to get receive buffer entry');
       
       Exit;
      end;      
     
     {Allocate Interrupt Request}
     PSMSC95XXNetwork(Network).InterruptRequest:=USBRequestAllocate(Device,PSMSC95XXNetwork(Network).InterruptEndpoint,SMSC95XXInterruptComplete,PSMSC95XXNetwork(Network).InterruptEndpoint.wMaxPacketSize,Network); 
     if PSMSC95XXNetwork(Network).InterruptRequest = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to allocate interrupt request');
        
       Exit;
      end;
      
     {Update Pending}
     Inc(PSMSC95XXNetwork(Network).PendingCount);
      
     {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Submitting interrupt request');
     {$ENDIF}
      
     {Submit Interrupt Request}
     Status:=USBRequestSubmit(PSMSC95XXNetwork(Network).InterruptRequest);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to submit interrupt request: ' + USBStatusToString(Status));
        
       {Update Pending}
       Dec(PSMSC95XXNetwork(Network).PendingCount);
       Exit;
      end;
      
     {Enable Endpoint Interrupts}
     Status:=SMSC95XXSetRegisterBits(Device,SMSC95XX_INT_EP_CONTROL,SMSC95XX_INT_EP_CONTROL_PHY_INT);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to enable endpoint interrupts: ' + USBStatusToString(Status));
       Exit;
      end; 
     
     {Enable LEDs}
     Status:=SMSC95XXSetRegisterBits(Device,SMSC95XX_LED_GPIO_CONFIG,SMSC95XX_LED_GPIO_CONFIG_SPD_LED or SMSC95XX_LED_GPIO_CONFIG_LNK_LED or SMSC95XX_LED_GPIO_CONFIG_FDX_LED);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set LED config: ' + USBStatusToString(Status));
       Exit;
      end; 
     
     {Enable Transmit and Receive}
     Status:=SMSC95XXSetRegisterBits(Device,SMSC95XX_MAC_CONTROL,SMSC95XX_MAC_CONTROL_TX_ENABLE or SMSC95XX_MAC_CONTROL_RX_ENABLE);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set MAC control: ' + USBStatusToString(Status));
       Exit;
      end; 
     
     Status:=SMSC95XXWriteRegister(Device,SMSC95XX_TX_CONFIG,SMSC95XX_TX_CONFIG_ON);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set transmit config: ' + USBStatusToString(Status));
       Exit;
      end; 
  
     {Set State to Open}
     Network.NetworkState:=NETWORK_STATE_OPEN;
 
     {Notify the State}
     NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_OPEN); 
  
     {Get Network Status}
     SMSC95XXPHYRead(Device,MII_BMSR,Value);
     if (Value and BMSR_LSTATUS) <> 0 then
      begin
       {Set Status to Up}
       Network.NetworkStatus:=NETWORK_STATUS_UP;
        
       {Notify the Status}
       NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_UP); 
      end;
  
     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Check Result}
     if Result <> ERROR_SUCCESS then
      begin
       {Check Interrupt Request}
       if PSMSC95XXNetwork(Network).InterruptRequest <> nil then
        begin
         {Cancel Interrupt Request}
         USBRequestCancel(PSMSC95XXNetwork(Network).InterruptRequest);
         
         {Release Interrupt Request}
         USBRequestRelease(PSMSC95XXNetwork(Network).InterruptRequest);
        end;
        
       {Check Receive Request}
       if PSMSC95XXNetwork(Network).ReceiveRequest <> nil then
        begin
         {Cancel Receive Request}
         USBRequestCancel(PSMSC95XXNetwork(Network).ReceiveRequest);
         
         {Release Receive Request}
         USBRequestRelease(PSMSC95XXNetwork(Network).ReceiveRequest);
        end;
       
       {Check Transmit Request}
       if PSMSC95XXNetwork(Network).TransmitRequest <> nil then
        begin
         {Release Transmit Request}
         USBRequestRelease(PSMSC95XXNetwork(Network).TransmitRequest);
        end;
       
       {Check Transmit Queue Buffer}
       if Network.TransmitQueue.Buffer <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Transmit Queue Entries}
         SetLength(Network.TransmitQueue.Entries,0);
          
         {Deallocate Transmit Queue Buffers}
         Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
         while Entry <> nil do
          begin
           {Release USB Request Buffer}
           USBBufferRelease(Entry.Buffer);
            
           {Deinitialize Packets}
           SetLength(Entry.Packets,0);
           
           Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
          end;
        
         {Deallocate Transmit Queue Buffer}
         BufferDestroy(Network.TransmitQueue.Buffer);
         
         Network.TransmitQueue.Buffer:=INVALID_HANDLE_VALUE;
        end;
       
       {Check Transmit Queue Semaphore}
       if Network.TransmitQueue.Wait <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Transmit Queue Semaphore}
         SemaphoreDestroy(Network.TransmitQueue.Wait);
          
         Network.TransmitQueue.Wait:=INVALID_HANDLE_VALUE;
        end;
       
       {Check Receive Queue Buffer}
       if Network.ReceiveQueue.Buffer <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Receive Queue Entries}
         SetLength(Network.ReceiveQueue.Entries,0);
        
         {Deallocate Receive Queue Buffers}
         Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
         while Entry <> nil do
          begin
           {Release USB Request Buffer}
           USBBufferRelease(Entry.Buffer);
          
           {Initialize Packets}
           SetLength(Entry.Packets,0);
           
           Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
          end;
         
         {Deallocate Receive Queue Buffer}
         BufferDestroy(Network.ReceiveQueue.Buffer);
         
         Network.ReceiveQueue.Buffer:=INVALID_HANDLE_VALUE;
        end;
        
       {Check Receive Queue Semaphore}
       if Network.ReceiveQueue.Wait <> INVALID_HANDLE_VALUE then
        begin
         {Deallocate Receive Queue Semaphore}
         SemaphoreDestroy(Network.ReceiveQueue.Wait);
          
         Network.ReceiveQueue.Wait:=INVALID_HANDLE_VALUE;
        end;
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

function SMSC95XXNetworkClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose for the SMSC95XX device}
{Note: Not intended to be called directly by applications, use NetworkDeviceClose instead}
var
 Status:LongWord;
 Message:TMessage;
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
    {Check Pending}
    if PSMSC95XXNetwork(Network).PendingCount <> 0 then
     begin
      {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Waiting for ' + IntToStr(PSMSC95XXNetwork(Network).PendingCount) + ' pending requests to complete');
      {$ENDIF}

      {Wait for Pending}
 
      {Setup Waiter}
      PSMSC95XXNetwork(Network).WaiterThread:=GetCurrentThreadId; 
   
      {Release the Lock}
      MutexUnlock(Network.Lock);
   
      {Wait for Message}
      ThreadReceiveMessage(Message); 
      
      {Acquire the Lock}
      if MutexLock(Network.Lock) <> ERROR_SUCCESS then Exit;
     end;
       
    {Set State to Closed}
    Network.NetworkState:=NETWORK_STATE_CLOSED;
 
    {Notify the State}
    NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_CLOSE); 
       
    {Disable Transmit and Receive}
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_TX_CONFIG,SMSC95XX_TX_CONFIG_STOP);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set transmit config stop: ' + USBStatusToString(Status));
      Exit;
     end;
    
    Status:=SMSC95XXClearRegisterBits(Device,SMSC95XX_MAC_CONTROL,SMSC95XX_MAC_CONTROL_TX_ENABLE or SMSC95XX_MAC_CONTROL_RX_ENABLE);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to disable transmit and receive: ' + USBStatusToString(Status));
      Exit;
     end;
   
    {Disable LEDs}   
    Status:=SMSC95XXClearRegisterBits(Device,SMSC95XX_LED_GPIO_CONFIG,SMSC95XX_LED_GPIO_CONFIG_SPD_LED or SMSC95XX_LED_GPIO_CONFIG_LNK_LED or SMSC95XX_LED_GPIO_CONFIG_FDX_LED);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to disable GPIO LEDs: ' + USBStatusToString(Status));
      Exit;
     end;
     
    {Check Interrupt Request}
    if PSMSC95XXNetwork(Network).InterruptRequest <> nil then
     begin
      {Release Interrupt Request}
      USBRequestRelease(PSMSC95XXNetwork(Network).InterruptRequest);
     end;
 
    {Check Receive Request}
    if PSMSC95XXNetwork(Network).ReceiveRequest <> nil then
     begin
      {Release Receive Request}
      USBRequestRelease(PSMSC95XXNetwork(Network).ReceiveRequest);
     end;
    
    {Check Transmit Request}
    if PSMSC95XXNetwork(Network).TransmitRequest <> nil then
     begin
      {Release Transmit Request}
      USBRequestRelease(PSMSC95XXNetwork(Network).TransmitRequest);
     end;
    
    {Check Transmit Queue Buffer}
    if Network.TransmitQueue.Buffer <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Transmit Queue Entries}
      SetLength(Network.TransmitQueue.Entries,0);
       
      {Deallocate Transmit Queue Buffers}
      Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
      while Entry <> nil do
       begin
        {Release USB Request Buffer}
        USBBufferRelease(Entry.Buffer);
        
        {Deinitialize Packets}
        SetLength(Entry.Packets,0);
        
        Entry:=BufferIterate(Network.TransmitQueue.Buffer,Entry);
       end;
     
      {Deallocate Transmit Queue Buffer}
      BufferDestroy(Network.TransmitQueue.Buffer);
      
      Network.TransmitQueue.Buffer:=INVALID_HANDLE_VALUE;
     end;
    
    {Check Transmit Queue Semaphore}
    if Network.TransmitQueue.Wait <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Transmit Queue Semaphore}
      SemaphoreDestroy(Network.TransmitQueue.Wait);
       
      Network.TransmitQueue.Wait:=INVALID_HANDLE_VALUE;
     end;
    
    {Check Receive Queue Buffer}
    if Network.ReceiveQueue.Buffer <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Receive Queue Entries}
      SetLength(Network.ReceiveQueue.Entries,0);
     
      {Deallocate Receive Queue Buffers}
      Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
      while Entry <> nil do
       begin
        {Release USB Request Buffer}
        USBBufferRelease(Entry.Buffer);
       
        {Initialize Packets}
        SetLength(Entry.Packets,0);
        
        Entry:=BufferIterate(Network.ReceiveQueue.Buffer,Entry);
       end;
      
      {Deallocate Receive Queue Buffer}
      BufferDestroy(Network.ReceiveQueue.Buffer);
      
      Network.ReceiveQueue.Buffer:=INVALID_HANDLE_VALUE;
     end;
     
    {Check Receive Queue Semaphore}
    if Network.ReceiveQueue.Wait <> INVALID_HANDLE_VALUE then
     begin
      {Deallocate Receive Queue Semaphore}
      SemaphoreDestroy(Network.ReceiveQueue.Wait);
       
      Network.ReceiveQueue.Wait:=INVALID_HANDLE_VALUE;
     end;
    
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

function SMSC95XXNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of NetworkDeviceControl for the SMSC95XX device}
{Note: Not intended to be called directly by applications, use NetworkDeviceControl instead}
var
 Value:Word;
 Status:LongWord;
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
 
 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX: Network Control (Request=' + IntToStr(Request) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Result}
    Result:=ERROR_OPERATION_FAILED;
    Status:=USB_STATUS_SUCCESS;
    
    {Check Request}
    case Request of
     NETWORK_CONTROL_CLEAR_STATS:begin
       {Clear Statistics}
       {Network}
       Network.ReceiveBytes:=0;
       Network.ReceiveCount:=0;
       Network.ReceiveErrors:=0;
       Network.TransmitBytes:=0;
       Network.TransmitCount:=0;
       Network.TransmitErrors:=0;
       Network.StatusCount:=0;
       Network.StatusErrors:=0;
       Network.BufferOverruns:=0;
       Network.BufferUnavailable:=0;
      end; 
     NETWORK_CONTROL_GET_STATS:begin
       {Get Statistics}
       if Argument2 < SizeOf(TNetworkStatistics) then Exit;
       
       {Network}
       PNetworkStatistics(Argument1).ReceiveBytes:=Network.ReceiveBytes;
       PNetworkStatistics(Argument1).ReceiveCount:=Network.ReceiveCount;
       PNetworkStatistics(Argument1).ReceiveErrors:=Network.ReceiveErrors;
       PNetworkStatistics(Argument1).TransmitBytes:=Network.TransmitBytes;
       PNetworkStatistics(Argument1).TransmitCount:=Network.TransmitCount;
       PNetworkStatistics(Argument1).TransmitErrors:=Network.TransmitErrors;
       PNetworkStatistics(Argument1).StatusCount:=Network.StatusCount;
       PNetworkStatistics(Argument1).StatusErrors:=Network.StatusErrors;
       PNetworkStatistics(Argument1).BufferOverruns:=Network.BufferOverruns;
       PNetworkStatistics(Argument1).BufferUnavailable:=Network.BufferUnavailable;
      end;     
     NETWORK_CONTROL_SET_MAC:begin     
       {Set the MAC for this device}
       Status:=SMSC95XXSetMacAddress(Device,PHardwareAddress(Argument1));
      end; 
     NETWORK_CONTROL_GET_MAC:begin    
       {Get the MAC for this device}
       Status:=SMSC95XXGetMacAddress(Device,PHardwareAddress(Argument1));
      end; 
     NETWORK_CONTROL_SET_LOOPBACK:begin  
       {Set Loopback Mode}          
       if LongBool(Argument1) then
        begin
         Status:=SMSC95XXModifyRegister(Device,SMSC95XX_MAC_CONTROL,not(LongWord(SMSC95XX_MAC_CONTROL_LOOPBK)),SMSC95XX_MAC_CONTROL_LOOPBK);
        end
       else
        begin
         Status:=SMSC95XXModifyRegister(Device,SMSC95XX_MAC_CONTROL,not(LongWord(SMSC95XX_MAC_CONTROL_LOOPBK)),0);
        end;     
      end; 
     NETWORK_CONTROL_RESET:begin       
       {Reset the device}  
       //To Do
      end; 
     NETWORK_CONTROL_DISABLE:begin     
       {Disable the device}
       //To Do
      end; 
     NETWORK_CONTROL_GET_HARDWARE:begin     
       {Get Hardware address for this device}
       Status:=SMSC95XXGetMacAddress(Device,PHardwareAddress(Argument1));
      end; 
     NETWORK_CONTROL_GET_BROADCAST:begin     
       {Get Broadcast address for this device}
       PHardwareAddress(Argument1)^:=ETHERNET_BROADCAST;
      end; 
     NETWORK_CONTROL_GET_MTU:begin     
       {Get MTU for this device}
       Argument2:=ETHERNET_MTU; 
      end; 
     NETWORK_CONTROL_GET_HEADERLEN:begin
       {Get Header length for this device}
       Argument2:=ETHERNET_HEADER_SIZE;
      end;  
     NETWORK_CONTROL_GET_LINK:begin
       {Get Link State for this device}
       //To Do //smsc95xx_link_ok_nopm //Dummy read ?
       SMSC95XXPHYRead(Device,MII_BMSR,Value);
       if (Value and BMSR_LSTATUS) <> 0 then
        begin
         {Link Up}
         Argument2:=NETWORK_LINK_UP;
        end
       else
        begin
         {Link Down}
         Argument2:=NETWORK_LINK_DOWN;
        end;
      end; 
     else
      begin
       Exit;
      end;   
    end;
 
    {Check Status}
    if Status <> USB_STATUS_SUCCESS then Exit;

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

function SMSC95XXBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferAllocate for the SMSC95XX device}
{Note: Not intended to be called directly by applications, use NetworkBufferAllocate instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Entry}
 Entry:=nil;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX: Buffer Allocate');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Set Result}
 Result:=ERROR_OPERATION_FAILED;

 {Wait for Entry (Transmit Buffer)}
 Entry:=BufferGet(Network.TransmitQueue.Buffer);
 if Entry <> nil then
  begin
   {Update Entry}
   Entry.Size:=PSMSC95XXNetwork(Network).TransmitRequestSize;
   Entry.Offset:=SMSC95XX_TX_OVERHEAD;
   Entry.Count:=PSMSC95XXNetwork(Network).TransmitPacketCount;
   
   {Update First Packet}
   Entry.Packets[0].Buffer:=Entry.Buffer;
   Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
   Entry.Packets[0].Length:=Entry.Size - Entry.Offset;
   
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX:  Entry.Size = ' + IntToStr(Entry.Size));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX:  Entry.Offset = ' + IntToStr(Entry.Offset));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX:  Entry.Count = ' + IntToStr(Entry.Count));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX:  Entry.Packets[0].Length = ' + IntToStr(Entry.Packets[0].Length));
   {$ENDIF}
   
   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SMSC95XXBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferRelease for the SMSC95XX device}
{Note: Not intended to be called directly by applications, use NetworkBufferRelease instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX: Buffer Release');
 {$ENDIF}
 
 {Check Entry}
 if Entry = nil then Exit;
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Free Entry (Receive Buffer)}
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

function SMSC95XXBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferReceive for the SMSC95XX device}
{Note: Not intended to be called directly by applications, use NetworkBufferReceive instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Entry}
 Entry:=nil;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX: Buffer Receive');
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
      Network.ReceiveQueue.Start:=(Network.ReceiveQueue.Start + 1) mod PSMSC95XXNetwork(Network).ReceiveEntryCount;
      
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

function SMSC95XXBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferTransmit for the SMSC95XX device}
{Note: Not intended to be called directly by applications, use NetworkBufferTransmit instead}
var
 Empty:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMSC95XX: Buffer Transmit');
 {$ENDIF}
  
 {Check Entry}
 if Entry = nil then Exit;
 if (Entry.Count = 0) or (Entry.Count > 1) then Exit;
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Wait for Entry}
 if SemaphoreWait(Network.TransmitQueue.Wait) = ERROR_SUCCESS then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Empty}
      Empty:=(Network.TransmitQueue.Count = 0);
      
      {Add Entry}
      Network.TransmitQueue.Entries[(Network.TransmitQueue.Start + Network.TransmitQueue.Count) mod PSMSC95XXNetwork(Network).TransmitEntryCount]:=Entry;
    
      {Update Count}
      Inc(Network.TransmitQueue.Count);
    
      {Check Empty}
      if Empty then
       begin
        {Start Transmit}
        SMSC95XXTransmitStart(PSMSC95XXNetwork(Network));
       end; 
      
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

procedure SMSC95XXTransmitStart(Network:PSMSC95XXNetwork);
{Transmit start function for the SMSC95XX Network device}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the network lock}
var
 Status:LongWord;
 Request:PUSBRequest;
 Entry:PNetworkEntry;
 Packet:PNetworkPacket;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMSC95XX: Transmit Start');
 {$ENDIF}
  
 {Check Count}
 if Network.Network.TransmitQueue.Count = 0 then Exit;

 {Get Entry}
 Entry:=Network.Network.TransmitQueue.Entries[Network.Network.TransmitQueue.Start];
 if Entry = nil then Exit;
   
 {Get Packet}
 Packet:=@Entry.Packets[0];
 
 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMSC95XX: Packet Length = ' + IntToStr(Packet.Length));
 {$ENDIF}
 
 {Get Request}
 Request:=Network.TransmitRequest;
 
 {Update Entry}
 Entry.DriverData:=Network;
 
 {Initialize Request}
 USBRequestInitialize(Request,SMSC95XXTransmitComplete,Entry.Buffer,Entry.Size,Entry);

 {Add TX Command A and B to the start of the packet data}
 PLongWord(PtrUInt(Request.Data) + 0)^:=LongWordNToLE(Packet.Length or SMSC95XX_TX_COMMAND_A_FIRST_SEG or SMSC95XX_TX_COMMAND_A_LAST_SEG);
 PLongWord(PtrUInt(Request.Data) + 4)^:=LongWordNToLE(Packet.Length);
 
 {Update Request}
 Request.Size:=Packet.Length + SMSC95XX_TX_OVERHEAD;

 {Update Pending}
 Inc(Network.PendingCount);

 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Submitting transmit request');
 {$ENDIF}
 
 {Submit the Request} 
 Status:=USBRequestSubmit(Request);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to submit transmit request: ' + USBStatusToString(Status));

   {Update Entry}
   Entry.DriverData:=nil;
   
   {Update Pending}
   Dec(Network.PendingCount);
  end;
end;
 
{==============================================================================}
{==============================================================================}
{SMSC95XX USB Functions}
function SMSC95XXDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the SMSC95XX driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Status:LongWord;
 Network:PSMSC95XXNetwork;
 NetworkInterface:PUSBInterface;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
 InterruptEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}
 
 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Interface bind not supported by driver');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check SMSC95XX Device}
 if SMSC95XXCheckDevice(Device) <> USB_STATUS_SUCCESS then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Device not found in supported device list');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device Speed}
 if Device.Speed <> USB_SPEED_HIGH then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Device speed is not USB_SPEED_HIGH');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Interface}
 NetworkInterface:=USBDeviceFindInterfaceByIndex(Device,0);
 if NetworkInterface = nil then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Device has no available interface');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Bulk IN Endpoint}
 ReceiveEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
 if ReceiveEndpoint = nil then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Device has no BULK IN endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Bulk OUT Endpoint}
 TransmitEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
 if TransmitEndpoint = nil then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Device has no BULK OUT endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Interrupt IN Endpoint}
 InterruptEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 if InterruptEndpoint = nil then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Device has no INTERRUPT IN endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Check Configuration}
 if Device.ConfigurationValue = 0 then
  begin
   {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Assigning configuration ' + IntToStr(Device.Configuration.Descriptor.bConfigurationValue) + ' (' + IntToStr(Device.Configuration.Descriptor.bNumInterfaces) + ' interfaces available)');
   {$ENDIF}
   
   {Set Configuration}
   Status:=USBDeviceSetConfiguration(Device,Device.Configuration.Descriptor.bConfigurationValue);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to set device configuration: ' + USBStatusToString(Status));
     
     {Return Result}
     Result:=Status;
     Exit;
    end;
  end;
 
 {USB device reset not required because the USB core already did a reset on the port during attach}
 
 {Create Network}
 Network:=PSMSC95XXNetwork(NetworkDeviceCreateEx(SizeOf(TSMSC95XXNetwork)));
 if Network = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to create new network device');
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update Network} 
 {Device}
 Network.Network.Device.DeviceBus:=DEVICE_BUS_USB;
 Network.Network.Device.DeviceType:=NETWORK_TYPE_ETHERNET;
 Network.Network.Device.DeviceFlags:=NETWORK_FLAG_RX_BUFFER or NETWORK_FLAG_TX_BUFFER or NETWORK_FLAG_RX_MULTIPACKET;
 Network.Network.Device.DeviceData:=Device;
 Network.Network.Device.DeviceDescription:=SMSC95XX_NETWORK_DESCRIPTION;
 {Network}
 Network.Network.NetworkState:=NETWORK_STATE_CLOSED;
 Network.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
 Network.Network.DeviceOpen:=SMSC95XXNetworkOpen;
 Network.Network.DeviceClose:=SMSC95XXNetworkClose;
 Network.Network.DeviceControl:=SMSC95XXNetworkControl;
 Network.Network.BufferAllocate:=SMSC95XXBufferAllocate;
 Network.Network.BufferRelease:=SMSC95XXBufferRelease;
 Network.Network.BufferReceive:=SMSC95XXBufferReceive;
 Network.Network.BufferTransmit:=SMSC95XXBufferTransmit;
 {Driver}
 Network.PHYLock:=INVALID_HANDLE_VALUE;
 {USB}
 Network.ReceiveEndpoint:=ReceiveEndpoint;
 Network.TransmitEndpoint:=TransmitEndpoint; 
 Network.InterruptEndpoint:=InterruptEndpoint;                
 Network.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Create PHY Lock}
 Network.PHYLock:=MutexCreate;
 if Network.PHYLock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to create PHY lock for network');
   
   {Destroy Network}
   NetworkDeviceDestroy(@Network.Network);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Register Network} 
 if NetworkDeviceRegister(@Network.Network) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to register new network device');
   
   {Destroy PHY Lock}
   MutexDestroy(Network.PHYLock);
   
   {Destroy Network}
   NetworkDeviceDestroy(@Network.Network);
   
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

function SMSC95XXDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the SMSC95XX driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Network:PSMSC95XXNetwork;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then Exit;
 
 {Check Driver}
 if Device.Driver <> SMSC95XXDriver then Exit;
 
 {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}
 
 {Get Network}
 Network:=PSMSC95XXNetwork(Device.DriverData);
 if Network = nil then Exit;
 
 {Close Network}
 SMSC95XXNetworkClose(@Network.Network);
 
 {Destroy PHY Lock}
 if Network.PHYLock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Network.PHYLock);
  end;
 
 {Update Device}
 Device.DriverData:=nil;

 {Deregister Network}
 if NetworkDeviceDeregister(@Network.Network) <> ERROR_SUCCESS then Exit;
 
 {Destroy Network}
 NetworkDeviceDestroy(@Network.Network);
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure SMSC95XXReceiveWorker(Request:PUSBRequest); 
{Called (by a Worker thread) to process a completed USB request from the SMSC95XX bulk IN endpoint}
{Request: The USB request which has completed}
var
 Data:Pointer;
 Size:LongWord;
 Status:LongWord;
 Offset:LongWord;
 Message:TMessage;
 FrameLength:LongWord;
 ReceiveStatus:LongWord;
 Next:PNetworkEntry;
 Entry:PNetworkEntry;
 Network:PSMSC95XXNetwork;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Entry}
 Entry:=PNetworkEntry(Request.DriverData);
 if Entry = nil then Exit;
 
 {Get Network}
 Network:=PSMSC95XXNetwork(Entry.DriverData);
 if Network <> nil then 
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
       
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Receive complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}
 
        {Get Data and Size}
        Data:=Request.Data;
        Size:=Request.ActualSize;
        if Size > 0 then
         begin
          {Get Next}
          Next:=nil;
          if BufferAvailable(Network.Network.ReceiveQueue.Buffer) > 0 then
           begin
            Next:=BufferGet(Network.Network.ReceiveQueue.Buffer);
           end;
          
          {Check Next} 
          if Next <> nil then
           begin
            {Check Receive Queue Count}
            if Network.Network.ReceiveQueue.Count < Network.ReceiveEntryCount then
             begin
              {Update Entry}
              Entry.Count:=0;
              
              while Size > 0 do
               begin
                {Get the Receive status word, which contains information about the next Ethernet frame}
                ReceiveStatus:=LongWordLEToN(PLongWord(PtrUInt(Data) + 0)^);
                
                {Extract FrameLength, which specifies the length of the next Ethernet frame from the MAC destination address to end of the
                 CRC following the payload. (This does not include the RX status word, which we instead account for in SMSC95XX_RX_OVERHEAD)}
                FrameLength:=(ReceiveStatus and SMSC95XX_RX_STATUS_FL) shr 16;
                
                {Check Receive Status and Frame Length}
                if ((ReceiveStatus and SMSC95XX_RX_STATUS_ES) <> 0) or ((FrameLength + SMSC95XX_RX_OVERHEAD) > Size) or (FrameLength > (ETHERNET_MAX_PACKET_SIZE + ETHERNET_CRC_SIZE)) or (FrameLength < (ETHERNET_HEADER_SIZE + ETHERNET_CRC_SIZE)) then
                 begin
                  if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Receive error (ReceiveStatus=' + IntToHex(ReceiveStatus,8) + ', FrameLength=' + IntToStr(FrameLength) + ')');
                  
                  {Update Statistics}
                  Inc(Network.Network.ReceiveErrors); 
                 end
                else if Entry.Count >= Network.ReceivePacketCount then
                 begin
                  if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Buffer overrun, packet discarded');
             
                  {Update Statistics}
                  Inc(Network.Network.BufferOverruns); 
                 end
                else
                 begin
                  {Update Entry}
                  Inc(Entry.Count);
                  
                  {Update Packet}
                  Entry.Packets[Entry.Count - 1].Buffer:=Data;
                  Entry.Packets[Entry.Count - 1].Data:=Data + Entry.Offset;
                  Entry.Packets[Entry.Count - 1].Length:=FrameLength - ETHERNET_CRC_SIZE;
                  
                  {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
                  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Receiving packet (Length=' + IntToStr(Entry.Packets[Entry.Count - 1].Length) + ', Count=' + IntToStr(Entry.Count) + ')');
                  {$ENDIF}
                  
                  {Update Statistics}
                  Inc(Network.Network.ReceiveCount); 
                  Inc(Network.Network.ReceiveBytes,Entry.Packets[Entry.Count - 1].Length); 
                 end;
                
                {Update Data and Size}
                Inc(Data,SMSC95XX_RX_OVERHEAD);
                Dec(Size,SMSC95XX_RX_OVERHEAD);
                
                {Update Size}
                Dec(Size,FrameLength);
                
                {Check Size}
                if Size > 0 then
                 begin
                  {Get Next Packet}
                  Inc(Data,FrameLength);
                  
                  {Get offset to next packet (Round to a multiple of 4)}
                  Offset:=SMSC95XX_RX_OVERHEAD + ((FrameLength + 3) and not(3));
                  if Offset > (SMSC95XX_RX_OVERHEAD + FrameLength) then
                   begin
                    {Update Data and Size}
                    Inc(Data,Offset - (SMSC95XX_RX_OVERHEAD + FrameLength));
                    Dec(Size,Offset - (SMSC95XX_RX_OVERHEAD + FrameLength));
                   end;
                 end;
               end;
               
              {Check Count}
              if Entry.Count > 0 then
               begin
                {Add Entry}
                Network.Network.ReceiveQueue.Entries[(Network.Network.ReceiveQueue.Start + Network.Network.ReceiveQueue.Count) mod Network.ReceiveEntryCount]:=Entry;
                    
                {Update Count}
                Inc(Network.Network.ReceiveQueue.Count);
                    
                {Signal Packet Received}
                SemaphoreSignal(Network.Network.ReceiveQueue.Wait);
               end
              else
               begin
                {Free Entry}
                BufferFree(Entry);
               end;
             end
            else
             begin
              if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Receive queue overrun, packet discarded');
              
              {Free Entry}
              BufferFree(Entry);
              
              {Update Statistics}
              Inc(Network.Network.ReceiveErrors); 
              Inc(Network.Network.BufferOverruns); 
             end;
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: No receive buffer available, packet discarded');
            
            {Get Next}
            Next:=Entry;
            
            {Update Statistics}
            Inc(Network.Network.ReceiveErrors); 
            Inc(Network.Network.BufferUnavailable); 
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed receive request (ActualSize=' + USBStatusToString(Request.ActualSize) + ')');
          
          {Get Next}
          Next:=Entry;
   
          {Update Statistics}
          Inc(Network.Network.ReceiveErrors); 
         end;
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed receive request (Status=' + USBStatusToString(Request.Status) + ')');
   
        {Get Next}
        Next:=Entry;
   
        {Update Statistics}
        Inc(Network.Network.ReceiveErrors); 
       end;
 
      {Update Pending}
      Dec(Network.PendingCount); 
 
      {Update Next}
      Next.DriverData:=nil;
 
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Free Next}
        BufferFree(Next);
        
        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {Check Waiter}
          if Network.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}
            
            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end; 
         end;
       end
      else
       begin      
        {Check Next} 
        if Next <> nil then
         begin
          {Update Pending}
          Inc(Network.PendingCount);
   
          {Update Next}
          Next.DriverData:=Network;
   
          {Initialize Request}
          USBRequestInitialize(Request,SMSC95XXReceiveComplete,Next.Buffer,Next.Size,Next);
          
          {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Resubmitting receive request');
          {$ENDIF}
  
          {Resubmit Request}
          Status:=USBRequestSubmit(Request);
          if Status <> USB_STATUS_SUCCESS then
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to resubmit receive request: ' + USBStatusToString(Status));
     
            {Update Pending}
            Dec(Network.PendingCount);
            
            {Update Next}
            Next.DriverData:=nil;
            
            {Free Next}
            BufferFree(Next);
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: No receive buffer available, cannot resubmit receive request');
          
          {Update Statistics}
          Inc(Network.Network.BufferUnavailable); 
         end;
       end;  
     finally
      {Release the Lock}
      MutexUnlock(Network.Network.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Receive request invalid');
  end;    
end;

{==============================================================================}

procedure SMSC95XXReceiveComplete(Request:PUSBRequest);
{Called when a USB request from the SMSC95XX bulk IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerScheduleEx(0,WORKER_FLAG_PRIORITY,TWorkerTask(SMSC95XXReceiveWorker),Request,nil);
end;

{==============================================================================}

procedure SMSC95XXTransmitWorker(Request:PUSBRequest); 
{Called (by a Worker thread) to process a completed USB request to the SMSC95XX bulk OUT endpoint}
{Request: The USB request which has completed}
var
 Message:TMessage;
 Entry:PNetworkEntry;
 Network:PSMSC95XXNetwork;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Entry}
 Entry:=PNetworkEntry(Request.DriverData);
 if Entry = nil then Exit;
 
 {Get Network}
 Network:=PSMSC95XXNetwork(Entry.DriverData);
 if Network <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, setting transmit request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
      
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Transmit complete');
        {$ENDIF}
        
        {Update Statistics}
        Inc(Network.Network.TransmitCount);
        Inc(Network.Network.TransmitBytes,Entry.Packets[0].Length);
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed transmit request (Status=' + USBStatusToString(Request.Status) + ')');
        
        {Update Statistics}
        Inc(Network.Network.TransmitErrors); 
       end;

      {Update Start}
      Network.Network.TransmitQueue.Start:=(Network.Network.TransmitQueue.Start + 1) mod PSMSC95XXNetwork(Network).TransmitEntryCount;
      
      {Update Count}
      Dec(Network.Network.TransmitQueue.Count);
      
      {Signal Queue Free}
      SemaphoreSignal(Network.Network.TransmitQueue.Wait); 

      {Update Entry}
      Entry.DriverData:=nil;
      
      {Free Entry (Transmit Buffer)}
      BufferFree(Entry);
       
      {Update Pending}
      Dec(Network.PendingCount); 
     
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {Check Waiter}
          if Network.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}
            
            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end; 
         end;
       end
      else
       begin
        {Check Count}
        if Network.Network.TransmitQueue.Count > 0 then
         begin
          {Start Transmit}
          SMSC95XXTransmitStart(Network);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Network.Network.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Transmit request invalid');
  end;    
end;

{==============================================================================}

procedure SMSC95XXTransmitComplete(Request:PUSBRequest);
{Called when a USB request to the SMSC95XX bulk OUT endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerScheduleEx(0,WORKER_FLAG_PRIORITY,TWorkerTask(SMSC95XXTransmitWorker),Request,nil);
end;

{==============================================================================}

procedure SMSC95XXInterruptWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from the SMSC95XX interrupt IN endpoint}
{Request: The USB request which has completed}
var
 Value:Word;
 Mask:LongWord;
 Status:LongWord;
 Message:TMessage;
 Network:PSMSC95XXNetwork;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Network}
 Network:=PSMSC95XXNetwork(Request.DriverData);
 if Network <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Network.Network.StatusCount); 

      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, setting interrupt request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
      
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Interrupt complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}
      
        {Clear PHY Interrupt}
        SMSC95XXPHYRead(Request.Device,SMSC95XX_PHY_INT_SRC,Value);
        
        {Clear Interrupt Status}
        SMSC95XXWriteRegister(Request.Device,SMSC95XX_INTERRUPT_STATUS,SMSC95XX_INTERRUPT_STATUS_CLEAR_ALL);
      
        {Check Size}
        if Request.ActualSize <> 4 then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Interrupt error (Size=' + IntToStr(Request.ActualSize) + ')');
         
          {Update Statistics}
          Inc(Network.Network.StatusErrors); 
         end
        else
         begin
          {Get Interrupt Mask}
          Mask:=LongWordLEToN(PLongWord(Request.Data)^);
          
          {Check for PHY Interrupt}
          if (Mask and SMSC95XX_INT_ENP_PHY_INT) <> 0 then
           begin
            {Get Network Status}
            SMSC95XXPHYRead(Request.Device,MII_BMSR,Value);
            if (Value and BMSR_LSTATUS) <> 0 then
             begin
              {Check Status}
              if Network.Network.NetworkStatus <> NETWORK_STATUS_UP then
               begin
                {Set Status to Up}
                Network.Network.NetworkStatus:=NETWORK_STATUS_UP;
              
                {Notify the Status}
                NotifierNotify(@Network.Network.Device,DEVICE_NOTIFICATION_UP); 
               end; 
             end
            else
             begin
              {Check Status}
              if Network.Network.NetworkStatus <> NETWORK_STATUS_DOWN then
               begin
                {Set Status to Down}
                Network.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
              
                {Notify the Status}
                NotifierNotify(@Network.Network.Device,DEVICE_NOTIFICATION_DOWN); 
               end; 
             end;
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Interrupt error - unknown interrupt (Mask=' + IntToHex(Mask,8) + ')');
            
            {Update Statistics}
            Inc(Network.Network.StatusErrors); 
           end;
         end;
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed interrupt request (Status=' + USBStatusToString(Request.Status) + ')');
        
        {Update Statistics}
        Inc(Network.Network.StatusErrors); 
       end;
     
      {Update Pending}
      Dec(Network.PendingCount); 
     
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {Check Waiter}
          if Network.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}
            
            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end; 
         end;
       end
      else
       begin
        {Update Pending}
        Inc(Network.PendingCount);
 
        {$IF DEFINED(SMSC95XX_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Resubmitting interrupt request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to resubmit interrupt request: ' + USBStatusToString(Status));
   
          {Update Pending}
          Dec(Network.PendingCount);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Network.Network.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Interrupt request invalid');
  end;    
end;

{==============================================================================}

procedure SMSC95XXInterruptComplete(Request:PUSBRequest);
{Called when a USB request from the SMSC95XX interrupt IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerScheduleEx(0,WORKER_FLAG_PRIORITY,TWorkerTask(SMSC95XXInterruptWorker),Request,nil);
end;

{==============================================================================}
{==============================================================================}
{SMSC95XX Helper Functions}
function SMSC95XXCheckDevice(Device:PUSBDevice):LongWord;
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
 for Count:=0 to SMSC95XX_DEVICE_ID_COUNT - 1 do
  begin
   if (SMSC95XX_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (SMSC95XX_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function SMSC95XXReadRegister(Device:PUSBDevice;Index:LongWord;var Data:LongWord):LongWord;
{Read from a register on the SMSC95XX USB Ethernet Adapter}
{Device: USB device to read from}
{Index: Index of the register to read}
{Data: Value to return to registers contents}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Send Read Register Request}
 Result:=USBControlRequest(Device,nil,SMSC95XX_VENDOR_REQUEST_READ_REGISTER,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_VENDOR or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,0,Index,@Data,SizeOf(LongWord));
end;

{==============================================================================}

function SMSC95XXWriteRegister(Device:PUSBDevice;Index,Data:LongWord):LongWord;
{Write to a register on the SMSC95XX USB Ethernet Adapter}
{Device: USB device to write to}
{Index: Index of the register to write}
{Data: Value to write to the register}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Send Write Register Request}
 Result:=USBControlRequest(Device,nil,SMSC95XX_VENDOR_REQUEST_WRITE_REGISTER,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_VENDOR or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,0,Index,@Data,SizeOf(LongWord));
end;

{==============================================================================}

function SMSC95XXModifyRegister(Device:PUSBDevice;Index,Mask,Value:LongWord):LongWord;
{Modify the value contained in a register on the SMSC LAN95XX USB Ethernet Adapter}
{Device: USB device to modify}
{Index: Index of the register to modify}
{Mask: Mask that contains 1 for the bits where the old value in the register
       will be kept rather than cleared (unless those bits also appear in 
       Value, in which case they will still be set)}
{Value: Mask of bits to set in the register}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Data:LongWord;
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Read Register}
 Status:=SMSC95XXReadRegister(Device,Index,Data);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
  
 {Modify Value}
 Data:=Data and Mask;
 Data:=Data or Value;
 
 {Write Register}
 Result:=SMSC95XXWriteRegister(Device,Index,Data);
end;

{==============================================================================}

function SMSC95XXSetRegisterBits(Device:PUSBDevice;Index,Value:LongWord):LongWord;
{Set bits in a register on the SMSC95XX USB Ethernet Adapter}
{Device: USB device to write to}
{Index: Index of the register to modify}
{Value: Bits to set in the register.  At positions where there is a 0, the old
        value in the register will be written}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Modify Register}
 Result:=SMSC95XXModifyRegister(Device,Index,$ffffffff,Value);
end;

{==============================================================================}

function SMSC95XXClearRegisterBits(Device:PUSBDevice;Index,Value:LongWord):LongWord;
{Clear bits in a register on the SMSC95XX USB Ethernet Adapter}
{Device: USB device to write to}
{Index: Index of the register to modify}
{Value: Bits to clear in the register.  At positions where there is a 0, the old
        value in the register will be written}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Modify Register}
 Result:=SMSC95XXModifyRegister(Device,Index,not(Value),0);
end;

{==============================================================================}

function SMSC95XXPHYRead(Device:PUSBDevice;Index:LongWord;var Value:Word):LongWord;
{Read a register from the MII Management serial interface on a SMSC95XX USB Ethernet Adapter}
{Device: USB device to read from}
{Index: Index of the register to read}
{Value: Value to return the register contents}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Data:LongWord;
 Status:LongWord;
 Address:LongWord;
 Network:PSMSC95XXNetwork;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Setup Value}
 Value:=0;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Get Network}
 Network:=PSMSC95XXNetwork(Device.DriverData);
 if Network = nil then Exit;
 
 {Acquire PHY Lock}
 if MutexLock(Network.PHYLock) = ERROR_SUCCESS then
  begin
   try
    {Wait for MII not busy}
    Status:=SMSC95XXPHYWaitNotBusy(Device);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
     
    {Set the Address, Index and Direction (Read from PHY)}
    Address:=SMSC95XX_INTERNAL_PHY_ID and SMSC95XX_PHY_ID_MASK;
    Index:=Index and SMSC95XX_REG_INDEX_MASK;
    Address:=(Address shl 11) or (Index shl 6) or SMSC95XX_MII_READ or SMSC95XX_MII_BUSY;
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_MII_ADDR,Address);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
    
    {Wait for MII not busy}
    Status:=SMSC95XXPHYWaitNotBusy(Device);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
    
    {Read the Data}
    Status:=SMSC95XXReadRegister(Device,SMSC95XX_MII_DATA,Data);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
    
    {Return Value}
    Value:=Data and $FFFF;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release PHY Lock}
    MutexUnlock(Network.PHYLock);
   end;
  end;
end;

{==============================================================================}

function SMSC95XXPHYWrite(Device:PUSBDevice;Index:LongWord;Value:Word):LongWord;
{Write a register to the MII Management serial interface on a SMSC95XX USB Ethernet Adapter}
{Device: USB device to write to}
{Index: Index of the register to write}
{Value: Value to write to the register}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Status:LongWord;
 Address:LongWord;
 Network:PSMSC95XXNetwork;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Get Network}
 Network:=PSMSC95XXNetwork(Device.DriverData);
 if Network = nil then Exit;
 
 {Acquire PHY Lock}
 if MutexLock(Network.PHYLock) = ERROR_SUCCESS then
  begin
   try
    {Wait for MII not busy}
    Status:=SMSC95XXPHYWaitNotBusy(Device);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
    
    {Write the Data}
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_MII_DATA,Value);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
 
    {Set the Address, Index and Direction (Write to PHY)}
    Address:=SMSC95XX_INTERNAL_PHY_ID and SMSC95XX_PHY_ID_MASK;
    Index:=Index and SMSC95XX_REG_INDEX_MASK;
    Address:=(Address shl 11) or (Index shl 6) or SMSC95XX_MII_WRITE or SMSC95XX_MII_BUSY;
    Status:=SMSC95XXWriteRegister(Device,SMSC95XX_MII_ADDR,Address);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
    
    {Wait for MII not busy}
    Status:=SMSC95XXPHYWaitNotBusy(Device);
    if Status <> USB_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release PHY Lock}
    MutexUnlock(Network.PHYLock);
   end;
  end;
end;

{==============================================================================}

function SMSC95XXPHYInitialize(Device:PUSBDevice):LongWord;
{Initialize default MII Management serial interface options on a SMSC95XX USB Ethernet Adapter}
{Device: USB device to initialize}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Value:Word;
 Timeout:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Reset PHY}
 SMSC95XXPHYWrite(Device,MII_BMCR,BMCR_RESET);
 
 {Wait for Reset to Complete}
 Timeout:=0;
 repeat
  MillisecondDelay(10);
  SMSC95XXPHYRead(Device,MII_BMCR,Value);

  Inc(Timeout)
 until ((Value and BMCR_RESET) = 0) or (Timeout >= 100);
 if Timeout >= 100 then
  begin
   Result:=USB_STATUS_TIMEOUT;
   Exit;
  end;
  
 {Clear Interrupt Status}
 SMSC95XXPHYRead(Device,SMSC95XX_PHY_INT_SRC,Value);
 
 {Enable PHY Interrupts}
 SMSC95XXPHYWrite(Device,SMSC95XX_PHY_INT_MASK,SMSC95XX_PHY_INT_MASK_DEFAULT);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function SMSC95XXPHYWaitNotBusy(Device:PUSBDevice):LongWord;
{Wait for the MII Management serial interface to be not busy on a SMSC95XX USB Ethernet Adapter}
{Device: USB device to wait for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      

{Note: Caller must hold the PHY Lock}
var
 Current:Int64;
 Value:LongWord;
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Wait for MII Busy}
 Current:=GetTickCount64;
 repeat
  Status:=SMSC95XXReadRegister(Device,SMSC95XX_MII_ADDR,Value);
  if Status <> USB_STATUS_SUCCESS then
   begin
    Result:=Status;
    Exit;
   end;
  
  if (Value and SMSC95XX_MII_BUSY) = 0 then
   begin
    Result:=USB_STATUS_SUCCESS;
    Exit;
   end;
 until GetTickCount64 > (Current + MILLISECONDS_PER_SECOND);
 
 Result:=USB_STATUS_TIMEOUT;
end;

{==============================================================================}

function SMSC95XXWaitEEPROM(Device:PUSBDevice):LongWord;
{Wait for the EEPROM (if present) to be not busy on a SMSC95XX USB Ethernet Adapter}
{Device: USB device to wait for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 //To Do
end;

{==============================================================================}

function SMSC95XXReadEEPROM(Device:PUSBDevice;Offset,Length:LongWord;Data:PByte):LongWord;
{Read from the EEPROM (if present) on a SMSC95XX USB Ethernet Adapter}
{Device: USB device to read from}
{Offset: The byte offset to start reading}
{Length: The number of bytes to read}
{Data: Pointer to a buffer to receive the data}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 //To Do //Continuing
end;

{==============================================================================}

function SMSC95XXWriteEEPROM(Device:PUSBDevice;Offset,Length:LongWord;Data:PByte):LongWord;
{Write to the EEPROM (if present) on a SMSC95XX USB Ethernet Adapter}
{Device: USB device to write to}
{Offset: The byte offset to start writing}
{Length: The number of bytes to write}
{Data: Pointer to a buffer containing the data}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 //To Do
end;

{==============================================================================}

function SMSC95XXSetMacAddress(Device:PUSBDevice;Address:PHardwareAddress):LongWord;
{Set the MAC address of the SMSC95XX USB Ethernet Adapter}
{Device: USB device to write to}
{Address: MAC address value to set}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Status:LongWord;
 AddressLow:LongWord;
 AddressHigh:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Check Address}
 if Address = nil then Exit;
 
 {Encode Address} 
 AddressLow:=Address[0] or (Address[1] shl 8) or (Address[2] shl 16) or (Address[3] shl 24);
 AddressHigh:=Address[4] or (Address[5] shl 8);
 
 {Write Address Low}
 Status:=SMSC95XXWriteRegister(Device,SMSC95XX_MAC_ADDRESS_LOW,AddressLow);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
 
 {Write Address High} 
 Result:=SMSC95XXWriteRegister(Device,SMSC95XX_MAC_ADDRESS_HIGH,AddressHigh);
end;

{==============================================================================}

function SMSC95XXGetMacAddress(Device:PUSBDevice;Address:PHardwareAddress):LongWord;
{Get the MAC address of the SMSC95XX USB Ethernet Adapter}
{Device: USB device read from}
{Address: Value to read the MAC address into}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
 AddressLow:LongWord;
 AddressHigh:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 
 {Check Address}
 if Address = nil then Exit;
 
 {Read Address Low}
 Status:=SMSC95XXReadRegister(Device,SMSC95XX_MAC_ADDRESS_LOW,AddressLow);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
 
 {Read Address High}
 Status:=SMSC95XXReadRegister(Device,SMSC95XX_MAC_ADDRESS_HIGH,AddressHigh);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
 
 {Decode Address}
 Address[0]:=(AddressLow shr 0) and $ff;
 Address[1]:=(AddressLow shr 8) and $ff;
 Address[2]:=(AddressLow shr 16) and $ff;
 Address[3]:=(AddressLow shr 24) and $ff;
 Address[4]:=(AddressHigh shr 0) and $ff;
 Address[5]:=(AddressHigh shr 8) and $ff;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}
{==============================================================================}

initialization
 SMSC95XXInit;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
