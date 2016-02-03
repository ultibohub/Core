{
SMSC LAN95xx USB Ethernet Driver.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi - Model A/B/A+/B+
 Raspberry Pi 2 - Model B

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

//To Do //For additional details on this device, including detecting Link status, Vendor and Device ids for each model,
        // Enabling auto-negotiation, checksum offload, multicast, VLAN etc
        //See: \u-boot-HEAD-5745f8c\drivers\usb\eth\smsc95xx.c
        //See also the Linux driver: \linux-rpi-4.1.y\drivers\net\usb\smsc95xx.c

//To Do //Use of SMSC95XX_BULK_IN_DELAY / SMSC95XX_DEFAULT_BULK_IN_DELAY //See notes
        //Use of SMSC95XX_AFC_CONFIG_DEFAULT //See notes
                      
//To Do //See also: smsc95xx_phy_initialize / mii_nway_restart / smsc95xx_mdio_read / smsc95xx_mdio_write etc
        //in \u-boot-HEAD-5745f8c\drivers\usb\eth\smsc95xx.c
        //and \u-boot-HEAD-5745f8c\include\linux\mii.h
                      
interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,Network,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {SMSC95XX specific constants}
 SMSC95XX_DRIVER_NAME = 'SMSC LAN95XX USB Ethernet Adapter Driver'; {Name of SMSC95XX driver}

 SMSC95XX_DEVICE_ID_COUNT = 5; {Number of supported Device IDs}
 
 SMSC95XX_TX_OVERHEAD = 8;
 SMSC95XX_RX_OVERHEAD = 4;
 
 SMSC95XX_HS_USB_PKT_SIZE = 512;
 SMSC95XX_FS_USB_PKT_SIZE = 64;
 
 SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE = (16 * 1024 + 5 * SMSC95XX_HS_USB_PKT_SIZE); 
 SMSC95XX_DEFAULT_FS_BURST_CAP_SIZE = (6 * 1024 + 33 * SMSC95XX_FS_USB_PKT_SIZE); 
 
 SMSC95XX_DEFAULT_BULK_IN_DELAY = $2000;
 
 SMSC95XX_MAX_TX_REQUESTS = 1;
 SMSC95XX_MAX_RX_REQUESTS = 1; //((60 * 1518) div SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE) + 1; {SMSC95XX_MAX_RX_REQUESTS (DIV_ROUND_UP(60 * 1518, SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE))} //To Do

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
 {TODO: this is set by SMSC's Linux driver.  I don't know what BIR stands for,
  but the BI might stand for Bulk In.  The observed behavior is that if you
  don't set this flag, latency for Rx, Tx, or both appears to increase, and
  Bulk IN transfers can complete immediately with 0 length even when no data
  has been received} 
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
 {TODO: This is set by SMSC's Linux driver at the same time as HW_CONFIG_MEF.  I have no idea what it stands for or what it does}
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
 
 {Unknown Register}
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
 
 {Unknown Register}
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
 
 {Unknown Register}
 SMSC95XX_MII_ADDR                     = $114;
 SMSC95XX_MII_WRITE                    = $02;
 SMSC95XX_MII_BUSY                     = $01;
 SMSC95XX_MII_READ                     = $00; {~of MII Write bit}

 {Unknown Register}
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
 
 {Unknown Register}
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
 SMSC95XX_PHY_INT_SRC_LINK_DOWN         = $0010;   //To Do //Critical //This will be useful to us //Need an Interrupt Endpoint

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
 PSMSC95XXNetworkDevice = ^TSMSC95XXNetworkDevice;
 TSMSC95XXNetworkDevice = record
  {Network Properties}
  Network:TNetworkDevice;
  {USB Properties}
  ReceiveRequests:array[0..SMSC95XX_MAX_RX_REQUESTS - 1] of PUSBRequest;  {USB requests for packet receive data}
  TransmitRequests:array[0..SMSC95XX_MAX_TX_REQUESTS - 1] of PUSBRequest; {USB requests for packet transmit data}
  InterruptRequest:PUSBRequest;                                           {USB request for interrupt data}
  ReceiveEndpoint:PUSBEndpointDescriptor;                                 {Bulk IN Endpoint}
  TransmitEndpoint:PUSBEndpointDescriptor;                                {Bulk OUT Endpoint}
  InterruptEndpoint:PUSBEndpointDescriptor;                               {Interrupt IN Endpoint}
  PendingCount:LongWord;                                                  {Number of USB requests pending for this network}
  WaiterThread:TThreadId;                                                 {Thread waiting for pending requests to complete (for network close)}
 end; 

 {SMSC95XX Device IDs} 
 PSMSC95XXDeviceId = ^TSMSC95XXDeviceId;
 TSMSC95XXDeviceId = record
  idVendor:Word;
  idProduct:Word;
 end;
 
{==============================================================================}
{var}
 {SMSC95XX specific variables}

const 
 SMSC95XXDeviceId:array[0..SMSC95XX_DEVICE_ID_COUNT - 1] of TSMSC95XXDeviceId = (
  (idVendor:$0424;idProduct:$ec00),  {LAN9512/LAN9514 Ethernet}
  (idVendor:$0424;idProduct:$9500),  {LAN9500 Ethernet}
  (idVendor:$0424;idProduct:$9730),  {LAN9730 Ethernet (HSIC)}
  (idVendor:$0424;idProduct:$9900),  {SMSC9500 USB Ethernet Device (SAL10)}
  (idVendor:$0424;idProduct:$9e00)); {LAN9500A Ethernet}
 
{==============================================================================}
{Initialization Functions}
procedure SMSC95XXInit;
 
{==============================================================================}
{SMSC95XX Network Functions}
function SMSC95XXDeviceOpen(Network:PNetworkDevice):LongWord;
function SMSC95XXDeviceClose(Network:PNetworkDevice):LongWord;
//Critical //DeviceAllocate/DeviceRelease
function SMSC95XXDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function SMSC95XXDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function SMSC95XXDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

{==============================================================================}
{SMSC95XX USB Functions}
function SMSC95XXDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function SMSC95XXDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
 
procedure SMSC95XXReceiveComplete(Request:PUSBRequest); 
procedure SMSC95XXTransmitComplete(Request:PUSBRequest); 
procedure SMSC95XXInterruptComplete(Request:PUSBRequest);
 
{==============================================================================}
{SMSC95XX Helper Functions}
function SMSC95XXCheckDevice(Device:PUSBDevice):LongWord;

function SMSC95XXReadRegister(Device:PUSBDevice;Index:LongWord;var Data:LongWord):LongWord;
function SMSC95XXWriteRegister(Device:PUSBDevice;Index,Data:LongWord):LongWord;
function SMSC95XXModifyRegister(Device:PUSBDevice;Index,Mask,Value:LongWord):LongWord;

function SMSC95XXSetRegisterBits(Device:PUSBDevice;Index,Value:LongWord):LongWord;
function SMSC95XXClearRegisterBits(Device:PUSBDevice;Index,Value:LongWord):LongWord;

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
function SMSC95XXDeviceOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen for the SMSC95XX device}
var
 Count:Integer;
 Status:LongWord;
 Device:PUSBDevice;
 Request:PUSBRequest;
 Requests:array[0..SMSC95XX_MAX_TX_REQUESTS - 1] of PUSBRequest;
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

    {Create Transmit Semaphore}
    Network.TransmitWait:=SemaphoreCreate(0);
    if Network.TransmitWait = INVALID_HANDLE_VALUE then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to create transmit semaphore');
      Exit;
     end;
     
    {Create Transmit Buffer}
    Network.TransmitBuffer:=BufferCreate(SizeOf(TUSBRequest) + ETHERNET_MAX_PACKET_SIZE + SMSC95XX_TX_OVERHEAD,SMSC95XX_MAX_TX_REQUESTS);
    if Network.TransmitBuffer = INVALID_HANDLE_VALUE then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to create transmit buffer');
      
      {Destroy Transmit Semaphore}
      SemaphoreDestroy(Network.TransmitWait);
      Exit;
     end;
 
    {Create Receive Buffer}
    Network.ReceiveBuffer:=BufferCreate(SizeOf(TNetworkPacket) + ETHERNET_MAX_PACKET_SIZE,NETWORK_BUFFER_SIZE);
    if Network.ReceiveBuffer = INVALID_HANDLE_VALUE then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to create receive buffer');
      
      {Destroy Transmit Semaphore}
      SemaphoreDestroy(Network.TransmitWait);
      
      {Destroy Transmit Buffer}
      BufferDestroy(Network.TransmitBuffer);
      Exit;
     end;
       
    {Initialize Transmit Requests}
    for Count:=0 to SMSC95XX_MAX_TX_REQUESTS - 1 do
     begin
      {Get Buffer}
      Request:=BufferGet(Network.TransmitBuffer);
      
      {Initialize Request}
      USBRequestInitialize(Request); //Remove //Critical
      
      {Initialize Request}
      Request.Device:=Device;
      Request.Endpoint:=PSMSC95XXNetworkDevice(Network).TransmitEndpoint;
      Request.Data:=Pointer(PtrUInt(Request) + PtrUInt(SizeOf(TUSBRequest)));
      Request.Callback:=SMSC95XXTransmitComplete;
      Request.DriverData:=Network;
      
      Requests[Count]:=Request;
     end;
    for Count:=0 to SMSC95XX_MAX_TX_REQUESTS - 1 do
     begin
      {Free Buffer}
      BufferFree(Requests[Count]);
     end;
 
    {Allocate Receive Requests}
    for Count:=0 to SMSC95XX_MAX_RX_REQUESTS - 1 do
     begin
      {Allocate Request}
      Request:=USBRequestAllocate(Device,PSMSC95XXNetworkDevice(Network).ReceiveEndpoint,SMSC95XXReceiveComplete,SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE,Network); 
      if Request = nil then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to allocate receive request');
        
        {Destroy Transmit Semaphore}
        SemaphoreDestroy(Network.TransmitWait);
        
        {Destroy Transmit Buffer}
        BufferDestroy(Network.TransmitBuffer);
        
        {Destroy Receive Buffer}
        BufferDestroy(Network.ReceiveBuffer);
        Exit;
       end;
      
      {Set Receive Request}
      PSMSC95XXNetworkDevice(Network).ReceiveRequests[Count]:=Request;
      
      {Update Pending}
      Inc(PSMSC95XXNetworkDevice(Network).PendingCount);
       
      {$IFDEF USB_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Submitting receive request');
      {$ENDIF}
      
      {Submit Request}
      Status:=USBRequestSubmit(Request);
      if Status <> USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to submit receive request: ' + USBStatusToString(Status));
        
        {Update Pending}
        Dec(PSMSC95XXNetworkDevice(Network).PendingCount);

        {Destroy Transmit Semaphore}
        SemaphoreDestroy(Network.TransmitWait);
        
        {Destroy Transmit Buffer}
        BufferDestroy(Network.TransmitBuffer);
        
        {Destroy Receive Buffer}
        BufferDestroy(Network.ReceiveBuffer);
        Exit;
       end;
     end;
 
    {Allocate Interrupt Request}
    //Critical
    
    {Enable Transmit and Receive}
    Device.LastError:=USB_STATUS_SUCCESS;
    //To Do //Change to individual functions
    SMSC95XXSetRegisterBits(Device,SMSC95XX_LED_GPIO_CONFIG,SMSC95XX_LED_GPIO_CONFIG_SPD_LED or SMSC95XX_LED_GPIO_CONFIG_LNK_LED or SMSC95XX_LED_GPIO_CONFIG_FDX_LED);
    SMSC95XXSetRegisterBits(Device,SMSC95XX_MAC_CONTROL,SMSC95XX_MAC_CONTROL_TX_ENABLE or SMSC95XX_MAC_CONTROL_RX_ENABLE);
    SMSC95XXWriteRegister(Device,SMSC95XX_TX_CONFIG,SMSC95XX_TX_CONFIG_ON);
    Status:=Device.LastError;
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to enable transmit and receive: ' + USBStatusToString(Status));
      
      {Destroy Transmit Semaphore}
      SemaphoreDestroy(Network.TransmitWait);     
      
      {Destroy Transmit Buffer}
      BufferDestroy(Network.TransmitBuffer);
      
      {Destroy Receive Buffer}
      BufferDestroy(Network.ReceiveBuffer);
      Exit;
     end;
 
    {Set State to Open}
    Network.NetworkState:=NETWORK_STATE_OPEN;

    {Notify the State}
    NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_OPEN); 
 
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

function SMSC95XXDeviceClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose for the SMSC95XX device}
var
 Count:Integer;
 Status:LongWord;
 Message:TMessage;
 Device:PUSBDevice;
 Request:PUSBRequest;
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
    if PSMSC95XXNetworkDevice(Network).PendingCount <> 0 then
     begin
      {$IFDEF USB_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Waiting for ' + IntToStr(PSMSC95XXNetworkDevice(Network).PendingCount) + ' pending requests to complete');
      {$ENDIF}

      {Wait for Pending}
 
      {Setup Waiter}
      PSMSC95XXNetworkDevice(Network).WaiterThread:=GetCurrentThreadId; 
   
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
    Device.LastError:=USB_STATUS_SUCCESS;
    //To Do //Change to individual functions
    SMSC95XXWriteRegister(Device,SMSC95XX_TX_CONFIG,SMSC95XX_TX_CONFIG_STOP);
    SMSC95XXClearRegisterBits(Device,SMSC95XX_MAC_CONTROL,SMSC95XX_MAC_CONTROL_TX_ENABLE or SMSC95XX_MAC_CONTROL_RX_ENABLE);
    SMSC95XXClearRegisterBits(Device,SMSC95XX_LED_GPIO_CONFIG,SMSC95XX_LED_GPIO_CONFIG_SPD_LED or SMSC95XX_LED_GPIO_CONFIG_LNK_LED or SMSC95XX_LED_GPIO_CONFIG_FDX_LED);
    Status:=Device.LastError;
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to disable transmit and receive: ' + USBStatusToString(Status));
      Exit;
     end;
 
    {Release Receive Requests}
    //To Do
 
    {Release Transmit Requests}
    //To Do
 
    {Destroy Receive Buffer}
    //To Do
       
    {Destroy Transmit Buffer}
    //To Do
      
    {Destroy Transmit Semaphore}
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

function SMSC95XXDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
{Implementation of NetworkDeviceRead for the SMSC95XX device}
var
 Packet:PNetworkPacket;
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

 {Wait for Packet}
 if SemaphoreWait(Network.Buffer.Wait) = ERROR_SUCCESS then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Set Result}
      Result:=ERROR_OPERATION_FAILED;

      {Remove the Packet}
      Packet:=Network.Buffer.Buffer[Network.Buffer.Start];
      if Packet <> nil then
       begin
        {Update Start}
        Network.Buffer.Start:=(Network.Buffer.Start + 1) mod NETWORK_BUFFER_SIZE;
 
        {Update Count}
        Dec(Network.Buffer.Count);
  
        {Copy the Packet}
        Length:=Size;
        if Packet.Length < Length then Length:=Packet.Length;
        System.Move(Packet.Buffer^,Buffer^,Length); //To Do //Critical //Non copy receive to be implemented !!
   
        {Return the Buffer}
        BufferFree(Packet);
 
        {Return Result}
        Result:=ERROR_SUCCESS;
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
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function SMSC95XXDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord; 
{Implementation of NetworkDeviceWrite for the SMSC95XX device}
var
 Unlock:Boolean;
 Status:LongWord;
 Request:PUSBRequest;
 TransmitCommandA:LongWord;
 TransmitCommandB:LongWord;
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
 if (Size < ETHERNET_HEADER_SIZE) or (Size > (ETHERNET_HEADER_SIZE + ETHERNET_MTU)) then Exit;
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;
 
 {Wait for Buffer}
 Request:=BufferGet(Network.TransmitBuffer);
 if Request <> nil then
  begin
   try
    {Acquire the Lock}
    if MutexLock(Network.Lock) = ERROR_SUCCESS then
     begin
      try
       Unlock:=True;
    
       {Set Result}
       Result:=ERROR_OPERATION_FAILED;
       
       {Copy the packet's data into the buffer, but also include two words at the
        beginning that contain device-specific flags.  These two fields are
        required, although we essentially just use them to tell the hardware we
        are transmitting one (1) packet with no extra bells and whistles}
       TransmitCommandA:=Size or SMSC95XX_TX_COMMAND_A_FIRST_SEG or SMSC95XX_TX_COMMAND_A_LAST_SEG;
       PLongWord(PtrUInt(Request.Data) + 0)^:=TransmitCommandA; //--LongWordNToBE(TransmitCommandA); //TestingRPi
       //--PByte(PtrUInt(Request.Data) + 0)^:=(TransmitCommandA shr 0) and $FF;   
       //--PByte(PtrUInt(Request.Data) + 1)^:=(TransmitCommandA shr 8) and $FF;
       //--PByte(PtrUInt(Request.Data) + 2)^:=(TransmitCommandA shr 16) and $FF;
       //--PByte(PtrUInt(Request.Data) + 3)^:=(TransmitCommandA shr 24) and $FF;   //To Do //Isn't this just a LongSwap into a PLongWord ? //To Do BEtoN()
       TransmitCommandB:=Size;
       PLongWord(PtrUInt(Request.Data) + 4)^:=TransmitCommandB; //--LongWordNToBE(TransmitCommandB); //TestingRPi
       //--PByte(PtrUInt(Request.Data) + 4)^:=(TransmitCommandB shr 0) and $FF;
       //--PByte(PtrUInt(Request.Data) + 5)^:=(TransmitCommandB shr 8) and $FF;
       //--PByte(PtrUInt(Request.Data) + 6)^:=(TransmitCommandB shr 16) and $FF;
       //--PByte(PtrUInt(Request.Data) + 7)^:=(TransmitCommandB shr 24) and $FF;   //To Do //Isn't this just a LongSwap into a PLongWord ? //To Do BEtoN()
       System.Move(Buffer^,Pointer(PtrUInt(Request.Data) + PtrUInt(SMSC95XX_TX_OVERHEAD))^,Size); //To Do //Critical //Non copy transmit to be implemented !!
 
       {Update Request}
       Request.Size:=Size + SMSC95XX_TX_OVERHEAD;

       {Update Pending}
       Inc(PSMSC95XXNetworkDevice(Network).PendingCount);
    
       {$IFDEF USB_DEBUG}
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Submitting transmit request');
       {$ENDIF}

       {Submit the Request} 
       Status:=USBRequestSubmit(Request);
       if Status <> USB_STATUS_SUCCESS then
        begin
         if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to submit transmit request: ' + USBStatusToString(Status));
      
         {Update Pending}
         Dec(PSMSC95XXNetworkDevice(Network).PendingCount);
         Exit;
        end;
    
       {Release the Lock}    
       MutexUnlock(Network.Lock);
       Unlock:=False;
    
       {Wait for Transmit}
       if SemaphoreWait(Network.TransmitWait) <> ERROR_SUCCESS then Exit;
    
       {Check Result}
       if Request.Status = USB_STATUS_SUCCESS then
        begin
         {Return Length}
         Length:=Size;
 
         {Return Result}  
         Result:=ERROR_SUCCESS;
        end; 
      finally
       {Release the Lock}
       if Unlock then MutexUnlock(Network.Lock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;  
   finally
    {Return the Buffer}
    BufferFree(Request);
   end  
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SMSC95XXDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Implementation of NetworkDeviceControl for the SMSC95XX device}
var
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
 
 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Result}
    Result:=ERROR_OPERATION_FAILED;
    
    {Check Request}
    Status:=USB_STATUS_SUCCESS;
    case Request of
     NETWORK_CONTROL_CLEAR_STATS:begin
       {Clear Statistics}
       //To Do
      end; 
     NETWORK_CONTROL_SET_MAC:begin     
       {Set the MAC for this device}
       Status:=SMSC95XXSetMacAddress(Device,PHardwareAddress(Argument1));  
      end; 
     NETWORK_CONTROL_GET_MAC:begin    
       {Get the MAC for this device}
       Status:=SMSC95XXGetMacAddress(Device,PHardwareAddress(Argument1));  //To Do //Change to Argument2 ?
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
       Status:=SMSC95XXGetMacAddress(Device,PHardwareAddress(Argument1));  //To do //Change to Argument2 ?
      end; 
     NETWORK_CONTROL_GET_BROADCAST:begin     
       {Get Broadcast address for this device}
       PHardwareAddress(Argument1)^:=ETHERNET_BROADCAST;  //To do //Change to Argument2 ?
      end; 
     NETWORK_CONTROL_GET_MTU:begin     
       {Get MTU for this device}
       Argument2:=ETHERNET_MTU; 
      end; 
     NETWORK_CONTROL_GET_HEADERLEN:begin
       {Get Header length for this device}
       Argument2:=ETHERNET_HEADER_SIZE;
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
{==============================================================================}
{SMSC95XX USB Functions}
function SMSC95XXDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the SMSC95XX driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Status:LongWord;
 Address:PHardwareAddress;
 Network:PSMSC95XXNetworkDevice;
 NetworkInterface:PUSBInterface;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
 InterruptEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check SMSC95XX Device}
 if SMSC95XXCheckDevice(Device) <> USB_STATUS_SUCCESS then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device Speed}
 if Device.Speed <> USB_SPEED_HIGH then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Interface}
 NetworkInterface:=USBDeviceFindInterfaceByIndex(Device,0);
 if NetworkInterface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Bulk IN Endpoint}
 ReceiveEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
 if ReceiveEndpoint = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Bulk OUT Endpoint}
 TransmitEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
 if TransmitEndpoint = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Interrupt IN Endpoint}
 InterruptEndpoint:=USBDeviceFindEndpointByType(Device,NetworkInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 if InterruptEndpoint = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Check Configuration}
 if Device.ConfigurationValue = 0 then
  begin
   {$IFDEF USB_DEBUG}
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
 
 //Critical //Do this more sensibly //Same for NetworkOpen
 Device.LastError:=USB_STATUS_SUCCESS;
 
 {Resetting the SMSC LAN95XX via its registers should not be necessary because the USB code already performed a reset on the USB port it's attached to}
 
 {Get MAC address}
 Address:=AllocMem(SizeOf(THardwareAddress));
 SMSC95XXGetMacAddress(Device,Address);
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Default Address = ' + HardwareAddressToString(Address^));
 {$ENDIF}
 if CompareHardwareBroadcast(Address^) then
  begin
   {Set MAC address}
   Address^:=StringToHardwareAddress(SMSC95XX_MAC_ADDRESS); 
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Hardware Address = ' + HardwareAddressToString(Address^));
   {$ENDIF}
   SMSC95XXSetMacAddress(Device,Address);
  end; 
 FreeMem(Address);
 
 {Allow multiple Ethernet frames to be received in a single USB transfer. Also set a couple flags of unknown function}
 SMSC95XXSetRegisterBits(Device,SMSC95XX_HW_CONFIG,SMSC95XX_HW_CONFIG_MEF or SMSC95XX_HW_CONFIG_BIR or SMSC95XX_HW_CONFIG_BCE);
 
 {Set the maximum USB (not network) packets per USB Receive transfer. Required when SMSC95XX_HW_CONFIG_MEF is set}
 SMSC95XXWriteRegister(Device,SMSC95XX_BURST_CAP,SMSC95XX_DEFAULT_HS_BURST_CAP_SIZE div SMSC95XX_HS_USB_PKT_SIZE);
 
 {Check for error and return}
 if Device.LastError <> USB_STATUS_SUCCESS then
  begin
   {Return Result}
   Result:=Device.LastError;
   Exit;
  end;
 
 {Create Network}
 Network:=PSMSC95XXNetworkDevice(NetworkDeviceCreateEx(SizeOf(TSMSC95XXNetworkDevice)));
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
 Network.Network.Device.DeviceFlags:=NETWORK_FLAG_NONE;     
 Network.Network.Device.DeviceData:=Device;
 {Network}
 Network.Network.NetworkState:=NETWORK_STATE_CLOSED;
 Network.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
 Network.Network.DeviceOpen:=SMSC95XXDeviceOpen;
 Network.Network.DeviceClose:=SMSC95XXDeviceClose;
 Network.Network.DeviceRead:=SMSC95XXDeviceRead;
 Network.Network.DeviceWrite:=SMSC95XXDeviceWrite;
 Network.Network.DeviceControl:=SMSC95XXDeviceControl;
 {Driver}
 {USB}
 Network.ReceiveEndpoint:=ReceiveEndpoint;
 Network.TransmitEndpoint:=TransmitEndpoint; 
 Network.InterruptEndpoint:=InterruptEndpoint;                
 Network.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Register Network} 
 if NetworkDeviceRegister(@Network.Network) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'SMSC95XX: Failed to register new network device');
   
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
 Network:PSMSC95XXNetworkDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then Exit;
 
 {Check Driver}
 if Device.Driver <> SMSC95XXDriver then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'SMSC95XX: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Get Network}
 Network:=PSMSC95XXNetworkDevice(Device.DriverData);
 if Network = nil then Exit;
 
 {Close Network}
 SMSC95XXDeviceClose(@Network.Network);
 
 {Update Device}
 Device.DriverData:=nil;

 {Deregister Network}
 if NetworkDeviceDeregister(@Network.Network) <> ERROR_SUCCESS then Exit;
 
 {Destroy Network}
 NetworkDeviceDestroy(@Network.Network);
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure SMSC95XXReceiveComplete(Request:PUSBRequest);
{Called when a USB request from the SMSC95XX bulk IN endpoint completes}
{Request: The USB request which has completed}
var
 Data:Pointer;
 DataEnd:Pointer;
 Status:LongWord;
 Message:TMessage;
 FrameLength:LongWord;
 ReceiveStatus:LongWord;
 Packet:PNetworkPacket;
 Network:PSMSC95XXNetworkDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Network}
 Network:=PSMSC95XXNetworkDevice(Request.DriverData);
 if Network <> nil then 
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Network.Network.ReceiveCount); 
 
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
       
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Receive complete');
        {$ENDIF}
        
        Data:=Request.Data;
        DataEnd:=Pointer(PtrUInt(Request.Data) + PtrUInt(Request.ActualSize));
   
        while Pointer(PtrUInt(Data) + SMSC95XX_RX_OVERHEAD + ETHERNET_HEADER_SIZE + ETHERNET_CRC_SIZE) <= DataEnd do
         begin
          {Get the Receive status word, which contains information about the next Ethernet frame}
          //--ReceiveStatus:=(PByte(PtrUInt(Data) + 0)^ shl 0) or (PByte(PtrUInt(Data) + 1)^ shl 8) or (PByte(PtrUInt(Data) + 2)^ shl 16) or (PByte(PtrUInt(Data) + 3)^ shl 24); //To Do //Isn't this just a LongSwap from a PLongWord ? //To Do BEtoN()
          ReceiveStatus:=PLongWord(PtrUInt(Data) + 0)^; //--LongWordBEToN(PLongWord(PtrUInt(Data) + 0)^); //TestingRPi
          
          {Extract FrameLength, which specifies the length of the next Ethernet frame from the MAC destination address to end of the
          CRC following the payload. (This does not include the Rx status word, which we instead account for in SMSC95XX_RX_OVERHEAD)}
          FrameLength:=(ReceiveStatus and SMSC95XX_RX_STATUS_FL) shr 16;
     
          {Check Receive Status and Frame Length}
          if ((ReceiveStatus and SMSC95XX_RX_STATUS_ES) <> 0) or ((FrameLength + SMSC95XX_RX_OVERHEAD) > (PtrUInt(DataEnd) - PtrUInt(Data))) or (FrameLength > (ETHERNET_MAX_PACKET_SIZE + ETHERNET_CRC_SIZE)) or (FrameLength < (ETHERNET_HEADER_SIZE + ETHERNET_CRC_SIZE)) then
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Receive error (ReceiveStatus=' + IntToHex(ReceiveStatus,8) + ', FrameLength=' + IntToStr(FrameLength) + ')');
       
            {Update Statistics}
            Inc(Network.Network.ReceiveErrors); 
           end
          else if Network.Network.Buffer.Count = NETWORK_BUFFER_SIZE then
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Buffer overrun, packet discarded');
       
            {Update Statistics}
            Inc(Network.Network.BufferOverruns); 
           end
          else 
           begin
            {Release the Lock}
            MutexUnlock(Network.Network.Lock);
            
            {Wait for Buffer}
            Packet:=BufferGet(Network.Network.ReceiveBuffer);
       
            {Acquire the Lock}
            if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
             begin
              {Check Packet}
              if Packet <> nil then
               begin
                {Update Packet}
                Packet.Buffer:=Pointer(PtrUInt(Packet) + PtrUInt(SizeOf(TNetworkPacket)));
                Packet.Data:=Packet.Buffer;
                Packet.Length:=FrameLength - ETHERNET_CRC_SIZE;
              
                {Copy Packet}
                System.Move(Pointer(PtrUInt(Data) + SMSC95XX_RX_OVERHEAD)^,Packet.Buffer^,Packet.Length); //To Do //Critical //Non copy receive to be implemented
              
                {Add the Packet}
                Network.Network.Buffer.Buffer[(Network.Network.Buffer.Start + Network.Network.Buffer.Count) mod NETWORK_BUFFER_SIZE]:=Packet;
              
                {Update Count}
                Inc(Network.Network.Buffer.Count);
       
                {$IFDEF USB_DEBUG}
                if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Receiving packet (Length=' + IntToStr(Packet.Length) + ', Count=' + IntToStr(Network.Network.Buffer.Count) + ')');
                {$ENDIF}
      
                {Signal Packet Received}
                SemaphoreSignal(Network.Network.Buffer.Wait);
               end
              else
               begin
                if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Buffer invalid, packet discarded');
                
                //To Do //Update Statistics ?
               end;               
             end
            else
             begin
              if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to acquire lock');
              Exit; //To Do //Do not exit ? Will deadlock the hub thread if waiting for detach ? //Must exit though ?
             end;
           end;      
          
          {Get Next Packet}
          PtrUInt(Data):=PtrUInt(Data) + SMSC95XX_RX_OVERHEAD + ((FrameLength + 3) and not(3));
         end;
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed receive request (Status=' + USBStatusToString(Request.Status) + ')');
   
        {Update Statistics}
        Inc(Network.Network.ReceiveErrors); 
       end;
 
      {Update Pending}
      Dec(Network.PendingCount); 
 
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {$IFDEF USB_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
          {$ENDIF}
          
          {Send Message}
          FillChar(Message,SizeOf(TMessage),0);
          ThreadSendMessage(Network.WaiterThread,Message);
         end;
       end
      else
       begin      
        {Update Pending}
        Inc(Network.PendingCount);
 
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Resubmitting receive request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed to resubmit receive request: ' + USBStatusToString(Status));
   
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
   if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Receive request invalid');
  end;    
end;

{==============================================================================}

procedure SMSC95XXTransmitComplete(Request:PUSBRequest);
{Called when a USB request from the SMSC95XX bulk OUT endpoint completes}
{Request: The USB request which has completed}
var
 Status:LongWord;
 Message:TMessage;
 Network:PSMSC95XXNetworkDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Network}
 Network:=PSMSC95XXNetworkDevice(Request.DriverData);
 if Network <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Network.Network.TransmitCount); 

      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
      
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
      
      {Check Result} 
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Transmit complete');
        {$ENDIF}
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'SMSC95XX: Failed transmit request (Status=' + USBStatusToString(Request.Status) + ')');
        
        {Update Statistics}
        Inc(Network.Network.TransmitErrors); 
       end;
 
      {Signal Packet Transmitted}
      SemaphoreSignal(Network.Network.TransmitWait);
     
      {Update Pending}
      Dec(Network.PendingCount); 
     
      {Check State}
      if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
       begin
        {Check Pending}
        if Network.PendingCount = 0 then
         begin
          {$IFDEF USB_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'SMSC95XX: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
          {$ENDIF}
          
          {Send Message}
          FillChar(Message,SizeOf(TMessage),0);
          ThreadSendMessage(Network.WaiterThread,Message);
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

procedure SMSC95XXInterruptComplete(Request:PUSBRequest);
{Called when a USB request from the SMSC95XX interrupt IN endpoint completes}
{Request: The USB request which has completed}
var
 Status:LongWord;
 Network:PSMSC95XXNetworkDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Network}
 Network:=PSMSC95XXNetworkDevice(Request.DriverData);
 if Network <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
    begin
     try

     //Critical
 
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
   if (SMSC95XXDeviceId[Count].idVendor = Device.Descriptor.idVendor) and (SMSC95XXDeviceId[Count].idProduct = Device.Descriptor.idProduct) then
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
 Result:=USBControlRequest(Device,nil,SMSC95XX_VENDOR_REQUEST_WRITE_REGISTER,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_VENDOR or  USB_BMREQUESTTYPE_RECIPIENT_DEVICE,0,Index,@Data,SizeOf(LongWord));
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
