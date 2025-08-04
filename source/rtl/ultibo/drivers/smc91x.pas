{
SMSC 91C9x/91C1xx Ethernet Driver.

Copyright (C) 2025 - SoftOz Pty Ltd.

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

  Linux - \drivers\net\ethernet\smsc\smc91x.c - Copyright (C) 2003 Monta Vista Software, Inc and others

  QEMU - \hw\net\smc91c111.c - Copyright (c) 2005 CodeSourcery, LLC

References
==========

 LAN91C111 - http://ww1.microchip.com/downloads/en/DeviceDoc/91c111.pdf
             http://www.microchip.com/wwwproducts/en/LAN91C111

SMSC 91C9x/91C1xx Ethernet
==========================

 The SMSC 91C9x/91C1xx are a family of Non-PCI 10/100 Ethernet Single Chip MAC + PHY devices.

 This driver is primarily intended to support the LAN91C111 Ethernet device included in the QEMU
 Versatile PB emulation however the driver is based on the equivalent Linux driver and as such
 includes (untested) support for a number of chip variants (see SMC91X_CHIP_* constants below).

 The QEMU emulation does not include support for a number of the features provided in the real
 chip so they are either currently not supported or are untested without access to a physical
 chip implementation for testing.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SMC91X;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Network,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {SMC91X specific constants}
 SMC91X_NETWORK_DESCRIPTION = 'SMSC 91C9x/91C1xx Ethernet';       {Description of SMC91X device}

 SMC91X_COMPLETION_THREAD_STACK_SIZE = SIZE_128K;                 {Stack size of receive and transmit completion thread}
 SMC91X_COMPLETION_THREAD_PRIORITY = THREAD_PRIORITY_HIGHER;      {Priority of receive and transmit completion thread}

 SMC91X_COMPLETION_THREAD_NAME = 'SMSC 91C9x/91C1xx Completion';  {Name of receive and transmit completion thread}

 SMC91X_COMPLETION_RECEIVE   = 0; {A receive message sent to the completion thread}
 SMC91X_COMPLETION_TRANSMIT  = 1; {A transmit message sent to the completion thread}
 SMC91X_COMPLETION_ALLOCATE  = 2; {An allocate message sent to the completion thread}
 SMC91X_COMPLETION_TERMINATE = 3; {A terminate message sent to the completion thread}

 SMC91X_MAX_TX_ENTRIES = SIZE_16;  {Number of Transmit buffers allocated}
 SMC91X_MAX_RX_ENTRIES = SIZE_256; {Number of Receive buffers allocated}

 SMC91X_MAX_PACKET_SIZE = 2048;

const
 {SMC91X Transmit Control register constants}
 SMC91X_TCR_ENABLE   = $0001; {Transmit enabled when set}
 SMC91X_TCR_LOOP     = $0002; {Loopback. General purpose output port used to control the LBK pin}
 SMC91X_TCR_FORCOL   = $0004; {When set, the FORCOL bit will force a collision by not deferring deliberately}
 SMC91X_TCR_PAD_EN   = $0080; {When set, will pad transmit frames shorter than 64 bytes with 00}
 SMC91X_TCR_NOCRC    = $0100; {Does not append CRC to transmitted frames when set}
 SMC91X_TCR_MON_CSN  = $0400; {When set monitors carrier while transmitting}
 SMC91X_TCR_FDUPLX   = $0800; {When set will cause frames to be received if they pass the address filter regardless of the source for the frame. When clear the node will not receive a frame sourced by itself}
 SMC91X_TCR_STP_SQET = $1000; {Stop transmission on SQET error. If this bit is set, will stop and disable the transmitter on SQE test error}
 SMC91X_TCR_EPH_LOOP = $2000; {Internal loopback at the EPH block}
 SMC91X_TCR_SWFDUP   = $8000; {Enables Switched Full Duplex mode}

 {Default settings}
 SMC91X_TCR_DEFAULT  = SMC91X_TCR_ENABLE or SMC91X_TCR_PAD_EN;
 SMC91X_TCR_CLEAR    = 0;

 {SMC91X EPH Status register constants (Ethernet Protocol Handler)}
 SMC91X_EPH_STATUS_TX_SUC   = $0001; {Last transmit was successful. Set if transmit completes without a fatal error}
 SMC91X_EPH_STATUS_SNGL_COL = $0002; {Single collision detected for the last transmit frame. Set when a collision is detected}
 SMC91X_EPH_STATUS_MUL_COL  = $0004; {Multiple collision detected for the last transmit frame. Set when more than one collision was experienced}
 SMC91X_EPH_STATUS_LTX_MULT = $0008; {Last transmit frame was a multicast. Set if frame was a multicast}
 SMC91X_EPH_STATUS_16COL    = $0010; {16 collisions reached. Set when 16 collisions are detected for a transmit frame}
 SMC91X_EPH_STATUS_SQET     = $0020; {Signal Quality Error Test}
 SMC91X_EPH_STATUS_LTXBRD   = $0040; {Last transmit frame was a broadcast. Set if frame was broadcast}
 SMC91X_EPH_STATUS_TXDEFR   = $0080; {Transmit Deferred. When set, carrier was detected during the first 6.4us of the inter frame gap}
 SMC91X_EPH_STATUS_LATCOL   = $0200; {Late collision detected on last transmit frame. If set a late collision was detected (later than 64 byte times into the frame)}
 SMC91X_EPH_STATUS_LOSTCARR = $0400; {Lost Carrier Sense. When set indicates that Carrier Sense was not present at end of preamble}
 SMC91X_EPH_STATUS_EXC_DEF  = $0800; {Excessive Deferral. When set last/current transmit was deferred for more than 1518 * 2 byte times}
 SMC91X_EPH_STATUS_CTR_ROL  = $1000; {Counter Roll Over. When set one or more 4 bit counters have reached maximum count (15)}
 SMC91X_EPH_STATUS_LINK_OK  = $4000; {General purpose input port driven by nLNK pin inverted}
 SMC91X_EPH_STATUS_TXUNRN   = $8000; {TX Underrun}

 {SMC91X Receive Control register constants}
 SMC91X_RCR_RX_ABORT  = $0001; {This bit is set if a receive frame was aborted due to length longer than 2K bytes. The frame will not be received. The bit is cleared by RESET or by the CPU writing it low}
 SMC91X_RCR_PRMS      = $0002; {Promiscuous mode. When set receives all frames. Does not receive its own transmission unless it is in Full Duplex}
 SMC91X_RCR_ALMUL     = $0004; {When set accepts all multicast frames (frames in which the first bit of DA is '1'). When clear accepts only the multicast frames that match the multicast table setting. Defaults low}
 SMC91X_RCR_RXEN      = $0100; {Enables the receiver when set. If cleared, completes receiving current frame and then goes idle. Defaults low on reset}
 SMC91X_RCR_STRIP_CRC = $0200; {When set, it strips the CRC on received frames. As a result, both the Byte Count and the frame format do not contain the CRC. When clear, the CRC is stored in memory following the packet. Defaults low}
 SMC91X_RCR_ABORT_ENB = $0200; {Enables abort of receive when collision occurs. Defaults low. When set, will automatically abort a packet being received when the appropriate collision input is activated}
 SMC91X_RCR_FILT_CAR  = $0400; {Filter Carrier. When set filters leading edge of carrier sense for 12 bit times (3 nibble times). Otherwise recognizes a receive frame as soon as carrier sense is active. (Does NOT filter RX DV on MII)}
 SMC91X_RCR_SOFTRST   = $8000; {Software-Activated Reset. Active high. Initiated by writing this bit high and terminated by writing the bit low}

 {Default settings}
 SMC91X_RCR_DEFAULT   = SMC91X_RCR_STRIP_CRC or SMC91X_RCR_RXEN;
 SMC91X_RCR_CLEAR     = 0;

 {SMC91X Receive/PHY Control register constants}
 SMC91X_RPC_SPEED     = $2000; {Speed select Input (1=100Mbps / 0=0Mbps)}
 SMC91X_RPC_DPLX      = $1000; {Duplex Select - This bit selects Full/Half Duplex operation (1=Full Duplex / 0=Half Duplex))}
 SMC91X_RPC_ANEG      = $0800; {Auto-Negotiation mode select}
 SMC91X_RPC_LS2A      = $0080; {LED select Signal Enable. These bits define what LED control signals are routed to the LEDA output pin on the Ethernet Controller. The default is 10/100 Link detected}
 SMC91X_RPC_LS1A      = $0040;
 SMC91X_RPC_LS0A      = $0020;
 SMC91X_RPC_LS2B      = $0010; {LED select Signal Enable. These bits define what LED control signals are routed to the LEDB output pin on the Ethernet Controller. The default is 10/100 Link detected}
 SMC91X_RPC_LS1B      = $0008;
 SMC91X_RPC_LS0B      = $0004;

 {Default Settings}
 SMC91X_RPC_DEFAULT   = SMC91X_RPC_ANEG or SMC91X_RPC_SPEED or SMC91X_RPC_DPLX;

 {RPC LS2A/LS1A/LS0A and LS2B/LS1B/LS0B values}
 SMC91X_RPC_LED_100_10 = $00; {LED = 100Mbps OR's with 10Mbps link detect}
 SMC91X_RPC_LED_RES    = $01; {LED = Reserved}
 SMC91X_RPC_LED_10     = $02; {LED = 10Mbps link detect}
 SMC91X_RPC_LED_FD     = $03; {LED = Full Duplex Mode}
 SMC91X_RPC_LED_TX_RX  = $04; {LED = TX or RX packet occurred}
 SMC91X_RPC_LED_100    = $05; {LED = 100Mbps link detect}
 SMC91X_RPC_LED_TX     = $06; {LED = TX packet occurred}
 SMC91X_RPC_LED_RX     = $07; {LED = RX packet occurred}

 {SMC91X Configuration register constants}
 SMC91X_CONFIG_EXT_PHY      = $0200; {External PHY Enabled (1=Enables the external MII / 0=Internal PHY is enabled)}
 SMC91X_CONFIG_GPCNTRL      = $0400; {This bit is a general purpose output port. Its inverse value drives pin nCNTRL and it is typically connected to a SELECT pin of the external PHY device such as a power enable}
 SMC91X_CONFIG_NO_WAIT      = $1000; {When set, does not request additional wait states. An exception to this are accesses to the Data Register if not ready for a transfer}
 SMC91X_CONFIG_EPH_POWER_EN = $8000; {Used to selectively power transition the EPH to a low power mode. When this bit is cleared (0), the Host will place the EPH into a low power mode}
 SMC91X_CONFIG_RESERVED     = $20B1; {Reserved values}

 {Default is powered-up, Internal Phy, Wait States, and pin nCNTRL=low}
 SMC91X_CONFIG_DEFAULT = SMC91X_CONFIG_EPH_POWER_EN;

 {SMC91X Control register constants}
 SMC91X_CTL_RCV_BAD       = $4000; {When set, bad CRC packets are received. When clear bad CRC packets do not generate interrupts and their memory is released}
 SMC91X_CTL_AUTO_RELEASE  = $0800; {When set, transmit pages are released by transmit completion if the transmission was successful (when TX_SUC is set)}
 SMC91X_CTL_LE_ENABLE     = $0080; {Link Error Enable. When set it enables the LINK_OK bit transition as one of the interrupts merged into the EPH INT bit}
 SMC91X_CTL_CR_ENABLE     = $0040; {Counter Roll over Enable. When set, it enables the CTR_ROL bit as one of the interrupts merged into the EPH INT bit}
 SMC91X_CTL_TE_ENABLE     = $0020; {Transmit Error Enable. When set it enables Transmit Error as one of the interrupts merged into the EPH INT bit}
 SMC91X_CTL_EEPROM_SELECT = $0004; {This bit allows the CPU to specify which registers the EEPROM RELOAD or STORE refers to}
 SMC91X_CTL_RELOAD        = $0002; {When set it will read the EEPROM and update relevant registers with its contents}
 SMC91X_CTL_STORE         = $0001; {When set, stores the contents of all relevant registers in the serial EEPROM}

 {SMC91X MMU Command register constants}
 SMC91X_MMU_CMD_BUSY      = 1; {When 1 the last command has not completed}
 SMC91X_MMU_CMD_NOP       = (0 shl 5); {NOOP - NO OPERATION}
 SMC91X_MMU_CMD_ALLOC     = (1 shl 5); {ALLOCATE MEMORY FOR TX (OR with number of 256 byte packets)}
 SMC91X_MMU_CMD_RESET     = (2 shl 5); {RESET MMU TO INITIAL STATE (Frees all memory allocations, clears relevant interrupts, resets packet FIFO pointers)}
 SMC91X_MMU_CMD_REMOVE    = (3 shl 5); {REMOVE FRAME FROM TOP OF RX FIFO (To be issued after CPU has completed processing of present receive frame)}
 SMC91X_MMU_CMD_RELEASE   = (4 shl 5); {REMOVE AND RELEASE TOP OF RX FIFO (Like 3) but also releases all memory used by the packet presently at the RX FIFO output)}
 SMC91X_MMU_CMD_FREEPKT   = (5 shl 5); {RELEASE SPECIFIC PACKET (Frees all pages allocated to the packet specified in the PACKET NUMBER REGISTER)}
 SMC91X_MMU_CMD_ENQUEUE   = (6 shl 5); {ENQUEUE PACKET NUMBER INTO TX FIFO (This is the normal method of transmitting a packet just loaded into RAM)}
 SMC91X_MMU_CMD_RSTTXFIFO = (7 shl 5); {RESET TX FIFOs (This command will reset both TX FIFOs)}

 {SMC91X Allocation Result register constants}
 SMC91X_AR_FAILED = $80; {FAILED - A zero indicates a successful allocation completion. If the allocation fails the bit is set and only cleared when the pending allocation is satisfied}

 {SMC91X TX FIFO Ports register constants}
 SMC91X_TXFIFO_TEMPTY = $80; {TEMPTY - No transmit packets in completion queue. For polling purposes, uses the TX_INT bit in the Interrupt Status Register}

 {SMC91X RX FIFO Ports register constants}
 SMC91X_RXFIFO_REMPTY = $80; {REMPTY - No receive packets queued in the RX FIFO. For polling purposes, uses the RCV_INT bit in the Interrupt Status Register}

 {SMC91X Pointer register constants}
 SMC91X_PTR_RCV     = $8000; {When RCV is set the address refers to the receive area and uses the output of RX FIFO as the packet number, when RCV is clear the address refers to the transmit area and uses the packet number at the Packet Number Register}
 SMC91X_PTR_AUTOINC = $4000; {It will auto-increment on accesses to the data register when AUTO INCR is set. The increment is by one for every byte access, by two for every word access, and by four for every double word access}
 SMC91X_PTR_READ    = $2000; {Determines the type of access to follow. If the READ bit is high the operation intended is a read}

 {SMC91X Interrupt Mask register constants}
 SMC91X_IM_MDINT        = $80; {PHY MI Register 18 Interrupt}
 SMC91X_IM_ERCV_INT     = $40; {Early Receive Interrupt}
 SMC91X_IM_EPH_INT      = $20; {Set by Ethernet Protocol Handler section}
 SMC91X_IM_RX_OVRN_INT  = $10; {Set by Receiver Overruns}
 SMC91X_IM_ALLOC_INT    = $08; {Set when allocation request is completed}
 SMC91X_IM_TX_EMPTY_INT = $04; {Set if the TX FIFO goes empty}
 SMC91X_IM_TX_INT       = $02; {Transmit Interrupt}
 SMC91X_IM_RCV_INT      = $01; {Receive Interrupt}

 {SMC91X Management Interface register constants}
 SMC91X_MII_MSK_CRS100 = $4000; {Disables CRS100 detection during tx half dup}
 SMC91X_MII_MDOE       = $0008; {MII Output Enable}
 SMC91X_MII_MCLK       = $0004; {MII Clock, pin MDCLK}
 SMC91X_MII_MDI        = $0002; {MII Input, pin MDI}
 SMC91X_MII_MDO        = $0001; {MII Output, pin MDO}

 SMC91X_MII_DELAY = 1; {The MII clock high/low times. 2 x this number gives the MII clock period in microseconds}

 {SMC91X Receive register constants}
 SMC91X_RCV_RCV_DISCRD = $0080; {Set to discard a packet being received. Will discard packets only in the process of being received}
 SMC91X_RCV_THRESHOLD  = $001F; {RCV Threshold Mask}

 {SMC91X Receive status constants}
 SMC91X_RCV_ALGNERR   = $8000; {Frame has alignment error}
 SMC91X_RCV_BRODCAST  = $4000; {Receive frame was Broadcast (The Multicast bit may also be set, software must ignore the Multicast bit for a Broadcast packet)}
 SMC91X_RCV_BADCRC    = $2000; {Frame has CRC error or RX_ER was asserted during reception}
 SMC91X_RCV_ODDFRAME  = $1000; {This bit when set indicates that the received frame has an odd number of bytes}
 SMC91X_RCV_TOOLONG   = $0800; {Frame length was longer than 802.3 maximum size (1518 bytes on the cable)}
 SMC91X_RCV_TOOSHORT  = $0400; {Frame length was shorter than 802.3 minimum size (64 bytes on the cable)}
 SMC91X_RCV_MULTICAST = $0001; {Receive frame was Multicast}
 SMC91X_RCV_ERRORS    = (SMC91X_RCV_ALGNERR or SMC91X_RCV_BADCRC or SMC91X_RCV_TOOLONG or SMC91X_RCV_TOOSHORT);

 {SMC91X Bank Select register constants}
 SMC91X_BANK_SELECT_0 = 0;
 SMC91X_BANK_SELECT_1 = 1;
 SMC91X_BANK_SELECT_2 = 2;
 SMC91X_BANK_SELECT_3 = 3;

const
 {SMC91X Chip constants (Bits 4-7 of the Revision register)}
 SMC91X_CHIP_COUNT   = 16;

 SMC91X_CHIP_9192    = 3;
 SMC91X_CHIP_9194    = 4;
 SMC91X_CHIP_9195    = 5;
 SMC91X_CHIP_9196    = 6;
 SMC91X_CHIP_91100   = 7;
 SMC91X_CHIP_91100FD = 8;
 SMC91X_CHIP_91111FD = 9;

 SMC91X_CHIP_NAMES:array[0..SMC91X_CHIP_COUNT - 1] of String = (
  '',
  '',
  '',
  'SMC91C90/91C92', {3}
  'SMC91C94',       {4}
  'SMC91C95',       {5}
  'SMC91C96',       {6}
  'SMC91C100',      {7}
  'SMC91C100FD',    {8}
  'SMC91C11xFD',    {9}
  '',
  '',
  '',
  '',
  '',
  '');

const
 {SMC91X PHY IDs}
 SMC91X_PHY_LAN83C183 = $0016f840; {LAN83C183 = LAN91C111 Internal PHY}
 SMC91X_PHY_LAN83C180 = $02821c50;

 {SMC91X PHY Register Addresses (LAN91C111 Internal PHY)}
 {Register 0. Control Register}
 {See: Network Generic MII registers (MII_BMCR)}

 {Register 1. Status Register}
 {See: Network Generic MII registers (MII_BMSR)}

 {Register 2 & 3. PHY Identifier Register}
 {See: Network Generic MII registers (MII_PHYSID1 / MII_PHYSID2)}

 {Register 4. Auto-Negotiation Advertisement Register}
 {See: Network Generic MII registers (MII_ADVERTISE)}

 {Register 5. Auto-Negotiation Remote End Capability Register}
 {See: Network Generic MII registers (MII_LPA)}

 {Register 16. Configuration 1- Structure and Bit Definition}
 SMC91X_PHY_CFG1_REG = $10;
 SMC91X_PHY_CFG1_LNKDIS  = $8000;   {1=Rx Link Detect Function disabled}
 SMC91X_PHY_CFG1_XMTDIS  = $4000;   {1=TP Transmitter Disabled}
 SMC91X_PHY_CFG1_XMTPDN  = $2000;   {1=TP Transmitter Powered Down}
 SMC91X_PHY_CFG1_BYPSCR  = $0400;   {1=Bypass scrambler/descrambler}
 SMC91X_PHY_CFG1_UNSCDS  = $0200;   {1=Unscramble Idle Reception Disable}
 SMC91X_PHY_CFG1_EQLZR   = $0100;   {1=Rx Equalizer Disabled}
 SMC91X_PHY_CFG1_CABLE   = $0080;   {1=STP(150ohm), 0=UTP(100ohm)}
 SMC91X_PHY_CFG1_RLVL0   = $0040;   {1=Rx Squelch level reduced by 4.5db}
 SMC91X_PHY_CFG1_TLVL_SHIFT = 2;    {Transmit Output Level Adjust}
 SMC91X_PHY_CFG1_TLVL_MASK = $003C;
 SMC91X_PHY_CFG1_TRF_MASK = $0003;  {Transmitter Rise/Fall time}

 {Register 17. Configuration 2 - Structure and Bit Definition}
 SMC91X_PHY_CFG2_REG = $11;
 SMC91X_PHY_CFG2_APOLDIS = $0020; {1=Auto Polarity Correction disabled}
 SMC91X_PHY_CFG2_JABDIS  = $0010; {1=Jabber disabled}
 SMC91X_PHY_CFG2_MREG    = $0008; {1=Multiple register access (MII mgt)}
 SMC91X_PHY_CFG2_INTMDIO = $0004; {1=Interrupt signaled with MDIO pulseo}

 {Register 18. Status Output - Structure and Bit Definition}
 SMC91X_PHY_INT_REG = $12;        {Status Output (Interrupt Status)}
 SMC91X_PHY_INT_INT      = $8000; {1=bits have changed since last read}
 SMC91X_PHY_INT_LNKFAIL  = $4000; {1=Link Not detected}
 SMC91X_PHY_INT_LOSSSYNC = $2000; {1=Descrambler has lost sync}
 SMC91X_PHY_INT_CWRD     = $1000; {1=Invalid 4B5B code detected on rx}
 SMC91X_PHY_INT_SSD      = $0800; {1=No Start Of Stream detected on rx}
 SMC91X_PHY_INT_ESD      = $0400; {1=No End Of Stream detected on rx}
 SMC91X_PHY_INT_RPOL     = $0200; {1=Reverse Polarity detected}
 SMC91X_PHY_INT_JAB      = $0100; {1=Jabber detected}
 SMC91X_PHY_INT_SPDDET   = $0080; {1=100Base-TX mode, 0=10Base-T mode}
 SMC91X_PHY_INT_DPLXDET  = $0040; {1=Device in Full Duplex}

 {Register 19. Mask - Structure and Bit Definition}
 SMC91X_PHY_MASK_REG  = $13; {Interrupt Mask} {Uses the same bit definitions as PHY_INT_REG}

 {Register 20. Reserved - Structure and Bit Definition}
 {Nothing}

{==============================================================================}
type
 {SMC91X specific types}
 {Layout of the SMC91X registers}
 TSMC91XBank0Registers = packed record
  TCR:Word;        {Transmit Control Register}
  EPH_STATUS:Word; {EPH Status Register}
  RCR:Word;        {Receive Control Register}
  COUNTER:Word;    {Counter Register}
  MIR:Word;        {Memory Information Register}
  RPCR:Word;       {Receive/Phy Control Register}
  RESERVED:Word;   {Reserved}
  BANK:Word;       {Bank Select Register}
 end;

 TSMC91XBank1Registers = packed record
  CONFIG:Word; {Configuration Register}
  BASE:Word;   {Base Address Register}
  ADDR0:Word;  {Individual Address Registers (0 and 1)}
  ADDR1:Word;  {Individual Address Registers (2 and 3)}
  ADDR2:Word;  {Individual Address Registers (4 and 5)}
  GP:Word;     {General Purpose Register}
  CTL:Word;    {Control Register}
  BANK:Word;   {Bank Select Register}
 end;

 TSMC91XFIFORegisters = packed record
  case Integer of
   0:(FIFO:Word);
   1:(TX:Byte;
      RX:Byte);
 end;

 TSMC91XDATARegisters = packed record
  case Integer of
   0:(DATA:LongWord);
   1:(DATAH:Word;
      DATAL:Word);
   2:(DATA0:Byte;
      DATA1:Byte;
      DATA2:Byte;
      DATA3:Byte);
 end;

 TSMC91XBank2Registers = packed record
  MMU_CMD:Word;              {MMU Command Register}
  PN:Byte;                   {Packet Number Register}
  AR:Byte;                   {Allocation Result Register}
  FIFO:TSMC91XFIFORegisters; {FIFO Ports Register}
  PTR:Word;                  {Pointer Register}
  DATA:TSMC91XDATARegisters; {Data Register}
  INT:Byte;                  {Interrupt Status Register}
  IM:Byte;                   {Interrupt Mask Register}
  BANK:Word;                 {Bank Select Register}
 end;

 TSMC91XBank3Registers = packed record
  MCAST1:Word; {Multicast Table Registers (0 and 1)}
  MCAST2:Word; {Multicast Table Registers (2 and 3)}
  MCAST3:Word; {Multicast Table Registers (4 and 5)}
  MCAST4:Word; {Multicast Table Registers (6 and 7)}
  MII:Word;    {Management Interface Register}
  REV:Word;    {Revision Register}
  RCV:Word;    {RCV Register}
  Bank:Word;   {Bank Select Register}
 end;

 PSMC91XRegisters = ^TSMC91XRegisters;
 TSMC91XRegisters = packed record
   case Integer of
    0:(Bank0:TSMC91XBank0Registers); {Bank 0 Registers}
    1:(Bank1:TSMC91XBank1Registers); {Bank 1 Registers}
    2:(Bank2:TSMC91XBank2Registers); {Bank 2 Registers}
    3:(Bank3:TSMC91XBank3Registers); {Bank 3 Registers}
 end;

 PSMC91XNetwork = ^TSMC91XNetwork;
 TSMC91XNetwork = record
  {Network Properties}
  Network:TNetworkDevice;
  {SMC91X Properties}
  IRQ:LongWord;
  Lock:TSpinHandle;                                                 {Device lock (Differs from lock in Network device) (Spin lock due to use by interrupt handler)}
  Thread:TThreadHandle;                                             {Thread for handling packet receive and transmit completion}
  Start:LongWord;                                                   {First receive entry available for incoming packet}
  Count:LongWord;                                                   {Number of receive entries available for incoming packets}
  Entries:array[0..(SMC91X_MAX_RX_ENTRIES - 1)] of PNetworkEntry;   {Queue of receive entries for handling incoming packets}
  Registers:PSMC91XRegisters;                                       {Device registers}
  Revision:Word;                                                    {Device revision}
  PHYId:LongWord;                                                   {Physical Interface (PHY) Address}
  PHYType:LongWord;                                                 {Physical Interface (PHY) Type}
  TCRFlags:LongWord;                                                {Current Transmit Control Register (TCR) flags}
  RCRFlags:LongWord;                                                {Current Receive Control Register (RCR) flags}
  RPCFlags:LongWord;                                                {Current Receive/PHY Control Register (RPC) flags}
  {Statistics Properties}
  InterruptCount:LongWord;                                          {Number of interrupt requests received by the device}
  CollisionCount:LongWord;                                          {Number of transmit collisions detected by the device}
 end;

{==============================================================================}
var
 {SMC91X specific variables}
 SMC91X_ALLOCATE_WAIT_COUNT:LongWord = 16;  {How long to wait for the SMC91X to allocate memory before deferring a packet}
 SMC91X_MAX_IRQ_COUNT:LongWord = 8;         {How many iterations of the interrupt handler are allowed on each interrupt}
 SMC91X_THROTTLE_TRANSMIT:Boolean = False;  {Determines if TX packets are sent one at a time or queued into SMC91X internal memory}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{SMC91X Functions}
function SMC91XNetworkCreate(Address:PtrUInt;const Name:String;IRQ:LongWord):PNetworkDevice;{$IFDEF API_EXPORT_SMC91X} stdcall; public name 'smc91x_network_create';{$ENDIF}

function SMC91XNetworkDestroy(Network:PNetworkDevice):LongWord;{$IFDEF API_EXPORT_SMC91X} stdcall; public name 'smc91x_network_destroy';{$ENDIF}

{==============================================================================}
{SMC91X Network Functions}
function SMC91XNetworkOpen(Network:PNetworkDevice):LongWord;
function SMC91XNetworkClose(Network:PNetworkDevice):LongWord;
function SMC91XNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function SMC91XBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function SMC91XBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
function SMC91XBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function SMC91XBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;

procedure SMC91XInterruptHandler(Network:PSMC91XNetwork);
procedure SMC91XInterruptReceive(Network:PSMC91XNetwork);
procedure SMC91XInterruptTransmit(Network:PSMC91XNetwork);

function SMC91XCompletionExecute(Network:PSMC91XNetwork):PtrInt;

procedure SMC91XTransmitStart(Network:PSMC91XNetwork);
procedure SMC91XTransmitPacket(Network:PSMC91XNetwork;Entry:PNetworkEntry;Packet:PNetworkPacket);

{==============================================================================}
{SMC91X Helper Functions}
function SMC91XCurrentBank(Network:PSMC91XNetwork):Word; inline;
procedure SMC91XSelectBank(Network:PSMC91XNetwork;Bank:Byte); inline;

function SMC91XGetBase(Network:PSMC91XNetwork):Word; inline;
function SMC91XGetRevision(Network:PSMC91XNetwork):Word; inline;

procedure SMC91XWaitMMUBusy(Network:PSMC91XNetwork); inline;

procedure SMC91XSetMMUCommand(Network:PSMC91XNetwork;Command:Word); inline;
procedure SMC91XSetInterruptMask(Network:PSMC91XNetwork;Mask:Byte); inline;

procedure SMC91XEnableInterrupt(Network:PSMC91XNetwork;Interrupt:Byte);
procedure SMC91XDisableInterrupt(Network:PSMC91XNetwork;Interrupt:Byte);

function SMC91XReset(Network:PSMC91XNetwork):LongWord;

function SMC91XEnable(Network:PSMC91XNetwork):LongWord;
function SMC91XShutdown(Network:PSMC91XNetwork):LongWord;

function SMC91XGetMACAddress(Network:PSMC91XNetwork;Address:PHardwareAddress):LongWord;
function SMC91XSetMACAddress(Network:PSMC91XNetwork;Address:PHardwareAddress):LongWord;

function SMC91XMIIIn(Network:PSMC91XNetwork;Bits:LongWord):LongWord;
procedure SMC91XMIIOut(Network:PSMC91XNetwork;Value,Bits:LongWord);

function SMC91XPHYRead(Network:PSMC91XNetwork;Addr,Reg:LongWord):LongWord;
procedure SMC91XPHYWrite(Network:PSMC91XNetwork;Addr,Reg,Data:LongWord);

function SMC91XPHYDetect(Network:PSMC91XNetwork):LongWord;
function SMC91XPHYReset(Network:PSMC91XNetwork):LongWord;
function SMC91XPHYFixed(Network:PSMC91XNetwork):LongWord;
function SMC91XPHYConfigure(Network:PSMC91XNetwork):LongWord;
function SMC91XPHYPowerdown(Network:PSMC91XNetwork):LongWord;
function SMC91XPHYCheckMedia(Network:PSMC91XNetwork):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {SMC91X specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{SMC91X Functions}
function SMC91XNetworkCreate(Address:PtrUInt;const Name:String;IRQ:LongWord):PNetworkDevice;{$IFDEF API_EXPORT_SMC91X} stdcall;{$ENDIF}
{Create and register a new SMC91X Network device which can be accessed using the Network API}
{Address: The address of the SMC91X registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ: The interrupt number for the SMC91X}
{Return: Pointer to the new Network device or nil if the Network device could not be created}
var
 Status:LongWord;
 SMC91XNetwork:PSMC91XNetwork;
begin
 {}
 Result:=nil;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'SMC91X: Network Create (Address=' + AddrToHex(Address) + ' Name=' + Name + ' IRQ=' + IntToStr(IRQ) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;

 {Check IRQ}
 {if IRQ = 0 then Exit;} {IRQ 0 is valid}

 {Create Network}
 SMC91XNetwork:=PSMC91XNetwork(NetworkDeviceCreateEx(SizeOf(TSMC91XNetwork)));
 if SMC91XNetwork <> nil then
  begin
   {Update Network}
   {Device}
   SMC91XNetwork.Network.Device.DeviceBus:=DEVICE_BUS_MMIO;
   SMC91XNetwork.Network.Device.DeviceType:=NETWORK_TYPE_ETHERNET;
   SMC91XNetwork.Network.Device.DeviceFlags:=NETWORK_FLAG_RX_BUFFER or NETWORK_FLAG_TX_BUFFER;
   SMC91XNetwork.Network.Device.DeviceData:=nil;
   if Length(Name) <> 0 then SMC91XNetwork.Network.Device.DeviceDescription:=Name else SMC91XNetwork.Network.Device.DeviceDescription:=SMC91X_NETWORK_DESCRIPTION;
   {Network}
   SMC91XNetwork.Network.NetworkState:=NETWORK_STATE_CLOSED;
   SMC91XNetwork.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
   SMC91XNetwork.Network.DeviceOpen:=SMC91XNetworkOpen;
   SMC91XNetwork.Network.DeviceClose:=SMC91XNetworkClose;
   SMC91XNetwork.Network.DeviceControl:=SMC91XNetworkControl;
   SMC91XNetwork.Network.BufferAllocate:=SMC91XBufferAllocate;
   SMC91XNetwork.Network.BufferRelease:=SMC91XBufferRelease;
   SMC91XNetwork.Network.BufferReceive:=SMC91XBufferReceive;
   SMC91XNetwork.Network.BufferTransmit:=SMC91XBufferTransmit;
   {SMC91X}
   SMC91XNetwork.IRQ:=IRQ;
   SMC91XNetwork.Lock:=INVALID_HANDLE_VALUE;
   SMC91XNetwork.Thread:=INVALID_HANDLE_VALUE;
   SMC91XNetwork.Start:=0;
   SMC91XNetwork.Count:=0;
   SMC91XNetwork.Registers:=PSMC91XRegisters(Address);
   SMC91XNetwork.TCRFlags:=SMC91X_TCR_DEFAULT;
   SMC91XNetwork.RCRFlags:=SMC91X_RCR_DEFAULT;
   SMC91XNetwork.RPCFlags:=SMC91X_RPC_DEFAULT;

   {Register Network}
   Status:=NetworkDeviceRegister(@SMC91XNetwork.Network);
   if Status = ERROR_SUCCESS then
    begin
     {Return Result}
     Result:=PNetworkDevice(SMC91XNetwork);
    end
   else
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(nil,'SMC91X: Failed to register new Network device: ' + ErrorToString(Status));

     {Destroy Network}
     NetworkDeviceDestroy(@SMC91XNetwork.Network);
    end;
  end
 else
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'SMC91X: Failed to create new Network device');
  end;
end;

{==============================================================================}

function SMC91XNetworkDestroy(Network:PNetworkDevice):LongWord;{$IFDEF API_EXPORT_SMC91X} stdcall;{$ENDIF}
{Close, deregister and destroy an SMC91X Network device created by this driver}
{Network: The Network device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Network Destroy');
 {$ENDIF}

 {Close Network}
 NetworkDeviceClose(Network);

 {Deregister Network}
 Result:=NetworkDeviceDeregister(Network);
 if Result = ERROR_SUCCESS then
  begin
   {Destroy Network}
   Result:=NetworkDeviceDestroy(Network);
   if Result <> ERROR_SUCCESS then
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(nil,'SMC91X: Failed to destroy Network device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'SMC91X: Failed to deregister Network device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{SMC91X Network Functions}
function SMC91XNetworkOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen API for SMC91X Network}
{Note: Not intended to be called directly by applications, use NetworkDeviceOpen instead}
var
 Value:Word;
 Revision:Word;
 ChipName:String;
 Message:TMessage;
 Entry:PNetworkEntry;
 Address:THardwareAddress;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Network Open');
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

    {Setup Defaults}
    PSMC91XNetwork(Network).Lock:=INVALID_HANDLE_VALUE;
    PSMC91XNetwork(Network).Thread:=INVALID_HANDLE_VALUE;
    PSMC91XNetwork(Network).Start:=0;
    PSMC91XNetwork(Network).Count:=0;
    PSMC91XNetwork(Network).TCRFlags:=SMC91X_TCR_DEFAULT;
    PSMC91XNetwork(Network).RCRFlags:=SMC91X_RCR_DEFAULT;
    PSMC91XNetwork(Network).RPCFlags:=SMC91X_RPC_DEFAULT;

    {Check Bank Select Register}
    Value:=SMC91XCurrentBank(PSMC91XNetwork(Network));
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Current Bank Select = ' + IntToHex(Value,4));
    {$ENDIF}
    if (Value and $FF00) <> $3300 then Exit;

    {Set and Check Bank Select Register}
    SMC91XSelectBank(PSMC91XNetwork(Network),SMC91X_BANK_SELECT_0);
    Value:=SMC91XCurrentBank(PSMC91XNetwork(Network));
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Bank Select Register = ' + IntToHex(Value,4));
    {$ENDIF}
    if (Value and $FF00) <> $3300 then Exit;

    {Get Base Addres Register}
    SMC91XSelectBank(PSMC91XNetwork(Network),SMC91X_BANK_SELECT_1);
    Value:=SMC91XGetBase(PSMC91XNetwork(Network));
    Value:=(Value and $1F00) shr 3;
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Base Address Register = ' + IntToHex(Value,4));
    {$ENDIF}
    if PtrUInt(PSMC91XNetwork(Network).Registers) and $3e0 <> Value then
     begin
      //To Do //Error ?
     end;

    {Get Revision Register}
    SMC91XSelectBank(PSMC91XNetwork(Network),SMC91X_BANK_SELECT_3);
    Revision:=SMC91XGetRevision(PSMC91XNetwork(Network));
    ChipName:=SMC91X_CHIP_NAMES[(Revision shr 4) and $F];
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Revision Register = ' + IntToHex(Revision,4));
    if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Chip Name = ' + ChipName);
    {$ENDIF}
    if (Revision and $FF00) <> $3300 then Exit;
    if Length(ChipName) = 0 then Exit;

    {Save Revision}
    PSMC91XNetwork(Network).Revision:=(Revision and $FF);

    {Check MAC address}
    if SMC91XGetMACAddress(PSMC91XNetwork(Network),@Address) <> ERROR_SUCCESS then Exit;
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  MAC Address = ' + HardwareAddressToString(Address));
    {$ENDIF}
    if CompareHardwareBroadcast(Address) or CompareHardwareDefault(Address) then Exit;

    {Reset Device}
    if SMC91XReset(PSMC91XNetwork(Network)) <> ERROR_SUCCESS then Exit;

    {Locate the PHY}
    if PSMC91XNetwork(Network).Revision >= (SMC91X_CHIP_91100 shl 4) then
     begin
      SMC91XPHYDetect(PSMC91XNetwork(Network));

      {QEMU does not implement MII}
      if PSMC91XNetwork(Network).PHYType = $FFFFFFFF then
       begin
        PSMC91XNetwork(Network).PHYType:=0;
       end;
     end;

    {Check PHY Type}
    if PSMC91XNetwork(Network).PHYType = 0 then
     begin
      {Enable Carrier Monitor}
      PSMC91XNetwork(Network).TCRFlags:=PSMC91XNetwork(Network).TCRFlags or SMC91X_TCR_MON_CSN;
     end;

    try
     {Allocate Receive Queue Buffer}
     Network.ReceiveQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),SMC91X_MAX_RX_ENTRIES);
     if Network.ReceiveQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to create receive queue buffer');

       Exit;
      end;

     {Allocate Receive Queue Semaphore}
     Network.ReceiveQueue.Wait:=SemaphoreCreate(0);
     if Network.ReceiveQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to create receive queue semaphore');

       Exit;
      end;

     {Allocate Receive Queue Buffers}
     Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=SMC91X_MAX_PACKET_SIZE;
       Entry.Offset:=0;
       Entry.Count:=1;

       {Allocate Receive Buffer}
       if DMAAvailable then
        begin
         Entry.Buffer:=DMAAllocateBuffer(Entry.Size);
        end
       else
        begin
         Entry.Buffer:=AllocMem(Entry.Size);
        end;
       if Entry.Buffer = nil then
        begin
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to allocate receive buffer');

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
     SetLength(Network.ReceiveQueue.Entries,SMC91X_MAX_RX_ENTRIES);

     {Allocate Transmit Queue Buffer}
     Network.TransmitQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),SMC91X_MAX_TX_ENTRIES);
     if Network.TransmitQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to create transmit queue buffer');

       Exit;
      end;

     {Allocate Transmit Queue Semaphore}
     Network.TransmitQueue.Wait:=SemaphoreCreate(SMC91X_MAX_TX_ENTRIES);
     if Network.TransmitQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to create transmit queue semaphore');

       Exit;
      end;

     {Allocate Transmit Queue Buffers}
     Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=SMC91X_MAX_PACKET_SIZE;
       Entry.Offset:=0;
       Entry.Count:=1;

       {Allocate Transmit Buffer}
       if DMAAvailable then
        begin
         Entry.Buffer:=DMAAllocateBuffer(Entry.Size);
        end
       else
        begin
         Entry.Buffer:=AllocMem(Entry.Size);
        end;
       if Entry.Buffer = nil then
        begin
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to allocate transmit buffer');

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

     {Allocate Transmit Queue Entries}
     SetLength(Network.TransmitQueue.Entries,SMC91X_MAX_TX_ENTRIES);

     {Allocate Lock}
     PSMC91XNetwork(Network).Lock:=SpinCreate;
     if PSMC91XNetwork(Network).Lock = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to create device lock');

       Exit;
      end;

     {Create Thread}
     PSMC91XNetwork(Network).Thread:=BeginThread(TThreadStart(SMC91XCompletionExecute),Network,PSMC91XNetwork(Network).Thread,SMC91X_COMPLETION_THREAD_STACK_SIZE);
     if PSMC91XNetwork(Network).Thread = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'SMC91X: Failed to create completion thread');

       Exit;
      end
     else
      begin
       ThreadSetPriority(PSMC91XNetwork(Network).Thread,SMC91X_COMPLETION_THREAD_PRIORITY);
       ThreadSetName(PSMC91XNetwork(Network).Thread,SMC91X_COMPLETION_THREAD_NAME);
      end;

     {Get Buffer (Receive Buffer)}
     Entry:=BufferGetEx(Network.ReceiveQueue.Buffer,0); {Do not wait}
     while Entry <> nil do
      begin
       {Add Entry}
       PSMC91XNetwork(Network).Entries[(PSMC91XNetwork(Network).Start + PSMC91XNetwork(Network).Count) mod SMC91X_MAX_RX_ENTRIES]:=Entry;

       {Update Count}
       Inc(PSMC91XNetwork(Network).Count);

       {Get Buffer (Receive Buffer)}
       Entry:=BufferGetEx(Network.ReceiveQueue.Buffer,0); {Do not wait}
      end;

     {Enable Device}
     if SMC91XEnable(PSMC91XNetwork(Network)) <> ERROR_SUCCESS then Exit;

     {Check PHY Type}
     if PSMC91XNetwork(Network).PHYType <> 0 then
      begin
       {Configure the PHY}
       //To Do //smc_phy_configure //Not supported in QEMU ?
      end
     else
      begin
       {Check Media}
       //To Do //smc_10bt_check_media //Not supported in QEMU ?
      end;

     {Request IRQ}
     RequestIRQ(IRQ_ROUTING,PSMC91XNetwork(Network).IRQ,TInterruptHandler(SMC91XInterruptHandler),Network);

     {Set State to Open}
     Network.NetworkState:=NETWORK_STATE_OPEN;

     {Notify the State}
     NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_OPEN);

     {Check PHY Type}
     if PSMC91XNetwork(Network).PHYType <> 0 then
      begin
       {Get Network Status}
       //To Do //SMC91XPHYRead //Not supported in QEMU ?
      end
     else
      begin
       {QEMU does not implement MII or EPH}
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
       {Check Thread}
       if PSMC91XNetwork(Network).Thread <> INVALID_HANDLE_VALUE then
        begin
         {Terminate Thread}
         Message.Msg:=PtrUInt(nil);
         Message.wParam:=SMC91X_COMPLETION_TERMINATE;
         ThreadSendMessage(PSMC91XNetwork(Network).Thread,Message);

         {Wait for Thread}
         ThreadWaitTerminate(PSMC91XNetwork(Network).Thread,INFINITE);
         PSMC91XNetwork(Network).Thread:=INVALID_HANDLE_VALUE;
        end;

       {Check Count}
       while PSMC91XNetwork(Network).Count > 0 do
        begin
         {Get Entry}
         Entry:=PSMC91XNetwork(Network).Entries[PSMC91XNetwork(Network).Start];

         {Update Start}
         PSMC91XNetwork(Network).Start:=(PSMC91XNetwork(Network).Start + 1) mod SMC91X_MAX_RX_ENTRIES;

         {Update Count}
         Dec(PSMC91XNetwork(Network).Count);

         {Free Buffer (Receive Buffer)}
         BufferFree(Entry);
        end;

       {Check Lock}
       if PSMC91XNetwork(Network).Lock <> INVALID_HANDLE_VALUE then
        begin
         {Destroy Lock}
         SpinDestroy(PSMC91XNetwork(Network).Lock);
         PSMC91XNetwork(Network).Lock:=INVALID_HANDLE_VALUE;
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
           {Deallocate Transmit Buffer}
           if DMAAvailable then
            begin
             DMAReleaseBuffer(Entry.Buffer);
            end
           else
            begin
             FreeMem(Entry.Buffer);
            end;

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
           {Deallocate Receive Buffer}
           if DMAAvailable then
            begin
             DMAReleaseBuffer(Entry.Buffer);
            end
           else
            begin
             FreeMem(Entry.Buffer);
            end;

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

function SMC91XNetworkClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose API for SMC91X Network}
{Note: Not intended to be called directly by applications, use NetworkDeviceClose instead}
var
 Message:TMessage;
 Entry:PNetworkEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Network Close');
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
    {Set State to Closed}
    Network.NetworkState:=NETWORK_STATE_CLOSED;

    {Notify the State}
    NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_CLOSE);

    {Disable Device}
    SMC91XShutdown(PSMC91XNetwork(Network));

    {Release IRQ}
    ReleaseIRQ(IRQ_ROUTING,PSMC91XNetwork(Network).IRQ,TInterruptHandler(SMC91XInterruptHandler),Network);

    {Check Thread}
    if PSMC91XNetwork(Network).Thread <> INVALID_HANDLE_VALUE then
     begin
      {Terminate Thread}
      Message.Msg:=PtrUInt(nil);
      Message.wParam:=SMC91X_COMPLETION_TERMINATE;
      ThreadSendMessage(PSMC91XNetwork(Network).Thread,Message);

      {Wait for Thread}
      ThreadWaitTerminate(PSMC91XNetwork(Network).Thread,INFINITE);
      PSMC91XNetwork(Network).Thread:=INVALID_HANDLE_VALUE;
     end;

    {Check Count}
    while PSMC91XNetwork(Network).Count > 0 do
     begin
      {Get Entry}
      Entry:=PSMC91XNetwork(Network).Entries[PSMC91XNetwork(Network).Start];

      {Update Start}
      PSMC91XNetwork(Network).Start:=(PSMC91XNetwork(Network).Start + 1) mod SMC91X_MAX_RX_ENTRIES;

      {Update Count}
      Dec(PSMC91XNetwork(Network).Count);

      {Free Buffer (Receive Buffer)}
      BufferFree(Entry);
     end;

    {Check Lock}
    if PSMC91XNetwork(Network).Lock <> INVALID_HANDLE_VALUE then
     begin
      {Destroy Lock}
      SpinDestroy(PSMC91XNetwork(Network).Lock);
      PSMC91XNetwork(Network).Lock:=INVALID_HANDLE_VALUE;
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
        {Deallocate Transmit Buffer}
        if DMAAvailable then
         begin
          DMAReleaseBuffer(Entry.Buffer);
         end
        else
         begin
          FreeMem(Entry.Buffer);
         end;

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
        {Deallocate Receive Buffer}
        if DMAAvailable then
         begin
          DMAReleaseBuffer(Entry.Buffer);
         end
        else
         begin
          FreeMem(Entry.Buffer);
         end;

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

function SMC91XNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of NetworkDeviceControl API for SMC91X Network}
{Note: Not intended to be called directly by applications, use NetworkDeviceControl instead}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Network Control (Request=' + IntToStr(Request) + ')');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Result}
    Result:=ERROR_OPERATION_FAILED;
    Status:=ERROR_SUCCESS;

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
       {SMC91X}
       PSMC91XNetwork(Network).InterruptCount:=0;
       PSMC91XNetwork(Network).CollisionCount:=0;
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
       {SMC91X}
       {Nothing}
      end;
     NETWORK_CONTROL_SET_MAC:begin
       {Acquire Lock}
       if SpinLockIRQ(PSMC91XNetwork(Network).Lock) = ERROR_SUCCESS then
        begin
         {Set the MAC for this device}
         Status:=SMC91XSetMACAddress(PSMC91XNetwork(Network),PHardwareAddress(Argument1));

         {Restore Bank 2}
         SMC91XSelectBank(PSMC91XNetwork(Network),SMC91X_BANK_SELECT_2);

         {Release Lock}
         SpinUnlockIRQ(PSMC91XNetwork(Network).Lock);
        end;
      end;
     NETWORK_CONTROL_GET_MAC:begin
       {Acquire Lock}
       if SpinLockIRQ(PSMC91XNetwork(Network).Lock) = ERROR_SUCCESS then
        begin
         {Get the MAC for this device}
         Status:=SMC91XGetMACAddress(PSMC91XNetwork(Network),PHardwareAddress(Argument1));

         {Restore Bank 2}
         SMC91XSelectBank(PSMC91XNetwork(Network),SMC91X_BANK_SELECT_2);

         {Release Lock}
         SpinUnlockIRQ(PSMC91XNetwork(Network).Lock);
        end;
      end;
     NETWORK_CONTROL_SET_LOOPBACK:begin
       {Set Loopback Mode}
       //To Do
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
       {Acquire Lock}
       if SpinLockIRQ(PSMC91XNetwork(Network).Lock) = ERROR_SUCCESS then
        begin
         {Get Hardware address for this device}
         Status:=SMC91XGetMACAddress(PSMC91XNetwork(Network),PHardwareAddress(Argument1));

         {Restore Bank 2}
         SMC91XSelectBank(PSMC91XNetwork(Network),SMC91X_BANK_SELECT_2);

         {Release Lock}
         SpinUnlockIRQ(PSMC91XNetwork(Network).Lock);
        end;
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
       {Check PHY Type}
       if PSMC91XNetwork(Network).PHYType <> 0 then
        begin
         {Get Network Status}
         Argument2:=NETWORK_LINK_UP; //To Do //SMC91XPHYRead //Not supported in QEMU ?
        end
       else
        begin
         {QEMU does not implement MII or EPH}
         {Link Up}
         Argument2:=NETWORK_LINK_UP;
        end;
      end;
     else
      begin
       Exit;
      end;
    end;

    {Check Status}
    if Status <> ERROR_SUCCESS then Exit;

    {Return Result}
    Result:=Status;
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

function SMC91XBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferAllocate API for SMC91X Network}
{Note: Not intended to be called directly by applications, use NetworkBufferAllocate instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Buffer Allocate');
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
   Entry.Size:=SMC91X_MAX_PACKET_SIZE;
   Entry.Offset:=0;
   Entry.Count:=1;

   {Update Packet}
   Entry.Packets[0].Buffer:=Entry.Buffer;
   Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
   Entry.Packets[0].Length:=Entry.Size - Entry.Offset;

   {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Entry.Size = ' + IntToStr(Entry.Size));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Entry.Offset = ' + IntToStr(Entry.Offset));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Entry.Count = ' + IntToStr(Entry.Count));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X:  Entry.Packets[0].Length = ' + IntToStr(Entry.Packets[0].Length));
   {$ENDIF}

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SMC91XBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferRelease API for SMC91X Network}
{Note: Not intended to be called directly by applications, use NetworkBufferRelease instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Buffer Release');
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

function SMC91XBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferReceive API for SMC91X Network}
{Note: Not intended to be called directly by applications, use NetworkBufferReceive instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Buffer Receive');
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
      Network.ReceiveQueue.Start:=(Network.ReceiveQueue.Start + 1) mod SMC91X_MAX_RX_ENTRIES;

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

function SMC91XBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferTransmit API for SMC91X Network}
{Note: Not intended to be called directly by applications, use NetworkBufferTransmit instead}
var
 Empty:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'SMC91X: Buffer Transmit');
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
      Network.TransmitQueue.Entries[(Network.TransmitQueue.Start + Network.TransmitQueue.Count) mod SMC91X_MAX_TX_ENTRIES]:=Entry;

      {Update Count}
      Inc(Network.TransmitQueue.Count);

      {Check Empty}
      if (Empty) or not(SMC91X_THROTTLE_TRANSMIT) then
       begin
        {Start Transmit}
        SMC91XTransmitStart(PSMC91XNetwork(Network));
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

procedure SMC91XInterruptHandler(Network:PSMC91XNetwork);
{Interrupt handler for the SMC91X Network device}
{Note: Not intended to be called directly by applications}
var
 Status:Byte;
 Counter:Word;
 CurrentIM:Byte;
 CurrentPTR:Word;
 Timeout:LongWord;
 Message:TMessage;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Acquire Lock}
 if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;

 {Update Statistics}
 Inc(Network.InterruptCount);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Save PTR}
 CurrentPTR:=Network.Registers.Bank2.PTR;

 {Save IM}
 CurrentIM:=Network.Registers.Bank2.IM;

 {Clear Interrupt Mask}
 Network.Registers.Bank2.IM:=0;

 {Setup Timeout}
 Timeout:=SMC91X_MAX_IRQ_COUNT;
 while Timeout > 0 do
  begin
   {Get Interrupts}
   Status:=Network.Registers.Bank2.INT;

   {Check Interrupts}
   Status:=Status and CurrentIM;
   if Status = 0 then Break;

   {Check for TX Interrupt}
   if (Status and SMC91X_IM_TX_INT) <> 0 then
    begin
     {Transmit}
     SMC91XInterruptTransmit(Network);
    end
   {Check for RCV Interrupt}
   else if (Status and SMC91X_IM_RCV_INT) <> 0 then
    begin
     {Receive}
     SMC91XInterruptReceive(Network);
    end
   {Check for Alloc Interrupt}
   else if (Status and SMC91X_IM_ALLOC_INT) <> 0 then
    begin
     {Update Mask}
     CurrentIM:=CurrentIM and not(SMC91X_IM_ALLOC_INT);

     {Acknowledge Interrupt}
     {Network.Registers.Bank2.INT:=SMC91X_IM_ALLOC_INT;} {Cleared Automatically}

     {Send Message}
     Message.Msg:=PtrUInt(nil);
     Message.wParam:=SMC91X_COMPLETION_ALLOCATE;
     ThreadSendMessage(Network.Thread,Message);
    end
   {Check for TX Empty Interrupt}
   else if (Status and SMC91X_IM_TX_EMPTY_INT) <> 0 then
    begin
     {Update Mask}
     CurrentIM:=CurrentIM and not(SMC91X_IM_TX_EMPTY_INT);

     {Acknowledge Interrupt}
     Network.Registers.Bank2.INT:=SMC91X_IM_TX_EMPTY_INT;

     {Get Counter Register}
     SMC91XSelectBank(Network,SMC91X_BANK_SELECT_0);
     Counter:=Network.Registers.Bank0.COUNTER;

     {Restore Bank 2}
     SMC91XSelectBank(Network,SMC91X_BANK_SELECT_2);

     {Update Single Collisions}
     Inc(Network.CollisionCount,Counter and $F);

     {Update Multiple Collisions}
     Counter:=Counter shr 4;
     Inc(Network.CollisionCount,Counter and $F);
    end
   {Check for RX Overrun Interrupt}
   else if (Status and SMC91X_IM_RX_OVRN_INT) <> 0 then
    begin
     {Acknowledge Interrupt}
     Network.Registers.Bank2.INT:=SMC91X_IM_RX_OVRN_INT;

     {Update Statistics}
     Inc(Network.Network.ReceiveErrors);
    end
   {Check for EPH Interrupt}
   else if (Status and SMC91X_IM_EPH_INT) <> 0 then
    begin
     {Acknowledge Interrupt}
     Network.Registers.Bank2.INT:=SMC91X_IM_EPH_INT;

     //To Do //smc_eph_interrupt //Not supported in QEMU?
    end
   {Check for PHY MI Interrupt}
   else if (Status and SMC91X_IM_MDINT) <> 0 then
    begin
     {Acknowledge Interrupt}
     Network.Registers.Bank2.INT:=SMC91X_IM_MDINT;

     //To Do //smc_phy_interrupt //Not supported in QEMU?
    end
   {Check for Early Receive Interrupt}
   else if (Status and SMC91X_IM_ERCV_INT) <> 0 then
    begin
     {Acknowledge Interrupt}
     Network.Registers.Bank2.INT:=SMC91X_IM_ERCV_INT;

     {Nothing}
    end;

   Dec(Timeout);
  end;

 {Restore IM}
 Network.Registers.Bank2.IM:=CurrentIM;

 {Restore PTR}
 Network.Registers.Bank2.PTR:=CurrentPTR;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Release Lock}
 SpinUnlockIRQ(Network.Lock);
end;

{==============================================================================}

procedure SMC91XInterruptReceive(Network:PSMC91XNetwork);
{Interrupt receive handler for the SMC91X Network device}
{Note: Not intended to be called directly by applications}
var
 Len:Word;
 Size:Word;
 Status:Word;
 Data:Pointer;
 PacketNo:Byte;
 Value:LongWord;
 Message:TMessage;
 Entry:PNetworkEntry;
 Packet:PNetworkPacket;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Get RX FIFO}
 PacketNo:=Network.Registers.Bank2.FIFO.RX;
 if (PacketNo and SMC91X_RXFIFO_REMPTY) = 0 then
  begin
   {Set Pointer}
   Network.Registers.Bank2.PTR:=SMC91X_PTR_RCV or SMC91X_PTR_AUTOINC or SMC91X_PTR_READ;

   {Read the Status and Length Words}
   Value:=Network.Registers.Bank2.DATA.DATA;
   Status:=Value and $FFFF;
   Len:=(Value shr 16) and $07FF;

   {Check the Length}
   if Len < 6 then
    begin
     Status:=Status or SMC91X_RCV_TOOSHORT;
    end;

   {Check the RX Status}
   if (Status and SMC91X_RCV_ERRORS) <> 0 then
    begin
     if ((Status and SMC91X_RCV_TOOLONG) <> 0) and (Len <= (ETHERNET_MAX_PACKET_SIZE + 6)) then
      begin
       {Allow VLAN Packets}
       Status:=Status and not(SMC91X_RCV_TOOLONG);
      end;
    end;

   if (Status and SMC91X_RCV_ERRORS) <> 0 then
    begin
     {Update Statistics}
     Inc(Network.Network.ReceiveErrors);
    end
   else
    begin
     {The LAN91C111 rev A never sets this bit}
     if Network.Revision = (SMC91X_CHIP_91111FD shl 4) then
      begin
       Status:=Status or SMC91X_RCV_ODDFRAME;
      end;

     {Get Size}
     {If Length is odd Size is Length - 5 else Size is Length - 6}
     if (Status and SMC91X_RCV_ODDFRAME) <> 0 then
      begin
       Size:=Len - 5;
      end
     else
      begin
       Size:=Len - 6;
      end;

     {Check Queue}
     if Network.Count > 0 then
      begin
       {Get Entry}
       Entry:=Network.Entries[Network.Start];
       if Entry <> nil then
        begin
         {Update Start}
         Network.Start:=(Network.Start + 1) mod SMC91X_MAX_RX_ENTRIES;

         {Update Count}
         Dec(Network.Count);

         {Update Entry}
         Entry.Size:=Size;
         Entry.Offset:=0;
         Entry.Count:=1;

         {Get Packet}
         Packet:=@Entry.Packets[0];

         {Update Packet}
         Packet.Buffer:=Entry.Buffer;
         Packet.Data:=Entry.Buffer + Entry.Offset;
         Packet.Length:=Entry.Size - Entry.Offset;

         {Copy Packet}
         Len:=Len - 4;
         if (Len and not(3)) < Size then Inc(Len,2);
         Data:=Packet.Data;
         while Len >= 4 do
          begin
           PLongWord(Data)^:=Network.Registers.Bank2.DATA.DATA;

           Dec(Len,4);
           Inc(Data,4);
          end;

         {Send Message}
         Message.Msg:=PtrUInt(Entry);
         Message.wParam:=SMC91X_COMPLETION_RECEIVE;
         ThreadSendMessage(Network.Thread,Message);
        end;
      end
     else
      begin
       {Update Statistics}
       Inc(Network.Network.BufferOverruns);
      end;
    end;

   {Release the Packet}
   SMC91XWaitMMUBusy(Network);
   SMC91XSetMMUCommand(Network,SMC91X_MMU_CMD_RELEASE);
  end;
end;

{==============================================================================}

procedure SMC91XInterruptTransmit(Network:PSMC91XNetwork);
{Interrupt transmit handler for the SMC91X Network device}
{Note: Not intended to be called directly by applications}
var
 Status:Word;
 PacketNo:Byte;
 CurrentPN:Byte;
 Value:LongWord;
 Message:TMessage;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Get TX FIFO}
 PacketNo:=Network.Registers.Bank2.FIFO.TX;
 if (PacketNo and SMC91X_TXFIFO_TEMPTY) = 0 then
  begin
   {Save PN}
   CurrentPN:=Network.Registers.Bank2.PN;

   {Set Packet No and Pointer}
   Network.Registers.Bank2.PN:=PacketNo;
   Network.Registers.Bank2.PTR:=SMC91X_PTR_AUTOINC or SMC91X_PTR_READ;

   {Read the Status and Length Words}
   Value:=Network.Registers.Bank2.DATA.DATA;
   Status:=Value and $FFFF;

   {Check the TX Status}
   if (Status and SMC91X_EPH_STATUS_TX_SUC) = 0 then
    begin
     {Update Statistics}
     Inc(Network.Network.TransmitErrors);
    end
   else if (Status and (SMC91X_EPH_STATUS_LATCOL or SMC91X_EPH_STATUS_16COL)) <> 0 then
    begin
     {Update Statistics}
     Inc(Network.CollisionCount);
    end;

   {Free the Packet}
   SMC91XWaitMMUBusy(Network);
   SMC91XSetMMUCommand(Network,SMC91X_MMU_CMD_FREEPKT);

   {Restore PN}
   SMC91XWaitMMUBusy(Network);
   Network.Registers.Bank2.PN:=CurrentPN;

   //To Do //Is this needed ? //See: smc_tx
   //{Re-enable Transmit}
   //SMC_SELECT_BANK(lp, 0);
   //SMC_SET_TCR(lp, lp->tcr_cur_mode);
   //SMC_SELECT_BANK(lp, 2);
  end;

 {Acknowledge Interrupt}
 Network.Registers.Bank2.INT:=SMC91X_IM_TX_INT;

 {Check Throttle}
 if SMC91X_THROTTLE_TRANSMIT then
  begin
   {Send Message}
   Message.Msg:=PtrUInt(nil);
   Message.wParam:=SMC91X_COMPLETION_TRANSMIT;
   ThreadSendMessage(Network.Thread,Message);
  end;
end;

{==============================================================================}

function SMC91XCompletionExecute(Network:PSMC91XNetwork):PtrInt;
{Completion handler thread for the SMC91X Network device}
{Note: Not intended to be called directly by applications}
var
 Message:TMessage;
 Entry:PNetworkEntry;
 Packet:PNetworkPacket;
begin
 {}
 Result:=0;
 try
  {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Completion Thread (ThreadID = ' + IntToHex(ThreadID,8) + ')');
  {$ENDIF}

  {Check Network}
  if Network = nil then Exit;

  while ThreadReceiveMessage(Message) = ERROR_SUCCESS do
   begin
    {Acquire the Lock}
    if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
     begin
      try
       {Get Buffer (Receive Buffer)}
       Entry:=BufferGetEx(Network.Network.ReceiveQueue.Buffer,0); {Do not wait}
       while Entry <> nil do
        begin
         {Acquire Lock}
         if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;

         {Add Entry}
         Network.Entries[(Network.Start + Network.Count) mod SMC91X_MAX_RX_ENTRIES]:=Entry;

         {Update Count}
         Inc(Network.Count);

         {Release Lock}
         SpinUnlockIRQ(Network.Lock);

         {Get Buffer (Receive Buffer)}
         Entry:=BufferGetEx(Network.Network.ReceiveQueue.Buffer,0); {Do not wait}
        end;

       {Check Message}
       case Message.wParam of
        SMC91X_COMPLETION_RECEIVE:begin
          {Receive}
          {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Completion Thread (Message=SMC91X_COMPLETION_RECEIVE)');
          {$ENDIF}

          {Get Entry}
          Entry:=PNetworkEntry(Message.Msg);
          if Entry <> nil then
           begin
            {Check Queue}
            if Network.Network.ReceiveQueue.Count < SMC91X_MAX_RX_ENTRIES then
             begin
              {Update Statistics}
              Inc(Network.Network.ReceiveCount);
              Inc(Network.Network.ReceiveBytes,Entry.Packets[0].Length);

              {Add Entry}
              Network.Network.ReceiveQueue.Entries[(Network.Network.ReceiveQueue.Start + Network.Network.ReceiveQueue.Count) mod SMC91X_MAX_RX_ENTRIES]:=Entry;

              {Update Count}
              Inc(Network.Network.ReceiveQueue.Count);

              {Signal Received}
              SemaphoreSignal(Network.Network.ReceiveQueue.Wait);
             end
            else
             begin
              {Update Statistics}
              Inc(Network.Network.BufferOverruns);
             end;
           end;
         end;
        SMC91X_COMPLETION_TRANSMIT:begin
          {Transmit}
          {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Completion Thread (Message=SMC91X_COMPLETION_TRANSMIT)');
          {$ENDIF}

          {Check Count}
          if (Network.Network.TransmitQueue.Count <> 0) and SMC91X_THROTTLE_TRANSMIT then
           begin
            {Start Transmit}
            SMC91XTransmitStart(Network);
           end;
         end;
        SMC91X_COMPLETION_ALLOCATE:begin
          {Allocate}
          {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Completion Thread (Message=SMC91X_COMPLETION_ALLOCATE)');
          {$ENDIF}

          {Check Count}
          if Network.Network.TransmitQueue.Count > 0 then
           begin
            {Get Entry}
            Entry:=Network.Network.TransmitQueue.Entries[Network.Network.TransmitQueue.Start];
            if Entry <> nil then
             begin
              {Get Packet}
              Packet:=@Entry.Packets[0];

              {Transmit Packet}
              SMC91XTransmitPacket(Network,Entry,Packet);
             end;
           end;
         end;
        SMC91X_COMPLETION_TERMINATE:begin
          {Terminate}
          {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Completion Thread (Message=SMC91X_COMPLETION_TERMINATE)');
          {$ENDIF}

          Exit;
         end;
       end;
      finally
       {Release the Lock}
       MutexUnlock(Network.Network.Lock);
      end;
     end;
   end;
 except
  on E: Exception do
   begin
    if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'SMC91X: Completion Thread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end;
end;

{==============================================================================}

procedure SMC91XTransmitStart(Network:PSMC91XNetwork);
{Transmit start function for the SMC91X Network device}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the network lock}
var
 Status:Byte;
 PageCount:Word;
 PollCount:Word;
 Entry:PNetworkEntry;
 Packet:PNetworkPacket;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Transmit Start');
 {$ENDIF}

 {Check Count}
 if Network.Network.TransmitQueue.Count = 0 then Exit;

 {Get Entry}
 Entry:=Network.Network.TransmitQueue.Entries[Network.Network.TransmitQueue.Start];
 if Entry = nil then Exit;

 {Get Packet}
 Packet:=@Entry.Packets[0];

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  Packet Length = ' + IntToStr(Packet.Length));
 {$ENDIF}

 {Get Count (Number of 256 byte pages minus 1)}
 {Size for allocation is packet length + 6 (for Status, Length and Control Words)}
 {If odd size then Last Byte is included in Control Word}
 PageCount:=((Packet.Length and not(1)) + (6 - 1)) shr 8;
 if PageCount > 7 then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'SMC91X: Packet too big (Length=' + IntToStr(Packet.Length));

   {Update Statistics}
   Inc(Network.Network.TransmitErrors);

   Exit;
  end;
 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  Page Count = ' + IntToStr(PageCount));
 {$ENDIF}

 {Check CPU Count}
 if CPUGetCount > 1 then
  begin
   {Acquire Lock}
   if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Allocate Memory}
 SMC91XSetMMUCommand(Network,SMC91X_MMU_CMD_ALLOC or PageCount);

 {Wait for Allocate}
 PollCount:=SMC91X_ALLOCATE_WAIT_COUNT;
 while PollCount > 0 do
  begin
   {Check Interrupt}
   Status:=Network.Registers.Bank2.INT;
   if (Status and SMC91X_IM_ALLOC_INT) <> 0 then
    begin
     {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  Packet Allocated');
     {$ENDIF}

     {Acknowledge Interrupt}
     {Network.Registers.Bank2.INT:=SMC91X_IM_ALLOC_INT;} {Cleared Automatically}
     Break;
    end;

   Dec(PollCount);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Check CPU Count}
 if CPUGetCount > 1 then
  begin
   {Release Lock}
   SpinUnlockIRQ(Network.Lock);
  end;

 {Check Poll count}
 if PollCount = 0 then
  begin
   {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  Poll Timeout, Packet Deferred');
   {$ENDIF}

   {Enable Interrupt}
   SMC91XEnableInterrupt(Network,SMC91X_IM_ALLOC_INT);
  end
 else
  begin
   {Transmit Packet}
   SMC91XTransmitPacket(Network,Entry,Packet);
  end;
end;

{==============================================================================}

procedure SMC91XTransmitPacket(Network:PSMC91XNetwork;Entry:PNetworkEntry;Packet:PNetworkPacket);
{Transmit packet function for the SMC91X Network device}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the network lock}
var
 Len:LongWord;
 Data:Pointer;
 PacketNo:Byte;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Transmit Packet');
 {$ENDIF}

 {Check Entry}
 if Entry = nil then Exit;

 {Check Packet}
 if Packet = nil then Exit;

 {Check CPU Count}
 if CPUGetCount > 1 then
  begin
   {Acquire Lock}
   if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;
  end;
 try
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}

  {Get Packet No}
  PacketNo:=Network.Registers.Bank2.AR;
  if (PacketNo and SMC91X_AR_FAILED) <> 0 then
   begin
    {Update Statistics}
    Inc(Network.Network.TransmitErrors);

    Exit;
   end;

  {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  Packet No = ' + IntToStr(PacketNo));
  {$ENDIF}

  {Get Packet Length and Data}
  Len:=Packet.Length;
  Data:=Packet.Data;

  {Set Packet No and Start of Packet}
  Network.Registers.Bank2.PN:=PacketNo;
  Network.Registers.Bank2.PTR:=SMC91X_PTR_AUTOINC;

  {Set Packet Status and Length}
  Network.Registers.Bank2.DATA.DATA:=0 or ((Len + 6) shl 16);

  {Send Packet Data}
  {First Word}
  if (Len >= 2) and ((PtrUInt(Data) and 2) <> 0) then
   begin
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  First Word = ' + IntToHex(PWord(Data)^,4));
    {$ENDIF}

    Network.Registers.Bank2.DATA.DATAH:=PWord(Data)^;

    Dec(Len,2);
    Inc(Data,2);
   end;
  {All LongWords}
  while Len >= 4 do
   begin
    Network.Registers.Bank2.DATA.DATA:=PLongWord(Data)^;

    Dec(Len,4);
    Inc(Data,4);
   end;
  {Last Word}
  if (Len and 2) <> 0 then
   begin
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  Last Word = ' + IntToHex(PWord(Data)^,4));
    {$ENDIF}

    Network.Registers.Bank2.DATA.DATAH:=PWord(Data)^;

    Dec(Len,2);
    Inc(Data,2);
   end;

  {Send Control Word and Last Byte if present}
  if Len > 0 then
   begin
    {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  Last Byte = ' + IntToHex(PByte(Data)^,2));
    {$ENDIF}

    Network.Registers.Bank2.DATA.DATAH:=$2000 or PByte(Data)^;
   end
  else
   begin
    Network.Registers.Bank2.DATA.DATAH:=0;
   end;

  {Enqueue Packet}
  SMC91XSetMMUCommand(Network,SMC91X_MMU_CMD_ENQUEUE);

  {Enable Interrupt}
  if CPUGetCount > 1 then
   begin
    Network.Registers.Bank2.IM:=Network.Registers.Bank2.IM or SMC91X_IM_TX_INT or SMC91X_IM_TX_EMPTY_INT;
   end
  else
   begin
    SMC91XEnableInterrupt(Network,SMC91X_IM_TX_INT or SMC91X_IM_TX_EMPTY_INT);
   end;

  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read}
 finally
  {Check CPU Count}
  if CPUGetCount > 1 then
   begin
    {Release Lock}
    SpinUnlockIRQ(Network.Lock);
   end;
 end;

 {Update Statistics}
 Inc(Network.Network.TransmitCount);
 Inc(Network.Network.TransmitBytes,Packet.Length);

 {Update Start}
 Network.Network.TransmitQueue.Start:=(Network.Network.TransmitQueue.Start + 1) mod SMC91X_MAX_TX_ENTRIES;

 {Update Count}
 Dec(Network.Network.TransmitQueue.Count);

 {Signal Queue Free}
 SemaphoreSignal(Network.Network.TransmitQueue.Wait);

 {Free Entry (Transmit Buffer)}
 BufferFree(Entry);
end;

{==============================================================================}
{==============================================================================}
{SMC91X Helper Functions}
function SMC91XCurrentBank(Network:PSMC91XNetwork):Word; inline;
{Get the currently selected bank from the bank select register}
{Network: The SMC91X Network device to get the bank from}
{Return: The currently selected bank (The high byte must be $33 to be valid)}

{Note: Caller must hold the network lock}
begin
 {}
 {Get Bank Select Register}
 Result:=Network.Registers.Bank0.BANK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}

procedure SMC91XSelectBank(Network:PSMC91XNetwork;Bank:Byte); inline;
{Set the currently selected bank in the bank select register}
{Network: The SMC91X Network device to set the bank for}
{Bank: The bank to select}

{Note: Caller must hold the network lock}
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set Bank Select Register}
 Network.Registers.Bank0.BANK:=Bank;
end;

{==============================================================================}

function SMC91XGetBase(Network:PSMC91XNetwork):Word; inline;
{Get the base address register from a SMC91X Network device}
{Network: The SMC91X Network device to get from}
{Return: The base addres register value}

{Note: Caller must hold the network lock}
begin
 {}
 {Get Base Address Register}
 Result:=Network.Registers.Bank1.BASE;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}

function SMC91XGetRevision(Network:PSMC91XNetwork):Word; inline;
{Get the revision register from a SMC91X Network device}
{Network: The SMC91X Network device to get from}
{Return: The revision register value}

{Note: Caller must hold the network lock}
begin
 {}
 {Get Base Address Register}
 Result:=Network.Registers.Bank3.REV;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}

procedure SMC91XWaitMMUBusy(Network:PSMC91XNetwork); inline;
{Wait for the MMU command register BUSY bit to be cleared for a SMC91X Network device}
{Network: The SMC91X Network device to wait for}

{Note: Caller must hold the network lock}
var
 Timeout:LongWord;
begin
 {}
 if (Network.Registers.Bank2.MMU_CMD and SMC91X_MMU_CMD_BUSY) <> 0 then
  begin
   Timeout:=10;
   while (Network.Registers.Bank2.MMU_CMD and SMC91X_MMU_CMD_BUSY) <> 0 do
    begin
     if Timeout = 0 then Break;
     ThreadSleep(0);

     Dec(Timeout);
    end;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
end;

{==============================================================================}

procedure SMC91XSetMMUCommand(Network:PSMC91XNetwork;Command:Word); inline;
{Set the MMU command register of a SMC91X Network device}
{Network: The SMC91X Network device to set for}
{Command: The command to set}

{Note: Caller must hold the network lock}
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set MMU Command Register}
 Network.Registers.Bank2.MMU_CMD:=Command;
end;

{==============================================================================}

procedure SMC91XSetInterruptMask(Network:PSMC91XNetwork;Mask:Byte); inline;
{Enable the specified mask in the interrupt mask register of a SMC91X Network device}
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set Interrupt Mask Register}
 Network.Registers.Bank2.IM:=Mask;
end;

{==============================================================================}

procedure SMC91XEnableInterrupt(Network:PSMC91XNetwork;Interrupt:Byte);
{Enable the specified interrupt in the interrupt mask register of a SMC91X Network device}
{Network: The SMC91X Network device to enable the interrupt for}
{Interrupt: The interrupt to enable}

{Note: Caller must hold the network lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Update Interrupt Mask}
 Network.Registers.Bank2.IM:=Network.Registers.Bank2.IM or Interrupt;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Release Lock}
 SpinUnlockIRQ(Network.Lock);
end;

{==============================================================================}

procedure SMC91XDisableInterrupt(Network:PSMC91XNetwork;Interrupt:Byte);
{Disable the specified interrupt in the interrupt mask register of a SMC91X Network device}
{Network: The SMC91X Network device to disable the interrupt for}
{Interrupt: The interrupt to disable}

{Note: Caller must hold the network lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Update Interrupt Mask}
 Network.Registers.Bank2.IM:=Network.Registers.Bank2.IM and not(Interrupt);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Release Lock}
 SpinUnlockIRQ(Network.Lock);
end;

{==============================================================================}

function SMC91XReset(Network:PSMC91XNetwork):LongWord;
{Reset a SMC91X Network device}
{Network: The SMC91X Network device to reset}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the network lock}
var
 Value:Word;
 Timeout:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Reset');
 {$ENDIF}

 {Acquire Lock}
 if Network.Lock <> INVALID_HANDLE_VALUE then if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable Interrupts}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_2);
 Network.Registers.Bank2.INT:=0;

 {Perform Soft Reset (Doesn't affect EEPROM)}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_0);
 Network.Registers.Bank0.RCR:=SMC91X_RCR_SOFTRST;

 {Reset Configuration Register}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_1);
 Value:=SMC91X_CONFIG_DEFAULT or SMC91X_CONFIG_RESERVED;
 Network.Registers.Bank1.CONFIG:=Value;

 {Wait}
 MicrosecondDelay(1);

 {Disable TX and RX}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_0);
 Network.Registers.Bank0.RCR:=SMC91X_RCR_CLEAR;
 Network.Registers.Bank0.TCR:=SMC91X_TCR_CLEAR;

 {Reset Control Register}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_1);
 Value:=Network.Registers.Bank1.CTL or SMC91X_CTL_LE_ENABLE;
 if SMC91X_THROTTLE_TRANSMIT then
  begin
   Value:=Value and not(SMC91X_CTL_AUTO_RELEASE);
  end
 else
  begin
   Value:=Value or SMC91X_CTL_AUTO_RELEASE;
  end;
 Network.Registers.Bank1.CTL:=Value;

 {Reset MMU}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_2);
 Network.Registers.Bank2.MMU_CMD:=SMC91X_MMU_CMD_RESET;
 if (Network.Registers.Bank2.MMU_CMD and SMC91X_MMU_CMD_BUSY) <> 0 then
  begin
   Timeout:=10;
   while (Network.Registers.Bank2.MMU_CMD and SMC91X_MMU_CMD_BUSY) <> 0 do
    begin
     if Timeout = 0 then Break;
     ThreadSleep(0);

     Dec(Timeout);
    end;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Release Lock}
 if Network.Lock <> INVALID_HANDLE_VALUE then SpinUnlockIRQ(Network.Lock);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XEnable(Network:PSMC91XNetwork):LongWord;
{Enable a SMC91X Network device and start sending and receiving}
{Network: The SMC91X Network device to enable}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the network lock}
var
 Mask:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Enable');
 {$ENDIF}

 {Acquire Lock}
 if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Enable Transmit and Receive}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_0);
 Network.Registers.Bank0.RCR:=Network.RCRFlags;
 Network.Registers.Bank0.TCR:=Network.TCRFlags;

 {Enable Interrupts}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_2);
 Mask:=SMC91X_IM_EPH_INT or SMC91X_IM_RX_OVRN_INT or SMC91X_IM_RCV_INT;
 if Network.Revision >= (SMC91X_CHIP_91100 shl 4) then
  begin
   Mask:=Mask or SMC91X_IM_MDINT;
  end;
 Network.Registers.Bank2.IM:=Mask;

 {Release Lock}
 SpinUnlockIRQ(Network.Lock);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XShutdown(Network:PSMC91XNetwork):LongWord;
{Shutdown a SMC91X Network device and stop sending and receiving}
{Network: The SMC91X Network device to shutdown}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Shutdown');
 {$ENDIF}

 {Acquire Lock}
 if SpinLockIRQ(Network.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable Interrupts}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_2);
 Network.Registers.Bank2.IM:=0;

 {Disable Transmit and Receive}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_0);
 Network.Registers.Bank0.RCR:=SMC91X_RCR_CLEAR;
 Network.Registers.Bank0.TCR:=SMC91X_TCR_CLEAR;

 {Release Lock}
 SpinUnlockIRQ(Network.Lock);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XGetMACAddress(Network:PSMC91XNetwork;Address:PHardwareAddress):LongWord;
{Get the current MAC address from a SMC91X Network device}
{Address: Pointer to a buffer to return the hardware address}
{Network: The SMC91X Network device to get from}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the network lock}
var
 Value:Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {Check Address}
 if Address = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Get MAC Address');
 {$ENDIF}

 {Select Bank 1}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_1);

 {Get Mac Address (Word0)}
 Value:=Network.Registers.Bank1.ADDR0;
 Address[0]:=(Value shr 0) and $ff;
 Address[1]:=(Value shr 8) and $ff;

 {Get Mac Address (Word2)}
 Value:=Network.Registers.Bank1.ADDR1;
 Address[2]:=(Value shr 0) and $ff;
 Address[3]:=(Value shr 8) and $ff;

 {Get Mac Address (Word2)}
 Value:=Network.Registers.Bank1.ADDR2;
 Address[4]:=(Value shr 0) and $ff;
 Address[5]:=(Value shr 8) and $ff;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XSetMACAddress(Network:PSMC91XNetwork;Address:PHardwareAddress):LongWord;
{Set the current MAC address for a SMC91X Network device}
{Address: Pointer to the hardware address to set}
{Network: The SMC91X Network device to set for}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {Check Address}
 if Address = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: Set MAC Address (Address=' + HardwareAddressToString(Address^) + ')');
 {$ENDIF}

 {Select Bank 1}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_1);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set Mac Address (Word0)}
 Network.Registers.Bank1.ADDR0:=Address[0] or (Address[1] shl 8);

 {Set Mac Address (Word1)}
 Network.Registers.Bank1.ADDR1:=Address[2] or (Address[3] shl 8);

 {Set Mac Address (Word2)}
 Network.Registers.Bank1.ADDR2:=Address[4] or (Address[5] shl 8);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XMIIIn(Network:PSMC91XNetwork;Bits:LongWord):LongWord;
{Input data from the MII Management serial interface}

{Note: Caller must hold the network lock}
var
 Reg:LongWord;
 Mask:LongWord;
 Value:LongWord;
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {Get Reg}
 Reg:=Network.Registers.Bank3.MII and not(SMC91X_MII_MCLK or SMC91X_MII_MDOE or SMC91X_MII_MDO);
 Network.Registers.Bank3.MII:=Reg;

 Mask:=1 shl (Bits - 1);
 Value:=0;
 while Mask <> 0 do
  begin
   if (Network.Registers.Bank3.MII and SMC91X_MII_MDI) <> 0 then
    begin
     Value:=Value or Mask;
    end;

   Network.Registers.Bank3.MII:=Reg;
   MicrosecondDelay(SMC91X_MII_DELAY);
   Network.Registers.Bank3.MII:=Reg or SMC91X_MII_MCLK;
   MicrosecondDelay(SMC91X_MII_DELAY);

   Mask:=Mask shr 1;
  end;

 Result:=Value;
end;

{==============================================================================}

procedure SMC91XMIIOut(Network:PSMC91XNetwork;Value,Bits:LongWord);
{Output data to the MII Management serial interface}

{Note: Caller must hold the network lock}
var
 Reg:LongWord;
 Mask:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {Get Reg}
 Reg:=Network.Registers.Bank3.MII and not(SMC91X_MII_MCLK or SMC91X_MII_MDOE or SMC91X_MII_MDO);
 Reg:=Reg or SMC91X_MII_MDOE;

 Mask:=1 shl (Bits - 1);
 while Mask <> 0 do
  begin
   if (Value and Mask) <> 0 then
    begin
     Reg:=Reg or SMC91X_MII_MDO;
    end
   else
    begin
     Reg:=Reg and not(SMC91X_MII_MDO);
    end;

   Network.Registers.Bank3.MII:=Reg;
   MicrosecondDelay(SMC91X_MII_DELAY);
   Network.Registers.Bank3.MII:=Reg or SMC91X_MII_MCLK;
   MicrosecondDelay(SMC91X_MII_DELAY);

   Mask:=Mask shr 1;
  end;
end;

{==============================================================================}

function SMC91XPHYRead(Network:PSMC91XNetwork;Addr,Reg:LongWord):LongWord;
{Reads a register from the MII Management serial interface}

{Note: Caller must hold the network lock}
var
 Data:LongWord;
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Select Bank 3}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_3);

 {Idle - 32 ones}
 SMC91XMIIOut(Network,$ffffffff,32);

 {Start code (01) + read (10) + Addr + Reg}
 SMC91XMIIOut(Network,(6 shl 10) or (Addr shl 5) or Reg,14);

 {Turnaround (2 bits) + Data}
 Data:=SMC91XMIIIn(Network,18);

 {Return to idle state}
 Network.Registers.Bank3.MII:=Network.Registers.Bank3.MII and not(SMC91X_MII_MCLK or SMC91X_MII_MDOE or SMC91X_MII_MDO);

 {Select Bank 2}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_2);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=Data;
end;

{==============================================================================}

procedure SMC91XPHYWrite(Network:PSMC91XNetwork;Addr,Reg,Data:LongWord);
{Writes a register to the MII Management serial interface}

{Note: Caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {Select Bank 3}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_3);

 //To Do //Not supported in QEMU ?

 {Select Bank 2}
 SMC91XSelectBank(Network,SMC91X_BANK_SELECT_2);
end;

{==============================================================================}

function SMC91XPHYDetect(Network:PSMC91XNetwork):LongWord;
{Detect the PHY type used in a SMC91X device}

{Note: Caller must hold the network lock}
var
 ID1:LongWord;
 ID2:LongWord;
 PHYAddr:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: PHY Detect');
 {$ENDIF}

 {Set Default}
 Network.PHYType:=0;

 {Scan all 32 PHY addresses if necessary, starting at PHY#1 to PHY#31, and then PHY#0 last}
 PHYAddr:=1;
 while PHYAddr < 33 do
  begin
   {Read the PHY identifiers}
   ID1:=SMC91XPHYRead(Network,PHYAddr and 31,MII_PHYSID1);
   ID2:=SMC91XPHYRead(Network,PHYAddr and 31,MII_PHYSID2);

   {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  ID1 = ' + IntToHex(ID1,8));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  ID2 = ' + IntToHex(ID2,8));
   {$ENDIF}

   {Make sure it is a valid identifier}
   if (ID1 <> $0000) and (ID1 <> $FFFF) and (ID1 <> $8000)
    and (ID2 <> $0000) and (ID2 <> $FFFF) and (ID2 <> $8000) then
    begin
     {Save the PHY address}
     Network.PHYId:=PHYAddr and 31;
     Network.PHYType:=(ID1 shl 16) or ID2;

     {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  PHYId = ' + IntToStr(Network.PHYId));
     if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X:  PHYType = ' + IntToHex(Network.PHYType,8));
     {$ENDIF}

     Break;
    end;

   Inc(PHYAddr);
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XPHYReset(Network:PSMC91XNetwork):LongWord;
{Perform a software reset on the PHY in a SMC91X device}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: PHY Reset');
 {$ENDIF}

 //To Do //Not supported in QEMU ?

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XPHYFixed(Network:PSMC91XNetwork):LongWord;
{Configure the PHY with a fixed configuration on a SMC91x device}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: PHY Fixed');
 {$ENDIF}

 //To Do //Not supported in QEMU ?

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XPHYConfigure(Network:PSMC91XNetwork):LongWord;
{Configure the PHY using auto-negotiation on a SMC91x device}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: PHY Configure');
 {$ENDIF}

 //To Do //Not supported in QEMU ?

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XPHYPowerdown(Network:PSMC91XNetwork):LongWord;
{Power down a PHY on a SMC91X device}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: PHY Powerdown');
 {$ENDIF}

 //To Do //Not supported in QEMU ?

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SMC91XPHYCheckMedia(Network:PSMC91XNetwork):LongWord;
{Check the media status of a PHY on a SMC91X device}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Registers = nil then Exit;

 {$IF DEFINED(SMC91X_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'SMC91X: PHY Check Media');
 {$ENDIF}

 //To Do //Not supported in QEMU ?

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}

{initialization}
 {Nothing}

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
