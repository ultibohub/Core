{
Broadcom GENET Gigabit Ethernet Driver.

Copyright (C) 2021 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi 4 - Model B

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\net\ethernet\broadcom\genet\bcmgenet.c - Copyright (c) 2014-2017 Broadcom

  Linux - \drivers\net\phy\mdio-bcm-unimac.c - Copyright (C) 2014-2017 Broadcom

  Linux - \drivers\net\phy\broadcom.c - Copyright (c) 2006  Maciej W. Rozycki

References
==========

 BCM54213PE Single Port RGMII Gigabit Ethernet Transceiver

  https://www.broadcom.com/products/ethernet-connectivity/copper-phy/gigabitphy/bcm54213pe

GENET
=====

This driver supports members of the Broadcom family of Gigabit Ethernet devices including
the BCM54213PE contained in the Raspberry Pi 4B.

These devices support speeds of 10BASE-T, 100BASE-TX and 1000BASE-T and include a range of
features including WOL and EEE.

The device included in the Raspberry Pi 4B uses a memory mapped interface which supports
multiple TX and RX queues with priority queueing, hardware filtering and DMA bus master
transfers.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GENET;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Network,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {GENET specific constants (Broadcom Gigabit Ethernet controller)}
 GENET_NETWORK_DESCRIPTION = 'Broadcom GENET (Gigabit Ethernet) controller';  {Description of GENET device}

 GENET_MAX_TX_ENTRIES = SIZE_256; {Number of Transmit buffers allocated}
 GENET_MAX_RX_ENTRIES = SIZE_512; {Number of Receive buffers allocated}

 GENET_MAX_PACKET_SIZE = 2048;

 {Version information}
 GENET_V1 = 1;
 GENET_V2 = 2;
 GENET_V3 = 3;
 GENET_V4 = 4;
 GENET_V5 = 5;

 {Total number of Buffer Descriptors, same for RX/TX}
 GENET_TOTAL_DESC = 256;

 {Max number of descriptor queues (plus 1 for default)}
 GENET_DESC_INDEX = 16;

 {Body(1500) + Header(14) + VLANTAG(4) + BRCMTAG(6) + FCS(4) = 1528}
 {Plus PAD(6) = 1536 which is a multiple of 256 bytes}
 GENET_ETH_BRCM_TAG_LEN = 6;
 GENET_ETH_PAD = 8;
 GENET_ETH_MAX_MTU_SIZE = (ETHERNET_MTU + ETHERNET_HEADER_SIZE + ETHERNET_VLAN_SIZE + GENET_ETH_BRCM_TAG_LEN + ETHERNET_CRC_SIZE + GENET_ETH_PAD);

 {Default highest priority queue for multi queue support}
 GENET_Q0_PRIORITY = 0;

 {Misc configuration}
 CLEAR_ALL_HFB = $FF;

 {DMA configuration}
 DMA_MAX_BURST_LENGTH = $08;
 DMA_FC_THRESH_HI = (GENET_TOTAL_DESC shr 4);
 DMA_FC_THRESH_LO = 5;

 {RX status bits}
 STATUS_RX_EXT_MASK = $1FFFFF;
 STATUS_RX_CSUM_MASK = $FFFF;
 STATUS_RX_CSUM_OK = $10000;
 STATUS_RX_CSUM_FR = $20000;
 STATUS_RX_PROTO_TCP = 0;
 STATUS_RX_PROTO_UDP = 1;
 STATUS_RX_PROTO_ICMP = 2;
 STATUS_RX_PROTO_OTHER = 3;
 STATUS_RX_PROTO_MASK = 3;
 STATUS_RX_PROTO_SHIFT = 18;
 STATUS_FILTER_INDEX_MASK = $FFFF;

 {TX status bits}
 STATUS_TX_CSUM_START_MASK = $7FFF;
 STATUS_TX_CSUM_START_SHIFT = 16;
 STATUS_TX_CSUM_PROTO_UDP = $8000;
 STATUS_TX_CSUM_OFFSET_MASK = $7FFF;
 STATUS_TX_CSUM_LV = $80000000;

 {DMA Descriptor}
 DMA_DESC_LENGTH_STATUS = $00; {in bytes of data in buffer}
 DMA_DESC_ADDRESS_LO = $04; {lower bits of PA}
 DMA_DESC_ADDRESS_HI = $08; {upper 32 bits of PA, GENETv4+}

 {UniMAC registers}
 UMAC_HD_BKP_CTRL     = $004;
  HD_FC_EN            = (1 shl 0);
  HD_FC_BKOFF_OK      = (1 shl 1);
  IPG_CONFIG_RX_SHIFT = 2;
  IPG_CONFIG_RX_MASK  = $1F;

 UMAC_CMD  = $008;
  CMD_TX_EN  = (1 shl 0);
  CMD_RX_EN  = (1 shl 1);
  UMAC_SPEED_10  = 0;
  UMAC_SPEED_100  = 1;
  UMAC_SPEED_1000 = 2;
  UMAC_SPEED_2500 = 3;
  CMD_SPEED_SHIFT = 2;
  CMD_SPEED_MASK  = 3;
  CMD_PROMISC  = (1 shl 4);
  CMD_PAD_EN  = (1 shl 5);
  CMD_CRC_FWD  = (1 shl 6);
  CMD_PAUSE_FWD  = (1 shl 7);
  CMD_RX_PAUSE_IGNORE = (1 shl 8);
  CMD_TX_ADDR_INS = (1 shl 9);
  CMD_HD_EN  = (1 shl 10);
  CMD_SW_RESET  = (1 shl 13);
  CMD_LCL_LOOP_EN = (1 shl 15);
  CMD_AUTO_CONFIG = (1 shl 22);
  CMD_CNTL_FRM_EN = (1 shl 23);
  CMD_NO_LEN_CHK  = (1 shl 24);
  CMD_RMT_LOOP_EN = (1 shl 25);
  CMD_PRBL_EN  = (1 shl 27);
  CMD_TX_PAUSE_IGNORE = (1 shl 28);
  CMD_TX_RX_EN  = (1 shl 29);
  CMD_RUNT_FILTER_DIS = (1 shl 30);

 UMAC_MAC0  = $00C;
 UMAC_MAC1  = $010;
 UMAC_MAX_FRAME_LEN = $014;

 UMAC_MODE  = $44;
  MODE_LINK_STATUS = (1 shl 5);

 UMAC_EEE_CTRL  = $064;
  EN_LPI_RX_PAUSE = (1 shl 0);
  EN_LPI_TX_PFC  = (1 shl 1);
  EN_LPI_TX_PAUSE = (1 shl 2);
  EEE_EN   = (1 shl 3);
  RX_FIFO_CHECK  = (1 shl 4);
  EEE_TX_CLK_DIS  = (1 shl 5);
  DIS_EEE_10M  = (1 shl 6);
  LP_IDLE_PREDICTION_MODE = (1 shl 7);

 UMAC_EEE_LPI_TIMER = $068;
 UMAC_EEE_WAKE_TIMER = $06C;
 UMAC_EEE_REF_COUNT = $070;
  EEE_REFERENCE_COUNT_MASK = $ffff;

 UMAC_TX_FLUSH  = $334;

 UMAC_MIB_START  = $400;

 UMAC_MDIO_CMD  = $614;
  MDIO_START_BUSY = (1 shl 29);
  MDIO_READ_FAIL  = (1 shl 28);
  MDIO_RD  = (2 shl 26);
  MDIO_WR  = (1 shl 26);
  MDIO_PMD_SHIFT  = 21;
  MDIO_PMD_MASK  = $1F;
  MDIO_REG_SHIFT  = 16;
  MDIO_REG_MASK  = $1F;

 UMAC_RBUF_OVFL_CNT_V1 = $61C;
  RBUF_OVFL_CNT_V2 = $80;
  RBUF_OVFL_CNT_V3PLUS = $94;

 UMAC_MPD_CTRL  = $620;
  MPD_EN   = (1 shl 0);
  MPD_PW_EN  = (1 shl 27);
  MPD_MSEQ_LEN_SHIFT = 16;
  MPD_MSEQ_LEN_MASK = $FF;

 UMAC_MPD_PW_MS  = $624;
 UMAC_MPD_PW_LS  = $628;
 UMAC_RBUF_ERR_CNT_V1 = $634;
 RBUF_ERR_CNT_V2  = $84;
 RBUF_ERR_CNT_V3PLUS = $98;
 UMAC_MDF_ERR_CNT = $638;
 UMAC_MDF_CTRL  = $650;
 UMAC_MDF_ADDR  = $654;
 UMAC_MIB_CTRL  = $580;
  MIB_RESET_RX  = (1 shl 0);
  MIB_RESET_RUNT  = (1 shl 1);
  MIB_RESET_TX  = (1 shl 2);

 {Receive buffer registers}
 RBUF_CTRL  = $00;
  RBUF_64B_EN  = (1 shl 0);
  RBUF_ALIGN_2B  = (1 shl 1);
  RBUF_BAD_DIS  = (1 shl 2);

 RBUF_STATUS  = $0C;
  RBUF_STATUS_WOL = (1 shl 0);
  RBUF_STATUS_MPD_INTR_ACTIVE = (1 shl 1);
  RBUF_STATUS_ACPI_INTR_ACTIVE = (1 shl 2);

 RBUF_CHK_CTRL  = $14;
  RBUF_RXCHK_EN  = (1 shl 0);
  RBUF_SKIP_FCS  = (1 shl 4);

 RBUF_ENERGY_CTRL = $9c;
  RBUF_EEE_EN  = (1 shl 0);
  RBUF_PM_EN  = (1 shl 1);

 RBUF_TBUF_SIZE_CTRL = $b4;

 {Hardware Filter Block (HFB)}
 RBUF_HFB_CTRL_V1 = $38;
  RBUF_HFB_FILTER_EN_SHIFT = 16;
  RBUF_HFB_FILTER_EN_MASK = $ffff0000;
  RBUF_HFB_EN  = (1 shl 0);
  RBUF_HFB_256B  = (1 shl 1);
  RBUF_ACPI_EN  = (1 shl 2);

 RBUF_HFB_LEN_V1  = $3C;
  RBUF_FLTR_LEN_MASK = $FF;
  RBUF_FLTR_LEN_SHIFT = 8;

 {Transmit buffer registers}
 TBUF_CTRL  = $00;
 TBUF_BP_MC  = $0C;
 TBUF_ENERGY_CTRL = $14;
  TBUF_EEE_EN  = (1 shl 0);
  TBUF_PM_EN  = (1 shl 1);

 TBUF_CTRL_V1  = $80;
 TBUF_BP_MC_V1  = $A0;

 {Hardware Filter Block (HFB) registers}
 HFB_CTRL  = $00;
 HFB_FLT_ENABLE_V3PLUS = $04;
 HFB_FLT_LEN_V2  = $04;
 HFB_FLT_LEN_V3PLUS = $1C;

 {UniMAC intrl2 registers}
 INTRL2_CPU_STAT  = $00;
 INTRL2_CPU_SET  = $04;
 INTRL2_CPU_CLEAR = $08;
 INTRL2_CPU_MASK_STATUS = $0C;
 INTRL2_CPU_MASK_SET = $10;
 INTRL2_CPU_MASK_CLEAR = $14;

 {INTRL2 IRQ0 definitions}
 UMAC_IRQ0_SCB  = (1 shl 0);
 UMAC_IRQ0_EPHY  = (1 shl 1);
 UMAC_IRQ0_PHY_DET_R = (1 shl 2);
 UMAC_IRQ0_PHY_DET_F = (1 shl 3);
 UMAC_IRQ0_LINK_UP = (1 shl 4);
 UMAC_IRQ0_LINK_DOWN = (1 shl 5);
 UMAC_IRQ0_LINK_EVENT = (UMAC_IRQ0_LINK_UP or UMAC_IRQ0_LINK_DOWN);
 UMAC_IRQ0_UMAC  = (1 shl 6);
 UMAC_IRQ0_UMAC_TSV = (1 shl 7);
 UMAC_IRQ0_TBUF_UNDERRUN = (1 shl 8);
 UMAC_IRQ0_RBUF_OVERFLOW = (1 shl 9);
 UMAC_IRQ0_HFB_SM  = (1 shl 10);
 UMAC_IRQ0_HFB_MM  = (1 shl 11);
 UMAC_IRQ0_MPD_R  = (1 shl 12);
 UMAC_IRQ0_RXDMA_MBDONE = (1 shl 13);
 UMAC_IRQ0_RXDMA_PDONE = (1 shl 14);
 UMAC_IRQ0_RXDMA_BDONE = (1 shl 15);
 UMAC_IRQ0_RXDMA_DONE = UMAC_IRQ0_RXDMA_MBDONE;
 UMAC_IRQ0_TXDMA_MBDONE = (1 shl 16);
 UMAC_IRQ0_TXDMA_PDONE = (1 shl 17);
 UMAC_IRQ0_TXDMA_BDONE = (1 shl 18);
 UMAC_IRQ0_TXDMA_DONE = UMAC_IRQ0_TXDMA_MBDONE;

 {Only valid for GENETv3+}
 UMAC_IRQ0_MDIO_DONE = (1 shl 23);
 UMAC_IRQ0_MDIO_ERROR = (1 shl 24);

 {INTRL2 IRQ1 definitions}
 UMAC_IRQ1_TX_INTR_MASK = $FFFF;
 UMAC_IRQ1_RX_INTR_MASK = $FFFF;
 UMAC_IRQ1_RX_INTR_SHIFT = 16;

 {Register block offsets}
 GENET_SYS_OFF       = $0000;
 GENET_GR_BRIDGE_OFF = $0040;
 GENET_EXT_OFF       = $0080;
 GENET_INTRL2_0_OFF  = $0200;
 GENET_INTRL2_1_OFF  = $0240;
 GENET_RBUF_OFF      = $0300;
 GENET_UMAC_OFF      = $0800;

 {SYS block offsets and register definitions}
 SYS_REV_CTRL  = $00;
 SYS_PORT_CTRL  = $04;
  PORT_MODE_INT_EPHY = 0;
  PORT_MODE_INT_GPHY = 1;
  PORT_MODE_EXT_EPHY = 2;
  PORT_MODE_EXT_GPHY = 3;
  PORT_MODE_EXT_RVMII_25 = (4 or (1 shl 4));
  PORT_MODE_EXT_RVMII_50 = 4;
  LED_ACT_SOURCE_MAC = (1 shl 9);

  SYS_RBUF_FLUSH_CTRL = $08;
  SYS_TBUF_FLUSH_CTRL = $0C;
  RBUF_FLUSH_CTRL_V1 = $04;

 {Ext block register offsets and definitions}
 EXT_EXT_PWR_MGMT = $00;
  EXT_PWR_DOWN_BIAS = (1 shl 0);
  EXT_PWR_DOWN_DLL = (1 shl 1);
  EXT_PWR_DOWN_PHY = (1 shl 2);
  EXT_PWR_DN_EN_LD = (1 shl 3);
  EXT_ENERGY_DET  = (1 shl 4);
  EXT_IDDQ_FROM_PHY = (1 shl 5);
  EXT_IDDQ_GLBL_PWR = (1 shl 7);
  EXT_PHY_RESET  = (1 shl 8);
  EXT_ENERGY_DET_MASK = (1 shl 12);
  EXT_PWR_DOWN_PHY_TX = (1 shl 16);
  EXT_PWR_DOWN_PHY_RX = (1 shl 17);
  EXT_PWR_DOWN_PHY_SD = (1 shl 18);
  EXT_PWR_DOWN_PHY_RD = (1 shl 19);
  EXT_PWR_DOWN_PHY_EN = (1 shl 20);

 EXT_RGMII_OOB_CTRL = $0C;
  RGMII_MODE_EN_V123 = (1 shl 0);
  RGMII_LINK  = (1 shl 4);
  OOB_DISABLE  = (1 shl 5);
  RGMII_MODE_EN  = (1 shl 6);
  ID_MODE_DIS  = (1 shl 16);

 EXT_GPHY_CTRL  = $1C;
  EXT_CFG_IDDQ_BIAS = (1 shl 0);
  EXT_CFG_PWR_DOWN = (1 shl 1);
  EXT_CK25_DIS  = (1 shl 4);
  EXT_GPHY_RESET  = (1 shl 5);

 {DMA rings size}
 DMA_RING_SIZE  = $40;
 DMA_RINGS_SIZE = (DMA_RING_SIZE * (GENET_DESC_INDEX + 1));

 {DMA registers common definitions}
 DMA_RW_POINTER_MASK = $1FF;
 DMA_P_INDEX_DISCARD_CNT_MASK = $FFFF;
 DMA_P_INDEX_DISCARD_CNT_SHIFT = 16;
 DMA_BUFFER_DONE_CNT_MASK = $FFFF;
 DMA_BUFFER_DONE_CNT_SHIFT = 16;
 DMA_P_INDEX_MASK = $FFFF;
 DMA_C_INDEX_MASK = $FFFF;

 {DMA ring size register}
 DMA_RING_SIZE_MASK = $FFFF;
 DMA_RING_SIZE_SHIFT = 16;
 DMA_RING_BUFFER_SIZE_MASK = $FFFF;

 {DMA interrupt threshold register}
 DMA_INTR_THRESHOLD_MASK = $01FF;

 {DMA XON/XOFF register}
 DMA_XON_THREHOLD_MASK = $FFFF;
 DMA_XOFF_THRESHOLD_MASK = $FFFF;
 DMA_XOFF_THRESHOLD_SHIFT = 16;

 {DMA flow period register}
 DMA_FLOW_PERIOD_MASK = $FFFF;
 DMA_MAX_PKT_SIZE_MASK = $FFFF;
 DMA_MAX_PKT_SIZE_SHIFT = 16;

 {DMA control register}
 DMA_EN   = (1 shl 0);
 DMA_RING_BUF_EN_SHIFT = $01;
 DMA_RING_BUF_EN_MASK = $FFFF;
 DMA_TSB_SWAP_EN  = (1 shl 20);

 {DMA status register}
 DMA_DISABLED  = (1 shl 0);
 DMA_DESC_RAM_INIT_BUSY = (1 shl 1);

 {DMA SCB burst size register}
 DMA_SCB_BURST_SIZE_MASK = $1F;

 {DMA activity vector register}
 DMA_ACTIVITY_VECTOR_MASK = $1FFFF;

 {DMA backpressure mask register}
 DMA_BACKPRESSURE_MASK = $1FFFF;
 DMA_PFC_ENABLE  = (1 shl 31);

 {DMA backpressure status register}
 DMA_BACKPRESSURE_STATUS_MASK = $1FFFF;

 {DMA override register}
 DMA_LITTLE_ENDIAN_MODE = (1 shl 0);
 DMA_REGISTER_MODE = (1 shl 1);

 {DMA timeout register}
 DMA_TIMEOUT_MASK = $FFFF;
 DMA_TIMEOUT_VAL  = 5000; {micro seconds}

 {TDMA rate limiting control register}
 DMA_RATE_LIMIT_EN_MASK = $FFFF;

 {TDMA arbitration control register}
 DMA_ARBITER_MODE_MASK = $03;
 DMA_RING_BUF_PRIORITY_MASK = $1F;
 DMA_RING_BUF_PRIORITY_SHIFT = 5;
 DMA_RATE_ADJ_MASK = $FF;

 {TX/RX DMA Descriptor common bits}
 DMA_BUFLENGTH_MASK = $0fff;
 DMA_BUFLENGTH_SHIFT = 16;
 DMA_OWN   = $8000;
 DMA_EOP   = $4000;
 DMA_SOP   = $2000;
 DMA_WRAP  = $1000;

 {TX specific DMA descriptor bits}
 DMA_TX_UNDERRUN  = $0200;
 DMA_TX_APPEND_CRC = $0040;
 DMA_TX_OW_CRC  = $0020;
 DMA_TX_DO_CSUM  = $0010;
 DMA_TX_QTAG_SHIFT = 7;

 {RX Specific DMA descriptor bits}
 DMA_RX_CHK_V3PLUS = $8000;
 DMA_RX_CHK_V12  = $1000;
 DMA_RX_BRDCAST  = $0040;
 DMA_RX_MULT  = $0020;
 DMA_RX_LG  = $0010;
 DMA_RX_NO  = $0008;
 DMA_RX_RXER  = $0004;
 DMA_RX_CRC_ERROR = $0002;
 DMA_RX_OV  = $0001;
 DMA_RX_FI_MASK  = $001F;
 DMA_RX_FI_SHIFT  = $0007;
 DMA_DESC_ALLOC_MASK = $00FF;

 DMA_ARBITER_RR  = $00;
 DMA_ARBITER_WRR  = $01;
 DMA_ARBITER_SP  = $02;

 {Power management mode}
 GENET_POWER_CABLE_SENSE = 0;
 GENET_POWER_PASSIVE     = 1;
 GENET_POWER_WOL_MAGIC   = 2;

 {Hardware flags}
 GENET_HAS_40BITS = (1 shl 0);
 GENET_HAS_EXT  = (1 shl 1);
 GENET_HAS_MDIO_INTR = (1 shl 2);
 GENET_HAS_MOCA_LINK_DET = (1 shl 3);

 {RX/TX DMA registers}
 DMA_RING_CFG       = 0;
 DMA_CTRL           = 1;
 DMA_STATUS         = 2;
 DMA_SCB_BURST_SIZE = 3;
 DMA_ARB_CTRL       = 4;
 DMA_PRIORITY_0     = 5;
 DMA_PRIORITY_1     = 6;
 DMA_PRIORITY_2     = 7;
 DMA_INDEX2RING_0   = 8;
 DMA_INDEX2RING_1   = 9;
 DMA_INDEX2RING_2   = 10;
 DMA_INDEX2RING_3   = 11;
 DMA_INDEX2RING_4   = 12;
 DMA_INDEX2RING_5   = 13;
 DMA_INDEX2RING_6   = 14;
 DMA_INDEX2RING_7   = 15;
 DMA_RING0_TIMEOUT  = 16;
 DMA_RING1_TIMEOUT  = 17;
 DMA_RING2_TIMEOUT  = 18;
 DMA_RING3_TIMEOUT  = 19;
 DMA_RING4_TIMEOUT  = 20;
 DMA_RING5_TIMEOUT  = 21;
 DMA_RING6_TIMEOUT  = 22;
 DMA_RING7_TIMEOUT  = 23;
 DMA_RING8_TIMEOUT  = 24;
 DMA_RING9_TIMEOUT  = 25;
 DMA_RING10_TIMEOUT = 26;
 DMA_RING11_TIMEOUT = 27;
 DMA_RING12_TIMEOUT = 28;
 DMA_RING13_TIMEOUT = 29;
 DMA_RING14_TIMEOUT = 30;
 DMA_RING15_TIMEOUT = 31;
 DMA_RING16_TIMEOUT = 32;

 {RDMA/TDMA ring registers}
 {Merge the common fields and just prefix with T/D the registers having different meaning depending on the direction}
 TDMA_READ_PTR        = 0;
 RDMA_WRITE_PTR       = TDMA_READ_PTR;
 TDMA_READ_PTR_HI     = 1;
 RDMA_WRITE_PTR_HI    = TDMA_READ_PTR_HI;
 TDMA_CONS_INDEX      = 2;
 RDMA_PROD_INDEX      = TDMA_CONS_INDEX;
 TDMA_PROD_INDEX      = 3;
 RDMA_CONS_INDEX      = TDMA_PROD_INDEX;
 DMA_RING_BUF_SIZE    = 4;
 DMA_START_ADDR       = 5;
 DMA_START_ADDR_HI    = 6;
 DMA_END_ADDR         = 7;
 DMA_END_ADDR_HI      = 8;
 DMA_MBUF_DONE_THRESH = 9;
 TDMA_FLOW_PERIOD     = 10;
 RDMA_XON_XOFF_THRESH = TDMA_FLOW_PERIOD;
 TDMA_WRITE_PTR       = 11;
 RDMA_READ_PTR        = TDMA_WRITE_PTR;
 TDMA_WRITE_PTR_HI    = 12;
 RDMA_READ_PTR_HI     = TDMA_WRITE_PTR_HI;

 {UniMAC specific constants (Broadcom UniMAC MDIO bus controller)}
 MDIO_CMD = $00;
  {See UMAC_MDIO_CMD}
 {MDIO_START_BUSY =(1 shl 29);
  MDIO_READ_FAIL  =(1 shl 28);
  MDIO_RD  = (2 shl 26);
  MDIO_WR  = (1 shl 26);
  MDIO_PMD_SHIFT = 21;
  MDIO_PMD_MASK = $1F;
  MDIO_REG_SHIFT = 16;
  MDIO_REG_MASK = $1F;}

 MDIO_CFG = $04;
  MDIO_C22 = (1 shl 0);
  MDIO_C45 = 0;
  MDIO_CLK_DIV_SHIFT = 4;
  MDIO_CLK_DIV_MASK = $3F;
  MDIO_SUPP_PREAMBLE = (1 shl 12);

 {Broadcom PHY ID}
 PHY_ID_BCM50610   = $0143bd60;
 PHY_ID_BCM50610M  = $0143bd70;
 PHY_ID_BCM5241    = $0143bc30;
 PHY_ID_BCMAC131   = $0143bc70;
 PHY_ID_BCM5481    = $0143bca0;
 PHY_ID_BCM5395    = $0143bcf0;
 PHY_ID_BCM54810   = $03625d00;
 PHY_ID_BCM5482    = $0143bcb0;
 PHY_ID_BCM5411    = $00206070;
 PHY_ID_BCM5421    = $002060e0;
 PHY_ID_BCM54210E  = $600d84a0;
 PHY_ID_BCM5464    = $002060b0;
 PHY_ID_BCM5461    = $002060c0;
 PHY_ID_BCM54612E  = $03625e60;
 PHY_ID_BCM54616S  = $03625d10;
 PHY_ID_BCM57780   = $03625d90;
 PHY_ID_BCM89610   = $03625cd0;

 PHY_ID_BCM7250    = $ae025280;
 PHY_ID_BCM7255    = $ae025120;
 PHY_ID_BCM7260    = $ae025190;
 PHY_ID_BCM7268    = $ae025090;
 PHY_ID_BCM7271    = $ae0253b0;
 PHY_ID_BCM7278    = $ae0251a0;
 PHY_ID_BCM7364    = $ae025260;
 PHY_ID_BCM7366    = $600d8490;
 PHY_ID_BCM7346    = $600d8650;
 PHY_ID_BCM7362    = $600d84b0;
 PHY_ID_BCM7425    = $600d86b0;
 PHY_ID_BCM7429    = $600d8730;
 PHY_ID_BCM7435    = $600d8750;
 PHY_ID_BCM74371   = $ae0252e0;
 PHY_ID_BCM7439    = $600d8480;
 PHY_ID_BCM7439_2  = $ae025080;
 PHY_ID_BCM7445    = $600d8510;

 PHY_ID_BCM_CYGNUS = $ae025200;
 PHY_ID_BCM_OMEGA  = $ae025100;

 PHY_ID_MASK       = $fffffff0;

 {Broadcom PHY Flags}
 PHY_BCM_FLAGS_MODE_COPPER   = $00000001;
 PHY_BCM_FLAGS_MODE_1000BX   = $00000002;
 PHY_BCM_FLAGS_INTF_SGMII    = $00000010;
 PHY_BCM_FLAGS_INTF_XAUI     = $00000020;
 PHY_BRCM_WIRESPEED_ENABLE   = $00000100;
 PHY_BRCM_AUTO_PWRDWN_ENABLE = $00000200;
 PHY_BRCM_RX_REFCLK_UNUSED   = $00000400;
 PHY_BRCM_STD_IBND_DISABLE   = $00000800;
 PHY_BRCM_EXT_IBND_RX_ENABLE = $00001000;
 PHY_BRCM_EXT_IBND_TX_ENABLE = $00002000;
 PHY_BRCM_CLEAR_RGMII_MODE   = $00004000;
 PHY_BRCM_DIS_TXCRXC_NOENRGY = $00008000;
 PHY_BRCM_EN_MASTER_MODE     = $00010000;

 {Broadcom BCM54XX register definitions, common to most Broadcom PHYs}
 MII_BCM54XX_ECR  = $10; {BCM54xx extended control register}
 MII_BCM54XX_ECR_IM = $1000; {Interrupt mask}
 MII_BCM54XX_ECR_IF = $0800; {Interrupt force}

 MII_BCM54XX_ESR  = $11; {BCM54xx extended status register}
 MII_BCM54XX_ESR_IS = $1000; {Interrupt status}

 MII_BCM54XX_EXP_DATA = $15; {Expansion register data}
 MII_BCM54XX_EXP_SEL = $17; {Expansion register select}
 MII_BCM54XX_EXP_SEL_SSD = $0e00; {Secondary SerDes select}
 MII_BCM54XX_EXP_SEL_ER = $0f00; {Expansion register select}
 MII_BCM54XX_EXP_SEL_ETC = $0d00; {Expansion register spare + 2k mem}

 MII_BCM54XX_AUX_CTL = $18; {Auxiliary control register}
 MII_BCM54XX_ISR  = $1a; {BCM54xx interrupt status register}
 MII_BCM54XX_IMR  = $1b; {BCM54xx interrupt mask register}
 MII_BCM54XX_INT_CRCERR = $0001; {CRC error}
 MII_BCM54XX_INT_LINK = $0002; {Link status changed}
 MII_BCM54XX_INT_SPEED = $0004; {Link speed change}
 MII_BCM54XX_INT_DUPLEX = $0008; {Duplex mode changed}
 MII_BCM54XX_INT_LRS = $0010; {Local receiver status changed}
 MII_BCM54XX_INT_RRS = $0020; {Remote receiver status changed}
 MII_BCM54XX_INT_SSERR = $0040; {Scrambler synchronization error}
 MII_BCM54XX_INT_UHCD = $0080; {Unsupported HCD negotiated}
 MII_BCM54XX_INT_NHCD = $0100; {No HCD}
 MII_BCM54XX_INT_NHCDL = $0200; {No HCD link}
 MII_BCM54XX_INT_ANPR = $0400; {Auto-negotiation page received}
 MII_BCM54XX_INT_LC = $0800; {All counters below 128}
 MII_BCM54XX_INT_HC = $1000; {Counter above 32768}
 MII_BCM54XX_INT_MDIX = $2000; {MDIX status change}
 MII_BCM54XX_INT_PSERR = $4000; {Pair swap error}

 MII_BCM54XX_SHD  = $1c; {0x1c shadow registers}
 MII_BCM54XX_SHD_WRITE = $8000;

 {Broadcom Auxilliary Control Shadow Access Registers (PHY REG 0x18)}
 MII_BCM54XX_AUXCTL_SHDWSEL_AUXCTL = $00;
 MII_BCM54XX_AUXCTL_ACTL_TX_6DB  = $0400;
 MII_BCM54XX_AUXCTL_ACTL_SMDSP_ENA = $0800;

 MII_BCM54XX_AUXCTL_SHDWSEL_MISC   = $07;
 MII_BCM54XX_AUXCTL_SHDWSEL_MISC_WIRESPEED_EN = $0010;
 MII_BCM54XX_AUXCTL_SHDWSEL_MISC_RGMII_SKEW_EN = $0100;
 MII_BCM54XX_AUXCTL_MISC_FORCE_AMDIX  = $0200;
 MII_BCM54XX_AUXCTL_MISC_WREN   = $8000;

 MII_BCM54XX_AUXCTL_SHDWSEL_READ_SHIFT = 12;
 MII_BCM54XX_AUXCTL_SHDWSEL_MASK = $0007;

 {Broadcom LED source encodings (These are used in BCM5461, BCM5481,BCM5482, and possibly some others)}
 BCM_LED_SRC_LINKSPD1 = $0;
 BCM_LED_SRC_LINKSPD2 = $1;
 BCM_LED_SRC_XMITLED = $2;
 BCM_LED_SRC_ACTIVITYLED = $3;
 BCM_LED_SRC_FDXLED = $4;
 BCM_LED_SRC_SLAVE = $5;
 BCM_LED_SRC_INTR = $6;
 BCM_LED_SRC_QUALITY = $7;
 BCM_LED_SRC_RCVLED = $8;
 BCM_LED_SRC_WIRESPEED = $9;
 BCM_LED_SRC_MULTICOLOR1 = $a;
 BCM_LED_SRC_OPENSHORT = $b;
 BCM_LED_SRC_OFF  = $e; {Tied high}
 BCM_LED_SRC_ON  = $f; {Tied low}

 {Broadcom Multicolor LED configurations (expansion register 4)}
 BCM_EXP_MULTICOLOR = (MII_BCM54XX_EXP_SEL_ER + $04);
 BCM_LED_MULTICOLOR_IN_PHASE = 1 shl 8;
 BCM_LED_MULTICOLOR_LINK_ACT = $0;
 BCM_LED_MULTICOLOR_SPEED = $1;
 BCM_LED_MULTICOLOR_ACT_FLASH = $2;
 BCM_LED_MULTICOLOR_FDX  = $3;
 BCM_LED_MULTICOLOR_OFF  = $4;
 BCM_LED_MULTICOLOR_ON  = $5;
 BCM_LED_MULTICOLOR_ALT  = $6;
 BCM_LED_MULTICOLOR_FLASH = $7;
 BCM_LED_MULTICOLOR_LINK  = $8;
 BCM_LED_MULTICOLOR_ACT  = $9;
 BCM_LED_MULTICOLOR_PROGRAM = $a;

 {BCM5482 Shadow registers}
 {Shadow values go into bits [14:10] of register 0x1c to select a shadow register to access}
 {00100: Reserved control register 2}
 BCM54XX_SHD_SCR2  = $04;
  BCM54XX_SHD_SCR2_WSPD_RTRY_DIS = $100;
  BCM54XX_SHD_SCR2_WSPD_RTRY_LMT_SHIFT = 2;
  BCM54XX_SHD_SCR2_WSPD_RTRY_LMT_OFFSET = 2;
  BCM54XX_SHD_SCR2_WSPD_RTRY_LMT_MASK = $7;

 {00101: Spare Control Register 3}
 BCM54XX_SHD_SCR3  = $05;
  BCM54XX_SHD_SCR3_DEF_CLK125 = $0001;
  BCM54XX_SHD_SCR3_DLLAPD_DIS = $0002;
  BCM54XX_SHD_SCR3_TRDDAPD = $0004;

 {01010: Auto Power-Down}
 BCM54XX_SHD_APD   = $0a;
  BCM_APD_CLR_MASK  = $FE9F; {clear bits 5, 6 & 8}
  BCM54XX_SHD_APD_EN  = $0020;
  BCM_NO_ANEG_APD_EN  = $0060; {bits 5 & 6}
  BCM_APD_SINGLELP_EN = $0100; {Bit 8}

 BCM5482_SHD_LEDS1 = $0d; {01101: LED Selector 1}
 BCM54XX_SHD_RGMII_MODE = $0b; {01011: RGMII Mode Selector}
 BCM5482_SHD_SSD  = $14; {10100: Secondary SerDes control}
 BCM5482_SHD_SSD_LEDM = $0008; {SSD LED Mode enable}
 BCM5482_SHD_SSD_EN = $0001; {SSD enable}
 BCM5482_SHD_MODE = $1f; {11111: Mode Control Register}
 BCM5482_SHD_MODE_1000BX = $0001; {Enable 1000BASE-X registers}

 {Expansion Shadow Access Registers (PHY REG 0x15, 0x16, and 0x17)}
 MII_BCM54XX_EXP_AADJ1CH0  = $001f;
  MII_BCM54XX_EXP_AADJ1CH0_SWP_ABCD_OEN = $0200;
  MII_BCM54XX_EXP_AADJ1CH0_SWSEL_THPF = $0100;
 MII_BCM54XX_EXP_AADJ1CH3  = $601f;
  MII_BCM54XX_EXP_AADJ1CH3_ADCCKADJ = $0002;
 MII_BCM54XX_EXP_EXP08   = $0F08;
  MII_BCM54XX_EXP_EXP08_RJCT_2MHZ = $0001;
  MII_BCM54XX_EXP_EXP08_EARLY_DAC_WAKE = $0200;
 MII_BCM54XX_EXP_EXP75   = $0f75;
  MII_BCM54XX_EXP_EXP75_VDACCTRL  = $003c;
  MII_BCM54XX_EXP_EXP75_CM_OSC  = $0001;
 MII_BCM54XX_EXP_EXP96   = $0f96;
  MII_BCM54XX_EXP_EXP96_MYST  = $0010;
 MII_BCM54XX_EXP_EXP97   = $0f97;
  MII_BCM54XX_EXP_EXP97_MYST  = $0c0c;

 {BCM5482 Secondary SerDes registers}
 BCM5482_SSD_1000BX_CTL  = $00; {1000BASE-X Control}
 BCM5482_SSD_1000BX_CTL_PWRDOWN = $0800; {Power-down SSD}
 BCM5482_SSD_SGMII_SLAVE  = $15; {SGMII Slave Register}
 BCM5482_SSD_SGMII_SLAVE_EN = $0002; {Slave mode enable}
 BCM5482_SSD_SGMII_SLAVE_AD = $0001; {Slave auto-detection}

 {BCM54810 Registers}
 BCM54810_EXP_BROADREACH_LRE_MISC_CTL = (MII_BCM54XX_EXP_SEL_ER + $90);
 BCM54810_EXP_BROADREACH_LRE_MISC_CTL_EN = (1 shl 0);
 BCM54810_SHD_CLK_CTL = $3;
 BCM54810_SHD_CLK_CTL_GTXCLK_EN = (1 shl 9);

 {BCM54612E Registers}
 BCM54612E_EXP_SPARE0 = (MII_BCM54XX_EXP_SEL_ETC + $34);
 BCM54612E_LED4_CLK125OUT_EN = (1 shl 1);

{==============================================================================}
type
 {GENET specific types (Broadcom Gigabit Ethernet controller)}
 PGENETNetwork = ^TGENETNetwork;

 {RX/TX Control Block}
 PGENETControlBlock = ^TGENETControlBlock;
 TGENETControlBlock = record
  DescriptorAddress:PtrUInt;
  Entry:PNetworkEntry;
 end;

 PGENETControlBlocks = ^TGENETControlBlocks;
 TGENETControlBlocks = array[0..GENET_TOTAL_DESC - 1] of TGENETControlBlock;

 {RX Ring Buffer}
 PGENETRXRing = ^TGENETRXRing;

 TGENETRXRingIntEnable = procedure(Network:PGENETNetwork;Ring:PGENETRXRing);{$IFDEF i386} stdcall;{$ENDIF}
 TGENETRXRingIntDisable = procedure(Network:PGENETNetwork;Ring:PGENETRXRing);{$IFDEF i386} stdcall;{$ENDIF}

 TGENETRXRing = record
  Network:PGENETNetwork;             {The owner of this RX ring}
  Worker:TWorkerHandle;              {Handle of worker currently servicing RX ring}
  Index:LongWord;                    {RX ring index}
  Size:LongWord;                     {RX ring size}
  Consumer:LongWord;                 {RX last consumer index}
  Read:LongWord;                     {RX ring read pointer}
  First:LongWord;                    {RX ring initial CB ptr}
  Last:LongWord;                     {RX ring end CB ptr}
  OldDiscards:LongWord;
  ControlBlocks:PGENETControlBlocks; {RX ring buffer control block}
  IntEnable:TGENETRXRingIntEnable;
  IntDisable:TGENETRXRingIntDisable;
 end;

 PGENETRXRings = ^TGENETRXRings;
 TGENETRXRings = array[0..GENET_DESC_INDEX] of TGENETRXRing;

 {TX Ring Buffer}
 PGENETTXRing = ^TGENETTXRing;

 TGENETTXRingIntEnable = procedure(Network:PGENETNetwork;Ring:PGENETTXRing);{$IFDEF i386} stdcall;{$ENDIF}
 TGENETTXRingIntDisable = procedure(Network:PGENETNetwork;Ring:PGENETTXRing);{$IFDEF i386} stdcall;{$ENDIF}

 TGENETTXRing = record
  Network:PGENETNetwork;             {The owner of this TX ring}
  Worker:TWorkerHandle;              {Handle of worker currently servicing TX ring}
  Index:LongWord;                    {TX ring index}
  Queue:LongWord;                    {TX queue index}
  Size:LongWord;                     {TX ring size}
  Clean:LongWord;                    {TX ring clean pointer}
  Consumer:LongWord;                 {TX last consumer index}
  Free:LongWord;                     {TX number of free descriptors}
  Write:LongWord;                    {TX ring write pointer SW copy}
  Producer:LongWord;                 {TX ring producer index SW copy}
  First:LongWord;                    {TX ring initial CB ptr}
  Last:LongWord;                     {TX ring end CB ptr}
  ControlBlocks:PGENETControlBlocks; {TX ring buffer control block}
  IntEnable:TGENETTXRingIntEnable;
  IntDisable:TGENETTXRingIntDisable;
 end;

 PGENETTXRings = ^TGENETTXRings;
 TGENETTXRings = array[0..GENET_DESC_INDEX] of TGENETTXRing;

 {RX/TX DMA registers}
 PGENETDMARegisters = ^TGENETDMARegisters;
 TGENETDMARegisters = array[DMA_RING_CFG..DMA_RING16_TIMEOUT] of Byte;

 {RX/TX DMA ring registers}
 PGENETDMARingRegisters = ^TGENETDMARingRegisters;
 TGENETDMARingRegisters = array[TDMA_READ_PTR..TDMA_WRITE_PTR_HI] of Byte;

 TGENETNetwork = record
  {Network Properties}
  Network:TNetworkDevice;
  {Driver Properties}
  IRQ0:LongWord;
  IRQ1:LongWord;
  Address:Pointer;                              {Device register base address}
  MDIOOffset:LongWord;
  Lock:TSpinHandle;                             {Device lock (Differs from lock in Network device) (Spin lock due to use by interrupt handler)}
  Version:LongWord;                             {Device version (GENET_V1..GENETV5)}
  PhyRevision:LongWord;                         {PHY revision (GENET_V1..GENETV4 devices only)}
  Status64Enable:LongBool;                      {True if 64 byte RX/TX status block is enabled (for hardware checksum etc)}
  RXChecksumEnable:LongBool;                    {True if RX checksum is enabled}
  CRCForwardEnable:LongBool;                    {True if CRC forwarding is enabled}
  IRQStatus:LongWord;                           {Non TX/RX interrupts forwarded to worker thread for servicing}
  PendingCount:LongWord;                        {Number of worker requests pending for this network}
  WaiterThread:TThreadId;                       {Thread waiting for pending requests to complete (for network close)}
  StatusTimer:TTimerHandle;                     {Timer for status change detection}

  {Hardware Parameters (Version specific)}
  TXQueues:LongWord;
  TXDescriptorsPerQueue:LongWord;
  RXQueues:LongWord;
  RXDescriptorsPerQueue:LongWord;
  BackPressureEnableShift:LongWord;
  BackPressureMask:LongWord;
  HFBFilterCount:LongWord;
  HFBFilterSize:LongWord;
  QTAGMask:LongWord;
  TXBufferOffset:LongWord;
  HFBOffset:LongWord;
  HFBRegOffset:LongWord;
  RXDMAOffset:LongWord;
  TXDMAOffset:LongWord;
  WordsPerDescriptor:LongWord;
  Flags:LongWord;
  Q16TXDescriptorCount:LongWord;
  Q16RXDescriptorCount:LongWord;

  {DMA Parameters (Version specific)}
  DMARXCheckBit:LongWord;
  RXDMARegOffset:LongWord;
  TXDMARegOffset:LongWord;
  DMADescriptorSize:LongWord;
  DMARegisters:TGENETDMARegisters;
  DMARingRegisters:TGENETDMARingRegisters;

  {PHY Parameters}
  PhyId:LongWord;        {PHY Identifier}
  PhyAddr:LongWord;      {PHY Bus Address}
  PhyMode:LongWord;      {PHY Interface Mode}
  PhyFlags:LongWord;     {PHY Flags}
  PhyInternal:LongBool;
  PhyExternal:LongBool;
  Link:LongInt;          {0 = Down / 1 = Up}
  Speed:LongInt;         {10/100/1000 Mbps}
  Duplex:LongInt;        {0 = Half / 1 = Full}
  Pause:LongInt;
  OldLink:LongInt;
  OldSpeed:LongInt;
  OldDuplex:LongInt;
  OldPause:LongInt;

  {RX/TX Parameters}
  RXRings:TGENETRXRings;
  TXRings:TGENETTXRings;
  RXControlBlocks:PGENETControlBlocks;
  TXControlBlocks:PGENETControlBlocks;

  {Statistics Properties}
  InterruptCount:LongWord; {Number of interrupt requests received by the device}
 end;

 PGENETStatus64 = ^TGENETStatus64;
 TGENETStatus64 = record
  LengthStatus:LongWord;           {Length and peripheral status}
  ExtendedStatus:LongWord;         {Extended status}
  RXChecksum:LongWord;             {Partial RX checksum}
  Unused1:array[0..8] of LongWord; {Unused}
  TXCheckusmInfo:LongWord;         {TX checksum info}
  Unused2:array[0..2] of LongWord; {Unused}
 end;

 {UniMAC specific types (Broadcom UniMAC MDIO bus controller)}
 {Nothing}

{==============================================================================}
var
 {GENET specific variables}
 GENET_PHY_MODE:String;
 GENET_PHY_ADDR:LongWord;
 GENET_SKIP_UMAC_RESET:Boolean;
 GENET_NO_PHY_INTERRUPT:Boolean;

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{GENET Functions}
function GENETNetworkCreate(Address:PtrUInt;MDIOOffset:LongWord;IRQ0,IRQ1:LongWord):PNetworkDevice;
function GENETNetworkDestroy(Network:PNetworkDevice):LongWord;

{==============================================================================}
{GENET Network Functions}
function GENETNetworkOpen(Network:PNetworkDevice):LongWord;
function GENETNetworkClose(Network:PNetworkDevice):LongWord;
function GENETNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function GENETBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function GENETBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
function GENETBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
function GENETBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;

{==============================================================================}
{GENET Helper Functions}
function GENETGetHardwareParameters(Network:PGENETNetwork):LongWord;

function GENETGetMACAddress(Network:PGENETNetwork;Address:PHardwareAddress):LongWord;
function GENETSetMACAddress(Network:PGENETNetwork;Address:PHardwareAddress):LongWord;

procedure GENETPowerUp(Network:PGENETNetwork;Mode:LongWord);
function GENETPowerDown(Network:PGENETNetwork;Mode:LongWord):LongWord;

procedure GENETResetUMAC(Network:PGENETNetwork);
procedure GENETUMACReset(Network:PGENETNetwork);
procedure GENETInitUMAC(Network:PGENETNetwork);

function GENETInitializeDMA(Network:PGENETNetwork):LongWord;
function GENETFinalizeDMA(Network:PGENETNetwork):LongWord;
function GENETShutdownDMA(Network:PGENETNetwork):LongWord;

function GENETDisableDMA(Network:PGENETNetwork;FlushRX:Boolean):LongWord;
procedure GENETEnableDMA(Network:PGENETNetwork;DMAControl:LongWord);

function GENETInitRXQueues(Network:PGENETNetwork):LongWord;
procedure GENETInitTXQueues(Network:PGENETNetwork);

function GENETInitRXRing(Network:PGENETNetwork;Index,Size,First,Last:LongWord):LongWord;
procedure GENETInitTXRing(Network:PGENETNetwork;Index,Size,First,Last:LongWord);

function GENETAllocRXBuffers(Network:PGENETNetwork;Ring:PGENETRXRing):LongWord;
procedure GENETFreeRXBuffers(Network:PGENETNetwork);

procedure GENETHFBInit(Network:PGENETNetwork);
procedure GENETHFBClear(Network:PGENETNetwork);

procedure GENETInterfaceStart(Network:PGENETNetwork);
procedure GENETInterfaceStop(Network:PGENETNetwork);

function GENETMIIProbe(Network:PGENETNetwork):LongWord;
procedure GENETMIISetup(Network:PGENETNetwork);
function GENETMIIConfig(Network:PGENETNetwork):LongWord;
function GENETMIIWait(Network:PGENETNetwork):LongWord;

function GENETPhyReadStatus(Network:PGENETNetwork):LongWord;

{==============================================================================}
{UniMAC Helper Functions}
procedure UniMACMDIOStart(Network:PGENETNetwork);
function UniMACMDIOBusy(Network:PGENETNetwork):LongWord;
function UniMACMDIOPoll(Network:PGENETNetwork):LongWord;
function UniMACMDIORead(Network:PGENETNetwork;Reg:LongWord;var Value:Word):LongWord;
function UniMACMDIOWrite(Network:PGENETNetwork;Reg:LongWord;Value:Word):LongWord;
function UniMACMDIOReset(Network:PGENETNetwork):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {GENET specific variables}

{==============================================================================}
{==============================================================================}
{GENET Network Functions}
procedure GENETTransmitStart(Network:PGENETNetwork); forward;

procedure GENETReceiveWorker(Ring:PGENETRXRing); forward;
procedure GENETTransmitWorker(Ring:PGENETTXRing); forward;
procedure GENETInterruptWorker(Network:PGENETNetwork); forward;

function GENETInterruptHandler0(Number,CPUID,Flags:LongWord;Network:PGENETNetwork):LongWord;{$IFDEF i386} stdcall;{$ENDIF} forward;
function GENETInterruptHandler1(Number,CPUID,Flags:LongWord;Network:PGENETNetwork):LongWord;{$IFDEF i386} stdcall;{$ENDIF} forward;

{==============================================================================}
{==============================================================================}
{GENET Helper Functions}
function GENETSysRead(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETSysWrite(Network:PGENETNetwork;Offset,Value:LongWord); forward;

function GENETExtRead(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETExtWrite(Network:PGENETNetwork;Offset,Value:LongWord); forward;

function GENETUMACRead(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETUMACWrite(Network:PGENETNetwork;Offset,Value:LongWord); forward;

//Interrupt L2 registers accessors
function GENETIntL20Read(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETIntL20Write(Network:PGENETNetwork;Offset,Value:LongWord); forward;

function GENETIntL21Read(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETIntL21Write(Network:PGENETNetwork;Offset,Value:LongWord); forward;

//HFB register accessors
function GENETHFBRead(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETHFBWrite(Network:PGENETNetwork;Offset,Value:LongWord); forward;

//GENET v2+ HFB control and filter len helpers
function GENETHFBRegRead(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETHFBRegWrite(Network:PGENETNetwork;Offset,Value:LongWord); forward;

//RBUF register accessors
function GENETRBUFRead(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure GENETRBUFWrite(Network:PGENETNetwork;Offset,Value:LongWord); forward;

function GENETRBUFCtrlGet(Network:PGENETNetwork):LongWord; forward;
procedure GENETRBUFCtrlSet(Network:PGENETNetwork;Value:LongWord); forward;

function GENETTBUFCtrlGet(Network:PGENETNetwork):LongWord; forward;
procedure GENETTBUFCtrlSet(Network:PGENETNetwork;Value:LongWord); forward;

function GENETBPMCGet(Network:PGENETNetwork):LongWord; forward;
procedure GENETBPMCSet(Network:PGENETNetwork;Value:LongWord); forward;

//RX/TX DMA register accessors
function GENETTDMARead(Network:PGENETNetwork;Reg:Byte):LongWord; forward;
procedure GENETTDMAWrite(Network:PGENETNetwork;Reg:Byte;Value:LongWord); forward;

function GENETRDMARead(Network:PGENETNetwork;Reg:Byte):LongWord; forward;
procedure GENETRDMAWrite(Network:PGENETNetwork;Reg:Byte;Value:LongWord); forward;

//RDMA/TDMA ring registers accessors
function GENETTDMARingRead(Network:PGENETNetwork;Ring:LongWord;Reg:Byte):LongWord; forward;
procedure GENETTDMARingWrite(Network:PGENETNetwork;Ring:LongWord;Reg:Byte;Value:LongWord); forward;

function GENETRDMARingRead(Network:PGENETNetwork;Ring:LongWord;Reg:Byte):LongWord; forward;
procedure GENETRDMARingWrite(Network:PGENETNetwork;Ring:LongWord;Reg:Byte;Value:LongWord); forward;

procedure GENETRXRingIntEnable(Network:PGENETNetwork;Ring:PGENETRXRing); forward;
procedure GENETRXRingIntDisable(Network:PGENETNetwork;Ring:PGENETRXRing); forward;
procedure GENETRXRing16IntEnable(Network:PGENETNetwork;Ring:PGENETRXRing); forward;
procedure GENETRXRing16IntDisable(Network:PGENETNetwork;Ring:PGENETRXRing); forward;

procedure GENETTXRingIntEnable(Network:PGENETNetwork;Ring:PGENETTXRing); forward;
procedure GENETTXRingIntDisable(Network:PGENETNetwork;Ring:PGENETTXRing); forward;
procedure GENETTXRing16IntEnable(Network:PGENETNetwork;Ring:PGENETTXRing); forward;
procedure GENETTXRing16IntDisable(Network:PGENETNetwork;Ring:PGENETTXRing); forward;

function GENETFreeRXControlBlock(Network:PGENETNetwork;Block:PGENETControlBlock):PNetworkEntry; forward;
function GENETRefillRXControlBlock(Network:PGENETNetwork;Block:PGENETControlBlock):PNetworkEntry; forward;

procedure GENETFreeTXControlBlock(Network:PGENETNetwork;Block:PGENETControlBlock); forward;
function GENETGetTXControlBlock(Network:PGENETNetwork;Ring:PGENETTXRing):PGENETControlBlock; forward;

procedure GENETReclaimTXControlBlocks(Network:PGENETNetwork); forward;
procedure GENETReclaimTXControlBlock(Network:PGENETNetwork;Ring:PGENETTXRing;Worker:Boolean); forward;

procedure GENETDMADescriptorSet(Network:PGENETNetwork;Descriptor,Address:PtrUInt;Value:LongWord); forward;
procedure GENETDMADescriptorSetAddress(Network:PGENETNetwork;Descriptor,Address:PtrUInt); forward;
procedure GENETDMADescriptorSetLengthStatus(Network:PGENETNetwork;Descriptor:PtrUInt;Value:LongWord); forward;

function GENETDMADescriptorGetAddress(Network:PGENETNetwork;Descriptor:PtrUInt):PtrUInt; forward;
function GENETDMADescriptorGetLengthStatus(Network:PGENETNetwork;Descriptor:PtrUInt):LongWord; forward;

procedure GENETSetRXMode(Network:PGENETNetwork); forward;
procedure GENETUMACEnableSet(Network:PGENETNetwork;Mask:LongWord;Enable:Boolean); forward;

procedure GENETInterruptDisable(Network:PGENETNetwork); forward;
procedure GENETLinkInterruptEnable(Network:PGENETNetwork); forward;

function GENETPhyGetId(Network:PGENETNetwork;var PhyId:LongWord):LongWord; forward;
function GENETPhyInitHardware(Network:PGENETNetwork):LongWord; forward;

function GENETBCMPhyReadShadow(Network:PGENETNetwork;Shadow:Word;var Value:Word):LongWord; forward;
function GENETBCMPhyWriteShadow(Network:PGENETNetwork;Shadow,Value:Word):LongWord; forward;

function GENETBCMPhyReadExpansion(Network:PGENETNetwork;Reg:Word;var Value:Word):LongWord; forward;
function GENETBCMPhyWriteExpansion(Network:PGENETNetwork;Reg:Word;Value:Word):LongWord; forward;

function GENETBCM54XXConfigInit(Network:PGENETNetwork):LongWord; forward;
function GENETBCM54210EConfigInit(Network:PGENETNetwork):LongWord; forward;
function GENETBCM54612EConfigInit(Network:PGENETNetwork):LongWord; forward;

function GENETBCM54XXPhyDSPConfig(Network:PGENETNetwork):LongWord; forward;
function GENETBCM50610A0Workaround(Network:PGENETNetwork):LongWord; forward;
function GENETBCM54XXConfigClockDelay(Network:PGENETNetwork):LongWord; forward;
function GENETBCM54XXAdjustRXReferenceClock(Network:PGENETNetwork):LongWord; forward;

function GENETBCM54XXAuxCtrlRead(Network:PGENETNetwork;Reg:Word;var Value:Word):LongWord; forward;
function GENETBCM54XXAuxCtrlWrite(Network:PGENETNetwork;Reg:Word;Value:Word):LongWord; forward;

procedure GENETStatusTimer(Network:PGENETNetwork); forward;

{==============================================================================}
{==============================================================================}
{UniMAC Helper Functions}
function _UniMACMDIORead(Network:PGENETNetwork;Offset:LongWord):LongWord; forward;
procedure _UniMACMDIOWrite(Network:PGENETNetwork;Offset,Value:LongWord); forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{GENET Functions}
function GENETNetworkCreate(Address:PtrUInt;MDIOOffset:LongWord;IRQ0,IRQ1:LongWord):PNetworkDevice;
{Create and register a new GENET Network device which can be accessed using the Network API}
{Address: The address of the GENET registers}
{MDIOOffset: The offset from address of the MDIO registers}
{IRQ0: The interrupt number for interrupt 0 of the GENET}
{IRQ1: The interrupt number for interrupt 1 of the GENET}
{Return: Pointer to the new Network device or nil if the Network device could not be created}
var
 Status:LongWord;
 WorkInt:LongWord;
 WorkBuffer:String;
 GENETNetwork:PGENETNetwork;
begin
 {}
 Result:=nil;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'GENET: Network Create (Address=' + AddrToHex(Address) + ' MDIOOffset=' + IntToHex(MDIOOffset,8) + ' IRQ0=' + IntToStr(IRQ0) + ' IRQ1=' + IntToStr(IRQ1) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;

 {Check IRQ0}
 {if IRQ0 = 0 then Exit;} {IRQ 0 is valid}

 {Check IRQ1}
 {if IRQ1 = 0 then Exit;} {IRQ 0 is valid}

 {Check Environment Variables}
 {GENET_PHY_MODE}
 WorkBuffer:=SysUtils.GetEnvironmentVariable('GENET_PHY_MODE');
 if Length(WorkBuffer) <> 0 then GENET_PHY_MODE:=WorkBuffer;

 {GENET_PHY_ADDR}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('GENET_PHY_ADDR'),GENET_PHY_ADDR);
 if WorkInt <> GENET_PHY_ADDR then GENET_PHY_ADDR:=WorkInt;

 {GENET_SKIP_UMAC_RESET}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('GENET_SKIP_UMAC_RESET'),0);
 if WorkInt <> 0 then GENET_SKIP_UMAC_RESET:=True;

 {GENET_NO_PHY_INTERRUPT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('GENET_NO_PHY_INTERRUPT'),0);
 if WorkInt <> 0 then GENET_NO_PHY_INTERRUPT:=True;

 {Create Network}
 GENETNetwork:=PGENETNetwork(NetworkDeviceCreateEx(SizeOf(TGENETNetwork)));
 if GENETNetwork <> nil then
  begin
   {Update Network}
   {Device}
   GENETNetwork.Network.Device.DeviceBus:=DEVICE_BUS_MMIO;
   GENETNetwork.Network.Device.DeviceType:=NETWORK_TYPE_ETHERNET;
   GENETNetwork.Network.Device.DeviceFlags:=NETWORK_FLAG_RX_BUFFER or NETWORK_FLAG_TX_BUFFER;
   GENETNetwork.Network.Device.DeviceData:=nil;
   GENETNetwork.Network.Device.DeviceDescription:=GENET_NETWORK_DESCRIPTION;
   {Network}
   GENETNetwork.Network.NetworkState:=NETWORK_STATE_CLOSED;
   GENETNetwork.Network.NetworkStatus:=NETWORK_STATUS_DOWN;
   GENETNetwork.Network.DeviceOpen:=GENETNetworkOpen;
   GENETNetwork.Network.DeviceClose:=GENETNetworkClose;
   GENETNetwork.Network.DeviceControl:=GENETNetworkControl;
   GENETNetwork.Network.BufferAllocate:=GENETBufferAllocate;
   GENETNetwork.Network.BufferRelease:=GENETBufferRelease;
   GENETNetwork.Network.BufferReceive:=GENETBufferReceive;
   GENETNetwork.Network.BufferTransmit:=GENETBufferTransmit;
   {GENET}
   GENETNetwork.IRQ0:=IRQ0;
   GENETNetwork.IRQ1:=IRQ1;
   GENETNetwork.Address:=Pointer(Address);
   GENETNetwork.MDIOOffset:=MDIOOffset;
   GENETNetwork.Lock:=INVALID_HANDLE_VALUE;
   GENETNetwork.WaiterThread:=INVALID_HANDLE_VALUE;
   GENETNetwork.StatusTimer:=INVALID_HANDLE_VALUE;

   {Register Network}
   Status:=NetworkDeviceRegister(@GENETNetwork.Network);
   if Status = ERROR_SUCCESS then
    begin
     {Return Result}
     Result:=PNetworkDevice(GENETNetwork);
    end
   else
    begin
     if NETWORK_LOG_ENABLED then NetworkLogError(nil,'GENET: Failed to register new Network device: ' + ErrorToString(Status));
    end;
  end
 else
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'GENET: Failed to create new Network device');
  end;
end;

{==============================================================================}

function GENETNetworkDestroy(Network:PNetworkDevice):LongWord;
{Close, deregister and destroy a GENET Network device created by this driver}
{Network: The Network device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Network Destroy');
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
     if NETWORK_LOG_ENABLED then NetworkLogError(nil,'GENET: Failed to destroy Network device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'GENET: Failed to deregister Network device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{GENET Network Functions}
function GENETNetworkOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen API for GENET Network}
{Note: Not intended to be called directly by applications, use NetworkDeviceOpen instead}
var
 Reg:LongWord;
 Status:LongWord;
 DMAControl:LongWord;
 Entry:PNetworkEntry;
 Address:PHardwareAddress;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Network Open');
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

    {Get Hardware Parameters}
    Status:=GENETGetHardwareParameters(PGENETNetwork(Network));
    if Status <> ERROR_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;

    //bcmgenet_probe

    {If this is an internal GPHY, power it on now, before UniMAC is
     brought out of reset as absolutely no UniMAC activity is allowed}
    if PGENETNetwork(Network).PhyInternal then
     begin
      GENETPowerUp(PGENETNetwork(Network),GENET_POWER_PASSIVE);
     end;

    {Reset UniMAC}
    GENETResetUMAC(PGENETNetwork(Network));

    //bcmgenet_open

    {If this is an internal GPHY, power it back on now, before UniMAC is
     brought out of reset as absolutely no UniMAC activity is allowed}
    if PGENETNetwork(Network).PhyInternal then
     begin
      GENETPowerUp(PGENETNetwork(Network),GENET_POWER_PASSIVE);
     end;

    {Reset UniMAC}
    GENETUMACReset(PGENETNetwork(Network));

    {Initialize UniMAC}
    GENETInitUMAC(PGENETNetwork(Network));

    {Check the CRC Forward Enable setting}
    Reg:=GENETUMACRead(PGENETNetwork(Network),UMAC_CMD);
    if (Reg and CMD_CRC_FWD) <> 0 then PGENETNetwork(Network).CRCForwardEnable:=True;

    {Get MAC address}
    Address:=AllocMem(SizeOf(THardwareAddress));
    GENETGetMacAddress(PGENETNetwork(Network),Address);
    {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Default Address = ' + HardwareAddressToString(Address^));
    {$ENDIF}

    {Check MAC Address}
    if not ValidHardwareAddress(Address^) then
     begin
      {Convert MAC address}
      Address^:=StringToHardwareAddress(GENET_MAC_ADDRESS);
      {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Hardware Address = ' + HardwareAddressToString(Address^));
      {$ENDIF}

      {Check MAC Address}
      if not ValidHardwareAddress(Address^) then
       begin
        {Random MAC Address}
        Address^:=RandomHardwareAddress;
        {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Random Address = ' + HardwareAddressToString(Address^));
        {$ENDIF}
       end;

      {Set MAC Address}
      Status:=GENETSetMacAddress(PGENETNetwork(Network),Address);
      if Status <> ERROR_SUCCESS then
       begin
        if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to set mac address: ' + ErrorToString(Status));
        FreeMem(Address);

        Result:=Status;
        Exit;
       end;
     end;
    FreeMem(Address);

    if PGENETNetwork(Network).PhyInternal then
     begin
      Reg:=GENETExtRead(PGENETNetwork(Network),EXT_EXT_PWR_MGMT);
      Reg:=Reg or EXT_ENERGY_DET_MASK;
      GENETExtWrite(PGENETNetwork(Network),EXT_EXT_PWR_MGMT,Reg);
     end;

    try
     {Allocate Receive Queue Buffer}
     Network.ReceiveQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),GENET_MAX_RX_ENTRIES);
     if Network.ReceiveQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to create receive queue buffer');

       Exit;
      end;

     {Allocate Receive Queue Semaphore}
     Network.ReceiveQueue.Wait:=SemaphoreCreate(0);
     if Network.ReceiveQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to create receive queue semaphore');

       Exit;
      end;

     {Allocate Receive Queue Buffers}
     Entry:=BufferIterate(Network.ReceiveQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=GENET_MAX_PACKET_SIZE;
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
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to allocate receive buffer');

         Exit;
        end;

       {Clean Cache}
       if not(DMA_CACHE_COHERENT) then
        begin
         CleanDataCacheRange(PtrUInt(Entry.Buffer),Entry.Size);
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
     SetLength(Network.ReceiveQueue.Entries,GENET_MAX_RX_ENTRIES);

     {Allocate Transmit Queue Buffer}
     Network.TransmitQueue.Buffer:=BufferCreate(SizeOf(TNetworkEntry),GENET_MAX_TX_ENTRIES);
     if Network.TransmitQueue.Buffer = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to create transmit queue buffer');

       Exit;
      end;

     {Allocate Transmit Queue Semaphore}
     Network.TransmitQueue.Wait:=SemaphoreCreate(GENET_MAX_TX_ENTRIES);
     if Network.TransmitQueue.Wait = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to create transmit queue semaphore');

       Exit;
      end;

     {Allocate Transmit Queue Buffers}
     Entry:=BufferIterate(Network.TransmitQueue.Buffer,nil);
     while Entry <> nil do
      begin
       {Initialize Entry}
       Entry.Size:=GENET_MAX_PACKET_SIZE;
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
         if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to allocate transmit buffer');

         Exit;
        end;

       {Clean Cache}
       if not(DMA_CACHE_COHERENT) then
        begin
         CleanDataCacheRange(PtrUInt(Entry.Buffer),Entry.Size);
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
     SetLength(Network.TransmitQueue.Entries,GENET_MAX_TX_ENTRIES);

     {Allocate Lock}
     PGENETNetwork(Network).Lock:=SpinCreate;
     if PGENETNetwork(Network).Lock = INVALID_HANDLE_VALUE then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to create device lock');

       Exit;
      end;

     {Disable RX/TX DMA and flush TX and RX queues}
     DMAControl:=GENETDisableDMA(PGENETNetwork(Network),True);

     {Reinitialize TXDMA and RXDMA and SW housekeeping}
     Status:=GENETInitializeDMA(PGENETNetwork(Network));
     if Status <> ERROR_SUCCESS then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to initialize DMA: ' + ErrorToString(Status));

       Result:=Status;
       Exit;
      end;

     {Always enable ring 16 - descriptor ring}
     GENETEnableDMA(PGENETNetwork(Network),DMAControl);

     {Init Hardware Filter Block}
     GENETHFBInit(PGENETNetwork(Network));

     {Request IRQ0}
     RegisterInterrupt(PGENETNetwork(Network).IRQ0,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(GENETInterruptHandler0),Network);

     {Request IRQ1}
     RegisterInterrupt(PGENETNetwork(Network).IRQ1,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(GENETInterruptHandler1),Network);

     {Init PHY}
     Status:=GENETMIIProbe(PGENETNetwork(Network));
     if Status <> ERROR_SUCCESS then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(Network,'GENET: Failed to connect to PHY: ' + ErrorToString(Status));

       Result:=Status;
       Exit;
      end;

     {Start Interface}
     GENETInterfaceStart(PGENETNetwork(Network));

     {Set State to Open}
     Network.NetworkState:=NETWORK_STATE_OPEN;

     {Notify the State}
     NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_OPEN);

     {Get Network Status}
     if PGENETNetwork(Network).Link = PHY_LINK_UP then
      begin
       {Set Status to Up}
       Network.NetworkStatus:=NETWORK_STATUS_UP;

       {Notify the Status}
       NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_UP);
      end;

     {Check PHY Interrupt}
     if GENET_NO_PHY_INTERRUPT then
      begin
       {Create Timer}
       PGENETNetwork(Network).StatusTimer:=TimerCreateEx(1000,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(GENETStatusTimer),Network); {Rescheduled by Timer Event}

       {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Created PHY Status Timer (Handle=' + AddrToHex(PGENETNetwork(Network).StatusTimer) + ')');
       {$ENDIF}
      end;

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Check Result}
     if Result <> ERROR_SUCCESS then
      begin
       {Release IRQ1}
       DeregisterInterrupt(PGENETNetwork(Network).IRQ1,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(GENETInterruptHandler1),Network);

       {Release IRQ0}
       DeregisterInterrupt(PGENETNetwork(Network).IRQ0,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(GENETInterruptHandler0),Network);

       {Shutdown DMA}
       GENETShutdownDMA(PGENETNetwork(Network));

       {Finalize DMA}
       GENETFinalizeDMA(PGENETNetwork(Network));

       {Check Lock}
       if PGENETNetwork(Network).Lock <> INVALID_HANDLE_VALUE then
        begin
         {Destroy Lock}
         SpinDestroy(PGENETNetwork(Network).Lock);
         PGENETNetwork(Network).Lock:=INVALID_HANDLE_VALUE;
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

       {Power Down internal PHY}
       if PGENETNetwork(Network).PhyInternal then
        begin
         GENETPowerDown(PGENETNetwork(Network),GENET_POWER_PASSIVE);
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

function GENETNetworkClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose API for GENET Network}
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

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Network Close');
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
    {Acquire Spin Lock}
    if SpinLockIRQ(PGENETNetwork(Network).Lock) = ERROR_SUCCESS then
     begin
      {Check Pending}
      if PGENETNetwork(Network).PendingCount <> 0 then
       begin
        {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Waiting for ' + IntToStr(PGENETNetwork(Network).PendingCount) + ' pending requests to complete');
        {$ENDIF}

        {Wait for Pending}

        {Setup Waiter}
        PGENETNetwork(Network).WaiterThread:=GetCurrentThreadId;

        {Release Spin Lock}
        SpinUnlockIRQ(PGENETNetwork(Network).Lock);

        {Release the Lock}
        MutexUnlock(Network.Lock);

        {Wait for Message}
        ThreadReceiveMessage(Message);

        {Acquire the Lock}
        if MutexLock(Network.Lock) <> ERROR_SUCCESS then Exit;
       end
      else
       begin
        {Release Spin Lock}
        SpinUnlockIRQ(PGENETNetwork(Network).Lock);
       end;
     end;

    {Set State to Closed}
    Network.NetworkState:=NETWORK_STATE_CLOSED;

    {Notify the State}
    NotifierNotify(@Network.Device,DEVICE_NOTIFICATION_CLOSE);

    {Check PHY Interrupt}
    if GENET_NO_PHY_INTERRUPT and (PGENETNetwork(Network).StatusTimer <> INVALID_HANDLE_VALUE) then
     begin
      {Destroy Timer}
      TimerDestroy(PGENETNetwork(Network).StatusTimer);
      PGENETNetwork(Network).StatusTimer:=INVALID_HANDLE_VALUE;
     end;

    //bcmgenet_close

    {Stop Interface}
    GENETInterfaceStop(PGENETNetwork(Network));

    {Release IRQ1}
    DeregisterInterrupt(PGENETNetwork(Network).IRQ1,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(GENETInterruptHandler1),Network);

    {Release IRQ0}
    DeregisterInterrupt(PGENETNetwork(Network).IRQ0,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(GENETInterruptHandler0),Network);

    {Power Down internal PHY}
    if PGENETNetwork(Network).PhyInternal then
     begin
      GENETPowerDown(PGENETNetwork(Network),GENET_POWER_PASSIVE);
     end;

    {Check Lock}
    if PGENETNetwork(Network).Lock <> INVALID_HANDLE_VALUE then
     begin
      {Destroy Lock}
      SpinDestroy(PGENETNetwork(Network).Lock);
      PGENETNetwork(Network).Lock:=INVALID_HANDLE_VALUE;
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

function GENETNetworkControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of NetworkDeviceControl API for GENET Network}
{Note: Not intended to be called directly by applications, use NetworkDeviceControl instead}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Network Control (Request=' + IntToStr(Request) + ')');
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
       {GENET}
       {Nothing}
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
       {GENET}
       {Nothing}
      end;
     NETWORK_CONTROL_SET_MAC:begin
       {Set the MAC for this device}
       Status:=GENETSetMacAddress(PGENETNetwork(Network),PHardwareAddress(Argument1));
      end;
     NETWORK_CONTROL_GET_MAC:begin
       {Get the MAC for this device}
       Status:=GENETGetMacAddress(PGENETNetwork(Network),PHardwareAddress(Argument1));
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
       {Get Hardware address for this device}
       Status:=GENETGetMacAddress(PGENETNetwork(Network),PHardwareAddress(Argument1));
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
       Status:=GENETPHYReadStatus(PGENETNetwork(Network));
       if Status = ERROR_SUCCESS then
        begin
         GENETMIISetup(PGENETNetwork(Network));
        end;
       if Status = ERROR_SUCCESS then
        begin
         if PGENETNetwork(Network).Link = PHY_LINK_UP then
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

function GENETBufferAllocate(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferAllocate API for GENET Network}
{Note: Not intended to be called directly by applications, use NetworkBufferAllocate instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Buffer Allocate');
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
   Entry.Size:=GENET_MAX_PACKET_SIZE;
   Entry.Offset:=0;
   if PGENETNetwork(Network).Status64Enable then Entry.Offset:=SizeOf(TGENETStatus64);
   Entry.Count:=1;

   {Update Packet}
   Entry.Packets[0].Buffer:=Entry.Buffer;
   Entry.Packets[0].Data:=Entry.Buffer + Entry.Offset;
   Entry.Packets[0].Length:=Entry.Size - Entry.Offset;

   {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET:  Entry.Size = ' + IntToStr(Entry.Size));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET:  Entry.Offset = ' + IntToStr(Entry.Offset));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET:  Entry.Count = ' + IntToStr(Entry.Count));
   if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET:  Entry.Packets[0].Length = ' + IntToStr(Entry.Packets[0].Length));
   {$ENDIF}

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function GENETBufferRelease(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferRelease API for GENET Network}
{Note: Not intended to be called directly by applications, use NetworkBufferRelease instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Buffer Release');
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

function GENETBufferReceive(Network:PNetworkDevice;var Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferReceive API for GENET Network}
{Note: Not intended to be called directly by applications, use NetworkBufferReceive instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Entry}
 Entry:=nil;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Buffer Receive');
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
      Network.ReceiveQueue.Start:=(Network.ReceiveQueue.Start + 1) mod GENET_MAX_RX_ENTRIES;

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

function GENETBufferTransmit(Network:PNetworkDevice;Entry:PNetworkEntry):LongWord;
{Implementation of NetworkBufferTransmit API for GENET Network}
{Note: Not intended to be called directly by applications, use NetworkBufferTransmit instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(Network,'GENET: Buffer Transmit');
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
      {Add Entry}
      Network.TransmitQueue.Entries[(Network.TransmitQueue.Start + Network.TransmitQueue.Count) mod GENET_MAX_TX_ENTRIES]:=Entry;

      {Update Count}
      Inc(Network.TransmitQueue.Count);

      {Start Transmit}
      GENETTransmitStart(PGENETNetwork(Network));

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

procedure GENETTransmitStart(Network:PGENETNetwork);
{Transmit start function for the GENET Network device}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the network lock}
var
 Index:LongWord;
 Size:LongWord;
 LengthStatus:LongWord;
 Ring:PGENETTXRing;
 Entry:PNetworkEntry;
 Block:PGENETControlBlock;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Transmit Start');
 {$ENDIF}

 {Check Count}
 if Network.Network.TransmitQueue.Count = 0 then Exit;

 {Get Entry}
 Entry:=Network.Network.TransmitQueue.Entries[Network.Network.TransmitQueue.Start];
 if Entry = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Packet Length = ' + IntToStr(Entry.Packets[0].Length));
 {$ENDIF}

 {Priority mapping:
  Priority = 0, (Unspecified) Packet goes through Ring 16
  Priority = 1, Packet goes to Ring 0 (Highest priority)
  Priority = 2, Packet goes to Ring 1
  Priority = 3, Packet goes to Ring 2
  Priority = 4, Packet goes to Ring 3}
 Index:=0; //To Do //Include priority field in TNetworkPacket
 if Index = 0 then
  begin
   Index:=GENET_DESC_INDEX;
  end
 else
  begin
   Dec(Index);
  end;

 {Get Ring}
 Ring:=@Network.TXRings[Index];

 {Check Free}
 if Ring.Free < 1 then Exit;

 {Get Control Block}
 Block:=GENETGetTXControlBlock(Network,Ring);
 if Block = nil then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Invalid TX control block (Write=' + IntToStr(Ring.Write) + ' Free=' + IntToStr(Ring.Free) + ')');
   Exit;
  end;

 {Set Block Entry}
 Block.Entry:=Entry;

 {Get Size}
 Size:=Entry.Packets[0].Length + Entry.Offset;

 {Clean Cache}
 if not(DMA_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(Entry.Buffer),Entry.Size);
  end;

 {Get Length Status}
 LengthStatus:=(Size shl DMA_BUFLENGTH_SHIFT) or (Network.QTAGMask shl DMA_TX_QTAG_SHIFT) or DMA_TX_APPEND_CRC or DMA_SOP or DMA_EOP;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set DMA Descriptor}
 GENETDMADescriptorSet(Network,Block.DescriptorAddress,PtrUInt(Entry.Buffer),LengthStatus);

 {Decrement free buffer count and advance our write pointer}
 Dec(Ring.Free);
 Inc(Ring.Producer);
 Ring.Producer:=Ring.Producer and DMA_P_INDEX_MASK;

 {Packets are ready, update producer index}
 GENETTDMARingWrite(Network,Ring.Index,TDMA_PROD_INDEX,Ring.Producer);

 {Update Start}
 Network.Network.TransmitQueue.Start:=(Network.Network.TransmitQueue.Start + 1) mod GENET_MAX_TX_ENTRIES;

 {Update Count}
 Dec(Network.Network.TransmitQueue.Count);

 {Signal Queue Free}
 SemaphoreSignal(Network.Network.TransmitQueue.Wait);

 {Update Statistics}
 Inc(Network.Network.TransmitCount);
 Inc(Network.Network.TransmitBytes,Entry.Packets[0].Length);

 //bcmgenet_xmit
end;

{==============================================================================}

procedure GENETReceiveWorker(Ring:PGENETRXRing);
{Called (by a Worker thread) to process a received packet}
{Ring: The ring buffer which received the packet}
var
 Len:LongWord;
 Mask:LongWord;
 Start:LongWord;
 Flags:LongWord;
 Discards:LongWord;
 Producer:LongWord;
 PacketsProcessed:LongWord;
 PacketsToProcess:LongWord;
 LengthStatus:LongWord;
 Message:TMessage;
 Network:PGENETNetwork;
 Entry:PNetworkEntry;
 Block:PGENETControlBlock;
begin
 {}
 {Check Ring}
 if Ring = nil then Exit;

 {Get Network}
 Network:=Ring.Network;
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Receive Worker (Index=' + IntToStr(Ring.Index) + ')');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
     begin
      {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Close pending, abandoning ring receive');
      {$ENDIF}
     end
    else
     begin
      {Memory Barrier}
      DataMemoryBarrier; {Before the First Write}

      {Clear status before servicing to reduce spurious interrupts}
      if Ring.Index = GENET_DESC_INDEX then
       begin
        GENETIntL20Write(Network,INTRL2_CPU_CLEAR,UMAC_IRQ0_RXDMA_DONE);
       end
      else
       begin
        Mask:=(1 shl (UMAC_IRQ1_RX_INTR_SHIFT + Ring.Index));
        GENETIntL21Write(Network,INTRL2_CPU_CLEAR,Mask);
       end;

      {Acquire Spin Lock}
      if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
       begin
        {Clear Worker}
        Ring.Worker:=INVALID_HANDLE_VALUE;

        {Release Spin Lock}
        SpinUnlockIRQ(Network.Lock);
       end;

      {Get Producer}
      Producer:=GENETRDMARingRead(Network,Ring.Index,RDMA_PROD_INDEX);

      {Update Discards}
      Discards:=(Producer shr DMA_P_INDEX_DISCARD_CNT_SHIFT) and DMA_P_INDEX_DISCARD_CNT_MASK;
      if Discards > Ring.OldDiscards then
       begin
        Discards:=Discards - Ring.OldDiscards;
        Inc(Ring.OldDiscards,Discards);

        {Update Statistics}
        Inc(Network.Network.ReceiveErrors,Discards);

        {Clear HW register when we reach 75% of maximum 0xFFFF}
        if Ring.OldDiscards >= $C000 then
         begin
          Ring.OldDiscards:=0;
          GENETRDMARingWrite(Network,Ring.Index,RDMA_PROD_INDEX,0);
         end;
       end;

      {Get Producers and Process Count}
      Producer:=Producer and DMA_P_INDEX_MASK;
      PacketsProcessed:=0;
      PacketsToProcess:=(Producer - Ring.Consumer) and DMA_C_INDEX_MASK;

      {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Interrupts=' + IntToStr(Network.InterruptCount));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Discards=' + IntToStr(Discards));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Producer Index=' + IntToStr(Producer));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Consumer Index=' + IntToStr(Ring.Consumer));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Read Pointer=' + IntToStr(Ring.Read));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Packets To Process=' + IntToStr(PacketsToProcess));
      {$ENDIF}

      {Process Packets}
      while PacketsProcessed < PacketsToProcess do
       begin
        Block:=@Network.RXControlBlocks[Ring.Read];

        Entry:=GENETRefillRXControlBlock(Network,Block);
        if Entry <> nil then
         begin
          {Check Status 64}
          if not Network.Status64Enable then
           begin
            LengthStatus:=GENETDMADescriptorGetLengthStatus(Network,Block.DescriptorAddress);
           end
          else
           begin
            LengthStatus:=PGENETStatus64(Entry.Buffer).LengthStatus;
           end;

          {DMA flags and length are still valid no matter how we got the Receive Status Vector (64B RSB or register)}
          Flags:=LengthStatus and $FFFF;
          Len:=LengthStatus shr DMA_BUFLENGTH_SHIFT;

          {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMA Flags=' + IntToHex(Flags,4));
          if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Length=' + IntToStr(Len));
          {$ENDIF}

          {Check for start and end of packet}
          if ((Flags and DMA_EOP) = 0) or ((Flags and DMA_SOP) = 0) then
           begin
            if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Dropping fragmented packet');

            {Free Entry}
            BufferFree(Entry);

            {Update Statistics}
            Inc(Network.Network.ReceiveErrors);
           end
          else
           begin
            {Check for error flags}
            if (Flags and (DMA_RX_CRC_ERROR or DMA_RX_OV or DMA_RX_NO or DMA_RX_LG or DMA_RX_RXER)) <> 0 then
             begin
              if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Receive error, packet discarded');

              {Free Entry}
              BufferFree(Entry);

              {Update Statistics}
              Inc(Network.Network.ReceiveErrors);
             end
            else
             begin
              {Check Receive Queue Count}
              if Network.Network.ReceiveQueue.Count < GENET_MAX_RX_ENTRIES then
               begin
                {Update Entry}
                Entry.Size:=Len;
                Entry.Count:=1;

                Start:=0;

                {Remove 64 status header}
                if Network.Status64Enable then
                 begin
                  Dec(Len,SizeOf(TGENETStatus64));
                  Inc(Start,SizeOf(TGENETStatus64));
                 end;

                {Remove hardware 2 bytes added for IP alignment}
                Dec(Len,2);
                Inc(Start,2);

                {Remove CRC}
                if Network.CRCForwardEnable then
                 begin
                  Dec(Len,ETHERNET_CRC_SIZE);
                 end;

                {Finish setting up the received packet and send it to the queue}
                Entry.Offset:=Start;

                {Update Packet}
                Entry.Packets[Entry.Count - 1].Buffer:=Entry.Buffer;
                Entry.Packets[Entry.Count - 1].Data:=Entry.Buffer + Entry.Offset;
                Entry.Packets[Entry.Count - 1].Length:=Len;

                {Add Entry}
                Network.Network.ReceiveQueue.Entries[(Network.Network.ReceiveQueue.Start + Network.Network.ReceiveQueue.Count) mod GENET_MAX_RX_ENTRIES]:=Entry;

                {Update Count}
                Inc(Network.Network.ReceiveQueue.Count);

                {Signal Packet Received}
                SemaphoreSignal(Network.Network.ReceiveQueue.Wait);

                {Update Statistics}
                Inc(Network.Network.ReceiveCount);
                Inc(Network.Network.ReceiveBytes,Len);
               end
              else
               begin
                if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Receive queue overrun, packet discarded');

                {Free Entry}
                BufferFree(Entry);

                {Update Statistics}
                Inc(Network.Network.ReceiveErrors);
                Inc(Network.Network.BufferOverruns);
               end;
             end;
           end;
         end
        else
         begin
          if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: No receive buffer available, packet discarded');

          {Update Statistics}
          Inc(Network.Network.ReceiveErrors);
          {Inc(Network.Network.BufferOverruns);} {Set by GENETRefillRXControlBlock}
         end;

        Inc(PacketsProcessed);

        {Update Ring}
        if Ring.Read < Ring.Last then
         begin
          Inc(Ring.Read);
         end
        else
         begin
          Ring.Read:=Ring.First;
         end;

        {Update Consumer}
        Ring.Consumer:=(Ring.Consumer + 1) and DMA_C_INDEX_MASK;
        GENETRDMARingWrite(Network,Ring.Index,RDMA_CONS_INDEX,Ring.Consumer);
       end;

      {Enable Interrupts}
      Ring.IntEnable(Network,Ring);

      {Memory Barrier}
      DataMemoryBarrier; {After the Last Read}
     end;

    {Acquire Spin Lock}
    if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
     begin
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
            {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end;

      {Release Spin Lock}
      SpinUnlockIRQ(Network.Lock);
     end;
   finally
    {Release the Lock}
    MutexUnlock(Network.Network.Lock);
   end;
  end
 else
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Failed to acquire lock');
  end;

 //bcmgenet_rx_poll / bcmgenet_desc_rx
end;

{==============================================================================}

procedure GENETTransmitWorker(Ring:PGENETTXRing);
{Called (by a Worker thread) to process a transmitted packet}
{Ring: The ring buffer which transmitted the packet}
var
 Message:TMessage;
 Network:PGENETNetwork;
begin
 {}
 {Check Ring}
 if Ring = nil then Exit;

 {Get Network}
 Network:=Ring.Network;
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Transmit Worker (Index=' + IntToStr(Ring.Index) + ')');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
     begin
      {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Close pending, abandoning ring transmit');
      {$ENDIF}
     end
    else
     begin
      {Reclaim TX Control Blocks}
      GENETReclaimTXControlBlock(Network,Ring,True);
     end;

    {Acquire Spin Lock}
    if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
     begin
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
            {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;

        {Release Spin Lock}
        SpinUnlockIRQ(Network.Lock);
       end
      else
       begin
        {Release Spin Lock}
        SpinUnlockIRQ(Network.Lock);

        {Check Count}
        if Network.Network.TransmitQueue.Count > 0 then
         begin
          {Start Transmit}
          GENETTransmitStart(Network);
         end;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Network.Network.Lock);
   end;
  end
 else
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Failed to acquire lock');
  end;

 //bcmgenet_tx_poll / bcmgenet_tx_reclaim / __bcmgenet_tx_reclaim
end;

{==============================================================================}

procedure GENETInterruptWorker(Network:PGENETNetwork);
{Called (by a Worker thread) to process a non RX or TX interrupt}
{Network: The network device which received the interrupt}
var
 Status:LongWord;
 Message:TMessage;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Interrupt Worker');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    if Network.Network.NetworkState = NETWORK_STATE_CLOSING then
     begin
      {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Close pending, abandoning interrupt worker');
      {$ENDIF}
     end
    else
     begin
      {Acquire Spin Lock}
      if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
       begin
        {Get and Clear Status}
        Status:=Network.IRQStatus;
        Network.IRQStatus:=0;

        {Release Spin Lock}
        SpinUnlockIRQ(Network.Lock);

        if (Status and UMAC_IRQ0_LINK_EVENT) <> 0 then
         begin
          {Get PHY Status}
          if GENETPHYReadStatus(Network) = ERROR_SUCCESS then
           begin
            GENETMIISetup(Network);

            {Check PHY Status}
            if Network.Link = PHY_LINK_UP then
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
           end;
         end;
       end;
     end;

    {Acquire Spin Lock}
    if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
     begin
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
            {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Close pending, sending message to waiter thread (Thread=' + IntToHex(Network.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Network.WaiterThread,Message);
            Network.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end;

      {Release Spin Lock}
      SpinUnlockIRQ(Network.Lock);
     end;
   finally
    {Release the Lock}
    MutexUnlock(Network.Network.Lock);
   end;
  end
 else
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Failed to acquire lock');
  end;

 //bcmgenet_irq_work / bcmgenet_irq_task
end;

{==============================================================================}

function GENETInterruptHandler0(Number,CPUID,Flags:LongWord;Network:PGENETNetwork):LongWord;
{Interrupt handler 0 (RX/TX default) for the GENET Network device}
{Note: Not intended to be called directly by applications}
var
 Status:LongWord;
 RXRing:PGENETRXRing;
 TXRing:PGENETTXRing;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;

 {Check Network}
 if Network = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Read IRQ status}
 Status:=GENETIntL20Read(Network,INTRL2_CPU_STAT) and not(GENETIntL20Read(Network,INTRL2_CPU_MASK_STATUS));
 if Status = 0 then Exit;

 {Update Statistics}
 Inc(Network.InterruptCount);

 {Clear Interrupts}
 GENETIntL20Write(Network,INTRL2_CPU_CLEAR,Status);

 {Check RX interrupt}
 if (Status and UMAC_IRQ0_RXDMA_DONE) <> 0 then
  begin
   RXRing:=@Network.RXRings[GENET_DESC_INDEX];
   RXRing.IntDisable(Network,RXRing);

   {Acquire Spin Lock}
   if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
    begin
     {Update Pending}
     Inc(Network.PendingCount);

     {Schedule Worker}
     if RXRing.Worker = INVALID_HANDLE_VALUE then
      begin
       RXRing.Worker:=WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(GENETReceiveWorker),RXRing,nil);
      end;

     {Release Spin Lock}
     SpinUnlockIRQ(Network.Lock);
    end;
  end;

 {Check TX interrupt}
 if (Status and UMAC_IRQ0_TXDMA_DONE) <> 0 then
  begin
   TXRing:=@Network.TXRings[GENET_DESC_INDEX];
   TXRing.IntDisable(Network,TXRing);

   {Acquire Spin Lock}
   if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
    begin
     {Update Pending}
     Inc(Network.PendingCount);

     {Schedule Worker}
     if TXRing.Worker = INVALID_HANDLE_VALUE then
      begin
       TXRing.Worker:=WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(GENETTransmitWorker),TXRing,nil);
      end;

     {Release Spin Lock}
     SpinUnlockIRQ(Network.Lock);
    end;
  end;

 {Check MDIO interrupt}
 if (Network.Flags and GENET_HAS_MDIO_INTR) <> 0 then
  begin
   if (Status and (UMAC_IRQ0_MDIO_DONE or UMAC_IRQ0_MDIO_ERROR)) <> 0 then
    begin
     {Nothing}
    end;
  end;

 {Check other interrupts}
 Status:=Status and (UMAC_IRQ0_LINK_EVENT or UMAC_IRQ0_PHY_DET_R);
 if Status <> 0 then
  begin
   {Acquire Spin Lock}
   if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
    begin
     {Update Pending}
     Inc(Network.PendingCount);

     {Update IRQ Status}
     Network.IRQStatus:=Network.IRQStatus or Status;

     {Schedule Worker}
     WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(GENETInterruptWorker),Network,nil);

     {Release Spin Lock}
     SpinUnlockIRQ(Network.Lock);
    end;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=INTERRUPT_RETURN_HANDLED;

 //bcmgenet_isr0
end;

{==============================================================================}

function GENETInterruptHandler1(Number,CPUID,Flags:LongWord;Network:PGENETNetwork):LongWord;
{Interrupt handler 1 (RX/TX priority) for the GENET Network device}
{Note: Not intended to be called directly by applications}
var
 Index:LongWord;
 Status:LongWord;
 RXRing:PGENETRXRing;
 TXRing:PGENETTXRing;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;

 {Check Network}
 if Network = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Read IRQ status}
 Status:=GENETIntL21Read(Network,INTRL2_CPU_STAT) and not(GENETIntL21Read(Network,INTRL2_CPU_MASK_STATUS));
 if Status = 0 then Exit;

 {Update Statistics}
 Inc(Network.InterruptCount);

 {Clear Interrupts}
 GENETIntL21Write(Network,INTRL2_CPU_CLEAR,Status);

 {Check RX priority queue interrupts}
 if Network.RXQueues > 0 then
  begin
   for Index:=0 to Network.RXQueues - 1 do
    begin
     if (Status and (1 shl (UMAC_IRQ1_RX_INTR_SHIFT + Index))) <> 0 then
      begin
       RXRing:=@Network.RXRings[Index];
       RXRing.IntDisable(Network,RXRing);

       {Acquire Spin Lock}
       if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
        begin
         {Update Pending}
         Inc(Network.PendingCount);

         {Schedule Worker}
         if RXRing.Worker = INVALID_HANDLE_VALUE then
          begin
           RXRing.Worker:=WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(GENETReceiveWorker),RXRing,nil);
          end;

         {Release Spin Lock}
         SpinUnlockIRQ(Network.Lock);
        end;
      end;
    end;
  end;

 {Check TX priority queue interrupts}
 if Network.TXQueues > 0 then
  begin
   for Index:=0 to Network.TXQueues - 1 do
    begin
     if (Status and (1 shl Index)) <> 0 then
      begin
       TXRing:=@Network.TXRings[Index];
       TXRing.IntDisable(Network,TXRing);

       {Acquire Spin Lock}
       if SpinLockIRQ(Network.Lock) = ERROR_SUCCESS then
        begin
         {Update Pending}
         Inc(Network.PendingCount);

         {Schedule Worker}
         if TXRing.Worker = INVALID_HANDLE_VALUE then
          begin
           TXRing.Worker:=WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(GENETTransmitWorker),TXRing,nil);
          end;

         {Release Spin Lock}
         SpinUnlockIRQ(Network.Lock);
        end;
      end;
    end;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=INTERRUPT_RETURN_HANDLED;

 //bcmgenet_isr1
end;

{==============================================================================}
{==============================================================================}
{GENET Helper Functions}
function GENETGetHardwareParameters(Network:PGENETNetwork):LongWord;
{Note: Caller must hold the network lock}
var
 Reg:LongWord;
 Major:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {Read GENET HW version}
 Reg:=GENETSysRead(Network,SYS_REV_CTRL);
 Major:=(Reg shr 24) and $0F;
 if Major = 6 then
  begin
   Major:=5;
  end
 else if Major = 5 then
  begin
   Major:=4;
  end
 else if Major = 0 then
  begin
   Major:=1;
  end;
 Network.Version:=Major;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Hardware major version: ' + IntToStr(Major));
 {$ENDIF}

 if (Network.Version < GENET_V1) or (Network.Version > GENET_V5) then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Hardware version not supported');

   Result:=ERROR_NOT_COMPATIBLE;
   Exit;
  end;

 {Get PHY Revision}
 Network.PhyRevision:=Reg and $ffff;
 if Network.Version = GENET_V5 then
  begin
   {The EPHY revision should come from the MDIO registers of the PHY not from GENET}
   if Network.PhyRevision <> 0 then
    begin
     if NETWORK_LOG_ENABLED then NetworkLogWarn(@Network.Network,'GENET: Hardware is reporting EPHY revision: ' + IntToHex(Network.PhyRevision,4));
    end;
  end
 else if (Network.PhyRevision = 0) or (Network.PhyRevision = $01ff) then
  begin
   {This is reserved so should require special treatment}
   if NETWORK_LOG_ENABLED then NetworkLogWarn(@Network.Network,'GENET: Invalid GPHY revision detected: ' + IntToHex(Network.PhyRevision,4));
  end
 else if (Network.PhyRevision and $f0) <> 0 then
  begin
   {This is the old scheme, just GPHY major, no minor nor patch}
   Network.PhyRevision:=Network.PhyRevision shl 8;
  end
 else if (Network.PhyRevision and $ff00) <> 0 then
  begin
   {This is the new scheme, GPHY major rolls over with $10 = rev G0}
  end;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Hardware PHY revision: ' + IntToHex(Network.PhyRevision,4));
 {$ENDIF}

 {Hardware Parameters}
 case Network.Version of
  GENET_V1:begin
    Network.TXQueues:=0;
    Network.TXDescriptorsPerQueue:=0;
    Network.RXQueues:=0;
    Network.RXDescriptorsPerQueue:=0;
    Network.BackPressureEnableShift:=16;
    Network.BackPressureMask:=$ffff;
    Network.HFBFilterCount:=16;
    Network.QTAGMask:=$1F;
    Network.HFBOffset:=$1000;
    Network.RXDMAOffset:=$2000;
    Network.TXDMAOffset:=$3000;
    Network.WordsPerDescriptor:=2;

    {DMA Parameters}
    Network.DMARXCheckBit:=DMA_RX_CHK_V12;
   end;
  GENET_V2:begin
    Network.TXQueues:=4;
    Network.TXDescriptorsPerQueue:=32;
    Network.RXQueues:=0;
    Network.RXDescriptorsPerQueue:=0;
    Network.BackPressureEnableShift:=16;
    Network.BackPressureMask:=$ffff;
    Network.HFBFilterCount:=16;
    Network.QTAGMask:=$1F;
    Network.TXBufferOffset:=$0600;
    Network.HFBOffset:=$1000;
    Network.HFBRegOffset:=$2000;
    Network.RXDMAOffset:=$3000;
    Network.TXDMAOffset:=$4000;
    Network.WordsPerDescriptor:=2;
    Network.Flags:=GENET_HAS_EXT;

    {DMA Parameters}
    Network.DMARXCheckBit:=DMA_RX_CHK_V12;
   end;
  GENET_V3:begin
    Network.TXQueues:=4;
    Network.TXDescriptorsPerQueue:=32;
    Network.RXQueues:=0;
    Network.RXDescriptorsPerQueue:=0;
    Network.BackPressureEnableShift:=17;
    Network.BackPressureMask:=$1ffff;
    Network.HFBFilterCount:=48;
    Network.HFBFilterSize:=128;
    Network.QTAGMask:=$3F;
    Network.TXBufferOffset:=$0600;
    Network.HFBOffset:=$8000;
    Network.HFBRegOffset:=$fc00;
    Network.RXDMAOffset:=$10000;
    Network.TXDMAOffset:=$11000;
    Network.WordsPerDescriptor:=2;
    Network.Flags:=GENET_HAS_EXT or GENET_HAS_MDIO_INTR or GENET_HAS_MOCA_LINK_DET;

    {DMA Parameters}
    Network.DMARXCheckBit:=DMA_RX_CHK_V3PLUS;
   end;
  GENET_V4:begin
    Network.TXQueues:=4;
    Network.TXDescriptorsPerQueue:=32;
    Network.RXQueues:=0;
    Network.RXDescriptorsPerQueue:=0;
    Network.BackPressureEnableShift:=17;
    Network.BackPressureMask:=$1ffff;
    Network.HFBFilterCount:=48;
    Network.HFBFilterSize:=128;
    Network.QTAGMask:=$3F;
    Network.TXBufferOffset:=$0600;
    Network.HFBOffset:=$8000;
    Network.HFBRegOffset:=$fc00;
    Network.RXDMAOffset:=$2000;
    Network.TXDMAOffset:=$4000;
    Network.WordsPerDescriptor:=3;
    Network.Flags:=GENET_HAS_40BITS or GENET_HAS_EXT or GENET_HAS_MDIO_INTR or GENET_HAS_MOCA_LINK_DET;

    {DMA Parameters}
    Network.DMARXCheckBit:=DMA_RX_CHK_V3PLUS;
   end;
  GENET_V5:begin
    Network.TXQueues:=4;
    Network.TXDescriptorsPerQueue:=32;
    Network.RXQueues:=0;
    Network.RXDescriptorsPerQueue:=0;
    Network.BackPressureEnableShift:=17;
    Network.BackPressureMask:=$1ffff;
    Network.HFBFilterCount:=48;
    Network.HFBFilterSize:=128;
    Network.QTAGMask:=$3F;
    Network.TXBufferOffset:=$0600;
    Network.HFBOffset:=$8000;
    Network.HFBRegOffset:=$fc00;
    Network.RXDMAOffset:=$2000;
    Network.TXDMAOffset:=$4000;
    Network.WordsPerDescriptor:=3;
    Network.Flags:=GENET_HAS_40BITS or GENET_HAS_EXT or GENET_HAS_MDIO_INTR or GENET_HAS_MOCA_LINK_DET;

    {DMA Parameters}
    Network.DMARXCheckBit:=DMA_RX_CHK_V3PLUS;
   end;
  else
   begin
    Result:=ERROR_NOT_FOUND;
    Exit;
   end;
 end;

 {Hardware Parameters}
 Network.Q16TXDescriptorCount:=GENET_TOTAL_DESC - (Network.TXQueues * Network.TXDescriptorsPerQueue);
 Network.Q16RXDescriptorCount:=GENET_TOTAL_DESC - (Network.RXQueues * Network.RXDescriptorsPerQueue);

 {DMA Parameters}
 Network.DMADescriptorSize:=Network.WordsPerDescriptor * SizeOf(LongWord);
 Network.RXDMARegOffset:=Network.RXDMAOffset + (GENET_TOTAL_DESC * Network.DMADescriptorSize);
 Network.TXDMARegOffset:=Network.TXDMAOffset + (GENET_TOTAL_DESC * Network.DMADescriptorSize);

 {$IFDEF CPUAARCH64}
 if (Network.Flags and GENET_HAS_40BITS) = 0 then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogWarn(@Network.Network,'GENET: Hardware does not support 40-bits PA');
  end;
 {$ENDIF CPUAARCH64}

 {DMA Registers}
 case Network.Version of
  GENET_V1:begin
    Network.DMARegisters[DMA_CTRL]:=$00;
    Network.DMARegisters[DMA_STATUS]:=$04;
    Network.DMARegisters[DMA_SCB_BURST_SIZE]:=$0C;
    Network.DMARegisters[DMA_ARB_CTRL]:=$30;
    Network.DMARegisters[DMA_PRIORITY_0]:=$34;
    Network.DMARegisters[DMA_PRIORITY_1]:=$38;
    Network.DMARegisters[DMA_PRIORITY_2]:=$3C;
    Network.DMARegisters[DMA_RING0_TIMEOUT]:=$2C;
    Network.DMARegisters[DMA_RING1_TIMEOUT]:=$30;
    Network.DMARegisters[DMA_RING2_TIMEOUT]:=$34;
    Network.DMARegisters[DMA_RING3_TIMEOUT]:=$38;
    Network.DMARegisters[DMA_RING4_TIMEOUT]:=$3c;
    Network.DMARegisters[DMA_RING5_TIMEOUT]:=$40;
    Network.DMARegisters[DMA_RING6_TIMEOUT]:=$44;
    Network.DMARegisters[DMA_RING7_TIMEOUT]:=$48;
    Network.DMARegisters[DMA_RING8_TIMEOUT]:=$4c;
    Network.DMARegisters[DMA_RING9_TIMEOUT]:=$50;
    Network.DMARegisters[DMA_RING10_TIMEOUT]:=$54;
    Network.DMARegisters[DMA_RING11_TIMEOUT]:=$58;
    Network.DMARegisters[DMA_RING12_TIMEOUT]:=$5c;
    Network.DMARegisters[DMA_RING13_TIMEOUT]:=$60;
    Network.DMARegisters[DMA_RING14_TIMEOUT]:=$64;
    Network.DMARegisters[DMA_RING15_TIMEOUT]:=$68;
    Network.DMARegisters[DMA_RING16_TIMEOUT]:=$6C;
   end;
  GENET_V2:begin
    Network.DMARegisters[DMA_RING_CFG]:=$00;
    Network.DMARegisters[DMA_CTRL]:=$04;
    Network.DMARegisters[DMA_STATUS]:=$08;
    Network.DMARegisters[DMA_SCB_BURST_SIZE]:=$0C;
    Network.DMARegisters[DMA_ARB_CTRL]:=$30;
    Network.DMARegisters[DMA_PRIORITY_0]:=$34;
    Network.DMARegisters[DMA_PRIORITY_1]:=$38;
    Network.DMARegisters[DMA_PRIORITY_2]:=$3C;
    Network.DMARegisters[DMA_RING0_TIMEOUT]:=$2C;
    Network.DMARegisters[DMA_RING1_TIMEOUT]:=$30;
    Network.DMARegisters[DMA_RING2_TIMEOUT]:=$34;
    Network.DMARegisters[DMA_RING3_TIMEOUT]:=$38;
    Network.DMARegisters[DMA_RING4_TIMEOUT]:=$3c;
    Network.DMARegisters[DMA_RING5_TIMEOUT]:=$40;
    Network.DMARegisters[DMA_RING6_TIMEOUT]:=$44;
    Network.DMARegisters[DMA_RING7_TIMEOUT]:=$48;
    Network.DMARegisters[DMA_RING8_TIMEOUT]:=$4c;
    Network.DMARegisters[DMA_RING9_TIMEOUT]:=$50;
    Network.DMARegisters[DMA_RING10_TIMEOUT]:=$54;
    Network.DMARegisters[DMA_RING11_TIMEOUT]:=$58;
    Network.DMARegisters[DMA_RING12_TIMEOUT]:=$5c;
    Network.DMARegisters[DMA_RING13_TIMEOUT]:=$60;
    Network.DMARegisters[DMA_RING14_TIMEOUT]:=$64;
    Network.DMARegisters[DMA_RING15_TIMEOUT]:=$68;
    Network.DMARegisters[DMA_RING16_TIMEOUT]:=$6C;
   end;
  GENET_V3,GENET_V4,GENET_V5:begin
    Network.DMARegisters[DMA_RING_CFG]:=$00;
    Network.DMARegisters[DMA_CTRL]:=$04;
    Network.DMARegisters[DMA_STATUS]:=$08;
    Network.DMARegisters[DMA_SCB_BURST_SIZE]:=$0C;
    Network.DMARegisters[DMA_ARB_CTRL]:=$2C;
    Network.DMARegisters[DMA_PRIORITY_0]:=$30;
    Network.DMARegisters[DMA_PRIORITY_1]:=$34;
    Network.DMARegisters[DMA_PRIORITY_2]:=$38;
    Network.DMARegisters[DMA_RING0_TIMEOUT]:=$2C;
    Network.DMARegisters[DMA_RING1_TIMEOUT]:=$30;
    Network.DMARegisters[DMA_RING2_TIMEOUT]:=$34;
    Network.DMARegisters[DMA_RING3_TIMEOUT]:=$38;
    Network.DMARegisters[DMA_RING4_TIMEOUT]:=$3c;
    Network.DMARegisters[DMA_RING5_TIMEOUT]:=$40;
    Network.DMARegisters[DMA_RING6_TIMEOUT]:=$44;
    Network.DMARegisters[DMA_RING7_TIMEOUT]:=$48;
    Network.DMARegisters[DMA_RING8_TIMEOUT]:=$4c;
    Network.DMARegisters[DMA_RING9_TIMEOUT]:=$50;
    Network.DMARegisters[DMA_RING10_TIMEOUT]:=$54;
    Network.DMARegisters[DMA_RING11_TIMEOUT]:=$58;
    Network.DMARegisters[DMA_RING12_TIMEOUT]:=$5c;
    Network.DMARegisters[DMA_RING13_TIMEOUT]:=$60;
    Network.DMARegisters[DMA_RING14_TIMEOUT]:=$64;
    Network.DMARegisters[DMA_RING15_TIMEOUT]:=$68;
    Network.DMARegisters[DMA_RING16_TIMEOUT]:=$6C;
    Network.DMARegisters[DMA_INDEX2RING_0]:=$70;
    Network.DMARegisters[DMA_INDEX2RING_1]:=$74;
    Network.DMARegisters[DMA_INDEX2RING_2]:=$78;
    Network.DMARegisters[DMA_INDEX2RING_3]:=$7C;
    Network.DMARegisters[DMA_INDEX2RING_4]:=$80;
    Network.DMARegisters[DMA_INDEX2RING_5]:=$84;
    Network.DMARegisters[DMA_INDEX2RING_6]:=$88;
    Network.DMARegisters[DMA_INDEX2RING_7]:=$8C;
   end;
  else
   begin
    Result:=ERROR_NOT_FOUND;
    Exit;
   end;
 end;

 {DMA Ring Registers}
 case Network.Version of
  GENET_V1,GENET_V2,GENET_V3:begin
    Network.DMARingRegisters[TDMA_READ_PTR]:=$00;
    Network.DMARingRegisters[TDMA_CONS_INDEX]:=$04;
    Network.DMARingRegisters[TDMA_PROD_INDEX]:=$08;
    Network.DMARingRegisters[DMA_RING_BUF_SIZE]:=$0C;
    Network.DMARingRegisters[DMA_START_ADDR]:=$10;
    Network.DMARingRegisters[DMA_END_ADDR]:=$14;
    Network.DMARingRegisters[DMA_MBUF_DONE_THRESH]:=$18;
    Network.DMARingRegisters[TDMA_FLOW_PERIOD]:=$1C;
    Network.DMARingRegisters[TDMA_WRITE_PTR]:=$20;
   end;
  GENET_V4,GENET_V5:begin
    {GENET v4 supports 40-bits pointer addressing for obvious reasons the LO
     and HI word parts are contiguous, but this offsets the other registers}
    Network.DMARingRegisters[TDMA_READ_PTR]:=$00;
    Network.DMARingRegisters[TDMA_READ_PTR_HI]:=$04;
    Network.DMARingRegisters[TDMA_CONS_INDEX]:=$08;
    Network.DMARingRegisters[TDMA_PROD_INDEX]:=$0C;
    Network.DMARingRegisters[DMA_RING_BUF_SIZE]:=$10;
    Network.DMARingRegisters[DMA_START_ADDR]:=$14;
    Network.DMARingRegisters[DMA_START_ADDR_HI]:=$18;
    Network.DMARingRegisters[DMA_END_ADDR]:=$1C;
    Network.DMARingRegisters[DMA_END_ADDR_HI]:=$20;
    Network.DMARingRegisters[DMA_MBUF_DONE_THRESH]:=$24;
    Network.DMARingRegisters[TDMA_FLOW_PERIOD]:=$28;
    Network.DMARingRegisters[TDMA_WRITE_PTR]:=$2C;
    Network.DMARingRegisters[TDMA_WRITE_PTR_HI]:=$30;
   end;
  else
   begin
    Result:=ERROR_NOT_FOUND;
    Exit;
   end;
 end;

 {PHY Parameters}
 Network.PhyAddr:=GENET_PHY_ADDR;
 Network.PhyMode:=MatchStringToPhyInterfaceMode(GENET_PHY_MODE);
 Network.PhyInternal:=(Network.PhyMode = PHY_INTERFACE_MODE_INTERNAL);
 Network.PhyExternal:=(Network.PhyMode <> PHY_INTERFACE_MODE_INTERNAL) and (Network.PhyMode <> PHY_INTERFACE_MODE_MOCA);

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then
  begin
   NetworkLogDebug(@Network.Network,'GENET: TXQueues: ' + IntToStr(Network.TXQueues));
   NetworkLogDebug(@Network.Network,'GENET: TXDescriptorsPerQueue: ' + IntToStr(Network.TXDescriptorsPerQueue));
   NetworkLogDebug(@Network.Network,'GENET: RXQueues: ' + IntToStr(Network.RXQueues));
   NetworkLogDebug(@Network.Network,'GENET: RXDescriptorsPerQueue: ' + IntToStr(Network.RXDescriptorsPerQueue));
   NetworkLogDebug(@Network.Network,'GENET: BackPressureEnableShift: ' + IntToStr(Network.BackPressureEnableShift));
   NetworkLogDebug(@Network.Network,'GENET: BackPressureMask: ' + IntToHex(Network.BackPressureMask,8));
   NetworkLogDebug(@Network.Network,'GENET: HFBFilterCount: ' + IntToStr(Network.HFBFilterCount));
   NetworkLogDebug(@Network.Network,'GENET: HFBFilterSize: ' + IntToStr(Network.HFBFilterSize));
   NetworkLogDebug(@Network.Network,'GENET: QTAGMask: ' + IntToHex(Network.QTAGMask,8));
   NetworkLogDebug(@Network.Network,'GENET: TXBufferOffset: ' + IntToHex(Network.TXBufferOffset,8));
   NetworkLogDebug(@Network.Network,'GENET: HFBOffset: ' + IntToHex(Network.HFBOffset,8));
   NetworkLogDebug(@Network.Network,'GENET: HFBRegOffset: ' + IntToHex(Network.HFBRegOffset,8));
   NetworkLogDebug(@Network.Network,'GENET: RXDMAOffset: ' + IntToHex(Network.RXDMAOffset,8));
   NetworkLogDebug(@Network.Network,'GENET: TXDMAOffset: ' + IntToHex(Network.TXDMAOffset,8));
   NetworkLogDebug(@Network.Network,'GENET: WordsPerDescriptor: ' + IntToStr(Network.WordsPerDescriptor));
   NetworkLogDebug(@Network.Network,'GENET: Flags: ' + IntToHex(Network.Flags,8));

   NetworkLogDebug(@Network.Network,'GENET: Q16TXDescriptorCount: ' + IntToStr(Network.Q16TXDescriptorCount));
   NetworkLogDebug(@Network.Network,'GENET: Q16RXDescriptorCount: ' + IntToStr(Network.Q16RXDescriptorCount));

   NetworkLogDebug(@Network.Network,'GENET: DMADescriptorSize: ' + IntToStr(Network.DMADescriptorSize));
   NetworkLogDebug(@Network.Network,'GENET: RXDMARegOffset: ' + IntToHex(Network.RXDMARegOffset,8));
   NetworkLogDebug(@Network.Network,'GENET: TXDMARegOffset: ' + IntToHex(Network.TXDMARegOffset,8));

   NetworkLogDebug(@Network.Network,'GENET: PhyAddr: ' + IntToStr(Network.PhyAddr));
   NetworkLogDebug(@Network.Network,'GENET: PhyMode: ' + PhyInterfaceModeToString(Network.PhyMode));
   NetworkLogDebug(@Network.Network,'GENET: PhyInternal: ' + BooleanToString(Network.PhyInternal));
   NetworkLogDebug(@Network.Network,'GENET: PhyExternal: ' + BooleanToString(Network.PhyExternal));
  end;
 {$ENDIF}

 Result:=ERROR_SUCCESS;

 //bcmgenet_set_hw_params
end;

{==============================================================================}

function GENETGetMACAddress(Network:PGENETNetwork;Address:PHardwareAddress):LongWord;
{Get the current MAC address from a GENET Network device}
{Address: Pointer to a buffer to return the hardware address}
{Network: The GENET Network device to get from}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the network lock}
var
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {Check Address}
 if Address = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Get MAC Address');
 {$ENDIF}

 {Get MAC Address}
 Value:=GENETUMACRead(Network,UMAC_MAC0);
 Address[0]:=(Value shr 24) and $FF;
 Address[1]:=(Value shr 16) and $FF;
 Address[2]:=(Value shr 8) and $FF;
 Address[3]:=Value and $FF;

 Value:=GENETUMACRead(Network,UMAC_MAC1);
 Address[4]:=(Value shr 8) and $FF;
 Address[5]:=Value and $FF;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Address = ' + HardwareAddressToString(Address^));
 {$ENDIF}

 Result:=ERROR_SUCCESS;

 //bcmgenet_set_hw_addr
end;

{==============================================================================}

function GENETSetMACAddress(Network:PGENETNetwork;Address:PHardwareAddress):LongWord;
{Set the current MAC address for a GENET Network device}
{Address: Pointer to the hardware address to set}
{Network: The GENET Network device to set for}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {Check Address}
 if Address = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Set MAC Address (Address=' + HardwareAddressToString(Address^) + ')');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set MAC Address}
 GENETUMACWrite(Network,UMAC_MAC0,(Address[0] shl 24) or (Address[1] shl 16) or (Address[2] shl 8) or (Address[3]));
 GENETUMACWrite(Network,UMAC_MAC1,(Address[4] shl 8) or (Address[5]));

 Result:=ERROR_SUCCESS;

 //bcmgenet_set_hw_addr
end;

{==============================================================================}

procedure GENETPowerUp(Network:PGENETNetwork;Mode:LongWord);
{Note: Caller must hold the network lock}
var
 Reg:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Power Up (Mode=' + IntToStr(Mode) + ')');
 {$ENDIF}

 {Check Flags}
 if (Network.Flags and GENET_HAS_EXT) = 0 then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 Reg:=GENETExtRead(Network,EXT_EXT_PWR_MGMT);

 case Mode of
  GENET_POWER_PASSIVE:begin
    Reg:=Reg and not(EXT_PWR_DOWN_DLL or EXT_PWR_DOWN_BIAS);
    if Network.Version = GENET_V5 then
     begin
      Reg:=Reg and not(EXT_PWR_DOWN_PHY_EN or EXT_PWR_DOWN_PHY_RD or EXT_PWR_DOWN_PHY_SD or EXT_PWR_DOWN_PHY_RX or EXT_PWR_DOWN_PHY_TX or EXT_IDDQ_GLBL_PWR);
      Reg:=Reg or EXT_PHY_RESET;

      GENETExtWrite(Network,EXT_EXT_PWR_MGMT,Reg);
      MillisecondDelay(1);

      Reg:=Reg and not(EXT_PHY_RESET);
     end
    else
     begin
      Reg:=Reg and not(EXT_PWR_DOWN_PHY);
      Reg:=Reg or EXT_PWR_DN_EN_LD;
     end;

    GENETExtWrite(Network,EXT_EXT_PWR_MGMT,Reg);
   end;
  GENET_POWER_CABLE_SENSE:begin
    {Enable APD}
    if Network.Version <> GENET_V5 then
     begin
      Reg:=Reg or EXT_PWR_DN_EN_LD;
      GENETExtWrite(Network,EXT_EXT_PWR_MGMT,Reg);
     end;
   end;
  GENET_POWER_WOL_MAGIC:begin
    //bcmgenet_wol_power_up_cfg
   end;
 end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcmgenet_power_up
end;

{==============================================================================}

function GENETPowerDown(Network:PGENETNetwork;Mode:LongWord):LongWord;
{Power down the unimac, based on mode}

{Note: Caller must hold the network lock}
var
 Reg:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Power Down (Mode=' + IntToStr(Mode) + ')');
 {$ENDIF}

 {Check Network}
 if Network = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 Result:=ERROR_SUCCESS;

 case Mode of
  GENET_POWER_PASSIVE:begin
    {Power down LED}
    if (Network.Flags and GENET_HAS_EXT) <> 0 then
     begin
      Reg:=GENETExtRead(Network,EXT_EXT_PWR_MGMT);
      if Network.Version = GENET_V5 then
       begin
        Reg:=Reg or (EXT_PWR_DOWN_PHY_EN or EXT_PWR_DOWN_PHY_RD or EXT_PWR_DOWN_PHY_SD or EXT_PWR_DOWN_PHY_RX or EXT_PWR_DOWN_PHY_TX or EXT_IDDQ_GLBL_PWR);
       end
      else
       begin
        Reg:=Reg or EXT_PWR_DOWN_PHY;
       end;

      Reg:=Reg or (EXT_PWR_DOWN_DLL or EXT_PWR_DOWN_BIAS);

      GENETExtWrite(Network,EXT_EXT_PWR_MGMT,Reg);
     end;
   end;
  GENET_POWER_CABLE_SENSE:begin
    {Nothing}
   end;
  GENET_POWER_WOL_MAGIC:begin
    //bcmgenet_wol_power_down_cfg
   end;
 end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcmgenet_power_down
end;

{==============================================================================}

procedure GENETResetUMAC(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Reset UMAC');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {7358a0/7552a0: bad default in RBUF_FLUSH_CTRL.umac_sw_rst}
 GENETRBUFCtrlSet(Network,0);
 MicrosecondDelay(10);

 {Check Skip}
 if GENET_SKIP_UMAC_RESET then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogWarn(@Network.Network,'GENET: Skipping UMAC reset');
   Exit;
  end;

 {Disable MAC while updating its registers}
 GENETUMACWrite(Network,UMAC_CMD,0);

 {Issue soft reset with (rg)mii loopback to ensure a stable rxclk}
 GENETUMACWrite(Network,UMAC_CMD,CMD_SW_RESET or CMD_LCL_LOOP_EN);

 //reset_umac
end;

{==============================================================================}

procedure GENETUMACReset(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
var
 Reg:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: UMAC Reset');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 Reg:=GENETRBUFCtrlGet(Network);
 Reg:=Reg or BIT(1);
 GENETRBUFCtrlSet(Network,Reg);

 MicrosecondDelay(10);

 Reg:=Reg and not(BIT(1));
 GENETRBUFCtrlSet(Network,Reg);

 MicrosecondDelay(10);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcmgenet_umac_reset
end;

{==============================================================================}

procedure GENETInitUMAC(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
var
 Reg:LongWord;
 Int0Enable:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Init UMAC');
 {$ENDIF}

 Int0Enable:=0;

 {Reset UniMAC}
 GENETResetUMAC(Network);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear TX/RX counters}
 GENETUMACWrite(Network,UMAC_MIB_CTRL,MIB_RESET_RX or MIB_RESET_TX or MIB_RESET_RUNT);
 GENETUMACWrite(Network,UMAC_MIB_CTRL,0);

 {Set max frame length}
 GENETUMACWrite(Network,UMAC_MAX_FRAME_LEN,GENET_ETH_MAX_MTU_SIZE);

 {Init rx registers, enable ip header optimization}
 Reg:=GENETRBUFRead(Network,RBUF_CTRL);
 Reg:=Reg or RBUF_ALIGN_2B;
 GENETRBUFWrite(Network,RBUF_CTRL,Reg);
 if (Network.Version <> GENET_V1) and (Network.Version <> GENET_V2) then
  begin
   GENETRBUFWrite(Network,RBUF_TBUF_SIZE_CTRL,1);
  end;

 {Disable interrupts}
 GENETInterruptDisable(Network);

 {Enable MDIO interrupts on GENET v3+}
 if (Network.Flags and GENET_HAS_MDIO_INTR) <> 0 then
  begin
   Int0Enable:=Int0Enable or UMAC_IRQ0_MDIO_DONE or UMAC_IRQ0_MDIO_ERROR;
  end;

 GENETIntL20Write(Network,INTRL2_CPU_MASK_CLEAR,Int0Enable);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //init_umac
end;

{==============================================================================}

function GENETInitializeDMA(Network:PGENETNetwork):LongWord;
{Note: Caller must hold the network lock}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Init DMA');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Initialize common RX ring structures}
 Network.RXControlBlocks:=AllocMem(SizeOf(TGENETControlBlocks));
 if Network.RXControlBlocks = nil then
  begin
   Result:=ERROR_OUTOFMEMORY;
   Exit;
  end;

 for Count:=0 to GENET_TOTAL_DESC - 1 do
  begin
   Network.RXControlBlocks[Count].DescriptorAddress:=PtrUInt(Network.Address) + Network.RXDMAOffset + (Count * Network.DMADescriptorSize);

   {Write DMA address (High 32 bits)}
   PLongWord(Network.RXControlBlocks[Count].DescriptorAddress + DMA_DESC_ADDRESS_HI)^:=0;

   {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: RX Control Descriptor Address (' + IntToStr(Count) + ') = ' + AddrToHex(Network.RXControlBlocks[Count].DescriptorAddress));
   {$ENDIF}
  end;

 {Initialize common TX ring structures}
 Network.TXControlBlocks:=AllocMem(SizeOf(TGENETControlBlocks));
 if Network.TXControlBlocks = nil then
  begin
   FreeMem(Network.RXControlBlocks);

   Result:=ERROR_OUTOFMEMORY;
   Exit;
  end;

 for Count:=0 to GENET_TOTAL_DESC - 1 do
  begin
   Network.TXControlBlocks[Count].DescriptorAddress:=PtrUInt(Network.Address) + Network.TXDMAOffset + (Count * Network.DMADescriptorSize);

   {Write DMA address (High 32 bits)}
   PLongWord(Network.TXControlBlocks[Count].DescriptorAddress + DMA_DESC_ADDRESS_HI)^:=0;

   {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: TX Control Descriptor Address (' + IntToStr(Count) + ') = ' + AddrToHex(Network.TXControlBlocks[Count].DescriptorAddress));
   {$ENDIF}
  end;

 {Init RX DMA}
 GENETRDMAWrite(Network,DMA_SCB_BURST_SIZE,DMA_MAX_BURST_LENGTH);

 {Initialize RX queues}
 Result:=GENETInitRXQueues(Network);
 if Result <> ERROR_SUCCESS then
  begin
   FreeMem(Network.RXControlBlocks);
   FreeMem(Network.TXControlBlocks);

   Exit;
  end;

 {Init TX DMA}
 GENETTDMAWrite(Network,DMA_SCB_BURST_SIZE,DMA_MAX_BURST_LENGTH);

 {Initialize TX queues}
 GENETInitTXQueues(Network);

 Result:=ERROR_SUCCESS;

 //bcmgenet_init_dma
end;

{==============================================================================}

function GENETFinalizeDMA(Network:PGENETNetwork):LongWord;
{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Finalize DMA');
 {$ENDIF}

 {Free RX Buffers}
 GENETFreeRXBuffers(Network);

 //bcmgenet_fini_dma
end;

 {==============================================================================}

function GENETShutdownDMA(Network:PGENETNetwork):LongWord;
{Note: Caller must hold the network lock}
var
 Reg:LongWord;
 Count:LongWord;
 Timeout:LongWord;
 DMAControl:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Shutdown DMA');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable TDMA to stop add more frames in TX DMA}
 Reg:=GENETTDMARead(Network,DMA_CTRL);
 Reg:=Reg and not(DMA_EN);
 GENETTDMAWrite(Network,DMA_CTRL,Reg);

 {Check TDMA status register to confirm TDMA is disabled}
 Timeout:=0;
 while Timeout < DMA_TIMEOUT_VAL do
  begin
   Reg:=GENETTDMARead(Network,DMA_STATUS);
   if (Reg and DMA_DISABLED) <> 0 then Break;

   MicrosecondDelay(1);
   Inc(Timeout);
  end;

 if Timeout = DMA_TIMEOUT_VAL then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogWarn(@Network.Network,'GENET: Timed out while disabling TX DMA');
   Result:=ERROR_TIMEOUT;
  end;

 {Wait 10ms for packet drain in both TX and RX DMA}
 MillisecondDelay(10);

 {Disable RDMA}
 Reg:=GENETRDMARead(Network,DMA_CTRL);
 Reg:=Reg and not(DMA_EN);
 GENETRDMAWrite(Network,DMA_CTRL,Reg);

 {Check RDMA status register to confirm RDMA is disabled}
 Timeout:=0;
 while Timeout < DMA_TIMEOUT_VAL do
  begin
   Reg:=GENETRDMARead(Network,DMA_STATUS);
   if (Reg and DMA_DISABLED) <> 0 then Break;

   MicrosecondDelay(1);
   Inc(Timeout);
  end;

 if Timeout = DMA_TIMEOUT_VAL then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogWarn(@Network.Network,'GENET: Timed out while disabling RX DMA');
   Result:=ERROR_TIMEOUT;
  end;

 DMAControl:=0;
 if Network.RXQueues > 0 then
  begin
   for Count:=0 to Network.RXQueues - 1 do
    begin
     DMAControl:=DMAControl or (1 shl (Count + DMA_RING_BUF_EN_SHIFT));
    end;

   Reg:=GENETRDMARead(Network,DMA_CTRL);
   Reg:=Reg and not(DMAControl);

   GENETRDMAWrite(Network,DMA_CTRL,Reg);
  end;

 DMAControl:=0;
 if Network.TXQueues > 0 then
  begin
   for Count:=0 to Network.TXQueues - 1 do
    begin
     DMAControl:=DMAControl or (1 shl (Count + DMA_RING_BUF_EN_SHIFT));
    end;

   Reg:=GENETTDMARead(Network,DMA_CTRL);
   Reg:=Reg and not(DMAControl);

   GENETTDMAWrite(Network,DMA_CTRL,Reg);
  end;

 if Result = ERROR_TIMEOUT then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //bcmgenet_dma_teardown
end;

{==============================================================================}

function GENETDisableDMA(Network:PGENETNetwork;FlushRX:Boolean):LongWord;
{Returns a reusable DMA control register value}

{Note: Caller must hold the network lock}
var
 Reg:LongWord;
 DMAControl:LongWord;
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Disable DMA');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable DMA}
 DMAControl:=(1 shl (GENET_DESC_INDEX + DMA_RING_BUF_EN_SHIFT)) or DMA_EN;
 Reg:=GENETTDMARead(Network,DMA_CTRL);
 Reg:=Reg and not(DMAControl);
 GENETTDMAWrite(Network,DMA_CTRL,Reg);

 Reg:=GENETRDMARead(Network,DMA_CTRL);
 Reg:=Reg and not(DMAControl);
 GENETRDMAWrite(Network,DMA_CTRL,Reg);

 GENETUMACWrite(Network,UMAC_TX_FLUSH,1);
 MicrosecondDelay(10);
 GENETUMACWrite(Network,UMAC_TX_FLUSH,0);

 if FlushRX then
  begin
   Reg:=GENETRBUFCtrlGet(Network);
   GENETRBUFCtrlSet(Network,Reg or BIT(0));
   MicrosecondDelay(10);
   GENETRBUFCtrlSet(Network,Reg);
   MicrosecondDelay(10);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMA Control=' + IntToHex(DMAControl,8));
 {$ENDIF}

 Result:=DMAControl;

 //bcmgenet_dma_disable
end;

{==============================================================================}

procedure GENETEnableDMA(Network:PGENETNetwork;DMAControl:LongWord);
{Note: Caller must hold the network lock}
var
 Reg:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Enable DMA (DMA Control=' + IntToHex(DMAControl,8) + ')');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Enable DMA}
 Reg:=GENETRDMARead(Network,DMA_CTRL);
 Reg:=Reg or DMAControl;
 GENETRDMAWrite(Network,DMA_CTRL,Reg);

 Reg:=GENETTDMARead(Network,DMA_CTRL);
 Reg:=Reg or DMAControl;
 GENETTDMAWrite(Network,DMA_CTRL,Reg);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcmgenet_enable_dma
end;

{==============================================================================}

function GENETInitRXQueues(Network:PGENETNetwork):LongWord;
{Initialize RX queues

  Queues 0-15 are priority queues. Hardware Filtering Block (HFB) can be
  used to direct traffic to these queues.

  Queue 16 is the default RX queue with Q16RXDescriptorCount descriptors}

{Note: Caller must hold the network lock}
var
 Count:LongWord;
 DMAEnable:Boolean;
 DMAControl:LongWord;
 RingConfig:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Init RX Queues');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable RX DMA}
 DMAControl:=GENETRDMARead(Network,DMA_CTRL);
 DMAEnable:=(DMAControl and DMA_EN) <> 0;
 DMAControl:=DMAControl and not(DMA_EN);
 GENETRDMAWrite(Network,DMA_CTRL,DMAControl);

 DMAControl:=0;
 RingConfig:=0;

 {Initialize RX priority queues}
 if Network.RXQueues > 0 then
  begin
   for Count:=0 to Network.RXQueues - 1 do
    begin
     Result:=GENETInitRXRing(Network,Count,Network.RXDescriptorsPerQueue,Count * Network.RXDescriptorsPerQueue,(Count + 1) * Network.RXDescriptorsPerQueue);
     if Result <> ERROR_SUCCESS then Exit;

     RingConfig:=RingConfig or (1 shl Count);
     DMAControl:=DMAControl or (1 shl (Count + DMA_RING_BUF_EN_SHIFT));
    end;
  end;

 {Initialize RX default queue 16}
 Result:=GENETInitRXRing(Network,GENET_DESC_INDEX,Network.Q16RXDescriptorCount,Network.RXQueues * Network.RXDescriptorsPerQueue,GENET_TOTAL_DESC);
 if Result <> ERROR_SUCCESS then Exit;

 RingConfig:=RingConfig or (1 shl GENET_DESC_INDEX);
 DMAControl:=DMAControl or (1 shl (GENET_DESC_INDEX + DMA_RING_BUF_EN_SHIFT));

 {Enable rings}
 GENETRDMAWrite(Network,DMA_RING_CFG,RingConfig);

 {Configure ring as descriptor ring and re-enable DMA if enabled}
 if DMAEnable then
  begin
   DMAControl:=DMAControl or DMA_EN;
  end;
 GENETRDMAWrite(Network,DMA_CTRL,DMAControl);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: RingConfig=' + IntToHex(RingConfig,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMAControl=' + IntToHex(DMAControl,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMAEnable=' + BoolToStr(DMAEnable,True));
 {$ENDIF}

 Result:=ERROR_SUCCESS;

 //bcmgenet_init_rx_queues
end;

{==============================================================================}

procedure GENETInitTXQueues(Network:PGENETNetwork);
{Initialize TX queues

  Queues 0-3 are priority-based, each one has 32 descriptors,
  with queue 0 being the highest priority queue.

  Queue 16 is the default TX queue with
  Q16TXDescriptorCount = 256 - 4 * 32 = 128 descriptors.

  The transmit control block pool is then partitioned as follows:
   - TX queue 0 uses TXControlBlocks[0..31]
   - TX queue 1 uses TXControlBlocks[32..63]
   - TX queue 2 uses TXControlBlocks[64..95]
   - TX queue 3 uses TXControlBlocks[96..127]
   - TX queue 16 uses TXControlBlocks[128..255]}

{Note: Caller must hold the network lock}

 function DMA_PRIO_REG_INDEX(Queue:LongWord):LongWord;
 begin
  Result:=Queue div 6;
 end;

 function DMA_PRIO_REG_SHIFT(Queue:LongWord):LongWord;
 begin
  Result:=(Queue mod 6) * DMA_RING_BUF_PRIORITY_SHIFT;
 end;

var
 Count:LongWord;
 DMAEnable:Boolean;
 DMAControl:LongWord;
 RingConfig:LongWord;
 DMAPriority:array[0..2] of LongWord = (0,0,0);
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Init TX Queues');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable TX DMA}
 DMAControl:=GENETTDMARead(Network,DMA_CTRL);
 DMAEnable:=(DMAControl and DMA_EN) <> 0;
 DMAControl:=DMAControl and not(DMA_EN);
 GENETTDMAWrite(Network,DMA_CTRL,DMAControl);

 DMAControl:=0;
 RingConfig:=0;

 {Enable strict priority arbiter mode}
 GENETTDMAWrite(Network,DMA_ARB_CTRL,DMA_ARBITER_SP);

 {Initialize TX priority queues}
 if Network.TXQueues > 0 then
  begin
   for Count:=0 to Network.TXQueues - 1 do
    begin
     GENETInitTXRing(Network,Count,Network.TXDescriptorsPerQueue,Count * Network.TXDescriptorsPerQueue,(Count + 1) * Network.TXDescriptorsPerQueue);

     RingConfig:=RingConfig or (1 shl Count);
     DMAControl:=DMAControl or (1 shl (Count + DMA_RING_BUF_EN_SHIFT));
     DMAPriority[DMA_PRIO_REG_INDEX(Count)]:=DMAPriority[DMA_PRIO_REG_INDEX(Count)] or ((GENET_Q0_PRIORITY + Count) shl DMA_PRIO_REG_SHIFT(Count));
    end;
  end;

 {Initialize TX default queue 16}
 GENETInitTXRing(Network,GENET_DESC_INDEX,Network.Q16TXDescriptorCount,Network.TXQueues * Network.TXDescriptorsPerQueue,GENET_TOTAL_DESC);

 RingConfig:=RingConfig or (1 shl GENET_DESC_INDEX);
 DMAControl:=DMAControl or (1 shl (GENET_DESC_INDEX + DMA_RING_BUF_EN_SHIFT));
 DMAPriority[DMA_PRIO_REG_INDEX(GENET_DESC_INDEX)]:=DMAPriority[DMA_PRIO_REG_INDEX(GENET_DESC_INDEX)] or ((GENET_Q0_PRIORITY + Network.TXQueues) shl DMA_PRIO_REG_SHIFT(GENET_DESC_INDEX));

 {Set TX queue priorities}
 GENETTDMAWrite(Network,DMA_PRIORITY_0,DMAPriority[0]);
 GENETTDMAWrite(Network,DMA_PRIORITY_1,DMAPriority[1]);
 GENETTDMAWrite(Network,DMA_PRIORITY_2,DMAPriority[2]);

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMAPriority[0]=' + IntToHex(DMAPriority[0],8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMAPriority[1]=' + IntToHex(DMAPriority[1],8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMAPriority[2]=' + IntToHex(DMAPriority[2],8));
 {$ENDIF}

 {Enable TX queues}
 GENETTDMAWrite(Network,DMA_RING_CFG,RingConfig);

 {Enable TX DMA}
 if DMAEnable then
  begin
   DMAControl:=DMAControl or DMA_EN;
  end;
 GENETTDMAWrite(Network,DMA_CTRL,DMAControl);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: RingConfig=' + IntToHex(RingConfig,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMAControl=' + IntToHex(DMAControl,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMAEnable=' + BoolToStr(DMAEnable,True));
 {$ENDIF}

 //bcmgenet_init_tx_queues
end;

{==============================================================================}

function GENETInitRXRing(Network:PGENETNetwork;Index,Size,First,Last:LongWord):LongWord;
{Initialize an RX DMA ring}

{Note: Caller must hold the network lock}
var
 Status:LongWord;
 Ring:PGENETRXRing;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {Check Index}
 if Index > GENET_DESC_INDEX then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Init RX Ring (Index=' + IntToStr(Index) + ' Size=' + IntToStr(Size) + ' First=' + IntToStr(First) + ' Last=' + IntToStr(Last) + ')');
 {$ENDIF}

 {Initialize Ring}
 Ring:=@Network.RXRings[Index];
 Ring.Network:=Network;
 Ring.Worker:=INVALID_HANDLE_VALUE;
 Ring.Index:=Index;
 Ring.Size:=Size;
 Ring.Consumer:=0;
 Ring.Read:=First;
 Ring.First:=First;
 Ring.Last:=Last - 1;
 Ring.OldDiscards:=0;
 Ring.ControlBlocks:=Network.RXControlBlocks;
 if Index = GENET_DESC_INDEX then
  begin
   Ring.IntEnable:=GENETRXRing16IntEnable;
   Ring.IntDisable:=GENETRXRing16IntDisable;
  end
 else
  begin
   Ring.IntEnable:=GENETRXRingIntEnable;
   Ring.IntDisable:=GENETRXRingIntDisable;
  end;

 {Allocate RX Buffer}
 Result:=GENETAllocRXBuffers(Network,Ring);
 if Result <> ERROR_SUCCESS then Exit;

 GENETRDMARingWrite(Network,Index,RDMA_PROD_INDEX,0);
 GENETRDMARingWrite(Network,Index,RDMA_CONS_INDEX,0);
 GENETRDMARingWrite(Network,Index,DMA_RING_BUF_SIZE,((Size shl DMA_RING_SIZE_SHIFT) or GENET_MAX_PACKET_SIZE));
 GENETRDMARingWrite(Network,Index,RDMA_XON_XOFF_THRESH,(DMA_FC_THRESH_LO shl DMA_XOFF_THRESHOLD_SHIFT) or DMA_FC_THRESH_HI);

 {Set start and end address, read and write pointers}
 GENETRDMARingWrite(Network,Index,DMA_START_ADDR,First * Network.WordsPerDescriptor);
 GENETRDMARingWrite(Network,Index,RDMA_READ_PTR,First * Network.WordsPerDescriptor);
 GENETRDMARingWrite(Network,Index,RDMA_WRITE_PTR,First * Network.WordsPerDescriptor);
 GENETRDMARingWrite(Network,Index,DMA_END_ADDR,Last * Network.WordsPerDescriptor - 1);

 Result:=ERROR_SUCCESS;

 //bcmgenet_init_rx_ring
end;

{==============================================================================}

procedure GENETInitTXRing(Network:PGENETNetwork;Index,Size,First,Last:LongWord);
{Initialize a TX DMA ring along with corresponding hardware registers}

{Note: Caller must hold the network lock}
var
 Ring:PGENETTXRing;
 FlowPeriodValue:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Index}
 if Index > GENET_DESC_INDEX then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Init TX Ring (Index=' + IntToStr(Index) + ' Size=' + IntToStr(Size) + ' First=' + IntToStr(First) + ' Last=' + IntToStr(Last) + ')');
 {$ENDIF}

 {Initialize Ring}
 Ring:=@Network.TXRings[Index];
 Ring.Network:=Network;
 Ring.Worker:=INVALID_HANDLE_VALUE;
 Ring.Index:=Index;
 Ring.Size:=Size;
 Ring.Clean:=First;
 Ring.Consumer:=0;
 Ring.Free:=Size;
 Ring.Write:=First;
 Ring.First:=First;
 Ring.Last:=Last - 1;
 Ring.Producer:=0;
 Ring.ControlBlocks:=Network.TXControlBlocks;
 if Index = GENET_DESC_INDEX then
  begin
   Ring.Queue:=0;
   Ring.IntEnable:=GENETTXRing16IntEnable;
   Ring.IntDisable:=GENETTXRing16IntDisable;
  end
 else
  begin
   Ring.Queue:=Index + 1;
   Ring.IntEnable:=GENETTXRingIntEnable;
   Ring.IntDisable:=GENETTXRingIntDisable;
  end;

 {Set flow period for ring != 16}
 FlowPeriodValue:=0;
 if Index <> GENET_DESC_INDEX then
  begin
   FlowPeriodValue:=GENET_ETH_MAX_MTU_SIZE shl 16;
  end;

 GENETTDMARingWrite(Network,Index,TDMA_PROD_INDEX,0);
 GENETTDMARingWrite(Network,Index,TDMA_CONS_INDEX,0);
 GENETTDMARingWrite(Network,Index,DMA_MBUF_DONE_THRESH,10);
 GENETTDMARingWrite(Network,Index,TDMA_FLOW_PERIOD,FlowPeriodValue); {Disable rate control for now}
 GENETTDMARingWrite(Network,Index,DMA_RING_BUF_SIZE,((Size shl DMA_RING_SIZE_SHIFT) or GENET_MAX_PACKET_SIZE));

 {Set start and end address, read and write pointers}
 GENETTDMARingWrite(Network,Index,DMA_START_ADDR,First * Network.WordsPerDescriptor);
 GENETTDMARingWrite(Network,Index,TDMA_READ_PTR,First * Network.WordsPerDescriptor);
 GENETTDMARingWrite(Network,Index,TDMA_WRITE_PTR,First * Network.WordsPerDescriptor);
 GENETTDMARingWrite(Network,Index,DMA_END_ADDR,Last * Network.WordsPerDescriptor - 1);

 //bcmgenet_init_tx_ring
end;

{==============================================================================}

function GENETAllocRXBuffers(Network:PGENETNetwork;Ring:PGENETRXRing):LongWord;
{Assign buffer to RX DMA descriptor}

{Note: Caller must hold the network lock}
var
 Count:LongWord;
 Entry:PNetworkEntry;
 Block:PGENETControlBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Alloc RX Buffers (Ring=' + IntToStr(Ring.Index) + ')');
 {$ENDIF}

 {Loop here for each buffer needing assignment}
 if Ring.Size > 0 then
  begin
   for Count:=0 to Ring.Size - 1 do
    begin
     Block:=@Ring.ControlBlocks[Ring.First + Count];

     {Fill Control Block}
     Entry:=GENETRefillRXControlBlock(Network,Block);
     if Entry <> nil then BufferFree(Entry);

     if Block.Entry = nil then
      begin
       Result:=ERROR_OUTOFMEMORY;
       Exit;
      end;
    end;
  end;

 Result:=ERROR_SUCCESS;

 // bcmgenet_alloc_rx_buffers
end;

{==============================================================================}

procedure GENETFreeRXBuffers(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
var
 Count:LongWord;
 Entry:PNetworkEntry;
 Block:PGENETControlBlock;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Free RX Buffers');
 {$ENDIF}

 for Count:=0 to GENET_TOTAL_DESC - 1 do
  begin
   Block:=@Network.RXControlBlocks[Count];

   {Free Control Block}
   Entry:=GENETFreeRXControlBlock(Network,Block);
   if Entry <> nil then BufferFree(Entry);
  end;

 // bcmgenet_free_rx_buffers
end;


{==============================================================================}

procedure GENETHFBInit(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: HFB Init');
 {$ENDIF}

 {Check Version}
 if (Network.Version = GENET_V1) or (Network.Version = GENET_V2) then Exit;

 {Clear HFB}
 GENETHFBClear(Network);

 //bcmgenet_hfb_init
end;

{==============================================================================}

procedure GENETHFBClear(Network:PGENETNetwork);
{Clear Hardware Filter Block and disable all filtering}

{Note: Caller must hold the network lock}
var
 Count:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: HFB Clear');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 GENETHFBRegWrite(Network,HFB_CTRL,0);
 GENETHFBRegWrite(Network,HFB_FLT_ENABLE_V3PLUS,0);
 GENETHFBRegWrite(Network,HFB_FLT_ENABLE_V3PLUS + 4,0);

 for Count:=DMA_INDEX2RING_0 to DMA_INDEX2RING_7 do
  begin
   GENETRDMAWrite(Network,Count,0);
  end;

 for Count:=0 to (Network.HFBFilterCount div 4) - 1 do
  begin
   GENETHFBRegWrite(Network,HFB_FLT_LEN_V3PLUS + Count * SizeOf(LongWord),0);
  end;

 if (Network.HFBFilterCount * Network.HFBFilterSize) > 0 then
  begin
   for Count:=0 to (Network.HFBFilterCount * Network.HFBFilterSize) - 1 do
    begin
     GENETHFBWrite(Network,Count * SizeOf(LongWord),0);
    end
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcmgenet_hfb_clear
end;

{==============================================================================}

procedure GENETInterfaceStart(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
var
 Count:LongWord;
 RXRing:PGENETRXRing;
 TXRing:PGENETTXRing;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Interface Start');
 {$ENDIF}

 {Start the network engine}
 GENETSetRXMode(Network);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Enable RX Ring Interrupts}
 if Network.RXQueues > 0 then
  begin
   for Count:=0 to Network.RXQueues - 1 do
    begin
     RXRing:=@Network.RXRings[Count];
     RXRing.IntEnable(Network,RXRing);
    end;
  end;

 RXRing:=@Network.RXRings[GENET_DESC_INDEX];
 RXRing.IntEnable(Network,RXRing);

 GENETUMACEnableSet(Network,CMD_TX_EN or CMD_RX_EN,True);

 {Enable TX Ring Interrupts}
 if Network.TXQueues > 0 then
  begin
   for Count:=0 to Network.TXQueues - 1 do
    begin
     TXRing:=@Network.TXRings[Count];
     TXRing.IntEnable(Network,TXRing);
    end;
  end;

 TXRing:=@Network.TXRings[GENET_DESC_INDEX];
 TXRing.IntEnable(Network,TXRing);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Monitor link interrupts now}
 GENETLinkInterruptEnable(Network);

 //bcmgenet_netif_start
end;

{==============================================================================}

procedure GENETInterfaceStop(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Interface Stop');
 {$ENDIF}

 {Disable MAC receive}
 GENETUMACEnableSet(Network,CMD_RX_EN,False);

 {Shutdown DMA}
 GENETShutdownDMA(Network);

 {Disable MAC transmit (TX DMA disabled must be done before this)}
 GENETUMACEnableSet(Network,CMD_TX_EN,False);

 {Disable Interrupts}
 GENETInterruptDisable(Network);

 Network.OldLink:=-1;
 Network.OldSpeed:=-1;
 Network.OldDuplex:=-1;
 Network.OldPause:=-1;

 {TX Reclaim}
 GENETReclaimTXControlBlocks(Network);

 {Finalize DMA}
 GENETFinalizeDMA(Network);

 //bcmgenet_netif_stop
end;

{==============================================================================}

function GENETMIIProbe(Network:PGENETNetwork):LongWord;
{Note: Caller must hold the network lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: MII Probe');
 {$ENDIF}

 {Communicate the integrated PHY revision}
 if Network.PhyInternal then
  begin
   Network.PhyFlags:=Network.PhyRevision;
  end
 else
  begin
   Network.PhyFlags:=PHY_BRCM_AUTO_PWRDWN_ENABLE;
  end;

 {Initialize link state variables that GENETMIISetup uses}
 Network.OldLink:=-1;
 Network.OldSpeed:=-1;
 Network.OldDuplex:=-1;
 Network.OldPause:=-1;

 {Reset the PHY}
 UniMACMDIOReset(Network);

 {Get PHY Identifier}
 GENETPhyGetId(Network,Network.PhyId);

 {Initialize PHY Hardware}
 GENETPhyInitHardware(Network);

 {Initialize the PHY Status}
 Result:=GENETPhyReadStatus(Network);
 if Result <> ERROR_SUCCESS then Exit;

 GENETMIISetup(Network);

 {Configure port multiplexer based on what the probed PHY device}
 Result:=GENETMIIConfig(Network);
 if Result <> ERROR_SUCCESS then Exit;

 {The internal PHY has its link interrupts routed to the Ethernet MAC ISRs.
  On GENETv5 there is a hardware issue that prevents the signaling of link UP interrupts
  when the link operates at 10Mbps, so fallback to polling for those versions of GENET.}
 if Network.PhyInternal and (Network.Version <> GENET_V5) then
  begin
   GENET_NO_PHY_INTERRUPT:=True;
  end;

 Result:=ERROR_SUCCESS;

 //bcmgenet_mii_probe
end;

{==============================================================================}

procedure GENETMIISetup(Network:PGENETNetwork);
{Setup netdev link state when PHY link status change and
 update UMAC and RGMII block when link up}

 {Note: Caller must hold the network lock}
var
 Reg:LongWord;
 CommandBits:LongWord;
 StatusChanged:Boolean;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: MII Setup');
 {$ENDIF}

 CommandBits:=0;
 StatusChanged:=False;

 {Check Link}
 if Network.OldLink <> Network.Link then
  begin
   StatusChanged:=True;
   Network.OldLink:=Network.Link;
  end;

 if Network.Link = PHY_LINK_UP then
  begin
   {Check Speed}
   if Network.OldSpeed <> Network.Speed then
    begin
     StatusChanged:=True;
     Network.OldSpeed:=Network.Speed;
    end;

   {Check Duplex}
   if Network.OldDuplex <> Network.Duplex then
    begin
     StatusChanged:=True;
     Network.OldDuplex:=Network.Duplex;
    end;

   {Check Pause}
   if Network.OldPause <> Network.Pause then
    begin
     StatusChanged:=True;
     Network.OldPause:=Network.Pause;
    end;

   {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Status Changed=' + BoolToStr(StatusChanged,True));
   {$ENDIF}

   {Done if nothing has changed}
   if not StatusChanged then Exit;

   {Get Speed}
   if Network.Speed = PHY_SPEED_1000 then
    begin
     CommandBits:=UMAC_SPEED_1000;
    end
   else if Network.Speed = PHY_SPEED_100 then
    begin
     CommandBits:=UMAC_SPEED_100;
    end
   else
    begin
     CommandBits:=UMAC_SPEED_10;
    end;
   CommandBits:=CommandBits shl CMD_SPEED_SHIFT;

   {Get Duplex}
   if Network.Duplex <> PHY_DUPLEX_FULL then
    begin
     CommandBits:=CommandBits or CMD_HD_EN;
    end;

   {Get Pause Capability}
   if Network.Pause = 0 then
    begin
     CommandBits:=CommandBits or CMD_RX_PAUSE_IGNORE or CMD_TX_PAUSE_IGNORE;
    end;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Program UMAC and RGMII block based on established link speed, duplex, and pause.
    The speed set in UMAC_CMD tells RGMII block which clock to use for transmit
    25MHz(100Mbps) or 125MHz(1Gbps). Receive clock is provided by the PHY}
   Reg:=GENETExtRead(Network,EXT_RGMII_OOB_CTRL);
   Reg:=Reg and not(OOB_DISABLE);
   Reg:=Reg or RGMII_LINK;
   GENETExtWrite(Network,EXT_RGMII_OOB_CTRL,Reg);

   Reg:=GENETUMACRead(Network,UMAC_CMD);
   Reg:=Reg and not((CMD_SPEED_MASK shl CMD_SPEED_SHIFT) or CMD_HD_EN or CMD_RX_PAUSE_IGNORE or CMD_TX_PAUSE_IGNORE);
   Reg:=Reg or CommandBits;
   GENETUMACWrite(Network,UMAC_CMD,Reg);

   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
  end;

 //bcmgenet_mii_setup
end;

{==============================================================================}

function GENETMIIConfig(Network:PGENETNetwork):LongWord;
{Note: Caller must hold the network lock}
var
 BMCR:Word;
 Reg:LongWord;
 Status:LongWord;
 IdMode:LongWord;
 PortControl:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: MII Config');
 {$ENDIF}

 Smallint(BMCR):=-1;
 IdMode:=0;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {MAC clocking workaround during reset of umac state machines}
 Reg:=GENETUMACRead(Network,UMAC_CMD);
 if (Reg and CMD_SW_RESET) <> 0 then
  begin
   {An MII PHY must be isolated to prevent TXC contention}
   if Network.PhyMode = PHY_INTERFACE_MODE_MII then
    begin
     Status:=UniMACMDIORead(Network,MII_BMCR,BMCR);
     if Status = ERROR_SUCCESS then
      begin
       Status:=UniMACMDIOWrite(Network,MII_BMCR,BMCR or BMCR_ISOLATE);
      end;
     if Status <> ERROR_SUCCESS then
      begin
       if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Failed to isolate PHY');

       Result:=Status;
       Exit;
      end;
    end;

   {Switch MAC clocking to RGMII generated clock}
   GENETSysWrite(Network,SYS_PORT_CTRL,PORT_MODE_EXT_GPHY);

   {Ensure 5 clks with RX disabled followed by 5 clks with Reset asserted}
   MicrosecondDelay(4);

   Reg:=Reg and not(CMD_SW_RESET or CMD_LCL_LOOP_EN);
   GENETUMACWrite(Network,UMAC_CMD,Reg);

   {Ensure 5 more clocks before Rx is enabled}
   MicrosecondDelay(2);
  end;

 case Network.PhyMode of
  PHY_INTERFACE_MODE_INTERNAL,PHY_INTERFACE_MODE_MOCA:begin
    {Irrespective of the actually configured PHY speed (100 or 1000) GENETv4 only has an internal GPHY
     so we will just end up masking the Gigabit features from what we support, not switching to the EPHY}
    if (Network.Version = GENET_V4) then
     begin
      PortControl:=PORT_MODE_INT_GPHY;
     end
    else
     begin
      PortControl:=PORT_MODE_INT_EPHY;
     end;

    GENETSysWrite(Network,SYS_PORT_CTRL,PortControl);

    if Network.PhyMode = PHY_INTERFACE_MODE_MOCA then
     begin
      //bcmgenet_moca_phy_setup
     end;
   end;
  PHY_INTERFACE_MODE_MII:begin
    GENETSysWrite(Network,SYS_PORT_CTRL,PORT_MODE_EXT_EPHY);

    {Restore the MII PHY after isolation}
    if Smallint(BMCR) >= 0 then
     begin
      UniMACMDIOWrite(Network,MII_BMCR,BMCR);
     end;
   end;
  PHY_INTERFACE_MODE_REVMII:begin
    PortControl:=PORT_MODE_EXT_RVMII_50;

    GENETSysWrite(Network,SYS_PORT_CTRL,PortControl);
   end;
  PHY_INTERFACE_MODE_RGMII,PHY_INTERFACE_MODE_RGMII_TXID,PHY_INTERFACE_MODE_RGMII_RXID:begin
    {RGMII_NO_ID: TXC transitions at the same time as TXD (requires PCB or receiver-side delay)
     RGMII: Add 2ns delay on TXC (90 degree shift)

     ID is implicitly disabled for 100Mbps (RG)MII operation}
    if Network.PhyMode = PHY_INTERFACE_MODE_RGMII then
     begin
      IdMode:=BIT(16); {ID_MODE_DIS}
     end;

    GENETSysWrite(Network,SYS_PORT_CTRL,PORT_MODE_EXT_GPHY);
   end;
  else
   begin
    Exit;
   end;
 end;

 {This is an external PHY (xMII), so we need to enable the RGMII block for the interface to work}
 if Network.PhyExternal then
  begin
   Reg:=GENETExtRead(Network,EXT_RGMII_OOB_CTRL);
   Reg:=Reg and not(ID_MODE_DIS);
   Reg:=Reg or IdMode;
   if (Network.Version = GENET_V1) or (Network.Version = GENET_V2) or (Network.Version = GENET_V3) then
    begin
     Reg:=Reg or RGMII_MODE_EN_V123;
    end
   else
    begin
     Reg:=Reg or RGMII_MODE_EN;
    end;
   GENETExtWrite(Network,EXT_RGMII_OOB_CTRL,Reg);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //bcmgenet_mii_config
end;

{==============================================================================}

function GENETMIIWait(Network:PGENETNetwork):LongWord;
{Note: Caller must hold the network lock}
var
 StartCount:Int64;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: MII Wait');
 {$ENDIF}

 StartCount:=ClockGetTotal;
 repeat
  {Linux driver calls wait_event_timeout with Timeout (in Jiffies) set to HZ / 100
   HZ is the number of Jiffies per second so HZ / 100 is 100th of a second or 10 milliseconds}
  if (ClockGetTotal - StartCount) >= (CLOCK_CYCLES_PER_MILLISECOND * 10) then Break;

 until (GENETUMACRead(Network,UMAC_MDIO_CMD) and MDIO_START_BUSY) = 0;

 Result:=ERROR_SUCCESS;

 //bcmgenet_mii_wait
end;

{==============================================================================}

function GENETPhyReadStatus(Network:PGENETNetwork):LongWord;
{Check the link status and update current link state}

{Note: Caller must hold the network lock}
var
 BMCR:Word;
 BMSR:Word;
 LPA:Word;
 STAT1000:Word;
 CTRL1000:Word;
 ADVERTISE:Word;
 Status:LongWord;
 SpeedDuplex:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: PHY Read Status');
 {$ENDIF}

 //genphy_update_link

 Status:=UniMACMDIORead(Network,MII_BMCR,BMCR);
 if Status <> ERROR_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Autoneg is being started, therefore disregard BMSR value and report link as down}
 BMSR:=0;
 if (BMCR and BMCR_ANRESTART) = 0 then
  begin
   {Read link and autonegotiation status}
   Status:=UniMACMDIORead(Network,MII_BMSR,BMSR);
   if Status <> ERROR_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;
  end;

 {Check link status}
 Network.Link:=PHY_LINK_DOWN;
 if (BMSR and BMSR_LSTATUS) <> 0 then
  begin
   Network.Link:=PHY_LINK_UP;
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 Network.Speed:=PHY_SPEED_UNKNOWN;
 Network.Duplex:=PHY_DUPLEX_UNKNOWN;
 Network.Pause:=0;

 //genphy_read_lpa

 Status:=UniMACMDIORead(Network,MII_STAT1000,STAT1000);
 if Status <> ERROR_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Status:=UniMACMDIORead(Network,MII_CTRL1000,CTRL1000);
 if Status <> ERROR_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 if (STAT1000 and LPA_1000MSFAIL) <> 0 then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Master/Slave resolution failed');

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 Status:=UniMACMDIORead(Network,MII_LPA,LPA);
 if Status <> ERROR_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Status:=UniMACMDIORead(Network,MII_ADVERTISE,ADVERTISE);
 if Status <> ERROR_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Speed/Duplex}
 SpeedDuplex:=STAT1000 and (CTRL1000 shl 2);
 if (SpeedDuplex and (LPA_1000FULL or LPA_1000HALF)) <> 0 then
  begin
   Network.Speed:=PHY_SPEED_1000;

   Network.Duplex:=PHY_DUPLEX_HALF;
   if (SpeedDuplex and LPA_1000FULL) <> 0 then
    begin
     Network.Duplex:=PHY_DUPLEX_FULL;
    end;
  end
 else
  begin
   SpeedDuplex:=LPA and ADVERTISE;

   if (SpeedDuplex and (LPA_100FULL or LPA_100HALF)) <> 0 then
    begin
     Network.Speed:=PHY_SPEED_100;

     Network.Duplex:=PHY_DUPLEX_HALF;
     if (SpeedDuplex and LPA_100FULL) <> 0 then
      begin
       Network.Duplex:=PHY_DUPLEX_FULL;
      end;
    end
   else if (SpeedDuplex and (LPA_10FULL or LPA_10HALF)) <> 0 then
    begin
     Network.Speed:=PHY_SPEED_10;

     Network.Duplex:=PHY_DUPLEX_HALF;
     if (SpeedDuplex and LPA_10FULL) <> 0 then
      begin
       Network.Duplex:=PHY_DUPLEX_FULL;
      end;
    end;
  end;

 {Get Pause}
 if Network.Duplex = PHY_DUPLEX_FULL then
  begin
   if (LPA and LPA_PAUSE_CAP) <> 0 then
    begin
     Network.Pause:=1;
    end;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Link=' + IntToStr(Network.Link));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Speed=' + PhyLinkSpeedToString(Network.Speed));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Duplex=' + PhyDuplexModeToString(Network.Duplex));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Pause=' + IntToStr(Network.Pause));
 {$ENDIF}

 Result:=ERROR_SUCCESS;

 //genphy_read_status
end;

{==============================================================================}

function GENETSysRead(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the SYS registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + GENET_SYS_OFF + Offset)^;

 //bcmgenet_sys_readl
end;

{==============================================================================}

procedure GENETSysWrite(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the SYS registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + GENET_SYS_OFF + Offset)^:=Value;

 //bcmgenet_sys_writel
end;

{==============================================================================}

function GENETExtRead(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the EXT registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + GENET_EXT_OFF + Offset)^;

 //bcmgenet_ext_readl
end;

{==============================================================================}

procedure GENETExtWrite(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the EXT registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + GENET_EXT_OFF + Offset)^:=Value;

 //bcmgenet_ext_writel
end;

{==============================================================================}

function GENETUMACRead(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the UMAC registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + GENET_UMAC_OFF + Offset)^;

 //bcmgenet_umac_readl
end;

{==============================================================================}

procedure GENETUMACWrite(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the UMAC registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + GENET_UMAC_OFF + Offset)^:=Value;

 //bcmgenet_umac_writel
end;

{==============================================================================}

function GENETIntL20Read(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the INT0 registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + GENET_INTRL2_0_OFF + Offset)^;

 //bcmgenet_intrl2_0_readl
end;

{==============================================================================}

procedure GENETIntL20Write(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the INT0 registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + GENET_INTRL2_0_OFF + Offset)^:=Value;

 //bcmgenet_intrl2_0_writel
end;

{==============================================================================}

function GENETIntL21Read(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the INT1 registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + GENET_INTRL2_1_OFF + Offset)^;

 //bcmgenet_intrl2_1_readl
end;

{==============================================================================}

procedure GENETIntL21Write(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the INT1 registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + GENET_INTRL2_1_OFF + Offset)^:=Value;

 //bcmgenet_intrl2_1_writel
end;

{==============================================================================}

function GENETHFBRead(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the HFB (Hardware Filter Buffer}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + Network.HFBOffset + Offset)^;

 //bcmgenet_hfb_readl
end;

{==============================================================================}

procedure GENETHFBWrite(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the HFB (Hardware Filter Buffer}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + Network.HFBOffset + Offset)^:=Value;

 //bcmgenet_hfb_writel
end;

{==============================================================================}

function GENETHFBRegRead(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the HFB registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + Network.HFBRegOffset + Offset)^;

 //bcmgenet_hfb_reg_readl
end;

{==============================================================================}

procedure GENETHFBRegWrite(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the HFB registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + Network.HFBRegOffset + Offset)^:=Value;

 //bcmgenet_hfb_reg_writel
end;

{==============================================================================}

function GENETRBUFRead(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
{Read a value from the RX Buffer registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + GENET_RBUF_OFF + Offset)^;

 //bcmgenet_rbuf_readl
end;

{==============================================================================}

procedure GENETRBUFWrite(Network:PGENETNetwork;Offset,Value:LongWord); inline;
{Write a value to the RX Buffer registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + GENET_RBUF_OFF + Offset)^:=Value;

 //bcmgenet_rbuf_writel
end;

{==============================================================================}

function GENETRBUFCtrlGet(Network:PGENETNetwork):LongWord; inline;
{Read from the RX Buffer flush control register}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 {Check Version}
 if Network.Version = GENET_V1 then
  begin
   Result:=GENETRBUFRead(Network,RBUF_FLUSH_CTRL_V1);
  end
 else
  begin
   Result:=GENETSysRead(Network,SYS_RBUF_FLUSH_CTRL);
  end;

 //bcmgenet_rbuf_ctrl_get
end;

{==============================================================================}

procedure GENETRBUFCtrlSet(Network:PGENETNetwork;Value:LongWord); inline;
{Write to the RX Buffer flush control register}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Version}
 if Network.Version = GENET_V1 then
  begin
   GENETRBUFWrite(Network,RBUF_FLUSH_CTRL_V1,Value);
  end
 else
  begin
   GENETSysWrite(Network,SYS_RBUF_FLUSH_CTRL,Value);
  end;

 //bcmgenet_rbuf_ctrl_set
end;

{==============================================================================}

function GENETTBUFCtrlGet(Network:PGENETNetwork):LongWord; inline;
{Read from the TX Buffer control register}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 {Check Version}
 if Network.Version = GENET_V1 then
  begin
   Result:=GENETRBUFRead(Network,TBUF_CTRL_V1);
  end
 else
  begin
   Result:=PLongWord(Network.Address + Network.TXBufferOffset + TBUF_CTRL)^;
  end;

 //bcmgenet_tbuf_ctrl_get
end;

{==============================================================================}

procedure GENETTBUFCtrlSet(Network:PGENETNetwork;Value:LongWord); inline;
{Write to the TX Buffer control register}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Version}
 if Network.Version = GENET_V1 then
  begin
   GENETRBUFWrite(Network,TBUF_CTRL_V1,Value);
  end
 else
  begin
   PLongWord(Network.Address + Network.TXBufferOffset + TBUF_CTRL)^:=Value;
  end;
end;

{==============================================================================}

function GENETBPMCGet(Network:PGENETNetwork):LongWord; inline;
{Read from the TX Buffer BPMC register}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 {Check Version}
 if Network.Version = GENET_V1 then
  begin
   Result:=GENETRBUFRead(Network,TBUF_BP_MC_V1);
  end
 else
  begin
   Result:=PLongWord(Network.Address + Network.TXBufferOffset + TBUF_BP_MC)^;
  end;

 //bcmgenet_bp_mc_get
end;

{==============================================================================}

procedure GENETBPMCSet(Network:PGENETNetwork;Value:LongWord); inline;
{Write to the TX Buffer BPMC register}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Version}
 if Network.Version = GENET_V1 then
  begin
   GENETRBUFWrite(Network,TBUF_BP_MC_V1,Value);
  end
 else
  begin
   PLongWord(Network.Address + Network.TXBufferOffset + TBUF_BP_MC)^:=Value;
  end;

 //bcmgenet_bp_mc_set
end;

{==============================================================================}

function GENETTDMARead(Network:PGENETNetwork;Reg:Byte):LongWord; inline;
{Read a value from the TX DMA registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + Network.TXDMARegOffset + DMA_RINGS_SIZE + Network.DMARegisters[Reg])^;

 //bcmgenet_tdma_readl
end;

{==============================================================================}

procedure GENETTDMAWrite(Network:PGENETNetwork;Reg:Byte;Value:LongWord); inline;
{Write a value to the TX DMA registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + Network.TXDMARegOffset + DMA_RINGS_SIZE + Network.DMARegisters[Reg])^:=Value;

 //bcmgenet_tdma_writel
end;

{==============================================================================}

function GENETRDMARead(Network:PGENETNetwork;Reg:Byte):LongWord; inline;
{Read a value from the RX DMA registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + Network.RXDMARegOffset + DMA_RINGS_SIZE + Network.DMARegisters[Reg])^;

 //bcmgenet_rdma_readl
end;

{==============================================================================}

procedure GENETRDMAWrite(Network:PGENETNetwork;Reg:Byte;Value:LongWord); inline;
{Write a value to the RX DMA registers}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + Network.RXDMARegOffset + DMA_RINGS_SIZE + Network.DMARegisters[Reg])^:=Value;

 //bcmgenet_rdma_writel
end;

{==============================================================================}

function GENETTDMARingRead(Network:PGENETNetwork;Ring:LongWord;Reg:Byte):LongWord; inline;
{Read a value from a TX DMA ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + Network.TXDMARegOffset + (DMA_RING_SIZE * Ring) + Network.DMARingRegisters[Reg])^;

 //bcmgenet_tdma_ring_readl
end;

{==============================================================================}

procedure GENETTDMARingWrite(Network:PGENETNetwork;Ring:LongWord;Reg:Byte;Value:LongWord); inline;
{Write a value to a TX DMA ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + Network.TXDMARegOffset + (DMA_RING_SIZE * Ring) + Network.DMARingRegisters[Reg])^:=Value;

 //bcmgenet_tdma_ring_writel
end;

{==============================================================================}

function GENETRDMARingRead(Network:PGENETNetwork;Ring:LongWord;Reg:Byte):LongWord; inline;
{Read a value from an RX DMA ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + Network.RXDMARegOffset + (DMA_RING_SIZE * Ring) + Network.DMARingRegisters[Reg])^;

 //bcmgenet_rdma_ring_readl
end;

{==============================================================================}

procedure GENETRDMARingWrite(Network:PGENETNetwork;Ring:LongWord;Reg:Byte;Value:LongWord); inline;
{Write a value to an RX DMA ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + Network.RXDMARegOffset + (DMA_RING_SIZE * Ring) + Network.DMARingRegisters[Reg])^:=Value;

 //bcmgenet_rdma_ring_writel
end;

{==============================================================================}

procedure GENETRXRingIntEnable(Network:PGENETNetwork;Ring:PGENETRXRing);
{Enable interrupts for an RX ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL21Write(Network,INTRL2_CPU_MASK_CLEAR,1 shl (UMAC_IRQ1_RX_INTR_SHIFT + Ring.Index));

 //bcmgenet_rx_ring_int_enable
end;

{==============================================================================}

procedure GENETRXRingIntDisable(Network:PGENETNetwork;Ring:PGENETRXRing);
{Disable interrupts for an RX ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL21Write(Network,INTRL2_CPU_MASK_SET,1 shl (UMAC_IRQ1_RX_INTR_SHIFT + Ring.Index));

 //bcmgenet_rx_ring_int_disable
end;

{==============================================================================}

procedure GENETRXRing16IntEnable(Network:PGENETNetwork;Ring:PGENETRXRing);
{Enable interrupts for RX ring 16}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL20Write(Network,INTRL2_CPU_MASK_CLEAR,UMAC_IRQ0_RXDMA_DONE);

 //bcmgenet_rx_ring16_int_enable
end;

{==============================================================================}

procedure GENETRXRing16IntDisable(Network:PGENETNetwork;Ring:PGENETRXRing);
{Disable interrupts for RX ring 16}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL20Write(Network,INTRL2_CPU_MASK_SET,UMAC_IRQ0_RXDMA_DONE);

 //bcmgenet_rx_ring16_int_disable
end;

{==============================================================================}

procedure GENETTXRingIntEnable(Network:PGENETNetwork;Ring:PGENETTXRing);
{Enable interrupts for a TX ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL21Write(Network,INTRL2_CPU_MASK_CLEAR,1 shl Ring.Index);

 //bcmgenet_tx_ring_int_enable
end;

{==============================================================================}

procedure GENETTXRingIntDisable(Network:PGENETNetwork;Ring:PGENETTXRing);
{Disable interrupts for a TX ring}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL21Write(Network,INTRL2_CPU_MASK_SET,1 shl Ring.Index);

 //bcmgenet_tx_ring_int_disable
end;

{==============================================================================}

procedure GENETTXRing16IntEnable(Network:PGENETNetwork;Ring:PGENETTXRing);
{Enable interrupts for TX ring 16}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL20Write(Network,INTRL2_CPU_MASK_CLEAR,UMAC_IRQ0_TXDMA_DONE);

 //bcmgenet_tx_ring16_int_enable
end;

{==============================================================================}

procedure GENETTXRing16IntDisable(Network:PGENETNetwork;Ring:PGENETTXRing);
{Disable interrupts for TX ring 16}

{Note: Internal only, caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 GENETIntL20Write(Network,INTRL2_CPU_MASK_SET,UMAC_IRQ0_TXDMA_DONE);

 //bcmgenet_tx_ring16_int_disable
end;

{==============================================================================}

function GENETFreeRXControlBlock(Network:PGENETNetwork;Block:PGENETControlBlock):PNetworkEntry;
{Simple helper to free a receive control block's resources}

{Note: Caller must hold the network lock}
var
 Entry:PNetworkEntry;
begin
 {}
 Result:=nil;

 {Check Network}
 if Network = nil then Exit;

 {Check Block}
 if Block = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Free RX Control Block (Block=' + PtrToHex(Block) + ')');
 {$ENDIF}

 {Remove RX buffer from the ring}
 Entry:=Block.Entry;
 Block.Entry:=nil;

 {Invalidate Cache}
 if not(DMA_CACHE_COHERENT) and (Entry <> nil) then
  begin
   InvalidateDataCacheRange(PtrUInt(Entry.Buffer),Entry.Size);
  end;

 Result:=Entry;

 //bcmgenet_free_rx_cb
end;

{==============================================================================}

function GENETRefillRXControlBlock(Network:PGENETNetwork;Block:PGENETControlBlock):PNetworkEntry;
{Note: Caller must hold the network lock}
var
 Entry:PNetworkEntry;
 Current:PNetworkEntry;
begin
 {}
 Result:=nil;

 {Check Network}
 if Network = nil then Exit;

 {Check Block}
 if Block = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Refill RX Control Block (Block=' + PtrToHex(Block) + ')');
 {$ENDIF}

 {Get an available RX buffer}
 Entry:=nil;
 if BufferAvailable(Network.Network.ReceiveQueue.Buffer) > 0 then
  begin
   Entry:=BufferGet(Network.Network.ReceiveQueue.Buffer);
  end;
 if Entry = nil then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(@Network.Network,'GENET: Failed to get receive buffer entry');

   {Update Statistics}
   Inc(Network.Network.BufferUnavailable);
   Exit;
  end;

 {Update Entry}
 Entry.Size:=GENET_MAX_PACKET_SIZE;
 Entry.Offset:=0;
 Entry.Count:=1;

 {Clean Cache}
 if not(DMA_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(Entry.Buffer),Entry.Size);
  end;

 {Get the current RX buffer from the ring}
 Current:=GENETFreeRXControlBlock(Network,Block);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Put the new RX buffer on the ring}
 Block.Entry:=Entry;
 GENETDMADescriptorSetAddress(Network,Block.DescriptorAddress,PtrUInt(Entry.Buffer));

 Result:=Current;

 //bcmgenet_rx_refill
end;

{==============================================================================}

procedure GENETFreeTXControlBlock(Network:PGENETNetwork;Block:PGENETControlBlock);
{Simple helper to free a transmit control block's resources}

{Note: Caller must hold the network lock}
var
 Entry:PNetworkEntry;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Block}
 if Block = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Free TX Control Block (Block=' + PtrToHex(Block) + ')');
 {$ENDIF}

 {Remove TX buffer from the ring}
 Entry:=Block.Entry;
 Block.Entry:=nil;

 {Free Buffer}
 if Entry <> nil then BufferFree(Entry);

 //bcmgenet_free_tx_cb
end;

{==============================================================================}

function GENETGetTXControlBlock(Network:PGENETNetwork;Ring:PGENETTXRing):PGENETControlBlock;
{Note: Caller must hold the network lock}
var
 Block:PGENETControlBlock;
begin
 {}
 Result:=nil;

 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Get TX Control Block (Index=' + IntToStr(Ring.Index) + ')');
 {$ENDIF}

 Block:=@Ring.ControlBlocks[Ring.Write];

 {Advance local write pointer} //To Do //Make this the same as the others ?
 if Ring.Write = Ring.Last then
  begin
   Ring.Write:=Ring.First;
  end
 else
  begin
   Inc(Ring.Write);
  end;

 Result:=Block;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Block=' + PtrToHex(Block));
 {$ENDIF}

 //bcmgenet_get_txcb
end;

{==============================================================================}

procedure GENETReclaimTXControlBlocks(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
var
 Count:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Reclaim TX Control Blocks');
 {$ENDIF}

 {Reclaim TX Buffers}
 if Network.TXQueues > 0 then
  begin
   for Count:=0 to Network.TXQueues - 1 do
    begin
     GENETReclaimTXControlBlock(Network,@Network.TXRings[Count],False)
    end;
  end;

 GENETReclaimTXControlBlock(Network,@Network.TXRings[GENET_DESC_INDEX],False)

 //bcmgenet_tx_reclaim_all
end;

{==============================================================================}

procedure GENETReclaimTXControlBlock(Network:PGENETNetwork;Ring:PGENETTXRing;Worker:Boolean);
{Note: Caller must hold the network lock}
var
 Consumer:LongWord;
 BuffersReady:LongWord;
 BuffersProcessed:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {Check Ring}
 if Ring = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Reclaim TX Control Block (Index=' + IntToStr(Ring.Index) + ')');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear status before servicing to reduce spurious interrupts}
 if Ring.Index = GENET_DESC_INDEX then
  begin
   GENETIntL20Write(Network,INTRL2_CPU_CLEAR,UMAC_IRQ0_TXDMA_DONE);
  end
 else
  begin
   GENETIntL21Write(Network,INTRL2_CPU_CLEAR,(1 shl Ring.Index));
  end;

 {Acquire Spin Lock}
 if Worker and (SpinLockIRQ(Network.Lock) = ERROR_SUCCESS) then
  begin
   {Clear Worker}
   Ring.Worker:=INVALID_HANDLE_VALUE;

   {Release Spin Lock}
   SpinUnlockIRQ(Network.Lock);
  end;

 {Compute how many buffers are transmitted since last transmit call}
 Consumer:=GENETTDMARingRead(Network,Ring.Index,TDMA_CONS_INDEX) and DMA_C_INDEX_MASK;
 BuffersProcessed:=0;
 BuffersReady:=(Consumer - Ring.Consumer) and DMA_C_INDEX_MASK;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Interrupts=' + IntToStr(Network.InterruptCount));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Consumer Index=' + IntToStr(Consumer));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Producer Index=' + IntToStr(Ring.Producer));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Write Pointer=' + IntToStr(Ring.Write));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Clean Pointer=' + IntToStr(Ring.Clean));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Free Count=' + IntToStr(Ring.Free));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Buffers Ready=' + IntToStr(BuffersReady));
 {$ENDIF}

 {Reclaim transmitted buffers}
 while BuffersProcessed < BuffersReady do
  begin
   GENETFreeTXControlBlock(Network,@Network.TXControlBlocks[Ring.Clean]);

   Inc(BuffersProcessed);

   {Update Ring}
   if Ring.Clean < Ring.Last then
    begin
     Inc(Ring.Clean);
    end
   else
    begin
     Ring.Clean:=Ring.First;
    end;
  end;

 {Update Free and Consumer}
 Inc(Ring.Free,BuffersProcessed);
 Ring.Consumer:=Consumer;

 {Enable Interrupts}
 Ring.IntEnable(Network,Ring);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcmgenet_tx_reclaim / __bcmgenet_tx_reclaim
end;

{==============================================================================}

procedure GENETDMADescriptorSet(Network:PGENETNetwork;Descriptor,Address:PtrUInt;Value:LongWord);
{Note: Caller must hold the network lock}
begin
 {}
 GENETDMADescriptorSetAddress(Network,Descriptor,Address);
 GENETDMADescriptorSetLengthStatus(Network,Descriptor,Value);

 //dmadesc_set
end;

{==============================================================================}

procedure GENETDMADescriptorSetAddress(Network:PGENETNetwork;Descriptor,Address:PtrUInt);
{Note: Caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMA Descriptor Set Address (Descriptor=' + AddrToHex(Descriptor) + ' Address=' + AddrToHex(Address) + ')');
 {$ENDIF}

 {On the Raspberry Pi 4 the GENET device is a "large address master" which is aware of the
  full 35-bit address space and therefore doesn't need to use the bus addresses that are
  required for the standard peripherals using the "legacy master" address space}

 {$IFDEF CPU32}
 {Write DMA address}
 PLongWord(Descriptor + DMA_DESC_ADDRESS_LO)^:=Address;
 {$ENDIF CPU32}

 {$IFDEF CPU64}
 {Write DMA address (Low 32 bits)}
 PLongWord(Descriptor + DMA_DESC_ADDRESS_LO)^:=Int64Rec(Address).Lo;

 {Register writes to GISB bus can take couple hundred nanoseconds
  and are done for each packet, save these expensive writes unless
  the platform is explicitly configured for 64-bits/LPAE}
 if (Network.Flags and GENET_HAS_40BITS) <> 0 then
  begin
   {Write DMA address (High 32 bits)}
   PLongWord(Descriptor + DMA_DESC_ADDRESS_HI)^:=Int64Rec(Address).Hi;
  end;
 {$ENDIF CPU64}

 //dmadesc_set_addr
end;

{==============================================================================}

procedure GENETDMADescriptorSetLengthStatus(Network:PGENETNetwork;Descriptor:PtrUInt;Value:LongWord);
{Note: Caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMA Descriptor Set Length Status (Descriptor=' + AddrToHex(Descriptor) + ' Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}

 {Write DMA length/status}
 PLongWord(Descriptor + DMA_DESC_LENGTH_STATUS)^:=Value;

 //dmadesc_set_length_status
end;

{==============================================================================}

function GENETDMADescriptorGetAddress(Network:PGENETNetwork;Descriptor:PtrUInt):PtrUInt;
{Note: Caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMA Descriptor Get Address (Descriptor=' + AddrToHex(Descriptor) + ')');
 {$ENDIF}

 {$IFDEF CPU32}
 {Read DMA address}
 Result:=PLongWord(Descriptor + DMA_DESC_ADDRESS_LO)^;
 {$ENDIF CPU32}

 {$IFDEF CPU64}
 {Read DMA address (Low 32-bits)}
 Int64Rec(Result).Lo:=PLongWord(Descriptor + DMA_DESC_ADDRESS_LO)^;

 {Register writes to GISB bus can take couple hundred nanoseconds
  and are done for each packet, save these expensive writes unless
  the platform is explicitly configured for 64-bits/LPAE}
 if (Network.Flags and GENET_HAS_40BITS) <> 0 then
  begin
   {Read DMA address (High 32 bits)}
   Int64Rec(Result).Hi:=PLongWord(Descriptor + DMA_DESC_ADDRESS_HI)^;
  end;
 {$ENDIF CPU64}

 //dmadesc_get_addr
end;

{==============================================================================}

function GENETDMADescriptorGetLengthStatus(Network:PGENETNetwork;Descriptor:PtrUInt):LongWord;
{Note: Caller must hold the network lock}
begin
 {}
 Result:=0;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: DMA Descriptor Get Length Status (Descriptor=' + AddrToHex(Descriptor) + ')');
 {$ENDIF}

 {Read DMA length/status}
 Result:=PLongWord(Descriptor + DMA_DESC_LENGTH_STATUS)^;

 //dmadesc_get_length_status
end;

{==============================================================================}

procedure GENETSetRXMode(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
const
 MAX_MDF_FILTER = 17;

 procedure GENETSetMDFAddress(Network:PGENETNetwork;const Address:THardwareAddress;var Offset,Count,Mask:LongWord);
 begin
  {}
  {Check Network}
  if Network = nil then Exit;

  GENETUMACWrite(Network,UMAC_MDF_ADDR + (Offset * 4),(Address[0] shl 8) or Address[1]);
  GENETUMACWrite(Network,UMAC_MDF_ADDR + ((Offset + 1) * 4),(Address[2] shl 24) or (Address[3] shl 16) or (Address[4] shl 8) or Address[5]);

  Inc(Offset,2);

  Mask:=Mask or (1 shl ((MAX_MDF_FILTER - 1) - Count));

  Inc(Count);

  //bcmgenet_set_mdf_addr
 end;

var
 Reg:LongWord;
 Mask:LongWord;
 Count:LongWord;
 Offset:LongWord;
 FilterCount:LongWord;
 Address:THardwareAddress;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Set RX Mode');
 {$ENDIF}

 FilterCount:=2; //To Do //Add multicast support

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Turn on promicuous mode if the number of filters needed
  exceeds the number filters supported by the hardware}
 Reg:=GENETUMACRead(Network,UMAC_CMD);
 if FilterCount > MAX_MDF_FILTER then
  begin
   Reg:=Reg or CMD_PROMISC;
   GENETUMACWrite(Network,UMAC_CMD,Reg);
   GENETUMACWrite(Network,UMAC_MDF_CTRL,0);
   Exit;
  end
 else
  begin
   Reg:=Reg and not(CMD_PROMISC);
   GENETUMACWrite(Network,UMAC_CMD,Reg);
  end;

 {Update MDF filter}
 Mask:=0;
 Count:=0;
 Offset:=0;

 {Broadcast Address}
 GENETSetMDFAddress(Network,HARDWARE_BROADCAST,Offset,Count,Mask);

 {Local Address}
 GENETGetMACAddress(Network,@Address);
 GENETSetMDFAddress(Network,Address,Offset,Count,Mask);

 {Enable filters}
 GENETUMACWrite(Network,UMAC_MDF_CTRL,Mask);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcmgenet_set_rx_mode
end;

{==============================================================================}

procedure GENETUMACEnableSet(Network:PGENETNetwork;Mask:LongWord;Enable:Boolean);
{Note: Caller must hold the network lock}
var
 Reg:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: UMAC Enable Set (Mask=' + IntToHex(Mask,8) + ')');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 Reg:=GENETUMACRead(Network,UMAC_CMD);
 if Enable then Reg:=Reg or Mask else Reg:=Reg and not(Mask);
 GENETUMACWrite(Network,UMAC_CMD,Reg);

 {UniMAC stops on a packet boundary, wait for a full-size packet to be processed}
 if not Enable then MicrosecondDelay(2000);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //umac_enable_set
end;

{==============================================================================}

procedure GENETInterruptDisable(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Interrupt Disable');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Mask all interrupts}
 GENETIntL20Write(Network,INTRL2_CPU_MASK_SET,$FFFFFFFF);
 GENETIntL20Write(Network,INTRL2_CPU_CLEAR,$FFFFFFFF);
 GENETIntL21Write(Network,INTRL2_CPU_MASK_SET,$FFFFFFFF);
 GENETIntL21Write(Network,INTRL2_CPU_CLEAR,$FFFFFFFF);

 //bcmgenet_intr_disable
end;

{==============================================================================}

procedure GENETLinkInterruptEnable(Network:PGENETNetwork);
{Note: Caller must hold the network lock}
var
 Int0Enable:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Link Interrupt Enable');
 {$ENDIF}

 Int0Enable:=0;

 {Monitor cable plug/unplugged event for internal PHY, external PHY and MoCA PHY}
 if Network.PhyInternal then
  begin
   Int0Enable:=Int0Enable or UMAC_IRQ0_LINK_EVENT;

   if (Network.Version = GENET_V1) or (Network.Version = GENET_V2) or (Network.Version = GENET_V3) then
    begin
     Int0Enable:=Int0Enable or UMAC_IRQ0_PHY_DET_R;
    end;
  end
 else if Network.PhyExternal then
  begin
   Int0Enable:=Int0Enable or UMAC_IRQ0_LINK_EVENT;
  end
 else if Network.PhyMode = PHY_INTERFACE_MODE_MOCA then
  begin
   if (Network.Flags and GENET_HAS_MOCA_LINK_DET) <> 0 then
    begin
     Int0Enable:=Int0Enable or UMAC_IRQ0_LINK_EVENT;
    end;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 GENETIntL20Write(Network,INTRL2_CPU_MASK_CLEAR,Int0Enable);

 //bcmgenet_link_intr_enable
end;

{==============================================================================}

function GENETPhyGetId(Network:PGENETNetwork;var PhyId:LongWord):LongWord;
var
 Value:Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: PHY Get Id');
 {$ENDIF}

 {Grab the bits from PHYSID1, and put them in the upper half}
 Result:=UniMACMDIORead(Network,MII_PHYSID1,Value);
 if Result <> ERROR_SUCCESS then Exit;

 PhyId:=Value shl 16;

 {Grab the bits from PHYSID2, and put them in the lower half}
 Result:=UniMACMDIORead(Network,MII_PHYSID2,Value);
 if Result <> ERROR_SUCCESS then Exit;

 PhyId:=PhyId or Value;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Id=' + IntToHex(PhyId,8));
 {$ENDIF}

 Result:=ERROR_SUCCESS;

 //get_phy_id
end;

{==============================================================================}

function GENETBRCMPhyModel(PhyId:LongWord):LongWord; inline;
begin
 {}
 Result:=PhyId and PHY_ID_MASK;
 //BRCM_PHY_MODEL
end;

{==============================================================================}

function GENETBRCMPhyRevision(PhyId:LongWord):LongWord; inline;
begin
 {}
 Result:=PhyId and not(PHY_ID_MASK);
 //BRCM_PHY_REV
end;

{==============================================================================}

function GENETPhyInitHardware(Network:PGENETNetwork):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: PHY Init Hardware');
 {$ENDIF}

 case GENETBRCMPhyModel(Network.PhyId) of
  PHY_ID_BCM5411,
  PHY_ID_BCM5421,
  PHY_ID_BCM54210E,
  PHY_ID_BCM5461,
  PHY_ID_BCM54612E,
  PHY_ID_BCM54616S,
  PHY_ID_BCM5464,
  PHY_ID_BCM5481,
  PHY_ID_BCM54810,
  PHY_ID_BCM50610,
  PHY_ID_BCM50610M,
  PHY_ID_BCM57780,
  PHY_ID_BCM89610:begin
    Result:=GENETBCM54XXConfigInit(Network);
   end;
  PHY_ID_BCM5482:begin
    //bcm5482_config_init
   end;
  PHY_ID_BCMAC131,
  PHY_ID_BCM5241:begin
    //brcm_fet_config_init
   end;
  PHY_ID_BCM5395:begin
    {Nothing}
   end;
 else
  begin
   Result:=ERROR_SUCCESS;
  end;
 end;

 //phy_init_hw
end;

{==============================================================================}

function GENETMIIBCM54XXShadowValue(Value:Word):Word; inline;
begin
 {}
 Result:=(Value and $1f) shl 10;

 //MII_BCM54XX_SHD_VAL
end;

{==============================================================================}

function GENETMIIBCM54XXShadowData(Data:Word):Word; inline;
begin
 {}
 Result:=(Data and $3ff) shl 0;

 //MII_BCM54XX_SHD_DATA
end;

{==============================================================================}

function GENETBCM5482ShadowLEDS1LED3(Source:Word):Word; inline;
begin
 {}
 Result:=(Source and $f) shl 4;

 //BCM5482_SHD_LEDS1_LED3
end;

{==============================================================================}

function GENETBCM5482ShadowLEDS1LED1(Source:Word):Word; inline;
begin
 {}
 Result:=(Source and $f) shl 0;

 //BCM5482_SHD_LEDS1_LED1
end;

{==============================================================================}

function GENETBCMPhyReadShadow(Network:PGENETNetwork;Shadow:Word;var Value:Word):LongWord;
begin
 {}
 Value:=0;

 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_SHD,GENETMIIBCM54XXShadowValue(Shadow));
 if Result <> ERROR_SUCCESS then Exit;

 Result:=UniMACMDIORead(Network,MII_BCM54XX_SHD,Value);
 if Result <> ERROR_SUCCESS then Exit;

 Value:=GENETMIIBCM54XXShadowData(Value);

 //bcm_phy_read_shadow
end;

{==============================================================================}

function GENETBCMPhyWriteShadow(Network:PGENETNetwork;Shadow,Value:Word):LongWord;
begin
 {}
 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_SHD,MII_BCM54XX_SHD_WRITE or GENETMIIBCM54XXShadowValue(Shadow) or GENETMIIBCM54XXShadowData(Value));

 //bcm_phy_write_shadow
end;

{==============================================================================}

function GENETBCMPhyReadExpansion(Network:PGENETNetwork;Reg:Word;var Value:Word):LongWord;
begin
 {}
 Value:=0;

 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_EXP_SEL,Reg);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=UniMACMDIORead(Network,MII_BCM54XX_EXP_DATA,Value);
 if Result <> ERROR_SUCCESS then Exit;

 {Restore default value.  It's okay if this write fails}
 UniMACMDIOWrite(Network,MII_BCM54XX_EXP_SEL,0);

 //__bcm_phy_read_exp
end;

{==============================================================================}

function GENETBCMPhyWriteExpansion(Network:PGENETNetwork;Reg:Word;Value:Word):LongWord;
begin
 {}
 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_EXP_SEL,Reg);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_EXP_DATA,Value);

 //__bcm_phy_write_exp
end;

{==============================================================================}

function GENETBCM54XXConfigInit(Network:PGENETNetwork):LongWord;
var
 Value:Word;
 LEDModes:array[0..1] of Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: BCM54XX Config Init');
 {$ENDIF}

 {LED Modes}
 LEDModes[0]:=BCM_LED_MULTICOLOR_LINK_ACT;
 LEDModes[1]:=BCM_LED_MULTICOLOR_LINK;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Get Extended Control Register}
 Result:=UniMACMDIORead(Network,MII_BCM54XX_ECR,Value);
 if Result <> ERROR_SUCCESS then Exit;

 {Mask interrupts globally}
 Value:=Value or MII_BCM54XX_ECR_IM;
 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_ECR,Value);
 if Result <> ERROR_SUCCESS then Exit;

 {Unmask events we are interested in}
 Value:=not(MII_BCM54XX_INT_DUPLEX or MII_BCM54XX_INT_SPEED or MII_BCM54XX_INT_LINK);
 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_IMR,Value);
 if Result <> ERROR_SUCCESS then Exit;

 case GENETBRCMPhyModel(Network.PhyId) of
  PHY_ID_BCM50610,PHY_ID_BCM50610M:begin
    if (Network.PhyFlags and PHY_BRCM_CLEAR_RGMII_MODE) <> 0 then
     begin
      GENETBCMPhyWriteShadow(Network,BCM54XX_SHD_RGMII_MODE,0);
     end;
   end;
 end;

 if (Network.PhyFlags and (PHY_BRCM_RX_REFCLK_UNUSED or PHY_BRCM_DIS_TXCRXC_NOENRGY or PHY_BRCM_AUTO_PWRDWN_ENABLE)) <> 0 then
  begin
   GENETBCM54XXAdjustRXReferenceClock(Network);
  end;

 case GENETBRCMPhyModel(Network.PhyId) of
  PHY_ID_BCM54210E:begin
    Result:=GENETBCM54210EConfigInit(Network);
    if Result <> ERROR_SUCCESS then Exit;
   end;
  PHY_ID_BCM54612E:begin
    Result:=GENETBCM54612EConfigInit(Network);
    if Result <> ERROR_SUCCESS then Exit;
   end;
  PHY_ID_BCM54810:begin
    {For BCM54810, we need to disable BroadR-Reach function}
    GENETBCMPhyReadExpansion(Network,BCM54810_EXP_BROADREACH_LRE_MISC_CTL,Value);
    Value:=Value and not(BCM54810_EXP_BROADREACH_LRE_MISC_CTL_EN);

    Result:=GENETBCMPhyWriteExpansion(Network,BCM54810_EXP_BROADREACH_LRE_MISC_CTL,Value);
    if Result <> ERROR_SUCCESS then Exit;
   end;
 end;

 GENETBCM54XXPhyDSPConfig(Network);

 Value:=GENETBCM5482ShadowLEDS1LED1(BCM_LED_SRC_MULTICOLOR1) or GENETBCM5482ShadowLEDS1LED3(BCM_LED_SRC_MULTICOLOR1);
 GENETBCMPhyWriteShadow(Network,BCM5482_SHD_LEDS1,Value);

 Value:=BCM_LED_MULTICOLOR_IN_PHASE or GENETBCM5482ShadowLEDS1LED1(LEDModes[0]) or GENETBCM5482ShadowLEDS1LED3(LEDModes[1]);
 GENETBCMPhyWriteExpansion(Network,BCM_EXP_MULTICOLOR,Value);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //bcm54xx_config_init
end;

{==============================================================================}

function GENETBCM54210EConfigInit(Network:PGENETNetwork):LongWord;
var
 Value:Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: BCM54210E Config Init');
 {$ENDIF}

 GENETBCM54XXConfigClockDelay(Network);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 if (Network.PhyFlags and PHY_BRCM_EN_MASTER_MODE) <> 0 then
  begin
   UniMACMDIORead(Network,MII_CTRL1000,Value);
   Value:=Value or CTL1000_AS_MASTER or CTL1000_ENABLE_MASTER;
   UniMACMDIOWrite(Network,MII_CTRL1000,Value);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //bcm54210e_config_init
end;

{==============================================================================}

function GENETBCM54612EConfigInit(Network:PGENETNetwork):LongWord;
var
 Value:Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: BCM54612E Config Init');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear TX internal delay unless requested}
 if (Network.PhyMode <> PHY_INTERFACE_MODE_RGMII_ID) and (Network.PhyMode <> PHY_INTERFACE_MODE_RGMII_TXID) then
  begin
    {Disable TXD to GTXCLK clock delay (default set)}
    {Bit 9 is the only field in shadow register 00011}
    GENETBCMPhyWriteShadow(Network,$03,0);
  end;

 {Clear RX internal delay unless requested}
 if (Network.PhyMode <> PHY_INTERFACE_MODE_RGMII_ID) and (Network.PhyMode <> PHY_INTERFACE_MODE_RGMII_RXID) then
  begin
    GENETBCM54XXAuxCtrlRead(Network,MII_BCM54XX_AUXCTL_SHDWSEL_MISC,Value);

    {Disable RXD to RXC delay (default set)}
    Value:=Value and not(MII_BCM54XX_AUXCTL_SHDWSEL_MISC_RGMII_SKEW_EN);

    {Clear shadow selector field}
    Value:=Value and not(MII_BCM54XX_AUXCTL_SHDWSEL_MASK);

    GENETBCM54XXAuxCtrlWrite(Network,MII_BCM54XX_AUXCTL_SHDWSEL_MISC,MII_BCM54XX_AUXCTL_MISC_WREN or Value);
  end;

 {Enable CLK125 MUX on LED4 if ref clock is enabled}
 if (Network.PhyFlags and PHY_BRCM_RX_REFCLK_UNUSED) = 0 then
  begin
   GENETBCMPhyReadExpansion(Network,BCM54612E_EXP_SPARE0,Value);

   Result:=GENETBCMPhyWriteExpansion(Network,BCM54612E_EXP_SPARE0,BCM54612E_LED4_CLK125OUT_EN or Value);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //bcm54612e_config_init
end;

{==============================================================================}

function GENETBCM54XXPhyDSPConfig(Network:PGENETNetwork):LongWord;
var
 Value:Word;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: BCM54XX Phy DSP Config');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Enable the SMDSP clock}
 Result:=GENETBCM54XXAuxCtrlWrite(Network,MII_BCM54XX_AUXCTL_SHDWSEL_AUXCTL,MII_BCM54XX_AUXCTL_ACTL_SMDSP_ENA or MII_BCM54XX_AUXCTL_ACTL_TX_6DB);
 if Result <> ERROR_SUCCESS then Exit;
 try
  case GENETBRCMPhyModel(Network.PhyId) of
   PHY_ID_BCM50610,
   PHY_ID_BCM50610M:begin
     {Clear bit 9 to fix a phy interop issue}
     Result:=GENETBCMPhyWriteExpansion(Network,MII_BCM54XX_EXP_EXP08,MII_BCM54XX_EXP_EXP08_RJCT_2MHZ);
     if Result <> ERROR_SUCCESS then Exit;

     if GENETBRCMPhyModel(Network.PhyId) = PHY_ID_BCM50610 then
      begin
       Result:=GENETBCM50610A0Workaround(Network);
       if Result <> ERROR_SUCCESS then Exit;
      end;
    end;
   PHY_ID_BCM57780:begin
     Result:=GENETBCMPhyReadExpansion(Network,MII_BCM54XX_EXP_EXP75,Value);
     if Result <> ERROR_SUCCESS then Exit;

     Value:=Value or MII_BCM54XX_EXP_EXP75_CM_OSC;
     Result:=GENETBCMPhyWriteExpansion(Network,MII_BCM54XX_EXP_EXP75,Value);
     if Result <> ERROR_SUCCESS then Exit;
    end;
  end;

  Result:=ERROR_SUCCESS;
 finally
  {Disable the SMDSP clock}
  Status:=GENETBCM54XXAuxCtrlWrite(Network,MII_BCM54XX_AUXCTL_SHDWSEL_AUXCTL,MII_BCM54XX_AUXCTL_ACTL_TX_6DB);
  if (Result = ERROR_SUCCESS) and (Status <> ERROR_SUCCESS) then
   begin
    Result:=Status;
   end;
 end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 //bcm54xx_phydsp_config
end;

{==============================================================================}

function GENETBCM50610A0Workaround(Network:PGENETNetwork):LongWord;
{Needs SMDSP clock enabled via GENETBCM54XXPhyDSPConfig}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: BCM50610 AO Workaround');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 Result:=GENETBCMPhyWriteExpansion(Network,MII_BCM54XX_EXP_AADJ1CH0,MII_BCM54XX_EXP_AADJ1CH0_SWP_ABCD_OEN or MII_BCM54XX_EXP_AADJ1CH0_SWSEL_THPF);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GENETBCMPhyWriteExpansion(Network,MII_BCM54XX_EXP_AADJ1CH3,MII_BCM54XX_EXP_AADJ1CH3_ADCCKADJ);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GENETBCMPhyWriteExpansion(Network,MII_BCM54XX_EXP_EXP75,MII_BCM54XX_EXP_EXP75_VDACCTRL);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GENETBCMPhyWriteExpansion(Network,MII_BCM54XX_EXP_EXP96,MII_BCM54XX_EXP_EXP96_MYST);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GENETBCMPhyWriteExpansion(Network,MII_BCM54XX_EXP_EXP97,MII_BCM54XX_EXP_EXP97_MYST);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=ERROR_SUCCESS;

 //bcm50610_a0_workaround
end;

{==============================================================================}

function GENETBCM54XXConfigClockDelay(Network:PGENETNetwork):LongWord;
var
 Value:Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: BCM54XX Config Clock Delay');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Handling PHY's internal RX clock delay}
 Result:=GENETBCM54XXAuxCtrlRead(Network,MII_BCM54XX_AUXCTL_SHDWSEL_MISC,Value);
 if Result <> ERROR_SUCCESS then Exit;

 Value:=Value or MII_BCM54XX_AUXCTL_MISC_WREN;
 if (Network.PhyMode = PHY_INTERFACE_MODE_RGMII) or (Network.PhyMode = PHY_INTERFACE_MODE_RGMII_TXID) then
  begin
   {Disable RGMII RXC-RXD skew}
   Value:=Value and not(MII_BCM54XX_AUXCTL_SHDWSEL_MISC_RGMII_SKEW_EN);
  end;
 if (Network.PhyMode = PHY_INTERFACE_MODE_RGMII_ID) or (Network.PhyMode = PHY_INTERFACE_MODE_RGMII_RXID) then
  begin
   {Enable RGMII RXC-RXD skew}
   Value:=Value or MII_BCM54XX_AUXCTL_SHDWSEL_MISC_RGMII_SKEW_EN;
  end;

 Result:=GENETBCM54XXAuxCtrlWrite(Network,MII_BCM54XX_AUXCTL_SHDWSEL_MISC,Value);
 if Result <> ERROR_SUCCESS then Exit;

 {Handling PHY's internal TX clock delay}
 Result:=GENETBCMPhyReadShadow(Network,BCM54810_SHD_CLK_CTL,Value);

 if (Network.PhyMode = PHY_INTERFACE_MODE_RGMII) or (Network.PhyMode = PHY_INTERFACE_MODE_RGMII_RXID) then
  begin
   {Disable internal TX clock delay}
   Value:=Value and not(BCM54810_SHD_CLK_CTL_GTXCLK_EN);
  end;
 if (Network.PhyMode = PHY_INTERFACE_MODE_RGMII_ID) or (Network.PhyMode = PHY_INTERFACE_MODE_RGMII_TXID) then
  begin
   {Enable internal TX clock delay}
   Value:=Value or BCM54810_SHD_CLK_CTL_GTXCLK_EN;
  end;

 Result:=GENETBCMPhyWriteShadow(Network,BCM54810_SHD_CLK_CTL,Value);
 if Result <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //bcm54xx_config_clock_delay
end;

{==============================================================================}

function GENETBCM54XXAdjustRXReferenceClock(Network:PGENETNetwork):LongWord;
var
 Value:Word;
 Current:Word;
 Clock125Enable:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: BCM54XX Adjust Reference Clock');
 {$ENDIF}

 Clock125Enable:=True;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Abort if we are using an untested phy}
 case GENETBRCMPhyModel(Network.PhyId) of
  PHY_ID_BCM57780,
  PHY_ID_BCM50610,
  PHY_ID_BCM50610M:begin
    Result:=GENETBCMPhyReadShadow(Network,BCM54XX_SHD_SCR3,Value);
    if Result <> ERROR_SUCCESS then Exit;
   end;
  else
   begin
    Exit;
   end;
 end;

 Current:=Value;

 if ((GENETBRCMPhyModel(Network.PhyId) = PHY_ID_BCM50610) or (GENETBRCMPhyModel(Network.PhyId) = PHY_ID_BCM50610M)) and (GENETBRCMPhyRevision(Network.PhyId) >= $3) then
  begin
   {Here, bit 0 _disables_ CLK125 when set, This bit is set by default}
   Clock125Enable:=False;
  end
 else
  begin
   if (Network.PhyFlags and PHY_BRCM_RX_REFCLK_UNUSED) <> 0 then
    begin
     {Here, bit 0 _enables_ CLK125 when set}
     Value:=Value and not(BCM54XX_SHD_SCR3_DEF_CLK125);
     Clock125Enable:=False;
    end;
  end;

 if not(Clock125Enable) or ((Network.PhyFlags and PHY_BRCM_AUTO_PWRDWN_ENABLE) <> 0) then
  begin
   Value:=Value and not(BCM54XX_SHD_SCR3_DLLAPD_DIS);
  end
 else
  begin
   Value:=Value or BCM54XX_SHD_SCR3_DLLAPD_DIS;
  end;

 if (Network.PhyFlags and PHY_BRCM_DIS_TXCRXC_NOENRGY) <> 0 then
  begin
   Value:=Value or BCM54XX_SHD_SCR3_TRDDAPD;
  end;

 if Current <> Value then
  begin
   Result:=GENETBCMPhyWriteShadow(Network,BCM54XX_SHD_SCR3,Value);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 Result:=GENETBCMPhyReadShadow(Network,BCM54XX_SHD_APD,Value);
 if Result <> ERROR_SUCCESS then Exit;

 Current:=Value;

 if not(Clock125Enable) or ((Network.PhyFlags and PHY_BRCM_AUTO_PWRDWN_ENABLE) <> 0) then
  begin
   Value:=Value or BCM54XX_SHD_APD_EN;
  end
 else
  begin
   Value:=Value and not(BCM54XX_SHD_APD_EN);
  end;

 if Current <> Value then
  begin
   Result:=GENETBCMPhyWriteShadow(Network,BCM54XX_SHD_APD,Value);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //bcm54xx_adjust_rxrefclk
end;

{==============================================================================}

function GENETBCM54XXAuxCtrlRead(Network:PGENETNetwork;Reg:Word;var Value:Word):LongWord;
begin
 {}
 Value:=0;

 {The register must be written to both the Shadow Register Select and the Shadow Read Register Selector}
 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_AUX_CTL,MII_BCM54XX_AUXCTL_SHDWSEL_MASK or (Reg shl MII_BCM54XX_AUXCTL_SHDWSEL_READ_SHIFT));
 if Result <> ERROR_SUCCESS then Exit;

 Result:=UniMACMDIORead(Network,MII_BCM54XX_AUX_CTL,Value);

 //bcm54xx_auxctl_read
end;

{==============================================================================}

function GENETBCM54XXAuxCtrlWrite(Network:PGENETNetwork;Reg:Word;Value:Word):LongWord;
begin
 {}
 Result:=UniMACMDIOWrite(Network,MII_BCM54XX_AUX_CTL,Reg or Value);

 //bcm54xx_auxctl_write
end;

{==============================================================================}

procedure GENETStatusTimer(Network:PGENETNetwork);
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: Status Timer');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Network.Network.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get PHY Status}
    if GENETPHYReadStatus(Network) = ERROR_SUCCESS then
     begin
      GENETMIISetup(Network);

      {Check PHY Status}
      if Network.Link = PHY_LINK_UP then
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
     end;

    {Enable Timer}
    TimerEnable(Network.StatusTimer);
   finally
    {Release the Lock}
    MutexUnlock(Network.Network.Lock);
   end;
  end;
end;


{==============================================================================}
{==============================================================================}
{UniMAC Helper Functions}
function _UniMACMDIORead(Network:PGENETNetwork;Offset:LongWord):LongWord; inline;
begin
 {}
 Result:=MDIO_READ_FAIL;

 {Check Network}
 if Network = nil then Exit;

 Result:=PLongWord(Network.Address + Network.MDIOOffset + Offset)^;

 //unimac_mdio_readl
end;

{==============================================================================}

procedure _UniMACMDIOWrite(Network:PGENETNetwork;Offset,Value:LongWord); inline;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 PLongWord(Network.Address + Network.MDIOOffset + Offset)^:=Value;

 //unimac_mdio_writel
end;

{==============================================================================}

procedure UniMACMDIOStart(Network:PGENETNetwork);
var
 Reg:LongWord;
begin
 {}
 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: UMAC MDIO Start');
 {$ENDIF}

 Reg:=_UniMACMDIORead(Network,MDIO_CMD);
 Reg:=Reg or MDIO_START_BUSY;
 _UniMACMDIOWrite(Network,MDIO_CMD,Reg);

 //unimac_mdio_start
end;

{==============================================================================}

function UniMACMDIOBusy(Network:PGENETNetwork):LongWord;
begin
 {}
 Result:=_UniMACMDIORead(Network,MDIO_CMD) and MDIO_START_BUSY;

 //unimac_mdio_busy
end;

{==============================================================================}

function UniMACMDIOPoll(Network:PGENETNetwork):LongWord;
var
 Timeout:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: UMAC MDIO Poll');
 {$ENDIF}

 Timeout:=1000;

 repeat
  if UniMACMDIOBusy(Network) = 0 then
   begin
    Result:=ERROR_SUCCESS;
    Exit;
   end;

  MicrosecondDelay(1000);

  Dec(Timeout);
 until Timeout = 0;

 Result:=ERROR_TIMEOUT;

 //unimac_mdio_poll
end;

{==============================================================================}

function UniMACMDIORead(Network:PGENETNetwork;Reg:LongWord;var Value:Word):LongWord;
var
 Command:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: UMAC MDIO Read');
 {$ENDIF}

 {Prepare the read operation}
 Command:=MDIO_RD or (Network.PhyAddr shl MDIO_PMD_SHIFT) or (Reg shl MDIO_REG_SHIFT);
 _UniMACMDIOWrite(Network,MDIO_CMD,Command);

 {Start MDIO transaction}
 UniMACMDIOStart(Network);

 {Wait for MII}
 Result:=GENETMIIWait(Network);
 if Result <> ERROR_SUCCESS then Exit;

 Command:=_UniMACMDIORead(Network,MDIO_CMD);

 if (Command and MDIO_READ_FAIL) <> 0 then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Value:=Command and $FFFF;

 Result:=ERROR_SUCCESS;

 //unimac_mdio_read
end;

{==============================================================================}

function UniMACMDIOWrite(Network:PGENETNetwork;Reg:LongWord;Value:Word):LongWord;
var
 Command:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: UMAC MDIO Write');
 {$ENDIF}

 {Prepare the write operation}
 Command:=MDIO_WR or (Network.PhyAddr shl MDIO_PMD_SHIFT) or (Reg shl MDIO_REG_SHIFT) or (Value and $FFFF);
 _UniMACMDIOWrite(Network,MDIO_CMD,Command);

 {Start MDIO transaction}
 UniMACMDIOStart(Network);

 {Wait for MII}
 Result:=GENETMIIWait(Network);

 //unimac_mdio_write
end;

{==============================================================================}

function UniMACMDIOReset(Network:PGENETNetwork):LongWord;
var
 Value:Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;

 {$IF DEFINED(GENET_DEBUG) or DEFINED(NETWORK_DEBUG)}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(@Network.Network,'GENET: UMAC MDIO Reset');
 {$ENDIF}

 {Dummy read of BMSR for PHY devices that fail to successfully
  be read from or written to for the first transaction}
 UniMACMDIORead(Network,MII_BMSR,Value);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 Result:=ERROR_SUCCESS;

 //unimac_mdio_reset
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
