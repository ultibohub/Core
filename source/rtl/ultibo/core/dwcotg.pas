{
USB Host Controller Driver for the Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller.

Copyright (C) 2020 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi - Model A/B/A+/B+
 Raspberry Pi - Model Zero/ZeroW
 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3/CM3+
 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  Embedded XINU - Copyright, Douglas Comer and Dennis Brylow
                   http://xinu.mscs.mu.edu/USB
                   http://xinu.mscs.mu.edu/Synopsys_DesignWare_High-Speed_USB_2.0_On-The-Go_Controller
                   https://github.com/xinu-os/xinu/blob/master/system/platforms/arm-rpi/usb_dwc_hcd.c
                   https://github.com/xinu-os/xinu/blob/master/system/platforms/arm-rpi/usb_dwc_regs.h
                   
  Linux DWCOTG driver - \drivers\usb\host\dwc_otg\*
  
  FreeBSD DWCOTG driver - \sys\dev\usb\controller\dwc_otg.c - Copyright (c) 2015 Daisuke Aoyama and others
  
  USPI (rsta2) - https://github.com/rsta2/uspi
  
  Circle (rsta2) - https://github.com/rsta2/circle/tree/master/lib/usb
   
References
==========


DWCOTG
======

This is a USB Host Controller Driver (HCD) that interfaces with the Synopsys
DesignWare Hi-Speed USB 2.0 On-The-Go Controller, henceforth abbreviated as
"DWC".  This is the USB Host Controller used on the BCM2835 SoC used on the
Raspberry Pi.
 
Please note that there is no publicly available official documentation for
this particular piece of hardware, and it uses its own custom host controller
interface rather than a standard one such as EHCI.  Therefore, this driver
was written on a best-effort basis using several sources to glean the
necessary hardware details, including the extremely complicated and difficult
to understand vendor provided Linux driver.
 
This file implements the Host Controller Driver Interface defined in TUSBHost.
Most importantly, it implements a function to power on and start the host
controller (HostStart) and a function to send and receive messages over the
USB (HostSubmit).
 
The DWC is controlled by reading and writing to/from memory-mapped registers.
The most important registers are the host channel registers.  On this
particular hardware, a "host channel", or simply "channel", is a set of
registers to which software can read and write to cause transactions to take
place on the USB.  A fixed number of host channels exist; on the Raspberry Pi
there are 8.  From the software's perspective, transactions using different
host channels can be executed at the same time.
 
Some of the host channel registers, as well as other registers, deal with
interrupts.  This driver makes heavy use of these and performs all USB
transfers in an interrupt-driven manner.  However, due to design flaws in
this hardware and in USB 2.0 itself, "interrupt" and "isochronous" transfers
still need to make use of software polling when checking for new data, even
though each individual transfer is itself interrupt-driven.  This means that,
for example, if your USB mouse specifies a polling rate of 100 times per
second, then it will, unfortunately, be polled 100 times per second in
software.  For more detail about how interrupts can be controlled on this
particular hardware, see the comment above DWCSetupInterrupts.

Another important concept is the idea of "packets", "transactions", and
"transfers".  A USB transfer, such as a single control message or bulk
request, may need to be split into multiple packets if it exceeds the
endpoint's maximum packet size.  Unfortunately, this has to be dealt with
explicitly in this code, as this hardware doesn't do it for us.  But at
least, from the viewpoint of this software, a "transaction" is essentially
the same as a "packet".
 
The "On-The-Go" in the name of this hardware means that it supports the USB
On-The-Go protocol, which allows it to act either as a host or a device.
However, we only are concerned with it acting as a host, which simplifies our
driver.
 
To simplify the USB core software, a useful design technique (as recommended
by the USB 2.0 standard and used in other implementations such as Linux's) is
to have the HCD present the root hub as a standard USB hub, even if the root
hub is integrated with the host controller and does not appear as a standard
hub at the hardware level.  This is the case with the DWC, and we implement
this design.  Therefore, some code in this file deals with faking requests
sent to the root hub.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DWCOTG;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,USB,SysUtils; 
            
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
            
{==============================================================================}
const
 {DWCOTG specific constants}
 DWC_NUM_CHANNELS = 8;                                      {Number of DWC host channels} //To Do //Move to GlobalConfig ? //Read from Registers ? //See max channels value in Linux driver
 
 DWC_SCHEDULER_THREAD_STACK_SIZE = SIZE_32K;                {Stack size of USB request scheduler thread}
 DWC_SCHEDULER_THREAD_PRIORITY = THREAD_PRIORITY_HIGHEST;   {Priority of USB request scheduler thread (should be fairly high so that USB transfers can be started as soon as possible)} 
                                                
 DWC_SCHEDULER_THREAD_NAME = 'DWC Transfer Scheduler';      {Name of USB request scheduler thread}
 
 DWC_COMPLETION_THREAD_STACK_SIZE = SIZE_32K;               {Stack size of USB request completion thread}
 DWC_COMPLETION_THREAD_PRIORITY = THREAD_PRIORITY_HIGHEST;  {Priority of USB request completion thread (should be fairly high so that USB transfers can be completed as soon as possible)} 
 
 DWC_COMPLETION_THREAD_NAME = 'DWC Transfer Completion';    {Name of USB request completion thread}
 
 DWC_RESUBMIT_THREAD_STACK_SIZE = SIZE_32K;                 {Stack size of USB request resubmit threads}
 DWC_RESUBMIT_THREAD_PRIORITY = THREAD_PRIORITY_CRITICAL;   {Priority of USB request resubmit threads (should be very high since these threads are used for the necessary software polling of interrupt endpoints, which are supposed to have guaranteed bandwidth)}
                                          
 DWC_RESUBMIT_THREAD_NAME = 'DWC Transfer Resubmit';        {Name of USB request resubmit threads}

 {DWC USB packet ID constants recognized by the DWC hardware}
 DWC_USB_PID_DATA0 = 0;
 DWC_USB_PID_DATA1 = 2;
 DWC_USB_PID_DATA2 = 1;
 DWC_USB_PID_SETUP = 3;
 DWC_USB_PID_MDATA = 3;
 
 {DWC FIFO values}
 DWC_RECEIVE_WORDS = 1024;           {Size of Rx FIFO in 4-byte words}
 DWC_TRANSMIT_WORDS = 1024;          {Size of Non-periodic Tx FIFO in 4-byte words}
 DWC_PERIODIC_TRANSMIT_WORDS = 1024; {Size of Periodic Tx FIFO in 4-byte words}
 
 {DWC Status codes}
 DWC_STATUS_SUCCESS             = 0;
 DWC_STATUS_STALLED             = 1;
 DWC_STATUS_FAILED              = 2;
 DWC_STATUS_TRANSFER_RESUBMIT   = 3; 
 DWC_STATUS_TRANSFER_RESTART    = 4;
 DWC_STATUS_TRANSACTION_RESTART = 5;
 DWC_STATUS_HOST_PORT_CHANGE    = 6;
 DWC_STATUS_ROOT_HUB_REQUEST    = 7;
 DWC_STATUS_INVALID             = 8;
 DWC_STATUS_CANCELLED           = 9;
 
 {DWC Complete Split}
 DWC_SPLIT_ERROR_RETRIES = 3;
 DWC_COMPLETE_SPLIT_RETRIES = 10;
 
 {DWC Register values}
 {TDWCRegisters: 0x0000 : OTG Control Register}
 DWC_OTG_CTRL_HST_SET_HNP_EN = (1 shl 10);
 
 {TDWCRegisters: 0x0008 : AHB Configuration Register}
 {Enable interrupts from the USB controller.  Disabled by default}
 DWC_AHB_INTERRUPT_ENABLE   = (1 shl 0);
 {Bits [4:1] of the AHB Configuration register were redefined by Broadcom for the BCM2835}
 {Max AXI burst length}
 BCM_DWC_AHB_AXI_BURST_MASK = (3 shl 1);
 {Wait for all outstanding AXI writes to complete before signalling (internally) that DMA is done}
 BCM_DWC_AHB_AXI_WAIT       = (1 shl 4);
 {Writing 1 to this bit in the AHB Configuration Register allows the USB controller to perform DMA (Disabled by default)}
 DWC_AHB_DMA_ENABLE         = (1 shl 5);
 {Unknown}
 DWC_AHB_MASTER_IDLE        = (1 shl 31);
 
 {TDWCRegisters: 0x000c : Core USB Configuration Register}
 DWC_USB_CFG_TOUTCAL_MASK            = (7 shl 0);
 DWC_USB_CFG_TOUTCAL_LIMIT           = (7 shl 0);
 DWC_USB_CFG_PHYIF16	             = (1 shl 3);
 DWC_USB_CFG_ULPI_UTMI_SEL	         = (1 shl 4);
 DWC_USB_CFG_FS_INTF                 = (1 shl 5);
 DWC_USB_CFG_PHY_SEL                 = (1 shl 6);
 DWC_USB_CFG_DDR_SEL                 = (1 shl 7);
 DWC_USB_CFG_SRP_CAPABLE 	         = (1 shl 8);
 DWC_USB_CFG_HNP_CAPABLE 	         = (1 shl 9);
 DWC_USB_CFG_USB_TRDTIM_MASK         = ($F shl 10);
 DWC_USB_CFG_RESERVED14              = (1 shl 14);
 DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL     = (1 shl 15);
 DWC_USB_CFG_OTG_UTMI_FS_SEL         = (1 shl 16);
 DWC_USB_CFG_ULPI_FSLS		         = (1 shl 17);
 DWC_USB_CFG_ULPI_AUTO_RES           = (1 shl 18);
 DWC_USB_CFG_ULPI_CLK_SUS_M          = (1 shl 19);
 DWC_USB_CFG_ULPI_EXT_VBUS_DRV       = (1 shl 20);
 DWC_USB_CFG_ULPI_INT_VBUS_INDICATOR = (1 shl 21);
 DWC_USB_CFG_TERM_SEL_DL_PULSE       = (1 shl 22);
 DWC_USB_CFG_INDICATOR_COMPLEMENT    = (1 shl 23);
 DWC_USB_CFG_INDICATOR_PASS_THROUGH  = (1 shl 24);
 DWC_USB_CFG_ULPI_INT_PROT_DIS       = (1 shl 25);
 DWC_USB_CFG_IC_USB_CAP              = (1 shl 26);
 DWC_USB_CFG_IC_TRAFFIC_PULL_REMOVE  = (1 shl 27);
 DWC_USB_CFG_TX_END_DELAY            = (1 shl 28);
 DWC_USB_CFG_FORCE_HOST_MODE         = (1 shl 29);
 DWC_USB_CFG_FORCE_DEV_MODE          = (1 shl 30);
 DWC_USB_CFG_RESERVED31              = (1 shl 31);

 {TDWCRegisters: 0x0010 : Core Reset Register}
 {Write 1 to this location in the Core Reset Register to start a soft reset.  This bit will then be cleared by the hardware when the reset is complete}
 DWC_SOFT_RESET      = (1 shl 0);
 
 {TDWCRegisters: 0x0014 : Core Interrupt Register}
 {This register contains the state of pending top-level DWC interrupts.  1 means interrupt pending while 0 means no interrupt pending}
 {Note that at least for port_intr and host_channel_intr, software must clear the interrupt somewhere else rather than by writing to this register}
 {Start of Frame.}
 DWC_CORE_INTERRUPTS_SOF_INTR          = (1 shl 3);  {Bit 3}
 {Host port status changed.  Software must examine the Host Port Control and Status Register to determine the current status of
  the host port and clear any flags in it that indicate a status change}
 DWC_CORE_INTERRUPTS_PORT_INTR         = (1 shl 24); {Bit 24}
 {Channel interrupt occurred.  Software must examine the Host All Channels Interrupt Register to determine which channel(s) have
  pending interrupts, then handle and clear the interrupts for these channels} 
 DWC_CORE_INTERRUPTS_HOST_CHANNEL_INTR = (1 shl 25); {Bit 25}
 {Disconnect interrupt indicated that a device has been disconnected from the root port}
 DWC_CORE_INTERRUPTS_DISCONNECT        = (1 shl 29); {Bit 29}
 
 {TDWCRegisters: 0x0040 : Vendor Id Register}
 DWC_VENDOR_ID_OTG2 = $4f542000;
 DWC_VENDOR_ID_OTG3 = $4f543000;
 
 DWC_VENDOR_ID_MASK = $fffff000;
 
 {TDWCRegisters: 0x0048 : Hardware Configuration 2}
 DWC_HWCFG2_OP_MODE_MASK                   = (7 shl 0);
 DWC_HWCFG2_OP_MODE_HNP_SRP_CAPABLE_OTG    = (0 shl 0);
 DWC_HWCFG2_OP_MODE_SRP_ONLY_CAPABLE_OTG   = (1 shl 0);
 DWC_HWCFG2_OP_MODE_NO_HNP_SRP_CAPABLE_OTG = (2 shl 0);
 DWC_HWCFG2_OP_MODE_SRP_CAPABLE_DEVICE     = (3 shl 0);
 DWC_HWCFG2_OP_MODE_NO_SRP_CAPABLE_DEVICE  = (4 shl 0);
 DWC_HWCFG2_OP_MODE_SRP_CAPABLE_HOST       = (5 shl 0);
 DWC_HWCFG2_OP_MODE_NO_SRP_CAPABLE_HOST    = (6 shl 0);
 
 DWC_HWCFG2_ARCHITECTURE_MASK              = (3 shl 3);
 DWC_HWCFG2_ARCHITECTURE_SLAVE_ONLY        = (0 shl 3);
 DWC_HWCFG2_ARCHITECTURE_EXT_DMA           = (1 shl 3);
 DWC_HWCFG2_ARCHITECTURE_INT_DMA           = (2 shl 3);
 
 DWC_HWCFG2_POINT2POINT                    = (1 shl 5);
 
 DWC_HWCFG2_HS_PHY_TYPE_MASK               = (3 shl 6);
 DWC_HWCFG2_HS_PHY_TYPE_NOT_SUPPORTED      = (0 shl 6);
 DWC_HWCFG2_HS_PHY_TYPE_UTMI		       = (1 shl 6);
 DWC_HWCFG2_HS_PHY_TYPE_ULPI		       = (2 shl 6);
 DWC_HWCFG2_HS_PHY_TYPE_UTMI_ULPI	       = (3 shl 6);
                                           
 DWC_HWCFG2_FS_PHY_TYPE_MASK               = (3 shl 8);
 DWC_HWCFG2_FS_PHY_TYPE_NOT_SUPPORTED      = (0 shl 8);
 DWC_HWCFG2_FS_PHY_TYPE_DEDICATED	       = (1 shl 8);
 DWC_HWCFG2_FS_PHY_TYPE_SHARED_UTMI	       = (2 shl 8);
 DWC_HWCFG2_FS_PHY_TYPE_SHARED_ULPI	       = (3 shl 8);
             
 DWC_HWCFG2_NUM_DEV_ENDPOINTS              = ($F shl 10);
 DWC_HWCFG2_NUM_HOST_CHANNELS              = ($F shl 14);
 
 DWC_HWCFG2_PERIODIC_ENDPOINT_SUPPORTED    = (1 shl 18);
 DWC_HWCFG2_DYNAMIC_FIFO                   = (1 shl 19);
 DWC_HWCFG2_MULTI_PROC_INT                 = (1 shl 20);
 DWC_HWCFG2_RESERVED21                     = (1 shl 21);
 DWC_HWCFG2_NON_PERIODIC_TX_QUEUE_DEPTH    = (3 shl 22);
 DWC_HWCFG2_HOST_PERIODIC_TX_QUEUE_DEPTH   = (3 shl 24);
 DWC_HWCFG2_DEV_TOKEN_QUEUE_DEPTH          = ($1F shl 26);
 DWC_HWCFG2_OTG_ENABLE_IC_USB              = (1 shl 31);
 
 {TDWCRegisters: 0x0400 : Host Configuration Register}
 DWC_HCFG_FS_LS_PHY_CLK_SEL_MASK      = (3 shl 0);   {FS/LS Phy Clock Select}
 DWC_HCFG_FS_LS_PHY_CLK_SEL_SHIFT     = 0;
 DWC_HCFG_FS_LS_PHY_CLK_SEL_30_60_MHZ = 0;
 DWC_HCFG_FS_LS_PHY_CLK_SEL_48_MHZ    = 1;
 DWC_HCFG_FS_LS_PHY_CLK_SEL_6_MHZ     = 2;
 
 DWC_HCFG_FS_LS_SUPPORT_ONLY          = (1 shl 2);   {FS/LS Only Support}
 
 DWC_HCFG_ENABLE_32KHZ                = (1 shl 7);   {Enable 32-KHz Suspend Mode}
 
 DWC_HCFG_RESUME_VALID_MASK           = ($FF shl 8); {Resume Validation Period}
 DWC_HCFG_RESUME_VALID_SHIFT          = 8; 
 
 DWC_HCFG_DESC_DMA                    = (1 shl 23);  {Enable Scatter/gather DMA in Host mode}
 
 DWC_HCFG_FRAME_LIST_ENTRIES_MASK     = (3 shl 24);  {Frame List Entries}
 DWC_HCFG_FRAME_LIST_ENTRIES_SHIFT    = 24;
 DWC_HCFG_FRAME_LIST_ENTRIES_8        = (0 shl 24);
 DWC_HCFG_FRAME_LIST_ENTRIES_8_SIZE   = 8;
 DWC_HCFG_FRAME_LIST_ENTRIES_16       = (1 shl 24);
 DWC_HCFG_FRAME_LIST_ENTRIES_16_SIZE  = 16;
 DWC_HCFG_FRAME_LIST_ENTRIES_32       = (3 shl 24);
 DWC_HCFG_FRAME_LIST_ENTRIES_32_SIZE  = 32;
 DWC_HCFG_FRAME_LIST_ENTRIES_64       = (3 shl 24);
 DWC_HCFG_FRAME_LIST_ENTRIES_64_SIZE  = 64;
 
 DWC_HCFG_PERSCHED_ENA                = (1 shl 26);  {Enable Periodic Scheduling} 
 DWC_HCFG_MODE_CH_TIM_EN              = (1 shl 31);
 
 {TDWCRegisters: 0x0404 : Host Frame Interval Register}
 DWC_HFIR_FRAME_INTERVAL_MASK    = ($FFFF shl 0);
 DWC_HFIR_FRAME_INT_RELOAD_CTL   = (1 shl 16);
 DWC_HFIR_RESERVED1              = ($FFFE  shl 17);
 
 {TDWCRegisters: 0x0408 : Host Frame Register}
 DWC_HFNUM_FRAME_NUMBER_MASK = $FFFF;
 
 {TDWCRegisters: 0x0440 : Host Port Control and Status Register}
 {This register provides the information needed to respond to status queries about the "host port", which is the port that is logically attached to the root hub}
 {When changing this register, software must read its value, then clear the enabled, connected_changed, enabled_changed, and overcurrent_changed members to avoid
  changing them, as those particular bits are cleared by writing 1}
 {1: a device is connected to this port. 0: no device is connected to this port. Changed by hardware only}
 DWC_HOST_PORT_CTRLSTATUS_CONNECTED           = (1 shl 0);    {Bit 0}
 {Set by hardware when connected bit changes.  Software can write 1 to acknowledge and clear.  The setting of this bit by hardware generates an interrupt that can be
  enabled by setting port_intr in the core_interrupt_mask register}
 DWC_HOST_PORT_CTRLSTATUS_CONNECTED_CHANGED   = (1 shl 1);    {Bit 1}
 {1: port is enabled. 0: port is disabled. Note: the host port is enabled by default after it is reset. Note: Writing 1 here appears to disable the port}
 DWC_HOST_PORT_CTRLSTATUS_ENABLED             = (1 shl 2);    {Bit 2}
 {Set by hardware when enabled bit changes.  Software can write 1 to acknowledge and clear.  The setting of this bit by hardware generates an interrupt that can be
  enabled by setting port_intr in the core_interrupt_mask register}
 DWC_HOST_PORT_CTRLSTATUS_ENABLED_CHANGED     = (1 shl 3);    {Bit 3} 
 {1: overcurrent condition active on this port 0: no overcurrent condition active on this port Changed by hardware only}
 DWC_HOST_PORT_CTRLSTATUS_OVERCURRENT         = (1 shl 4);    {Bit 4}
 {Set by hardware when the overcurrent bit changes.  The software can write 1 to acknowledge and clear.  The setting of this bit by hardware generates the interrupt that can be
  enabled by setting port_intr in the core_interrupt_mask register}
 DWC_HOST_PORT_CTRLSTATUS_OVERCURRENT_CHANGED = (1 shl 5);    {Bit 5}
 {Set by software to set resume signalling}
 DWC_HOST_PORT_CTRLSTATUS_RESUME              = (1 shl 6);    {Bit 6}
 {Set by software to suspend the port}
 DWC_HOST_PORT_CTRLSTATUS_SUSPENDED           = (1 shl 7);    {Bit 7}
 {Software can set this to start a reset on this port. Software must clear this after waiting 60 milliseconds for the reset to complete}
 DWC_HOST_PORT_CTRLSTATUS_RESET               = (1 shl 8);    {Bit 8}
 DWC_HOST_PORT_CTRLSTATUS_RESERVED            = (1 shl 9);    {Bit 9}
 {Current logic of data lines (10: logic of D+; 11: logic of D-). Changed by hardware only}
 DWC_HOST_PORT_CTRLSTATUS_LINE_STATUS         = (3 shl 10);   {Bits 10-11}
 {1: port is powered. 0: port is not powered. Software can change this bit to power on (1) or power off (0) the port}
 DWC_HOST_PORT_CTRLSTATUS_POWERED             = (1 shl 12);   {Bit 12}
 DWC_HOST_PORT_CTRLSTATUS_TEST_CONTROL        = ($0F shl 13); {Bits 13-16}
 {Speed of attached device (if any).  This should only be considered meaningful if the connected bit is set. 00: high speed; 01: full speed; 10: low speed Changed by hardware only}
 DWC_HOST_PORT_CTRLSTATUS_SPEED               = (3 shl 17);   {Bits 17-18}
 DWC_HOST_PORT_CTRLSTATUS_RESERVED2           = ($1FFF shl 19); {Bits 19-32}
 
 {TDWCHostChannel: 0x0000 : Channel Characteristics Register}
 {Contains various fields that must be set to prepare this channel for a transfer to or from a particular endpoint on a particular USB device}
 {This register only needs to be programmed one time when doing a transfer, regardless of how many packets it consists of, unless the channel is
  re-programmed for a different transfer or the transfer is moved to a different channel}
 {Maximum packet size the endpoint is capable of sending or receiving.  Must be programmed by software before starting the transfer}
 DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE    = ($7FF shl 0);  {Bits 0-10}
 {Endpoint number (low 4 bits of bEndpointAddress).  Must be programmed by software before starting the transfer}
 DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_NUMBER    = ($0F shl 11);  {Bits 11-14}
 {Endpoint direction (high bit of bEndpointAddress).  Must be programmed by software before starting the transfer}
 DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_DIRECTION = (1 shl 15);    {Bit  15}
 DWC_HOST_CHANNEL_CHARACTERISTICS_RESERVED           = (1 shl 16);    {Bit  16}
 {1 when the device being communicated with is attached at low speed; 0 otherwise.  Must be programmed by software before starting the transfer}
 DWC_HOST_CHANNEL_CHARACTERISTICS_LOWSPEED           = (1 shl 17);    {Bit  17}
 {Endpoint type (low 2 bits of bmAttributes).  Must be programmed by software before starting the transfer}
 DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_TYPE      = (3 shl 18);    {Bits 18-19}
 {Maximum number of transactions that can be executed per microframe as part of this transfer.  Normally 1, but should be set to 1 + (bits 11 and 12 of wMaxPacketSize)
  for high-speed interrupt and isochronous endpoints.  Must be programmed by software before starting the transfer}
 DWC_HOST_CHANNEL_CHARACTERISTICS_PACKETS_PER_FRAME  = (3 shl 20);    {Bits 20-21}
 {USB device address of the device on which the endpoint is located.  Must be programmed by software before starting the transfer}
 DWC_HOST_CHANNEL_CHARACTERISTICS_DEVICE_ADDRESS     = ($7F shl 22);  {Bits 22-28}
 {Just before enabling the channel (for all transactions), software needs to set this to the opposite of the low bit of the host_frame_number register.
  Otherwise the hardware will issue frame overrun errors on some transactions.  TODO: what exactly does this do?}
 DWC_HOST_CHANNEL_CHARACTERISTICS_ODD_FRAME          = (1 shl 29);    {Bit  29}
 {Software can set this to 1 to halt the channel. Not needed during normal operation as the channel halts automatically when a transaction completes or an error occurs}
 DWC_HOST_CHANNEL_CHARACTERISTICS_CHANNEL_DISABLE    = (1 shl 30);    {Bit  30}
 {Software can set this to 1 to enable the channel, thereby actually starting the transaction on the USB.  This must only be done after the characteristics, split_control,
  and transfer registers, and possibly other registers (depending on the transfer) have been programmed} 
 DWC_HOST_CHANNEL_CHARACTERISTICS_CHANNEL_ENABLE     = (1 shl 31);    {Bit  31}
 
 {TDWCHostChannel: 0x0004 : Channel Split Control Register}
 {This register is used to set up Split Transactions for communicating with low or full-speed devices attached to a high-speed hub.  When doing so, set split_enable to 1 and
  the other fields as documented. Otherwise, software must clear this register before starting the transfer}
 {Like the Channel Characteristics register, this register only needs to be programmed one time if the channel is enabled multiple times to send all the packets of a single transfer} 
 {0-based index of the port on the high-speed hub on which the low or full-speed device is attached.}
 DWC_HOST_CHANNEL_SPLIT_CONTROL_PORT_ADDRESS         = ($7F shl 0);   {Bits 0-6}
 {USB device address of the high-speed hub that acts as the Transaction Translator for this low or full-speed device. This is not necessarily the hub the device is physically
  connected to, since that could be a full-speed or low-speed hub.  Instead, software must walk up the USB device tree (towards the root hub) until a high-speed hub is found and
  use its device address here}
 DWC_HOST_CHANNEL_SPLIT_CONTROL_HUB_ADDRESS          = ($7F shl 7);   {Bits 7-13}
 {TODO: what exactly does this do?}
 DWC_HOST_CHANNEL_SPLIT_CONTROL_TRANSACTION_POSITION = (3 shl 14);    {Bits 14-15} 
 {0: Do a Start Split transaction 1: Do a Complete Split transaction. When split transactions are enabled, this must be programmed by software before enabling the channel.
  Note that you must begin with a Start Split transaction and alternate this bit for each transaction until the transfer is complete}
 DWC_HOST_CHANNEL_SPLIT_CONTROL_COMPLETE_SPLIT       = (1 shl 16);    {Bit  16}
 DWC_HOST_CHANNEL_SPLIT_CONTROL_RESERVED             = ($3FFF shl 17); {Bits 17-30}
 {Set to 1 to enable Split Transactions}
 DWC_HOST_CHANNEL_SPLIT_CONTROL_SPLIT_ENABLE         = (1 shl 31);    {Bit  31}
 
 {TDWCHostChannel: 0x0008 : Channel Interrupts Register}
 {Bitmask of status conditions that have occurred on this channel}
 {These bits can be used with or without "real" interrupts.  To have the CPU get a real interrupt when one of these bits gets set, set the appropriate bit in the interrupt_mask,
  and also ensure that interrupts from the channel are enabled in the host_channels_interrupt_mask register, channel interrupts overall are enabled in the core_interrupt_mask register,
  and interrupts from the DWC hardware overall are enabled in the ahb_configuration register and by any system-specific interrupt controller}
 {The requested USB transfer has successfully completed
  Exceptions and caveats:
  - When doing split transactions, this bit will be set after a Complete Split transaction has finished, even though the overall transfer may not actually be complete.
  - The transfer will only be complete up to the extent that data was programmed into the channel.  For example, control transfers have 3 phases, each of which must be programmed
    into the channel separately.  This flag will be set after each of these phases has successfully completed.
  - An OUT transfer is otherwise considered complete when exactly the requested number of bytes of data have been successfully transferred, while an IN transfer is otherwise
    considered complete when exactly the requested number of bytes of data have been successfully transferred or a shorter-than-expected packet was received}
 DWC_HOST_CHANNEL_INTERRUPTS_TRANSFER_COMPLETED       = (1 shl 0);   {Bit 0}
 {The channel has halted.  After this bit has been set, the channel sits idle and nothing else will happen until software takes action.
  Channels may halt for several reasons.  From our experience these cover all possible situations in which software needs to take action, so this is the only channel interrupt that
  actually needs to be enabled.  At least in DMA mode, the controller to some extent will act autonomously to complete transfers and only issue this interrupt when software needs
  to take action.
  Situations in which a channel will halt include but probably are not limited to:
  - The transfer has completed, thereby setting the transfer_completed flag as documented above.
  - A Start Split or Complete Split transaction has finished.                 
  - The hub sent a NYET packet when trying to execute a Complete Split transaction, thereby signalling that the Split transaction is not yet complete.
  - The device sent a NAK packet, thereby signalling it had no data to send at the time, when trying to execute an IN interrupt transfer.
  - One of several errors has occurred, such as an AHB error, data toggle error, tranasction error, stall condition, or frame overrun error}
 DWC_HOST_CHANNEL_INTERRUPTS_CHANNEL_HALTED           = (1 shl 1);    {Bit 1}
 {An error occurred on the ARM Advanced High-Performance Bus (AHB)}
 DWC_HOST_CHANNEL_INTERRUPTS_AHB_ERROR                = (1 shl 2);    {Bit 2}
 {The device issued a STALL handshake packet (endpoint is halted or control pipe request is not supported)}
 DWC_HOST_CHANNEL_INTERRUPTS_STALL_RESPONSE_RECEIVED  = (1 shl 3);    {Bit 3}
 {The device issued a NAK handshake packet (receiving device cannot accept data or transmitting device cannot send data)
  The channel will halt with this bit set when performing an IN transfer from an interrupt endpoint that has no data to send
  As this requires software intervention to restart the channel, this means that polling of interrupt endpoints (e.g. on hubs and HID devices) must be done in software, even if
  the actual transactions themselves are interrupt-driven}
 DWC_HOST_CHANNEL_INTERRUPTS_NAK_RESPONSE_RECEIVED    = (1 shl 4);    {Bit 4}
 {The device issued an ACK handshake packet (receiving device acknowledged error-free packet)}
 DWC_HOST_CHANNEL_INTERRUPTS_ACK_RESPONSE_RECEIVED    = (1 shl 5);    {Bit 5}
 {The device issued a NYET handshake packet}
 DWC_HOST_CHANNEL_INTERRUPTS_NYET_RESPONSE_RECEIVED   = (1 shl 6);    {Bit 6}
 {From our experience this seems to usually indicate that software programmed the channel incorrectly}
 DWC_HOST_CHANNEL_INTERRUPTS_TRANSACTION_ERROR        = (1 shl 7);    {Bit 7}
 {Unexpected bus activity occurred}
 DWC_HOST_CHANNEL_INTERRUPTS_BABBLE_ERROR             = (1 shl 8);    {Bit 8}
 {TODO}
 DWC_HOST_CHANNEL_INTERRUPTS_FRAME_OVERRUN            = (1 shl 9);    {Bit 9}
 {When issuing a series of DATA transactions to an endpoint, the correct DATA0 or DATA1 packet ID was not specified in the packet_id member of the transfer register}
 DWC_HOST_CHANNEL_INTERRUPTS_DATA_TOGGLE_ERROR        = (1 shl 10);   {Bit 10}
 DWC_HOST_CHANNEL_INTERRUPTS_BUFFER_NOT_AVAILABLE     = (1 shl 11);   {Bit 11}
 DWC_HOST_CHANNEL_INTERRUPTS_EXCESS_TRANSACTION_ERROR = (1 shl 12);   {Bit 12}
 DWC_HOST_CHANNEL_INTERRUPTS_FRAME_LIST_ROLLOVER      = (1 shl 13);   {Bit 13}
 DWC_HOST_CHANNEL_INTERRUPTS_RESERVED                 = ($3FFFF shl 14);   {Bits 14-31}
 
 {TDWCHostChannel: 0x000c : Channel Interrupts Mask Register}
 {This has the same format as the Channel Interrupts Register, but software uses this to enable (1) or disable (0) the corresponding interrupt.  Defaults to all 0's after a reset}
 DWC_HOST_CHANNEL_INTERRUPT_MASK_TRANSFER_COMPLETED       = DWC_HOST_CHANNEL_INTERRUPTS_TRANSFER_COMPLETED;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_CHANNEL_HALTED           = DWC_HOST_CHANNEL_INTERRUPTS_CHANNEL_HALTED;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_AHB_ERROR                = DWC_HOST_CHANNEL_INTERRUPTS_AHB_ERROR;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_STALL_RESPONSE_RECEIVED  = DWC_HOST_CHANNEL_INTERRUPTS_STALL_RESPONSE_RECEIVED;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_NAK_RESPONSE_RECEIVED    = DWC_HOST_CHANNEL_INTERRUPTS_NAK_RESPONSE_RECEIVED;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_ACK_RESPONSE_RECEIVED    = DWC_HOST_CHANNEL_INTERRUPTS_ACK_RESPONSE_RECEIVED;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_NYET_RESPONSE_RECEIVED   = DWC_HOST_CHANNEL_INTERRUPTS_NYET_RESPONSE_RECEIVED;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_TRANSACTION_ERROR        = DWC_HOST_CHANNEL_INTERRUPTS_TRANSACTION_ERROR;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_BABBLE_ERROR             = DWC_HOST_CHANNEL_INTERRUPTS_BABBLE_ERROR;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_FRAME_OVERRUN            = DWC_HOST_CHANNEL_INTERRUPTS_FRAME_OVERRUN;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_DATA_TOGGLE_ERROR        = DWC_HOST_CHANNEL_INTERRUPTS_DATA_TOGGLE_ERROR;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_BUFFER_NOT_AVAILABLE     = DWC_HOST_CHANNEL_INTERRUPTS_BUFFER_NOT_AVAILABLE;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_EXCESS_TRANSACTION_ERROR = DWC_HOST_CHANNEL_INTERRUPTS_EXCESS_TRANSACTION_ERROR;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_FRAME_LIST_ROLLOVER      = DWC_HOST_CHANNEL_INTERRUPTS_FRAME_LIST_ROLLOVER;
 DWC_HOST_CHANNEL_INTERRUPT_MASK_RESERVED                 = DWC_HOST_CHANNEL_INTERRUPTS_RESERVED;
 
 {TDWCHostChannel: 0x0010 : Channel Transfer Register}
 {Used to store additional information about the transfer.  This must be programmed before beginning the transfer}
 {Size of the data to send or receive, in bytes.  Software must program this before beginning the transfer.  This can be greater than the maximum packet length.
  For IN transfers, the hardware decrements this field for each packet received by the number of bytes received.  For split transactions, the decrement happens after the Complete Split
  rather than the Start Split.  Software can subtract this field from the original transfer size in order to determine the number of bytes received at any given point, including
  when the transfer has encountered an error or has completed with either the full size or a short size.
  For OUT transfers, the hardware does not update this field as expected.  It will not be decremented when data is transmitted, at least not in every case; hence, software
  cannot rely on its value to indicate how many bytes of data have been transmitted so far.  Instead, software must inspect the packet_count field and assume that all data was
  transmitted if packet_count is 0, or that the amount of data transmitted is equal to the endpoint's maximum packet size times [the original packet count minus packet_count] if
  packet_count is nonzero}
 DWC_HOST_CHANNEL_TRANSFER_SIZE         = ($7FFFF shl 0);   {Bits 0-18}
 {Number of packets left to transmit or maximum number of packets left to receive.  Software must program this before beginning the transfer.  The packet count is calculated as
  the size divided by the maximum packet size, rounded up to the nearest whole packet.  As a special case, if the transfer size is 0 bytes, the packet count must be set to 1.
  The hardware will decrement this register when a packet is successfully sent or received.  In the case of split transactions, this happens after the Complete Split rather
  than after the Start Split.  If the final received packet of an IN transfer is short, it is still counted}
 DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT = ($3FF shl 19);    {Bits 19-28}
 {High 2 bits of the Packet ID used in the USB protocol. When performing the SETUP phase of a control transfer, specify 0x3 here to generate the needed SETUP token.
  When performing the DATA phase of a control transfer, initially specify 0x2 here to begin the DATA packets with the needed DATA1 Packet ID.
  When performing the STATUS phase of a control transfer, specify 0x2 here to generate the needed DATA1 Packet ID.
  When starting a bulk, isochronous, or interrupt transfer, specify 0x0 here to generate the needed DATA0 Packet ID.
  In the case of a transfer consisting of multiple DATA packets, the hardware will update this field with the Packet ID to use for the next packet.  This field therefore only
  needs to be re-programmed if the transfer is moved to a different channel or the channel is re-used before the transfer is complete.  When doing so, software must save this
  field so that it can be re-programmed correctly}
 DWC_HOST_CHANNEL_TRANSFER_PACKET_ID    = ($03 shl 29);     {Bits 29-30}
 {Do PING protocol when 1 (See Section 8.5.1 of Universal Serial Bus Specification 2.0)}
 DWC_HOST_CHANNEL_TRANSFER_DO_PING      = (1 shl 31);       {Bit  31}
            
{==============================================================================}
type
 {DWCOTG specific types}
 
 {TDWCRegisters: 0x0500 : Array of host channels. Each host channel can be used to
  execute an independent USB transfer or transaction simultaneously.  A USB
  transfer may consist of multiple transactions, or packets.  To avoid
  having to re-program the channel, it may be useful to use one channel for
  all transactions of a transfer before allowing other transfers to be
  scheduled on it.}
 PDWCHostChannel = ^TDWCHostChannel;
 TDWCHostChannel = record
  {0x0000 : Channel Characteristics}
  {Contains various fields that must be set to prepare this channel for a transfer to or from a particular endpoint on a particular USB device}
  {This register only needs to be programmed one time when doing a transfer, regardless of how many packets it consists of, unless the channel is
   re-programmed for a different transfer or the transfer is moved to a different channel}
  Characteristics:LongWord;
  
  {0x0004 : Channel Split Control}
  {This register is used to set up Split Transactions for communicating with low or full-speed devices attached to a high-speed hub.  When doing so, set split_enable to 1 and
   the other fields as documented. Otherwise, software must clear this register before starting the transfer}
  {Like the Channel Characteristics register, this register only needs to be programmed one time if the channel is enabled multiple times to send all the packets of a single transfer} 
  SplitControl:LongWord;
  
  {0x0008 : Channel Interrupts}
  {Bitmask of status conditions that have occurred on this channel}
  {These bits can be used with or without "real" interrupts.  To have the CPU get a real interrupt when one of these bits gets set, set the appropriate bit in the interrupt_mask,
   and also ensure that interrupts from the channel are enabled in the host_channels_interrupt_mask register, channel interrupts overall are enabled in the core_interrupt_mask register,
   and interrupts from the DWC hardware overall are enabled in the ahb_configuration register and by any system-specific interrupt controller}
  Interrupts:LongWord;
  
  {0x000c : Channel Interrupts Mask}
  {This has the same format as the Channel Interrupts Register, but software uses this to enable (1) or disable (0) the corresponding interrupt.  Defaults to all 0's after a reset}
  InterruptMask:LongWord;
  
  {0x0010 : Channel Transfer}
  {Used to store additional information about the transfer.  This must be programmed before beginning the transfer}
  Transfer:LongWord;
  
  {0x0014 : Channel DMA Address}
  {Word-aligned address at which the hardware will read or write data using Direct Memory Access.  This must be programmed before beginning the transfer, unless the size of the data
   to send or receive is 0. The hardware will increment this address by the number of bytes successfully received or sent, which will correspond to the size decrease in transfer.size}
  {Note: DMA must be enabled in the AHB Configuration Register before this register can be used.  Otherwise, the hardware is considered to be in Slave mode and must be controlled a
   different way, which we do not use in our driver and do not attempt to document}
  {BCM2835-specific note:  Addresses written to this register must be bus addresses, not ARM physical addresses} 
  DMAAddress:LongWord;
  
  {0x0018 : Reserved}
  Reserved0x0018:LongWord;
  
  {0x001C : Reserved}
  Reserved0x001C:LongWord;
 end;
     
 {Layout of the registers of the DesignWare Hi-Speed USB 2.0 On-The-Go
  Controller.  There is no official documentation for these; however, the
  register locations (and to some extent the meanings) can be found in other
  code, such as the Linux driver for this hardware that Synopsys contributed.
 
  We do not explicitly define every bit in the registers because the majority
  are not used by our driver and would complicate this file.  For example, we
  do not attempt to document any features that are specific to suspend,
  hibernation, the OTG protocol, or to the core acting in device mode rather
  than host mode.
 
  The bits and fields we do use in our driver we have tried to completely
  document based on our understanding of what they do.  We cannot guarantee
  that all the information is correct, as we do not have access to any official
  documentation}
 PDWCRegisters = ^TDWCRegisters;
 TDWCRegisters = record
  {Core registers}
  {0x0000 : OTG Control and Status}
  OTGControl:LongWord;
   
  {0x0004 : OTG Interrupt}
  OTGInterrupt:LongWord;
   
  {0x0008 : Core AHB Configuration}
  {This register configures some of the interactions the DWC has with the rest of the system}
  AHBConfiguration:LongWord;
   
  {0x000c : Core USB Configuration}
  CoreUSBConfiguration:LongWord;
   
  {0x0010 : Core Reset}
  {Software can use this register to cause the DWC to reset itself}
  CoreReset:LongWord;
   
  {0x0014 : Core Interrupt}
  {This register contains the state of pending top-level DWC interrupts.  1 means interrupt pending while 0 means no interrupt pending}
  {Note that at least for port_intr and host_channel_intr, software must clear the interrupt somewhere else rather than by writing to this register}
  CoreInterrupts:LongWord;
   
  {0x0018 : Core Interrupt Mask}
  {This register has the same format as the Core Interrupt Register and configures whether the corresponding interrupt is enabled (1) or disabled (0).  Initial state after reset is all 0's}
  CoreInterruptMask:LongWord;
   
  {0x001c : Receive Status Queue Read (Read Only)}
  ReceiveStatus:LongWord;
   
  {0x0020 : Receive Status Queue Read & POP (Read Only)}
  ReceiveStatusPop:LongWord;
   
  {0x0024 : Receive FIFO Size}
  {This register contains the size of the Receive FIFO, in 4-byte words}
  {This register must be set by software before using the controller; see the note in the documentation for the hwcfg3 register about configuring the dynamic FIFOs}
  ReceiveFIFOSize:LongWord;
   
  {0x0028 : Non Periodic Transmit FIFO Size}
  {The low 16 bits of this register contain the offset of the Nonperiodic Transmit FIFO, in 4-byte words, from the start of the memory reserved by the controller for dynamic FIFOs.
   The high 16 bits of this register contain its size, in 4-byte words}
  {This register must be set by software before using the controller; see the note in the documentation for the hwcfg3 register about configuring the dynamic FIFOs} 
  NonPeriodicTransmitFIFOSize:LongWord;
   
  {0x002c : Non Periodic Transmit FIFO/Queue Status (Read Only)}
  NonPeriodicTransmitFIFOStatus:LongWord;
   
  {0x0030 : I2C Access}
  I2CControl:LongWord;
   
  {0x0034 : PHY Vendor Control}
  PHYVendorControl:LongWord;
   
  {0x0038 : General Purpose Input/Output}
  GPIO:LongWord;
   
  {0x003c : User ID}
  UserId:LongWord;
   
  {0x0040 : Vendor ID (Read Only)}
  VendorId:LongWord;
   
  {0x0044 : User HW Config1 (Read Only)}
  HWCfg1:LongWord;
   
  {0x0048 : User HW Config2 (Read Only)}
  HWCfg2:LongWord;
   
  {0x004c : User HW Config3 (Read Only)}
  {The high 16 bits of this read-only register contain the maximum total size, in words, of the dynamic FIFOs (Rx, Nonperiodic Tx, and Periodic Tx).
   Software must set up these three dynamic FIFOs in the rx_fifo_size, nonperiodic_tx_fifo_size, and host_periodic_tx_fifo_size registers such that
   their total size does not exceed this maximum total size and no FIFOs overlap}
  {Note: Software must explicitly configure the dynamic FIFOs even if the controller is operating in DMA mode, since the default values for the
   FIFO sizes and offsets may be invalid.  For example, in Broadcom's instantiation of this controller for the BCM2835, only 4080 words are
   available for dynamic FIFOs, but the dynamic FIFO sizes are set to 4096, 32, and 0, which are invalid as they add up to more than 4080.
   IF YOU DO NOT DO THIS YOU WILL GET SILENT MEMORY CORRUPTION} 
  {The low 16 bits of this register contain various flags that are not documented here as we don't use any in our driver} 
  HWCfg3:LongWord;
   
  {0x0050 : User HW Config4 (Read Only)}
  HWCfg4:LongWord;
   
  {0x0054 :  Core LPM Configuration}
  CoreLPMConfiguration:LongWord;
  
  {0x0058 : Global PowerDown}
  GlobalPowerDown:LongWord;
   
  {0x005c : Global DFIFO SW Config}
  GlobalFIFOConfig:LongWord;
   
  {0x0060 : ADP Control (Attach Detection Protocol)}
  ADPControl:LongWord;
   
  {0x0064 : Reserved }
  Reserved0x0064:array[1..39] of LongWord;
   
  {0x0100 : Host Periodic Transmit FIFO Size}
  {The low 16 bits of this register configure the offset of the Periodic Transmit FIFO, in 4-byte words, from the start of the memory reserved by the controller for dynamic FIFOs.
   The high 16 bits of this register configure its size, in 4-byte words}
  {This register should be set by software before using the controller; see the note in the documentation for the hwcfg3 register about configuring the dynamic FIFOs} 
  HostPeriodicTransmitFIFOSize:LongWord;
   
  {0x0104 : Device Periodic Transmit FIFO#n (if dedicated fifos are disabled, otherwise Device Transmit FIFO#n)}
  Reserved0x0104:array[1..191] of LongWord;
   
  {Host registers}
  {The registers beginning at this point are considered to be the "Host" registers. These are used for the "Host" half of the OTG (On-The-Go) protocol, which allows this hardware
   to act as either a USB host or a USB device.  This is the only half we are concerned with in this driver and we do not declare the corresponding Device registers}
   
  {0x0400 : Host Configuration}
  HostConfiguration:LongWord;
   
  {0x0404 : Host Frame Interval}
  HostFrameInterval:LongWord;
   
  {0x0408 : Host Frame Number / Frame Remaining}
  HostFrameNumber:LongWord;
   
  {0x040c : Reserved}
  Reserved0x040c:LongWord;
   
  {0x0410 : Host Periodic Transmit FIFO/ Queue Status}
  HostFIFOStatus:LongWord;
   
  {0x0414 : Host All Channels Interrupt}
  {This register contains a bit for each host channel that indicates whether an interrupt has occurred on that host channel.  You cannot clear the interrupts by writing
   to this register; use the channel-specific interrupt registers instead}
  HostChannelsInterrupt:LongWord;
   
  {0x0418 : Host All Channels Interrupt Mask}
  {Same format as the Host All Channels Interrupt Register, but a 1 in this register indicates that the corresponding host channel interrupt is enabled.  Software can
   change this register.  Defaults to all 0's after a reset}
  HostChannelsInterruptMask:LongWord;
   
  {0x041c : Host Frame List Base Address Register}
  HostFrameList:LongWord;
   
  {0x0420}
  Reserved0x0420:array[1..8] of LongWord;
   
  {0x0440 : Host Port Control and Status}
  {This register provides the information needed to respond to status queries about the "host port", which is the port that is logically attached to the root hub}
  {When changing this register, software must read its value, then clear the enabled, connected_changed, enabled_changed, and overcurrent_changed members to avoid
   changing them, as those particular bits are cleared by writing 1}
  HostPortControlStatus:LongWord;
   
  {0x0444}
  Reserved0x0444:array[1..47] of LongWord;

  {Host channel registers}
  {0x0500 : Array of Host Channels}
  {Each host channel can be used to execute an independent USB transfer or transaction simultaneously.  A USB transfer may consist of multiple transactions, or packets.
   To avoid having to re-program the channel, it may be useful to use one channel for all transactions of a transfer before allowing other transfers to be scheduled on it}
  HostChannels:array[0..DWC_NUM_CHANNELS - 1] of TDWCHostChannel;
  
  {0x0600}
  Reserved0x0600:array[1..((($800 - $500) - (DWC_NUM_CHANNELS * SizeOf(TDWCHostChannel))) div SizeOf(LongWord))] of LongWord;
   
  {Device registers}
  {0x0800}
  Reserved0x0800:array[1..(($E00 - $800) div SizeOf(LongWord))] of LongWord;
 
  {0x0e00 : Power and Clock Gating Control}
  PowerClockControl:LongWord;
 end;
 
 {DWC Root Hub Configuration}
 PDWCRootHubConfiguration = ^TDWCRootHubConfiguration;
 TDWCRootHubConfiguration = packed record
  ConfigurationDescriptor:TUSBConfigurationDescriptor;
  InterfaceDescriptor:TUSBInterfaceDescriptor;
  EndpointDescriptor:TUSBEndpointDescriptor;
 end;
 
 {DWC USB Transfer}
 PDWCUSBTransfer = ^TDWCUSBTransfer;
 TDWCUSBTransfer = record
  {Transfer Properties}
  //To Do //Add properties specific to DWCOTG
  {Split Transaction Properties}
  //To Do //Add Hub and Port properties for Split
 end;
 
 {DWC USB Host}
 PDWCUSBHost = ^TDWCUSBHost;
 TDWCUSBHost = record
  {USB Properties}
  Host:TUSBHost;
  {DWCOTG Properties}
  Lock:TSpinHandle;                                              {Host lock (Differs from lock in Host portion) (Spin lock due to use by interrupt handler)}
  Registers:PDWCRegisters;                                       {Memory mapped registers of the Synopsys DesignWare Hi-Speed USB 2.0 OTG Controller}
  SchedulerThread:TThreadHandle;                                 {Thread ID of USB request scheduler thread} 
  CompletionThread:TThreadHandle;                                {Thread ID of USB request completion thread} 
  SchedulerMailslot:TMailslotHandle;                             {USB requests that have been submitted to the Host but not yet started on a channel}
  {Channel Properties}
  DMABuffers:array[0..DWC_NUM_CHANNELS - 1] of Pointer;          {DMA buffers allocated for each hardware channel (4 byte aligned / 1 per channel)}
  ChannelRequests:array[0..DWC_NUM_CHANNELS - 1] of PUSBRequest; {Current USB request pending on each hardware channel (or nil of no request is pending)}
  ChannelFreeMask:LongWord;                                      {Bitmap of channel free (1) or used (0) status}
  ChannelFreeLock:TMutexHandle;                                  {Lock for access to ChannelFreeMask}
  ChannelFreeWait:TSemaphoreHandle;                              {Number of free channels in ChannelFreeMask}
  StartOfFrameMask:LongWord;                                     {Bitmap of channels waiting for Start of Frame}
  StartOfFrameLock:TSpinHandle;                                  {Lock for access to StartOfFrameMask (Spin lock due to use by interrupt handler)}
  LastFrameNumber:LongWord;                                      {Frame Number at the last Start Of Frame interrupt}
  {Root Hub Properties}
  HubStatus:PUSBHubStatus;                                       {Hub status for the root hub}
  PortStatus:PUSBPortStatus;                                     {Host port status for the root hub (Obtained from port interrupt due to status change)}
  DeviceStatus:PUSBDeviceStatus;                                 {Device status for the root hub}
  HubDescriptor:PUSBHubDescriptor;                               {Hub descriptor for the root hub}
  DeviceDescriptor:PUSBDeviceDescriptor;                         {Device descriptor for the root hub}
  HubConfiguration:PDWCRootHubConfiguration;                     {Configuration, Interface and Endpoint descriptors for the root hub}
  HubStringTable:array[0..2] of PUSBStringDescriptor;            {String table for Language, Product and Manufacturer strings for the root hub}
  HubProductString:PUSBStringDescriptor;                         {Product identifier string for the root hub}
  HubLanguageString:PUSBStringDescriptor;                        {Language identifier string for the root hub}
  HubManufacturerString:PUSBStringDescriptor;                    {Manufacturer identifier string for the root hub}
  HubStatusChange:PUSBRequest;                                   {Status change request to the root hub interrupt endpoint (nil if no request is pending)}
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                       {Number of interrupt requests received by the host controller}
  PortInterruptCount:LongWord;                                   {Number of port interrupts received by the host controller}
  ChannelInterruptCount:LongWord;                                {Number of channel interrupts received by the host controller}
  StartOfFrameInterruptCount:LongWord;                           {Number of start of frame interrupts received by the host controller} 
  DisconnectInterruptCount:LongWord;                             {Number of disconnect interrupts received by the host controller} 
  ResubmitCount:LongWord;                                        {Number of requests resubmitted for later retry}
  StartOfFrameCount:LongWord;                                    {Number of requests queued to wait for start of frame}
  DMABufferReadCount:LongWord;                                   {Number of IN requests that required a DMA buffer copy}
  DMABufferWriteCount:LongWord;                                  {Number of OUT requests that required a DMA buffer copy}
  
  NAKResponseCount:LongWord;                                      {Number of NAK responses received by the host controller}
  NYETResponseCount:LongWord;                                    {Number of NYET responses received by the host controller}
  StallResponseCount:LongWord;                                   {Number of Stall responses received by the host controller}
  RequestCancelCount:LongWord;                                   {Number of requests Cancelled by the host controller}
  
  AHBErrorCount:LongWord;                                        {Number of AHB errors received by the host controller}
  TransactionErrorCount:LongWord;                                {Number of transaction errors received by the host controller}
  BabbleErrorCount:LongWord;                                     {Number of babble errors received by the host controller}
  ExcessTransactionCount:LongWord;                               {Number of excess transaction errors received by the host controller}
  FrameListRolloverCount:LongWord;                               {Number of frame list rollover errors received by the host controller}
  DataToggleErrorCount:LongWord;                                 {Number of data toggle errors received by the host controller}
  FrameOverrunCount:LongWord;                                    {Number of frame overrun errors received by the host controller}
  
  ShortAttemptCount:LongWord;                                    {Number of short attempts where transfer size was less than the request size}
  StartSplitCount:LongWord;                                      {Number of start split transactions}
  CompleteSplitCount:LongWord;                                   {Number of complete split transactions}
  CompleteSplitRestartCount:LongWord;                            {Number of times a complete split transaction has been restarted at start split due to errors}
  
  NoChannelCompletedCount:LongWord;                              {Number of times the channel completed interrupt bit was not set when a request completed}
  NoPacketsTransferredCount:LongWord;                            {Number of times no packets were transferred but no error occured when a channel halted}
 end;
 
{==============================================================================}
{var}
 {DWCOTG specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure DWCInit;

{==============================================================================}
{DWCOTG Functions}
//Host methods
function DWCHostStart(Host:PUSBHost):LongWord;
function DWCHostStop(Host:PUSBHost):LongWord; 
function DWCHostReset(Host:PUSBHost):LongWord;    
function DWCHostResetEx(Host:PDWCUSBHost):LongWord;
function DWCHostSubmit(Host:PUSBHost;Request:PUSBRequest):LongWord;
function DWCHostCancel(Host:PUSBHost;Request:PUSBRequest):LongWord;

function DWCHostResubmit(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;

function DWCHostPowerOn(Host:PDWCUSBHost):LongWord;   
function DWCHostPowerOff(Host:PDWCUSBHost):LongWord;

function DWCHostCheck(Host:PDWCUSBHost):LongWord; 
function DWCHostInit(Host:PDWCUSBHost):LongWord; 
function DWCHostSetup(Host:PDWCUSBHost):LongWord; 
function DWCHostStartFrameInterrupt(Host:PDWCUSBHost;Enable:Boolean):LongWord; 

//Channel methods
function DWCAllocateChannel(Host:PDWCUSBHost):LongWord;
function DWCReleaseChannel(Host:PDWCUSBHost;Channel:LongWord):LongWord;

function DWCChannelStartTransfer(Host:PDWCUSBHost;Channel:LongWord;Request:PUSBRequest):LongWord;
function DWCChannelStartTransaction(Host:PDWCUSBHost;Channel:LongWord;Request:PUSBRequest):LongWord;

//Host Port methods
function DWCHostPortReset(Host:PDWCUSBHost):LongWord;

function DWCHostPortPowerOn(Host:PDWCUSBHost):LongWord; 

function DWCHostPortGetStatus(Host:PDWCUSBHost):LongWord;

function DWCHostPortSetFeature(Host:PDWCUSBHost;Feature:Word):LongWord;
function DWCHostPortClearFeature(Host:PDWCUSBHost;Feature:Word):LongWord;

procedure DWCHostPortStatusChanged(Host:PDWCUSBHost); 

//Root Hub Methods
function DWCRootHubRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;

function DWCRootHubControlRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;

function DWCRootHubClassRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;
function DWCRootHubStandardRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;

//Scheduler methods
function DWCSchedulerStart(Host:PDWCUSBHost):LongWord;

function DWCSchedulerExecute(Host:PDWCUSBHost):PtrInt;
function DWCCompletionExecute(Host:PDWCUSBHost):PtrInt;
function DWCResubmitExecute(Request:PUSBRequest):PtrInt;

//Interrupt methods
procedure DWCInterruptHandler(Host:PDWCUSBHost);
function DWCChannelInterrupt(Host:PDWCUSBHost;Channel:LongWord):LongWord;
function DWCChannelCompleted(Host:PDWCUSBHost;Request:PUSBRequest;Channel,Interrupts:LongWord):LongWord;

{==============================================================================}
{DWCOTG Helper Functions}
function DWCDivRoundUp(Number,Denominator:LongWord):LongWord;

function DWCCalculateFrameInterval(Host:PDWCUSBHost):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {DWCOTG specific variables}
 DWCInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function DWCHostSetupDMA(Host:PDWCUSBHost):LongWord; forward;
function DWCHostSetupInterrupts(Host:PDWCUSBHost):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DWCInit;
var
 Status:LongWord;
 WorkInt:LongWord;
 DWCHost:PDWCUSBHost;
begin
 {}
 {Check Initialized}
 if DWCInitialized then Exit;
 
 {Initialize DWCOTG_FIQ_ENABLED}
 if not(FIQ_ENABLED) then DWCOTG_FIQ_ENABLED:=False;
  
 {Check Environment Variables}
 {DWCOTG_FULL_SPEED_ONLY}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('DWCOTG_FULL_SPEED_ONLY'),0);
 if WorkInt <> 0 then DWCOTG_FULL_SPEED_ONLY:=True;

 {DWCOTG_FS_LS_LOW_POWER_CLOCK}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('DWCOTG_FS_LS_LOW_POWER_CLOCK'),0);
 if WorkInt <> 0 then DWCOTG_FS_LS_LOW_POWER_CLOCK:=True;

 {DWCOTG_LS_LOW_PWR_PHY_CLOCK_6MHZ}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('DWCOTG_LS_LOW_PWR_PHY_CLOCK_6MHZ'),0);
 if WorkInt <> 0 then DWCOTG_LS_LOW_PWR_PHY_CLOCK_6MHZ:=True;
  
 {Create USB Host}
 if DWCOTG_REGISTER_HOST then
  begin
   DWCHost:=PDWCUSBHost(USBHostCreateEx(SizeOf(TDWCUSBHost))); 
   if DWCHost <> nil then
    begin
     {Update USB Host}
     {Device}
     DWCHost.Host.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     DWCHost.Host.Device.DeviceType:=USBHOST_TYPE_DWCOTG;
     DWCHost.Host.Device.DeviceFlags:=USBHOST_FLAG_NONE;
     DWCHost.Host.Device.DeviceData:=nil;
     if DWCOTG_DMA_SHARED_MEMORY then DWCHost.Host.Device.DeviceFlags:=DWCHost.Host.Device.DeviceFlags or USBHOST_FLAG_SHARED;
     if DWCOTG_DMA_NOCACHE_MEMORY then DWCHost.Host.Device.DeviceFlags:=DWCHost.Host.Device.DeviceFlags or USBHOST_FLAG_NOCACHE;
     {USB}
     DWCHost.Host.HostStart:=DWCHostStart;
     DWCHost.Host.HostStop:=DWCHostStop;
     DWCHost.Host.HostReset:=DWCHostReset;
     DWCHost.Host.HostSubmit:=DWCHostSubmit;
     DWCHost.Host.HostCancel:=DWCHostCancel;
     DWCHost.Host.Alignment:=DWCOTG_DMA_ALIGNMENT;
     DWCHost.Host.Multiplier:=DWCOTG_DMA_MULTIPLIER;
     DWCHost.Host.MaxTransfer:=DWC_HOST_CHANNEL_TRANSFER_SIZE and not(3);
     {DWCOTG}
     DWCHost.Lock:=INVALID_HANDLE_VALUE;
     DWCHost.Registers:=nil;
     DWCHost.SchedulerThread:=INVALID_HANDLE_VALUE;
     DWCHost.CompletionThread:=INVALID_HANDLE_VALUE;
     DWCHost.SchedulerMailslot:=INVALID_HANDLE_VALUE;
     {Channel}
     DWCHost.ChannelFreeLock:=INVALID_HANDLE_VALUE;
     DWCHost.ChannelFreeWait:=INVALID_HANDLE_VALUE;
     DWCHost.StartOfFrameLock:=INVALID_HANDLE_VALUE;
     {Root Hub (Hub Status)}
     DWCHost.HubStatus:=GetMem(SizeOf(TUSBHubStatus));
     DWCHost.HubStatus.wHubStatus:=USB_HUB_STATUS_LOCAL_POWER;
     DWCHost.HubStatus.wHubChange:=0;
     {Root Hub (Port Status)}
     DWCHost.PortStatus:=GetMem(SizeOf(TUSBPortStatus));
     DWCHost.PortStatus.wPortStatus:=0;
     DWCHost.PortStatus.wPortChange:=0;
     {Root Hub (Device Status)}   
     DWCHost.DeviceStatus:=GetMem(SizeOf(TUSBDeviceStatus));
     DWCHost.DeviceStatus.wStatus:=USB_DEVICE_STATUS_SELF_POWERED;
     {Root Hub (Hub Descriptor)}
     DWCHost.HubDescriptor:=GetMem(SizeOf(TUSBHubDescriptor) + (1 * SizeOf(Byte))); {Allow for Data} {1 because TUSBHubDescriptor already includes 1 element in varData}
     DWCHost.HubDescriptor.bDescLength:=SizeOf(TUSBHubDescriptor) + (1 * SizeOf(Byte)); {bDescLength is the base size plus the length of the varData}
     DWCHost.HubDescriptor.bDescriptorType:=USB_DESCRIPTOR_TYPE_HUB;
     DWCHost.HubDescriptor.bNbrPorts:=1;
     DWCHost.HubDescriptor.wHubCharacteristics:=0;
     DWCHost.HubDescriptor.bPwrOn2PwrGood:=0;
     DWCHost.HubDescriptor.bHubContrCurrent:=0;
     PUSBHubDescriptorData(@DWCHost.HubDescriptor.varData)^[0]:=$00;  {DeviceRemovable}
     PUSBHubDescriptorData(@DWCHost.HubDescriptor.varData)^[1]:=$FF;  {PortPwrCtrlMask}
     {Root Hub (Device Descriptor)}
     DWCHost.DeviceDescriptor:=GetMem(SizeOf(TUSBDeviceDescriptor));
     DWCHost.DeviceDescriptor.bLength:=SizeOf(TUSBDeviceDescriptor);
     DWCHost.DeviceDescriptor.bDescriptorType:=USB_DESCRIPTOR_TYPE_DEVICE;
     DWCHost.DeviceDescriptor.bcdUSB:=$200;                           {USB version 2.0 (binary coded decimal)}
     DWCHost.DeviceDescriptor.bDeviceClass:=USB_CLASS_CODE_HUB;
     DWCHost.DeviceDescriptor.bDeviceSubClass:=0;
     DWCHost.DeviceDescriptor.bDeviceProtocol:=0;
     DWCHost.DeviceDescriptor.bMaxPacketSize0:=64;
     DWCHost.DeviceDescriptor.idVendor:=0;
     DWCHost.DeviceDescriptor.idProduct:=0;
     DWCHost.DeviceDescriptor.bcdDevice:=0;
     DWCHost.DeviceDescriptor.iManufacturer:=2;
     DWCHost.DeviceDescriptor.iProduct:=1;
     DWCHost.DeviceDescriptor.iSerialNumber:=0;
     DWCHost.DeviceDescriptor.bNumConfigurations:=1;
     {Root Hub (Configuration Descriptor)}
     DWCHost.HubConfiguration:=GetMem(SizeOf(TDWCRootHubConfiguration));
     DWCHost.HubConfiguration.ConfigurationDescriptor.bLength:=SizeOf(TUSBConfigurationDescriptor);
     DWCHost.HubConfiguration.ConfigurationDescriptor.bDescriptorType:=USB_DESCRIPTOR_TYPE_CONFIGURATION;
     DWCHost.HubConfiguration.ConfigurationDescriptor.wTotalLength:=SizeOf(TDWCRootHubConfiguration);
     DWCHost.HubConfiguration.ConfigurationDescriptor.bNumInterfaces:=1;
     DWCHost.HubConfiguration.ConfigurationDescriptor.bConfigurationValue:=1;
     DWCHost.HubConfiguration.ConfigurationDescriptor.iConfiguration:=0;
     DWCHost.HubConfiguration.ConfigurationDescriptor.bmAttributes:=USB_CONFIGURATION_ATTRIBUTE_RESERVED_HIGH or USB_CONFIGURATION_ATTRIBUTE_SELF_POWERED;
     DWCHost.HubConfiguration.ConfigurationDescriptor.bMaxPower:=0;
     {Root Hub (Interface Descriptor)}
     DWCHost.HubConfiguration.InterfaceDescriptor.bLength:=SizeOf(TUSBInterfaceDescriptor);
     DWCHost.HubConfiguration.InterfaceDescriptor.bDescriptorType:=USB_DESCRIPTOR_TYPE_INTERFACE;
     DWCHost.HubConfiguration.InterfaceDescriptor.bInterfaceNumber:=0;
     DWCHost.HubConfiguration.InterfaceDescriptor.bAlternateSetting:=0;
     DWCHost.HubConfiguration.InterfaceDescriptor.bNumEndpoints:=1;
     DWCHost.HubConfiguration.InterfaceDescriptor.bInterfaceClass:=USB_CLASS_CODE_HUB;
     DWCHost.HubConfiguration.InterfaceDescriptor.bInterfaceSubClass:=0;
     DWCHost.HubConfiguration.InterfaceDescriptor.bInterfaceProtocol:=0;
     DWCHost.HubConfiguration.InterfaceDescriptor.iInterface:=0;
     {Root Hub (Endpoint Descriptor)}
     DWCHost.HubConfiguration.EndpointDescriptor.bLength:=SizeOf(TUSBEndpointDescriptor);
     DWCHost.HubConfiguration.EndpointDescriptor.bDescriptorType:=USB_DESCRIPTOR_TYPE_ENDPOINT;
     DWCHost.HubConfiguration.EndpointDescriptor.bEndpointAddress:=1 or (USB_DIRECTION_IN shl 7);
     DWCHost.HubConfiguration.EndpointDescriptor.bmAttributes:=USB_TRANSFER_TYPE_INTERRUPT;
     DWCHost.HubConfiguration.EndpointDescriptor.wMaxPacketSize:=64;
     DWCHost.HubConfiguration.EndpointDescriptor.bInterval:=$FF;
     {Root Hub (Product String)}
     DWCHost.HubProductString:=GetMem(SizeOf(TUSBStringDescriptor) + (15 * SizeOf(Word))); {Allow for Product Identifier} {15 because TUSBStringDescriptor already includes 1 element in bString}
     DWCHost.HubProductString.bLength:=SizeOf(TUSBStringDescriptor) + (15 * SizeOf(Word));
     DWCHost.HubProductString.bDescriptorType:=USB_DESCRIPTOR_TYPE_STRING;
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[0]:=Ord('U');          {UTF-16LE string}
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[1]:=Ord('S');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[2]:=Ord('B');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[3]:=Ord(' ');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[4]:=Ord('2');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[5]:=Ord('.');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[6]:=Ord('0');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[7]:=Ord(' ');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[8]:=Ord('R');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[9]:=Ord('o');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[10]:=Ord('o');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[11]:=Ord('t');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[12]:=Ord(' ');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[13]:=Ord('H');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[14]:=Ord('u');
     PUSBStringDescriptorString(@DWCHost.HubProductString.bString)^[15]:=Ord('b');
     {Root Hub (Language String)}
     DWCHost.HubLanguageString:=GetMem(SizeOf(TUSBStringDescriptor) + (0 * SizeOf(Word))); {Allow for 16 bit Language Identifier} {0 because TUSBStringDescriptor already includes 1 element in bString}
     DWCHost.HubLanguageString.bLength:=SizeOf(TUSBStringDescriptor) + (0 * SizeOf(Word));
     DWCHost.HubLanguageString.bDescriptorType:=USB_DESCRIPTOR_TYPE_STRING;
     PWord(@DWCHost.HubLanguageString.bString)^:=USB_LANGID_US_ENGLISH;
     {Root Hub (Manufacturer String)}
     DWCHost.HubManufacturerString:=GetMem(SizeOf(TUSBStringDescriptor) + (5 * SizeOf(Word))); {Allow for Manufacturer Identifier} {5 because TUSBStringDescriptor already includes 1 element in bString}
     DWCHost.HubManufacturerString.bLength:=SizeOf(TUSBStringDescriptor) + (5 * SizeOf(Word));
     DWCHost.HubManufacturerString.bDescriptorType:=USB_DESCRIPTOR_TYPE_STRING;
     PUSBStringDescriptorString(@DWCHost.HubManufacturerString.bString)^[0]:=Ord('U');          {UTF-16LE string}
     PUSBStringDescriptorString(@DWCHost.HubManufacturerString.bString)^[1]:=Ord('l');
     PUSBStringDescriptorString(@DWCHost.HubManufacturerString.bString)^[2]:=Ord('t');
     PUSBStringDescriptorString(@DWCHost.HubManufacturerString.bString)^[3]:=Ord('i');
     PUSBStringDescriptorString(@DWCHost.HubManufacturerString.bString)^[4]:=Ord('b');
     PUSBStringDescriptorString(@DWCHost.HubManufacturerString.bString)^[5]:=Ord('o');
     {Root Hub (String Table)}
     DWCHost.HubStringTable[0]:=DWCHost.HubLanguageString;
     DWCHost.HubStringTable[1]:=DWCHost.HubProductString;
     DWCHost.HubStringTable[2]:=DWCHost.HubManufacturerString;
     
     {Register USB Host}
     Status:=USBHostRegister(@DWCHost.Host); 
     if Status <> ERROR_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to register new USB host: ' + ErrorToString(Status));
      end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to create new USB host');
    end;
  end;
  
 DWCInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{DWCOTG Functions}
function DWCHostStart(Host:PUSBHost):LongWord;
{Implementation of USBHostStart for the DesignWare Hi-Speed USB 2.0 On-The-Go Controller.

 See usb.pas for the documentation of this interface of the Host Controller Driver}
var
 Count:LongWord;
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Starting USB Host');
 {$ENDIF}
 
 {Check Registers}
 if DWCOTG_REGS_BASE = 0 then
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
  
 {Setup Registers}
 PDWCUSBHost(Host).Registers:=PDWCRegisters(DWCOTG_REGS_BASE);
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: DWCOTG_REGS_BASE = ' + IntToHex(DWCOTG_REGS_BASE,8));
 {$ENDIF}
 
 {Create Host Lock}
 PDWCUSBHost(Host).Lock:=SpinCreate;
 if PDWCUSBHost(Host).Lock = INVALID_HANDLE_VALUE then
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;

 {Allocate DMA Buffers} //To Do //Critical //Move lower ? //Free on failure ?
 for Count:=0 to DWC_NUM_CHANNELS - 1 do
  begin
   if DWCOTG_DMA_SHARED_MEMORY then
    begin
     PDWCUSBHost(Host).DMABuffers[Count]:=GetSharedAlignedMem(RoundUp(USB_MAX_PACKET_SIZE,DWCOTG_DMA_MULTIPLIER),DWCOTG_DMA_ALIGNMENT);
    end
   else if DWCOTG_DMA_NOCACHE_MEMORY then
    begin
     PDWCUSBHost(Host).DMABuffers[Count]:=GetNoCacheAlignedMem(RoundUp(USB_MAX_PACKET_SIZE,DWCOTG_DMA_MULTIPLIER),DWCOTG_DMA_ALIGNMENT);
    end
   else
    begin
     PDWCUSBHost(Host).DMABuffers[Count]:=GetAlignedMem(RoundUp(USB_MAX_PACKET_SIZE,DWCOTG_DMA_MULTIPLIER),DWCOTG_DMA_ALIGNMENT);
    end;
   if not(DWCOTG_DMA_CACHE_COHERENT) then
    begin
     CleanDataCacheRange(PtrUInt(PDWCUSBHost(Host).DMABuffers[Count]),RoundUp(USB_MAX_PACKET_SIZE,DWCOTG_DMA_MULTIPLIER));
    end;
  end;

 {Check Host}
 Status:=DWCHostCheck(PDWCUSBHost(Host));
 if Status <> USB_STATUS_SUCCESS then
  begin
   {Destroy Host Lock}
   SpinDestroy(PDWCUSBHost(Host).Lock);
   
   Result:=Status;
   Exit;
  end;
 
 {Power on Host} 
 Status:=DWCHostPowerOn(PDWCUSBHost(Host));
 if Status <> USB_STATUS_SUCCESS then
  begin
   {Destroy Host Lock}
   SpinDestroy(PDWCUSBHost(Host).Lock);
   
   Result:=Status;
   Exit;
  end;

 {Init Host}
 Status:=DWCHostInit(PDWCUSBHost(Host));
 if Status <> USB_STATUS_SUCCESS then
  begin
   {Power off Host}
   DWCHostPowerOff(PDWCUSBHost(Host));
   
   {Destroy Host Lock}
   SpinDestroy(PDWCUSBHost(Host).Lock);
   
   Result:=Status;
   Exit;
  end;

 {Setup Host}
 Status:=DWCHostSetup(PDWCUSBHost(Host));
 if Status <> USB_STATUS_SUCCESS then
  begin
   {Power off Host}
   DWCHostPowerOff(PDWCUSBHost(Host));
   
   {Destroy Host Lock}
   SpinDestroy(PDWCUSBHost(Host).Lock);
   
   Result:=Status;
   Exit;
  end;

 {Start Scheduler}
 Status:=DWCSchedulerStart(PDWCUSBHost(Host));
 if Status <> USB_STATUS_SUCCESS then
  begin
   {Power off Host}
   DWCHostPowerOff(PDWCUSBHost(Host));

   {Destroy Host Lock}
   SpinDestroy(PDWCUSBHost(Host).Lock);
  end;
 
 {Return Result}
 Result:=Status;
end;

{==============================================================================}

function DWCHostStop(Host:PUSBHost):LongWord; 
{Implementation of USBHostStop for the DesignWare Hi-Speed USB 2.0 On-The-Go Controller. 
 
 See usb.pas for the documentation of this interface of the Host Controller Driver}
var
 Count:LongWord;
 Status:LongWord; 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Stopping USB Host');
 {$ENDIF}
 
 {Release the IRQ/FIQ}
 if DWCOTG_FIQ_ENABLED then
  begin
   ReleaseFIQ(FIQ_ROUTING,DWCOTG_IRQ,TInterruptHandler(DWCInterruptHandler),Host);
  end
 else
  begin
   ReleaseIRQ(IRQ_ROUTING,DWCOTG_IRQ,TInterruptHandler(DWCInterruptHandler),Host);
  end;

 {Stop Completion Thread}
 if PDWCUSBHost(Host).CompletionThread <> INVALID_HANDLE_VALUE then
  begin
   KillThread(PDWCUSBHost(Host).CompletionThread); //To Do //Signal thread to terminate, don't kill
  end; 
  
 {Stop Scheduler Thread}
 if PDWCUSBHost(Host).SchedulerThread <> INVALID_HANDLE_VALUE then
  begin
   KillThread(PDWCUSBHost(Host).SchedulerThread); //To Do //Signal thread to terminate, don't kill
  end; 
 
 {Free Scheduler Mailslot}
 if PDWCUSBHost(Host).SchedulerMailslot <> INVALID_HANDLE_VALUE then
  begin
   MailslotDestroy(PDWCUSBHost(Host).SchedulerMailslot);
  end; 
 
 {Free Channel Free Semaphore}
 if PDWCUSBHost(Host).ChannelFreeWait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(PDWCUSBHost(Host).ChannelFreeWait);
  end; 
 
 {Free Channel Free Lock}
 if PDWCUSBHost(Host).ChannelFreeLock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(PDWCUSBHost(Host).ChannelFreeLock);
  end; 

 {Free Start of Frame Lock}
 if PDWCUSBHost(Host).StartOfFrameLock <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(PDWCUSBHost(Host).StartOfFrameLock);
  end; 
 
 {Power off Host}
 Status:=DWCHostPowerOff(PDWCUSBHost(Host));
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
  end;
 
 {Release DMA Buffers}
 for Count:=0 to DWC_NUM_CHANNELS - 1 do
  begin
   FreeMem(PDWCUSBHost(Host).DMABuffers[Count]);
  end;
 
 {Free Host Lock}
 if PDWCUSBHost(Host).Lock <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(PDWCUSBHost(Host).Lock);
  end; 
 
 {Update Host}
 {DWCOTG}
 PDWCUSBHost(Host).Lock:=INVALID_HANDLE_VALUE;
 PDWCUSBHost(Host).Registers:=nil;
 PDWCUSBHost(Host).SchedulerThread:=INVALID_HANDLE_VALUE;
 PDWCUSBHost(Host).CompletionThread:=INVALID_HANDLE_VALUE;
 PDWCUSBHost(Host).SchedulerMailslot:=INVALID_HANDLE_VALUE;
 PDWCUSBHost(Host).ChannelFreeLock:=INVALID_HANDLE_VALUE;
 PDWCUSBHost(Host).ChannelFreeWait:=INVALID_HANDLE_VALUE;
 //To Do 
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCHostReset(Host:PUSBHost):LongWord; 
{Performs a software reset of the DWC OTG Controller}
var
 ResultCode:LongWord; 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Resetting USB Host controller');
 {$ENDIF}
 
 {Acquire the Lock}
 if DWCOTG_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(PDWCUSBHost(Host).Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(PDWCUSBHost(Host).Lock);
  end;  
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Reset Host}
    Result:=DWCHostResetEx(PDWCUSBHost(Host));
   finally
    {Release the Lock}
    if DWCOTG_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(PDWCUSBHost(Host).Lock);
     end
    else
     begin
      SpinUnlockIRQ(PDWCUSBHost(Host).Lock);
     end;     
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end;

{==============================================================================}

function DWCHostResetEx(Host:PDWCUSBHost):LongWord;
{Performs a software reset of the DWC OTG Controller}

{Note: Caller must hold the Host lock} 
var
 Count:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Resetting USB Host controller');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Enable Soft Reset}
 Host.Registers.CoreReset:=DWC_SOFT_RESET;
 
 {Wait for Reset}
 Count:=0;
 while (Host.Registers.CoreReset and DWC_SOFT_RESET) = DWC_SOFT_RESET do
  begin
   Inc(Count);
   
   {Wait until Complete}
   if Count > 10000 then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to reset host controller');
     
     Result:=USB_STATUS_TIMEOUT;
     Exit;
    end;
   
   MicrosecondDelay(1);   
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
  
 {Wait for 3 PHY Clocks}  
 MillisecondDelay(100);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCHostSubmit(Host:PUSBHost;Request:PUSBRequest):LongWord;
{Implementation of USBHostSubmit for the DesignWare Hi-Speed USB 2.0 On-The-Go Controller.

 See usb.pas for the documentation of this interface of the Host Controller Driver.
 
 This Host Controller Driver implements this interface asynchronously, as
 intended. Furthermore, it uses a simplistic scheduling algorithm where it
 places requests into a single queue and executes them in the order
 they were submitted.  Transfers that need to be retried, including periodic
 transfers that receive a NAK reply and split transactions that receive a NYET
 reply when doing the Complete Split transaction, are scheduled to be retried
 at an appropriate time by separate code that shortcuts the main queue when
 the timer expires}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the device lock}
var
 Status:LongWord; 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Submitting request (Request=' + PtrToHex(Request) + ')');
 {$ENDIF}
 
 {Send Request}
 Status:=MailslotSend(PDWCUSBHost(Host).SchedulerMailslot,PtrInt(Request));
 if Status <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'DWCOTG: Failed to submit request');
   
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCHostCancel(Host:PUSBHost;Request:PUSBRequest):LongWord;
{Implementation of USBHostCancel for the DesignWare Hi-Speed USB 2.0 On-The-Go Controller.

 See usb.pas for the documentation of this interface of the Host Controller Driver}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure} 

{Note: Caller must hold the device lock}
var
 Count:LongWord;
 Message:TMessage;
 Channel:LongWord;
 ResultCode:LongWord;
 Characteristics:LongWord;
 HostChannel:PDWCHostChannel;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Cancelling request (Request=' + PtrToHex(Request) + ')');
 {$ENDIF}
 
 {Acquire the Channel Lock (Must be before Host Lock)}
 if MutexLock(PDWCUSBHost(Host).ChannelFreeLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Host Lock}
    if DWCOTG_FIQ_ENABLED then
     begin
      ResultCode:=SpinLockIRQFIQ(PDWCUSBHost(Host).Lock);
     end
    else
     begin
      ResultCode:=SpinLockIRQ(PDWCUSBHost(Host).Lock);
     end;  
    if ResultCode = ERROR_SUCCESS then
     begin
      try
       {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG:  (Status=' + USBStatusToString(Request.Status) + ')');
       {$ENDIF}
       
       {Check Request}
       if Request.Status = USB_STATUS_NOT_PROCESSED then
        begin
         {Update Request}
         Request.Status:=USB_STATUS_CANCELLED;
         
         {Scheduler will complete cancel}
         
         {Return Result}
         Result:=USB_STATUS_SUCCESS;
        end
       else if Request.Status = USB_STATUS_NOT_COMPLETED then
        begin
         {Update Request}
         Request.Status:=USB_STATUS_CANCELLED;

         {Find Channel}
         Channel:=LongWord(INVALID_HANDLE_VALUE);
         for Count:=0 to DWC_NUM_CHANNELS - 1 do
          begin
           if PDWCUSBHost(Host).ChannelRequests[Count] = Request then
            begin
             Channel:=Count;
             Break;
            end;
          end;
         
         {Check Channel}
         if Channel <> LongWord(INVALID_HANDLE_VALUE) then
          begin
           {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
           if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: (Channel=' + IntToStr(Channel) + ')');
           {$ENDIF}
           
           {Get Host Channel}
           HostChannel:=@PDWCUSBHost(Host).Registers.HostChannels[Channel];
           
           {Memory Barrier}
           DataMemoryBarrier; {Before the First Write}
           
           {Get Characteristics}
           Characteristics:=HostChannel.Characteristics;
           
           {Check Enabled}
           if (Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_CHANNEL_ENABLE) <> 0 then
            begin
             {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
             if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Channel is Enabled');
             {$ENDIF}
             
             {Disable Channel}
             Characteristics:=(Characteristics or DWC_HOST_CHANNEL_CHARACTERISTICS_CHANNEL_DISABLE);
             
             {Set Characteristics}
             HostChannel.Characteristics:=Characteristics;
            end
           else
            begin
             {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
             if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Channel is not Enabled');
             {$ENDIF}
            end;
           
           {Memory Barrier}
           DataMemoryBarrier; {After the Last Read} 
           
           {Interrupt handler will complete cancel}
          end
         else
          begin
           {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
           if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: (No Channel)'); 
           {$ENDIF}
           
           {Interrupt handler or Resubmit thread will complete cancel}
          end;          
         
         {Return Result}
         Result:=USB_STATUS_SUCCESS;
        end
       else
        begin
         {Return Result}
         Result:=USB_STATUS_OPERATION_FAILED;
        end;     
      finally
       {Release the Host Lock}
       if DWCOTG_FIQ_ENABLED then
        begin
         SpinUnlockIRQFIQ(PDWCUSBHost(Host).Lock);
        end
       else
        begin
         SpinUnlockIRQ(PDWCUSBHost(Host).Lock);
        end;     
       end;   
     end
    else
     begin
      Result:=USB_STATUS_OPERATION_FAILED;
     end;  
   finally
    {Release the Channel Lock}
    MutexUnlock(PDWCUSBHost(Host).ChannelFreeLock);
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end;

{==============================================================================}

function DWCHostResubmit(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;
{Called when a USB transfer needs to be retried at a later time due to no data
 being available from the endpoint.
 For periodic transfers (e.g. polling an interrupt endpoint), the exact time
 at which the transfer must be retried is specified by the bInterval member of
 the endpoint descriptor.  For low and full-speed devices, bInterval specifies
 the number of millisconds to wait before the next poll, while for high-speed
 devices it specifies the exponent (plus one) of a power-of-two number of
 milliseconds to wait before the next poll.
 To actually implement delaying a transfer, we associate each transfer with a
 thread created on-demand.  Each such thread simply enters a loop where it
 calls sleep() for the appropriate number of milliseconds, then retries the
 transfer.  A semaphore is needed to make the thread do nothing until the
 request has actually been resubmitted.
 Note: this code gets used to scheduling polling of IN interrupt endpoints,
 including those on hubs and HID devices.  Thus, polling of these devices for
 status changes (in the case of hubs) or new input (in the case of HID
 devices) is done in software.  This wakes up the CPU a lot and wastes time
 and energy.  But with USB 2.0, there is no way around this, other than by
 suspending the USB device which we don't support}
{Request: USB transfer to resubmit}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Resubmitting request');
 {$ENDIF}
 
 {Update Statistics}
 Inc(Host.ResubmitCount);
 
 {Check Resubmit Semaphore}
 if Request.ResubmitSemaphore = INVALID_HANDLE_VALUE then
  begin
   {Create Resubmit Semaphore}
   Request.ResubmitSemaphore:=SemaphoreCreate(0);
   if Request.ResubmitSemaphore = INVALID_HANDLE_VALUE then
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'DWCOTG: Failed to create resubmit semaphore');
     
     Result:=USB_STATUS_OPERATION_FAILED;
     Exit;
    end;
  end;
 
 {Check Resubmit Thread}
 if Request.ResubmitThread = INVALID_HANDLE_VALUE then 
  begin
   {Create Resubmit Thread}
   Request.ResubmitThread:=BeginThread(TThreadStart(DWCResubmitExecute),Request,Request.ResubmitThread,DWC_RESUBMIT_THREAD_STACK_SIZE);
   if Request.ResubmitThread = INVALID_HANDLE_VALUE then 
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'DWCOTG: Failed to create resubmit thread');
     
     Result:=USB_STATUS_OPERATION_FAILED;
     Exit;
    end
   else
    begin
     ThreadSetPriority(Request.ResubmitThread,DWC_RESUBMIT_THREAD_PRIORITY);
     ThreadSetName(Request.ResubmitThread,DWC_RESUBMIT_THREAD_NAME + ' (' + DeviceGetName(@Request.Device.Device) + ')');
    end;    
  end;
 
 {Signal Resubmit Semaphore}
 SemaphoreSignal(Request.ResubmitSemaphore);
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCHostPowerOn(Host:PDWCUSBHost):LongWord;   
{Power on the DWCOTG Host controller}
{Host: The DWCOTG host to power on}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 if USB_LOG_ENABLED then USBLogInfo(nil,'DWCOTG: Powering on Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller');

 {Acquire the Lock}
 if SpinLock(Host.Lock) = ERROR_SUCCESS then {Not IRQ/FIQ during startup}
  begin
   try
    {Power on Host}
    Status:=PowerOn(DWCOTG_POWER_ID);
    if Status <> ERROR_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to power on Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller');
      
      Result:=USB_STATUS_HARDWARE_ERROR;
      Exit;
     end;

    {Wait for 3 PHY Clocks}  
    {MillisecondDelay(100);}

    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    SpinUnlock(Host.Lock); {Not IRQ/FIQ}
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end;

{==============================================================================}

function DWCHostPowerOff(Host:PDWCUSBHost):LongWord;
{Powers off the DWCOTG Host controller}
{Host: The DWCOTG host to power off}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 if USB_LOG_ENABLED then USBLogInfo(nil,'DWCOTG: Powering off Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller');
 
 {Acquire the Lock}
 if SpinLock(Host.Lock) = ERROR_SUCCESS then {Not IRQ/FIQ during shutdown}
  begin
   try
    {Power off Host}
    Status:=PowerOff(DWCOTG_POWER_ID); 
    if Status <> ERROR_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to power off Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller');
      
      Result:=USB_STATUS_HARDWARE_ERROR;
      Exit;
     end;
 
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    SpinUnlock(Host.Lock); {Not IRQ/FIQ}
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end;

{==============================================================================}

function DWCHostCheck(Host:PDWCUSBHost):LongWord; 
{Check the DWC OTG USB Host Controller to confirm it is valid}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 VendorId:LongWord;
 ResultCode:LongWord; 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Checking USB Host');
 {$ENDIF}

 {Acquire the Lock}
 if DWCOTG_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(Host.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(Host.Lock);
  end;  
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Get Vendor Id}
    VendorId:=Host.Registers.VendorId;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  VendorId = ' + IntToHex(VendorId,8));
    {$ENDIF}
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
    
    {Check Vendor Id}
    if ((VendorId and DWC_VENDOR_ID_MASK) <> DWC_VENDOR_ID_OTG2) and ((VendorId and DWC_VENDOR_ID_MASK) <> DWC_VENDOR_ID_OTG3) then
     begin
      Result:=USB_STATUS_OPERATION_FAILED;
      Exit;
     end;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    if DWCOTG_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(Host.Lock);
     end
    else
     begin
      SpinUnlockIRQ(Host.Lock);
     end;     
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;
   
{==============================================================================}

function DWCHostInit(Host:PDWCUSBHost):LongWord; 
{Initialize the DWC OTG USB Host Controller with core USB settings and perform a host reset}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 HWCfg2:LongWord;
 ResultCode:LongWord; 
 AHBConfiguration:LongWord;
 CoreUSBConfiguration:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Initializing USB Host');
 {$ENDIF}
 
 {Acquire the Lock}
 if DWCOTG_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(Host.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(Host.Lock);
  end;  
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}

    {Get Core USB Configuration}
    CoreUSBConfiguration:=Host.Registers.CoreUSBConfiguration;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  CoreUSBConfiguration = ' + IntToHex(CoreUSBConfiguration,8));
    {$ENDIF}
    
    {Disable ULPI External VBUS and External TS Dline pulsing}
    CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_ULPI_EXT_VBUS_DRV);
    CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_TERM_SEL_DL_PULSE);
   
    {Save Core USB Configuration}
    Host.Registers.CoreUSBConfiguration:=CoreUSBConfiguration;
    
    {Reset Host}  
    Result:=DWCHostResetEx(Host);
    if Result <> USB_STATUS_SUCCESS then Exit;

    {Get Core USB Configuration}
    CoreUSBConfiguration:=Host.Registers.CoreUSBConfiguration;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  CoreUSBConfiguration (After Reset) = ' + IntToHex(CoreUSBConfiguration,8));
    {$ENDIF}
    
    {Check for Full Speed / Low Speed only}
    if DWCOTG_FULL_SPEED_ONLY then
     begin
      {Enable FS PHY Select}
      CoreUSBConfiguration:=CoreUSBConfiguration or DWC_USB_CFG_PHY_SEL;
     end
    else 
     begin
      {Disable ULPI PHY and 16 bit PHY Width}
      CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_ULPI_UTMI_SEL);
      CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_PHYIF16);
     end; 
    
    {Save Core USB Configuration}
    Host.Registers.CoreUSBConfiguration:=CoreUSBConfiguration;

    {Reset Host}
    Result:=DWCHostResetEx(Host);
    if Result <> USB_STATUS_SUCCESS then Exit;
    
    {Get Hardware Configuration 2}
    HWCfg2:=Host.Registers.HWCfg2;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  HWCfg2 = ' + IntToHex(HWCfg2,8));
    {$ENDIF}
    
    {Check DMA Architecture}
    if (HWCfg2 and DWC_HWCFG2_ARCHITECTURE_INT_DMA) <> DWC_HWCFG2_ARCHITECTURE_INT_DMA then
     begin
      Result:=USB_STATUS_OPERATION_FAILED;
      Exit;
     end;
    
    {Get Core USB Configuration}
    CoreUSBConfiguration:=Host.Registers.CoreUSBConfiguration;
    
    {Check High Speed and Full Speed PHY Type}
    if ((HWCfg2 and DWC_HWCFG2_HS_PHY_TYPE_ULPI) = DWC_HWCFG2_HS_PHY_TYPE_ULPI) and ((HWCfg2 and DWC_HWCFG2_FS_PHY_TYPE_DEDICATED) = DWC_HWCFG2_FS_PHY_TYPE_DEDICATED) then
     begin
      {Enable ULPI FSLS}
      CoreUSBConfiguration:=CoreUSBConfiguration or DWC_USB_CFG_ULPI_FSLS;
      CoreUSBConfiguration:=CoreUSBConfiguration or DWC_USB_CFG_ULPI_CLK_SUS_M;
     end
    else
     begin
      {Disable ULPI FSLS}
      CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_ULPI_FSLS);
      CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_ULPI_CLK_SUS_M);
     end;     
    
    {Save Core USB Configuration}
    Host.Registers.CoreUSBConfiguration:=CoreUSBConfiguration;
    
    {Check Channel Count}
    if ((HWCfg2 and DWC_HWCFG2_NUM_HOST_CHANNELS) shr 14) + 1 > DWC_NUM_CHANNELS then
     begin
      Result:=USB_STATUS_OPERATION_FAILED;
      Exit;
     end;
     
    {Get AHB Configuration}
    AHBConfiguration:=Host.Registers.AHBConfiguration;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  AHBConfiguration = ' + IntToHex(AHBConfiguration,8));
    {$ENDIF}
    
    {Enable DMA and AXI Writes}
    AHBConfiguration:=AHBConfiguration or DWC_AHB_DMA_ENABLE;
    AHBConfiguration:=AHBConfiguration or BCM_DWC_AHB_AXI_WAIT;
    AHBConfiguration:=AHBConfiguration and not(BCM_DWC_AHB_AXI_BURST_MASK);

    {Save AHB Configuration}
    Host.Registers.AHBConfiguration:=AHBConfiguration;

    {Get Core USB Configuration}
    CoreUSBConfiguration:=Host.Registers.CoreUSBConfiguration;

    {Disable HNP and SRP}
    CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_HNP_CAPABLE);
    CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_SRP_CAPABLE);
    
    {Save Core USB Configuration}
    Host.Registers.CoreUSBConfiguration:=CoreUSBConfiguration;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  CoreUSBConfiguration (After Init) = ' + IntToHex(CoreUSBConfiguration,8));
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  AHBConfiguration (After Init) = ' + IntToHex(AHBConfiguration,8));
    {$ENDIF}
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    if DWCOTG_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(Host.Lock);
     end
    else
     begin
      SpinUnlockIRQ(Host.Lock);
     end;     
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;
 
{==============================================================================}

function DWCHostSetup(Host:PDWCUSBHost):LongWord; 
{Configure the DWC OTG USB Host Controller with....}
var
 ResultCode:LongWord; 
 HostFrameInterval:LongWord;
 PowerClockControl:LongWord;
 HostConfiguration:LongWord;
 CoreUSBConfiguration:LongWord;
 HostPortControlStatus:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;

 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Configuring USB Host');
 {$ENDIF}

 {Acquire the Lock}
 if DWCOTG_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(Host.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(Host.Lock);
  end;  
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Get Core USB Configuration}
    CoreUSBConfiguration:=Host.Registers.CoreUSBConfiguration;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  CoreUSBConfiguration = ' + IntToHex(CoreUSBConfiguration,8));
    {$ENDIF}
    
    {Set HS/FS Timeout Calibration} 
    CoreUSBConfiguration:=CoreUSBConfiguration or DWC_USB_CFG_TOUTCAL_LIMIT;
    
    {Save Core USB Configuration}
    Host.Registers.CoreUSBConfiguration:=CoreUSBConfiguration;
    
    {Get Power and Clock Control}
    PowerClockControl:=Host.Registers.PowerClockControl;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  PowerClockControl = ' + IntToHex(PowerClockControl,8));
    {$ENDIF}
    
    {Restart the PHY Clock}
    PowerClockControl:=0;
    
    {Save Power and Clock Control}
    Host.Registers.PowerClockControl:=PowerClockControl;
    
    {Get Host Configuration}
    HostConfiguration:=Host.Registers.HostConfiguration;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  HostConfiguration = ' + IntToHex(HostConfiguration,8));
    {$ENDIF}
    
    {Check for Full Speed / Low Speed only}
    if DWCOTG_FULL_SPEED_ONLY then
     begin
       {Full speed PHY at 48MHz}
       HostConfiguration:=HostConfiguration or DWC_HCFG_FS_LS_SUPPORT_ONLY;
       
       HostConfiguration:=HostConfiguration and not(DWC_HCFG_FS_LS_PHY_CLK_SEL_MASK);
       HostConfiguration:=HostConfiguration or (DWC_HCFG_FS_LS_PHY_CLK_SEL_48_MHZ shl DWC_HCFG_FS_LS_PHY_CLK_SEL_SHIFT);
     end
    else
     begin
       {High speed PHY at full speed or high speed}
       HostConfiguration:=HostConfiguration and not(DWC_HCFG_FS_LS_PHY_CLK_SEL_MASK);
       HostConfiguration:=HostConfiguration or (DWC_HCFG_FS_LS_PHY_CLK_SEL_30_60_MHZ shl DWC_HCFG_FS_LS_PHY_CLK_SEL_SHIFT);
     end;
     
    {Save Host Configuration}
    Host.Registers.HostConfiguration:=HostConfiguration;
    
    {Get Host Frame Interval}
    HostFrameInterval:=Host.Registers.HostFrameInterval;
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  HostFrameInterval = ' + IntToHex(HostFrameInterval,8));
    {$ENDIF}
    
    {Check Host Frame Interval}
    if DWCOTG_HOST_FRAME_INTERVAL then
     begin
      {Enable Reload Control}
      {This bit allows dynamic reloading of the HFIR register during runtime. This bit needs to be programmed during initial configuration and its value must not be changed during runtime}
      HostFrameInterval:=HostFrameInterval or DWC_HFIR_FRAME_INT_RELOAD_CTL;
      
      {Save Host Frame Interval}
      Host.Registers.HostFrameInterval:=HostFrameInterval;
     end; 
    
    {Setup DMA}
    Result:=DWCHostSetupDMA(Host);
    if Result <> USB_STATUS_SUCCESS then Exit;
    
    {Get Host Port Control and Status}
    HostPortControlStatus:=DWCHostPortGetStatus(Host);
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  HostPortControlStatus = ' + IntToHex(HostPortControlStatus,8));
    {$ENDIF}
    
    {Enable Host Port Power}
    HostPortControlStatus:=HostPortControlStatus or DWC_HOST_PORT_CTRLSTATUS_POWERED;
    
    {Save Host Port Control and Status}
    Host.Registers.HostPortControlStatus:=HostPortControlStatus;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
    
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  PowerClockControl (After Setup) = ' + IntToHex(PowerClockControl,8));
    if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  HostFrameInterval (After Setup) = ' + IntToHex(HostFrameInterval,8));
    {$ENDIF}
    
    {Setup Interrupts}
    Result:=DWCHostSetupInterrupts(Host);
    if Result <> USB_STATUS_SUCCESS then Exit;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    if DWCOTG_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(Host.Lock);
     end
    else
     begin
      SpinUnlockIRQ(Host.Lock);
     end;     
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function DWCHostSetupDMA(Host:PDWCUSBHost):LongWord;
{Set up the DWC OTG USB Host Controller for DMA (direct memory access).  This
 makes it possible for the Host Controller to directly access in-memory
 buffers when performing USB transfers}
 
{Note: Caller must hold the host lock} 
var
 ResultCode:LongWord; 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Enabling DMA');
 {$ENDIF}
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: ' + IntToStr(Host.Registers.HWCfg3 shr 16) + ' words of RAM available for dynamic FIFOs');
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Original FIFO sizes: Rx ' + IntToHex(Host.Registers.ReceiveFIFOSize,8) + ',  Non Periodic Tx ' + IntToHex(Host.Registers.NonPeriodicTransmitFIFOSize,8) + ', Periodic Tx ' + IntToHex(Host.Registers.HostPeriodicTransmitFIFOSize,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {First configure the Host Controller's FIFO sizes.  This is required
  because the default values (at least in Broadcom's instantiation of the
  Synopsys USB block) do not work correctly.  If software fails to do this,
  receiving data will fail in virtually impossible to debug ways that cause
  memory corruption.  This is true even though we are using DMA and not
  otherwise interacting with the Host Controller's FIFOs in this driver}
 Host.Registers.ReceiveFIFOSize:=DWC_RECEIVE_WORDS;
 Host.Registers.NonPeriodicTransmitFIFOSize:=(DWC_TRANSMIT_WORDS shl 16) or DWC_RECEIVE_WORDS;
 Host.Registers.HostPeriodicTransmitFIFOSize:=(DWC_PERIODIC_TRANSMIT_WORDS shl 16) or (DWC_RECEIVE_WORDS + DWC_TRANSMIT_WORDS);
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: New FIFO sizes: Rx ' + IntToHex(Host.Registers.ReceiveFIFOSize,8) + ',  Non Periodic Tx ' + IntToHex(Host.Registers.NonPeriodicTransmitFIFOSize,8) + ', Periodic Tx ' + IntToHex(Host.Registers.HostPeriodicTransmitFIFOSize,8));
 {$ENDIF}
 
 {Actually enable DMA by setting the appropriate flag; also set an extra flag available only in Broadcom's
  instantiation of the Synopsys USB block that may or may not actually be needed}
 Host.Registers.AHBConfiguration:=Host.Registers.AHBConfiguration or (DWC_AHB_DMA_ENABLE or BCM_DWC_AHB_AXI_WAIT);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end; 

{==============================================================================}

function DWCHostSetupInterrupts(Host:PDWCUSBHost):LongWord; 
{Performs initial setup of the Synopsys Designware USB 2.0 On-The-Go
 Controller (DWC) interrupts.
 The DWC contains several levels of interrupt registers, detailed in the
 following list.  Note that for each level, each bit of the "interrupt"
 register contains the state of a pending interrupt (1 means interrupt
 pending; write 1 to clear), while the "interrupt mask" register has the same
 format but is used to turn the corresponding interrupt on or off (1 means on;
 write 1 to turn on; write 0 to turn off).
  - The AHB configuration register contains a mask bit used to enable/disable
    all interrupts whatsoever from the DWC hardware.
  - The "Core" interrupt and interrupt mask registers control top-level
    interrupts.  For example, a single bit in these registers corresponds to
    all channel interrupts.
  - The "Host All Channels" interrupt and interrupt mask registers control all
    interrupts on each channel.
  - The "Channel" interrupt and interrupt mask registers, of which one copy
    exists for each channel, control individual interrupt types on that
    channel.
 We can assume that an interrupt only occurs if it is enabled in all the
 places listed above.  Furthermore, it only seems to work to clear interrupts
 at the lowest level; for example, a channel interrupt must be cleared in its
 individual channel interrupt register rather than in one of the higher level
 interrupt registers.
 The above just covers the DWC-specific interrupt registers.  In addition to
 those, the system will have other ways to control interrupts.  For example,
 on the BCM2835 (Raspberry Pi), the interrupt line going to the DWC is just
 one of many dozen and can be enabled/disabled using the interrupt controller.
 In the code below we enable this interrupt line and register a handler
 function so that we can actually get interrupts from the DWC.
 And all that's in addition to the CPSR of the ARM processor itself, or the
 equivalent on other CPUs.  So all in all, you literally have to enable
 interrupts in 6 different places to get an interrupt when a USB transfer has
 completed}
 
{Note: Caller must hold the host lock} 
var
 ResultCode:LongWord;
 CoreInterruptMask:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Enabling Interrupts');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Clear all pending core interrupts}
 Host.Registers.CoreInterruptMask:=0;
 Host.Registers.CoreInterrupts:=$FFFFFFFF;

 {Enable core host channel and host port interrupts}
 Host.Registers.CoreInterruptMask:=DWC_CORE_INTERRUPTS_HOST_CHANNEL_INTR or DWC_CORE_INTERRUPTS_PORT_INTR;

 {Request the IRQ/FIQ}
 if DWCOTG_FIQ_ENABLED then
  begin
   RequestFIQ(FIQ_ROUTING,DWCOTG_IRQ,TInterruptHandler(DWCInterruptHandler),Host); 
  end
 else
  begin 
   RequestIRQ(IRQ_ROUTING,DWCOTG_IRQ,TInterruptHandler(DWCInterruptHandler),Host);
  end; 

 {Enable interrupts for entire USB host controller.  (Yes that's what we just did, but this one is controlled by the host controller itself}
 Host.Registers.AHBConfiguration:=(Host.Registers.AHBConfiguration or DWC_AHB_INTERRUPT_ENABLE);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCHostStartFrameInterrupt(Host:PDWCUSBHost;Enable:Boolean):LongWord; 
var
 ResultCode:LongWord;
 CoreInterruptMask:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Setting Start of Frame Interrupt');
 {$ENDIF}
 
 {Acquire the Lock}
 if DWCOTG_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(Host.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(Host.Lock);
  end;  
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}

    {Get Interrupt Mask}
    CoreInterruptMask:=Host.Registers.CoreInterruptMask;
    
    {Update Interrupt Mask}
    if Enable then
     begin
      CoreInterruptMask:=(CoreInterruptMask or DWC_CORE_INTERRUPTS_SOF_INTR);     
     end
    else
     begin
      CoreInterruptMask:=(CoreInterruptMask and not(DWC_CORE_INTERRUPTS_SOF_INTR));
     end;
    
    {Set Interrupt Mask}
    Host.Registers.CoreInterruptMask:=CoreInterruptMask;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    {Release the Lock}
    if DWCOTG_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(Host.Lock);
     end
    else
     begin
      SpinUnlockIRQ(Host.Lock);
     end;     
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function DWCAllocateChannel(Host:PDWCUSBHost):LongWord;
{Get the next available host channel on the supplied DWC host}
{Host: The DWC host to get available channel from}
{Return: Channel number of the next available channel}
var
 Channel:LongWord;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);
 
 {Check Host}
 if Host = nil then Exit;
 
 {Wait for Semaphore}
 if SemaphoreWait(Host.ChannelFreeWait) <> ERROR_SUCCESS then Exit;
 
 {Acquire the Lock}
 if MutexLock(Host.ChannelFreeLock) = ERROR_SUCCESS then
  begin
   try
    {Get Free Channel}
    Channel:=FirstBitSet(Host.ChannelFreeMask);
 
    {Update Channel Free}
    Host.ChannelFreeMask:=Host.ChannelFreeMask xor (1 shl Channel);
 
    {Return Result}
    Result:=Channel;
   finally
    {Release the Lock}
    MutexUnlock(Host.ChannelFreeLock);
   end;   
  end;
end;

{==============================================================================}

function DWCReleaseChannel(Host:PDWCUSBHost;Channel:LongWord):LongWord;
{Mark the specified host channel on the supplied DWC host as available}
{Host: The DWC host to mark available channel on}
{Channel: The channel number to mark as available}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Channel}
 if Channel >= DWC_NUM_CHANNELS then Exit;
 
 {Acquire the Lock}
 if MutexLock(Host.ChannelFreeLock) = ERROR_SUCCESS then
  begin
   try
    {Update Channel Free}
    Host.ChannelFreeMask:=Host.ChannelFreeMask or (1 shl Channel);
 
    {Signal Semaphore}
    SemaphoreSignal(Host.ChannelFreeWait);
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Host.ChannelFreeLock);
   end;   
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end;

{==============================================================================}

function DWCChannelStartTransfer(Host:PDWCUSBHost;Channel:LongWord;Request:PUSBRequest):LongWord;
{Start or restart a USB request on a channel of the supplied DWC host}
{Host: The DWC host to start the request on}
{Channel: The channel number to start the request on}
{Request: USB request to start}

{Note: Caller must hold the host lock}          
{Note: Can be called by the interrupt handler} 

//To Do //Need to do some rework of this to better handle the bitmask mapping that C normally does as part of a union
var
 Data:Pointer;
 Transfer:LongWord;
 SplitControl:LongWord;
 Characteristics:LongWord;
 HostChannel:PDWCHostChannel;
 TransactionTranslatorHub:PUSBDevice;
 TransactionTranslatorHubPort:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;

 {Check Request}
 if Request = nil then Exit;

 {Check Channel}
 if Channel >= DWC_NUM_CHANNELS then Exit;
 
 {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Starting transfer on channel ' + IntToStr(Channel));
 {$ENDIF}
 
 {Get Host Channel}
 HostChannel:=@Host.Registers.HostChannels[Channel];
 
 {Setup Parameters}
 Characteristics:=0;
 SplitControl:=0;
 Transfer:=0;
 Request.ShortAttempt:=False;
 Request.StartOfFrame:=False;
 
 {Determine Endpoint Number, Endpoint Type, Maximum Packet Size and Packets Per Frame}
 if Request.Endpoint <> nil then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Using Endpoint characteristics');
   {$ENDIF}

   {Endpoint explicitly specified.  Get the needed information from the endpoint descriptor}
   
   {Setup Endpoint Number}
   Characteristics:=(Characteristics or ((Request.Endpoint.bEndpointAddress and $0F) shl 11));
   
   {Setup Endpoint Type}
   Characteristics:=(Characteristics or ((Request.Endpoint.bmAttributes and $03) shl 18));
   
   {Setup Max Packet Size}
   Characteristics:=(Characteristics or ((Request.Endpoint.wMaxPacketSize and $7FF) shl 0));
   
   {Setup Packets Per Frame}
   Characteristics:=(Characteristics or (1 shl 20));
   
   {Check Speed}
   if Request.Device.Speed = USB_SPEED_HIGH then
    begin
     {Adjust Packets Per Frame}
     Characteristics:=(Characteristics or (((Request.Endpoint.wMaxPacketSize shr 11) and $03) + 1) shl 20); {Plus 1 to account for original Packets per Frame above}
    end;
  end
 else
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Using Default characteristics');
   {$ENDIF}

   {Default control endpoint.  The endpoint number, endpoint type and packets per frame are pre-determined, while the maximum packet size can be found in the device descriptor}
   
   {Setup Endpoint Number}
   Characteristics:=(Characteristics or (0 shl 11));
   
   {Setup Endpoint Type}
   Characteristics:=(Characteristics or (USB_TRANSFER_TYPE_CONTROL shl 18));
   
   {Setup Max Packet Size}
   Characteristics:=(Characteristics or (Request.Device.Descriptor.bMaxPacketSize0 shl 0));
   
   {Setup Packets per Frame}
   Characteristics:=(Characteristics or (1 shl 20));
  end;

 {Determine the endpoint direction, data pointer, data size, and initial packet ID.  For control transfers, the overall phase of the control transfer must be taken into account}  
 if ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_TYPE) shr 18) = USB_TRANSFER_TYPE_CONTROL then
  begin
   {Starting or re-starting a control transfer}
   {Check Control Phase}
   case Request.ControlPhase of
    USB_CONTROL_PHASE_SETUP:begin
      {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Starting SETUP transaction');
      {$ENDIF}

      {SETUP phase of control transfer}
      
      {Setup Endpoint Direction}
      Characteristics:=(Characteristics or (USB_DIRECTION_OUT shl 15));
      
      {Setup Data}
      Data:=Request.SetupData;
      
      {Setup Transfer Size}
      Transfer:=(Transfer or (SizeOf(TUSBControlSetupData) shl 0));
      
      {Setup Transfer Packet ID}
      Transfer:=(Transfer or (DWC_USB_PID_SETUP shl 29));
     end;
    USB_CONTROL_PHASE_DATA:begin
      {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Starting DATA transaction');
      if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG:  (Size=' + IntToStr(Request.Size) + ', ActualSize=' + IntToStr(Request.ActualSize) + ')');
      {$ENDIF}

      {DATA phase of control transfer}

      {Setup Endpoint Direction}
      Characteristics:=(Characteristics or ((Request.SetupData.bmRequestType shr 7) shl 15));
      
      {Setup Data} {We need to carefully take into account that we might be re-starting a partially complete transfer}
      Data:=Pointer(PtrUInt(Request.Data) + Request.ActualSize);
      
      {Setup Transfer Size}
      Transfer:=(Transfer or ((Request.Size - Request.ActualSize) shl 0));
      
      {Setup Transfer Packet ID}
      if Request.ActualSize = 0 then
       begin
        {First transaction in the DATA phase, use a DATA1 packet ID}
        Transfer:=(Transfer or (DWC_USB_PID_DATA1 shl 29));
       end
      else
       begin
        {Later transaction in the DATA phase, restore the saved packet ID (will be DATA0 or DATA1)}
        Transfer:=(Transfer or (Request.NextDataPID shl 29));
       end;
     end;
    else    
     begin
      {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Starting STATUS transaction');
      {$ENDIF}
      
      {STATUS phase of control transfer}
      
      {The direction of the STATUS transaction is opposite the direction of the DATA transactions, or from device to host if there were no DATA transactions}
      {Setup Endpoint Direction}
      if ((Request.SetupData.bmRequestType shr 7) = USB_DIRECTION_OUT) or (Request.SetupData.wLength = 0) then
       begin
        Characteristics:=(Characteristics or (USB_DIRECTION_IN shl 15));
       end
      else
       begin
        Characteristics:=(Characteristics or (USB_DIRECTION_OUT shl 15));
       end;       
      {The STATUS transaction has no data buffer, yet must use a DATA1 packet ID}
      
      {Setup Data}
      Data:=@Request.StatusData;
      
      {Setup Transfer Size}
      Transfer:=(Transfer or (0 shl 0));
      
      {Setup Transfer Packet ID}
      Transfer:=(Transfer or (DWC_USB_PID_DATA1 shl 29));
     end;   
   end;
  end
 else
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Starting non-control transaction');
   {$ENDIF}
   
   {Starting or re-starting a non-control transfer}
   
   {Setup Endpoint Direction}
   Characteristics:=(Characteristics or ((Request.Endpoint.bEndpointAddress shr 7) shl 15));
   
   {As is the case for the DATA phase of control transfers, we need to carefully take into account that we might be restarting a partially complete transfer}
   {Setup Data}
   Data:=Pointer(PtrUInt(Request.Data) + Request.ActualSize);
   
   {Setup Transfer Size}
   Transfer:=(Transfer or ((Request.Size - Request.ActualSize) shl 0));
   
   {This hardware does not accept interrupt transfers started with more data than fits in one (micro)frame that is, the maximum packets per frame allowed by
    the endpoint times the maximum packet size allowed by the endpoint}
   if ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_TYPE) shr 18) = USB_TRANSFER_TYPE_INTERRUPT then
    begin
     if ((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0) > (((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_PACKETS_PER_FRAME) shr 20) * ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) shr 0)) then 
      begin
       {Adjust Transfer Size}
       Transfer:=(Transfer and not(DWC_HOST_CHANNEL_TRANSFER_SIZE)); {Reset Transfer Size}
       Transfer:=(Transfer or ((((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_PACKETS_PER_FRAME) shr 20) * ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) shr 0)) shl 0));
       
       {Set Short Attempt}
       Request.ShortAttempt:=True;
       
       {Update Statistics}
       Inc(Host.ShortAttemptCount); 
      end;
      
     {Check for Speed}
     if Request.Device.Speed <> USB_SPEED_HIGH then
      begin
       {Note: Both Linux and FreeBSD place all split interrupt requests into the control queue in order to ease the timing pressure}
       
       {Change the Endpoint type from Interrupt to Control}
       Characteristics:=(Characteristics and not(USB_TRANSFER_TYPE_INTERRUPT shl 18));
      end; 
    end;
   
   {Setup Transfer Packet ID}
   Transfer:=(Transfer or (Request.NextDataPID shl 29));
  end;  
 
 {Set device address}
 Characteristics:=(Characteristics or (Request.Device.Address shl 22));

 {If communicating with a low or full-speed device, program the split control register.  Also cap the attempted transfer size to the maximum packet size, since the transfer will very
  likely have to be resubmitted to wait for the Complete Split portion (and then rescheduled on a possibly different channel later).  And finally, set the low speed flag in the Channel
  Characteristics register if communicating with a low-speed device}
 if Request.Device.Speed <> USB_SPEED_HIGH then
  begin 
   {Determine which hub is acting as the Transaction Translator}
   TransactionTranslatorHub:=Request.Device;
   repeat 
    TransactionTranslatorHubPort:=TransactionTranslatorHub.PortNumber;
    TransactionTranslatorHub:=TransactionTranslatorHub.Parent;
   until TransactionTranslatorHub.Speed = USB_SPEED_HIGH;
   
   {Check for Root Hub (Do not do Split on Root Hub)}
   if not USBIsRootHub(TransactionTranslatorHub) then
    begin
     {Setup Port Address}
     SplitControl:=(SplitControl or ((TransactionTranslatorHubPort - 1) shl 0));
     
     {Setup Hub Addresss}
     SplitControl:=(SplitControl or (TransactionTranslatorHub.Address shl 7));
     
     {Setup Split Enable}
     SplitControl:=(SplitControl or DWC_HOST_CHANNEL_SPLIT_CONTROL_SPLIT_ENABLE);
     
     {Set Is Split}
     Request.IsSplit:=True;
     
     {Set Start of Frame}
     Request.StartOfFrame:=True;
     
     {Update Statistics}
     Inc(Host.StartOfFrameCount);
     
     {Check Transfer Size}
     if ((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0) > ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) shr 0) then
      begin
       {Adjust Transfer Size}
       Transfer:=(Transfer and not(DWC_HOST_CHANNEL_TRANSFER_SIZE)); {Reset Transfer Size}
       Transfer:=(Transfer or (((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) shr 0) shl 0));
       
       {Set Short Attempt}
       Request.ShortAttempt:=True;
       
       {Update Statistics}
       Inc(Host.ShortAttemptCount); 
      end; 
    end;
    
   if Request.Device.Speed = USB_SPEED_LOW then
    begin
     {Setup Low Speed}
     Characteristics:=(Characteristics or DWC_HOST_CHANNEL_CHARACTERISTICS_LOWSPEED);
    end;    
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Setup DMA buffer}
 if (Request.Flags and USB_REQUEST_FLAG_COMPATIBLE) = USB_REQUEST_FLAG_COMPATIBLE then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Using data buffer at ' + PtrToHex(Data) + ' for DMA transaction');
   {$ENDIF}
   
   {Can DMA directly}
   if DWCOTG_DMA_BUS_ADDRESSES then
    begin
     HostChannel.DMAAddress:=PhysicalToBusAddress(Data);
    end
   else
    begin   
     HostChannel.DMAAddress:=PtrUInt(Data);
    end;

   {For OUT endpoints, flush the data to send into the DMA buffer}
   if ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_DIRECTION) shr 15) = USB_DIRECTION_OUT then
    begin
     if not(DWCOTG_DMA_CACHE_COHERENT) then
      begin
       {Flush the data cache}
       CleanDataCacheRange(PtrUInt(Data),((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0));
      end; 
    end
   else
    begin
     {For IN endpoints, ensure there is no uncommitted data in the DMA buffer region}
     if not(DWCOTG_DMA_CACHE_COHERENT) then 
      begin
       {Flush the data cache}
       CleanDataCacheRange(PtrUInt(Data),((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0));
      end; 
    end;
  end
 else
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Using DMA buffer at ' + PtrToHex(Host.DMABuffers[Channel]) + ' for DMA transaction');
   {$ENDIF}

   {Use a buffer for DMA (If the attempted transfer size overflows this alternate buffer cap it to the greatest number of whole packets that fit)}
   if DWCOTG_DMA_BUS_ADDRESSES then
    begin
     HostChannel.DMAAddress:=PhysicalToBusAddress(Host.DMABuffers[Channel]);
    end
   else
    begin   
     HostChannel.DMAAddress:=PtrUInt(Host.DMABuffers[Channel]);
    end; 
   
   {Check Transfer Size}
   if ((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0) > USB_MAX_PACKET_SIZE then
    begin
     {Adjust Transfer Size}
     Transfer:=(Transfer and not(DWC_HOST_CHANNEL_TRANSFER_SIZE)); {Reset Transfer Size}
     Transfer:=(Transfer or ((USB_MAX_PACKET_SIZE - (USB_MAX_PACKET_SIZE mod ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) shr 0))) shl 0));
     
     {Set Short Attempt}
     Request.ShortAttempt:=True;
     
     {Update Statistics}
     Inc(Host.ShortAttemptCount); 
    end;
  
   {For OUT endpoints, copy and flush the data to send into the DMA buffer}
   if ((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_DIRECTION) shr 15) = USB_DIRECTION_OUT then
    begin
     {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Copying data to DMA buffer');
     {$ENDIF}
     {Update Statistics}
     Inc(Host.DMABufferWriteCount);
     
     {Copy the data to the DMA buffer}
     System.Move(Data^,Host.DMABuffers[Channel]^,((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0));

     if not(DWCOTG_DMA_CACHE_COHERENT) then
      begin
       {Flush the data cache}
       CleanDataCacheRange(PtrUInt(Host.DMABuffers[Channel]),((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0));
      end; 
    end;
  end;  
  
 {Set pointer to start of next chunk of data to send/receive (may be different from the actual DMA address to be used by the hardware if an alternate buffer was selected above)}
 Request.CurrentData:=Data;
 
 {Calculate the number of packets being set up for this transfer}
 {Setup Packet Count}
 Transfer:=(Transfer or (DWCDivRoundUp(((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0),((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) shr 0)) shl 19));
 if ((Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) = 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Increasing packet count to 1');
   {$ENDIF}

   {The hardware requires that at least one packet is specified, even for zero length transfers}
   
   {Adjust Packet Count}
   Transfer:=(Transfer or (1 shl 19));
  end;
 
 {Remember the actual size and number of packets we are attempting to transfer}
 Request.AttemptedSize:=((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0);
 Request.AttemptedBytesRemaining:=((Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0);
 Request.AttemptedPacketsRemaining:=((Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19);
 if not(Request.CompleteSplit) then Request.BytesAttempted:=Request.BytesAttempted + Request.AttemptedSize;
 
 {Save the request for the interrupt handler}
 Host.ChannelRequests[Channel]:=Request;

 {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if USB_LOG_ENABLED then
  begin
   USBLogDebug(Request.Device,'DWCOTG: Setting up transaction on channel ' + IntToStr(Channel) + ':');
   USBLogDebug(Request.Device,'DWCOTG:   CHARACTERISTICS_MAX_PACKET_SIZE=' + IntToStr(Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) + ', CHARACTERISTICS_ENDPOINT_NUMBER=' + IntToStr((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_NUMBER) shr 11) + ',');
   USBLogDebug(Request.Device,'DWCOTG:   CHARACTERISTICS_ENDPOINT_DIRECTION=' + USBDirectionToString((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_DIRECTION) shr 15) + ', CHARACTERISTICS_LOWSPEED=' + IntToStr((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_LOWSPEED) shr 17) + ',');
   USBLogDebug(Request.Device,'DWCOTG:   CHARACTERISTICS_ENDPOINT_TYPE=' + USBTransferTypeToString((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_TYPE) shr 18) + ', CHARACTERISTICS_DEVICE_ADDRESS=' + IntToStr((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_DEVICE_ADDRESS) shr 22) + ',');
   USBLogDebug(Request.Device,'DWCOTG:   TRANSFER_SIZE=' + IntToStr(Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) + ', TRANSFER_PACKET_COUNT=' + IntToStr((Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ',');
   USBLogDebug(Request.Device,'DWCOTG:   TRANSFER_PACKET_ID=' + IntToStr((Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_ID) shr 29) + ', SPLIT_CONTROL_SPLIT_ENABLE=' + IntToStr((SplitControl and DWC_HOST_CHANNEL_SPLIT_CONTROL_SPLIT_ENABLE) shr 31) + ',');
   USBLogDebug(Request.Device,'DWCOTG:   CompleteSplit=' + BooleanToString(Request.CompleteSplit));
   USBLogDebug(Request.Device,'DWCOTG:   DMAAddress=' + IntToHex(HostChannel.DMAAddress,8) + ', CurrentData=' + PtrToHex(Request.CurrentData));
   USBLogDebug(Request.Device,'DWCOTG:   AttemptedSize=' + IntToStr(Request.AttemptedSize) + ', AttemptedBytesRemaining=' + IntToStr(Request.AttemptedBytesRemaining) + ', AttemptedPacketsRemaining=' + IntToStr(Request.AttemptedPacketsRemaining));
  end; 
 {$ENDIF}
 
 {Program the channel registers}
 HostChannel.Characteristics:=Characteristics;
 HostChannel.SplitControl:=SplitControl;
 HostChannel.Transfer:=Transfer;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Enable the channel, and start the USB transfer}    
 Result:=DWCChannelStartTransaction(Host,Channel,Request);
end;

{==============================================================================}

function DWCChannelStartTransaction(Host:PDWCUSBHost;Channel:LongWord;Request:PUSBRequest):LongWord;
{Start a USB transaction on a channel of the supplied DWC host}
{Host: The DWC host to start the transaction on}
{Channel: The host channel number to start the transaction on}
{Request: USB request set up for the next transaction}

{Note: Caller must hold the host lock} 
{Note: Can be called by the interrupt handler}          
var
 NextFrame:LongWord;
 ResultCode:LongWord;
 SplitControl:LongWord;
 InterruptMask:LongWord;
 Characteristics:LongWord;
 HostChannel:PDWCHostChannel;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;

 {Check Request}
 if Request = nil then Exit;
 
 {Check Channel}
 if Channel >= DWC_NUM_CHANNELS then Exit;

 {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Starting transaction on channel ' + IntToStr(Channel));
 {$ENDIF}
 
 {Get Host Channel}
 HostChannel:=@Host.Registers.HostChannels[Channel];
   
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
   
 {Clear pending interrupts}
 HostChannel.InterruptMask:=0;
 HostChannel.Interrupts:=$FFFFFFFF;
 
 {Check Start of Frame}
 if Request.StartOfFrame then
  begin
   {Acquire the Lock}
   if DWCOTG_FIQ_ENABLED then
    begin
     ResultCode:=SpinLockIRQFIQ(Host.StartOfFrameLock);
    end
   else
    begin
     ResultCode:=SpinLockIRQ(Host.StartOfFrameLock);
    end;  
   if ResultCode = ERROR_SUCCESS then
    begin
     try
      {Update SOF Wait bitmap}
      Host.StartOfFrameMask:=Host.StartOfFrameMask or (1 shl Channel);
     finally
      {Release the Lock}
      if DWCOTG_FIQ_ENABLED then
       begin
        SpinUnlockIRQFIQ(Host.StartOfFrameLock);
       end
      else
       begin
        SpinUnlockIRQ(Host.StartOfFrameLock);
       end;     
     end;   
    end;
   
   {Enable Start of Frame Interrupt}
   Host.Registers.CoreInterruptMask:=Host.Registers.CoreInterruptMask or DWC_CORE_INTERRUPTS_SOF_INTR;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end
 else
  begin 
   {Set whether this transaction is the completion part of a split transaction or not}
   SplitControl:=HostChannel.SplitControl;
   if Request.CompleteSplit then SplitControl:=(SplitControl or DWC_HOST_CHANNEL_SPLIT_CONTROL_COMPLETE_SPLIT) else SplitControl:=(SplitControl and not(DWC_HOST_CHANNEL_SPLIT_CONTROL_COMPLETE_SPLIT));
   HostChannel.SplitControl:=SplitControl;
   
   if (SplitControl and DWC_HOST_CHANNEL_SPLIT_CONTROL_COMPLETE_SPLIT) = 0 then
    begin
     Request.CompleteSplitRetries:=0;
    end;  
   if (SplitControl and DWC_HOST_CHANNEL_SPLIT_CONTROL_SPLIT_ENABLE) <> 0 then
    begin
     if not(Request.CompleteSplit) then 
      begin
       {$IFDEF USB_DEBUG}
       Inc(Request.StartSplitAttempts);
       {$ENDIF}
       
       {Update Statistics}
       Inc(Host.StartSplitCount)
      end
     else
      begin
       {$IFDEF USB_DEBUG}
       Inc(Request.CompleteSplitAttempts);
       {$ENDIF}
       
       {Update Statistics}
       Inc(Host.CompleteSplitCount);
      end; 
    end;  
   
   {Check for interrupt or isochronous endpoint}
   Characteristics:=HostChannel.Characteristics;
   if (((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_TYPE) shr 18) = USB_TRANSFER_TYPE_INTERRUPT) or (((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_TYPE) shr 18) = USB_TRANSFER_TYPE_ISOCHRONOUS) then
    begin
     {Check if the next frame is odd or even}
     NextFrame:=(Host.Registers.HostFrameNumber and DWC_HFNUM_FRAME_NUMBER_MASK) + 1;
     if (NextFrame and 1) <> 0 then Characteristics:=(Characteristics or DWC_HOST_CHANNEL_CHARACTERISTICS_ODD_FRAME) else Characteristics:=(Characteristics and not(DWC_HOST_CHANNEL_CHARACTERISTICS_ODD_FRAME));
     {Characteristics:=(Characteristics or DWC_HOST_CHANNEL_CHARACTERISTICS_CHANNEL_ENABLE);} {Moved to last}
     HostChannel.Characteristics:=Characteristics;
    end; 
   
   {Set the channel's interrupt mask to any interrupts we need to ensure that DWCInterruptHandler gets called when the software must take action on
    the transfer.  Furthermore, make sure interrupts from this channel are enabled in the Host All Channels Interrupt Mask Register.  Note: if you
    enable more channel interrupts here, DWCInterruptHandler needs to be changed to account for interrupts other than channel halted}
   HostChannel.InterruptMask:=DWC_HOST_CHANNEL_INTERRUPTS_CHANNEL_HALTED;
   Host.Registers.HostChannelsInterruptMask:=(Host.Registers.HostChannelsInterruptMask or (1 shl Channel));
   
   {Enable the channel}
   HostChannel.Characteristics:=HostChannel.Characteristics or DWC_HOST_CHANNEL_CHARACTERISTICS_CHANNEL_ENABLE;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
  end;  
end;

{==============================================================================}

function DWCHostPortReset(Host:PDWCUSBHost):LongWord;  
{Resets the DWC host port (The USB port that is attached to the root hub)}

{Note: Caller must hold the Host lock} 
var
 HostPortControlStatus:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Resetting host port');
 {$ENDIF}
 
 {Get the Status}
 HostPortControlStatus:=DWCHostPortGetStatus(Host);

 {Set the reset flag on the port, then clear it after a certain amount of time}
 HostPortControlStatus:=HostPortControlStatus or DWC_HOST_PORT_CTRLSTATUS_RESET;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Write the Status}
 Host.Registers.HostPortControlStatus:=HostPortControlStatus;

 {Wait for Reset}
 MillisecondDelay(60);

 {Clear the reset flag on the port}
 HostPortControlStatus:=HostPortControlStatus and not(DWC_HOST_PORT_CTRLSTATUS_RESET);

 {Write the Status}
 Host.Registers.HostPortControlStatus:=HostPortControlStatus;

 {Wait for Recovery}
 MillisecondDelay(USB_PORT_RESET_RECOVERY);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end; 

{==============================================================================}

function DWCHostPortPowerOn(Host:PDWCUSBHost):LongWord; 
{Powers on the DWC host port (The USB port that is attached to the root hub)}

{Note: Caller must hold the Host lock} 
var
 HostPortControlStatus:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Powering on host port');
 {$ENDIF}
 
 {Get the Status}
 HostPortControlStatus:=DWCHostPortGetStatus(Host);

 {Set the Powered bit}
 HostPortControlStatus:=HostPortControlStatus or DWC_HOST_PORT_CTRLSTATUS_POWERED;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Write the Status}
 Host.Registers.HostPortControlStatus:=HostPortControlStatus;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCHostPortGetStatus(Host:PDWCUSBHost):LongWord;
{Read the Host Port Control and Status register with the intention of
 modifying it.  Due to the inconsistent design of the bits in this register,
 this requires zeroing the write-clear bits so they aren't unintentionally
 cleared by writing back 1's to them}

{Note: Caller must hold the Host lock}
var
 HostPortControlStatus:LongWord;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);
 
 {Check Host}
 if Host = nil then Exit;
 
 {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Reading host port status');
 {$ENDIF}
 
 {Read the Status}
 HostPortControlStatus:=Host.Registers.HostPortControlStatus;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Zero the write clear bits}
 HostPortControlStatus:=HostPortControlStatus and not(DWC_HOST_PORT_CTRLSTATUS_ENABLED);
 HostPortControlStatus:=HostPortControlStatus and not(DWC_HOST_PORT_CTRLSTATUS_CONNECTED_CHANGED);
 HostPortControlStatus:=HostPortControlStatus and not(DWC_HOST_PORT_CTRLSTATUS_ENABLED_CHANGED);
 HostPortControlStatus:=HostPortControlStatus and not(DWC_HOST_PORT_CTRLSTATUS_OVERCURRENT_CHANGED);
 
 {Return Result}
 Result:=HostPortControlStatus;
end; 

{==============================================================================}

function DWCHostPortSetFeature(Host:PDWCUSBHost;Feature:Word):LongWord;
{Handle a SetPortFeature request on the port attached to the root hub}

{Note: Caller must hold the Host lock} 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 Result:=USB_STATUS_UNSUPPORTED_REQUEST;
 
 {Check Feature}
 case Feature of
  USB_PORT_POWER:begin
    {Power on Port}
    Result:=DWCHostPortPowerOn(Host);
   end;
  USB_PORT_RESET:begin
    {Reset Port}
    Result:=DWCHostPortReset(Host);
   end;
 end;
end;

{==============================================================================}

function DWCHostPortClearFeature(Host:PDWCUSBHost;Feature:Word):LongWord;
{Handle a ClearPortFeature request on the port attached to the root hub}

{Note: Caller must hold the Host lock} 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Status}
 if Host.PortStatus = nil then Exit;
 
 Result:=USB_STATUS_UNSUPPORTED_REQUEST;
 
 {Check Feature}
 case Feature of
  USB_C_PORT_CONNECTION:begin
    {Clear Connected Change}
    Host.PortStatus.wPortChange:=Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_CONNECTED); 
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  USB_C_PORT_ENABLE:begin
    {Clear Enabled Change}
    Host.PortStatus.wPortChange:=Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_ENABLED); 
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;  
  USB_C_PORT_SUSPEND:begin
    {Clear Suspended Change}
    Host.PortStatus.wPortChange:=Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_SUSPENDED); 
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  USB_C_PORT_OVER_CURRENT:begin
    {Clear Overcurrent Change}
    Host.PortStatus.wPortChange:=Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_OVERCURRENT); 
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  USB_C_PORT_RESET:begin
    {Clear Reset Change}
    Host.PortStatus.wPortChange:=Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_RESET); 
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;  
 end;
end;

{==============================================================================}

procedure DWCHostPortStatusChanged(Host:PDWCUSBHost); 
{Complete any outstanding IN interrupt status change request}
{Host: The DWCOTG host for the change request}

{Note: Caller must hold the Host lock} 
{Note: Can be called by the interrupt handler} 
var
 Message:TMessage;
 Request:PUSBRequest;
begin
 {}
 {Check Host}
 if Host = nil then Exit;
 
 {Get Request}
 Request:=Host.HubStatusChange;
 
 {Check Request}
 if Request <> nil then
  begin
   {Clear Request}
   Host.HubStatusChange:=nil;
   
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Host port status changed on root hub');
   {$ENDIF}
   
   {Update Status}
   PByte(Request.Data)^:=$02; {0x2 means Port 1 status changed (bit 0 is used for the hub itself)}
   
   {Update Request}
   Request.ActualSize:=1;
   Request.Status:=USB_STATUS_SUCCESS;
   
   {Send to the Completion thread}
   FillChar(Message,SizeOf(TMessage),0);
   Message.Msg:=PtrUInt(Request);
   Message.wParam:=INVALID_HANDLE_VALUE;
   Message.lParam:=DWC_STATUS_HOST_PORT_CHANGE;
   ThreadSendMessage(Host.CompletionThread,Message);
  end;
end;

{==============================================================================}

function DWCRootHubRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;
{Perform a request to the root hub}
{Host: The DWCOTG host for the request}
{Request: The USB request to perform}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the Host lock} 
var
 Message:TMessage;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 
 {Check Endpoint}
 if Request.Endpoint = nil then
  begin
   {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Performing request to root hub default endpoint');
   {$ENDIF}
   
   {Control request (Default Endpoint)}
   Request.Status:=DWCRootHubControlRequest(Host,Request);
   
   {Send to the Completion thread}
   FillChar(Message,SizeOf(TMessage),0);
   Message.Msg:=PtrUInt(Request);
   Message.wParam:=INVALID_HANDLE_VALUE;
   Message.lParam:=DWC_STATUS_ROOT_HUB_REQUEST;
   ThreadSendMessage(Host.CompletionThread,Message);
   
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
  end
 else
  begin
   {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Registering status change request to root hub');
   {$ENDIF}
   
   {Check Request}
   if Host.HubStatusChange = nil then
    begin
     {Interrupt request (Status Endpoint)}
     Host.HubStatusChange:=Request;
     if Host.PortStatus.wPortChange <> 0 then
      begin
       DWCHostPortStatusChanged(Host);
      end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Cannot register multiple status change requests for root hub');
     
     {Update Request}
     Request.Status:=USB_STATUS_UNSUPPORTED_REQUEST;
     
     {Send to the Completion thread}
     FillChar(Message,SizeOf(TMessage),0);
     Message.Msg:=PtrUInt(Request);
     Message.wParam:=INVALID_HANDLE_VALUE;
     Message.lParam:=DWC_STATUS_HOST_PORT_CHANGE;
     ThreadSendMessage(Host.CompletionThread,Message);
    end;
    
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
  end;
end;

{==============================================================================}

function DWCRootHubControlRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;
{Perform a control request to or from the root hub}
{Host: The DWCOTG host for the request}
{Request: The USB request to perform}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the Host lock} 
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.SetupData = nil then Exit;
 
 Result:=USB_STATUS_UNSUPPORTED_REQUEST;
 
 {Check Request Type}
 case (Request.SetupData.bmRequestType and USB_BMREQUESTTYPE_TYPE_MASK) of
  USB_BMREQUESTTYPE_TYPE_CLASS:begin
    {Class Request}
    Result:=DWCRootHubClassRequest(Host,Request);
   end;
  USB_BMREQUESTTYPE_TYPE_STANDARD:begin
    {Standard Request}
    Result:=DWCRootHubStandardRequest(Host,Request);
   end;
 end;
end;

{==============================================================================}

function DWCRootHubClassRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;
{Perform a hub specific control request to the root hub}
{Host: The DWCOTG host for the request}
{Request: Hub specific request to the root hub}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the Host lock} 
var
 Len:Word;
 Setup:PUSBControlSetupData;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.SetupData = nil then Exit;
 
 {Get Setup Data}
 Setup:=Request.SetupData;
 
 Result:=USB_STATUS_UNSUPPORTED_REQUEST;
 
 {Check Request}
 case Setup.bRequest of
  USB_HUB_REQUEST_GET_DESCRIPTOR:begin
    {Get Hub Descriptor}
    {Check Descriptor Type}
    case (Setup.wValue shr 8) of
     USB_DESCRIPTOR_TYPE_HUB:begin
       {GetHubDescriptor (11.24.2)}
       Result:=USB_STATUS_INVALID_PARAMETER;
       
       {Check Data}
       if Request.Data = nil then Exit;
       
       {Check Descriptor}
       if Host.HubDescriptor = nil then Exit;
       
       {Get Length}
       Len:=Setup.wLength;
       if Host.HubDescriptor.bDescLength < Setup.wLength then Len:=Host.HubDescriptor.bDescLength; //To Do //Add Min() function somewhere
       
       {Copy Descriptor}
       System.Move(Host.HubDescriptor^,Request.Data^,Len);
       
       {Update Request}
       Request.ActualSize:=Len;
       
       {Return Result}
       Result:=USB_STATUS_SUCCESS;
      end;     
    end;
   end;
  USB_HUB_REQUEST_GET_STATUS:begin
    {Get Hub Status}
    {Check Request Type}
    case (Setup.bmRequestType and USB_BMREQUESTTYPE_RECIPIENT_MASK) of
     USB_BMREQUESTTYPE_RECIPIENT_DEVICE:begin
       {GetHubStatus (11.24.2)}
       Result:=USB_STATUS_INVALID_PARAMETER;
       
       {Check Data}
       if Request.Data = nil then Exit;
       
       {Check Status}
       if Host.HubStatus = nil then Exit;
       
       {Check Length}
       if Setup.wLength >= SizeOf(TUSBHubStatus) then
        begin
         {Copy Status}
         System.Move(Host.HubStatus^,Request.Data^,SizeOf(TUSBHubStatus));
         
         {Update Request}
         Request.ActualSize:=SizeOf(TUSBHubStatus);
         
         {Return Result}
         Result:=USB_STATUS_SUCCESS;
        end;
      end;
     USB_BMREQUESTTYPE_RECIPIENT_OTHER:begin
       {GetPortStatus (11.24.2)}
       Result:=USB_STATUS_INVALID_PARAMETER;
       
       {Check Data}
       if Request.Data = nil then Exit;
       
       {Check Status}
       if Host.PortStatus = nil then Exit;
       
       {Check Length}
       if Setup.wLength >= SizeOf(TUSBPortStatus) then
        begin
         {Copy Status}
         System.Move(Host.PortStatus^,Request.Data^,SizeOf(TUSBPortStatus));
         
         {Update Request}
         Request.ActualSize:=SizeOf(TUSBPortStatus);
         
         {Return Result}
         Result:=USB_STATUS_SUCCESS;
        end;
      end;     
    end;
   end;
  USB_HUB_REQUEST_SET_FEATURE:begin
    {Set Hub Feature}
    {Check Request Type}
    case (Setup.bmRequestType and USB_BMREQUESTTYPE_RECIPIENT_MASK) of
     USB_BMREQUESTTYPE_RECIPIENT_DEVICE:begin
       {SetHubFeature (11.24.2)}
      end;
     USB_BMREQUESTTYPE_RECIPIENT_OTHER:begin
       {SetPortFeature (11.24.2)}
       Result:=DWCHostPortSetFeature(Host,Setup.wValue);
      end;     
    end;
   end;
  USB_HUB_REQUEST_CLEAR_FEATURE:begin
    {Clear Hub Feature}
    {Check Request Type}
    case (Setup.bmRequestType and USB_BMREQUESTTYPE_RECIPIENT_MASK) of
     USB_BMREQUESTTYPE_RECIPIENT_DEVICE:begin
       {ClearHubFeature (11.24.2)}
      end;
     USB_BMREQUESTTYPE_RECIPIENT_OTHER:begin
       {ClearPortFeature (11.24.2)}
       Result:=DWCHostPortClearFeature(Host,Setup.wValue);
      end;     
    end;
   end;  
 end; 
end;

{==============================================================================}

function DWCRootHubStandardRequest(Host:PDWCUSBHost;Request:PUSBRequest):LongWord;
{Perform a standard (non hub specific) control request to the root hub}
{Host: The DWCOTG host for the request}
{Request: Standard request to the root hub}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the Host lock} 
var
 Len:Word;
 Setup:PUSBControlSetupData;
 Descriptor:PUSBStringDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.SetupData = nil then Exit;
 
 {Get Setup Data}
 Setup:=Request.SetupData;
 
 Result:=USB_STATUS_UNSUPPORTED_REQUEST;
 
 {Check Request Type}
 case Setup.bRequest of
  USB_DEVICE_REQUEST_GET_STATUS:begin
    {Get Status}
    Result:=USB_STATUS_INVALID_PARAMETER;
       
    {Check Data}
    if Request.Data = nil then Exit;
    
    {Check Status}
    if Host.DeviceStatus = nil then Exit;
    
    {Get Length}
    Len:=SizeOf(TUSBDeviceStatus);
    if Setup.wLength < SizeOf(TUSBDeviceStatus) then Len:=Setup.wLength;  //To Do //Add Min() function somewhere
    
    {Copy Status}
    System.Move(Host.DeviceStatus^,Request.Data^,Len);
    
    {Update Request}
    Request.ActualSize:=Len;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;  
  USB_DEVICE_REQUEST_SET_ADDRESS:begin
    {Set Address}
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  USB_DEVICE_REQUEST_GET_DESCRIPTOR:begin
    {Get Descriptor}
    {Check Descriptor Type}
    case (Setup.wValue shr 8) of
     USB_DESCRIPTOR_TYPE_DEVICE:begin
       {Get Device Descriptor}
       Result:=USB_STATUS_INVALID_PARAMETER;
       
       {Check Data}
       if Request.Data = nil then Exit;
       
       {Check Descriptor}
       if Host.DeviceDescriptor = nil then Exit;
       
       {Get Length}
       Len:=Host.DeviceDescriptor.bLength;
       if Setup.wLength < Host.DeviceDescriptor.bLength then Len:=Setup.wLength;  //To Do //Add Min() function somewhere
       
       {Copy Descriptor}
       System.Move(Host.DeviceDescriptor^,Request.Data^,Len);
       
       {Update Request}
       Request.ActualSize:=Len;
       
       {Return Result}
       Result:=USB_STATUS_SUCCESS;
      end;
     USB_DESCRIPTOR_TYPE_CONFIGURATION:begin
       {Get Configuration Descriptor}
       Result:=USB_STATUS_INVALID_PARAMETER;
       
       {Check Data}
       if Request.Data = nil then Exit;
       
       {Check Descriptor}
       if Host.HubConfiguration = nil then Exit;
       
       {Get Length}
       Len:=Host.HubConfiguration.ConfigurationDescriptor.wTotalLength;
       if Setup.wLength < Host.HubConfiguration.ConfigurationDescriptor.wTotalLength then Len:=Setup.wLength;  //To Do //Add Min() function somewhere
       
       {Copy Descriptor}
       System.Move(Host.HubConfiguration.ConfigurationDescriptor,Request.Data^,Len); 
       
       {Update Request}
       Request.ActualSize:=Len;
       
       {Return Result}
       Result:=USB_STATUS_SUCCESS;
      end;
     USB_DESCRIPTOR_TYPE_STRING:begin
       {Get String Descriptor}
       Result:=USB_STATUS_INVALID_PARAMETER;
       
       {Check Data}
       if Request.Data = nil then Exit;
       
       {Check Index (low byte of wValue)}
       if ((Setup.wValue and $FF) <= High(Host.HubStringTable)) then
        begin
         {Get Descriptor}
         Descriptor:=Host.HubStringTable[Setup.wValue and $FF];
         if Descriptor = nil then Exit;
         
         {Get Length}
         Len:=Descriptor.bLength;
         if Setup.wLength < Descriptor.bLength then Len:=Setup.wLength;  //To Do //Add Min() function somewhere
         
         {Copy Descriptor}
         System.Move(Descriptor^,Request.Data^,Len);
         
         {Update Request}
         Request.ActualSize:=Len;
         
         {Return Result}
         Result:=USB_STATUS_SUCCESS;
        end;
      end;
    end;
   end;
  USB_DEVICE_REQUEST_GET_CONFIGURATION:begin
    {Get Configuration}
    Result:=USB_STATUS_INVALID_PARAMETER;
    
    {Check Data}
    if Request.Data = nil then Exit;
    
    {Check Length}
    if Setup.wLength >= 1 then
     begin
      {Copy Configuration}
      System.Move(Request.Device.ConfigurationValue,Request.Data^,1);
      
      {Update Request}
      Request.ActualSize:=1;
     end;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  USB_DEVICE_REQUEST_SET_CONFIGURATION:begin
    {Set Configuration}
    if Setup.wValue <= 1 then
     begin
      {Return Result}
      Result:=USB_STATUS_SUCCESS;
     end;
   end;  
 end;
end; 

{==============================================================================}

function DWCSchedulerStart(Host:PDWCUSBHost):LongWord;
{Initialize a bitmask and semaphore that keep track of the Free/Used status
 of the host channels and a queue in which to place submitted USB transfer
 requests, then start the USB request scheduler thread}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Channel Count}
 if DWC_NUM_CHANNELS > (8 * SizeOf(LongWord)) then Exit;
 
 {Create Channel Free Lock}
 Host.ChannelFreeLock:=MutexCreate;
 if Host.ChannelFreeLock = INVALID_HANDLE_VALUE then 
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to create channel free lock');
   
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 {Create Channel Free Semaphore}
 Host.ChannelFreeWait:=SemaphoreCreate(DWC_NUM_CHANNELS);
 if Host.ChannelFreeWait = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to create channel free semaphore');
   
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 {Setup Channel Free Mask}
 Host.ChannelFreeMask:=(1 shl DWC_NUM_CHANNELS) - 1;
 
 {Create Start of Frame Lock}
 Host.StartOfFrameLock:=SpinCreate;
 if Host.StartOfFrameLock = INVALID_HANDLE_VALUE then 
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to create start of frame lock');
   
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 {Setup Start of Frame Mask}
 Host.StartOfFrameMask:=0;
 
 {Create Scheduler Mailslot}
 Host.SchedulerMailslot:=MailslotCreate(1024); //To Do //Make this a constant
 if Host.SchedulerMailslot = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to create scheduler mailslot');
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 //To Do
  
 {Create Scheduler Thread}
 Host.SchedulerThread:=BeginThread(TThreadStart(DWCSchedulerExecute),Host,Host.SchedulerThread,DWC_SCHEDULER_THREAD_STACK_SIZE);
 if Host.SchedulerThread = INVALID_HANDLE_VALUE then 
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to create scheduler thread');
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end
 else
  begin
   ThreadSetPriority(Host.SchedulerThread,DWC_SCHEDULER_THREAD_PRIORITY);
   ThreadSetName(Host.SchedulerThread,DWC_SCHEDULER_THREAD_NAME);
  end;  
  
 {Create Completion Thread}
 Host.CompletionThread:=BeginThread(TThreadStart(DWCCompletionExecute),Host,Host.CompletionThread,DWC_COMPLETION_THREAD_STACK_SIZE);
 if Host.CompletionThread = INVALID_HANDLE_VALUE then 
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Failed to create completion thread');
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end
 else
  begin
   ThreadSetPriority(Host.CompletionThread,DWC_COMPLETION_THREAD_PRIORITY);
   ThreadSetName(Host.CompletionThread,DWC_COMPLETION_THREAD_NAME);
  end;  
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCSchedulerExecute(Host:PDWCUSBHost):PtrInt;
{USB request scheduler thread

 This thread receives requests that have been submitted and schedules them on
 the next available channel.
 
 This is a very simplistic scheduler that does not take into account bandwidth
 requirements or which endpoint a transfer is for}
 
{Host: USB host to service submitted requests for} 
var
 Message:TMessage;
 Channel:LongWord;
 Request:PUSBRequest;
 ResultCode:LongWord;
begin
 {}
 Result:=0;
 try
  {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
  if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Scheduler thread ID = ' + IntToHex(ThreadID,8)); 
  {$ENDIF}
  
  {Check Host}
  if Host = nil then Exit;
  
  while True do
   begin
    {Get Request}
    Request:=PUSBRequest(MailslotReceive(Host.SchedulerMailslot));
    if PtrInt(Request) <> PtrInt(INVALID_HANDLE_VALUE) then
     begin
      {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Scheduler received message (Request=' + PtrToHex(Request) + ')');
      {$ENDIF}
      
      {Update Statistics}
      Inc(Host.Host.RequestCount); 
 
      if USBIsRootHub(Request.Device) then
       begin
        {Perform a request to the root hub}
        {Acquire the Lock}
        if DWCOTG_FIQ_ENABLED then
         begin
          ResultCode:=SpinLockIRQFIQ(Host.Lock);
         end
        else
         begin
          ResultCode:=SpinLockIRQ(Host.Lock);
         end;  
        if ResultCode = ERROR_SUCCESS then
         begin
          try
           {Check Request}
           if Request.Status = USB_STATUS_NOT_PROCESSED then
            begin
             {Update Request}
             Request.Status:=USB_STATUS_NOT_COMPLETED;
             
             {Perform Request}
             DWCRootHubRequest(Host,Request);
            end
           else
            begin
             {Send to the Completion thread}
             FillChar(Message,SizeOf(TMessage),0);
             Message.Msg:=PtrUInt(Request);
             Message.wParam:=INVALID_HANDLE_VALUE;
             Message.lParam:=DWC_STATUS_INVALID;
             ThreadSendMessage(Host.CompletionThread,Message);
            end;
          finally
           {Release the Lock}
           if DWCOTG_FIQ_ENABLED then
            begin
             SpinUnlockIRQFIQ(Host.Lock);
            end
           else
            begin
             SpinUnlockIRQ(Host.Lock);
            end;     
          end;   
         end;
       end
      else
       begin
        {Schedule the request on a channel}
        {Get Channel}
        Channel:=DWCAllocateChannel(Host);
        if Channel <> LongWord(INVALID_HANDLE_VALUE) then
         begin
          {Acquire the Lock}
          if DWCOTG_FIQ_ENABLED then
           begin
            ResultCode:=SpinLockIRQFIQ(Host.Lock);
           end
          else
           begin
            ResultCode:=SpinLockIRQ(Host.Lock);
           end;  
          if ResultCode = ERROR_SUCCESS then
           begin
            try
             {Check Request}
             if Request.Status = USB_STATUS_NOT_PROCESSED then
              begin
               {Update Request}
               Request.Status:=USB_STATUS_NOT_COMPLETED;
             
               {Start Transfer}
               DWCChannelStartTransfer(Host,Channel,Request);
              end
             else
              begin
               {Send to the Completion thread}
               FillChar(Message,SizeOf(TMessage),0);
               Message.Msg:=PtrUInt(Request);
               Message.wParam:=Channel;
               Message.lParam:=DWC_STATUS_INVALID;
               ThreadSendMessage(Host.CompletionThread,Message);
              end;
            finally
             {Release the Lock}
             if DWCOTG_FIQ_ENABLED then
              begin
               SpinUnlockIRQFIQ(Host.Lock);
              end
             else
              begin
               SpinUnlockIRQ(Host.Lock);
              end;     
            end;   
           end;
         end
        else
         begin
          {Update Request}
          Request.Status:=USB_STATUS_OPERATION_FAILED;

          {Send to the Completion thread}
          FillChar(Message,SizeOf(TMessage),0);
          Message.Msg:=PtrUInt(Request);
          Message.wParam:=Channel;
          Message.lParam:=DWC_STATUS_INVALID;
          ThreadSendMessage(Host.CompletionThread,Message);
         end;         
       end;    
     end
    else 
     begin
      if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Scheduler mailslot receive failed'); 
     end;    
   end;
 except
  on E: Exception do
   begin
    if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: SchedulerThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end; 
end;

{==============================================================================}

function DWCCompletionExecute(Host:PDWCUSBHost):PtrInt;
{USB request completion thread
 
 This thread receives completed requests which have either succeeded or failed 
 and calls the completion handler which will call the registered callback for
 the request.
 
 This thread also receives requests that need to be resubmitted and resubmits
 them for later processing by another thread}

{Host: USB host to service completed requests for}

{Message contents are as follows:
 
  Msg = Request to be completed
  wParam = Channel that the request executed on (or INVALID_HANDLE_VALUE if no channel was allocated)
  lParam = Status of the channel interrupt
 
}
var
 Status:LongWord;
 Channel:LongWord;
 Message:TMessage;
 Request:PUSBRequest;
 InterruptStatus:LongWord;
begin
 {}
 Result:=0;
 try
  {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
  if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Completion thread ID = ' + IntToHex(ThreadID,8)); 
  {$ENDIF}
 
  {Check Host}
  if Host = nil then Exit;
  
  while True do
   begin
    {Get Message}
    FillChar(Message,SizeOf(TMessage),0);
    if ThreadReceiveMessage(Message) = ERROR_SUCCESS then
     begin
      {Get Request}
      Request:=PUSBRequest(Message.Msg);
      Channel:=Message.wParam;
      InterruptStatus:=Message.lParam;
      
      {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Completion received message (Request=' + PtrToHex(Request) + ' Channel=' + IntToHex(Channel,8) + ' Status=' + IntToHex(InterruptStatus,8) + ')');
      {$ENDIF}
      
      {Check Request}
      if Request <> nil then
       begin
        {Check Channel}
        if Channel <> LongWord(INVALID_HANDLE_VALUE) then
         begin
          {Release Channel}
          Host.ChannelRequests[Channel]:=nil;
          DWCReleaseChannel(Host,Channel);
         end; 
        
        {Check Status}
        case InterruptStatus of 
         DWC_STATUS_HOST_PORT_CHANGE,DWC_STATUS_ROOT_HUB_REQUEST,DWC_STATUS_INVALID,DWC_STATUS_CANCELLED:begin
           {Update Statistics}
           if Request.Status <> USB_STATUS_SUCCESS then Inc(Host.Host.RequestErrors);
           
           {Complete the request}
           USBRequestComplete(Request);
          end;
         DWC_STATUS_TRANSFER_RESUBMIT:begin
           {If we got here because we received a NAK or NYET, resubmit the request for a later time}
           Status:=DWCHostResubmit(Host,Request);
           if Status <> USB_STATUS_SUCCESS then
            begin
             Request.Status:=Status;
 
             {Update Statistics}
             if Request.Status <> USB_STATUS_SUCCESS then Inc(Host.Host.RequestErrors);
             
             {Complete the request}
             USBRequestComplete(Request);
            end;    
          end;         
         else
          begin
           {Set the actual transferred size, unless we are doing a control transfer and aren't on the DATA phase}
           if not(USBIsControlRequest(Request)) or (Request.ControlPhase = USB_CONTROL_PHASE_DATA) then
            begin
             Request.ActualSize:=PtrUInt(Request.CurrentData) - PtrUInt(Request.Data);
            end;
          
           {Update Statistics}
           if Request.Status <> USB_STATUS_SUCCESS then Inc(Host.Host.RequestErrors);
 
           {Complete the request}
           USBRequestComplete(Request);
          end;
        end;  
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Completion invalid request'); 
       end;
     end
    else
     begin
      if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: Completion receive message failed'); 
     end;    
   end;
 except
  on E: Exception do
   begin
    if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: CompletionThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end; 
end;
 
{==============================================================================}

function DWCResubmitExecute(Request:PUSBRequest):PtrInt;
{USB request resubmit thread

 An instance of this thread is created for each request that needs to be resubmitted,
 this thread then either waits for a predetermined number of milliseconds before
 scheduling the request on the next available channel.
 
 Once the request has been resubmitted the thread waits for the semaphore to be
 signaled again. If the request is a periodic polling of an endpoint then it will
 be resubmitted continuously at the predetermined interval, otherwise the semaphore
 will be destroyed by the completion handler and the thread will terminate}
 
{Request: USB request to resubmit}
var
 Host:PDWCUSBHost;
 Channel:LongWord;
 Message:TMessage;
 Interval:LongWord;
 ResultCode:LongWord;
begin
 {}
 Result:=0;
 try
  {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)} 
  if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Resubmit thread ID = ' + IntToHex(ThreadID,8)); 
  {$ENDIF}                     
  
  {Check Request}
  if Request = nil then Exit;
  if Request.Device = nil then Exit;
  
  {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Resubmit request = ' + PtrToHex(Request)); 
  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Resubmit endpoint descriptor = ' + PtrToHex(Request.Endpoint)); 
  if USB_LOG_ENABLED and (Request.Endpoint <> nil) then USBLogDebug(Request.Device,'DWCOTG: Resubmit endpoint interval = ' + IntToHex(Request.Endpoint.bInterval,8)); 
  {$ENDIF}
 
  {Check Endpoint}
  Interval:=0;
  if Request.Endpoint <> nil then
   begin
    {Check Speed}
    if Request.Device.Speed = USB_SPEED_HIGH then
     begin
      Interval:=(1 shl (Request.Endpoint.bInterval - 1)) div USB_UFRAMES_PER_MS;
     end
    else
     begin
      Interval:=Request.Endpoint.bInterval div USB_FRAMES_PER_MS;
     end;
   end
  else
   begin
    Interval:=10;
   end;   
  {Check Interval}
  if Interval < 1 then
   begin
    Interval:=1;
   end;  
 
  {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Resubmit milliseconds = ' + IntToStr(Interval)); 
  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Resubmit StartOfFrame = ' + BooleanToString(Request.StartOfFrame)); 
  {$ENDIF}
  
  {Get Host}
  Host:=PDWCUSBHost(Request.Device.Host);
  if Host = nil then Exit;
 
  while True do
   begin
    {Wait for the Resubmit Semaphore}
    if SemaphoreWait(Request.ResubmitSemaphore) <> ERROR_SUCCESS then Break; {Break to terminate thread}
     
    {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Resubmit waiting ' + IntToStr(Interval) + 'ms to start request again');
    {$ENDIF}
    
    {Wait Milliseconds}
    ThreadSleep(Interval);
    
    {Get Channel}
    Channel:=DWCAllocateChannel(Host);
    if Channel <> LongWord(INVALID_HANDLE_VALUE) then
     begin
      {Acquire the Lock}
      if DWCOTG_FIQ_ENABLED then
       begin
        ResultCode:=SpinLockIRQFIQ(Host.Lock);
       end
      else
       begin
        ResultCode:=SpinLockIRQ(Host.Lock);
       end;  
      if ResultCode = ERROR_SUCCESS then
       begin
        try
         {Check Request}
         if Request.Status = USB_STATUS_NOT_COMPLETED then
          begin
           {Start Transfer}
           DWCChannelStartTransfer(Host,Channel,Request);
          end 
         else
          begin
           {Send to the Completion thread}
           FillChar(Message,SizeOf(TMessage),0);
           Message.Msg:=PtrUInt(Request);
           Message.wParam:=Channel;
           Message.lParam:=DWC_STATUS_INVALID;
           ThreadSendMessage(Host.CompletionThread,Message);
          end;
        finally
         {Release the Lock}
         if DWCOTG_FIQ_ENABLED then
          begin
           SpinUnlockIRQFIQ(Host.Lock);
          end
         else
          begin
           SpinUnlockIRQ(Host.Lock);
          end;     
         end;   
       end;
     end
    else
     begin
      {Update Request}
      Request.Status:=USB_STATUS_OPERATION_FAILED;

      {Send to the Completion thread}
      FillChar(Message,SizeOf(TMessage),0);
      Message.Msg:=PtrUInt(Request);
      Message.wParam:=Channel;
      Message.lParam:=DWC_STATUS_INVALID;
      ThreadSendMessage(Host.CompletionThread,Message);
     end;         
   end;  
   
  {$IF DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)}
  if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Resubmit thread terminating (Request = ' + PtrToHex(Request) + ')'); 
  {$ENDIF}               
 except
  on E: Exception do
   begin
    if USB_LOG_ENABLED then USBLogError(nil,'DWCOTG: ResubmitThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end; 
end;

{==============================================================================}

procedure DWCInterruptHandler(Host:PDWCUSBHost);
{Interrupt handler for the DWCOTG controller}
{Host: The DWC host where the interrupt occurred}
var
 Channel:LongWord;
 Message:TMessage;
 Request:PUSBRequest;
 ResultCode:LongWord;
 FrameNumber:LongWord;
 CoreInterrupts:LongWord;
 ChannelInterrupt:LongWord;
 HostFrameInterval:LongWord;
 PortSpeed:LongWord;
 ClockSelect:LongWord;
 RequireReset:Boolean;
 HostConfiguration:LongWord;
 CoreUSBConfiguration:LongWord; 
 HostPortControlStatus:LongWord;
begin
 {}
 {Check Host} 
 if Host = nil then Exit;
 
 {Acquire the Lock}
 if DWCOTG_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(Host.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(Host.Lock);
  end;  
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(Host.InterruptCount); 

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
 
    {Get Core Interrupts}
    CoreInterrupts:=Host.Registers.CoreInterrupts;

    {Check Start of Frame (SOF) Interrupt}
    if (CoreInterrupts and DWC_CORE_INTERRUPTS_SOF_INTR) <> 0 then
     begin
      {Update Statistics}
      Inc(Host.StartOfFrameInterruptCount);
   
      {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Received Start of Frame interrupt (HostFrameNumber=' + IntToHex(Host.Registers.HostFrameNumber,8) + ')');
      {$ENDIF}
      
      {Start of Frame (SOF) interrupt occurred}
      
      {Compare Last Frame Number}
      FrameNumber:=(Host.Registers.HostFrameNumber and DWC_HFNUM_FRAME_NUMBER_MASK);
      if FrameNumber <> Host.LastFrameNumber then
       begin
        {Save Frame Number}
        Host.LastFrameNumber:=FrameNumber;
        
        {Check Microframe Number}
        if (FrameNumber and $07) < 5 then
         begin
          if Host.StartOfFrameMask <> 0 then
           begin
            {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
            if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:  Waking up channel waiting for start of frame');
            {$ENDIF}
            
            {Acquire the Lock}
            if DWCOTG_FIQ_ENABLED then
             begin
              ResultCode:=SpinLockIRQFIQ(Host.StartOfFrameLock);
             end
            else
             begin
              ResultCode:=SpinLockIRQ(Host.StartOfFrameLock);
             end;  
            if ResultCode = ERROR_SUCCESS then
             begin
              try
               repeat
                {Get first channel waiting for SOF}
                Channel:=FirstBitSet(Host.StartOfFrameMask);
           
                {Get Request}
                Request:=Host.ChannelRequests[Channel];
                if Request <> nil then 
                 begin
                  {Clear Start of Frame}
                  Request.StartOfFrame:=False;
                   
                  {Restart Transaction}
                  DWCChannelStartTransaction(Host,Channel,Request);
                 end;
           
                {Remove the channel from the waiting mask}
                Host.StartOfFrameMask:=Host.StartOfFrameMask and not(1 shl Channel);
               until Host.StartOfFrameMask = 0; 
              finally
               {Release the Lock}
               if DWCOTG_FIQ_ENABLED then
                begin
                 SpinUnlockIRQFIQ(Host.StartOfFrameLock);
                end
               else
                begin
                 SpinUnlockIRQ(Host.StartOfFrameLock);
                end;     
              end;   
             end;
           end;
         end;
      
        {Check Start of Frame Mask}
        if Host.StartOfFrameMask = 0 then
         begin
          {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
          if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Disabling start of frame interrupt');
          {$ENDIF}
          
          {Disable Start of Frame Interrupt}
          Host.Registers.CoreInterruptMask:=Host.Registers.CoreInterruptMask and not(DWC_CORE_INTERRUPTS_SOF_INTR);
         end;
       end;  
     
      {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Clearing start of frame interrupt');
      {$ENDIF}

      {Clear Start of Frame Interrupt} 
      Host.Registers.CoreInterrupts:=DWC_CORE_INTERRUPTS_SOF_INTR;
     end;
 
    {Check Host Channel Interrupt}
    if (CoreInterrupts and DWC_CORE_INTERRUPTS_HOST_CHANNEL_INTR) <> 0 then
     begin
      {Update Statistics}
      Inc(Host.ChannelInterruptCount); 
   
      {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Channel interrupt detected');
      {$ENDIF}
      
      {One or more channels has an interrupt pending}
      
      {A bit in the "Host All Channels Interrupt Register" is set if an interrupt has occurred on the corresponding host channel. Process all set bits}
      ChannelInterrupt:=Host.Registers.HostChannelsInterrupt;
      repeat
       Channel:=FirstBitSet(ChannelInterrupt);
       DWCChannelInterrupt(Host,Channel);
       ChannelInterrupt:=ChannelInterrupt xor (1 shl Channel);
      until ChannelInterrupt = 0;
     end;

    {Check Host Port Interrupt}
    if (CoreInterrupts and DWC_CORE_INTERRUPTS_PORT_INTR) <> 0 then
     begin
      RequireReset:=False;
      
      {Update Statistics}
      Inc(Host.PortInterruptCount); 
      
      {Status of the host port changed, Update Host.PortStatus}
      {Get Host Port Control Status}
      HostPortControlStatus:=Host.Registers.HostPortControlStatus;
  
      {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Port interrupt detected: HostPortControlStatus=' + IntToHex(HostPortControlStatus,8));
      {$ENDIF}
      
      {Update RootHub Host Port Status} 
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_CONNECTED) <> 0 then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_CONNNECTED) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_CONNNECTED));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_ENABLED) <> 0 then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_ENABLED) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_ENABLED));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_SUSPENDED) <> 0 then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_SUSPENDED) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_SUSPENDED));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_OVERCURRENT) <> 0 then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_OVERCURRENT) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_OVERCURRENT));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_RESET) <> 0 then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_RESET) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_RESET));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_POWERED) <> 0 then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_POWERED) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_POWERED));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_SPEED) = (USB_SPEED_LOW shl 17) then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_LOW_SPEED_ATTACHED) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_LOW_SPEED_ATTACHED));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_SPEED) = (USB_SPEED_HIGH shl 17) then Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus or USB_PORT_STATUS_HIGH_SPEED_ATTACHED) else Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_HIGH_SPEED_ATTACHED));
      
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_CONNECTED_CHANGED) <> 0 then Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange or USB_PORT_CHANGE_CONNECTED) else Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_CONNECTED));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_ENABLED_CHANGED) <> 0 then Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange or USB_PORT_CHANGE_ENABLED) else Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_ENABLED));
      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_OVERCURRENT_CHANGED) <> 0 then Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange or USB_PORT_CHANGE_OVERCURRENT) else Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange and not(USB_PORT_CHANGE_OVERCURRENT));
 
      {Check Port Enabled Change}
      if (Host.PortStatus.wPortChange and USB_PORT_CHANGE_ENABLED) = USB_PORT_CHANGE_ENABLED then
       begin
        {Check Port Status Connected}
        (*if (Host.PortStatus.wPortStatus and USB_PORT_STATUS_CONNNECTED) <> USB_PORT_STATUS_CONNNECTED then
         begin
          {Force Port Connnected Change}
          Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange or USB_PORT_CHANGE_CONNECTED);
         end;*)
         
        {Check Port Status Enabled}
        if (Host.PortStatus.wPortStatus and USB_PORT_STATUS_ENABLED) = USB_PORT_STATUS_ENABLED then
         begin
          {Check Host Frame Interval}
          if DWCOTG_HOST_FRAME_INTERVAL then
           begin
            {Calculate Host Frame Interval}
            HostFrameInterval:=Host.Registers.HostFrameInterval;
            HostFrameInterval:=(HostFrameInterval and not(DWC_HFIR_FRAME_INTERVAL_MASK)) or DWCCalculateFrameInterval(Host);
            Host.Registers.HostFrameInterval:=HostFrameInterval;
           end; 
         
          {Check FS/LS Low Power Clock}
          if DWCOTG_FS_LS_LOW_POWER_CLOCK then
           begin
            {Get USB Configuration}
            CoreUSBConfiguration:=Host.Registers.CoreUSBConfiguration;
            
            {Check Port Speed}
            PortSpeed:=(HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_SPEED) shr 17;
            if (PortSpeed = USB_SPEED_LOW) or (PortSpeed = USB_SPEED_FULL) then
             begin
              {Set Low Power Clock Select}
              if (CoreUSBConfiguration and DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL) = 0 then
               begin
                CoreUSBConfiguration:=CoreUSBConfiguration or DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL;
                
                {Set USB Configuration}
                Host.Registers.CoreUSBConfiguration:=CoreUSBConfiguration;
                
                RequireReset:=True;
               end;
              
              {Get Host Configuration}
              HostConfiguration:=Host.Registers.HostConfiguration;
              
              {Get Clock Select}
              ClockSelect:=(HostConfiguration and DWC_HCFG_FS_LS_PHY_CLK_SEL_MASK) shr DWC_HCFG_FS_LS_PHY_CLK_SEL_SHIFT;
              
              {Check Port Speed}
              if (PortSpeed = USB_SPEED_LOW) and DWCOTG_LS_LOW_PWR_PHY_CLOCK_6MHZ then
               begin
                {Check Clock Select}
                if ClockSelect <> DWC_HCFG_FS_LS_PHY_CLK_SEL_6_MHZ then
                 begin
                  {Set 6MHz Clock}
                  ClockSelect:=DWC_HCFG_FS_LS_PHY_CLK_SEL_6_MHZ;
                  HostConfiguration:=HostConfiguration and not(DWC_HCFG_FS_LS_PHY_CLK_SEL_MASK);
                  HostConfiguration:=HostConfiguration or (ClockSelect shl DWC_HCFG_FS_LS_PHY_CLK_SEL_SHIFT);
                  
                  {Set Host Configuration}
                  Host.Registers.HostConfiguration:=HostConfiguration;
                  
                  RequireReset:=True;
                 end;
               end
              else
               begin
                {Check Clock Select}
                if ClockSelect <> DWC_HCFG_FS_LS_PHY_CLK_SEL_48_MHZ then
                 begin
                  {Set 48MHz Clock}
                  ClockSelect:=DWC_HCFG_FS_LS_PHY_CLK_SEL_48_MHZ;
                  HostConfiguration:=HostConfiguration and not(DWC_HCFG_FS_LS_PHY_CLK_SEL_MASK);
                  HostConfiguration:=HostConfiguration or (ClockSelect shl DWC_HCFG_FS_LS_PHY_CLK_SEL_SHIFT);
                  
                  {Set Host Configuration}
                  Host.Registers.HostConfiguration:=HostConfiguration;
                  
                  RequireReset:=True;
                 end;
               end;
             end
            else
             begin
              {Clear Low Power Clock Select}
              if (CoreUSBConfiguration and DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL) = DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL then
               begin
                CoreUSBConfiguration:=CoreUSBConfiguration and not(DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL);
                
                {Set USB Configuration}
                Host.Registers.CoreUSBConfiguration:=CoreUSBConfiguration;
                
                RequireReset:=True;
               end;
             end;
           end;
         end;
       end;
      
      {Clear the interrupt(s), which are "write-clear", by writing the Host Port Control and Status register back to itself. But as a special case, 'enabled' must be written as 0; otherwise the port will apparently disable itself}
      HostPortControlStatus:=(HostPortControlStatus and not(DWC_HOST_PORT_CTRLSTATUS_ENABLED));
      Host.Registers.HostPortControlStatus:=HostPortControlStatus;
   
      if RequireReset then
       begin
        {Reset Port}
        DWCHostPortReset(Host);
       end
      else 
       begin
        {Complete status change request to the root hub if one has been submitted}
        DWCHostPortStatusChanged(Host);
       end;
     end;
 
    {Check Host Disconnect Interrupt}
    if (CoreInterrupts and DWC_CORE_INTERRUPTS_DISCONNECT) <> 0 then
     begin
      {Update Statistics}
      Inc(Host.DisconnectInterruptCount); 
      
      {Check Host Port Control Status}
      HostPortControlStatus:=Host.Registers.HostPortControlStatus;

      if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_CONNECTED) = 0 then
       begin
        {Update Host Port Status}
        Host.PortStatus.wPortChange:=(Host.PortStatus.wPortChange or USB_PORT_CHANGE_CONNECTED);
        Host.PortStatus.wPortStatus:=(Host.PortStatus.wPortStatus and not(USB_PORT_STATUS_CONNNECTED));
        
        {Complete status change request to the root hub if one has been submitted}
        DWCHostPortStatusChanged(Host);
       end;
       
      {Clear Disconnect Interrupt} 
      Host.Registers.CoreInterrupts:=DWC_CORE_INTERRUPTS_DISCONNECT;
     end; 
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read}
   finally
    {Release the Lock}
    if DWCOTG_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(Host.Lock);
     end
    else
     begin
      SpinUnlockIRQ(Host.Lock);
     end;     
   end;   
  end;
end;

{==============================================================================}

function DWCChannelInterrupt(Host:PDWCUSBHost;Channel:LongWord):LongWord;
{Handle a channel interrupt on the specified channel}
{Host: The DWC host where the interrupt occurred}
{Channel: DWC host channel where the channel interrupt occurred} 
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Only called by DWCInterruptHandler}          
{Note: Caller must hold the host lock}          
var
 Status:LongWord;
 Message:TMessage;
 Request:PUSBRequest;
 Interrupts:LongWord;
 HostChannel:PDWCHostChannel;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Channel}
 if Channel >= DWC_NUM_CHANNELS then Exit;
 
 {Get Request}
 Request:=Host.ChannelRequests[Channel];
 if Request = nil then Exit;
 
 {Get Host Channel}
 HostChannel:=@Host.Registers.HostChannels[Channel];
 
 {Get Channel Interrupts}
 Interrupts:=HostChannel.Interrupts;
 
 {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Handling channel ' + IntToStr(Channel) + ' halted interrupt');
 if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG:   (Interrupts=' + IntToHex(Interrupts,8) + ', Characteristics=' + IntToHex(HostChannel.Characteristics,8) + ', Transfer=' + IntToHex(HostChannel.Transfer,8) + ')');
 {$ENDIF}
 
 {Check Request Status}
 if Request.Status <> USB_STATUS_NOT_COMPLETED then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Request status invalid (Status=' + USBStatusToString(Request.Status) + ')');
   {$ENDIF}
  
   {Request status invalid, transfer cancelled}
   Status:=DWC_STATUS_CANCELLED;
   
   {Update Statistics}
   Inc(Host.RequestCancelCount); 
  end
 {Check the Interrupts for errors}
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_STALL_RESPONSE_RECEIVED) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Hardware stall on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   {Hardware stall, transfer failed}
   Status:=DWC_STATUS_STALLED;
   
   {Update Statistics}
   Inc(Host.StallResponseCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_AHB_ERROR) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: AHB error on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   {AHB error, transfer failed}
   Status:=DWC_STATUS_FAILED;
   
   {Update Statistics}
   Inc(Host.AHBErrorCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_TRANSACTION_ERROR) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Transaction error on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   if Request.IsSplit then
    begin
     {On a split transaction retry three times before failing}
     Inc(Request.SplitErrorCount);
     if Request.SplitErrorCount < DWC_SPLIT_ERROR_RETRIES then
      begin
       Request.CompleteSplit:=False;
       
       {Transaction error, resubmit the transfer}
       Status:=DWC_STATUS_TRANSFER_RESUBMIT;
      end
     else
      begin
       {Transaction error, transfer failed}
       Status:=DWC_STATUS_FAILED;
      end;
    end
   else
    begin   
     {Transaction error, transfer failed}
     Status:=DWC_STATUS_FAILED;
    end; 
   
   {Update Statistics}
   Inc(Host.TransactionErrorCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_BABBLE_ERROR) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Babble error on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   {Babble error, transfer failed}
   Status:=DWC_STATUS_FAILED;
   
   {Update Statistics}
   Inc(Host.BabbleErrorCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_EXCESS_TRANSACTION_ERROR) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Excess transaction error on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   {Excess transaction error, transfer failed}
   Status:=DWC_STATUS_FAILED;
   
   {Update Statistics}
   Inc(Host.ExcessTransactionCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_FRAME_LIST_ROLLOVER) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Frame list rollover error on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   {Frame list rollover error, transfer failed}
   Status:=DWC_STATUS_FAILED;
   
   {Update Statistics}
   Inc(Host.FrameListRolloverCount); 
  end
 else if ((Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_NYET_RESPONSE_RECEIVED) <> 0) and not(Request.CompleteSplit) then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: NYET response on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   {NYET response when not complete split, transfer failed}
   Status:=DWC_STATUS_FAILED;
   
   {Update Statistics}
   Inc(Host.NYETResponseCount); 
  end
 else if ((Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_DATA_TOGGLE_ERROR) <> 0) and ((HostChannel.Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_DIRECTION) = (USB_DIRECTION_OUT shl 15)) then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Data toggle error on channel ' + IntToStr(Channel) + ' (Interrupts=' + IntToHex(Interrupts,8) + ', TRANSFER_PACKET_COUNT=' + IntToStr((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19) + ')');
   {$ENDIF}

   {Data toggle error on an OUT request, transfer failed}
   Status:=DWC_STATUS_FAILED;
   
   {Update Statistics}
   Inc(Host.DataToggleErrorCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_FRAME_OVERRUN) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Frame overrun on channel ' + IntToStr(Channel) + ', restarting transaction');
   {$ENDIF}

   {Frame overrun error, restart transaction}
   Status:=DWC_STATUS_TRANSACTION_RESTART;
   
   {Update Statistics}
   Inc(Host.FrameOverrunCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_NYET_RESPONSE_RECEIVED) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: NYET response on channel ' + IntToStr(Channel));
   {$ENDIF}
   
   {$IFDEF USB_DEBUG}
   Inc(Request.CompleteSplitNYETs);
   {$ENDIF}
   
   {Device sent NYET packet when completing a split transaction. Try the CompleteSplit again later.  As a special case, if too many NYETs are received, restart the entire split transaction.
    (Apparently, because of frame overruns or some other reason it's possible for NYETs to be issued indefinitely until the transaction is retried.)}
   Inc(Request.CompleteSplitRetries);
   if Request.CompleteSplitRetries >= DWC_COMPLETE_SPLIT_RETRIES then 
    begin
     {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Restarting split transaction (CompleteSplit tried ' + IntToStr(Request.CompleteSplitRetries) + ' times)');
     {$ENDIF}
     
     Request.CompleteSplit:=False;
     
     {$IFDEF USB_DEBUG}
     Inc(Request.CompleteSplitRestarts);
     {$ENDIF}
     
     {Update Statistics}
     Inc(Host.CompleteSplitRestartCount);
     
     {NYET response on a complete split, resubmit the transfer}
     Status:=DWC_STATUS_TRANSFER_RESUBMIT; 
    end
   else
    begin
     {NYET response on a complete split, restart transaction} 
     Status:=DWC_STATUS_TRANSACTION_RESTART; 
    end;    
   
   {Update Statistics}
   Inc(Host.NYETResponseCount); 
  end
 else if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_NAK_RESPONSE_RECEIVED) <> 0 then
  begin
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: NAK response on channel ' + IntToStr(Channel));
   {$ENDIF}
   
   {$IFDEF USB_DEBUG}
   if Request.IsSplit then
    begin
     if not(Request.CompleteSplit) then Inc(Request.StartSplitNAKs) else Inc(Request.CompleteSplitNAKs);
    end;
   {$ENDIF}
   
   {Device sent NAK packet. This happens when the device had no data to send at this time. Try again later. Special case: if the NAK was sent during a Complete Split transaction,
    restart with the Start Split, not the Complete Split}
   Request.CompleteSplit:=False;
   
   {NAK response, resubmit the transfer}
   Status:=DWC_STATUS_TRANSFER_RESUBMIT;
   
   {Update Statistics}
   Inc(Host.NAKResponseCount); 
  end
 else 
  begin
   {No error occurred}
   Status:=DWCChannelCompleted(Host,Request,Channel,Interrupts);
  end;
  
 {Check Status}
 case Status of
  DWC_STATUS_SUCCESS:begin
    Request.Status:=USB_STATUS_SUCCESS;
   end;
  DWC_STATUS_STALLED:begin
    Request.Status:=USB_STATUS_HARDWARE_STALL;
   end;  
  DWC_STATUS_FAILED:begin
    Request.Status:=USB_STATUS_HARDWARE_ERROR;
   end;
  DWC_STATUS_TRANSFER_RESUBMIT:begin
    {Nothing}
   end;
  DWC_STATUS_TRANSFER_RESTART:begin
    if Request.IsSplit then
     begin
      {Set Start of Frame}
      Request.StartOfFrame:=True; 
     
      {Update Statistics}
      Inc(Host.StartOfFrameCount);
     end;
     
    DWCChannelStartTransfer(Host,Channel,Request);
    Exit;
   end;
  DWC_STATUS_TRANSACTION_RESTART:begin
    if Request.IsSplit then 
     begin
      {Set Start of Frame}
      Request.StartOfFrame:=True; 
     
      {Update Statistics}
      Inc(Host.StartOfFrameCount);
     end;
  
    DWCChannelStartTransaction(Host,Channel,Request);
    Exit;
   end;
  DWC_STATUS_CANCELLED:begin
    Request.Status:=USB_STATUS_CANCELLED;
   end;
 end;
 
 {Transfer complete, transfer encountered an error, or transfer needs to be retried later}
 {Save the data packet ID}
 Request.NextDataPID:=((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_ID) shr 29);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Clear and disable interrupts on this channel}
 HostChannel.InterruptMask:=0;
 HostChannel.Interrupts:=$FFFFFFFF;

 {Send to the Completion thread}
 FillChar(Message,SizeOf(TMessage),0);
 Message.Msg:=PtrUInt(Request);
 Message.wParam:=Channel;
 Message.lParam:=Status;
 ThreadSendMessage(Host.CompletionThread,Message);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function DWCChannelCompleted(Host:PDWCUSBHost;Request:PUSBRequest;Channel,Interrupts:LongWord):LongWord;
{Handle a channel interrupt where no error occurred}
{Request: The USB request currently scheduled on this channel}
{Host: The DWC host where the interrupt occurred}
{Channel: DWC host channel where the channel interrupt occurred} 
{Interrupts: The currently pending interrupts on this channel}

{Note: Only called by DWCChannelInterrupt}
{Note: Caller must hold the host lock}
var
 Direction:LongWord;
 TransferType:LongWord;
 MaxPacketSize:LongWord;
 Characteristics:LongWord;
 BytesTransferred:LongWord;
 PacketsRemaining:LongWord;
 PacketsTransferred:LongWord;
 HostChannel:PDWCHostChannel;
begin
 {}
 Result:=DWC_STATUS_FAILED;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;

 {Check Channel}
 if Channel >= DWC_NUM_CHANNELS then Exit;
 
 {Get Host Channel}
 HostChannel:=@Host.Registers.HostChannels[Channel];
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {The hardware seems to update transfer.packet_count as expected, so we can look at it before deciding whether to use transfer.size (which is not always updated as expected)}
 PacketsRemaining:=((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_COUNT) shr 19);
 PacketsTransferred:=(Request.AttemptedPacketsRemaining - PacketsRemaining);
 
 {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: ' + IntToStr(PacketsTransferred) + ' packets transferred on channel ' + IntToStr(Channel));
 {$ENDIF}
 
 if PacketsTransferred <> 0 then
  begin
   BytesTransferred:=0;
   Characteristics:=HostChannel.Characteristics;
   MaxPacketSize:=((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_MAX_PACKET_SIZE) shr 0);
   Direction:=((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_DIRECTION) shr 15);
   TransferType:=((Characteristics and DWC_HOST_CHANNEL_CHARACTERISTICS_ENDPOINT_TYPE) shr 18);
   
   {Check for Speed}
   if Request.Device.Speed <> USB_SPEED_HIGH then
    begin
     {Note: Both Linux and FreeBSD place all split interrupt requests into the control queue in order to ease the timing pressure}
     
     {Fixup Transfer Type set by start transfer}
     if USBIsInterruptRequest(Request) then TransferType:=USB_TRANSFER_TYPE_INTERRUPT;
    end; 
   
   {Calculate number of bytes transferred and copy data from DMA buffer if needed}
   if Direction = USB_DIRECTION_IN then
    begin
     {The transfer.size field seems to be updated sanely for IN transfers.  (Good thing too, since otherwise it would be impossible to determine the length of short packets...)}
     BytesTransferred:=(Request.AttemptedBytesRemaining - ((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_SIZE) shr 0));
     
     {Check the DMA compatibility}
     if (Request.Flags and USB_REQUEST_FLAG_COMPATIBLE) = USB_REQUEST_FLAG_COMPATIBLE then
      begin
       if not(DWCOTG_DMA_CACHE_COHERENT) then
        begin
         {Invalidate the data cache}
         InvalidateDataCacheRange(PtrUInt(PtrUInt(Request.CurrentData) + PtrUInt(Request.AttemptedSize - Request.AttemptedBytesRemaining)),BytesTransferred);
        end;
      end
     else 
      begin
       {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Copying data from DMA buffer');
       {$ENDIF}
       
       {Update Statistics}
       Inc(Host.DMABufferReadCount);
       
       if not(DWCOTG_DMA_CACHE_COHERENT) then
        begin
         {Invalidate the data cache}
         InvalidateDataCacheRange(PtrUInt(PtrUInt(Host.DMABuffers[Channel]) + PtrUInt(Request.AttemptedSize - Request.AttemptedBytesRemaining)),BytesTransferred);
        end; 
       
       {Copy the data from the DMA buffer}
       System.Move(Pointer(PtrUInt(Host.DMABuffers[Channel]) + PtrUInt(Request.AttemptedSize - Request.AttemptedBytesRemaining))^,Request.CurrentData^,BytesTransferred);
      end;
    end
   else
    begin
     {Ignore transfer.size field for OUT transfers because it's not updated sanely}
     if PacketsTransferred > 1 then
      begin
       {More than one packet transferred: all except the last must have been MaxPacketSize}
       BytesTransferred:=BytesTransferred + (MaxPacketSize * (PacketsTransferred - 1));
      end;

     {If the last packet in this transfer attempt was transmitted, its size is the remainder of the attempted transfer size. Otherwise, it's another MaxPacketSize}      
     if (PacketsRemaining = 0) and (((Request.AttemptedSize mod MaxPacketSize) <> 0) or (Request.AttemptedSize = 0)) then
      begin
       BytesTransferred:=BytesTransferred + (Request.AttemptedSize mod MaxPacketSize);
      end
     else
      begin
       BytesTransferred:=BytesTransferred + MaxPacketSize;
      end;      
    end;
   
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Calculated ' + IntToStr(BytesTransferred) + ' bytes transferred');
   {$ENDIF}
   
   {Account for packets and bytes transferred}   
   Request.AttemptedPacketsRemaining:=(Request.AttemptedPacketsRemaining - PacketsTransferred);
   Request.AttemptedBytesRemaining:=(Request.AttemptedBytesRemaining - BytesTransferred);
   Request.BytesTransferred:=(Request.BytesTransferred + BytesTransferred);
   PtrUInt(Request.CurrentData):=(PtrUInt(Request.CurrentData) + BytesTransferred);
   
   {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Attempted packets remaining is ' + IntToStr(Request.AttemptedPacketsRemaining));
   if USB_LOG_ENABLED then USBLogDebug(nil,'DWCOTG: Attempted bytes remaining is ' + IntToStr(Request.AttemptedBytesRemaining));
   {$ENDIF}
   
   {Check if transfer complete (at least to the extent that data was programmed into the channel)}
   if (Request.AttemptedPacketsRemaining = 0) or ((Direction = USB_DIRECTION_IN) and (BytesTransferred < (PacketsTransferred * MaxPacketSize))) then
    begin
     {The transfer_completed flag should have been set by the hardware, although it's essentially meaningless because it gets set at other times as well. 
      (For example, it appears to be set when a split transaction has completed, even if there are still packets remaining to be transferred)}
     if (Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_TRANSFER_COMPLETED) = 0 then
      begin
       {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: TRANSFER_COMPLETED flag not set on channel ' + IntToStr(Channel) + ' as expected (Interrupts=' + IntToHex(Interrupts,8) + ', Transfer=' + IntToHex(HostChannel.Transfer,8) + ')');
       {$ENDIF}
       
       Result:=DWC_STATUS_FAILED;
       
       {Update Statistics}
       Inc(Host.NoChannelCompletedCount);
       
       Exit;
      end;
      
     {If we programmed less than the desired transfer size into the channels (for one of several reasons--- see dwc_channel_start_xfer()), continue to attempt the transfer,
      unless it was an interrupt transfer, in which case at most one attempt should be made, or if fewer bytes were transferred than attempted (for an IN transfer), indicating
      the transfer is already done}
     if (Request.ShortAttempt) and (Request.AttemptedBytesRemaining = 0) and (TransferType <> USB_TRANSFER_TYPE_INTERRUPT) then
      begin
       {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Starting next part of ' + IntToStr(Request.Size) + '-byte transfer after short attempt of ' + IntToStr(Request.AttemptedSize) + ' bytes');
       {$ENDIF}
       
       Request.CompleteSplit:=False;
       Request.NextDataPID:=((HostChannel.Transfer and DWC_HOST_CHANNEL_TRANSFER_PACKET_ID) shr 29);
       if not(USBIsControlRequest(Request)) or (Request.ControlPhase = USB_CONTROL_PHASE_DATA) then
        begin
         Request.ActualSize:=PtrUInt(Request.CurrentData) - PtrUInt(Request.Data);
        end;
       
       {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Transfer needs restart (Short Attempt) on channel ' + IntToStr(Channel));
       {$ENDIF}
       
       Result:=DWC_STATUS_TRANSFER_RESTART;
       Exit;
      end;

     {Unlike other transfers, control transfers consist of multiple phases. If we only just completed the SETUP or DATA phase of a control transfer, advance to the next phase
      and do not signal transfer completion}
     if (USBIsControlRequest(Request)) and (Request.ControlPhase < USB_CONTROL_PHASE_STATUS) then
      begin
       {Reset the CompleteSplit flag}
       Request.CompleteSplit:=False;
       
       {Record bytes transferred if we just completed the data phase}
       if Request.ControlPhase = USB_CONTROL_PHASE_DATA then
        begin
         Request.ActualSize:=PtrUInt(Request.CurrentData) - PtrUInt(Request.Data);
        end;
      
       {Advance to the next phase}      
       Inc(Request.ControlPhase);
       
       {Skip DATA phase if there is no data to send/receive}
       if (Request.ControlPhase = USB_CONTROL_PHASE_DATA) and (Request.Size = 0) then
        begin
         Inc(Request.ControlPhase);
        end;
       
       {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Transfer needs restart (Control Phase) on channel ' + IntToStr(Channel));
       if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG:  (Control phase=' + IntToStr(Request.ControlPhase) + ')');
       {$ENDIF}
       
       Result:=DWC_STATUS_TRANSFER_RESTART;
       Exit;
      end;
      
     {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Transfer completed on channel ' + IntToStr(Channel));
     {$ENDIF}

     {Transfer is actually complete (or at least, it was an IN transfer that completed with fewer bytes transferred than requested)} 
     Result:=DWC_STATUS_SUCCESS;
     Exit;
    end
   else 
    begin
     {Transfer not complete, so start the next transaction}
     {Flip the CompleteSplit flag if doing split transactions} 
     if (HostChannel.SplitControl and DWC_HOST_CHANNEL_SPLIT_CONTROL_SPLIT_ENABLE) <> 0 then
      begin
       Request.CompleteSplit:=not(Request.CompleteSplit);
      end;
     
     {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Continuing transfer (Not Complete) (CompleteSplit=' + BooleanToString(Request.CompleteSplit) + ')');
     {$ENDIF}
     
     Result:=DWC_STATUS_TRANSACTION_RESTART;
     Exit;
    end;    
  end
 else
  begin
   {No packets transferred, but no error flag was set.  This is expected only if we just did a Start Split transaction, in which case we should continue on to the Complete Split
    transaction. We also check for the ack_response_received flag, which should be set to indicate that the device acknowledged the Start Split transaction}
   if ((Interrupts and DWC_HOST_CHANNEL_INTERRUPTS_ACK_RESPONSE_RECEIVED) <> 0) and ((HostChannel.SplitControl and DWC_HOST_CHANNEL_SPLIT_CONTROL_SPLIT_ENABLE) <> 0) and not(Request.CompleteSplit) then
    begin
     {Start CompleteSplit}
     Request.CompleteSplit:=True;
     
     {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'DWCOTG: Continuing transfer (No Packets) (CompleteSplit=' + BooleanToString(Request.CompleteSplit) + ')');
     {$ENDIF}
     
     Result:=DWC_STATUS_TRANSACTION_RESTART;
     
     Exit;
    end
   else
    begin
     {$IF (DEFINED(DWCOTG_DEBUG) or DEFINED(USB_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if USB_LOG_ENABLED then USBLogError(Request.Device,'DWCOTG: No packets transferred');
     {$ENDIF}
     
     Result:=DWC_STATUS_FAILED;
     
     {Update Statistics}
     Inc(Host.NoPacketsTransferredCount); 
     
     Exit;
    end;    
  end;  
end;

{==============================================================================}
{==============================================================================}
{DWCOTG Helper Functions}
function DWCDivRoundUp(Number,Denominator:LongWord):LongWord;
begin
 {}
 Result:=0;
 
 if Denominator = 0 then Exit;
 
 Result:=((Number + Denominator) - 1) div Denominator;
end;

{==============================================================================}

function DWCCalculateFrameInterval(Host:PDWCUSBHost):LongWord;
{Calculate the frame interval register value based on USB configuration and port speed}
{Note: The host frame interval register can only be modified when the port enabled bit
 is set in the host port control and status register}
{Host: The DWCOTG host to calculate the interval for}

{Note: Caller must hold the Host lock}  
var
 Clock:LongWord;
 HWCfg2:LongWord;
 CoreUSBConfiguration:LongWord;
 HostPortControlStatus:LongWord;    
begin
 {}
 Result:=0;
 
 if Host = nil then Exit;
 
 {Set Default}
 Clock:=60;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Hardware Configuration 2}
 HWCfg2:=Host.Registers.HWCfg2;
 
 {Get Core USB Configuration}
 CoreUSBConfiguration:=Host.Registers.CoreUSBConfiguration;
 
 {Get Host Port Control and Status}
 HostPortControlStatus:=Host.Registers.HostPortControlStatus;
 
 {Check PHY Type}
 if ((CoreUSBConfiguration and DWC_USB_CFG_PHY_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_ULPI_UTMI_SEL) = DWC_USB_CFG_ULPI_UTMI_SEL) and ((CoreUSBConfiguration and DWC_USB_CFG_PHYIF16) = 0) then
  begin
   Clock:=60;
  end;
 if ((CoreUSBConfiguration and DWC_USB_CFG_PHY_SEL) = DWC_USB_CFG_PHY_SEL) and ((HWCfg2 and DWC_HWCFG2_FS_PHY_TYPE_MASK) = DWC_HWCFG2_FS_PHY_TYPE_SHARED_ULPI) then
  begin
   Clock:=48;
  end;
 if ((CoreUSBConfiguration and DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_PHY_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_ULPI_UTMI_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_PHYIF16) = DWC_USB_CFG_PHYIF16) then
  begin
   Clock:=30;
  end;
 if ((CoreUSBConfiguration and DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_PHY_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_ULPI_UTMI_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_PHYIF16) = 0) then
  begin
   Clock:=60;
  end; 
 if ((CoreUSBConfiguration and DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL) = DWC_USB_CFG_PHY_LOW_PWR_CLK_SEL) and ((CoreUSBConfiguration and DWC_USB_CFG_PHY_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_ULPI_UTMI_SEL) = 0) and ((CoreUSBConfiguration and DWC_USB_CFG_PHYIF16) = DWC_USB_CFG_PHYIF16) then
  begin
   Clock:=48;
  end;
 if ((CoreUSBConfiguration and DWC_USB_CFG_PHY_SEL) = DWC_USB_CFG_PHY_SEL) and ((CoreUSBConfiguration and DWC_USB_CFG_PHYIF16) = 0) and ((HWCfg2 and DWC_HWCFG2_FS_PHY_TYPE_MASK) = DWC_HWCFG2_FS_PHY_TYPE_SHARED_UTMI) then
  begin
   Clock:=48;
  end;
 if ((CoreUSBConfiguration and DWC_USB_CFG_PHY_SEL) = DWC_USB_CFG_PHY_SEL) and ((HWCfg2 and DWC_HWCFG2_FS_PHY_TYPE_MASK) = DWC_HWCFG2_FS_PHY_TYPE_DEDICATED) then
  begin
   Clock:=48;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Check Speed}
 if (HostPortControlStatus and DWC_HOST_PORT_CTRLSTATUS_SPEED) = (USB_SPEED_HIGH shl 17) then
  begin
   {Return High Speed Value}
   Result:=125 * Clock;
  end
 else
  begin
   {Return Full Speed / Low Speed Value}
   Result:=1000 * Clock;
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 DWCInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
