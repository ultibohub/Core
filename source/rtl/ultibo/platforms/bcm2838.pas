{
Ultibo Definitions specific to the Broadcom 2838 System on chip.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 ARMv8 (Cortex A72)

Boards
======

 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

 Linux - raspberrypi-firmware.h - Copyright (C) 2015 Broadcom


 //See: include\soc\bcm2835\raspberrypi-firmware.h

 //New Mailbox Requests
 //RPI_FIRMWARE_GET_FIRMWARE_VARIANT =                   0x00000002, <<----
 //RPI_FIRMWARE_GET_FIRMWARE_HASH =                      0x00000003, <<----

 //RPI_FIRMWARE_GET_EDID_BLOCK_DISPLAY =                 0x00030023, <<----

 //RPI_FIRMWARE_SET_PLANE =                              0x00048015,
 //RPI_FIRMWARE_GET_DISPLAY_TIMING =                     0x00040017,
 //RPI_FIRMWARE_SET_TIMING =                             0x00048017,
 //RPI_FIRMWARE_GET_DISPLAY_CFG =                        0x00040018,
 //RPI_FIRMWARE_SET_DISPLAY_POWER =                         0x00048019,

 //RPI_FIRMWARE_FRAMEBUFFER_GET_VSYNC =                  0x0004000e, <<----

 //RPI_FIRMWARE_NOTIFY_REBOOT =                          0x00030048, <<----

 //RPI_FIRMWARE_GET_STC =                                0x0003000b, <<----

 //RPI_FIRMWARE_NOTIFY_XHCI_RESET =                      0x00030058, <<----
 //RPI_FIRMWARE_GET_REBOOT_FLAGS =                       0x00030064, <<----
 //RPI_FIRMWARE_SET_REBOOT_FLAGS =                       0x00038064,  <<----

References
==========

 BCM2711 ARM Peripherals

  https://datasheets.raspberrypi.com/bcm2711/bcm2711-peripherals.pdf

 QA7 Rev3.4

  https://datasheets.raspberrypi.com/bcm2836/bcm2836-peripherals.pdf

Broadcom BCM2838
================

The BCM2838 is the SoC used by the Raspberry Pi models 4B/400/CM4, it is often also referred to as the BCM2711 but
for consistency with the earlier models we use that number for the core driver unit BCM2711.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit BCM2838;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {BCM2838 specific constants}
 BCM2838_CPU_COUNT = 4;

 {ARM Physical to VC legacy IO Mapping}
 BCM2838_VCIO_ALIAS    = $7E000000;

 {ARM Physical to VC legacy Bus Mapping}
 BCM2838_VCBUS_0_ALIAS = $00000000; {0 Alias - L1 and L2 cached}
 BCM2838_VCBUS_4_ALIAS = $40000000; {4 Alias - L2 cache coherent (non allocating)}
 BCM2838_VCBUS_8_ALIAS = $80000000; {8 Alias - L2 cached (only)}
 BCM2838_VCBUS_C_ALIAS = $C0000000; {C Alias - Direct uncached} {Suitable for RPi 4 Model B}

 {Physical memory addresses of BCM2838 peripherals}
 BCM2838_PERIPHERALS_BASE = $FE000000;  {Mapped to VC legacy address 7E000000}
 BCM2838_PERIPHERALS_SIZE = SIZE_16M + SIZE_8M; {24M}

 {Physical memory addresses of BCM2838 extended peripherals (GENET, PCIe)}
 BCM2838_EXT_PERIPHERALS_BASE = $FC000000;  {Mapped to VC legacy address 7C000000}
 BCM2838_EXT_PERIPHERALS_SIZE = SIZE_32M;

 {Physical memory address of BCM2838 PCIe address space}
 BCM2838_PCI_ADDRESS_BASE = $600000000;

 {Address of PCI outbound address space (Mapped to Physical PCIe address space)}
 BCM2838_PCI_OUTBOUND_BASE = $F8000000;
 BCM2838_PCI_OUTBOUND_SIZE = SIZE_64M;

 {Physical memory address of BCM2838 PCIe DMA range}
 BCM2838_PCI_DMA_RANGE_BASE = $400000000;

 {Address of PCI inbound address space}
 BCM2838_PCI_INBOUND_BASE = $00000000;
 BCM2838_PCI_INBOUND_SIZE = SIZE_4G; {Limited to 3GB on some revisions}

 {System Timer}
 BCM2838_SYSTEM_TIMER_REGS_BASE = BCM2838_PERIPHERALS_BASE + $3000;

 {DMA controller (Channels 0 to 14)}
 BCM2838_DMA0_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7000;
 BCM2838_DMA1_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7100;
 BCM2838_DMA2_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7200;
 BCM2838_DMA3_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7300;
 BCM2838_DMA4_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7400;
 BCM2838_DMA5_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7500;
 BCM2838_DMA6_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7600;
 BCM2838_DMA7_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7700; {Lite DMA engine}
 BCM2838_DMA8_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7800; {Lite DMA engine}
 BCM2838_DMA9_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $7900; {Lite DMA engine}
 BCM2838_DMA10_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $7A00; {Lite DMA engine}
 BCM2838_DMA11_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $7B00; {40bit DMA engine} {DMA channel 11 is able to access the PCIe interface}
 BCM2838_DMA12_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $7C00; {40bit DMA engine}
 BCM2838_DMA13_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $7D00; {40bit DMA engine}
 BCM2838_DMA14_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $7E00; {40bit DMA engine}

 BCM2838_DMA_INT_STATUS_BASE    = BCM2838_PERIPHERALS_BASE + $7FE0;
 BCM2838_DMA_ENABLE_BASE        = BCM2838_PERIPHERALS_BASE + $7FF0;

 {ARM Legacy Interrupt Controller}
 BCM2838_INTERRUPT_REGS_BASE    = BCM2838_PERIPHERALS_BASE + $B200; {Note: Documentation states 0xB000 but the offsets begin at 0x200 so 0xB200 will be correct}

 {ARM Timer}
 BCM2838_TIMER_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $B400; {Note: Documentation states 0xB000 but the offsets begin at 0x400 so 0xB400 will be correct}

 {ARM Doorbell (VCHIQ)}
 BCM2838_DOORBELL_REGS_BASE     = BCM2838_PERIPHERALS_BASE + $B840;
 BCM2838_VCHIQ_REGS_BASE        = BCM2838_DOORBELL_REGS_BASE;

 {ARM Mailbox 0}
 BCM2838_MAILBOX0_REGS_BASE     = BCM2838_PERIPHERALS_BASE + $B880;

 {ARM Mailbox 1}
 BCM2838_MAILBOX1_REGS_BASE     = BCM2838_PERIPHERALS_BASE + $B8A0;

 {Power Management, Reset controller and Watchdog}
 BCM2838_PM_REGS_BASE           = BCM2838_PERIPHERALS_BASE + $100000;

 {Clock Management}
 BCM2838_CM_REGS_BASE           = BCM2838_PERIPHERALS_BASE + $101000;

 {PCM Clock}
 BCM2838_PCM_CLOCK_BASE         = BCM2838_PERIPHERALS_BASE + $101098;

 {Random Number Generator}
 BCM2838_RNG_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $104000;

 {GPIO}
 BCM2838_GPIO_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $200000; {Note: This is shown in BCM2711 ARM Peripherals as 0x7E215000 which is thought to be an error}

 {UART0 (PL011)}
 BCM2838_PL011_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $201000;
 BCM2838_UART0_REGS_BASE        = BCM2838_PL011_REGS_BASE;

 {UART2 (PL011)}
 BCM2838_UART2_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $201400;

 {UART3 (PL011)}
 BCM2838_UART3_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $201600;

 {UART4 (PL011)}
 BCM2838_UART4_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $201800;

 {UART5 (PL011)}
 BCM2838_UART5_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $201A00;

 BCM2838_UART0_REGS_BASES:array[0..5] of LongWord = (
  BCM2838_UART0_REGS_BASE,  {UART0}
  0,                        {None}
  BCM2838_UART2_REGS_BASE,  {UART2}
  BCM2838_UART3_REGS_BASE,  {UART3}
  BCM2838_UART4_REGS_BASE,  {UART4}
  BCM2838_UART5_REGS_BASE); {UART5}

 {EMMC1 (SDHOST)}
 BCM2838_EMMC1_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $202000;
 BCM2838_SDHOST_REGS_BASE       = BCM2838_EMMC1_REGS_BASE;

 {PCM / I2S Audio}
 BCM2838_PCM_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $203000;

 {SPI0}
 BCM2838_SPI0_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $204000;

 {SPI3}
 BCM2838_SPI3_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $204600;

 {SPI4}
 BCM2838_SPI4_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $204800;

 {SPI5}
 BCM2838_SPI5_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $204A00;

 {SPI6}
 BCM2838_SPI6_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $204C00;

 BCM2838_SPI0_REGS_BASES:array[0..6] of LongWord = (
  BCM2838_SPI0_REGS_BASE,  {SPI0}
  0,                       {None}
  0,                       {None}
  BCM2838_SPI3_REGS_BASE,  {SPI3}
  BCM2838_SPI4_REGS_BASE,  {SPI4}
  BCM2838_SPI5_REGS_BASE,  {SPI5}
  BCM2838_SPI6_REGS_BASE); {SPI6}

 {Peripheral Control (PACTL_CS)}
 BCM2838_PACTL_CS_REGS_BASE     = BCM2838_PERIPHERALS_BASE + $204E00;

 {BSC0 (I2C0) (Broadcom Serial Controller)}
 BCM2838_BSC0_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $205000;
 BCM2838_I2C0_REGS_BASE         = BCM2838_BSC0_REGS_BASE;

 {I2C3}
 BCM2838_I2C3_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $205600;

 {I2C4}
 BCM2838_I2C4_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $205800;

 {I2C5}
 BCM2838_I2C5_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $205A00; {Note: This is shown in BCM2711 ARM Peripherals as 0x7E205A80 which is thought to be an error}

 {I2C6}
 BCM2838_I2C6_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $205C00;

 {I2C7} {Note: I2C7 master is used dedicated with the HDMI interface and must not be used}
 BCM2838_I2C7_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $205E00;

 {BCM2838_I2C0_REGS_BASES - See below}

 {Pixel Valve 0}
 BCM2838_PIXELVALVE0_REGS_BASE  = BCM2838_PERIPHERALS_BASE + $206000;

 {Pixel Valve 1}
 BCM2838_PIXELVALVE1_REGS_BASE  = BCM2838_PERIPHERALS_BASE + $207000;

 {DPI (Display Parallel Interface)}
 BCM2838_DPI_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $208000;

 {DSI0 (Display Serial Interface}
 BCM2838_DSI0_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $209000;

 {PWM0 (Pulse Width Modulator)}
 BCM2838_PWM0_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $20C000;

 {PWM1 (Pulse Width Modulator)}
 BCM2838_PWM1_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $20C800;

 BCM2838_PWM0_REGS_BASES:array[0..1] of LongWord = (
  BCM2838_PWM0_REGS_BASE,  {PWM0}
  BCM2838_PWM1_REGS_BASE); {PWM1}

 {I2C/SPI Slave}
 BCM2838_I2CSPI_REGS_BASE       = BCM2838_PERIPHERALS_BASE + $214000;

 {AUX (UART1, SPI1 and SPI2)}
 BCM2838_AUX_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $215000;
 BCM2838_UART1_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $215040;
 BCM2838_SPI1_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $215080;
 BCM2838_SPI2_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $2150C0;

 {EMMC0 (SDHCI)}
 BCM2838_EMMC0_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $300000;
 BCM2838_SDHCI_REGS_BASE        = BCM2838_EMMC0_REGS_BASE;

 {EMMC2}
 BCM2838_EMMC2_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $340000;

 {HVS}
 BCM2838_HVS_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $400000;

 {SMI (Firmware KMS)}
 BCM2838_SMI_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $600000;
 BCM2838_FIRMWAREKMS_REGS_BASE  = BCM2838_SMI_REGS_BASE;

 {DSI1 (Display Serial Interface}
 BCM2838_DSI1_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $700000;

 {CSI0 (Camera Serial Interface}
 BCM2838_CSI0_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $800000;

 {CSI1 (Camera Serial Interface}
 BCM2838_CSI1_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $801000;

 {BSC1 (I2C1) (Broadcom Serial Controller)}
 BCM2838_BSC1_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $804000;
 BCM2838_I2C1_REGS_BASE         = BCM2838_BSC1_REGS_BASE;

 {BSC2 (I2C2) (Broadcom Serial Controller)} {Note: I2C2 master is used dedicated with the HDMI interface and must not be used}
 BCM2838_BSC2_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $805000;
 BCM2838_I2C2_REGS_BASE         = BCM2838_BSC2_REGS_BASE;

 BCM2838_I2C0_REGS_BASES:array[0..7] of LongWord = (
  BCM2838_I2C0_REGS_BASE,  {I2C0}
  BCM2838_I2C1_REGS_BASE,  {I2C1}
  BCM2838_I2C2_REGS_BASE,  {I2C2}
  BCM2838_I2C3_REGS_BASE,  {I2C3}
  BCM2838_I2C4_REGS_BASE,  {I2C4}
  BCM2838_I2C5_REGS_BASE,  {I2C5}
  BCM2838_I2C6_REGS_BASE,  {I2C6}
  BCM2838_I2C7_REGS_BASE); {I2C7}

 {VEC}
 BCM2838_VEC_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $806000;

 {Pixel Valve 2}
 BCM2838_PIXELVALVE2_REGS_BASE  = BCM2838_PERIPHERALS_BASE + $807000;

 {HDMI}
 BCM2838_HDMI_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $902000;

 {USB (Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller)}
 BCM2838_USB_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $980000;
 BCM2838_DWCOTG_REGS_BASE       = BCM2838_USB_REGS_BASE;

 {XHCI (Additional on chip generic XHCI USB host controller)}
 BCM2838_XHCI_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $9C0000; {This is separate to the VL805 XHCI controller}

 {HEVC Decoder}
 BCM2838_HEVC_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $B00000;

 {ARGON}
 BCM2838_ARGON_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $B10000;

 {H264 Decoder}
 BCM2838_H264_REGS_BASE         = BCM2838_PERIPHERALS_BASE + $B20000;

 {VP9 Decoder}
 BCM2838_VP9_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $B30000;

 {V3D}
 BCM2838_V3D_REGS_BASE          = BCM2838_PERIPHERALS_BASE + $C04000;

 {DMA controller (Channel 15)} {Note: DMA15 is exclusively used by the VPU}
 BCM2838_DMA15_REGS_BASE        = BCM2838_PERIPHERALS_BASE + $E05000;

const
 {Chip Revision}
 BCM2838_CHIP_REVISION_BASE     = BCM2838_EXT_PERIPHERALS_BASE + $404000; {See: https://elinux.org/The_Undocumented_Pi#BCM2711}

 {PCIe}
 BCM2838_PCIE_REGS_BASE         = BCM2838_EXT_PERIPHERALS_BASE + $1500000;

 {GENET}
 BCM2838_GENET_REGS_BASE        = BCM2838_EXT_PERIPHERALS_BASE + $1580000;
 BCM2838_GENET_MDIO_OFFSET      = $0E14;

 {Thermal}
 BCM2838_THERMAL_REGS_BASE      = BCM2838_EXT_PERIPHERALS_BASE + $15D2200;

const
 {IRQ lines of BCM2838 peripherals}
 BCM2838_SPI_BASE = 32; {Added to the SPI ID in device tree to obtain the real ID}
 BCM2838_PPI_BASE = 16; {Added to the PPI ID in device tree to obtain the real ID}
 BCM2838_SGI_BASE = 0;

 {ARM Generic Timer}
 BCM2838_IRQ_TIMER0         = BCM2838_PPI_BASE + 13; {IRQ_TYPE_LEVEL_LOW / CPU_MASK_0 or CPU_MASK_1 or CPU_MASK_2 or CPU_MASK_3} {Core n PS timer IRQ}
 BCM2838_IRQ_TIMER1         = BCM2838_PPI_BASE + 14; {IRQ_TYPE_LEVEL_LOW / CPU_MASK_0 or CPU_MASK_1 or CPU_MASK_2 or CPU_MASK_3} {Core n PNS timer IRQ}
 BCM2838_IRQ_TIMER2         = BCM2838_PPI_BASE + 11; {IRQ_TYPE_LEVEL_LOW / CPU_MASK_0 or CPU_MASK_1 or CPU_MASK_2 or CPU_MASK_3} {Core n V timer IRQ}
 BCM2838_IRQ_TIMER3         = BCM2838_PPI_BASE + 10; {IRQ_TYPE_LEVEL_LOW / CPU_MASK_0 or CPU_MASK_1 or CPU_MASK_2 or CPU_MASK_3} {Core n HP timer IRQ}

 {Legacy IRQ/FIQ}
 BCM2838_FIQ_LEGACY         = BCM2838_PPI_BASE + 12; {Legacy FIQn}
 BCM2838_IRQ_LEGACY         = BCM2838_PPI_BASE + 15; {Legacy IRQn}

 {ARM Mailbox IRQs (BCM2838_SPI_BASE + 0 to 15)}
 BCM2838_IRQ_MAILBOX0_0     = BCM2838_SPI_BASE + 0;  {IRQ_TYPE_LEVEL_HIGH} {Core 0}
 BCM2838_IRQ_MAILBOX1_0     = BCM2838_SPI_BASE + 1;  {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX2_0     = BCM2838_SPI_BASE + 2;  {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX3_0     = BCM2838_SPI_BASE + 3;  {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX0_1     = BCM2838_SPI_BASE + 4;  {IRQ_TYPE_LEVEL_HIGH} {Core 1}
 BCM2838_IRQ_MAILBOX1_1     = BCM2838_SPI_BASE + 5;  {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX2_1     = BCM2838_SPI_BASE + 6;  {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX3_1     = BCM2838_SPI_BASE + 7;  {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX0_2     = BCM2838_SPI_BASE + 8;  {IRQ_TYPE_LEVEL_HIGH} {Core 2}
 BCM2838_IRQ_MAILBOX1_2     = BCM2838_SPI_BASE + 9;  {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX2_2     = BCM2838_SPI_BASE + 10; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX3_2     = BCM2838_SPI_BASE + 11; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX0_3     = BCM2838_SPI_BASE + 12; {IRQ_TYPE_LEVEL_HIGH} {Core 3}
 BCM2838_IRQ_MAILBOX1_3     = BCM2838_SPI_BASE + 13; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX2_3     = BCM2838_SPI_BASE + 14; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MAILBOX3_3     = BCM2838_SPI_BASE + 15; {IRQ_TYPE_LEVEL_HIGH}

 {ARM PMU}
 BCM2838_IRQ_PMU0           = BCM2838_SPI_BASE + 16; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PMU1           = BCM2838_SPI_BASE + 17; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PMU2           = BCM2838_SPI_BASE + 18; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PMU3           = BCM2838_SPI_BASE + 19; {IRQ_TYPE_LEVEL_HIGH}

 {AXI Quiet}
 BCM2838_IRQ_LOCAL_AXIQUIET = BCM2838_SPI_BASE + 20; {IRQ_TYPE_LEVEL_HIGH}

 {ARM Local Timer}
 BCM2838_IRQ_LOCAL_TIMER    = BCM2838_SPI_BASE + 21; {IRQ_TYPE_LEVEL_HIGH}

 {AXI Error}
 BCM2838_IRQ_LOCAL_AXIERR   = BCM2838_SPI_BASE + 22; {IRQ_TYPE_LEVEL_HIGH} {TBD}

 {ARM peripheral IRQs (BCM2838_SPI_BASE + 32 to 47)}
 {ARM Timer}
 BCM2838_IRQ_ARM_TIMER      = BCM2838_SPI_BASE + 32; {IRQ_TYPE_LEVEL_HIGH}

 {ARM Mailbox}
 BCM2838_IRQ_ARM_MAILBOX    = BCM2838_SPI_BASE + 33; {IRQ_TYPE_LEVEL_HIGH}

 {ARM Doorbell (VCHIQ)}
 BCM2838_IRQ_ARM_DOORBELL0  = BCM2838_SPI_BASE + 34; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_DOORBELL1  = BCM2838_SPI_BASE + 35; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_VCHIQ          = BCM2838_IRQ_ARM_DOORBELL0;

 {ARM VPU Halted}
 BCM2838_IRQ_ARM_VPU0HALTED = BCM2838_SPI_BASE + 36; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_VPU1HALTED = BCM2838_SPI_BASE + 37; {IRQ_TYPE_LEVEL_HIGH}

 {ARM Error}
 BCM2838_IRQ_ARM_ADDRERROR  = BCM2838_SPI_BASE + 38; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_AXIERROR   = BCM2838_SPI_BASE + 39; {IRQ_TYPE_LEVEL_HIGH}

 {ARM Software Interrupt (SWI)}
 BCM2838_IRQ_ARM_SWI0       = BCM2838_SPI_BASE + 40; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_SWI1       = BCM2838_SPI_BASE + 41; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_SWI2       = BCM2838_SPI_BASE + 42; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_SWI3       = BCM2838_SPI_BASE + 43; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_SWI4       = BCM2838_SPI_BASE + 44; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_SWI5       = BCM2838_SPI_BASE + 45; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_SWI6       = BCM2838_SPI_BASE + 46; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_ARM_SWI7       = BCM2838_SPI_BASE + 47; {IRQ_TYPE_LEVEL_HIGH}

 {VC peripheral IRQs (BCM2838_SPI_BASE + 64 to 127)}
 {System Timer}
 BCM2838_IRQ_SYSTEM_TIMER_0 = BCM2838_SPI_BASE + 64; {Already used by the VideoCore GPU (Do not use)}
 BCM2838_IRQ_SYSTEM_TIMER_1 = BCM2838_SPI_BASE + 65;
 BCM2838_IRQ_SYSTEM_TIMER_2 = BCM2838_SPI_BASE + 66; {Already used by the VideoCore GPU (Do not use)}
 BCM2838_IRQ_SYSTEM_TIMER_3 = BCM2838_SPI_BASE + 67;

 {H264 Codec}
 BCM2838_IRQ_H264_0         = BCM2838_SPI_BASE + 68; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_H264_1         = BCM2838_SPI_BASE + 69; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_H264_2         = BCM2838_SPI_BASE + 70; {IRQ_TYPE_LEVEL_HIGH}

 {JPEG}
 BCM2838_IRQ_JPEG           = BCM2838_SPI_BASE + 71; {IRQ_TYPE_LEVEL_HIGH}

 {ISP}
 BCM2838_IRQ_ISP            = BCM2838_SPI_BASE + 72; {IRQ_TYPE_LEVEL_HIGH}

 {USB (Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller)}
 BCM2838_IRQ_USB            = BCM2838_SPI_BASE + 73; {IRQ_TYPE_LEVEL_HIGH}

 {V3D}
 BCM2838_IRQ_V3D            = BCM2838_SPI_BASE + 74; {IRQ_TYPE_LEVEL_HIGH}

 {Transposer}
 BCM2838_IRQ_TXP            = BCM2838_SPI_BASE + 75; {IRQ_TYPE_LEVEL_HIGH}

 {Multicore Sync}
 BCM2838_IRQ_MULTICORESYNC0 = BCM2838_SPI_BASE + 76; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MULTICORESYNC1 = BCM2838_SPI_BASE + 77; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MULTICORESYNC2 = BCM2838_SPI_BASE + 78; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_MULTICORESYNC3 = BCM2838_SPI_BASE + 79; {IRQ_TYPE_LEVEL_HIGH}

 {DMA}
 BCM2838_IRQ_DMA0           = BCM2838_SPI_BASE + 80; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA1           = BCM2838_SPI_BASE + 81; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA2           = BCM2838_SPI_BASE + 82; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA3           = BCM2838_SPI_BASE + 83; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA4           = BCM2838_SPI_BASE + 84; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA5           = BCM2838_SPI_BASE + 85; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA6           = BCM2838_SPI_BASE + 86; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA7           = BCM2838_SPI_BASE + 87; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA8           = BCM2838_SPI_BASE + 87; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA9           = BCM2838_SPI_BASE + 88; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA10          = BCM2838_SPI_BASE + 88; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA11          = BCM2838_SPI_BASE + 89; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA12          = BCM2838_SPI_BASE + 90; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA13          = BCM2838_SPI_BASE + 91; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_DMA14          = BCM2838_SPI_BASE + 92; {IRQ_TYPE_LEVEL_HIGH}

 BCM2838_IRQ_DMA7_8         = BCM2838_IRQ_DMA7;
 BCM2838_IRQ_DMA9_10        = BCM2838_IRQ_DMA9;

 {AUX (UART1, SPI1 and SPI2)}
 BCM2838_IRQ_AUX            = BCM2838_SPI_BASE + 93; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_UART1          = BCM2838_IRQ_AUX;
 BCM2838_IRQ_SPI1           = BCM2838_IRQ_AUX;
 BCM2838_IRQ_SPI2           = BCM2838_IRQ_AUX;

 {ARM}
 BCM2838_IRQ_ARM            = BCM2838_SPI_BASE + 94; {IRQ_TYPE_LEVEL_HIGH}

 {DMA} {Note: DMA15 is exclusively used by the VPU}
 BCM2838_IRQ_DMA15          = BCM2838_SPI_BASE + 95; {IRQ_TYPE_LEVEL_HIGH}

 {HDMI CEC}
 BCM2838_IRQ_HDMI_CEC       = BCM2838_SPI_BASE + 96; {IRQ_TYPE_LEVEL_HIGH}

 {HVS}
 BCM2838_IRQ_HVS            = BCM2838_SPI_BASE + 97; {IRQ_TYPE_LEVEL_HIGH}

 {ARGON (RPIVID)}
 BCM2838_IRQ_ARGON          = BCM2838_SPI_BASE + 98; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_RPIVID         = BCM2838_IRQ_ARGON;

 {SDC}
 BCM2838_IRQ_SDC            = BCM2838_SPI_BASE + 99; {IRQ_TYPE_LEVEL_HIGH}

 {DSI0}
 BCM2838_IRQ_DSI0           = BCM2838_SPI_BASE + 100; {IRQ_TYPE_LEVEL_HIGH}

 {Pixel Valve 2}
 BCM2838_IRQ_PIXELVALVE2    = BCM2838_SPI_BASE + 101; {IRQ_TYPE_LEVEL_HIGH}

 {CSI0 (Camera Serial Interface}
 BCM2838_IRQ_CSI0           = BCM2838_SPI_BASE + 102; {IRQ_TYPE_LEVEL_HIGH}

 {CSI1 (Camera Serial Interface}
 BCM2838_IRQ_CSI1           = BCM2838_SPI_BASE + 103; {IRQ_TYPE_LEVEL_HIGH}

 {HDMI}
 BCM2838_IRQ_HDMI_0         = BCM2838_SPI_BASE + 104; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_HDMI_1         = BCM2838_SPI_BASE + 105; {IRQ_TYPE_LEVEL_HIGH}

 {Pixel Valve 3}
 BCM2838_IRQ_PIXELVALVE3    = BCM2838_SPI_BASE + 106; {IRQ_TYPE_LEVEL_HIGH}

 {I2C / SPI Slave}
 BCM2838_IRQ_I2CSPI         = BCM2838_SPI_BASE + 107; {IRQ_TYPE_LEVEL_HIGH}

 {DSI1}
 BCM2838_IRQ_DSI1           = BCM2838_SPI_BASE + 108; {IRQ_TYPE_LEVEL_HIGH}

 {Pixel Valve 0}
 BCM2838_IRQ_PIXELVALVE0    = BCM2838_SPI_BASE + 109; {IRQ_TYPE_LEVEL_HIGH}

 {Pixel Valve 1/4}
 BCM2838_IRQ_PIXELVALVE1    = BCM2838_SPI_BASE + 110; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PIXELVALVE4    = BCM2838_IRQ_PIXELVALVE1;

 {CPR}
 BCM2838_IRQ_CPR            = BCM2838_SPI_BASE + 111; {IRQ_TYPE_LEVEL_HIGH}

 {SMI (Firmware KMS)}
 BCM2838_IRQ_SMI            = BCM2838_SPI_BASE + 112; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_FIRMWAREKMS    = BCM2838_IRQ_SMI;

 {GPIO}
 BCM2838_IRQ_GPIO_0         = BCM2838_SPI_BASE + 113; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_GPIO_1         = BCM2838_SPI_BASE + 114; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_GPIO_2         = BCM2838_SPI_BASE + 115; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_GPIO_3         = BCM2838_SPI_BASE + 116; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_GPIO_ALL       = BCM2838_IRQ_GPIO_3;

 {BSC0 (I2C0) (Broadcom Serial Controller)}
 BCM2838_IRQ_BSC0           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_I2C0           = BCM2838_IRQ_BSC0;

 {BSC1 (I2C1) (Broadcom Serial Controller)}
 BCM2838_IRQ_BSC1           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_I2C1           = BCM2838_IRQ_BSC1;

 {BSC2 (I2C2) (Broadcom Serial Controller)}
 BCM2838_IRQ_BSC2           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_I2C2           = BCM2838_IRQ_BSC2;

 {I2C3}
 BCM2838_IRQ_I2C3           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}

 {I2C4}
 BCM2838_IRQ_I2C4           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}

 {I2C5}
 BCM2838_IRQ_I2C5           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}

 {I2C6}
 BCM2838_IRQ_I2C6           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}

 {I2C7}
 BCM2838_IRQ_I2C7           = BCM2838_SPI_BASE + 117; {IRQ_TYPE_LEVEL_HIGH}

 BCM2838_I2C0_IRQS:array[0..7] of LongWord = (
  BCM2838_IRQ_I2C0,  {I2C0}
  BCM2838_IRQ_I2C1,  {I2C1}
  BCM2838_IRQ_I2C2,  {I2C2}
  BCM2838_IRQ_I2C3,  {I2C3}
  BCM2838_IRQ_I2C4,  {I2C4}
  BCM2838_IRQ_I2C5,  {I2C5}
  BCM2838_IRQ_I2C6,  {I2C6}
  BCM2838_IRQ_I2C7); {I2C7}

 {SPI0}
 BCM2838_IRQ_SPI0           = BCM2838_SPI_BASE + 118; {IRQ_TYPE_LEVEL_HIGH}

 {SPI3}
 BCM2838_IRQ_SPI3           = BCM2838_SPI_BASE + 118; {IRQ_TYPE_LEVEL_HIGH}

 {SPI4}
 BCM2838_IRQ_SPI4           = BCM2838_SPI_BASE + 118; {IRQ_TYPE_LEVEL_HIGH}

 {SPI5}
 BCM2838_IRQ_SPI5           = BCM2838_SPI_BASE + 118; {IRQ_TYPE_LEVEL_HIGH}

 {SPI6}
 BCM2838_IRQ_SPI6           = BCM2838_SPI_BASE + 118; {IRQ_TYPE_LEVEL_HIGH}

 BCM2838_SPI0_IRQS:array[0..6] of LongWord = (
  BCM2838_IRQ_SPI0,  {SPI0}
  0,                 {None}
  0,                 {None}
  BCM2838_IRQ_SPI3,  {SPI3}
  BCM2838_IRQ_SPI4,  {SPI4}
  BCM2838_IRQ_SPI5,  {SPI5}
  BCM2838_IRQ_SPI6); {SPI6}

 {I2S PCM sound}
 BCM2838_IRQ_I2SPCM         = BCM2838_SPI_BASE + 119; {IRQ_TYPE_LEVEL_HIGH}

 {EMMC1 (SDHOST)}
 BCM2838_IRQ_EMMC1          = BCM2838_SPI_BASE + 120; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_SDHOST         = BCM2838_IRQ_EMMC1;

 {UART0 (PL011)}
 BCM2838_IRQ_PL011          = BCM2838_SPI_BASE + 121; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_UART0          = BCM2838_IRQ_PL011;

 {UART2 (PL011)}
 BCM2838_IRQ_UART2          = BCM2838_SPI_BASE + 121; {IRQ_TYPE_LEVEL_HIGH}

 {UART3 (PL011)}
 BCM2838_IRQ_UART3          = BCM2838_SPI_BASE + 121; {IRQ_TYPE_LEVEL_HIGH}

 {UART4 (PL011)}
 BCM2838_IRQ_UART4          = BCM2838_SPI_BASE + 121; {IRQ_TYPE_LEVEL_HIGH}

 {UART5 (PL011)}
 BCM2838_IRQ_UART5          = BCM2838_SPI_BASE + 121; {IRQ_TYPE_LEVEL_HIGH}

 BCM2838_UART0_IRQS:array[0..5] of LongWord = (
  BCM2838_IRQ_UART0,  {UART0}
  0,                  {None}
  BCM2838_IRQ_UART2,  {UART2}
  BCM2838_IRQ_UART3,  {UART3}
  BCM2838_IRQ_UART4,  {UART4}
  BCM2838_IRQ_UART5); {UART5}

 {ETH_PCIe L2}
 BCM2838_IRQ_ETH_PCIE       = BCM2838_SPI_BASE + 122; {IRQ_TYPE_LEVEL_HIGH}

 {VEC}
 BCM2838_IRQ_VEC            = BCM2838_SPI_BASE + 123; {IRQ_TYPE_LEVEL_HIGH}

 {CPG}
 BCM2838_IRQ_CPG            = BCM2838_SPI_BASE + 124; {IRQ_TYPE_LEVEL_HIGH}

 {Random Number Generator}
 BCM2838_IRQ_RNG            = BCM2838_SPI_BASE + 125; {IRQ_TYPE_LEVEL_HIGH}

 {EMMC0 (SDHCI)}
 BCM2838_IRQ_EMMC0          = BCM2838_SPI_BASE + 126; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_SDHCI          = BCM2838_IRQ_EMMC0;

 {EMMC2 (SDHCI)}
 BCM2838_IRQ_EMMC2          = BCM2838_SPI_BASE + 126; {IRQ_TYPE_LEVEL_HIGH}

 {ETH_PCIe Secure}
 BCM2838_IRQ_ETH_PCIE_SEC   = BCM2838_SPI_BASE + 127; {IRQ_TYPE_LEVEL_HIGH}

 {ETH_PCIe L2 IRQs (BCM2838_SPI_BASE + 128 to 184)}
 {Thermal (AVS)}
 BCM2838_IRQ_THERMAL        = BCM2838_SPI_BASE + 137; {IRQ_TYPE_LEVEL_HIGH}

 {PCIe}
 BCM2838_IRQ_PCIE_INTA      = BCM2838_SPI_BASE + 143; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PCIE_INTB      = BCM2838_SPI_BASE + 144; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PCIE_INTC      = BCM2838_SPI_BASE + 145; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PCIE_INTD      = BCM2838_SPI_BASE + 146; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_PCIE_MSI       = BCM2838_SPI_BASE + 148; {IRQ_TYPE_LEVEL_HIGH}

 {GENET}
 BCM2838_IRQ_GENET_0        = BCM2838_SPI_BASE + 157; {IRQ_TYPE_LEVEL_HIGH}
 BCM2838_IRQ_GENET_1        = BCM2838_SPI_BASE + 158; {IRQ_TYPE_LEVEL_HIGH}

 {XHCI (Additional on chip generic XHCI USB host controller)}
 BCM2838_IRQ_XHCI           = BCM2838_SPI_BASE + 176; {IRQ_TYPE_LEVEL_HIGH} {This is separate to the VL805 XHCI controller}

 {Number of shared IRQs}
 BCM2838_SHARED_IRQ_COUNT = 988; {SPI 32-1019}

 {Number of private IRQs}
 BCM2838_PRIVATE_IRQ_COUNT = 16; {PPI 16-31}

 {Number of software IRQs}
 BCM2838_SOFTWARE_IRQ_COUNT = 16; {SGI 0-15}

 {Number of reserved IRQs}
 BCM2838_RESERVED_IRQ_COUNT = 4; {ID 1020-1023}

 {Total number of IRQs available}
 BCM2838_IRQ_COUNT = BCM2838_SHARED_IRQ_COUNT + BCM2838_PRIVATE_IRQ_COUNT + BCM2838_SOFTWARE_IRQ_COUNT + BCM2838_RESERVED_IRQ_COUNT;

 {Total number of FIQs available}
 BCM2838_FIQ_COUNT = BCM2838_IRQ_COUNT;

const
 {System Timer frequencies}
 BCM2838_SYSTEM_TIMER_FREQUENCY = 1000000; {Default clock frequency of the BCM2838 System Timer (1MHz)}

 {System Timer Control/Status register bits (See Section 12)}
 BCM2838_SYSTEM_TIMER_CS_0 = (1 shl 0); {Already used by the VideoCore GPU (Do not use)}
 BCM2838_SYSTEM_TIMER_CS_1 = (1 shl 1);
 BCM2838_SYSTEM_TIMER_CS_2 = (1 shl 2); {Already used by the VideoCore GPU (Do not use)}
 BCM2838_SYSTEM_TIMER_CS_3 = (1 shl 3);

const
 {DMA Control and Status register bits (See Section 4)}
 BCM2838_DMA_CS_ACTIVE                         = (1 shl 0);   {Activate the DMA (This bit enables the DMA. The DMA will start if this bit is set and the CB_ADDR is non zero. The DMA transfer can be paused and resumed by clearing, then setting it again)}
 BCM2838_DMA_CS_END                            = (1 shl 1);   {DMA End Flag (Set when the transfer described by the current control block is complete. Write 1 to clear)}
 BCM2838_DMA_CS_INT                            = (1 shl 2);   {Interrupt Status (This is set when the transfer for the CB ends and INTEN is set to 1. Write 1 to clear)}
 BCM2838_DMA_CS_DREQ                           = (1 shl 3);   {DREQ State (Indicates the state of the selected DREQ (Data Request) signal, ie. the DREQ selected by the PERMAP field of the transfer info)}
 BCM2838_DMA_CS_PAUSED                         = (1 shl 4);   {DMA Paused State (Indicates if the DMA is currently paused and not transferring data)}
 BCM2838_DMA_CS_DREQ_PAUSED                    = (1 shl 5);   {DMA Paused by DREQ State (Indicates if the DMA is currently paused and not transferring data due to the DREQ being inactive)}
 BCM2838_DMA_CS_WAITING_FOR_OUTSTANDING_WRITES = (1 shl 6);   {DMA is Waiting for the Last Write to be Received (Indicates if the DMA is currently waiting for any outstanding writes to be received, and is not transferring data)}
 {Bit 7 Reserved - Write as 0, read as don't care}
 BCM2838_DMA_CS_ERROR                          = (1 shl 8);   {DMA Error (Indicates if the DMA has detected an error)}
 {Bits 9:15 Reserved - Write as 0, read as don't care}
 BCM2838_DMA_CS_PRIORITY                       = ($F shl 16); {AXI Priority Level (Sets the priority of normal AXI bus transactions. Zero is the lowest priority)}
 BCM2838_DMA_CS_PANIC_PRIORITY                 = ($F shl 20); {AXI Panic Priority Level (Sets the priority of panicking AXI bus transactions)}
 {Bits 24:27 Reserved - Write as 0, read as don't care}
 BCM2838_DMA_CS_WAIT_FOR_OUTSTANDING_WRITES    = (1 shl 28);  {Wait for outstanding writes (When set to 1, the DMA will keep a tally of the AXI writes going out and the write responses coming in)}
 BCM2838_DMA_CS_DISDEBUG                       = (1 shl 29);  {Disable debug pause signal (When set to 1, the DMA will not stop when the debug pause signal is asserted)}
 BCM2838_DMA_CS_ABORT                          = (1 shl 30);  {Abort DMA (Writing a 1 to this bit will abort the current DMA CB. The DMA will load the next CB and attempt to continue. The bit cannot be read, and will self clear)}
 BCM2838_DMA_CS_RESET                          = (1 shl 31);  {DMA Channel Reset (Writing a 1 to this bit will reset the DMA. The bit cannot be read, and will self clear)}

 BCM2838_DMA_CS_PRIORITY_SHIFT = 16;
 BCM2838_DMA_CS_PANIC_PRIORITY_SHIFT = 20;

 BCM2838_DMA_CS_PRIORITY_DEFAULT = 0;
 BCM2838_DMA_CS_PRIORITY_MAX     = 15;

 {DMA Transfer Information bits (See Section 4)}
 BCM2838_DMA_TI_INTEN          = (1 shl 0);    {Interrupt Enable (1 = Generate an interrupt when the transfer described by the current Control Block completes)}
 BCM2838_DMA_TI_2DMODE         = (1 shl 1);    {2D Mode (1 = 2D mode interpret the TXFR_LEN register as YLENGTH number of transfers each of XLENGTH, and add the strides to the address after each transfer)}
 {Bit 2 Reserved - Write as 0, read as don't care}
 BCM2838_DMA_TI_WAIT_RESP      = (1 shl 3);    {Wait for a Write Response (When set this makes the DMA wait until it receives the AXI write response for each write. This ensures that multiple writes cannot get stacked in the AXI bus pipeline)}
 BCM2838_DMA_TI_DEST_INC       = (1 shl 4);    {Destination Address Increment (1 = Destination address increments after each write The address will increment by 4, if DEST_WIDTH=0 else by 32)}
 BCM2838_DMA_TI_DEST_WIDTH     = (1 shl 5);    {Destination Transfer Width (1 = Use 128-bit destination write width. 0 = Use 32-bit destination write width)}
 BCM2838_DMA_TI_DEST_DREQ      = (1 shl 6);    {Control Destination Writes with DREQ (1 = The DREQ selected by PERMAP will gate the destination writes)}
 BCM2838_DMA_TI_DEST_IGNORE    = (1 shl 7);    {Ignore Writes (1 = Do not perform destination writes. 0 = Write data to destination)}
 BCM2838_DMA_TI_SRC_INC        = (1 shl 8);    {Source Address Increment (1 = Source address increments after each read. The address will increment by 4, if S_WIDTH=0 else by 32)}
 BCM2838_DMA_TI_SRC_WIDTH      = (1 shl 9);    {Source Transfer Width (1 = Use 128-bit source read width. 0 = Use 32-bit source read width)}
 BCM2838_DMA_TI_SRC_DREQ       = (1 shl 10);   {Control Source Reads with DREQ (1 = The DREQ selected by PERMAP will gate the source reads)}
 BCM2838_DMA_TI_SRC_IGNORE     = (1 shl 11);   {Ignore Reads (1 = Do not perform source reads. In addition, destination writes will zero all the write strobes. This is used for fast cache fill operations)}
 BCM2838_DMA_TI_BURST_LENGTH   = ($F shl 12);  {Burst Transfer Length (Indicates the burst length of the DMA transfers)}
 BCM2838_DMA_TI_PERMAP         = ($1F shl 16); {Peripheral Mapping (Indicates the peripheral number (1-31) whose ready signal shall be used to control the rate of the transfers)}
 BCM2838_DMA_TI_WAITS          = ($3E shl 21); {Add Wait Cycles (This slows down the DMA throughput by setting the number of dummy cycles burnt after each DMA read or write operation is completed)}
 BCM2838_DMA_TI_NO_WIDE_BURSTS = (1 shl 26);   {Don't Do wide writes as a 2 beat burst (This prevents the DMA from issuing wide writes as 2 beat AXI bursts. This is an inefficient access mode, so the default is to use the bursts)}
 {Bits 27:31 Reserved - Write as 0, read as don't care}

 {Note: BCM2838_DMA_TI_2DMODE, BCM2838_DMA_TI_DEST_IGNORE, BCM2838_DMA_TI_SRC_IGNORE and BCM2838_DMA_TI_NO_WIDE_BURSTS not available on DMA Lite channels}

 BCM2838_DMA_TI_PERMAP_SHIFT = 16;
 BCM2838_DMA_TI_BURST_LENGTH_SHIFT = 12;

 BCM2838_DMA_TI_BURST_LENGTH_DEFAULT = 0;
 BCM2838_DMA_TI_BURST_LENGTH_MAX     = 15;

 {DMA Transfer Length bits (See Section 4)}
 BCM2838_DMA_TXFR_LEN_XLENGTH = ($FFFF shl 0);  {Transfer Length in bytes}
 BCM2838_DMA_TXFR_LEN_YLENGTH = ($3FFF shl 16); {When in 2D mode, This is the Y transfer length, indicating how many xlength transfers are performed. When in normal linear mode this becomes the top bits of the XLENGTH}

 {Note: BCM2838_DMA_TXFR_LEN_YLENGTH not available on DMA Lite channels}

 {DMA 2D Stride bits (See Section 4)}
 BCM2838_DMA_STRIDE_S_STRIDE  = ($FFFF shl 0);  {Destination Stride (2D Mode) (Signed (2 s complement) byte increment to apply to the destination address at the end of each row in 2D mode)}
 BCM2838_DMA_STRIDE_D_STRIDE  = ($FFFF shl 16); {Source Stride (2D Mode) (Signed (2 s complement) byte increment to apply to the source address at the end of each row in 2D mode)}

 {Note: BCM2838_DMA_STRIDE_S_STRIDE and BCM2838_DMA_STRIDE_D_STRIDE not available on DMA Lite channels}

 {DMA Debug register bits (See Section 4)}
 BCM2838_DMA_DEBUG_READ_LAST_NOT_SET_ERROR = (1 shl 0);     {Read Last Not Set Error}
 BCM2838_DMA_DEBUG_FIFO_ERROR              = (1 shl 1);     {Fifo Error}
 BCM2838_DMA_DEBUG_READ_ERROR              = (1 shl 2);     {Slave Read Response Error}
 {Bit 3 Reserved - Write as 0, read as don't care}
 BCM2838_DMA_DEBUG_OUTSTANDING_WRITES      = ($F shl 4);    {DMA Outstanding Writes Counter}
 BCM2838_DMA_DEBUG_DMA_ID                  = ($FF shl 8);   {DMA ID}
 BCM2838_DMA_DEBUG_DMA_STATE               = ($1FF shl 16); {DMA State Machine State}
 BCM2838_DMA_DEBUG_VERSION                 = (7 shl 25);    {DMA Version}
 BCM2838_DMA_DEBUG_LITE                    = (1 shl 28);    {DMA Lite}
 {Bits 29:31 Reserved - Write as 0, read as don't care}

 {DMA4 Control and Status register bits (See Section 4)}
 BCM2838_DMA4_CS_ACTIVE                         = (1 shl 0);   {Activate the DMA4 (This bit enables the DMA4 to start transferring data)}
 BCM2838_DMA4_CS_END                            = (1 shl 1);   {End Flag (Set when the transfer described by the current Control Block is complete)}
 BCM2838_DMA4_CS_INT                            = (1 shl 2);   {Interrupt Status (If interrupts are enabled (INTEN is set to 1) the interrupt is set when the transfer for the current CB is completed)}
 BCM2838_DMA4_CS_DREQ                           = (1 shl 3);   {DREQ State (Indicates the state of the selected DREQ (Data Request)signal)}
 BCM2838_DMA4_CS_RD_PAUSED                      = (1 shl 4);   {DMA read Paused State (Indicates that the DMA4 is currently paused and not reading data)}
 BCM2838_DMA4_CS_WR_PAUSED                      = (1 shl 5);   {DMA Write Paused State (Indicates that the DMA4 is currently paused and not writing data)}
 BCM2838_DMA4_CS_DREQ_STOPS_DMA                 = (1 shl 6);   {DMA Paused by DREQ State (This indicates that the DMA4 is currently paused and not transferring data due to the selected DREQ being inactive)}
 BCM2838_DMA4_CS_WAITING_FOR_OUTSTANDING_WRITES = (1 shl 7);   {The DMA4 is Waiting for all the Write Response to be returned}
 {Bits 8:9 Reserved}
 BCM2838_DMA4_CS_ERROR                          = (1 shl 10);  {DMA Error (Indicates if the DMA4 has detected an error)}
 {Bits 11:15 Reserved}
 BCM2838_DMA4_CS_QOS_MASK                       = ($F shl 16); {AXI QOS Level (Sets the QOS level of normal AXI bus transactions)}
 BCM2838_DMA4_CS_QOS_DEFAULT                    = (0 shl 16);
 BCM2838_DMA4_CS_PANIC_QOS_MASK                 = ($F shl 20); {AXI Panic QOS Level (Sets the QOS level of AXI bus transactions when the DMA4 is panicking)}
 BCM2838_DMA4_CS_PANIC_QOS_DEFAULT              = (0 shl 20);
 BCM2838_DMA4_CS_DMA_BUSY                       = (1 shl 24);  {Indicates the DMA4 is BUSY (This indicates that the DMA4 is operating or waiting for outstanding data or otherwise in use)}
 BCM2838_DMA4_CS_OUTSTANDING_TRANSACTIONS       = (1 shl 25);  {Indicates that there are outstanding AXI transfers, either outstanding read data or outstanding write responses}
 {Bits 26:27 Reserved}
 BCM2838_DMA4_CS_WAIT_FOR_OUTSTANDING_WRITES    = (1 shl 28);  {Wait for outstanding writes (The DMA4 keeps a tally of the AXI writes requests going out and the write responses coming in)}
 BCM2838_DMA4_CS_DISDEBUG                       = (1 shl 29);  {Disable Debug Pause Signal (When set to 1, the DMA4 will not pause when the debug pause signal is asserted)}
 BCM2838_DMA4_CS_ABORT                          = (1 shl 30);  {Abort DMA (Writing a 1 to this bit will cleanly abort the current DMA transfer. The abort will cause the DMA4 to zero its length counters and thus it will complete the current transfer and wait until all outstanding bus activity has finished)}
 BCM2838_DMA4_CS_HALT                           = (1 shl 31);  {Halt DMA (Writing a 1 to this bit will cleanly halt the current DMA transfer. The halt will cause the DMA4 to zero its length counters and thus it will complete the current transfer and wait until all outstanding bus activity has finished)}

 {DMA4 Control Block Address bits (See Section 4)}
 BCM2838_DMA4_CB_ADDR_MASK  = $0000001FFFFFFFE0;
 BCM2838_DMA4_CB_ADDR_SHIFT = 5; {Write Address shr 5 to the register}

 {DMA4 Debug register bits (See Section 4)}
 BCM2838_DMA4_DEBUG_WRITE_ERROR           = (1 shl 0);   {Slave Write Response Error}
 BCM2838_DMA4_DEBUG_FIFO_ERROR            = (1 shl 1);   {FIFO Error}
 BCM2838_DMA4_DEBUG_READ_ERROR            = (1 shl 2);   {Slave Read Response Error}
 BCM2838_DMA4_DEBUG_READ_CB_ERROR         = (1 shl 3);   {Slave Read Response Error During Control Block Read}
 {Bits 4:7 Reserved}
 BCM2838_DMA4_DEBUG_INT_ON_ERROR          = (1 shl 8);   {Generate an interrupt if an error is detected}
 BCM2838_DMA4_DEBUG_HALT_ON_ERROR         = (1 shl 9);   {Instruct the DMA4 to HALT if it detects an error.}
 BCM2838_DMA4_DEBUG_ABORT_ON_ERROR        = (1 shl 10);  {Instruct the DMA4 to ABORT if it detects an error}
 BCM2838_DMA4_DEBUG_DISABLE_CLK_GATE      = (1 shl 11);  {Disable the clock gating logic}
 {Bits 12:13 Reserved}
 BCM2838_DMA4_DEBUG_R_STATE_MASK          = ($F shl 14); {Read State Machine State (Returns the value of the DMA4 engine’s read state machine)}
 BCM2838_DMA4_DEBUG_R_IDLE                = (0 shl 14);
 BCM2838_DMA4_DEBUG_R_WAIT_CB_DATA        = (1 shl 14);
 BCM2838_DMA4_DEBUG_R_CALC                = (2 shl 14);
 BCM2838_DMA4_DEBUG_R_READ4K              = (3 shl 14);
 BCM2838_DMA4_DEBUG_R_READING             = (4 shl 14);
 BCM2838_DMA4_DEBUG_R_READFIFO_FULL       = (5 shl 14);
 BCM2838_DMA4_DEBUG_R_WAIT_WRITE_COMPLETE = (6 shl 14);
 BCM2838_DMA4_DEBUG_W_STATE_MASK          = ($F shl 18); {Write State Machine State (Returns the value of the DMA4 engine’s write state machine)}
 BCM2838_DMA4_DEBUG_W_IDLE                = (0 shl 18);
 BCM2838_DMA4_DEBUG_W_PRELOAD             = (1 shl 18);
 BCM2838_DMA4_DEBUG_W_CALC                = (2 shl 18);
 BCM2838_DMA4_DEBUG_W_WRITE4K             = (3 shl 18);
 BCM2838_DMA4_DEBUG_W_READFIFO_EMPTY      = (4 shl 18);
 BCM2838_DMA4_DEBUG_W_WAIT_OUTSTANDING    = (5 shl 18);
 {Bit 22 Reserved}
 BCM2838_DMA4_DEBUG_RESET                 = (1 shl 23);  {DMA Reset (This is a hard reset of the DMA4 state machine and certain internal registers)}
 BCM2838_DMA4_DEBUG_ID_MASK               = ($F shl 24); {ID (Returns the ID of this DMA4. This is also used as the AXI subid)}
 BCM2838_DMA4_DEBUG_VERSION_MASK          = ($F shl 28); {DMA Version (DMA version number, indicating control bit field changes)}

 {DMA4 Transfer Information bits (See Section 4)}
 BCM2838_DMA4_TI_INTEN           = (1 shl 0);    {Interrupt Enable}
 BCM2838_DMA4_TI_TDMODE          = (1 shl 1);    {2D Mode (Perform a 2D transfer instead of a normal linear transfer. In 2D mode the DMA4 will interpret the length field as an X and a Y length. It will execute Y+1 transfers each of length X)}
 BCM2838_DMA4_TI_WAIT_RESP       = (1 shl 2);    {Wait for a Write Response (When set this makes the DMA4 wait until it receives the AXI write response for each write)}
 BCM2838_DMA4_TI_WAIT_RD_RESP    = (1 shl 3);    {Wait for a Read Response (When set this makes the DMA4 wait until it receives all the data from each read)}
 {Bits 4:8 Reserved}
 BCM2838_DMA4_TI_PERMAP_MASK     = ($1F shl 9);  {Peripheral Mapping (Indicates the DREQ of selected peripheral (1-31))}
 BCM2838_DMA4_TI_PERMAP_SHIFT    = 9;
 BCM2838_DMA4_TI_S_DREQ          = (1 shl 14);   {Control Source Reads with DREQ (This is used when reading from a peripheral that has a DREQ flow control available)}
 BCM2838_DMA4_TI_D_DREQ          = (1 shl 15);   {Control Destination Writes with DREQ (This is used when writing to a peripheral that has a DREQ flow control available to control the data flow)}
 BCM2838_DMA4_TI_S_WAITS_MASK    = ($FF shl 16); {Read Wait Cycles (This slows down the DMA throughput by setting the number of dummy cycles burnt before each DMA AXI read operation is started)}
 BCM2838_DMA4_TI_S_WAITS_DEFAULT = (0 shl 16);
 BCM2838_DMA4_TI_D_WAITS_MASK    = ($FF shl 24); {Write Wait Cycles (This slows down the DMA throughput by setting the number of dummy cycles before each AXI Write operation is started)}
 BCM2838_DMA4_TI_D_WAITS_DEFAULT = (0 shl 24);

 {DMA4 Source Address bits (See Section 4)}
 BCM2838_DMA4_SRC_ADDR_MASK = $00000000FFFFFFFF;

 {DMA4 Source Information bits (See Section 4)}
 BCM2838_DMA4_SRCI_ADDR_MASK            = ($FF shl 0);    {High Bits of the Source Address [40:32]}
 BCM2838_DMA4_SRCI_BURST_LENGTH_MASK    = ($F shl 8);     {Burst Transfer Length}
 BCM2838_DMA4_SRCI_BURST_LENGTH_SHIFT   = 8;
 BCM2838_DMA4_SRCI_BURST_LENGTH_DEFAULT = (0 shl 8);
 BCM2838_DMA4_SRCI_BURST_LENGTH_MAX     = (15 shl 8);
 BCM2838_DMA4_SRCI_INC                  = (1 shl 12);     {Increment the Source Address}
 BCM2838_DMA4_SRCI_SIZE_MASK            = (3 shl 13);     {Source Transfer Width (The DMA4 will perform all AXI source reads with this AXI transfer width)}
 BCM2838_DMA4_SRCI_SIZE_256             = (3 shl 13);     {Note: On the BCM2711 the width cannot be set larger than 128}
 BCM2838_DMA4_SRCI_SIZE_128             = (2 shl 13);
 BCM2838_DMA4_SRCI_SIZE_64              = (1 shl 13);
 BCM2838_DMA4_SRCI_SIZE_32              = (0 shl 13);
 BCM2838_DMA4_SRCI_IGNORE               = (1 shl 15);     {Ignore Reads (The DMA4 will perform a normal transfer except that it will not produce any reads. The DMA4 will write zero data)}
 BCM2838_DMA4_SRCI_STRIDE               = ($FFFF shl 16); {Source Stride (This is only used in 2D transfer mode (TDMODE))}

 {DMA4 Dest Address bits (See Section 4)}
 BCM2838_DMA4_DEST_ADDR_MASK = $00000000FFFFFFFF;

 {DMA4 Dest Information bits (See Section 4)}
 BCM2838_DMA4_DESTI_ADDR_MASK            = ($FF shl 0);    {High Bits of the Destination Address [40:32]}
 BCM2838_DMA4_DESTI_BURST_LENGTH_MASK    = ($F shl 8);     {Burst Transfer Length}
 BCM2838_DMA4_DESTI_BURST_LENGTH_SHIFT   = 8;
 BCM2838_DMA4_DESTI_BURST_LENGTH_DEFAULT = (0 shl 8);
 BCM2838_DMA4_DESTI_BURST_LENGTH_MAX     = (15 shl 8);
 BCM2838_DMA4_DESTI_INC                  = (1 shl 12);     {Destination Address Increment}
 BCM2838_DMA4_DESTI_SIZE_MASK            = (3 shl 13);     {Destination Transfer Width (The DMA4 will perform all AXI destination writes with this AXI transfer width)}
 BCM2838_DMA4_DESTI_SIZE_256             = (3 shl 13);     {Note: On the BCM2711 the width cannot be set larger than 128}
 BCM2838_DMA4_DESTI_SIZE_128             = (2 shl 13);
 BCM2838_DMA4_DESTI_SIZE_64              = (1 shl 13);
 BCM2838_DMA4_DESTI_SIZE_32              = (0 shl 13);
 BCM2838_DMA4_DESTI_IGNORE               = (1 shl 15);     {Ignore Destination Writes}
 BCM2838_DMA4_DESTI_STRIDE               = ($FFFF shl 16); {Destination Stride (This is only used in 2D transfer mode (TDMODE))}

 {DMA4 Transfer Length bits (See Section 4)}
 BCM2838_DMA4_LEN_XLENGTH_MASK = ($FFFF shl 0);  {Transfer Length in bytes}
 BCM2838_DMA4_LEN_YLENGTH_MASK = ($3FFF shl 16); {When in 2D mode, This is the Y transfer length, indicating how many xlength transfers are performed. When in normal linear mode this becomes the top bits of the XLENGTH}
 {Bits 30:31 Reserved}

 {DMA4 Next Control Block Address bits (See Section 4)}
 BCM2838_DMA4_NEXT_CB_ADDR_MASK  = $0000001FFFFFFFE0;
 BCM2838_DMA4_NEXT_CB_ADDR_SHIFT = 5; {Write Address shr 5 to the register}

 {DMA4 Debug2 register Address bits (See Section 4)}
 BCM2838_DMA4_DEBUG2_OUTSTANDING_WRITES_MASK = ($1FF shl 0);  {Outstanding Write Response Count}
 {Bits 9:15 Reserved}
 BCM2838_DMA4_DEBUG2_OUTSTANDING_READS_MASK  = ($1FF shl 16); {Outstanding read Words Count}
 {Bits 25:31 Reserved}

 {DMA Engine Interrupt Status register bits (See Section 4)}
 BCM2838_DMA_INT_STATUS_0  = (1 shl 0);
 BCM2838_DMA_INT_STATUS_1  = (1 shl 1);
 BCM2838_DMA_INT_STATUS_2  = (1 shl 2);
 BCM2838_DMA_INT_STATUS_3  = (1 shl 3);
 BCM2838_DMA_INT_STATUS_4  = (1 shl 4);
 BCM2838_DMA_INT_STATUS_5  = (1 shl 5);
 BCM2838_DMA_INT_STATUS_6  = (1 shl 6);
 BCM2838_DMA_INT_STATUS_7  = (1 shl 7);
 BCM2838_DMA_INT_STATUS_8  = (1 shl 8);
 BCM2838_DMA_INT_STATUS_9  = (1 shl 9);
 BCM2838_DMA_INT_STATUS_10 = (1 shl 10);
 BCM2838_DMA_INT_STATUS_11 = (1 shl 11);
 BCM2838_DMA_INT_STATUS_12 = (1 shl 12);
 BCM2838_DMA_INT_STATUS_13 = (1 shl 13);
 BCM2838_DMA_INT_STATUS_14 = (1 shl 14);
 BCM2838_DMA_INT_STATUS_15 = (1 shl 15);
 {Bits 16:31 Reserved}

 {DMA Engine Enable register bits (See Section 4)}
 BCM2838_DMA_ENABLE_0  = (1 shl 0);
 BCM2838_DMA_ENABLE_1  = (1 shl 1);
 BCM2838_DMA_ENABLE_2  = (1 shl 2);
 BCM2838_DMA_ENABLE_3  = (1 shl 3);
 BCM2838_DMA_ENABLE_4  = (1 shl 4);
 BCM2838_DMA_ENABLE_5  = (1 shl 5);
 BCM2838_DMA_ENABLE_6  = (1 shl 6);
 BCM2838_DMA_ENABLE_7  = (1 shl 7);
 BCM2838_DMA_ENABLE_8  = (1 shl 8);
 BCM2838_DMA_ENABLE_9  = (1 shl 9);
 BCM2838_DMA_ENABLE_10 = (1 shl 10);
 BCM2838_DMA_ENABLE_11 = (1 shl 11);
 BCM2838_DMA_ENABLE_12 = (1 shl 12);
 BCM2838_DMA_ENABLE_13 = (1 shl 13);
 BCM2838_DMA_ENABLE_14 = (1 shl 14);
 {Bits 15:23 Reserved}
 BCM2838_DMA_PAGE_MASK        = ($F shl 24); {Set the 1G SDRAM ram page that the 30-bit DMA engines (DMA0-6) will access when addressing the 1G uncached range C000_0000->ffff_ffff}
 BCM2838_DMA_PAGE_DEFAULT     = (0 shl 24);  {E.g. setting this to 1 will mean that when the DMA writes to C000_0000 (uncached) the final address in SDRAM will be 4000_0000 ( page<<30 | addr[29:0] )}
 BCM2838_DMA_PAGELITE_MASK    = ($F shl 28); {Set the 1G SDRAM ram page that the DMA Lite engines (DMA7-10) will access when addressing the 1G uncached range C000_0000->ffff_ffff}
 BCM2838_DMA_PAGELITE_DEFAULT = (0 shl 28);  {E.g. setting this to 1 will mean that when the DMA writes to C000_0000 (uncached) the final address in SDRAM will be 4000_0000 ( pagelite<<30 | addr[29:0] )}

 {DMA Engine DREQ Peripherals (See Section 4)}
 BCM2838_DMA_DREQ_NONE         = 0;
 BCM2838_DMA_DREQ_DSI0         = 1;
 BCM2838_DMA_DREQ_PWM1         = 1;  {The alternate DREQs are available by changing the DMA_CNTRL_MUX bits in the PACTL_CS register}
 BCM2838_DMA_DREQ_PCMTX        = 2;
 BCM2838_DMA_DREQ_PCMRX        = 3;
 BCM2838_DMA_DREQ_SMI          = 4;
 BCM2838_DMA_DREQ_PWM0         = 5;
 BCM2838_DMA_DREQ_SPI0TX       = 6;
 BCM2838_DMA_DREQ_SPI0RX       = 7;
 BCM2838_DMA_DREQ_BSCSPITX     = 8;  {BSC/SPI Slave}
 BCM2838_DMA_DREQ_BSCSPIRX     = 9;  {BSC/SPI Slave}
 BCM2838_DMA_DREQ_HDMI0        = 10;
 BCM2838_DMA_DREQ_EMMC0        = 11; {SDHCI}
 BCM2838_DMA_DREQ_UART0TX      = 12;
 BCM2838_DMA_DREQ_EMMC1        = 13; {SDHOST}
 BCM2838_DMA_DREQ_UART0RX      = 14;
 BCM2838_DMA_DREQ_DSI1         = 15;
 BCM2838_DMA_DREQ_SPI1TX       = 16;
 BCM2838_DMA_DREQ_HDMI1        = 17;
 BCM2838_DMA_DREQ_SPI1RX       = 18;
 BCM2838_DMA_DREQ_UART3TX      = 19;
 BCM2838_DMA_DREQ_SPI4TX       = 19; {The alternate DREQs are available by changing the DMA_CNTRL_MUX bits in the PACTL_CS register}
 BCM2838_DMA_DREQ_UART3RX      = 20;
 BCM2838_DMA_DREQ_SPI4RX       = 20; {The alternate DREQs are available by changing the DMA_CNTRL_MUX bits in the PACTL_CS register}
 BCM2838_DMA_DREQ_UART5TX      = 21;
 BCM2838_DMA_DREQ_SPI5TX       = 21; {The alternate DREQs are available by changing the DMA_CNTRL_MUX bits in the PACTL_CS register}
 BCM2838_DMA_DREQ_UART5RX      = 22;
 BCM2838_DMA_DREQ_SPI5RX       = 22; {The alternate DREQs are available by changing the DMA_CNTRL_MUX bits in the PACTL_CS register}
 BCM2838_DMA_DREQ_SPI6TX       = 23;
 BCM2838_DMA_DREQ_SCALER_FIFO0 = 24; {Scaler FIFO 0 & SMI} {The SMI element can be disabled by setting the SMI_DISABLE bit in the DMA_DREQ_CONTROL register}
 BCM2838_DMA_DREQ_SCALER_FIFO1 = 25; {Scaler FIFO 1 & SMI} {The SMI element can be disabled by setting the SMI_DISABLE bit in the DMA_DREQ_CONTROL register}
 BCM2838_DMA_DREQ_SCALER_FIFO2 = 26; {Scaler FIFO 2 & SMI} {The SMI element can be disabled by setting the SMI_DISABLE bit in the DMA_DREQ_CONTROL register}
 BCM2838_DMA_DREQ_SPI6RX       = 27;
 BCM2838_DMA_DREQ_UART2TX      = 28;
 BCM2838_DMA_DREQ_UART2RX      = 29;
 BCM2838_DMA_DREQ_UART4TX      = 30;
 BCM2838_DMA_DREQ_UART4RX      = 31;

const
 {BSC (I2C0/1/2) Control register bits (See 3.2)}
 BCM2838_BSC_C_I2CEN = (1 shl 15); {I2C Enable (0 = BSC controller is disabled / 1 = BSC controller is enabled)}
 BCM2838_BSC_C_INTR  = (1 shl 10); {INTR Interrupt on RX (0 = Don t generate interrupts on RXR condition / 1 = Generate interrupt while RXR = 1)}
 BCM2838_BSC_C_INTT  = (1 shl 9);  {INTT Interrupt on TX (0 = Don t generate interrupts on TXW condition / 1 = Generate interrupt while TXW = 1)}
 BCM2838_BSC_C_INTD  = (1 shl 8);  {INTD Interrupt on DONE (0 = Don t generate interrupts on DONE condition / 1 = Generate interrupt while DONE = 1)}
 BCM2838_BSC_C_ST    = (1 shl 7);  {ST Start Transfer (0 = No action / 1 = Start a new transfer. One shot operation. Read back as 0)}
 BCM2838_BSC_C_CLEAR = (1 shl 5);  {CLEAR FIFO Clear (00 = No action / x1 = Clear FIFO. One shot operation / 1x = Clear FIFO. One shot operation)}
 BCM2838_BSC_C_READ  = (1 shl 0);  {READ Read Transfer (0 = Write Packet Transfer / 1 = Read Packet Transfer)}

 {BSC (I2C0/1/2) Status register bits (See 3.2)}
 BCM2838_BSC_S_CLKT = (1 shl 9); {CLKT Clock Stretch Timeout (0 = No errors detected. 1 = Slave has held the SCL signal low (clock stretching) for longer and that specified in the I2CCLKT register Cleared by writing 1 to the field)}
 BCM2838_BSC_S_ERR  = (1 shl 8); {ERR ACK Error (0 = No errors detected. 1 = Slave has not acknowledged its address. Cleared by writing 1 to the field)}
 BCM2838_BSC_S_RXF  = (1 shl 7); {RXF - FIFO Full (0 = FIFO is not full. 1 = FIFO is full. If a read is underway, no further serial data will be received until data is read from FIFO)}
 BCM2838_BSC_S_TXE  = (1 shl 6); {TXE - FIFO Empty (0 = FIFO is not empty. 1 = FIFO is empty. If a write is underway, no further serial data can be transmitted until data is written to the FIFO)}
 BCM2838_BSC_S_RXD  = (1 shl 5); {RXD - FIFO contains Data (0 = FIFO is empty. 1 = FIFO contains at least 1 byte. Cleared by reading sufficient data from FIFO)}
 BCM2838_BSC_S_TXD  = (1 shl 4); {TXD - FIFO can accept Data (0 = FIFO is full. The FIFO cannot accept more data. 1 = FIFO has space for at least 1 byte)}
 BCM2838_BSC_S_RXR  = (1 shl 3); {RXR - FIFO needs Reading (full) (0 = FIFO is less than full and a read is underway. 1 = FIFO is or more full and a read is underway. Cleared by reading sufficient data from the FIFO)}
 BCM2838_BSC_S_TXW  = (1 shl 2); {TXW - FIFO needs Writing (full) (0 = FIFO is at least full and a write is underway (or sufficient data to send). 1 = FIFO is less then full and a write is underway. Cleared by writing sufficient data to the FIFO)}
 BCM2838_BSC_S_DONE = (1 shl 1); {DONE Transfer Done (0 = Transfer not completed. 1 = Transfer complete. Cleared by writing 1 to the field)}
 BCM2838_BSC_S_TA   = (1 shl 0); {TA Transfer Active (0 = Transfer not active. 1 = Transfer active)}

 {BSC (I2C0/1/2) Data Length register bits (See 3.2)}
 BCM2838_BSC_DLEN_MASK = $FFFF; {Data Length. (Writing to DLEN specifies the number of bytes to be transmitted/received. Reading from DLEN when TA = 1 or DONE = 1, returns the number of bytes still to be transmitted or received)}

 {BSC (I2C0/1/2) Slave Address register bits (See 3.2)}
 BCM2838_BSC_A_MASK = $7F; {Slave Address.}

 {BSC (I2C0/1/2) Data FIFO register bits (See 3.2)}
 BCM2838_BSC_FIFO_MASK = $FF; {Writes to the register write transmit data to the FIFO. Reads from register reads received data from the FIFO.}
 BCM2838_BSC_FIFO_SIZE = 16;

 {BSC (I2C0/1/2) Clock Divider register bits (See 3.2)}
 BCM2838_BSC_CDIV_MASK = $FFFF; {Clock Divider (SCL = core clock / CDIV) (CDIV is always rounded down to an even number)}

 {BSC (I2C0/1/2) Data Delay register bits (See 3.2)}
 BCM2838_BSC_DEL_FEDL_MASK = ($FFFF shl 16); {FEDL Falling Edge Delay (Number of core clock cycles to wait after the falling edge of SCL before outputting next bit of data)}
 BCM2838_BSC_DEL_REDL_MASK = ($FFFF shl 0);  {REDL Rising Edge Delay (Number of core clock cycles to wait after the rising edge of SCL before reading the next bit of data)}
 BCM2838_BSC_DEL_FEDL_SHIFT = 16;
 BCM2838_BSC_DEL_REDL_SHIFT = 0;

 {BSC (I2C0/1/2) Clock Stretch Timeout register bits (See 3.2)}
 BCM2838_BSC_CLKT_TOUT_MASK = $FFFF; {TOUT Clock Stretch Timeout Value (Number of SCL clock cycles to wait after the rising edge of SCL before deciding that the slave is not responding)}

const
 {SPI0 register bits (See 10.5)}
 BCM2838_SPI0_CS_LEN_LONG = (1 shl 25); {Enable Long data word in Lossi mode if DMA_LEN is set (0 = writing to the FIFO will write a single byte / 1 = writing to the FIFO will write a 32 bit word)}
 BCM2838_SPI0_CS_DMA_LEN  = (1 shl 24); {Enable DMA mode in Lossi mode}
 BCM2838_SPI0_CS_CSPOL2   = (1 shl 23); {Chip Select 2 Polarity (0 = Chip select is active low / 1 = Chip select is active high)}
 BCM2838_SPI0_CS_CSPOL1   = (1 shl 22); {Chip Select 1 Polarity (0 = Chip select is active low / 1 = Chip select is active high)}
 BCM2838_SPI0_CS_CSPOL0   = (1 shl 21); {Chip Select 0 Polarity (0 = Chip select is active low / 1 = Chip select is active high)}
 BCM2838_SPI0_CS_RXF      = (1 shl 20); {RXF - RX FIFO Full (0 = RXFIFO is not full / 1 = RX FIFO is full. No further serial data will be sent/received until data is read from FIFO)}
 BCM2838_SPI0_CS_RXR      = (1 shl 19); {RXR RX FIFO needs Reading (full) (0 = RX FIFO is less than full (or not active TA = 0) / 1 = RX FIFO is or more full. Cleared by reading sufficient data from the RX FIFO or setting TA to 0)}
 BCM2838_SPI0_CS_TXD      = (1 shl 18); {TXD TX FIFO can accept Data (0 = TX FIFO is full and so cannot accept more data / 1 = TX FIFO has space for at least 1 byte)}
 BCM2838_SPI0_CS_RXD      = (1 shl 17); {RXD RX FIFO contains Data (0 = RX FIFO is empty / 1 = RX FIFO contains at least 1 byte)}
 BCM2838_SPI0_CS_DONE     = (1 shl 16); {DONE Transfer Done (0 = Transfer is in progress (or not active TA = 0) / 1 = Transfer is complete. Cleared by writing more data to the TX FIFO or setting TA to 0)}
 BCM2838_SPI0_CS_TE_EN    = (1 shl 15); {Unused}
 BCM2838_SPI0_CS_LMONO    = (1 shl 14); {Unused}
 BCM2838_SPI0_CS_LEN      = (1 shl 13); {LEN LoSSI enable (0 = The serial interface will behave as an SPI master / 1 = The serial interface will behave as a LoSSI master)}
 BCM2838_SPI0_CS_REN      = (1 shl 12); {REN Read Enable. If this bit is set, the SPI peripheral will be able to send data to this device (0 = We intend to write to the SPI peripheral / 1 = We intend to read from the SPI peripheral)}
 BCM2838_SPI0_CS_ADCS     = (1 shl 11); {ADCS Automatically Deassert Chip Select (0 = Don t automatically deassert chip select at the end of a DMA transfer chip select is manually controlled by software. / 1 = Automatically deassert chip select at the end of a DMA transfer as determined by SPIDLEN)}
 BCM2838_SPI0_CS_INTR     = (1 shl 10); {INTR Interrupt on RXR (0 = Don t generate interrupts on RX FIFO condition / 1 = Generate interrupt while RXR = 1)}
 BCM2838_SPI0_CS_INTD     = (1 shl 9);  {INTD Interrupt on Done (0 = Don t generate interrupt on transfer complete / 1 = Generate interrupt when DONE = 1)}
 BCM2838_SPI0_CS_DMAEN    = (1 shl 8);  {DMAEN DMA Enable (0 = No DMA requests will be issued / 1 = Enable DMA operation. Peripheral generates data requests. These will be taken in four-byte words until the SPIDLEN has been reached}
 BCM2838_SPI0_CS_TA       = (1 shl 7);  {Transfer Active (0 = Transfer not active / 1 = Transfer active)}
 BCM2838_SPI0_CS_CSPOL    = (1 shl 6);  {Chip Select Polarity (0 = Chip select lines are active low / 1 = Chip select lines are active high}
 BCM2838_SPI0_CS_CLEAR_RX = (1 shl 5);  {CLEAR FIFO Clear (00 = No action / x1 = Clear TX FIFO. One shot operation / 1x = Clear RX FIFO. One shot operation)}
 BCM2838_SPI0_CS_CLEAR_TX = (1 shl 4);  {As above}
 BCM2838_SPI0_CS_CPOL     = (1 shl 3);  {Clock Polarity (0 = Rest state of clock = low / 1 = Rest state of clock = high)}
 BCM2838_SPI0_CS_CPHA     = (1 shl 2);  {Clock Phase (0 = First SCLK transition at middle of data bit / 1 = First SCLK transition at beginning of data bit)}
 BCM2838_SPI0_CS_CS_0     = (0 shl 0);  {Chip Select (00 = Chip select 0 / 01 = Chip select 1 / 10 = Chip select 2 / 11 = Reserved}
 BCM2838_SPI0_CS_CS_1     = (1 shl 0);  {As above}
 BCM2838_SPI0_CS_CS_2     = (2 shl 0);  {As above}

 BCM2838_SPI0_CS_CS_MASK  = (3 shl 0);

 BCM2838_SPI0_FIFO_DMA_DATA = $FFFFFFFF; {DMA Mode (DMAEN set) If TA is clear, the first 32-bit write to this register will control SPIDLEN and SPICS. Subsequent reads and writes will be taken as four-byte data words to be read/written to the FIFOs}
 BCM2838_SPI0_FIFO_IRQ_DATA = $000000FF; {Poll/Interrupt Mode (DMAEN clear, TA set) Writes to the register write bytes to TX FIFO. Reads from register read bytes from the RX FIFO}

 BCM2838_SPI0_CLK_CDIV    = $0000FFFF; {Clock Divider (SCLK = Core Clock / CDIV) If CDIV is set to 0, the divisor is 65536. The divisor must be a multiple of 2. Odd numbers rounded down. The maximum SPI clock rate is of the APB clock}

 BCM2838_SPI0_DLEN_LEN    = $0000FFFF; {Data Length. The number of bytes to transfer. This field is only valid for DMA mode (DMAEN set) and controls how many bytes to transmit (and therefore receive)}

 BCM2838_SPI0_LTOH_TOH    = $0000000F; {This sets the Output Hold delay in APB clocks (A value of 0 causes a 1 clock delay)}

 BCM2838_SPI0_DC_RPANIC   = ($FF shl 24); {DMA Read Panic Threshold (Generate the Panic signal to the RX DMA engine whenever the RX FIFO level is greater than this amount)}
 BCM2838_SPI0_DC_RDREQ    = ($FF shl 16); {DMA Read Request Threshold (Generate A DREQ to the RX DMA engine whenever the RX FIFO level is greater than this amount) (RX DREQ is also generated if thetransfer has finished but the RXFIFO isn't empty)}
 BCM2838_SPI0_DC_TPANIC   = ($FF shl 8);  {DMA Write Panic Threshold (Generate the Panic signal to the TX DMA engine whenever the TX FIFO level is less than or equal to this amount)}
 BCM2838_SPI0_DC_TDREQ    = ($FF shl 0);  {DMA Write Request Threshold (Generate a DREQ signal to the TX DMA engine whenever the TX FIFO level is less than or equal to this amount)}

const
 {I2C / SPI Slave DR (Data) register bits (See 11.2)}
 BCM2838_I2CSPI_DR_RXFLEVEL_MASK  = ($1F shl 27); {RX FIFO Level}
 BCM2838_I2CSPI_DR_RXFLEVEL_SHIFT = 27;
 BCM2838_I2CSPI_DR_TXFLEVEL_MASK  = ($1F shl 22); {TX FIFO Level}
 BCM2838_I2CSPI_DR_TXFLEVEL_SHIFT = 22;
 BCM2838_I2CSPI_DR_RXBUSY         = (1 shl 21);   {Receive Busy}
 BCM2838_I2CSPI_DR_TXFE           = (1 shl 20);   {TX FIFO Empty}
 BCM2838_I2CSPI_DR_RXFF           = (1 shl 19);   {RX FIFO Full}
 BCM2838_I2CSPI_DR_TXFF           = (1 shl 18);   {TX FIFO Full}
 BCM2838_I2CSPI_DR_RXFE           = (1 shl 17);   {RX FIFO Empty}
 BCM2838_I2CSPI_DR_TXBUSY         = (1 shl 16);   {Transmit Busy}
 BCM2838_I2CSPI_DR_UE             = (1 shl 9);    {TX Underrun Error}
 BCM2838_I2CSPI_DR_OE             = (1 shl 8);    {RX Overrun Error}
 BCM2838_I2CSPI_DR_DATA_MASK      = ($FF shl 0);  {Received/Transferred data}

 {I2C / SPI Slave RSR (Status) register bits (See 11.2)}
 BCM2838_I2CSPI_RSR_UE = (1 shl 1); {TX Underrun Error}
 BCM2838_I2CSPI_RSR_OE = (1 shl 0); {RX Overrun Error}

 {I2C / SPI Slave SLV (Slave) register bits (See 11.2)}
 BCM2838_I2CSPI_SLV_ADDR_MASK = ($7F shl 0); {I2C Slave Address}

 {I2C / SPI Slave CR (Control) register bits (See 11.2)}
 BCM2838_I2CSPI_CR_INV_TXF  = (1 shl 13); {Inverse TX status flags}
 BCM2838_I2CSPI_CR_TESTFIFO = (1 shl 11); {TEST FIFO}
 BCM2838_I2CSPI_CR_INV_RXF  = (1 shl 10); {Inverse RX status flags}
 BCM2838_I2CSPI_CR_RXE      = (1 shl 9);  {Receive Enable}
 BCM2838_I2CSPI_CR_TXE      = (1 shl 8);  {Transmit Enable}
 BCM2838_I2CSPI_CR_BRK      = (1 shl 7);  {Break current operation}
 BCM2838_I2CSPI_CR_CPOL     = (1 shl 4);  {Clock Polarity}
 BCM2838_I2CSPI_CR_CPHA     = (1 shl 3);  {Clock Phase}
 BCM2838_I2CSPI_CR_I2C      = (1 shl 2);  {I2C Enable}
 BCM2838_I2CSPI_CR_SPI      = (1 shl 1);  {SPI Enable}
 BCM2838_I2CSPI_CR_EN       = (1 shl 0);  {Enable Device}

 {I2C / SPI Slave FR (Flags) register bits (See 11.2)}
 BCM2838_I2CSPI_RXFLEVEL_MASK  = ($1F shl 11); {RX FIFO Level}
 BCM2838_I2CSPI_RXFLEVEL_SHIFT = 11;
 BCM2838_I2CSPI_TXFLEVEL_MASK  = ($1F shl 6);  {TX FIFO Level}
 BCM2838_I2CSPI_TXFLEVEL_SHIFT = 6;
 BCM2838_I2CSPI_FR_RXBUSY      = (1 shl 5);    {Receive Busy}
 BCM2838_I2CSPI_FR_TXFE        = (1 shl 4);    {TX FIFO Empty}
 BCM2838_I2CSPI_FR_RXFF        = (1 shl 3);    {RX FIFO Full}
 BCM2838_I2CSPI_FR_TXFF        = (1 shl 2);    {TX FIFO Full}
 BCM2838_I2CSPI_FR_RXFE        = (1 shl 1);    {RX FIFO Empty}
 BCM2838_I2CSPI_FR_TXBUSY      = (1 shl 0);    {Transmit Busy}

 {I2C / SPI Slave IFLS (Interrupt FIFO Level Select) register bits (See 11.2)}
 BCM2838_I2CSPI_IFLS_RXIFLSEL_MASK  = (7 shl 3); {RX Interrupt FIFO Level Select}
 BCM2838_I2CSPI_IFLS_RXIFLSEL_SHIFT = 3;
 BCM2838_I2CSPI_IFLS_RXIFLSEL1_8    = (0 shl 3); {000 RX FIFO gets 1/8 full}
 BCM2838_I2CSPI_IFLS_RXIFLSEL1_4    = (1 shl 3); {001 RX FIFO gets 1/4 full}
 BCM2838_I2CSPI_IFLS_RXIFLSEL1_2    = (2 shl 3); {010 RX FIFO gets 1/2 full}
 BCM2838_I2CSPI_IFLS_RXIFLSEL3_4    = (3 shl 3); {011 RX FIFO gets 3/4 full}
 BCM2838_I2CSPI_IFLS_RXIFLSEL7_8    = (4 shl 3); {100 RX FIFO gets 7/8 full}
 BCM2838_I2CSPI_IFLS_TXIFLSEL_MASK  = (7 shl 0); {TX Interrupt FIFO Level Select}
 BCM2838_I2CSPI_IFLS_TXIFLSEL_SHIFT = 0;
 BCM2838_I2CSPI_IFLS_TXIFLSEL1_8    = (0 shl 0); {000 TX FIFO gets 1/8 full}
 BCM2838_I2CSPI_IFLS_TXIFLSEL1_4    = (1 shl 0); {001 TX FIFO gets 1/4 full}
 BCM2838_I2CSPI_IFLS_TXIFLSEL1_2    = (2 shl 0); {010 TX FIFO gets 1/2 full}
 BCM2838_I2CSPI_IFLS_TXIFLSEL3_4    = (3 shl 0); {011 TX FIFO gets 3/4 full}
 BCM2838_I2CSPI_IFLS_TXIFLSEL7_8    = (4 shl 0); {100 TX FIFO gets 7/8 full}

 {I2C / SPI Slave IMSC (Interrupt Mask Set Clear) register bits (See 11.2)}
 BCM2838_I2CSPI_IMSC_OEIM = (1 shl 3); {Overrun error interrupt mask}
 BCM2838_I2CSPI_IMSC_BEIM = (1 shl 2); {Break error interrupt mask}
 BCM2838_I2CSPI_IMSC_TXIM = (1 shl 1); {Transmit interrupt mask}
 BCM2838_I2CSPI_IMSC_RXIM = (1 shl 0); {Receive interrupt mask}

 {I2C / SPI Slave RIS (Raw Interrupt Status) register bits (See 11.2)}
 BCM2838_I2CSPI_RIS_OERIS = (1 shl 3); {Overrun error interrupt status}
 BCM2838_I2CSPI_RIS_BERIS = (1 shl 2); {Break error interrupt status}
 BCM2838_I2CSPI_RIS_TXRIS = (1 shl 1); {Transmit interrupt status}
 BCM2838_I2CSPI_RIS_RXRIS = (1 shl 0); {Receive interrupt status}

 {I2C / SPI Slave MIS (Masked Interrupt Status) register bits (See 11.2)}
 BCM2838_I2CSPI_MIS_OEMIS = (1 shl 3); {Overrun error masked interrupt status}
 BCM2838_I2CSPI_MIS_BEMIS = (1 shl 2); {Break error masked interrupt status}
 BCM2838_I2CSPI_MIS_TXMIS = (1 shl 1); {Transmit masked interrupt status}
 BCM2838_I2CSPI_MIS_RXMIS = (1 shl 0); {Receive masked interrupt status}

 {I2C / SPI Slave ICR (Interrupt Clear Register) register bits (See 11.2)}
 BCM2838_I2CSPI_ICR_OEIC = (1 shl 3); {Overrun error interrupt clear}
 BCM2838_I2CSPI_ICR_BEIC = (1 shl 2); {Break error interrupt clear}
 BCM2838_I2CSPI_ICR_TXIC = (1 shl 1); {Transmit interrupt clear}
 BCM2838_I2CSPI_ICR_RXIC = (1 shl 0); {Receive interrupt clear}

//const
 {AUX (UART1, SPI1 and SPI2) register bits (See 2.1)}
 //To Do

//const
 {PCM / I2S register bits (See 8.8)}
 //To Do

const
 {Pulse Width Modulator (PWM) Control register bits (See 9.6)}
 BCM2838_PWM_CTL_MSEN2 = (1 shl 15); {Channel 2 M/S Enable (0: PWM algorithm is used / 1: M/S transmission is used)}
 {Bit 14 Reserved - Write as 0, read as don't care}
 BCM2838_PWM_CTL_USEF2 = (1 shl 13); {Channel 2 Use Fifo (0: Data register is transmitted / 1: Fifo is used for transmission)}
 BCM2838_PWM_CTL_POLA2 = (1 shl 12); {Channel 2 Polarity (0 : 0=low 1=high / 1: 1=low 0=high)}
 BCM2838_PWM_CTL_SBIT2 = (1 shl 11); {Channel 2 Silence Bit (Defines the state of the output when no transmission takes place)}
 BCM2838_PWM_CTL_RPTL2 = (1 shl 10); {Channel 2 Repeat Last Data (0: Transmission interrupts when FIFO is empty / 1: Last data in FIFO is transmitted repeatedly until FIFO is not empty)}
 BCM2838_PWM_CTL_MODE2 = (1 shl 9);  {Channel 2 Mode (0: PWM mode / 1: Serialiser mode)}
 BCM2838_PWM_CTL_PWEN2 = (1 shl 8);  {Channel 2 Enable (0: Channel is disabled / 1: Channel is enabled)}
 BCM2838_PWM_CTL_MSEN1 = (1 shl 7);  {Channel 1 M/S Enable (0: PWM algorithm is used / 1: M/S transmission is used)}
 BCM2838_PWM_CTL_CLRF1 = (1 shl 6);  {Clear Fifo (1: Clears FIFO / 0: Has no effect) (This is a single shot operation. This bit always reads 0)}
 BCM2838_PWM_CTL_USEF1 = (1 shl 5);  {Channel 1 Use Fifo (0: Data register is transmitted / 1: Fifo is used for transmission)}
 BCM2838_PWM_CTL_POLA1 = (1 shl 4);  {Channel 1 Polarity (0 : 0=low 1=high / 1: 1=low 0=high)}
 BCM2838_PWM_CTL_SBIT1 = (1 shl 3);  {Channel 1 Silence Bit (Defines the state of the output when no transmission takes place)}
 BCM2838_PWM_CTL_RPTL1 = (1 shl 2);  {Channel 1 Repeat Last Data (0: Transmission interrupts when FIFO is empty / 1: Last data in FIFO is transmitted repeatedly until FIFO is not empty)}
 BCM2838_PWM_CTL_MODE1 = (1 shl 1);  {Channel 1 Mode (0: PWM mode / 1: Serialiser mode)}
 BCM2838_PWM_CTL_PWEN1 = (1 shl 0);  {Channel 1 Enable (0: Channel is disabled / 1: Channel is enabled)}

 {Pulse Width Modulator (PWM) Status register bits (See 9.6)}
 BCM2838_PWM_STA_STA4  = (1 shl 12); {Channel 4 State}
 BCM2838_PWM_STA_STA3  = (1 shl 11); {Channel 3 State}
 BCM2838_PWM_STA_STA2  = (1 shl 10); {Channel 2 State}
 BCM2838_PWM_STA_STA1  = (1 shl 9);  {Channel 1 State}
 BCM2838_PWM_STA_BERR  = (1 shl 8);  {Bus Error Flag}
 BCM2838_PWM_STA_GAPO4 = (1 shl 7);  {Channel 4 Gap Occurred Flag}
 BCM2838_PWM_STA_GAPO3 = (1 shl 6);  {Channel 3 Gap Occurred Flag}
 BCM2838_PWM_STA_GAPO2 = (1 shl 5);  {Channel 2 Gap Occurred Flag}
 BCM2838_PWM_STA_GAPO1 = (1 shl 4);  {Channel 1 Gap Occurred Flag}
 BCM2838_PWM_STA_RERR1 = (1 shl 3);  {Fifo Read Error Flag}
 BCM2838_PWM_STA_WERR1 = (1 shl 2);  {Fifo Write Error Flag}
 BCM2838_PWM_STA_EMPT1 = (1 shl 1);  {Fifo Empty Flag}
 BCM2838_PWM_STA_FULL1 = (1 shl 0);  {Fifo Full Flag}

 {Pulse Width Modulator (PWM) DMA configuration register bits (See 9.6)}
 BCM2838_PWM_DMAC_ENAB  = (1 shl 31);  {DMA Enable (0: DMA disabled / 1: DMA enabled)}
 BCM2838_PWM_DMAC_PANIC = ($FF shl 8); {DMA Threshold for PANIC signal (Default: 0x7)}
 BCM2838_PWM_DMAC_DREQ  = ($FF shl 0); {DMA Threshold for DREQ signal (Default: 0x7)}

 {Pulse Width Modulator (PWM) Registers}
 BCM2838_PWM_CTL  = $00000000; {PWM Control}
 BCM2838_PWM_STA  = $00000004; {PWM Status}
 BCM2838_PWM_DMAC = $00000008; {PWM DMA Configuration}
 BCM2838_PWM_RNG1 = $00000010; {PWM Channel 1 Range}
 BCM2838_PWM_DAT1 = $00000014; {PWM Channel 1 Data}
 BCM2838_PWM_FIF1 = $00000018; {PWM FIFO Input}
 BCM2838_PWM_RNG2 = $00000020; {PWM Channel 2 Range}
 BCM2838_PWM_DAT2 = $00000024; {PWM Channel 2 Data}

const
 {PL011 UART Data register bits (See 13.4)}
 BCM2838_PL011_DR_OE    = (1 shl 11);   {Overrun error}
 BCM2838_PL011_DR_BE    = (1 shl 10);   {Break error}
 BCM2838_PL011_DR_PE    = (1 shl 9);    {Parity error}
 BCM2838_PL011_DR_FE    = (1 shl 8);    {Framing error}
 BCM2838_PL011_DR_DATA  = ($FF shl 0);  {Receive / Transmit data}
 BCM2838_PL011_DR_ERROR = BCM2838_PL011_DR_OE or BCM2838_PL011_DR_BE or BCM2838_PL011_DR_PE or BCM2838_PL011_DR_FE;

 {PL011 UART Receive Status / Error Clear register bits (See 13.4)}
 BCM2838_PL011_RSRECR_OE = (1 shl 3); {Overrun error}
 BCM2838_PL011_RSRECR_BE = (1 shl 2); {Break error}
 BCM2838_PL011_RSRECR_PE = (1 shl 1); {Parity error}
 BCM2838_PL011_RSRECR_FE = (1 shl 0); {Framing error}

 {PL011 UART Flag register bits (See 13.4)}
 BCM2838_PL011_FR_RI   = (1 shl 8); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_FR_TXFE = (1 shl 7); {Transmit FIFO empty}
 BCM2838_PL011_FR_RXFF = (1 shl 6); {Receive FIFO full}
 BCM2838_PL011_FR_TXFF = (1 shl 5); {Transmit FIFO full}
 BCM2838_PL011_FR_RXFE = (1 shl 4); {Receive FIFO empty}
 BCM2838_PL011_FR_BUSY = (1 shl 3); {UART busy}
 BCM2838_PL011_FR_DCD  = (1 shl 2); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_FR_DSR  = (1 shl 1); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_FR_CTS  = (1 shl 0); {Clear to send (This bit is the complement of the UART clear to send, nUARTCTS, modem status input. That is, the bit is 1 when nUARTCTS is LOW)}

 {PL011 UART IrDA register bits (See 13.4)}
  {This register is disabled, writing to it has no effect and reading returns 0}

 {PL011 UART Integer Baud Rate Divisor register bits (See 13.4)}
 BCM2838_PL011_IBRD_MASK = ($FFFF shl 0);

 {PL011 UART Fractional Baud Rate Divisor register bits (See 13.4)}
 BCM2838_PL011_FBRD_MASK = ($3F shl 0);

 {PL011 UART Line Control register bits (See 13.4)}
 BCM2838_PL011_LCRH_SPS   = (1 shl 7); {Stick parity select}
 BCM2838_PL011_LCRH_WLEN  = (3 shl 5); {Word length}
 BCM2838_PL011_LCRH_WLEN8 = (3 shl 5); { 8 bits}
 BCM2838_PL011_LCRH_WLEN7 = (2 shl 5); { 7 bits}
 BCM2838_PL011_LCRH_WLEN6 = (1 shl 5); { 6 bits}
 BCM2838_PL011_LCRH_WLEN5 = (0 shl 5); { 5 bits}
 BCM2838_PL011_LCRH_FEN   = (1 shl 4); {Enable FIFOs}
 BCM2838_PL011_LCRH_STP2  = (1 shl 3); {Two stop bits select}
 BCM2838_PL011_LCRH_EPS   = (1 shl 2); {Even parity select (0 = odd parity / 1 = even parity)}
 BCM2838_PL011_LCRH_PEN   = (1 shl 1); {Parity enable}
 BCM2838_PL011_LCRH_BRK   = (1 shl 0); {Send break}

 {PL011 UART Control register bits (See 13.4)}
 BCM2838_PL011_CR_CTSEN  = (1 shl 15); {CTS hardware flow control enable (If this bit is set to 1 data is only transmitted when the nUARTCTS signal is asserted)}
 BCM2838_PL011_CR_RTSEN  = (1 shl 14); {RTS hardware flow control enable (If this bit is set to 1 data is only requested when there is space in the receive FIFO for it to be received)}
 BCM2838_PL011_CR_OUT2   = (1 shl 13); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_CR_OUT1   = (1 shl 12); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_CR_RTS    = (1 shl 11); {Request to send (This bit is the complement of the UART request to send, nUARTRTS, modem status output. That is, when the bit is programmed to a 1 then nUARTRTS is LOW)}
 BCM2838_PL011_CR_DTR    = (1 shl 10); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_CR_RXE    = (1 shl 9);  {Receive enable}
 BCM2838_PL011_CR_TXE    = (1 shl 8);  {Transmit enable}
 BCM2838_PL011_CR_LBE    = (1 shl 7);  {Loopback enable}
 {Bits 6:3 Reserved - Write as 0, read as don't care}
 BCM2838_PL011_CR_SIRLP  = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_CR_SIREN  = (1 shl 1);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_CR_UARTEN = (1 shl 0);  {UART enable}

 {PL011 UART Interrupt FIFO Level Select register bits (See 13.4)}
 BCM2838_PL011_IFLS_RXIFPSEL    = (7 shl 9); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_IFLS_TXIFPSEL    = (7 shl 6); {Unsupported, write zero, read as don't care}
 BCM2838_PL011_IFLS_RXIFLSEL    = (7 shl 3); {Receive interrupt FIFO level select}
 BCM2838_PL011_IFLS_RXIFLSEL1_8 = (0 shl 3); { b000 = Receive FIFO becomes >= 1/8 full}
 BCM2838_PL011_IFLS_RXIFLSEL1_4 = (1 shl 3); { b001 = Receive FIFO becomes >= 1/4 full}
 BCM2838_PL011_IFLS_RXIFLSEL1_2 = (2 shl 3); { b010 = Receive FIFO becomes >= 1/2 full}
 BCM2838_PL011_IFLS_RXIFLSEL3_4 = (3 shl 3); { b011 = Receive FIFO becomes >= 3/4 full}
 BCM2838_PL011_IFLS_RXIFLSEL7_8 = (4 shl 3); { b100 = Receive FIFO becomes >= 7/8 full}
 BCM2838_PL011_IFLS_TXIFLSEL    = (7 shl 0); {Transmit interrupt FIFO level select}
 BCM2838_PL011_IFLS_TXIFLSEL1_8 = (0 shl 0); { b000 = Transmit FIFO becomes <= 1/8 full}
 BCM2838_PL011_IFLS_TXIFLSEL1_4 = (1 shl 0); { b001 = Transmit FIFO becomes <= 1/4 full}
 BCM2838_PL011_IFLS_TXIFLSEL1_2 = (2 shl 0); { b010 = Transmit FIFO becomes <= 1/2 full}
 BCM2838_PL011_IFLS_TXIFLSEL3_4 = (3 shl 0); { b011 = Transmit FIFO becomes <= 3/4 full}
 BCM2838_PL011_IFLS_TXIFLSEL7_8 = (4 shl 0); { b100 = Transmit FIFO becomes <= 7/8 full}

 {PL011 UART Interrupt Mask Set/Clear register bits (See 13.4)}
 BCM2838_PL011_IMSC_OEIM   = (1 shl 10); {Overrun error interrupt mask}
 BCM2838_PL011_IMSC_BEIM   = (1 shl 9);  {Break error interrupt mask}
 BCM2838_PL011_IMSC_PEIM   = (1 shl 8);  {Parity error interrupt mask}
 BCM2838_PL011_IMSC_FEIM   = (1 shl 7);  {Framing error interrupt mask}
 BCM2838_PL011_IMSC_RTIM   = (1 shl 6);  {Receive timeout interrupt mask}
 BCM2838_PL011_IMSC_TXIM   = (1 shl 5);  {Transmit interrupt mask}
 BCM2838_PL011_IMSC_RXIM   = (1 shl 4);  {Receive interrupt mask}
 BCM2838_PL011_IMSC_DSRMIM = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_IMSC_DCDMIM = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_IMSC_CTSMIM = (1 shl 1);  {nUARTCTS modem interrupt mask}
 BCM2838_PL011_IMSC_RIMIM  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART Raw Interrupt Status register bits (See 13.4)}
 BCM2838_PL011_RIS_OERIS   = (1 shl 10); {Overrun error interrupt status}
 BCM2838_PL011_RIS_BERIS   = (1 shl 9);  {Break error interrupt status}
 BCM2838_PL011_RIS_PERIS   = (1 shl 8);  {Parity error interrupt status}
 BCM2838_PL011_RIS_FERIS   = (1 shl 7);  {Framing error interrupt status}
 BCM2838_PL011_RIS_RTRIS   = (1 shl 6);  {Receive timeout interrupt status}
 BCM2838_PL011_RIS_TXRIS   = (1 shl 5);  {Transmit interrupt status}
 BCM2838_PL011_RIS_RXRIS   = (1 shl 4);  {Receive interrupt status}
 BCM2838_PL011_RIS_DSRMRIS = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_RIS_DCDMRIS = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_RIS_CTSMRIS = (1 shl 1);  {nUARTCTS modem interrupt status}
 BCM2838_PL011_RIS_RIMRIS  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART Masked Interrupt Status register bits (See 13.4)}
 BCM2838_PL011_MIS_OEMIS   = (1 shl 10); {Overrun error masked interrupt status}
 BCM2838_PL011_MIS_BEMIS   = (1 shl 9);  {Break error masked interrupt status}
 BCM2838_PL011_MIS_PEMIS   = (1 shl 8);  {Parity error masked interrupt status}
 BCM2838_PL011_MIS_FEMIS   = (1 shl 7);  {Framing error masked interrupt status}
 BCM2838_PL011_MIS_RTMIS   = (1 shl 6);  {Receive timeout masked interrupt status}
 BCM2838_PL011_MIS_TXMIS   = (1 shl 5);  {Transmit masked interrupt status}
 BCM2838_PL011_MIS_RXMIS   = (1 shl 4);  {Receive masked interrupt status}
 BCM2838_PL011_MIS_DSRMMIS = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_MIS_DCDMMIS = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_MIS_CTSMMIS = (1 shl 1);  {nUARTCTS modem masked interrupt status}
 BCM2838_PL011_MIS_RIMMIS  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART Interrupt Clear register bits (See 13.4)}
 BCM2838_PL011_ICR_OEIC   = (1 shl 10); {Overrun error interrupt clear}
 BCM2838_PL011_ICR_BEIC   = (1 shl 9);  {Break error interrupt clear}
 BCM2838_PL011_ICR_PEIC   = (1 shl 8);  {Parity error interrupt clear}
 BCM2838_PL011_ICR_FEIC   = (1 shl 7);  {Framing error interrupt clear}
 BCM2838_PL011_ICR_RTIC   = (1 shl 6);  {Receive timeout interrupt clear}
 BCM2838_PL011_ICR_TXIC   = (1 shl 5);  {Transmit interrupt clear}
 BCM2838_PL011_ICR_RXIC   = (1 shl 4);  {Receive interrupt clear}
 BCM2838_PL011_ICR_DSRMIC = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_ICR_DCDMIC = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2838_PL011_ICR_CTSMIC = (1 shl 1);  {nUARTCTS modem interrupt clear}
 BCM2838_PL011_ICR_RIMIC  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART DMA Control register bits (See 13.4)}
  {This register is disabled, writing to it has no effect and reading returns 0}

 {PL011 UART Test Control register bits (See 13.4)}

 {PL011 UART Integration Test Input register bits (See 13.4)}

 {PL011 UART Integration Test Output register bits (See 13.4)}

 {PL011 UART Test Data register bits (See 13.4)}

const
 {ARM Legacy Interrupt Controller registers}
 BCM2838_ARM_INTERRUPT_IRQ0_PENDING0 = $00000000; {ARM Core 0 IRQ Enabled Interrupt Pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ0_PENDING1 = $00000004; {ARM Core 0 IRQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ0_PENDING2 = $00000008; {ARM Core 0 IRQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_IRQ0_SET_EN_0 = $00000010; {Write to Set ARM Core 0 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ0_SET_EN_1 = $00000014; {Write to Set ARM Core 0 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ0_SET_EN_2 = $00000018; {Write to Set ARM Core 0 IRQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_IRQ0_CLR_EN_0 = $00000020; {Write to Clear ARM Core 0 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ0_CLR_EN_1 = $00000024; {Write to Clear ARM Core 0 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ0_CLR_EN_2 = $00000028; {Write to Clear ARM Core 0 IRQ enable bits [79:64]}
 BCM2838_ARM_INTERRUPT_IRQ_STATUS0   = $00000030; {Interrupt Line bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ_STATUS1   = $00000034; {Interrupt Line bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ_STATUS2   = $00000038; {Interrupt Line bits [79:64]}

 BCM2838_ARM_INTERRUPT_IRQ1_PENDING0 = $00000040; {ARM Core 1 IRQ Enabled Interrupt pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ1_PENDING1 = $00000044; {ARM Core 1 IRQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ1_PENDING2 = $00000048; {ARM Core 1 IRQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_IRQ1_SET_EN_0 = $00000050; {Write to Set ARM Core 1 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ1_SET_EN_1 = $00000054; {Write to Set ARM Core 1 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ1_SET_EN_2 = $00000058; {Write to Set ARM Core 1 IRQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_IRQ1_CLR_EN_0 = $00000060; {Write to Clear ARM Core 1 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ1_CLR_EN_1 = $00000064; {Write to Clear ARM Core 1 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ1_CLR_EN_2 = $00000068; {Write to Clear ARM Core 1 IRQ enable bits [79:64]}

 BCM2838_ARM_INTERRUPT_IRQ2_PENDING0 = $00000080; {ARM Core 2 IRQ Enabled Interrupt pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ2_PENDING1 = $00000084; {ARM Core 2 IRQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ2_PENDING2 = $00000088; {ARM Core 2 IRQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_IRQ2_SET_EN_0 = $00000090; {Write to Set ARM Core 2 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ2_SET_EN_1 = $00000094; {Write to Set ARM Core 2 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ2_SET_EN_2 = $00000098; {Write to Set ARM Core 2 IRQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_IRQ2_CLR_EN_0 = $000000A0; {Write to Clear ARM Core 2 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ2_CLR_EN_1 = $000000A4; {Write to Clear ARM Core 2 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ2_CLR_EN_2 = $000000A8; {Write to Clear ARM Core 2 IRQ enable bits [79:64]}

 BCM2838_ARM_INTERRUPT_IRQ3_PENDING0 = $000000C0; {ARM Core 3 IRQ Enabled Interrupt pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ3_PENDING1 = $000000C4; {ARM Core 3 IRQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ3_PENDING2 = $000000C8; {ARM Core 3 IRQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_IRQ3_SET_EN_0 = $000000D0; {Write to Set ARM Core 3 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ3_SET_EN_1 = $000000D4; {Write to Set ARM Core 3 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ3_SET_EN_2 = $000000D8; {Write to Set ARM Core 3 IRQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_IRQ3_CLR_EN_0 = $000000E0; {Write to Clear ARM Core 3 IRQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_IRQ3_CLR_EN_1 = $000000E4; {Write to Clear ARM Core 3 IRQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_IRQ3_CLR_EN_2 = $000000E8; {Write to Clear ARM Core 3 IRQ enable bits [79:64]}

 BCM2838_ARM_INTERRUPT_FIQ0_PENDING0 = $00000100; {ARM Core 0 FIQ Enabled Interrupt pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ0_PENDING1 = $00000104; {ARM Core 0 FIQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ0_PENDING2 = $00000108; {ARM Core 0 FIQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_FIQ0_SET_EN_0 = $00000110; {Write to Set ARM Core 0 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ0_SET_EN_1 = $00000114; {Write to Set ARM Core 0 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ0_SET_EN_2 = $00000118; {Write to Set ARM Core 0 FIQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_FIQ0_CLR_EN_0 = $00000120; {Write to Clear ARM Core 0 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ0_CLR_EN_1 = $00000124; {Write to Clear ARM Core 0 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ0_CLR_EN_2 = $00000128; {Write to Clear ARM Core 0 FIQ enable bits [79:64]}

 BCM2838_ARM_INTERRUPT_FIQ1_PENDING0 = $00000140; {ARM Core 1 FIQ Enabled Interrupt pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ1_PENDING1 = $00000144; {ARM Core 1 FIQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ1_PENDING2 = $00000148; {ARM Core 1 FIQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_FIQ1_SET_EN_0 = $00000150; {Write to Set ARM Core 1 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ1_SET_EN_1 = $00000154; {Write to Set ARM Core 1 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ1_SET_EN_2 = $00000158; {Write to Set ARM Core 1 FIQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_FIQ1_CLR_EN_0 = $00000160; {Write to Clear ARM Core 1 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ1_CLR_EN_1 = $00000164; {Write to Clear ARM Core 1 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ1_CLR_EN_2 = $00000168; {Write to Clear ARM Core 1 FIQ enable bits [79:64]}

 BCM2838_ARM_INTERRUPT_FIQ2_PENDING0 = $00000180; {ARM Core 2 FIQ Enabled Interrupt pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ2_PENDING1 = $00000184; {ARM Core 2 FIQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ2_PENDING2 = $00000188; {ARM Core 2 FIQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_FIQ2_SET_EN_0 = $00000190; {Write to Set ARM Core 2 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ2_SET_EN_1 = $00000194; {Write to Set ARM Core 2 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ2_SET_EN_2 = $00000198; {Write to Set ARM Core 2 FIQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_FIQ2_CLR_EN_0 = $000001A0; {Write to Clear ARM Core 2 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ2_CLR_EN_1 = $000001A4; {Write to Clear ARM Core 2 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ2_CLR_EN_2 = $000001A8; {Write to Clear ARM Core 2 FIQ enable bits [79:64]}

 BCM2838_ARM_INTERRUPT_FIQ3_PENDING0 = $000001C0; {ARM Core 3 FIQ Enabled Interrupt pending bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ3_PENDING1 = $000001C4; {ARM Core 3 FIQ Enabled Interrupt pending bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ3_PENDING2 = $000001C8; {ARM Core 3 FIQ Enabled Interrupt pending bits [79:64]}
 BCM2838_ARM_INTERRUPT_FIQ3_SET_EN_0 = $000001D0; {Write to Set ARM Core 3 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ3_SET_EN_1 = $000001D4; {Write to Set ARM Core 3 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ3_SET_EN_2 = $000001D8; {Write to Set ARM Core 3 FIQ enable bits[79:64]}
 BCM2838_ARM_INTERRUPT_FIQ3_CLR_EN_0 = $000001E0; {Write to Clear ARM Core 3 FIQ enable bits [31:0]}
 BCM2838_ARM_INTERRUPT_FIQ3_CLR_EN_1 = $000001E4; {Write to Clear ARM Core 3 FIQ enable bits [63:32]}
 BCM2838_ARM_INTERRUPT_FIQ3_CLR_EN_2 = $000001E8; {Write to Clear ARM Core 3 FIQ enable bits [79:64]}

 BCM2838_ARM_INTERRUPT_SWIRQ_SET     = $000001F0; {Write to Set Software Interrupt sources}
 BCM2838_ARM_INTERRUPT_SWIRQ_CLEAR   = $000001F4; {Write to Clear Software Interrupt sources}

 BCM2838_ARM_INTERRUPT_CORE_OFFSET = BCM2838_ARM_INTERRUPT_IRQ1_PENDING0 - BCM2838_ARM_INTERRUPT_IRQ0_PENDING0; {Offset between Core registers}
 BCM2838_ARM_INTERRUPT_FIQ_OFFSET = BCM2838_ARM_INTERRUPT_FIQ0_PENDING0 - BCM2838_ARM_INTERRUPT_IRQ0_PENDING0; {Offset betwwen IRQ and FIQ registers}

const
 {ARM Timer register bits (See 14.2)}
 BCM2838_ARM_TIMER_CONTROL_COUNTER_PRESCALE = ($FF shl 16); {Free running counter pre-scaler (Freq is sys_clk/(prescale+1))}
 BCM2838_ARM_TIMER_CONTROL_COUNTER_ENABLED  = (1 shl 9);    {0 : Free running counter Disabled / 1 : Free running counter Enabled}
 BCM2838_ARM_TIMER_CONTROL_DEBUG_HALT       = (1 shl 8);    {0 : Timers keeps running if ARM is in debug halted mode / 1 : Timers halted if ARM is in debug halted mode}
 BCM2838_ARM_TIMER_CONTROL_TIMER_ENABLED    = (1 shl 7);    {0 : Timer disabled / 1 : Timer enabled}
 BCM2838_ARM_TIMER_CONTROL_INT_ENABLED      = (1 shl 5);    {0 : Timer interrupt disabled / 1 : Timer interrupt enabled}
 BCM2838_ARM_TIMER_CONTROL_PRESCALE         = (3 shl 2);    {Pre-scale bits: 00 : pre-scale is clock / 1 (No pre-scale) / 01 : pre-scale is clock / 16 / 10 : pre-scale is clock / 256 / 11 : pre-scale is clock / 1}
 BCM2838_ARM_TIMER_CONTROL_32BIT            = (1 shl 1);    {0 : 16-bit counters / 1 : 32-bit counter}
 BCM2838_ARM_TIMER_CONTROL_ONESHOT          = (1 shl 0);    {0 = wrapping mode (default) / 1 = one-shot mode (Not supported by BCM2838)}

 BCM2838_ARM_TIMER_RAW_IRQ_PENDING = (1 shl 0); {0 : The interrupt pending bits is clear / 1 : The interrupt pending bit is set}

 BCM2838_ARM_TIMER_MASKED_IRQ_PENDING = (1 shl 0); {0 : Interrupt line not asserted / 1 :Interrupt line is asserted, (the interrupt pending and the interrupt enable bit are set)}

 BCM2838_ARM_TIMER_PREDIVIDER_MASK = ($3FF shl 0); {Pre-divider value (timer_clock = apb_clock/(pre_divider+1))}

const
 {Power Management, Reset controller and Watchdog}
 BCM2838_PM_PASSWORD               = $5A000000;

 BCM2838_PM_RSTC_WRCFG_CLR         = $FFFFFFCF;
 BCM2838_PM_RSTC_WRCFG_SET           = $00000030;
 BCM2838_PM_RSTC_WRCFG_FULL_RESET  = $00000020;
 BCM2838_PM_RSTC_RESET               = $00000102;

 BCM2838_PM_RSTS_HADPOR_SET        = $00001000;
 BCM2838_PM_RSTS_HADSRH_SET        = $00000400;
 BCM2838_PM_RSTS_HADSRF_SET        = $00000200;
 BCM2838_PM_RSTS_HADSRQ_SET        = $00000100;
 BCM2838_PM_RSTS_HADWRH_SET        = $00000040;
 BCM2838_PM_RSTS_HADWRF_SET        = $00000020;
 BCM2838_PM_RSTS_HADWRQ_SET        = $00000010;
 BCM2838_PM_RSTS_HADDRH_SET        = $00000004;
 BCM2838_PM_RSTS_HADDRF_SET        = $00000002;
 BCM2838_PM_RSTS_HADDRQ_SET        = $00000001;

 BCM2838_PM_RSTS_RASPBERRYPI_HALT  = $00000555; {Special value to tell the Raspberry Pi firmware not to reboot, sets Boot Partition to 63 (0x3f)}

 BCM2838_PM_WDOG_RESET                = $00000000;
 BCM2838_PM_WDOG_TIME_MASK           = $000FFFFF;

 BCM2838_PM_WDOG_TICKS_PER_SECOND      = (1 shl 16);
 BCM2838_PM_WDOG_TICKS_PER_MILLISECOND = (BCM2838_PM_WDOG_TICKS_PER_SECOND div 1000);

const
 {Random Number Generator}
 BCM2838_RNG_CTRL_OFFSET             = $00;
 BCM2838_RNG_CTRL_RNG_RBGEN_MASK     = $00001FFF;
 BCM2838_RNG_CTRL_RNG_RBGEN_ENABLE   = $00000001;
 BCM2838_RNG_CTRL_RNG_RBGEN_DISABLE  = $00000000;
 BCM2838_RNG_CTRL_RNG_DIV_CTRL_SHIFT = 13;

 BCM2838_RNG_SOFT_RESET_OFFSET       = $04;
 BCM2838_RNG_SOFT_RESET              = $00000001;

 BCM2838_RBG_SOFT_RESET_OFFSET       = $08;
 BCM2838_RBG_SOFT_RESET              = $00000001;

 BCM2838_RNG_TOTAL_BIT_COUNT_OFFSET  = $0C;

 BCM2838_RNG_TOTAL_BIT_COUNT_THRESHOLD_OFFSET = $10;

 BCM2838_RNG_INT_STATUS_OFFSET                           = $18;
 BCM2838_RNG_INT_STATUS_MASTER_FAIL_LOCKOUT_IRQ_MASK     = $80000000;
 BCM2838_RNG_INT_STATUS_STARTUP_TRANSITIONS_MET_IRQ_MASK = $00020000;
 BCM2838_RNG_INT_STATUS_NIST_FAIL_IRQ_MASK               = $00000020;
 BCM2838_RNG_INT_STATUS_TOTAL_BITS_COUNT_IRQ_MASK        = $00000001;

 BCM2838_RNG_INT_ENABLE_OFFSET       = $1C;

 BCM2838_RNG_FIFO_DATA_OFFSET        = $20;

 BCM2838_RNG_FIFO_COUNT_OFFSET                   = $24;
 BCM2838_RNG_FIFO_COUNT_RNG_FIFO_COUNT_MASK      = $000000FF;
 BCM2838_RNG_FIFO_COUNT_RNG_FIFO_THRESHOLD_SHIFT = 8;

const
 {Clock Management (See Section 6)}
 BCM2838_CM_PASSWORD               = $5A000000;

 {Clock Manager CM_*CTL register bits (See 6.3)}
 BCM2838_CM_CTL_MASH_0         = (0 shl 9); {MASH control - 0 = integer division}
 BCM2838_CM_CTL_MASH_1         = (1 shl 9); {MASH control - 1 = 1-stage MASH (equivalent to non-MASH dividers)}
 BCM2838_CM_CTL_MASH_2         = (2 shl 9); {MASH control - 2 = 2-stage MASH}
 BCM2838_CM_CTL_MASH_3         = (3 shl 9); {MASH control - 3 = 3-stage MASH (To avoid lock-ups and glitches do not change this control while BUSY=1 and do not change this control at the same time as asserting ENAB)}
 BCM2838_CM_CTL_FLIP           = (1 shl 8); {MASH control - Invert the clock generator output (To avoid output glitches do not switch this control while BUSY=1)}
 BCM2838_CM_CTL_BUSY           = (1 shl 7); {Clock generator is running (To avoid glitches and lock-ups, clock sources and setups must not be changed while this flag is set)}
 BCM2838_CM_CTL_GATE           = (1 shl 6); {Unused}
 BCM2838_CM_CTL_KILL           = (1 shl 5); {Kill the clock generator (0 = no action / 1 = stop and reset the clock generator) (This is intended for test/debug only)}
 BCM2838_CM_CTL_ENAB           = (1 shl 4); {Enable the clock generator}

 BCM2838_CM_CTL_SRC_GND        = (0 shl 0); {Clock source - 0 Hz GND}
 BCM2838_CM_CTL_SRC_OSC        = (1 shl 0); {Clock source - 54 MHz Oscillator}
 BCM2838_CM_CTL_SRC_TESTDEBUG0 = (2 shl 0); {Clock source - 0 Hz Testdebug0}
 BCM2838_CM_CTL_SRC_TESTDEBUG1 = (3 shl 0); {Clock source - 0 Hz Testdebug1}
 BCM2838_CM_CTL_SRC_PLLA       = (4 shl 0); {Clock source - 0 Hz PLLA}
 BCM2838_CM_CTL_SRC_PLLC       = (5 shl 0); {Clock source - 1000 MHz PLLC (changes with overclock settings)}
 BCM2838_CM_CTL_SRC_PLLD       = (6 shl 0); {Clock source - 750 MHz PLLD}
 BCM2838_CM_CTL_SRC_HDMI       = (7 shl 0); {Clock source - Unused}

 {Clock Manager CM_*DIV register bits (See 6.3)}
 BCM2838_CM_DIV_INT_MASK  = $00FFF000; {Integer part of divisor (This value has a minimum limit determined by the MASH setting) (To avoid lock-ups and glitches do not change this control while BUSY=1)}
 BCM2838_CM_DIV_FRAC_MASK = $00000FFF; {Fractional part of divisor (To avoid lock-ups and glitches do not change this control while BUSY=1)}

 {Clock Manager Registers}
 BCM2838_CM_GNRICCTL =  $00000000; {Generic Clock Control}
 BCM2838_CM_GNRICDIV =  $00000004; {Generic Clock Divisor}
 BCM2838_CM_VPUCTL   =  $00000008; {VPU Clock Control}
 BCM2838_CM_VPUDIV   =  $0000000C; {VPU Clock Divisor}
 BCM2838_CM_SYSCTL   =  $00000010; {System Clock Control}
 BCM2838_CM_SYSDIV   =  $00000014; {System Clock Divisor}
 BCM2838_CM_PERIACTL =  $00000018; {PERIA Clock Control}
 BCM2838_CM_PERIADIV =  $0000001C; {PERIA Clock Divisor}
 BCM2838_CM_PERIICTL =  $00000020; {PERII Clock Control}
 BCM2838_CM_PERIIDIV =  $00000024; {PERII Clock Divisor}
 BCM2838_CM_H264CTL  =  $00000028; {H264 Clock Control}
 BCM2838_CM_H264DIV  =  $0000002C; {H264 Clock Divisor}
 BCM2838_CM_ISPCTL   =  $00000030; {ISP Clock Control}
 BCM2838_CM_ISPDIV   =  $00000034; {ISP Clock Divisor}
 BCM2838_CM_V3DCTL   =  $00000038; {V3D Clock Control}
 BCM2838_CM_V3DDIV   =  $0000003C; {V3D Clock Divisor}
 BCM2838_CM_CAM0CTL  =  $00000040; {Camera 0 Clock Control}
 BCM2838_CM_CAM0DIV  =  $00000044; {Camera 0 Clock Divisor}
 BCM2838_CM_CAM1CTL  =  $00000048; {Camera 1 Clock Control}
 BCM2838_CM_CAM1DIV  =  $0000004C; {Camera 1 Clock Divisor}
 BCM2838_CM_CCP2CTL  =  $00000050; {CCP2 Clock Control}
 BCM2838_CM_CCP2DIV  =  $00000054; {CCP2 Clock Divisor}
 BCM2838_CM_DSI0ECTL =  $00000058; {DSI0E Clock Control}
 BCM2838_CM_DSI0EDIV =  $0000005C; {DSI0E Clock Divisor}
 BCM2838_CM_DSI0PCTL =  $00000060; {DSI0P Clock Control}
 BCM2838_CM_DSI0PDIV =  $00000064; {DSI0P Clock Divisor}
 BCM2838_CM_DPICTL   =  $00000068; {DPI Clock Control}
 BCM2838_CM_DPIDIV   =  $0000006C; {DPI Clock Divisor}
 BCM2838_CM_GP0CTL   =  $00000070; {General Purpose 0 Clock Control}
 BCM2838_CM_GP0DIV   =  $00000074; {General Purpose 0 Clock Divisor}
 BCM2838_CM_GP1CTL   =  $00000078; {General Purpose 1 Clock Control}
 BCM2838_CM_GP1DIV   =  $0000007C; {General Purpose 1 Clock Divisor}
 BCM2838_CM_GP2CTL   =  $00000080; {General Purpose 2 Clock Control}
 BCM2838_CM_GP2DIV   =  $00000084; {General Purpose 2 Clock Divisor}
 BCM2838_CM_HSMCTL   =  $00000088; {HSM Clock Control}
 BCM2838_CM_HSMDIV   =  $0000008C; {HSM Clock Divisor}
 BCM2838_CM_OTPCTL   =  $00000090; {OTP Clock Control}
 BCM2838_CM_OTPDIV   =  $00000094; {OTP Clock Divisor}
 BCM2838_CM_PCMCTL   =  $00000098; {PCM / I2S Clock Control}
 BCM2838_CM_PCMDIV   =  $0000009C; {PCM / I2S Clock Divisor}
 BCM2838_CM_PWMCTL   =  $000000A0; {PWM Clock Control}
 BCM2838_CM_PWMDIV   =  $000000A4; {PWM Clock Divisor}
 BCM2838_CM_SLIMCTL  =  $000000A8; {SLIM Clock Control}
 BCM2838_CM_SLIMDIV  =  $000000AC; {SLIM Clock Divisor}
 BCM2838_CM_SMICTL   =  $000000B0; {SMI Clock Control}
 BCM2838_CM_SMIDIV   =  $000000B4; {SMI Clock Divisor}
 BCM2838_CM_TCNTCTL  =  $000000C0; {TCNT Clock Control}
 BCM2838_CM_TCNTDIV  =  $000000C4; {TCNT Clock Divisor}
 BCM2838_CM_TECCTL   =  $000000C8; {TEC Clock Control}
 BCM2838_CM_TECDIV   =  $000000CC; {TEC Clock Divisor}
 BCM2838_CM_TD0CTL   =  $000000D0; {TD0 Clock Control}
 BCM2838_CM_TD0DIV   =  $000000D4; {TD0 Clock Divisor}
 BCM2838_CM_TD1CTL   =  $000000D8; {TD1 Clock Control}
 BCM2838_CM_TD1DIV   =  $000000DC; {TD1 Clock Divisor}
 BCM2838_CM_TSENSCTL =  $000000E0; {TSENS Clock Control}
 BCM2838_CM_TSENSDIV =  $000000E4; {TSENS Clock Divisor}
 BCM2838_CM_TIMERCTL =  $000000E8; {Timer Clock Control}
 BCM2838_CM_TIMERDIV =  $000000EC; {Timer Clock Divisor}
 BCM2838_CM_UARTCTL  =  $000000F0; {UART Clock Control}
 BCM2838_CM_UARTDIV  =  $000000F4; {UART Clock Divisor}
 BCM2838_CM_VECCTL   =  $000000F8; {VEC Clock Control}
 BCM2838_CM_VECDIV   =  $000000FC; {VEC Clock Divisor}

 BCM2838_CM_OSCCOUNT =  $00000100; {Oscillator Count}
 BCM2838_CM_PLLA     =  $00000104; {PLLA}
 BCM2838_CM_PLLC     =  $00000108; {PLLC}
 BCM2838_CM_PLLD     =  $0000010C; {PLLD}
 BCM2838_CM_PLLH     =  $00000110; {PLLH}
 BCM2838_CM_LOCK     =  $00000114; {Lock}
 BCM2838_CM_EVENT    =  $00000118; {Event}
 BCM2838_CM_INTEN    =  $00000118; {INTEN}
 BCM2838_CM_DSI0HSCK =  $00000120; {DSI0HSCK}
 BCM2838_CM_CKSM     =  $00000124; {CKSM}
 BCM2838_CM_OSCFREQI =  $00000128; {Oscillator Frequency Integer}
 BCM2838_CM_OSCFREQF =  $0000012C; {Oscillator Frequency Fraction}
 BCM2838_CM_PLLTCTL  =  $00000130; {PLLT Control}
 BCM2838_CM_PLLTCNT0 =  $00000134; {PLLT0 Count}
 BCM2838_CM_PLLTCNT1 =  $00000138; {PLLT1 Count}
 BCM2838_CM_PLLTCNT2 =  $0000013C; {PLLT2 Count}
 BCM2838_CM_PLLTCNT3 =  $00000140; {PLLT3 Count}
 BCM2838_CM_TDCLKEN  =  $00000144; {TD Clock Enable}
 BCM2838_CM_BURSTCTL =  $00000148; {Burst Control}
 BCM2838_CM_BURSTCNT =  $0000014C; {Burst Count}
 BCM2838_CM_DSI1ECTL =  $00000158; {DSI1E Clock Control}
 BCM2838_CM_DSI1EDIV =  $0000015C; {DSI1E Clock Divisor}
 BCM2838_CM_DSI1PCTL =  $00000160; {DSI1P Clock Control}
 BCM2838_CM_DSI1PDIV =  $00000164; {DSI1P Clock Divisor}
 BCM2838_CM_DFTCTL   =  $00000168; {DFT Clock Control}
 BCM2838_CM_DFTDIV   =  $0000016C; {DFT Clock Divisor}
 BCM2838_CM_PLLB     =  $00000170; {PLLB}

 BCM2838_CM_PULSECTL =  $00000190; {Pulse Clock Control}
 BCM2838_CM_PULSEDIV =  $00000194; {Pulse Clock Divisor}
 BCM2838_CM_SDCCTL   =  $000001A8; {SDC Clock Control}
 BCM2838_CM_SDCDIV   =  $000001AC; {SDC Clock Divisor}
 BCM2838_CM_ARMCTL   =  $000001B0; {ARM Clock Control}
 BCM2838_CM_ARMDIV   =  $000001B4; {ARM Clock Divisor}
 BCM2838_CM_AVEOCTL  =  $000001B8; {AVEO Clock Control}
 BCM2838_CM_AVEODIV  =  $000001BC; {AVEO Clock Divisor}
 BCM2838_CM_EMMCCTL  =  $000001C0; {EMMC Clock Control}
 BCM2838_CM_EMMCDIV  =  $000001C4; {EMMC Clock Divisor}

const
 {BCM2838 Mailboxes}
 BCM2838_MAILBOX_0  = 0;
 BCM2838_MAILBOX_1  = 1;

 {BCM2838 Mailbox 0 channels (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
 BCM2838_MAILBOX0_CHANNEL_POWER_MGMT         = 0;
 BCM2838_MAILBOX0_CHANNEL_FRAMEBUFFER        = 1;
 BCM2838_MAILBOX0_CHANNEL_UART               = 2;
 BCM2838_MAILBOX0_CHANNEL_VCHIQ              = 3;
 BCM2838_MAILBOX0_CHANNEL_LEDS               = 4;
 BCM2838_MAILBOX0_CHANNEL_BUTTONS            = 5;
 BCM2838_MAILBOX0_CHANNEL_TOUCHSCREEN        = 6;
 BCM2838_MAILBOX0_CHANNEL_UNKNOWN            = 7;
 BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = 8;
 BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_VCARM = 9;

 {BCM2838 Mailbox 1 channels (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
  {Currently unknown}

 {The BCM2838 mailboxes pass 28-bit messages (The low 4 bits of the 32-bit value are used to specify the channel)}
 BCM2838_MAILBOX_CHANNEL_MASK = $0000000F;
 BCM2838_MAILBOX_DATA_MASK    = $FFFFFFF0;

 {BCM2838 mailbox status flags}
 BCM2838_MAILBOX_STATUS_FULL  = $80000000;
 BCM2838_MAILBOX_STATUS_EMPTY = $40000000;

 {BCM2838 mailbox configuration flags}
 BCM2838_MAILBOX_CONFIG_IRQENABLE = $00000001;

 {BCM2838 mailbox property tags (See https://github.com/raspberrypi/firmware/wiki/Mailbox-property-interface)(or \include\soc\bcm2835\raspberrypi-firmware.h)}
 {VideoCore}
 BCM2838_MBOX_TAG_GET_FIRMWARE_REV  = $00000001;
 {Hardware}
 BCM2838_MBOX_TAG_GET_BOARD_MODEL   = $00010001;
 BCM2838_MBOX_TAG_GET_BOARD_REV        = $00010002;

 BCM2838_MBOX_TAG_GET_MAC_ADDRESS    = $00010003;

 BCM2838_MBOX_TAG_GET_BOARD_SERIAL  = $00010004;

 BCM2838_MBOX_TAG_GET_ARM_MEMORY    = $00010005;
 BCM2838_MBOX_TAG_GET_VC_MEMORY        = $00010006;

 BCM2838_MBOX_TAG_GET_CLOCKS        = $00010007;
 {Shared Resource Management}
 BCM2838_MBOX_TAG_GET_POWER_STATE    = $00020001; {Response indicates current state}
 BCM2838_MBOX_TAG_GET_TIMING        = $00020002; {Response indicates wait time required after turning a device on before power is stable}
 BCM2838_MBOX_TAG_SET_POWER_STATE    = $00028001; {Response indicates new state, with/without waiting for the power to become stable}

 BCM2838_MBOX_TAG_GET_CLOCK_STATE   = $00030001;
 BCM2838_MBOX_TAG_SET_CLOCK_STATE   = $00038001;

 BCM2838_MBOX_TAG_GET_CLOCK_RATE    = $00030002;
 BCM2838_MBOX_TAG_SET_CLOCK_RATE    = $00038002;

 BCM2838_MBOX_TAG_GET_CLOCK_MAX_RATE = $00030004; {Return the maximum supported clock rate for the given clock. Clocks should not be set higher than this}
 BCM2838_MBOX_TAG_GET_CLOCK_MIN_RATE = $00030007; {Return the minimum supported clock rate for the given clock. This may be used when idle}

 BCM2838_MBOX_TAG_GET_TURBO         = $00030009; {Get the turbo state for index id. id should be 0. level will be zero for non-turbo and one for turbo}
 BCM2838_MBOX_TAG_SET_TURBO         = $00038009; {Set the turbo state for index id. id should be zero. level will be zero for non-turbo and one for turbo. This will cause GPU clocks to be set to maximum when enabled and minimum when disabled}

 BCM2838_MBOX_TAG_GET_STC           = $0003000b; {}
 {Voltage}
 BCM2838_MBOX_TAG_GET_VOLTAGE       = $00030003; {The voltage value may be clamped to the supported range. A value of 0x80000000 means the id was not valid}
 BCM2838_MBOX_TAG_SET_VOLTAGE       = $00038003; {The voltage value may be clamped to the supported range. A value of 0x80000000 means the id was not valid}

 BCM2838_MBOX_TAG_GET_MAX_VOLTAGE   = $00030005; {Return the maximum supported voltage rate for the given id. Voltages should not be set higher than this}
 BCM2838_MBOX_TAG_GET_MIN_VOLTAGE   = $00030008; {Return the minimum supported voltage rate for the given id. This may be used when idle}

 BCM2838_MBOX_TAG_GET_TEMP          = $00030006; {Return the temperature of the SoC in thousandths of a degree C. id should be zero}
 BCM2838_MBOX_TAG_GET_MAX_TEMP      = $0003000a; {Return the maximum safe temperature of the SoC in thousandths of a degree C. id should be zero. Overclock may be disabled above this temperature}
 {GPU}
 BCM2838_MBOX_TAG_ALLOCATE_MEMORY   = $0003000c; {Allocates contiguous memory on the GPU. size and alignment are in bytes}
 BCM2838_MBOX_TAG_LOCK_MEMORY       = $0003000d; {Lock buffer in place, and return a bus address. Must be done before memory can be accessed}
 BCM2838_MBOX_TAG_UNLOCK_MEMORY     = $0003000e; {Unlock buffer. It retains contents, but may move. Needs to be locked before next use. status=0 is success}
 BCM2838_MBOX_TAG_RELEASE_MEMORY    = $0003000f; {Free the memory buffer. status=0 is success}

 BCM2838_MBOX_TAG_EXECUTE_CODE      = $00030010; {Calls the function at given (bus) address and with arguments given. E.g. r0 = fn(r0, r1, r2, r3, r4, r5); It blocks until call completes}
 BCM2838_MBOX_TAG_EXECUTE_QPU       = $00030011; {Execute an assembled block of code using one or more Quad Processing Units (QPUs)}
 BCM2838_MBOX_TAG_ENABLE_QPU        = $00030012; {Enable the Quad Processing Units (QPUs)}

 BCM2838_MBOX_TAG_GET_DISPMANX_HANDLE = $00030014; {Gets the mem_handle associated with a created dispmanx resource. This can be locked and the memory directly written from the arm to avoid having to copy the image data to GPU}
 BCM2838_MBOX_TAG_GET_EDID_BLOCK    = $00030020; {This reads the specified EDID block from attached HDMI/DVI device. There will always be at least one block of 128 bytes, but there may be additional blocks. You should keep requesting blocks (starting from 0) until the status returned is non-zero}

 BCM2838_MBOX_TAG_GET_CUSTOMER_OTP  = $00030021;
 BCM2838_MBOX_TAG_SET_CUSTOMER_OTP  = $00038021;

 BCM2838_MBOX_TAG_GET_DOMAIN_STATE  = $00030030;
 BCM2838_MBOX_TAG_SET_DOMAIN_STATE  = $00038030;

 BCM2838_MBOX_TAG_GET_GPIO_STATE    = $00030041; {Get the current state of a GPIO expander pin}
 BCM2838_MBOX_TAG_SET_GPIO_STATE    = $00038041; {Set the current state of a GPIO expander pin}

 BCM2838_MBOX_TAG_SET_SDHOST_CLOCK  = $00038042; {Tell the firmware the SD Host clock setting so it will be adjusted for changes in core frequency}

 BCM2838_MBOX_TAG_GET_GPIO_CONFIG   = $00030043; {Get the current configuration of a GPIO expander pin}
 BCM2838_MBOX_TAG_SET_GPIO_CONFIG   = $00038043; {Set the current configuration of a GPIO expander pin}

 BCM2838_MBOX_TAG_GET_THROTTLED     = $00030046;

 BCM2838_MBOX_TAG_GET_CLOCK_MEASURED = $00030047;

 BCM2838_MBOX_TAG_GET_PERIPH_REG    = $00030045;
 BCM2838_MBOX_TAG_SET_PERIPH_REG    = $00038045;

 BCM2838_MBOX_TAG_GET_POE_HAT_VAL   = $00030049;
 BCM2838_MBOX_TAG_SET_POE_HAT_VAL   = $00038049;

 BCM2838_MBOX_TAG_NOTIFY_REBOOT     = $00030048;
 BCM2838_MBOX_TAG_NOTIFY_XHCI_RESET = $00030058;

 BCM2838_MBOX_TAG_GET_BOOT_MODE     = $0003005b; {Get the boot mode that was used by the firmware to load the kernel (1 = SD, 4 = MSD etc)}

 BCM2838_MBOX_TAG_GET_REBOOT_FLAGS  = $00030064;
 BCM2838_MBOX_TAG_SET_REBOOT_FLAGS  = $00038064;

 BCM2838_MBOX_TAG_NOTIFY_DISPLAY_DONE = $00030066;

 {Frame Buffer}
 BCM2838_MBOX_TAG_ALLOCATE_BUFFER    = $00040001; {If the requested alignment is unsupported then the current base and size (which may be 0 if not allocated) is returned and no change occurs}
 BCM2838_MBOX_TAG_RELEASE_BUFFER    = $00048001; {Releases and disables the frame buffer}

 BCM2838_MBOX_TAG_SET_BLANK_SCREEN  = $00040002;
 BCM2838_MBOX_TAG_TEST_BLANK_SCREEN = $00044002; {Previously BCM2838_MBOX_TAG_TST_BLANK_SCREEN}

 BCM2838_MBOX_TAG_GET_PHYSICAL_W_H    = $00040003; {Note that the "physical (display)" size is the size of the allocated buffer in memory, not the resolution of the video signal sent to the display device}
 BCM2838_MBOX_TAG_TEST_PHYSICAL_W_H    = $00044003;
 BCM2838_MBOX_TAG_SET_PHYSICAL_W_H    = $00048003;

 BCM2838_MBOX_TAG_GET_VIRTUAL_W_H    = $00040004; {Note that the "virtual (buffer)" size is the portion of buffer that is sent to the display device, not the resolution the buffer itself. This may be smaller than the allocated buffer size in order to implement panning}
 BCM2838_MBOX_TAG_TEST_VIRTUAL_W_H    = $00044004;
 BCM2838_MBOX_TAG_SET_VIRTUAL_W_H    = $00048004;

 BCM2838_MBOX_TAG_GET_DEPTH            = $00040005;
 BCM2838_MBOX_TAG_TEST_DEPTH        = $00044005;
 BCM2838_MBOX_TAG_SET_DEPTH            = $00048005;

 BCM2838_MBOX_TAG_GET_PIXEL_ORDER    = $00040006;
 BCM2838_MBOX_TAG_TEST_PIXEL_ORDER    = $00044006;
 BCM2838_MBOX_TAG_SET_PIXEL_ORDER    = $00048006;

 BCM2838_MBOX_TAG_GET_ALPHA_MODE    = $00040007;
 BCM2838_MBOX_TAG_TEST_ALPHA_MODE    = $00044007;
 BCM2838_MBOX_TAG_SET_ALPHA_MODE    = $00048007;

 BCM2838_MBOX_TAG_GET_PITCH            = $00040008;
 BCM2838_MBOX_TAG_TEST_PITCH        = $00044008; {Previously BCM2838_MBOX_TAG_TST_PITCH}
 BCM2838_MBOX_TAG_SET_PITCH         = $00048008;

 BCM2838_MBOX_TAG_GET_VIRTUAL_OFFSET    = $00040009; {Offset of physical display window within virtual buffer}
 BCM2838_MBOX_TAG_TEST_VIRTUAL_OFFSET    = $00044009;
 BCM2838_MBOX_TAG_SET_VIRTUAL_OFFSET    = $00048009;

 BCM2838_MBOX_TAG_GET_OVERSCAN        = $0004000a;
 BCM2838_MBOX_TAG_TEST_OVERSCAN        = $0004400a;
 BCM2838_MBOX_TAG_SET_OVERSCAN        = $0004800a;

 BCM2838_MBOX_TAG_GET_PALETTE        = $0004000b;
 BCM2838_MBOX_TAG_TEST_PALETTE        = $0004400b;
 BCM2838_MBOX_TAG_SET_PALETTE        = $0004800b;

 BCM2838_MBOX_TAG_GET_LAYER         = $0004000c;
 BCM2838_MBOX_TAG_TEST_LAYER        = $0004400c; {Previously BCM2838_MBOX_TAG_TST_LAYER}
 BCM2838_MBOX_TAG_SET_LAYER         = $0004800c;

 BCM2838_MBOX_TAG_GET_TRANSFORM     = $0004000d;
 BCM2838_MBOX_TAG_TEST_TRANSFORM    = $0004400d; {Previously BCM2838_MBOX_TAG_TST_TRANSFORM}
 BCM2838_MBOX_TAG_SET_TRANSFORM     = $0004800d;

 BCM2838_MBOX_TAG_TEST_VSYNC        = $0004400e; {Previously BCM2838_MBOX_TAG_TST_VSYNC}
 BCM2838_MBOX_TAG_SET_VSYNC         = $0004800e;

 BCM2838_MBOX_TAG_SET_BACKLIGHT     = $0004800f;

 BCM2838_MBOX_TAG_GET_TOUCHBUF      = $0004000f;
 BCM2838_MBOX_TAG_SET_TOUCHBUF      = $0004801f;

 BCM2838_MBOX_TAG_GET_GPIOVIRTBUF   = $00040010;
 BCM2838_MBOX_TAG_SET_GPIOVIRTBUF   = $00048020;

 BCM2838_MBOX_TAG_GET_DISPLAY_ID       = $00040016;
 BCM2838_MBOX_TAG_SET_DISPLAY_NUM      = $00048013;
 BCM2838_MBOX_TAG_GET_NUM_DISPLAYS     = $00040013;
 BCM2838_MBOX_TAG_GET_DISPLAY_SETTINGS = $00040014;

 BCM2838_MBOX_TAG_SET_PLANE          = $00048015;
 BCM2838_MBOX_TAG_GET_DISPLAY_TIMING = $00040017;
 BCM2838_MBOX_TAG_SET_TIMING         = $00048017;
 BCM2838_MBOX_TAG_GET_DISPLAY_CFG    = $00040018;
 BCM2838_MBOX_TAG_SET_DISPLAY_POWER  = $00048019;

 BCM2838_MBOX_TAG_SET_CURSOR_INFO   = $00008010; {00008011} {These were reversed in the documentation, see Linux \include\soc\bcm2835\raspberrypi-firmware.h}
 BCM2838_MBOX_TAG_SET_CURSOR_STATE  = $00008011; {00008010}
 {VCHIQ}
 BCM2838_MBOX_TAG_VCHIQ_INIT        = $00048010;
 {Config}
 BCM2838_MBOX_TAG_GET_COMMAND_LINE  = $00050001;
 {Shared Resource Management}
 BCM2838_MBOX_TAG_GET_DMA_CHANNELS  = $00060001; {Caller assumes that the VC has enabled all the usable DMA channels}
 {End}
 BCM2838_MBOX_TAG_END               = $00000000;

 {BCM2838 mailbox tag Get Board Revision values (See: http://elinux.org/RPi_HardwareHistory)}
 BCM2838_BOARD_REV_2B_1        = $00A01041;
 BCM2838_BOARD_REV_2B_2        = $00A21041;
 BCM2838_BOARD_REV_2B_3     = $00A22042;

 BCM2838_BOARD_REV_3B_1     = $00A02082;
 BCM2838_BOARD_REV_3B_2     = $00A22082;
 BCM2838_BOARD_REV_3B_3     = $00A32082;

 BCM2838_BOARD_REV_CM3_1    = $00A020A0;
 BCM2838_BOARD_REV_CM3_2    = $00A220A0;

 BCM2838_BOARD_REV_MASK     = $00FFFFFF; {Mask off the warranty bit}

 {BCM2838 mailbox tag Get Board Revision bit fields (See: https://www.raspberrypi.org/documentation/hardware/raspberrypi/revision-codes/README.md)}
 BCM2838_BOARD_REVISION_PCB_MASK             = ($F shl 0);  {PCB Revision Number}

 BCM2838_BOARD_REVISION_MODEL_MASK           = ($FF shl 4); {Model Number}
 BCM2838_BOARD_REVISION_MODEL_A              = (0 shl 4);   {Model A (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_B              = (1 shl 4);   {Model B (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_APLUS          = (2 shl 4);   {Model A+ (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_BPLUS          = (3 shl 4);   {Model B+ (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_2B             = (4 shl 4);   {Model 2B (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_ALPHA          = (5 shl 4);   {Unknown}
 BCM2838_BOARD_REVISION_MODEL_COMPUTE        = (6 shl 4);   {Compute Module (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_UNKNOWN        = (7 shl 4);   {Unknown}
 BCM2838_BOARD_REVISION_MODEL_3B             = (8 shl 4);   {Model 3B (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_ZERO           = (9 shl 4);   {Model Zero (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_COMPUTE3       = (10 shl 4);  {Compute Module 3 (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_UNKNOWN_2      = (11 shl 4);  {Unknown}
 BCM2838_BOARD_REVISION_MODEL_ZERO_W         = (12 shl 4);  {Model Zero W (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_3BPLUS         = (13 shl 4);  {Model 3B+ (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_3APLUS         = (14 shl 4);  {Model 3A+ (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_COMPUTE3PLUS   = (16 shl 4);  {Compute Module 3+ (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_4B             = (17 shl 4);  {Model 4B}
 BCM2838_BOARD_REVISION_MODEL_ZERO2_W        = (18 shl 4);  {Model Zero 2 W (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_MODEL_400            = (19 shl 4);  {Pi 400}
 BCM2838_BOARD_REVISION_MODEL_CM4            = (20 shl 4);  {Compute Module 4}
 BCM2838_BOARD_REVISION_MODEL_CM4S           = (21 shl 4);  {Compute Module 4S}

 BCM2838_BOARD_REVISION_PROCESSOR_MASK       = ($F shl 12); {Processor Type}
 BCM2838_BOARD_REVISION_PROCESSOR_BCM2835    = (0 shl 12);  {BCM2835 (Cannot occur on BCM2838)}
 BCM2838_BOARD_REVISION_PROCESSOR_BCM2836    = (1 shl 12);  {BCM2836}
 BCM2838_BOARD_REVISION_PROCESSOR_BCM2837    = (2 shl 12);  {BCM2837}
 BCM2838_BOARD_REVISION_PROCESSOR_BCM2838    = (3 shl 12);  {BCM2838}

 BCM2838_BOARD_REVISION_MANUFACTURER_MASK       = ($F shl 16); {Manufacturer}
 BCM2838_BOARD_REVISION_MANUFACTURER_SONY       = (0 shl 16);  {Sony}
 BCM2838_BOARD_REVISION_MANUFACTURER_EGOMAN     = (1 shl 16);  {Egoman}
 BCM2838_BOARD_REVISION_MANUFACTURER_EMBEST     = (2 shl 16);  {Embest}
 BCM2838_BOARD_REVISION_MANUFACTURER_SONY_JAPAN = (3 shl 16);  {Sony (Japan)}
 BCM2838_BOARD_REVISION_MANUFACTURER_EMBEST2    = (4 shl 16);  {Embest}
 BCM2838_BOARD_REVISION_MANUFACTURER_STADIUM    = (5 shl 16);  {Stadium}

 BCM2838_BOARD_REVISION_MEMORY_MASK          = ($7 shl 20); {Memory Size}
 BCM2838_BOARD_REVISION_MEMORY_256M          = (0 shl 20);  {256M}
 BCM2838_BOARD_REVISION_MEMORY_512M          = (1 shl 20);  {512M}
 BCM2838_BOARD_REVISION_MEMORY_1024M         = (2 shl 20);  {1024M}
 BCM2838_BOARD_REVISION_MEMORY_2048M         = (3 shl 20);  {2048M}
 BCM2838_BOARD_REVISION_MEMORY_4096M         = (4 shl 20);  {4096M}
 BCM2838_BOARD_REVISION_MEMORY_8192M         = (5 shl 20);  {8192M}

 BCM2838_BOARD_REVISION_ENCODED_FLAG         = (1 shl 23);  {Encoded Flag, if set then revision uses this encoding}

 BCM2838_BOARD_REVISION_MASK                 = $00FFFFFF;   {Mask off the warranty bits}

 {BCM2838 mailbox tag Power State devices}
 BCM2838_MBOX_POWER_DEVID_SDHCI        = 0;
 BCM2838_MBOX_POWER_DEVID_UART0        = 1;
 BCM2838_MBOX_POWER_DEVID_UART1        = 2;
 BCM2838_MBOX_POWER_DEVID_USB_HCD    = 3;
 BCM2838_MBOX_POWER_DEVID_I2C0        = 4;
 BCM2838_MBOX_POWER_DEVID_I2C1        = 5;
 BCM2838_MBOX_POWER_DEVID_I2C2        = 6;
 BCM2838_MBOX_POWER_DEVID_SPI        = 7;
 BCM2838_MBOX_POWER_DEVID_CCP2TX    = 8;

 BCM2838_MBOX_POWER_DEVID_UNKNOWN   = $FFFFFFFF;

 {BCM2838 mailbox tag Power State requests}
 BCM2838_MBOX_SET_POWER_STATE_REQ_OFF    = (0 shl 0);
 BCM2838_MBOX_SET_POWER_STATE_REQ_ON    = (1 shl 0);
 BCM2838_MBOX_SET_POWER_STATE_REQ_WAIT    = (1 shl 1);

 {BCM2838 mailbox tag Power State values}
 BCM2838_MBOX_POWER_STATE_RESP_OFF        = (0 shl 0);
 BCM2838_MBOX_POWER_STATE_RESP_ON        = (1 shl 0);
 BCM2838_MBOX_POWER_STATE_RESP_NODEV    = (1 shl 1);  {Device doesn't exist}

 {BCM2838 mailbox tag Clock State/Rate ids}
 BCM2838_MBOX_CLOCK_ID_RESERVED  = 0;
 BCM2838_MBOX_CLOCK_ID_EMMC         = 1;
 BCM2838_MBOX_CLOCK_ID_UART         = 2;
 BCM2838_MBOX_CLOCK_ID_ARM         = 3;
 BCM2838_MBOX_CLOCK_ID_CORE         = 4;
 BCM2838_MBOX_CLOCK_ID_V3D         = 5;
 BCM2838_MBOX_CLOCK_ID_H264         = 6;
 BCM2838_MBOX_CLOCK_ID_ISP         = 7;
 BCM2838_MBOX_CLOCK_ID_SDRAM     = 8;
 BCM2838_MBOX_CLOCK_ID_PIXEL     = 9;
 BCM2838_MBOX_CLOCK_ID_PWM         = 10;
 BCM2838_MBOX_CLOCK_ID_HEVC         = 11;
 BCM2838_MBOX_CLOCK_ID_EMMC2     = 12;
 BCM2838_MBOX_CLOCK_ID_M2MC         = 13;
 BCM2838_MBOX_CLOCK_ID_PIXEL_BVB = 14;

 BCM2838_MBOX_CLOCK_ID_UNKNOWN   = $FFFFFFFF;

 {BCM2838 mailbox tag Clock State requests}
 BCM2838_MBOX_SET_CLOCK_STATE_REQ_OFF      = (0 shl 0);
 BCM2838_MBOX_SET_CLOCK_STATE_REQ_ON      = (1 shl 0);
 BCM2838_MBOX_SET_CLOCK_STATE_REQ_NOCLOCK = (1 shl 1);  {Clock doesn't exist}

 {BCM2838 mailbox tag Clock State values}
 BCM2838_MBOX_CLOCK_STATE_RESP_OFF          = (0 shl 0);
 BCM2838_MBOX_CLOCK_STATE_RESP_ON          = (1 shl 0);
 BCM2838_MBOX_CLOCK_STATE_RESP_NOCLOCK      = (1 shl 1);  {Clock doesn't exist}

 {BCM2838 mailbox tag Clock Rate turbo}
 BCM2838_MBOX_CLOCK_RATE_REQ_SKIP_TURBO  = (1 shl 0);

 {BCM2838 mailbox tag Voltage ids}
 BCM2838_MBOX_VOLTAGE_ID_RESERVED = $00000000;
 BCM2838_MBOX_VOLTAGE_ID_CORE     = $00000001;
 BCM2838_MBOX_VOLTAGE_ID_SDRAM_C  = $00000002;
 BCM2838_MBOX_VOLTAGE_ID_SDRAM_P  = $00000003;
 BCM2838_MBOX_VOLTAGE_ID_SDRAM_I  = $00000004;

 {BCM2838 mailbox tag Voltage values}
 BCM2838_MBOX_VOLTAGE_INVALID = $80000000;  {A value of 0x80000000 means the id was not valid}

 {BCM2838 mailbox tag Temperature ids}
 BCM2838_MBOX_TEMP_ID_SOC = 0;

 {BCM2838 mailbox Display ids (These are compatible with the DISPMANX_ID_* values)}
 BCM2838_MBOX_DISPLAY_ID_MAIN_LCD    = 0;
 BCM2838_MBOX_DISPLAY_ID_AUX_LCD     = 1;
 BCM2838_MBOX_DISPLAY_ID_HDMI0       = 2;
 BCM2838_MBOX_DISPLAY_ID_SDTV        = 3;
 BCM2838_MBOX_DISPLAY_ID_FORCE_LCD   = 4;
 BCM2838_MBOX_DISPLAY_ID_FORCE_TV    = 5;
 BCM2838_MBOX_DISPLAY_ID_FORCE_OTHER = 6; {Non-default display}
 BCM2838_MBOX_DISPLAY_ID_HDMI1       = 7;
 BCM2838_MBOX_DISPLAY_ID_FORCE_TV2   = 8;

 {BCM2838 mailbox tag Memory flags}
 BCM2838_MBOX_MEM_FLAG_DISCARDABLE      = (1 shl 0); {Can be resized to 0 at any time. Use for cached data}
 BCM2838_MBOX_MEM_FLAG_NORMAL           = (0 shl 2); {Normal allocating alias. Don't use from ARM}
 BCM2838_MBOX_MEM_FLAG_DIRECT           = (1 shl 2); {0xC alias uncached}
 BCM2838_MBOX_MEM_FLAG_COHERENT         = (2 shl 2); {0x8 alias. Non-allocating in L2 but coherent}
 BCM2838_MBOX_MEM_FLAG_L1_NONALLOCATING = (BCM2838_MBOX_MEM_FLAG_DIRECT or BCM2838_MBOX_MEM_FLAG_COHERENT); {Allocating in L2}
 BCM2838_MBOX_MEM_FLAG_ZERO             = (1 shl 4); {Initialise buffer to all zeros}
 BCM2838_MBOX_MEM_FLAG_NO_INIT          = (1 shl 5); {Don't initialise (default is initialise to all ones}
 BCM2838_MBOX_MEM_FLAG_HINT_PERMALOCK   = (1 shl 6); {Likely to be locked for long periods of time}

 {BCM2838 mailbox tag Blank Screen values}
 BCM2838_MBOX_BLANK_SCREEN_REQ_ON      = (1 shl 0);

 {BCM2838 mailbox tag Pixel Order values}
 BCM2838_MBOX_PIXEL_ORDER_BGR        = 0;
 BCM2838_MBOX_PIXEL_ORDER_RGB        = 1;

 {BCM2838 mailbox tag Alpha Mode values}
 BCM2838_MBOX_ALPHA_MODE_0_OPAQUE        = 0;
 BCM2838_MBOX_ALPHA_MODE_0_TRANSPARENT    = 1;
 BCM2838_MBOX_ALPHA_MODE_IGNORED        = 2;

 {BCM2838 mailbox tag Palette values}
 BCM2838_MBOX_PALETTE_INVALID = $00000001;

 {BCM2838 mailbox tag Cursor State values}
 BCM2838_MBOX_CURSOR_INVISIBLE = 0;
 BCM2838_MBOX_CURSOR_VISIBLE   = 1;

 {BCM2838 mailbox tag Cursor State flags}
 BCM2838_MBOX_CURSOR_STATE_DISPLAY_COORDS     = (0 shl 0);
 BCM2838_MBOX_CURSOR_STATE_FRAMEBUFFER_COORDS = (1 shl 0);

 {BCM2838 mailbox tag Cursor values}
 BCM2838_MBOX_CURSOR_INVALID = $00000001;

 {BCM2838 mailbox request / response codes}
 BCM2838_MBOX_REQUEST_CODE          = $00000000;
 BCM2838_MBOX_RESPONSE_CODE_SUCCESS    = $80000000;
 BCM2838_MBOX_RESPONSE_CODE_ERROR    = $80000001;

 {BCM2838 mailbox tag request / response codes}
 BCM2838_MBOX_TAG_REQUEST_CODE  = $00000000;
 BCM2838_MBOX_TAG_RESPONSE_CODE = $80000000;

const
 {BCM2838 GPIO constants}
 BCM2838_GPIO_PIN_COUNT = 58;
 BCM2838_GPIO_BANK_COUNT = 2;

 BCM2838_GPIO_SIGNATURE = $6770696F; {The ASCII value 'GPIO' returned when reading from a write only register}

 {BCM2838 Virtual GPIO constants}
 BCM2838_VIRTUAL_GPIO_PIN_COUNT = 2;  {Raspberry Pi 4B only}

 {Function Select Registers}
 BCM2838_GPFSEL0 = $00000000; {GPIO Function Select 0}
 BCM2838_GPFSEL1 = $00000004; {GPIO Function Select 1}
 BCM2838_GPFSEL2 = $00000008; {GPIO Function Select 2}
 BCM2838_GPFSEL3 = $0000000C; {GPIO Function Select 3}
 BCM2838_GPFSEL4 = $00000010; {GPIO Function Select 4}
 BCM2838_GPFSEL5 = $00000014; {GPIO Function Select 5}

 {Pin Output Set Registers}
 BCM2838_GPSET0 = $0000001C; {GPIO Pin Output Set 0}
 BCM2838_GPSET1 = $00000020; {GPIO Pin Output Set 1}

 {Pin Output Clear Registers}
 BCM2838_GPCLR0 = $00000028; {GPIO Pin Output Clear 0}
 BCM2838_GPCLR1 = $0000002C; {GPIO Pin Output Clear 1}

 {Pin Level Registers}
 BCM2838_GPLEV0 = $00000034; {GPIO Pin Level 0}
 BCM2838_GPLEV1 = $00000038; {GPIO Pin Level 1}

 {Pin Event Detect Status Registers}
 BCM2838_GPEDS0 = $00000040; {GPIO Pin Event Detect Status 0}
 BCM2838_GPEDS1 = $00000044; {GPIO Pin Event Detect Status 1}

 {Pin Rising Edge Detect Enable Registers}
 BCM2838_GPREN0 = $0000004c; {GPIO Pin Rising Edge Detect Enable 0}
 BCM2838_GPREN1 = $00000050; {GPIO Pin Rising Edge Detect Enable 1}

 {Pin Falling Edge Detect Enable Registers}
 BCM2838_GPFEN0 = $00000058; {GPIO Pin Falling Edge Detect Enable 0}
 BCM2838_GPFEN1 = $0000005c; {GPIO Pin Falling Edge Detect Enable 1}

 {Pin High Detect Enable Registers}
 BCM2838_GPHEN0 = $00000064; {GPIO Pin High Detect Enable 0}
 BCM2838_GPHEN1 = $00000068; {GPIO Pin High Detect Enable 1}

 {Pin Low Detect Enable Registers}
 BCM2838_GPLEN0 = $00000070; {GPIO Pin Low Detect Enable 0}
 BCM2838_GPLEN1 = $00000074; {GPIO Pin Low Detect Enable 1}

 {Pin Async. Rising Edge Detect Registers}
 BCM2838_GPAREN0 = $0000007c; {GPIO Pin Async. Rising Edge Detect 0}
 BCM2838_GPAREN1 = $00000080; {GPIO Pin Async. Rising Edge Detect 1}

 {Pin Async. Falling Edge Detect Registers}
 BCM2838_GPAFEN0 = $00000088; {GPIO Pin Async. Falling Edge Detect 0}
 BCM2838_GPAFEN1 = $0000008c; {GPIO Pin Async. Falling Edge Detect 1}

 {Pin Mux Register (See: https://github.com/raspberrypi/documentation/issues/1209)}
 BCM2838_GPPINMUX = $000000D0;

 {Pin Pull-up/down Mode Registers}
 BCM2838_GPPUD0 = $000000E4; {GPIO Pin Pull-up/down Mode 0}
 BCM2838_GPPUD1 = $000000E8; {GPIO Pin Pull-up/down Mode 1}
 BCM2838_GPPUD2 = $000000EC; {GPIO Pin Pull-up/down Mode 2}
 BCM2838_GPPUD3 = $000000F0; {GPIO Pin Pull-up/down Mode 3}

 {Function Select Mask}
 BCM2838_GPFSEL_MASK = 7;

 {Function Select Values}
 BCM2838_GPFSEL_IN   = 0;
 BCM2838_GPFSEL_OUT  = 1;
 BCM2838_GPFSEL_ALT0 = 4;
 BCM2838_GPFSEL_ALT1 = 5;
 BCM2838_GPFSEL_ALT2 = 6;
 BCM2838_GPFSEL_ALT3 = 7;
 BCM2838_GPFSEL_ALT4 = 3;
 BCM2838_GPFSEL_ALT5 = 2;

 {Pin Output Set Mask}
 BCM2838_GPSET_MASK = 1;

 {Pin Output Clear Mask}
 BCM2838_GPCLR_MASK = 1;

 {Pin Level Mask}
 BCM2838_GPLEV_MASK = 1;

 {Pin Event Detect Status Mask}
 BCM2838_GPEDS_MASK = 1;

 {Pin Rising Edge Detect Enable Mask}
 BCM2838_GPREN_MASK = 1;

 {Pin Falling Edge Detect Enable Mask}
 BCM2838_GPFEN_MASK = 1;

 {Pin High Detect Enable Mask}
 BCM2838_GPHEN_MASK = 1;

 {Pin Low Detect Enable Mask}
 BCM2838_GPLEN_MASK = 1;

 {Pin Async. Rising Edge Detect Mask}
 BCM2838_GPAREN_MASK = 1;

 {Pin Async. Falling Edge Detect Mask}
 BCM2838_GPAFEN_MASK = 1;

 {Pull-up/down Mode Mask}
 BCM2838_GPPUD_MASK = 3;

 {Pull-up/down Mode Values}
 BCM2838_GPPUD_NONE = 0;
 BCM2838_GPPUD_UP   = 1;
 BCM2838_GPPUD_DOWN = 2;

 {Pin Mux Values}
 BCM2838_GPPINMUX_RGMII_PHY_GPIO = 1; {If set enables the RGMII PHY signals onto GPIOs 46+}
 BCM2838_GPPINMUX_LEGACY_EMMC    = 2; {If set legacy EMMC controller (Arasan SD) is connected to the SD card socket}

{==============================================================================}
const
 {BCM2838 ARM local constants}
 BCM2838_ARM_LOCAL_BASE  =  $FF800000;
 BCM2838_ARM_LOCAL_SIZE  =  SIZE_8M;

 {Physical memory addresses of BCM2838 ARM local peripherals  (See: BCM2711 ARM Peripherals)}
 BCM2838_ARM_LOCAL_REGS_BASE    = (BCM2838_ARM_LOCAL_BASE + $0000);

 {GICv2}
 BCM2838_GICDIST_REGS_BASE      = BCM2838_ARM_LOCAL_BASE + $41000;
 BCM2838_GICCPU_REGS_BASE       = BCM2838_ARM_LOCAL_BASE + $42000;

const
 {IRQ lines of BCM2838 ARM local peripherals (See: BCM2711 ARM Peripherals)}
 {ARM Generic Timers}
 BCM2838_IRQ_LOCAL_ARM_CNTPSIRQ  = BCM2838_IRQ_TIMER0;
 BCM2838_IRQ_LOCAL_ARM_CNTPNSIRQ = BCM2838_IRQ_TIMER1;
 BCM2838_IRQ_LOCAL_ARM_CNTVIRQ   = BCM2838_IRQ_TIMER2;
 BCM2838_IRQ_LOCAL_ARM_CNTHPIRQ  = BCM2838_IRQ_TIMER3;

 {ARM Malboxes0-3}
 BCM2838_IRQ_LOCAL_ARM_MAILBOX0:array[0..BCM2838_CPU_COUNT - 1] of LongWord = (
  BCM2838_IRQ_MAILBOX0_0,
  BCM2838_IRQ_MAILBOX0_1,
  BCM2838_IRQ_MAILBOX0_2,
  BCM2838_IRQ_MAILBOX0_3);

 BCM2838_IRQ_LOCAL_ARM_MAILBOX1:array[0..BCM2838_CPU_COUNT - 1] of LongWord = (
  BCM2838_IRQ_MAILBOX1_0,
  BCM2838_IRQ_MAILBOX1_1,
  BCM2838_IRQ_MAILBOX1_2,
  BCM2838_IRQ_MAILBOX1_3);

 BCM2838_IRQ_LOCAL_ARM_MAILBOX2:array[0..BCM2838_CPU_COUNT - 1] of LongWord = (
  BCM2838_IRQ_MAILBOX2_0,
  BCM2838_IRQ_MAILBOX2_1,
  BCM2838_IRQ_MAILBOX2_2,
  BCM2838_IRQ_MAILBOX2_3);

 BCM2838_IRQ_LOCAL_ARM_MAILBOX3:array[0..BCM2838_CPU_COUNT - 1] of LongWord = (
  BCM2838_IRQ_MAILBOX3_0,
  BCM2838_IRQ_MAILBOX3_1,
  BCM2838_IRQ_MAILBOX3_2,
  BCM2838_IRQ_MAILBOX3_3);

 {ARM Performance Monitoring}
 BCM2838_IRQ_LOCAL_ARM_PMU:array[0..BCM2838_CPU_COUNT - 1] of LongWord = (
  BCM2838_IRQ_PMU0,
  BCM2838_IRQ_PMU1,
  BCM2838_IRQ_PMU2,
  BCM2838_IRQ_PMU3);

 {AXI Outstanding}
 BCM2838_IRQ_LOCAL_ARM_AXIQUIET = BCM2838_IRQ_LOCAL_AXIQUIET;

 {ARM Local Timer}
 BCM2838_IRQ_LOCAL_ARM_TIMER = BCM2838_IRQ_LOCAL_TIMER;

 {AXI Error}
 BCM2838_IRQ_LOCAL_ARM_AXIERR = BCM2838_IRQ_LOCAL_AXIERR;

const
 {ARM local Control register bits (See Table 103 ARM_CONTROL register)}
 BCM2838_ARM_LOCAL_CONTROL_AXIERRIRQ_DIS = (1 shl 6); {Masks the AXI Error interrupt}
 BCM2838_ARM_LOCAL_CONTROL_AXIERRIRQ_EN  = (0 shl 6); {Enables the AXI Error interrupt}
 BCM2838_ARM_LOCAL_CONTROL_APB_CLOCK     = (1 shl 7); {64-bit Core timer runs from the APB clock}
 BCM2838_ARM_LOCAL_CONTROL_CRYSTAL_CLOCK = (0 shl 7); {64-bit Core timer runs from the Crystal clock}
 BCM2838_ARM_LOCAL_CONTROL_INCREMENT_2   = (1 shl 8); {64-bit Core timer increments by 2}
 BCM2838_ARM_LOCAL_CONTROL_INCREMENT_1   = (0 shl 8); {64-bit Core timer increments by 1}

 {ARM local Core Timer Prescaler register bits}
  {Nothing}

 {ARM local Core Interrupt Routing register bits (See Table 104 CORE_IRQ_CONTROL Register)}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_IRQ0 = (0 shl 4); {AXI_ERR IRQ goes to IRQ input of core 0}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_IRQ1 = (1 shl 4); {AXI_ERR IRQ goes to IRQ input of core 1}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_IRQ2 = (2 shl 4); {AXI_ERR IRQ goes to IRQ input of core 2}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_IRQ3 = (3 shl 4); {AXI_ERR IRQ goes to IRQ input of core 3}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_FIQ0 = (4 shl 4); {AXI_ERR FIQ goes to FIQ input of core 0}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_FIQ1 = (5 shl 4); {AXI_ERR FIQ goes to FIQ input of core 1}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_FIQ2 = (6 shl 4); {AXI_ERR FIQ goes to FIQ input of core 2}
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_FIQ3 = (7 shl 4); {AXI_ERR FIQ goes to FIQ input of core 3}

 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_MASK = (7 shl 4);
 BCM2838_ARM_LOCAL_AXI_ERR_INT_ROUTING_SHIFT = 4;

 {ARM local PM Interrupt Routing Set/Clear register bits (See Table 105/106 PMU_CONTROL_SET/CLR Register }
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_IRQ0 = (1 shl 0); {Core 0 PM IRQ Enable (This bit is only valid if bit 4 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_IRQ1 = (1 shl 1); {Core 1 PM IRQ Enable (This bit is only valid if bit 5 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_IRQ2 = (1 shl 2); {Core 2 PM IRQ Enable (This bit is only valid if bit 6 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_IRQ3 = (1 shl 3); {Core 3 PM IRQ Enable (This bit is only valid if bit 7 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_FIQ0 = (1 shl 4); {Core 0 PM FIQ Enable (If set, this bit overrides the IRQ bit 0)}
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_FIQ1 = (1 shl 5); {Core 1 PM FIQ Enable (If set, this bit overrides the IRQ bit 1)}
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_FIQ2 = (1 shl 6); {Core 2 PM FIQ Enable (If set, this bit overrides the IRQ bit 2)}
 BCM2838_ARM_LOCAL_PM_INT_ROUTING_FIQ3 = (1 shl 7); {Core 3 PM FIQ Enable (If set, this bit overrides the IRQ bit 3)}

 {ARM local Core Timer Low register bits}
  {Nothing}

 {ARM local Core Timer High register bits}
  {Nothing}

 {ARM local Peripheral Interrupt Routing register bits (See Table 107 PERI_IRQ_ROUTE0 Register)}
 BCM2838_ARM_LOCAL_PERIPHERAL_WRITE_MASKS = ($01 shl 24); {This field must be written with 0x01, otherwise changes to LOCAL_TIMER_IRQ will be ignored}

 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_IRQ0 = (0 shl 0); {Local timer interrupt goes to Core 0 IRQ}
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_IRQ1 = (1 shl 0); {Local timer interrupt goes to Core 1 IRQ}
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_IRQ2 = (2 shl 0); {Local timer interrupt goes to Core 2 IRQ}
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_IRQ3 = (3 shl 0); {Local timer interrupt goes to Core 3 IRQ}
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_FIQ0 = (4 shl 0); {Local timer interrupt goes to Core 0 FIQ}
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_FIQ1 = (5 shl 0); {Local timer interrupt goes to Core 1 FIQ}
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_FIQ2 = (6 shl 0); {Local timer interrupt goes to Core 2 FIQ}
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_FIQ3 = (7 shl 0); {Local timer interrupt goes to Core 3 FIQ}

 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_MASK = (7 shl 0);
 BCM2838_ARM_LOCAL_TIMER_INT_ROUTING_SHIFT = 0;

 {ARM local AXI Outstanding Count register bits}
 BCM2838_ARM_LOCAL_AXI_COUNT_READ_MASK  = ($3FF shl 0); {Outstanding reads counter}
 BCM2838_ARM_LOCAL_AXI_COUNT_WRITE_MASK = ($3FF shl 16); {Outstanding writes counter}

 {ARM local AXI Outstanding IRQ register bits (Core 0 Only)(See Table 108 AXI_QUIET_TIME Register)}
 BCM2838_ARM_LOCAL_AXI_QUIET_IRQ_ENABLE  = (1 shl 20);
 BCM2838_ARM_LOCAL_AXI_QUIET_IRQ_TIMEOUT = ($FFFFF shl 0);

 {ARM local Local Timer Control register bits (See Table 109 LOCAL_TIMER_CONTROL Register)}
 BCM2838_ARM_LOCAL_TIMER_CONTROL_INT_STATUS  = (1 shl 31);       {Interrupt flag (Read Only)}
 BCM2838_ARM_LOCAL_TIMER_CONTROL_INT_ENABLE  = (1 shl 29);       {Interrupt enable (1= enabled)}
 BCM2838_ARM_LOCAL_TIMER_CONTROL_ENABLE      = (1 shl 28);       {Timer enable (1 = enabled)}
 BCM2838_ARM_LOCAL_TIMER_CONTROL_VALUE_MASK = ($0FFFFFFF shl 0); {Re-load value}

 {ARM local Local Timer Clear Reload register bits (Write Only)(See Table 110 LOCAL_TIMER_IRQ Register)}
 BCM2838_ARM_LOCAL_TIMER_CLEAR_INT    = (1 shl 31); {Interrupt flag clear when written as 1 (Write Only)}
 BCM2838_ARM_LOCAL_TIMER_CLEAR_RELOAD = (1 shl 30); {Local timer reloaded when written as 1 (Write Only)}

 {ARM local Timer Interrupt Control register bits (See Table 111 TIMER_CNTRL0, TIMER_CNTRL1, TIMER_CNTRL2 & TIMER_CNTRL3 Registers)}
 {Note: These are the ARM Generic Timers (See ARM Architecture Reference Manual)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSIRQ   = (1 shl 0); {Physical Secure Timer IRQ Enable (This bit is only valid if bit 4 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSIRQ  = (1 shl 1); {Physical Non Secure Timer IRQ Enable (This bit is only valid if bit 5 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPIRQ   = (1 shl 2); {Hypervisor Timer IRQ Enable (This bit is only valid if bit 6 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTVIRQ    = (1 shl 3); {Virtual Timer IRQ Enable (This bit is only valid if bit 7 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPSFIQ   = (1 shl 4); {Physical Secure Timer FIQ Enable (If set, this bit overrides the IRQ bit 0)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTPNSFIQ  = (1 shl 5); {Physical Non Secure Timer FIQ Enable (If set, this bit overrides the IRQ bit 1)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTHPFIQ   = (1 shl 6); {Hypervisor Timer FIQ Enable (If set, this bit overrides the IRQ bit 2)}
 BCM2838_ARM_LOCAL_TIMER_INT_CONTROL_CNTVFIQ    = (1 shl 7); {Virtual Timer FIQ Enable (If set, this bit overrides the IRQ bit 3)}

 {ARM local Mailbox Interrupt Control register bits (See Table 112 MAILBOX_CNTRL0, MAILBOX_CNTRL1, MAILBOX_CNTRL2 & MAILBOX_CNTRL3 Registers)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0IRQ = (1 shl 0); {Mailbox-0 IRQ Enable (This bit is only valid if bit 4 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1IRQ = (1 shl 1); {Mailbox-1 IRQ Enable (This bit is only valid if bit 5 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2IRQ = (1 shl 2); {Mailbox-2 IRQ Enable (This bit is only valid if bit 6 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3IRQ = (1 shl 3); {Mailbox-3 IRQ Enable (This bit is only valid if bit 7 is clear otherwise it is ignored)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX0FIQ = (1 shl 0); {Mailbox-0 FIQ Enable (If set, this bit overrides the IRQ bit 0)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX1FIQ = (1 shl 0); {Mailbox-1 FIQ Enable (If set, this bit overrides the IRQ bit 1)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX2FIQ = (1 shl 0); {Mailbox-2 FIQ Enable (If set, this bit overrides the IRQ bit 2)}
 BCM2838_ARM_LOCAL_MAILBOX_INT_CONTROL_MAILBOX3FIQ = (1 shl 0); {Mailbox-3 FIQ Enable (If set, this bit overrides the IRQ bit 3)}

 {ARM local IRQ Source register bits (See Table 113 IRQ_SOURCE0, IRQ_SOURCE1, IRQ_SOURCE2 & IRQ_SOURCE3 Registers)}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_CNTPSIRQ      = (1 shl 0);  {Physical Secure Timer Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_CNTPNSIRQ     = (1 shl 1);  {Physical Non Secure Timer Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_CNTHPIRQ      = (1 shl 2);  {Hypervisor Timer Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_CNTVIRQ       = (1 shl 3);  {Virtual Timer Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_MAILBOX0      = (1 shl 4);  {Mailbox 0 Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_MAILBOX1      = (1 shl 5);  {Mailbox 1 Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_MAILBOX2      = (1 shl 6);  {Mailbox 2 Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_MAILBOX3      = (1 shl 7);  {Mailbox 3 Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_GPU           = (1 shl 8);  {VideoCore Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_PMU           = (1 shl 9);  {Performance Monitor Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_AXI_QUIET     = (1 shl 10); {AXI Outstanding Interrupt (Core 0 Only)}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_TIMER         = (1 shl 11); {Local Timer Interrupt}
 BCM2838_ARM_LOCAL_IRQ_SOURCE_AXI_ERR       = (1 shl 30); {AXI error, as reported by the ARM L2 cache}

 {ARM local FIQ Source register bits (See Table 114 FIQ_SOURCE0, FIQ_SOURCE1, FIQ_SOURCE2 & FIQ_SOURCE3 Registers)}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_CNTPSIRQ      = (1 shl 0);  {Physical Secure Timer Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_CNTPNSIRQ     = (1 shl 1);  {Physical Non Secure Timer Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_CNTHPIRQ      = (1 shl 2);  {Hypervisor Timer Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_CNTVIRQ       = (1 shl 3);  {Virtual Timer Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_MAILBOX0      = (1 shl 4);  {Mailbox 0 Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_MAILBOX1      = (1 shl 5);  {Mailbox 1 Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_MAILBOX2      = (1 shl 6);  {Mailbox 2 Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_MAILBOX3      = (1 shl 7);  {Mailbox 3 Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_GPU           = (1 shl 8);  {VideoCore Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_PMU           = (1 shl 9);  {Performance Monitor Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_TIMER         = (1 shl 11); {Local Timer Fast Interrupt}
 BCM2838_ARM_LOCAL_FIQ_SOURCE_AXI_ERR       = (1 shl 30); {AXI error, as reported by the ARM L2 cache}

{==============================================================================}
{$PACKRECORDS 4}
type
 {BCM2838 specific structures}
 {Layout of the BCM2838 System Timer registers}
 PBCM2838SystemTimerRegisters = ^TBCM2838SystemTimerRegisters;
 TBCM2838SystemTimerRegisters = record
  CS:LongWord;  {System Timer Control/Status}
  CLO:LongWord; {System Timer Counter Lower 32 bits}
  CHI:LongWord; {System Timer Counter Higher 32 bits}
  C0:LongWord;  {System Timer Compare 0. Already used by the VideoCore GPU (Do not use)}
  C1:LongWord;  {System Timer Compare 1}
  C2:LongWord;  {System Timer Compare 2. Already used by the VideoCore GPU (Do not use)}
  C3:LongWord;  {System Timer Compare 3}
 end;

type
 {Layout of the BCM2838 DMA Channel registers}
 PBCM2838DMARegisters = ^TBCM2838DMARegisters;
 TBCM2838DMARegisters = record
  CS:LongWord;         {DMA Channel Control and Status}
  CONBLK_AD:LongWord;  {DMA Channel Control Block Address}
  TI:LongWord;         {DMA Channel CB Word 0 (Transfer Information)}
  SOURCE_AD:LongWord;  {DMA Channel CB Word 1 (Source Address)}
  DEST_AD:LongWord;    {DMA Channel CB Word 2 (Destination Address)}
  TXFR_LEN:LongWord;   {DMA Channel CB Word 3 (Transfer Length)}
  STRIDE:LongWord;     {DMA Channel CB Word 4 (2D Mode Stride) (Not applicable to Lite channels)}
  NEXTCONBK:LongWord;  {DMA Channel CB Word 5 (Next Control Block Address)}
  DEBUG:LongWord;      {DMA Channel Debug}
 end;

 {Layout of the BCM2838 DMA40 Channel registers}
 PBCM2838DMA40Registers = ^TBCM2838DMA40Registers;
 TBCM2838DMA40Registers = record
  CS:LongWord;         {DMA Channel Control and Status}
  CB:LongWord;         {DMA Channel Control Block Address}
  Reserved1:LongWord;
  DEBUG:LongWord;      {DMA Channel Debug}
  TI:LongWord;         {DMA Channel CB Word 0 (Transfer Information)}
  SRC:LongWord;        {DMA Channel CB Word 1 (Source Address)}
  SRCI:LongWord;       {DMA Channel CB Word 2 (Source Information)}
  DEST:LongWord;       {DMA Channel CB Word 3 (Destination Address)}
  DESTI:LongWord;      {DMA Channel CB Word 4 (Destination Information)}
  LEN:LongWord;        {DMA Channel CB Word 5 (Transfer Length)}
  NEXT_CB:LongWord;    {DMA Channel CB Word 6 (Next Control Block Address)}
  DEBUG2:LongWord;     {DMA Channel Debug}
 end;

type
 {Layout of BCM2838 DMA Control Block structure} {Must be 32byte (256bit) aligned}
 PBCM2838DMAControlBlock = ^TBCM2838DMAControlBlock;
 TBCM2838DMAControlBlock = record
  TransferInformation:LongWord;
  SourceAddress:LongWord;
  DestinationAddress:LongWord;
  TransferLength:LongWord;
  ModeStride:LongWord; {Not applicable to Lite channels}
  NextControlBlockAddress:LongWord;
  Reserved1:LongWord;
  Reserved2:LongWord;
 end;

  {Layout of BCM2838 DMA40 Control Block structure} {Must be 32byte (256bit) aligned}
 PBCM2838DMA40ControlBlock = ^TBCM2838DMA40ControlBlock;
 TBCM2838DMA40ControlBlock = record
  TransferInformation:LongWord;
  SourceAddress:LongWord;
  SourceInformation:LongWord;
  DestinationAddress:LongWord;
  DestinationInformation:LongWord;
  TransferLength:LongWord;
  NextControlBlockAddress:LongWord;
  Reserved1:LongWord;
 end;

type
 {Layout of the BCM2838 BSC (I2C) registers}
 PBCM2838BSCRegisters = ^TBCM2838BSCRegisters;
 TBCM2838BSCRegisters = record
  C:LongWord;    {Control}
  S:LongWord;    {Status}
  DLEN:LongWord; {Data Length}
  A:LongWord;    {Slave Address}
  FIFO:LongWord; {Data FIFO}
  CDIV:LongWord; {Clock Divider}
  DEL:LongWord;  {Data Delay}
  CLKT:LongWord; {Clock Stretch Timeout}
 end;

type
 {Layout of the BCM2838 SPI0 registers}
 PBCM2838SPI0Registers = ^TBCM2838SPI0Registers;
 TBCM2838SPI0Registers = record
  CS:LongWord;    {SPI Master Control and Status}
  FIFO:LongWord;  {SPI Master TX and RX FIFOs}
  CLK:LongWord;   {SPI Master Clock Divider}
  DLEN:LongWord;  {SPI Master Data Length}
  LTOH:LongWord;  {SPI LOSSI mode TOH}
  DC:LongWord;    {SPI DMA DREQ Controls}
 end;

type
 {Layout of the BCM2838 I2C / SPI Slave registers}
 PBCM2838I2CSPIRegisters = ^TBCM2838I2CSPIRegisters;
 TBCM2838I2CSPIRegisters = record
  DR:LongWord;         {Data Register}
  RSR:LongWord;        {Operation status register and error clear register}
  SLV:LongWord;        {I2C SPI Address Register holds the I2C slave address value}
  CR:LongWord;         {Control register is used to configure the I2C or SPI operation}
  FR:LongWord;         {Flag register}
  IFLS:LongWord;       {Interrupt fifo level select register}
  IMSC:LongWord;       {Interrupt Mask Set Clear Register}
  RIS:LongWord;        {Raw Interrupt Status Register}
  MIS:LongWord;        {Masked Interrupt Status Register}
  ICR:LongWord;        {Interrupt Clear Register}
  DMACR:LongWord;      {DMA Control Register}
  TDR:LongWord;        {FIFO Test Data Register}
  GPUSTAT:LongWord;    {GPU Status Register}
  HCTRL:LongWord;      {Host Control Register}
  DEBUG1:LongWord;     {I2C Debug Register}
  DEBUG2:LongWord;     {SPI Debug Register}
 end;

type
 {Layout of the BCM2838 AUX (UART1, SPI1 and SPI2) registers}
 PBCM2838AUXRegisters = ^TBCM2838AUXRegisters;
 TBCM2838AUXRegisters = record
  AUX_IRQ:LongWord;        {Auxiliary Interrupt status}
  AUX_ENABLE:LongWord;     {Auxiliary enables}
  Reserved01:LongWord;
  Reserved02:LongWord;
  Reserved03:LongWord;
  Reserved04:LongWord;
  Reserved05:LongWord;
  Reserved06:LongWord;
  Reserved07:LongWord;
  Reserved08:LongWord;
  Reserved09:LongWord;
  Reserved0A:LongWord;
  Reserved0B:LongWord;
  Reserved0C:LongWord;
  Reserved0D:LongWord;
  Reserved0E:LongWord;
  AUX_MU_IO:LongWord;      {Mini Uart I/O Data}
  AUX_MU_IER:LongWord;     {Mini Uart Interrupt Enable}
  AUX_MU_IIR:LongWord;     {Mini Uart Interrupt Identify}
  AUX_MU_LCR:LongWord;     {Mini Uart Line Control}
  AUX_MU_MCR:LongWord;     {Mini Uart Modem Control}
  AUX_MU_LSR:LongWord;     {Mini Uart Line Status}
  AUX_MU_MSR:LongWord;     {Mini Uart Modem Status}
  AUX_MU_SCRATCH:LongWord; {Mini Uart Scratch}
  AUX_MU_CNTL:LongWord;    {Mini Uart Extra Control}
  AUX_MU_STAT:LongWord;    {Mini Uart Extra Status}
  AUX_MU_BAUD:LongWord;    {Mini Uart Baudrate}
  Reserved11:LongWord;
  Reserved12:LongWord;
  Reserved13:LongWord;
  Reserved14:LongWord;
  Reserved15:LongWord;
  AUX_SPI1_CNTL0:LongWord; {SPI 1 Control register 0}
  AUX_SPI1_CNTL1:LongWord; {SPI 1 Control register 1}
  AUX_SPI1_STAT:LongWord;  {SPI 1 Status}
  Reserved21:LongWord;
  AUX_SPI1_IO:LongWord;    {SPI 1 Data}
  AUX_SPI1_PEEK:LongWord;  {SPI 1 Peek}
  Reserved31:LongWord;
  Reserved32:LongWord;
  Reserved33:LongWord;
  Reserved34:LongWord;
  Reserved35:LongWord;
  Reserved36:LongWord;
  Reserved37:LongWord;
  Reserved38:LongWord;
  Reserved39:LongWord;
  Reserved3A:LongWord;
  AUX_SPI2_CNTL0:LongWord; {SPI 2 Control register 0}
  AUX_SPI2_CNTL1:LongWord; {SPI 2 Control register 1}
  AUX_SPI2_STAT:LongWord;  {SPI 2 Status}
  Reserved40:LongWord;
  AUX_SPI2_IO:LongWord;    {SPI 2 Data}
  AUX_SPI2_PEEK:LongWord;  {SPI 2 Peek}
 end;

type
 {Layout of the BCM2838 PCM / I2S registers}
 PBCM2838PCMRegisters = ^TBCM2838PCMRegisters;
 TBCM2838PCMRegisters = record
  CS_A:LongWord;     {PCM Control and Status}
  FIFO_A:LongWord;   {PCM FIFO Data}
  MODE_A:LongWord;   {PCM Mode}
  RXC_A:LongWord;    {PCM Receive Configuration}
  TXC_A:LongWord;    {PCM Transmit Configuration}
  DREQ_A:LongWord;   {PCM DMA Request Level}
  INTEN_A:LongWord;  {PCM Interrupt Enables}
  INTSTC_A:LongWord; {PCM Interrupt Status & Clear}
  GRAY:LongWord;     {PCM Gray Mode Control}
 end;

type
 {Layout of the BCM2838 Pulse Width Modulator (PWM) registers}
 PBCM2838PWMRegisters = ^TBCM2838PWMRegisters;
 TBCM2838PWMRegisters = record
  CTL:LongWord;         {PWM Control}
  STA:LongWord;         {PWM Status}
  DMAC:LongWord;        {PWM DMA Configuration}
  Reserved1:LongWord;
  RNG1:LongWord;        {PWM Channel 1 Range}
  DAT1:LongWord;        {PWM Channel 1 Data}
  FIF1:LongWord;        {PWM FIFO Input}
  Reserved2:LongWord;
  RNG2:LongWord;        {PWM Channel 2 Range}
  DAT2:LongWord;        {PWM Channel 2 Data}
 end;

type
 {Layout of the BCM2838 PL011 UART registers}
 PBCM2838PL011Registers = ^TBCM2838PL011Registers;
 TBCM2838PL011Registers = record
  DR:LongWord;       {Data Register}
  RSRECR:LongWord;   {Receive Status Register / Error Clear Register}
  Reserved01:LongWord;
  Reserved02:LongWord;
  Reserved03:LongWord;
  Reserved04:LongWord;
  FR:LongWord;       {Flag register}
  Reserved05:LongWord;
  ILPR:LongWord;     {Not in use}
  IBRD:LongWord;     {Integer Baud rate divisor}
  FBRD:LongWord;     {Fractional Baud rate divisor}
  LCRH:LongWord;     {Line Control register}
  CR:LongWord;       {Control register}
  IFLS:LongWord;     {Interrupt FIFO Level Select Register}
  IMSC:LongWord;     {Interrupt Mask Set Clear Register}
  RIS:LongWord;      {Raw Interrupt Status Register}
  MIS:LongWord;      {Masked Interrupt Status Register}
  ICR:LongWord;      {Interrupt Clear Register}
  DMACR:LongWord;    {DMA Control Register}
  Reserved11:LongWord;
  Reserved12:LongWord;
  Reserved13:LongWord;
  Reserved14:LongWord;
  Reserved15:LongWord;
  Reserved16:LongWord;
  Reserved17:LongWord;
  Reserved18:LongWord;
  Reserved19:LongWord;
  Reserved1A:LongWord;
  Reserved1B:LongWord;
  Reserved1C:LongWord;
  Reserved1D:LongWord;
  ITCR:LongWord;     {Test Control Register}
  ITIP:LongWord;     {Integration Test Input Register}
  ITOP:LongWord;     {Integration Test Output Register}
  TDR:LongWord;      {Test Data Register}
 end;

type
 {Layout of the BCM2838 ARM Timer registers}
 PBCM2838ARMTimerRegisters = ^TBCM2838ARMTimerRegisters;
 TBCM2838ARMTimerRegisters = record
  Load:LongWord;        {Timer Load register}
  Value:LongWord;       {Timer Value register}
  Control:LongWord;     {Timer control register}
  IRQClear:LongWord;    {Timer IRQ clear register}
  RawIRQ:LongWord;      {Timer Raw IRQ register}
  MaskedIRQ:LongWord;   {Timer Masked IRQ register}
  Reload:LongWord;      {Timer Reload register}
  Predivider:LongWord;  {The timer pre-divider register}
  Counter:LongWord;     {Free running counter}
 end;

type
 {Layout of the BCM2838 Power Management Watchdog registers}
 PBCM2838PMWatchdogRegisters = ^TBCM2838PMWatchdogRegisters;
 TBCM2838PMWatchdogRegisters = record
  Reserved1:LongWord;
  Reserved2:LongWord;
  Reserved3:LongWord;
  Reserved4:LongWord;
  Reserved5:LongWord;
  Reserved6:LongWord;
  Reserved7:LongWord;
  RSTC:LongWord;
  RSTS:LongWord;
  WDOG:LongWord;
 end;

type
 {Layout of the BCM2838 Random Number Generator registers}
 PBCM2838RNGRegisters = ^TBCM2838RNGRegisters;
 TBCM2838RNGRegisters = record
  Control:LongWord;
  SoftResetRNG:LongWord;
  SoftResetRBG:LongWord;
  TotalBitCount:LongWord;
  TBCThreshold:LongWord;
  Reserved1:LongWord;
  IntStatus:LongWord;
  IntEnable:LongWord;
  FIFOData:LongWord;
  FIFOCount:LongWord;
 end;

type
 {Layout of the BCM2838 GPIO registers}
 PBCM2838GPIORegisters = ^TBCM2838GPIORegisters;
 TBCM2838GPIORegisters = record
  GPFSEL0:LongWord;     {GPIO Function Select 0}
  GPFSEL1:LongWord;     {GPIO Function Select 1}
  GPFSEL2:LongWord;     {GPIO Function Select 2}
  GPFSEL3:LongWord;     {GPIO Function Select 3}
  GPFSEL4:LongWord;     {GPIO Function Select 4}
  GPFSEL5:LongWord;     {GPIO Function Select 5}
  Reserved1:LongWord;
  GPSET0:LongWord;      {GPIO Pin Output Set 0}
  GPSET1:LongWord;      {GPIO Pin Output Set 1}
  Reserved2:LongWord;
  GPCLR0:LongWord;      {GPIO Pin Output Clear 0}
  GPCLR1:LongWord;      {GPIO Pin Output Clear 1}
  Reserved3:LongWord;
  GPLEV0:LongWord;      {GPIO Pin Level 0}
  GPLEV1:LongWord;      {GPIO Pin Level 1}
  Reserved4:LongWord;
  GPEDS0:LongWord;      {GPIO Pin Event Detect Status 0}
  GPEDS1:LongWord;      {GPIO Pin Event Detect Status 1}
  Reserved5:LongWord;
  GPREN0:LongWord;      {GPIO Pin Rising Edge Detect Enable 0}
  GPREN1:LongWord;      {GPIO Pin Rising Edge Detect Enable 1}
  Reserved6:LongWord;
  GPFEN0:LongWord;      {GPIO Pin Falling Edge Detect Enable 0}
  GPFEN1:LongWord;      {GPIO Pin Falling Edge Detect Enable 1}
  Reserved7:LongWord;
  GPHEN0:LongWord;      {GPIO Pin High Detect Enable 0}
  GPHEN1:LongWord;      {GPIO Pin High Detect Enable 1}
  Reserved8:LongWord;
  GPLEN0:LongWord;      {GPIO Pin Low Detect Enable 0}
  GPLEN1:LongWord;      {GPIO Pin Low Detect Enable 1}
  Reserved9:LongWord;
  GPAREN0:LongWord;     {GPIO Pin Async. Rising Edge Detect 0}
  GPAREN1:LongWord;     {GPIO Pin Async. Rising Edge Detect 1}
  Reserved10:LongWord;
  GPAFEN0:LongWord;     {GPIO Pin Async. Falling Edge Detect 0}
  GPAFEN1:LongWord;     {GPIO Pin Async. Falling Edge Detect 1}
  Reserved11:LongWord;
  Reserved12:LongWord;
  Reserved13:LongWord;
  Reserved14:LongWord;
  Reserved15:LongWord;
  Reserved16:LongWord;
  Reserved17:LongWord;
  Reserved18:LongWord;
  Reserved19:LongWord;
  Reserved20:LongWord;
  Reserved21:LongWord;
  Reserved22:LongWord;
  Reserved23:LongWord;
  Reserved24:LongWord;
  Reserved25:LongWord;
  Reserved26:LongWord;
  Reserved27:LongWord;
  Reserved28:LongWord;
  Reserved29:LongWord;
  Reserved30:LongWord;
  Reserved31:LongWord;
  GPPUD0:LongWord;      {GPIO Pin Pull-up/down Mode 0}
  GPPUD1:LongWord;      {GPIO Pin Pull-up/down Mode 1}
  GPPUD2:LongWord;      {GPIO Pin Pull-up/down Mode 2}
  GPPUD3:LongWord;      {GPIO Pin Pull-up/down Mode 3}
 end;

type
 {Layout of the BCM2838 Mailbox0 registers (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
 PBCM2838Mailbox0Registers = ^TBCM2838Mailbox0Registers;
 TBCM2838Mailbox0Registers = record
  Read:LongWord;      {Offset 0x00}{The read register for mailbox 0}
  Reserved1:LongWord; {Offset 0x04}
  Reserved2:LongWord; {Offset 0x08}
  Reserved3:LongWord; {Offset 0x0C}
  Peek:LongWord;      {Offset 0x10}{Read from the mailbox without removing data from it}
  Sender:LongWord;    {Offset 0x14}{Sender ID (bottom 2 bits only)}
  Status:LongWord;    {Offset 0x18}{The status register for mailbox 0}
  Config:LongWord;    {Offset 0x1C}{The configuration register for mailbox 0}
 end;

 {Layout of the BCM2838 Mailbox1 registers (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
 PBCM2838Mailbox1Registers = ^TBCM2838Mailbox1Registers;
 TBCM2838Mailbox1Registers = record
  Write:LongWord;     {Offset 0x00}{The write register for mailbox 0 (Also the read register for mailbox 1)}
  Reserved1:LongWord; {Offset 0x04}
  Reserved2:LongWord; {Offset 0x08}
  Reserved3:LongWord; {Offset 0x0C}
  Reserved4:LongWord; {Offset 0x10}
  Reserved5:LongWord; {Offset 0x14}
  Status:LongWord;    {Offset 0x18}{The status register for mailbox 1}
  Config:LongWord;    {Offset 0x1C}{The configuration register for mailbox 1}
 end;

type
 {Layout of the BCM2838 Mailbox Framebuffer request} {This structure must be 16 byte aligned when passed to the GPU}
 PBCM2838MailboxFramebuffer = ^TBCM2838MailboxFramebuffer;
 TBCM2838MailboxFramebuffer = record
  PhysicalWidth:LongWord;  {Requested width of Physical Framebuffer}
  PhysicalHeight:LongWord; {Requested height of Physical Framebuffer}
  VirtualWidth:LongWord;   {Requested width of Virtual Display}
  VirtualHeight:LongWord;  {Requested height of Virtual Display}
  Pitch:LongWord;           {Zero on request, Number of Bytes per Row in response}
  Depth:LongWord;           {Requested Colour Depth in Bits per Pixel}
  OffsetX:LongWord;           {Requested X offset of Virtual Framebuffer}
  OffsetY:LongWord;           {Requested Y offset of Virtual Framebuffer}
  Address:LongWord;           {Framebuffer address (Zero on request, Failure if zero in response)}
  Size:LongWord;           {Framebuffer size (Zero on request, Size in bytes in response)}
 end;

type
 {Layout of the BCM2838 Mailbox Property tags} {These structures must be 16 byte aligned when passed to the GPU}
 {Header}
 PBCM2838MailboxHeader = ^TBCM2838MailboxHeader;
 TBCM2838MailboxHeader = record
  Size:LongWord;  {Total buffer size in bytes (including the header values, the end tag and padding)}
  Code:LongWord;  {Request/response code }
 end;

 {Footer}
 PBCM2838MailboxFooter = ^TBCM2838MailboxFooter;
 TBCM2838MailboxFooter = record
  Tag:LongWord;   {BCM2838_MBOX_TAG_END}
 end;

 {Tag Header}
 PBCM2838MailboxTagHeader = ^TBCM2838MailboxTagHeader;
 TBCM2838MailboxTagHeader = record
  Tag:LongWord;     {Tag identifier}
  Size:LongWord;    {Value buffer size in bytes}
  Length:LongWord;  {1 bit (MSB) request/response indicator (0=request, 1=response), 31 bits (LSB) value length in bytes}
 end;

 {Tag No Request}
 PBCM2838MailboxTagNoRequest = ^TBCM2838MailboxTagNoRequest;
 TBCM2838MailboxTagNoRequest = record
  {Nothing}
 end;

 {Tag No Response}
 PBCM2838MailboxTagNoResponse = ^TBCM2838MailboxTagNoResponse;
 TBCM2838MailboxTagNoResponse = record
  {Nothing}
 end;

 {Get Firmware Revision}
 TBCM2838MailboxTagFirmwareRevisionResponse = record
  Revision:LongWord;
 end;

 PBCM2838MailboxTagGetFirmwareRevision = ^TBCM2838MailboxTagGetFirmwareRevision;
 TBCM2838MailboxTagGetFirmwareRevision = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagFirmwareRevisionResponse);
 end;

 {Get Board Model}
 TBCM2838MailboxTagBoardModelResponse = record
  Model:LongWord;
 end;

 PBCM2838MailboxTagGetBoardModel = ^TBCM2838MailboxTagGetBoardModel;
 TBCM2838MailboxTagGetBoardModel = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagBoardModelResponse);
 end;

 {Get Board Revision}
 TBCM2838MailboxTagBoardRevisionResponse = record
  Revision:LongWord;
 end;

 PBCM2838MailboxTagGetBoardRevision = ^TBCM2838MailboxTagGetBoardRevision;
 TBCM2838MailboxTagGetBoardRevision = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagBoardRevisionResponse);
 end;

 {Get MAC Address}
 TBCM2838MailboxTagMACAddressResponse = record
  MAC:array[0..5] of Byte; {MAC address in network byte order}
  Padding:Word;
 end;

 PBCM2838MailboxTagGetMACAddress = ^TBCM2838MailboxTagGetMACAddress;
 TBCM2838MailboxTagGetMACAddress = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagMACAddressResponse);
 end;

 {Get Board Serial}
 TBCM2838MailboxTagBoardSerialResponse = record
  Serial:Int64;
 end;

 PBCM2838MailboxTagGetBoardSerial = ^TBCM2838MailboxTagGetBoardSerial;
 TBCM2838MailboxTagGetBoardSerial = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagBoardSerialResponse);
 end;

 {Get ARM Memory}
 TBCM2838MailboxTagARMMemoryResponse = record
  Address:LongWord; {Base address in bytes}
  Size:LongWord;    {Size in bytes}
 end;

 PBCM2838MailboxTagGetARMMemory = ^TBCM2838MailboxTagGetARMMemory;
 TBCM2838MailboxTagGetARMMemory = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagARMMemoryResponse);
 end;

 {Get VC Memory}
 TBCM2838MailboxTagVCMemoryResponse = record
  Address:LongWord; {Base address in bytes}
  Size:LongWord;    {Size in bytes}
 end;

 PBCM2838MailboxTagGetVCMemory = ^TBCM2838MailboxTagGetVCMemory;
 TBCM2838MailboxTagGetVCMemory = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagVCMemoryResponse);
 end;

 {Get Clocks}
 TBCM2838MailboxTagClockResponse = record
  ParentId:LongWord;
  ClockId:LongWord;
 end;

 TBCM2838MailboxTagClocksResponse = record
  Clocks:array[0..255] of TBCM2838MailboxTagClockResponse;
 end;

 PBCM2838MailboxTagGetClocks = ^TBCM2838MailboxTagGetClocks;
 TBCM2838MailboxTagGetClocks = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagClocksResponse);
 end;

 {Get Power State}
 TBCM2838MailboxTagGetPowerStateRequest = record
  DeviceId:LongWord;
 end;

 TBCM2838MailboxTagPowerStateResponse = record
  DeviceId:LongWord;
  State:LongWord;
 end;

 PBCM2838MailboxTagGetPowerState = ^TBCM2838MailboxTagGetPowerState;
 TBCM2838MailboxTagGetPowerState = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetPowerStateRequest);
  1:(Response:TBCM2838MailboxTagPowerStateResponse);
 end;

 {Get Timing}
 TBCM2838MailboxTagTimingRequest = record
  DeviceId:LongWord;
 end;

 TBCM2838MailboxTagTimingResponse = record
  DeviceId:LongWord;
  Wait:LongWord;      {Enable wait time in microseconds}
 end;

 PBCM2838MailboxTagGetTiming = ^TBCM2838MailboxTagGetTiming;
 TBCM2838MailboxTagGetTiming = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagTimingRequest);
  1:(Response:TBCM2838MailboxTagTimingResponse);
 end;

 {Set Power State}
 TBCM2838MailboxTagSetPowerStateRequest = record
  DeviceId:LongWord;
  State:LongWord;
 end;

 PBCM2838MailboxTagSetPowerState = ^TBCM2838MailboxTagSetPowerState;
 TBCM2838MailboxTagSetPowerState = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetPowerStateRequest);
  1:(Response:TBCM2838MailboxTagPowerStateResponse);
 end;

 {Get Clock State}
 TBCM2838MailboxTagGetClockStateRequest = record
  ClockId:LongWord;
 end;

 TBCM2838MailboxTagClockStateResponse = record
  ClockId:LongWord;
  State:LongWord;
 end;

 PBCM2838MailboxTagGetClockState = ^TBCM2838MailboxTagGetClockState;
 TBCM2838MailboxTagGetClockState = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetClockStateRequest);
  1:(Response:TBCM2838MailboxTagClockStateResponse);
 end;

 {Set Clock State}
 TBCM2838MailboxTagSetClockStateRequest = record
  ClockId:LongWord;
  State:LongWord;
 end;

 PBCM2838MailboxTagSetClockState = ^TBCM2838MailboxTagSetClockState;
 TBCM2838MailboxTagSetClockState = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetClockStateRequest);
  1:(Response:TBCM2838MailboxTagClockStateResponse);
 end;

 {Get Clock Rate}
 TBCM2838MailboxTagGetClockRateRequest = record
  ClockId:LongWord;
 end;

 TBCM2838MailboxTagClockRateResponse = record
  ClockId:LongWord;
  Rate:LongWord; {In Hz}
 end;

 PBCM2838MailboxTagGetClockRate = ^TBCM2838MailboxTagGetClockRate;
 TBCM2838MailboxTagGetClockRate = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetClockRateRequest);
  1:(Response:TBCM2838MailboxTagClockRateResponse);
 end;

 {Set Clock Rate}
 TBCM2838MailboxTagSetClockRateRequest = record
  ClockId:LongWord;
  Rate:LongWord; {In Hz}
  SkipTurbo:LongWord;
 end;

 PBCM2838MailboxTagSetClockRate = ^TBCM2838MailboxTagSetClockRate;
 TBCM2838MailboxTagSetClockRate = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetClockRateRequest);
  1:(Response:TBCM2838MailboxTagClockRateResponse);
 end;

 {Get Clock Max Rate}
 TBCM2838MailboxTagGetClockMaxRateRequest = record
  ClockId:LongWord;
 end;

 TBCM2838MailboxTagGetClockMaxRateResponse = record
  ClockId:LongWord;
  Rate:LongWord; {In Hz}
 end;

 PBCM2838MailboxTagGetClockMaxRate = ^TBCM2838MailboxTagGetClockMaxRate;
 TBCM2838MailboxTagGetClockMaxRate = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetClockMaxRateRequest);
  1:(Response:TBCM2838MailboxTagGetClockMaxRateResponse);
 end;

 {Get Clock Min Rate}
 PBCM2838MailboxTagGetClockMinRate = ^TBCM2838MailboxTagGetClockMinRate;
 TBCM2838MailboxTagGetClockMinRate = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetClockMaxRateRequest);
  1:(Response:TBCM2838MailboxTagGetClockMaxRateResponse);
 end;

 {Get Clock Measured Rate}
 PBCM2838MailboxTagGetClockMeasuredRate = ^TBCM2838MailboxTagGetClockMeasuredRate;
 TBCM2838MailboxTagGetClockMeasuredRate = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetClockRateRequest);
  1:(Response:TBCM2838MailboxTagClockRateResponse);
 end;

 {Get Turbo}
 TBCM2838MailboxTagGetTurboRequest = record
  Id:LongWord;
 end;

 TBCM2838MailboxTagTurboResponse = record
  Id:LongWord;
  Level:LongWord;
 end;

 PBCM2838MailboxTagGetTurbo = ^TBCM2838MailboxTagGetTurbo;
 TBCM2838MailboxTagGetTurbo = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetTurboRequest);
  1:(Response:TBCM2838MailboxTagTurboResponse);
 end;

 {Set Turbo}
 TBCM2838MailboxTagSetTurboRequest = record
  Id:LongWord;
  Level:LongWord;
 end;

 PBCM2838MailboxTagSetTurbo = ^TBCM2838MailboxTagSetTurbo;
 TBCM2838MailboxTagSetTurbo = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetTurboRequest);
  1:(Response:TBCM2838MailboxTagTurboResponse);
 end;

 {Get Voltage}
 TBCM2838MailboxTagGetVoltageRequest = record
  VoltageId:LongWord;
 end;

 TBCM2838MailboxTagVoltageResponse = record
  VoltageId:LongWord;
  Value:LongWord;     {Offset from 1.2V in units of 0.025V}
 end;

 PBCM2838MailboxTagGetVoltage = ^TBCM2838MailboxTagGetVoltage;
 TBCM2838MailboxTagGetVoltage = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetVoltageRequest);
  1:(Response:TBCM2838MailboxTagVoltageResponse);
 end;

 {Set Voltage}
 TBCM2838MailboxTagSetVoltageRequest = record
  VoltageId:LongWord;
  Value:LongWord;     {Offset from 1.2V in units of 0.025V}
 end;

 PBCM2838MailboxTagSetVoltage = ^TBCM2838MailboxTagSetVoltage;
 TBCM2838MailboxTagSetVoltage = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetVoltageRequest);
  1:(Response:TBCM2838MailboxTagVoltageResponse);
 end;

 {Get Max Voltage}
 PBCM2838MailboxTagGetMaxVoltage = ^TBCM2838MailboxTagGetMaxVoltage;
 TBCM2838MailboxTagGetMaxVoltage = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetVoltageRequest);
  1:(Response:TBCM2838MailboxTagVoltageResponse);
 end;

 {Get Min Voltage}
 PBCM2838MailboxTagGetMinVoltage = ^TBCM2838MailboxTagGetMinVoltage;
 TBCM2838MailboxTagGetMinVoltage = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetVoltageRequest);
  1:(Response:TBCM2838MailboxTagVoltageResponse);
 end;

 {Get Temperature}
 TBCM2838MailboxTagTemperatureRequest = record
  TemperatureId:LongWord;
 end;

 TBCM2838MailboxTagTemperatureResponse = record
  TemperatureId:LongWord; {Should be zero}
  Temperature:LongWord;   {Return the temperature of the SoC in thousandths of a degree C}
 end;

 PBCM2838MailboxTagGetTemperature = ^TBCM2838MailboxTagGetTemperature;
 TBCM2838MailboxTagGetTemperature = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagTemperatureRequest);
  1:(Response:TBCM2838MailboxTagTemperatureResponse);
 end;

 {Get Max Temp}
 PBCM2838MailboxTagGetMaxTemperature = ^TBCM2838MailboxTagGetMaxTemperature;
 TBCM2838MailboxTagGetMaxTemperature = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagTemperatureRequest);
  1:(Response:TBCM2838MailboxTagTemperatureResponse);
 end;

 {Allocate Memory}
 TBCM2838MailboxTagAllocateMemoryRequest = record
  Size:LongWord;
  Alignment:LongWord;
  Flags:LongWord;
 end;

 TBCM2838MailboxTagAllocateMemoryResponse = record
  Handle:THandle;
 end;

 PBCM2838MailboxTagAllocateMemory = ^TBCM2838MailboxTagAllocateMemory;
 TBCM2838MailboxTagAllocateMemory = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagAllocateMemoryRequest);
  1:(Response:TBCM2838MailboxTagAllocateMemoryResponse);
 end;

 {Lock Memory}
 TBCM2838MailboxTagLockMemoryRequest = record
  Handle:THandle;
 end;

 TBCM2838MailboxTagLockMemoryResponse = record
  Address:LongWord; {Bus Address}
 end;

 PBCM2838MailboxTagLockMemory = ^TBCM2838MailboxTagLockMemory;
 TBCM2838MailboxTagLockMemory = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagLockMemoryRequest);
  1:(Response:TBCM2838MailboxTagLockMemoryResponse);
 end;

 {Unlock Memory}
 TBCM2838MailboxTagUnlockMemoryResponse = record
  Status:LongWord; {0 is Success}
 end;

 PBCM2838MailboxTagUnlockMemory = ^TBCM2838MailboxTagUnlockMemory;
 TBCM2838MailboxTagUnlockMemory = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagLockMemoryRequest);
  1:(Response:TBCM2838MailboxTagUnlockMemoryResponse);
 end;

 {Release Memory}
 PBCM2838MailboxTagReleaseMemory = ^TBCM2838MailboxTagReleaseMemory;
 TBCM2838MailboxTagReleaseMemory = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagLockMemoryRequest);
  1:(Response:TBCM2838MailboxTagUnlockMemoryResponse);
 end;

 {Execute Code}
 TBCM2838MailboxTagExecuteCodeRequest = record
  Address:Pointer; {Bus Address}
  R0:LongWord;
  R1:LongWord;
  R2:LongWord;
  R3:LongWord;
  R4:LongWord;
  R5:LongWord;
 end;

 TBCM2838MailboxTagExecuteCodeResponse = record
  R0:LongWord;
 end;

 PBCM2838MailboxTagExecuteCode = ^TBCM2838MailboxTagExecuteCode;
 TBCM2838MailboxTagExecuteCode = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagExecuteCodeRequest);
  1:(Response:TBCM2838MailboxTagExecuteCodeResponse);
 end;

 {Execute QPU}
 TBCM2838MailboxTagExecuteQPURequest = record
  NumQPUs:LongWord;
  Control:LongWord;
  NoFlush:LongWord;
  Timeout:LongWord; {Milliseconds}
 end;

 TBCM2838MailboxTagExecuteQPUResponse = record
  Status:LongWord; {0 is Success / 0x80000000 is Timeout}
 end;

 PBCM2838MailboxTagExecuteQPU = ^TBCM2838MailboxTagExecuteQPU;
 TBCM2838MailboxTagExecuteQPU = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagExecuteQPURequest);
  1:(Response:TBCM2838MailboxTagExecuteQPUResponse);
 end;

 {Enable QPU}
 TBCM2838MailboxTagEnableQPURequest = record
  Enable:LongWord;
 end;

 TBCM2838MailboxTagEnableQPUResponse = record
  Status:LongWord;  {0 is Success}
 end;

 PBCM2838MailboxTagEnableQPU = ^TBCM2838MailboxTagEnableQPU;
 TBCM2838MailboxTagEnableQPU = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagEnableQPURequest);
  1:(Response:TBCM2838MailboxTagEnableQPUResponse);
 end;

 {Get Dispmanx Handle}
 TBCM2838MailboxTagGetDispmanxHandleRequest = record
  Resource:THandle;
 end;

 TBCM2838MailboxTagGetDispmanxHandleResponse = record
  Status:LongWord; {0 is Success}
  Memory:THandle;
 end;

 PBCM2838MailboxTagGetDispmanxHandle = ^TBCM2838MailboxTagGetDispmanxHandle;
 TBCM2838MailboxTagGetDispmanxHandle = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetDispmanxHandleRequest);
  1:(Response:TBCM2838MailboxTagGetDispmanxHandleResponse);
 end;

 {Get EDID Block}
 TBCM2838MailboxTagGetEDIDBlockRequest = record
  Block:LongWord; {Starting from 0}
 end;

 TBCM2838MailboxTagGetEDIDBlockResponse = record
  Block:LongWord; {Starting from 0}
  Status:LongWord; {0 is Success}
  EDID:array[0..127] of Byte;
 end;

 PBCM2838MailboxTagGetEDIDBlock = ^TBCM2838MailboxTagGetEDIDBlock;
 TBCM2838MailboxTagGetEDIDBlock = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetEDIDBlockRequest);
  1:(Response:TBCM2838MailboxTagGetEDIDBlockResponse);
 end;

 {Get GPIO State}
 TBCM2838MailboxTagGPIOStateRequest = record
  GPIO:LongWord;
  State:LongWord;
 end;

 TBCM2838MailboxTagGPIOStateResponse = record
  GPIO:LongWord;
  State:LongWord;
 end;

 PBCM2838MailboxTagGetGPIOState = ^TBCM2838MailboxTagGetGPIOState;
 TBCM2838MailboxTagGetGPIOState = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGPIOStateRequest);
  1:(Response:TBCM2838MailboxTagGPIOStateResponse);
 end;

 {Set GPIO State}
 PBCM2838MailboxTagSetGPIOState = ^TBCM2838MailboxTagSetGPIOState;
 TBCM2838MailboxTagSetGPIOState = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGPIOStateRequest);
  1:(Response:TBCM2838MailboxTagGPIOStateResponse);
 end;

 {Get GPIO Config}
 TBCM2838MailboxTagGetGPIOConfigRequest = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
 end;

 TBCM2838MailboxTagGetGPIOConfigResponse = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
 end;

 PBCM2838MailboxTagGetGPIOConfig = ^TBCM2838MailboxTagGetGPIOConfig;
 TBCM2838MailboxTagGetGPIOConfig = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetGPIOConfigRequest);
  1:(Response:TBCM2838MailboxTagGetGPIOConfigResponse);
 end;

 {Set GPIO Config}
 TBCM2838MailboxTagSetGPIOConfigRequest = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
  State:LongWord;
 end;

 TBCM2838MailboxTagSetGPIOConfigResponse = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
  State:LongWord;
 end;

 PBCM2838MailboxTagSetGPIOConfig = ^TBCM2838MailboxTagSetGPIOConfig;
 TBCM2838MailboxTagSetGPIOConfig = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetGPIOConfigRequest);
  1:(Response:TBCM2838MailboxTagSetGPIOConfigResponse);
 end;

 {Get Throttled}
 TBCM2838MailboxTagGetThrottledRequest = record
  Value:LongWord;
 end;

 TBCM2838MailboxTagGetThrottledResponse = record
  Value:LongWord;
 end;

 PBCM2838MailboxTagGetThrottled = ^TBCM2838MailboxTagGetThrottled;
 TBCM2838MailboxTagGetThrottled = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetThrottledRequest);
  1:(Response:TBCM2838MailboxTagGetThrottledResponse);
 end;

 {Get Boot Mode}
 TBCM2838MailboxTagGetBootModeResponse = record
  BootMode:LongWord;
  SignedBoot:LongWord;
 end;

 PBCM2838MailboxTagGetBootMode = ^TBCM2838MailboxTagGetBootMode;
 TBCM2838MailboxTagGetBootMode = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagGetBootModeResponse);
 end;

 {Allocate Buffer}
 TBCM2838MailboxTagAllocateBufferRequest = record
  Alignment:LongWord; {Bytes}
 end;

 TBCM2838MailboxTagAllocateBufferResponse = record
  Address:LongWord; {Base Address in Bytes}
  Size:LongWord;    {Size in Bytes}
 end;

 PBCM2838MailboxTagAllocateBuffer = ^TBCM2838MailboxTagAllocateBuffer;
 TBCM2838MailboxTagAllocateBuffer = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagAllocateBufferRequest);
  1:(Response:TBCM2838MailboxTagAllocateBufferResponse);
 end;

 {Release Buffer}
 PBCM2838MailboxTagReleaseBuffer = ^TBCM2838MailboxTagReleaseBuffer;
 TBCM2838MailboxTagReleaseBuffer = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagNoResponse);
 end;

 {Blank Screen}
 TBCM2838MailboxTagBlankScreenRequest = record
  State:LongWord;
 end;

 TBCM2838MailboxTagBlankScreenResponse = record
  State:LongWord;
 end;

 PBCM2838MailboxTagBlankScreen = ^TBCM2838MailboxTagBlankScreen;
 TBCM2838MailboxTagBlankScreen = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagBlankScreenRequest);
  1:(Response:TBCM2838MailboxTagBlankScreenResponse);
 end;

 {Get Physical}
 TBCM2838MailboxTagPhysicalRequest = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 TBCM2838MailboxTagPhysicalResponse = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 PBCM2838MailboxTagGetPhysical = ^TBCM2838MailboxTagGetPhysical;
 TBCM2838MailboxTagGetPhysical = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagPhysicalResponse);
 end;

 {Test Physical}
 PBCM2838MailboxTagTestPhysical = ^TBCM2838MailboxTagTestPhysical;
 TBCM2838MailboxTagTestPhysical = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagPhysicalRequest);
  1:(Response:TBCM2838MailboxTagPhysicalResponse);
 end;

 {Set Physical}
 PBCM2838MailboxTagSetPhysical = ^TBCM2838MailboxTagSetPhysical;
 TBCM2838MailboxTagSetPhysical = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagPhysicalRequest);
  1:(Response:TBCM2838MailboxTagPhysicalResponse);
 end;

 {Get Virtual}
 TBCM2838MailboxTagVirtualRequest = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 TBCM2838MailboxTagVirtualResponse = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 PBCM2838MailboxTagGetVirtual = ^TBCM2838MailboxTagGetVirtual;
 TBCM2838MailboxTagGetVirtual = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagVirtualResponse);
 end;

 {Test Virtual}
 PBCM2838MailboxTagTestVirtual = ^TBCM2838MailboxTagTestVirtual;
 TBCM2838MailboxTagTestVirtual = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagVirtualRequest);
  1:(Response:TBCM2838MailboxTagVirtualResponse);
 end;

 {Set Virtual}
 PBCM2838MailboxTagSetVirtual = ^TBCM2838MailboxTagSetVirtual;
 TBCM2838MailboxTagSetVirtual = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagVirtualRequest);
  1:(Response:TBCM2838MailboxTagVirtualResponse);
 end;

 {Get Depth}
 TBCM2838MailboxTagDepthRequest = record
  Depth:LongWord;   {Bits per pixel}
 end;

 TBCM2838MailboxTagDepthResponse = record
  Depth:LongWord;   {Bits per pixel}
 end;

 PBCM2838MailboxTagGetDepth = ^TBCM2838MailboxTagGetDepth;
 TBCM2838MailboxTagGetDepth = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagDepthResponse);
 end;

 {Test Depth}
 PBCM2838MailboxTagTestDepth = ^TBCM2838MailboxTagTestDepth;
 TBCM2838MailboxTagTestDepth = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagDepthRequest);
  1:(Response:TBCM2838MailboxTagDepthResponse);
 end;

 {Set Depth}
 PBCM2838MailboxTagSetDepth = ^TBCM2838MailboxTagSetDepth;
 TBCM2838MailboxTagSetDepth = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagDepthRequest);
  1:(Response:TBCM2838MailboxTagDepthResponse);
 end;

 {Get Pixel Order}
 TBCM2838MailboxTagPixelOrderRequest = record
  Order:LongWord;
 end;

 TBCM2838MailboxTagPixelOrderResponse = record
  Order:LongWord;
 end;

 PBCM2838MailboxTagGetPixelOrder = ^TBCM2838MailboxTagGetPixelOrder;
 TBCM2838MailboxTagGetPixelOrder = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagPixelOrderResponse);
 end;

 {Test Pixel Order}
 PBCM2838MailboxTagTestPixelOrder = ^TBCM2838MailboxTagTestPixelOrder;
 TBCM2838MailboxTagTestPixelOrder = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagPixelOrderRequest);
  1:(Response:TBCM2838MailboxTagPixelOrderResponse);
 end;

 {Set Pixel Order}
 PBCM2838MailboxTagSetPixelOrder = ^TBCM2838MailboxTagSetPixelOrder;
 TBCM2838MailboxTagSetPixelOrder = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagPixelOrderRequest);
  1:(Response:TBCM2838MailboxTagPixelOrderResponse);
 end;

 {Get Alpha Mode}
 TBCM2838MailboxTagAlphaModeRequest = record
  Mode:LongWord;
 end;

 TBCM2838MailboxTagAlphaModeResponse = record
  Mode:LongWord;
 end;

 PBCM2838MailboxTagGetAlphaMode = ^TBCM2838MailboxTagGetAlphaMode;
 TBCM2838MailboxTagGetAlphaMode = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagAlphaModeResponse);
 end;

 {Test Alpha Mode}
 PBCM2838MailboxTagTestAlphaMode = ^TBCM2838MailboxTagTestAlphaMode;
 TBCM2838MailboxTagTestAlphaMode = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagAlphaModeRequest);
  1:(Response:TBCM2838MailboxTagAlphaModeResponse);
 end;

 {Set Alpha Mode}
 PBCM2838MailboxTagSetAlphaMode = ^TBCM2838MailboxTagSetAlphaMode;
 TBCM2838MailboxTagSetAlphaMode = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagAlphaModeRequest);
  1:(Response:TBCM2838MailboxTagAlphaModeResponse);
 end;

 {Get Pitch}
 TBCM2838MailboxTagPitchResponse = record
  Pitch:LongWord;  {Bytes per line}
 end;

 PBCM2838MailboxTagGetPitch = ^TBCM2838MailboxTagGetPitch;
 TBCM2838MailboxTagGetPitch = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagPitchResponse);
 end;

 {Get Virtual Offset}
 TBCM2838MailboxTagVirtualOffsetRequest = record
  X:LongWord; {Pixels}
  Y:LongWord; {Pixels}
 end;

 TBCM2838MailboxTagVirtualOffsetResponse = record
  X:LongWord; {Pixels}
  Y:LongWord; {Pixels}
 end;

 PBCM2838MailboxTagGetVirtualOffset = ^TBCM2838MailboxTagGetVirtualOffset;
 TBCM2838MailboxTagGetVirtualOffset = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagVirtualOffsetResponse);
 end;

 {Test Virtual Offset}
 PBCM2838MailboxTagTestVirtualOffset = ^TBCM2838MailboxTagTestVirtualOffset;
 TBCM2838MailboxTagTestVirtualOffset = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagVirtualOffsetRequest);
  1:(Response:TBCM2838MailboxTagVirtualOffsetResponse);
 end;

 {Set Virtual Offset}
 PBCM2838MailboxTagSetVirtualOffset = ^TBCM2838MailboxTagSetVirtualOffset;
 TBCM2838MailboxTagSetVirtualOffset = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagVirtualOffsetRequest);
  1:(Response:TBCM2838MailboxTagVirtualOffsetResponse);
 end;

 {Get Overscan}
 TBCM2838MailboxTagOverscanRequest = record
  Top:LongWord;    {Pixels}
  Bottom:LongWord; {Pixels}
  Left:LongWord;   {Pixels}
  Right:LongWord;  {Pixels}
 end;

 TBCM2838MailboxTagOverscanResponse = record
  Top:LongWord;    {Pixels}
  Bottom:LongWord; {Pixels}
  Left:LongWord;   {Pixels}
  Right:LongWord;  {Pixels}
 end;

 PBCM2838MailboxTagGetOverscan = ^TBCM2838MailboxTagGetOverscan;
 TBCM2838MailboxTagGetOverscan = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagOverscanResponse);
 end;

 {Test Overscan}
 PBCM2838MailboxTagTestOverscan = ^TBCM2838MailboxTagTestOverscan;
 TBCM2838MailboxTagTestOverscan = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagOverscanRequest);
  1:(Response:TBCM2838MailboxTagOverscanResponse);
 end;

 {Set Overscan}
 PBCM2838MailboxTagSetOverscan = ^TBCM2838MailboxTagSetOverscan;
 TBCM2838MailboxTagSetOverscan = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagOverscanRequest);
  1:(Response:TBCM2838MailboxTagOverscanResponse);
 end;

 {Get Palette}
 TBCM2838MailboxTagGetPaletteResponse = record
  Values:array[0..255] of LongWord;    {RGBA Palette Values}
 end;

 PBCM2838MailboxTagGetPalette = ^TBCM2838MailboxTagGetPalette;
 TBCM2838MailboxTagGetPalette = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagGetPaletteResponse);
 end;

 {Test Palette}
 TBCM2838MailboxTagPaletteRequest = record
  Offset:LongWord; {First palette index to set (0-255)}
  Length:LongWord; {Number of palette entries to set (1-256)}
  Values:array[0..255] of LongWord;    {RGBA Palette Values}
 end;

 TBCM2838MailboxTagPaletteResponse = record
  Status:LongWord;
 end;

 PBCM2838MailboxTagTestPalette = ^TBCM2838MailboxTagTestPalette;
 TBCM2838MailboxTagTestPalette = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagPaletteRequest);
  1:(Response:TBCM2838MailboxTagPaletteResponse);
 end;

 {Set Palette}
 PBCM2838MailboxTagSetPalette = ^TBCM2838MailboxTagSetPalette;
 TBCM2838MailboxTagSetPalette = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagPaletteRequest);
  1:(Response:TBCM2838MailboxTagPaletteResponse);
 end;

 {Get Layer}
 TBCM2838MailboxTagLayerRequest = record
  Layer:LongInt;
 end;

 TBCM2838MailboxTagLayerResponse = record
  Layer:LongInt;
 end;

 PBCM2838MailboxTagGetLayer = ^TBCM2838MailboxTagGetLayer;
 TBCM2838MailboxTagGetLayer = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagLayerResponse);
 end;

 {Test Layer}
 PBCM2838MailboxTagTestLayer = ^TBCM2838MailboxTagTestLayer;
 TBCM2838MailboxTagTestLayer = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagLayerRequest);
  1:(Response:TBCM2838MailboxTagLayerResponse);
 end;

 {Set Layer}
 PBCM2838MailboxTagSetLayer = ^TBCM2838MailboxTagSetLayer;
 TBCM2838MailboxTagSetLayer = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagLayerRequest);
  1:(Response:TBCM2838MailboxTagLayerResponse);
 end;

 {Get Touch Buffer}
 TBCM2838MailboxTagGetTouchResponse = record
  Address:LongWord;
 end;

 PBCM2838MailboxTagGetTouch = ^TBCM2838MailboxTagGetTouch;
 TBCM2838MailboxTagGetTouch = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagGetTouchResponse);
 end;

 {Set Touch Buffer}
 TBCM2838MailboxTagSetTouchRequest = record
  Address:LongWord;
 end;

 TBCM2838MailboxTagSetTouchResponse = record
  Status:LongWord;
 end;

 PBCM2838MailboxTagSetTouch = ^TBCM2838MailboxTagSetTouch;
 TBCM2838MailboxTagSetTouch = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetTouchRequest);
  1:(Response:TBCM2838MailboxTagSetTouchResponse);
 end;

 {Get Virtual GPIO Buffer}
 TBCM2838MailboxTagGetVirtualGPIOResponse = record
  Address:LongWord;
 end;

 PBCM2838MailboxTagGetVirtualGPIO = ^TBCM2838MailboxTagGetVirtualGPIO;
 TBCM2838MailboxTagGetVirtualGPIO = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagGetVirtualGPIOResponse);
 end;

 {Set Virtual GPIO Buffer}
 TBCM2838MailboxTagSetVirtualGPIORequest = record
  Address:LongWord;
 end;

 TBCM2876MailboxTagSetVirtualGPIOResponse = record
  Status:LongWord;
 end;

 PBCM2838MailboxTagSetVirtualGPIO = ^TBCM2838MailboxTagSetVirtualGPIO;
 TBCM2838MailboxTagSetVirtualGPIO = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetVirtualGPIORequest);
  1:(Response:TBCM2876MailboxTagSetVirtualGPIOResponse);
 end;

 {Test Vsync}
 PBCM2838MailboxTagTestVsync = ^TBCM2838MailboxTagTestVsync;
 TBCM2838MailboxTagTestVsync = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagNoResponse);
 end;

 {Set Vsync}
 PBCM2838MailboxTagSetVsync = ^TBCM2838MailboxTagSetVsync;
 TBCM2838MailboxTagSetVsync = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagNoResponse);
 end;

 {Set Backlight}
 TBCM2838MailboxTagSetBacklightRequest = record
  Brightness:LongWord;
 end;

 TBCM2838MailboxTagSetBacklightResponse = record
  Brightness:LongWord;
 end;

 PBCM2838MailboxTagSetBacklight = ^TBCM2838MailboxTagSetBacklight;
 TBCM2838MailboxTagSetBacklight = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetBacklightRequest);
  1:(Response:TBCM2838MailboxTagSetBacklightResponse);
 end;

 {Get Display Id}
 TBCM2838MailboxTagGetDisplayIdRequest = record
  DisplayNum:LongWord;
 end;

 TBCM2838MailboxTagGetDisplayIdResponse = record
  DisplayId:LongWord;
 end;

 PBCM2838MailboxTagGetDisplayId = ^TBCM2838MailboxTagGetDisplayId;
 TBCM2838MailboxTagGetDisplayId = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetDisplayIdRequest);
  1:(Response:TBCM2838MailboxTagGetDisplayIdResponse);
 end;

 {Set Display Num}
 TBCM2838MailboxTagSetDisplayNumRequest = record
  DisplayNum:LongWord;
 end;

 TBCM2838MailboxTagSetDisplayNumResponse = record
  DisplayNum:LongWord;
 end;

 PBCM2838MailboxTagSetDisplayNum = ^TBCM2838MailboxTagSetDisplayNum;
 TBCM2838MailboxTagSetDisplayNum = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetDisplayNumRequest);
  1:(Response:TBCM2838MailboxTagSetDisplayNumResponse);
 end;

 {Get Num Displays}
 TBCM2838MailboxTagGetNumDisplaysRequest = record
  NumDisplays:LongWord;
 end;

 TBCM2838MailboxTagGetNumDisplaysResponse = record
  NumDisplays:LongWord;
 end;

 PBCM2838MailboxTagGetNumDisplays = ^TBCM2838MailboxTagGetNumDisplays;
 TBCM2838MailboxTagGetNumDisplays = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetNumDisplaysRequest);
  1:(Response:TBCM2838MailboxTagGetNumDisplaysResponse);
 end;

 {Get Display Settings}
 TBCM2838MailboxTagGetDisplaySettingsRequest = record
  DisplayNum:LongWord;
  Width:LongWord;
  Height:LongWord;
  Depth:LongWord;
  Pitch:LongWord;
  VirtualWidth:LongWord;
  VirtualHeight:LongWord;
  VirtualWidthOffset:LongWord;
  VirtualHeightOffset:LongWord;
  BusAddress:LongWord;
 end;

 TBCM2838MailboxTagGetDisplaySettingsResponse = record
  DisplayNum:LongWord;
  Width:LongWord;
  Height:LongWord;
  Depth:LongWord;
  Pitch:LongWord;
  VirtualWidth:LongWord;
  VirtualHeight:LongWord;
  VirtualWidthOffset:LongWord;
  VirtualHeightOffset:LongWord;
  BusAddress:LongWord;
 end;

 PBCM2838MailboxTagGetDisplaySettings = ^TBCM2838MailboxTagGetDisplaySettings;
 TBCM2838MailboxTagGetDisplaySettings = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagGetDisplaySettingsRequest);
  1:(Response:TBCM2838MailboxTagGetDisplaySettingsResponse);
 end;

 {Set Cursor Info}
 TBCM2838MailboxTagSetCursorInfoRequest = record
  Width:LongWord;    {Pixels}
  Height:LongWord;   {Pixels}
  Reserved:LongWord;
  Pixels:Pointer;    {Format is 32bpp (ARGB) (Width and Height should be >= 16 and (Width * Height) <= 64)}
  HotspotX:LongWord;
  HotspotY:LongWord;
 end;

 TBCM2838MailboxTagCursorResponse = record
  Status:LongWord;
 end;

 PBCM2838MailboxTagSetCursorInfo = ^TBCM2838MailboxTagSetCursorInfo;
 TBCM2838MailboxTagSetCursorInfo = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetCursorInfoRequest);
  1:(Response:TBCM2838MailboxTagCursorResponse);
 end;

 {Set Cursor State}
 TBCM2838MailboxTagSetCursorStateRequest = record
  Enable:LongWord;
  X:LongWord;        {Pixels}
  Y:LongWord;        {Pixels}
  Flags:LongWord;
 end;

 PBCM2838MailboxTagSetCursorState = ^TBCM2838MailboxTagSetCursorState;
 TBCM2838MailboxTagSetCursorState = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagSetCursorStateRequest);
  1:(Response:TBCM2838MailboxTagCursorResponse);
 end;

 {VCHIQ Init}
 TBCM2838MailboxTagVCHIQInitRequest = record
  Address:LongWord;
 end;

 TBCM2838MailboxTagVCHIQInitResponse = record
  Status:LongWord;  {0 is Success}
 end;

 PBCM2838MailboxTagVCHIQInit = ^TBCM2838MailboxTagVCHIQInit;
 TBCM2838MailboxTagVCHIQInit = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagVCHIQInitRequest);
  1:(Response:TBCM2838MailboxTagVCHIQInitResponse);
 end;

 {Get Command Line}
 TBCM2838MailboxTagCommandLineResponse = record
  CommandLine:array[0..1023] of Char;
 end;

 PBCM2838MailboxTagGetCommandLine = ^TBCM2838MailboxTagGetCommandLine;
 TBCM2838MailboxTagGetCommandLine = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagCommandLineResponse);
 end;

 {Get DMA Channels}
 TBCM2838MailboxTagDMAChannelsResponse = record
  Channels:LongWord;
 end;

 PBCM2838MailboxTagGetDMAChannels = ^TBCM2838MailboxTagGetDMAChannels;
 TBCM2838MailboxTagGetDMAChannels = record
  Header:TBCM2838MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2838MailboxTagNoRequest);
  1:(Response:TBCM2838MailboxTagDMAChannelsResponse);
 end;

 {Create Buffer (A combination tag to allocate and configure a framebuffer in one request)}
 PBCM2838MailboxTagCreateBuffer = ^TBCM2838MailboxTagCreateBuffer;
 TBCM2838MailboxTagCreateBuffer = record
  Physical:TBCM2838MailboxTagSetPhysical;
  Vertual:TBCM2838MailboxTagSetVirtual;
  Depth:TBCM2838MailboxTagSetDepth;
  Order:TBCM2838MailboxTagSetPixelOrder;
  Mode:TBCM2838MailboxTagSetAlphaMode;
  Offset:TBCM2838MailboxTagSetVirtualOffset;
  Overscan:TBCM2838MailboxTagSetOverscan;
  Allocate:TBCM2838MailboxTagAllocateBuffer;
  Pitch:TBCM2838MailboxTagGetPitch;
 end;

 {Query Buffer (A combination tag to query all framebuffer properties in one request)}
 PBCM2838MailboxTagQueryBuffer = ^TBCM2838MailboxTagQueryBuffer;
 TBCM2838MailboxTagQueryBuffer = record
  Physical:TBCM2838MailboxTagGetPhysical;
  Vertual:TBCM2838MailboxTagGetVirtual;
  Depth:TBCM2838MailboxTagGetDepth;
  Order:TBCM2838MailboxTagGetPixelOrder;
  Mode:TBCM2838MailboxTagGetAlphaMode;
  Offset:TBCM2838MailboxTagGetVirtualOffset;
  Overscan:TBCM2838MailboxTagGetOverscan;
  Pitch:TBCM2838MailboxTagGetPitch;
 end;

{==============================================================================}
type
 {BCM2838 ARM local structures (See: BCM2711 ARM Peripherals)}
 PBCM2838ARMLocalMailboxWriteRegisters = ^TBCM2838ARMLocalMailboxWriteRegisters;
 TBCM2838ARMLocalMailboxWriteRegisters = record
  Mailbox0Write:LongWord; {Mailbox 0 write-set (WO)}
  Mailbox1Write:LongWord; {Mailbox 1 write-set (WO)}
  Mailbox2Write:LongWord; {Mailbox 2 write-set (WO)}
  Mailbox3Write:LongWord; {Mailbox 3 write-set (WO)}
 end;

 PBCM2838ARMLocalMailboxReadClearRegisters = ^TBCM2838ARMLocalMailboxReadClearRegisters;
 TBCM2838ARMLocalMailboxReadClearRegisters = record
  Mailbox0ReadClear:LongWord; {Mailbox 0 read & write-high-to-clear}
  Mailbox1ReadClear:LongWord; {Mailbox 1 read & write-high-to-clear}
  Mailbox2ReadClear:LongWord; {Mailbox 2 read & write-high-to-clear}
  Mailbox3ReadClear:LongWord; {Mailbox 3 read & write-high-to-clear}
 end;

 PBCM2838ARMLocalRegisters = ^TBCM2838ARMLocalRegisters;
 TBCM2838ARMLocalRegisters = record
  Control:LongWord;                                                {Control register $0000}
  Reserved1:LongWord;                                              {Unused $0004}
  CoreTimerPrescaler:LongWord;                                     {Core timer prescaler $0008}
  CoreInterruptRouting:LongWord;                                   {Core Interrupts routing $000C}
  PMInterruptRoutingSet:LongWord;                                  {Performance Monitor Interrupts routing-set $0010}
  PMInterruptRoutingClear:LongWord;                                {Performance Monitor Interrupts routing-clear $0014}
  Reserved2:LongWord;                                              {Unused $0018}
  CoreTimerLow:LongWord;                                           {Core timer access LS 32 bits $001C}
  CoreTimerHigh:LongWord;                                          {Core timer access MS 32 bits $0020}
  PeripheralIntRouting0:LongWord;                                  {Peripheral Interrupts 0-7 routing (1 to 7 Unused) $0024}
  PeripheralIntRouting1:LongWord;                                  {Peripheral Interrupts 8-15 routing (Unused) $0028}
  AXIOutstandingCount:LongWord;                                    {AXI outstanding counters $002C}
  AXIOutstandingIRQ:LongWord;                                      {AXI outstanding IRQ $0030}
  LocalTimerControl:LongWord;                                      {Local timer control & status $0034}
  LocalTimerClearReload:LongWord;                                  {Local timer IRQ clear & reload $0038}
  Reserved3:LongWord;                                              {Unused $003C}
  TimersIntControl:array[0..BCM2838_CPU_COUNT - 1] of LongWord;    {Core0-3 Timers Interrupt control $0040-004C}
  MailboxIntControl:array[0..BCM2838_CPU_COUNT - 1] of LongWord;   {Core0-3 Mailboxes Interrupt control $0050-005C}
  IRQSource:array[0..BCM2838_CPU_COUNT - 1] of LongWord;           {Core0-3 IRQ Source $0060-006C}
  FIQSource:array[0..BCM2838_CPU_COUNT - 1] of LongWord;           {Core0-3 FIQ Source $0070-007C}
  MailboxWrite:array[0..BCM2838_CPU_COUNT - 1] of TBCM2838ARMLocalMailboxWriteRegisters;          {Core0-3 Mailbox 0-3 write-set (WO) $0080-00BC}
  MailboxReadClear:array[0..BCM2838_CPU_COUNT - 1] of TBCM2838ARMLocalMailboxReadClearRegisters;  {Core0-3 Mailbox 0-3 read & write-high-to-clear $00C0-00FC}
 end;

 PBCM2838VirtualGPIOBuffer = ^TBCM2838VirtualGPIOBuffer;
 TBCM2838VirtualGPIOBuffer = record
  Buffer:Pointer;
  Address:PtrUInt;
  CachedBuffer:LongBool;
  EnableDisable:array[0..BCM2838_VIRTUAL_GPIO_PIN_COUNT - 1] of LongWord; {Two packed 16-bit counts of enabled and disabled / Allows host to detect a brief enable that was missed}
 end;
{$PACKRECORDS DEFAULT}
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
