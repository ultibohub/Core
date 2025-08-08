{
Ultibo Definitions specific to the Broadcom 2835 System on chip.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)

Boards
======

 Raspberry Pi - Model A/B/A+/B+/CM1
 Raspberry Pi - Model Zero/ZeroW

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

 Linux - \arch\arm\mach-bcm2708\include\mach\platform.h - Copyright (C) Broadcom
 Linux - \arch\arm\mach-bcm2708\include\mach\irqs.h - Copyright (C) Broadcom
 Linux - \arch\arm\mach-bcm2708\include\mach\vcio.h - Copyright (C) Broadcom

References
==========

 BCM2835 ARM Peripherals

  https://datasheets.raspberrypi.com/bcm2835/bcm2835-peripherals.pdf

 Raspberry Pi Mailboxes

  https://github.com/raspberrypi/firmware/wiki/Mailboxes


Broadcom BCM2835
================

 Some of the information in this file is documented in the Broadcom BCM2835-ARM-Peripherals document, some can only be found in the Linux source.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit BCM2835;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {BCM2835 specific constants}
 BCM2835_CPU_COUNT = 1;

 {ARM Physical to VC IO Mapping (See: BCM2835-ARM-Peripherals.pdf)}
 BCM2835_VCIO_ALIAS    = $7E000000;

 {ARM Physical to VC Bus Mapping (See: BCM2835-ARM-Peripherals.pdf)}
 BCM2835_VCBUS_0_ALIAS = $00000000; {0 Alias - L1 and L2 cached}
 BCM2835_VCBUS_4_ALIAS = $40000000; {4 Alias - L2 cache coherent (non allocating)} {Suitable for RPi Model A/B/A+/B+ if disable_l2cache=0 in config.txt (Default)}
 BCM2835_VCBUS_8_ALIAS = $80000000; {8 Alias - L2 cached (only)}
 BCM2835_VCBUS_C_ALIAS = $C0000000; {C Alias - Direct uncached} {Suitable for RPi Model A/B/A+/B+ only if disable_l2cache=1 in config.txt}

 {Physical memory addresses of BCM2835 peripherals (See: BCM2835-ARM-Peripherals.pdf)}
 BCM2835_PERIPHERALS_BASE = $20000000;  {Mapped to VC address 7E000000}
 BCM2835_PERIPHERALS_SIZE = SIZE_16M;

 {Interrupt Controller 0}
 BCM2835_IC0_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $2000;

 {System Timer (See Section 12)}
 BCM2835_SYSTEM_TIMER_REGS_BASE = BCM2835_PERIPHERALS_BASE + $3000;

 {Message based Parallel Host Interface}
 BCM2835_MPHI_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $6000;

 {DMA controller (Channels 0 to 14) (See Section 4)}
 BCM2835_DMA0_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7000;
 BCM2835_DMA1_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7100;
 BCM2835_DMA2_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7200;
 BCM2835_DMA3_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7300;
 BCM2835_DMA4_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7400;
 BCM2835_DMA5_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7500;
 BCM2835_DMA6_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7600;
 BCM2835_DMA7_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7700;
 BCM2835_DMA8_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7800;
 BCM2835_DMA9_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $7900;
 BCM2835_DMA10_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $7A00;
 BCM2835_DMA11_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $7B00;
 BCM2835_DMA12_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $7C00;
 BCM2835_DMA13_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $7D00;
 BCM2835_DMA14_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $7E00;

 BCM2835_DMA_INT_STATUS_BASE    = BCM2835_PERIPHERALS_BASE + $7FE0;
 BCM2835_DMA_ENABLE_BASE        = BCM2835_PERIPHERALS_BASE + $7FF0;

 {ARM Interrupt Controller (See Section 7)}
 BCM2835_INTERRUPT_REGS_BASE    = BCM2835_PERIPHERALS_BASE + $B200; {Note: Broadcom states 0xB000 but the offsets begin at 0x200 so 0xB200 will be correct}

 {ARM Timer (See Section 14)}
 BCM2835_TIMER_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $B400; {Note: Broadcom states 0xB000 but the offsets begin at 0x400 so 0xB400 will be correct}

 {ARM Doorbell}
 BCM2835_DOORBELL_REGS_BASE     = BCM2835_PERIPHERALS_BASE + $B840;

 {ARM Mailbox 0}
 BCM2835_MAILBOX0_REGS_BASE     = BCM2835_PERIPHERALS_BASE + $B880;

 {ARM Mailbox 1}
 BCM2835_MAILBOX1_REGS_BASE     = BCM2835_PERIPHERALS_BASE + $B8A0;

 {Power Management, Reset controller and Watchdog}
 BCM2835_PM_REGS_BASE           = BCM2835_PERIPHERALS_BASE + $100000;

 {Clock Management}
 BCM2835_CM_REGS_BASE           = BCM2835_PERIPHERALS_BASE + $101000;

 {PCM Clock}
 BCM2835_PCM_CLOCK_BASE         = BCM2835_PERIPHERALS_BASE + $101098;

 {Random Number Generator}
 BCM2835_RNG_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $104000;

 {GPIO (See Section 6)}
 BCM2835_GPIO_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $200000;

 {UART0 (PL011) (See Section 13)}
 BCM2835_PL011_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $201000;

 {MMCI0}
 BCM2835_MMCI0_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $202000;
 BCM2835_SDHOST_REGS_BASE       = BCM2835_MMCI0_REGS_BASE;

 {PCM / I2S Audio (See Section 8)}
 BCM2835_PCM_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $203000;

 {SPI0 (See Section 10)}
 BCM2835_SPI0_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $204000;

 {BSC0 (I2C) (Broadcom Serial Controller)(See Section 3)}
 BCM2835_BSC0_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $205000;

 {Pixel Valve 0}
 BCM2835_PIXELVALVE0_REGS_BASE  = BCM2835_PERIPHERALS_BASE + $206000;

 {Pixel Valve 1}
 BCM2835_PIXELVALVE1_REGS_BASE  = BCM2835_PERIPHERALS_BASE + $207000;

 {DPI (Display Parallel Interface)}
 BCM2835_DPI_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $208000;

 {DSI0 (Display Serial Interface}
 BCM2835_DSI0_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $209000;

 {PWM (Pulse Width Modulator)(See Section 9)}
 BCM2835_PWM_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $20C000;

 {I2C/SPI Slave (See Section 11)}
 BCM2835_I2CSPI_REGS_BASE       = BCM2835_PERIPHERALS_BASE + $214000;

 {AUX (UART1, SPI1 and SPI2) (See Section 2)}
 BCM2835_AUX_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $215000;
 BCM2835_UART1_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $215040;
 BCM2835_SPI1_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $215080;
 BCM2835_SPI2_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $2150C0;

 {SD host controller (EMMC - External Mass Media Controller)(See Section 5)}
 BCM2835_SDHCI_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $300000;

 {HVS}
 BCM2835_HVS_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $400000;

 {SMI (Firmware KMS)}
 BCM2835_SMI_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $600000;

 {DSI1 (Display Serial Interface}
 BCM2835_DSI1_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $700000;

 {BSC1 (I2C) (Broadcom Serial Controller)(See Section 3)}
 BCM2835_BSC1_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $804000;

 {BSC2 (I2C) (Broadcom Serial Controller)(See Section 3)} {Note: BSC2 master is used dedicated with the HDMI interface and should not be used}
 BCM2835_BSC2_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $805000;

 {VEC}
 BCM2835_VEC_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $806000;

 {Pixel Valve 2}
 BCM2835_PIXELVALVE2_REGS_BASE  = BCM2835_PERIPHERALS_BASE + $807000;

 {HDMI}
 BCM2835_HDMI_REGS_BASE         = BCM2835_PERIPHERALS_BASE + $902000;

 {USB (Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller)(See Section 15)}
 BCM2835_USB_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $980000;

 {V3D}
 BCM2835_V3D_REGS_BASE          = BCM2835_PERIPHERALS_BASE + $C00000;

 {DMA controller (Channel 15 (See Section 4))}
 BCM2835_DMA15_REGS_BASE        = BCM2835_PERIPHERALS_BASE + $E05000;

const
 {IRQ lines of BCM2835 peripherals (IRQs 0-63 are those shared between the GPU and CPU, IRQs 64+ are CPU-specific)}
 {IRQs 0 to 31 appear in the IRQ_pending_1 register}
 {System Timer}
 BCM2835_IRQ_SYSTEM_TIMER_0 = 0; {Already used by the VideoCore GPU (Do not use)}
 BCM2835_IRQ_SYSTEM_TIMER_1 = 1;
 BCM2835_IRQ_SYSTEM_TIMER_2 = 2; {Already used by the VideoCore GPU (Do not use)}
 BCM2835_IRQ_SYSTEM_TIMER_3 = 3;

 {Codec}
 BCM2835_IRQ_CODEC0         = 4;
 BCM2835_IRQ_CODEC1         = 5;
 BCM2835_IRQ_CODEC2         = 6;

 {JPEG}
 BCM2835_IRQ_JPEG           = 7;  {Also available as IRQ 74 in the IRQ_basic_pending register}

 {ISP}
 BCM2835_IRQ_ISP            = 8;

 {USB (Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller)}
 BCM2835_IRQ_USB            = 9;  {Also available as IRQ 75 in the IRQ_basic_pending register}

 {3D}
 BCM2835_IRQ_3D             = 10; {Also available as IRQ 76 in the IRQ_basic_pending register}

 {Transposer}
 BCM2835_IRQ_TRANSPOSER     = 11;

 {Multicore Sync}
 BCM2835_IRQ_MULTICORESYNC0 = 12;
 BCM2835_IRQ_MULTICORESYNC1 = 13;
 BCM2835_IRQ_MULTICORESYNC2 = 14;
 BCM2835_IRQ_MULTICORESYNC3 = 15;

 {DMA}
 BCM2835_IRQ_DMA0           = 16;
 BCM2835_IRQ_DMA1           = 17;
 BCM2835_IRQ_DMA2           = 18; {Also available as IRQ 77 in the IRQ_basic_pending register}
 BCM2835_IRQ_DMA3           = 19; {Also available as IRQ 78 in the IRQ_basic_pending register}
 BCM2835_IRQ_DMA4           = 20;
 BCM2835_IRQ_DMA5           = 21;
 BCM2835_IRQ_DMA6           = 22;
 BCM2835_IRQ_DMA7           = 23;
 BCM2835_IRQ_DMA8           = 24;
 BCM2835_IRQ_DMA9           = 25;
 BCM2835_IRQ_DMA10          = 26;
 BCM2835_IRQ_DMA11_14       = 27; {BCM2835_IRQ_DMA11} {This IRQ is actually shared between DMA channels 11, 12, 13 and 14}
 BCM2835_IRQ_DMA_ALL        = 28; {BCM2835_IRQ_DMA12} {This IRQ is triggered by any DMA channel (all channels interrupt to allow DMA FIQ)}

 {AUX (UART1, SPI1 and SPI2)}
 BCM2835_IRQ_AUX            = 29;

 {ARM}
 BCM2835_IRQ_ARM            = 30;

 {GPUDMA}
 BCM2835_IRQ_GPUDMA         = 31;

 {IRQs 32 to 63 appear in the IRQ_pending_2 register}
 {Hostport}
 BCM2835_IRQ_HOSTPORT       = 32;

 {Videoscaler}
 BCM2835_IRQ_VIDEOSCALER    = 33;

 {CCP2TX}
 BCM2835_IRQ_CCP2TX         = 34;

 {SDC}
 BCM2835_IRQ_SDC            = 35;

 {DSI0}
 BCM2835_IRQ_DSI0           = 36;

 {AVE}
 BCM2835_IRQ_AVE            = 37;

 {CAM}
 BCM2835_IRQ_CAM0           = 38;
 BCM2835_IRQ_CAM1           = 39;

 {HDMI}
 BCM2835_IRQ_HDMI0          = 40;
 BCM2835_IRQ_HDMI1          = 41;

 {Pixelvalve}
 BCM2835_IRQ_PIXELVALVE1    = 42;

 {I2C / SPI Slave}
 BCM2835_IRQ_I2CSPI         = 43;

 {DSI1}
 BCM2835_IRQ_DSI1           = 44;

 {PWA}
 BCM2835_IRQ_PWA0           = 45;
 BCM2835_IRQ_PWA1           = 46;

 {CPR}
 BCM2835_IRQ_CPR            = 47;

 {SMI}
 BCM2835_IRQ_SMI            = 48;

 {GPIO}
 BCM2835_IRQ_GPIO_0         = 49; {Bank0}
 BCM2835_IRQ_GPIO_1         = 50; {Bank1}
 BCM2835_IRQ_GPIO_2         = 51; {Bank2 (Non existent in BCM2835)}
 BCM2835_IRQ_GPIO_ALL       = 52; {BCM2835_IRQ_GPIO_3} {Any Bank (all banks interrupt to allow GPIO FIQ)}

 {I2C}
 BCM2835_IRQ_I2C            = 53; {Also available as IRQ 79 in the IRQ_basic_pending register}

 {SPI}
 BCM2835_IRQ_SPI            = 54; {Also available as IRQ 80 in the IRQ_basic_pending register}

 {I2S PCM sound}
 BCM2835_IRQ_I2SPCM         = 55; {Also available as IRQ 81 in the IRQ_basic_pending register}

 {SDHOST}
 BCM2835_IRQ_SDHOST         = 56; {Also available as IRQ 82 in the IRQ_basic_pending register}

 {PL011 UART}
 BCM2835_IRQ_PL011          = 57; {Also available as IRQ 83 in the IRQ_basic_pending register}

 {Slimbus}
 BCM2835_IRQ_SLIMBUS        = 58;

 {VEC}
 BCM2835_IRQ_VEC            = 59;

 {CPG}
 BCM2835_IRQ_CPG            = 60;

 {RNG}
 BCM2835_IRQ_RNG            = 61;

 {SD card host controller (EMMC)}
 BCM2835_IRQ_SDHCI          = 62; {Also available as IRQ 84 in the IRQ_basic_pending register}

 {AVSPMON}
 BCM2835_IRQ_AVSPMON        = 63;

 {IRQs 64 to 84 appear in the IRQ_basic_pending register}
 {ARM Timer}
 BCM2835_IRQ_ARM_TIMER        = 64;  {ARM IRQ 0}

 {ARM Mailbox}
 BCM2835_IRQ_ARM_MAILBOX      = 65;  {ARM IRQ 1}

 {ARM Doorbell}
 BCM2835_IRQ_ARM_DOORBELL0    = 66;  {ARM IRQ 2}
 BCM2835_IRQ_ARM_DOORBELL1    = 67;  {ARM IRQ 3}

 {ARM GPU Halted}
 BCM2835_IRQ_ARM_GPU0HALTED   = 68;  {ARM IRQ 4}
 BCM2835_IRQ_ARM_GPU1HALTED   = 69;  {ARM IRQ 5}

 {ARM Illegal Access}
 BCM2835_IRQ_ARM_ILLEGALTYPE0 = 70;  {ARM IRQ 6}
 BCM2835_IRQ_ARM_ILLEGALTYPE1 = 71;  {ARM IRQ 7}

 {ARM Pending}
 BCM2835_IRQ_ARM_PENDING0     = 72;  {ARM IRQ 8}
 BCM2835_IRQ_ARM_PENDING1     = 73;  {ARM IRQ 9}

 {ARM JPEG}
 BCM2835_IRQ_ARM_JPEG         = 74;  {ARM IRQ 10}

 {ARM USB}
 BCM2835_IRQ_ARM_USB          = 75;  {ARM IRQ 11}

 {ARM 3D}
 BCM2835_IRQ_ARM_3D           = 76;  {ARM IRQ 12}

 {ARM DMA}
 BCM2835_IRQ_ARM_DMA2         = 77;  {ARM IRQ 13}
 BCM2835_IRQ_ARM_DMA3         = 78;  {ARM IRQ 14}

 {ARM I2C}
 BCM2835_IRQ_ARM_I2C          = 79;  {ARM IRQ 15}

 {ARM SPI}
 BCM2835_IRQ_ARM_SPI          = 80;  {ARM IRQ 16}

 {ARM I2SPCM}
 BCM2835_IRQ_ARM_I2SPCM       = 81;  {ARM IRQ 17}

 {ARM SDHOST}
 BCM2835_IRQ_ARM_SDHOST       = 82;  {ARM IRQ 18}

 {ARM PL011 UART}
 BCM2835_IRQ_ARM_PL011        = 83;  {ARM IRQ 19}

 {ARM SDHCI}
 BCM2835_IRQ_ARM_SDHCI        = 84;  {ARM IRQ 20}

 {IRQs 85 to 95 (ARM IRQs 21 to 31) are not assigned}

 {Number of IRQs shared between the GPU and ARM (These correspond to the IRQs that show up in the IRQ_pending_1 and IRQ_pending_2 registers)}
 BCM2835_GPU_IRQ_COUNT   = 64;

 {Number of ARM specific IRQs (These correspond to IRQs that show up in the first 8 bits of IRQ_basic_pending)}
 BCM2835_ARM_IRQ_COUNT = 32; {Previously 8}

 {Total number of IRQs available}
 BCM2835_IRQ_COUNT = BCM2835_GPU_IRQ_COUNT + BCM2835_ARM_IRQ_COUNT; {72}

 {Total number of FIQs available}
 BCM2835_FIQ_COUNT = 1;

const
 {System Timer frequencies}
 BCM2835_SYSTEM_TIMER_FREQUENCY = 1000000; {Default clock frequency of the BCM2835 System Timer (1MHz)}

 {System Timer Control/Status register bits (See Section 12)}
 BCM2835_SYSTEM_TIMER_CS_0 = (1 shl 0); {Already used by the VideoCore GPU (Do not use)}
 BCM2835_SYSTEM_TIMER_CS_1 = (1 shl 1);
 BCM2835_SYSTEM_TIMER_CS_2 = (1 shl 2); {Already used by the VideoCore GPU (Do not use)}
 BCM2835_SYSTEM_TIMER_CS_3 = (1 shl 3);

const
 {DMA Control and Status register bits (See Section 4)}
 BCM2835_DMA_CS_ACTIVE                         = (1 shl 0);   {Activate the DMA (This bit enables the DMA. The DMA will start if this bit is set and the CB_ADDR is non zero. The DMA transfer can be paused and resumed by clearing, then setting it again)}
 BCM2835_DMA_CS_END                            = (1 shl 1);   {DMA End Flag (Set when the transfer described by the current control block is complete. Write 1 to clear)}
 BCM2835_DMA_CS_INT                            = (1 shl 2);   {Interrupt Status (This is set when the transfer for the CB ends and INTEN is set to 1. Write 1 to clear)}
 BCM2835_DMA_CS_DREQ                           = (1 shl 3);   {DREQ State (Indicates the state of the selected DREQ (Data Request) signal, ie. the DREQ selected by the PERMAP field of the transfer info)}
 BCM2835_DMA_CS_PAUSED                         = (1 shl 4);   {DMA Paused State (Indicates if the DMA is currently paused and not transferring data)}
 BCM2835_DMA_CS_DREQ_PAUSED                    = (1 shl 5);   {DMA Paused by DREQ State (Indicates if the DMA is currently paused and not transferring data due to the DREQ being inactive)}
 BCM2835_DMA_CS_WAITING_FOR_OUTSTANDING_WRITES = (1 shl 6);   {DMA is Waiting for the Last Write to be Received (Indicates if the DMA is currently waiting for any outstanding writes to be received, and is not transferring data)}
 {Bit 7 Reserved - Write as 0, read as don't care}
 BCM2835_DMA_CS_ERROR                          = (1 shl 8);   {DMA Error (Indicates if the DMA has detected an error)}
 {Bits 9:15 Reserved - Write as 0, read as don't care}
 BCM2835_DMA_CS_PRIORITY                       = ($F shl 16); {AXI Priority Level (Sets the priority of normal AXI bus transactions. Zero is the lowest priority)}
 BCM2835_DMA_CS_PANIC_PRIORITY                 = ($F shl 20); {AXI Panic Priority Level (Sets the priority of panicking AXI bus transactions)}
 {Bits 24:27 Reserved - Write as 0, read as don't care}
 BCM2835_DMA_CS_WAIT_FOR_OUTSTANDING_WRITES    = (1 shl 28);  {Wait for outstanding writes (When set to 1, the DMA will keep a tally of the AXI writes going out and the write responses coming in)}
 BCM2835_DMA_CS_DISDEBUG                       = (1 shl 29);  {Disable debug pause signal (When set to 1, the DMA will not stop when the debug pause signal is asserted)}
 BCM2835_DMA_CS_ABORT                          = (1 shl 30);  {Abort DMA (Writing a 1 to this bit will abort the current DMA CB. The DMA will load the next CB and attempt to continue. The bit cannot be read, and will self clear)}
 BCM2835_DMA_CS_RESET                          = (1 shl 31);  {DMA Channel Reset (Writing a 1 to this bit will reset the DMA. The bit cannot be read, and will self clear)}

 BCM2835_DMA_CS_PRIORITY_SHIFT = 16;
 BCM2835_DMA_CS_PANIC_PRIORITY_SHIFT = 20;

 BCM2835_DMA_CS_PRIORITY_DEFAULT = 0;
 BCM2835_DMA_CS_PRIORITY_MAX     = 15;

 {DMA Transfer Information bits (See Section 4)}
 BCM2835_DMA_TI_INTEN          = (1 shl 0);    {Interrupt Enable (1 = Generate an interrupt when the transfer described by the current Control Block completes)}
 BCM2835_DMA_TI_2DMODE         = (1 shl 1);    {2D Mode (1 = 2D mode interpret the TXFR_LEN register as YLENGTH number of transfers each of XLENGTH, and add the strides to the address after each transfer)}
 {Bit 2 Reserved - Write as 0, read as don't care}
 BCM2835_DMA_TI_WAIT_RESP      = (1 shl 3);    {Wait for a Write Response (When set this makes the DMA wait until it receives the AXI write response for each write. This ensures that multiple writes cannot get stacked in the AXI bus pipeline)}
 BCM2835_DMA_TI_DEST_INC       = (1 shl 4);    {Destination Address Increment (1 = Destination address increments after each write The address will increment by 4, if DEST_WIDTH=0 else by 32)}
 BCM2835_DMA_TI_DEST_WIDTH     = (1 shl 5);    {Destination Transfer Width (1 = Use 128-bit destination write width. 0 = Use 32-bit destination write width)}
 BCM2835_DMA_TI_DEST_DREQ      = (1 shl 6);    {Control Destination Writes with DREQ (1 = The DREQ selected by PERMAP will gate the destination writes)}
 BCM2835_DMA_TI_DEST_IGNORE    = (1 shl 7);    {Ignore Writes (1 = Do not perform destination writes. 0 = Write data to destination)}
 BCM2835_DMA_TI_SRC_INC        = (1 shl 8);    {Source Address Increment (1 = Source address increments after each read. The address will increment by 4, if S_WIDTH=0 else by 32)}
 BCM2835_DMA_TI_SRC_WIDTH      = (1 shl 9);    {Source Transfer Width (1 = Use 128-bit source read width. 0 = Use 32-bit source read width)}
 BCM2835_DMA_TI_SRC_DREQ       = (1 shl 10);   {Control Source Reads with DREQ (1 = The DREQ selected by PERMAP will gate the source reads)}
 BCM2835_DMA_TI_SRC_IGNORE     = (1 shl 11);   {Ignore Reads (1 = Do not perform source reads. In addition, destination writes will zero all the write strobes. This is used for fast cache fill operations)}
 BCM2835_DMA_TI_BURST_LENGTH   = ($F shl 12);  {Burst Transfer Length (Indicates the burst length of the DMA transfers)}
 BCM2835_DMA_TI_PERMAP         = ($1F shl 16); {Peripheral Mapping (Indicates the peripheral number (1-31) whose ready signal shall be used to control the rate of the transfers)}
 BCM2835_DMA_TI_WAITS          = ($3E shl 21); {Add Wait Cycles (This slows down the DMA throughput by setting the number of dummy cycles burnt after each DMA read or write operation is completed)}
 BCM2835_DMA_TI_NO_WIDE_BURSTS = (1 shl 26);   {Don't Do wide writes as a 2 beat burst (This prevents the DMA from issuing wide writes as 2 beat AXI bursts. This is an inefficient access mode, so the default is to use the bursts)}
 {Bits 27:31 Reserved - Write as 0, read as don't care}

 {Note: BCM2835_DMA_TI_2DMODE, BCM2835_DMA_TI_DEST_IGNORE, BCM2835_DMA_TI_SRC_IGNORE and BCM2835_DMA_TI_NO_WIDE_BURSTS not available on DMA Lite channels}

 BCM2835_DMA_TI_PERMAP_SHIFT = 16;
 BCM2835_DMA_TI_BURST_LENGTH_SHIFT = 12;

 BCM2835_DMA_TI_BURST_LENGTH_DEFAULT = 0;
 BCM2835_DMA_TI_BURST_LENGTH_MAX     = 15;

 {DMA Transfer Length bits (See Section 4)}
 BCM2835_DMA_TXFR_LEN_XLENGTH = ($FFFF shl 0);  {Transfer Length in bytes}
 BCM2835_DMA_TXFR_LEN_YLENGTH = ($3FFF shl 16); {When in 2D mode, This is the Y transfer length, indicating how many xlength transfers are performed. When in normal linear mode this becomes the top bits of the XLENGTH}

 {Note: BCM2835_DMA_TXFR_LEN_YLENGTH not available on DMA Lite channels}

 {DMA 2D Stride bits (See Section 4)}
 BCM2835_DMA_STRIDE_S_STRIDE  = ($FFFF shl 0);  {Destination Stride (2D Mode) (Signed (2 s complement) byte increment to apply to the destination address at the end of each row in 2D mode)}
 BCM2835_DMA_STRIDE_D_STRIDE  = ($FFFF shl 16); {Source Stride (2D Mode) (Signed (2 s complement) byte increment to apply to the source address at the end of each row in 2D mode)}

 {Note: BCM2835_DMA_STRIDE_S_STRIDE and BCM2835_DMA_STRIDE_D_STRIDE not available on DMA Lite channels}

 {DMA Debug register bits (See Section 4)}
 BCM2835_DMA_DEBUG_READ_LAST_NOT_SET_ERROR = (1 shl 0);     {Read Last Not Set Error}
 BCM2835_DMA_DEBUG_FIFO_ERROR              = (1 shl 1);     {Fifo Error}
 BCM2835_DMA_DEBUG_READ_ERROR              = (1 shl 2);     {Slave Read Response Error}
 {Bit 3 Reserved - Write as 0, read as don't care}
 BCM2835_DMA_DEBUG_OUTSTANDING_WRITES      = ($F shl 4);    {DMA Outstanding Writes Counter}
 BCM2835_DMA_DEBUG_DMA_ID                  = ($FF shl 8);   {DMA ID}
 BCM2835_DMA_DEBUG_DMA_STATE               = ($1FF shl 16); {DMA State Machine State}
 BCM2835_DMA_DEBUG_VERSION                 = (7 shl 25);    {DMA Version}
 BCM2835_DMA_DEBUG_LITE                    = (1 shl 28);    {DMA Lite}
 {Bits 29:31 Reserved - Write as 0, read as don't care}

 {DMA Engine Interrupt Status register bits (See Section 4)}
 BCM2835_DMA_INT_STATUS_0  = (1 shl 0);
 BCM2835_DMA_INT_STATUS_1  = (1 shl 1);
 BCM2835_DMA_INT_STATUS_2  = (1 shl 2);
 BCM2835_DMA_INT_STATUS_3  = (1 shl 3);
 BCM2835_DMA_INT_STATUS_4  = (1 shl 4);
 BCM2835_DMA_INT_STATUS_5  = (1 shl 5);
 BCM2835_DMA_INT_STATUS_6  = (1 shl 6);
 BCM2835_DMA_INT_STATUS_7  = (1 shl 7);
 BCM2835_DMA_INT_STATUS_8  = (1 shl 8);
 BCM2835_DMA_INT_STATUS_9  = (1 shl 9);
 BCM2835_DMA_INT_STATUS_10 = (1 shl 10);
 BCM2835_DMA_INT_STATUS_11 = (1 shl 11);
 BCM2835_DMA_INT_STATUS_12 = (1 shl 12);
 BCM2835_DMA_INT_STATUS_13 = (1 shl 13);
 BCM2835_DMA_INT_STATUS_14 = (1 shl 14);
 BCM2835_DMA_INT_STATUS_15 = (1 shl 15);

 {DMA Engine Enable register bits (See Section 4)}
 BCM2835_DMA_ENABLE_0  = (1 shl 0);
 BCM2835_DMA_ENABLE_1  = (1 shl 1);
 BCM2835_DMA_ENABLE_2  = (1 shl 2);
 BCM2835_DMA_ENABLE_3  = (1 shl 3);
 BCM2835_DMA_ENABLE_4  = (1 shl 4);
 BCM2835_DMA_ENABLE_5  = (1 shl 5);
 BCM2835_DMA_ENABLE_6  = (1 shl 6);
 BCM2835_DMA_ENABLE_7  = (1 shl 7);
 BCM2835_DMA_ENABLE_8  = (1 shl 8);
 BCM2835_DMA_ENABLE_9  = (1 shl 9);
 BCM2835_DMA_ENABLE_10 = (1 shl 10);
 BCM2835_DMA_ENABLE_11 = (1 shl 11);
 BCM2835_DMA_ENABLE_12 = (1 shl 12);
 BCM2835_DMA_ENABLE_13 = (1 shl 13);
 BCM2835_DMA_ENABLE_14 = (1 shl 14);

 {DMA Engine DREQ Peripherals (See Section 4)}
 BCM2835_DMA_DREQ_NONE         = 0;
 BCM2835_DMA_DREQ_DSI0         = 1;
 BCM2835_DMA_DREQ_PCMTX        = 2;
 BCM2835_DMA_DREQ_PCMRX        = 3;
 BCM2835_DMA_DREQ_SMI          = 4;
 BCM2835_DMA_DREQ_PWM          = 5;
 BCM2835_DMA_DREQ_SPITX        = 6;
 BCM2835_DMA_DREQ_SPIRX        = 7;
 BCM2835_DMA_DREQ_BSCSPITX     = 8;  {BSC/SPI Slave}
 BCM2835_DMA_DREQ_BSCSPIRX     = 9;  {BSC/SPI Slave}
 BCM2835_DMA_DREQ_RESERVED1    = 10;
 BCM2835_DMA_DREQ_EMMC         = 11; {SDHCI}
 BCM2835_DMA_DREQ_UARTTX       = 12;
 BCM2835_DMA_DREQ_SDHOST       = 13; {SDHOST}
 BCM2835_DMA_DREQ_UARTRX       = 14;
 BCM2835_DMA_DREQ_DSI1         = 15;
 BCM2835_DMA_DREQ_SLIMBUS_MCTX = 16;
 BCM2835_DMA_DREQ_HDMI         = 17;
 BCM2835_DMA_DREQ_SLIMBUS_MCRX = 18;
 BCM2835_DMA_DREQ_SLIMBUS_DC0  = 19;
 BCM2835_DMA_DREQ_SLIMBUS_DC1  = 20;
 BCM2835_DMA_DREQ_SLIMBUS_DC2  = 21;
 BCM2835_DMA_DREQ_SLIMBUS_DC3  = 22;
 BCM2835_DMA_DREQ_SLIMBUS_DC4  = 23;
 BCM2835_DMA_DREQ_SCALER_FIFO0 = 24;
 BCM2835_DMA_DREQ_SCALER_FIFO1 = 25;
 BCM2835_DMA_DREQ_SCALER_FIFO2 = 26;
 BCM2835_DMA_DREQ_SLIMBUS_DC5  = 27;
 BCM2835_DMA_DREQ_SLIMBUS_DC6  = 28;
 BCM2835_DMA_DREQ_SLIMBUS_DC7  = 29;
 BCM2835_DMA_DREQ_SLIMBUS_DC8  = 30;
 BCM2835_DMA_DREQ_SLIMBUS_DC9  = 31;

const
 {BSC (I2C0/1/2) Control register bits (See 3.2)}
 BCM2835_BSC_C_I2CEN = (1 shl 15); {I2C Enable (0 = BSC controller is disabled / 1 = BSC controller is enabled)}
 BCM2835_BSC_C_INTR  = (1 shl 10); {INTR Interrupt on RX (0 = Don t generate interrupts on RXR condition / 1 = Generate interrupt while RXR = 1)}
 BCM2835_BSC_C_INTT  = (1 shl 9);  {INTT Interrupt on TX (0 = Don t generate interrupts on TXW condition / 1 = Generate interrupt while TXW = 1)}
 BCM2835_BSC_C_INTD  = (1 shl 8);  {INTD Interrupt on DONE (0 = Don t generate interrupts on DONE condition / 1 = Generate interrupt while DONE = 1)}
 BCM2835_BSC_C_ST    = (1 shl 7);  {ST Start Transfer (0 = No action / 1 = Start a new transfer. One shot operation. Read back as 0)}
 BCM2835_BSC_C_CLEAR = (1 shl 5);  {CLEAR FIFO Clear (00 = No action / x1 = Clear FIFO. One shot operation / 1x = Clear FIFO. One shot operation)}
 BCM2835_BSC_C_READ  = (1 shl 0);  {READ Read Transfer (0 = Write Packet Transfer / 1 = Read Packet Transfer)}

 {BSC (I2C0/1/2) Status register bits (See 3.2)}
 BCM2835_BSC_S_CLKT = (1 shl 9); {CLKT Clock Stretch Timeout (0 = No errors detected. 1 = Slave has held the SCL signal low (clock stretching) for longer and that specified in the I2CCLKT register Cleared by writing 1 to the field)}
 BCM2835_BSC_S_ERR  = (1 shl 8); {ERR ACK Error (0 = No errors detected. 1 = Slave has not acknowledged its address. Cleared by writing 1 to the field)}
 BCM2835_BSC_S_RXF  = (1 shl 7); {RXF - FIFO Full (0 = FIFO is not full. 1 = FIFO is full. If a read is underway, no further serial data will be received until data is read from FIFO)}
 BCM2835_BSC_S_TXE  = (1 shl 6); {TXE - FIFO Empty (0 = FIFO is not empty. 1 = FIFO is empty. If a write is underway, no further serial data can be transmitted until data is written to the FIFO)}
 BCM2835_BSC_S_RXD  = (1 shl 5); {RXD - FIFO contains Data (0 = FIFO is empty. 1 = FIFO contains at least 1 byte. Cleared by reading sufficient data from FIFO)}
 BCM2835_BSC_S_TXD  = (1 shl 4); {TXD - FIFO can accept Data (0 = FIFO is full. The FIFO cannot accept more data. 1 = FIFO has space for at least 1 byte)}
 BCM2835_BSC_S_RXR  = (1 shl 3); {RXR - FIFO needs Reading (full) (0 = FIFO is less than full and a read is underway. 1 = FIFO is or more full and a read is underway. Cleared by reading sufficient data from the FIFO)}
 BCM2835_BSC_S_TXW  = (1 shl 2); {TXW - FIFO needs Writing (full) (0 = FIFO is at least full and a write is underway (or sufficient data to send). 1 = FIFO is less then full and a write is underway. Cleared by writing sufficient data to the FIFO)}
 BCM2835_BSC_S_DONE = (1 shl 1); {DONE Transfer Done (0 = Transfer not completed. 1 = Transfer complete. Cleared by writing 1 to the field)}
 BCM2835_BSC_S_TA   = (1 shl 0); {TA Transfer Active (0 = Transfer not active. 1 = Transfer active)}

 {BSC (I2C0/1/2) Data Length register bits (See 3.2)}
 BCM2835_BSC_DLEN_MASK = $FFFF; {Data Length. (Writing to DLEN specifies the number of bytes to be transmitted/received. Reading from DLEN when TA = 1 or DONE = 1, returns the number of bytes still to be transmitted or received)}

 {BSC (I2C0/1/2) Slave Address register bits (See 3.2)}
 BCM2835_BSC_A_MASK = $7F; {Slave Address.}

 {BSC (I2C0/1/2) Data FIFO register bits (See 3.2)}
 BCM2835_BSC_FIFO_MASK = $FF; {Writes to the register write transmit data to the FIFO. Reads from register reads received data from the FIFO.}
 BCM2835_BSC_FIFO_SIZE = 16;

 {BSC (I2C0/1/2) Clock Divider register bits (See 3.2)}
 BCM2835_BSC_CDIV_MASK = $FFFF; {Clock Divider (SCL = core clock / CDIV) (CDIV is always rounded down to an even number)}

 {BSC (I2C0/1/2) Data Delay register bits (See 3.2)}
 BCM2835_BSC_DEL_FEDL_MASK = ($FFFF shl 16); {FEDL Falling Edge Delay (Number of core clock cycles to wait after the falling edge of SCL before outputting next bit of data)}
 BCM2835_BSC_DEL_REDL_MASK = ($FFFF shl 0);  {REDL Rising Edge Delay (Number of core clock cycles to wait after the rising edge of SCL before reading the next bit of data)}
 BCM2835_BSC_DEL_FEDL_SHIFT = 16;
 BCM2835_BSC_DEL_REDL_SHIFT = 0;

 {BSC (I2C0/1/2) Clock Stretch Timeout register bits (See 3.2)}
 BCM2835_BSC_CLKT_TOUT_MASK = $FFFF; {TOUT Clock Stretch Timeout Value (Number of SCL clock cycles to wait after the rising edge of SCL before deciding that the slave is not responding)}

const
 {SPI0 register bits (See 10.5)}
 BCM2835_SPI0_CS_LEN_LONG = (1 shl 25); {Enable Long data word in Lossi mode if DMA_LEN is set (0 = writing to the FIFO will write a single byte / 1 = writing to the FIFO will write a 32 bit word)}
 BCM2835_SPI0_CS_DMA_LEN  = (1 shl 24); {Enable DMA mode in Lossi mode}
 BCM2835_SPI0_CS_CSPOL2   = (1 shl 23); {Chip Select 2 Polarity (0 = Chip select is active low / 1 = Chip select is active high)}
 BCM2835_SPI0_CS_CSPOL1   = (1 shl 22); {Chip Select 1 Polarity (0 = Chip select is active low / 1 = Chip select is active high)}
 BCM2835_SPI0_CS_CSPOL0   = (1 shl 21); {Chip Select 0 Polarity (0 = Chip select is active low / 1 = Chip select is active high)}
 BCM2835_SPI0_CS_RXF      = (1 shl 20); {RXF - RX FIFO Full (0 = RXFIFO is not full / 1 = RX FIFO is full. No further serial data will be sent/received until data is read from FIFO)}
 BCM2835_SPI0_CS_RXR      = (1 shl 19); {RXR RX FIFO needs Reading (full) (0 = RX FIFO is less than full (or not active TA = 0) / 1 = RX FIFO is or more full. Cleared by reading sufficient data from the RX FIFO or setting TA to 0)}
 BCM2835_SPI0_CS_TXD      = (1 shl 18); {TXD TX FIFO can accept Data (0 = TX FIFO is full and so cannot accept more data / 1 = TX FIFO has space for at least 1 byte)}
 BCM2835_SPI0_CS_RXD      = (1 shl 17); {RXD RX FIFO contains Data (0 = RX FIFO is empty / 1 = RX FIFO contains at least 1 byte)}
 BCM2835_SPI0_CS_DONE     = (1 shl 16); {DONE Transfer Done (0 = Transfer is in progress (or not active TA = 0) / 1 = Transfer is complete. Cleared by writing more data to the TX FIFO or setting TA to 0)}
 BCM2835_SPI0_CS_TE_EN    = (1 shl 15); {Unused}
 BCM2835_SPI0_CS_LMONO    = (1 shl 14); {Unused}
 BCM2835_SPI0_CS_LEN      = (1 shl 13); {LEN LoSSI enable (0 = The serial interface will behave as an SPI master / 1 = The serial interface will behave as a LoSSI master)}
 BCM2835_SPI0_CS_REN      = (1 shl 12); {REN Read Enable. If this bit is set, the SPI peripheral will be able to send data to this device (0 = We intend to write to the SPI peripheral / 1 = We intend to read from the SPI peripheral)}
 BCM2835_SPI0_CS_ADCS     = (1 shl 11); {ADCS Automatically Deassert Chip Select (0 = Don t automatically deassert chip select at the end of a DMA transfer chip select is manually controlled by software. / 1 = Automatically deassert chip select at the end of a DMA transfer as determined by SPIDLEN)}
 BCM2835_SPI0_CS_INTR     = (1 shl 10); {INTR Interrupt on RXR (0 = Don t generate interrupts on RX FIFO condition / 1 = Generate interrupt while RXR = 1)}
 BCM2835_SPI0_CS_INTD     = (1 shl 9);  {INTD Interrupt on Done (0 = Don t generate interrupt on transfer complete / 1 = Generate interrupt when DONE = 1)}
 BCM2835_SPI0_CS_DMAEN    = (1 shl 8);  {DMAEN DMA Enable (0 = No DMA requests will be issued / 1 = Enable DMA operation. Peripheral generates data requests. These will be taken in four-byte words until the SPIDLEN has been reached}
 BCM2835_SPI0_CS_TA       = (1 shl 7);  {Transfer Active (0 = Transfer not active / 1 = Transfer active)}
 BCM2835_SPI0_CS_CSPOL    = (1 shl 6);  {Chip Select Polarity (0 = Chip select lines are active low / 1 = Chip select lines are active high}
 BCM2835_SPI0_CS_CLEAR_RX = (1 shl 5);  {CLEAR FIFO Clear (00 = No action / x1 = Clear TX FIFO. One shot operation / 1x = Clear RX FIFO. One shot operation)}
 BCM2835_SPI0_CS_CLEAR_TX = (1 shl 4);  {As above}
 BCM2835_SPI0_CS_CPOL     = (1 shl 3);  {Clock Polarity (0 = Rest state of clock = low / 1 = Rest state of clock = high)}
 BCM2835_SPI0_CS_CPHA     = (1 shl 2);  {Clock Phase (0 = First SCLK transition at middle of data bit / 1 = First SCLK transition at beginning of data bit)}
 BCM2835_SPI0_CS_CS_0     = (0 shl 0);  {Chip Select (00 = Chip select 0 / 01 = Chip select 1 / 10 = Chip select 2 / 11 = Reserved}
 BCM2835_SPI0_CS_CS_1     = (1 shl 0);  {As above}
 BCM2835_SPI0_CS_CS_2     = (2 shl 0);  {As above}

 BCM2835_SPI0_CS_CS_MASK  = (3 shl 0);

 BCM2835_SPI0_FIFO_DMA_DATA = $FFFFFFFF; {DMA Mode (DMAEN set) If TA is clear, the first 32-bit write to this register will control SPIDLEN and SPICS. Subsequent reads and writes will be taken as four-byte data words to be read/written to the FIFOs}
 BCM2835_SPI0_FIFO_IRQ_DATA = $000000FF; {Poll/Interrupt Mode (DMAEN clear, TA set) Writes to the register write bytes to TX FIFO. Reads from register read bytes from the RX FIFO}

 BCM2835_SPI0_CLK_CDIV    = $0000FFFF; {Clock Divider (SCLK = Core Clock / CDIV) If CDIV is set to 0, the divisor is 65536. The divisor must be a multiple of 2. Odd numbers rounded down. The maximum SPI clock rate is of the APB clock}

 BCM2835_SPI0_DLEN_LEN    = $0000FFFF; {Data Length. The number of bytes to transfer. This field is only valid for DMA mode (DMAEN set) and controls how many bytes to transmit (and therefore receive)}

 BCM2835_SPI0_LTOH_TOH    = $0000000F; {This sets the Output Hold delay in APB clocks (A value of 0 causes a 1 clock delay)}

 BCM2835_SPI0_DC_RPANIC   = ($FF shl 24); {DMA Read Panic Threshold (Generate the Panic signal to the RX DMA engine whenever the RX FIFO level is greater than this amount)}
 BCM2835_SPI0_DC_RDREQ    = ($FF shl 16); {DMA Read Request Threshold (Generate A DREQ to the RX DMA engine whenever the RX FIFO level is greater than this amount) (RX DREQ is also generated if thetransfer has finished but the RXFIFO isn't empty)}
 BCM2835_SPI0_DC_TPANIC   = ($FF shl 8);  {DMA Write Panic Threshold (Generate the Panic signal to the TX DMA engine whenever the TX FIFO level is less than or equal to this amount)}
 BCM2835_SPI0_DC_TDREQ    = ($FF shl 0);  {DMA Write Request Threshold (Generate a DREQ signal to the TX DMA engine whenever the TX FIFO level is less than or equal to this amount)}

const
 {I2C / SPI Slave DR (Data) register bits (See 11.2)}
 BCM2835_I2CSPI_DR_RXFLEVEL_MASK  = ($1F shl 27); {RX FIFO Level}
 BCM2835_I2CSPI_DR_RXFLEVEL_SHIFT = 27;
 BCM2835_I2CSPI_DR_TXFLEVEL_MASK  = ($1F shl 22); {TX FIFO Level}
 BCM2835_I2CSPI_DR_TXFLEVEL_SHIFT = 22;
 BCM2835_I2CSPI_DR_RXBUSY         = (1 shl 21);   {Receive Busy}
 BCM2835_I2CSPI_DR_TXFE           = (1 shl 20);   {TX FIFO Empty}
 BCM2835_I2CSPI_DR_RXFF           = (1 shl 19);   {RX FIFO Full}
 BCM2835_I2CSPI_DR_TXFF           = (1 shl 18);   {TX FIFO Full}
 BCM2835_I2CSPI_DR_RXFE           = (1 shl 17);   {RX FIFO Empty}
 BCM2835_I2CSPI_DR_TXBUSY         = (1 shl 16);   {Transmit Busy}
 BCM2835_I2CSPI_DR_UE             = (1 shl 9);    {TX Underrun Error}
 BCM2835_I2CSPI_DR_OE             = (1 shl 8);    {RX Overrun Error}
 BCM2835_I2CSPI_DR_DATA_MASK      = ($FF shl 0);  {Received/Transferred data}

 {I2C / SPI Slave RSR (Status) register bits (See 11.2)}
 BCM2835_I2CSPI_RSR_UE = (1 shl 1); {TX Underrun Error}
 BCM2835_I2CSPI_RSR_OE = (1 shl 0); {RX Overrun Error}

 {I2C / SPI Slave SLV (Slave) register bits (See 11.2)}
 BCM2835_I2CSPI_SLV_ADDR_MASK = ($7F shl 0); {I2C Slave Address}

 {I2C / SPI Slave CR (Control) register bits (See 11.2)}
 BCM2835_I2CSPI_CR_INV_TXF  = (1 shl 13); {Inverse TX status flags}
 BCM2835_I2CSPI_CR_TESTFIFO = (1 shl 11); {TEST FIFO}
 BCM2835_I2CSPI_CR_INV_RXF  = (1 shl 10); {Inverse RX status flags}
 BCM2835_I2CSPI_CR_RXE      = (1 shl 9);  {Receive Enable}
 BCM2835_I2CSPI_CR_TXE      = (1 shl 8);  {Transmit Enable}
 BCM2835_I2CSPI_CR_BRK      = (1 shl 7);  {Break current operation}
 BCM2835_I2CSPI_CR_CPOL     = (1 shl 4);  {Clock Polarity}
 BCM2835_I2CSPI_CR_CPHA     = (1 shl 3);  {Clock Phase}
 BCM2835_I2CSPI_CR_I2C      = (1 shl 2);  {I2C Enable}
 BCM2835_I2CSPI_CR_SPI      = (1 shl 1);  {SPI Enable}
 BCM2835_I2CSPI_CR_EN       = (1 shl 0);  {Enable Device}

 {I2C / SPI Slave FR (Flags) register bits (See 11.2)}
 BCM2835_I2CSPI_RXFLEVEL_MASK  = ($1F shl 11); {RX FIFO Level}
 BCM2835_I2CSPI_RXFLEVEL_SHIFT = 11;
 BCM2835_I2CSPI_TXFLEVEL_MASK  = ($1F shl 6);  {TX FIFO Level}
 BCM2835_I2CSPI_TXFLEVEL_SHIFT = 6;
 BCM2835_I2CSPI_FR_RXBUSY      = (1 shl 5);    {Receive Busy}
 BCM2835_I2CSPI_FR_TXFE        = (1 shl 4);    {TX FIFO Empty}
 BCM2835_I2CSPI_FR_RXFF        = (1 shl 3);    {RX FIFO Full}
 BCM2835_I2CSPI_FR_TXFF        = (1 shl 2);    {TX FIFO Full}
 BCM2835_I2CSPI_FR_RXFE        = (1 shl 1);    {RX FIFO Empty}
 BCM2835_I2CSPI_FR_TXBUSY      = (1 shl 0);    {Transmit Busy}

 {I2C / SPI Slave IFLS (Interrupt FIFO Level Select) register bits (See 11.2)}
 BCM2835_I2CSPI_IFLS_RXIFLSEL_MASK  = (7 shl 3); {RX Interrupt FIFO Level Select}
 BCM2835_I2CSPI_IFLS_RXIFLSEL_SHIFT = 3;
 BCM2835_I2CSPI_IFLS_RXIFLSEL1_8    = (0 shl 3); {000 RX FIFO gets 1/8 full}
 BCM2835_I2CSPI_IFLS_RXIFLSEL1_4    = (1 shl 3); {001 RX FIFO gets 1/4 full}
 BCM2835_I2CSPI_IFLS_RXIFLSEL1_2    = (2 shl 3); {010 RX FIFO gets 1/2 full}
 BCM2835_I2CSPI_IFLS_RXIFLSEL3_4    = (3 shl 3); {011 RX FIFO gets 3/4 full}
 BCM2835_I2CSPI_IFLS_RXIFLSEL7_8    = (4 shl 3); {100 RX FIFO gets 7/8 full}
 BCM2835_I2CSPI_IFLS_TXIFLSEL_MASK  = (7 shl 0); {TX Interrupt FIFO Level Select}
 BCM2835_I2CSPI_IFLS_TXIFLSEL_SHIFT = 0;
 BCM2835_I2CSPI_IFLS_TXIFLSEL1_8    = (0 shl 0); {000 TX FIFO gets 1/8 full}
 BCM2835_I2CSPI_IFLS_TXIFLSEL1_4    = (1 shl 0); {001 TX FIFO gets 1/4 full}
 BCM2835_I2CSPI_IFLS_TXIFLSEL1_2    = (2 shl 0); {010 TX FIFO gets 1/2 full}
 BCM2835_I2CSPI_IFLS_TXIFLSEL3_4    = (3 shl 0); {011 TX FIFO gets 3/4 full}
 BCM2835_I2CSPI_IFLS_TXIFLSEL7_8    = (4 shl 0); {100 TX FIFO gets 7/8 full}

 {I2C / SPI Slave IMSC (Interrupt Mask Set Clear) register bits (See 11.2)}
 BCM2835_I2CSPI_IMSC_OEIM = (1 shl 3); {Overrun error interrupt mask}
 BCM2835_I2CSPI_IMSC_BEIM = (1 shl 2); {Break error interrupt mask}
 BCM2835_I2CSPI_IMSC_TXIM = (1 shl 1); {Transmit interrupt mask}
 BCM2835_I2CSPI_IMSC_RXIM = (1 shl 0); {Receive interrupt mask}

 {I2C / SPI Slave RIS (Raw Interrupt Status) register bits (See 11.2)}
 BCM2835_I2CSPI_RIS_OERIS = (1 shl 3); {Overrun error interrupt status}
 BCM2835_I2CSPI_RIS_BERIS = (1 shl 2); {Break error interrupt status}
 BCM2835_I2CSPI_RIS_TXRIS = (1 shl 1); {Transmit interrupt status}
 BCM2835_I2CSPI_RIS_RXRIS = (1 shl 0); {Receive interrupt status}

 {I2C / SPI Slave MIS (Masked Interrupt Status) register bits (See 11.2)}
 BCM2835_I2CSPI_MIS_OEMIS = (1 shl 3); {Overrun error masked interrupt status}
 BCM2835_I2CSPI_MIS_BEMIS = (1 shl 2); {Break error masked interrupt status}
 BCM2835_I2CSPI_MIS_TXMIS = (1 shl 1); {Transmit masked interrupt status}
 BCM2835_I2CSPI_MIS_RXMIS = (1 shl 0); {Receive masked interrupt status}

 {I2C / SPI Slave ICR (Interrupt Clear Register) register bits (See 11.2)}
 BCM2835_I2CSPI_ICR_OEIC = (1 shl 3); {Overrun error interrupt clear}
 BCM2835_I2CSPI_ICR_BEIC = (1 shl 2); {Break error interrupt clear}
 BCM2835_I2CSPI_ICR_TXIC = (1 shl 1); {Transmit interrupt clear}
 BCM2835_I2CSPI_ICR_RXIC = (1 shl 0); {Receive interrupt clear}

//const
 {AUX (UART1, SPI1 and SPI2) register bits (See 2.1)}
 //To Do

//const
 {PCM / I2S register bits (See 8.8)}
 //To Do

const
 {Pulse Width Modulator (PWM) Control register bits (See 9.6)}
 BCM2835_PWM_CTL_MSEN2 = (1 shl 15); {Channel 2 M/S Enable (0: PWM algorithm is used / 1: M/S transmission is used)}
 {Bit 14 Reserved - Write as 0, read as don't care}
 BCM2835_PWM_CTL_USEF2 = (1 shl 13); {Channel 2 Use Fifo (0: Data register is transmitted / 1: Fifo is used for transmission)}
 BCM2835_PWM_CTL_POLA2 = (1 shl 12); {Channel 2 Polarity (0 : 0=low 1=high / 1: 1=low 0=high)}
 BCM2835_PWM_CTL_SBIT2 = (1 shl 11); {Channel 2 Silence Bit (Defines the state of the output when no transmission takes place)}
 BCM2835_PWM_CTL_RPTL2 = (1 shl 10); {Channel 2 Repeat Last Data (0: Transmission interrupts when FIFO is empty / 1: Last data in FIFO is transmitted repetedly until FIFO is not empty)}
 BCM2835_PWM_CTL_MODE2 = (1 shl 9);  {Channel 2 Mode (0: PWM mode / 1: Serialiser mode)}
 BCM2835_PWM_CTL_PWEN2 = (1 shl 8);  {Channel 2 Enable (0: Channel is disabled / 1: Channel is enabled)}
 BCM2835_PWM_CTL_MSEN1 = (1 shl 7);  {Channel 1 M/S Enable (0: PWM algorithm is used / 1: M/S transmission is used)}
 BCM2835_PWM_CTL_CLRF1 = (1 shl 6);  {Clear Fifo (1: Clears FIFO / 0: Has no effect) (This is a single shot operation. This bit always reads 0)}
 BCM2835_PWM_CTL_USEF1 = (1 shl 5);  {Channel 1 Use Fifo (0: Data register is transmitted / 1: Fifo is used for transmission)}
 BCM2835_PWM_CTL_POLA1 = (1 shl 4);  {Channel 1 Polarity (0 : 0=low 1=high / 1: 1=low 0=high)}
 BCM2835_PWM_CTL_SBIT1 = (1 shl 3);  {Channel 1 Silence Bit (Defines the state of the output when no transmission takes place)}
 BCM2835_PWM_CTL_RPTL1 = (1 shl 2);  {Channel 1 Repeat Last Data (0: Transmission interrupts when FIFO is empty / 1: Last data in FIFO is transmitted repetedly until FIFO is not empty)}
 BCM2835_PWM_CTL_MODE1 = (1 shl 1);  {Channel 1 Mode (0: PWM mode / 1: Serialiser mode)}
 BCM2835_PWM_CTL_PWEN1 = (1 shl 0);  {Channel 1 Enable (0: Channel is disabled / 1: Channel is enabled)}

 {Pulse Width Modulator (PWM) Status register bits (See 9.6)}
 BCM2835_PWM_STA_STA4  = (1 shl 12); {Channel 4 State}
 BCM2835_PWM_STA_STA3  = (1 shl 11); {Channel 3 State}
 BCM2835_PWM_STA_STA2  = (1 shl 10); {Channel 2 State}
 BCM2835_PWM_STA_STA1  = (1 shl 9);  {Channel 1 State}
 BCM2835_PWM_STA_BERR  = (1 shl 8);  {Bus Error Flag}
 BCM2835_PWM_STA_GAPO4 = (1 shl 7);  {Channel 4 Gap Occurred Flag}
 BCM2835_PWM_STA_GAPO3 = (1 shl 6);  {Channel 3 Gap Occurred Flag}
 BCM2835_PWM_STA_GAPO2 = (1 shl 5);  {Channel 2 Gap Occurred Flag}
 BCM2835_PWM_STA_GAPO1 = (1 shl 4);  {Channel 1 Gap Occurred Flag}
 BCM2835_PWM_STA_RERR1 = (1 shl 3);  {Fifo Read Error Flag}
 BCM2835_PWM_STA_WERR1 = (1 shl 2);  {Fifo Write Error Flag}
 BCM2835_PWM_STA_EMPT1 = (1 shl 1);  {Fifo Empty Flag}
 BCM2835_PWM_STA_FULL1 = (1 shl 0);  {Fifo Full Flag}

 {Pulse Width Modulator (PWM) DMA configuration register bits (See 9.6)}
 BCM2835_PWM_DMAC_ENAB  = (1 shl 31);  {DMA Enable (0: DMA disabled / 1: DMA enabled)}
 BCM2835_PWM_DMAC_PANIC = ($FF shl 8); {DMA Threshold for PANIC signal (Default: 0x7)}
 BCM2835_PWM_DMAC_DREQ  = ($FF shl 0); {DMA Threshold for DREQ signal (Default: 0x7)}

 {Pulse Width Modulator (PWM) Registers}
 BCM2835_PWM_CTL  = $00000000; {PWM Control}
 BCM2835_PWM_STA  = $00000004; {PWM Status}
 BCM2835_PWM_DMAC = $00000008; {PWM DMA Configuration}
 BCM2835_PWM_RNG1 = $00000010; {PWM Channel 1 Range}
 BCM2835_PWM_DAT1 = $00000014; {PWM Channel 1 Data}
 BCM2835_PWM_FIF1 = $00000018; {PWM FIFO Input}
 BCM2835_PWM_RNG2 = $00000020; {PWM Channel 2 Range}
 BCM2835_PWM_DAT2 = $00000024; {PWM Channel 2 Data}

const
 {PL011 UART Data register bits (See 13.4)}
 BCM2835_PL011_DR_OE    = (1 shl 11);   {Overrun error}
 BCM2835_PL011_DR_BE    = (1 shl 10);   {Break error}
 BCM2835_PL011_DR_PE    = (1 shl 9);    {Parity error}
 BCM2835_PL011_DR_FE    = (1 shl 8);    {Framing error}
 BCM2835_PL011_DR_DATA  = ($FF shl 0);  {Receive / Transmit data}
 BCM2835_PL011_DR_ERROR = BCM2835_PL011_DR_OE or BCM2835_PL011_DR_BE or BCM2835_PL011_DR_PE or BCM2835_PL011_DR_FE;

 {PL011 UART Receive Status / Error Clear register bits (See 13.4)}
 BCM2835_PL011_RSRECR_OE = (1 shl 3); {Overrun error}
 BCM2835_PL011_RSRECR_BE = (1 shl 2); {Break error}
 BCM2835_PL011_RSRECR_PE = (1 shl 1); {Parity error}
 BCM2835_PL011_RSRECR_FE = (1 shl 0); {Framing error}

 {PL011 UART Flag register bits (See 13.4)}
 BCM2835_PL011_FR_RI   = (1 shl 8); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_FR_TXFE = (1 shl 7); {Transmit FIFO empty}
 BCM2835_PL011_FR_RXFF = (1 shl 6); {Receive FIFO full}
 BCM2835_PL011_FR_TXFF = (1 shl 5); {Transmit FIFO full}
 BCM2835_PL011_FR_RXFE = (1 shl 4); {Receive FIFO empty}
 BCM2835_PL011_FR_BUSY = (1 shl 3); {UART busy}
 BCM2835_PL011_FR_DCD  = (1 shl 2); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_FR_DSR  = (1 shl 1); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_FR_CTS  = (1 shl 0); {Clear to send (This bit is the complement of the UART clear to send, nUARTCTS, modem status input. That is, the bit is 1 when nUARTCTS is LOW)}

 {PL011 UART IrDA register bits (See 13.4)}
  {This register is disabled, writing to it has no effect and reading returns 0}

 {PL011 UART Integer Baud Rate Divisor register bits (See 13.4)}
 BCM2835_PL011_IBRD_MASK = ($FFFF shl 0);

 {PL011 UART Fractional Baud Rate Divisor register bits (See 13.4)}
 BCM2835_PL011_FBRD_MASK = ($3F shl 0);

 {PL011 UART Line Control register bits (See 13.4)}
 BCM2835_PL011_LCRH_SPS   = (1 shl 7); {Stick parity select}
 BCM2835_PL011_LCRH_WLEN  = (3 shl 5); {Word length}
 BCM2835_PL011_LCRH_WLEN8 = (3 shl 5); { 8 bits}
 BCM2835_PL011_LCRH_WLEN7 = (2 shl 5); { 7 bits}
 BCM2835_PL011_LCRH_WLEN6 = (1 shl 5); { 6 bits}
 BCM2835_PL011_LCRH_WLEN5 = (0 shl 5); { 5 bits}
 BCM2835_PL011_LCRH_FEN   = (1 shl 4); {Enable FIFOs}
 BCM2835_PL011_LCRH_STP2  = (1 shl 3); {Two stop bits select}
 BCM2835_PL011_LCRH_EPS   = (1 shl 2); {Even parity select (0 = odd parity / 1 = even parity)}
 BCM2835_PL011_LCRH_PEN   = (1 shl 1); {Parity enable}
 BCM2835_PL011_LCRH_BRK   = (1 shl 0); {Send break}

 {PL011 UART Control register bits (See 13.4)}
 BCM2835_PL011_CR_CTSEN  = (1 shl 15); {CTS hardware flow control enable (If this bit is set to 1 data is only transmitted when the nUARTCTS signal is asserted)}
 BCM2835_PL011_CR_RTSEN  = (1 shl 14); {RTS hardware flow control enable (If this bit is set to 1 data is only requested when there is space in the receive FIFO for it to be received)}
 BCM2835_PL011_CR_OUT2   = (1 shl 13); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_CR_OUT1   = (1 shl 12); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_CR_RTS    = (1 shl 11); {Request to send (This bit is the complement of the UART request to send, nUARTRTS, modem status output. That is, when the bit is programmed to a 1 then nUARTRTS is LOW)}
 BCM2835_PL011_CR_DTR    = (1 shl 10); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_CR_RXE    = (1 shl 9);  {Receive enable}
 BCM2835_PL011_CR_TXE    = (1 shl 8);  {Transmit enable}
 BCM2835_PL011_CR_LBE    = (1 shl 7);  {Loopback enable}
 {Bits 6:3 Reserved - Write as 0, read as don't care}
 BCM2835_PL011_CR_SIRLP  = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_CR_SIREN  = (1 shl 1);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_CR_UARTEN = (1 shl 0);  {UART enable}

 {PL011 UART Interrupt FIFO Level Select register bits (See 13.4)}
 BCM2835_PL011_IFLS_RXIFPSEL    = (7 shl 9); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_IFLS_TXIFPSEL    = (7 shl 6); {Unsupported, write zero, read as don't care}
 BCM2835_PL011_IFLS_RXIFLSEL    = (7 shl 3); {Receive interrupt FIFO level select}
 BCM2835_PL011_IFLS_RXIFLSEL1_8 = (0 shl 3); { b000 = Receive FIFO becomes >= 1/8 full}
 BCM2835_PL011_IFLS_RXIFLSEL1_4 = (1 shl 3); { b001 = Receive FIFO becomes >= 1/4 full}
 BCM2835_PL011_IFLS_RXIFLSEL1_2 = (2 shl 3); { b010 = Receive FIFO becomes >= 1/2 full}
 BCM2835_PL011_IFLS_RXIFLSEL3_4 = (3 shl 3); { b011 = Receive FIFO becomes >= 3/4 full}
 BCM2835_PL011_IFLS_RXIFLSEL7_8 = (4 shl 3); { b100 = Receive FIFO becomes >= 7/8 full}
 BCM2835_PL011_IFLS_TXIFLSEL    = (7 shl 0); {Transmit interrupt FIFO level select}
 BCM2835_PL011_IFLS_TXIFLSEL1_8 = (0 shl 0); { b000 = Transmit FIFO becomes <= 1/8 full}
 BCM2835_PL011_IFLS_TXIFLSEL1_4 = (1 shl 0); { b001 = Transmit FIFO becomes <= 1/4 full}
 BCM2835_PL011_IFLS_TXIFLSEL1_2 = (2 shl 0); { b010 = Transmit FIFO becomes <= 1/2 full}
 BCM2835_PL011_IFLS_TXIFLSEL3_4 = (3 shl 0); { b011 = Transmit FIFO becomes <= 3/4 full}
 BCM2835_PL011_IFLS_TXIFLSEL7_8 = (4 shl 0); { b100 = Transmit FIFO becomes <= 7/8 full}

 {PL011 UART Interrupt Mask Set/Clear register bits (See 13.4)}
 BCM2835_PL011_IMSC_OEIM   = (1 shl 10); {Overrun error interrupt mask}
 BCM2835_PL011_IMSC_BEIM   = (1 shl 9);  {Break error interrupt mask}
 BCM2835_PL011_IMSC_PEIM   = (1 shl 8);  {Parity error interrupt mask}
 BCM2835_PL011_IMSC_FEIM   = (1 shl 7);  {Framing error interrupt mask}
 BCM2835_PL011_IMSC_RTIM   = (1 shl 6);  {Receive timeout interrupt mask}
 BCM2835_PL011_IMSC_TXIM   = (1 shl 5);  {Transmit interrupt mask}
 BCM2835_PL011_IMSC_RXIM   = (1 shl 4);  {Receive interrupt mask}
 BCM2835_PL011_IMSC_DSRMIM = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_IMSC_DCDMIM = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_IMSC_CTSMIM = (1 shl 1);  {nUARTCTS modem interrupt mask}
 BCM2835_PL011_IMSC_RIMIM  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART Raw Interrupt Status register bits (See 13.4)}
 BCM2835_PL011_RIS_OERIS   = (1 shl 10); {Overrun error interrupt status}
 BCM2835_PL011_RIS_BERIS   = (1 shl 9);  {Break error interrupt status}
 BCM2835_PL011_RIS_PERIS   = (1 shl 8);  {Parity error interrupt status}
 BCM2835_PL011_RIS_FERIS   = (1 shl 7);  {Framing error interrupt status}
 BCM2835_PL011_RIS_RTRIS   = (1 shl 6);  {Receive timeout interrupt status}
 BCM2835_PL011_RIS_TXRIS   = (1 shl 5);  {Transmit interrupt status}
 BCM2835_PL011_RIS_RXRIS   = (1 shl 4);  {Receive interrupt status}
 BCM2835_PL011_RIS_DSRMRIS = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_RIS_DCDMRIS = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_RIS_CTSMRIS = (1 shl 1);  {nUARTCTS modem interrupt status}
 BCM2835_PL011_RIS_RIMRIS  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART Masked Interrupt Status register bits (See 13.4)}
 BCM2835_PL011_MIS_OEMIS   = (1 shl 10); {Overrun error masked interrupt status}
 BCM2835_PL011_MIS_BEMIS   = (1 shl 9);  {Break error masked interrupt status}
 BCM2835_PL011_MIS_PEMIS   = (1 shl 8);  {Parity error masked interrupt status}
 BCM2835_PL011_MIS_FEMIS   = (1 shl 7);  {Framing error masked interrupt status}
 BCM2835_PL011_MIS_RTMIS   = (1 shl 6);  {Receive timeout masked interrupt status}
 BCM2835_PL011_MIS_TXMIS   = (1 shl 5);  {Transmit masked interrupt status}
 BCM2835_PL011_MIS_RXMIS   = (1 shl 4);  {Receive masked interrupt status}
 BCM2835_PL011_MIS_DSRMMIS = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_MIS_DCDMMIS = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_MIS_CTSMMIS = (1 shl 1);  {nUARTCTS modem masked interrupt status}
 BCM2835_PL011_MIS_RIMMIS  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART Interrupt Clear register bits (See 13.4)}
 BCM2835_PL011_ICR_OEIC   = (1 shl 10); {Overrun error interrupt clear}
 BCM2835_PL011_ICR_BEIC   = (1 shl 9);  {Break error interrupt clear}
 BCM2835_PL011_ICR_PEIC   = (1 shl 8);  {Parity error interrupt clear}
 BCM2835_PL011_ICR_FEIC   = (1 shl 7);  {Framing error interrupt clear}
 BCM2835_PL011_ICR_RTIC   = (1 shl 6);  {Receive timeout interrupt clear}
 BCM2835_PL011_ICR_TXIC   = (1 shl 5);  {Transmit interrupt clear}
 BCM2835_PL011_ICR_RXIC   = (1 shl 4);  {Receive interrupt clear}
 BCM2835_PL011_ICR_DSRMIC = (1 shl 3);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_ICR_DCDMIC = (1 shl 2);  {Unsupported, write zero, read as don't care}
 BCM2835_PL011_ICR_CTSMIC = (1 shl 1);  {nUARTCTS modem interrupt clear}
 BCM2835_PL011_ICR_RIMIC  = (1 shl 0);  {Unsupported, write zero, read as don't care}

 {PL011 UART DMA Control register bits (See 13.4)}
  {This register is disabled, writing to it has no effect and reading returns 0}

 {PL011 UART Test Control register bits (See 13.4)}

 {PL011 UART Integration Test Input register bits (See 13.4)}

 {PL011 UART Integration Test Output register bits (See 13.4)}

 {PL011 UART Test Data register bits (See 13.4)}

const
 {ARM Interrupt Controller registers (See 7.5)}
 BCM2835_ARM_INTERRUPT_BASIC_PENDING = $00000000; {IRQ basic pending}
 BCM2835_ARM_INTERRUPT_IRQ_PENDING1  = $00000004; {IRQ pending 1}
 BCM2835_ARM_INTERRUPT_IRQ_PENDING2  = $00000008; {IRQ pending 2}
 BCM2835_ARM_INTERRUPT_FIQ_CONTROL   = $0000000C; {FIQ control}
 BCM2835_ARM_INTERRUPT_IRQ_ENABLE1   = $00000010; {Enable IRQs 1}
 BCM2835_ARM_INTERRUPT_IRQ_ENABLE2   = $00000014; {Enable IRQs 2}
 BCM2835_ARM_INTERRUPT_BASIC_ENABLE  = $00000018; {Enable Basic IRQs}
 BCM2835_ARM_INTERRUPT_IRQ_DISABLE1  = $0000001C; {Disable IRQs 1}
 BCM2835_ARM_INTERRUPT_IRQ_DISABLE2  = $00000020; {Disable IRQs 2}
 BCM2835_ARM_INTERRUPT_BASIC_DISABLE = $00000024; {Disable Basic IRQs}

 {ARM Interrupt Controller register bits (See 7.5)}
 BCM2835_ARM_INTERRUPT_FIQ_ENABLE = (1 shl 7);   {FIQ enable (Set this bit to 1 to enable FIQ generation. If set to 0 bits 6:0 are don't care)}
 BCM2835_ARM_INTERRUPT_FIQ_SOURCE = ($7F shl 0); {Select FIQ Source (0..127)}

const
 {ARM Timer register bits (See 14.2)}
 BCM2835_ARM_TIMER_CONTROL_COUNTER_PRESCALE = ($FF shl 16); {Free running counter pre-scaler (Freq is sys_clk/(prescale+1))}
 BCM2835_ARM_TIMER_CONTROL_COUNTER_ENABLED  = (1 shl 9);    {0 : Free running counter Disabled / 1 : Free running counter Enabled}
 BCM2835_ARM_TIMER_CONTROL_DEBUG_HALT       = (1 shl 8);    {0 : Timers keeps running if ARM is in debug halted mode / 1 : Timers halted if ARM is in debug halted mode}
 BCM2835_ARM_TIMER_CONTROL_TIMER_ENABLED    = (1 shl 7);    {0 : Timer disabled / 1 : Timer enabled}
 BCM2835_ARM_TIMER_CONTROL_INT_ENABLED      = (1 shl 5);    {0 : Timer interrupt disabled / 1 : Timer interrupt enabled}
 BCM2835_ARM_TIMER_CONTROL_PRESCALE         = (3 shl 2);    {Pre-scale bits: 00 : pre-scale is clock / 1 (No pre-scale) / 01 : pre-scale is clock / 16 / 10 : pre-scale is clock / 256 / 11 : pre-scale is clock / 1}
 BCM2835_ARM_TIMER_CONTROL_32BIT            = (1 shl 1);    {0 : 16-bit counters / 1 : 32-bit counter}
 BCM2835_ARM_TIMER_CONTROL_ONESHOT          = (1 shl 0);    {0 = wrapping mode (default) / 1 = one-shot mode (Not supported by BCM2835)}

 BCM2835_ARM_TIMER_RAW_IRQ_PENDING = (1 shl 0); {0 : The interrupt pending bits is clear / 1 : The interrupt pending bit is set}

 BCM2835_ARM_TIMER_MASKED_IRQ_PENDING = (1 shl 0); {0 : Interrupt line not asserted / 1 :Interrupt line is asserted, (the interrupt pending and the interrupt enable bit are set)}

 BCM2835_ARM_TIMER_PREDIVIDER_MASK = ($3FF shl 0); {Pre-divider value (timer_clock = apb_clock/(pre_divider+1))}

const
 {Power Management, Reset controller and Watchdog}
 BCM2835_PM_PASSWORD               = $5A000000;

 BCM2835_PM_RSTC_WRCFG_CLR         = $FFFFFFCF;
 BCM2835_PM_RSTC_WRCFG_SET           = $00000030;
 BCM2835_PM_RSTC_WRCFG_FULL_RESET  = $00000020;
 BCM2835_PM_RSTC_RESET               = $00000102;

 BCM2835_PM_RSTS_HADPOR_SET        = $00001000;
 BCM2835_PM_RSTS_HADSRH_SET        = $00000400;
 BCM2835_PM_RSTS_HADSRF_SET        = $00000200;
 BCM2835_PM_RSTS_HADSRQ_SET        = $00000100;
 BCM2835_PM_RSTS_HADWRH_SET        = $00000040;
 BCM2835_PM_RSTS_HADWRF_SET        = $00000020;
 BCM2835_PM_RSTS_HADWRQ_SET        = $00000010;
 BCM2835_PM_RSTS_HADDRH_SET        = $00000004;
 BCM2835_PM_RSTS_HADDRF_SET        = $00000002;
 BCM2835_PM_RSTS_HADDRQ_SET        = $00000001;

 BCM2835_PM_RSTS_RASPBERRYPI_HALT  = $00000555; {Special value to tell the Raspberry Pi firmware not to reboot, sets Boot Partition to 63 (0x3f)}

 BCM2835_PM_WDOG_RESET                = $00000000;
 BCM2835_PM_WDOG_TIME_MASK           = $000FFFFF;

 BCM2835_PM_WDOG_TICKS_PER_SECOND      = (1 shl 16);
 BCM2835_PM_WDOG_TICKS_PER_MILLISECOND = (BCM2835_PM_WDOG_TICKS_PER_SECOND div 1000);

const
 {Random Number Generator}
 BCM2835_RANDOM_DISABLE       = $00000000; {Disable Random Number Generator}
 BCM2835_RANDOM_ENABLE        = $00000001; {Enable Random Number Generator}
 BCM2835_RANDOM_DOUBLE_SPEED  = $00000002; {Double Speed Mode (Less Random)}

const
 {Clock Management (See Section 6)}
 BCM2835_CM_PASSWORD               = $5A000000;

 {Clock Manager CM_*CTL register bits (See 6.3)}
 BCM2835_CM_CTL_MASH_0         = (0 shl 9); {MASH control - 0 = integer division}
 BCM2835_CM_CTL_MASH_1         = (1 shl 9); {MASH control - 1 = 1-stage MASH (equivalent to non-MASH dividers)}
 BCM2835_CM_CTL_MASH_2         = (2 shl 9); {MASH control - 2 = 2-stage MASH}
 BCM2835_CM_CTL_MASH_3         = (3 shl 9); {MASH control - 3 = 3-stage MASH (To avoid lock-ups and glitches do not change this control while BUSY=1 and do not change this control at the same time as asserting ENAB)}
 BCM2835_CM_CTL_FLIP           = (1 shl 8); {MASH control - Invert the clock generator output (To avoid output glitches do not switch this control while BUSY=1)}
 BCM2835_CM_CTL_BUSY           = (1 shl 7); {Clock generator is running (To avoid glitches and lock-ups, clock sources and setups must not be changed while this flag is set)}
 BCM2835_CM_CTL_GATE           = (1 shl 6); {Unused}
 BCM2835_CM_CTL_KILL           = (1 shl 5); {Kill the clock generator (0 = no action / 1 = stop and reset the clock generator) (This is intended for test/debug only)}
 BCM2835_CM_CTL_ENAB           = (1 shl 4); {Enable the clock generator}

 BCM2835_CM_CTL_SRC_GND        = (0 shl 0); {Clock source - 0 Hz GND}
 BCM2835_CM_CTL_SRC_OSC        = (1 shl 0); {Clock source - 19.2 MHz Oscillator}
 BCM2835_CM_CTL_SRC_TESTDEBUG0 = (2 shl 0); {Clock source - 0 Hz Testdebug0}
 BCM2835_CM_CTL_SRC_TESTDEBUG1 = (3 shl 0); {Clock source - 0 Hz Testdebug1}
 BCM2835_CM_CTL_SRC_PLLA       = (4 shl 0); {Clock source - 0 Hz PLLA per}
 BCM2835_CM_CTL_SRC_PLLC       = (5 shl 0); {Clock source - 1000 MHz PLLC per (changes with overclock settings)}
 BCM2835_CM_CTL_SRC_PLLD       = (6 shl 0); {Clock source - 500 MHz PLLD per}
 BCM2835_CM_CTL_SRC_HDMI       = (7 shl 0); {Clock source - 216 MHz HDMI auxiliary}

 {Clock Manager CM_*DIV register bits (See 6.3)}
 BCM2835_CM_DIV_INT_MASK  = $00FFF000; {Integer part of divisor (This value has a minimum limit determined by the MASH setting) (To avoid lock-ups and glitches do not change this control while BUSY=1)}
 BCM2835_CM_DIV_FRAC_MASK = $00000FFF; {Fractional part of divisor (To avoid lock-ups and glitches do not change this control while BUSY=1)}

 {Clock Manager Registers}
 BCM2835_CM_GNRICCTL =  $00000000; {Generic Clock Control}
 BCM2835_CM_GNRICDIV =  $00000004; {Generic Clock Divisor}
 BCM2835_CM_VPUCTL   =  $00000008; {VPU Clock Control}
 BCM2835_CM_VPUDIV   =  $0000000C; {VPU Clock Divisor}
 BCM2835_CM_SYSCTL   =  $00000010; {System Clock Control}
 BCM2835_CM_SYSDIV   =  $00000014; {System Clock Divisor}
 BCM2835_CM_PERIACTL =  $00000018; {PERIA Clock Control}
 BCM2835_CM_PERIADIV =  $0000001C; {PERIA Clock Divisor}
 BCM2835_CM_PERIICTL =  $00000020; {PERII Clock Control}
 BCM2835_CM_PERIIDIV =  $00000024; {PERII Clock Divisor}
 BCM2835_CM_H264CTL  =  $00000028; {H264 Clock Control}
 BCM2835_CM_H264DIV  =  $0000002C; {H264 Clock Divisor}
 BCM2835_CM_ISPCTL   =  $00000030; {ISP Clock Control}
 BCM2835_CM_ISPDIV   =  $00000034; {ISP Clock Divisor}
 BCM2835_CM_V3DCTL   =  $00000038; {V3D Clock Control}
 BCM2835_CM_V3DDIV   =  $0000003C; {V3D Clock Divisor}
 BCM2835_CM_CAM0CTL  =  $00000040; {Camera 0 Clock Control}
 BCM2835_CM_CAM0DIV  =  $00000044; {Camera 0 Clock Divisor}
 BCM2835_CM_CAM1CTL  =  $00000048; {Camera 1 Clock Control}
 BCM2835_CM_CAM1DIV  =  $0000004C; {Camera 1 Clock Divisor}
 BCM2835_CM_CCP2CTL  =  $00000050; {CCP2 Clock Control}
 BCM2835_CM_CCP2DIV  =  $00000054; {CCP2 Clock Divisor}
 BCM2835_CM_DSI0ECTL =  $00000058; {DSI0E Clock Control}
 BCM2835_CM_DSI0EDIV =  $0000005C; {DSI0E Clock Divisor}
 BCM2835_CM_DSI0PCTL =  $00000060; {DSI0P Clock Control}
 BCM2835_CM_DSI0PDIV =  $00000064; {DSI0P Clock Divisor}
 BCM2835_CM_DPICTL   =  $00000068; {DPI Clock Control}
 BCM2835_CM_DPIDIV   =  $0000006C; {DPI Clock Divisor}
 BCM2835_CM_GP0CTL   =  $00000070; {General Purpose 0 Clock Control}
 BCM2835_CM_GP0DIV   =  $00000074; {General Purpose 0 Clock Divisor}
 BCM2835_CM_GP1CTL   =  $00000078; {General Purpose 1 Clock Control}
 BCM2835_CM_GP1DIV   =  $0000007C; {General Purpose 1 Clock Divisor}
 BCM2835_CM_GP2CTL   =  $00000080; {General Purpose 2 Clock Control}
 BCM2835_CM_GP2DIV   =  $00000084; {General Purpose 2 Clock Divisor}
 BCM2835_CM_HSMCTL   =  $00000088; {HSM Clock Control}
 BCM2835_CM_HSMDIV   =  $0000008C; {HSM Clock Divisor}
 BCM2835_CM_OTPCTL   =  $00000090; {OTP Clock Control}
 BCM2835_CM_OTPDIV   =  $00000094; {OTP Clock Divisor}
 BCM2835_CM_PCMCTL   =  $00000098; {PCM / I2S Clock Control}
 BCM2835_CM_PCMDIV   =  $0000009C; {PCM / I2S Clock Divisor}
 BCM2835_CM_PWMCTL   =  $000000A0; {PWM Clock Control}
 BCM2835_CM_PWMDIV   =  $000000A4; {PWM Clock Divisor}
 BCM2835_CM_SLIMCTL  =  $000000A8; {SLIM Clock Control}
 BCM2835_CM_SLIMDIV  =  $000000AC; {SLIM Clock Divisor}
 BCM2835_CM_SMICTL   =  $000000B0; {SMI Clock Control}
 BCM2835_CM_SMIDIV   =  $000000B4; {SMI Clock Divisor}
 BCM2835_CM_TCNTCTL  =  $000000C0; {TCNT Clock Control}
 BCM2835_CM_TCNTDIV  =  $000000C4; {TCNT Clock Divisor}
 BCM2835_CM_TECCTL   =  $000000C8; {TEC Clock Control}
 BCM2835_CM_TECDIV   =  $000000CC; {TEC Clock Divisor}
 BCM2835_CM_TD0CTL   =  $000000D0; {TD0 Clock Control}
 BCM2835_CM_TD0DIV   =  $000000D4; {TD0 Clock Divisor}
 BCM2835_CM_TD1CTL   =  $000000D8; {TD1 Clock Control}
 BCM2835_CM_TD1DIV   =  $000000DC; {TD1 Clock Divisor}
 BCM2835_CM_TSENSCTL =  $000000E0; {TSENS Clock Control}
 BCM2835_CM_TSENSDIV =  $000000E4; {TSENS Clock Divisor}
 BCM2835_CM_TIMERCTL =  $000000E8; {Timer Clock Control}
 BCM2835_CM_TIMERDIV =  $000000EC; {Timer Clock Divisor}
 BCM2835_CM_UARTCTL  =  $000000F0; {UART Clock Control}
 BCM2835_CM_UARTDIV  =  $000000F4; {UART Clock Divisor}
 BCM2835_CM_VECCTL   =  $000000F8; {VEC Clock Control}
 BCM2835_CM_VECDIV   =  $000000FC; {VEC Clock Divisor}

 BCM2835_CM_OSCCOUNT =  $00000100; {Oscillator Count}
 BCM2835_CM_PLLA     =  $00000104; {PLLA}
 BCM2835_CM_PLLC     =  $00000108; {PLLC}
 BCM2835_CM_PLLD     =  $0000010C; {PLLD}
 BCM2835_CM_PLLH     =  $00000110; {PLLH}
 BCM2835_CM_LOCK     =  $00000114; {Lock}
 BCM2835_CM_EVENT    =  $00000118; {Event}
 BCM2835_CM_INTEN    =  $00000118; {INTEN}
 BCM2835_CM_DSI0HSCK =  $00000120; {DSI0HSCK}
 BCM2835_CM_CKSM     =  $00000124; {CKSM}
 BCM2835_CM_OSCFREQI =  $00000128; {Oscillator Frequency Integer}
 BCM2835_CM_OSCFREQF =  $0000012C; {Oscillator Frequency Fraction}
 BCM2835_CM_PLLTCTL  =  $00000130; {PLLT Control}
 BCM2835_CM_PLLTCNT0 =  $00000134; {PLLT0 Count}
 BCM2835_CM_PLLTCNT1 =  $00000138; {PLLT1 Count}
 BCM2835_CM_PLLTCNT2 =  $0000013C; {PLLT2 Count}
 BCM2835_CM_PLLTCNT3 =  $00000140; {PLLT3 Count}
 BCM2835_CM_TDCLKEN  =  $00000144; {TD Clock Enable}
 BCM2835_CM_BURSTCTL =  $00000148; {Burst Control}
 BCM2835_CM_BURSTCNT =  $0000014C; {Burst Count}
 BCM2835_CM_DSI1ECTL =  $00000158; {DSI1E Clock Control}
 BCM2835_CM_DSI1EDIV =  $0000015C; {DSI1E Clock Divisor}
 BCM2835_CM_DSI1PCTL =  $00000160; {DSI1P Clock Control}
 BCM2835_CM_DSI1PDIV =  $00000164; {DSI1P Clock Divisor}
 BCM2835_CM_DFTCTL   =  $00000168; {DFT Clock Control}
 BCM2835_CM_DFTDIV   =  $0000016C; {DFT Clock Divisor}
 BCM2835_CM_PLLB     =  $00000170; {PLLB}

 BCM2835_CM_PULSECTL =  $00000190; {Pulse Clock Control}
 BCM2835_CM_PULSEDIV =  $00000194; {Pulse Clock Divisor}
 BCM2835_CM_SDCCTL   =  $000001A8; {SDC Clock Control}
 BCM2835_CM_SDCDIV   =  $000001AC; {SDC Clock Divisor}
 BCM2835_CM_ARMCTL   =  $000001B0; {ARM Clock Control}
 BCM2835_CM_ARMDIV   =  $000001B4; {ARM Clock Divisor}
 BCM2835_CM_AVEOCTL  =  $000001B8; {AVEO Clock Control}
 BCM2835_CM_AVEODIV  =  $000001BC; {AVEO Clock Divisor}
 BCM2835_CM_EMMCCTL  =  $000001C0; {EMMC Clock Control}
 BCM2835_CM_EMMCDIV  =  $000001C4; {EMMC Clock Divisor}

const
 {BCM2835 Mailboxes}
 BCM2835_MAILBOX_0  = 0;
 BCM2835_MAILBOX_1  = 1;

 {BCM2835 Mailbox 0 channels (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
 BCM2835_MAILBOX0_CHANNEL_POWER_MGMT         = 0;
 BCM2835_MAILBOX0_CHANNEL_FRAMEBUFFER        = 1;
 BCM2835_MAILBOX0_CHANNEL_UART               = 2;
 BCM2835_MAILBOX0_CHANNEL_VCHIQ              = 3;
 BCM2835_MAILBOX0_CHANNEL_LEDS               = 4;
 BCM2835_MAILBOX0_CHANNEL_BUTTONS            = 5;
 BCM2835_MAILBOX0_CHANNEL_TOUCHSCREEN        = 6;
 BCM2835_MAILBOX0_CHANNEL_UNKNOWN            = 7;
 BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = 8;
 BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_VCARM = 9;

 {BCM2835 Mailbox 1 channels (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
  {Currently unknown}

 {The BCM2835 mailboxes pass 28-bit messages (The low 4 bits of the 32-bit value are used to specify the channel)}
 BCM2835_MAILBOX_CHANNEL_MASK = $0000000F;
 BCM2835_MAILBOX_DATA_MASK    = $FFFFFFF0;

 {BCM2835 mailbox status flags}
 BCM2835_MAILBOX_STATUS_FULL  = $80000000;
 BCM2835_MAILBOX_STATUS_EMPTY = $40000000;

 {BCM2835 mailbox configuration flags}
 BCM2835_MAILBOX_CONFIG_IRQENABLE = $00000001;

 {BCM2835 mailbox property tags (See https://github.com/raspberrypi/firmware/wiki/Mailbox-property-interface)(or \include\soc\bcm2835\raspberrypi-firmware.h)}
 {VideoCore}
 BCM2835_MBOX_TAG_GET_FIRMWARE_REV  = $00000001;
 {Hardware}
 BCM2835_MBOX_TAG_GET_BOARD_MODEL   = $00010001;
 BCM2835_MBOX_TAG_GET_BOARD_REV        = $00010002;

 BCM2835_MBOX_TAG_GET_MAC_ADDRESS    = $00010003;

 BCM2835_MBOX_TAG_GET_BOARD_SERIAL  = $00010004;

 BCM2835_MBOX_TAG_GET_ARM_MEMORY    = $00010005;
 BCM2835_MBOX_TAG_GET_VC_MEMORY        = $00010006;

 BCM2835_MBOX_TAG_GET_CLOCKS        = $00010007;
 {Shared Resource Management}
 BCM2835_MBOX_TAG_GET_POWER_STATE    = $00020001; {Response indicates current state}
 BCM2835_MBOX_TAG_GET_TIMING        = $00020002; {Response indicates wait time required after turning a device on before power is stable}
 BCM2835_MBOX_TAG_SET_POWER_STATE    = $00028001; {Response indicates new state, with/without waiting for the power to become stable}

 BCM2835_MBOX_TAG_GET_CLOCK_STATE   = $00030001;
 BCM2835_MBOX_TAG_SET_CLOCK_STATE   = $00038001;

 BCM2835_MBOX_TAG_GET_CLOCK_RATE    = $00030002;
 BCM2835_MBOX_TAG_SET_CLOCK_RATE    = $00038002;

 BCM2835_MBOX_TAG_GET_CLOCK_MAX_RATE = $00030004; {Return the maximum supported clock rate for the given clock. Clocks should not be set higher than this}
 BCM2835_MBOX_TAG_GET_CLOCK_MIN_RATE = $00030007; {Return the minimum supported clock rate for the given clock. This may be used when idle}

 BCM2835_MBOX_TAG_GET_TURBO         = $00030009; {Get the turbo state for index id. id should be 0. level will be zero for non-turbo and one for turbo}
 BCM2835_MBOX_TAG_SET_TURBO         = $00038009; {Set the turbo state for index id. id should be zero. level will be zero for non-turbo and one for turbo. This will cause GPU clocks to be set to maximum when enabled and minimum when disabled}

 BCM2835_MBOX_TAG_GET_STC           = $0003000b; {}
 {Voltage}
 BCM2835_MBOX_TAG_GET_VOLTAGE       = $00030003; {The voltage value may be clamped to the supported range. A value of 0x80000000 means the id was not valid}
 BCM2835_MBOX_TAG_SET_VOLTAGE       = $00038003; {The voltage value may be clamped to the supported range. A value of 0x80000000 means the id was not valid}

 BCM2835_MBOX_TAG_GET_MAX_VOLTAGE   = $00030005; {Return the maximum supported voltage rate for the given id. Voltages should not be set higher than this}
 BCM2835_MBOX_TAG_GET_MIN_VOLTAGE   = $00030008; {Return the minimum supported voltage rate for the given id. This may be used when idle}

 BCM2835_MBOX_TAG_GET_TEMP          = $00030006; {Return the temperature of the SoC in thousandths of a degree C. id should be zero}
 BCM2835_MBOX_TAG_GET_MAX_TEMP      = $0003000a; {Return the maximum safe temperature of the SoC in thousandths of a degree C. id should be zero. Overclock may be disabled above this temperature}
 {GPU}
 BCM2835_MBOX_TAG_ALLOCATE_MEMORY   = $0003000c; {Allocates contiguous memory on the GPU. size and alignment are in bytes}
 BCM2835_MBOX_TAG_LOCK_MEMORY       = $0003000d; {Lock buffer in place, and return a bus address. Must be done before memory can be accessed}
 BCM2835_MBOX_TAG_UNLOCK_MEMORY     = $0003000e; {Unlock buffer. It retains contents, but may move. Needs to be locked before next use. status=0 is success}
 BCM2835_MBOX_TAG_RELEASE_MEMORY    = $0003000f; {Free the memory buffer. status=0 is success}

 BCM2835_MBOX_TAG_EXECUTE_CODE      = $00030010; {Calls the function at given (bus) address and with arguments given. E.g. r0 = fn(r0, r1, r2, r3, r4, r5); It blocks until call completes}
 BCM2835_MBOX_TAG_EXECUTE_QPU       = $00030011; {Execute an assembled block of code using one or more Quad Processing Units (QPUs)}
 BCM2835_MBOX_TAG_ENABLE_QPU        = $00030012; {Enable the Quad Processing Units (QPUs)}

 BCM2835_MBOX_TAG_GET_DISPMANX_HANDLE = $00030014; {Gets the mem_handle associated with a created dispmanx resource. This can be locked and the memory directly written from the arm to avoid having to copy the image data to GPU}
 BCM2835_MBOX_TAG_GET_EDID_BLOCK    = $00030020; {This reads the specified EDID block from attached HDMI/DVI device. There will always be at least one block of 128 bytes, but there may be additional blocks. You should keep requesting blocks (starting from 0) until the status returned is non-zero}

 BCM2835_MBOX_TAG_GET_CUSTOMER_OTP  = $00030021;
 BCM2835_MBOX_TAG_SET_CUSTOMER_OTP  = $00038021;

 BCM2835_MBOX_TAG_GET_DOMAIN_STATE  = $00030030;
 BCM2835_MBOX_TAG_SET_DOMAIN_STATE  = $00038030;

 BCM2835_MBOX_TAG_GET_GPIO_STATE    = $00030041; {Get the current state of a GPIO expander pin (Not applicable on BCM2835)}
 BCM2835_MBOX_TAG_SET_GPIO_STATE    = $00038041; {Set the current state of a GPIO expander pin (Not applicable on BCM2835)}

 BCM2835_MBOX_TAG_SET_SDHOST_CLOCK  = $00038042; {Tell the firmware the SD Host clock setting so it will be adjusted for changes in core frequency}

 BCM2835_MBOX_TAG_GET_GPIO_CONFIG   = $00030043; {Get the current configuration of a GPIO expander pin (Not applicable on BCM2835)}
 BCM2835_MBOX_TAG_SET_GPIO_CONFIG   = $00038043; {Set the current configuration of a GPIO expander pin (Not applicable on BCM2835)}

 BCM2835_MBOX_TAG_GET_THROTTLED     = $00030046;

 BCM2835_MBOX_TAG_GET_CLOCK_MEASURED = $00030047;

 BCM2835_MBOX_TAG_GET_PERIPH_REG    = $00030045;
 BCM2835_MBOX_TAG_SET_PERIPH_REG    = $00038045;

 BCM2835_MBOX_TAG_GET_POE_HAT_VAL   = $00030049;
 BCM2835_MBOX_TAG_SET_POE_HAT_VAL   = $00038049;

 BCM2835_MBOX_TAG_NOTIFY_REBOOT     = $00030048;
 BCM2835_MBOX_TAG_NOTIFY_XHCI_RESET = $00030058;

 BCM2835_MBOX_TAG_GET_BOOT_MODE     = $0003005b; {Get the boot mode that was used by the firmware to load the kernel (1 = SD, 4 = MSD etc)}

 BCM2835_MBOX_TAG_GET_REBOOT_FLAGS  = $00030064;
 BCM2835_MBOX_TAG_SET_REBOOT_FLAGS  = $00038064;

 BCM2835_MBOX_TAG_NOTIFY_DISPLAY_DONE = $00030066;

 {Frame Buffer}
 BCM2835_MBOX_TAG_ALLOCATE_BUFFER    = $00040001; {If the requested alignment is unsupported then the current base and size (which may be 0 if not allocated) is returned and no change occurs}
 BCM2835_MBOX_TAG_RELEASE_BUFFER    = $00048001; {Releases and disables the frame buffer}

 BCM2835_MBOX_TAG_SET_BLANK_SCREEN  = $00040002;
 BCM2835_MBOX_TAG_TEST_BLANK_SCREEN = $00044002; {Previously BCM2835_MBOX_TAG_TST_BLANK_SCREEN}

 BCM2835_MBOX_TAG_GET_PHYSICAL_W_H    = $00040003; {Note that the "physical (display)" size is the size of the allocated buffer in memory, not the resolution of the video signal sent to the display device}
 BCM2835_MBOX_TAG_TEST_PHYSICAL_W_H    = $00044003;
 BCM2835_MBOX_TAG_SET_PHYSICAL_W_H    = $00048003;

 BCM2835_MBOX_TAG_GET_VIRTUAL_W_H    = $00040004; {Note that the "virtual (buffer)" size is the portion of buffer that is sent to the display device, not the resolution the buffer itself. This may be smaller than the allocated buffer size in order to implement panning}
 BCM2835_MBOX_TAG_TEST_VIRTUAL_W_H    = $00044004;
 BCM2835_MBOX_TAG_SET_VIRTUAL_W_H    = $00048004;

 BCM2835_MBOX_TAG_GET_DEPTH            = $00040005;
 BCM2835_MBOX_TAG_TEST_DEPTH        = $00044005;
 BCM2835_MBOX_TAG_SET_DEPTH            = $00048005;

 BCM2835_MBOX_TAG_GET_PIXEL_ORDER    = $00040006;
 BCM2835_MBOX_TAG_TEST_PIXEL_ORDER    = $00044006;
 BCM2835_MBOX_TAG_SET_PIXEL_ORDER    = $00048006;

 BCM2835_MBOX_TAG_GET_ALPHA_MODE    = $00040007;
 BCM2835_MBOX_TAG_TEST_ALPHA_MODE    = $00044007;
 BCM2835_MBOX_TAG_SET_ALPHA_MODE    = $00048007;

 BCM2835_MBOX_TAG_GET_PITCH            = $00040008;
 BCM2835_MBOX_TAG_TEST_PITCH        = $00044008; {Previously BCM2835_MBOX_TAG_TST_PITCH}
 BCM2835_MBOX_TAG_SET_PITCH         = $00048008;

 BCM2835_MBOX_TAG_GET_VIRTUAL_OFFSET    = $00040009; {Offset of physical display window within virtual buffer}
 BCM2835_MBOX_TAG_TEST_VIRTUAL_OFFSET    = $00044009;
 BCM2835_MBOX_TAG_SET_VIRTUAL_OFFSET    = $00048009;

 BCM2835_MBOX_TAG_GET_OVERSCAN        = $0004000a;
 BCM2835_MBOX_TAG_TEST_OVERSCAN        = $0004400a;
 BCM2835_MBOX_TAG_SET_OVERSCAN        = $0004800a;

 BCM2835_MBOX_TAG_GET_PALETTE        = $0004000b;
 BCM2835_MBOX_TAG_TEST_PALETTE        = $0004400b;
 BCM2835_MBOX_TAG_SET_PALETTE        = $0004800b;

 BCM2835_MBOX_TAG_GET_LAYER         = $0004000c;
 BCM2835_MBOX_TAG_TEST_LAYER        = $0004400c; {Previously BCM2835_MBOX_TAG_TST_LAYER}
 BCM2835_MBOX_TAG_SET_LAYER         = $0004800c;

 BCM2835_MBOX_TAG_GET_TRANSFORM     = $0004000d;
 BCM2835_MBOX_TAG_TEST_TRANSFORM    = $0004400d; {Previously BCM2835_MBOX_TAG_TST_TRANSFORM}
 BCM2835_MBOX_TAG_SET_TRANSFORM     = $0004800d;

 BCM2835_MBOX_TAG_TEST_VSYNC        = $0004400e; {Previously BCM2835_MBOX_TAG_TST_VSYNC}
 BCM2835_MBOX_TAG_SET_VSYNC         = $0004800e;

 BCM2835_MBOX_TAG_SET_BACKLIGHT     = $0004800f;

 BCM2835_MBOX_TAG_GET_TOUCHBUF      = $0004000f;
 BCM2835_MBOX_TAG_SET_TOUCHBUF      = $0004801f;

 BCM2835_MBOX_TAG_GET_GPIOVIRTBUF   = $00040010;
 BCM2835_MBOX_TAG_SET_GPIOVIRTBUF   = $00048020;

 BCM2835_MBOX_TAG_GET_DISPLAY_ID       = $00040016;
 BCM2835_MBOX_TAG_SET_DISPLAY_NUM      = $00048013;
 BCM2835_MBOX_TAG_GET_NUM_DISPLAYS     = $00040013;
 BCM2835_MBOX_TAG_GET_DISPLAY_SETTINGS = $00040014;

 BCM2835_MBOX_TAG_SET_PLANE          = $00048015;
 BCM2835_MBOX_TAG_GET_DISPLAY_TIMING = $00040017;
 BCM2835_MBOX_TAG_SET_TIMING         = $00048017;
 BCM2835_MBOX_TAG_GET_DISPLAY_CFG    = $00040018;
 BCM2835_MBOX_TAG_SET_DISPLAY_POWER  = $00048019;

 BCM2835_MBOX_TAG_SET_CURSOR_INFO   = $00008010; {00008011} {These were reversed in the documentation, see Linux \include\soc\bcm2835\raspberrypi-firmware.h}
 BCM2835_MBOX_TAG_SET_CURSOR_STATE  = $00008011; {00008010}
 {VCHIQ}
 BCM2835_MBOX_TAG_VCHIQ_INIT        = $00048010;
 {Config}
 BCM2835_MBOX_TAG_GET_COMMAND_LINE  = $00050001;
 {Shared Resource Management}
 BCM2835_MBOX_TAG_GET_DMA_CHANNELS  = $00060001; {Caller assumes that the VC has enabled all the usable DMA channels}
 {End}
 BCM2835_MBOX_TAG_END               = $00000000;

 {BCM2835 mailbox tag Get Board Revision values (See: http://elinux.org/RPi_HardwareHistory)}
 BCM2835_BOARD_REV_B_I2C0_2    = $00000002;
 BCM2835_BOARD_REV_B_I2C0_3    = $00000003;
 BCM2835_BOARD_REV_B_I2C1_4    = $00000004;
 BCM2835_BOARD_REV_B_I2C1_5    = $00000005;
 BCM2835_BOARD_REV_B_I2C1_6    = $00000006;
 BCM2835_BOARD_REV_A_7        = $00000007;
 BCM2835_BOARD_REV_A_8        = $00000008;
 BCM2835_BOARD_REV_A_9        = $00000009;
 BCM2835_BOARD_REV_B_REV2_d    = $0000000D;
 BCM2835_BOARD_REV_B_REV2_e    = $0000000E;
 BCM2835_BOARD_REV_B_REV2_f    = $0000000F;
 BCM2835_BOARD_REV_B_PLUS    = $00000010;
 BCM2835_BOARD_REV_CM        = $00000011;
 BCM2835_BOARD_REV_A_PLUS    = $00000012;
 BCM2835_BOARD_REV_B_PLUS_2 = $00000013;
 BCM2835_BOARD_REV_CM_2        = $00000014;
 BCM2835_BOARD_REV_A_PLUS_2    = $00000015; {512MB version of the A+}

 BCM2835_BOARD_REV_APLUS_1  = $00900021;
 BCM2835_BOARD_REV_ZERO_1   = $00900092;
 BCM2835_BOARD_REV_ZERO_2   = $00900093;
 BCM2835_BOARD_REV_ZERO_3   = $00920093;
 BCM2835_BOARD_REV_ZERO_W_1 = $009000C1;

 BCM2835_BOARD_REV_MASK     = $00FFFFFF; {Mask off the warranty bit}

 {BCM2835 mailbox tag Get Board Revision bit fields (See: https://www.raspberrypi.org/documentation/hardware/raspberrypi/revision-codes/README.md)}
 BCM2835_BOARD_REVISION_PCB_MASK             = ($F shl 0);  {PCB Revision Number}

 BCM2835_BOARD_REVISION_MODEL_MASK           = ($FF shl 4); {Model Number}
 BCM2835_BOARD_REVISION_MODEL_A              = (0 shl 4);   {Model A}
 BCM2835_BOARD_REVISION_MODEL_B              = (1 shl 4);   {Model B}
 BCM2835_BOARD_REVISION_MODEL_APLUS          = (2 shl 4);   {Model A+}
 BCM2835_BOARD_REVISION_MODEL_BPLUS          = (3 shl 4);   {Model B+}
 BCM2835_BOARD_REVISION_MODEL_2B             = (4 shl 4);   {Model 2B (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_ALPHA          = (5 shl 4);   {Unknown}
 BCM2835_BOARD_REVISION_MODEL_COMPUTE        = (6 shl 4);   {Compute Module}
 BCM2835_BOARD_REVISION_MODEL_UNKNOWN        = (7 shl 4);   {Unknown}
 BCM2835_BOARD_REVISION_MODEL_3B             = (8 shl 4);   {Model 3B (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_ZERO           = (9 shl 4);   {Model Zero}
 BCM2835_BOARD_REVISION_MODEL_COMPUTE3       = (10 shl 4);  {Compute Module 3 (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_UNKNOWN_2      = (11 shl 4);  {Unknown}
 BCM2835_BOARD_REVISION_MODEL_ZERO_W         = (12 shl 4);  {Model Zero W}
 BCM2835_BOARD_REVISION_MODEL_3BPLUS         = (13 shl 4);  {Model 3B+ (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_3APLUS         = (14 shl 4);  {Model 3A+ (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_COMPUTE3PLUS   = (16 shl 4);  {Compute Module 3+ (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_4B             = (17 shl 4);  {Model 4B (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_ZERO2_W        = (18 shl 4);  {Model Zero 2 W (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_400            = (19 shl 4);  {Pi 400 (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_CM4            = (20 shl 4);  {Compute Module 4 (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_MODEL_CM4S           = (21 shl 4);  {Compute Module 4S (Cannot occur on BCM2835)}

 BCM2835_BOARD_REVISION_PROCESSOR_MASK       = ($F shl 12); {Processor Type}
 BCM2835_BOARD_REVISION_PROCESSOR_BCM2835    = (0 shl 12);  {BCM2835}
 BCM2835_BOARD_REVISION_PROCESSOR_BCM2836    = (1 shl 12);  {BCM2836 (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_PROCESSOR_BCM2837    = (2 shl 12);  {BCM2837 (Cannot occur on BCM2835)}
 BCM2835_BOARD_REVISION_PROCESSOR_BCM2838    = (3 shl 12);  {BCM2838 (Cannot occur on BCM2835)}

 BCM2835_BOARD_REVISION_MANUFACTURER_MASK       = ($F shl 16); {Manufacturer}
 BCM2835_BOARD_REVISION_MANUFACTURER_SONY       = (0 shl 16);  {Sony}
 BCM2835_BOARD_REVISION_MANUFACTURER_EGOMAN     = (1 shl 16);  {Egoman}
 BCM2835_BOARD_REVISION_MANUFACTURER_EMBEST     = (2 shl 16);  {Embest}
 BCM2835_BOARD_REVISION_MANUFACTURER_SONY_JAPAN = (3 shl 16);  {Sony (Japan)}
 BCM2835_BOARD_REVISION_MANUFACTURER_EMBEST2    = (4 shl 16);  {Embest}
 BCM2835_BOARD_REVISION_MANUFACTURER_STADIUM    = (5 shl 16);  {Stadium}

 BCM2835_BOARD_REVISION_MEMORY_MASK          = ($7 shl 20); {Memory Size}
 BCM2835_BOARD_REVISION_MEMORY_256M          = (0 shl 20);  {256M}
 BCM2835_BOARD_REVISION_MEMORY_512M          = (1 shl 20);  {512M}
 BCM2835_BOARD_REVISION_MEMORY_1024M         = (2 shl 20);  {1024M}
 BCM2835_BOARD_REVISION_MEMORY_2048M         = (3 shl 20);  {2048M}
 BCM2835_BOARD_REVISION_MEMORY_4096M         = (4 shl 20);  {4096M}
 BCM2835_BOARD_REVISION_MEMORY_8192M         = (5 shl 20);  {8192M}

 BCM2835_BOARD_REVISION_ENCODED_FLAG         = (1 shl 23);  {Encoded Flag, if set then revision uses this encoding}

 BCM2835_BOARD_REVISION_MASK                 = $00FFFFFF;   {Mask off the warranty bits}

 {BCM2835 mailbox tag Power State devices}
 BCM2835_MBOX_POWER_DEVID_SDHCI        = 0;
 BCM2835_MBOX_POWER_DEVID_UART0        = 1;
 BCM2835_MBOX_POWER_DEVID_UART1        = 2;
 BCM2835_MBOX_POWER_DEVID_USB_HCD    = 3;
 BCM2835_MBOX_POWER_DEVID_I2C0        = 4;
 BCM2835_MBOX_POWER_DEVID_I2C1        = 5;
 BCM2835_MBOX_POWER_DEVID_I2C2        = 6;
 BCM2835_MBOX_POWER_DEVID_SPI        = 7;
 BCM2835_MBOX_POWER_DEVID_CCP2TX    = 8;

 BCM2835_MBOX_POWER_DEVID_UNKNOWN   = $FFFFFFFF;

 {BCM2835 mailbox tag Power State requests}
 BCM2835_MBOX_SET_POWER_STATE_REQ_OFF    = (0 shl 0);
 BCM2835_MBOX_SET_POWER_STATE_REQ_ON    = (1 shl 0);
 BCM2835_MBOX_SET_POWER_STATE_REQ_WAIT    = (1 shl 1);

 {BCM2835 mailbox tag Power State values}
 BCM2835_MBOX_POWER_STATE_RESP_OFF        = (0 shl 0);
 BCM2835_MBOX_POWER_STATE_RESP_ON        = (1 shl 0);
 BCM2835_MBOX_POWER_STATE_RESP_NODEV    = (1 shl 1);  {Device doesn't exist}

 {BCM2835 mailbox tag Clock State/Rate ids}
 BCM2835_MBOX_CLOCK_ID_RESERVED = 0;
 BCM2835_MBOX_CLOCK_ID_EMMC    = 1;
 BCM2835_MBOX_CLOCK_ID_UART    = 2;
 BCM2835_MBOX_CLOCK_ID_ARM    = 3;
 BCM2835_MBOX_CLOCK_ID_CORE    = 4;
 BCM2835_MBOX_CLOCK_ID_V3D    = 5;
 BCM2835_MBOX_CLOCK_ID_H264    = 6;
 BCM2835_MBOX_CLOCK_ID_ISP    = 7;
 BCM2835_MBOX_CLOCK_ID_SDRAM = 8;
 BCM2835_MBOX_CLOCK_ID_PIXEL = 9;
 BCM2835_MBOX_CLOCK_ID_PWM    = 10;

 BCM2835_MBOX_CLOCK_ID_UNKNOWN   = $FFFFFFFF;

 {BCM2835 mailbox tag Clock State requests}
 BCM2835_MBOX_SET_CLOCK_STATE_REQ_OFF      = (0 shl 0);
 BCM2835_MBOX_SET_CLOCK_STATE_REQ_ON      = (1 shl 0);
 BCM2835_MBOX_SET_CLOCK_STATE_REQ_NOCLOCK = (1 shl 1);  {Clock doesn't exist}

 {BCM2835 mailbox tag Clock State values}
 BCM2835_MBOX_CLOCK_STATE_RESP_OFF          = (0 shl 0);
 BCM2835_MBOX_CLOCK_STATE_RESP_ON          = (1 shl 0);
 BCM2835_MBOX_CLOCK_STATE_RESP_NOCLOCK      = (1 shl 1);  {Clock doesn't exist}

 {BCM2835 mailbox tag Clock Rate turbo}
 BCM2835_MBOX_CLOCK_RATE_REQ_SKIP_TURBO  = (1 shl 0);

 {BCM2835 mailbox tag Voltage ids}
 BCM2835_MBOX_VOLTAGE_ID_RESERVED = $00000000;
 BCM2835_MBOX_VOLTAGE_ID_CORE     = $00000001;
 BCM2835_MBOX_VOLTAGE_ID_SDRAM_C  = $00000002;
 BCM2835_MBOX_VOLTAGE_ID_SDRAM_P  = $00000003;
 BCM2835_MBOX_VOLTAGE_ID_SDRAM_I  = $00000004;

 {BCM2835 mailbox tag Voltage values}
 BCM2835_MBOX_VOLTAGE_INVALID = $80000000;  {A value of 0x80000000 means the id was not valid}

 {BCM2835 mailbox tag Temperature ids}
 BCM2835_MBOX_TEMP_ID_SOC = 0;

 {BCM2835 mailbox Display ids (These are compatible with the DISPMANX_ID_* values)}
 BCM2835_MBOX_DISPLAY_ID_MAIN_LCD    = 0;
 BCM2835_MBOX_DISPLAY_ID_AUX_LCD     = 1;
 BCM2835_MBOX_DISPLAY_ID_HDMI0       = 2;
 BCM2835_MBOX_DISPLAY_ID_SDTV        = 3;
 BCM2835_MBOX_DISPLAY_ID_FORCE_LCD   = 4;
 BCM2835_MBOX_DISPLAY_ID_FORCE_TV    = 5;
 BCM2835_MBOX_DISPLAY_ID_FORCE_OTHER = 6; {Non-default display}
 BCM2835_MBOX_DISPLAY_ID_HDMI1       = 7; {Not applicable on BCM2835}
 BCM2835_MBOX_DISPLAY_ID_FORCE_TV2   = 8; {Not applicable on BCM2835}

 {BCM2835 mailbox tag Memory flags}
 BCM2835_MBOX_MEM_FLAG_DISCARDABLE      = (1 shl 0); {Can be resized to 0 at any time. Use for cached data}
 BCM2835_MBOX_MEM_FLAG_NORMAL           = (0 shl 2); {Normal allocating alias. Don't use from ARM}
 BCM2835_MBOX_MEM_FLAG_DIRECT           = (1 shl 2); {0xC alias uncached}
 BCM2835_MBOX_MEM_FLAG_COHERENT         = (2 shl 2); {0x8 alias. Non-allocating in L2 but coherent}
 BCM2835_MBOX_MEM_FLAG_L1_NONALLOCATING = (BCM2835_MBOX_MEM_FLAG_DIRECT or BCM2835_MBOX_MEM_FLAG_COHERENT); {Allocating in L2}
 BCM2835_MBOX_MEM_FLAG_ZERO             = (1 shl 4); {Initialise buffer to all zeros}
 BCM2835_MBOX_MEM_FLAG_NO_INIT          = (1 shl 5); {Don't initialise (default is initialise to all ones}
 BCM2835_MBOX_MEM_FLAG_HINT_PERMALOCK   = (1 shl 6); {Likely to be locked for long periods of time}

 {BCM2835 mailbox tag Blank Screen values}
 BCM2835_MBOX_BLANK_SCREEN_REQ_ON      = (1 shl 0);

 {BCM2835 mailbox tag Pixel Order values}
 BCM2835_MBOX_PIXEL_ORDER_BGR        = 0;
 BCM2835_MBOX_PIXEL_ORDER_RGB        = 1;

 {BCM2835 mailbox tag Alpha Mode values}
 BCM2835_MBOX_ALPHA_MODE_0_OPAQUE        = 0;
 BCM2835_MBOX_ALPHA_MODE_0_TRANSPARENT    = 1;
 BCM2835_MBOX_ALPHA_MODE_IGNORED        = 2;

 {BCM2835 mailbox tag Palette values}
 BCM2835_MBOX_PALETTE_INVALID = $00000001;

 {BCM2835 mailbox tag Cursor State values}
 BCM2835_MBOX_CURSOR_INVISIBLE = 0;
 BCM2835_MBOX_CURSOR_VISIBLE   = 1;

 {BCM2835 mailbox tag Cursor State flags}
 BCM2835_MBOX_CURSOR_STATE_DISPLAY_COORDS     = (0 shl 0);
 BCM2835_MBOX_CURSOR_STATE_FRAMEBUFFER_COORDS = (1 shl 0);

 {BCM2835 mailbox tag Cursor values}
 BCM2835_MBOX_CURSOR_INVALID = $00000001;

 {BCM2835 mailbox request / response codes}
 BCM2835_MBOX_REQUEST_CODE          = $00000000;
 BCM2835_MBOX_RESPONSE_CODE_SUCCESS    = $80000000;
 BCM2835_MBOX_RESPONSE_CODE_ERROR    = $80000001;

 {BCM2835 mailbox tag request / response codes}
 BCM2835_MBOX_TAG_REQUEST_CODE  = $00000000;
 BCM2835_MBOX_TAG_RESPONSE_CODE = $80000000;

const
 {BCM2835 GPIO constants}
 BCM2835_GPIO_PIN_COUNT = 54;
 BCM2835_GPIO_BANK_COUNT = 2;

 BCM2835_GPIO_SIGNATURE = $6770696F; {The ASCII value 'GPIO' returned when reading from a write only register}

 {Function Select Registers}
 BCM2835_GPFSEL0 = $00000000; {GPIO Function Select 0}
 BCM2835_GPFSEL1 = $00000004; {GPIO Function Select 1}
 BCM2835_GPFSEL2 = $00000008; {GPIO Function Select 2}
 BCM2835_GPFSEL3 = $0000000C; {GPIO Function Select 3}
 BCM2835_GPFSEL4 = $00000010; {GPIO Function Select 4}
 BCM2835_GPFSEL5 = $00000014; {GPIO Function Select 5}

 {Pin Output Set Registers}
 BCM2835_GPSET0 = $0000001C; {GPIO Pin Output Set 0}
 BCM2835_GPSET1 = $00000020; {GPIO Pin Output Set 1}

 {Pin Output Clear Registers}
 BCM2835_GPCLR0 = $00000028; {GPIO Pin Output Clear 0}
 BCM2835_GPCLR1 = $0000002C; {GPIO Pin Output Clear 1}

 {Pin Level Registers}
 BCM2835_GPLEV0 = $00000034; {GPIO Pin Level 0}
 BCM2835_GPLEV1 = $00000038; {GPIO Pin Level 1}

 {Pin Event Detect Status Registers}
 BCM2835_GPEDS0 = $00000040; {GPIO Pin Event Detect Status 0}
 BCM2835_GPEDS1 = $00000044; {GPIO Pin Event Detect Status 1}

 {Pin Rising Edge Detect Enable Registers}
 BCM2835_GPREN0 = $0000004c; {GPIO Pin Rising Edge Detect Enable 0}
 BCM2835_GPREN1 = $00000050; {GPIO Pin Rising Edge Detect Enable 1}

 {Pin Falling Edge Detect Enable Registers}
 BCM2835_GPFEN0 = $00000058; {GPIO Pin Falling Edge Detect Enable 0}
 BCM2835_GPFEN1 = $0000005c; {GPIO Pin Falling Edge Detect Enable 1}

 {Pin High Detect Enable Registers}
 BCM2835_GPHEN0 = $00000064; {GPIO Pin High Detect Enable 0}
 BCM2835_GPHEN1 = $00000068; {GPIO Pin High Detect Enable 1}

 {Pin Low Detect Enable Registers}
 BCM2835_GPLEN0 = $00000070; {GPIO Pin Low Detect Enable 0}
 BCM2835_GPLEN1 = $00000074; {GPIO Pin Low Detect Enable 1}

 {Pin Async. Rising Edge Detect Registers}
 BCM2835_GPAREN0 = $0000007c; {GPIO Pin Async. Rising Edge Detect 0}
 BCM2835_GPAREN1 = $00000080; {GPIO Pin Async. Rising Edge Detect 1}

 {Pin Async. Falling Edge Detect Registers}
 BCM2835_GPAFEN0 = $00000088; {GPIO Pin Async. Falling Edge Detect 0}
 BCM2835_GPAFEN1 = $0000008c; {GPIO Pin Async. Falling Edge Detect 1}

 {Pin Pull-up/down Enable Registers}
 BCM2835_GPPUD = $00000094; {GPIO Pin Pull-up/down Enable}

 {Pin Pull-up/down Enable Clock Registers}
 BCM2835_GPPUDCLK0 = $00000098; {GPIO Pin Pull-up/down Enable Clock 0}
 BCM2835_GPPUDCLK1 = $0000009C; {GPIO Pin Pull-up/down Enable Clock 1}

 {Function Select Mask}
 BCM2835_GPFSEL_MASK = 7;

 {Function Select Values}
 BCM2835_GPFSEL_IN   = 0;
 BCM2835_GPFSEL_OUT  = 1;
 BCM2835_GPFSEL_ALT0 = 4;
 BCM2835_GPFSEL_ALT1 = 5;
 BCM2835_GPFSEL_ALT2 = 6;
 BCM2835_GPFSEL_ALT3 = 7;
 BCM2835_GPFSEL_ALT4 = 3;
 BCM2835_GPFSEL_ALT5 = 2;

 {Pin Output Set Mask}
 BCM2835_GPSET_MASK = 1;

 {Pin Output Clear Mask}
 BCM2835_GPCLR_MASK = 1;

 {Pin Level Mask}
 BCM2835_GPLEV_MASK = 1;

 {Pin Event Detect Status Mask}
 BCM2835_GPEDS_MASK = 1;

 {Pin Rising Edge Detect Enable Mask}
 BCM2835_GPREN_MASK = 1;

 {Pin Falling Edge Detect Enable Mask}
 BCM2835_GPFEN_MASK = 1;

 {Pin High Detect Enable Mask}
 BCM2835_GPHEN_MASK = 1;

 {Pin Low Detect Enable Mask}
 BCM2835_GPLEN_MASK = 1;

 {Pin Async. Rising Edge Detect Mask}
 BCM2835_GPAREN_MASK = 1;

 {Pin Async. Falling Edge Detect Mask}
 BCM2835_GPAFEN_MASK = 1;

 {Pull-up/down Enable Mask}
 BCM2835_GPPUD_MASK = 3;

 {Pull-up/down Enable Values}
 BCM2835_GPPUD_NONE = 0;
 BCM2835_GPPUD_DOWN = 1;
 BCM2835_GPPUD_UP   = 2;

 {Pin Pull-up/down Enable Clock Mask}
 BCM2835_GPPUDCLK_MASK = 1;

{==============================================================================}
{$PACKRECORDS 4}
type
 {BCM2835 specific structures}

 {Layout of the BCM2835 Interrupt Controller registers (See 7.5)}
 PBCM2835InterruptRegisters = ^TBCM2835InterruptRegisters;
 TBCM2835InterruptRegisters = record
  IRQ_basic_pending:LongWord;
  IRQ_pending_1:LongWord;
  IRQ_pending_2:LongWord;
  FIQ_control:LongWord;
  Enable_IRQs_1:LongWord;
  Enable_IRQs_2:LongWord;
  Enable_Basic_IRQs:LongWord;
  Disable_IRQs_1:LongWord;
  Disable_IRQs_2:LongWord;
  Disable_Basic_IRQs:LongWord;
 end;

type
 {Layout of the BCM2835 System Timer registers (See 12.1)}
 PBCM2835SystemTimerRegisters = ^TBCM2835SystemTimerRegisters;
 TBCM2835SystemTimerRegisters = record
  CS:LongWord;  {System Timer Control/Status}
  CLO:LongWord; {System Timer Counter Lower 32 bits}
  CHI:LongWord; {System Timer Counter Higher 32 bits}
  C0:LongWord;  {System Timer Compare 0. Already used by the VideoCore GPU (Do not use)}
  C1:LongWord;  {System Timer Compare 1}
  C2:LongWord;  {System Timer Compare 2. Already used by the VideoCore GPU (Do not use)}
  C3:LongWord;  {System Timer Compare 3}
 end;

type
 {Layout of the BCM2835 DMA Channel registers (See 4.2.1.2)}
 PBCM2835DMARegisters = ^TBCM2835DMARegisters;
 TBCM2835DMARegisters = record
  CS:LongWord;         {DMA Channel Control and Status}
  CONBLK_AD:LongWord;  {DMA Channel Control Block Address}
  TI:LongWord;         {DMA Channel CB Word 0 (Transfer Information)}
  SOURCE_AD:LongWord;  {DMA Channel CB Word 1 (Source Address)}
  DEST_AD:LongWord;    {DMA Channel CB Word 2 (Destination Address)}
  TXFR_LEN:LongWord;   {DMA Channel CB Word 3 (Transfer Length)}
  STRIDE:LongWord;     {DMA Channel CB Word 4 (2D Stride)}
  NEXTCONBK:LongWord;  {DMA Channel CB Word 5 (Next CB Address)}
  DEBUG:LongWord;      {DMA Channel Debug}
 end;

type
 {Layout of BCM2835 DMA Control Block structure (See 4.2.1.1)} {Must be 32byte (256bit) aligned}
 PBCM2835DMAControlBlock = ^TBCM2835DMAControlBlock;
 TBCM2835DMAControlBlock = record
  TransferInformation:LongWord;
  SourceAddress:LongWord;
  DestinationAddress:LongWord;
  TransferLength:LongWord;
  ModeStride:LongWord;
  NextControlBlockAddress:LongWord;
  Reserved1:LongWord;
  Reserved2:LongWord;
 end;

type
 {Layout of the BCM2835 BSC (I2C) registers (See 3.2)}
 PBCM2835BSCRegisters = ^TBCM2835BSCRegisters;
 TBCM2835BSCRegisters = record
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
 {Layout of the BCM2835 SPI0 registers (See 10.5)}
 PBCM2835SPI0Registers = ^TBCM2835SPI0Registers;
 TBCM2835SPI0Registers = record
  CS:LongWord;    {SPI Master Control and Status}
  FIFO:LongWord;  {SPI Master TX and RX FIFOs}
  CLK:LongWord;   {SPI Master Clock Divider}
  DLEN:LongWord;  {SPI Master Data Length}
  LTOH:LongWord;  {SPI LOSSI mode TOH}
  DC:LongWord;    {SPI DMA DREQ Controls}
 end;

type
 {Layout of the BCM2835 I2C / SPI Slave registers (See 11.2)}
 PBCM2835I2CSPIRegisters = ^TBCM2835I2CSPIRegisters;
 TBCM2835I2CSPIRegisters = record
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
 {Layout of the BCM2835 AUX (UART1, SPI1 and SPI2) registers (See 2.1)}
 PBCM2835AUXRegisters = ^TBCM2835AUXRegisters;
 TBCM2835AUXRegisters = record
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
 {Layout of the BCM2835 PCM / I2S registers (See 8.8)}
 PBCM2835PCMRegisters = ^TBCM2835PCMRegisters;
 TBCM2835PCMRegisters = record
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
 {Layout of the BCM2835 Pulse Width Modulator (PWM) registers (See 9.6)}
 PBCM2835PWMRegisters = ^TBCM2835PWMRegisters;
 TBCM2835PWMRegisters = record
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
 {Layout of the BCM2835 PL011 UART registers (See 13.4)}
 PBCM2835PL011Registers = ^TBCM2835PL011Registers;
 TBCM2835PL011Registers = record
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
 {Layout of the BCM2835 ARM Timer registers (See 14.2)}
 PBCM2835ARMTimerRegisters = ^TBCM2835ARMTimerRegisters;
 TBCM2835ARMTimerRegisters = record
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
 {Layout of the BCM2835 Power Management Watchdog registers}
 PBCM2835PMWatchdogRegisters = ^TBCM2835PMWatchdogRegisters;
 TBCM2835PMWatchdogRegisters = record
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
 {Layout of the BCM2835 Random Number Generator registers}
 PBCM2835RNGRegisters = ^TBCM2835RNGRegisters;
 TBCM2835RNGRegisters = record
  Control:LongWord;
  Status:LongWord;
  Data:LongWord;
  FFThreshold:LongWord;
 end;

type
 {Layout of the BCM2835 GPIO registers (See 6.1)}
 PBCM2835GPIORegisters = ^TBCM2835GPIORegisters;
 TBCM2835GPIORegisters = record
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
  GPPUD:LongWord;       {GPIO Pin Pull-up/down Enable}
  GPPUDCLK0:LongWord;   {GPIO Pin Pull-up/down Enable Clock 0}
  GPPUDCLK1:LongWord;   {GPIO Pin Pull-up/down Enable Clock 1}
  Reserved12:LongWord;
  {Test:LongWord;}      {Test}
 end;

type
 {Layout of the BCM2835 Mailbox0 registers (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
 PBCM2835Mailbox0Registers = ^TBCM2835Mailbox0Registers;
 TBCM2835Mailbox0Registers = record
  Read:LongWord;      {Offset 0x00}{The read register for mailbox 0}
  Reserved1:LongWord; {Offset 0x04}
  Reserved2:LongWord; {Offset 0x08}
  Reserved3:LongWord; {Offset 0x0C}
  Peek:LongWord;      {Offset 0x10}{Read from the mailbox without removing data from it}
  Sender:LongWord;    {Offset 0x14}{Sender ID (bottom 2 bits only)}
  Status:LongWord;    {Offset 0x18}{The status register for mailbox 0}
  Config:LongWord;    {Offset 0x1C}{The configuration register for mailbox 0}
 end;

 {Layout of the BCM2835 Mailbox1 registers (See https://github.com/raspberrypi/firmware/wiki/Mailboxes)}
 PBCM2835Mailbox1Registers = ^TBCM2835Mailbox1Registers;
 TBCM2835Mailbox1Registers = record
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
 {Layout of the BCM2835 Mailbox Framebuffer request} {This structure must be 16 byte aligned when passed to the GPU}
 PBCM2835MailboxFramebuffer = ^TBCM2835MailboxFramebuffer;
 TBCM2835MailboxFramebuffer = record
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
 {Layout of the BCM2835 Mailbox Property tags} {These structures must be 16 byte aligned when passed to the GPU}
 {Header}
 PBCM2835MailboxHeader = ^TBCM2835MailboxHeader;
 TBCM2835MailboxHeader = record
  Size:LongWord;  {Total buffer size in bytes (including the header values, the end tag and padding)}
  Code:LongWord;  {Request/response code }
 end;

 {Footer}
 PBCM2835MailboxFooter = ^TBCM2835MailboxFooter;
 TBCM2835MailboxFooter = record
  Tag:LongWord;   {BCM2835_MBOX_TAG_END}
 end;

 {Tag Header}
 PBCM2835MailboxTagHeader = ^TBCM2835MailboxTagHeader;
 TBCM2835MailboxTagHeader = record
  Tag:LongWord;     {Tag identifier}
  Size:LongWord;    {Value buffer size in bytes}
  Length:LongWord;  {1 bit (MSB) request/response indicator (0=request, 1=response), 31 bits (LSB) value length in bytes}
 end;

 {Tag No Request}
 PBCM2835MailboxTagNoRequest = ^TBCM2835MailboxTagNoRequest;
 TBCM2835MailboxTagNoRequest = record
  {Nothing}
 end;

 {Tag No Response}
 PBCM2835MailboxTagNoResponse = ^TBCM2835MailboxTagNoResponse;
 TBCM2835MailboxTagNoResponse = record
  {Nothing}
 end;

 {Get Firmware Revision}
 TBCM2835MailboxTagFirmwareRevisionResponse = record
  Revision:LongWord;
 end;

 PBCM2835MailboxTagGetFirmwareRevision = ^TBCM2835MailboxTagGetFirmwareRevision;
 TBCM2835MailboxTagGetFirmwareRevision = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagFirmwareRevisionResponse);
 end;

 {Get Board Model}
 TBCM2835MailboxTagBoardModelResponse = record
  Model:LongWord;
 end;

 PBCM2835MailboxTagGetBoardModel = ^TBCM2835MailboxTagGetBoardModel;
 TBCM2835MailboxTagGetBoardModel = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagBoardModelResponse);
 end;

 {Get Board Revision}
 TBCM2835MailboxTagBoardRevisionResponse = record
  Revision:LongWord;
 end;

 PBCM2835MailboxTagGetBoardRevision = ^TBCM2835MailboxTagGetBoardRevision;
 TBCM2835MailboxTagGetBoardRevision = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagBoardRevisionResponse);
 end;

 {Get MAC Address}
 TBCM2835MailboxTagMACAddressResponse = record
  MAC:array[0..5] of Byte; {MAC address in network byte order}
  Padding:Word;
 end;

 PBCM2835MailboxTagGetMACAddress = ^TBCM2835MailboxTagGetMACAddress;
 TBCM2835MailboxTagGetMACAddress = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagMACAddressResponse);
 end;

 {Get Board Serial}
 TBCM2835MailboxTagBoardSerialResponse = record
  Serial:Int64;
 end;

 PBCM2835MailboxTagGetBoardSerial = ^TBCM2835MailboxTagGetBoardSerial;
 TBCM2835MailboxTagGetBoardSerial = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagBoardSerialResponse);
 end;

 {Get ARM Memory}
 TBCM2835MailboxTagARMMemoryResponse = record
  Address:LongWord; {Base address in bytes}
  Size:LongWord;    {Size in bytes}
 end;

 PBCM2835MailboxTagGetARMMemory = ^TBCM2835MailboxTagGetARMMemory;
 TBCM2835MailboxTagGetARMMemory = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagARMMemoryResponse);
 end;

 {Get VC Memory}
 TBCM2835MailboxTagVCMemoryResponse = record
  Address:LongWord; {Base address in bytes}
  Size:LongWord;    {Size in bytes}
 end;

 PBCM2835MailboxTagGetVCMemory = ^TBCM2835MailboxTagGetVCMemory;
 TBCM2835MailboxTagGetVCMemory = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagVCMemoryResponse);
 end;

 {Get Clocks}
 TBCM2835MailboxTagClockResponse = record
  ParentId:LongWord;
  ClockId:LongWord;
 end;

 TBCM2835MailboxTagClocksResponse = record
  Clocks:array[0..255] of TBCM2835MailboxTagClockResponse;
 end;

 PBCM2835MailboxTagGetClocks = ^TBCM2835MailboxTagGetClocks;
 TBCM2835MailboxTagGetClocks = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagClocksResponse);
 end;

 {Get Power State}
 TBCM2835MailboxTagGetPowerStateRequest = record
  DeviceId:LongWord;
 end;

 TBCM2835MailboxTagPowerStateResponse = record
  DeviceId:LongWord;
  State:LongWord;
 end;

 PBCM2835MailboxTagGetPowerState = ^TBCM2835MailboxTagGetPowerState;
 TBCM2835MailboxTagGetPowerState = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetPowerStateRequest);
  1:(Response:TBCM2835MailboxTagPowerStateResponse);
 end;

 {Get Timing}
 TBCM2835MailboxTagTimingRequest = record
  DeviceId:LongWord;
 end;

 TBCM2835MailboxTagTimingResponse = record
  DeviceId:LongWord;
  Wait:LongWord;      {Enable wait time in microseconds}
 end;

 PBCM2835MailboxTagGetTiming = ^TBCM2835MailboxTagGetTiming;
 TBCM2835MailboxTagGetTiming = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagTimingRequest);
  1:(Response:TBCM2835MailboxTagTimingResponse);
 end;

 {Set Power State}
 TBCM2835MailboxTagSetPowerStateRequest = record
  DeviceId:LongWord;
  State:LongWord;
 end;

 PBCM2835MailboxTagSetPowerState = ^TBCM2835MailboxTagSetPowerState;
 TBCM2835MailboxTagSetPowerState = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetPowerStateRequest);
  1:(Response:TBCM2835MailboxTagPowerStateResponse);
 end;

 {Get Clock State}
 TBCM2835MailboxTagGetClockStateRequest = record
  ClockId:LongWord;
 end;

 TBCM2835MailboxTagClockStateResponse = record
  ClockId:LongWord;
  State:LongWord;
 end;

 PBCM2835MailboxTagGetClockState = ^TBCM2835MailboxTagGetClockState;
 TBCM2835MailboxTagGetClockState = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetClockStateRequest);
  1:(Response:TBCM2835MailboxTagClockStateResponse);
 end;

 {Set Clock State}
 TBCM2835MailboxTagSetClockStateRequest = record
  ClockId:LongWord;
  State:LongWord;
 end;

 PBCM2835MailboxTagSetClockState = ^TBCM2835MailboxTagSetClockState;
 TBCM2835MailboxTagSetClockState = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetClockStateRequest);
  1:(Response:TBCM2835MailboxTagClockStateResponse);
 end;

 {Get Clock Rate}
 TBCM2835MailboxTagGetClockRateRequest = record
  ClockId:LongWord;
 end;

 TBCM2835MailboxTagClockRateResponse = record
  ClockId:LongWord;
  Rate:LongWord; {In Hz}
 end;

 PBCM2835MailboxTagGetClockRate = ^TBCM2835MailboxTagGetClockRate;
 TBCM2835MailboxTagGetClockRate = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetClockRateRequest);
  1:(Response:TBCM2835MailboxTagClockRateResponse);
 end;

 {Set Clock Rate}
 TBCM2835MailboxTagSetClockRateRequest = record
  ClockId:LongWord;
  Rate:LongWord; {In Hz}
  SkipTurbo:LongWord;
 end;

 PBCM2835MailboxTagSetClockRate = ^TBCM2835MailboxTagSetClockRate;
 TBCM2835MailboxTagSetClockRate = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetClockRateRequest);
  1:(Response:TBCM2835MailboxTagClockRateResponse);
 end;

 {Get Clock Max Rate}
 TBCM2835MailboxTagGetClockMaxRateRequest = record
  ClockId:LongWord;
 end;

 TBCM2835MailboxTagGetClockMaxRateResponse = record
  ClockId:LongWord;
  Rate:LongWord; {In Hz}
 end;

 PBCM2835MailboxTagGetClockMaxRate = ^TBCM2835MailboxTagGetClockMaxRate;
 TBCM2835MailboxTagGetClockMaxRate = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetClockMaxRateRequest);
  1:(Response:TBCM2835MailboxTagGetClockMaxRateResponse);
 end;

 {Get Clock Min Rate}
 PBCM2835MailboxTagGetClockMinRate = ^TBCM2835MailboxTagGetClockMinRate;
 TBCM2835MailboxTagGetClockMinRate = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetClockMaxRateRequest);
  1:(Response:TBCM2835MailboxTagGetClockMaxRateResponse);
 end;

 {Get Clock Measured Rate}
 PBCM2835MailboxTagGetClockMeasuredRate = ^TBCM2835MailboxTagGetClockMeasuredRate;
 TBCM2835MailboxTagGetClockMeasuredRate = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetClockRateRequest);
  1:(Response:TBCM2835MailboxTagClockRateResponse);
 end;

 {Get Turbo}
 TBCM2835MailboxTagGetTurboRequest = record
  Id:LongWord;
 end;

 TBCM2835MailboxTagTurboResponse = record
  Id:LongWord;
  Level:LongWord;
 end;

 PBCM2835MailboxTagGetTurbo = ^TBCM2835MailboxTagGetTurbo;
 TBCM2835MailboxTagGetTurbo = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetTurboRequest);
  1:(Response:TBCM2835MailboxTagTurboResponse);
 end;

 {Set Turbo}
 TBCM2835MailboxTagSetTurboRequest = record
  Id:LongWord;
  Level:LongWord;
 end;

 PBCM2835MailboxTagSetTurbo = ^TBCM2835MailboxTagSetTurbo;
 TBCM2835MailboxTagSetTurbo = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetTurboRequest);
  1:(Response:TBCM2835MailboxTagTurboResponse);
 end;

 {Get Voltage}
 TBCM2835MailboxTagGetVoltageRequest = record
  VoltageId:LongWord;
 end;

 TBCM2835MailboxTagVoltageResponse = record
  VoltageId:LongWord;
  Value:LongWord;     {Offset from 1.2V in units of 0.025V}
 end;

 PBCM2835MailboxTagGetVoltage = ^TBCM2835MailboxTagGetVoltage;
 TBCM2835MailboxTagGetVoltage = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetVoltageRequest);
  1:(Response:TBCM2835MailboxTagVoltageResponse);
 end;

 {Set Voltage}
 TBCM2835MailboxTagSetVoltageRequest = record
  VoltageId:LongWord;
  Value:LongWord;     {Offset from 1.2V in units of 0.025V}
 end;

 PBCM2835MailboxTagSetVoltage = ^TBCM2835MailboxTagSetVoltage;
 TBCM2835MailboxTagSetVoltage = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetVoltageRequest);
  1:(Response:TBCM2835MailboxTagVoltageResponse);
 end;

 {Get Max Voltage}
 PBCM2835MailboxTagGetMaxVoltage = ^TBCM2835MailboxTagGetMaxVoltage;
 TBCM2835MailboxTagGetMaxVoltage = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetVoltageRequest);
  1:(Response:TBCM2835MailboxTagVoltageResponse);
 end;

 {Get Min Voltage}
 PBCM2835MailboxTagGetMinVoltage = ^TBCM2835MailboxTagGetMinVoltage;
 TBCM2835MailboxTagGetMinVoltage = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetVoltageRequest);
  1:(Response:TBCM2835MailboxTagVoltageResponse);
 end;

 {Get Temperature}
 TBCM2835MailboxTagTemperatureRequest = record
  TemperatureId:LongWord;
 end;

 TBCM2835MailboxTagTemperatureResponse = record
  TemperatureId:LongWord; {Should be zero}
  Temperature:LongWord;   {Return the temperature of the SoC in thousandths of a degree C}
 end;

 PBCM2835MailboxTagGetTemperature = ^TBCM2835MailboxTagGetTemperature;
 TBCM2835MailboxTagGetTemperature = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagTemperatureRequest);
  1:(Response:TBCM2835MailboxTagTemperatureResponse);
 end;

 {Get Max Temp}
 PBCM2835MailboxTagGetMaxTemperature = ^TBCM2835MailboxTagGetMaxTemperature;
 TBCM2835MailboxTagGetMaxTemperature = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagTemperatureRequest);
  1:(Response:TBCM2835MailboxTagTemperatureResponse);
 end;

 {Allocate Memory}
 TBCM2835MailboxTagAllocateMemoryRequest = record
  Size:LongWord;
  Alignment:LongWord;
  Flags:LongWord;
 end;

 TBCM2835MailboxTagAllocateMemoryResponse = record
  Handle:THandle;
 end;

 PBCM2835MailboxTagAllocateMemory = ^TBCM2835MailboxTagAllocateMemory;
 TBCM2835MailboxTagAllocateMemory = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagAllocateMemoryRequest);
  1:(Response:TBCM2835MailboxTagAllocateMemoryResponse);
 end;

 {Lock Memory}
 TBCM2835MailboxTagLockMemoryRequest = record
  Handle:THandle;
 end;

 TBCM2835MailboxTagLockMemoryResponse = record
  Address:LongWord; {Bus Address}
 end;

 PBCM2835MailboxTagLockMemory = ^TBCM2835MailboxTagLockMemory;
 TBCM2835MailboxTagLockMemory = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagLockMemoryRequest);
  1:(Response:TBCM2835MailboxTagLockMemoryResponse);
 end;

 {Unlock Memory}
 TBCM2835MailboxTagUnlockMemoryResponse = record
  Status:LongWord; {0 is Success}
 end;

 PBCM2835MailboxTagUnlockMemory = ^TBCM2835MailboxTagUnlockMemory;
 TBCM2835MailboxTagUnlockMemory = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagLockMemoryRequest);
  1:(Response:TBCM2835MailboxTagUnlockMemoryResponse);
 end;

 {Release Memory}
 PBCM2835MailboxTagReleaseMemory = ^TBCM2835MailboxTagReleaseMemory;
 TBCM2835MailboxTagReleaseMemory = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagLockMemoryRequest);
  1:(Response:TBCM2835MailboxTagUnlockMemoryResponse);
 end;

 {Execute Code}
 TBCM2835MailboxTagExecuteCodeRequest = record
  Address:Pointer; {Bus Address}
  R0:LongWord;
  R1:LongWord;
  R2:LongWord;
  R3:LongWord;
  R4:LongWord;
  R5:LongWord;
 end;

 TBCM2835MailboxTagExecuteCodeResponse = record
  R0:LongWord;
 end;

 PBCM2835MailboxTagExecuteCode = ^TBCM2835MailboxTagExecuteCode;
 TBCM2835MailboxTagExecuteCode = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagExecuteCodeRequest);
  1:(Response:TBCM2835MailboxTagExecuteCodeResponse);
 end;

 {Execute QPU}
 TBCM2835MailboxTagExecuteQPURequest = record
  NumQPUs:LongWord;
  Control:LongWord;
  NoFlush:LongWord;
  Timeout:LongWord; {Milliseconds}
 end;

 TBCM2835MailboxTagExecuteQPUResponse = record
  Status:LongWord; {0 is Success / 0x80000000 is Timeout}
 end;

 PBCM2835MailboxTagExecuteQPU = ^TBCM2835MailboxTagExecuteQPU;
 TBCM2835MailboxTagExecuteQPU = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagExecuteQPURequest);
  1:(Response:TBCM2835MailboxTagExecuteQPUResponse);
 end;

 {Enable QPU}
 TBCM2835MailboxTagEnableQPURequest = record
  Enable:LongWord;
 end;

 TBCM2835MailboxTagEnableQPUResponse = record
  Status:LongWord;  {0 is Success}
 end;

 PBCM2835MailboxTagEnableQPU = ^TBCM2835MailboxTagEnableQPU;
 TBCM2835MailboxTagEnableQPU = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagEnableQPURequest);
  1:(Response:TBCM2835MailboxTagEnableQPUResponse);
 end;

 {Get Dispmanx Handle}
 TBCM2835MailboxTagGetDispmanxHandleRequest = record
  Resource:THandle;
 end;

 TBCM2835MailboxTagGetDispmanxHandleResponse = record
  Status:LongWord; {0 is Success}
  Memory:THandle;
 end;

 PBCM2835MailboxTagGetDispmanxHandle = ^TBCM2835MailboxTagGetDispmanxHandle;
 TBCM2835MailboxTagGetDispmanxHandle = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetDispmanxHandleRequest);
  1:(Response:TBCM2835MailboxTagGetDispmanxHandleResponse);
 end;

 {Get EDID Block}
 TBCM2835MailboxTagGetEDIDBlockRequest = record
  Block:LongWord; {Starting from 0}
 end;

 TBCM2835MailboxTagGetEDIDBlockResponse = record
  Block:LongWord; {Starting from 0}
  Status:LongWord; {0 is Success}
  EDID:array[0..127] of Byte;
 end;

 PBCM2835MailboxTagGetEDIDBlock = ^TBCM2835MailboxTagGetEDIDBlock;
 TBCM2835MailboxTagGetEDIDBlock = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetEDIDBlockRequest);
  1:(Response:TBCM2835MailboxTagGetEDIDBlockResponse);
 end;

 {Get GPIO State}
 TBCM2835MailboxTagGPIOStateRequest = record
  GPIO:LongWord;
  State:LongWord;
 end;

 TBCM2835MailboxTagGPIOStateResponse = record
  GPIO:LongWord;
  State:LongWord;
 end;

 PBCM2835MailboxTagGetGPIOState = ^TBCM2835MailboxTagGetGPIOState;
 TBCM2835MailboxTagGetGPIOState = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGPIOStateRequest);
  1:(Response:TBCM2835MailboxTagGPIOStateResponse);
 end;

 {Set GPIO State}
 PBCM2835MailboxTagSetGPIOState = ^TBCM2835MailboxTagSetGPIOState;
 TBCM2835MailboxTagSetGPIOState = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGPIOStateRequest);
  1:(Response:TBCM2835MailboxTagGPIOStateResponse);
 end;

 {Get GPIO Config}
 TBCM2835MailboxTagGetGPIOConfigRequest = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
 end;

 TBCM2835MailboxTagGetGPIOConfigResponse = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
 end;

 PBCM2835MailboxTagGetGPIOConfig = ^TBCM2835MailboxTagGetGPIOConfig;
 TBCM2835MailboxTagGetGPIOConfig = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetGPIOConfigRequest);
  1:(Response:TBCM2835MailboxTagGetGPIOConfigResponse);
 end;

 {Set GPIO Config}
 TBCM2835MailboxTagSetGPIOConfigRequest = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
  State:LongWord;
 end;

 TBCM2835MailboxTagSetGPIOConfigResponse = record
  GPIO:LongWord;
  Direction:LongWord;
  Polarity:LongWord;
  Terminator:LongWord;
  PullUp:LongWord;
  State:LongWord;
 end;

 PBCM2835MailboxTagSetGPIOConfig = ^TBCM2835MailboxTagSetGPIOConfig;
 TBCM2835MailboxTagSetGPIOConfig = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetGPIOConfigRequest);
  1:(Response:TBCM2835MailboxTagSetGPIOConfigResponse);
 end;

 {Get Throttled}
 TBCM2835MailboxTagGetThrottledRequest = record
  Value:LongWord;
 end;

 TBCM2835MailboxTagGetThrottledResponse = record
  Value:LongWord;
 end;

 PBCM2835MailboxTagGetThrottled = ^TBCM2835MailboxTagGetThrottled;
 TBCM2835MailboxTagGetThrottled = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetThrottledRequest);
  1:(Response:TBCM2835MailboxTagGetThrottledResponse);
 end;

 {Get Boot Mode}
 TBCM2835MailboxTagGetBootModeResponse = record
  BootMode:LongWord;
  SignedBoot:LongWord;
 end;

 PBCM2835MailboxTagGetBootMode = ^TBCM2835MailboxTagGetBootMode;
 TBCM2835MailboxTagGetBootMode = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagGetBootModeResponse);
 end;

 {Allocate Buffer}
 TBCM2835MailboxTagAllocateBufferRequest = record
  Alignment:LongWord; {Bytes}
 end;

 TBCM2835MailboxTagAllocateBufferResponse = record
  Address:LongWord; {Base Address in Bytes}
  Size:LongWord;    {Size in Bytes}
 end;

 PBCM2835MailboxTagAllocateBuffer = ^TBCM2835MailboxTagAllocateBuffer;
 TBCM2835MailboxTagAllocateBuffer = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagAllocateBufferRequest);
  1:(Response:TBCM2835MailboxTagAllocateBufferResponse);
 end;

 {Release Buffer}
 PBCM2835MailboxTagReleaseBuffer = ^TBCM2835MailboxTagReleaseBuffer;
 TBCM2835MailboxTagReleaseBuffer = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagNoResponse);
 end;

 {Blank Screen}
 TBCM2835MailboxTagBlankScreenRequest = record
  State:LongWord;
 end;

 TBCM2835MailboxTagBlankScreenResponse = record
  State:LongWord;
 end;

 PBCM2835MailboxTagBlankScreen = ^TBCM2835MailboxTagBlankScreen;
 TBCM2835MailboxTagBlankScreen = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagBlankScreenRequest);
  1:(Response:TBCM2835MailboxTagBlankScreenResponse);
 end;

 {Get Physical}
 TBCM2835MailboxTagPhysicalRequest = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 TBCM2835MailboxTagPhysicalResponse = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 PBCM2835MailboxTagGetPhysical = ^TBCM2835MailboxTagGetPhysical;
 TBCM2835MailboxTagGetPhysical = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagPhysicalResponse);
 end;

 {Test Physical}
 PBCM2835MailboxTagTestPhysical = ^TBCM2835MailboxTagTestPhysical;
 TBCM2835MailboxTagTestPhysical = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagPhysicalRequest);
  1:(Response:TBCM2835MailboxTagPhysicalResponse);
 end;

 {Set Physical}
 PBCM2835MailboxTagSetPhysical = ^TBCM2835MailboxTagSetPhysical;
 TBCM2835MailboxTagSetPhysical = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagPhysicalRequest);
  1:(Response:TBCM2835MailboxTagPhysicalResponse);
 end;

 {Get Virtual}
 TBCM2835MailboxTagVirtualRequest = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 TBCM2835MailboxTagVirtualResponse = record
  Width:LongWord;   {Pixels}
  Height:Longword;  {Pixels}
 end;

 PBCM2835MailboxTagGetVirtual = ^TBCM2835MailboxTagGetVirtual;
 TBCM2835MailboxTagGetVirtual = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagVirtualResponse);
 end;

 {Test Virtual}
 PBCM2835MailboxTagTestVirtual = ^TBCM2835MailboxTagTestVirtual;
 TBCM2835MailboxTagTestVirtual = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagVirtualRequest);
  1:(Response:TBCM2835MailboxTagVirtualResponse);
 end;

 {Set Virtual}
 PBCM2835MailboxTagSetVirtual = ^TBCM2835MailboxTagSetVirtual;
 TBCM2835MailboxTagSetVirtual = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagVirtualRequest);
  1:(Response:TBCM2835MailboxTagVirtualResponse);
 end;

 {Get Depth}
 TBCM2835MailboxTagDepthRequest = record
  Depth:LongWord;   {Bits per pixel}
 end;

 TBCM2835MailboxTagDepthResponse = record
  Depth:LongWord;   {Bits per pixel}
 end;

 PBCM2835MailboxTagGetDepth = ^TBCM2835MailboxTagGetDepth;
 TBCM2835MailboxTagGetDepth = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagDepthResponse);
 end;

 {Test Depth}
 PBCM2835MailboxTagTestDepth = ^TBCM2835MailboxTagTestDepth;
 TBCM2835MailboxTagTestDepth = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagDepthRequest);
  1:(Response:TBCM2835MailboxTagDepthResponse);
 end;

 {Set Depth}
 PBCM2835MailboxTagSetDepth = ^TBCM2835MailboxTagSetDepth;
 TBCM2835MailboxTagSetDepth = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagDepthRequest);
  1:(Response:TBCM2835MailboxTagDepthResponse);
 end;

 {Get Pixel Order}
 TBCM2835MailboxTagPixelOrderRequest = record
  Order:LongWord;
 end;

 TBCM2835MailboxTagPixelOrderResponse = record
  Order:LongWord;
 end;

 PBCM2835MailboxTagGetPixelOrder = ^TBCM2835MailboxTagGetPixelOrder;
 TBCM2835MailboxTagGetPixelOrder = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagPixelOrderResponse);
 end;

 {Test Pixel Order}
 PBCM2835MailboxTagTestPixelOrder = ^TBCM2835MailboxTagTestPixelOrder;
 TBCM2835MailboxTagTestPixelOrder = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagPixelOrderRequest);
  1:(Response:TBCM2835MailboxTagPixelOrderResponse);
 end;

 {Set Pixel Order}
 PBCM2835MailboxTagSetPixelOrder = ^TBCM2835MailboxTagSetPixelOrder;
 TBCM2835MailboxTagSetPixelOrder = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagPixelOrderRequest);
  1:(Response:TBCM2835MailboxTagPixelOrderResponse);
 end;

 {Get Alpha Mode}
 TBCM2835MailboxTagAlphaModeRequest = record
  Mode:LongWord;
 end;

 TBCM2835MailboxTagAlphaModeResponse = record
  Mode:LongWord;
 end;

 PBCM2835MailboxTagGetAlphaMode = ^TBCM2835MailboxTagGetAlphaMode;
 TBCM2835MailboxTagGetAlphaMode = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagAlphaModeResponse);
 end;

 {Test Alpha Mode}
 PBCM2835MailboxTagTestAlphaMode = ^TBCM2835MailboxTagTestAlphaMode;
 TBCM2835MailboxTagTestAlphaMode = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagAlphaModeRequest);
  1:(Response:TBCM2835MailboxTagAlphaModeResponse);
 end;

 {Set Alpha Mode}
 PBCM2835MailboxTagSetAlphaMode = ^TBCM2835MailboxTagSetAlphaMode;
 TBCM2835MailboxTagSetAlphaMode = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagAlphaModeRequest);
  1:(Response:TBCM2835MailboxTagAlphaModeResponse);
 end;

 {Get Pitch}
 TBCM2835MailboxTagPitchResponse = record
  Pitch:LongWord;  {Bytes per line}
 end;

 PBCM2835MailboxTagGetPitch = ^TBCM2835MailboxTagGetPitch;
 TBCM2835MailboxTagGetPitch = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagPitchResponse);
 end;

 {Get Virtual Offset}
 TBCM2835MailboxTagVirtualOffsetRequest = record
  X:LongWord; {Pixels}
  Y:LongWord; {Pixels}
 end;

 TBCM2835MailboxTagVirtualOffsetResponse = record
  X:LongWord; {Pixels}
  Y:LongWord; {Pixels}
 end;

 PBCM2835MailboxTagGetVirtualOffset = ^TBCM2835MailboxTagGetVirtualOffset;
 TBCM2835MailboxTagGetVirtualOffset = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagVirtualOffsetResponse);
 end;

 {Test Virtual Offset}
 PBCM2835MailboxTagTestVirtualOffset = ^TBCM2835MailboxTagTestVirtualOffset;
 TBCM2835MailboxTagTestVirtualOffset = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagVirtualOffsetRequest);
  1:(Response:TBCM2835MailboxTagVirtualOffsetResponse);
 end;

 {Set Virtual Offset}
 PBCM2835MailboxTagSetVirtualOffset = ^TBCM2835MailboxTagSetVirtualOffset;
 TBCM2835MailboxTagSetVirtualOffset = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagVirtualOffsetRequest);
  1:(Response:TBCM2835MailboxTagVirtualOffsetResponse);
 end;

 {Get Overscan}
 TBCM2835MailboxTagOverscanRequest = record
  Top:LongWord;    {Pixels}
  Bottom:LongWord; {Pixels}
  Left:LongWord;   {Pixels}
  Right:LongWord;  {Pixels}
 end;

 TBCM2835MailboxTagOverscanResponse = record
  Top:LongWord;    {Pixels}
  Bottom:LongWord; {Pixels}
  Left:LongWord;   {Pixels}
  Right:LongWord;  {Pixels}
 end;

 PBCM2835MailboxTagGetOverscan = ^TBCM2835MailboxTagGetOverscan;
 TBCM2835MailboxTagGetOverscan = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagOverscanResponse);
 end;

 {Test Overscan}
 PBCM2835MailboxTagTestOverscan = ^TBCM2835MailboxTagTestOverscan;
 TBCM2835MailboxTagTestOverscan = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagOverscanRequest);
  1:(Response:TBCM2835MailboxTagOverscanResponse);
 end;

 {Set Overscan}
 PBCM2835MailboxTagSetOverscan = ^TBCM2835MailboxTagSetOverscan;
 TBCM2835MailboxTagSetOverscan = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagOverscanRequest);
  1:(Response:TBCM2835MailboxTagOverscanResponse);
 end;

 {Get Palette}
 TBCM2835MailboxTagGetPaletteResponse = record
  Values:array[0..255] of LongWord;    {RGBA Palette Values}
 end;

 PBCM2835MailboxTagGetPalette = ^TBCM2835MailboxTagGetPalette;
 TBCM2835MailboxTagGetPalette = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagGetPaletteResponse);
 end;

 {Test Palette}
 TBCM2835MailboxTagPaletteRequest = record
  Offset:LongWord; {First palette index to set (0-255)}
  Length:LongWord; {Number of palette entries to set (1-256)}
  Values:array[0..255] of LongWord;    {RGBA Palette Values}
 end;

 TBCM2835MailboxTagPaletteResponse = record
  Status:LongWord;
 end;

 PBCM2835MailboxTagTestPalette = ^TBCM2835MailboxTagTestPalette;
 TBCM2835MailboxTagTestPalette = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagPaletteRequest);
  1:(Response:TBCM2835MailboxTagPaletteResponse);
 end;

 {Set Palette}
 PBCM2835MailboxTagSetPalette = ^TBCM2835MailboxTagSetPalette;
 TBCM2835MailboxTagSetPalette = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagPaletteRequest);
  1:(Response:TBCM2835MailboxTagPaletteResponse);
 end;

 {Get Layer}
 TBCM2835MailboxTagLayerRequest = record
  Layer:LongInt;
 end;

 TBCM2835MailboxTagLayerResponse = record
  Layer:LongInt;
 end;

 PBCM2835MailboxTagGetLayer = ^TBCM2835MailboxTagGetLayer;
 TBCM2835MailboxTagGetLayer = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagLayerResponse);
 end;

 {Test Layer}
 PBCM2835MailboxTagTestLayer = ^TBCM2835MailboxTagTestLayer;
 TBCM2835MailboxTagTestLayer = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagLayerRequest);
  1:(Response:TBCM2835MailboxTagLayerResponse);
 end;

 {Set Layer}
 PBCM2835MailboxTagSetLayer = ^TBCM2835MailboxTagSetLayer;
 TBCM2835MailboxTagSetLayer = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagLayerRequest);
  1:(Response:TBCM2835MailboxTagLayerResponse);
 end;

 {Get Touch Buffer}
 TBCM2835MailboxTagGetTouchResponse = record
  Address:LongWord;
 end;

 PBCM2835MailboxTagGetTouch = ^TBCM2835MailboxTagGetTouch;
 TBCM2835MailboxTagGetTouch = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagGetTouchResponse);
 end;

 {Set Touch Buffer}
 TBCM2835MailboxTagSetTouchRequest = record
  Address:LongWord;
 end;

 TBCM2835MailboxTagSetTouchResponse = record
  Status:LongWord;
 end;

 PBCM2835MailboxTagSetTouch = ^TBCM2835MailboxTagSetTouch;
 TBCM2835MailboxTagSetTouch = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetTouchRequest);
  1:(Response:TBCM2835MailboxTagSetTouchResponse);
 end;

 {Get Virtual GPIO Buffer}
 TBCM2835MailboxTagGetVirtualGPIOResponse = record
  Address:LongWord;
 end;

 PBCM2835MailboxTagGetVirtualGPIO = ^TBCM2835MailboxTagGetVirtualGPIO;
 TBCM2835MailboxTagGetVirtualGPIO = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagGetVirtualGPIOResponse);
 end;

 {Set Virtual GPIO Buffer}
 TBCM2835MailboxTagSetVirtualGPIORequest = record
  Address:LongWord;
 end;

 TBCM2835MailboxTagSetVirtualGPIOResponse = record
  Status:LongWord;
 end;

 PBCM2835MailboxTagSetVirtualGPIO = ^TBCM2835MailboxTagSetVirtualGPIO;
 TBCM2835MailboxTagSetVirtualGPIO = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetVirtualGPIORequest);
  1:(Response:TBCM2835MailboxTagSetVirtualGPIOResponse);
 end;

 {Test Vsync}
 PBCM2835MailboxTagTestVsync = ^TBCM2835MailboxTagTestVsync;
 TBCM2835MailboxTagTestVsync = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagNoResponse);
 end;

 {Set Vsync}
 PBCM2835MailboxTagSetVsync = ^TBCM2835MailboxTagSetVsync;
 TBCM2835MailboxTagSetVsync = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagNoResponse);
 end;

 {Set Backlight}
 TBCM2835MailboxTagSetBacklightRequest = record
  Brightness:LongWord;
 end;

 TBCM2835MailboxTagSetBacklightResponse = record
  Brightness:LongWord;
 end;

 PBCM2835MailboxTagSetBacklight = ^TBCM2835MailboxTagSetBacklight;
 TBCM2835MailboxTagSetBacklight = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetBacklightRequest);
  1:(Response:TBCM2835MailboxTagSetBacklightResponse);
 end;

 {Get Display Id}
 TBCM2835MailboxTagGetDisplayIdRequest = record
  DisplayNum:LongWord;
 end;

 TBCM2835MailboxTagGetDisplayIdResponse = record
  DisplayId:LongWord;
 end;

 PBCM2835MailboxTagGetDisplayId = ^TBCM2835MailboxTagGetDisplayId;
 TBCM2835MailboxTagGetDisplayId = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetDisplayIdRequest);
  1:(Response:TBCM2835MailboxTagGetDisplayIdResponse);
 end;

 {Set Display Num}
 TBCM2835MailboxTagSetDisplayNumRequest = record
  DisplayNum:LongWord;
 end;

 TBCM2835MailboxTagSetDisplayNumResponse = record
  DisplayNum:LongWord;
 end;

 PBCM2835MailboxTagSetDisplayNum = ^TBCM2835MailboxTagSetDisplayNum;
 TBCM2835MailboxTagSetDisplayNum = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetDisplayNumRequest);
  1:(Response:TBCM2835MailboxTagSetDisplayNumResponse);
 end;

 {Get Num Displays}
 TBCM2835MailboxTagGetNumDisplaysRequest = record
  NumDisplays:LongWord;
 end;

 TBCM2835MailboxTagGetNumDisplaysResponse = record
  NumDisplays:LongWord;
 end;

 PBCM2835MailboxTagGetNumDisplays = ^TBCM2835MailboxTagGetNumDisplays;
 TBCM2835MailboxTagGetNumDisplays = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetNumDisplaysRequest);
  1:(Response:TBCM2835MailboxTagGetNumDisplaysResponse);
 end;

 {Get Display Settings}
 TBCM2835MailboxTagGetDisplaySettingsRequest = record
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

 TBCM2835MailboxTagGetDisplaySettingsResponse = record
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

 PBCM2835MailboxTagGetDisplaySettings = ^TBCM2835MailboxTagGetDisplaySettings;
 TBCM2835MailboxTagGetDisplaySettings = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagGetDisplaySettingsRequest);
  1:(Response:TBCM2835MailboxTagGetDisplaySettingsResponse);
 end;

 {Set Cursor Info}
 TBCM2835MailboxTagSetCursorInfoRequest = record
  Width:LongWord;    {Pixels}
  Height:LongWord;   {Pixels}
  Reserved:LongWord;
  Pixels:Pointer;    {Format is 32bpp (ARGB) (Width and Height should be >= 16 and (Width * Height) <= 64)}
  HotspotX:LongWord;
  HotspotY:LongWord;
 end;

 TBCM2835MailboxTagCursorResponse = record
  Status:LongWord;
 end;

 PBCM2835MailboxTagSetCursorInfo = ^TBCM2835MailboxTagSetCursorInfo;
 TBCM2835MailboxTagSetCursorInfo = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetCursorInfoRequest);
  1:(Response:TBCM2835MailboxTagCursorResponse);
 end;

 {Set Cursor State}
 TBCM2835MailboxTagSetCursorStateRequest = record
  Enable:LongWord;
  X:LongWord;        {Pixels}
  Y:LongWord;        {Pixels}
  Flags:LongWord;
 end;

 PBCM2835MailboxTagSetCursorState = ^TBCM2835MailboxTagSetCursorState;
 TBCM2835MailboxTagSetCursorState = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagSetCursorStateRequest);
  1:(Response:TBCM2835MailboxTagCursorResponse);
 end;

 {VCHIQ Init}
 TBCM2835MailboxTagVCHIQInitRequest = record
  Address:LongWord;
 end;

 TBCM2835MailboxTagVCHIQInitResponse = record
  Status:LongWord;  {0 is Success}
 end;

 PBCM2835MailboxTagVCHIQInit = ^TBCM2835MailboxTagVCHIQInit;
 TBCM2835MailboxTagVCHIQInit = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagVCHIQInitRequest);
  1:(Response:TBCM2835MailboxTagVCHIQInitResponse);
 end;

 {Get Command Line}
 TBCM2835MailboxTagCommandLineResponse = record
  CommandLine:array[0..1023] of Char;
 end;

 PBCM2835MailboxTagGetCommandLine = ^TBCM2835MailboxTagGetCommandLine;
 TBCM2835MailboxTagGetCommandLine = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagCommandLineResponse);
 end;

 {Get DMA Channels}
 TBCM2835MailboxTagDMAChannelsResponse = record
  Channels:LongWord;
 end;

 PBCM2835MailboxTagGetDMAChannels = ^TBCM2835MailboxTagGetDMAChannels;
 TBCM2835MailboxTagGetDMAChannels = record
  Header:TBCM2835MailboxTagHeader;
  case Integer of
  0:(Request:TBCM2835MailboxTagNoRequest);
  1:(Response:TBCM2835MailboxTagDMAChannelsResponse);
 end;

 {Create Buffer (A combination tag to allocate and configure a framebuffer in one request)}
 PBCM2835MailboxTagCreateBuffer = ^TBCM2835MailboxTagCreateBuffer;
 TBCM2835MailboxTagCreateBuffer = record
  Physical:TBCM2835MailboxTagSetPhysical;
  Vertual:TBCM2835MailboxTagSetVirtual;
  Depth:TBCM2835MailboxTagSetDepth;
  Order:TBCM2835MailboxTagSetPixelOrder;
  Mode:TBCM2835MailboxTagSetAlphaMode;
  Offset:TBCM2835MailboxTagSetVirtualOffset;
  Overscan:TBCM2835MailboxTagSetOverscan;
  Allocate:TBCM2835MailboxTagAllocateBuffer;
  Pitch:TBCM2835MailboxTagGetPitch;
 end;

 {Query Buffer (A combination tag to query all framebuffer properties in one request)}
 PBCM2835MailboxTagQueryBuffer = ^TBCM2835MailboxTagQueryBuffer;
 TBCM2835MailboxTagQueryBuffer = record
  Physical:TBCM2835MailboxTagGetPhysical;
  Vertual:TBCM2835MailboxTagGetVirtual;
  Depth:TBCM2835MailboxTagGetDepth;
  Order:TBCM2835MailboxTagGetPixelOrder;
  Mode:TBCM2835MailboxTagGetAlphaMode;
  Offset:TBCM2835MailboxTagGetVirtualOffset;
  Overscan:TBCM2835MailboxTagGetOverscan;
  Pitch:TBCM2835MailboxTagGetPitch;
 end;
{$PACKRECORDS DEFAULT}
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
