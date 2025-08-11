{
Ultibo MMC/SD/SDIO interface unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

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

   Linux MMC/SDHCI drivers
   U-Boot MMC/SDHCI drivers

   U-Boot

    \drivers\mmc\bcm2835_sdhci.c
    \drivers\mmc\sdhci.c
    \drivers\mmc\mmc.c
    \drivers\mmc\mmc_spi.c
    \drivers\mmc\mmc_write.c
    \include\mmc.h
    \include\sdhci.h

   Linux

   General

    \drivers\mmc\core\core.c
    \drivers\mmc\core\host.c
    \drivers\mmc\card\block.c


   SDHCI

    \drivers\mmc\host\bcm2835-mmc.c
    \drivers\mmc\host\sdhci.c

   MMC

    \drivers\mmc\core\mmc.c
    \drivers\mmc\core\mmc_ops.c

   SD

    \drivers\mmc\core\sd.c
    \drivers\mmc\core\sd_ops.c

   SDIO

    \drivers\mmc\core\sdio.c
    \drivers\mmc\core\sdio_ops.c

   Include

    \include\linux\mmc


References
==========

 SD Host Controller Simplified Specification V3.0

   https://www.sdcard.org/downloads/pls/simplified_specs/partA2_300.pdf

 SD Host Controller Simplified Specification V4.20

   https://www.sdcard.org/downloads/pls

 SDIO Simplified Specification V3.0

   https://www.sdcard.org/downloads/pls/simplified_specs/partE1_300.pdf

 SD Physical Layer Simplified Specification V4.10.pdf

   https://www.sdcard.org/downloads/pls/simplified_specs/part1_410.pdf

 Embedded Multi-Media Card (e•MMC) Electrical Standard (5.1)

   https://www.jedec.org/standards-documents/docs/jesd84-b51

 Others

   http://elm-chan.org/docs/mmc/mmc_e.html


SD/MMC Devices
==============

This unit implements the standards based part of the SD/MMC specification including the standard SDHCI interfaces.

For each platform a SDHCI module needs to be provided that implements the platform specific parts of the SDHCI interface.

A fully standards compliant SDHCI host controller needs only a procedure to register it with the SD/MMC core and it
can operate using the standard functions provided in this unit, any deviations from the standard will require custom
functions to be provided to override the standard functionality.

This is similar in model to USB and other interfaces in Ultibo, where the generic interface unit requires a platform specific
module to register with it in order to communicate with platform specific devices.

The SD/MMC interfaces are normally 2 tier (ie Host and Device) whereas the USB interface is 3 tier (Host, Device and Driver)
however SDIO support adds a driver model to encapsulate support for device functions which can provide a variety of functionality
including WiFi, Bluetooth, Ethernet and others.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit MMC;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.DMA,
  Core.Storage,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  DMA,
  Storage,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

//To Do //For SPI based MMC/SDHCI see: \drivers\mmc\mmc_spi.c
        //For RPMB (Replay Protected Memory Block) see: \drivers\mmc\rpmb.c

        //mmc_select_hwpart / mmc_switch_part

        // Boot //mmc_boot_partition_size_change / mmc_set_boot_bus_width / mmc_set_part_conf / mmc_set_rst_n_function
        // RPMB //mmc_rpmb_set_key /  mmc_rpmb_get_counter / mmc_rpmb_read / mmc_rpmb_write

//To Do //UHS-I and II Initialize  (Tuning etc)
        //Host/Device Flags (Decode OCR/CID/CSD/SCR/SSR etc)
        //Host/Device Capabilities (specifically Device) (Decode OCR/CID/CSD/SCR/SSR etc)
        //Device Voltages (Decode OCR)

//To Do //TestingSDIO
        //Locks around some SDIOFunction handling (Interrupt etc)

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {MMC specific constants}
 MMC_NAME_PREFIX = 'MMC';    {Name prefix for MMC Devices}

 MMC_DEVICE_DESCRIPTION  = 'MMC/SD Device';         {Description of MMC/SD device}
 MMC_STORAGE_DESCRIPTION = 'MMC/SD Storage Device'; {Description of MMC/SD storage device}

 MMC_STATUS_TIMER_INTERVAL = 1000;

 MMC_DEFAULT_BLOCKSIZE = 512;
 MMC_DEFAULT_BLOCKSHIFT = 9;

 {MMC Device Types}
 MMC_TYPE_NONE      = 0;
 MMC_TYPE_MMC       = 1; {An MMC specification card}
 MMC_TYPE_SD        = 2; {An SD specification card}
 MMC_TYPE_SDIO      = 3; {An SDIO specification card}
 MMC_TYPE_SD_COMBO  = 4; {An SD/SDIO combination card}

 MMC_TYPE_MAX       = 4;

 {MMC Type Names}
 MMC_TYPE_NAMES:array[MMC_TYPE_NONE..MMC_TYPE_MAX] of String = (
  'MMC_TYPE_NONE',
  'MMC_TYPE_MMC',
  'MMC_TYPE_SD',
  'MMC_TYPE_SDIO',
  'MMC_TYPE_SD_COMBO');

 {MMC Device States}
 MMC_STATE_EJECTED  = 0;
 MMC_STATE_INSERTED = 1;

 MMC_STATE_MAX      = 1;

 {MMC State Names}
 MMC_STATE_NAMES:array[MMC_STATE_EJECTED..MMC_STATE_MAX] of String = (
  'MMC_STATE_EJECTED',
  'MMC_STATE_INSERTED');

 {MMC Device Flags}
 MMC_FLAG_NONE              = $00000000;
 MMC_FLAG_CARD_PRESENT      = $00000001;  {Card is present}
 MMC_FLAG_WRITE_PROTECT     = $00000002;  {Card is write protected}
 MMC_FLAG_HIGH_CAPACITY     = $00000004;  {High Capacity (SDHC)}
 MMC_FLAG_EXT_CAPACITY      = $00000008;  {Extended Capacity (SDXC)}
 MMC_FLAG_UHS_I             = $00000010;  {Ultra High Speed (UHS-I)}
 MMC_FLAG_UHS_II            = $00000020;  {Ultra High Speed (UHS-II)}
 MMC_FLAG_BLOCK_ADDRESSED   = $00000040;  {Block Addressed (SDHC/SDXC and others)}
 MMC_FLAG_AUTO_BLOCK_COUNT  = $00000080;  {Controller supports Auto CMD23 (Set Block Count)}
 MMC_FLAG_AUTO_COMMAND_STOP = $00000100;  {Controller supports Auto CMD12 (Stop Transmission)}
 MMC_FLAG_DDR_MODE          = $00000200;  {Device supports DDR mode}
 MMC_FLAG_NON_REMOVABLE     = $00000400;  {Device is non removable, only check for presence once}
 MMC_FLAG_SET_BLOCK_COUNT   = $00000800;  {Device supports CMD23 (Set Block Count)}

 {MMC/SD/SDIO Status Codes}
 MMC_STATUS_SUCCESS                   = 0;  {Function successful}
 MMC_STATUS_TIMEOUT                   = 1;  {The operation timed out}
 MMC_STATUS_NO_MEDIA                  = 2;  {No media present in device}
 MMC_STATUS_HARDWARE_ERROR            = 3;  {Hardware error of some form occurred}
 MMC_STATUS_INVALID_DATA              = 4;  {Invalid data was received}
 MMC_STATUS_INVALID_PARAMETER         = 5;  {An invalid parameter was passed to the function}
 MMC_STATUS_INVALID_SEQUENCE          = 6;  {Invalid sequence encountered}
 MMC_STATUS_OUT_OF_MEMORY             = 7;  {No memory available for operation}
 MMC_STATUS_UNSUPPORTED_REQUEST       = 8;  {The request is unsupported}
 MMC_STATUS_NOT_PROCESSED             = 9;  {The MMC transfer has not yet been processed}
 MMC_STATUS_OPERATION_FAILED          = 10; {The operation was not able to be completed}
 MMC_STATUS_DEVICE_DETACHED           = 11; {SDIO device was detached}
 MMC_STATUS_DEVICE_UNSUPPORTED        = 12; {SDIO device is unsupported by the driver}
 MMC_STATUS_NOT_BOUND                 = 13; {SDIO device is not bound to a driver}
 MMC_STATUS_ALREADY_BOUND             = 14; {SDIO device is already bound to a driver}
 MMC_STATUS_NOT_READY                 = 15; {The device is not ready yet, retry again later}

 {MMC/SD/SDIO Versions}
 SDIO_VERSION_SDIO   = $00040000;
 SDIO_VERSION_1_00   = (SDIO_VERSION_SDIO or $0100);
 SDIO_VERSION_1_10   = (SDIO_VERSION_SDIO or $010a);
 SDIO_VERSION_1_20   = (SDIO_VERSION_SDIO or $0114);
 SDIO_VERSION_2_00   = (SDIO_VERSION_SDIO or $0200);
 SDIO_VERSION_3_00   = (SDIO_VERSION_SDIO or $0300);

 SD_VERSION_SD       = $00020000;
 SD_VERSION_1_0      = (SD_VERSION_SD or $0100);
 SD_VERSION_1_10     = (SD_VERSION_SD or $010a);
 SD_VERSION_2        = (SD_VERSION_SD or $0200);
 SD_VERSION_3        = (SD_VERSION_SD or $0300);
 SD_VERSION_4        = (SD_VERSION_SD or $0400);

 MMC_VERSION_MMC     = $00010000;
 MMC_VERSION_1_2     = (MMC_VERSION_MMC or $0102);
 MMC_VERSION_1_4     = (MMC_VERSION_MMC or $0104);
 MMC_VERSION_2_2     = (MMC_VERSION_MMC or $0202);
 MMC_VERSION_3       = (MMC_VERSION_MMC or $0300);
 MMC_VERSION_4       = (MMC_VERSION_MMC or $0400);
 MMC_VERSION_4_1     = (MMC_VERSION_MMC or $0401);
 MMC_VERSION_4_2     = (MMC_VERSION_MMC or $0402);
 MMC_VERSION_4_3     = (MMC_VERSION_MMC or $0403);
 MMC_VERSION_4_41    = (MMC_VERSION_MMC or $0429);
 MMC_VERSION_4_5     = (MMC_VERSION_MMC or $0405);
 MMC_VERSION_5_0     = (MMC_VERSION_MMC or $0500);
 MMC_VERSION_5_1     = (MMC_VERSION_MMC or $0501);
 MMC_VERSION_UNKNOWN = (MMC_VERSION_MMC);

 {MMC/SD Capabilities (From: /include/linux/mmc/host.h)}
 MMC_CAP_4_BIT_DATA      = (1 shl 0); {Can the host do 4 bit transfers}
 MMC_CAP_MMC_HIGHSPEED   = (1 shl 1); {Can do MMC high-speed timing}
 MMC_CAP_SD_HIGHSPEED    = (1 shl 2); {Can do SD high-speed timing}
 MMC_CAP_SDIO_IRQ        = (1 shl 3); {Can signal pending SDIO IRQs}
 MMC_CAP_SPI             = (1 shl 4); {Talks only SPI protocols}
 MMC_CAP_NEEDS_POLL      = (1 shl 5); {Needs polling for card-detection}
 MMC_CAP_8_BIT_DATA      = (1 shl 6); {Can the host do 8 bit transfers}
 MMC_CAP_AGGRESSIVE_PM   = (1 shl 7); {Suspend = (e)MMC/SD at idle}
 MMC_CAP_NONREMOVABLE    = (1 shl 8); {Nonremovable eg. eMMC}
 MMC_CAP_WAIT_WHILE_BUSY = (1 shl 9); {Waits while card is busy}
 MMC_CAP_3_3V_DDR        = (1 shl 11); {Host supports eMMC DDR 3.3V}
 MMC_CAP_1_8V_DDR        = (1 shl 12); {Host supports eMMC DDR 1.8V}
 MMC_CAP_1_2V_DDR        = (1 shl 13); {Host supports eMMC DDR 1.2V}
 MMC_CAP_DDR             = (MMC_CAP_3_3V_DDR or MMC_CAP_1_8V_DDR or MMC_CAP_1_2V_DDR);
 MMC_CAP_POWER_OFF_CARD  = (1 shl 14); {Can power off after boot}
 MMC_CAP_BUS_WIDTH_TEST  = (1 shl 15); {CMD14/CMD19 bus width ok}
 MMC_CAP_UHS_SDR12       = (1 shl 16); {Host supports UHS SDR12 mode}
 MMC_CAP_UHS_SDR25       = (1 shl 17); {Host supports UHS SDR25 mode}
 MMC_CAP_UHS_SDR50       = (1 shl 18); {Host supports UHS SDR50 mode}
 MMC_CAP_UHS_SDR104      = (1 shl 19); {Host supports UHS SDR104 mode}
 MMC_CAP_UHS_DDR50       = (1 shl 20); {Host supports UHS DDR50 mode}
 MMC_CAP_UHS             = (MMC_CAP_UHS_SDR12 or MMC_CAP_UHS_SDR25 or MMC_CAP_UHS_SDR50 or MMC_CAP_UHS_SDR104 or MMC_CAP_UHS_DDR50);
 MMC_CAP_SYNC_RUNTIME_PM = (1 shl 21); {Synced runtime PM suspends}
 MMC_CAP_NEED_RSP_BUSY   = (1 shl 22); {Commands with R1B can't use R1}
 MMC_CAP_DRIVER_TYPE_A   = (1 shl 23); {Host supports Driver Type A}
 MMC_CAP_DRIVER_TYPE_C   = (1 shl 24); {Host supports Driver Type C}
 MMC_CAP_DRIVER_TYPE_D   = (1 shl 25); {Host supports Driver Type D}
 MMC_CAP_DONE_COMPLETE   = (1 shl 27); {RW reqs can be completed within mmc_request_done()}
 MMC_CAP_CD_WAKE         = (1 shl 28); {Enable card detect wake}
 MMC_CAP_CMD_DURING_TFR  = (1 shl 29); {Commands during data transfer}
 MMC_CAP_CMD23           = (1 shl 30); {CMD23 supported}
 MMC_CAP_HW_RESET        = (1 shl 31); {Reset the eMMC card via RST_n}

 {MMC/SD Capabilities2 (From: /include/linux/mmc/host.h)}
 MMC_CAP2_BOOTPART_NOACC            = (1 shl 0); {Boot partition no access}
 MMC_CAP2_FULL_PWR_CYCLE            = (1 shl 2); {Can do full power cycle}
 MMC_CAP2_FULL_PWR_CYCLE_IN_SUSPEND = (1 shl 3); {Can do full power cycle in suspend}
 MMC_CAP2_HS200_1_8V_SDR            = (1 shl 5); {Can support HS200 1.8V}
 MMC_CAP2_HS200_1_2V_SDR            = (1 shl 6); {Can support HS200 1.2V}
 MMC_CAP2_HS200                     = (MMC_CAP2_HS200_1_8V_SDR or MMC_CAP2_HS200_1_2V_SDR);
 MMC_CAP2_CD_ACTIVE_HIGH            = (1 shl 10); {Card-detect signal active high}
 MMC_CAP2_RO_ACTIVE_HIGH            = (1 shl 11); {Write-protect signal active high}
 MMC_CAP2_NO_PRESCAN_POWERUP        = (1 shl 14); {Don't power up before scan}
 MMC_CAP2_HS400_1_8V                = (1 shl 15); {Can support HS400 1.8V}
 MMC_CAP2_HS400_1_2V                = (1 shl 16); {Can support HS400 1.2V}
 MMC_CAP2_HS400                     = (MMC_CAP2_HS400_1_8V or MMC_CAP2_HS400_1_2V);
 MMC_CAP2_HSX00_1_8V                = (MMC_CAP2_HS200_1_8V_SDR or MMC_CAP2_HS400_1_8V);
 MMC_CAP2_HSX00_1_2V                = (MMC_CAP2_HS200_1_2V_SDR or MMC_CAP2_HS400_1_2V);
 MMC_CAP2_SDIO_IRQ_NOTHREAD         = (1 shl 17); {Don't create a thread to poll for SDIO IRQ}
 MMC_CAP2_NO_WRITE_PROTECT          = (1 shl 18); {No physical write protect pin, assume that card is always read-write}
 MMC_CAP2_NO_SDIO                   = (1 shl 19); {Do not send SDIO commands during initialization}
 MMC_CAP2_HS400_ES                  = (1 shl 20); {Host supports enhanced strobe}
 MMC_CAP2_NO_SD                     = (1 shl 21); {Do not send SD commands during initialization}
 MMC_CAP2_NO_MMC                    = (1 shl 22); {Do not send = (e)MMC commands during initialization}
 MMC_CAP2_CQE                       = (1 shl 23); {Has eMMC command queue engine}
 MMC_CAP2_CQE_DCMD                  = (1 shl 24); {CQE can issue a direct command}
 MMC_CAP2_AVOID_3_3V                = (1 shl 25); {Host must negotiate down from 3.3V}
 MMC_CAP2_MERGE_CAPABLE             = (1 shl 26); {Host can merge a segment over the segment size}

 {MMC/SD Directions}
 MMC_DATA_READ        = 1;
 MMC_DATA_WRITE        = 2;

 {MMC/SD Bus Widths}
 MMC_BUS_WIDTH_1    = 0;
 MMC_BUS_WIDTH_4    = 2;
 MMC_BUS_WIDTH_8    = 3;

 {MMC Bus Speeds (Hz)}
 MMC_BUS_SPEED_DEFAULT   = 0;
 MMC_BUS_SPEED_HS26      = 26000000;
 MMC_BUS_SPEED_HS52      = 52000000;
 MMC_BUS_SPEED_DDR       = 52000000;
 MMC_BUS_SPEED_HS200     = 200000000;

 {MMC/SD Timing (From: /include/linux/mmc/host.h)}
 MMC_TIMING_LEGACY        = 0;
 MMC_TIMING_MMC_HS        = 1;
 MMC_TIMING_SD_HS        = 2;
 MMC_TIMING_UHS_SDR12    = 3;
 MMC_TIMING_UHS_SDR25    = 4;
 MMC_TIMING_UHS_SDR50    = 5;
 MMC_TIMING_UHS_SDR104    = 6;
 MMC_TIMING_UHS_DDR50    = 7;
 MMC_TIMING_MMC_DDR52    = 8;
 MMC_TIMING_MMC_HS200    = 9;
 MMC_TIMING_MMC_HS400    = 10;

 {MMC/SD Signal Voltage (From: /include/linux/mmc/host.h)}
 MMC_SIGNAL_VOLTAGE_330 = 0;
 MMC_SIGNAL_VOLTAGE_180 = 1;
 MMC_SIGNAL_VOLTAGE_120 = 2;

 {MMC/SD Driver Type (From: /include/linux/mmc/host.h)}
 MMC_SET_DRIVER_TYPE_B = 0;
 MMC_SET_DRIVER_TYPE_A = 1;
 MMC_SET_DRIVER_TYPE_C = 2;
 MMC_SET_DRIVER_TYPE_D = 3;

 {MMC Commands (From: /include/linux/mmc/mmc.h)}
 {Class 1}
 MMC_CMD_GO_IDLE_STATE          = 0;
 MMC_CMD_SEND_OP_COND          = 1;
 MMC_CMD_ALL_SEND_CID          = 2;
 MMC_CMD_SET_RELATIVE_ADDR      = 3;
 MMC_CMD_SET_DSR              = 4;
 MMC_CMD_SLEEP_AWAKE          = 5;
 MMC_CMD_SWITCH                  = 6;
 MMC_CMD_SELECT_CARD          = 7;
 MMC_CMD_SEND_EXT_CSD          = 8;
 MMC_CMD_SEND_CSD              = 9;
 MMC_CMD_SEND_CID              = 10;
 MMC_CMD_READ_DAT_UNTIL_STOP  = 11;
 MMC_CMD_STOP_TRANSMISSION      = 12;
 MMC_CMD_SEND_STATUS          = 13;
 MMC_CMD_BUS_TEST_R           = 14;
 MMC_CMD_GO_INACTIVE_STATE    = 15;
 MMC_CMD_BUS_TEST_W           = 19;
 MMC_CMD_SPI_READ_OCR          = 58;
 MMC_CMD_SPI_CRC_ON_OFF          = 59;

 {Class 2}
 MMC_CMD_SET_BLOCKLEN          = 16;
 MMC_CMD_READ_SINGLE_BLOCK      = 17;
 MMC_CMD_READ_MULTIPLE_BLOCK  = 18;
 MMC_CMD_SEND_TUNING_BLOCK    = 19;
 MMC_CMD_SEND_TUNING_BLOCK_HS200 = 21;

 {Class 3}
 MMC_CMD_WRITE_DAT_UNTIL_STOP = 20;

 {Class 4}
 MMC_CMD_SET_BLOCK_COUNT      = 23;
 MMC_CMD_WRITE_SINGLE_BLOCK   =    24;
 MMC_CMD_WRITE_MULTIPLE_BLOCK =    25;
 MMC_CMD_PROGRAM_CID          = 26;
 MMC_CMD_PROGRAM_CSD          = 27;

 {Class 6}
 MMC_CMD_SET_WRITE_PROT       = 28;
 MMC_CMD_CLR_WRITE_PROT       = 29;
 MMC_CMD_SEND_WRITE_PROT      = 30;

 {Class 5}
 MMC_CMD_ERASE_GROUP_START      = 35;
 MMC_CMD_ERASE_GROUP_END      = 36;
 MMC_CMD_ERASE                  = 38;

 {Class 9}
 MMC_CMD_FAST_IO              = 39;
 MMC_CMD_GO_IRQ_STATE         = 40;

 {Class 7}
 MMC_CMD_LOCK_UNLOCK          = 42;

 {Class 8}
 MMC_CMD_APP_CMD              = 55;
 MMC_CMD_GEN_CMD              = 56;
 MMC_CMD_RES_MAN              = 62;

 MMC_CMD62_ARG1            = $EFAC62EC;
 MMC_CMD62_ARG2            = $00CBAEA7;

 {MMC Response Types (From: /include/linux/mmc/mmc.h)}
 {Native}
 MMC_RSP_PRESENT = (1 shl 0);
 MMC_RSP_136     = (1 shl 1); {136 bit response}
 MMC_RSP_CRC     = (1 shl 2); {Expect valid crc}
 MMC_RSP_BUSY     = (1 shl 3); {Card may send busy}
 MMC_RSP_OPCODE     = (1 shl 4); {Response contains opcode}

 {These are the native response types, and correspond to valid bit patterns of the above flags. One additional valid pattern is all zeros, which means we don't expect a response}
 MMC_RSP_NONE    = (0);
 MMC_RSP_R1        = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);
 MMC_RSP_R1B    = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE or MMC_RSP_BUSY);
 MMC_RSP_R2        = (MMC_RSP_PRESENT or MMC_RSP_136 or MMC_RSP_CRC);
 MMC_RSP_R3        = (MMC_RSP_PRESENT);
 MMC_RSP_R4        = (MMC_RSP_PRESENT);
 MMC_RSP_R5        = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);
 MMC_RSP_R6        = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);
 MMC_RSP_R7        = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);

 {Non-SPI Command Type Flags}
 MMC_CMD_MASK = (3 shl 5);
 MMC_CMD_AC   = (0 shl 5); {Addressed Command, no Data}
 MMC_CMD_ADTC = (1 shl 5); {Addressed Data Transfer Command}
 MMC_CMD_BC   = (2 shl 5); {Broadcast Command, no Response}
 MMC_CMD_BCR  = (3 shl 5); {Broadcast Command with Response}

 {SPI}
 MMC_RSP_SPI_S1      = (1 shl 7);        {One status byte}
 MMC_RSP_SPI_S2      = (1 shl 8);        {Second byte}
 MMC_RSP_SPI_B4      = (1 shl 9);        {Four data bytes}
 MMC_RSP_SPI_BUSY = (1 shl 10);        {Card may send busy}

 {These are the SPI response types for MMC, SD, and SDIO cards. Commands return R1, with maybe more info. Zero is an error type, callers must always provide the appropriate MMC_RSP_SPI_Rx flags}
 MMC_RSP_SPI_R1      = (MMC_RSP_SPI_S1);
 MMC_RSP_SPI_R1B  = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_BUSY);
 MMC_RSP_SPI_R2      = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_S2);
 MMC_RSP_SPI_R3      = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_B4);
 MMC_RSP_SPI_R4      = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_B4);
 MMC_RSP_SPI_R5      = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_S2);
 MMC_RSP_SPI_R7      = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_B4);

 {MMC Response Values}
 {R1 - MMC status in R1, for native mode (SPI bits are different)}
 MMC_RSP_R1_OUT_OF_RANGE        = (1 shl 31);
 MMC_RSP_R1_ADDRESS_ERROR        = (1 shl 30);
 MMC_RSP_R1_BLOCK_LEN_ERROR        = (1 shl 29);
 MMC_RSP_R1_ERASE_SEQ_ERROR     = (1 shl 28);
 MMC_RSP_R1_ERASE_PARAM            = (1 shl 27);
 MMC_RSP_R1_WP_VIOLATION        = (1 shl 26);
 MMC_RSP_R1_CARD_IS_LOCKED        = (1 shl 25);
 MMC_RSP_R1_LOCK_UNLOCK_FAILED    = (1 shl 24);
 MMC_RSP_R1_COM_CRC_ERROR        = (1 shl 23);
 MMC_RSP_R1_ILLEGAL_COMMAND        = (1 shl 22);
 MMC_RSP_R1_CARD_ECC_FAILED        = (1 shl 21);
 MMC_RSP_R1_CC_ERROR            = (1 shl 20);
 MMC_RSP_R1_ERROR                = (1 shl 19);
 MMC_RSP_R1_UNDERRUN            = (1 shl 18);
 MMC_RSP_R1_OVERRUN                = (1 shl 17);
 MMC_RSP_R1_CID_CSD_OVERWRITE    = (1 shl 16);
 MMC_RSP_R1_WP_ERASE_SKIP        = (1 shl 15);
 MMC_RSP_R1_CARD_ECC_DISABLED    = (1 shl 14);
 MMC_RSP_R1_ERASE_RESET            = (1 shl 13);
 //MMC_RSP_R1_STATUS(x)            (x & 0xFFFFE000)
 //MMC_RSP_R1_CURRENT_STATE(x)    ((x & 0x00001E00) >> 9)
 MMC_RSP_R1_READY_FOR_DATA        = (1 shl 8);
 MMC_RSP_R1_SWITCH_ERROR        = (1 shl 7);
 MMC_RSP_R1_EXCEPTION_EVENT        = (1 shl 6);
 MMC_RSP_R1_APP_CMD                = (1 shl 5);
 MMC_RSP_R1_AKE_SEQ_ERROR       = (1 shl 3);

 {R1 SPI - MMC/SD in SPI mode reports R1 status always, and R2 for SEND_STATUS. R1 is the low order byte, R2 is the next highest byte, when present}
 MMC_RSP_R1_SPI_IDLE            = (1 shl 0);
 MMC_RSP_R1_SPI_ERASE_RESET        = (1 shl 1);
 MMC_RSP_R1_SPI_ILLEGAL_COMMAND    = (1 shl 2);
 MMC_RSP_R1_SPI_COM_CRC            = (1 shl 3);
 MMC_RSP_R1_SPI_ERASE_SEQ        = (1 shl 4);
 MMC_RSP_R1_SPI_ADDRESS            = (1 shl 5);
 MMC_RSP_R1_SPI_PARAMETER        = (1 shl 6);
 {R1 bit 7 is always zero}

 {R2 SPI - See above}
 MMC_RSP_R2_SPI_CARD_LOCKED         = (1 shl 8);
 MMC_RSP_R2_SPI_WP_ERASE_SKIP     = (1 shl 9);    {Or lock/unlock fail}
 MMC_RSP_R2_SPI_LOCK_UNLOCK_FAIL = MMC_RSP_R2_SPI_WP_ERASE_SKIP;
 MMC_RSP_R2_SPI_ERROR             = (1 shl 10);
 MMC_RSP_R2_SPI_CC_ERROR         = (1 shl 11);
 MMC_RSP_R2_SPI_CARD_ECC_ERROR     = (1 shl 12);
 MMC_RSP_R2_SPI_WP_VIOLATION     = (1 shl 13);
 MMC_RSP_R2_SPI_ERASE_PARAM         = (1 shl 14);
 MMC_RSP_R2_SPI_OUT_OF_RANGE     = (1 shl 15);    {Or CSD overwrite}
 MMC_RSP_R2_SPI_CSD_OVERWRITE     = MMC_RSP_R2_SPI_OUT_OF_RANGE;

 {MMC Operation Condition Register (OCR) values} {See: Section 5.1 of SD Physical Layer Simplified Specification V4.10}
 MMC_OCR_BUSY           = $80000000; {Busy Status - 0 = Initializing / 1 = Initialization Complete}
 MMC_OCR_HCS           = $40000000; {Card Capacity Status - 0 = SDSC / 1 = SDHC or SDXC}
 MMC_OCR_UHS_II        = $20000000; {UHS-II Card Status - 0 = Non UHS-II Card / 1 = UHS-II Card}
 MMC_OCR_S18A          = $01000000; {Switching to 1.8V Accepted - 0 = Continue current voltage signaling / 1 = Ready for switching signal voltage}
 MMC_OCR_VOLTAGE_MASK  = $007FFF80;
 MMC_OCR_ACCESS_MODE   = $60000000; //To Do //??

 {MMC Card Status Register (CSR) values} {See: Section 4.10.1 of SD Physical Layer Simplified Specification Version 4.10}
 {Note: These map to the Native mode R1 response values}
 MMC_CARD_STATUS_MASK            = not($0206BF7F);  //To Do //??
 MMC_CARD_STATUS_ERROR            = (1 shl 19);
 MMC_CARD_STATUS_CURRENT_STATE  = ($0F shl 9); {See MMC_CURRENT_STATE_ definitions below}
 MMC_CARD_STATUS_READY_FOR_DATA = (1 shl 8);
 MMC_CARD_STATUS_SWITCH_ERROR   = (1 shl 7);

 {MMC Current State values (From Card Status Register or R1 Response)}
 MMC_CURRENT_STATE_IDLE      = (0 shl 9);
 MMC_CURRENT_STATE_READY     = (1 shl 9);
 MMC_CURRENT_STATE_IDENT     = (2 shl 9);
 MMC_CURRENT_STATE_STBY      = (3 shl 9);
 MMC_CURRENT_STATE_TRAN      = (4 shl 9);
 MMC_CURRENT_STATE_DATA      = (5 shl 9);
 MMC_CURRENT_STATE_RCV       = (6 shl 9);
 MMC_CURRENT_STATE_PRG         = (7 shl 9);
 MMC_CURRENT_STATE_DIS       = (8 shl 9);

 {MMC Card Identification Data (CID) values} {See: Section 5.2 of SD Physical Layer Simplified Specification Version 4.10}
 {MMC CID Fields}
 MMC_CID_MID       = 1;  {Manufacturer ID}
 MMC_CID_OID       = 2;  {OEM/Application ID}
 MMC_CID_PNM0      = 3;  {Product name (Byte 0)}
 MMC_CID_PNM1      = 4;  {Product name (Byte 1)}
 MMC_CID_PNM2      = 5;  {Product name (Byte 2)}
 MMC_CID_PNM3      = 6;  {Product name (Byte 3)}
 MMC_CID_PNM4      = 7;  {Product name (Byte 4)}
 MMC_CID_PNM5      = 8;  {Product name (Byte 5)}
 MMC_CID_PNM6      = 9;  {Product name (Byte 6)}
 MMC_CID_PRV       = 10; {Product revision}
 MMC_CID_HRV       = 11; {Hardware revision}
 MMC_CID_FRV       = 12; {Firmware revision}
 MMC_CID_PSN       = 13; {Product serial number}
 MMC_CID_MDT_YEAR  = 14; {Manufacturing year}
 MMC_CID_MDT_MONTH = 15; {Manufacturing month}
 MMC_CID_CRC       = 16; {CRC}

 {MMC Card Specific Data (CSD) values} {See: Section 5.3 of SD Physical Layer Simplified Specification Version 4.10}
 {MMC CSD Fields}
 MMC_CSD_STRUCTURE            = 1;
 MMC_CSD_SPECVER              = 2; {MMC/eMMC Only}
 MMC_CSD_TAAC_UNIT            = 3;
 MMC_CSD_TAAC_VALUE           = 4;
 MMC_CSD_NSAC                 = 5;
 MMC_CSD_TRAN_SPEED_UNIT      = 6;
 MMC_CSD_TRAN_SPEED_VALUE     = 37;
 MMC_CSD_CCC                  = 7;
 MMC_CSD_READ_BL_LEN          = 8;
 MMC_CSD_READ_BL_PARTIAL      = 9;
 MMC_CSD_WRITE_BLK_MISALIGN   = 10;
 MMC_CSD_READ_BLK_MISALIGN    = 11;
 MMC_CSD_DSR_IMP              = 12;
 MMC_CSD_C_SIZE               = 13;
 MMC_CSD_VDD_R_CURR_MIN       = 14;
 MMC_CSD_VDD_R_CURR_MAX       = 15;
 MMC_CSD_VDD_W_CURR_MIN       = 16;
 MMC_CSD_VDD_W_CURR_MAX       = 17;
 MMC_CSD_C_SIZE_MULT          = 18;
 MMC_CSD_ERASE_BLK_EN         = 19; {SD Specification}
 MMC_CSD_SECTOR_SIZE          = 20; {MMC/eMMC Specification / SD Specification}
 MMC_CSD_ERASE_GRP_SIZE       = 21; {MMC/eMMC Specification}
 MMC_CSD_ERASE_GRP_MULT       = 22; {MMC/eMMC Specification}
 MMC_CSD_WP_GRP_SIZE          = 23;
 MMC_CSD_WP_GRP_ENABLE        = 24;
 MMC_CSD_DEFAULT_ECC          = 25; {MMC/eMMC Only}
 MMC_CSD_R2W_FACTOR           = 26;
 MMC_CSD_WRITE_BL_LEN         = 27;
 MMC_CSD_WRITE_BL_PARTIAL     = 28;
 MMC_CSD_CONTENT_PROT_APP     = 29; {MMC/eMMC Only}
 MMC_CSD_FILE_FORMAT_GRP      = 30;
 MMC_CSD_COPY                 = 31;
 MMC_CSD_PERM_WRITE_PROTECT   = 32;
 MMC_CSD_TMP_WRITE_PROTECT    = 33;
 MMC_CSD_FILE_FORMAT          = 34;
 MMC_CSD_ECC                  = 35; {MMC/eMMC Only}
 MMC_CSD_CRC                  = 36;

 {MMC CSD Structure values}
 MMC_CSD_STRUCT_VER_1_0  = 0;           {Valid for system specification 1.0 - 1.2}
 MMC_CSD_STRUCT_VER_1_1  = 1;           {Valid for system specification 1.4 - 2.2}
 MMC_CSD_STRUCT_VER_1_2  = 2;           {Valid for system specification 3.1 - 3.2 - 3.31 - 4.0 - 4.1}
 MMC_CSD_STRUCT_EXT_CSD  = 3;           {Version is coded in CSD_STRUCTURE in EXT_CSD}

 {MMC CSD Spec Version values}
 MMC_CSD_SPEC_VER_0   = 0;           {Implements system specification 1.0 - 1.2}
 MMC_CSD_SPEC_VER_1   = 1;           {Implements system specification 1.4}
 MMC_CSD_SPEC_VER_2   = 2;           {Implements system specification 2.0 - 2.2}
 MMC_CSD_SPEC_VER_3   = 3;           {Implements system specification 3.1 - 3.2 - 3.31}
 MMC_CSD_SPEC_VER_4   = 4;           {Implements system specification 4.0 - 4.1}

 {MMC CSD TAAC Time units}
 MMC_CSD_TAAC_UNITS:array[0..7] of LongWord = (
  1,
  10,
  100,
  1000,
  10000,
  100000,
  1000000,
  10000000);

 {MMC CSD TAAC Time values}
 MMC_CSD_TAAC_VALUES:array[0..15] of LongWord = (
  0,
  10,
  12,
  13,
  15,
  20,
  25,
  30,
  35,
  40,
  45,
  50,
  55,
  60,
  70,
  80);

 {MMC CSD Card Command Class (CCC) values} {See: Table 4-21 (Page 68) of SD Physical Layer Simplified Specification V4.10}
 MMC_CCC_BASIC            = (1 shl 0);    {(Class 0) Basic protocol functions (CMD0,1,2,3,4,7,9,10,12,13,15) (and for SPI, CMD58,59)}
 MMC_CCC_STREAM_READ    = (1 shl 1);    {(Class 1) Stream read commands (CMD11)}
 MMC_CCC_BLOCK_READ        = (1 shl 2);    {(Class 2) Block read commands (CMD16,17,18)}
 MMC_CCC_STREAM_WRITE    = (1 shl 3);    {(Class 3) Stream write commands (CMD20)}
 MMC_CCC_BLOCK_WRITE    = (1 shl 4);    {(Class 4) Block write commands (CMD16,24,25,26,27)}
 MMC_CCC_ERASE            = (1 shl 5);    {(Class 5) Ability to erase blocks (CMD32,33,34,35,36,37,38,39)}
 MMC_CCC_WRITE_PROT        = (1 shl 6);    {(Class 6) Ability to write protect blocks (CMD28,29,30)}
 MMC_CCC_LOCK_CARD        = (1 shl 7);    {(Class 7) Ability to lock down card (CMD16,CMD42)}
 MMC_CCC_APP_SPEC        = (1 shl 8);    {(Class 8) Application specific (CMD55,56,57,ACMD*)}
 MMC_CCC_IO_MODE        = (1 shl 9);    {(Class 9) I/O mode (CMD5,39,40,52,53)}
 MMC_CCC_SWITCH            = (1 shl 10);    {(Class 10) High speed switch (CMD6,34,35,36,37,50)}
 MMC_CCC_EXTENSION        = (1 shl 11);   {(Class 11) Extension (CMD?)}

 {MMC CSD Transfer Speed Rate units}
 MMC_CSD_TRAN_SPEED_UNITS:array[0..7] of LongWord = (
  10000,
  100000,
  1000000,
  10000000,
  0,
  0,
  0,
  0);

 {MMC CSD Transfer Speed Time values}
 MMC_CSD_TRAN_SPEED_VALUES:array[0..15] of LongWord = (
  0,
  10,
  12,
  13,
  15,
  20,
  25,
  30,
  35,
  40,
  45,
  50,
  55,
  60,
  70,
  80);

 SECURE_ERASE       = $80000000; //To Do //?? (Used by MMC_CMD_ERASE)

 {MMC Voltage Values}
 MMC_VDD_165_195    = $00000080; {VDD voltage 1.65 - 1.95}
 MMC_VDD_20_21        = $00000100; {VDD voltage 2.0 ~ 2.1}
 MMC_VDD_21_22        = $00000200; {VDD voltage 2.1 ~ 2.2}
 MMC_VDD_22_23        = $00000400; {VDD voltage 2.2 ~ 2.3}
 MMC_VDD_23_24        = $00000800; {VDD voltage 2.3 ~ 2.4}
 MMC_VDD_24_25        = $00001000; {VDD voltage 2.4 ~ 2.5}
 MMC_VDD_25_26        = $00002000; {VDD voltage 2.5 ~ 2.6}
 MMC_VDD_26_27        = $00004000; {VDD voltage 2.6 ~ 2.7}
 MMC_VDD_27_28        = $00008000; {VDD voltage 2.7 ~ 2.8}
 MMC_VDD_28_29        = $00010000; {VDD voltage 2.8 ~ 2.9}
 MMC_VDD_29_30        = $00020000; {VDD voltage 2.9 ~ 3.0}
 MMC_VDD_30_31        = $00040000; {VDD voltage 3.0 ~ 3.1}
 MMC_VDD_31_32        = $00080000; {VDD voltage 3.1 ~ 3.2}
 MMC_VDD_32_33        = $00100000; {VDD voltage 3.2 ~ 3.3}
 MMC_VDD_33_34        = $00200000; {VDD voltage 3.3 ~ 3.4}
 MMC_VDD_34_35        = $00400000; {VDD voltage 3.4 ~ 3.5}
 MMC_VDD_35_36        = $00800000; {VDD voltage 3.5 ~ 3.6}

 {MMC Switch Mode Values}
 MMC_SWITCH_MODE_CMD_SET     = $00; {Change the command set}
 MMC_SWITCH_MODE_SET_BITS     = $01; {Set bits in EXT_CSD byte addressed by index which are 1 in value field}
 MMC_SWITCH_MODE_CLEAR_BITS     = $02; {Clear bits in EXT_CSD byte    addressed by index, which are 1 in value field}
 MMC_SWITCH_MODE_WRITE_BYTE     = $03; {Set target byte to value}

 {MMC EXT_CSD fields}
 EXT_CSD_CMDQ_MODE_EN = 15; {R/W}
 EXT_CSD_FLUSH_CACHE = 32; {W}
 EXT_CSD_CACHE_CTRL = 33; {R/W}
 EXT_CSD_POWER_OFF_NOTIFICATION = 34; {R/W}
 EXT_CSD_PACKED_FAILURE_INDEX = 35; {RO}
 EXT_CSD_PACKED_CMD_STATUS = 36; {RO}
 EXT_CSD_EXP_EVENTS_STATUS = 54; {RO, 2 bytes}
 EXT_CSD_EXP_EVENTS_CTRL = 56; {R/W, 2 bytes}
 EXT_CSD_DATA_SECTOR_SIZE = 61; {R}
 EXT_CSD_ENH_START_ADDR = 136; {R/W}
 EXT_CSD_ENH_SIZE_MULT = 140; {R/W}
 EXT_CSD_GP_SIZE_MULT = 143; {R/W}
 EXT_CSD_PARTITION_SETTING_COMPLETED = 155; {R/W}
 EXT_CSD_PARTITION_ATTRIBUTE = 156; {R/W}
 EXT_CSD_MAX_ENH_SIZE_MULT = 157; {R}
 EXT_CSD_PARTITION_SUPPORT = 160; {RO}
 EXT_CSD_HPI_MGMT = 161; {R/W}
 EXT_CSD_RST_N_FUNCTION = 162; {R/W}
 EXT_CSD_BKOPS_EN = 163; {R/W}
 EXT_CSD_BKOPS_START = 164; {W}
 EXT_CSD_SANITIZE_START = 165; {W}
 EXT_CSD_WR_REL_PARAM = 166; {RO}
 EXT_CSD_WR_REL_SET    = 167; {R/W}
 EXT_CSD_RPMB_MULT = 168; {RO}
 EXT_CSD_FW_CONFIG = 169; {R/W}
 EXT_CSD_BOOT_WP = 173; {R/W}
 EXT_CSD_ERASE_GROUP_DEF = 175; {R/W}
 EXT_CSD_BOOT_BUS_CONDITIONS = 177; {R/W/E}
 EXT_CSD_PART_CONFIG = 179; {R/W}
 EXT_CSD_ERASED_MEM_CONT = 181; {RO}
 EXT_CSD_BUS_WIDTH = 183; {R/W}
 EXT_CSD_STROBE_SUPPORT = 184; {RO}
 EXT_CSD_HS_TIMING = 185; {R/W}
 EXT_CSD_POWER_CLASS = 187; {R/W}
 EXT_CSD_REV = 192; {RO}
 EXT_CSD_STRUCTURE = 194; {RO}
 EXT_CSD_CARD_TYPE = 196; {RO}
 EXT_CSD_DRIVER_STRENGTH = 197; {RO}
 EXT_CSD_OUT_OF_INTERRUPT_TIME = 198; {RO}
 EXT_CSD_PART_SWITCH_TIME = 199; {RO}
 EXT_CSD_PWR_CL_52_195 = 200; {RO}
 EXT_CSD_PWR_CL_26_195 = 201; {RO}
 EXT_CSD_PWR_CL_52_360 = 202; {RO}
 EXT_CSD_PWR_CL_26_360 = 203; {RO}
 EXT_CSD_SEC_CNT = 212; {RO, 4 bytes}
 EXT_CSD_S_A_TIMEOUT = 217; {RO}
 EXT_CSD_REL_WR_SEC_C = 222; {RO}
 EXT_CSD_HC_WP_GRP_SIZE = 221; {RO}
 EXT_CSD_ERASE_TIMEOUT_MULT = 223; {RO}
 EXT_CSD_HC_ERASE_GRP_SIZE = 224; {RO}
 EXT_CSD_BOOT_SIZE_MULT = 226; {RO}
 EXT_CSD_SEC_TRIM_MULT = 229; {RO}
 EXT_CSD_SEC_ERASE_MULT = 230; {RO}
 EXT_CSD_SEC_FEATURE_SUPPORT = 231; {RO}
 EXT_CSD_TRIM_MULT = 232; {RO}
 EXT_CSD_PWR_CL_200_195 = 236; {RO}
 EXT_CSD_PWR_CL_200_360 = 237; {RO}
 EXT_CSD_PWR_CL_DDR_52_195 = 238; {RO}
 EXT_CSD_PWR_CL_DDR_52_360 = 239; {RO}
 EXT_CSD_BKOPS_STATUS = 246; {RO}
 EXT_CSD_POWER_OFF_LONG_TIME = 247; {RO}
 EXT_CSD_GENERIC_CMD6_TIME = 248; {RO}
 EXT_CSD_CACHE_SIZE = 249; {RO, 4 bytes}
 EXT_CSD_PWR_CL_DDR_200_360 = 253; {RO}
 EXT_CSD_FIRMWARE_VERSION = 254; {RO, 8 bytes}
 EXT_CSD_PRE_EOL_INFO = 267; {RO}
 EXT_CSD_DEVICE_LIFE_TIME_EST_TYP_A = 268; {RO}
 EXT_CSD_DEVICE_LIFE_TIME_EST_TYP_B = 269; {RO}
 EXT_CSD_CMDQ_DEPTH = 307; {RO}
 EXT_CSD_CMDQ_SUPPORT = 308; {RO}
 EXT_CSD_SUPPORTED_MODE = 493; {RO}
 EXT_CSD_TAG_UNIT_SIZE = 498; {RO}
 EXT_CSD_DATA_TAG_SUPPORT = 499; {RO}
 EXT_CSD_MAX_PACKED_WRITES = 500; {RO}
 EXT_CSD_MAX_PACKED_READS = 501; {RO}
 EXT_CSD_BKOPS_SUPPORT = 502; {RO}
 EXT_CSD_HPI_FEATURES = 503; {RO}

 {MMC EXT_CSD field definitions}
 EXT_CSD_PARTITION_ATTRIBUTE_ENH_4   = $10;
 EXT_CSD_PARTITION_ATTRIBUTE_ENH_3   = $08;
 EXT_CSD_PARTITION_ATTRIBUTE_ENH_2   = $04;
 EXT_CSD_PARTITION_ATTRIBUTE_ENH_1   = $02;
 EXT_CSD_PARTITION_ATTRIBUTE_ENH_USR = $01;

 EXT_CSD_PARTITION_EXT_ATTRIBUTE_EN = $04;
 EXT_CSD_PARTITION_ENH_ATTRIBUTE_EN = $02;
 EXT_CSD_PARTITION_PARTITIONING_EN  = $01;

 EXT_CSD_WR_REL_PARAM_EN = (1 shl 2);
 EXT_CSD_WR_REL_PARAM_EN_RPMB_REL_WR = (1 shl 4);

 EXT_CSD_BOOT_WP_B_PWR_WP_DIS = $40;
 EXT_CSD_BOOT_WP_B_PERM_WP_DIS = $10;
 EXT_CSD_BOOT_WP_B_PERM_WP_EN = $04;
 EXT_CSD_BOOT_WP_B_PWR_WP_EN = $01;

 EXT_CSD_PART_CONFIG_ACC_MASK = $07;
 EXT_CSD_PART_CONFIG_ACC_BOOT0 = $01;
 EXT_CSD_PART_CONFIG_ACC_RPMB = $03;
 EXT_CSD_PART_CONFIG_ACC_GP0 = $04;

 EXT_CSD_PART_SETTING_COMPLETED = $01;
 EXT_CSD_PART_SUPPORT_PART_EN = $01;

 EXT_CSD_CMD_SET_NORMAL = (1 shl 0);
 EXT_CSD_CMD_SET_SECURE = (1 shl 1);
 EXT_CSD_CMD_SET_CPSECURE = (1 shl 2);

 EXT_CSD_CARD_TYPE_HS_26 = (1 shl 0); {Card can run at 26MHz}
 EXT_CSD_CARD_TYPE_HS_52 = (1 shl 1); {Card can run at 52MHz}
 EXT_CSD_CARD_TYPE_HS = (EXT_CSD_CARD_TYPE_HS_26 or EXT_CSD_CARD_TYPE_HS_52);
 EXT_CSD_CARD_TYPE_DDR_1_8V = (1 shl 2); {Card can run at 52MHz / DDR mode @1.8V or 3V I/O}
 EXT_CSD_CARD_TYPE_DDR_1_2V = (1 shl 3); {Card can run at 52MHz / DDR mode @1.2V I/O}
 EXT_CSD_CARD_TYPE_DDR_52 = (EXT_CSD_CARD_TYPE_DDR_1_8V or EXT_CSD_CARD_TYPE_DDR_1_2V);
 EXT_CSD_CARD_TYPE_HS200_1_8V = (1 shl 4); {Card can run at 200MHz}
 EXT_CSD_CARD_TYPE_HS200_1_2V = (1 shl 5); {Card can run at 200MHz / SDR mode @1.2V I/O}
 EXT_CSD_CARD_TYPE_HS200 = (EXT_CSD_CARD_TYPE_HS200_1_8V or EXT_CSD_CARD_TYPE_HS200_1_2V);
 EXT_CSD_CARD_TYPE_HS400_1_8V = (1 shl 6); {Card can run at 200MHz DDR, 1.8V}
 EXT_CSD_CARD_TYPE_HS400_1_2V = (1 shl 7); {Card can run at 200MHz DDR, 1.2V}
 EXT_CSD_CARD_TYPE_HS400 = (EXT_CSD_CARD_TYPE_HS400_1_8V or EXT_CSD_CARD_TYPE_HS400_1_2V);
 EXT_CSD_CARD_TYPE_HS400ES = (1 shl 8); {Card can run at HS400ES}

 EXT_CSD_BUS_WIDTH_1 = 0; {Card is in 1 bit mode}
 EXT_CSD_BUS_WIDTH_4 = 1; {Card is in 4 bit mode}
 EXT_CSD_BUS_WIDTH_8 = 2; {Card is in 8 bit mode}
 EXT_CSD_DDR_BUS_WIDTH_4 = 5; {Card is in 4 bit DDR mode}
 EXT_CSD_DDR_BUS_WIDTH_8 = 6; {Card is in 8 bit DDR mode}
 EXT_CSD_BUS_WIDTH_STROBE = 1 shl 7; {Enhanced strobe mode}

 EXT_CSD_TIMING_BC = 0; {Backwards compatility}
 EXT_CSD_TIMING_HS = 1; {High speed}
 EXT_CSD_TIMING_HS200 = 2; {HS200}
 EXT_CSD_TIMING_HS400 = 3; {HS400}
 EXT_CSD_DRV_STR_SHIFT = 4; {Driver Strength shift}

 EXT_CSD_SEC_ER_EN = 1 shl 0;
 EXT_CSD_SEC_BD_BLK_EN = 1 shl 2;
 EXT_CSD_SEC_GB_CL_EN = 1 shl 4;
 EXT_CSD_SEC_SANITIZE = 1 shl 6; {v4.5 only}

 EXT_CSD_RST_N_EN_MASK = $03;
 EXT_CSD_RST_N_ENABLED = 1; {RST_n is enabled on card}

 EXT_CSD_NO_POWER_NOTIFICATION = 0;
 EXT_CSD_POWER_ON = 1;
 EXT_CSD_POWER_OFF_SHORT = 2;
 EXT_CSD_POWER_OFF_LONG = 3;

 EXT_CSD_PWR_CL_8BIT_MASK = $F0; {8 bit PWR CLS}
 EXT_CSD_PWR_CL_4BIT_MASK = $0F; {8 bit PWR CLS}
 EXT_CSD_PWR_CL_8BIT_SHIFT = 4;
 EXT_CSD_PWR_CL_4BIT_SHIFT = 0;

 EXT_CSD_PACKED_EVENT_EN = 1 shl 3;

 {EXCEPTION_EVENT_STATUS field}
 EXT_CSD_URGENT_BKOPS = 1 shl 0;
 EXT_CSD_DYNCAP_NEEDED = 1 shl 1;
 EXT_CSD_SYSPOOL_EXHAUSTED = 1 shl 2;
 EXT_CSD_PACKED_FAILURE = 1 shl 3;

 EXT_CSD_PACKED_GENERIC_ERROR = 1 shl 0;
 EXT_CSD_PACKED_INDEXED_ERROR = 1 shl 1;

 {BKOPS status level}
 EXT_CSD_BKOPS_LEVEL_2 = $02;

 {BKOPS modes}
 EXT_CSD_MANUAL_BKOPS_MASK = $01;
 EXT_CSD_AUTO_BKOPS_MASK = $02;

 {Command Queue}
 EXT_CSD_CMDQ_MODE_ENABLED = 1 shl 0;
 EXT_CSD_CMDQ_DEPTH_MASK = $1F;
 EXT_CSD_CMDQ_SUPPORTED = 1 shl 0;

 {High Speed Max}
 MMC_HIGH_26_MAX_DTR = 26000000;
 MMC_HIGH_52_MAX_DTR = 52000000;
 MMC_HIGH_DDR_MAX_DTR = 52000000;
 MMC_HS200_MAX_DTR = 200000000;

 {Minimum partition switch timeout}
 MMC_MIN_PART_SWITCH_TIME = 300; {Milliseconds}

 {MMC ?????}
 MMCPART_NOAVAILABLE = ($ff);
 PART_ACCESS_MASK     = ($07);
 PART_SUPPORT         = ($01);
 ENHNCD_SUPPORT         = ($02);
 PART_ENH_ATTRIB     = ($1f);

 {Maximum block size for MMC}
 MMC_MAX_BLOCK_LEN    = 512;

 {Maximum block count for MMC}
 MMC_MAX_BLOCK_COUNT = 65535;

 {The number of MMC physical partitions.  These consist of: boot partitions (2), general purpose partitions (4) and RPMB partition (1) in MMC v4.4}
 MMC_NUM_BOOT_PARTITION    = 2;
 MMC_NUM_GP_PARTITION   = 4;
 MMC_NUM_PHY_PARTITION  = 7;

 {Timeouts}
 MMC_DEFAULT_CMD6_TIMEOUT_MS = 500;
 MMC_MIN_CACHE_EN_TIMEOUT_MS = 1600;

 {Sizes}
 MMC_FIRMWARE_VERSION_LEN = 8;

 {Version specific features}
 MMC_DISCARD_FEATURE = $01;

 {Busy Poll Commands}
 MMC_BUSY_CMD6  = 0;
 MMC_BUSY_ERASE = 1;
 MMC_BUSY_HPI   = 2;

 {Erase/Trim/Discard Arguments}
 MMC_ERASE_ARG        = $00000000;
 MMC_SECURE_ERASE_ARG = $80000000;
 MMC_TRIM_ARG         = $00000001;
 MMC_DISCARD_ARG      = $00000003;
 MMC_SECURE_TRIM1_ARG = $80000001;
 MMC_SECURE_TRIM2_ARG = $80008000;
 MMC_SECURE_ARGS      = $80000000;
 MMC_TRIM_ARGS        = $00008001;

 {MMC logging}
 MMC_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {MMC debugging messages}
 MMC_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {MMC informational messages, such as a device being attached or detached}
 MMC_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {MMC warning messages}
 MMC_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {MMC error messages}
 MMC_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No MMC messages}

var
 MMC_DEFAULT_LOG_LEVEL:LongWord = MMC_LOG_LEVEL_DEBUG; {Minimum level for MMC messages.  Only messages with level greater than or equal to this will be printed}

var
 {MMC logging}
 MMC_LOG_ENABLED:Boolean;

{==============================================================================}
const
 {SD specific constants}
 SD_DEFAULT_BLOCKSIZE = 512;
 SD_DEFAULT_BLOCKSHIFT = 9;

 {SD Bus Widths}
 SD_BUS_WIDTH_1  = 0;
 SD_BUS_WIDTH_4  = 2;

 {SD Bus Speeds (Hz)}
 SD_BUS_SPEED_DEFAULT    = 25000000;
 SD_BUS_SPEED_HS         = 50000000;
 SD_BUS_SPEED_UHS_SDR12  = 25000000;
 SD_BUS_SPEED_UHS_SDR25  = 50000000;
 SD_BUS_SPEED_UHS_DDR50  = 50000000;
 SD_BUS_SPEED_UHS_SDR50  = 100000000;
 SD_BUS_SPEED_UHS_SDR104 = 208000000;

 {SD Commands (From: /include/linux/mmc/sd.h)}
 {Class 0}
 SD_CMD_SEND_RELATIVE_ADDR      = 3;
 SD_CMD_SEND_IF_COND          = 8;
 SD_CMD_SWITCH_VOLTAGE        = 11;

 {Class 10}
 SD_CMD_SWITCH                = 6;  {See: 4.3.10 Switch Function Command}

 {Class 5}
 SD_CMD_ERASE_WR_BLK_START      = 32;
 SD_CMD_ERASE_WR_BLK_END      = 33;

 {Application commands}
 SD_CMD_APP_SET_BUS_WIDTH      = 6;
 SD_CMD_APP_SD_STATUS         = 13;
 SD_CMD_APP_SEND_NUM_WR_BLKS  = 22;
 SD_CMD_APP_SEND_OP_COND      = 41;
 SD_CMD_APP_SEND_SCR          = 51;

 {SD_CMD_SWITCH argument format:
      [31] Check (0) or switch (1)
      [30:24] Reserved (0)
      [23:20] Function group 6
      [19:16] Function group 5
      [15:12] Function group 4
      [11:8] Function group 3
      [7:4] Function group 2
      [3:0] Function group 1}

 {SD Switch Mode Values}
 SD_SWITCH_MODE_CHECK    = 0;
 SD_SWITCH_MODE_SWITCH    = 1;

 {SD Switch Function Groups}
 SD_SWITCH_FUNCTION_GROUP_ACCESS    = 0; {Access Mode}
 SD_SWITCH_FUNCTION_GROUP_COMMAND    = 1; {Command System}
 SD_SWITCH_FUNCTION_GROUP_DRIVER    = 2; {Driver Strength}
 SD_SWITCH_FUNCTION_GROUP_POWER     = 3; {Power Limit}

 {SD Switch Access Modes}
 SD_SWITCH_ACCESS_MODE_DEF      = 0;  {Default SDR12}
 SD_SWITCH_ACCESS_MODE_HS      = 1;  {High Speed SDR25}
 SD_SWITCH_ACCESS_MODE_SDR50  = 2;  {SDR50 (1.8V only)}
 SD_SWITCH_ACCESS_MODE_SDR104 = 3;  {SDR104 (1.8V only)}
 SD_SWITCH_ACCESS_MODE_DDR50  = 4;  {DDR50 (1.8V only)}

 {SD Switch Command System}
 SD_SWITCH_COMMAND_SYSTEM_DEF      = 0;  {Default}
 SD_SWITCH_COMMAND_SYSTEM_EC      = 1;  {For eC}
 SD_SWITCH_COMMAND_SYSTEM_OTP      = 3;  {OTP}
 SD_SWITCH_COMMAND_SYSTEM_ASSD      = 4;  {ASSD}

 {SD Switch Driver Strength}
 SD_SWITCH_DRIVER_STRENGTH_DEF     = 0; {Default Type B}
 SD_SWITCH_DRIVER_STRENGTH_TYPE_A  = 1; {Type A}
 SD_SWITCH_DRIVER_STRENGTH_TYPE_C  = 2; {Type C}
 SD_SWITCH_DRIVER_STRENGTH_TYPE_D  = 3; {Type D}

 {SD Switch Power Limit}
 SD_SWITCH_POWER_LIMIT_DEF    = 0;  {Default 0.72W}
 SD_SWITCH_POWER_LIMIT_144    = 1;  {1.44W}
 SD_SWITCH_POWER_LIMIT_216    = 2;  {2.16W (Embedded only)}
 SD_SWITCH_POWER_LIMIT_288    = 3;  {2.88W (Embedded only)}
 SD_SWITCH_POWER_LIMIT_180    = 4;  {1.80W}

 {SD_CMD_SEND_IF_COND argument format:
      [31:12] Reserved (0)
      [11:8] Host Voltage Supply Flags
      [7:0] Check Pattern (0xAA)}

 {SD Send Interface Condition Values}
 SD_SEND_IF_COND_CHECK_PATTERN = $AA;
 SD_SEND_IF_COND_VOLTAGE_MASK  = $00FF8000;  {MMC_VDD_27_28, MMC_VDD_28_29, MMC_VDD_29_30, MMC_VDD_30_31, MMC_VDD_31_32, MMC_VDD_32_33, MMC_VDD_33_34, MMC_VDD_34_35, MMC_VDD_35_36}

 {SD Send Operation Condition Values}
 SD_SEND_OP_COND_VOLTAGE_MASK  = $00FF8000;  {MMC_VDD_27_28, MMC_VDD_28_29, MMC_VDD_29_30, MMC_VDD_30_31, MMC_VDD_31_32, MMC_VDD_32_33, MMC_VDD_33_34, MMC_VDD_34_35, MMC_VDD_35_36}

 {SD Operation Condition Register values} {See: Section 5.1 of SD Physical Layer Simplified Specification V4.10}
 SD_OCR_CCS           = $40000000; {Card Capacity Status - 0 = SDSC / 1 = SDHC or SDXC}
 SD_OCR_UHS_II     = $20000000; {UHS-II Card Status - 0 = Non UHS-II Card / 1 = UHS-II Card}
 SD_OCR_XPC           = $10000000;    {SDXC Power Control}
 SD_OCR_S18A       = $01000000; {1.8V Switching Accepted}

 {SD CSD Structure values}
 SD_CSD_STRUCT_VER_1_0  = 0;    {Standard Capacity}
 SD_CSD_STRUCT_VER_2_0  = 1;    {High Capacity and Extended Capacity}

 {SD Status Register (SSR) values} {See: Section 4.10.2 of SD Physical Layer Simplified Specification Version 4.10}
 {SD SSR Fields}
 SD_SSR_DAT_BUS_WIDTH          = 1;
 SD_SSR_SECURED_MODE           = 2;
 SD_SSR_SD_CARD_TYPE           = 3;
 SD_SSR_SIZE_OF_PROTECTED_AREA = 4;
 SD_SSR_SPEED_CLASS            = 5;
 SD_SSR_PERFORMANCE_MOVE       = 6;
 SD_SSR_AU_SIZE                = 7;
 SD_SSR_ERASE_SIZE             = 8;
 SD_SSR_ERASE_TIMEOUT          = 9;
 SD_SSR_ERASE_OFFSET           = 10;
 SD_SSR_UHS_SPEED_GRADE        = 11;
 SD_SSR_UHS_AU_SIZE            = 12;

 {SD SSR Bus Width values}
 SD_SSR_BUS_WIDTH_1           = 0;  {1 (default)}
 SD_SSR_BUS_WIDTH_4           = 2;  {4 bit width}

 {SD SSR Card Type values}
 SD_SSR_CARD_TYPE_RW  = $0000;  {Regular SD RD/WR Card}
 SD_SSR_CARD_TYPE_ROM = $0001;  {SD ROM Card}
 SD_SSR_CARD_TYPE_OTP = $0002;  {OTP}

 {SD SSR Speed Class values}
 SD_SSR_SPEED_CLASS_0    = $00;   {Class 0}
 SD_SSR_SPEED_CLASS_2    = $01;   {Class 2}
 SD_SSR_SPEED_CLASS_4    = $02;   {Class 4}
 SD_SSR_SPEED_CLASS_6    = $03;   {Class 6}
 SD_SSR_SPEED_CLASS_10   = $04;   {Class 10}

 {SD SSR AU Size values}
 SD_SSR_AU_SIZE_VALUES:array[0..15] of LongWord = (
  0,               {Not Defined}
  $00004000,       {16 KB}
  $00008000,       {32 KB}
  $00010000,       {64 KB}
  $00020000,       {128 KB}
  $00040000,       {256 KB}
  $00080000,       {512 KB}
  $00100000,       {1 MB}
  $00200000,       {2 MB}
  $00400000,       {4 MB}
  $00800000,       {8 MB}
  $00800000 + $00400000,      {12 MB}
  $01000000,       {16 MB}
  $01000000 + $00800000,      {24 MB}
  $02000000,       {32 MB}
  $04000000);      {64 MB}

 {SD SSR UHS Speed Grade values}
 SD_SSR_UHS_SPEED_GRADE_0  = 0;  {Less than 10MB/sec}
 SD_SSR_UHS_SPEED_GRADE_1  = 1;  {10MB/sec and above}

 {SD SSR UHS AU Size values}
 SD_SSR_UHS_AU_SIZE_VALUES:array[0..15] of LongWord = (
  0,               {Not Defined}
  0,               {Not Used}
  0,               {Not Used}
  0,               {Not Used}
  0,               {Not Used}
  0,               {Not Used}
  0,               {Not Used}
  $00100000,       {1 MB}
  $00200000,       {2 MB}
  $00400000,       {4 MB}
  $00800000,       {8 MB}
  $00800000 + $00400000,      {12 MB}
  $01000000,       {16 MB}
  $01000000 + $00800000,      {24 MB}
  $02000000,       {32 MB}
  $04000000);      {64 MB}

 {SD Switch Status values} {See: Section 4.3.10 of SD Physical Layer Simplified Specification Version 4.10}
 {SD Switch Fields}
 SD_SWITCH_MAXIMUM_CURRENT    = 1;
 SD_SWITCH_GROUP6_SUPPORT     = 2;
 SD_SWITCH_GROUP5_SUPPORT     = 3;
 SD_SWITCH_GROUP4_SUPPORT     = 4;
 SD_SWITCH_GROUP3_SUPPORT     = 5;
 SD_SWITCH_GROUP2_SUPPORT     = 6;
 SD_SWITCH_GROUP1_SUPPORT     = 7;
 SD_SWITCH_GROUP6_SELECTION   = 8;
 SD_SWITCH_GROUP5_SELECTION   = 9;
 SD_SWITCH_GROUP4_SELECTION   = 10;
 SD_SWITCH_GROUP3_SELECTION   = 11;
 SD_SWITCH_GROUP2_SELECTION   = 12;
 SD_SWITCH_GROUP1_SELECTION   = 13;
 SD_SWITCH_STRUCT_VERSION     = 14;
 SD_SWITCH_GROUP6_BUSY_STATUS = 15;
 SD_SWITCH_GROUP5_BUSY_STATUS = 16;
 SD_SWITCH_GROUP4_BUSY_STATUS = 17;
 SD_SWITCH_GROUP3_BUSY_STATUS = 18;
 SD_SWITCH_GROUP2_BUSY_STATUS = 19;
 SD_SWITCH_GROUP1_BUSY_STATUS = 20;

 {SD Switch Access Mode (Bus Speed) Support (Group 1)}
 SD_SWITCH_GROUP1_SDR12       = (1 shl 0);
 SD_SWITCH_GROUP1_HS          = (1 shl 1);
 SD_SWITCH_GROUP1_SDR25       = (1 shl 1);
 SD_SWITCH_GROUP1_SDR50       = (1 shl 2);
 SD_SWITCH_GROUP1_SDR104      = (1 shl 3);
 SD_SWITCH_GROUP1_DDR50       = (1 shl 4);

 {SD Switch Driver Strength Support (Group 3)}
 SD_SWITCH_GROUP3_TYPE_B      = (1 shl 0);
 SD_SWITCH_GROUP3_TYPE_A      = (1 shl 1);
 SD_SWITCH_GROUP3_TYPE_C      = (1 shl 2);
 SD_SWITCH_GROUP3_TYPE_D      = (1 shl 3);

 {SD Switch Structure Versions}
 SD_SWITCH_STRUCT_VER_0       = 0;  {Bits 511:376 are defined (SD_SWITCH_MAXIMUM_CURRENT to SD_SWITCH_GROUP1_SELECTION)}
 SD_SWITCH_STRUCT_VER_1       = 1;  {Bits 511:272 are defined (SD_SWITCH_MAXIMUM_CURRENT to SD_SWITCH_GROUP1_BUSY_STATUS}

 {SD Configuration Register (SCR) values} {See: Section 5.6 of SD Physical Layer Simplified Specification Version 4.10}
 {SD SCR Fields}
 SD_SCR_STRUCTURE             = 1;
 SD_SCR_SD_SPEC               = 2;
 SD_SCR_DATA_STAT_AFTER_ERASE = 3;
 SD_SCR_SD_SECURITY           = 4;
 SD_SCR_SD_BUS_WIDTHS         = 5;
 SD_SCR_SD_SPEC3              = 6;
 SD_SCR_EX_SECURITY           = 7;
 SD_SCR_SD_SPEC4              = 8;
 SD_SCR_CMD_SUPPORT           = 9;

 {SD SCR Structure values}
 SD_SCR_STRUCT_VER_1_0  = 0;    {Valid for system specification 1.01 - 4.0}

 {SD SCR Spec Version values}
 SD_SCR_SPEC_VER_0        = 0;    {Implements system specification 1.0 - 1.01}
 SD_SCR_SPEC_VER_1        = 1;    {Implements system specification 1.10}
 SD_SCR_SPEC_VER_2        = 2;    {Implements system specification 2.00-4.0X}

 {SD SCR Security values}
 SD_SCR_SECURITY_VER_0 = 0;     {No Security}
 SD_SCR_SECURITY_VER_2 = 2;     {SDSC Card (Security Version 1.01)}
 SD_SCR_SECURITY_VER_3 = 3;     {SDHC Card (Security Version 2.00)}
 SD_SCR_SECURITY_VER_4 = 4;     {SDXC Card (Security Version 3.xx)}

 {SD SCR Bus Width values}
 SD_SCR_BUS_WIDTH_1       = (1 shl 0);     {1 bit (DAT0)}
 SD_SCR_BUS_WIDTH_4       = (1 shl 2);     {4 bit (DAT0-3)}

 {SD SCR Extended Security values}
 SD_SCR_EX_SECURITY_VER_0 = 0;  {Extended Security is not supported}

 {SD SCR Command Support values}
 SD_SCR_CMD20_SUPPORT    = (1 shl 0);  {Mandatory for SDXC card}
 SD_SCR_CMD23_SUPPORT    = (1 shl 1);  {Mandatory for UHS104 card}
 SD_SCR_CMD48_49_SUPPORT = (1 shl 2);  {Optional}
 SD_SCR_CMD58_59_SUPPORT = (1 shl 3);  {Optional (If CMD58/59 is supported, CMD48/49 shall be supported)}

{==============================================================================}
const
 {SDIO specific constants}
 {SDIO Function States}
 SDIO_STATE_DETACHED  = 0;
 SDIO_STATE_DETACHING = 1;
 SDIO_STATE_ATTACHING = 2;
 SDIO_STATE_ATTACHED  = 3;

 SDIO_STATE_MAX       = 3;

 {SDIO Function State Names}
 SDIO_STATE_NAMES:array[SDIO_STATE_DETACHED..SDIO_STATE_MAX] of String = (
  'SDIO_STATE_DETACHED',
  'SDIO_STATE_DETACHING',
  'SDIO_STATE_ATTACHING',
  'SDIO_STATE_ATTACHED');

 {SDIO Function Status}
 SDIO_STATUS_UNBOUND   = 0;
 SDIO_STATUS_BOUND     = 1;

 SDIO_STATUS_MAX       = 1;

 {SDIO Function Status Names}
 SDIO_STATUS_NAMES:array[SDIO_STATUS_UNBOUND..SDIO_STATUS_MAX] of String = (
  'SDIO_STATUS_UNBOUND',
  'SDIO_STATUS_BOUND');

 {SDIO Commands (From: /include/linux/mmc/sdio.h)}
 SDIO_CMD_SEND_OP_COND       =   5;
 SDIO_CMD_RW_DIRECT          =  52;
 SDIO_CMD_RW_EXTENDED        =  53;

 {SDIO_CMD_RW_DIRECT argument format:
       [31] R/W flag
       [30:28] Function number
       [27] RAW flag
       [25:9] Register address
       [7:0] Data}

 {SDIO_CMD_RW_EXTENDED argument format:
       [31] R/W flag
       [30:28] Function number
       [27] Block mode
       [26] Increment address
       [25:9] Register address
       [8:0] Byte/block count}


 {SDIO Response Values (From: /include/linux/mmc/sdio.h)}
 {R4}
 SDIO_RSP_R4_18V_PRESENT    = (1 shl 24);
 SDIO_RSP_R4_MEMORY_PRESENT = (1 shl 27);

 {R5}
 SDIO_RSP_R5_COM_CRC_ERROR        = (1 shl 15);
 SDIO_RSP_R5_ILLEGAL_COMMAND    = (1 shl 14);
 SDIO_RSP_R5_ERROR                = (1 shl 11);
 SDIO_RSP_R5_FUNCTION_NUMBER    = (1 shl 9);
 SDIO_RSP_R5_OUT_OF_RANGE        = (1 shl 8);
 //SDIO_RSP_R5_STATUS(x)        (x & 0xCB00)
 //SDIO_RSP_R5_IO_CURRENT_STATE(x)    ((x & 0x3000) >> 12) /* s, b */

 {SDIO status in R5
  Type
    e : error bit
    s : status bit
    r : detected and set for the actual command response
    x : detected and set during command execution. the host must poll the card by sending status command in order to read these bits.
  Clear condition
    a : according to the card state
    b : always related to the previous command. Reception of a valid command will clear it (with a delay of one command)
    c : clear by read}

 {SDIO Card Common Control Registers (CCCR)}
 SDIO_CCCR_CCCR        = $00;
 SDIO_CCCR_SD        = $01;
 SDIO_CCCR_IOEx        = $02;
 SDIO_CCCR_IORx        = $03;
 SDIO_CCCR_IENx        = $04;    {Function/Master Interrupt Enable}
 SDIO_CCCR_INTx        = $05;    {Function Interrupt Pending}
 SDIO_CCCR_ABORT    = $06;    {Function abort/card reset}
 SDIO_CCCR_IF        = $07;    {Bus interface controls}
 SDIO_CCCR_CAPS        = $08;
 SDIO_CCCR_CIS        = $09;    {Common CIS pointer (3 bytes)}
 {Following 4 regs are valid only if SBS is set}
 SDIO_CCCR_SUSPEND    = $0c;
 SDIO_CCCR_SELx        = $0d;
 SDIO_CCCR_EXECx    = $0e;
 SDIO_CCCR_READYx    = $0f;
 SDIO_CCCR_BLKSIZE    = $10;
 SDIO_CCCR_POWER    = $12;
 SDIO_CCCR_SPEED    = $13;
 SDIO_CCCR_UHS        = $14;
 SDIO_CCCR_DRIVE_STRENGTH = $15;

 {SDIO CCCR CCCR Register values}
 SDIO_CCCR_REV_1_00    = 0;    {CCCR/FBR Version 1.00}
 SDIO_CCCR_REV_1_10    = 1;    {CCCR/FBR Version 1.10}
 SDIO_CCCR_REV_1_20    = 2;    {CCCR/FBR Version 1.20}
 SDIO_CCCR_REV_3_00    = 3;    {CCCR/FBR Version 3.00}

 SDIO_SDIO_REV_1_00    = 0;    {SDIO Spec Version 1.00}
 SDIO_SDIO_REV_1_10    = 1;    {SDIO Spec Version 1.10}
 SDIO_SDIO_REV_1_20    = 2;    {SDIO Spec Version 1.20}
 SDIO_SDIO_REV_2_00    = 3;    {SDIO Spec Version 2.00}
 SDIO_SDIO_REV_3_00    = 4;    {SDIO Spec Version 3.00}

 {SDIO CCCR SD Register values}
 SDIO_SD_REV_1_01    = 0;    {SD Physical Spec Version 1.01}
 SDIO_SD_REV_1_10    = 1;    {SD Physical Spec Version 1.10}
 SDIO_SD_REV_2_00    = 2;    {SD Physical Spec Version 2.00}
 SDIO_SD_REV_3_00    = 3;    {SD Physical Spev Version 3.00}

 {SDIO CCCR IF Register values}
 SDIO_BUS_WIDTH_MASK     = $03;    {data bus width setting}
 SDIO_BUS_WIDTH_1BIT     = $00;
 SDIO_BUS_WIDTH_RESERVED = $01;
 SDIO_BUS_WIDTH_4BIT     = $02;
 SDIO_BUS_ECSI             = $20;    {Enable continuous SPI interrupt}
 SDIO_BUS_SCSI             = $40;    {Support continuous SPI interrupt}

 SDIO_BUS_ASYNC_INT         = $20;

 SDIO_BUS_CD_DISABLE     = $80;    {disable pull-up on DAT3 (pin 1)}

 {SDIO CCCR CAPS Register values}
 SDIO_CCCR_CAP_SDC    = $01;    {Can do CMD52 while data transfer}
 SDIO_CCCR_CAP_SMB    = $02;    {Can do multi-block xfers (CMD53)}
 SDIO_CCCR_CAP_SRW    = $04;    {Supports read-wait protocol}
 SDIO_CCCR_CAP_SBS    = $08;    {Supports suspend/resume}
 SDIO_CCCR_CAP_S4MI    = $10;    {Interrupt during 4-bit CMD53}
 SDIO_CCCR_CAP_E4MI    = $20;    {Enable ints during 4-bit CMD53}
 SDIO_CCCR_CAP_LSC    = $40;    {Low speed card}
 SDIO_CCCR_CAP_4BLS    = $80;    {4 bit low speed card}

 {SDIO CCCR POWER Register values}
 SDIO_POWER_SMPC    = $01;    {Supports Master Power Control}
 SDIO_POWER_EMPC    = $02;    {Enable Master Power Control}

 {SDIO CCCR SPEED Register values}
 SDIO_SPEED_SHS            = $01;    {Supports High-Speed mode}
 SDIO_SPEED_BSS_SHIFT    = 1;
 SDIO_SPEED_BSS_MASK    = (7 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR12        = (0 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR25        = (1 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR50        = (2 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR104        = (3 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_DDR50        = (4 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_EHS            = SDIO_SPEED_SDR25;    {Enable High-Speed}

 {SDIO CCCR UHS Register values}
 SDIO_UHS_SDR50     = $01;
 SDIO_UHS_SDR104 = $02;
 SDIO_UHS_DDR50     = $04;

 {SDIO CCCR DRIVE STRENGTH Register values}
 SDIO_SDTx_MASK            = $07;
 SDIO_DRIVE_SDTA        = (1 shl 0);
 SDIO_DRIVE_SDTC        = (1 shl 1);
 SDIO_DRIVE_SDTD        = (1 shl 2);
 SDIO_DRIVE_DTSx_MASK    = $03;
 SDIO_DRIVE_DTSx_SHIFT    = 4;
 SDIO_DTSx_SET_TYPE_B    = (0 shl SDIO_DRIVE_DTSx_SHIFT);
 SDIO_DTSx_SET_TYPE_A    = (1 shl SDIO_DRIVE_DTSx_SHIFT);
 SDIO_DTSx_SET_TYPE_C    = (2 shl SDIO_DRIVE_DTSx_SHIFT);
 SDIO_DTSx_SET_TYPE_D    = (3 shl SDIO_DRIVE_DTSx_SHIFT);

 {SDIO Function Basic Registers (FBR)}
 {SDIO_FBR_BASE(f)    ((f) * $100)} {Base of function f's FBRs}
 SDIO_FBR_STD_IF        = $00;
 SDIO_FBR_STD_IF_EXT    = $01;
 SDIO_FBR_POWER            = $02;
 SDIO_FBR_CIS            = $09;    {CIS pointer (3 bytes)}
 SDIO_FBR_CSA            = $0C;    {CSA pointer (3 bytes)}
 SDIO_FBR_CSA_DATA        = $0F;
 SDIO_FBR_BLKSIZE        = $10;    {Block size (2 bytes)}

 {SDIO FBR IF Register values}
 SDIO_FBR_SUPPORTS_CSA    = $40;    {Supports Code Storage Area}
 SDIO_FBR_ENABLE_CSA    = $80;    {Enable Code Storage Area}

 {SDIO FBR POWER Register values}
 SDIO_FBR_POWER_SPS    = $01;    {Supports Power Selection}
 SDIO_FBR_POWER_EPS    = $02;    {Enable (low) Power Selection}

 {SDIO Function Classes}
 SDIO_CLASS_NONE   = $00; {Not a SDIO standard interface}
 SDIO_CLASS_UART   = $01; {Standard UART interface}
 SDIO_CLASS_BT_A   = $02; {Type-A BlueTooth std interface}
 SDIO_CLASS_BT_B   = $03; {Type-B BlueTooth std interface}
 SDIO_CLASS_GPS    = $04; {GPS standard interface}
 SDIO_CLASS_CAMERA = $05; {Camera standard interface}
 SDIO_CLASS_PHS    = $06; {PHS standard interface}
 SDIO_CLASS_WLAN   = $07; {WLAN interface}
 SDIO_CLASS_ATA    = $08; {Embedded SDIO-ATA std interface}
 SDIO_CLASS_BT_AMP = $09; {Type-A Bluetooth AMP interface}

 {SDIO Vendors}
 SDIO_VENDOR_ID_STE            = $0020;
 SDIO_VENDOR_ID_INTEL          = $0089;
 SDIO_VENDOR_ID_CGUYS          = $0092;
 SDIO_VENDOR_ID_TI             = $0097;
 SDIO_VENDOR_ID_ATHEROS        = $0271;
 SDIO_VENDOR_ID_BROADCOM       = $02d0;
 SDIO_VENDOR_ID_MARVELL        = $02df;
 SDIO_VENDOR_ID_MEDIATEK       = $037a;
 SDIO_VENDOR_ID_MICROCHIP_WILC = $0296;
 SDIO_VENDOR_ID_SIANO          = $039a;
 SDIO_VENDOR_ID_RSI            = $041b;
 SDIO_VENDOR_ID_TI_WL1251      = $104c;

 {SDIO Devices}
 SDIO_DEVICE_ID_STE_CW1200              = $2280;

 SDIO_DEVICE_ID_INTEL_IWMC3200WIMAX     = $1402;
 SDIO_DEVICE_ID_INTEL_IWMC3200WIFI      = $1403;
 SDIO_DEVICE_ID_INTEL_IWMC3200TOP       = $1404;
 SDIO_DEVICE_ID_INTEL_IWMC3200GPS       = $1405;
 SDIO_DEVICE_ID_INTEL_IWMC3200BT        = $1406;
 SDIO_DEVICE_ID_INTEL_IWMC3200WIMAX_2G5 = $1407;

 SDIO_DEVICE_ID_CGUYS_EW_CG1102GC       = $0004;

 SDIO_DEVICE_ID_TI_WL1271               = $4076;

 SDIO_DEVICE_ID_ATHEROS_AR6003_00       = $0300;
 SDIO_DEVICE_ID_ATHEROS_AR6003_01       = $0301;
 SDIO_DEVICE_ID_ATHEROS_AR6004_00       = $0400;
 SDIO_DEVICE_ID_ATHEROS_AR6004_01       = $0401;
 SDIO_DEVICE_ID_ATHEROS_AR6004_02       = $0402;
 SDIO_DEVICE_ID_ATHEROS_AR6004_18       = $0418;
 SDIO_DEVICE_ID_ATHEROS_AR6004_19       = $0419;
 SDIO_DEVICE_ID_ATHEROS_AR6005          = $050A;
 SDIO_DEVICE_ID_ATHEROS_QCA9377         = $0701;

 SDIO_DEVICE_ID_BROADCOM_NINTENDO_WII   = $044b;
 SDIO_DEVICE_ID_BROADCOM_43241          = $4324;
 SDIO_DEVICE_ID_BROADCOM_4329           = $4329;
 SDIO_DEVICE_ID_BROADCOM_4330           = $4330;
 SDIO_DEVICE_ID_BROADCOM_4334           = $4334;
 SDIO_DEVICE_ID_BROADCOM_4335_4339      = $4335;
 SDIO_DEVICE_ID_BROADCOM_4339           = $4339;
 SDIO_DEVICE_ID_BROADCOM_4345           = $4345;
 SDIO_DEVICE_ID_BROADCOM_4354           = $4354;
 SDIO_DEVICE_ID_BROADCOM_CYPRESS_89359  = $4355;
 SDIO_DEVICE_ID_BROADCOM_4356           = $4356;
 SDIO_DEVICE_ID_BROADCOM_4359           = $4359;
 SDIO_DEVICE_ID_BROADCOM_CYPRESS_4373   = $4373;
 SDIO_DEVICE_ID_BROADCOM_CYPRESS_43012  = $a804;
 SDIO_DEVICE_ID_BROADCOM_43143          = $a887;
 SDIO_DEVICE_ID_BROADCOM_43340          = $a94c;
 SDIO_DEVICE_ID_BROADCOM_43341          = $a94d;
 SDIO_DEVICE_ID_BROADCOM_43362          = $a962;
 SDIO_DEVICE_ID_BROADCOM_43364          = $a9a4;
 SDIO_DEVICE_ID_BROADCOM_43430          = $a9a6;
 SDIO_DEVICE_ID_BROADCOM_43455          = $a9bf;

 SDIO_DEVICE_ID_MARVELL_LIBERTAS        = $9103;
 SDIO_DEVICE_ID_MARVELL_8688_WLAN       = $9104;
 SDIO_DEVICE_ID_MARVELL_8688_BT         = $9105;
 SDIO_DEVICE_ID_MARVELL_8786_WLAN       = $9116;
 SDIO_DEVICE_ID_MARVELL_8787_WLAN       = $9119;
 SDIO_DEVICE_ID_MARVELL_8787_BT         = $911a;
 SDIO_DEVICE_ID_MARVELL_8787_BT_AMP     = $911b;
 SDIO_DEVICE_ID_MARVELL_8797_F0         = $9128;
 SDIO_DEVICE_ID_MARVELL_8797_WLAN       = $9129;
 SDIO_DEVICE_ID_MARVELL_8797_BT         = $912a;
 SDIO_DEVICE_ID_MARVELL_8897_WLAN       = $912d;
 SDIO_DEVICE_ID_MARVELL_8897_BT         = $912e;
 SDIO_DEVICE_ID_MARVELL_8887_F0         = $9134;
 SDIO_DEVICE_ID_MARVELL_8887_WLAN       = $9135;
 SDIO_DEVICE_ID_MARVELL_8887_BT         = $9136;
 SDIO_DEVICE_ID_MARVELL_8801_WLAN       = $9139;
 SDIO_DEVICE_ID_MARVELL_8997_F0         = $9140;
 SDIO_DEVICE_ID_MARVELL_8997_WLAN       = $9141;
 SDIO_DEVICE_ID_MARVELL_8997_BT         = $9142;
 SDIO_DEVICE_ID_MARVELL_8977_WLAN       = $9145;
 SDIO_DEVICE_ID_MARVELL_8977_BT         = $9146;
 SDIO_DEVICE_ID_MARVELL_8987_WLAN       = $9149;
 SDIO_DEVICE_ID_MARVELL_8987_BT         = $914a;

 SDIO_DEVICE_ID_MEDIATEK_MT7663         = $7663;
 SDIO_DEVICE_ID_MEDIATEK_MT7668         = $7668;

 SDIO_DEVICE_ID_MICROCHIP_WILC1000      = $5347;

 SDIO_DEVICE_ID_SIANO_NOVA_B0           = $0201;
 SDIO_DEVICE_ID_SIANO_NICE              = $0202;
 SDIO_DEVICE_ID_SIANO_VEGA_A0           = $0300;
 SDIO_DEVICE_ID_SIANO_VENICE            = $0301;
 SDIO_DEVICE_ID_SIANO_MING              = $0302;
 SDIO_DEVICE_ID_SIANO_PELE              = $0500;
 SDIO_DEVICE_ID_SIANO_RIO               = $0600;
 SDIO_DEVICE_ID_SIANO_DENVER_2160       = $0700;
 SDIO_DEVICE_ID_SIANO_DENVER_1530       = $0800;
 SDIO_DEVICE_ID_SIANO_NOVA_A0           = $1100;
 SDIO_DEVICE_ID_SIANO_STELLAR           = $5347;

 SDIO_DEVICE_ID_TI_WL1251               = $9066;

 {SDIO CIS Tuple Codes}
 CISTPL_NULL = $00;
 CISTPL_CHECKSUM = $10;
 CISTPL_VERS_1 = $15;
 CISTPL_ALTSTR = $16;
 CISTPL_MANFID = $20;
 CISTPL_FUNCID = $21;
 CISTPL_FUNCE = $22;
 CISTPL_SDIO_STD = $91;
 CISTPL_SDIO_EXT = $92;
 CISTPL_END = $FF;

 SDIO_MAX_FUNCTIONS = 7;

 SDIO_READ_CIS_TIMEOUT_MS = (10 * 1000); {10 seconds}

{==============================================================================}
const
 {SDHCI specific constants}
 SDHCI_NAME_PREFIX = 'SDHCI';     {Name prefix for SDHCI Devices}

 {SDHCI Host Types}
 SDHCI_TYPE_NONE      = 0;
 SDHCI_TYPE_MMC       = 1; {An MMC specification host controller}
 SDHCI_TYPE_SD        = 2; {An SD specification host controller}
 SDHCI_TYPE_MMCI      = 3; {An MMCI specification host controller}

 SDHCI_TYPE_MAX       = 3;

 {SDHCI Type Names}
 SDHCI_TYPE_NAMES:array[SDHCI_TYPE_NONE..SDHCI_TYPE_MAX] of String = (
  'SDHCI_TYPE_NONE',
  'SDHCI_TYPE_MMC',
  'SDHCI_TYPE_SD',
  'SDHCI_TYPE_MMCI');

 {SDHCI Host States}
 SDHCI_STATE_DISABLED = 0;
 SDHCI_STATE_ENABLED  = 1;

 SDHCI_STATE_MAX      = 1;

 {SDHCI State Names}
 SDHCI_STATE_NAMES:array[SDHCI_STATE_DISABLED..SDHCI_STATE_MAX] of String = (
  'SDHCI_STATE_DISABLED',
  'SDHCI_STATE_ENABLED');

 {SDHCI Host Flags}
 SDHCI_FLAG_NONE          = $00000000;
 SDHCI_FLAG_SDMA          = $00000001; {Host Controller supports SDMA specification}
 SDHCI_FLAG_ADMA          = $00000002; {Host Controller supports ADMA specification}
 SDHCI_FLAG_SPI           = $00000004; {Host Controller uses SPI interface}
 SDHCI_FLAG_CRC_ENABLE    = $00000008;
 SDHCI_FLAG_NON_STANDARD  = $00000010; {Host Controller uses a non standard interface (not supporting SDHCI register layout)}
 SDHCI_FLAG_AUTO_CMD12    = $00000020; {Host Controller supports Auto CMD12 (Stop Transmission)}
 SDHCI_FLAG_AUTO_CMD23    = $00000040; {Host Controller supports Auto CMD23 (Set Block Count)}
 SDHCI_FLAG_64_BIT_DMA    = $00000080; {Host Controller supports 64-bit ADMA}
 SDHCI_FLAG_EXTERNAL_DMA  = $00000100; {Host Controller requires external DMA engine to perform transfers}
 SDHCI_FLAG_BUS_ADDRESSES = $00000200; {Host Controller requires use of bus addresses for SDMA/ADMA transfers}

 {SDHCI Controller Registers}
 SDHCI_DMA_ADDRESS         = $00;
 SDHCI_ARGUMENT2         = SDHCI_DMA_ADDRESS;
 SDHCI_32BIT_BLK_CNT     = SDHCI_DMA_ADDRESS;
 SDHCI_BLOCK_SIZE         = $04;
 SDHCI_BLOCK_COUNT         = $06;
 SDHCI_ARGUMENT             = $08;
 SDHCI_TRANSFER_MODE     = $0C;
 SDHCI_COMMAND             = $0E;
 SDHCI_RESPONSE             = $10;
 SDHCI_BUFFER             = $20;
 SDHCI_PRESENT_STATE     = $24;
 SDHCI_HOST_CONTROL         = $28;
 SDHCI_POWER_CONTROL     = $29;
 SDHCI_BLOCK_GAP_CONTROL = $2A;
 SDHCI_WAKE_UP_CONTROL     = $2B;
 SDHCI_CLOCK_CONTROL     = $2C;
 SDHCI_TIMEOUT_CONTROL     = $2E;
 SDHCI_SOFTWARE_RESET     = $2F;
 SDHCI_INT_STATUS         = $30;
 SDHCI_INT_ENABLE         = $34;
 SDHCI_SIGNAL_ENABLE     = $38;
 SDHCI_AUTO_CMD_STATUS   = $3C; {SDHCI_ACMD12_ERR}
 SDHCI_HOST_CONTROL2     = $3E;
 SDHCI_CAPABILITIES      = $40;
 SDHCI_CAPABILITIES_1     = $44;
 SDHCI_MAX_CURRENT         = $48;
 {4C-4F reserved for more max current}
 SDHCI_SET_ACMD12_ERROR     = $50;
 SDHCI_SET_INT_ERROR     = $52;
 SDHCI_ADMA_ERROR         = $54;
 {55-57 reserved}
 SDHCI_ADMA_ADDRESS         = $58;
 SDHCI_ADMA_ADDRESS_HI   = $5C;
 {5D-65 reserved}
 SDHCI_PRESET_FOR_SDR12  = $66;
 SDHCI_PRESET_FOR_SDR25  = $68;
 SDHCI_PRESET_FOR_SDR50  = $6A;
 SDHCI_PRESET_FOR_SDR104 = $6C;
 SDHCI_PRESET_FOR_DDR50  = $6E;
 {6F-73 reserved}
 SDHCI_PRESET_FOR_HS400  = $74; {Non-standard}
 {75-FB reserved}
 SDHCI_SLOT_INT_STATUS     = $FC;
 SDHCI_HOST_VERSION         = $FE;

 {SDHCI Transfer Modes}
 SDHCI_TRNS_DMA            = $01;
 SDHCI_TRNS_BLK_CNT_EN    = $02;
 SDHCI_TRNS_AUTO_CMD12    = $04;  {SDHCI_TRNS_ACMD12}
 SDHCI_TRNS_AUTO_CMD23    = $08;
 SDHCI_TRNS_AUTO_SEL    = SDHCI_TRNS_AUTO_CMD12 or SDHCI_TRNS_AUTO_CMD23;
 SDHCI_TRNS_READ        = $10;
 SDHCI_TRNS_MULTI        = $20;

 {SDHCI Command Values}
 SDHCI_CMD_RESP_MASK = $03;
 SDHCI_CMD_CRC         = $08;
 SDHCI_CMD_INDEX     = $10;
 SDHCI_CMD_DATA         = $20;
 SDHCI_CMD_ABORTCMD     = $C0;

 {SDHCI Command Response Values}
 SDHCI_CMD_RESP_NONE       = $00;
 SDHCI_CMD_RESP_LONG       = $01;
 SDHCI_CMD_RESP_SHORT       = $02;
 SDHCI_CMD_RESP_SHORT_BUSY = $03;

 {SDHCI Present State Values}
 SDHCI_CMD_INHIBIT             = $00000001;
 SDHCI_DATA_INHIBIT             = $00000002;
 SDHCI_DOING_WRITE             = $00000100;
 SDHCI_DOING_READ             = $00000200;
 SDHCI_SPACE_AVAILABLE         = $00000400;
 SDHCI_DATA_AVAILABLE         = $00000800;
 SDHCI_CARD_PRESENT             = $00010000;
 SDHCI_CARD_STATE_STABLE     = $00020000; {SDHCI_CD_STABLE}
 SDHCI_CARD_DETECT_PIN_LEVEL = $00040000; {SDHCI_CD_LVL}
 SDHCI_WRITE_PROTECT         = $00080000; {Set if Write Enabled / Clear if Write Protected}
 SDHCI_DATA_LEVEL_MASK       = $00F00000; {SDHCI_DATA_LVL_MASK}
 SDHCI_DATA_0_LEVEL_MASK     = $00100000; {SDHCI_DATA_0_LVL_MASK}
 SDHCI_CMD_LEVEL             = $01000000; {SDHCI_CMD_LVL}

 {SDHCI Host Control Values}
 SDHCI_CTRL_LED            = $01;
 SDHCI_CTRL_4BITBUS        = $02;
 SDHCI_CTRL_HISPD        = $04;
 SDHCI_CTRL_DMA_MASK    = $18;
 SDHCI_CTRL_SDMA        = $00;
 SDHCI_CTRL_ADMA1        = $08;
 SDHCI_CTRL_ADMA32        = $10;
 SDHCI_CTRL_ADMA64        = $18;
 SDHCI_CTRL_ADMA3         = $18;
 SDHCI_CTRL_8BITBUS     = $20;
 SDHCI_CTRL_CD_TEST_INS = $40;
 SDHCI_CTRL_CD_TEST        = $80;

 {SDHCI Power Control Values}
 SDHCI_POWER_ON        = $01;
 SDHCI_POWER_180    = $0A;
 SDHCI_POWER_300    = $0C;
 SDHCI_POWER_330    = $0E;

 {SDHCI Wakeup Control Values}
 SDHCI_WAKE_ON_INT      = $01;
 SDHCI_WAKE_ON_INSERT = $02;
 SDHCI_WAKE_ON_REMOVE = $04;

 {SDHCI Clock Control Values}
 SDHCI_DIVIDER_SHIFT    = 8;
 SDHCI_DIVIDER_HI_SHIFT    = 6;
 SDHCI_DIV_MASK            = $FF;
 SDHCI_DIV_MASK_LEN        = 8;
 SDHCI_DIV_HI_MASK        = $0300;
 SDHCI_PROG_CLOCK_MODE  = $0020;
 SDHCI_CLOCK_CARD_EN    = $0004;
 SDHCI_CLOCK_PLL_EN     = $0008;
 SDHCI_CLOCK_INT_STABLE = $0002;
 SDHCI_CLOCK_INT_EN        = $0001;

 {SDHCI Software Reset Values}
 SDHCI_RESET_ALL    = $01;
 SDHCI_RESET_CMD    = $02;
 SDHCI_RESET_DATA    = $04;

 {SDHCI Interrupt Values}
 SDHCI_INT_RESPONSE        = $00000001;
 SDHCI_INT_DATA_END        = $00000002;
 SDHCI_INT_BLK_GAP      = $00000004;
 SDHCI_INT_DMA_END        = $00000008;
 SDHCI_INT_SPACE_AVAIL    = $00000010;
 SDHCI_INT_DATA_AVAIL    = $00000020;
 SDHCI_INT_CARD_INSERT    = $00000040;
 SDHCI_INT_CARD_REMOVE    = $00000080;
 SDHCI_INT_CARD_INT        = $00000100;
 SDHCI_INT_RETUNE       = $00001000;
 SDHCI_INT_CQE          = $00004000;
 SDHCI_INT_ERROR        = $00008000;
 SDHCI_INT_TIMEOUT        = $00010000;
 SDHCI_INT_CRC            = $00020000;
 SDHCI_INT_END_BIT        = $00040000;
 SDHCI_INT_INDEX        = $00080000;
 SDHCI_INT_DATA_TIMEOUT = $00100000;
 SDHCI_INT_DATA_CRC        = $00200000;
 SDHCI_INT_DATA_END_BIT = $00400000;
 SDHCI_INT_BUS_POWER    = $00800000;
 SDHCI_INT_AUTO_CMD_ERR    = $01000000; {SDHCI_INT_ACMD12ERR}
 SDHCI_INT_ADMA_ERROR    = $02000000;

 SDHCI_INT_NORMAL_MASK    = $00007FFF;
 SDHCI_INT_ERROR_MASK    = $FFFF8000;

 SDHCI_INT_CMD_MASK        = (SDHCI_INT_RESPONSE or SDHCI_INT_TIMEOUT or SDHCI_INT_CRC or SDHCI_INT_END_BIT or SDHCI_INT_INDEX or SDHCI_INT_AUTO_CMD_ERR);
 SDHCI_INT_DATA_MASK    = (SDHCI_INT_DATA_END or SDHCI_INT_DMA_END or SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_END_BIT or SDHCI_INT_ADMA_ERROR or SDHCI_INT_BLK_GAP);
 SDHCI_INT_ALL_MASK        = (LongWord(-1));

 SDHCI_CQE_INT_ERR_MASK = (SDHCI_INT_ADMA_ERROR or SDHCI_INT_BUS_POWER or SDHCI_INT_DATA_END_BIT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_INDEX or SDHCI_INT_END_BIT or SDHCI_INT_CRC or SDHCI_INT_TIMEOUT);
 SDHCI_CQE_INT_MASK     = (SDHCI_CQE_INT_ERR_MASK or SDHCI_INT_CQE);

 {SDHCI Auto CMD Status Values}
 SDHCI_AUTO_CMD_TIMEOUT = $00000002;
 SDHCI_AUTO_CMD_CRC     = $00000004;
 SDHCI_AUTO_CMD_END_BIT = $00000008;
 SDHCI_AUTO_CMD_INDEX   = $00000010;

 {SDHCI Host Control 2 Values}
 SDHCI_CTRL_UHS_MASK          = $0007;
 SDHCI_CTRL_UHS_SDR12         = $0000;
 SDHCI_CTRL_UHS_SDR25         = $0001;
 SDHCI_CTRL_UHS_SDR50         = $0002;
 SDHCI_CTRL_UHS_SDR104        = $0003;
 SDHCI_CTRL_UHS_DDR50         = $0004;
 SDHCI_CTRL_HS400             = $0005; {Non-standard}
 SDHCI_CTRL_VDD_180           = $0008;
 SDHCI_CTRL_DRV_TYPE_MASK     = $0030;
 SDHCI_CTRL_DRV_TYPE_B        = $0000;
 SDHCI_CTRL_DRV_TYPE_A        = $0010;
 SDHCI_CTRL_DRV_TYPE_C        = $0020;
 SDHCI_CTRL_DRV_TYPE_D        = $0030;
 SDHCI_CTRL_EXEC_TUNING       = $0040;
 SDHCI_CTRL_TUNED_CLK         = $0080;
 SDHCI_CMD23_ENABLE           = $0800;
 SDHCI_CTRL_V4_MODE           = $1000;
 SDHCI_CTRL_64BIT_ADDR        = $2000;
 SDHCI_CTRL_PRESET_VAL_ENABLE = $8000;

 {SDHCI Capabilities Values}
 SDHCI_TIMEOUT_CLK_MASK         = $0000003F;
 SDHCI_TIMEOUT_CLK_SHIFT     = 0;
 SDHCI_TIMEOUT_CLK_UNIT         = $00000080;
 SDHCI_CLOCK_BASE_MASK         = $00003F00;
 SDHCI_CLOCK_V3_BASE_MASK    = $0000FF00;
 SDHCI_CLOCK_BASE_SHIFT         = 8;
 SDHCI_CLOCK_BASE_MULTIPLIER = 1000000;
 SDHCI_MAX_BLOCK_MASK         = $00030000;
 SDHCI_MAX_BLOCK_SHIFT       = 16;
 SDHCI_CAN_DO_8BIT             = $00040000;
 SDHCI_CAN_DO_ADMA2             = $00080000;
 SDHCI_CAN_DO_ADMA1             = $00100000;
 SDHCI_CAN_DO_HISPD             = $00200000;
 SDHCI_CAN_DO_SDMA             = $00400000;
 SDHCI_CAN_VDD_330             = $01000000;
 SDHCI_CAN_VDD_300             = $02000000;
 SDHCI_CAN_VDD_180             = $04000000;
 SDHCI_CAN_64BIT_V4          = $08000000;
 SDHCI_CAN_64BIT             = $10000000;

 {SDHCI Capabilities 1 Values}
 SDHCI_SUPPORT_SDR50             = $00000001;
 SDHCI_SUPPORT_SDR104            = $00000002;
 SDHCI_SUPPORT_DDR50             = $00000004;
 SDHCI_DRIVER_TYPE_A             = $00000010;
 SDHCI_DRIVER_TYPE_C             = $00000020;
 SDHCI_DRIVER_TYPE_D             = $00000040;
 SDHCI_RETUNING_TIMER_COUNT_MASK = $00000F00; {GENMASK(11,8)}
 SDHCI_USE_SDR50_TUNING          = $00002000;
 SDHCI_RETUNING_MODE_MASK        = $0000C000; {GENMASK(15,14)}
 SDHCI_CLOCK_MUL_MASK            = $00FF0000; {GENMASK(23,16)}
 SDHCI_CAN_DO_ADMA3              = $08000000;
 SDHCI_SUPPORT_HS400             = $80000000; {Non-standard}

 {SDHCI Max Current Values}
 SDHCI_MAX_CURRENT_LIMIT      = $000000FF; {GENMASK(7,0)}
 SDHCI_MAX_CURRENT_330_MASK   = $000000FF; {GENMASK(7,0)}
 SDHCI_MAX_CURRENT_300_MASK   = $0000FF00; {GENMASK(15,8)}
 SDHCI_MAX_CURRENT_180_MASK   = $00FF0000; {GENMASK(23,16)}
 SDHCI_MAX_CURRENT_MULTIPLIER = 4;

 {SDHCI Preset Values}
 SDHCI_PRESET_DRV_MASK        = $0000C000; {GENMASK(15,14)}
 SDHCI_PRESET_DRV_SHIFT       = 14;
 SDHCI_PRESET_CLKGEN_SEL      = 1 shl 10; {BIT(10)}
 SDHCI_PRESET_SDCLK_FREQ_MASK = $000003FF; {GENMASK(9,0)}

 {SDHCI Host Version Values}
 SDHCI_VENDOR_VER_MASK    = $FF00;
 SDHCI_VENDOR_VER_SHIFT    = 8;
 SDHCI_SPEC_VER_MASK    = $00FF;
 SDHCI_SPEC_VER_SHIFT    = 0;
 SDHCI_SPEC_100            = 0;
 SDHCI_SPEC_200            = 1;
 SDHCI_SPEC_300            = 2;
 SDHCI_SPEC_400            = 3;
 SDHCI_SPEC_410            = 4;
 SDHCI_SPEC_420            = 5;

 {SDHCI Clock Dividers}
 SDHCI_MAX_CLOCK_DIV_SPEC_200     = 256;
 SDHCI_MAX_CLOCK_DIV_SPEC_300     = 2046;

 {SDHCI Quirks/Bugs}
 {From Linux /include/linux/mmc/sdhci.h}
 SDHCI_QUIRK_CLOCK_BEFORE_RESET            = (1 shl 0); {Controller doesn't honor resets unless we touch the clock register}
 SDHCI_QUIRK_FORCE_DMA                    = (1 shl 1); {Controller has bad caps bits, but really supports DMA}
 SDHCI_QUIRK_NO_CARD_NO_RESET            = (1 shl 2); {Controller doesn't like to be reset when there is no card inserted}
 SDHCI_QUIRK_SINGLE_POWER_WRITE            = (1 shl 3); {Controller doesn't like clearing the power reg before a change}
 SDHCI_QUIRK_RESET_CMD_DATA_ON_IOS        = (1 shl 4); {Controller has flaky internal state so reset it on each ios change}
 SDHCI_QUIRK_BROKEN_DMA                    = (1 shl 5); {Controller has an unusable DMA engine}
 SDHCI_QUIRK_BROKEN_ADMA                = (1 shl 6); {Controller has an unusable ADMA engine}
 SDHCI_QUIRK_32BIT_DMA_ADDR                = (1 shl 7); {Controller can only DMA from 32-bit aligned addresses}
 SDHCI_QUIRK_32BIT_DMA_SIZE                = (1 shl 8); {Controller can only DMA chunk sizes that are a multiple of 32 bits}
 SDHCI_QUIRK_32BIT_ADMA_SIZE            = (1 shl 9); {Controller can only ADMA chunks that are a multiple of 32 bits}
 SDHCI_QUIRK_RESET_AFTER_REQUEST        = (1 shl 10); {Controller needs to be reset after each request to stay stable}
 SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER    = (1 shl 11); {Controller needs voltage and power writes to happen separately}
 SDHCI_QUIRK_BROKEN_TIMEOUT_VAL            = (1 shl 12); {Controller provides an incorrect timeout value for transfers}
 SDHCI_QUIRK_BROKEN_SMALL_PIO            = (1 shl 13); {Controller has an issue with buffer bits for small transfers}
 SDHCI_QUIRK_NO_BUSY_IRQ                = (1 shl 14); {Controller does not provide transfer-complete interrupt when not busy}
 SDHCI_QUIRK_BROKEN_CARD_DETECTION        = (1 shl 15); {Controller has unreliable card detection}
 SDHCI_QUIRK_INVERTED_WRITE_PROTECT        = (1 shl 16); {Controller reports inverted write-protect state}
 SDHCI_QUIRK_PIO_NEEDS_DELAY            = (1 shl 18); {Controller does not like fast PIO transfers}
 SDHCI_QUIRK_FORCE_BLK_SZ_2048            = (1 shl 20); {Controller has to be forced to use block size of 2048 bytes}
 SDHCI_QUIRK_NO_MULTIBLOCK                = (1 shl 21); {Controller cannot do multi-block transfers}
 SDHCI_QUIRK_FORCE_1_BIT_DATA            = (1 shl 22); {Controller can only handle 1-bit data transfers}
 SDHCI_QUIRK_DELAY_AFTER_POWER            = (1 shl 23); {Controller needs 10ms delay between applying power and clock}
 SDHCI_QUIRK_DATA_TIMEOUT_USES_SDCLK    = (1 shl 24); {Controller uses SDCLK instead of TMCLK for data timeouts}
 SDHCI_QUIRK_CAP_CLOCK_BASE_BROKEN        = (1 shl 25); {Controller reports wrong base clock capability}
 SDHCI_QUIRK_NO_ENDATTR_IN_NOPDESC        = (1 shl 26); {Controller cannot support End Attribute in NOP ADMA descriptor}
 SDHCI_QUIRK_MISSING_CAPS                = (1 shl 27); {Controller is missing device caps. Use caps provided by host}
 SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12        = (1 shl 28); {Controller uses Auto CMD12 command to stop the transfer}
 SDHCI_QUIRK_NO_HISPD_BIT                = (1 shl 29); {Controller doesn't have HISPD bit field in HI-SPEED SD card}
 SDHCI_QUIRK_BROKEN_ADMA_ZEROLEN_DESC    = (1 shl 30); {Controller treats ADMA descriptors with length 0000h incorrectly}
 SDHCI_QUIRK_UNSTABLE_RO_DETECT            = (1 shl 31); {The read-only detection via SDHCI_PRESENT_STATE register is unstable}

 {SDHCI More Quirks/Bugs}
 {From Linux /include/linux/mmc/sdhci.h}
 SDHCI_QUIRK2_HOST_OFF_CARD_ON                  = (1 shl 0);
 SDHCI_QUIRK2_HOST_NO_CMD23                     = (1 shl 1);
 SDHCI_QUIRK2_NO_1_8_V                          = (1 shl 2); {The system physically doesn't support 1.8v, even if the host does}
 SDHCI_QUIRK2_PRESET_VALUE_BROKEN               = (1 shl 3);
 SDHCI_QUIRK2_CARD_ON_NEEDS_BUS_ON              = (1 shl 4);
 SDHCI_QUIRK2_BROKEN_HOST_CONTROL               = (1 shl 5); {Controller has a non-standard host control register}
 SDHCI_QUIRK2_BROKEN_HS200                      = (1 shl 6); {Controller does not support HS200}
 SDHCI_QUIRK2_BROKEN_DDR50                      = (1 shl 7); {Controller does not support DDR50}
 SDHCI_QUIRK2_STOP_WITH_TC                      = (1 shl 8); {Stop command (CMD12) can set Transfer Complete when not using MMC_RSP_BUSY}
 SDHCI_QUIRK2_BROKEN_64_BIT_DMA                 = (1 shl 9); {Controller does not support 64-bit DMA}
 SDHCI_QUIRK2_CLEAR_TRANSFERMODE_REG_BEFORE_CMD = (1 shl 10); {Need clear transfer mode register before send cmd}
 SDHCI_QUIRK2_CAPS_BIT63_FOR_HS400              = (1 shl 11); {Capability register bit-63 indicates HS400 support}
 SDHCI_QUIRK2_TUNING_WORK_AROUND                = (1 shl 12); {Forced tuned clock}
 SDHCI_QUIRK2_SUPPORT_SINGLE                    = (1 shl 13); {Disable the block count for single block transactions}
 SDHCI_QUIRK2_ACMD23_BROKEN                     = (1 shl 14); {Controller broken with using ACMD23}
 SDHCI_QUIRK2_CLOCK_DIV_ZERO_BROKEN             = (1 shl 15); {Broken Clock divider zero in controller}
 SDHCI_QUIRK2_RSP_136_HAS_CRC                   = (1 shl 16); {Controller has CRC in 136 bit Command Response}
 SDHCI_QUIRK2_DISABLE_HW_TIMEOUT                = (1 shl 17); {Disable HW timeout if the requested timeout is more than the maximum obtainable timeout}
 SDHCI_QUIRK2_USE_32BIT_BLK_CNT                 = (1 shl 18); {32-bit block count may not support eMMC where upper bits of CMD23 are used for other purposes}
                                                              {Support 16-bit block count by default otherwise, SDHCI_QUIRK2_USE_32BIT_BLK_CNT can be selected to use 32-bit block count}

 {SDHCI Host SDMA buffer boundary (Valid values from 4K to 512K in powers of 2)}
 SDHCI_DEFAULT_BOUNDARY_SIZE  = (512 * 1024); {Default to 512K boundary}
 SDHCI_DEFAULT_BOUNDARY_ARG   = 7;            {(ilog2(SDHCI_DEFAULT_BOUNDARY_SIZE) - 12)}

 {ADMA2 32-bit DMA descriptor size}
 SDHCI_ADMA2_32_DESC_SIZE = 8;

 {ADMA2 data alignment}
 SDHCI_ADMA2_ALIGN = 4;
 SDHCI_ADMA2_MASK  = (SDHCI_ADMA2_ALIGN - 1);

 {ADMA2 descriptor alignment}
 SDHCI_ADMA2_DESC_ALIGN = 8;

 {ADMA2 64-bit DMA descriptor size}
 SDHCI_ADMA2_64_DESC_SIZE    = 12;
 SDHCI_ADMA2_64_DESC_V4_SIZE = 16; {Use 128-bit descriptor, if Host Version 4 Enable is set in the Host Control 2 register}

 {ADMA2 descriptor attributes}
 SDHCI_ADMA2_DESC_ATTR_VALID = $01;
 SDHCI_ADMA2_DESC_ATTR_END   = $02;
 SDHCI_ADMA2_DESC_ATTR_INT   = $04;
 SDHCI_ADMA2_DESC_ATTR_NOP   = $00;
 SDHCI_ADMA2_DESC_ATTR_TRAN  = $20;
 SDHCI_ADMA2_DESC_ATTR_LINK  = $30;

 SDHCI_ADMA2_DESC_TRAN_VALID    = SDHCI_ADMA2_DESC_ATTR_TRAN or SDHCI_ADMA2_DESC_ATTR_VALID; {0x21}
 SDHCI_ADMA2_DESC_NOP_END_VALID    = SDHCI_ADMA2_DESC_ATTR_NOP or SDHCI_ADMA2_DESC_ATTR_END or SDHCI_ADMA2_DESC_ATTR_VALID; {0x3}
 SDHCI_ADMA2_DESC_END            = SDHCI_ADMA2_DESC_ATTR_END; {0x2}

 {SDHCI maximum segments (assuming a 512KB maximum request size and a minimum 4KB page size)}
 SDHCI_MAX_SEGS = 128;

 {SDHCI Timeout Value}
 SDHCI_TIMEOUT_VALUE  = $0E;

{==============================================================================}
type
 {MMC specific types}

 {MMC Command}
 PMMCData = ^TMMCData;
 PMMCCommand = ^TMMCCommand;
 TMMCCommand = record
  {Command Properties}
  Command:Word;
  Argument:LongWord;
  ResponseType:LongWord;
  Response:array[0..3] of LongWord;
  Status:LongWord;
  Data:PMMCData;
  Timeout:LongWord; {Milliseconds}
  {Host Properties}
  DataCompleted:Boolean;
  BusyCompleted:Boolean;
  TuningCompleted:Boolean;
  CommandCompleted:Boolean;
 end;

 {MMC Data}
 TMMCData = record
  {Data Properties}
  Data:Pointer;
  Flags:LongWord;
  BlockSize:LongWord;
  BlockCount:LongWord;
  {Host Properties}
  BlockOffset:LongWord;
  BlocksRemaining:LongWord;
  BytesRemaining:LongWord;
  BytesTransfered:LongWord;
 end;

 {MMC Card Identification Data (CID)} {See: Section 5.2 of SD Physical Layer Simplified Specification Version 4.10}
 PMMCCardIdentificationData = ^TMMCCardIdentificationData;
 TMMCCardIdentificationData = record
  {Card Values}
  ManufacturerId:Byte;
  OEMId:Word;
  ProductName:array[0..7] of Char; {Max 0 to 6, 1 extra for null terminator}
  ProductRevision:Byte;
  HardwareRevision:Byte;
  FirmwareRevision:Byte;
  ProductSerialNumber:LongWord;
  ManufacturingMonth:Byte;
  ManufacturingYear:Word;
  CRC:Byte;                        {CRC7 checksum}
 end;

 {MMC Card Specific Data (CSD)} {See: Section 5.3 of SD Physical Layer Simplified Specification Version 4.10}
 TMMCCardSpecificSDEraseData = record {Defined here for CSD}
  EraseBlockEnable:Boolean;   {Erase single block enable}
  SectorSize:Byte;            {Erase sector size}
 end;

 TMMCCardSpecificMMC22EraseData = record
  SectorSize:Byte;            {Erase sector size}
  EraseGroupSize:Byte;        {Erase group size}
 end;

 TMMCCardSpecificMMC31EraseData = record
  EraseGroupSize:Byte;        {Erase group size}
  EraseGroupMultiplier:Byte;  {Erase group size multiplier}
 end;

 TMMCCardSpecificEraseData = record
  case Integer of
   0:(MMC22:TMMCCardSpecificMMC22EraseData);
   1:(MMC31:TMMCCardSpecificMMC31EraseData);
   2:(SD:TMMCCardSpecificSDEraseData);
 end;

 PMMCCardSpecificData = ^TMMCCardSpecificData;
 TMMCCardSpecificData = record
  {Card Values}
  CSDStructure:Byte;                {CSD structure version}
  SpecVersion:Byte;                 {System specification version} {MMC/eMMC Only}
  {TAAC:Byte;}                      {Data read access-time-1}
  {NSAC:Byte;}                      {Data read access-time-2 in CLK cycles (NSAC*100)}
  {TransferSpeed:Byte;}             {Max. data transfer rate}
  CCC:Word;                         {Card command classes}
  ReadBlockLength:Byte;             {Max. read data block length}
  ReadBlockPartial:Boolean;         {Partial blocks for read allowed}
  WriteBlockMisalign:Boolean;       {Write block misalignment}
  ReadBlockMisalign:Boolean;        {Read block misalignment}
  DSRImplemented:Boolean;           {DSR implemented}
  DeviceSize:LongWord;              {Device size}
  VDDReadCurrentMin:Byte;           {Max. read current @VDD min}
  VDDReadCurrentMax:Byte;           {Max. read current @VDD max}
  VDDWriteCurrentMin:Byte;          {Max. write current @VDD min}
  VDDWriteCurrentMax:Byte;          {Max. write current @VDD max}
  DeviceSizeMultiplier:Byte;        {Device size multiplier}
  Erase:TMMCCardSpecificEraseData;  {Erase group details}
  WriteProtectGroupSize:Byte;       {Write protect group size}
  WriteProtectGroupEnable:Boolean;  {Write protect group enable}
  DefaultECC:Byte;                  {Manufacturer default ECC} {MMC/eMMC Only}
  ReadToWriteFactor:Byte;           {Write speed factor}
  WriteBlockLength:Byte;            {Max. write data block length}
  WriteBlockPartial:Boolean;        {Partial blocks for write allowed}
  ContentProtectApplication:Boolean;{Content protection application} {MMC/eMMC Only}
  FileFormatGroup:Byte;             {File format group}
  CopyFlag:Boolean;                 {Copy flag}
  PermanentWriteProtect:Boolean;    {Permanent write protection}
  TemporaryWriteProtect:Boolean;    {Temporary write protection}
  FileFormat:Byte;                  {File format}
  ECC:Byte;                         {ECC code} {MMC/eMMC Only}
  CRC:Byte;                         {CRC}
  {Calculated Values}
  DataAccessTime:LongWord;          {In Nanoseconds}
  DataAccessClocks:Word;            {In Clock cycles}
  DataTransferRate:LongWord;        {In Hz}
  EraseSize:LongWord;               {In Sectors}
  BlockSize:LongWord;               {"Normalized" Block Size}
  BlockCount:LongWord;              {In "Normalized" Blocks}
  BlockShift:LongWord;              {"Normalized" Block Shift}
 end;

 PMMCExtendedCardSpecificData = ^TMMCExtendedCardSpecificData;
 TMMCExtendedCardSpecificData = record
  {Card Values}
  Revision:Byte; {Extended CSD revision (192)}

  CacheControl:Byte; {Control to turn the Cache ON/OFF (33)}
  PowerOffNotification:Byte; {Power Off Notification (34)}

  PartitionSupport:Byte; {Partitioning Support (160)}
  HardwareResetFunction:Byte; {H/W reset function (162])}
  WriteReliabilityParameter:Byte; {Write reliability parameter register (166])}

  RPMBSizeMult:Byte; {RPMB Size Multiplier (168)}
  EraseGroupDef:Byte; {High-density erase group definition (175)}
  PartConfig:Byte; {Partition configuration (179)}
  ErasedMemoryContent:Byte; {Erased memory content (181)}
  StrobeSupport:Byte; {Strobe Support (184)}
  CSDStructure:Byte; {CSD STRUCTURE (194)}
  CardType:Byte; {Device type (196)}
  DriverStrength:Byte; {I/O Driver Strength (197)}
  OutOfInterruptTime:Byte; {Out-of-interrupt busy timing (198)}

  SectorCount:array[0..3] of Byte; {Sector Count (212)(4 bytes)}
  SATimeout:Byte; {Sleep/awake timeout (217)}
  HCEraseGapSize:Byte; {High-capacity write protect group size (221)}
  ReliableSectors:Byte; {Reliable write sector count (222)}
  EraseTimeoutMult:Byte; {High-capacity erase timeout (223)}
  HCEraseGrpSize:Byte; {High-capacity erase unit size (224)}
  SecTRIMMult:Byte; {Secure TRIM Multiplier (229)}
  SecEraseMult:Byte; {Secure Erase Multiplier (230)}
  SecFeatureSupport:Byte; {Secure Feature support (231)}
  TRIMMult:Byte; {TRIM Multiplier (232)}

  PowerClass52MHz195:Byte; {Power class for 52 MHz at 1.95 V (200)}
  PowerClass26MHz195:Byte; {Power class for 26 MHz at 1.95 V (201)}
  PowerClass52MHz360:Byte; {Power class for 52 MHz at 3.6 V (202)}
  PowerClass26MHz360:Byte; {Power class for 26 MHz at 3.6 V (203)}
  PowerClass200MHz195:Byte; {Power class for 200MHz, at VCCQ = 1.3V, VCC = 3.6V (236)}
  PowerClass200MHz360:Byte; {Power class for 200MHz at VCCQ = 1.95V, VCC = 3.6V (237)}
  PowerClassDDR52MHz195:Byte; {Power class for 52MHz, DDR at VCC = 1.95V (238)}
  PowerClassDDR52MHz360:Byte; {Power class for 52MHz, DDR at VCC = 3.6V (239)}
  PowerClassDDR200MHz360:Byte; {Power class for 200MHz, DDR at VCC = 3.6V (253)}

  BKOPSStatus:Byte; {Background operations status (246)}

  FirmwareVersion:array[0..MMC_FIRMWARE_VERSION_LEN - 1] of Byte; {(254) (8 bytes)}

  PreEndOfLifeInfo:Byte; {Pre EOL information (267)}
  DeviceLifetimeEstimateA:Byte; {Device life time estimation type A (268)}
  DeviceLifetimeEstimateB:Byte; {Device life time estimation type B (269)}

  MaxPackedWrites:Byte; {Max packed write commands (500)}
  MaxPackedReads:Byte; {Max packed read commands (501])}


  {Calculated Values}
  Sectors:LongWord;
  SleepAwakeTime:LongWord; {100ns units}
  PartitionSwitchTime:LongWord; {ms}
  GenericCMD6Time:LongWord; {10ms units}
  PowerOffLongTime:LongWord; {ms}
  HCEraseSize:LongWord; {Sectors}
  HCEraseTimeout:LongWord; {Milliseconds}
  DataSectorSize:LongWord; {512 bytes or 4KB}
  DataTagUnitSize:LongWord;
  HSMaxRate:LongWord; {Hz}
  HS200MaxRate:LongWord; {Hz}
  AvailableTypes:LongWord;
  BootPartitionSize:UInt64; {Bytes}
  EnhancedAreaOffset:UInt64; {Bytes}
  EnhancedAreaSize:UInt64; {KB}
  CacheSize:LongWord; {KB}
  PartitionSettingCompleted:Boolean; {Enable Bit}
  TRIMTimeout:LongWord; {Milliseconds}
  PartitionSizes:array[0..MMC_NUM_GP_PARTITION - 1] of UInt64; {Bytes}
  BootReadOnlySupport:LongWord; {Read Only Lock Support}
  BootReadOnlyLockable:Boolean;
  FieldFirmwareUpdate:Boolean; {Firmware upgrade support}
  CommandQueueSupport:Boolean; {Command Queue supported}
  CommandQueueDepth:LongWord; {Command Queue depth}
  BackgroundOperations:Boolean; {BKOPS Supported}
  ManualBKOPSEnable:Boolean; {Manual BKOPS Supported}
  AutoBKOPSEnable:Boolean; {Auto BKOPS Supported}
  HPI:Boolean; {HPI (High Priority Interrupt) Supported}
  HPIEnable:Boolean; {HPI Enabled}
  HPICommand:LongWord; {CMD used as HPI}
  RPMBSize:UInt64; {Bytes}
  EnhancedRPMBSupport:Boolean;
  ErasedByte:Byte; {Value after Erase}
  FeatureSupport:LongWord; {Version specific features (eg MMC_DISCARD_FEATURE)}
 end;

 {SD Status Data (SSR)} {See: Section 4.10.2 of SD Physical Layer Simplified Specification Version 4.10} {Defined here for MMC Device}
 PSDStatusData = ^TSDStatusData;
 TSDStatusData = record
  {Card Values}
  BusWidth:Byte;              {Shows the currently defined data bus width that was defined by SET_BUS_WIDTH command}
  SecuredMode:Boolean;        {Card is in Secured Mode of operation (refer to the "Part 3 Security Specification")}
  CardType:Word;              {In the future, the 8 LSBs will be used to define different variations of an SD Memory Card (Each bit will define different SD Types). The 8 MSBs will be used to define SD Cards that do not comply with the Physical Layer Specification}
  ProtectedSize:LongWord;     {Size of protected area}
  SpeedClass:Byte;            {Speed Class of the card}
  PerformanceMove:Byte;       {Performance of move indicated by 1 [MB/s] step}
  EraseSize:Word;             {Number of AUs to be erased at a time}
  EraseTimeout:Byte;          {Timeout value for erasing areas specified by UNIT_OF_ERASE_AU}
  EraseOffset:Byte;           {Fixed offset value added to erase time}
  UHSSpeedGrade:Byte;         {Speed Grade for UHS mode}
  {Calculated Values}
  AllocationUnitSize:Byte;    {Size of Allocation Unit}
  UHSAllocationUnitSize:Byte; {Size of Allocation Unit for UHS mode}
 end;

 {SD Switch Data} {See: Section 4.3.10 of SD Physical Layer Simplified Specification Version 4.10}
 PSDSwitchData = ^TSDSwitchData;
 TSDSwitchData = record
  {Card Values}
  MaximumCurrent:Word;   {Maximum Current/Power Consumption}
  Group6Support:Word;    {Support Bits of Functions in Function Group 6}
  Group5Support:Word;    {Support Bits of Functions in Function Group 5}
  Group4Support:Word;    {Support Bits of Functions in Function Group 4}
  Group3Support:Word;    {Support Bits of Functions in Function Group 3}
  Group2Support:Word;    {Support Bits of Functions in Function Group 2}
  Group1Support:Word;    {Support Bits of Functions in Function Group 1}
  Group6Selection:Byte;  {Function Selection of Function Group 6}
  Group5Selection:Byte;  {Function Selection of Function Group 5}
  Group4Selection:Byte;  {Function Selection of Function Group 4}
  Group3Selection:Byte;  {Function Selection of Function Group 3}
  Group2Selection:Byte;  {Function Selection of Function Group 2}
  Group1Selection:Byte;  {Function Selection of Function Group 1}
  StructureVersion:Byte; {Data Structure Version}
  Group6BusyStatus:Word; {Busy Status of functions in group 6}
  Group5BusyStatus:Word; {Busy Status of functions in group 5}
  Group4BusyStatus:Word; {Busy Status of functions in group 4}
  Group3BusyStatus:Word; {Busy Status of functions in group 3}
  Group2BusyStatus:Word; {Busy Status of functions in group 2}
  Group1BusyStatus:Word; {Busy Status of functions in group 1}
 end;

 {SD Configuration Data (SCR)} {See: Section 5.6 of SD Physical Layer Simplified Specification Version 4.10} {Defined here for MMC Device}
 PSDConfigurationData = ^TSDConfigurationData;
 TSDConfigurationData = record
  {Card Values}
  SCRStructure:Byte;       {SCR Structure version}
  SpecVersion:Byte;        {SD Memory Card - Spec. Version}
  DataAfterErase:Byte;     {Data status after erases}
  Security:Byte;           {CPRM Security Support}
  BusWidths:Byte;          {DAT Bus widths supported}
  SpecVersion3:Boolean;    {Spec. Version 3.00 or higher}
  ExtendedSecurity:Byte;   {Extended Security Support}
  SpecVersion4:Boolean;    {Spec. Version 4.00 or higher}
  CommandSupport:Byte;     {Command Support bits}
  {Calculated Values}
  ErasedByte:Byte;         {Value after Erase}
 end;

 {MMC Device}
 PMMCDevice = ^TMMCDevice;
 PSDIOCCCR = ^TSDIOCCCR;         {Forward declared to satisfy MMCDevice}
 PSDIOCIS = ^TSDIOCIS;           {Forward declared to satisfy MMCDevice}
 PSDIOTuple = ^TSDIOTuple;       {Forward declared to satisfy MMCDevice}
 PSDIOFunction = ^TSDIOFunction; {Forward declared to satisfy MMCDevice}

 {MMC Enumeration Callback}
 TMMCEnumerate = function(MMC:PMMCDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {MMC Notification Callback}
 TMMCNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {MMC Device Methods}
 TMMCDeviceInitialize = function(MMC:PMMCDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TMMCDeviceDeinitialize = function(MMC:PMMCDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TMMCDeviceGetCardDetect = function(MMC:PMMCDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TMMCDeviceGetWriteProtect = function(MMC:PMMCDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TMMCDeviceSendCommand = function(MMC:PMMCDevice;Command:PMMCCommand):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TMMCDeviceSetIOS = function(MMC:PMMCDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TMMCDevice = record
  {Device Properties}
  Device:TDevice;                                  {The Device entry for this MMC}
  {MMC Properties}
  MMCId:LongWord;                                  {Unique Id of this MMC in the MMC table}
  MMCState:LongWord;                               {MMC state (eg MMC_STATE_INSERTED)}
  DeviceInitialize:TMMCDeviceInitialize;           {A Device specific DeviceInitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceDeinitialize:TMMCDeviceDeinitialize;       {A Device specific DeviceDeinitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetCardDetect:TMMCDeviceGetCardDetect;     {A Device specific DeviceGetCardDetect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetWriteProtect:TMMCDeviceGetWriteProtect; {A Device specific DeviceGetWriteProtect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSendCommand:TMMCDeviceSendCommand;         {A Device specific DeviceSendCommand method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSetIOS:TMMCDeviceSetIOS;                   {A Device specific DeviceSetIOS method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  CommandCount:LongWord;
  CommandErrors:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                               {Device lock}
  Version:LongWord;
  Clock:LongWord;
  Timing:LongWord;
  BusWidth:LongWord;
  DriverType:LongWord;
  SignalVoltage:LongWord;
  Voltages:LongWord;
  Capabilities:LongWord;
  Capabilities2:LongWord;
  EraseSize:LongWord;                              {Erase Size in Sectors}
  EraseShift:LongWord;
  EraseArgument:LongWord;
  PreferredEraseSize:LongWord;                     {Preferred Erase Size in Sectors}
  EnhancedStrobe:LongBool;
  {Register Properties}                            {See: Table 3-2: SD Memory Card Registers of SD Physical Layer Simplified Specification Version 4.10}
  InterfaceCondition:LongWord;                     {Interface Condition Result}
  OperationCondition:LongWord;                     {Operation Condition Register (OCR)} {See: Section 5.1 of SD Physical Layer Simplified Specification Version 4.10}
  RelativeCardAddress:LongWord;                    {Relative Card Address (RCA) (Word)} {See: Section 5.4 of SD Physical Layer Simplified Specification Version 4.10}
  CardSpecific:array[0..3] of LongWord;            {Card Specific Data (CSD)}           {See: Section 5.3 of SD Physical Layer Simplified Specification Version 4.10}
  CardIdentification:array[0..3] of LongWord;      {Card Identification Data (CID)}     {See: Section 5.2 of SD Physical Layer Simplified Specification Version 4.10}
  ExtendedCardSpecific:PByte;                      {Extended Card Specific Data (EXTCSD)}{See: Section 7.4 of Embedded Multi-Media Card (eMMC) Electrical Standard 5.1}
  CardStatus:LongWord;                             {Card Status Register (CSR)}         {See: Section 4.10.1 of SD Physical Layer Simplified Specification Version 4.10}
  DriverStage:LongWord;                            {Driver Stage Register (DSR) (Word)} {See: Section 5.5 of SD Physical Layer Simplified Specification Version 4.10}
  SDStatus:array[0..15] of LongWord;               {SD Status Register (SSR)}           {See: Section 4.10.2 of SD Physical Layer Simplified Specification Version 4.10}
  SDSwitch:array[0..15] of LongWord;               {SD Switch Status}                   {See: Section 4.3.10 of SD Physical Layer Simplified Specification Version 4.10}
  SDConfiguration:array[0..3] of LongWord;         {SD Configuration Register (SCR)}    {See: Section 5.6 of SD Physical Layer Simplified Specification Version 4.10}
  {Configuration Properties}                       {Note: 16 bytes instead of 8 to allow correct operation of MMCExtractBits, offset handled by SDDeviceSendSDConfiguration}
  CardSpecificData:TMMCCardSpecificData;
  CardIdentificationData:TMMCCardIdentificationData;
  ExtendedCardSpecificData:TMMCExtendedCardSpecificData;
  SDStatusData:TSDStatusData;
  SDSwitchData:TSDSwitchData;
  SDConfigurationData:TSDConfigurationData;
  {SDIO Properties}
  CCCR:PSDIOCCCR;                                  {SDIO Common Card Register Information}
  CIS:PSDIOCIS;                                    {SDIO Common CIS (Card Information Structure) Information}
  Tuples:PSDIOTuple;                               {SDIO CIS (Card Information Structure) tuples on this MMC}
  SDIOCount:LongWord;                              {SDIO function count for this MMC (Where Applicable)}
  SDIOFunctions:array[0..SDIO_MAX_FUNCTIONS - 1] of PSDIOFunction; {SDIO functions on this MMC (Where Applicable)}
  SDIOInterruptFunction:PSDIOFunction;             {SDIO function for all interrupts (If only one interrupt is registered)}
  {Storage Properties}
  Storage:PStorageDevice;                          {The Storage entry for this MMC (Where Applicable)}
  {Internal Properties}
  Prev:PMMCDevice;                                 {Previous entry in MMC table}
  Next:PMMCDevice;                                 {Next entry in MMC table}
 end;

{==============================================================================}
{type}
 {SD specific types}

{==============================================================================}
{type}
 {SDIO specific types}
 {PSDIOCCCR = ^TSDIOCCCR;} {Declared above for MMCDevice}
 TSDIOCCCR = record
  {SDIO Properties}
  CCCRVersion:Byte;                          {CCCR (Card Common Control Register) Version (00h)}
  SDIOVersion:Byte;                          {SDIO Specification Version (00h)}
  SDVersion:Byte;                            {SD Specification Version (04h)}
  CCCRCapabilities:Byte;                     {CCCR Capabilities Register (08h)}
  CCCRPowerControl:Byte;                     {CCCR Power Control Register (12h)}
  CCCRBusSpeed:Byte;                         {CCCR Bus Speed Select Register (13h)}
  CCCRUHSSupport:Byte;                       {CCCR UHS-I Support Register (14h)}
  CCCRDriverStrength:Byte;                   {CCCR Driver Strength Register (15h)}
  {Driver Properties}
  MultiBlock:Boolean;                        {Card Capability Multi Block Transfer (SMB)}
  LowSpeed:Boolean;                          {Card Capability Low Speed Card (LSC)}
  WideBus:Boolean;                           {Card Capability 4-bit Mode for Low Speed Card (4BLS)}
  HighPower:Boolean;                         {Support Master Power Control (SMPC)}
  HighSpeed:Boolean;                         {Support High Speed (SHS)}
 end;

 {Common SDIO CIS tuple}
 {PSDIOCIS = ^TSDIOCIS;} {Declared above for MMCDevice}
 TSDIOCIS = record
  Vendor:Word;
  Device:Word;
  BlockSize:Word;
  MaxClock:LongWord;
 end;

 {SDIO function CIS tuple (Function specific)}
 {PSDIOTuple = ^TSDIOTuple;} {Declared above for MMCDevice}
 TSDIOTuple = record
  Next:PSDIOTuple;
  Code:Byte;
  Size:Byte;
  Data:array[0..0] of Byte;
 end;

 {SDIO Function}
 PSDIODriver = ^TSDIODriver;       {Forward declared to satisfy SDIOFunction}

 {PSDIOFunction = ^TSDIOFunction;} {Declared above for MMCDevice}

 {SDIO Function Interrupt Handler}
 TSDIOInterruptHandler = procedure(Func:PSDIOFunction);{$IFDEF i386} stdcall;{$ENDIF}

 {SDIO Function Enumeration Callback}
 TSDIOFunctionEnumerate = function(Func:PSDIOFunction;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSDIOFunction = record
  {Function Properties}
  SDIOState:LongWord;                     {SDIO function state (eg SDIO_STATE_ATTACHED)}
  SDIOStatus:LongWord;                    {SDIO function status (eg SDIO_STATUS_BOUND)}
  MMC:PMMCDevice;                         {The MMC device for this function}
  Number:LongWord;                        {The function number}
  ClassId:Byte;                           {Standard class Id}
  VendorId:Word;                          {Vendor Id}
  DeviceId:Word;                          {Device Id}
  BlockSize:LongWord;                     {Current block size}
  MaxBlockSize:LongWord;                  {Maximum block size}
  EnableTimeout:LongWord;                 {Function enable timeout}
  Handler:TSDIOInterruptHandler;          {Interrupt handler for this function}
  DMABuffer:Pointer;                      {DMA compatible buffer for small reads and writes}
  Tuples:PSDIOTuple;                      {CIS (Card Information Structure) tuples for this function}
  Driver:PSDIODriver;                     {Driver this function is bound to, if any}
  DriverData:Pointer;                     {Private data for the driver of this SDIO device}
 end;

 {SDIO Driver}
 {PSDIODriver = ^TSDIODriver;} {Declared above for SDIOFunction}

 {SDIO Driver Enumeration Callback}
 TSDIODriverEnumerate = function(Driver:PSDIODriver;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {SDIO Driver Methods}
 TSDIODriverBind = function(MMC:PMMCDevice;Func:PSDIOFunction):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDIODriverUnbind = function(MMC:PMMCDevice;Func:PSDIOFunction):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSDIODriver = record
  {Driver Properties}
  Driver:TDriver;                 {The Driver entry for this SDIO Driver}
  {SDIO Properties}
  DriverBind:TSDIODriverBind;     {A Driver specific DriverBind method implementing the standard SDIO driver interface}
  DriverUnbind:TSDIODriverUnbind; {A Driver specific DriverUnbind method implementing the standard SDIO driver interface}
  {Interface Properties}
  Lock:TMutexHandle;              {Driver lock}
  {Internal Properties}
  Prev:PSDIODriver;               {Previous entry in Driver table}
  Next:PSDIODriver;               {Next entry in Driver table}
 end;

{==============================================================================}
{type}
 {SDHCI specific types}
 {ADMA2 32-bit descriptor (See ADMA2 Descriptor Format - SD Host Controller Simplified Specification Version 4.20)}
 PSDHCIADMA2Descriptor32 = ^TSDHCIADMA2Descriptor32;
 TSDHCIADMA2Descriptor32 = packed record
  Command:Word;
  Len:Word;
  Address:LongWord;
 end;

 {ADMA2 64-bit descriptors (See ADMA2 Descriptor Format - SD Host Controller Simplified Specification Version 4.20)}
 {Note 12-byte descriptor can't always be 8-byte aligned}
 PSDHCIADMA2Descriptor64 = ^TSDHCIADMA2Descriptor64;
 TSDHCIADMA2Descriptor64 = packed record
  Command:Word;
  Len:Word;
  AddressLow:LongWord;
  AddressHigh:LongWord;
 end;

 PSDHCIADMA2Descriptor64v4 = ^TSDHCIADMA2Descriptor64v4;
 TSDHCIADMA2Descriptor64v4 = packed record
  Command:Word;
  Len:Word;
  AddressLow:LongWord;
  AddressHigh:LongWord;
  Reserved:LongWord;
 end;

 {SDHCI Host}
 PSDHCIHost = ^TSDHCIHost;

 {SDHCI Enumeration Callback}
 TSDHCIEnumerate = function(SDHCI:PSDHCIHost;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {SDHCI Notification Callback}
 TSDHCINotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {SDHCI Host Methods}
 TSDHCIHostStart = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostStop = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostLock = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostUnlock = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSignal = function(SDHCI:PSDHCIHost;Semaphore:TSemaphoreHandle):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadByte = function(SDHCI:PSDHCIHost;Reg:LongWord):Byte;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadWord = function(SDHCI:PSDHCIHost;Reg:LongWord):Word;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadLong = function(SDHCI:PSDHCIHost;Reg:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteByte = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteWord = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteLong = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReset = function(SDHCI:PSDHCIHost;Mask:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostHardwareReset = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetPower = function(SDHCI:PSDHCIHost;Power:Word):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetClock = function(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetTiming = function(SDHCI:PSDHCIHost;Timing:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetBusWidth = function(SDHCI:PSDHCIHost;BusWidth:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetClockDivider = function(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetControlRegister = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostPrepareDMA = function(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostStartDMA = function(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostStopDMA = function(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetupCardIRQ = function(SDHCI:PSDHCIHost;Enable:LongBool):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostCompleteCardIRQ = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSDHCIHost = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this SDHCI}
  {SDHCI Properties}
  SDHCIId:LongWord;                    {Unique Id of this SDHCI in the SDHCI table}
  SDHCIState:LongWord;                 {SDHCI state (eg SDHCI_STATE_ENABLED)}
  HostStart:TSDHCIHostStart;           {A Host specific HostStart method implementing a standard SDHCI host interface}
  HostStop:TSDHCIHostStop;             {A Host specific HostStop method implementing a standard SDHCI host interface}
  HostLock:TSDHCIHostLock;             {A Host specific HostLock method implementing a standard SDHCI host interface}
  HostUnlock:TSDHCIHostUnlock;         {A Host specific HostUnlock method implementing a standard SDHCI host interface}
  HostSignal:TSDHCIHostSignal;         {A Host specific HostSignal method implementing a standard SDHCI host interface}
  HostReadByte:TSDHCIHostReadByte;     {A Host specific HostReadByte method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostReadWord:TSDHCIHostReadWord;     {A Host specific HostReadWord method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostReadLong:TSDHCIHostReadLong;     {A Host specific HostReadLong method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteByte:TSDHCIHostWriteByte;   {A Host specific HostWriteByte method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteWord:TSDHCIHostWriteWord;   {A Host specific HostWriteWord method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteLong:TSDHCIHostWriteLong;   {A Host specific HostWriteLong method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostReset:TSDHCIHostReset;                       {A Host specific HostReset method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostHardwareReset:TSDHCIHostHardwareReset;       {A Host specific HostHardwareReset method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetPower:TSDHCIHostSetPower;                 {A Host specific HostSetPower method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetClock:TSDHCIHostSetClock;                 {A Host specific HostSetClock method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetTiming:TSDHCIHostSetTiming;               {A Host specific HostSetTiming method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetBusWidth:TSDHCIHostSetBusWidth;           {A Host specific HostSetBusWidth method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetClockDivider:TSDHCIHostSetClockDivider;
  HostSetControlRegister:TSDHCIHostSetControlRegister;
  HostPrepareDMA:TSDHCIHostPrepareDMA;             {A Host specific HostPrepareDMA method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostStartDMA:TSDHCIHostStartDMA;                 {A Host specific HostStartDMA method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostStopDMA:TSDHCIHostStopDMA;                   {A Host specific HostStopDMA method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetupCardIRQ:TSDHCIHostSetupCardIRQ;         {A Host specific HostSetupCardIRQ method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostCompleteCardIRQ:TSDHCIHostCompleteCardIRQ;   {A Host specific HostCompleteCardIRQ method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  DeviceInitialize:TMMCDeviceInitialize;           {A Device specific DeviceInitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceDeinitialize:TMMCDeviceDeinitialize;       {A Device specific DeviceDeinitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetCardDetect:TMMCDeviceGetCardDetect;     {A Device specific DeviceGetCardDetect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetWriteProtect:TMMCDeviceGetWriteProtect; {A Device specific DeviceGetWriteProtect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSendCommand:TMMCDeviceSendCommand;         {A Device specific DeviceSendCommand method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSetIOS:TMMCDeviceSetIOS;                   {A Device specific DeviceSetIOS method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Spin:TSpinHandle;                    {Host lock (Spin) for use by interrupt handlers}
  Lock:TMutexHandle;                   {Host lock (Mutex) for use by other functions}
  Address:Pointer;                     {Host register base address}
  Version:LongWord;                    {Host version information}
  Quirks:LongWord;                     {Host quirks/bugs flags}
  Quirks2:LongWord;                    {Host additional quirks/bugs flags}
  Clock:LongWord;                      {Host current clock}
  Power:LongWord;                      {Host current power}
  Timing:LongWord;                     {Host current timing}
  BusWidth:LongWord;                   {Host current bus width}
  Interrupts:LongWord;                 {Host interrupts to be handled}
  Voltages:LongWord;                   {Host configured voltage flags}
  Capabilities:LongWord;               {Host configured capabilities flags}
  Capabilities2:LongWord;              {Host configured additional capabilities flags}
  TimeoutFrequency:LongWord;           {Host configured timeout clock frequency (KHz)}
  MinimumFrequency:LongWord;           {Host configured minimum frequency (Hz)}
  MaximumFrequency:LongWord;           {Host configured maximum frequency (Hz)}
  MaximumBlockSize:LongWord;           {Host configured maximum block size}
  MaximumBlockCount:LongWord;          {Host configured maximum block count}
  MaximumRequestSize:LongWord;         {Host configured maximum request size}
  MaximumBusyTimeout:LongWord;         {Host configured maximum busy timeout (Milliseconds)}
  MinimumDMASize:LongWord;             {Minimum size for DMA read or write (Use PIO if less)}
  MaximumPIOBlocks:LongWord;           {Maximum blocks for PIO read or write (Use DMA if greater)}
  PresetEnabled:LongBool;              {Version 3.00 Preset Values Enabled (If applicable)}
  CardIRQEnabled:LongBool;             {SDIO card interrupt is enabled}
  CardIRQWorker:TWorkerHandle;         {SDIO card interrupt current worker}
  CardIRQDevice:PMMCDevice;            {SDIO card interrupt device}
  CardIRQCount:LongWord;               {SDIO card interrupt function registered count}
  Command:PMMCCommand;                 {Currently processing command}
  Wait:TSemaphoreHandle;               {Command completed semaphore}
  UseDMA:LongBool;                     {Use DMA for the current data transfer}
  DMAData:TDMAData;                    {External DMA data descriptor for current request (If applicable)}
  DMAWait:TSemaphoreHandle;            {External DMA completed semaphore}
  DMASlave:LongWord;                   {External DMA slave (DREQ) Id for this device (If applicable)}
  DMABuffer:Pointer;                   {DMA bounce buffer for the current request (If applicable)}
  DMADirection:LongWord;               {DMA data direction for current request (If applicable)}
  SDMABoundary:LongWord;               {SDMA buffer boundary argument}
  ADMATable:PSDHCIADMA2Descriptor64v4; {ADMA descriptors for current request (If applicable)}
  ADMABuffer:Pointer;                  {ADMA alignment buffers for the current request (If applicable)}
  ADMATableSize:LongWord;              {ADMA descriptor table size in bytes (If applicable)}
  ADMATableCount:LongWord;             {ADMA descriptor table entry count (If applicable)}
  ADMABufferSize:LongWord;             {ADMA alignment buffers size in bytes (If applicable)}
  ADMADescriptorSize:LongWord;         {ADMA descriptor size in bytes (If applicable)}
  {Configuration Properties}
  PresetVoltages:LongWord;             {Host predefined voltage flags}
  PresetCapabilities:LongWord;         {Host predefined capabilities flags}
  PresetCapabilities2:LongWord;        {Host predefined additional capabilities flags}
  ClockMinimum:LongWord;               {Host predefined minimum clock frequency}
  ClockMaximum:LongWord;               {Host predefined maximum clock frequency}
  DriverStageRegister:LongWord;        {Host predefined driver stage register (DSR)}
  EnableV4Mode:LongBool;               {Enable SDHCI version 4 protocol support}
  {Statistics Properties}
  RequestCount:LongWord;               {Number of requests that have been submitted to this host}
  RequestErrors:LongWord;              {Number of requests that have failed on this host}
  DataRequestCount:LongWord;           {Number of data requests that have been submitted to this host}
  CommandRequestCount:LongWord;        {Number of command requests that have been submitted to this host}
  PIODataTransferCount:LongWord;       {Number of data requests that have been submitted for PIO transfer on this host}
  DMADataTransferCount:LongWord;       {Number of data requests that have been submitted for DMA transfer on this host}
  InterruptCount:LongWord;             {Number of interrupt requests received by the host}
  DataInterruptCount:LongWord;         {Number of data interrupt requests received by the host}
  CommandInterruptCount:LongWord;      {Number of command interrupt requests received by the host}
  {Internal Properties}
  Prev:PSDHCIHost;                     {Previous entry in SDHCI table}
  Next:PSDHCIHost;                     {Next entry in SDHCI table}
 end;

{==============================================================================}
{var}
 {MMC specific variables}

{==============================================================================}
{var}
 {SD specific variables}

{==============================================================================}
{var}
 {SDIO specific variables}

{==============================================================================}
{var}
 {SDHCI specific variables}

{==============================================================================}
{Initialization Functions}
procedure MMCInit;
function MMCStart:LongWord;
function MMCStop:LongWord;

procedure MMCAsyncStart(SDHCI:PSDHCIHost);

{==============================================================================}
{MMC Functions}
function MMCDeviceReadBlocks(MMC:PMMCDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function MMCDeviceWriteBlocks(MMC:PMMCDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;

function MMCDeviceEraseBlocks(MMC:PMMCDevice;const Start,Count:Int64):LongWord;

function MMCDeviceGoIdle(MMC:PMMCDevice):LongWord;

function MMCDeviceSetClock(MMC:PMMCDevice;Clock:LongWord):LongWord;
function MMCDeviceSetTiming(MMC:PMMCDevice;Timing:LongWord):LongWord;
function MMCDeviceSetBusWidth(MMC:PMMCDevice;Width:LongWord):LongWord;

function MMCDeviceSetBlockLength(MMC:PMMCDevice;Length:LongWord):LongWord;
function MMCDeviceSetBlockCount(MMC:PMMCDevice;Count:LongWord;Relative:Boolean):LongWord;
function MMCDeviceSetDriverStage(MMC:PMMCDevice;DriverStage:LongWord):LongWord;

function MMCDeviceStopTransmission(MMC:PMMCDevice):LongWord;

function MMCDeviceSelectCard(MMC:PMMCDevice):LongWord;
function MMCDeviceDeselectCard(MMC:PMMCDevice):LongWord;

function MMCDeviceSwitch(MMC:PMMCDevice;Setting,Index,Value:Byte;Timeout:LongWord):LongWord;
function MMCDeviceSwitchEx(MMC:PMMCDevice;Setting,Index,Value:Byte;Timeout,Timing:LongWord;SendStatus,RetryCRCError:Boolean):LongWord;

function MMCDevicePollForBusy(MMC:PMMCDevice;Timeout,Command:LongWord):LongWord;
function MMCDevicePollForBusyEx(MMC:PMMCDevice;Timeout,Command:LongWord;SendStatus,RetryCRCError:Boolean):LongWord;

function MMCDeviceSendCardStatus(MMC:PMMCDevice):LongWord;

function MMCDeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord;

function MMCDeviceSendCardSpecific(MMC:PMMCDevice):LongWord;
function MMCDeviceDecodeCardSpecific(MMC:PMMCDevice):LongWord;

function MMCDeviceSendCardIdentification(MMC:PMMCDevice):LongWord;
function MMCDeviceSendAllCardIdentification(MMC:PMMCDevice):LongWord;
function MMCDeviceDecodeCardIdentification(MMC:PMMCDevice):LongWord;

function MMCDeviceGetExtendedCardSpecific(MMC:PMMCDevice):LongWord;
function MMCDeviceSendExtendedCardSpecific(MMC:PMMCDevice):LongWord;
function MMCDeviceDecodeExtendedCardSpecific(MMC:PMMCDevice):LongWord;

function MMCDeviceSetRelativeAddress(MMC:PMMCDevice):LongWord;

function MMCDeviceSPISetCRC(MMC:PMMCDevice;Enable:Boolean):LongWord;
function MMCDeviceSPIReadOperationCondition(MMC:PMMCDevice;HighCapacity:Boolean):LongWord;

function MMCDeviceInsert(MMC:PMMCDevice):LongWord;
function MMCDeviceRemove(MMC:PMMCDevice):LongWord;

function MMCDeviceInitialize(MMC:PMMCDevice):LongWord;
function MMCDeviceDeinitialize(MMC:PMMCDevice):LongWord;

function MMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
function MMCDeviceGetWriteProtect(MMC:PMMCDevice):LongWord;
function MMCDeviceSendCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord;
function MMCDeviceSetIOS(MMC:PMMCDevice):LongWord;

function MMCDeviceCreate:PMMCDevice;
function MMCDeviceCreateEx(Size:LongWord):PMMCDevice;
function MMCDeviceDestroy(MMC:PMMCDevice):LongWord;

function MMCDeviceRegister(MMC:PMMCDevice):LongWord;
function MMCDeviceDeregister(MMC:PMMCDevice):LongWord;

function MMCDeviceFind(MMCId:LongWord):PMMCDevice;
function MMCDeviceFindByDevice(Device:PDevice):PMMCDevice;
function MMCDeviceFindByName(const Name:String):PMMCDevice; inline;
function MMCDeviceFindByDescription(const Description:String):PMMCDevice; inline;
function MMCDeviceEnumerate(Callback:TMMCEnumerate;Data:Pointer):LongWord;

function MMCDeviceNotification(MMC:PMMCDevice;Callback:TMMCNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{SD Functions}
function SDDeviceSwitch(MMC:PMMCDevice;Mode,Group:Integer;Value:Byte;Buffer:Pointer):LongWord;

function SDDeviceSwitchHighspeed(MMC:PMMCDevice):LongWord;

function SDDeviceSetBusWidth(MMC:PMMCDevice;Width:LongWord):LongWord;

function SDDeviceSendInterfaceCondition(MMC:PMMCDevice):LongWord;

function SDDeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord;

function SDDeviceGetCardSpecific(MMC:PMMCDevice):LongWord;
function SDDeviceDecodeCardSpecific(MMC:PMMCDevice):LongWord;

function SDDeviceGetCardIdentification(MMC:PMMCDevice):LongWord;
function SDDeviceDecodeCardIdentification(MMC:PMMCDevice):LongWord;

function SDDeviceSendSDStatus(MMC:PMMCDevice):LongWord;
function SDDeviceDecodeSDStatus(MMC:PMMCDevice):LongWord;

function SDDeviceSendSDSwitch(MMC:PMMCDevice):LongWord;
function SDDeviceDecodeSDSwitch(MMC:PMMCDevice):LongWord;

function SDDeviceSendSDConfiguration(MMC:PMMCDevice):LongWord;
function SDDeviceDecodeSDConfiguration(MMC:PMMCDevice):LongWord;

function SDDeviceSendRelativeAddress(MMC:PMMCDevice):LongWord;

function SDDeviceSendApplicationCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord;

{==============================================================================}
{SDIO Functions}
//Device Methods
function SDIODeviceReset(MMC:PMMCDevice):LongWord;

function SDIODeviceEnableWideBus(MMC:PMMCDevice):LongWord;
function SDIODeviceDisableWideBus(MMC:PMMCDevice):LongWord;

function SDIODeviceEnableHighspeed(MMC:PMMCDevice):LongWord;
function SDIODeviceSwitchHighspeed(MMC:PMMCDevice;Enable:Boolean):LongWord;

function SDIODeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord;

function SDIODeviceReadWriteDirect(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Input:Byte;Output:PByte):LongWord;
function SDIODeviceReadWriteExtended(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Increment:Boolean;Buffer:Pointer;BlockCount,BlockSize:LongWord):LongWord;

function SDIODeviceReadByte(MMC:PMMCDevice;Address:LongWord;Output:PByte):LongWord;
function SDIODeviceWriteByte(MMC:PMMCDevice;Address:LongWord;Input:Byte):LongWord;

function SDIODeviceReadCCCR(MMC:PMMCDevice):LongWord;

function SDIODeviceReadFBR(Func:PSDIOFunction):LongWord;

function SDIODeviceReadCIS(MMC:PMMCDevice;Func:PSDIOFunction):LongWord;

function SDIODeviceReadCommonCIS(MMC:PMMCDevice):LongWord;
function SDIODeviceReadFunctionCIS(Func:PSDIOFunction):LongWord;

function SDIODeviceProcessInterrupts(MMC:PMMCDevice):LongWord;
function SDIODeviceRegisterInterrupt(MMC:PMMCDevice;Func:PSDIOFunction;Handler:TSDIOInterruptHandler):LongWord;
function SDIODeviceDeregisterInterrupt(MMC:PMMCDevice;Func:PSDIOFunction):LongWord;

function SDIODeviceBindFunctions(MMC:PMMCDevice):LongWord;
function SDIODeviceUnbindFunctions(MMC:PMMCDevice;Driver:PSDIODriver):LongWord;

//Function Methods
function SDIOFunctionAllocate(MMC:PMMCDevice;Number:LongWord):PSDIOFunction;
function SDIOFunctionRelease(Func:PSDIOFunction):LongWord;

function SDIOFunctionFind(MMC:PMMCDevice;Number:LongWord):PSDIOFunction;
function SDIOFunctionFindById(MMC:PMMCDevice;VendorId,DeviceId:Word):PSDIOFunction;
function SDIOFunctionEnumerate(MMC:PMMCDevice;Callback:TSDIOFunctionEnumerate;Data:Pointer):LongWord;

function SDIOFunctionBind(Func:PSDIOFunction;Driver:PSDIODriver):LongWord;
function SDIOFunctionUnbind(Func:PSDIOFunction;Driver:PSDIODriver):LongWord;

function SDIOFunctionEnable(Func:PSDIOFunction):LongWord;
function SDIOFunctionDisable(Func:PSDIOFunction):LongWord;

function SDIOFunctionSetBlockSize(Func:PSDIOFunction;BlockSize:LongWord):LongWord;

function SDIOFunctionReadWriteExtended(Func:PSDIOFunction;Write:Boolean;Address:LongWord;Increment:Boolean;Buffer:Pointer;Size:LongWord):LongWord;

function SDIOFunctionRead(Func:PSDIOFunction;Address:LongWord;Buffer:Pointer;Size:LongWord):LongWord;
function SDIOFunctionWrite(Func:PSDIOFunction;Address:LongWord;Buffer:Pointer;Size:LongWord):LongWord;

function SDIOFunctionReadByte(Func:PSDIOFunction;Address:LongWord;Output:PByte):LongWord;
function SDIOFunctionWriteByte(Func:PSDIOFunction;Address:LongWord;Input:Byte):LongWord;
function SDIOFunctionWriteReadByte(Func:PSDIOFunction;Address:LongWord;Input:Byte;Output:PByte):LongWord;

function SDIOFunctionReadWord(Func:PSDIOFunction;Address:LongWord;Output:PWord):LongWord;
function SDIOFunctionWriteWord(Func:PSDIOFunction;Address:LongWord;Input:Word):LongWord;

function SDIOFunctionReadLong(Func:PSDIOFunction;Address:LongWord;Output:PLongWord):LongWord;
function SDIOFunctionWriteLong(Func:PSDIOFunction;Address:LongWord;Input:LongWord):LongWord;

function SDIOFunctionRegisterInterrupt(Func:PSDIOFunction;Handler:TSDIOInterruptHandler):LongWord;
function SDIOFunctionDeregisterInterrupt(Func:PSDIOFunction):LongWord;

//Host Methods
function SDIOHostDispatchInterrupt(SDHCI:PSDHCIHost;IRQ,FIQ:Boolean):LongWord;

//Driver Methods
function SDIODriverCreate:PSDIODriver;
function SDIODriverCreateEx(Size:LongWord):PSDIODriver;
function SDIODriverDestroy(Driver:PSDIODriver):LongWord;

function SDIODriverRegister(Driver:PSDIODriver):LongWord;
function SDIODriverDeregister(Driver:PSDIODriver):LongWord;

function SDIODriverFind(DriverId:LongWord):PSDIODriver;
function SDIODriverFindByName(const Name:String):PSDIODriver; inline;
function SDIODriverEnumerate(Callback:TSDIODriverEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{SDHCI Functions}
function SDHCIHostReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
function SDHCIHostHardwareReset(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
function SDHCIHostSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;
function SDHCIHostSetTiming(SDHCI:PSDHCIHost;Timing:LongWord):LongWord;
function SDHCIHostSetBusWidth(SDHCI:PSDHCIHost;BusWidth:LongWord):LongWord;

function SDHCIHostPrepareDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;
function SDHCIHostStartDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;
function SDHCIHostStopDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;
procedure SDHCIHostCompleteDMA(Request:PDMARequest);{$IFDEF i386} stdcall;{$ENDIF}

function SDHCIHostSetupCardIRQ(SDHCI:PSDHCIHost;Enable:LongBool):LongWord;
function SDHCIHostCompleteCardIRQ(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostTransferPIO(SDHCI:PSDHCIHost):LongWord;
function SDHCIHostTransferDMA(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostFinishCommand(SDHCI:PSDHCIHost):LongWord;
function SDHCIHostFinishData(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostCommandInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord;var ReturnMask:LongWord):LongWord;
function SDHCIHostDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;

function SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
function SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostLock(SDHCI:PSDHCIHost):LongWord; inline;
function SDHCIHostUnlock(SDHCI:PSDHCIHost):LongWord; inline;

function SDHCIHostSignal(SDHCI:PSDHCIHost;Semaphore:TSemaphoreHandle):LongWord; inline;

function SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; inline;
function SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; inline;
function SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; inline;
procedure SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); inline;
procedure SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); inline;
procedure SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); inline;

function SDHCIHostSetClockDivider(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;
function SDHCIHostSetControlRegister(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostGetADMAAddress(SDHCI:PSDHCIHost):PtrUInt;
procedure SDHCIHostSetADMAAddress(SDHCI:PSDHCIHost;Address:PtrUInt);
function SDHCIHostGetSDMAAddress(SDHCI:PSDHCIHost;Command:PMMCCommand):PtrUInt;
procedure SDHCIHostSetSDMAAddress(SDHCI:PSDHCIHost;Address:PtrUInt);

procedure SDHCIHostWriteADMADescriptor(SDHCI:PSDHCIHost;var Descriptor:Pointer;Command,Len:Word;Address:PtrUInt);

function SDHCIHostCreate:PSDHCIHost;
function SDHCIHostCreateEx(Size:LongWord):PSDHCIHost;
function SDHCIHostDestroy(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostRegister(SDHCI:PSDHCIHost):LongWord;
function SDHCIHostDeregister(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostFind(SDHCIId:LongWord):PSDHCIHost;
function SDHCIHostEnumerate(Callback:TSDHCIEnumerate;Data:Pointer):LongWord;

function SDHCIHostNotification(SDHCI:PSDHCIHost;Callback:TSDHCINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{MMC Helper Functions}
function MMCGetCount:LongWord;

function MMCDeviceCheck(MMC:PMMCDevice):PMMCDevice;

function MMCIsSD(MMC:PMMCDevice):Boolean;
function MMCIsSDIO(MMC:PMMCDevice):Boolean;

function MMCGetSDHCI(MMC:PMMCDevice):PSDHCIHost; inline;

function MMCGetCIDValue(MMC:PMMCDevice;Version,Value:LongWord):LongWord;
function MMCGetCSDValue(MMC:PMMCDevice;Value:LongWord):LongWord;

function MMCExtractBits(Buffer:Pointer;Start,Size:LongWord):LongWord;
function MMCExtractBitsEx(Buffer:Pointer;Length,Start,Size:LongWord):LongWord;

function MMCIsMultiCommand(Command:Word):Boolean; inline;

function MMCIsNonRemovable(MMC:PMMCDevice):Boolean; inline;
function MMCHasExtendedCSD(MMC:PMMCDevice):Boolean; inline;
function MMCHasSetBlockCount(MMC:PMMCDevice):Boolean; inline;
function MMCHasAutoBlockCount(MMC:PMMCDevice):Boolean; inline;
function MMCHasAutoCommandStop(MMC:PMMCDevice):Boolean; inline;

function MMCStatusToString(Status:LongWord):String;

function MMCVersionToString(Version:LongWord):String;
function MMCTimingToString(Timing:LongWord):String;
function MMCBusWidthToString(BusWidth:LongWord):String;
function MMCDriverTypeToString(DriverType:LongWord):String;
function MMCSignalVoltageToString(SignalVoltage:LongWord):String;

function MMCDeviceTypeToString(MMCType:LongWord):String;
function MMCDeviceStateToString(MMCState:LongWord):String;

function MMCDeviceStateToNotification(State:LongWord):LongWord;

procedure MMCLog(Level:LongWord;MMC:PMMCDevice;const AText:String);
procedure MMCLogInfo(MMC:PMMCDevice;const AText:String); inline;
procedure MMCLogWarn(MMC:PMMCDevice;const AText:String); inline;
procedure MMCLogError(MMC:PMMCDevice;const AText:String); inline;
procedure MMCLogDebug(MMC:PMMCDevice;const AText:String); inline;

{==============================================================================}
{SD Helper Functions}
function SDGetMaxClock(MMC:PMMCDevice):LongWord;

function SDGetCIDValue(MMC:PMMCDevice;Value:LongWord):LongWord;
function SDGetCSDValue(MMC:PMMCDevice;Version,Value:LongWord):LongWord;
function SDGetSCRValue(MMC:PMMCDevice;Value:LongWord):LongWord;
function SDGetSSRValue(MMC:PMMCDevice;Value:LongWord):LongWord;
function SDGetSwitchValue(MMC:PMMCDevice;Value:LongWord):LongWord;

function SDVersionToString(Version:LongWord):String;
function SDBusWidthToString(BusWidth:LongWord):String;

{==============================================================================}
{SDIO Helper Functions}
function SDIODriverGetCount:LongWord;

function SDIODriverCheck(Driver:PSDIODriver):PSDIODriver;

function SDIODeviceGetMaxClock(MMC:PMMCDevice):LongWord;

function SDIOFunctionGetMMC(Func:PSDIOFunction):PMMCDevice;
function SDIOFunctionGetSDHCI(Func:PSDIOFunction):PSDHCIHost;

function SDIOVersionToString(Version:LongWord):String;

function SDIOFunctionStateToString(SDIOState:LongWord):String;
function SDIOFunctionStatusToString(SDIOStatus:LongWord):String;

function SDIOFunctionStateToNotification(State:LongWord):LongWord;
function SDIOFunctionStatusToNotification(Status:LongWord):LongWord;

{==============================================================================}
{SDHCI Helper Functions}
function SDHCIGetCount:LongWord;

function SDHCIHostCheck(SDHCI:PSDHCIHost):PSDHCIHost;

function SDHCIIsSPI(SDHCI:PSDHCIHost):Boolean;
function SDHCIHasDMA(SDHCI:PSDHCIHost):Boolean;
function SDHCIHasCMD23(SDHCI:PSDHCIHost):Boolean;
function SDHCIAutoCMD12(SDHCI:PSDHCIHost):Boolean;
function SDHCIAutoCMD23(SDHCI:PSDHCIHost):Boolean;

function SDHCIGetVersion(SDHCI:PSDHCIHost):Word;

function SDHCIGetCommand(Command:Word):Word;
function SDHCIMakeCommand(Command,Flags:Word):Word;
function SDHCIMakeBlockSize(DMA,BlockSize:Word):Word;

function SDHCIVersionToString(Version:LongWord):String;
function SDHCIPowerToString(Power:LongWord):String;

function SDHCIDeviceTypeToString(SDHCIType:LongWord):String; inline;
function SDHCIHostTypeToString(SDHCIType:LongWord):String;
function SDHCIDeviceStateToString(SDHCIState:LongWord):String; inline;
function SDHCIHostStateToString(SDHCIState:LongWord):String;

function SDHCIHostStateToNotification(State:LongWord):LongWord;

{==============================================================================}
{MMC Storage Functions}
function MMCStorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function MMCStorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function MMCStorageDeviceErase(Storage:PStorageDevice;const Start,Count:Int64):LongWord;
function MMCStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

{==============================================================================}
{SDIO Macro Replacement Functions}
function SDIO_FBR_BASE(Number:LongWord):LongWord; inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {MMC specific variables}
 MMCInitialized:Boolean;
 MMCStarted:Boolean;

 MMCDeviceTable:PMMCDevice;
 MMCDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 MMCDeviceTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{var}
 {SD specific variables}

{==============================================================================}
{==============================================================================}
var
 {SDIO specific variables}
 SDIODriverTable:PSDIODriver;
 SDIODriverTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SDIODriverTableCount:LongWord;

{==============================================================================}
{==============================================================================}
var
 {SDHCI specific variables}
 SDHCIHostTable:PSDHCIHost;
 SDHCIHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SDHCIHostTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
//MMC
function MMCDeviceSetBusSpeed(MMC:PMMCDevice):LongWord; forward;
function MMCDeviceSetEraseSize(MMC:PMMCDevice):LongWord; forward;

function MMCDeviceSelectHS(MMC:PMMCDevice):LongWord; forward;
function MMCDeviceSelectHSDDR(MMC:PMMCDevice):LongWord; forward;

function MMCDeviceSelectTiming(MMC:PMMCDevice):LongWord; forward;
function MMCDeviceSelectBusWidth(MMC:PMMCDevice):LongWord; forward;
function MMCDeviceSelectDriverType(MMC:PMMCDevice):LongWord; forward;
function MMCDeviceSelectPowerClass(MMC:PMMCDevice):LongWord; forward;

function MMCDeviceTestBusWidth(MMC:PMMCDevice;BusWidth:LongWord):LongWord; forward;

function MMCDeviceInitializeSDIO(MMC:PMMCDevice):LongWord; forward;
function MMCDeviceInitializeSD(MMC:PMMCDevice):LongWord; forward;
function MMCDeviceInitializeMMC(MMC:PMMCDevice):LongWord; forward;

//SD
function SDDeviceSetBusSpeed(MMC:PMMCDevice):LongWord; forward;

function SDDeviceSelectBusSpeed(MMC:PMMCDevice):LongWord; forward;
function SDDeviceSelectDriverType(MMC:PMMCDevice):LongWord; forward;

//SDIO

//SDHCI
function SDHCIHostGetPresetValue(SDHCI:PSDHCIHost):Word; forward;
function SDHCIHostEnablePresetValue(SDHCI:PSDHCIHost;Enable:Boolean):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure MMCInit;
{Performs basic initialization of the MMC/SD core, after this devices, hosts
 and drivers can be registered however nothing will work until MMCStart is called}
begin
 {}
 {Check Initialized}
 if MMCInitialized then Exit;

 {Initialize Logging}
 MMC_LOG_ENABLED:=(MMC_DEFAULT_LOG_LEVEL <> MMC_LOG_LEVEL_NONE);

 {Initialize MMC Device Table}
 MMCDeviceTable:=nil;
 MMCDeviceTableLock:=CriticalSectionCreate;
 MMCDeviceTableCount:=0;
 if MMCDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to create MMC device table lock');
  end;

 {Initialize SDIO Driver Table}
 SDIODriverTable:=nil;
 SDIODriverTableLock:=CriticalSectionCreate;
 SDIODriverTableCount:=0;
 if SDIODriverTableLock = INVALID_HANDLE_VALUE then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to create SDIO driver table lock');
  end;

 {Initialize SDHCI Host Table}
 SDHCIHostTable:=nil;
 SDHCIHostTableLock:=CriticalSectionCreate;
 SDHCIHostTableCount:=0;
 if SDHCIHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to create SDHCI host table lock');
  end;

 MMCInitialized:=True;
end;


{==============================================================================}

function MMCStart:LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=ERROR_SUCCESS;

 {Check Started}
 if MMCStarted then Exit;

 Result:=ERROR_INVALID_PARAMETER;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    SDHCI:=SDHCIHostTable;
    while SDHCI <> nil do
     begin
      {Start Host}
      SDHCIHostStart(SDHCI);

      {Get Next}
      SDHCI:=SDHCI.Next;
     end;

    {Set Started}
    MMCStarted:=True;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MMCStop:LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=ERROR_SUCCESS;

 {Check Started}
 if not(MMCStarted) then Exit;

 Result:=ERROR_INVALID_PARAMETER;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    SDHCI:=SDHCIHostTable;
    while SDHCI <> nil do
     begin
      {Stop Host}
      SDHCIHostStop(SDHCI);

      {Get Next}
      SDHCI:=SDHCI.Next;
     end;

    {Set Started}
    MMCStarted:=False;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

procedure MMCAsyncStart(SDHCI:PSDHCIHost);
begin
 {}
 {Wait for Ready}
 while not(SysInitCompleted) do
  begin
   ThreadSleep(0);
  end;

 {Check Host}
 if SDHCI = nil then
  begin
   {Start MMC}
   MMCStart;
  end
 else
  begin
   {Check Host}
   if SDHCIHostCheck(SDHCI) <> SDHCI then Exit;

   {Acquire the Lock}
   if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
    begin
     try
      {Start Host}
      SDHCIHostStart(SDHCI);
     finally
      {Release the Lock}
      CriticalSectionUnlock(SDHCIHostTableLock);
     end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Internal Functions}
function MMCDeviceSetBusSpeed(MMC:PMMCDevice):LongWord;
var
  MaximumRate:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Bus Speed');
 {$ENDIF}

 {Determine Maximum Rate}
 MaximumRate:=LongWord(-1);

 if ((MMC.Timing = MMC_TIMING_MMC_HS200) or (MMC.Timing = MMC_TIMING_MMC_HS400)) and (MaximumRate > MMC.ExtendedCardSpecificData.HS200MaxRate) then
  begin
   MaximumRate:=MMC.ExtendedCardSpecificData.HS200MaxRate;
  end
 else if ((MMC.Timing = MMC_TIMING_SD_HS) or (MMC.Timing = MMC_TIMING_MMC_HS)) and (MaximumRate > MMC.ExtendedCardSpecificData.HSMaxRate) then
  begin
   MaximumRate:=MMC.ExtendedCardSpecificData.HSMaxRate;
  end
 else if MaximumRate > MMC.CardSpecificData.DataTransferRate then
  begin
   MaximumRate:=MMC.CardSpecificData.DataTransferRate;
  end;

 {Set Clock}
 Result:=MMCDeviceSetClock(MMC,MaximumRate);

 //See: mmc_set_bus_speed in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSetEraseSize(MMC:PMMCDevice):LongWord;
var
 Size:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Erase Size');
 {$ENDIF}

 if not MMCIsSD(MMC) then
  begin
   if (MMC.ExtendedCardSpecificData.EraseGroupDef and $01) <> 0 then
    begin
     MMC.EraseSize:=MMC.ExtendedCardSpecificData.HCEraseSize;
    end
   else
    begin
     MMC.EraseSize:=MMC.CardSpecificData.EraseSize;
    end;
  end;

 if IsPowerOf2(MMC.EraseSize) then
  begin
   MMC.EraseShift:=FirstBitSet(MMC.EraseSize);
  end
 else
  begin
   MMC.EraseShift:=0;
  end;

 {Calculate Preferred Erase Size}
 if MMCIsSD(MMC) and (MMC.SDStatusData.AllocationUnitSize > 0) then
  begin
   MMC.PreferredEraseSize:=MMC.SDStatusData.AllocationUnitSize;
   MMC.EraseShift:=FirstBitSet(MMC.SDStatusData.AllocationUnitSize);
  end
 else if MMC.EraseSize > 0 then
  begin
   Size:=(MMC.CardSpecificData.BlockCount shl (MMC.CardSpecificData.BlockShift - 9)) shr 11;
   if Size < 128 then
    begin
     MMC.PreferredEraseSize:=SIZE_512K div 512;
    end
   else if Size < 512 then
    begin
     MMC.PreferredEraseSize:=SIZE_1M div 512;
    end
   else if Size < 1024 then
    begin
     MMC.PreferredEraseSize:=SIZE_2M div 512;
    end
   else
    begin
     MMC.PreferredEraseSize:=SIZE_4M div 512;
    end;

   if MMC.PreferredEraseSize < MMC.EraseSize then
    begin
     MMC.PreferredEraseSize:=MMC.EraseSize;
    end
   else
    begin
     Size:=MMC.PreferredEraseSize mod MMC.EraseSize;
     if Size > 0 then MMC.PreferredEraseSize:=MMC.PreferredEraseSize + MMC.EraseSize - Size;
    end;
  end
 else
  begin
   MMC.PreferredEraseSize:=0;
  end;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC EraseSize = ' + IntToStr(MMC.EraseSize));
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC EraseShift = ' + IntToStr(MMC.EraseShift));
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC PreferredEraseSize = ' + IntToStr(MMC.PreferredEraseSize));
 {$ENDIF}

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_set_erase_size in \drivers\mmc\core\mmc.c
 //     mmc_init_erase in \drivers\mmc\core\core.c
end;

{==============================================================================}

function MMCDeviceSelectHS(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Select HS');
 {$ENDIF}

 Result:=MMCDeviceSwitchEx(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_HS_TIMING,EXT_CSD_TIMING_HS,MMC.ExtendedCardSpecificData.GenericCMD6Time,MMC_TIMING_MMC_HS,True,True);
 if Result <> MMC_STATUS_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'MMC device switch to Highspeed failed');
  end;

 //See: mmc_select_hs in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSelectHSDDR(MMC:PMMCDevice):LongWord;
var
 SDHCI:PSDHCIHost;
 BusWidth:LongWord;
 ExtCSDBusWidth:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Select HS DDR');
 {$ENDIF}

 {Check Available Type}
 if (MMC.ExtendedCardSpecificData.AvailableTypes and EXT_CSD_CARD_TYPE_DDR_52) = 0 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Bus Width}
 BusWidth:=MMC.BusWidth;
 if BusWidth = MMC_BUS_WIDTH_1 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;
 if BusWidth = MMC_BUS_WIDTH_8 then ExtCSDBusWidth:=EXT_CSD_DDR_BUS_WIDTH_8 else ExtCSDBusWidth:=EXT_CSD_DDR_BUS_WIDTH_4;

 Result:=MMCDeviceSwitchEx(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_BUS_WIDTH,ExtCSDBusWidth,MMC.ExtendedCardSpecificData.GenericCMD6Time,MMC_TIMING_MMC_DDR52,True,True);
 if Result <> MMC_STATUS_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'MMC device switch to DDR Bus Width failed');
   Exit;
  end;

 {Check for 1.2v DDR}
 if (MMC.ExtendedCardSpecificData.AvailableTypes and EXT_CSD_CARD_TYPE_DDR_1_2V) <> 0 then
  begin
   //To Do //mmc_set_signal_voltage
  end;

 {Check for 1.8v DDR}
 if ((MMC.ExtendedCardSpecificData.AvailableTypes and EXT_CSD_CARD_TYPE_DDR_1_8V) <> 0) and ((SDHCI.Capabilities and MMC_CAP_1_8V_DDR) <> 0) then
  begin
   //To Do //mmc_set_signal_voltage
  end;

 {Revert to 3.3v DDR on failure}
 if Result <> MMC_STATUS_SUCCESS then
  begin
   //To Do //mmc_set_signal_voltage
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_select_hs_ddr in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSelectTiming(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Select Timing');
 {$ENDIF}

 {Check Extended CSD}
 if MMCHasExtendedCSD(MMC) then
  begin
   {if (MMC.ExtendedCardSpecificData.AvailableTypes and EXT_CSD_CARD_TYPE_HS400ES) <> 0 then
    begin
     Result:=MMCDeviceSelectHS400ES(MMC);
    end} {Not currently supported}
   {else if (MMC.ExtendedCardSpecificData.AvailableTypes and EXT_CSD_CARD_TYPE_HS200) <> 0 then
    begin
     Result:=MMCDeviceSelectHS200(MMC);
    end} {Not currently supported}
   if (MMC.ExtendedCardSpecificData.AvailableTypes and EXT_CSD_CARD_TYPE_HS) <> 0 then
    begin
     Result:=MMCDeviceSelectHS(MMC);
    end
   else
    begin
     Result:=MMC_STATUS_SUCCESS;
    end;

   if (Result <> MMC_STATUS_SUCCESS) and (Result <> MMC_STATUS_INVALID_DATA) then Exit;
  end;

 {Set the Bus Speed}
 Result:=MMCDeviceSetBusSpeed(MMC);

 //See: mmc_select_timing in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSelectBusWidth(MMC:PMMCDevice):LongWord;
const
 BUS_WIDTHS:array[0..1] of LongWord = (MMC_BUS_WIDTH_8,MMC_BUS_WIDTH_4);
 EXT_CSD_BUS_WIDTHS:array[0..1] of LongWord = (EXT_CSD_BUS_WIDTH_8,EXT_CSD_BUS_WIDTH_4);

var
 Index:LongWord;
 Status:LongWord;
 BusWidth:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Select Bus Width');
 {$ENDIF}

 {Check Extended CSD}
 if not MMCHasExtendedCSD(MMC) then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Supported Bus Widths}
 if (SDHCI.Capabilities and (MMC_CAP_4_BIT_DATA or MMC_CAP_8_BIT_DATA)) = 0 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check for 8 bit support}
 if (SDHCI.Capabilities and MMC_CAP_8_BIT_DATA) <> 0 then Index:=0 else Index:=1;

 {MMC cards dont have a configuration register to notify supported bus width}
 {So a bus test should be performed to identify the supported bus width}
 BusWidth:=MMC_BUS_WIDTH_1;
 Status:=MMC_STATUS_SUCCESS;
 while Index <= High(BUS_WIDTHS) do
  begin
   {Try to switch to new bus width, if the switch fails try the next width}
   Status:=MMCDeviceSwitch(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_BUS_WIDTH,EXT_CSD_BUS_WIDTHS[Index],MMC.ExtendedCardSpecificData.GenericCMD6Time);
   if Status = MMC_STATUS_SUCCESS then
    begin
     {Set Bus Width}
     BusWidth:=BUS_WIDTHS[Index];
     MMCDeviceSetBusWidth(MMC,BusWidth);

     {Test Bus Width}
     Status:=MMCDeviceTestBusWidth(MMC,BusWidth);
     if Status = MMC_STATUS_SUCCESS then Break;

     if MMC_LOG_ENABLED then MMCLogError(nil,'MMC device failed bus width test (Width=' + MMCBusWidthToString(BusWidth) + ')');
    end;

   Inc(Index);
  end;

 Result:=Status;

 //See: mmc_select_bus_width in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSelectDriverType(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Select Driver Type');
 {$ENDIF}

 //To Do //Driver Types

 //See: mmc_select_driver_type in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSelectPowerClass(MMC:PMMCDevice):LongWord;
var
 BusWidth:LongWord;
 ExtCSDBusWidth:LongWord;
 PowerClassValue:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Select Power Class');
 {$ENDIF}

 {Check Bus Width}
 BusWidth:=MMC.BusWidth;
 if BusWidth = MMC_BUS_WIDTH_1 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check Extended CSD}
 if MMCHasExtendedCSD(MMC) then
  begin
   {Check DDR}
   if (MMC.ExtendedCardSpecificData.AvailableTypes and EXT_CSD_CARD_TYPE_DDR_52) <> 0 then
    begin
     if BusWidth = MMC_BUS_WIDTH_8 then ExtCSDBusWidth:=EXT_CSD_DDR_BUS_WIDTH_8 else ExtCSDBusWidth:=EXT_CSD_DDR_BUS_WIDTH_4;
    end
   else
    begin
     if BusWidth = MMC_BUS_WIDTH_8 then ExtCSDBusWidth:=EXT_CSD_BUS_WIDTH_8 else ExtCSDBusWidth:=EXT_CSD_BUS_WIDTH_4;
    end;

   {Select Power Class Value}
   PowerClassValue:=0;
   //To Do //__mmc_select_powerclass

   {Check Bus Width}
   if (ExtCSDBusWidth and (EXT_CSD_BUS_WIDTH_8 or EXT_CSD_DDR_BUS_WIDTH_8)) <> 0 then
    begin
     {8 bit bus}
     PowerClassValue:=(PowerClassValue and EXT_CSD_PWR_CL_8BIT_MASK) shr EXT_CSD_PWR_CL_8BIT_SHIFT;
    end
   else
    begin
     {4 bit bus}
     PowerClassValue:=(PowerClassValue and EXT_CSD_PWR_CL_4BIT_MASK) shr EXT_CSD_PWR_CL_4BIT_SHIFT;
    end;

   {Set the Power Class Value}
   if PowerClassValue > 0 then
    begin
     Result:=MMCDeviceSwitch(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_POWER_CLASS,PowerClassValue,MMC.ExtendedCardSpecificData.GenericCMD6Time);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_select_powerclass in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceTestBusWidth(MMC:PMMCDevice;BusWidth:LongWord):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Test Bus Width (Width=' + MMCBusWidthToString(BusWidth) + ')');
 {$ENDIF}

 {Check Bus Width}
 if BusWidth = MMC_BUS_WIDTH_1 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check Extended CSD}
 if MMCHasExtendedCSD(MMC) then
  begin

   //To Do //mmc_compare_ext_csds

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: mmc_compare_ext_csds in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceInitializeSDIO(MMC:PMMCDevice):LongWord;

 function SDSetupCardMemory(MMC:PMMCDevice;SDHCI:PSDHCIHost):LongWord;
 begin
  {}
  {Get SD Configuration}
  Result:=SDDeviceSendSDConfiguration(MMC);
  if Result <> MMC_STATUS_SUCCESS then Exit;

  {Decode SD Configuration}
  Result:=SDDeviceDecodeSDConfiguration(MMC);
  if Result <> MMC_STATUS_SUCCESS then Exit;

  {Get SD Status}
  Result:=SDDeviceSendSDStatus(MMC);
  if Result <> MMC_STATUS_SUCCESS then Exit;

  {Decode SD Status}
  Result:=SDDeviceDecodeSDStatus(MMC);
  if Result <> MMC_STATUS_SUCCESS then Exit;

  {Set Erase Size}
  MMCDeviceSetEraseSize(MMC);

  {Get Switch}
  Result:=SDDeviceSendSDSwitch(MMC);
  if Result <> MMC_STATUS_SUCCESS then Exit;

  {Decode Switch}
  Result:=SDDeviceDecodeSDSwitch(MMC);
  if Result <> MMC_STATUS_SUCCESS then Exit;

  {Check for SPI}
  if SDHCIIsSPI(SDHCI) then
   begin
    {Enable SPI CRC} {Located AFTER the reading of the card registers because some SDHC cards are not able to provide valid CRCs for non-512-byte blocks}
    Result:=MMCDeviceSPISetCRC(MMC,True);
    if Result <> MMC_STATUS_SUCCESS then Exit;
   end;

  {Check Write Protect}
  Result:=MMCDeviceGetWriteProtect(MMC);
  if Result <> MMC_STATUS_SUCCESS then Exit;

  //See: mmc_sd_setup_card in \drivers\mmc\core\sd.c
 end;

var
 Funcs:LongWord;
 Count:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Initialize SDIO');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check for an SDIO Card}
 if SDIODeviceSendOperationCondition(MMC,True) = MMC_STATUS_SUCCESS then
  begin
   {SDIO Card}
   MMC.MMCState:=MMC_STATE_INSERTED;
   MMC.Device.DeviceBus:=DEVICE_BUS_SD;
   MMC.Device.DeviceType:=MMC_TYPE_SDIO;
   if (MMC.OperationCondition and SDIO_RSP_R4_MEMORY_PRESENT) <> 0 then MMC.Device.DeviceType:=MMC_TYPE_SD_COMBO;
   MMC.RelativeCardAddress:=0;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Initialize Card Type is SDIO');
   {$ENDIF}

   {Get the Operation Condition}
   Result:=SDIODeviceSendOperationCondition(MMC,False);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Check for SPI}
   if SDHCIIsSPI(SDHCI) then
    begin
     {Enable SPI CRC}
     Result:=MMCDeviceSPISetCRC(MMC,True);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Check for Combo}
   if MMC.Device.DeviceType = MMC_TYPE_SD_COMBO then
    begin
     //To Do //TestingSDIO //Preserve MMC.OperationCondition during call to SDDeviceGetCardIdentification

     {Get Card Identification}
     Result:=SDDeviceGetCardIdentification(MMC);
     if Result <> MMC_STATUS_SUCCESS then MMC.Device.DeviceType:=MMC_TYPE_SDIO;
    end;

   {If UHS-I mode supported switch to 1.8V signaling}
   if (MMC.OperationCondition and SDIO_RSP_R4_18V_PRESENT) <> 0 then
    begin
     //To Do //mmc_set_uhs_voltage
    end;

   {Check for SPI}
   if not(SDHCIIsSPI(SDHCI)) then
    begin
     {Get Relative Address}
     Result:=SDDeviceSendRelativeAddress(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Check for Combo}
   if MMC.Device.DeviceType = MMC_TYPE_SD_COMBO then
    begin
     {Get Card Specific}
     Result:=SDDeviceGetCardSpecific(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;

     {Decode Card Identification}
     Result:=SDDeviceDecodeCardIdentification(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Check for SPI}
   if not(SDHCIIsSPI(SDHCI)) then
    begin
     {Select Card}
     Result:=MMCDeviceSelectCard(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Read the common registers}
   Result:=SDIODeviceReadCCCR(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Read the common CIS tuples}
   Result:=SDIODeviceReadCommonCIS(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Check for Combo}
   if MMC.Device.DeviceType = MMC_TYPE_SD_COMBO then
    begin
     {Setup Card Memory}
     Result:=SDSetupCardMemory(MMC,SDHCI);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       {Set the Card to Idle State}
       MMCDeviceGoIdle(MMC);

       {Check for SPI}
       if SDHCIIsSPI(SDHCI) then
        begin
         {Enable SPI CRC}
         MMCDeviceSPISetCRC(MMC,True);
        end;

       {Revert to SDIO only}
       MMC.Device.DeviceType:=MMC_TYPE_SDIO;
      end;
    end;

   {Disconnect card detection pull-up resistor}
   //To Do //sdio_disable_cd

   {Initialize UHS-I if card supports 1.8v and UHS signaling}
   if (MMC.OperationCondition and SDIO_RSP_R4_18V_PRESENT) <> 0 then
    begin
     //To Do //mmc_sdio_init_uhs_card etc
    end;

   {Switch to High Speed if supported}
   Result:=SDIODeviceEnableHighspeed(MMC);
   if Result = MMC_STATUS_SUCCESS then
    begin
     {Set Timing}
     Result:=MMCDeviceSetTiming(MMC,MMC_TIMING_SD_HS);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end
   else if Result <> MMC_STATUS_UNSUPPORTED_REQUEST then
    begin
     Exit;
    end;

   {Set Clock}
   Result:=MMCDeviceSetClock(MMC,SDIODeviceGetMaxClock(MMC));
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Switch to 4 bit bus if supported}
   Result:=SDIODeviceEnableWideBus(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Update Device}
   if not SDHCIHasCMD23(SDHCI) then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_SET_BLOCK_COUNT);

   {Update Storage}
   MMC.Storage.Device.DeviceBus:=DEVICE_BUS_SD;

   {Get SDIO Functions}
   Funcs:=(MMC.OperationCondition and $70000000) shr 28;

   {Create SDIO Functions}
   for Count:=0 to Funcs - 1 do
    begin
     {Update Count}
     Inc(MMC.SDIOCount);

     {Allocate Function}
     MMC.SDIOFunctions[Count]:=SDIOFunctionAllocate(MMC,Count + 1);
     if MMC.SDIOFunctions[Count] = nil then
      begin
       Result:=MMC_STATUS_OPERATION_FAILED;
       Break;
      end;
    end;
   if Result <> MMC_STATUS_SUCCESS then
    begin
     for Count:=0 to SDIO_MAX_FUNCTIONS - 1 do
      begin
       SDIOFunctionRelease(MMC.SDIOFunctions[Count]);
      end;

     Exit;
    end;

   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 //See: mmc_sdio_init_card in \drivers\mmc\core\sdio.c
 //     mmc_attach_sdio in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function MMCDeviceInitializeSD(MMC:PMMCDevice):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Initialize SD');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check for an SD Card}
 if SDDeviceSendOperationCondition(MMC,True) = MMC_STATUS_SUCCESS then
  begin
   {SD Card}
   MMC.MMCState:=MMC_STATE_INSERTED;
   MMC.Device.DeviceBus:=DEVICE_BUS_SD;
   MMC.Device.DeviceType:=MMC_TYPE_SD;
   MMC.RelativeCardAddress:=0;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Initialize Card Type is SD');
   {$ENDIF}

   {Check for SPI}
   if SDHCIIsSPI(SDHCI) then
    begin
     {Set the Card to Idle State}
     MMCDeviceGoIdle(MMC);

     {Read the Operation Condition}
     Result:=MMCDeviceSPIReadOperationCondition(MMC,False);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Get Card Identification}
   Result:=SDDeviceGetCardIdentification(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Check for SPI}
   if not(SDHCIIsSPI(SDHCI)) then
    begin
     {Get Relative Address}
     Result:=SDDeviceSendRelativeAddress(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Get Card Specific}
   Result:=SDDeviceGetCardSpecific(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Decode Card Identification}
   Result:=SDDeviceDecodeCardIdentification(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Set Driver Stage}
   if MMC.CardSpecificData.DSRImplemented and (SDHCI.DriverStageRegister <> 0) then
    begin
     MMCDeviceSetDriverStage(MMC,SDHCI.DriverStageRegister);
    end;

   {Check for SPI}
   if not(SDHCIIsSPI(SDHCI)) then
    begin
     {Select Card}
     Result:=MMCDeviceSelectCard(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Get SD Configuration}
   Result:=SDDeviceSendSDConfiguration(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Decode SD Configuration}
   Result:=SDDeviceDecodeSDConfiguration(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Get SD Status}
   Result:=SDDeviceSendSDStatus(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Decode SD Status}
   Result:=SDDeviceDecodeSDStatus(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Set Erase Size}
   MMCDeviceSetEraseSize(MMC);

   {Get Switch}
   Result:=SDDeviceSendSDSwitch(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Decode Switch}
   Result:=SDDeviceDecodeSDSwitch(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Check for SPI}
   if SDHCIIsSPI(SDHCI) then
    begin
     {Enable SPI CRC} {Located AFTER the reading of the card registers because some SDHC cards are not able to provide valid CRCs for non-512-byte blocks}
     Result:=MMCDeviceSPISetCRC(MMC,True);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Check Write Protect}
   Result:=MMCDeviceGetWriteProtect(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   //To Do //Check for UHS-I and do UHS-I init //mmc_sd_init_uhs_card etc

   {Switch to High Speed if supported}
   Result:=SDDeviceSwitchHighspeed(MMC);
   if Result = MMC_STATUS_SUCCESS then
    begin
     {Set Timing}
     Result:=MMCDeviceSetTiming(MMC,MMC_TIMING_SD_HS);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end
   else if Result <> MMC_STATUS_UNSUPPORTED_REQUEST then
    begin
     Exit;
    end;

   {Set Clock}
   Result:=MMCDeviceSetClock(MMC,SDGetMaxClock(MMC));
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Switch to 4 bit bus if supported}
   if ((SDHCI.Capabilities and MMC_CAP_4_BIT_DATA) <> 0) and ((MMC.SDConfigurationData.BusWidths and SD_SCR_BUS_WIDTH_4) <> 0) then
    begin
     Result:=SDDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_4);
     if Result <> MMC_STATUS_SUCCESS then Exit;

     Result:=MMCDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_4);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Update Device}
   if not SDHCIHasCMD23(SDHCI) then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_SET_BLOCK_COUNT);

   {Update Storage}
   {Device}
   MMC.Storage.Device.DeviceBus:=DEVICE_BUS_SD;
   MMC.Storage.Device.DeviceFlags:=MMC.Storage.Device.DeviceFlags and not(STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA);
   if (MMC.Device.DeviceFlags and MMC_FLAG_WRITE_PROTECT) <> 0 then MMC.Storage.Device.DeviceFlags:=MMC.Storage.Device.DeviceFlags or STORAGE_FLAG_READ_ONLY;
   {Storage}
   {MMC.Storage.StorageState:=STORAGE_STATE_INSERTED;} {Handled by caller during notification}
   {Driver}
   MMC.Storage.BlockSize:=MMC.CardSpecificData.BlockSize;
   MMC.Storage.BlockCount:=MMC.CardSpecificData.BlockCount;
   MMC.Storage.BlockShift:=MMC.CardSpecificData.BlockShift;

   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 //See: mmc_sd_init_card in \drivers\mmc\core\sd.c
 //     mmc_attach_sd in \drivers\mmc\core\sd.c
 //     mmc_sd_setup_card in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function MMCDeviceInitializeMMC(MMC:PMMCDevice):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Initialize MMC');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check for an MMC Card}
 if MMCDeviceSendOperationCondition(MMC,True) = MMC_STATUS_SUCCESS then
  begin
   {MMC Card}
   MMC.MMCState:=MMC_STATE_INSERTED;
   MMC.Device.DeviceBus:=DEVICE_BUS_MMC;
   MMC.Device.DeviceType:=MMC_TYPE_MMC;
   MMC.RelativeCardAddress:=1;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Initialize Card Type is MMC');
   {$ENDIF}

   {Check for SPI}
   if SDHCIIsSPI(SDHCI) then
    begin
     {Set the Card to Idle State}
     MMCDeviceGoIdle(MMC);

     {Read the Operation Condition}
     Result:=MMCDeviceSPIReadOperationCondition(MMC,False);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Set the Card to Idle State}
   MMCDeviceGoIdle(MMC);

   {Set the High Capacity bit in the Operation Conditions}
   MMC.OperationCondition:=MMC.OperationCondition or MMC_OCR_HCS;

   {Get the Operation Condition}
   Result:=MMCDeviceSendOperationCondition(MMC,False);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Check for SPI}
   if SDHCIIsSPI(SDHCI) then
    begin
     {Enable SPI CRC}
     Result:=MMCDeviceSPISetCRC(MMC,True);
     if Result <> MMC_STATUS_SUCCESS then Exit;

     {Get Card Identification}
     Result:=MMCDeviceSendCardIdentification(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end
   else
    begin
     {Get Card Identification}
     Result:=MMCDeviceSendAllCardIdentification(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;

     {Set Relative Address}
     Result:=MMCDeviceSetRelativeAddress(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Get Card Specific}
   Result:=MMCDeviceSendCardSpecific(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Decode Card Specific (Must be before CID)}
   Result:=MMCDeviceDecodeCardSpecific(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Decode Card Identification}
   Result:=MMCDeviceDecodeCardIdentification(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Set Driver Stage}
   if MMC.CardSpecificData.DSRImplemented and (SDHCI.DriverStageRegister <> 0) then
    begin
     MMCDeviceSetDriverStage(MMC,SDHCI.DriverStageRegister);
    end;

   {Check for SPI}
   if not(SDHCIIsSPI(SDHCI)) then
    begin
     {Select Card}
     Result:=MMCDeviceSelectCard(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;
    end;

   {Get Extended Card Specific}
   Result:=MMCDeviceSendExtendedCardSpecific(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Decode Extended Card Specific}
   Result:=MMCDeviceDecodeExtendedCardSpecific(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Check for Block Addressing}
   if (MMC.OperationCondition and MMC_OCR_HCS) <> 0 then
    begin
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_BLOCK_ADDRESSED);
    end;

   {Set Erase Size}
   MMCDeviceSetEraseSize(MMC);

   {Enable ERASE_GRP_DEF}
   if MMC.ExtendedCardSpecificData.Revision >= 3 then
    begin
     Result:=MMCDeviceSwitch(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_ERASE_GROUP_DEF,1,MMC.ExtendedCardSpecificData.GenericCMD6Time);
     if (Result <> MMC_STATUS_SUCCESS) and (Result <> MMC_STATUS_INVALID_DATA) then Exit;

     if Result = MMC_STATUS_INVALID_DATA then
      begin
       Result:=MMC_STATUS_SUCCESS;

       {Disable Enhanced Area Offset and Size}
       MMC.ExtendedCardSpecificData.EnhancedAreaOffset:=0;
       MMC.ExtendedCardSpecificData.EnhancedAreaSize:=0;
      end
     else
      begin
       MMC.ExtendedCardSpecificData.EraseGroupDef:=$01;

       {Recalculate Erase Size}
       MMCDeviceSetEraseSize(MMC);
      end;
    end;

   {Ensure eMMC user default partition is enabled}
   if (MMC.ExtendedCardSpecificData.PartConfig and EXT_CSD_PART_CONFIG_ACC_MASK) <> 0 then
    begin
     MMC.ExtendedCardSpecificData.PartConfig:=MMC.ExtendedCardSpecificData.PartConfig and not(EXT_CSD_PART_CONFIG_ACC_MASK);

     Result:=MMCDeviceSwitch(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_PART_CONFIG,MMC.ExtendedCardSpecificData.PartConfig,MMC.ExtendedCardSpecificData.PartitionSwitchTime);
     if (Result <> MMC_STATUS_SUCCESS) and (Result <> MMC_STATUS_INVALID_DATA) then Exit;
    end;

   {Enable power_off_notification byte}
   if MMC.ExtendedCardSpecificData.Revision >= 6 then
    begin
     Result:=MMCDeviceSwitch(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_POWER_OFF_NOTIFICATION,EXT_CSD_POWER_ON,MMC.ExtendedCardSpecificData.GenericCMD6Time);
     if (Result <> MMC_STATUS_SUCCESS) and (Result <> MMC_STATUS_INVALID_DATA) then Exit;

     if Result = MMC_STATUS_SUCCESS then MMC.ExtendedCardSpecificData.PowerOffNotification:=EXT_CSD_POWER_ON;
    end;

   {Set Erase Argument}
   if (MMC.ExtendedCardSpecificData.FeatureSupport and MMC_DISCARD_FEATURE) <> 0 then
    begin
     MMC.EraseArgument:=MMC_DISCARD_ARG;
    end
   else if (MMC.ExtendedCardSpecificData.SecFeatureSupport and EXT_CSD_SEC_GB_CL_EN) <> 0 then
    begin
     MMC.EraseArgument:=MMC_TRIM_ARG;
    end
   else
    begin
     MMC.EraseArgument:=MMC_ERASE_ARG;
    end;

   {Select Timing}
   Result:=MMCDeviceSelectTiming(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   if MMC.Timing = MMC_TIMING_MMC_HS200 then
    begin
     {Not currently supported}
    end
   else if not(MMC.EnhancedStrobe) then
    begin
     {Select the Bus Width}
     Result:=MMCDeviceSelectBusWidth(MMC);
     if Result <> MMC_STATUS_SUCCESS then Exit;

     if (MMC.BusWidth > MMC_BUS_WIDTH_1) and ((MMC.Timing = MMC_TIMING_SD_HS) or (MMC.Timing = MMC_TIMING_MMC_HS)) then
      begin
       Result:=MMCDeviceSelectHSDDR(MMC);
       if Result <> MMC_STATUS_SUCCESS then Exit;
      end;
    end;

   {Select Power Class}
   Result:=MMCDeviceSelectPowerClass(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Enable HPI Feature}
   {Not currently supported}

   {Enable Cache if present}
   if MMC.ExtendedCardSpecificData.CacheSize > 0 then
    begin
     Result:=MMCDeviceSwitch(MMC,EXT_CSD_CMD_SET_NORMAL,EXT_CSD_CACHE_CTRL,1,Max(MMC.ExtendedCardSpecificData.GenericCMD6Time,MMC_MIN_CACHE_EN_TIMEOUT_MS));
     if (Result <> MMC_STATUS_SUCCESS) and (Result <> MMC_STATUS_INVALID_DATA) then Exit;

     if Result = MMC_STATUS_SUCCESS then MMC.ExtendedCardSpecificData.CacheControl:=1 else MMC.ExtendedCardSpecificData.CacheControl:=0;
    end;

   {Enable Command Queue}
   {Not currently supported}

   {Update Device}
   if not SDHCIHasCMD23(SDHCI) then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_SET_BLOCK_COUNT);

   {Update Storage}
   MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
   MMC.Storage.Device.DeviceFlags:=MMC.Storage.Device.DeviceFlags and not(STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA);
   if (MMC.Device.DeviceFlags and MMC_FLAG_WRITE_PROTECT) <> 0 then MMC.Storage.Device.DeviceFlags:=MMC.Storage.Device.DeviceFlags or STORAGE_FLAG_READ_ONLY;
   {Storage}
   {MMC.Storage.StorageState:=STORAGE_STATE_INSERTED;} {Handled by caller during notification}
   {Driver}
   if MMCHasExtendedCSD(MMC) then
    begin
     MMC.Storage.BlockSize:=MMC.ExtendedCardSpecificData.DataSectorSize;
     MMC.Storage.BlockCount:=MMC.ExtendedCardSpecificData.Sectors;
     MMC.Storage.BlockShift:=FirstBitSet(MMC.ExtendedCardSpecificData.DataSectorSize);
    end
   else
    begin
     MMC.Storage.BlockSize:=MMC.CardSpecificData.BlockSize;
     MMC.Storage.BlockCount:=MMC.CardSpecificData.BlockCount;
     MMC.Storage.BlockShift:=MMC.CardSpecificData.BlockShift;
    end;

   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 //See: mmc_init_card in \drivers\mmc\core\mmc.c
 //     mmc_attach_mmc in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function SDDeviceSetBusSpeed(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Set Bus Speed');
 {$ENDIF}

 //To Do //UHS-I and UHS-II Bus Speeds

 //See: sd_set_bus_speed_mode in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceSelectBusSpeed(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Select Bus Speed');
 {$ENDIF}

 //To Do //UHS-I and UHS-II Bus Speeds

 //See: sd_update_bus_speed_mode in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceSelectDriverType(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Select Driver Type');
 {$ENDIF}

 //To Do //Driver Types

 //See: sd_select_driver_type in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDIOCISParseTupleVERS1(MMC:PMMCDevice;Func:PSDIOFunction;Tuple:PSDIOTuple):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Tuple}
 if Tuple = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO CIS Parse Tuple CISTPL_VERS_1');
 {$ENDIF}

 {Check Function}
 if Func = nil then
  begin

   //To Do //TestingSDIO

  end
 else
  begin

   //To Do //TestingSDIO

  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: cistpl_vers_1 in \drivers\mmc\core\sdio_cis.c
end;

{==============================================================================}

function SDIOCISParseTupleMANFID(MMC:PMMCDevice;Func:PSDIOFunction;Tuple:PSDIOTuple):LongWord;
var
  Vendor:Word;
  Device:Word;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Tuple}
 if Tuple = nil then Exit;

 {Check Size}
 if Tuple.Size < 4 then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO CIS Parse Tuple CISTPL_MANFID');
 {$ENDIF}

 {Get Vendor (TPLMID_MANF)}
 Vendor:=Tuple.Data[0] or (Tuple.Data[1] shl 8);

 {Get Device (TPLMID_CARD)}
 Device:=Tuple.Data[2] or (Tuple.Data[3] shl 8);

 {Check Function}
 if Func = nil then
  begin
   {Common CIS}
   MMC.CIS.Vendor:=Vendor;
   MMC.CIS.Device:=Device;
  end
 else
  begin
   {Function CIS}
   Func.VendorId:=Vendor;
   Func.DeviceId:=Device;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: cistpl_manfid in \drivers\mmc\core\sdio_cis.c
end;

{==============================================================================}

function SDIOCISParseTupleFUNCE(MMC:PMMCDevice;Func:PSDIOFunction;Tuple:PSDIOTuple):LongWord;

const
 FUNCE_SPEED_VALUES:array[0..15] of Byte = (0, 10, 12, 13, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 70, 80);
 FUNCE_SPEED_UNITS:array[0..7] of LongWord = (10000, 100000, 1000000, 10000000, 0, 0, 0, 0);

var
 Version:Byte;
 MinSize:Byte;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Tuple}
 if Tuple = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO CIS Parse Tuple CISTPL_FUNCE');
 {$ENDIF}

 {Check First Byte}
 if Tuple.Data[0] = 0 then
  begin
   {Check Function}
   if Func <> nil then Exit;

   {Check Size}
   if Tuple.Size < 4 then Exit;

   {Get Block Size (TPLFE_FN0_BLK_SIZE)}
   MMC.CIS.BlockSize:=Tuple.Data[1] or (Tuple.Data[2] shl 8);

   {Get Max Clock (TPLFE_MAX_TRAN_SPEED)}
   MMC.CIS.MaxClock:=FUNCE_SPEED_VALUES[(Tuple.Data[3] shr 3) and 15] * FUNCE_SPEED_UNITS[Tuple.Data[3] and 7];
  end
 else if Tuple.Data[0] = 1 then
  begin
   {Check Function}
   if Func = nil then Exit;

   {Check Size (Depends on Version}
   Version:=MMC.CCCR.SDIOVersion;
   if Version = SDIO_SDIO_REV_1_00 then MinSize:=28 else MinSize:=42;

   if (Tuple.Size = 28) and (Version = SDIO_SDIO_REV_1_10) then
    begin
     {Force to Version 1.0}
     Version:=SDIO_SDIO_REV_1_00;
    end
   else if Tuple.Size < MinSize then
    begin
     Exit;
    end;

   {Get Max Block Size (TPLFE_MAX_BLK_SIZE)}
   Func.MaxBlockSize:=Tuple.Data[12] or (Tuple.Data[13] shl 8);

   {Get Enable Timeout (TPLFE_ENABLE_TIMEOUT_VAL)}
   if Version > SDIO_SDIO_REV_1_00 then
    begin
     Func.EnableTimeout:=(Tuple.Data[28] or (Tuple.Data[29] shl 8)) * 10;
    end
   else
    begin
     {Default to 1 second}
     Func.EnableTimeout:=1000;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: cistpl_funce_common in \drivers\mmc\core\sdio_cis.c
 //     cistpl_funce_func in \drivers\mmc\core\sdio_cis.c
end;

{==============================================================================}

function SDIOFunctionMaxByteSize(Func:PSDIOFunction):LongWord;
var
 Value:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=0;

 {Check Function}
 if Func = nil then Exit;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(Func.MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Get SDHCI Maximum}
 Value:=SDHCI.MaximumBlockSize;

 {Get Function Maximum}
 Value:=Min(Value,Func.MaxBlockSize);

 {Get Maximum}
 Result:=Min(Value,512);

 //See: sdio_max_byte_size in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDHCIHostGetPresetValue(SDHCI:PSDHCIHost):Word;
var
 Preset:Word;
begin
 {}
 Result:=0;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Get Preset Value');
 {$ENDIF}

 Preset:=0;

 {Get Preset}
 case SDHCI.Timing of
  MMC_TIMING_UHS_SDR12:Preset:=SDHCIHostReadWord(SDHCI,SDHCI_PRESET_FOR_SDR12);
  MMC_TIMING_UHS_SDR25:Preset:=SDHCIHostReadWord(SDHCI,SDHCI_PRESET_FOR_SDR25);
  MMC_TIMING_UHS_SDR50:Preset:=SDHCIHostReadWord(SDHCI,SDHCI_PRESET_FOR_SDR50);
  MMC_TIMING_UHS_SDR104,
  MMC_TIMING_MMC_HS200:Preset:=SDHCIHostReadWord(SDHCI,SDHCI_PRESET_FOR_SDR104);
  MMC_TIMING_UHS_DDR50,
  MMC_TIMING_MMC_DDR52:Preset:=SDHCIHostReadWord(SDHCI,SDHCI_PRESET_FOR_DDR50);
  MMC_TIMING_MMC_HS400:Preset:=SDHCIHostReadWord(SDHCI,SDHCI_PRESET_FOR_HS400);
 else
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'MMC has invalid UHS-I timing, defaulting preset to SDR12');

   Preset:=SDHCIHostReadWord(SDHCI,SDHCI_PRESET_FOR_SDR12);
  end;
 end;

 Result:=Preset;

 //See: sdhci_get_preset_value in sdhci.c
end;

{==============================================================================}

function SDHCIHostEnablePresetValue(SDHCI:PSDHCIHost;Enable:Boolean):LongWord;
var
 Control2:Word;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Enable Preset Value (Enable=' + BoolToStr(Enable,True) + ')');
 {$ENDIF}

 {Only valid for Host Controller v3.00 or above}
 if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
  begin
   if SDHCI.PresetEnabled <> Enable then
    begin
     {Enable or Disable preset value}
     Control2:=SDHCIHostReadWord(SDHCI,SDHCI_HOST_CONTROL2);

     if Enable then
      begin
       Control2:=Control2 or SDHCI_CTRL_PRESET_VAL_ENABLE;
      end
     else
      begin
       Control2:=Control2 and not(SDHCI_CTRL_PRESET_VAL_ENABLE);
      end;

     SDHCIHostWriteWord(SDHCI,SDHCI_HOST_CONTROL2,Control2);
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdhci_enable_preset_value in sdhci.c
end;

{==============================================================================}
{==============================================================================}
{MMC Functions}
function MMCDeviceReadBlocks(MMC:PMMCDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
var
 Status:LongWord;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Read Blocks (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_READ_SINGLE_BLOCK;
 if Count > 1 then Command.Command:=MMC_CMD_READ_MULTIPLE_BLOCK;
 Command.Argument:=Start;
 if (MMC.Device.DeviceFlags and MMC_FLAG_BLOCK_ADDRESSED) = 0 then Command.Argument:=(Command.Argument shl MMC.Storage.BlockShift);
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 Command.Data:=@Data;

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 Data.Data:=Buffer;
 Data.Flags:=MMC_DATA_READ;
 Data.BlockSize:=MMC.Storage.BlockSize;
 Data.BlockCount:=Count;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Command.Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_read_blocks in U-Boot mmc.c
end;

{==============================================================================}

function MMCDeviceWriteBlocks(MMC:PMMCDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
var
 Status:LongWord;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Write Blocks (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_WRITE_SINGLE_BLOCK;
 if Count > 1 then Command.Command:=MMC_CMD_WRITE_MULTIPLE_BLOCK;
 Command.Argument:=Start;
 if (MMC.Device.DeviceFlags and MMC_FLAG_BLOCK_ADDRESSED) = 0 then Command.Argument:=(Command.Argument shl MMC.Storage.BlockShift);
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 Command.Data:=@Data;

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 Data.Data:=Buffer;
 Data.Flags:=MMC_DATA_WRITE;
 Data.BlockSize:=MMC.Storage.BlockSize;
 Data.BlockCount:=Count;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Command.Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_write_blocks in U-Boot mmc_write.c
end;

{==============================================================================}

function MMCDeviceEraseBlocks(MMC:PMMCDevice;const Start,Count:Int64):LongWord;
var
 Status:LongWord;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Erase Blocks (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 //To Do

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 //To Do

 //See: mmc_erase_t in U-Boot mmc_write.c
end;

{==============================================================================}

function MMCDeviceGoIdle(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Go Idle');
 {$ENDIF}

 {Delay 1ms}
 MicrosecondDelay(1000);

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_GO_IDLE_STATE;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_NONE;
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Delay 2ms}
 MicrosecondDelay(2000);

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_go_idle in U-Boot mmc.c
 //     mmc_go_idle in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSetClock(MMC:PMMCDevice;Clock:LongWord):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Clock (Clock=' + IntToStr(Clock) + ')');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Clock}
 if Clock > SDHCI.MaximumFrequency then
  begin
   Clock:=SDHCI.MaximumFrequency;
  end;
 if Clock < SDHCI.MinimumFrequency then
  begin
   Clock:=SDHCI.MinimumFrequency;
  end;

 {Set Clock}
 MMC.Clock:=Clock;

 {Set IOS}
 Result:=MMCDeviceSetIOS(MMC);

 //See: mmc_set_clock in U-Boot mmc.c
 //See: mmc_set_clock in \drivers\mmc\core\core.c
end;

{==============================================================================}

function MMCDeviceSetTiming(MMC:PMMCDevice;Timing:LongWord):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Timing (Timing=' + MMCTimingToString(Timing) + ')');
 {$ENDIF}

 {Set Timing}
 MMC.Timing:=Timing;

 {Set IOS}
 Result:=MMCDeviceSetIOS(MMC);

 //See: mmc_set_timing in \drivers\mmc\core\core.c
end;

{==============================================================================}

function MMCDeviceSetBusWidth(MMC:PMMCDevice;Width:LongWord):LongWord;
{Reference: Section 3.4 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Bus Width (Width=' + MMCBusWidthToString(Width) + ')');
 {$ENDIF}

 {Set Bus Width}
 MMC.BusWidth:=Width;

 {Set IOS}
 Result:=MMCDeviceSetIOS(MMC);

 //See: mmc_set_bus_width in U-Boot mmc.c
 //See: mmc_set_bus_width in \drivers\mmc\core\core.c
end;

{==============================================================================}

function MMCDeviceSetBlockLength(MMC:PMMCDevice;Length:LongWord):LongWord;
var
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Block Length (Length=' + IntToStr(Length) + ')');
 {$ENDIF}

 {Check Block Address}
 if (MMC.Device.DeviceFlags and MMC_FLAG_BLOCK_ADDRESSED) = MMC_FLAG_BLOCK_ADDRESSED then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check DDR}
 if (MMC.Device.DeviceFlags and MMC_FLAG_DDR_MODE) = MMC_FLAG_DDR_MODE then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SET_BLOCKLEN;
 Command.Argument:=Length;
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 Command.Data:=nil;

 {Send Command}
 Result:=MMCDeviceSendCommand(MMC,@Command);

 //See: mmc_set_blocklen in U-Boot mmc.c
 //See: mmc_set_blocklen in \drivers\mmc\core\core.c
end;

{==============================================================================}

function MMCDeviceSetBlockCount(MMC:PMMCDevice;Count:LongWord;Relative:Boolean):LongWord;
var
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Block Count (Count=' + IntToStr(Count) + ' Relative=' + BoolToStr(Relative,True) + ')');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SET_BLOCK_COUNT;
 Command.Argument:=Count and $0000FFFF;
 if Relative then Command.Argument:=Command.Argument or (1 shl 31);
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 Command.Data:=nil;

 {Send Command}
 Result:=MMCDeviceSendCommand(MMC,@Command);

 //See: mmc_set_blockcount in \drivers\mmc\core\core.c
end;

{==============================================================================}

function MMCDeviceSetDriverStage(MMC:PMMCDevice;DriverStage:LongWord):LongWord;
var
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Driver Stage (DSR=' + IntToHex(DriverStage,8) + ')');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SET_DSR;
 Command.Argument:=(DriverStage shl 16) or $FFFF;
 Command.ResponseType:=MMC_RSP_NONE;
 Command.Data:=nil;

 {Send Command}
 Result:=MMCDeviceSendCommand(MMC,@Command);

 //See: mmc_set_dsr in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceStopTransmission(MMC:PMMCDevice):LongWord;
var
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Stop Transmission');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_STOP_TRANSMISSION;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_R1B;
 Command.Data:=nil;

 {Send Command}
 Result:=MMCDeviceSendCommand(MMC,@Command);
end;

{==============================================================================}

function MMCDeviceSelectCard(MMC:PMMCDevice):LongWord;
var
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Select Card');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SELECT_CARD;
 Command.Argument:=(MMC.RelativeCardAddress shl 16);
 Command.ResponseType:=MMC_RSP_R1;
 Command.Data:=nil;

 {Send Command}
 Result:=MMCDeviceSendCommand(MMC,@Command);

 //See: _mmc_select_card in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceDeselectCard(MMC:PMMCDevice):LongWord;
var
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Deselect Card');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SELECT_CARD;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_NONE;
 Command.Data:=nil;

 {Send Command}
 Result:=MMCDeviceSendCommand(MMC,@Command);

 //See: _mmc_select_card in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSwitch(MMC:PMMCDevice;Setting,Index,Value:Byte;Timeout:LongWord):LongWord;
{Modifies an Extended CSD register for the specificed MMC device}
{MMC: The MMC Device to modify}
{Setting: The Extended CSD command set (eg EXT_CSD_CMD_SET_NORMAL)}
{Index: The index of the Extended CSD register to be set}
{Value: The value to be set in the Extended CSD register}
{Timeout: Command timeout in milliseconds}
begin
 {}
 Result:=MMCDeviceSwitchEx(MMC,Setting,Index,Value,Timeout,MMC_TIMING_LEGACY,True,False);
end;

{==============================================================================}

function MMCDeviceSwitchEx(MMC:PMMCDevice;Setting,Index,Value:Byte;Timeout,Timing:LongWord;SendStatus,RetryCRCError:Boolean):LongWord;
{Modifies an Extended CSD register for the specificed MMC device}
{MMC: The MMC Device to modify}
{Setting: The Extended CSD command set (eg EXT_CSD_CMD_SET_NORMAL)}
{Index: The index of the Extended CSD register to be set}
{Value: The value to be set in the Extended CSD register}
{Timeout: Command timeout in milliseconds}
{Timing: New timing to enable after change (eg MMC_TIMING_MMC_HS)}
{SendStatus: Use the MMC_CMD_SEND_STATUS command to poll for busy}
{RetryCRCError: Retry if CRC error occurs when polling for busy}
var
 Status:LongWord;
 UseBusy:Boolean;
 OldTiming:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Switch');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Timeout}
 if Timeout = 0 then Timeout:=MMC.ExtendedCardSpecificData.GenericCMD6Time;

 {Check Busy}
 UseBusy:=True;
 if ((SDHCI.Capabilities and MMC_CAP_NEED_RSP_BUSY) = 0) and (SDHCI.MaximumBusyTimeout > 0) and (Timeout > SDHCI.MaximumBusyTimeout) then
  begin
   UseBusy:=False;
  end;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SWITCH;
 Command.Argument:=(MMC_SWITCH_MODE_WRITE_BYTE shl 24) or (Index shl 16) or (Value shl 8) or Setting;
 Command.Data:=nil;
 if UseBusy then
  begin
   Command.ResponseType:=MMC_RSP_SPI_R1B or MMC_RSP_R1B;
   Command.Timeout:=Timeout;
  end
 else
  begin
   Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
  end;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check for Polling}
 if not(SDHCIIsSPI(SDHCI)) and (((SDHCI.Capabilities and MMC_CAP_WAIT_WHILE_BUSY) = 0) or not(UseBusy)) then
  begin
   {Poll for command completion}
   Status:=MMCDevicePollForBusyEx(MMC,Timeout,MMC_BUSY_CMD6,SendStatus,RetryCRCError);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;
  end;

 {Check Timing}
 OldTiming:=MMC.Timing;
 if Timing > MMC_TIMING_LEGACY then MMCDeviceSetTiming(MMC,Timing);

 {Check Send Status}
 if SendStatus then
  begin
   Status:=MMCDeviceSendCardStatus(MMC);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     if Timing > MMC_TIMING_LEGACY then MMCDeviceSetTiming(MMC,OldTiming);

     Result:=Status;
     Exit;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_switch in U-Boot mmc.c
 //See: __mmc_switch in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDevicePollForBusy(MMC:PMMCDevice;Timeout,Command:LongWord):LongWord;
{Poll the specified MMC device for command completion using busy status}
begin
 {}
 Result:=MMCDevicePollForBusyEx(MMC,Timeout,Command,True,False);
end;

{==============================================================================}

function MMCDevicePollForBusyEx(MMC:PMMCDevice;Timeout,Command:LongWord;SendStatus,RetryCRCError:Boolean):LongWord;
{Poll the specified MMC device for command completion using busy status}
const
 R1_STATUS = $FFF9A000;
 R1_CURRENT_STATE = $00001E00;
 R1_READY_FOR_DATA = (1 shl 8);

 R1_STATE_TRAN = 4;

var
 Wait:Int64;
 Busy:Boolean;
 Expired:Boolean;
 Status:LongWord;
 Delay:LongWord;
 MaxDelay:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Poll Busy');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {If polling for busy is not supported or use of send status is not requested then simply wait for the specified timeout}
 if not(SendStatus) then
  begin
   MillisecondDelay(Timeout);

   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Calculate Timeout}
 Wait:=ClockGetTotal + (Timeout * CLOCK_CYCLES_PER_MILLISECOND);

 {Wait for not busy}
 Busy:=False;
 Delay:=32;
 MaxDelay:=32768;
 repeat
  {Check for timeout expired}
  Expired:=(ClockGetTotal > Wait);

  {Get Card Status}
  Status:=MMCDeviceSendCardStatus(MMC);
  if RetryCRCError and (Status = MMC_STATUS_INVALID_SEQUENCE) then
   begin
    Busy:=True;
   end
  else if Status <> MMC_STATUS_SUCCESS then
   begin
    Result:=Status;
    Exit;
   end
  else
   begin
    case Command of
     MMC_BUSY_CMD6:begin
       if SDHCIIsSPI(SDHCI) then
        begin
         if (MMC.CardStatus and MMC_RSP_R1_SPI_ILLEGAL_COMMAND) <> 0 then
          begin
           Result:=MMC_STATUS_INVALID_DATA;
           Exit;
          end;
        end
       else
        begin
         if (MMC.CardStatus and MMC_RSP_R1_SWITCH_ERROR) <> 0 then
          begin
           Result:=MMC_STATUS_INVALID_DATA;
           Exit;
          end;
        end;
      end;
     MMC_BUSY_ERASE:begin
       if (MMC.CardStatus and R1_STATUS) <> 0 then
        begin
         Result:=MMC_STATUS_HARDWARE_ERROR;
         Exit;
        end;
      end;
     MMC_BUSY_HPI:begin
       {Nothing}
      end;
     else
      begin
       Result:=MMC_STATUS_INVALID_PARAMETER;
       Exit;
      end;
    end;

    {Check Card Status}
    Busy:=not(((MMC.CardStatus and R1_READY_FOR_DATA) <> 0) and (((MMC.CardStatus and R1_CURRENT_STATE) shr 9) = R1_STATE_TRAN));
   end;

  {Timeout if the device is still busy}
  if Expired and Busy then
   begin
    Result:=MMC_STATUS_TIMEOUT;
    Exit;
   end;

  {Throttle the polling rate}
  if Busy then
   begin
    MicrosecondDelay(Delay);
    if Delay < MaxDelay then Delay:=Delay * 2;
   end;

 until not(Busy);

 Result:=MMC_STATUS_SUCCESS;

 //See: __mmc_poll_for_busy in \drivers\mmc\core\mmc_ops.c
 //     mmc_busy_status in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSendCardStatus(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Card Status');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SEND_STATUS;
 Command.Argument:=(MMC.RelativeCardAddress shl 16);
 Command.ResponseType:=MMC_RSP_SPI_R2 or MMC_RSP_R1;
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 MMC.CardStatus:=Command.Response[0];

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_send_status in U-Boot mmc.c
 //See: __mmc_send_status in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord;
{See: }
var
 Status:LongWord;
 Timeout:Integer;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Operation Condition');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SEND_OP_COND;
 Command.Argument:=0;
 if not(Probe) then
  begin
   if not(SDHCIIsSPI(SDHCI)) then
    begin
     Command.Argument:=MMC.OperationCondition;

     //To Do //Need to select a voltage that is compatible between the card and the host
             //Use SDHCI.Volatages to select from MMC.OperationCondition
             //See: mmc_select_voltage
    end;
  end;
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R4;
 Command.Data:=nil;

 {Setup Timeout}
 Timeout:=100;
 while Timeout > 0 do
  begin
   {Send Command}
   Status:=MMCDeviceSendCommand(MMC,@Command);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Single pass only on probe}
   if Probe then Break;

   {Wait until reset completes}
   if SDHCIIsSPI(SDHCI) then
    begin
     if (Command.Response[0] and MMC_RSP_R1_SPI_IDLE) = 0 then Break;
    end
   else
    begin
     if (Command.Response[0] and MMC_OCR_BUSY) <> 0 then Break;
    end;

   Dec(Timeout);
   if Timeout = 0 then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Operation Condition Busy Status Timeout');

     Result:=MMC_STATUS_TIMEOUT;
     Exit;
    end;
   MillisecondDelay(10);
  end;

 {Get Response}
 if not(SDHCIIsSPI(SDHCI)) then
  begin
   MMC.OperationCondition:=Command.Response[0];

   if not(Probe) and ((MMC.OperationCondition and MMC_OCR_HCS) = MMC_OCR_HCS) then
    begin
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_HIGH_CAPACITY);

     {$IFDEF MMC_DEBUG}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Operation Condition (Flags=MMC_FLAG_HIGH_CAPACITY)');
     {$ENDIF}
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_send_op_cond in U-Boot mmc.c
 //See: mmc_send_op_cond in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSendCardSpecific(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Card Specific');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check for SPI}
 if not(SDHCIIsSPI(SDHCI)) then
  begin
   {Native Mode}
   {Setup Command}
   FillChar(Command,SizeOf(TMMCCommand),0);
   Command.Command:=MMC_CMD_SEND_CSD;
   Command.Argument:=(MMC.RelativeCardAddress shl 16);
   Command.ResponseType:=MMC_RSP_R2; {Native Mode Only}
   Command.Data:=nil;

   {Send Command}
   Status:=MMCDeviceSendCommand(MMC,@Command);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Get Response}
   System.Move(Command.Response,MMC.CardSpecific,SizeOf(LongWord) * 4);

   Result:=MMC_STATUS_SUCCESS;
  end
 else
  begin
   {SPI Mode}
   {Setup Command}
   FillChar(Command,SizeOf(TMMCData),0);
   //To Do

   {Setup Data}
   FillChar(Data,SizeOf(TMMCCommand),0);
   //To Do

   {Send Command}
   //To Do //This is a Data Command in SPI

   //See: mmc_send_cxd_data in \drivers\mmc\core\mmc_ops.c
  end;

 //See: mmc_send_csd in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceDecodeCardSpecific(MMC:PMMCDevice):LongWord;
{Given a 128-bit response, decode to our card CSD structure}
var
 NSAC:Byte;
 TAACUnit:Byte;
 TAACValue:Byte;
 TranSpeedUnit:Byte;
 TranSpeedValue:Byte;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Decode Card Specific');
 {$ENDIF}

 {Decode Card Specific}
 MMC.CardSpecificData.CSDStructure:=MMCGetCSDValue(MMC,MMC_CSD_STRUCTURE);

 {Check Version}
 case MMC.CardSpecificData.CSDStructure of
  MMC_CSD_STRUCT_VER_1_0:begin
    {Not Supported}
   end;
  MMC_CSD_STRUCT_VER_1_1,MMC_CSD_STRUCT_VER_1_2,MMC_CSD_STRUCT_EXT_CSD:begin
    {Calculate Data Access Time}
    TAACUnit:=MMCGetCSDValue(MMC,MMC_CSD_TAAC_UNIT);
    TAACValue:=MMCGetCSDValue(MMC,MMC_CSD_TAAC_VALUE);
    MMC.CardSpecificData.DataAccessTime:=((MMC_CSD_TAAC_UNITS[TAACUnit] * MMC_CSD_TAAC_VALUES[TAACValue]) + 9) div 10;

    {Calculate Data Access Clocks}
    NSAC:=MMCGetCSDValue(MMC,MMC_CSD_NSAC);
    MMC.CardSpecificData.DataAccessClocks:=NSAC * 100;

    {Calculate Data Transfer Rate}
    TranSpeedUnit:=MMCGetCSDValue(MMC,MMC_CSD_TRAN_SPEED_UNIT);
    TranSpeedValue:=MMCGetCSDValue(MMC,MMC_CSD_TRAN_SPEED_VALUE);
    MMC.CardSpecificData.DataTransferRate:=MMC_CSD_TRAN_SPEED_UNITS[TranSpeedUnit] * MMC_CSD_TRAN_SPEED_VALUES[TranSpeedValue];

    {Get Card Data}
    MMC.CardSpecificData.SpecVersion:=MMCGetCSDValue(MMC,MMC_CSD_SPECVER);
    MMC.CardSpecificData.CCC:=MMCGetCSDValue(MMC,MMC_CSD_CCC);
    MMC.CardSpecificData.ReadBlockLength:=MMCGetCSDValue(MMC,MMC_CSD_READ_BL_LEN);
    MMC.CardSpecificData.ReadBlockPartial:=(MMCGetCSDValue(MMC,MMC_CSD_READ_BL_PARTIAL) = 1);
    MMC.CardSpecificData.WriteBlockMisalign:=(MMCGetCSDValue(MMC,MMC_CSD_WRITE_BLK_MISALIGN) = 1);
    MMC.CardSpecificData.ReadBlockMisalign:=(MMCGetCSDValue(MMC,MMC_CSD_READ_BLK_MISALIGN) = 1);
    MMC.CardSpecificData.DSRImplemented:=(MMCGetCSDValue(MMC,MMC_CSD_DSR_IMP) = 1);
    MMC.CardSpecificData.DeviceSize:=MMCGetCSDValue(MMC,MMC_CSD_C_SIZE);
    MMC.CardSpecificData.VDDReadCurrentMin:=MMCGetCSDValue(MMC,MMC_CSD_VDD_R_CURR_MIN);
    MMC.CardSpecificData.VDDReadCurrentMax:=MMCGetCSDValue(MMC,MMC_CSD_VDD_R_CURR_MAX);
    MMC.CardSpecificData.VDDWriteCurrentMin:=MMCGetCSDValue(MMC,MMC_CSD_VDD_W_CURR_MIN);
    MMC.CardSpecificData.VDDWriteCurrentMax:=MMCGetCSDValue(MMC,MMC_CSD_VDD_W_CURR_MAX);
    MMC.CardSpecificData.DeviceSizeMultiplier:=MMCGetCSDValue(MMC,MMC_CSD_C_SIZE_MULT);
    MMC.CardSpecificData.Erase.MMC31.EraseGroupSize:=MMCGetCSDValue(MMC,MMC_CSD_ERASE_GRP_SIZE);
    MMC.CardSpecificData.Erase.MMC31.EraseGroupMultiplier:=MMCGetCSDValue(MMC,MMC_CSD_ERASE_GRP_MULT);
    MMC.CardSpecificData.WriteProtectGroupSize:=MMCGetCSDValue(MMC,MMC_CSD_WP_GRP_SIZE);
    MMC.CardSpecificData.WriteProtectGroupEnable:=(MMCGetCSDValue(MMC,MMC_CSD_WP_GRP_ENABLE) = 1);
    MMC.CardSpecificData.DefaultECC:=MMCGetCSDValue(MMC,MMC_CSD_DEFAULT_ECC);
    MMC.CardSpecificData.ReadToWriteFactor:=MMCGetCSDValue(MMC,MMC_CSD_R2W_FACTOR);
    MMC.CardSpecificData.WriteBlockLength:=MMCGetCSDValue(MMC,MMC_CSD_WRITE_BL_LEN);
    MMC.CardSpecificData.WriteBlockPartial:=(MMCGetCSDValue(MMC,MMC_CSD_WRITE_BL_PARTIAL) = 1);
    MMC.CardSpecificData.ContentProtectApplication:=(MMCGetCSDValue(MMC,MMC_CSD_CONTENT_PROT_APP) = 1);
    MMC.CardSpecificData.FileFormatGroup:=MMCGetCSDValue(MMC,MMC_CSD_FILE_FORMAT_GRP);
    MMC.CardSpecificData.CopyFlag:=(MMCGetCSDValue(MMC,MMC_CSD_COPY) = 1);
    MMC.CardSpecificData.PermanentWriteProtect:=(MMCGetCSDValue(MMC,MMC_CSD_PERM_WRITE_PROTECT) = 1);
    MMC.CardSpecificData.TemporaryWriteProtect:=(MMCGetCSDValue(MMC,MMC_CSD_TMP_WRITE_PROTECT) = 1);
    MMC.CardSpecificData.FileFormat:=MMCGetCSDValue(MMC,MMC_CSD_FILE_FORMAT);
    MMC.CardSpecificData.ECC:=MMCGetCSDValue(MMC,MMC_CSD_ECC);
    MMC.CardSpecificData.CRC:=MMCGetCSDValue(MMC,MMC_CSD_CRC);

    {Get Version}
    if MMC.CardSpecificData.CSDStructure <> MMC_CSD_STRUCT_EXT_CSD then
     begin
      if MMC.CardSpecificData.SpecVersion = MMC_CSD_SPEC_VER_0 then
       begin
        MMC.Version:=MMC_VERSION_1_2;
       end
      else if MMC.CardSpecificData.SpecVersion = MMC_CSD_SPEC_VER_1 then
       begin
        MMC.Version:=MMC_VERSION_1_4;
       end
      else if MMC.CardSpecificData.SpecVersion = MMC_CSD_SPEC_VER_2 then
       begin
        MMC.Version:=MMC_VERSION_2_2;
       end
      else if MMC.CardSpecificData.SpecVersion = MMC_CSD_SPEC_VER_3 then
       begin
        MMC.Version:=MMC_VERSION_3;
       end
      else if MMC.CardSpecificData.SpecVersion = MMC_CSD_SPEC_VER_4 then
       begin
        MMC.Version:=MMC_VERSION_4;
       end
      {Higher versions are encoded in the Extended CSD}
     end;

    {Check CMD23 Support}
    if MMC.CardSpecificData.SpecVersion >= MMC_CSD_SPEC_VER_3 then
     begin
      MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_SET_BLOCK_COUNT;
     end;

    {Calculate Block Size}
    MMC.CardSpecificData.BlockSize:=(1 shl MMC.CardSpecificData.ReadBlockLength);

    {Calculate Block Count}
    MMC.CardSpecificData.BlockCount:=(MMC.CardSpecificData.DeviceSize + 1) shl (MMC.CardSpecificData.DeviceSizeMultiplier + 2);

    {Calculate Block Shift}
    MMC.CardSpecificData.BlockShift:=MMC.CardSpecificData.ReadBlockLength;

    {Calculate Erase Size}
    if MMC.CardSpecificData.WriteBlockLength >= 9 then
     begin
      MMC.CardSpecificData.EraseSize:=(MMC.CardSpecificData.Erase.MMC31.EraseGroupSize + 1) * (MMC.CardSpecificData.Erase.MMC31.EraseGroupMultiplier + 1);
      MMC.CardSpecificData.EraseSize:=MMC.CardSpecificData.EraseSize shl (MMC.CardSpecificData.WriteBlockLength - 9);
     end;

    Result:=MMC_STATUS_SUCCESS;
   end;
 end;

 {Log Card Specific}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' MMC Card Specific:');
   MMCLogDebug(nil,'  CSDStructure = ' + IntToStr(MMC.CardSpecificData.CSDStructure));
   if Result = MMC_STATUS_SUCCESS then
    begin
     {Card Data}
     MMCLogDebug(nil,'  SpecVersion = ' + IntToStr(MMC.CardSpecificData.SpecVersion));
     MMCLogDebug(nil,'  CCC = ' + IntToHex(MMC.CardSpecificData.CCC,8));
     MMCLogDebug(nil,'  ReadBlockLength = ' + IntToStr(MMC.CardSpecificData.ReadBlockLength));
     MMCLogDebug(nil,'  ReadBlockPartial = ' + BoolToStr(MMC.CardSpecificData.ReadBlockPartial,True));
     MMCLogDebug(nil,'  WriteBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.WriteBlockMisalign,True));
     MMCLogDebug(nil,'  ReadBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.ReadBlockMisalign,True));
     MMCLogDebug(nil,'  DSRImplemented = ' + BoolToStr(MMC.CardSpecificData.DSRImplemented,True));
     MMCLogDebug(nil,'  DeviceSize = ' + IntToStr(MMC.CardSpecificData.DeviceSize));
     MMCLogDebug(nil,'  VDDReadCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMin));
     MMCLogDebug(nil,'  VDDReadCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMax));
     MMCLogDebug(nil,'  VDDWriteCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMin));
     MMCLogDebug(nil,'  VDDWriteCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMax));
     MMCLogDebug(nil,'  DeviceSizeMultiplier = ' + IntToStr(MMC.CardSpecificData.DeviceSizeMultiplier));
     MMCLogDebug(nil,'  EraseGroupSize = ' + IntToStr(MMC.CardSpecificData.Erase.MMC31.EraseGroupSize));
     MMCLogDebug(nil,'  EraseGroupMultiplier = ' + IntToStr(MMC.CardSpecificData.Erase.MMC31.EraseGroupMultiplier));
     MMCLogDebug(nil,'  WriteProtectGroupSize = ' + IntToStr(MMC.CardSpecificData.WriteProtectGroupSize));
     MMCLogDebug(nil,'  WriteProtectGroupEnable = ' + BoolToStr(MMC.CardSpecificData.WriteProtectGroupEnable,True));
     MMCLogDebug(nil,'  DefaultECC = ' + IntToStr(MMC.CardSpecificData.DefaultECC));
     MMCLogDebug(nil,'  ReadToWriteFactor = ' + IntToStr(MMC.CardSpecificData.ReadToWriteFactor));
     MMCLogDebug(nil,'  WriteBlockLength = ' + IntToStr(MMC.CardSpecificData.WriteBlockLength));
     MMCLogDebug(nil,'  WriteBlockPartial = ' + BoolToStr(MMC.CardSpecificData.WriteBlockPartial,True));
     MMCLogDebug(nil,'  ContentProtectApplication = ' + BoolToStr(MMC.CardSpecificData.ContentProtectApplication,True));
     MMCLogDebug(nil,'  FileFormatGroup = ' + IntToStr(MMC.CardSpecificData.FileFormatGroup));
     MMCLogDebug(nil,'  CopyFlag = ' + BoolToStr(MMC.CardSpecificData.CopyFlag,True));
     MMCLogDebug(nil,'  PermanentWriteProtect = ' + BoolToStr(MMC.CardSpecificData.PermanentWriteProtect,True));
     MMCLogDebug(nil,'  TemporaryWriteProtect = ' + BoolToStr(MMC.CardSpecificData.TemporaryWriteProtect,True));
     MMCLogDebug(nil,'  FileFormat = ' + IntToStr(MMC.CardSpecificData.FileFormat));
     MMCLogDebug(nil,'  ECC = ' + IntToStr(MMC.CardSpecificData.ECC));
     MMCLogDebug(nil,'  CRC = ' + IntToStr(MMC.CardSpecificData.CRC));
     {Calculated Values}
     MMCLogDebug(nil,'  DataAccessTime = ' + IntToStr(MMC.CardSpecificData.DataAccessTime));
     MMCLogDebug(nil,'  DataAccessClocks = ' + IntToStr(MMC.CardSpecificData.DataAccessClocks));
     MMCLogDebug(nil,'  DataTransferRate = ' + IntToStr(MMC.CardSpecificData.DataTransferRate));
     MMCLogDebug(nil,'  EraseSize = ' + IntToStr(MMC.CardSpecificData.EraseSize));
     MMCLogDebug(nil,'  BlockSize = ' + IntToStr(MMC.CardSpecificData.BlockSize));
     MMCLogDebug(nil,'  BlockCount = ' + IntToStr(MMC.CardSpecificData.BlockCount));
     MMCLogDebug(nil,'  BlockShift = ' + IntToStr(MMC.CardSpecificData.BlockShift));
     MMCLogDebug(nil,'  Version = ' + MMCVersionToString(MMC.Version));
     MMCLogDebug(nil,'  CMD23Support = ' + BoolToStr((MMC.Device.DeviceFlags and MMC_FLAG_SET_BLOCK_COUNT) <> 0,True));
    end;
  end;
 {$ENDIF}

 //See: mmc_decode_csd in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSendCardIdentification(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Card Identification');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check for SPI}
 if not(SDHCIIsSPI(SDHCI)) then
  begin
   {Native Mode}
   {Setup Command}
   FillChar(Command,SizeOf(TMMCCommand),0);
   Command.Command:=MMC_CMD_SEND_CID;
   Command.Argument:=(MMC.RelativeCardAddress shl 16);
   Command.ResponseType:=MMC_RSP_R2; {Native Mode Only}
   Command.Data:=nil;

   {Send Command}
   Status:=MMCDeviceSendCommand(MMC,@Command);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Get Response}
   System.Move(Command.Response,MMC.CardIdentification,SizeOf(LongWord) * 4);

   Result:=MMC_STATUS_SUCCESS;
  end
 else
  begin
   {SPI Mode}
   {Setup Command}
   FillChar(Command,SizeOf(TMMCCommand),0);
   //To Do

   {Setup Data}
   FillChar(Data,SizeOf(TMMCCommand),0);
   //To Do

   {Send Command}
   //To Do //This is a Data Command in SPI

   //See: mmc_send_cxd_data in \drivers\mmc\core\mmc_ops.c
  end;

 //See: mmc_startup in U-Boot mmc.c
 //See: mmc_send_cid in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSendAllCardIdentification(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send All Card Identification');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_ALL_SEND_CID;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_R2; {Not valid in SPI mode}
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 System.Move(Command.Response,MMC.CardIdentification,SizeOf(LongWord) * 4);

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_startup in U-Boot mmc.c
 //See: mmc_all_send_cid in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceDecodeCardIdentification(MMC:PMMCDevice):LongWord;
{Given a 128-bit response, decode to our card CID structure}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Decode Card Identification');
 {$ENDIF}

 {Check Version}
 case MMC.CardSpecificData.SpecVersion of
  MMC_CSD_SPEC_VER_0,MMC_CSD_SPEC_VER_1:begin
    {MMC v1.0 - v1.2 / MMC v1.4}
    {Decode Card Identification}
    MMC.CardIdentificationData.ManufacturerId:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_MID);
    MMC.CardIdentificationData.ProductName[0]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM0));
    MMC.CardIdentificationData.ProductName[1]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM1));
    MMC.CardIdentificationData.ProductName[2]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM2));
    MMC.CardIdentificationData.ProductName[3]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM3));
    MMC.CardIdentificationData.ProductName[4]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM4));
    MMC.CardIdentificationData.ProductName[5]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM5));
    MMC.CardIdentificationData.ProductName[6]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM6));
    MMC.CardIdentificationData.ProductName[7]:=#0; {Null terminator}
    MMC.CardIdentificationData.ProductRevision:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PRV);
    MMC.CardIdentificationData.HardwareRevision:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_HRV);
    MMC.CardIdentificationData.FirmwareRevision:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_FRV);
    MMC.CardIdentificationData.ProductSerialNumber:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PSN);
    MMC.CardIdentificationData.ManufacturingMonth:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_MDT_MONTH);
    MMC.CardIdentificationData.ManufacturingYear:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_MDT_YEAR);
    MMC.CardIdentificationData.CRC:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_CRC);

    Result:=MMC_STATUS_SUCCESS;
   end;
  MMC_CSD_SPEC_VER_2,MMC_CSD_SPEC_VER_3,MMC_CSD_SPEC_VER_4:begin
    {MMC v2.0 - v2.2 / MMC v3.1 - v3.3 / MMC v4}
    {Decode Card Identification}
    MMC.CardIdentificationData.ManufacturerId:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_MID);
    MMC.CardIdentificationData.OEMId:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_OID);
    MMC.CardIdentificationData.ProductName[0]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM0));
    MMC.CardIdentificationData.ProductName[1]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM1));
    MMC.CardIdentificationData.ProductName[2]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM2));
    MMC.CardIdentificationData.ProductName[3]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM3));
    MMC.CardIdentificationData.ProductName[4]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM4));
    MMC.CardIdentificationData.ProductName[5]:=Char(MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PNM5));
    MMC.CardIdentificationData.ProductName[6]:=#0; {Null terminator}
    MMC.CardIdentificationData.ProductRevision:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PRV);
    MMC.CardIdentificationData.HardwareRevision:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_HRV);
    MMC.CardIdentificationData.FirmwareRevision:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_FRV);
    MMC.CardIdentificationData.ProductSerialNumber:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_PSN);
    MMC.CardIdentificationData.ManufacturingMonth:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_MDT_MONTH);
    MMC.CardIdentificationData.ManufacturingYear:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_MDT_YEAR);
    MMC.CardIdentificationData.CRC:=MMCGetCIDValue(MMC,MMC.CardSpecificData.SpecVersion,MMC_CID_CRC);

    Result:=MMC_STATUS_SUCCESS;
   end;
 end;

 {Log Card Identification}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' MMC Card Identification:');
   if Result = MMC_STATUS_SUCCESS then
    begin
     MMCLogDebug(nil,'  ManufacturerId = ' + IntToStr(MMC.CardIdentificationData.ManufacturerId));
     MMCLogDebug(nil,'  OEMId = ' + IntToStr(MMC.CardIdentificationData.OEMId));
     MMCLogDebug(nil,'  ProductName = ' + MMC.CardIdentificationData.ProductName);
     MMCLogDebug(nil,'  ProductRevision = ' + IntToStr(MMC.CardIdentificationData.ProductRevision));
     MMCLogDebug(nil,'  HardwareRevision = ' + IntToStr(MMC.CardIdentificationData.HardwareRevision));
     MMCLogDebug(nil,'  FirmwareRevision = ' + IntToStr(MMC.CardIdentificationData.FirmwareRevision));
     MMCLogDebug(nil,'  ProductSerialNumber = ' + IntToStr(MMC.CardIdentificationData.ProductSerialNumber));
     MMCLogDebug(nil,'  ManufacturingMonth = ' + IntToStr(MMC.CardIdentificationData.ManufacturingMonth));
     MMCLogDebug(nil,'  ManufacturingYear = ' + IntToStr(MMC.CardIdentificationData.ManufacturingYear));
     MMCLogDebug(nil,'  CRC = ' + IntToStr(MMC.CardIdentificationData.CRC));
    end;
  end;
 {$ENDIF}

 //See: mmc_decode_cid in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceGetExtendedCardSpecific(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Extended Card Specific');
 {$ENDIF}

 {Check Extended CSD}
 if MMCHasExtendedCSD(MMC) then
  begin
   {Get Extended Card Specific}
   Result:=MMCDeviceSendExtendedCardSpecific(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;

   {Decode Extended Card Specific}
   Result:=MMCDeviceDecodeExtendedCardSpecific(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_read_ext_csd in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSendExtendedCardSpecific(MMC:PMMCDevice):LongWord;
const
 EXTENDED_CSD_SIZE = 512;

var
 SDHCI:PSDHCIHost;
 Status:LongWord;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Extended Card Specific');
 {$ENDIF}

 {Check Extended CSD}
 if not MMCHasExtendedCSD(MMC) then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Allocate Extended Card Specific}
 if MMC.ExtendedCardSpecific = nil then
  begin
   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   if SDHCIHasDMA(SDHCI) then
    begin
     MMC.ExtendedCardSpecific:=DMABufferAllocate(DMAHostGetDefault,EXTENDED_CSD_SIZE);

     {Check Cache}
     if not(DMA_CACHE_COHERENT) and (MMC.ExtendedCardSpecific <> nil) then
      begin
       {Clean Cache (Dest)}
       CleanDataCacheRange(PtrUInt(MMC.ExtendedCardSpecific),EXTENDED_CSD_SIZE);
      end;
    end
   else
    begin
     MMC.ExtendedCardSpecific:=AllocMem(EXTENDED_CSD_SIZE);
    end;

   if MMC.ExtendedCardSpecific = nil then
    begin
     Result:=MMC_STATUS_OUT_OF_MEMORY;
     Exit;
    end;
  end;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SEND_EXT_CSD;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 Command.Data:=@Data;

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 Data.Data:=MMC.ExtendedCardSpecific;
 Data.Flags:=MMC_DATA_READ;
 Data.BlockSize:=EXTENDED_CSD_SIZE;
 Data.BlockCount:=1;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Command.Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_get_ext_csd in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

procedure MMCDeviceSelectCardType(MMC:PMMCDevice);
begin
 {}
 {Check MMC}
 if MMC = nil then Exit;

 if ((MMC.Capabilities and MMC_CAP_MMC_HIGHSPEED) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_HS_26) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HSMaxRate:=MMC_HIGH_26_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_HS_26;
  end;
 if ((MMC.Capabilities and MMC_CAP_MMC_HIGHSPEED) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_HS_52) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HSMaxRate:=MMC_HIGH_52_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_HS_52;
  end;
 if ((MMC.Capabilities and (MMC_CAP_1_8V_DDR or MMC_CAP_3_3V_DDR)) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_DDR_1_8V) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HSMaxRate:=MMC_HIGH_DDR_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_DDR_1_8V;
  end;
 if ((MMC.Capabilities and MMC_CAP_1_2V_DDR) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_DDR_1_2V) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HSMaxRate:=MMC_HIGH_DDR_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_DDR_1_2V;
  end;

 if ((MMC.Capabilities2 and MMC_CAP2_HS200_1_8V_SDR) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_HS200_1_8V) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HS200MaxRate:=MMC_HS200_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_HS200_1_8V;
  end;
 if ((MMC.Capabilities2 and MMC_CAP2_HS200_1_2V_SDR) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_HS200_1_2V) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HS200MaxRate:=MMC_HS200_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_HS200_1_2V;
  end;
 if ((MMC.Capabilities2 and MMC_CAP2_HS400_1_8V) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_HS400_1_8V) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HS200MaxRate:=MMC_HS200_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_HS400_1_8V;
  end;
 if ((MMC.Capabilities2 and MMC_CAP2_HS400_1_2V) <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_HS400_1_2V) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.HS200MaxRate:=MMC_HS200_MAX_DTR;
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_HS400_1_2V;
  end;
 if ((MMC.Capabilities2 and MMC_CAP2_HS400_ES) <> 0) and (MMC.ExtendedCardSpecificData.StrobeSupport <> 0) and ((MMC.ExtendedCardSpecificData.CardType and EXT_CSD_CARD_TYPE_HS400) <> 0) then
  begin
   MMC.ExtendedCardSpecificData.AvailableTypes:=MMC.ExtendedCardSpecificData.AvailableTypes or EXT_CSD_CARD_TYPE_HS400ES;
  end;
end;


{==============================================================================}

function MMCDeviceDecodeExtendedCardSpecific(MMC:PMMCDevice):LongWord;
var
 Index:LongWord;
 HCEraseGroupSize:Byte;
 HCWriteProtectGroupSize:Byte;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Decode Extended Card Specific');
 {$ENDIF}

 {Check Extended CSD}
 if not MMCHasExtendedCSD(MMC) then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check Extended Card Specific}
 if MMC.ExtendedCardSpecific = nil then Exit;

 {Decode Extended Card Specific}
 MMC.ExtendedCardSpecificData.CSDStructure:=MMC.ExtendedCardSpecific[EXT_CSD_STRUCTURE];

 {Check Version}
 case MMC.ExtendedCardSpecificData.CSDStructure of
  MMC_CSD_STRUCT_VER_1_0:begin
    {Not Supported}
    if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Extended CSD structure version not supported');
   end;
  MMC_CSD_STRUCT_VER_1_1,MMC_CSD_STRUCT_VER_1_2:begin
    {Get Card Data}
    MMC.ExtendedCardSpecificData.Revision:=MMC.ExtendedCardSpecific[EXT_CSD_REV];

    MMC.ExtendedCardSpecificData.SectorCount[0]:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 0];
    MMC.ExtendedCardSpecificData.SectorCount[1]:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 1];
    MMC.ExtendedCardSpecificData.SectorCount[2]:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 2];
    MMC.ExtendedCardSpecificData.SectorCount[3]:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 3];

    if MMC.ExtendedCardSpecificData.Revision >= 2 then
     begin
      MMC.ExtendedCardSpecificData.Sectors:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 0] shl 0
                                         or MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 1] shl 8
                                         or MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 2] shl 16
                                         or MMC.ExtendedCardSpecific[EXT_CSD_SEC_CNT + 3] shl 24;

      {Cards with density > 2GB are sector addressed}
      if MMC.ExtendedCardSpecificData.Sectors > ((2 * 1024 * 1024 * 1024) div 512) then
       begin
        MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_BLOCK_ADDRESSED);

        {$IFDEF MMC_DEBUG}
        if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Decode Extended Card Specific (Flags=MMC_FLAG_BLOCK_ADDRESSED)');
        {$ENDIF}
       end;
     end;

    MMC.ExtendedCardSpecificData.StrobeSupport:=MMC.ExtendedCardSpecific[EXT_CSD_STROBE_SUPPORT];
    MMC.ExtendedCardSpecificData.CardType:=MMC.ExtendedCardSpecific[EXT_CSD_CARD_TYPE];

    MMCDeviceSelectCardType(MMC);

    MMC.ExtendedCardSpecificData.SATimeout:=MMC.ExtendedCardSpecific[EXT_CSD_S_A_TIMEOUT];
    MMC.ExtendedCardSpecificData.EraseTimeoutMult:=MMC.ExtendedCardSpecific[EXT_CSD_ERASE_TIMEOUT_MULT];
    MMC.ExtendedCardSpecificData.HCEraseGrpSize:=MMC.ExtendedCardSpecific[EXT_CSD_HC_ERASE_GRP_SIZE];

    if MMC.ExtendedCardSpecificData.Revision >= 3 then
     begin
      MMC.ExtendedCardSpecificData.PartConfig:=MMC.ExtendedCardSpecific[EXT_CSD_PART_CONFIG];

      {Partition Switch Time value is in units of 10ms, store as ms}
      MMC.ExtendedCardSpecificData.PartitionSwitchTime:=10 * MMC.ExtendedCardSpecific[EXT_CSD_PART_SWITCH_TIME];
      {Check the Partition Switch Time}
      if (MMC.ExtendedCardSpecificData.PartitionSwitchTime > 0) and (MMC.ExtendedCardSpecificData.PartitionSwitchTime < MMC_MIN_PART_SWITCH_TIME) then
       begin
        MMC.ExtendedCardSpecificData.PartitionSwitchTime:=MMC_MIN_PART_SWITCH_TIME;
       end;

      {Sleep / Awake timeout}
      if (MMC.ExtendedCardSpecificData.SATimeout > 0) and (MMC.ExtendedCardSpecificData.SATimeout < $17) then
       begin
        MMC.ExtendedCardSpecificData.SleepAwakeTime:=1 shl MMC.ExtendedCardSpecificData.SATimeout;
       end;

      MMC.ExtendedCardSpecificData.EraseGroupDef:=MMC.ExtendedCardSpecific[EXT_CSD_ERASE_GROUP_DEF];
      MMC.ExtendedCardSpecificData.HCEraseTimeout:=300 * MMC.ExtendedCardSpecific[EXT_CSD_ERASE_TIMEOUT_MULT];
      MMC.ExtendedCardSpecificData.HCEraseSize:=MMC.ExtendedCardSpecific[EXT_CSD_HC_ERASE_GRP_SIZE] shl 10;
      MMC.ExtendedCardSpecificData.ReliableSectors:=MMC.ExtendedCardSpecific[EXT_CSD_REL_WR_SEC_C];

      {Boot Partitions (There are two boot regions of equal size, defined in multiples of 128K)}
      if (MMC.ExtendedCardSpecific[EXT_CSD_BOOT_SIZE_MULT] > 0) and ((MMC.Capabilities2 and MMC_CAP2_BOOTPART_NOACC) = 0) then
       begin
        MMC.ExtendedCardSpecificData.BootPartitionSize:=MMC.ExtendedCardSpecific[EXT_CSD_BOOT_SIZE_MULT] shl 17;
       end;
     end;

    MMC.ExtendedCardSpecificData.HCEraseGapSize:=MMC.ExtendedCardSpecific[EXT_CSD_HC_WP_GRP_SIZE];
    MMC.ExtendedCardSpecificData.SecTRIMMult:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_TRIM_MULT];
    MMC.ExtendedCardSpecificData.SecEraseMult:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_ERASE_MULT];
    MMC.ExtendedCardSpecificData.SecFeatureSupport:=MMC.ExtendedCardSpecific[EXT_CSD_SEC_FEATURE_SUPPORT];
    MMC.ExtendedCardSpecificData.TRIMMult:=MMC.ExtendedCardSpecific[EXT_CSD_TRIM_MULT];
    MMC.ExtendedCardSpecificData.PartitionSupport:=MMC.ExtendedCardSpecific[EXT_CSD_PARTITION_SUPPORT];
    MMC.ExtendedCardSpecificData.DriverStrength:=MMC.ExtendedCardSpecific[EXT_CSD_DRIVER_STRENGTH];

    if MMC.ExtendedCardSpecificData.Revision >= 4 then
     begin
      MMC.ExtendedCardSpecificData.PartitionSettingCompleted:=False;
      if (MMC.ExtendedCardSpecific[EXT_CSD_PARTITION_SETTING_COMPLETED] and EXT_CSD_PART_SETTING_COMPLETED) <> 0 then
       begin
        MMC.ExtendedCardSpecificData.PartitionSettingCompleted:=True;
       end;

      {Enhanced area feature support - check whether the eMMC card has the Enhanced area enabled}
      if (MMC.ExtendedCardSpecificData.PartitionSupport and EXT_CSD_PARTITION_ENH_ATTRIBUTE_EN) <> 0 then
       begin
        if (MMC.ExtendedCardSpecific[EXT_CSD_PARTITION_ATTRIBUTE] and EXT_CSD_PARTITION_ATTRIBUTE_ENH_USR) <> 0 then
         begin
          if MMC.ExtendedCardSpecificData.PartitionSettingCompleted then
           begin
            HCEraseGroupSize:=MMC.ExtendedCardSpecific[EXT_CSD_HC_ERASE_GRP_SIZE];
            HCWriteProtectGroupSize:=MMC.ExtendedCardSpecific[EXT_CSD_HC_WP_GRP_SIZE];

            {Calculate the enhanced data area offset, in bytes}
            MMC.ExtendedCardSpecificData.EnhancedAreaOffset:=MMC.ExtendedCardSpecific[EXT_CSD_ENH_START_ADDR + 3] shl 24
                                                          or MMC.ExtendedCardSpecific[EXT_CSD_ENH_START_ADDR + 2] shl 16
                                                          or MMC.ExtendedCardSpecific[EXT_CSD_ENH_START_ADDR + 1] shl 8
                                                          or MMC.ExtendedCardSpecific[EXT_CSD_ENH_START_ADDR];

            if (MMC.Device.DeviceFlags and MMC_FLAG_BLOCK_ADDRESSED) <> 0 then
             begin
              MMC.ExtendedCardSpecificData.EnhancedAreaOffset:=MMC.ExtendedCardSpecificData.EnhancedAreaOffset shl 9;
             end;

            {Calculate the enhanced data area size, in kilobytes}
            MMC.ExtendedCardSpecificData.EnhancedAreaSize:=MMC.ExtendedCardSpecific[EXT_CSD_ENH_SIZE_MULT + 2] shl 16
                                                        or MMC.ExtendedCardSpecific[EXT_CSD_ENH_SIZE_MULT + 1] shl 8
                                                        or MMC.ExtendedCardSpecific[EXT_CSD_ENH_SIZE_MULT];
            MMC.ExtendedCardSpecificData.EnhancedAreaSize:=MMC.ExtendedCardSpecificData.EnhancedAreaSize * (HCEraseGroupSize * HCWriteProtectGroupSize);
            MMC.ExtendedCardSpecificData.EnhancedAreaSize:=MMC.ExtendedCardSpecificData.EnhancedAreaSize shl 9;
           end
          else
           begin
            if MMC_LOG_ENABLED then MMCLogWarn(nil,'MMC defines enhanced area without partition setting complete');
           end;
         end;
       end;

      {General purpose partition feature support - check if Extended CSD has the size of general purpose partitions}
      if (MMC.ExtendedCardSpecificData.PartitionSupport and EXT_CSD_PARTITION_PARTITIONING_EN) <> 0 then
       begin
        HCEraseGroupSize:=MMC.ExtendedCardSpecific[EXT_CSD_HC_ERASE_GRP_SIZE];
        HCWriteProtectGroupSize:=MMC.ExtendedCardSpecific[EXT_CSD_HC_WP_GRP_SIZE];

        for Index:=0 to MMC_NUM_GP_PARTITION - 1 do
         begin
          if (MMC.ExtendedCardSpecific[EXT_CSD_GP_SIZE_MULT + Index * 3] <> 0)
           and (MMC.ExtendedCardSpecific[EXT_CSD_GP_SIZE_MULT + Index * 3 + 1] <> 0)
           and (MMC.ExtendedCardSpecific[EXT_CSD_GP_SIZE_MULT + Index * 3 + 2] <> 0) then
           begin
            if not MMC.ExtendedCardSpecificData.PartitionSettingCompleted then
             begin
              if MMC_LOG_ENABLED then MMCLogWarn(nil,'MMC has partition size defined without partition complete');
              Break;
             end;


            MMC.ExtendedCardSpecificData.PartitionSizes[Index]:=MMC.ExtendedCardSpecific[EXT_CSD_GP_SIZE_MULT + Index * 3 + 2] shl 16
                                                             or MMC.ExtendedCardSpecific[EXT_CSD_GP_SIZE_MULT + Index * 3 + 1] shl 8
                                                             or MMC.ExtendedCardSpecific[EXT_CSD_GP_SIZE_MULT + Index * 3];
            MMC.ExtendedCardSpecificData.PartitionSizes[Index]:=MMC.ExtendedCardSpecificData.PartitionSizes[Index] * (HCEraseGroupSize * HCWriteProtectGroupSize);
            MMC.ExtendedCardSpecificData.PartitionSizes[Index]:=MMC.ExtendedCardSpecificData.PartitionSizes[Index] shl 19;
           end;
         end;
       end;

      MMC.ExtendedCardSpecificData.TRIMTimeout:=300 * MMC.ExtendedCardSpecific[EXT_CSD_TRIM_MULT];
      MMC.ExtendedCardSpecificData.BootReadOnlySupport:=MMC.ExtendedCardSpecific[EXT_CSD_BOOT_WP];
      MMC.ExtendedCardSpecificData.BootReadOnlyLockable:=True;

      {Save power class values}
      MMC.ExtendedCardSpecificData.PowerClass52MHz195:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_52_195];
      MMC.ExtendedCardSpecificData.PowerClass26MHz195:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_26_195];
      MMC.ExtendedCardSpecificData.PowerClass52MHz360:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_52_360];
      MMC.ExtendedCardSpecificData.PowerClass26MHz360:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_26_360];
      MMC.ExtendedCardSpecificData.PowerClass200MHz195:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_200_195];
      MMC.ExtendedCardSpecificData.PowerClass200MHz360:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_200_360];
      MMC.ExtendedCardSpecificData.PowerClassDDR52MHz195:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_DDR_52_195];
      MMC.ExtendedCardSpecificData.PowerClassDDR52MHz360:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_DDR_52_360];
      MMC.ExtendedCardSpecificData.PowerClassDDR200MHz360:=MMC.ExtendedCardSpecific[EXT_CSD_PWR_CL_DDR_200_360];
     end;

    if MMC.ExtendedCardSpecificData.Revision >= 5 then
     begin
      {Adjust production date as per JEDEC JESD84-B451}
      if MMC.CardIdentificationData.ManufacturingYear < 2010 then MMC.CardIdentificationData.ManufacturingYear:=MMC.CardIdentificationData.ManufacturingYear + 16;

      {Check whether the eMMC card supports BKOPS}
      if (MMC.ExtendedCardSpecific[EXT_CSD_BKOPS_SUPPORT] and $01) <> 0 then
       begin
        MMC.ExtendedCardSpecificData.BackgroundOperations:=True;
        MMC.ExtendedCardSpecificData.ManualBKOPSEnable:=(MMC.ExtendedCardSpecific[EXT_CSD_BKOPS_EN] and EXT_CSD_MANUAL_BKOPS_MASK) <> 0;
        MMC.ExtendedCardSpecificData.BKOPSStatus:=MMC.ExtendedCardSpecific[EXT_CSD_BKOPS_STATUS];
        MMC.ExtendedCardSpecificData.AutoBKOPSEnable:=(MMC.ExtendedCardSpecific[EXT_CSD_BKOPS_EN] and EXT_CSD_AUTO_BKOPS_MASK) <> 0;
       end;

      {Check whether the eMMC card supports HPI}
      if (MMC.ExtendedCardSpecific[EXT_CSD_HPI_FEATURES] and $01) <> 0 then
       begin
        MMC.ExtendedCardSpecificData.HPI:=True;
        if (MMC.ExtendedCardSpecific[EXT_CSD_HPI_FEATURES] and $02) <> 0 then
         begin
          MMC.ExtendedCardSpecificData.HPICommand:=MMC_CMD_STOP_TRANSMISSION;
         end
        else
         begin
          MMC.ExtendedCardSpecificData.HPICommand:=MMC_CMD_SEND_STATUS;
         end;

        {Indicate the maximum timeout to close a command interrupted by HPI}
        MMC.ExtendedCardSpecificData.OutOfInterruptTime:=MMC.ExtendedCardSpecific[EXT_CSD_OUT_OF_INTERRUPT_TIME] * 10;
       end;

      MMC.ExtendedCardSpecificData.WriteReliabilityParameter:=MMC.ExtendedCardSpecific[EXT_CSD_WR_REL_PARAM];
      MMC.ExtendedCardSpecificData.HardwareResetFunction:=MMC.ExtendedCardSpecific[EXT_CSD_RST_N_FUNCTION];

      {RPMB regions are defined in multiples of 128K}
      MMC.ExtendedCardSpecificData.RPMBSizeMult:=MMC.ExtendedCardSpecific[EXT_CSD_RPMB_MULT];
      if MMC.ExtendedCardSpecificData.RPMBSizeMult > 0 then
       begin
        MMC.ExtendedCardSpecificData.RPMBSize:=MMC.ExtendedCardSpecific[EXT_CSD_RPMB_MULT] shl 17;
       end;
     end;


    MMC.ExtendedCardSpecificData.ErasedMemoryContent:=MMC.ExtendedCardSpecific[EXT_CSD_ERASED_MEM_CONT];
    if MMC.ExtendedCardSpecificData.ErasedMemoryContent > 0 then
     begin
      MMC.ExtendedCardSpecificData.ErasedByte:=$FF;
     end
    else
     begin
      MMC.ExtendedCardSpecificData.ErasedByte:=$00;
     end;

    {eMMC v4.5 or later}
    MMC.ExtendedCardSpecificData.GenericCMD6Time:=MMC_DEFAULT_CMD6_TIMEOUT_MS;
    if MMC.ExtendedCardSpecificData.Revision >= 6 then
     begin
      MMC.ExtendedCardSpecificData.FeatureSupport:=MMC.ExtendedCardSpecificData.FeatureSupport or MMC_DISCARD_FEATURE;

      MMC.ExtendedCardSpecificData.GenericCMD6Time:=10 * MMC.ExtendedCardSpecific[EXT_CSD_GENERIC_CMD6_TIME];
      MMC.ExtendedCardSpecificData.PowerOffLongTime:=10 * MMC.ExtendedCardSpecific[EXT_CSD_POWER_OFF_LONG_TIME];

      MMC.ExtendedCardSpecificData.CacheSize:=MMC.ExtendedCardSpecific[EXT_CSD_CACHE_SIZE + 0]
                                           or MMC.ExtendedCardSpecific[EXT_CSD_CACHE_SIZE + 1] shl 8
                                           or MMC.ExtendedCardSpecific[EXT_CSD_CACHE_SIZE + 2] shl 16
                                           or MMC.ExtendedCardSpecific[EXT_CSD_CACHE_SIZE + 3] shl 24;

      if MMC.ExtendedCardSpecific[EXT_CSD_DATA_SECTOR_SIZE] = 1 then
       begin
        MMC.ExtendedCardSpecificData.DataSectorSize:=4096;
       end
      else
       begin
        MMC.ExtendedCardSpecificData.DataSectorSize:=512;
       end;

      MMC.ExtendedCardSpecificData.MaxPackedWrites:=MMC.ExtendedCardSpecific[EXT_CSD_MAX_PACKED_WRITES];
      MMC.ExtendedCardSpecificData.MaxPackedReads:=MMC.ExtendedCardSpecific[EXT_CSD_MAX_PACKED_READS];

      if ((MMC.ExtendedCardSpecific[EXT_CSD_DATA_TAG_SUPPORT] and $01) <> 0) and (MMC.ExtendedCardSpecific[EXT_CSD_TAG_UNIT_SIZE] <= 8) then
       begin
        MMC.ExtendedCardSpecificData.DataTagUnitSize:=(1 shl MMC.ExtendedCardSpecific[EXT_CSD_TAG_UNIT_SIZE]) * MMC.ExtendedCardSpecificData.DataSectorSize;
       end
      else
       begin
        MMC.ExtendedCardSpecificData.DataTagUnitSize:=0;
       end;
     end
    else
     begin
      MMC.ExtendedCardSpecificData.DataSectorSize:=512;
     end;

    {The GENERIC_CMD6_TIME is to be used "unless a specific timeout is defined when
     accessing a specific field", so use it here if there is no PARTITION_SWITCH_TIME}
    if MMC.ExtendedCardSpecificData.PartitionSwitchTime = 0 then
     begin
      MMC.ExtendedCardSpecificData.PartitionSwitchTime:=MMC.ExtendedCardSpecificData.GenericCMD6Time;
     end;
    {Some eMMC set the value too low so set a minimum}
    if MMC.ExtendedCardSpecificData.PartitionSwitchTime < MMC_MIN_PART_SWITCH_TIME then
     begin
      MMC.ExtendedCardSpecificData.PartitionSwitchTime:=MMC_MIN_PART_SWITCH_TIME;
     end;

    {eMMC v5 or later}
    if MMC.ExtendedCardSpecificData.Revision >= 7 then
     begin
      System.Move(MMC.ExtendedCardSpecific[EXT_CSD_FIRMWARE_VERSION],MMC.ExtendedCardSpecificData.FirmwareVersion[0],MMC_FIRMWARE_VERSION_LEN);

      MMC.ExtendedCardSpecificData.FieldFirmwareUpdate:=((MMC.ExtendedCardSpecific[EXT_CSD_SUPPORTED_MODE] and $01) <> 0) and ((MMC.ExtendedCardSpecific[EXT_CSD_FW_CONFIG] and $01) <> 0);

      MMC.ExtendedCardSpecificData.PreEndOfLifeInfo:=MMC.ExtendedCardSpecific[EXT_CSD_PRE_EOL_INFO];
      MMC.ExtendedCardSpecificData.DeviceLifetimeEstimateA:=MMC.ExtendedCardSpecific[EXT_CSD_DEVICE_LIFE_TIME_EST_TYP_A];
      MMC.ExtendedCardSpecificData.DeviceLifetimeEstimateB:=MMC.ExtendedCardSpecific[EXT_CSD_DEVICE_LIFE_TIME_EST_TYP_B];
     end;

    {eMMC v5.1 or later}
    if MMC.ExtendedCardSpecificData.Revision >= 8 then
     begin
      MMC.ExtendedCardSpecificData.CommandQueueSupport:=(MMC.ExtendedCardSpecific[EXT_CSD_CMDQ_SUPPORT] and EXT_CSD_CMDQ_SUPPORTED) <> 0;
      MMC.ExtendedCardSpecificData.CommandQueueDepth:=(MMC.ExtendedCardSpecific[EXT_CSD_CMDQ_DEPTH] and EXT_CSD_CMDQ_DEPTH_MASK) + 1;

      {Exclude inefficiently small queue depths}
      if MMC.ExtendedCardSpecificData.CommandQueueDepth <= 2 then
       begin
        MMC.ExtendedCardSpecificData.CommandQueueSupport:=False;
        MMC.ExtendedCardSpecificData.CommandQueueDepth:=0;
       end;

      MMC.ExtendedCardSpecificData.EnhancedRPMBSupport:=(MMC.ExtendedCardSpecificData.WriteReliabilityParameter and EXT_CSD_WR_REL_PARAM_EN_RPMB_REL_WR) <> 0;
     end;

    {Get Version}
    case MMC.ExtendedCardSpecificData.Revision of
     0:MMC.Version:=MMC_VERSION_4;
     1:MMC.Version:=MMC_VERSION_4_1;
     2:MMC.Version:=MMC_VERSION_4_2;
     3:MMC.Version:=MMC_VERSION_4_3;
     5:MMC.Version:=MMC_VERSION_4_41;
     6:MMC.Version:=MMC_VERSION_4_5;
     7:MMC.Version:=MMC_VERSION_5_0;
     8:MMC.Version:=MMC_VERSION_5_1;
    end;

    Result:=MMC_STATUS_SUCCESS;
   end;
  else
   begin
    {Version Unknown}
    if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Extended CSD structure version unknown');
   end;
 end;

 {Log Extended Card Specific}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' MMC Extended Card Specific:');
   MMCLogDebug(nil,'  CSDStructure = ' + IntToStr(MMC.ExtendedCardSpecificData.CSDStructure));
   if Result = MMC_STATUS_SUCCESS then
    begin
     {Card Data}
     MMCLogDebug(nil,'  Revision = ' + IntToStr(MMC.ExtendedCardSpecificData.Revision));

     MMCLogDebug(nil,'  CacheControl = ' + IntToStr(MMC.ExtendedCardSpecificData.CacheControl));
     MMCLogDebug(nil,'  PowerOffNotification = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerOffNotification));

     MMCLogDebug(nil,'  PartitionSupport = ' + IntToHex(MMC.ExtendedCardSpecificData.PartitionSupport,2));
     MMCLogDebug(nil,'  HardwareResetFunction = ' + IntToHex(MMC.ExtendedCardSpecificData.HardwareResetFunction,2));
     MMCLogDebug(nil,'  WriteReliabilityParameter = ' + IntToHex(MMC.ExtendedCardSpecificData.WriteReliabilityParameter,2));

     MMCLogDebug(nil,'  RPMBSizeMult = ' + IntToStr(MMC.ExtendedCardSpecificData.RPMBSizeMult));
     MMCLogDebug(nil,'  EraseGroupDef = ' + IntToHex(MMC.ExtendedCardSpecificData.EraseGroupDef,2));
     MMCLogDebug(nil,'  PartConfig = ' + IntToHex(MMC.ExtendedCardSpecificData.PartConfig,2));
     MMCLogDebug(nil,'  ErasedMemoryContent = ' + IntToStr(MMC.ExtendedCardSpecificData.ErasedMemoryContent));
     MMCLogDebug(nil,'  StrobeSupport = ' + IntToHex(MMC.ExtendedCardSpecificData.StrobeSupport,2));
     MMCLogDebug(nil,'  CardType = ' + IntToHex(MMC.ExtendedCardSpecificData.CardType,2));
     MMCLogDebug(nil,'  DriverStrength = ' + IntToHex(MMC.ExtendedCardSpecificData.DriverStrength,2));
     MMCLogDebug(nil,'  OutOfInterruptTime = ' + IntToStr(MMC.ExtendedCardSpecificData.OutOfInterruptTime));

     {MMCLogDebug(nil,'  SectorCount = ' + IntToStr(MMC.ExtendedCardSpecificData.SectorCount));}
     MMCLogDebug(nil,'  SATimeout = ' + IntToHex(MMC.ExtendedCardSpecificData.SATimeout,2));
     MMCLogDebug(nil,'  HCEraseGapSize = ' + IntToStr(MMC.ExtendedCardSpecificData.HCEraseGapSize));
     MMCLogDebug(nil,'  ReliableSectors = ' + IntToStr(MMC.ExtendedCardSpecificData.ReliableSectors));
     MMCLogDebug(nil,'  EraseTimeoutMult = ' + IntToStr(MMC.ExtendedCardSpecificData.EraseTimeoutMult));
     MMCLogDebug(nil,'  HCEraseGrpSize = ' + IntToStr(MMC.ExtendedCardSpecificData.HCEraseGrpSize));
     MMCLogDebug(nil,'  SecTRIMMult = ' + IntToStr(MMC.ExtendedCardSpecificData.SecTRIMMult));
     MMCLogDebug(nil,'  SecEraseMult = ' + IntToStr(MMC.ExtendedCardSpecificData.SecEraseMult));
     MMCLogDebug(nil,'  SecFeatureSupport = ' + IntToHex(MMC.ExtendedCardSpecificData.SecFeatureSupport,2));
     MMCLogDebug(nil,'  TRIMMult = ' + IntToStr(MMC.ExtendedCardSpecificData.TRIMMult));

     MMCLogDebug(nil,'  PowerClass52MHz195 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClass52MHz195));
     MMCLogDebug(nil,'  PowerClass26MHz195 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClass26MHz195));
     MMCLogDebug(nil,'  PowerClass52MHz360 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClass52MHz360));
     MMCLogDebug(nil,'  PowerClass26MHz360 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClass26MHz360));
     MMCLogDebug(nil,'  PowerClass200MHz195 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClass200MHz195));
     MMCLogDebug(nil,'  PowerClass200MHz360 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClass200MHz360));
     MMCLogDebug(nil,'  PowerClassDDR52MHz195 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClassDDR52MHz195));
     MMCLogDebug(nil,'  PowerClassDDR52MHz360 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClassDDR52MHz360));
     MMCLogDebug(nil,'  PowerClassDDR200MHz360 = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerClassDDR200MHz360));

     MMCLogDebug(nil,'  BKOPSStatus = ' + IntToHex(MMC.ExtendedCardSpecificData.BKOPSStatus,2));

     {MMCLogDebug(nil,'  FirmwareVersion = ' + IntToStr(MMC.ExtendedCardSpecificData.FirmwareVersion));}

     MMCLogDebug(nil,'  PreEndOfLifeInfo = ' + IntToHex(MMC.ExtendedCardSpecificData.PreEndOfLifeInfo,2));
     MMCLogDebug(nil,'  DeviceLifetimeEstimateA = ' + IntToHex(MMC.ExtendedCardSpecificData.DeviceLifetimeEstimateA,2));
     MMCLogDebug(nil,'  DeviceLifetimeEstimateB = ' + IntToHex(MMC.ExtendedCardSpecificData.DeviceLifetimeEstimateB,2));

     MMCLogDebug(nil,'  MaxPackedWrites = ' + IntToStr(MMC.ExtendedCardSpecificData.MaxPackedWrites));
     MMCLogDebug(nil,'  MaxPackedReads = ' + IntToStr(MMC.ExtendedCardSpecificData.MaxPackedReads));

     {Calculated Values}
     MMCLogDebug(nil,'  Sectors = ' + IntToStr(MMC.ExtendedCardSpecificData.Sectors));
     MMCLogDebug(nil,'  SleepAwakeTime = ' + IntToStr(MMC.ExtendedCardSpecificData.SleepAwakeTime));
     MMCLogDebug(nil,'  PartitionSwitchTime = ' + IntToStr(MMC.ExtendedCardSpecificData.PartitionSwitchTime));
     MMCLogDebug(nil,'  GenericCMD6Time = ' + IntToStr(MMC.ExtendedCardSpecificData.GenericCMD6Time));
     MMCLogDebug(nil,'  PowerOffLongTime = ' + IntToStr(MMC.ExtendedCardSpecificData.PowerOffLongTime));
     MMCLogDebug(nil,'  HCEraseSize = ' + IntToStr(MMC.ExtendedCardSpecificData.HCEraseSize));
     MMCLogDebug(nil,'  HCEraseTimeout = ' + IntToStr(MMC.ExtendedCardSpecificData.HCEraseTimeout));
     MMCLogDebug(nil,'  DataSectorSize = ' + IntToStr(MMC.ExtendedCardSpecificData.DataSectorSize));
     MMCLogDebug(nil,'  DataTagUnitSize = ' + IntToStr(MMC.ExtendedCardSpecificData.DataTagUnitSize));
     MMCLogDebug(nil,'  HSMaxRate = ' + IntToStr(MMC.ExtendedCardSpecificData.HSMaxRate));
     MMCLogDebug(nil,'  HS200MaxRate = ' + IntToStr(MMC.ExtendedCardSpecificData.HS200MaxRate));
     MMCLogDebug(nil,'  AvailableTypes = ' + IntToHex(MMC.ExtendedCardSpecificData.AvailableTypes,8));
     MMCLogDebug(nil,'  BootPartitionSize = ' + IntToStr(MMC.ExtendedCardSpecificData.BootPartitionSize));
     MMCLogDebug(nil,'  EnhancedAreaOffset = ' + IntToStr(MMC.ExtendedCardSpecificData.EnhancedAreaOffset));
     MMCLogDebug(nil,'  EnhancedAreaSize = ' + IntToStr(MMC.ExtendedCardSpecificData.EnhancedAreaSize));
     MMCLogDebug(nil,'  CacheSize = ' + IntToStr(MMC.ExtendedCardSpecificData.CacheSize));
     MMCLogDebug(nil,'  PartitionSettingCompleted = ' + BoolToStr(MMC.ExtendedCardSpecificData.PartitionSettingCompleted,True));
     MMCLogDebug(nil,'  TRIMTimeout = ' + IntToStr(MMC.ExtendedCardSpecificData.TRIMTimeout));
     for Index:=0 to MMC_NUM_GP_PARTITION - 1 do
      begin
       if MMC.ExtendedCardSpecificData.PartitionSizes[Index] > 0 then
        begin
         MMCLogDebug(nil,'  PartitionSize[' + IntToStr(Index) + '] = ' + IntToStr(MMC.ExtendedCardSpecificData.PartitionSizes[Index]));
        end;
      end;
     MMCLogDebug(nil,'  BootReadOnlySupport = ' + IntToHex(MMC.ExtendedCardSpecificData.BootReadOnlySupport,2));
     MMCLogDebug(nil,'  BootReadOnlyLockable = ' + BoolToStr(MMC.ExtendedCardSpecificData.BootReadOnlyLockable,True));
     MMCLogDebug(nil,'  FieldFirmwareUpdate = ' + BoolToStr(MMC.ExtendedCardSpecificData.FieldFirmwareUpdate,True));
     MMCLogDebug(nil,'  CommandQueueSupport = ' + BoolToStr(MMC.ExtendedCardSpecificData.CommandQueueSupport,True));
     MMCLogDebug(nil,'  CommandQueueDepth = ' + IntToStr(MMC.ExtendedCardSpecificData.CommandQueueDepth));
     MMCLogDebug(nil,'  BackgroundOperations = ' + BoolToStr(MMC.ExtendedCardSpecificData.BackgroundOperations,True));
     MMCLogDebug(nil,'  ManualBKOPSEnable = ' + BoolToStr(MMC.ExtendedCardSpecificData.ManualBKOPSEnable,True));
     MMCLogDebug(nil,'  AutoBKOPSEnable = ' + BoolToStr(MMC.ExtendedCardSpecificData.AutoBKOPSEnable,True));
     MMCLogDebug(nil,'  HPI = ' + BoolToStr(MMC.ExtendedCardSpecificData.HPI,True));
     MMCLogDebug(nil,'  HPIEnable = ' + BoolToStr(MMC.ExtendedCardSpecificData.HPIEnable,True));
     MMCLogDebug(nil,'  HPICommand = ' + IntToStr(MMC.ExtendedCardSpecificData.HPICommand));
     MMCLogDebug(nil,'  RPMBSize = ' + IntToStr(MMC.ExtendedCardSpecificData.RPMBSize));
     MMCLogDebug(nil,'  EnhancedRPMBSupport = ' + BoolToStr(MMC.ExtendedCardSpecificData.EnhancedRPMBSupport,True));
     MMCLogDebug(nil,'  ErasedByte = ' + IntToHex(MMC.ExtendedCardSpecificData.ErasedByte,8));
     MMCLogDebug(nil,'  FeatureSupport = ' + IntToHex(MMC.ExtendedCardSpecificData.FeatureSupport,8));
     MMCLogDebug(nil,'  Version = ' + MMCVersionToString(MMC.Version));
    end;
  end;
 {$ENDIF}

 //See: mmc_decode_ext_csd in \drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSetRelativeAddress(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Relative Address');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SET_RELATIVE_ADDR;
 Command.Argument:=(MMC.RelativeCardAddress shl 16);
 Command.ResponseType:=MMC_RSP_R1; {Not valid in SPI mode}
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_set_relative_addr in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSPISetCRC(MMC:PMMCDevice;Enable:Boolean):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC SPI Set CRC (Enable=' + BoolToStr(Enable,True) + ')');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SPI_CRC_ON_OFF;
 Command.Argument:=0;
 if Enable then Command.Argument:=1;
 Command.ResponseType:=MMC_RSP_SPI_R1;
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Update SDHCI}
 if Enable then
  begin
   SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags or SDHCI_FLAG_CRC_ENABLE;
  end
 else
  begin
   SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags and not(SDHCI_FLAG_CRC_ENABLE);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_startup in U-Boot mmc.c
 //See: mmc_spi_set_crc in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceSPIReadOperationCondition(MMC:PMMCDevice;HighCapacity:Boolean):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC SPI Read Operation Condition');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SPI_READ_OCR;
 Command.Argument:=0;
 if HighCapacity then Command.Argument:=(1 shl 30);
 Command.ResponseType:=MMC_RSP_SPI_R3;
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 MMC.OperationCondition:=Command.Response[1];

 Result:=MMC_STATUS_SUCCESS;

 //See: sd_send_op_cond or mmc_complete_op_cond in U-Boot mmc.c
 //See: mmc_spi_read_ocr in \drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceInsert(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 //To Do //See Section 3.1 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf

end;

{==============================================================================}

function MMCDeviceRemove(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 //To Do //See Section 3.1 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf

end;

{==============================================================================}

function MMCDeviceInitialize(MMC:PMMCDevice):LongWord;
{Reference: Section 3.6 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Initialize}
 if Assigned(MMC.DeviceInitialize) then
  begin
   Result:=MMC.DeviceInitialize(MMC);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Initialize');
   {$ENDIF}

   {Get Card Detect}
   Result:=MMCDeviceGetCardDetect(MMC);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Check Card Detect}
   if (MMC.Device.DeviceFlags and MMC_FLAG_CARD_PRESENT) = 0 then
    begin
     Result:=MMC_STATUS_NO_MEDIA;
     Exit;
    end;

   {Update Device}
   if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_AUTO_CMD23) <> 0 then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_AUTO_BLOCK_COUNT;
   if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_AUTO_CMD12) <> 0 then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_AUTO_COMMAND_STOP;
   if (SDHCI.Capabilities and MMC_CAP_NONREMOVABLE) <> 0 then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_NON_REMOVABLE;

   {Set Initial Power}
   Result:=SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI.Voltages));
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Set Initial Bus Width}
   Result:=MMCDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_1);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Set Initial Clock}
   Result:=MMCDeviceSetClock(MMC,MMC_BUS_SPEED_DEFAULT);
   if Result <> MMC_STATUS_SUCCESS then Exit;

   {Perform an SDIO Reset}
   SDIODeviceReset(MMC);

   {Set the Card to Idle State}
   MMCDeviceGoIdle(MMC);

   {Get the Interface Condition}
   SDDeviceSendInterfaceCondition(MMC);

   {Check for an SDIO Card}
   Result:=MMCDeviceInitializeSDIO(MMC);
   if Result = MMC_STATUS_SUCCESS then Exit;

   {Check for an SD Card}
   Result:=MMCDeviceInitializeSD(MMC);
   if Result = MMC_STATUS_SUCCESS then Exit;

   {Check for an MMC Card}
   Result:=MMCDeviceInitializeMMC(MMC);
   if Result = MMC_STATUS_SUCCESS then Exit;

   {Return Result}
   Result:=MMC_STATUS_NO_MEDIA;
  end;

 //See: U-Boot mmc_start_init / mmc_complete_init / mmc_init / mmc_startup in mmc.c
 //     U-Boot sdhci_init in sdhci.c

 //See: mmc_init_card in \drivers\mmc\core\mmc.c
 //     mmc_attach_mmc in \drivers\mmc\core\mmc.c

 //     mmc_sd_init_card in \drivers\mmc\core\sd.c
 //     mmc_attach_sd in \drivers\mmc\core\sd.c
 //     mmc_sd_setup_card in \drivers\mmc\core\sd.c

 //     mmc_sdio_init_card in \drivers\mmc\core\sdio.c
 //     mmc_attach_sdio in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function MMCDeviceDeinitialize(MMC:PMMCDevice):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Initialize}
 if Assigned(MMC.DeviceDeinitialize) then
  begin
   Result:=MMC.DeviceDeinitialize(MMC);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Deinitialize');
   {$ENDIF}

   {Update MMC}
   {Device}
   MMC.Device.DeviceBus:=DEVICE_BUS_MMC;
   MMC.Device.DeviceType:=MMC_TYPE_MMC;
   MMC.Device.DeviceFlags:=MMC_FLAG_NONE;
   {MMC}
   MMC.MMCState:=MMC_STATE_EJECTED;
   {Driver}
   MMC.Version:=0;
   MMC.Clock:=0;
   MMC.Timing:=MMC_TIMING_LEGACY;
   MMC.BusWidth:=MMC_BUS_WIDTH_1;
   MMC.DriverType:=MMC_SET_DRIVER_TYPE_B;
   MMC.SignalVoltage:=MMC_SIGNAL_VOLTAGE_330;
   {MMC.Voltages:=0;} {Not reset}
   {MMC.Capabilities:=0;} {Not reset}
   {MMC.Capabilities2:=0;} {Not reset}
   MMC.EraseSize:=0;
   MMC.EraseShift:=0;
   MMC.EraseArgument:=0;
   MMC.PreferredEraseSize:=0;
   MMC.EnhancedStrobe:=False;
   {Register}
   MMC.InterfaceCondition:=0;
   MMC.OperationCondition:=0;
   MMC.RelativeCardAddress:=0;
   FillChar(MMC.CardSpecific,SizeOf(MMC.CardSpecific),0);
   FillChar(MMC.CardIdentification,SizeOf(MMC.CardIdentification),0);
   if MMC.ExtendedCardSpecific <> nil then
    begin
     if SDHCIHasDMA(SDHCI) then DMABufferRelease(MMC.ExtendedCardSpecific) else FreeMem(MMC.ExtendedCardSpecific);
    end;
   MMC.ExtendedCardSpecific:=nil;
   MMC.CardStatus:=0;
   MMC.DriverStage:=0;
   FillChar(MMC.SDStatus,SizeOf(MMC.SDStatus),0);
   FillChar(MMC.SDSwitch,SizeOf(MMC.SDSwitch),0);
   FillChar(MMC.SDConfiguration,SizeOf(MMC.SDConfiguration),0);
   {Configuration}
   FillChar(MMC.CardSpecificData,SizeOf(TMMCCardSpecificData),0);
   FillChar(MMC.CardIdentificationData,SizeOf(TMMCCardIdentificationData),0);
   FillChar(MMC.ExtendedCardSpecificData,SizeOf(TMMCExtendedCardSpecificData),0);
   FillChar(MMC.SDStatusData,SizeOf(TSDStatusData),0);
   FillChar(MMC.SDSwitchData,SizeOf(TSDSwitchData),0);
   FillChar(MMC.SDConfigurationData,SizeOf(TSDConfigurationData),0);

   {Check Storage}
   if MMC.Storage <> nil then
    begin
     {Update Storage}
     {Device}
     MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
     MMC.Storage.Device.DeviceType:=STORAGE_TYPE_REMOVABLE;
     MMC.Storage.Device.DeviceFlags:=STORAGE_FLAG_REMOVABLE or STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA;
     {Storage}
     {MMC.Storage.StorageState:=STORAGE_STATE_EJECTED;} {Handled by caller during notification}
     {Driver}
     MMC.Storage.BlockSize:=0;
     MMC.Storage.BlockShift:=0;
     MMC.Storage.BlockCount:=0;
     if MMC.Storage.Vendor <> nil then FreeMem(MMC.Storage.Vendor);
     if MMC.Storage.Product <> nil then FreeMem(MMC.Storage.Product);
     if MMC.Storage.Revision <> nil then FreeMem(MMC.Storage.Revision);
     MMC.Storage.Vendor:=nil;
     MMC.Storage.Product:=nil;
     MMC.Storage.Revision:=nil;
    end;

   Result:=MMC_STATUS_SUCCESS;
  end;
end;

{==============================================================================}

function MMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Card Detect');
 {$ENDIF}

 {Check Get Card Detect}
 if Assigned(MMC.DeviceGetCardDetect) then
  begin
   Result:=MMC.DeviceGetCardDetect(MMC);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   {Check Non Removable}
   if MMCIsNonRemovable(MMC) then
    begin
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);

     Result:=MMC_STATUS_SUCCESS;
     Exit;
    end;

   {Check Broken Card Detect}
   if (SDHCI.Quirks and SDHCI_QUIRK_BROKEN_CARD_DETECTION) <> 0 then
    begin
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);

     Result:=MMC_STATUS_SUCCESS;
     Exit;
    end;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Card Detect (SDHCI_PRESENT_STATE=' + IntToHex(SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE),8) + ')');
   {$ENDIF}

   {Get Card Present}
   if (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and SDHCI_CARD_PRESENT) <> 0 then
    begin
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);

     {$IFDEF MMC_DEBUG}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Card Detect (Flags=MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end
   else
    begin
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);

     {$IFDEF MMC_DEBUG}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: mmc_getcd in mmc.c
end;

{==============================================================================}

function MMCDeviceGetWriteProtect(MMC:PMMCDevice):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Write Protect');
 {$ENDIF}

 {Check Get Write Protect}
 if Assigned(MMC.DeviceGetWriteProtect) then
  begin
   Result:=MMC.DeviceGetWriteProtect(MMC);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Write Protect (SDHCI_PRESENT_STATE=' + IntToHex(SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE),8) + ')');
   {$ENDIF}

   {Get Write Protect}
   if (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and SDHCI_WRITE_PROTECT) = 0 then
    begin
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_WRITE_PROTECT);

     {$IFDEF MMC_DEBUG}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Write Protect (Flags=MMC_FLAG_WRITE_PROTECT)');
     {$ENDIF}
    end
   else
    begin
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_WRITE_PROTECT);

     {$IFDEF MMC_DEBUG}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Get Write Protectt (Flags=not MMC_FLAG_WRITE_PROTECT)');
     {$ENDIF}
    end;

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: mmc_getwp in mmc.c
end;

{==============================================================================}

function MMCDeviceSendCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord;
var
 Mask:LongWord;
 Mode:LongWord;
 Flags:LongWord;
 Status:LongWord;
 Timeout:LongWord;
 Control:LongWord;
 PIOInterrupts:LongWord;
 DMAInterrupts:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command');
 {$ENDIF}

 {Check Send Command}
 if Assigned(MMC.DeviceSendCommand) then
  begin
   Result:=MMC.DeviceSendCommand(MMC,Command);
  end
 else
  begin
   {Default Method}
   {Check Command}
   if Command = nil then Exit;

   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   {Acquire the Lock}
   if MutexLock(MMC.Lock) = ERROR_SUCCESS then
    begin
     try
      {Setup Status}
      Command.Status:=MMC_STATUS_NOT_PROCESSED;
      Command.DataCompleted:=False;
      Command.BusyCompleted:=False;
      Command.TuningCompleted:=False;
      Command.CommandCompleted:=False;
      try
       {Update Statistics}
       Inc(SDHCI.RequestCount);

       if ((SDHCI.Quirks2 and SDHCI_QUIRK2_STOP_WITH_TC) <> 0) and (Command.Command = MMC_CMD_STOP_TRANSMISSION) then
        begin
         Command.ResponseType:=Command.ResponseType or MMC_RSP_BUSY;
        end;

       {Wait Timeout (10ms)}
       Timeout:=1000;
       Mask:=SDHCI_CMD_INHIBIT;
       if (Command.Data <> nil) or ((Command.ResponseType and MMC_RSP_BUSY) <> 0) then
        begin
         Mask:=Mask or SDHCI_DATA_INHIBIT;
        end;

       {We shouldn't wait for data inihibit for stop commands, even though they might use busy signaling}
       if Command.Command = MMC_CMD_STOP_TRANSMISSION then
        begin
         Mask:=Mask and not(SDHCI_DATA_INHIBIT);
        end;

       {Wait for Command Inhibit and optionally Data Inhibit to be clear}
       while (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and Mask) <> 0 do
        begin
         if Timeout = 0 then
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Command Inhibit Timeout');

           Command.Status:=MMC_STATUS_TIMEOUT;
           Exit;
          end;

         Dec(Timeout);
         MicrosecondDelay(10);
        end;

       {Check Response Type}
       if ((Command.ResponseType and MMC_RSP_136) <> 0) and ((Command.ResponseType and MMC_RSP_BUSY) <> 0) then
        begin
         if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Command Invalid Response Type');

         Command.Status:=MMC_STATUS_INVALID_PARAMETER;
         Exit;
        end;

       {Setup Command Flags}
       if (Command.ResponseType and MMC_RSP_PRESENT) = 0 then
        begin
         Flags:=SDHCI_CMD_RESP_NONE;
        end
       else if (Command.ResponseType and MMC_RSP_136) <> 0 then
        begin
         Flags:=SDHCI_CMD_RESP_LONG;
        end
       else if (Command.ResponseType and MMC_RSP_BUSY) <> 0 then
        begin
         Flags:=SDHCI_CMD_RESP_SHORT_BUSY;
        end
       else
        begin
         Flags:=SDHCI_CMD_RESP_SHORT;
        end;

       if (Command.ResponseType and MMC_RSP_CRC) <> 0 then
        begin
         Flags:=Flags or SDHCI_CMD_CRC;
        end;
       if (Command.ResponseType and MMC_RSP_OPCODE) <> 0 then
        begin
         Flags:=Flags or SDHCI_CMD_INDEX;
        end;
       {CMD19 is special in that the Data Present Select should be set}
       if (Command.Data <> nil) or (Command.Command = MMC_CMD_SEND_TUNING_BLOCK) or (Command.Command = MMC_CMD_SEND_TUNING_BLOCK_HS200) then
        begin
         Flags:=Flags or SDHCI_CMD_DATA;
        end;

       {Write Timeout Control}
       if (Command.Data <> nil) or ((Command.ResponseType and MMC_RSP_BUSY) <> 0) then
        begin
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_TIMEOUT_CONTROL (Value=' + IntToHex(SDHCI_TIMEOUT_VALUE,8) + ')');
         {$ENDIF}
         SDHCIHostWriteByte(SDHCI,SDHCI_TIMEOUT_CONTROL,SDHCI_TIMEOUT_VALUE);
        end;

       {Check Data}
       if Command.Data = nil then
        begin
         {Setup Command}
         SDHCI.UseDMA:=False;

         if (SDHCI.Quirks2 and SDHCI_QUIRK2_CLEAR_TRANSFERMODE_REG_BEFORE_CMD) <> 0 then
          begin
           {Clear Transfer Mode}
           Mode:=0;
          end
         else
          begin
           {Setup Transfer Mode}
           Mode:=SDHCIHostReadWord(SDHCI,SDHCI_TRANSFER_MODE);

           {Clear Auto CMD settings for non data CMDs}
           Mode:=Mode and not(SDHCI_TRNS_AUTO_CMD12 or SDHCI_TRNS_AUTO_CMD23);

           {Clear Block Count, Multi, Read and DMA for non data CMDs}
           Mode:=Mode and not(SDHCI_TRNS_BLK_CNT_EN or SDHCI_TRNS_MULTI or SDHCI_TRNS_READ or SDHCI_TRNS_DMA);
          end;

         {Write Argument}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command.Argument,8) + ')');
         {$ENDIF}
         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command.Argument);

         {Write Transfer Mode (Except when tuning)}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(Mode,4) + ')');
         {$ENDIF}
         if Command.Command <> MMC_CMD_SEND_TUNING_BLOCK_HS200 then SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,Mode);
        end
       else
        begin
         {Setup Data}
         Command.Data.BlockOffset:=0;
         Command.Data.BlocksRemaining:=Command.Data.BlockCount;
         Command.Data.BytesRemaining:=Command.Data.BlockSize * Command.Data.BlockCount;
         Command.Data.BytesTransfered:=0;

         {Check DMA}
         SDHCI.UseDMA:=SDHCIHasDMA(SDHCI) and (Command.Data.BlockCount > SDHCI.MaximumPIOBlocks) and (Command.Data.BytesRemaining >= SDHCI.MinimumDMASize);
         if SDHCI.UseDMA then
          begin
           {Prepare DMA}
           Status:=SDHCIHostPrepareDMA(SDHCI,Command);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             Result:=Status;
             Exit;
            end;
          end
         else
          begin
           {Adjust the DMA selection as some controllers can't do PIO properly when the selection is ADMA}
           Control:=SDHCIHostReadByte(SDHCI,SDHCI_HOST_CONTROL);
           Control:=Control and not(SDHCI_CTRL_DMA_MASK);
           SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Control);
          end;

         {Setup Transfer Mode}
         Mode:=0;
         if (SDHCI.Quirks2 and SDHCI_QUIRK2_SUPPORT_SINGLE) = 0 then
          begin
           Mode:=SDHCI_TRNS_BLK_CNT_EN;
          end;
         if MMCIsMultiCommand(Command.Command) or (Command.Data.BlockCount > 1) then
          begin
           Mode:=SDHCI_TRNS_BLK_CNT_EN or SDHCI_TRNS_MULTI;

           if not(MMCHasSetBlockCount(MMC)) and SDHCIAutoCMD12(SDHCI) and (Command.Command <> SDIO_CMD_RW_EXTENDED) then
            begin
             {Set Auto CMD12 for open ended transfer}
             Mode:=Mode or SDHCI_TRNS_AUTO_CMD12;
            end
           else if (Command.Data.BlockCount > 1) and MMCHasSetBlockCount(MMC) and SDHCIAutoCMD23(SDHCI) then
            begin
            {Set Auto CMD23 if block count is known}
             Mode:=Mode or SDHCI_TRNS_AUTO_CMD23;

             SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT2,Command.Data.BlockCount);
            end;
          end;
         if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
          begin
           Mode:=Mode or SDHCI_TRNS_READ;
          end;
         if SDHCI.UseDMA then
          begin
           Mode:=Mode or SDHCI_TRNS_DMA;
          end;

         {Setup Interrupts}
         PIOInterrupts:=SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL;
         DMAInterrupts:=SDHCI_INT_DMA_END or SDHCI_INT_ADMA_ERROR;
         if SDHCI.UseDMA then
          begin
           SDHCI.Interrupts:=(SDHCI.Interrupts and not PIOInterrupts) or DMAInterrupts;
          end
         else
          begin
           SDHCI.Interrupts:=(SDHCI.Interrupts and not DMAInterrupts) or PIOInterrupts;
          end;
         if SDHCIAutoCMD12(SDHCI) or SDHCIAutoCMD23(SDHCI) then
          begin
           SDHCI.Interrupts:=SDHCI.Interrupts or SDHCI_INT_AUTO_CMD_ERR;
          end
         else
          begin
           SDHCI.Interrupts:=SDHCI.Interrupts and not(SDHCI_INT_AUTO_CMD_ERR);
          end;
         SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
         SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);

         {Write DMA Boundary and Block Size}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_BLOCK_SIZE (Value=' + IntToStr(Command.Data.BlockSize) + ')');
         {$ENDIF}
         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_SIZE,SDHCIMakeBlockSize(SDHCI.SDMABoundary,Command.Data.BlockSize));

         {Write Block Count}
         if (SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_410) and SDHCI.EnableV4Mode and ((SDHCI.Quirks2 and SDHCI_QUIRK2_USE_32BIT_BLK_CNT) <> 0) then
          begin
           {If 32-bit block count is enabled then 16-bit block count must be zero}
           if SDHCIHostReadWord(SDHCI,SDHCI_BLOCK_COUNT) > 0 then
            begin
             SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_COUNT,0);
            end;

           {$IFDEF MMC_DEBUG}
           if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_32BIT_BLK_CNT (Value=' + IntToStr(Command.Data.BlockCount) + ')');
           {$ENDIF}
           SDHCIHostWriteLong(SDHCI,SDHCI_32BIT_BLK_CNT,Command.Data.BlockCount);
          end
         else
          begin
           {$IFDEF MMC_DEBUG}
           if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_BLOCK_COUNT (Value=' + IntToStr(Command.Data.BlockCount) + ')');
           {$ENDIF}
           SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_COUNT,Command.Data.BlockCount);
          end;

         {Write Argument}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command.Argument,8) + ')');
         {$ENDIF}
         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command.Argument);

         {Write Transfer Mode}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(Mode,4) + ')');
         {$ENDIF}
         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,Mode);
        end;

       {Setup Command}
       SDHCI.Command:=Command;
       try
        {Write Command}
        {$IFDEF MMC_DEBUG}
        if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_COMMAND (Value=' + IntToHex(SDHCIMakeCommand(Command.Command,Flags),4) + ')');
        {$ENDIF}
        SDHCIHostWriteWord(SDHCI,SDHCI_COMMAND,SDHCIMakeCommand(Command.Command,Flags));

        {Wait for Completion}
        if Command.Data = nil then
         begin
          {Update Statistics}
          Inc(SDHCI.CommandRequestCount);

          {Get Command Timeout (Default 1000ms)}
          Timeout:=1000;
          if Command.Timeout > Timeout then Timeout:=Command.Timeout;

          {Wait for Signal with Timeout}
          Status:=SemaphoreWaitEx(SDHCI.Wait,Timeout);
          if Status <> ERROR_SUCCESS then
           begin
            if Status = ERROR_WAIT_TIMEOUT then
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Command Response Timeout');

              Command.Status:=MMC_STATUS_TIMEOUT;
             end
            else
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Command Response Failure');

              Command.Status:=MMC_STATUS_HARDWARE_ERROR;
             end;
           end;
         end
        else
         begin
          {Update Statistics}
          Inc(SDHCI.DataRequestCount);

          {Check DMA}
          if SDHCI.UseDMA then
           begin
            {Start DMA}
            if SDHCIHostStartDMA(SDHCI,Command) <> MMC_STATUS_SUCCESS then Exit;

            {Update Statistics}
            Inc(SDHCI.DMADataTransferCount);
           end
          else
           begin
            {Update Statistics}
            Inc(SDHCI.PIODataTransferCount);
           end;

          {Get Data Timeout (Default 5000ms)}
          Timeout:=5000;
          if Command.Timeout > Timeout then Timeout:=Command.Timeout;

          {Wait for Signal with Timeout}
          Status:=SemaphoreWaitEx(SDHCI.Wait,Timeout);
          if Status <> ERROR_SUCCESS then
           begin
            if Status = ERROR_WAIT_TIMEOUT then
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Data Response Timeout');

              Command.Status:=MMC_STATUS_TIMEOUT;
             end
            else
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Data Response Failure');

              Command.Status:=MMC_STATUS_HARDWARE_ERROR;
             end;
           end;

          {Check and Stop DMA}
          if SDHCI.UseDMA then SDHCIHostStopDMA(SDHCI,Command);
         end;
       finally
        {Reset Command}
        SDHCI.Command:=nil;
       end;
      finally
       {Check Reset Required}
       if (Command.Status <> MMC_STATUS_SUCCESS) or ((SDHCI.Quirks and SDHCI_QUIRK_RESET_AFTER_REQUEST) <> 0) then
        begin
         {Some hosts require clock set before reset}
         if (SDHCI.Quirks and SDHCI_QUIRK_CLOCK_BEFORE_RESET) <> 0 then
          begin
           SDHCIHostSetClock(SDHCI,SDHCI.Clock);
          end;

         {Reset Host}
         SDHCIHostReset(SDHCI,SDHCI_RESET_CMD);
         SDHCIHostReset(SDHCI,SDHCI_RESET_DATA);
        end;

       {Check Status}
       if Command.Status <> MMC_STATUS_SUCCESS then
        begin
         {Update Statistics}
         Inc(SDHCI.RequestErrors);

         {Return Result}
         Result:=Command.Status;
        end;
      end;

     finally
      {Release the Lock}
      MutexUnlock(MMC.Lock);
     end;
    end;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command completed: ' + MMCStatusToString(Command.Status));
   {$ENDIF}
   if Command.Status = MMC_STATUS_SUCCESS then Result:=MMC_STATUS_SUCCESS;
  end;

 //See: mmc_send_cmd in mmc.c
 //     sdhci_send_command in sdhci.c
 //See: bcm2835_mmc_send_command in \drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_send_command in \drivers\mmc\host\sdhci.c
end;

{==============================================================================}

function MMCDeviceSetIOS(MMC:PMMCDevice):LongWord;
var
 Clock:Word;
 Preset:Word;
 Control:Byte;
 Control2:Word;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set IOS (Clock=' + IntToStr(MMC.Clock) + ' BusWidth=' + MMCBusWidthToString(MMC.BusWidth) + ' Timing=' + MMCBusWidthToString(MMC.Timing) + ')');
 {$ENDIF}

 {Check Set IOS}
 if Assigned(MMC.DeviceSetIOS) then
  begin
   Result:=MMC.DeviceSetIOS(MMC);
  end
 else
  begin
   {Default Method}
   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
    begin
     if (SDHCI.Quirks2 and SDHCI_QUIRK2_PRESET_VALUE_BROKEN) = 0 then
      begin
       {Disable Preset Value}
       {SDHCIHostEnablePresetValue(SDHCI,False);} {Not currently supported}
      end;
    end;

   {Set Control Register}
   SDHCIHostSetControlRegister(SDHCI);

   {Check Clock}
   if MMC.Clock <> SDHCI.Clock then
    begin
     {Set Clock}
     SDHCIHostSetClock(SDHCI,MMC.Clock);

     {Update Clock}
     SDHCI.Clock:=MMC.Clock;
    end;

   {Set Power}
   SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI.Voltages));

   {Set Bus Width}
   SDHCIHostSetBusWidth(SDHCI,MMC.BusWidth);

   {Update Bus Width}
   SDHCI.BusWidth:=MMC.BusWidth;

   {Get Control}
   Control:=SDHCIHostReadByte(SDHCI,SDHCI_HOST_CONTROL);

   {Check High Speed Timing}
   if (MMC.Timing = MMC_TIMING_SD_HS) or
      (MMC.Timing = MMC_TIMING_MMC_HS) or
      (MMC.Timing = MMC_TIMING_MMC_HS400) or
      (MMC.Timing = MMC_TIMING_MMC_HS200) or
      (MMC.Timing = MMC_TIMING_MMC_DDR52) or
      (MMC.Timing = MMC_TIMING_UHS_SDR50) or
      (MMC.Timing = MMC_TIMING_UHS_SDR104) or
      (MMC.Timing = MMC_TIMING_UHS_DDR50) or
      (MMC.Timing = MMC_TIMING_UHS_SDR25) then
    begin
     Control:=Control or SDHCI_CTRL_HISPD;
    end
   else
    begin
     Control:=Control and not(SDHCI_CTRL_HISPD);
    end;
   if (SDHCI.Quirks and SDHCI_QUIRK_NO_HISPD_BIT) <> 0 then
    begin
     Control:=Control and not(SDHCI_CTRL_HISPD);
    end;

   if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
    begin
     if not SDHCI.PresetEnabled then
      begin
       {Set Control}
       SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Control);

       {Set Driver Strength if the preset value enable is not set}
       Control2:=SDHCIHostReadWord(SDHCI,SDHCI_HOST_CONTROL2);
       Control2:=Control2 and not(SDHCI_CTRL_DRV_TYPE_MASK);
       if MMC.DriverType = MMC_SET_DRIVER_TYPE_A then
        begin
         Control2:=Control2 or SDHCI_CTRL_DRV_TYPE_A;
        end
       else if MMC.DriverType = MMC_SET_DRIVER_TYPE_B then
        begin
         Control2:=Control2 or SDHCI_CTRL_DRV_TYPE_B;
        end
       else if MMC.DriverType = MMC_SET_DRIVER_TYPE_C then
        begin
         Control2:=Control2 or SDHCI_CTRL_DRV_TYPE_C;
        end
       else if MMC.DriverType = MMC_SET_DRIVER_TYPE_D then
        begin
         Control2:=Control2 or SDHCI_CTRL_DRV_TYPE_D;
        end
       else
        begin
         if MMC_LOG_ENABLED then MMCLogWarn(nil,'MMC invalid driver type, defaulting to MMC_SET_DRIVER_TYPE_B');

         Control2:=Control2 or SDHCI_CTRL_DRV_TYPE_B;
        end;

       {Set Control2}
       SDHCIHostWriteWord(SDHCI,SDHCI_HOST_CONTROL2,Control2);
      end
     else
      begin
       {According to SDHC Spec v3.00, if the Preset Value Enable in the Host Control 2 register is set, we
        need to reset SD Clock Enable before changing High Speed Enable to avoid generating clock gliches}

       {Reset SD Clock Enable}
       Clock:=SDHCIHostReadWord(SDHCI,SDHCI_CLOCK_CONTROL);
       Clock:=Clock and not(SDHCI_CLOCK_CARD_EN);
       SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,Clock);

       {Set Control}
       SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Control);

       {Enable SD Clock}
       SDHCIHostSetClock(SDHCI,SDHCI.Clock);
      end;

     {Reset SD Clock Enable}
     Clock:=SDHCIHostReadWord(SDHCI,SDHCI_CLOCK_CONTROL);
     Clock:=Clock and not(SDHCI_CLOCK_CARD_EN);
     SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,Clock);

     {Set Timing}
     SDHCIHostSetTiming(SDHCI,MMC.Timing);

     {Update Timing}
     SDHCI.Timing:=MMC.Timing;

     if (SDHCI.Quirks2 and SDHCI_QUIRK2_PRESET_VALUE_BROKEN) = 0 then
      begin
       {Check Timing}
       if (MMC.Timing = MMC_TIMING_UHS_SDR12) or
          (MMC.Timing = MMC_TIMING_UHS_SDR25) or
          (MMC.Timing = MMC_TIMING_UHS_SDR50) or
          (MMC.Timing = MMC_TIMING_UHS_SDR104) or
          (MMC.Timing = MMC_TIMING_UHS_DDR50) or
          (MMC.Timing = MMC_TIMING_MMC_DDR52) then
        begin
         {Enable Preset Value}
         {SDHCIHostEnablePresetValue(SDHCI,True);} {Not currently supported}

         {Get Driver Type}
         {Preset:=SDHCIHostGetPresetValue(SDHCI);
         MMC.DriverType:=(Preset and SDHCI_PRESET_DRV_MASK) shr SDHCI_PRESET_DRV_SHIFT;} {Not currently supported}
        end;
      end;

     {Enable SD Clock}
     SDHCIHostSetClock(SDHCI,SDHCI.Clock);
    end
   else
    begin
     {Set Control}
     SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Control);
    end;

   if (SDHCI.Quirks and SDHCI_QUIRK_RESET_CMD_DATA_ON_IOS) <> 0 then
    begin
     SDHCIHostReset(SDHCI,SDHCI_RESET_CMD or SDHCI_RESET_DATA);
    end;

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: mmc_set_ios in mmc.c
 //     sdhci_set_ios in sdhci.c
 //See: bcm2835_mmc_set_ios in \drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_do_set_ios in \drivers\mmc\host\sdhci.c
end;

{==============================================================================}

function MMCDeviceCreate:PMMCDevice;
{Create a new MMC entry}
{Return: Pointer to new MMC entry or nil if MMC could not be created}
begin
 {}
 Result:=MMCDeviceCreateEx(SizeOf(TMMCDevice));
end;

{==============================================================================}

function MMCDeviceCreateEx(Size:LongWord):PMMCDevice;
{Create a new MMC entry}
{Size: Size in bytes to allocate for new MMC (Including the MMC entry)}
{Return: Pointer to new MMC entry or nil if MMC could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TMMCDevice) then Exit;

 {Create MMC}
 Result:=PMMCDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=MMC_TYPE_NONE;
 Result.Device.DeviceFlags:=MMC_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update MMC}
 Result.MMCId:=DEVICE_ID_ANY;
 Result.MMCState:=MMC_STATE_EJECTED;
 Result.DeviceInitialize:=nil;
 Result.DeviceDeinitialize:=nil;
 Result.DeviceGetCardDetect:=nil;
 Result.DeviceGetWriteProtect:=nil;
 Result.DeviceSendCommand:=nil;
 Result.DeviceSetIOS:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to create lock for MMC device');

   MMCDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function MMCDeviceDestroy(MMC:PMMCDevice):LongWord;
{Destroy an existing MMC entry}
var
 Count:LongWord;
 SDHCI:PSDHCIHost;
 Tuple:PSDIOTuple;
 Current:PSDIOTuple;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;
 if MMC.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check MMC}
 Result:=ERROR_IN_USE;
 if MMCDeviceCheck(MMC) = MMC then Exit;

 {Check State}
 if MMC.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if MMC.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(MMC.Lock);
  end;

 {Free Extended Card Specific}
 if MMC.ExtendedCardSpecific <> nil then
  begin
   {Get SDHCI}
   SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
   if SDHCI = nil then Exit;

   if SDHCIHasDMA(SDHCI) then
    begin
     DMABufferRelease(MMC.ExtendedCardSpecific);
    end
   else
    begin
     FreeMem(MMC.ExtendedCardSpecific);
    end;
  end;

 {Release Functions}
 for Count:=0 to SDIO_MAX_FUNCTIONS - 1 do
  begin
   SDIOFunctionRelease(MMC.SDIOFunctions[Count]);
  end;

 {Free Tuples}
 Tuple:=MMC.Tuples;
 while Tuple <> nil do
  begin
   Current:=Tuple;
   Tuple:=Tuple.Next;

   FreeMem(Current);
  end;

 {Free CIS}
 if MMC.CIS <> nil then
  begin
   FreeMem(MMC.CIS);
  end;

 {Free CCCR}
 if MMC.CCCR <> nil then
  begin
   FreeMem(MMC.CCCR);
  end;

 {Destroy MMC}
 Result:=DeviceDestroy(@MMC.Device);
end;

{==============================================================================}

function MMCDeviceRegister(MMC:PMMCDevice):LongWord;
{Register a new MMC in the MMC table}
var
 MMCId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;
 if MMC.MMCId <> DEVICE_ID_ANY then Exit;
 if MMC.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check MMC}
 Result:=ERROR_ALREADY_EXISTS;
 if MMCDeviceCheck(MMC) = MMC then Exit;

 {Check State}
 if MMC.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert MMC}
 if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update MMC}
    MMCId:=0;
    while MMCDeviceFind(MMCId) <> nil do
     begin
      Inc(MMCId);
     end;
    MMC.MMCId:=MMCId;

    {Update Device}
    MMC.Device.DeviceName:=MMC_NAME_PREFIX + IntToStr(MMC.MMCId);
    MMC.Device.DeviceClass:=DEVICE_CLASS_MMC;

    {Register Device}
    Result:=DeviceRegister(@MMC.Device);
    if Result <> ERROR_SUCCESS then
     begin
      MMC.MMCId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Check Storage}
    if MMC.Storage <> nil then
     begin
      {Register Storage}
      Result:=StorageDeviceRegister(MMC.Storage); {Already a PStorageDevice}
      if Result <> ERROR_SUCCESS then
       begin
        DeviceDeregister(@MMC.Device);
        MMC.MMCId:=DEVICE_ID_ANY;
        Exit;
       end;
     end;

    {Link MMC}
    if MMCDeviceTable = nil then
     begin
      MMCDeviceTable:=MMC;
     end
    else
     begin
      MMC.Next:=MMCDeviceTable;
      MMCDeviceTable.Prev:=MMC;
      MMCDeviceTable:=MMC;
     end;

    {Increment Count}
    Inc(MMCDeviceTableCount);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(MMCDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MMCDeviceDeregister(MMC:PMMCDevice):LongWord;
{Deregister a MMC from the MMC table}
var
 Prev:PMMCDevice;
 Next:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;
 if MMC.MMCId = DEVICE_ID_ANY then Exit;
 if MMC.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check MMC}
 Result:=ERROR_NOT_FOUND;
 if MMCDeviceCheck(MMC) <> MMC then Exit;

 {Check State}
 if MMC.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove MMC}
 if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Storage}
    if MMC.Storage <> nil then
     begin
      {Deregister Storage}
      Result:=StorageDeviceDeregister(MMC.Storage); {Already a PStorageDevice}
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Deregister Device}
    Result:=DeviceDeregister(@MMC.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink MMC}
    Prev:=MMC.Prev;
    Next:=MMC.Next;
    if Prev = nil then
     begin
      MMCDeviceTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(MMCDeviceTableCount);

    {Update MMC}
    MMC.MMCId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(MMCDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MMCDeviceFind(MMCId:LongWord):PMMCDevice;
var
 MMC:PMMCDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if MMCId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get MMC}
    MMC:=MMCDeviceTable;
    while MMC <> nil do
     begin
      {Check State}
      if MMC.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if MMC.MMCId = MMCId then
         begin
          Result:=MMC;
          Exit;
         end;
       end;

       {Get Next}
      MMC:=MMC.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(MMCDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function MMCDeviceFindByDevice(Device:PDevice):PMMCDevice;
{Find an MMC/SD device by the matching DeviceData property}
{Device: The device entry to match with the DeviceData value}
{Return: The MMC/SD device matched or nil if none found}
var
 MMC:PMMCDevice;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get MMC}
    MMC:=MMCDeviceTable;
    while MMC <> nil do
     begin
      {Check State}
      if MMC.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Device}
        if MMC.Device.DeviceData = Device then
         begin
          Result:=MMC;
          Exit;
         end;
       end;

       {Get Next}
      MMC:=MMC.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(MMCDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function MMCDeviceFindByName(const Name:String):PMMCDevice; inline;
begin
 {}
 Result:=PMMCDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function MMCDeviceFindByDescription(const Description:String):PMMCDevice; inline;
begin
 {}
 Result:=PMMCDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function MMCDeviceEnumerate(Callback:TMMCEnumerate;Data:Pointer):LongWord;
var
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    Result:=ERROR_OPERATION_FAILED;

    {Get MMC}
    MMC:=MMCDeviceTable;
    while MMC <> nil do
     begin
      {Check State}
      if MMC.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(MMC,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      MMC:=MMC.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(MMCDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MMCDeviceNotification(MMC:PMMCDevice;Callback:TMMCNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_MMC,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check MMC}
   if MMC.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@MMC.Device,DEVICE_CLASS_MMC,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{SD Functions}
function SDDeviceSwitch(MMC:PMMCDevice;Mode,Group:Integer;Value:Byte;Buffer:Pointer):LongWord;
{See: 4.3.10 of SD Physical Layer Simplified Specification V4.10}
{Buffer must point to a 64 byte buffer for Switch Status information}
var
 Status:LongWord;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Switch');
 {$ENDIF}

 {Check Buffer}
 if Buffer = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SD_CMD_SWITCH;
 Command.Argument:=(Mode shl 31) or $00FFFFFF;
 Command.Argument:=Command.Argument and not($F shl (Group * 4));
 Command.Argument:=Command.Argument or Value shl (Group * 4);
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 Command.Data:=@Data;

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 Data.Data:=Buffer;
 Data.Flags:=MMC_DATA_READ;
 Data.BlockSize:=64;
 Data.BlockCount:=1;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sd_switch in U-Boot  mmc.c
 //See: mmc_sd_switch in \drivers\mmc\core\sd_ops.c
end;

{==============================================================================}

function SDDeviceSwitchHighspeed(MMC:PMMCDevice):LongWord;
var
 Count:Integer;
 Status:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Switch Highspeed');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Spec Version}
 if MMC.SDConfigurationData.SpecVersion < SD_SCR_SPEC_VER_1 then
  begin
   Result:=MMC_STATUS_UNSUPPORTED_REQUEST;
   Exit;
  end;

 {Check Command Class}
 if (MMC.CardSpecificData.CCC and MMC_CCC_SWITCH) = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SD Switch Highspeed card does not support Switch Class');

   Result:=MMC_STATUS_UNSUPPORTED_REQUEST;
   Exit;
  end;

 {Check Host Capability}
 if (SDHCI.Capabilities and MMC_CAP_SD_HIGHSPEED) = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SD Switch Highspeed host does not support Highspeed');

   Result:=MMC_STATUS_UNSUPPORTED_REQUEST;
   Exit;
  end;

 {Check Card Support}
 if (MMC.SDSwitchData.Group1Support and SD_SWITCH_GROUP1_HS) = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SD Switch Highspeed card does not support Highspeed');

   Result:=MMC_STATUS_UNSUPPORTED_REQUEST;
   Exit;
  end;

 {Set the Switch function}
 Status:=SDDeviceSwitch(MMC,SD_SWITCH_MODE_SWITCH,SD_SWITCH_FUNCTION_GROUP_ACCESS,SD_SWITCH_ACCESS_MODE_HS,@MMC.SDSwitch);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 for Count:=0 to 15 do
  begin
   MMC.SDSwitch[Count]:=LongWordBEtoN(MMC.SDSwitch[Count]);
  end;

 {Decode Response}
 Status:=SDDeviceDecodeSDSwitch(MMC);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Response}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 if MMC.SDSwitchData.Group1Selection <> SD_SWITCH_ACCESS_MODE_HS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SD Switch Highspeed failed to switch card to Highspeed');

   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_sd_switch_hs in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceSetBusWidth(MMC:PMMCDevice;Width:LongWord):LongWord;
{See: Table 4-30 in Section 4.7.4 of SD Physical Layer Simplified Specification V4.10}
var
 Status:LongWord;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Set Bus Width (Width=' + IntToStr(Width) + ')');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SD_CMD_APP_SET_BUS_WIDTH;
 Command.Argument:=0;
 case Width of
  MMC_BUS_WIDTH_1:Command.Argument:=SD_BUS_WIDTH_1;
  MMC_BUS_WIDTH_4:Command.Argument:=SD_BUS_WIDTH_4;
  else
   begin
    Exit;
   end;
 end;
 Command.ResponseType:=MMC_RSP_R1;
 Command.Data:=nil;

 {Send Command}
 Status:=SDDeviceSendApplicationCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_app_set_bus_width in \drivers\mmc\core\sd_ops.c
end;

{==============================================================================}

function SDDeviceSendInterfaceCondition(MMC:PMMCDevice):LongWord;
{See: 4.3.13 of SD Physical Layer Simplified Specification V4.10

 CMD8 (SEND_IF_COND) must be invoked to support SD 2.0 cards
 The card must be in Idle State before issuing this command

 This command will fail harmlessly for SD 1.0 cards
}
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send Interface Condition');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SD_CMD_SEND_IF_COND;
 Command.Argument:=SD_SEND_IF_COND_CHECK_PATTERN;
 if (SDHCI.Voltages and SD_SEND_IF_COND_VOLTAGE_MASK) <> 0 then
  begin
   {Set bit 8 if the host supports voltages between 2.7 and 3.6 V}
   Command.Argument:=(1 shl 8) or SD_SEND_IF_COND_CHECK_PATTERN;
  end;
 Command.ResponseType:=MMC_RSP_SPI_R7 or MMC_RSP_R7;
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Response}
 if SDHCIIsSPI(SDHCI) then
  begin
   if (Command.Response[1] and $FF) <> SD_SEND_IF_COND_CHECK_PATTERN then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SD Send Interface Condition failure (Response=' + IntToHex(Command.Response[1] and $FF,8) + ')');

     Exit;
    end;

   {Get Response}
   MMC.InterfaceCondition:=Command.Response[1];
  end
 else
  begin
   if (Command.Response[0] and $FF) <> SD_SEND_IF_COND_CHECK_PATTERN then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SD Send Interface Condition failure (Response=' + IntToHex(Command.Response[0] and $FF,8) + ')');

     Exit;
    end;

   {Get Response}
   MMC.InterfaceCondition:=Command.Response[0];
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_send_if_cond in U-Boot mmc.c
 //See: mmc_send_if_cond in \drivers\mmc\core\sd_ops.c
end;

{==============================================================================}

function SDDeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord;
{See: 4.2.3.1 of SD Physical Layer Simplified Specification V4.10}
var
 Status:LongWord;
 Timeout:Integer;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send Operation Condition');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SD_CMD_APP_SEND_OP_COND;
 Command.Argument:=0;
 if not(Probe) then
  begin
   if SDHCIIsSPI(SDHCI) then
    begin
     {SPI only defines one bit}
     Command.Argument:=(MMC.OperationCondition and SD_OCR_CCS);
    end
   else
    begin
     Command.Argument:=MMC.OperationCondition;

     //To Do //Need to select a voltage that is compatible between the card and the host
             //Use SDHCI.Volatages to select from MMC.OperationCondition
             //See: mmc_select_voltage

     {Set the Voltage bits in the OCR if not doing a probe operation}
     //Command.Argument:=(SDHCI.Voltages and SD_SEND_OP_COND_VOLTAGE_MASK);
    end;
  end;
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R3;
 Command.Data:=nil;

 {Setup Timeout}
 Timeout:=100;
 while Timeout > 0 do
  begin
   {Send Command}
   Status:=SDDeviceSendApplicationCommand(MMC,@Command);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Single pass only on probe}
   if Probe then Break;

   {Wait until reset completes}
   if SDHCIIsSPI(SDHCI) then
    begin
     if (Command.Response[0] and MMC_RSP_R1_SPI_IDLE) = 0 then Break;
    end
   else
    begin
     if (Command.Response[0] and MMC_OCR_BUSY) <> 0 then Break;
    end;

   Dec(Timeout);
   if Timeout = 0 then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SD Send Operation Condition Busy Status Timeout');

     Exit;
    end;
   MillisecondDelay(10);
  end;

 {Get Response}
 if not(SDHCIIsSPI(SDHCI)) then
  begin
   MMC.OperationCondition:=Command.Response[0];

   if not(Probe) and ((MMC.OperationCondition and SD_OCR_CCS) = SD_OCR_CCS) then
    begin
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_HIGH_CAPACITY);

     {$IFDEF MMC_DEBUG}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send Operation Condition (Flags=MMC_FLAG_HIGH_CAPACITY)');
     {$ENDIF}
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sd_send_op_cond in U-Boot mmc.c
 //See: mmc_send_app_op_cond in \drivers\mmc\core\sd_ops.c
end;

{==============================================================================}

function SDDeviceGetCardSpecific(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Get Card Specific');
 {$ENDIF}

 {Get Card Specific}
 Result:=MMCDeviceSendCardSpecific(MMC);
 if Result <> MMC_STATUS_SUCCESS then
  begin
   Exit;
  end;

 {Decode Card Specific}
 Result:=SDDeviceDecodeCardSpecific(MMC);
 if Result <> MMC_STATUS_SUCCESS then
  begin
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_sd_get_csd in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceDecodeCardSpecific(MMC:PMMCDevice):LongWord;
{Given a 128-bit response, decode to our card CSD structure}
var
 NSAC:Byte;
 TAACUnit:Byte;
 TAACValue:Byte;
 TranSpeedUnit:Byte;
 TranSpeedValue:Byte;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Decode Card Specific');
 {$ENDIF}

 {Decode Card Specific}
 MMC.CardSpecificData.CSDStructure:=SDGetCSDValue(MMC,0,MMC_CSD_STRUCTURE);

 {Check Version}
 case MMC.CardSpecificData.CSDStructure of
  SD_CSD_STRUCT_VER_1_0:begin
    {Calculate Data Access Time}
    TAACUnit:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TAAC_UNIT);
    TAACValue:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TAAC_VALUE);
    MMC.CardSpecificData.DataAccessTime:=((MMC_CSD_TAAC_UNITS[TAACUnit] * MMC_CSD_TAAC_VALUES[TAACValue]) + 9) div 10;

    {Calculate Data Access Clocks}
    NSAC:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_NSAC);
    MMC.CardSpecificData.DataAccessClocks:=NSAC * 100;

    {Calculate Data Transfer Rate}
    TranSpeedUnit:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TRAN_SPEED_UNIT);
    TranSpeedValue:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TRAN_SPEED_VALUE);
    MMC.CardSpecificData.DataTransferRate:=MMC_CSD_TRAN_SPEED_UNITS[TranSpeedUnit] * MMC_CSD_TRAN_SPEED_VALUES[TranSpeedValue];

    {Get Card Data}
    MMC.CardSpecificData.CCC:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_CCC);
    MMC.CardSpecificData.ReadBlockLength:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_READ_BL_LEN);
    MMC.CardSpecificData.ReadBlockPartial:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_READ_BL_PARTIAL) = 1);
    MMC.CardSpecificData.WriteBlockMisalign:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_WRITE_BLK_MISALIGN) = 1);
    MMC.CardSpecificData.ReadBlockMisalign:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_READ_BLK_MISALIGN) = 1);
    MMC.CardSpecificData.DSRImplemented:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_DSR_IMP) = 1);
    MMC.CardSpecificData.DeviceSize:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_C_SIZE);
    MMC.CardSpecificData.VDDReadCurrentMin:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_VDD_R_CURR_MIN);
    MMC.CardSpecificData.VDDReadCurrentMax:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_VDD_R_CURR_MAX);
    MMC.CardSpecificData.VDDWriteCurrentMin:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_VDD_W_CURR_MIN);
    MMC.CardSpecificData.VDDWriteCurrentMax:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_VDD_W_CURR_MAX);
    MMC.CardSpecificData.DeviceSizeMultiplier:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_C_SIZE_MULT);
    MMC.CardSpecificData.Erase.SD.EraseBlockEnable:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_ERASE_BLK_EN) = 1);
    MMC.CardSpecificData.Erase.SD.SectorSize:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_SECTOR_SIZE);
    MMC.CardSpecificData.WriteProtectGroupSize:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_WP_GRP_SIZE);
    MMC.CardSpecificData.WriteProtectGroupEnable:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_WP_GRP_ENABLE) = 1);
    MMC.CardSpecificData.ReadToWriteFactor:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_R2W_FACTOR);
    MMC.CardSpecificData.WriteBlockLength:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_WRITE_BL_LEN);
    MMC.CardSpecificData.WriteBlockPartial:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_WRITE_BL_PARTIAL) = 1);
    MMC.CardSpecificData.FileFormatGroup:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_FILE_FORMAT_GRP);
    MMC.CardSpecificData.CopyFlag:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_COPY) = 1);
    MMC.CardSpecificData.PermanentWriteProtect:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_PERM_WRITE_PROTECT) = 1);
    MMC.CardSpecificData.TemporaryWriteProtect:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TMP_WRITE_PROTECT) = 1);
    MMC.CardSpecificData.FileFormat:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_FILE_FORMAT);
    MMC.CardSpecificData.CRC:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_CRC);

    {Calculate Block Size}
    MMC.CardSpecificData.BlockSize:=(1 shl MMC.CardSpecificData.ReadBlockLength);

    {Calculate Block Count}
    MMC.CardSpecificData.BlockCount:=(MMC.CardSpecificData.DeviceSize + 1) shl (MMC.CardSpecificData.DeviceSizeMultiplier + 2);

    {Calculate Block Shift}
    MMC.CardSpecificData.BlockShift:=MMC.CardSpecificData.ReadBlockLength;

    {Calculate Erase Size}
    if MMC.CardSpecificData.Erase.SD.EraseBlockEnable then
     begin
      MMC.CardSpecificData.EraseSize:=1;
     end
    else if MMC.CardSpecificData.WriteBlockLength >= 9 then
     begin
      MMC.CardSpecificData.EraseSize:=MMC.CardSpecificData.Erase.SD.SectorSize + 1;
      MMC.CardSpecificData.EraseSize:=MMC.CardSpecificData.EraseSize shl (MMC.CardSpecificData.WriteBlockLength - 9);
     end;

    {Normalize Block Size, Count and Shift (See: Section 4.3.2 of Physical Layer Simplified Specification Version 4.10)}
    if MMC.CardSpecificData.ReadBlockLength > SD_DEFAULT_BLOCKSHIFT then
     begin
      {Normalize Block Size}
      MMC.CardSpecificData.BlockSize:=SD_DEFAULT_BLOCKSIZE;

      {Normalize Block Count}
      MMC.CardSpecificData.BlockCount:=(MMC.CardSpecificData.BlockCount shl (MMC.CardSpecificData.ReadBlockLength - SD_DEFAULT_BLOCKSHIFT));

      {Normalize Block Shift}
      MMC.CardSpecificData.BlockShift:=SD_DEFAULT_BLOCKSHIFT;
     end;

    Result:=MMC_STATUS_SUCCESS;
   end;
  SD_CSD_STRUCT_VER_2_0:begin
    {In CSD version 2 (for SDHC/SDXC) many fields are now fixed values. Supply specified values to avoid issues with buggy cards}
    MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_BLOCK_ADDRESSED);

    {$IFDEF MMC_DEBUG}
    if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Decode Card Specific (Flags=MMC_FLAG_BLOCK_ADDRESSED)');
    {$ENDIF}

    {Data Access Time / Data Access Clocks}
    MMC.CardSpecificData.DataAccessTime:=0;   {Unused}
    MMC.CardSpecificData.DataAccessClocks:=0; {Unused}

    {Calculate Data Transfer Rate}
    TranSpeedUnit:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TRAN_SPEED_UNIT);
    TranSpeedValue:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TRAN_SPEED_VALUE);
    MMC.CardSpecificData.DataTransferRate:=MMC_CSD_TRAN_SPEED_UNITS[TranSpeedUnit] * MMC_CSD_TRAN_SPEED_VALUES[TranSpeedValue];

    {Get Card Data}
    MMC.CardSpecificData.CCC:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_CCC);
    MMC.CardSpecificData.ReadBlockLength:=9;
    MMC.CardSpecificData.ReadBlockPartial:=False;
    MMC.CardSpecificData.WriteBlockMisalign:=False;
    MMC.CardSpecificData.ReadBlockMisalign:=False;
    MMC.CardSpecificData.DSRImplemented:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_DSR_IMP) = 1);
    MMC.CardSpecificData.DeviceSize:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_C_SIZE);
    MMC.CardSpecificData.Erase.SD.EraseBlockEnable:=True;
    MMC.CardSpecificData.Erase.SD.SectorSize:=127;
    MMC.CardSpecificData.WriteProtectGroupSize:=0;
    MMC.CardSpecificData.WriteProtectGroupEnable:=False;
    MMC.CardSpecificData.ReadToWriteFactor:=4; {Unused}
    MMC.CardSpecificData.WriteBlockLength:=9;
    MMC.CardSpecificData.WriteBlockPartial:=False;
    MMC.CardSpecificData.FileFormatGroup:=0;
    MMC.CardSpecificData.CopyFlag:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_COPY) = 1);
    MMC.CardSpecificData.PermanentWriteProtect:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_PERM_WRITE_PROTECT) = 1);
    MMC.CardSpecificData.TemporaryWriteProtect:=(SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_TMP_WRITE_PROTECT) = 1);
    MMC.CardSpecificData.FileFormat:=0;
    MMC.CardSpecificData.CRC:=SDGetCSDValue(MMC,MMC.CardSpecificData.CSDStructure,MMC_CSD_CRC);

    {Calculate Block Size}
    MMC.CardSpecificData.BlockSize:=(1 shl MMC.CardSpecificData.ReadBlockLength);

    {Calculate Block Count}
    MMC.CardSpecificData.BlockCount:=(MMC.CardSpecificData.DeviceSize + 1) shl 10;
    if MMC.CardSpecificData.DeviceSize >= $00FFFF then
     begin
      MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_EXT_CAPACITY);

      {$IFDEF MMC_DEBUG}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Decode Card Specific (Flags=MMC_FLAG_EXT_CAPACITY)');
      {$ENDIF}
     end;

    {Calculate Block Shift}
    MMC.CardSpecificData.BlockShift:=MMC.CardSpecificData.ReadBlockLength;

    {Calculate Erase Size}
    MMC.CardSpecificData.EraseSize:=1;

    Result:=MMC_STATUS_SUCCESS;
   end;
 end;

 {Log Card Specific}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' SD Card Specific:');
   MMCLogDebug(nil,'  CSDStructure = ' + IntToStr(MMC.CardSpecificData.CSDStructure));
   if Result = MMC_STATUS_SUCCESS then
    begin
     {Card Data}
     MMCLogDebug(nil,'  CCC = ' + IntToHex(MMC.CardSpecificData.CCC,8));
     MMCLogDebug(nil,'  ReadBlockLength = ' + IntToStr(MMC.CardSpecificData.ReadBlockLength));
     MMCLogDebug(nil,'  ReadBlockPartial = ' + BoolToStr(MMC.CardSpecificData.ReadBlockPartial,True));
     MMCLogDebug(nil,'  WriteBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.WriteBlockMisalign,True));
     MMCLogDebug(nil,'  ReadBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.ReadBlockMisalign,True));
     MMCLogDebug(nil,'  DSRImplemented = ' + BoolToStr(MMC.CardSpecificData.DSRImplemented,True));
     MMCLogDebug(nil,'  DeviceSize = ' + IntToHex(MMC.CardSpecificData.DeviceSize,8));
     MMCLogDebug(nil,'  VDDReadCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMin));
     MMCLogDebug(nil,'  VDDReadCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMax));
     MMCLogDebug(nil,'  VDDWriteCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMin));
     MMCLogDebug(nil,'  VDDWriteCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMax));
     MMCLogDebug(nil,'  DeviceSizeMultiplier = ' + IntToStr(MMC.CardSpecificData.DeviceSizeMultiplier));
     MMCLogDebug(nil,'  EraseBlockEnable = ' + BoolToStr(MMC.CardSpecificData.Erase.SD.EraseBlockEnable,True));
     MMCLogDebug(nil,'  EraseSectorSize = ' + IntToStr(MMC.CardSpecificData.Erase.SD.SectorSize));
     MMCLogDebug(nil,'  WriteProtectGroupSize = ' + IntToStr(MMC.CardSpecificData.WriteProtectGroupSize));
     MMCLogDebug(nil,'  WriteProtectGroupEnable = ' + BoolToStr(MMC.CardSpecificData.WriteProtectGroupEnable,True));
     MMCLogDebug(nil,'  ReadToWriteFactor = ' + IntToStr(MMC.CardSpecificData.ReadToWriteFactor));
     MMCLogDebug(nil,'  WriteBlockLength = ' + IntToStr(MMC.CardSpecificData.WriteBlockLength));
     MMCLogDebug(nil,'  WriteBlockPartial = ' + BoolToStr(MMC.CardSpecificData.WriteBlockPartial,True));
     MMCLogDebug(nil,'  FileFormatGroup = ' + IntToStr(MMC.CardSpecificData.FileFormatGroup));
     MMCLogDebug(nil,'  CopyFlag = ' + BoolToStr(MMC.CardSpecificData.CopyFlag,True));
     MMCLogDebug(nil,'  PermanentWriteProtect = ' + BoolToStr(MMC.CardSpecificData.PermanentWriteProtect,True));
     MMCLogDebug(nil,'  TemporaryWriteProtect = ' + BoolToStr(MMC.CardSpecificData.TemporaryWriteProtect,True));
     MMCLogDebug(nil,'  FileFormat = ' + IntToStr(MMC.CardSpecificData.FileFormat));
     MMCLogDebug(nil,'  CRC = ' + IntToStr(MMC.CardSpecificData.CRC));
     {Calculated Values}
     MMCLogDebug(nil,'  DataAccessTime = ' + IntToStr(MMC.CardSpecificData.DataAccessTime));
     MMCLogDebug(nil,'  DataAccessClocks = ' + IntToStr(MMC.CardSpecificData.DataAccessClocks));
     MMCLogDebug(nil,'  DataTransferRate = ' + IntToStr(MMC.CardSpecificData.DataTransferRate));
     MMCLogDebug(nil,'  EraseSize = ' + IntToStr(MMC.CardSpecificData.EraseSize));
     MMCLogDebug(nil,'  BlockSize = ' + IntToStr(MMC.CardSpecificData.BlockSize));
     MMCLogDebug(nil,'  BlockCount = ' + IntToStr(MMC.CardSpecificData.BlockCount));
     MMCLogDebug(nil,'  BlockShift = ' + IntToStr(MMC.CardSpecificData.BlockShift));
    end;
  end;
 {$ENDIF}

 //See: mmc_decode_csd in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceGetCardIdentification(MMC:PMMCDevice):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Get Card Identification');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Set the Card to Idle State}
 MMCDeviceGoIdle(MMC);

 {Check Interface Condition}
 if SDDeviceSendInterfaceCondition(MMC) = MMC_STATUS_SUCCESS then
  begin
   {If the card supports SEND_IF_COND the ensure we the the SD_OCR_CCS bit to indicate we can handle SDHC cards}
   MMC.OperationCondition:=MMC.OperationCondition or SD_OCR_CCS;
  end;

 {Check for Host UHS support}
 //To Do //mmc_host_uhs
  {If the host supports one of UHS-I modes, request the card to switch to 1.8V signaling level. If the card has failed repeatedly to switch however, skip this}
  //MMC.OperationCondition:=MMC.OperationCondition or SD_OCR_S18A;

 {Check for Host Max Current}
 //To Do //sd_get_host_max_current
  {If the host can supply more than 150mA at current voltage, XPC should be set to 1}
  //MMC.OperationCondition:=MMC.OperationCondition or SD_OCR_XPC;

 {Get Operation Condition}
 Result:=SDDeviceSendOperationCondition(MMC,False);
 if Result <> MMC_STATUS_SUCCESS then
  begin
   Exit;
  end;

 {Check for SPI}
 if not(SDHCIIsSPI(SDHCI)) then
  begin
   {If CCS and S18A in the response is set, start Signal Voltage Switch procedure. SPI mode doesn't support CMD11}
   if (MMC.OperationCondition and (SD_OCR_CCS or SD_OCR_S18A)) = (SD_OCR_CCS or SD_OCR_S18A) then
    begin
     //To Do //mmc_set_uhs_voltage
    end;
  end;

 {Check for SPI}
 if SDHCIIsSPI(SDHCI) then
  begin
   {Get Card Identification}
   Result:=MMCDeviceSendCardIdentification(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
  end
 else
  begin
   {Get All Card Identification}
   Result:=MMCDeviceSendAllCardIdentification(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_sd_get_cid in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceDecodeCardIdentification(MMC:PMMCDevice):LongWord;
{Given a 128-bit response, decode to our card CID structure}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Decode Card Identification');
 {$ENDIF}

 {Decode Card Identification}
 MMC.CardIdentificationData.ManufacturerId:=SDGetCIDValue(MMC,MMC_CID_MID);
 MMC.CardIdentificationData.OEMId:=SDGetCIDValue(MMC,MMC_CID_OID);
 MMC.CardIdentificationData.ProductName[0]:=Char(SDGetCIDValue(MMC,MMC_CID_PNM0));
 MMC.CardIdentificationData.ProductName[1]:=Char(SDGetCIDValue(MMC,MMC_CID_PNM1));
 MMC.CardIdentificationData.ProductName[2]:=Char(SDGetCIDValue(MMC,MMC_CID_PNM2));
 MMC.CardIdentificationData.ProductName[3]:=Char(SDGetCIDValue(MMC,MMC_CID_PNM3));
 MMC.CardIdentificationData.ProductName[4]:=Char(SDGetCIDValue(MMC,MMC_CID_PNM4));
 MMC.CardIdentificationData.ProductName[5]:=#0; {Null terminator}
 MMC.CardIdentificationData.ProductRevision:=SDGetCIDValue(MMC,MMC_CID_PRV);
 MMC.CardIdentificationData.HardwareRevision:=SDGetCIDValue(MMC,MMC_CID_HRV);
 MMC.CardIdentificationData.FirmwareRevision:=SDGetCIDValue(MMC,MMC_CID_FRV);
 MMC.CardIdentificationData.ProductSerialNumber:=SDGetCIDValue(MMC,MMC_CID_PSN);
 MMC.CardIdentificationData.ManufacturingMonth:=SDGetCIDValue(MMC,MMC_CID_MDT_MONTH);
 MMC.CardIdentificationData.ManufacturingYear:=SDGetCIDValue(MMC,MMC_CID_MDT_YEAR);
 MMC.CardIdentificationData.CRC:=SDGetCIDValue(MMC,MMC_CID_CRC);

 {Log Card Identification}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' SD Card Identification:');
   MMCLogDebug(nil,'  ManufacturerId = ' + IntToStr(MMC.CardIdentificationData.ManufacturerId));
   MMCLogDebug(nil,'  OEMId = ' + IntToStr(MMC.CardIdentificationData.OEMId));
   MMCLogDebug(nil,'  ProductName = ' + MMC.CardIdentificationData.ProductName);
   MMCLogDebug(nil,'  ProductRevision = ' + IntToStr(MMC.CardIdentificationData.ProductRevision));
   MMCLogDebug(nil,'  HardwareRevision = ' + IntToStr(MMC.CardIdentificationData.HardwareRevision));
   MMCLogDebug(nil,'  FirmwareRevision = ' + IntToStr(MMC.CardIdentificationData.FirmwareRevision));
   MMCLogDebug(nil,'  ProductSerialNumber = ' + IntToStr(MMC.CardIdentificationData.ProductSerialNumber));
   MMCLogDebug(nil,'  ManufacturingMonth = ' + IntToStr(MMC.CardIdentificationData.ManufacturingMonth));
   MMCLogDebug(nil,'  ManufacturingYear = ' + IntToStr(MMC.CardIdentificationData.ManufacturingYear));
   MMCLogDebug(nil,'  CRC = ' + IntToStr(MMC.CardIdentificationData.CRC));
  end;
 {$ENDIF}

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_decode_cid in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceSendSDStatus(MMC:PMMCDevice):LongWord;
var
 Count:Integer;
 Status:LongWord;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send SD Status');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SD_CMD_APP_SD_STATUS;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_SPI_R2 or MMC_RSP_R1;
 Command.Data:=@Data;

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 Data.Data:=@MMC.SDStatus;
 Data.Flags:=MMC_DATA_READ;
 Data.BlockSize:=64;
 Data.BlockCount:=1;

 {Send Command}
 Status:=SDDeviceSendApplicationCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 for Count:=0 to 15 do
  begin
   MMC.SDStatus[Count]:=LongWordBEtoN(MMC.SDStatus[Count]);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_read_ssr in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceDecodeSDStatus(MMC:PMMCDevice):LongWord;
var
 AUSize:Byte;
 UHSAUSize:Byte;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Decode SD Status');
 {$ENDIF}

 {Decode SD Status}
 {Get Card Data}
 MMC.SDStatusData.BusWidth:=SDGetSSRValue(MMC,SD_SSR_DAT_BUS_WIDTH);
 MMC.SDStatusData.SecuredMode:=(SDGetSSRValue(MMC,SD_SSR_SECURED_MODE) = 1);
 MMC.SDStatusData.CardType:=SDGetSSRValue(MMC,SD_SSR_SD_CARD_TYPE);
 MMC.SDStatusData.ProtectedSize:=SDGetSSRValue(MMC,SD_SSR_SIZE_OF_PROTECTED_AREA);
 MMC.SDStatusData.SpeedClass:=SDGetSSRValue(MMC,SD_SSR_SPEED_CLASS);
 MMC.SDStatusData.PerformanceMove:=SDGetSSRValue(MMC,SD_SSR_PERFORMANCE_MOVE);
 MMC.SDStatusData.EraseSize:=SDGetSSRValue(MMC,SD_SSR_ERASE_SIZE);
 MMC.SDStatusData.EraseTimeout:=SDGetSSRValue(MMC,SD_SSR_ERASE_TIMEOUT);
 MMC.SDStatusData.EraseOffset:=SDGetSSRValue(MMC,SD_SSR_ERASE_OFFSET);
 MMC.SDStatusData.UHSSpeedGrade:=SDGetSSRValue(MMC,SD_SSR_UHS_SPEED_GRADE);

 {Calculate Allocation Unit Size}
 AUSize:=SDGetSSRValue(MMC,SD_SSR_AU_SIZE);
 MMC.SDStatusData.AllocationUnitSize:=SD_SSR_AU_SIZE_VALUES[AUSize];

 {Calculate UHS Allocation Unit Size}
 UHSAUSize:=SDGetSSRValue(MMC,SD_SSR_UHS_AU_SIZE);
 MMC.SDStatusData.UHSAllocationUnitSize:=SD_SSR_UHS_AU_SIZE_VALUES[UHSAUSize];

 {Log SD Status}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   {Card Data }
   MMCLogDebug(nil,' SD Status:');
   MMCLogDebug(nil,'  BusWidth = ' + IntToHex(MMC.SDStatusData.BusWidth,2));
   MMCLogDebug(nil,'  SecuredMode = ' + BoolToStr(MMC.SDStatusData.SecuredMode,True));
   MMCLogDebug(nil,'  CardType = ' + IntToHex(MMC.SDStatusData.CardType,4));
   MMCLogDebug(nil,'  ProtectedSize = ' + IntToStr(MMC.SDStatusData.ProtectedSize));
   MMCLogDebug(nil,'  SpeedClass = ' + IntToHex(MMC.SDStatusData.SpeedClass,2));
   MMCLogDebug(nil,'  PerformanceMove = ' + IntToStr(MMC.SDStatusData.PerformanceMove));
   MMCLogDebug(nil,'  EraseSize = ' + IntToStr(MMC.SDStatusData.EraseSize));
   MMCLogDebug(nil,'  EraseTimeout = ' + IntToStr(MMC.SDStatusData.EraseTimeout));
   MMCLogDebug(nil,'  EraseOffset = ' + IntToStr(MMC.SDStatusData.EraseOffset));
   MMCLogDebug(nil,'  UHSSpeedGrade = ' + IntToHex(MMC.SDStatusData.UHSSpeedGrade,2));
   {Calculated Values}
   MMCLogDebug(nil,'  AllocationUnitSize = ' + IntToStr(MMC.SDStatusData.AllocationUnitSize));
   MMCLogDebug(nil,'  UHSAllocationUnitSize = ' + IntToStr(MMC.SDStatusData.UHSAllocationUnitSize));
  end;
 {$ENDIF}

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_read_ssr in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceSendSDSwitch(MMC:PMMCDevice):LongWord;
var
 Count:Integer;
 Status:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send SD Switch');
 {$ENDIF}

 {Check Spec Version}
 if MMC.SDConfigurationData.SpecVersion < SD_SCR_SPEC_VER_1 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check Command Class}
 if (MMC.CardSpecificData.CCC and MMC_CCC_SWITCH) = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SD Send Switch card does not support Switch Class');

   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Read the Switch function Status}
 Status:=SDDeviceSwitch(MMC,SD_SWITCH_MODE_CHECK,SD_SWITCH_FUNCTION_GROUP_ACCESS,SD_SWITCH_ACCESS_MODE_DEF,@MMC.SDSwitch);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 for Count:=0 to 15 do
  begin
   MMC.SDSwitch[Count]:=LongWordBEtoN(MMC.SDSwitch[Count]);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_read_switch in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceDecodeSDSwitch(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Decode SD Switch');
 {$ENDIF}

 {Decode SD Switch}
 {Get Card Data}
 MMC.SDSwitchData.MaximumCurrent:=SDGetSwitchValue(MMC,SD_SWITCH_MAXIMUM_CURRENT);
 MMC.SDSwitchData.Group6Support:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP6_SUPPORT);
 MMC.SDSwitchData.Group5Support:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP5_SUPPORT);
 MMC.SDSwitchData.Group4Support:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP4_SUPPORT);
 MMC.SDSwitchData.Group3Support:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP3_SUPPORT);
 MMC.SDSwitchData.Group2Support:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP2_SUPPORT);
 MMC.SDSwitchData.Group1Support:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP1_SUPPORT);
 MMC.SDSwitchData.Group6Selection:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP6_SELECTION);
 MMC.SDSwitchData.Group5Selection:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP5_SELECTION);
 MMC.SDSwitchData.Group4Selection:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP4_SELECTION);
 MMC.SDSwitchData.Group3Selection:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP3_SELECTION);
 MMC.SDSwitchData.Group2Selection:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP2_SELECTION);
 MMC.SDSwitchData.Group1Selection:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP1_SELECTION);
 MMC.SDSwitchData.StructureVersion:=SDGetSwitchValue(MMC,SD_SWITCH_STRUCT_VERSION);
 if MMC.SDSwitchData.StructureVersion = SD_SWITCH_STRUCT_VER_1 then
  begin
   MMC.SDSwitchData.Group6BusyStatus:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP6_BUSY_STATUS);
   MMC.SDSwitchData.Group5BusyStatus:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP5_BUSY_STATUS);
   MMC.SDSwitchData.Group4BusyStatus:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP4_BUSY_STATUS);
   MMC.SDSwitchData.Group3BusyStatus:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP3_BUSY_STATUS);
   MMC.SDSwitchData.Group2BusyStatus:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP2_BUSY_STATUS);
   MMC.SDSwitchData.Group1BusyStatus:=SDGetSwitchValue(MMC,SD_SWITCH_GROUP1_BUSY_STATUS);
  end;

 {Log SD Switch}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   {Card Data }
   MMCLogDebug(nil,' SD Switch:');
   MMCLogDebug(nil,'  MaximumCurrent = ' + IntToStr(MMC.SDSwitchData.MaximumCurrent));
   MMCLogDebug(nil,'  Group6Support = ' + IntToHex(MMC.SDSwitchData.Group6Support,4));
   MMCLogDebug(nil,'  Group5Support = ' + IntToHex(MMC.SDSwitchData.Group5Support,4));
   MMCLogDebug(nil,'  Group4Support = ' + IntToHex(MMC.SDSwitchData.Group4Support,4));
   MMCLogDebug(nil,'  Group3Support = ' + IntToHex(MMC.SDSwitchData.Group3Support,4));
   MMCLogDebug(nil,'  Group2Support = ' + IntToHex(MMC.SDSwitchData.Group2Support,4));
   MMCLogDebug(nil,'  Group1Support = ' + IntToHex(MMC.SDSwitchData.Group1Support,4));
   MMCLogDebug(nil,'  Group6Selection = ' + IntToHex(MMC.SDSwitchData.Group6Selection,2));
   MMCLogDebug(nil,'  Group5Selection = ' + IntToHex(MMC.SDSwitchData.Group5Selection,2));
   MMCLogDebug(nil,'  Group4Selection = ' + IntToHex(MMC.SDSwitchData.Group4Selection,2));
   MMCLogDebug(nil,'  Group3Selection = ' + IntToHex(MMC.SDSwitchData.Group3Selection,2));
   MMCLogDebug(nil,'  Group2Selection = ' + IntToHex(MMC.SDSwitchData.Group2Selection,2));
   MMCLogDebug(nil,'  Group1Selection = ' + IntToHex(MMC.SDSwitchData.Group1Selection,2));
   MMCLogDebug(nil,'  StructureVersion = ' + IntToStr(MMC.SDSwitchData.StructureVersion));
   if MMC.SDSwitchData.StructureVersion = SD_SWITCH_STRUCT_VER_1 then
    begin
     MMCLogDebug(nil,'  Group6BusyStatus = ' + IntToHex(MMC.SDSwitchData.Group6BusyStatus,4));
     MMCLogDebug(nil,'  Group5BusyStatus = ' + IntToHex(MMC.SDSwitchData.Group5BusyStatus,4));
     MMCLogDebug(nil,'  Group4BusyStatus = ' + IntToHex(MMC.SDSwitchData.Group4BusyStatus,4));
     MMCLogDebug(nil,'  Group3BusyStatus = ' + IntToHex(MMC.SDSwitchData.Group3BusyStatus,4));
     MMCLogDebug(nil,'  Group2BusyStatus = ' + IntToHex(MMC.SDSwitchData.Group2BusyStatus,4));
     MMCLogDebug(nil,'  Group1BusyStatus = ' + IntToHex(MMC.SDSwitchData.Group1BusyStatus,4));
    end;
  end;
 {$ENDIF}

 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}

function SDDeviceSendSDConfiguration(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 Data:TMMCData;
 Command:TMMCCommand;
 SDConfiguration:array[0..1] of LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send SD Configuration');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SD_CMD_APP_SEND_SCR;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 Command.Data:=@Data;

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 Data.Data:=@SDConfiguration;
 Data.Flags:=MMC_DATA_READ;
 Data.BlockSize:=8;
 Data.BlockCount:=1;

 {Send Command}
 Status:=SDDeviceSendApplicationCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 MMC.SDConfiguration[2]:=LongWordBEtoN(SDConfiguration[0]);
 MMC.SDConfiguration[3]:=LongWordBEtoN(SDConfiguration[1]);

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_app_send_scr in \drivers\mmc\core\sd_ops.c
end;

{==============================================================================}

function SDDeviceDecodeSDConfiguration(MMC:PMMCDevice):LongWord;
{Given a 64-bit response, decode to our card SCR structure}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Decode SD Configuration');
 {$ENDIF}

 {Decode SD Configuration}
 MMC.SDConfigurationData.SCRStructure:=SDGetSCRValue(MMC,SD_SCR_STRUCTURE);

 {Check Version}
 case MMC.SDConfigurationData.SCRStructure of
  SD_SCR_STRUCT_VER_1_0:begin
    {Get Card Data}
    MMC.SDConfigurationData.SpecVersion:=SDGetSCRValue(MMC,SD_SCR_SD_SPEC);
    MMC.SDConfigurationData.DataAfterErase:=SDGetSCRValue(MMC,SD_SCR_DATA_STAT_AFTER_ERASE);
    MMC.SDConfigurationData.Security:=SDGetSCRValue(MMC,SD_SCR_SD_SECURITY);
    MMC.SDConfigurationData.BusWidths:=SDGetSCRValue(MMC,SD_SCR_SD_BUS_WIDTHS);
    if MMC.SDConfigurationData.SpecVersion = SD_SCR_SPEC_VER_2 then
     begin
      MMC.SDConfigurationData.SpecVersion3:=(SDGetSCRValue(MMC,SD_SCR_SD_SPEC3) = 1);
     end;
    MMC.SDConfigurationData.ExtendedSecurity:=SDGetSCRValue(MMC,SD_SCR_EX_SECURITY);
    if MMC.SDConfigurationData.SpecVersion3 then
     begin
      MMC.SDConfigurationData.SpecVersion4:=(SDGetSCRValue(MMC,SD_SCR_SD_SPEC4) = 1);
      MMC.SDConfigurationData.CommandSupport:=SDGetSCRValue(MMC,SD_SCR_CMD_SUPPORT);
     end;

    {Get Version}
    if MMC.SDConfigurationData.SpecVersion = SD_SCR_SPEC_VER_0 then
     begin
      MMC.Version:=SD_VERSION_1_0;
     end
    else if MMC.SDConfigurationData.SpecVersion = SD_SCR_SPEC_VER_1 then
     begin
      MMC.Version:=SD_VERSION_1_10;
     end
    else if MMC.SDConfigurationData.SpecVersion = SD_SCR_SPEC_VER_2 then
     begin
      MMC.Version:=SD_VERSION_2;
      if MMC.SDConfigurationData.SpecVersion3 then MMC.Version:=SD_VERSION_3;
      if MMC.SDConfigurationData.SpecVersion4 then MMC.Version:=SD_VERSION_4;
     end;

    {Calculate Erase Byte}
    if MMC.SDConfigurationData.DataAfterErase = 1 then
     begin
      MMC.SDConfigurationData.ErasedByte:=$FF;
     end
    else
     begin
      MMC.SDConfigurationData.ErasedByte:=$00;
     end;

    {Check CMD23 Support}
    if (MMC.SDConfigurationData.CommandSupport and SD_SCR_CMD23_SUPPORT) <> 0 then
     begin
      MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_SET_BLOCK_COUNT;
     end;

    Result:=MMC_STATUS_SUCCESS;
   end;
 end;

 {Log SD Configuration}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' SD Configuration:');
   MMCLogDebug(nil,'  SCRStructure = ' + IntToStr(MMC.SDConfigurationData.SCRStructure));
   if Result = MMC_STATUS_SUCCESS then
    begin
     {Card Data}
     MMCLogDebug(nil,'  SpecVersion = ' + IntToStr(MMC.SDConfigurationData.SpecVersion));
     MMCLogDebug(nil,'  DataAfterErase = ' + IntToStr(MMC.SDConfigurationData.DataAfterErase));
     MMCLogDebug(nil,'  Security = ' + IntToHex(MMC.SDConfigurationData.Security,8));
     MMCLogDebug(nil,'  BusWidths = ' + IntToHex(MMC.SDConfigurationData.BusWidths,8));
     MMCLogDebug(nil,'  SpecVersion3 = ' + BoolToStr(MMC.SDConfigurationData.SpecVersion3,True));
     MMCLogDebug(nil,'  ExtendedSecurity = ' + IntToHex(MMC.SDConfigurationData.ExtendedSecurity,8));
     MMCLogDebug(nil,'  SpecVersion4 = ' + BoolToStr(MMC.SDConfigurationData.SpecVersion4,True));
     MMCLogDebug(nil,'  CommandSupport = ' + IntToHex(MMC.SDConfigurationData.CommandSupport,8));
     {Calculated Values}
     MMCLogDebug(nil,'  ErasedByte = ' + IntToHex(MMC.SDConfigurationData.ErasedByte,2));
     MMCLogDebug(nil,'  Version = ' + SDVersionToString(MMC.Version));
     MMCLogDebug(nil,'  CMD23Support = ' + BoolToStr((MMC.Device.DeviceFlags and MMC_FLAG_SET_BLOCK_COUNT) <> 0,True));
    end;
  end;
 {$ENDIF}

 //See: mmc_decode_scr in \drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceSendRelativeAddress(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send Relative Address');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SD_CMD_SEND_RELATIVE_ADDR;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_R6; {Not valid in SPI mode}
 Command.Data:=nil;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Response}
 MMC.RelativeCardAddress:=(Command.Response[0] shr 16);
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send Relative Address (Address=' + IntToHex(MMC.RelativeCardAddress,8) + ')');
 {$ENDIF}

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_send_relative_addr in \drivers\mmc\core\sd_ops.c
end;

{==============================================================================}

function SDDeviceSendApplicationCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 ApplicationCommand:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Send Application Command');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Application Command}
 FillChar(ApplicationCommand,SizeOf(TMMCCommand),0);
 ApplicationCommand.Command:=MMC_CMD_APP_CMD;
 ApplicationCommand.Argument:=(MMC.RelativeCardAddress shl 16);
 ApplicationCommand.ResponseType:=MMC_RSP_SPI_R1 or MMC_RSP_R1;
 ApplicationCommand.Data:=nil;

 {Send Application Command}
 Status:=MMCDeviceSendCommand(MMC,@ApplicationCommand);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Response}
 if SDHCIIsSPI(SDHCI) then
  begin
   if (ApplicationCommand.Response[0] and MMC_RSP_R1_SPI_ILLEGAL_COMMAND) <> 0 then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SD Send Application Command Illegal Command');

     Command.Status:=MMC_STATUS_UNSUPPORTED_REQUEST;
     Exit;
    end;
  end
 else
  begin
   if (ApplicationCommand.Response[0] and MMC_RSP_R1_APP_CMD) = 0 then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SD Send Application Command Not Supported');

     Command.Status:=MMC_STATUS_UNSUPPORTED_REQUEST;
     Exit;
    end;
  end;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_wait_for_app_cmd in \drivers\mmc\core\sd_ops.c
end;

{==============================================================================}
{==============================================================================}
{SDIO Functions}
function SDIODeviceReset(MMC:PMMCDevice):LongWord;
{See: SDIO Simplified Specification V2.0, 4.4 Reset for SDIO}
var
 Abort:Byte;
 Status:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Reset');
 {$ENDIF}

 {Get Abort Value}
 Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_ABORT,0,@Abort);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Abort:=$08;
  end
 else
  begin
   Abort:=Abort or $08;
  end;

 {Set Abort Value}
 Result:=SDIODeviceReadWriteDirect(MMC,True,0,SDIO_CCCR_ABORT,Abort,nil);

 //See: sdio_reset in \drivers\mmc\core\sdio_ops.c
end;

{==============================================================================}

function SDIODeviceEnableWideBus(MMC:PMMCDevice):LongWord;
var
 Control:Byte;
 Status:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Enable Wide Bus');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Supported Bus Width}
 if (SDHCI.Capabilities and MMC_CAP_4_BIT_DATA) = 0 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check Card Common Control Registers (CCCR)}
 if MMC.CCCR.LowSpeed and not MMC.CCCR.WideBus then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Get Interface Controls}
 Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_IF,0,@Control);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Bus Width}
 if (Control and SDIO_BUS_WIDTH_MASK) = SDIO_BUS_WIDTH_RESERVED then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDIO_CCCR_IF value is invalid');
  end;

 {Enable 4-bit Bus}
 Control:=Control and not(SDIO_BUS_WIDTH_MASK);
 Control:=Control or SDIO_BUS_WIDTH_4BIT;

 {Set Interface Controls}
 Status:=SDIODeviceReadWriteDirect(MMC,True,0,SDIO_CCCR_IF,Control,nil);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check for SD Combo}
 if MMC.Device.DeviceType = MMC_TYPE_SD_COMBO then
  begin
   {Check supported Bus Width}
   if (MMC.SDConfigurationData.BusWidths and SD_SCR_BUS_WIDTH_4) <> 0 then
    begin
     {Set SD Bus Width}
     Status:=SDDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_4);
     if Status <> MMC_STATUS_SUCCESS then
      begin
       {Get Interface Controls}
       if SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_IF,0,@Control) = MMC_STATUS_SUCCESS then
        begin
         {Reset Bus Width}
         Control:=Control and not(SDIO_BUS_WIDTH_4BIT);
         Control:=Control or SDIO_BUS_ASYNC_INT;

         {Set Interface Controls}
         SDIODeviceReadWriteDirect(MMC,True,0,SDIO_CCCR_IF,Control,nil);
        end;

       Result:=Status;
       Exit;
      end;
    end;
  end;

 {Set MMC Bus Width}
 Result:=MMCDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_4);

 //See: sdio_enable_4bit_bus in \drivers\mmc\core\sdio.c
 //     sdio_enable_wide in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function SDIODeviceDisableWideBus(MMC:PMMCDevice):LongWord;
var
 Control:Byte;
 Status:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Disable Wide Bus');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Supported Bus Width}
 if (SDHCI.Capabilities and MMC_CAP_4_BIT_DATA) = 0 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Check for SD Combo}
 if MMC.Device.DeviceType = MMC_TYPE_SD_COMBO then
  begin
   {Check supported Bus Width}
   if (MMC.SDConfigurationData.BusWidths and SD_SCR_BUS_WIDTH_4) <> 0 then
    begin
     {Set SD Bus Width}
     Status:=SDDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_1);
     if Status <> MMC_STATUS_SUCCESS then
      begin
       Result:=Status;
       Exit;
      end;
    end;
  end;

 {Check Card Common Control Registers (CCCR)}
 if MMC.CCCR.LowSpeed and not MMC.CCCR.WideBus then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Get Interface Controls}
 Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_IF,0,@Control);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Bus Width}
 if (Control and SDIO_BUS_WIDTH_4BIT) = 0 then
  begin
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;

 {Disable 4-bit Bus}
 Control:=Control and not(SDIO_BUS_WIDTH_4BIT);
 Control:=Control or SDIO_BUS_ASYNC_INT;

 {Set Interface Controls}
 Status:=SDIODeviceReadWriteDirect(MMC,True,0,SDIO_CCCR_IF,Control,nil);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Set MMC Bus Width}
 Result:=MMCDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_1);

 //See: sdio_disable_4bit_bus in \drivers\mmc\core\sdio.c
 //     sdio_disable_wide in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function SDIODeviceEnableHighspeed(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Enable Highspeed');
 {$ENDIF}

 Result:=SDIODeviceSwitchHighspeed(MMC,True);
 if Result <> MMC_STATUS_SUCCESS then Exit;

 if MMC.Device.DeviceType = MMC_TYPE_SDIO then Exit;

 Result:=SDDeviceSwitchHighspeed(MMC);
 if Result <> MMC_STATUS_SUCCESS then SDIODeviceSwitchHighspeed(MMC,False);

 //See: sdio_enable_hs in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function SDIODeviceSwitchHighspeed(MMC:PMMCDevice;Enable:Boolean):LongWord;
var
 Speed:Byte;
 Status:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Switch Highspeed');
 {$ENDIF}

 {Check Highspeed}
 Result:=MMC_STATUS_UNSUPPORTED_REQUEST;
 if (MMC.Capabilities and MMC_CAP_SD_HIGHSPEED) = 0 then Exit;
 if (MMC.CCCR = nil) or not(MMC.CCCR.HighSpeed) then Exit;

 {Get SPEED Register}
 Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_SPEED,0,@Speed);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 if Enable then
  begin
   Speed:=Speed or SDIO_SPEED_EHS;
  end
 else
  begin
   Speed:=Speed and not(SDIO_SPEED_EHS);
  end;

 {Set SPEED Register}
 Status:=SDIODeviceReadWriteDirect(MMC,True,0,SDIO_CCCR_SPEED,Speed,nil);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_sdio_switch_hs in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function SDIODeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord;
var
 Status:LongWord;
 Timeout:Integer;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Send Operation Condition');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SDIO_CMD_SEND_OP_COND;
 Command.Argument:=0;
 if not(Probe) then
  begin
   Command.Argument:=MMC.OperationCondition;

   //To Do //Need to select a voltage that is compatible between the card and the host
           //Use SDHCI.Volatages to select from MMC.OperationCondition
           //See: mmc_select_voltage
  end;
 Command.ResponseType:=MMC_RSP_SPI_R4 or MMC_RSP_R4;
 Command.Data:=nil;

 {Setup Timeout}
 Timeout:=100;
 while Timeout > 0 do
  begin
   {Send Command}
   Status:=MMCDeviceSendCommand(MMC,@Command);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Single pass only on probe}
   if Probe then Break;

   {Wait until reset completes}
   if SDHCIIsSPI(SDHCI) then
    begin
     {Both MMC_RSP_R1_SPI_IDLE and MMC_OCR_BUSY indicate an initialized card under SPI,
      but some cards (Marvell's) only behave when looking at this one}
     if (Command.Response[1] and MMC_OCR_BUSY) <> 0 then Break;
    end
   else
    begin
     if (Command.Response[0] and MMC_OCR_BUSY) <> 0 then Break;
    end;

   Dec(Timeout);
   if Timeout = 0 then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDIO Send Operation Condition Busy Status Timeout');

     Exit;
    end;
   MillisecondDelay(10);
  end;

 {Get Response}
 if SDHCIIsSPI(SDHCI) then
  begin
   MMC.OperationCondition:=Command.Response[1];
   //To Do //SD_OCR_CCS etc (see: MMC/SD)
  end
 else
  begin
   MMC.OperationCondition:=Command.Response[0];
   //To Do //SD_OCR_CCS etc (see: MMC/SD)
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_send_io_op_cond in \drivers\mmc\core\sdio_ops.c
end;

{==============================================================================}

function SDIODeviceReadWriteDirect(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Input:Byte;Output:PByte):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read Write Direct');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Operation (SDIO_MAX_FUNCTIONS)}
 if Operation > 7 then Exit;

 {Check Address}
 if (Address and not($0001FFFF)) <> 0 then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SDIO_CMD_RW_DIRECT;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_SPI_R5 or MMC_RSP_R5;
 Command.Data:=nil;

 {Setup Argument}
 if Write then Command.Argument:=$80000000;
 Command.Argument:=Command.Argument or (Operation shl 28);
 if Write and (Output <> nil) then Command.Argument:=Command.Argument or $08000000;
 Command.Argument:=Command.Argument or (Address shl 9);
 Command.Argument:=Command.Argument or Input;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Result}
 if SDHCIIsSPI(SDHCI) then
  begin
   {Host driver already reported errors}
  end
 else
  begin
   if (Command.Response[0] and SDIO_RSP_R5_ERROR) <> 0 then
    begin
     Result:=MMC_STATUS_HARDWARE_ERROR;
     Exit;
    end;
   if (Command.Response[0] and SDIO_RSP_R5_FUNCTION_NUMBER) <> 0 then
    begin
     Result:=MMC_STATUS_INVALID_PARAMETER;
     Exit;
    end;
   if (Command.Response[0] and SDIO_RSP_R5_OUT_OF_RANGE) <> 0 then
    begin
     Result:=MMC_STATUS_INVALID_DATA;
     Exit;
    end;
  end;

 {Get Output}
 if Output <> nil then
  begin
   if SDHCIIsSPI(SDHCI) then
    begin
     Output^:=(Command.Response[0] shr 8) and $FF;
    end
   else
    begin
     Output^:=Command.Response[0] and $FF;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_io_rw_direct_host in \drivers\mmc\core\sdio_ops.c
end;

{==============================================================================}

function SDIODeviceReadWriteExtended(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Increment:Boolean;Buffer:Pointer;BlockCount,BlockSize:LongWord):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Data:TMMCData;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read Write Extended');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Block Size}
 if BlockSize = 0 then Exit;

 {Check Operation (SDIO_MAX_FUNCTIONS)}
 if Operation > 7 then Exit;

 {Check Address}
 if (Address and not($0001FFFF)) <> 0 then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SDIO_CMD_RW_EXTENDED;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_SPI_R5 or MMC_RSP_R5;
 Command.Data:=@Data;

 {Setup Argument}
 if Write then Command.Argument:=$80000000;
 Command.Argument:=Command.Argument or (Operation shl 28);
 if Increment then Command.Argument:=Command.Argument or $04000000;
 Command.Argument:=Command.Argument or (Address shl 9);
 if BlockCount = 0 then
  begin
   {Byte Mode}
   if (BlockSize < 512) then Command.Argument:=Command.Argument or BlockSize;
  end
 else
  begin
   {Block Mode}
   Command.Argument:=Command.Argument or $08000000 or BlockCount;
  end;

 {Setup Data}
 FillChar(Data,SizeOf(TMMCData),0);
 Data.Data:=Buffer;
 if Write then Data.Flags:=MMC_DATA_WRITE else Data.Flags:=MMC_DATA_READ;
 Data.BlockSize:=BlockSize;
 if BlockCount = 0 then Data.BlockCount:=1 else Data.BlockCount:=BlockCount;

 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Check Result}
 if SDHCIIsSPI(SDHCI) then
  begin
   {Host driver already reported errors}
  end
 else
  begin
   if (Command.Response[0] and SDIO_RSP_R5_ERROR) <> 0 then
    begin
     Result:=MMC_STATUS_HARDWARE_ERROR;
     Exit;
    end;
   if (Command.Response[0] and SDIO_RSP_R5_FUNCTION_NUMBER) <> 0 then
    begin
     Result:=MMC_STATUS_INVALID_PARAMETER;
     Exit;
    end;
   if (Command.Response[0] and SDIO_RSP_R5_OUT_OF_RANGE) <> 0 then
    begin
     Result:=MMC_STATUS_INVALID_DATA;
     Exit;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: mmc_io_rw_extended in \drivers\mmc\core\sdio_ops.c
end;

{==============================================================================}

function SDIODeviceReadByte(MMC:PMMCDevice;Address:LongWord;Output:PByte):LongWord;
{Wrapper for reading a single byte from Function 0}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Output}
 if Output = nil then Exit;

 {Read Byte}
 Result:=SDIODeviceReadWriteDirect(MMC,False,0,Address,0,Output);

 //See: sdio_f0_readb in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIODeviceWriteByte(MMC:PMMCDevice;Address:LongWord;Input:Byte):LongWord;
{Wrapper for writing a single byte to Function 0}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Address}
 if (Address < $0F) or (Address > $FF) then Exit;

 {Write Byte}
 Result:=SDIODeviceReadWriteDirect(MMC,True,0,Address,Input,nil);

 //See: sdio_f0_writeb in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIODeviceReadCCCR(MMC:PMMCDevice):LongWord;
var
 Status:LongWord;
 CCCRVersion:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read CCCR');
 {$ENDIF}

 {Allocate CCCR}
 if MMC.CCCR = nil then MMC.CCCR:=AllocMem(SizeOf(TSDIOCCCR));
 if MMC.CCCR = nil then Exit;

 {Get CCCR Register}
 Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_CCCR,0,@MMC.CCCR.CCCRVersion);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get CCCR Version}
 CCCRVersion:=MMC.CCCR.CCCRVersion and $0f;
 if CCCRVersion > SDIO_CCCR_REV_3_00 then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Unknown CCCR structure version received');

   Result:=MMC_STATUS_INVALID_DATA;
   Exit;
  end;

 {Get SDIO Version}
 MMC.CCCR.SDIOVersion:=(MMC.CCCR.CCCRVersion and $f0) shr 4;
 MMC.CCCR.CCCRVersion:=CCCRVersion;

 {Get SD Register}
 Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_SD,0,@MMC.CCCR.SDVersion);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get CAPS Register}
 Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_CAPS,0,@MMC.CCCR.CCCRCapabilities);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 if (MMC.CCCR.CCCRCapabilities and SDIO_CCCR_CAP_SMB) <> 0 then
  begin
   MMC.CCCR.MultiBlock:=True;
  end;
 if (MMC.CCCR.CCCRCapabilities and SDIO_CCCR_CAP_LSC) <> 0 then
  begin
   MMC.CCCR.LowSpeed:=True;
  end;
 if (MMC.CCCR.CCCRCapabilities and SDIO_CCCR_CAP_4BLS) <> 0 then
  begin
   MMC.CCCR.WideBus:=True;
  end;

 {Check CCCR Version}
 if CCCRVersion >= SDIO_CCCR_REV_1_10 then
  begin
   {Get POWER Register}
   Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_POWER,0,@MMC.CCCR.CCCRPowerControl);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   if (MMC.CCCR.CCCRPowerControl and SDIO_POWER_SMPC) <> 0 then
    begin
     MMC.CCCR.HighPower:=True;
    end;
  end;

 {Check CCCR Version}
 if CCCRVersion >= SDIO_CCCR_REV_1_20 then
  begin
   {Get SPEED Register}
   Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_SPEED,0,@MMC.CCCR.CCCRBusSpeed);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Setup Defaults}
   MMC.SDConfigurationData.SpecVersion3:=False;
   MMC.SDSwitchData.Group1Support:=0;
   MMC.SDSwitchData.Group3Support:=0;

   {Check CCCR Version}
   if (CCCRVersion >= SDIO_CCCR_REV_3_00) and ((MMC.OperationCondition and SDIO_RSP_R4_18V_PRESENT) <> 0) then
    begin
     MMC.SDConfigurationData.SpecVersion3:=True;

     {Get UHS Register}
     Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_UHS,0,@MMC.CCCR.CCCRUHSSupport);
     if Status <> MMC_STATUS_SUCCESS then
      begin
       Result:=Status;
       Exit;
      end;

     {Check UHS Support}
     //To Do //SDIO_UHS_DDR50/SDIO_UHS_SDR50/SDIO_UHS_SDR104

     {Get DRIVE STRENGTH Register}
     Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_DRIVE_STRENGTH,0,@MMC.CCCR.CCCRDriverStrength);
     if Status <> MMC_STATUS_SUCCESS then
      begin
       Result:=Status;
       Exit;
      end;

     if (MMC.CCCR.CCCRDriverStrength and SDIO_DRIVE_SDTA) <> 0 then
      begin
       MMC.SDSwitchData.Group3Support:=MMC.SDSwitchData.Group3Support or SD_SWITCH_GROUP3_TYPE_A;
      end;
     if (MMC.CCCR.CCCRDriverStrength and SDIO_DRIVE_SDTC) <> 0 then
      begin
       MMC.SDSwitchData.Group3Support:=MMC.SDSwitchData.Group3Support or SD_SWITCH_GROUP3_TYPE_C;
      end;
     if (MMC.CCCR.CCCRDriverStrength and SDIO_DRIVE_SDTD) <> 0 then
      begin
       MMC.SDSwitchData.Group3Support:=MMC.SDSwitchData.Group3Support or SD_SWITCH_GROUP3_TYPE_D;
      end;
    end;

   {Check for High Speed}
   if MMC.SDSwitchData.Group1Support = 0 then
    begin
     if (MMC.CCCR.CCCRBusSpeed and SDIO_SPEED_SHS) <> 0 then
      begin
       MMC.CCCR.HighSpeed:=True;
       MMC.SDSwitchData.Group1Support:=MMC.SDSwitchData.Group1Support or SD_SWITCH_GROUP1_HS;
      end;
    end;
  end;

 {Get Version}
 if MMC.CCCR.SDIOVersion = SDIO_SDIO_REV_1_00 then
  begin
   MMC.Version:=SDIO_VERSION_1_00;
  end
 else if MMC.CCCR.SDIOVersion = SDIO_SDIO_REV_1_10 then
  begin
   MMC.Version:=SDIO_VERSION_1_10;
  end
 else if MMC.CCCR.SDIOVersion = SDIO_SDIO_REV_1_20 then
  begin
   MMC.Version:=SDIO_VERSION_1_20;
  end
 else if MMC.CCCR.SDIOVersion = SDIO_SDIO_REV_2_00 then
  begin
   MMC.Version:=SDIO_VERSION_2_00;
  end
 else if MMC.CCCR.SDIOVersion = SDIO_SDIO_REV_3_00 then
  begin
   MMC.Version:=SDIO_VERSION_3_00;
  end;

 {Log CCCR Configuration}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' CCCR Configuration:');
   {CCCR Registers Data}
   MMCLogDebug(nil,'  CCCRVersion = ' + IntToHex(MMC.CCCR.CCCRVersion,2));
   MMCLogDebug(nil,'  SDIOVersion = ' + IntToHex(MMC.CCCR.SDIOVersion,2));
   MMCLogDebug(nil,'  SDVersion = ' + IntToHex(MMC.CCCR.SDVersion,2));
   MMCLogDebug(nil,'  CCCRCapabilities = ' + IntToHex(MMC.CCCR.CCCRCapabilities,2));
   MMCLogDebug(nil,'  CCCRPowerControl = ' + IntToHex(MMC.CCCR.CCCRPowerControl,2));
   MMCLogDebug(nil,'  CCCRBusSpeed = ' + IntToHex(MMC.CCCR.CCCRBusSpeed,2));
   MMCLogDebug(nil,'  CCCRUHSSupport = ' + IntToHex(MMC.CCCR.CCCRUHSSupport,2));
   MMCLogDebug(nil,'  CCCRDriverStrength = ' + IntToHex(MMC.CCCR.CCCRDriverStrength,2));
   {Calculated Values}
   MMCLogDebug(nil,'  MultiBlock = ' + BoolToStr(MMC.CCCR.MultiBlock,True));
   MMCLogDebug(nil,'  LowSpeed = ' + BoolToStr(MMC.CCCR.LowSpeed,True));
   MMCLogDebug(nil,'  WideBus = ' + BoolToStr(MMC.CCCR.WideBus,True));
   MMCLogDebug(nil,'  HighPower = ' + BoolToStr(MMC.CCCR.HighPower,True));
   MMCLogDebug(nil,'  HighSpeed = ' + BoolToStr(MMC.CCCR.HighSpeed,True));
   MMCLogDebug(nil,'  Version = ' + SDIOVersionToString(MMC.Version));
  end;
 {$ENDIF}

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_read_cccr in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function SDIODeviceReadFBR(Func:PSDIOFunction):LongWord;
var
 Data:Byte;
 Status:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read FBR (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Get STD_IF Register}
 Status:=SDIODeviceReadWriteDirect(Func.MMC,False,0,SDIO_FBR_BASE(Func.Number) + SDIO_FBR_STD_IF,0,@Data);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Function Interface Code}
 Data:=Data and $0F;

 {Check for Extended Function Interface Code}
 if Data = $0F then
  begin
   {Get STD_IF_EXT Register}
   Status:=SDIODeviceReadWriteDirect(Func.MMC,False,0,SDIO_FBR_BASE(Func.Number) + SDIO_FBR_STD_IF_EXT,0,@Data);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;
  end;

 {Update Class Id}
 Func.ClassId:=Data;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_read_fbr in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function SDIODeviceReadCIS(MMC:PMMCDevice;Func:PSDIOFunction):LongWord;
var
 Data:Byte;
 Code:Byte;
 Link:Byte;
 Count:LongWord;
 Status:LongWord;
 Number:LongWord;
 Address:LongWord;
 Timeout:Int64;
 Prev:PSDIOTuple;
 Tuple:PSDIOTuple;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read CIS');
 {$ENDIF}

 {Get function number}
 if Func <> nil then Number:=Func.Number else Number:=0;

 {Read 3 byte CIS pointer}
 {Note: Works for both common CIS and function CIS as SDIO_CCCR_CIS and SDIO_FBR_CIS are the same offset}
 Address:=0;
 for Count:=0 to 2 do
  begin
   {Read Byte}
   Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_FBR_BASE(Number) + SDIO_FBR_CIS + Count,0,@Data);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Store to Address}
   Address:=Address or (Data shl (Count * 8));
  end;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,' CIS Pointer = ' + IntToStr(Address));
 {$ENDIF}

 if Func <> nil then
  begin
   {Get Previous}
   Prev:=Func.Tuples;
  end
 else
  begin
   {Allocate Common CIS}
   if MMC.CIS = nil then MMC.CIS:=AllocMem(SizeOf(TSDIOCIS));
   if MMC.CIS = nil then Exit;

   {Get Previous}
   Prev:=MMC.Tuples;
  end;

 {Find Last}
 if Prev <> nil then
  begin
   while Prev.Next <> nil do
    begin
     Prev:=Prev.Next;
    end;
  end;

 {Get Timeout}
 Timeout:=ClockGetTotal + (SDIO_READ_CIS_TIMEOUT_MS * CLOCK_CYCLES_PER_MILLISECOND);

 {Read Tuples}
 while True do
  begin
   {Read Code}
   Status:=SDIODeviceReadWriteDirect(MMC,False,0,Address,0,@Code);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   Inc(Address);

   {Check for End}
   if Code = CISTPL_END then Break;

   {Check for Null}
   if Code = CISTPL_NULL then Continue;

   {Read Link (Offset to Next Tuple)}
   Status:=SDIODeviceReadWriteDirect(MMC,False,0,Address,0,@Link);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   Inc(Address);

   {Check for Link 0xFF (End of Chain)}
   if Link = $FF then Break;

   {Allocate Tuple}
   Tuple:=AllocMem(SizeOf(TSDIOTuple) + Link);
   if Tuple = nil then
    begin
     Result:=MMC_STATUS_OUT_OF_MEMORY;
     Exit;
    end;

   {Read Data}
   for Count:=0 to Link - 1 do
    begin
     Status:=SDIODeviceReadWriteDirect(MMC,False,0,Address + Count,0,@Tuple.Data[Count]);
     if Status <> MMC_STATUS_SUCCESS then Break;
    end;

   if Status <> MMC_STATUS_SUCCESS then
    begin
     {Free Tuple}
     FreeMem(Tuple);

     Result:=Status;
     Exit;
    end;

   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,' CIS Tuple Code = ' + IntToHex(Code,2) + ' Link = ' + IntToHex(Link,2));
   {$ENDIF}

   {Store the Tuple}
   Tuple.Code:=Code;
   Tuple.Size:=Link;
   if Prev = nil then
    begin
     if Func <> nil then
      begin
       Func.Tuples:=Tuple;
      end
     else
      begin
       MMC.Tuples:=Tuple;
      end;
    end
   else
    begin
     Prev.Next:=Tuple;
    end;
   Prev:=Tuple;

   {Parse the Tuple}
   case Code of
    CISTPL_VERS_1:SDIOCISParseTupleVERS1(MMC,Func,Tuple);
    CISTPL_MANFID:SDIOCISParseTupleMANFID(MMC,Func,Tuple);
    {CISTPL_FUNCID} {Nothing}
    CISTPL_FUNCE:SDIOCISParseTupleFUNCE(MMC,Func,Tuple);
    {CISTPL_SDIO_STD} {Nothing}
   end;

   {Check Timeout}
   if ClockGetTotal > Timeout then
    begin
     Result:=MMC_STATUS_TIMEOUT;
     Exit;
    end;

   {Move to Next}
   Inc(Address,Link);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_read_cis in \drivers\mmc\core\sdio_cis.c
end;

{==============================================================================}

function SDIODeviceReadCommonCIS(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read Common CIS');
 {$ENDIF}

 {Read CIS}
 Result:=SDIODeviceReadCIS(MMC,nil);

 {Log CIS Configuration}
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then
  begin
   MMCLogDebug(nil,' CIS Configuration:');
   MMCLogDebug(nil,'  Vendor = ' + IntToHex(MMC.CIS.Vendor,4));
   MMCLogDebug(nil,'  Device = ' + IntToHex(MMC.CIS.Device,4));
   MMCLogDebug(nil,'  BlockSize = ' + IntToStr(MMC.CIS.BlockSize));
   MMCLogDebug(nil,'  MaxClock = ' + IntToStr(MMC.CIS.MaxClock));
  end;
 {$ENDIF}

 //See: sdio_read_common_cis in \drivers\mmc\core\sdio_cis.c
end;

{==============================================================================}

function SDIODeviceReadFunctionCIS(Func:PSDIOFunction):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read Function CIS (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Read CIS}
 Result:=SDIODeviceReadCIS(Func.MMC,Func);
 if Result <> MMC_STATUS_SUCCESS then Exit;

 {Vendor and Device id is optional for function CIS}
 if Func.VendorId = 0 then
  begin
   {Copy from common CIS}
   Func.VendorId:=Func.MMC.CIS.Vendor;
   Func.DeviceId:=Func.MMC.CIS.Device;
  end;

 //See: sdio_read_func_cis in \drivers\mmc\core\sdio_cis.c
end;

{==============================================================================}

function SDIODeviceProcessInterrupts(MMC:PMMCDevice):LongWord;
var
 Pending:Byte;
 Count:LongWord;
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Func:PSDIOFunction;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC,'SDIO Process Interrupts');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Acquire the Lock}
 if MutexLock(SDHCI.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get INTx Register}
    Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_INTx,0,@Pending);
    if Status <> MMC_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;

    {Set default status}
    Status:=MMC_STATUS_SUCCESS;

    {Check Interrupt Function}
    Func:=MMC.SDIOInterruptFunction;
    if Func <> nil then
     begin
      if not Assigned(Func.Handler) then
       begin
        if MMC_LOG_ENABLED then MMCLogWarn(MMC,'Pending IRQ for interrupt function with no handler');

        Status:=MMC_STATUS_INVALID_PARAMETER;
       end
      else
       begin
        {Dispatch Interrupt}
        Func.Handler(Func);
       end;
     end
    else
     begin
      {Check Functions}
      for Count:=1 to SDIO_MAX_FUNCTIONS do
       begin
        if (Pending and (1 shl Count)) <> 0 then
         begin
          Func:=MMC.SDIOFunctions[Count - 1];
          if Func = nil then
           begin
            if MMC_LOG_ENABLED then MMCLogWarn(MMC,'Pending IRQ for non-existent function');

            Status:=MMC_STATUS_INVALID_PARAMETER;
           end
          else
           begin
            if not Assigned(Func.Handler) then
             begin
              if MMC_LOG_ENABLED then MMCLogWarn(MMC,'Pending IRQ for function with no handler');

              Status:=MMC_STATUS_INVALID_PARAMETER;
             end
            else
             begin
              {Dispatch Interrupt}
              Func.Handler(Func);
             end;
           end;
         end;
       end;
     end;

    if Status <> MMC_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;

    {Clear Worker}
    SDHCI.CardIRQWorker:=INVALID_HANDLE_VALUE;

    {Complete IRQ}
    Result:=SDHCIHostCompleteCardIRQ(SDHCI);
   finally
    {Release the Lock}
    MutexUnlock(SDHCI.Lock);
   end;
  end;

 //See: process_sdio_pending_irqs in \drivers\mmc\core\sdio_irq.c
 //     sdio_get_pending_irqs in \drivers\mmc\core\sdio_irq.c
end;

{==============================================================================}

function SDIODeviceRegisterInterrupt(MMC:PMMCDevice;Func:PSDIOFunction;Handler:TSDIOInterruptHandler):LongWord;
var
 Reg:Byte;
 Count:LongWord;
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Next:PSDIOFunction;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Function}
 if Func = nil then Exit;

 {Check Handler}
 if not Assigned(Handler) then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC,'SDIO Register Interrupt (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Acquire the Lock}
 if MutexLock(SDHCI.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Existing}
    if Assigned(Func.Handler) then Exit;
    if (SDHCI.CardIRQDevice <> nil) and (SDHCI.CardIRQDevice <> MMC) then Exit;

    {Update Function}
    Func.Handler:=Handler;

    {Update Card IRQ Device}
    SDHCI.CardIRQDevice:=MMC;

    {Update Card IRQ Count}
    Inc(SDHCI.CardIRQCount);
    if SDHCI.CardIRQCount = 1 then
     begin
      {Enable Card IRQ}
      Status:=SDHCIHostSetupCardIRQ(SDHCI,True);
      if Status <> MMC_STATUS_SUCCESS then
       begin
        {Reset Function}
        Func.Handler:=nil;

        {Reset Card IRQ Device}
        SDHCI.CardIRQDevice:=nil;

        {Reset Card IRQ Count}
        SDHCI.CardIRQCount:=0;

        Result:=Status;
        Exit;
       end;
     end;

    {Check Card IRQ Count}
    MMC.SDIOInterruptFunction:=nil;
    if SDHCI.CardIRQCount = 1 then
     begin
      {Check Functions}
      for Count:=1 to SDIO_MAX_FUNCTIONS do
       begin
        Next:=MMC.SDIOFunctions[Count - 1];
        if (Next <> nil) and Assigned(Next.Handler) then
         begin
          MMC.SDIOInterruptFunction:=Next;
          Break;
         end;
       end;
     end;

    {Get IENx Register}
    Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_IENx,0,@Reg);
    if Status <> MMC_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;

    {Enable Function Interrupt}
    Reg:=Reg or (1 shl Func.Number);

    {Enable Master Interrupt}
    Reg:=Reg or 1;

    {Set IENx Register}
    Status:=SDIODeviceReadWriteDirect(MMC,True,0,SDIO_CCCR_IENx,Reg,nil);
    if Status <> MMC_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;

    Result:=MMC_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(SDHCI.Lock);
   end;
  end;

 //See: sdio_claim_irq in \drivers\mmc\core\sdio_irq.c
 //     sdio_card_irq_get in \drivers\mmc\core\sdio_irq.c
 //     sdio_single_irq_set in \drivers\mmc\core\sdio_irq.c
end;

{==============================================================================}

function SDIODeviceDeregisterInterrupt(MMC:PMMCDevice;Func:PSDIOFunction):LongWord;
var
 Reg:Byte;
 Count:LongWord;
 Status:LongWord;
 SDHCI:PSDHCIHost;
 Next:PSDIOFunction;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC,'SDIO Deregister Interrupt (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Acquire the Lock}
 if MutexLock(SDHCI.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Existing}
    if not Assigned(Func.Handler) then Exit;
    if SDHCI.CardIRQCount < 1 then Exit;
    if (SDHCI.CardIRQDevice = nil) or (SDHCI.CardIRQDevice <> MMC) then Exit;

    {Get IENx Register}
    Status:=SDIODeviceReadWriteDirect(MMC,False,0,SDIO_CCCR_IENx,0,@Reg);
    if Status <> MMC_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;

    {Disable Function Interrupt}
    Reg:=Reg and not(1 shl Func.Number);

    {Disable Master Interrupt if no more Function Interrupt}
    if (Reg and $FE) = 0 then Reg:=0;

    {Set IENx Register}
    Status:=SDIODeviceReadWriteDirect(MMC,True,0,SDIO_CCCR_IENx,Reg,nil);
    if Status <> MMC_STATUS_SUCCESS then
     begin
      Result:=Status;
      Exit;
     end;

    {Update Function}
    Func.Handler:=nil;

    {Update Card IRQ Count}
    Dec(SDHCI.CardIRQCount);
    if SDHCI.CardIRQCount = 0 then
     begin
      {Update Card IRQ Device}
      SDHCI.CardIRQDevice:=nil;

      {Disable Card IRQ}
      Status:=SDHCIHostSetupCardIRQ(SDHCI,False);
      if Status <> MMC_STATUS_SUCCESS then
       begin
        Result:=Status;
        Exit;
       end;
     end;

    {Check Card IRQ Count}
    MMC.SDIOInterruptFunction:=nil;
    if SDHCI.CardIRQCount = 1 then
     begin
      {Check Functions}
      for Count:=1 to SDIO_MAX_FUNCTIONS do
       begin
        Next:=MMC.SDIOFunctions[Count - 1];
        if (Next <> nil) and Assigned(Next.Handler) then
         begin
          MMC.SDIOInterruptFunction:=Next;
          Break;
         end;
       end;
     end;

    Result:=MMC_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(SDHCI.Lock);
   end;
  end;

 //See: sdio_card_irq_put in \drivers\mmc\core\sdio_irq.c
 //     sdio_release_irq in \drivers\mmc\core\sdio_irq.c
end;

{==============================================================================}

function SDIODeviceBindFunctions(MMC:PMMCDevice):LongWord;
{Attempt to bind SDIO functions on an MMC device to one of the registered drivers}
{MMC: The MMC device to attempt to bind a driver to}
{Return: MMC_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Status:LongWord;
 Func:PSDIOFunction;
 Driver:PSDIODriver;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC,'SDIO Bind Functions');
 {$ENDIF}

 {Check MMC}
 if MMCDeviceCheck(MMC) <> MMC then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDIODriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Set Default}
    Status:=MMC_STATUS_DEVICE_UNSUPPORTED;

    {Check Function Count}
    if MMC.SDIOCount > 0 then
     begin
      {Get Driver}
      Driver:=SDIODriverTable;
      while Driver <> nil do
       begin
        {Check Functions}
        for Count:=1 to SDIO_MAX_FUNCTIONS do
         begin
          Func:=MMC.SDIOFunctions[Count - 1];
          if (Func <> nil) and (Func.Driver = nil) then
           begin
            {$IFDEF MMC_DEBUG}
            if MMC_LOG_ENABLED then MMCLogDebug(MMC,'Attempting to bind ' + DriverGetName(@Driver.Driver) + ' to function ' + IntToStr(Func.Number));
            {$ENDIF}

            {Attempt to Bind}
            Status:=Driver.DriverBind(MMC,Func);
            if Status <> MMC_STATUS_DEVICE_UNSUPPORTED then
             begin
              {Check Status}
              if Status = MMC_STATUS_SUCCESS then
               begin
                if MMC_LOG_ENABLED then MMCLogInfo(nil,'Bound ' + DriverGetName(@Driver.Driver) + ' to ' + DeviceGetName(@MMC.Device) + ' (Function=' + IntToStr(Func.Number) + ')');

                {Check Bind (Driver may call directly)}
                if Func.Driver = nil then
                 begin
                  Status:=SDIOFunctionBind(Func,Driver);
                  if Status <> MMC_STATUS_SUCCESS then
                   begin
                    {Return Error}
                    Result:=Status;
                    Exit;
                   end;
                 end;
               end
              else
               begin
                {Return Error}
                Result:=Status;
                Exit;
               end;
             end;
           end;
         end;

        {Get Next}
        Driver:=Driver.Next;
       end;
     end;

    {Return Result}
    Result:=Status;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDIODriverTableLock);
   end;
  end;
end;

{==============================================================================}

function SDIODeviceUnbindFunctions(MMC:PMMCDevice;Driver:PSDIODriver):LongWord;
{Unbind SDIO functions on an MMC device from a driver}
{MMC: The MMC device to unbind a driver from}
{Driver: The driver to unbind the MMC device from (nil to unbind from current driver)}
{Return: MMC_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Status:LongWord;
 Func:PSDIOFunction;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC,'SDIO Unbind Functions');
 {$ENDIF}

 {Check MMC}
 if MMCDeviceCheck(MMC) <> MMC then Exit;

 {Set Default}
 Status:=MMC_STATUS_NOT_BOUND;

 {Check Function Count}
 if MMC.SDIOCount > 0 then
  begin
   {Check Functions}
   for Count:=1 to SDIO_MAX_FUNCTIONS do
    begin
     Func:=MMC.SDIOFunctions[Count - 1];
     if Func <> nil then
      begin
       {Check Driver}
       if (Driver = nil) then
        begin
         if (Func.Driver <> nil) and Assigned(Func.Driver.DriverUnbind) then
          begin
           {$IFDEF MMC_DEBUG}
           if MMC_LOG_ENABLED then MMCLogDebug(MMC,'Unbinding ' + DriverGetName(@Func.Driver.Driver) + ' from function ' + IntToStr(Func.Number));
           {$ENDIF}

           {Unbind Driver}
           Func.Driver.DriverUnbind(MMC,Func);

           {Check Unbind (Driver may call directly)}
           if Func.Driver <> nil then
            begin
             SDIOFunctionUnbind(Func,Func.Driver);
            end;

           Status:=MMC_STATUS_SUCCESS;
          end;
        end
       else
        begin
         if (Func.Driver = Driver) and Assigned(Driver.DriverUnbind) then
          begin
           {$IFDEF MMC_DEBUG}
           if MMC_LOG_ENABLED then MMCLogDebug(MMC,'Unbinding ' + DriverGetName(@Driver.Driver) + ' from function ' + IntToStr(Func.Number));
           {$ENDIF}

           {Unbind Driver}
           Driver.DriverUnbind(MMC,Func);

           {Check Unbind (Driver may call directly)}
           if Func.Driver <> nil then
            begin
             SDIOFunctionUnbind(Func,Driver);
            end;

           Status:=MMC_STATUS_SUCCESS;
          end;
        end;
      end;
    end;
  end;

 {Return Result}
 Result:=Status;
end;

{==============================================================================}

function SDIOFunctionAllocate(MMC:PMMCDevice;Number:LongWord):PSDIOFunction;
var
 Status:LongWord;
 Func:PSDIOFunction;
begin
 {}
 Result:=nil;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Allocate (Number=' + IntToStr(Number) + ')');
 {$ENDIF}

 {Check Number}
 if (Number < 1) or (Number > SDIO_MAX_FUNCTIONS) then Exit;

 {Acquire the Lock}
 if MutexLock(MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    {Allocate Function}
    Func:=PSDIOFunction(AllocMem(SizeOf(TSDIOFunction)));
    if Func = nil then Exit;

    {Update Function}
    Func.MMC:=MMC;
    Func.Number:=Number;
    Func.SDIOState:=SDIO_STATE_DETACHED;
    Func.SDIOStatus:=SDIO_STATUS_UNBOUND;

    {Allocate DMA Buffer}
    Func.DMABuffer:=DMABufferAllocate(DMAHostGetDefault,SizeOf(LongWord));
    if Func.DMABuffer = nil then
     begin
      {Release Function}
      SDIOFunctionRelease(Func);

      Exit;
     end;

    if not(DMA_CACHE_COHERENT) then
     begin
      {Clean Cache (Dest)}
      CleanDataCacheRange(PtrUInt(Func.DMABuffer),SizeOf(LongWord));
     end;

    {Update State}
    Func.SDIOState:=SDIO_STATE_ATTACHING;

    {Read Function Basic Information Register (FBR)}
    Status:=SDIODeviceReadFBR(Func);
    if Status <> MMC_STATUS_SUCCESS then
     begin
      {Release Function}
      SDIOFunctionRelease(Func);

      Exit;
     end;

    {Read Function Card Information Structure (CIS)}
    Status:=SDIODeviceReadFunctionCIS(Func);
    if Status <> MMC_STATUS_SUCCESS then
     begin
      {Release Function}
      SDIOFunctionRelease(Func);

      Exit;
     end;

    {Update State}
    Func.SDIOState:=SDIO_STATE_ATTACHED;

    {Log Function Configuration}
    {$IFDEF MMC_DEBUG}
    if MMC_LOG_ENABLED then
     begin
      MMCLogDebug(nil,' Function Configuration:');
      MMCLogDebug(nil,'  Number = ' + IntToStr(Func.Number));
      MMCLogDebug(nil,'  ClassId = ' + IntToHex(Func.ClassId,2));
      MMCLogDebug(nil,'  VendorId = ' + IntToHex(Func.VendorId,4));
      MMCLogDebug(nil,'  DeviceId = ' + IntToHex(Func.DeviceId,4));
      MMCLogDebug(nil,'  BlockSize = ' + IntToStr(Func.BlockSize));
      MMCLogDebug(nil,'  MaxBlockSize = ' + IntToStr(Func.MaxBlockSize));
      MMCLogDebug(nil,'  EnableTimeout = ' + IntToStr(Func.EnableTimeout));
      MMCLogDebug(nil,'  State = ' + SDIOFunctionStateToString(Func.SDIOState));
      MMCLogDebug(nil,'  Status = ' + SDIOFunctionStatusToString(Func.SDIOStatus));
     end;
    {$ENDIF}

    {Return Function}
    Result:=Func;
   finally
    {Release the Lock}
    MutexUnlock(MMC.Lock);
   end;
  end;

 //See: sdio_alloc_func in \drivers\mmc\core\sdio_bus.c
end;

{==============================================================================}

function SDIOFunctionRelease(Func:PSDIOFunction):LongWord;
var
 MMC:PMMCDevice;
 Tuple:PSDIOTuple;
 Current:PSDIOTuple;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Release (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Get MMC}
 MMC:=Func.MMC;

 {Acquire the Lock}
 if MutexLock(MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    {Update State}
    Func.SDIOState:=SDIO_STATE_DETACHING;

    {Remove Function}
    Func.MMC.SDIOFunctions[Func.Number - 1]:=nil;

    {Release DMA Buffer}
    if Func.DMABuffer <> nil then
     begin
      DMABufferRelease(Func.DMABuffer);
     end;

    {Free Tuples}
    Tuple:=Func.Tuples;
    while Tuple <> nil do
     begin
      Current:=Tuple;
      Tuple:=Tuple.Next;

      FreeMem(Current);
     end;
    Func.Tuples:=nil;

    {Update State}
    Func.SDIOState:=SDIO_STATE_DETACHED;

    {Release Function}
    FreeMem(Func);

    Result:=MMC_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(MMC.Lock);
   end;
  end;

 //See: sdio_release_func in \drivers\mmc\core\sdio_bus.c
 //     sdio_free_func_cis in \drivers\mmc\core\sdio_cis.c
end;

{==============================================================================}

function SDIOFunctionFind(MMC:PMMCDevice;Number:LongWord):PSDIOFunction;
begin
 {}
 Result:=nil;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC,'SDIO Function Find (Function=' + IntToStr(Number) + ')');
 {$ENDIF}

 {Check Number}
 if (Number < 1) or (Number > SDIO_MAX_FUNCTIONS) then Exit;

 {Acquire the Lock}
 if MutexLock(MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Function}
    Result:=MMC.SDIOFunctions[Number - 1];
   finally
    {Release the Lock}
    MutexUnlock(MMC.Lock);
   end;
  end;
end;

{==============================================================================}

function SDIOFunctionFindById(MMC:PMMCDevice;VendorId,DeviceId:Word):PSDIOFunction;
var
 Count:LongWord;
 Func:PSDIOFunction;
begin
 {}
 Result:=nil;

 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(MMC,'SDIO Function Find By Id (VendorId=' + IntToHex(VendorId,4) + ' DeviceId=' + IntToHex(DeviceId,4) + ')');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    {Find Function}
    for Count:=1 to SDIO_MAX_FUNCTIONS do
     begin
      Func:=MMC.SDIOFunctions[Count - 1];
      if Func <> nil then
       begin
        if (Func.VendorId = VendorId) and (Func.DeviceId = DeviceId) then
         begin
          Result:=Func;
          Exit;
         end;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(MMC.Lock);
   end;
  end;
end;

{==============================================================================}

function SDIOFunctionEnumerate(MMC:PMMCDevice;Callback:TSDIOFunctionEnumerate;Data:Pointer):LongWord;
var
 Count:LongWord;
 Func:PSDIOFunction;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if MutexLock(MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    Result:=ERROR_OPERATION_FAILED;

    {Enumerate Functions}
    for Count:=1 to SDIO_MAX_FUNCTIONS do
     begin
      Func:=MMC.SDIOFunctions[Count - 1];
      if Func <> nil then
       begin
        if Callback(Func,Data) <> ERROR_SUCCESS then Exit;
       end;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(MMC.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDIOFunctionBind(Func:PSDIOFunction;Driver:PSDIODriver):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Check Driver}
 if Driver = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Bind (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Func.MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    Result:=MMC_STATUS_DEVICE_DETACHED;
    if Func.SDIOState <> SDIO_STATE_ATTACHED then Exit;

    {Check Status}
    Result:=MMC_STATUS_ALREADY_BOUND;
    if Func.Driver <> nil then Exit;
    if Func.SDIOStatus <> SDIO_STATUS_UNBOUND then Exit;

    {Update Status}
    Func.Driver:=Driver;
    Func.SDIOStatus:=SDIO_STATUS_BOUND;

    Result:=MMC_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Func.MMC.Lock);
   end;
  end;
end;

{==============================================================================}

function SDIOFunctionUnbind(Func:PSDIOFunction;Driver:PSDIODriver):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Check Driver}
 if Driver = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Unbind (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Acquire the Lock}
 if MutexLock(Func.MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    Result:=MMC_STATUS_DEVICE_DETACHED;
    if Func.SDIOState <> SDIO_STATE_ATTACHED then Exit;

    {Check Status}
    Result:=MMC_STATUS_NOT_BOUND;
    if Func.Driver <> Driver then Exit;
    if Func.SDIOStatus <> SDIO_STATUS_BOUND then Exit;

    {Update Status}
    Func.Driver:=nil;
    Func.SDIOStatus:=SDIO_STATUS_UNBOUND;

    Result:=MMC_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Func.MMC.Lock);
   end;
  end;
end;

{==============================================================================}

function SDIOFunctionEnable(Func:PSDIOFunction):LongWord;
var
 Reg:Byte;
 Timeout:Int64;
 Status:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Enable (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Get I/O Enable}
 Status:=SDIODeviceReadWriteDirect(Func.MMC,False,0,SDIO_CCCR_IOEx,0,@Reg);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Enable Function}
 Reg:=Reg or (1 shl Func.Number);

 {Set I/O Enable}
 Status:=SDIODeviceReadWriteDirect(Func.MMC,True,0,SDIO_CCCR_IOEx,Reg,nil);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Get Timeout}
 Timeout:=ClockGetTotal + (Func.EnableTimeout * CLOCK_CYCLES_PER_MILLISECOND);

 {Wait for I/O Ready}
 while True do
  begin
   {Get I/O Ready}
   Status:=SDIODeviceReadWriteDirect(Func.MMC,False,0,SDIO_CCCR_IORx,0,@Reg);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDIO Failed to enable device (Function=' + IntToStr(Func.Number) + ')');

     Result:=Status;
     Exit;
    end;

   {Check I/O Ready}
   if (Reg and (1 shl Func.Number)) <> 0 then Break;

   {Check Timeout}
   if ClockGetTotal > Timeout then
    begin
     Result:=MMC_STATUS_TIMEOUT;
     Exit;
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_enable_func in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionDisable(Func:PSDIOFunction):LongWord;
var
 Reg:Byte;
 Status:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Disable (Function=' + IntToStr(Func.Number) + ')');
 {$ENDIF}

 {Get I/O Enable}
 Status:=SDIODeviceReadWriteDirect(Func.MMC,False,0,SDIO_CCCR_IOEx,0,@Reg);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Disable Function}
 Reg:=Reg and not(1 shl Func.Number);

 {Set I/O Enable}
 Status:=SDIODeviceReadWriteDirect(Func.MMC,True,0,SDIO_CCCR_IOEx,Reg,nil);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_disable_func in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionSetBlockSize(Func:PSDIOFunction;BlockSize:LongWord):LongWord;
var
 Status:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Set Block Size (Function=' + IntToStr(Func.Number) + ' Size=' + IntToStr(BlockSize) + ')');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(Func.MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Block Size}
 if BlockSize > SDHCI.MaximumBlockSize then Exit;

 {Get Default Size}
 if BlockSize = 0 then
  begin
   BlockSize:=Min(Func.MaxBlockSize,SDHCI.MaximumBlockSize);
   BlockSize:=Min(BlockSize,512);
  end;

 {Set Block Size (Lower)}
 Status:=SDIODeviceReadWriteDirect(Func.MMC,True,0,SDIO_FBR_BASE(Func.Number) + SDIO_FBR_BLKSIZE,BlockSize and $FF,nil);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Set Block Size (Upper)}
 Status:=SDIODeviceReadWriteDirect(Func.MMC,True,0,SDIO_FBR_BASE(Func.Number) + SDIO_FBR_BLKSIZE + 1,(BlockSize shr 8) and $FF,nil);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Update Block Size}
 Func.BlockSize:=BlockSize;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_set_block_size in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionReadWriteExtended(Func:PSDIOFunction;Write:Boolean;Address:LongWord;Increment:Boolean;Buffer:Pointer;Size:LongWord):LongWord;
{Perform an SDIO read or write to the specified function at the specified address}
{Handles splitting any size read or write into multiple IO_RW_EXTENDED requests, accounting for maximum block sizes}
var
 SDHCI:PSDHCIHost;
 Remain:LongWord;
 Status:LongWord;
 BlockCount:LongWord;
 MaxBlockCount:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Function Read Write Extended (Function=' + IntToStr(Func.Number) + ' Write=' + BoolToStr(Write,True) + ' Address=' + IntToHex(Address,8) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Block Size}
 if Func.BlockSize = 0 then Exit;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(Func.MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Perform Block Mode Transfer}
 Remain:=Size;
 if Func.MMC.CCCR.MultiBlock and (Size > SDIOFunctionMaxByteSize(Func)) then
  begin
   {IO_RW_EXTENDED only allows 511 blocks maximum}
   MaxBlockCount:=Min(SDHCI.MaximumBlockCount,511);

   while Remain > Func.BlockSize do
    begin
     {Get Block Count}
     BlockCount:=Remain div Func.BlockSize;
     if BlockCount > MaxBlockCount then BlockCount:=MaxBlockCount;

     {Get Transfer Size}
     Size:=(BlockCount * Func.BlockSize);

     {Perform Transfer}
     Status:=SDIODeviceReadWriteExtended(Func.MMC,Write,Func.Number,Address,Increment,Buffer,BlockCount,Func.BlockSize);
     if Status <> MMC_STATUS_SUCCESS then
      begin
       Result:=Status;
       Exit;
      end;

     {Update Position}
     Dec(Remain,Size);
     Inc(Buffer,Size);
     if Increment then Inc(Address,Size);
    end;
  end;

 {Perform Byte Mode Transfer}
 while Remain > 0 do
  begin
   {Get Transfer Size}
   Size:=Min(Remain,SDIOFunctionMaxByteSize(Func));

   {Perform Transfer (Block Count = 0)}
   Status:=SDIODeviceReadWriteExtended(Func.MMC,Write,Func.Number,Address,Increment,Buffer,0,Size);
   if Status <> MMC_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Update Position}
   Dec(Remain,Size);
   Inc(Buffer,Size);
   if Increment then Inc(Address,Size);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_io_rw_ext_helper in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionRead(Func:PSDIOFunction;Address:LongWord;Buffer:Pointer;Size:LongWord):LongWord;
{Wrapper for reading multiple bytes from an SDIO function}
begin
 {}
 Result:=SDIOFunctionReadWriteExtended(Func,False,Address,True,Buffer,Size);

 //See: sdio_memcpy_fromio in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionWrite(Func:PSDIOFunction;Address:LongWord;Buffer:Pointer;Size:LongWord):LongWord;
{Wrapper for writing multiple bytes to an SDIO function}
begin
 {}
 Result:=SDIOFunctionReadWriteExtended(Func,True,Address,True,Buffer,Size);

 //See: sdio_memcpy_toio in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionReadByte(Func:PSDIOFunction;Address:LongWord;Output:PByte):LongWord;
{Wrapper for reading a single byte from an SDIO function}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Check Output}
 if Output = nil then Exit;

 {Read Byte}
 Result:=SDIODeviceReadWriteDirect(Func.MMC,False,Func.Number,Address,0,Output);

 //See: sdio_readb in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionWriteByte(Func:PSDIOFunction;Address:LongWord;Input:Byte):LongWord;
{Wrapper for writing a single byte to an SDIO function}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Write Byte}
 Result:=SDIODeviceReadWriteDirect(Func.MMC,True,Func.Number,Address,Input,nil);

 //See: sdio_writeb in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionWriteReadByte(Func:PSDIOFunction;Address:LongWord;Input:Byte;Output:PByte):LongWord;
{Wrapper for performing a read after write (RAW) operation on an SDIO function}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Check Output}
 if Output = nil then Exit;

 {Write/Read Byte}
 Result:=SDIODeviceReadWriteDirect(Func.MMC,True,Func.Number,Address,Input,Output);

 //See: sdio_writeb_readb in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionReadWord(Func:PSDIOFunction;Address:LongWord;Output:PWord):LongWord;
{Wrapper for reading a single word from an SDIO function}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Read Word}
 Result:=SDIOFunctionRead(Func,Address,Func.DMABuffer,SizeOf(Word));

 {Copy Buffer}
 if Result = MMC_STATUS_SUCCESS then Output^:=PWord(Func.DMABuffer)^;

 //See: sdio_readw in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionWriteWord(Func:PSDIOFunction;Address:LongWord;Input:Word):LongWord;
{Wrapper for writing a single word to an SDIO function}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Copy Value}
 PWord(Func.DMABuffer)^:=Input;

 {Write Word}
 Result:=SDIOFunctionWrite(Func,Address,Func.DMABuffer,SizeOf(Word));

 //See: sdio_writew in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionReadLong(Func:PSDIOFunction;Address:LongWord;Output:PLongWord):LongWord;
{Wrapper for reading a single longword from an SDIO function}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Read LongWord}
 Result:=SDIOFunctionRead(Func,Address,Func.DMABuffer,SizeOf(LongWord));

 {Copy Buffer}
 if Result = MMC_STATUS_SUCCESS then Output^:=PLongWord(Func.DMABuffer)^;

 //See: sdio_readl in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionWriteLong(Func:PSDIOFunction;Address:LongWord;Input:LongWord):LongWord;
{Wrapper for writing a single longword to an SDIO function}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Copy Value}
 PLongWord(Func.DMABuffer)^:=Input;

 {Write LongWord}
 Result:=SDIOFunctionWrite(Func,Address,Func.DMABuffer,SizeOf(LongWord));

 //See: sdio_writel in \drivers\mmc\core\sdio_io.c
end;

{==============================================================================}

function SDIOFunctionRegisterInterrupt(Func:PSDIOFunction;Handler:TSDIOInterruptHandler):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Register Interrupt}
 Result:=SDIODeviceRegisterInterrupt(Func.MMC,Func,Handler);

 //See: sdio_claim_irq in \drivers\mmc\core\sdio_irq.c
end;

{==============================================================================}

function SDIOFunctionDeregisterInterrupt(Func:PSDIOFunction):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check Function}
 if Func = nil then Exit;

 {Deregister Interrupt}
 Result:=SDIODeviceDeregisterInterrupt(Func.MMC,Func);

 //See: sdio_release_irq in \drivers\mmc\core\sdio_irq.c
end;

{==============================================================================}

function SDIOHostDispatchInterrupt(SDHCI:PSDHCIHost;IRQ,FIQ:Boolean):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Schedule Worker}
 if (SDHCI.CardIRQWorker = INVALID_HANDLE_VALUE) and (SDHCI.CardIRQDevice <> nil) then
  begin
   if FIQ then
    begin
     if WorkerScheduleFIQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(SDIODeviceProcessInterrupts),SDHCI.CardIRQDevice,nil) = ERROR_SUCCESS then
      begin
       SDHCI.CardIRQWorker:=0; {FIQ worker doesn't return a handle as it doesn't allow interval or repeat}
      end;
    end
   else if IRQ then
    begin
     if WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(SDIODeviceProcessInterrupts),SDHCI.CardIRQDevice,nil) = ERROR_SUCCESS then
      begin
       SDHCI.CardIRQWorker:=0; {IRQ worker doesn't return a handle as it doesn't allow interval or repeat}
      end;
    end
   else
    begin
     SDHCI.CardIRQWorker:=WorkerScheduleEx(0,WORKER_FLAG_PRIORITY,TWorkerTask(SDIODeviceProcessInterrupts),SDHCI.CardIRQDevice,nil);
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdio_signal_irq in \drivers\mmc\core\sdio_irq.c
end;

{==============================================================================}

function SDIODriverCreate:PSDIODriver;
{Create a new SDIO Driver entry}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=SDIODriverCreateEx(SizeOf(TSDIODriver));
end;

{==============================================================================}

function SDIODriverCreateEx(Size:LongWord):PSDIODriver;
{Create a new SDIO Driver entry}
{Size: Size in bytes to allocate for new driver (Including the driver entry)}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TSDIODriver) then Exit;

 {Create Driver}
 Result:=PSDIODriver(DriverCreateEx(Size));
 if Result = nil then Exit;

 {Update Driver}
 Result.DriverBind:=nil;
 Result.DriverUnbind:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to create lock for SDIO driver');

   SDIODriverDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SDIODriverDestroy(Driver:PSDIODriver):LongWord;
{Destroy an existing SDIO Driver entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Driver}
 Result:=ERROR_IN_USE;
 if SDIODriverCheck(Driver) = Driver then Exit;

 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if Driver.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Driver.Lock);
  end;

 {Destroy Driver}
 Result:=DriverDestroy(@Driver.Driver);
end;

{==============================================================================}

function SDIODriverRegister(Driver:PSDIODriver):LongWord;
{Register a new Driver in the SDIO Driver table}
var
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.DriverId <> DRIVER_ID_ANY then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Bind}
 if not(Assigned(Driver.DriverBind)) then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Cannot register driver, Bind function must be implemented');

   Exit;
  end;

 {Check Unbind}
 if not(Assigned(Driver.DriverUnbind)) then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Cannot register driver, Unbind function must be implemented');

   Exit;
  end;

 {Check Driver}
 Result:=ERROR_ALREADY_EXISTS;
 if SDIODriverCheck(Driver) = Driver then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Cannot register driver, already registered');

   Exit;
  end;

 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;

 {Insert Driver}
 if CriticalSectionLock(SDIODriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Driver}
    Driver.Driver.DriverClass:=DRIVER_CLASS_SDIO;

    {Register Driver}
    Result:=DriverRegister(@Driver.Driver);
    if Result <> ERROR_SUCCESS then Exit;

    {Link Driver}
    if SDIODriverTable = nil then
     begin
      SDIODriverTable:=Driver;
     end
    else
     begin
      Driver.Next:=SDIODriverTable;
      SDIODriverTable.Prev:=Driver;
      SDIODriverTable:=Driver;
     end;

    {Increment Count}
    Inc(SDIODriverTableCount);

    if MMC_LOG_ENABLED then MMCLogInfo(nil,'Registered ' + DriverGetName(@Driver.Driver) + ' (Id=' + IntToStr(Driver.Driver.DriverId) + ')');

    {Release Driver Table Lock}
    CriticalSectionUnlock(SDIODriverTableLock);

    {Acquire the Lock}
    if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Acquire the Lock}
       if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
        begin
         try
          {Get MMC}
          MMC:=MMCDeviceTable;
          while MMC <> nil do
           begin
            {Check SDIO}
            if MMC.SDIOCount > 0 then
             begin
              {Bind Functions}
              SDIODeviceBindFunctions(MMC);
             end;

            {Get Next}
            MMC:=MMC.Next;
           end;

          {Return Result}
          Result:=ERROR_SUCCESS;
         finally
          {Release the Lock}
          CriticalSectionUnlock(MMCDeviceTableLock);
         end;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(SDHCIHostTableLock);
      end;
     end;
   finally
    if Result <> ERROR_SUCCESS then CriticalSectionUnlock(SDIODriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDIODriverDeregister(Driver:PSDIODriver):LongWord;
{Deregister a Driver from the SDIO Driver table}
var
 MMC:PMMCDevice;
 Prev:PSDIODriver;
 Next:PSDIODriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.DriverId = DRIVER_ID_ANY then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Driver}
 Result:=ERROR_NOT_FOUND;
 if SDIODriverCheck(Driver) <> Driver then Exit;

 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_REGISTERED then Exit;

 {Remove Driver}
 if CriticalSectionLock(SDIODriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Acquire the Lock}
       if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
        begin
         try
          {Get MMC}
          MMC:=MMCDeviceTable;
          while MMC <> nil do
           begin
            {Check SDIO}
            if MMC.SDIOCount > 0 then
             begin
              {Unbind Functions}
              SDIODeviceUnbindFunctions(MMC,Driver);
             end;

            {Get Next}
            MMC:=MMC.Next;
           end;

          {Return Result}
          Result:=ERROR_SUCCESS;
         finally
          {Release the Lock}
          CriticalSectionUnlock(MMCDeviceTableLock);
         end;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(SDHCIHostTableLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
      Exit;
     end;

    {Deregister Driver}
    Result:=DriverDeregister(@Driver.Driver);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Driver}
    Prev:=Driver.Prev;
    Next:=Driver.Next;
    if Prev = nil then
     begin
      SDIODriverTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(SDIODriverTableCount);

    if MMC_LOG_ENABLED then MMCLogInfo(nil,'Deregistered ' + DriverGetName(@Driver.Driver));

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SDIODriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDIODriverFind(DriverId:LongWord):PSDIODriver;
{Find a driver by Id in the SDIO Driver table}
var
 Driver:PSDIODriver;
begin
 {}
 Result:=nil;

 {Check Id}
 if DriverId = DRIVER_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDIODriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=SDIODriverTable;
    while Driver <> nil do
     begin
      {Check State}
      if Driver.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        {Check Id}
        if Driver.Driver.DriverId = DriverId then
         begin
          Result:=Driver;
          Exit;
         end;
       end;

      {Get Next}
      Driver:=Driver.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDIODriverTableLock);
   end;
  end;
end;

{==============================================================================}

function SDIODriverFindByName(const Name:String):PSDIODriver; inline;
{Find a driver by name in the Driver table}
begin
 {}
 Result:=PSDIODriver(DriverFindByName(Name));
end;

{==============================================================================}

function SDIODriverEnumerate(Callback:TSDIODriverEnumerate;Data:Pointer):LongWord;
var
 Driver:PSDIODriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDIODriverTableLock) = ERROR_SUCCESS then
  begin
   try
    Result:=ERROR_OPERATION_FAILED;

    {Get Driver}
    Driver:=SDIODriverTable;
    while Driver <> nil do
     begin
      {Check State}
      if Driver.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        if Callback(Driver,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Driver:=Driver.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDIODriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{SDHCI Functions}
function SDHCIHostReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
{Default software reset function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
{Reference: Section 3.3 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 MMC:PMMCDevice;
 Timeout:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Reset (Mask=' + IntToHex(Mask,2) + ')');
 {$ENDIF}

 {Check Host Reset}
 if Assigned(SDHCI.HostReset) then
  begin
   {Host Reset Method}
   Result:=SDHCI.HostReset(SDHCI,Mask);
  end
 else
  begin
   {Default Reset Method}
   if (SDHCI.Quirks and SDHCI_QUIRK_NO_CARD_NO_RESET) <> 0 then
    begin
     {Get MMC}
     MMC:=MMCDeviceFindByDevice(@SDHCI.Device);
     if (MMC <> nil) and (MMC.MMCState = MMC_STATE_EJECTED) then
      begin
       Result:=MMC_STATUS_SUCCESS;
       Exit;
      end;
    end;

   {Setup Timeout (100ms)}
   Timeout:=100;

   {Send Reset}
   SDHCIHostWriteByte(SDHCI,SDHCI_SOFTWARE_RESET,Mask);

   {Reset Clock}
   if (Mask and SDHCI_RESET_ALL) <> 0 then
    begin
     SDHCI.Clock:=0;
     SDHCI.Power:=0;
    end;

   {Wait for Completion}
   while (SDHCIHostReadByte(SDHCI,SDHCI_SOFTWARE_RESET) and Mask) <> 0 do
    begin
     if Timeout = 0 then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Reset timeout (Mask=' + IntToHex(Mask,8) + ')');

       Exit;
      end;
     Dec(Timeout);
     MicrosecondDelay(1000);
    end;

   {Reset Interrupts}
   if (Mask and SDHCI_RESET_ALL) <> 0 then
    begin
     SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
     SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);
    end;

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_reset in sdhci.c
 //     sdhci_host->reset in sdhci.h
end;

{==============================================================================}

function SDHCIHostHardwareReset(SDHCI:PSDHCIHost):LongWord;
{Default hardware reset function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Hardware Reset');
 {$ENDIF}

 {Check Host Hardware Reset}
 if Assigned(SDHCI.HostHardwareReset) then
  begin
   {Host Hardware Reset Method}
   Result:=SDHCI.HostHardwareReset(SDHCI);
  end
 else
  begin
   {Default Hardware Reset Method}
   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_hw_reset in sdhci.c
 //     sdhci_host->hw_reset in sdhci.h
end;

{==============================================================================}

function SDHCIHostSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
{Default set power function for SDHCI host controllers}

{Power: A shift value to indicate the first available value in the Voltages mask}
{       Caller can use FirstBitSet(SDHCI.Voltages) to obtain the value of Power}
{       If there are no values set then Power will be -1 ($FFFF) to indicate nothing or unknown}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
{Reference: Section 3.3 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 Value:Byte;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Set Power (Power=' + IntToStr(Power) + ')');
 {$ENDIF}

 {Check Set Power}
 if Assigned(SDHCI.HostSetPower) then
  begin
   {Host Set Power Method}
   Result:=SDHCI.HostSetPower(SDHCI,Power);
  end
 else
  begin
   {Default Set Power Method}
   Value:=0;
   if Power <> $FFFF then
    begin
     case (1 shl Power) of
      MMC_VDD_165_195:Value:=SDHCI_POWER_180;
      MMC_VDD_29_30,MMC_VDD_30_31:Value:=SDHCI_POWER_300;
      MMC_VDD_32_33,MMC_VDD_33_34:Value:=SDHCI_POWER_330;
     end;
    end;

   {Compare Power Value}
   if SDHCI.Power = Value then
    begin
     Result:=MMC_STATUS_SUCCESS;
     Exit;
    end;

   {Save Power Value}
   SDHCI.Power:=Value;

   if Value = 0 then
    begin
     {Power Off}
     SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,0);
    end
   else
    begin
     {Clear power register before setting a new value}
     if (SDHCI.Quirks and SDHCI_QUIRK_SINGLE_POWER_WRITE) = 0 then
      begin
       SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,0);
      end;

     {Power On}
     if (SDHCI.Quirks and SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER) <> 0 then
      begin
       SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,Value);
      end;
     Value:=Value or SDHCI_POWER_ON;
     SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,Value);

     if (SDHCI.Quirks and SDHCI_QUIRK_DELAY_AFTER_POWER) <> 0 then
      begin
       MillisecondDelay(10);
      end;
    end;

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_set_power in sdhci.c
 //     sdhci_host->set_power in sdhci.h
end;

{==============================================================================}

function SDHCIHostSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;
{Default set clock function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
{Reference: Section 3.2 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var
 Value:Word;
 Timeout:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Set Clock (Clock=' + IntToStr(Clock) + ')');
 {$ENDIF}

 {Check Set Clock}
 if Assigned(SDHCI.HostSetClock) then
  begin
   {Host Set Clock Method}
   Result:=SDHCI.HostSetClock(SDHCI,Clock);
  end
 else
  begin
   {Default Set Clock Method}
   {Clock Off}
   SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,0);

   {Check Clock}
   Result:=MMC_STATUS_SUCCESS;
   if Clock = 0 then Exit;

   {Check Version}
   if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
    begin
     {Version 3.00 divisors must be a multiple of 2}
     if SDHCI.MaximumFrequency <= Clock then
      begin
       Divider:=1;
      end
     else
      begin
       Divider:=2;
       while Divider < SDHCI_MAX_CLOCK_DIV_SPEC_300 do
        begin
         if (SDHCI.MaximumFrequency div Divider) <= Clock then Break;
         Divider:=Divider + 2;
        end;
      end;
    end
   else
    begin
     {Version 2.00 divisors must be a power of 2}
     Divider:=1;
     while Divider < SDHCI_MAX_CLOCK_DIV_SPEC_200 do
      begin
       if (SDHCI.MaximumFrequency div Divider) <= Clock then Break;
       Divider:=Divider * 2;
      end;
    end;
   Divider:=Divider shr 1;

   {Set Clock Divider}
   SDHCIHostSetClockDivider(SDHCI,0,Divider);

   {Set Clock}
   Value:=(Divider and SDHCI_DIV_MASK) shl SDHCI_DIVIDER_SHIFT;
   Value:=Value or (((Divider and SDHCI_DIV_HI_MASK) shr SDHCI_DIV_MASK_LEN) shl SDHCI_DIVIDER_HI_SHIFT);
   Value:=Value or SDHCI_CLOCK_INT_EN;
   SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,Value);

   {Wait 20ms for Clock Stable}
   Timeout:=20;
   Value:=SDHCIHostReadWord(SDHCI,SDHCI_CLOCK_CONTROL);
   while (Value and SDHCI_CLOCK_INT_STABLE) = 0 do
    begin
     if Timeout = 0 then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Clock stable timeout');

       Exit;
      end;
     Dec(Timeout);
     MicrosecondDelay(1000);
     Value:=SDHCIHostReadWord(SDHCI,SDHCI_CLOCK_CONTROL);
    end;

   {Clock On}
   Value:=Value or SDHCI_CLOCK_CARD_EN;
   SDHCIHostWriteWord(SDHCI,SDHCI_CLOCK_CONTROL,Value);

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_set_clock in sdhci.c
 //     sdhci_host->set_clock in sdhci.h
 //See also: \drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostSetTiming(SDHCI:PSDHCIHost;Timing:LongWord):LongWord;
{Default set timing function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 Control2:Word;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Set Timing (Timing=' + MMCTimingToString(Timing) + ')');
 {$ENDIF}

 {Check Set Timing}
 if Assigned(SDHCI.HostSetTiming) then
  begin
   {Host Set Timing Method}
   Result:=SDHCI.HostSetTiming(SDHCI,Timing);
  end
 else
  begin
   {Default Set Timing Method}
   Control2:=SDHCIHostReadWord(SDHCI,SDHCI_HOST_CONTROL2);
   Control2:=Control2 and not(SDHCI_CTRL_UHS_MASK);

   {Select Bus Speed Mode for host}
   if (Timing = MMC_TIMING_MMC_HS200) or (Timing = MMC_TIMING_UHS_SDR104) then
    begin
     Control2:=Control2 or SDHCI_CTRL_UHS_SDR104;
    end
   else if Timing = MMC_TIMING_UHS_SDR12 then
    begin
     Control2:=Control2 or SDHCI_CTRL_UHS_SDR12;
    end
   else if Timing = MMC_TIMING_UHS_SDR25 then
    begin
     Control2:=Control2 or SDHCI_CTRL_UHS_SDR25;
    end
   else if Timing = MMC_TIMING_UHS_SDR50 then
    begin
     Control2:=Control2 or SDHCI_CTRL_UHS_SDR50;
    end
   else if (Timing = MMC_TIMING_UHS_DDR50) or (Timing = MMC_TIMING_MMC_DDR52) then
    begin
     Control2:=Control2 or SDHCI_CTRL_UHS_DDR50;
    end
   else if Timing = MMC_TIMING_MMC_HS400 then
    begin
     Control2:=Control2 or SDHCI_CTRL_HS400; {Non-standard}
    end;

   SDHCIHostWriteWord(SDHCI,SDHCI_HOST_CONTROL2,Control2);

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_set_uhs_signaling in sdhci.c
 //     sdhci_host->set_uhs_signaling in sdhci.h
end;

{==============================================================================}

function SDHCIHostSetBusWidth(SDHCI:PSDHCIHost;BusWidth:LongWord):LongWord;
{Default set bus width function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 Control:Byte;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Set Bus Width (BusWidth=' + MMCBusWidthToString(BusWidth) + ')');
 {$ENDIF}

 {Check Set Bus Width}
 if Assigned(SDHCI.HostSetBusWidth) then
  begin
   {Host Set Bus Width Method}
   Result:=SDHCI.HostSetBusWidth(SDHCI,BusWidth);
  end
 else
  begin
   {Default Set Bus Width Method}
   Control:=SDHCIHostReadByte(SDHCI,SDHCI_HOST_CONTROL);
   if BusWidth = MMC_BUS_WIDTH_8 then
    begin
     Control:=Control and not(SDHCI_CTRL_4BITBUS);
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       Control:=Control or SDHCI_CTRL_8BITBUS;
      end;
    end
   else
    begin
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       Control:=Control and not(SDHCI_CTRL_8BITBUS);
      end;
     if BusWidth = MMC_BUS_WIDTH_4 then
      begin
       Control:=Control or SDHCI_CTRL_4BITBUS;
      end
     else
      begin
       Control:=Control and not(SDHCI_CTRL_4BITBUS);
      end;
    end;

   {Set Control}
   SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Control);

   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_set_bus_width in sdhci.c
end;

{==============================================================================}

function SDHCIHostPrepareDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;
{Default DMA transfer prepare function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 Size:LongWord;
 Offset:LongWord;
 Address:PtrUInt;
 Control:LongWord;
 Control2:LongWord;
 LengthMask:LongWord;
 OffsetMask:PtrUInt;
 AlignBuffer:Pointer;
 DescriptorBuffer:Pointer;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Prepare DMA');
 {$ENDIF}

 {Check Command}
 if Command = nil then Exit;

 {Check Data}
 if Command.Data = nil then Exit;

 {Check Prepare DMA}
 if Assigned(SDHCI.HostPrepareDMA) then
  begin
   {Host Prepare DMA Method}
   Result:=SDHCI.HostPrepareDMA(SDHCI,Command);
  end
 else
  begin
   {Default Prepare DMA Method}
   if SDHCI.UseDMA then
    begin
     {Check External DMA}
     if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_EXTERNAL_DMA) <> 0 then
      begin
       {External DMA Transfer}
       {Free DMA Buffer}
       if SDHCI.DMABuffer <> nil then
        begin
         DMABufferRelease(SDHCI.DMABuffer);

         SDHCI.DMABuffer:=nil;
        end;

       {Validate Data Buffer}
       if DMABufferValidate(DMAHostGetDefault,Command.Data.Data,Command.Data.BlockCount * Command.Data.BlockSize) = ERROR_NOT_COMPATIBLE then
        begin
         SDHCI.DMABuffer:=DMABufferAllocate(DMAHostGetDefault,Command.Data.BlockCount * Command.Data.BlockSize);
        end;

       {Clear DMA Data}
       FillChar(SDHCI.DMAData,SizeOf(TDMAData),0);

       {Check Direction}
       if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
        begin
         {Device to Memory}
         SDHCI.DMADirection:=DMA_DIR_DEV_TO_MEM;

         {Setup DMA Data}
         SDHCI.DMAData.Source:=Pointer(PtrUInt(SDHCI.Address) + PtrUInt(SDHCI_BUFFER));
         SDHCI.DMAData.Dest:=Command.Data.Data;
         SDHCI.DMAData.Flags:=DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_DREQ or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN;
         SDHCI.DMAData.StrideLength:=0;
         SDHCI.DMAData.SourceStride:=0;
         SDHCI.DMAData.DestStride:=0;
         SDHCI.DMAData.Size:=Command.Data.BlockCount * Command.Data.BlockSize;

         {Check DMA Buffer}
         if SDHCI.DMABuffer <> nil then
          begin
           if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Using DMA bounce buffer for read data destination (' + IntToStr(SDHCI.DMAData.Size) + ' bytes)');

           {Use DMA Buffer}
           SDHCI.DMAData.Dest:=SDHCI.DMABuffer;
          end;

         {Check Cache}
         if not(DMA_CACHE_COHERENT) then
          begin
           {Clean Cache (Dest)}
           CleanDataCacheRange(PtrUInt(SDHCI.DMAData.Dest),SDHCI.DMAData.Size);
          end;
        end
       else
        begin
         {Memory to Device}
         SDHCI.DMADirection:=DMA_DIR_MEM_TO_DEV;

         {Setup DMA Data}
         SDHCI.DMAData.Source:=Command.Data.Data;
         SDHCI.DMAData.Dest:=Pointer(PtrUInt(SDHCI.Address) + PtrUInt(SDHCI_BUFFER));
         SDHCI.DMAData.Flags:=DMA_DATA_FLAG_DEST_NOINCREMENT or DMA_DATA_FLAG_DEST_DREQ or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_NOINVALIDATE;
         SDHCI.DMAData.StrideLength:=0;
         SDHCI.DMAData.SourceStride:=0;
         SDHCI.DMAData.DestStride:=0;
         SDHCI.DMAData.Size:=Command.Data.BlockCount * Command.Data.BlockSize;

         {Check DMA Buffer}
         if SDHCI.DMABuffer <> nil then
          begin
           if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Copying data to DMA bounce buffer from write data source (' + IntToStr(SDHCI.DMAData.Size) + ' bytes)');

           {Copy Data to DMA Buffer}
           SDHCI.DMAData.Source:=SDHCI.DMABuffer;
           System.Move(Command.Data.Data^,SDHCI.DMAData.Source^,SDHCI.DMAData.Size);
          end;

         {DMA Host will Clean Cache on Source Data}
        end;

       Result:=MMC_STATUS_SUCCESS;
      end
     else
      begin
       {Check for SDMA or ADMA}
       if (SDHCI.Device.DeviceFlags and (SDHCI_FLAG_SDMA or SDHCI_FLAG_ADMA)) <> 0 then
        begin
         {Check for Size and Alignment issues}
         LengthMask:=0;
         OffsetMask:=0;

         if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0 then
          begin
           if (SDHCI.Quirks and SDHCI_QUIRK_32BIT_ADMA_SIZE) <> 0 then
            begin
             LengthMask:=3;
             OffsetMask:=3;
            end;
          end
         else
          begin
           if (SDHCI.Quirks and SDHCI_QUIRK_32BIT_DMA_SIZE) <> 0 then
            begin
             LengthMask:=3;
            end;
           if (SDHCI.Quirks and SDHCI_QUIRK_32BIT_DMA_ADDR) <> 0 then
            begin
             OffsetMask:=3;
            end;
          end;

         if (LengthMask <> 0) or (OffsetMask <> 0) then
          begin
           {Check Data Length}
           if ((Command.Data.BlockCount * Command.Data.BlockSize) and LengthMask) <> 0 then
            begin
             {$IFDEF MMC_DEBUG}
             if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Reverting to PIO due to data size');
             {$ENDIF}

             SDHCI.UseDMA:=False;
            end;

           {Check Data Offset}
           if (PtrUInt(Command.Data.Data) and OffsetMask) <> 0 then
            begin
             {$IFDEF MMC_DEBUG}
             if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Reverting to PIO due to data alignment');
             {$ENDIF}

             SDHCI.UseDMA:=False;
            end;
          end;

         if SDHCI.UseDMA then
          begin
           if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0 then
            begin
             {Advanced DMA (ADMA) Transfer}
             {Build Descriptor Table}
             AlignBuffer:=SDHCI.ADMABuffer;
             DescriptorBuffer:=SDHCI.ADMATable;

             {Get Data Address and Size}
             Address:=PtrUInt(Command.Data.Data);
             Size:=Command.Data.BlockCount * Command.Data.BlockSize;

             {The SDHCI specification states that ADMA addresses must be 32-bit (4 byte) aligned}
             {If they aren't, then we use a bounce buffer for the (up to three) bytes that are not aligned}
             Offset:=(SDHCI_ADMA2_ALIGN - (Address and SDHCI_ADMA2_MASK)) and SDHCI_ADMA2_MASK;
             if Offset > 0 then
              begin
               {Check Direction}
               if (Command.Data.Flags and MMC_DATA_WRITE) <> 0 then
                begin
                 {Copy unaligned data to buffer}
                 System.Move(Command.Data.Data^,AlignBuffer^,Offset);
                end;

               {Write Descriptor (Tran, Valid)}
               SDHCIHostWriteADMADescriptor(SDHCI,DescriptorBuffer,SDHCI_ADMA2_DESC_TRAN_VALID,Offset,PtrUInt(AlignBuffer));

               {Update Buffer}
               Inc(AlignBuffer,SDHCI_ADMA2_ALIGN);

               {Update Data Address and Size}
               Inc(Address,Offset);
               Dec(Size,Offset);
              end;

             if Size > 0 then
              begin
               {Write Descriptor (Tran, Valid)}
               SDHCIHostWriteADMADescriptor(SDHCI,DescriptorBuffer,SDHCI_ADMA2_DESC_TRAN_VALID,Size,Address);
              end;

             if (SDHCI.Quirks and SDHCI_QUIRK_NO_ENDATTR_IN_NOPDESC) <> 0 then
              begin
               {Mark the last descriptor as the terminating descriptor}
               if DescriptorBuffer <> SDHCI.ADMATable then
                begin
                 Dec(DescriptorBuffer,SDHCI.ADMADescriptorSize);

                 {32-bit and 64-bit descriptors have the Command field in the same position}
                 PSDHCIADMA2Descriptor64(DescriptorBuffer).Command:=PSDHCIADMA2Descriptor64(DescriptorBuffer).Command or WordNtoLE(SDHCI_ADMA2_DESC_END);
                end;
              end
             else
              begin
               {Write Terminating Descriptor (Nop, End, Valid)}
               SDHCIHostWriteADMADescriptor(SDHCI,DescriptorBuffer,SDHCI_ADMA2_DESC_NOP_END_VALID,0,0);
              end;

             {Check Cache}
             if not(DMA_CACHE_COHERENT) then
              begin
               {Clean Cache (Table)}
               CleanDataCacheRange(PtrUInt(SDHCI.ADMATable),SDHCI.ADMATableSize);

               {Check Direction}
               if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
                begin
                 {Clean Cache (Dest)}
                 CleanDataCacheRange(PtrUInt(Command.Data.Data),Command.Data.BlockCount * Command.Data.BlockSize);
                end
               else
                begin
                 {Clean Cache (Data)}
                 CleanDataCacheRange(PtrUInt(Command.Data.Data),Command.Data.BlockCount * Command.Data.BlockSize);

                 {Clean Cache (Align Buffer)}
                 if Offset > 0 then CleanDataCacheRange(PtrUInt(SDHCI.ADMABuffer),SDHCI.ADMABufferSize);
                end;
              end;

             {Set ADMA Address}
             SDHCIHostSetADMAAddress(SDHCI,SDHCIHostGetADMAAddress(SDHCI));
             {$IFDEF MMC_DEBUG}
             if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI SDHCI_ADMA_ADDRESS=' + AddrToHex(SDHCIHostGetADMAAddress(SDHCI)));
             {$ENDIF}
            end
           else
            begin
             {Standard DMA (SDMA) Transfer}
             {Free DMA Buffer}
             if SDHCI.DMABuffer <> nil then
              begin
               DMABufferRelease(SDHCI.DMABuffer);

               SDHCI.DMABuffer:=nil;
              end;

             {Validate Data Buffer}
             if DMABufferValidate(DMAHostGetDefault,Command.Data.Data,Command.Data.BlockCount * Command.Data.BlockSize) = ERROR_NOT_COMPATIBLE then
              begin
               SDHCI.DMABuffer:=DMABufferAllocate(DMAHostGetDefault,Command.Data.BlockCount * Command.Data.BlockSize);
              end;

             {Check Direction}
             if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
              begin
               {Check DMA Buffer}
               if SDHCI.DMABuffer <> nil then
                begin
                 if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Using DMA bounce buffer for read data destination (' + IntToStr(Command.Data.BlockCount * Command.Data.BlockSize) + ' bytes)');

                 {Check Cache}
                 if not(DMA_CACHE_COHERENT) then
                  begin
                   {Clean Cache (Dest)}
                   CleanDataCacheRange(PtrUInt(SDHCI.DMABuffer),Command.Data.BlockCount * Command.Data.BlockSize);
                  end;
                end
               else
                begin
                 {Check Cache}
                 if not(DMA_CACHE_COHERENT) then
                  begin
                   {Clean Cache (Dest)}
                   CleanDataCacheRange(PtrUInt(Command.Data.Data),Command.Data.BlockCount * Command.Data.BlockSize);
                  end;
                end;
              end
             else
              begin
               {Check DMA Buffer}
               if SDHCI.DMABuffer <> nil then
                begin
                 if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Copying data to DMA bounce buffer from write data source (' + IntToStr(Command.Data.BlockCount * Command.Data.BlockSize) + ' bytes)');

                 {Copy Data to DMA Buffer}
                 System.Move(Command.Data.Data^,SDHCI.DMABuffer^,Command.Data.BlockCount * Command.Data.BlockSize);

                 {Check Cache}
                 if not(DMA_CACHE_COHERENT) then
                  begin
                   {Clean Cache (Data)}
                   CleanDataCacheRange(PtrUInt(SDHCI.DMABuffer),Command.Data.BlockCount * Command.Data.BlockSize);
                  end;
                end
               else
                begin
                 {Check Cache}
                 if not(DMA_CACHE_COHERENT) then
                  begin
                   {Clean Cache (Data)}
                   CleanDataCacheRange(PtrUInt(Command.Data.Data),Command.Data.BlockCount * Command.Data.BlockSize);
                  end;
                end;
              end;

             {Set SDMA Address}
             SDHCIHostSetSDMAAddress(SDHCI,SDHCIHostGetSDMAAddress(SDHCI,Command));
             {$IFDEF MMC_DEBUG}
             if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI SDHCI_DMA_ADDRESS=' + AddrToHex(SDHCIHostGetSDMAAddress(SDHCI,Command)));
             {$ENDIF}
            end;
          end;

         {Setup Control}
         Control:=SDHCIHostReadByte(SDHCI,SDHCI_HOST_CONTROL);

         {Adjust the DMA selection as some controllers can't do PIO properly when the selection is ADMA}
         Control:=Control and not(SDHCI_CTRL_DMA_MASK);

         if SDHCI.UseDMA then
          begin
           {If DMA Select is zero then SDMA is selected}
           if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0 then
            begin
             Control:=Control or SDHCI_CTRL_ADMA32;
            end;

           if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_64_BIT_DMA) <> 0 then
            begin
             {If V4 mode enabled, all supported DMA can use 64-bit addressing if controller
              supports 64-bit address, otherwise only ADMA can support 64-bit addressing}
             if SDHCI.EnableV4Mode then
              begin
               {Setup Control2}
               Control2:=SDHCIHostReadWord(SDHCI,SDHCI_HOST_CONTROL2);
               Control2:=Control2 or SDHCI_CTRL_64BIT_ADDR;
               SDHCIHostWriteWord(SDHCI,SDHCI_HOST_CONTROL2,Control2);
              end
             else if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0 then
              begin
               {Don't need to undo SDHCI_CTRL_ADMA32 in order to set SDHCI_CTRL_ADMA64}
               Control:=Control or SDHCI_CTRL_ADMA64;
              end;
            end;
          end;

         SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Control);

         Result:=MMC_STATUS_SUCCESS;
        end;
      end;
    end;
  end;

 //See: sdhci_prepare_data in sdhci.c
 //     sdhci_external_dma_prepare_data in sdhci.c
 //     sdhci_pre_dma_transfer in sdhci.c
 //     sdhci_adma_table_pre in sdhci.c
 //     sdhci_config_dma in sdhci.c
 //     bcm2835_mmc_transfer_dma in bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostStartDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;
{Default DMA transfer start function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Start DMA');
 {$ENDIF}

 {Check Command}
 if Command = nil then Exit;

 {Check Data}
 if Command.Data = nil then Exit;

 {Check Start DMA}
 if Assigned(SDHCI.HostStartDMA) then
  begin
   {Host Start DMA Method}
   Result:=SDHCI.HostStartDMA(SDHCI,Command);
  end
 else
  begin
   {Default Start DMA Method}
   if SDHCI.UseDMA then
    begin
     {Check External DMA}
     if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_EXTERNAL_DMA) <> 0 then
      begin
       {Start DMA}
       if DMATransferRequestEx(DMAHostGetDefault,@SDHCI.DMAData,SDHCIHostCompleteDMA,SDHCI,SDHCI.DMADirection,SDHCI.DMASlave,DMA_REQUEST_FLAG_NONE) <> ERROR_SUCCESS then
        begin
         if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Failed to start external DMA request');

         Result:=MMC_STATUS_HARDWARE_ERROR;
         Exit;
        end;
      end;

     Result:=MMC_STATUS_SUCCESS;
    end;
  end;

 //See: sdhci_external_dma_pre_transfer in sdhci.c
 //     bcm2835_mmc_transfer_dma in bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostStopDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord;
{Default DMA transfer stop function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 Size:LongWord;
 Status:LongWord;
 AlignBuffer:Pointer;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Stop DMA');
 {$ENDIF}

 {Check Command}
 if Command = nil then Exit;

 {Check Data}
 if Command.Data = nil then Exit;

 {Check Stop DMA}
 if Assigned(SDHCI.HostStopDMA) then
  begin
   {Host Stop DMA Method}
   Result:=SDHCI.HostStopDMA(SDHCI,Command);
  end
 else
  begin
   {Default Stop DMA Method}
   if SDHCI.UseDMA then
    begin
     {Check External DMA}
     if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_EXTERNAL_DMA) <> 0 then
      begin
       {Check Status}
       if Command.Status = MMC_STATUS_SUCCESS then
        begin
         {Wait for Signal with Timeout (5000ms)}
         Status:=SemaphoreWaitEx(SDHCI.DMAWait,5000);
         if Status <> ERROR_SUCCESS then
          begin
           if Status = ERROR_WAIT_TIMEOUT then
            begin
             if MMC_LOG_ENABLED then MMCLogError(nil,'MMC DMA Wait Timeout');

             Command.Status:=MMC_STATUS_TIMEOUT;
            end
           else
            begin
             if MMC_LOG_ENABLED then MMCLogError(nil,'MMC DMA Wait Failure');

             Command.Status:=MMC_STATUS_HARDWARE_ERROR;
            end;

           Result:=Command.Status;
           Exit;
          end;
        end
       else
        begin
         {Cancel / Cleanup the DMA transfer}
         {Nothing in the default method}
        end;

       Result:=MMC_STATUS_SUCCESS;
      end
     else
      begin
       {Check for SDMA or ADMA}
       if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0 then
        begin
         {Check Status}
         if Command.Status = MMC_STATUS_SUCCESS then
          begin
           {Check Data Direction}
           if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
            begin
             {Check Cache}
             if not(DMA_CACHE_COHERENT) then
              begin
               {Invalidate Cache (Data)}
               InvalidateDataCacheRange(PtrUInt(Command.Data.Data),Command.Data.BlockCount * Command.Data.BlockSize);
              end;

             if (PtrUInt(Command.Data.Data) and SDHCI_ADMA2_MASK) <> 0 then
              begin
               {Check Cache}
               if not(DMA_CACHE_COHERENT) then
                begin
                 {Invalidate Cache (Align Buffer)}
                 InvalidateDataCacheRange(PtrUInt(SDHCI.ADMABuffer),SDHCI.ADMABufferSize);
                end;

               {Determine unaligned data size}
               AlignBuffer:=SDHCI.ADMABuffer;
               Size:=SDHCI_ADMA2_ALIGN - (PtrUInt(Command.Data.Data) and SDHCI_ADMA2_MASK);

               {Copy unaligned data from buffer}
               System.Move(AlignBuffer^,Command.Data.Data^,Size);

               {Update Buffer}
               Inc(AlignBuffer,SDHCI_ADMA2_ALIGN);
              end;
            end;
          end;

         Result:=MMC_STATUS_SUCCESS;
        end
       else if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_SDMA) <> 0 then
        begin
         {Check Status}
         if Command.Status = MMC_STATUS_SUCCESS then
          begin
           {Check Data Direction}
           if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
            begin
             {Check DMA Buffer}
             if SDHCI.DMABuffer <> nil then
              begin
               if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Copying data from DMA bounce buffer to read data destination (' + IntToStr(Command.Data.BlockCount * Command.Data.BlockSize) + ' bytes)');

               {Check Cache}
               if not(DMA_CACHE_COHERENT) then
                begin
                 {Invalidate Cache}
                 InvalidateDataCacheRange(PtrUInt(SDHCI.DMABuffer),Command.Data.BlockCount * Command.Data.BlockSize);
                end;

               {Copy Data from DMA Buffer}
               System.Move(SDHCI.DMABuffer^,Command.Data.Data^,Command.Data.BlockCount * Command.Data.BlockSize);
              end
             else
              begin
               {Check Cache}
               if not(DMA_CACHE_COHERENT) then
                begin
                 {Invalidate Cache}
                 InvalidateDataCacheRange(PtrUInt(Command.Data.Data),Command.Data.BlockCount * Command.Data.BlockSize);
                end;
              end;
            end;
          end;

         Result:=MMC_STATUS_SUCCESS;
        end;
      end;
    end;
  end;

 //See: sdhci_adma_table_post in sdhci.c
end;

{==============================================================================}

procedure SDHCIHostCompleteDMA(Request:PDMARequest);{$IFDEF i386} stdcall;{$ENDIF}
{Default DMA request completion callback for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 SDHCI:PSDHCIHost;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(Request.DriverData);
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Complete DMA');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then Exit;

 {Check Data}
 if SDHCI.Command.Data = nil then Exit;

 {Check External DMA}
 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_EXTERNAL_DMA) = 0 then Exit;

 {Check DMA Buffer}
 if (SDHCI.DMABuffer <> nil) and ((SDHCI.Command.Data.Flags and MMC_DATA_READ) <> 0) then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Copying data from DMA bounce buffer to read data destination (' + IntToStr(SDHCI.Command.Data.BlockCount * SDHCI.Command.Data.BlockSize) + ' bytes)');

   {Copy Data from DMA Buffer}
   System.Move(SDHCI.DMABuffer^,SDHCI.Command.Data.Data^,SDHCI.Command.Data.BlockCount * SDHCI.Command.Data.BlockSize);
  end;

 {Signal DMA Completed}
 SemaphoreSignal(SDHCI.DMAWait);

 //See: bcm2835_mmc_dma_complete in bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostSetupCardIRQ(SDHCI:PSDHCIHost;Enable:LongBool):LongWord;
{Default Card IRQ setup function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Setup Card IRQ');
 {$ENDIF}

 {Check Setup Card IRQ}
 if Assigned(SDHCI.HostSetupCardIRQ) then
  begin
   {Host Setup Card IRQ Method}
   Result:=SDHCI.HostSetupCardIRQ(SDHCI,Enable);
  end
 else
  begin
   {Default Setup Card IRQ Method}
   {Acquire Spin Lock}
   if SDHCIHostLock(SDHCI) = ERROR_SUCCESS then
    begin
     try
      {Check Enable}
      if Enable then
       begin
        SDHCI.CardIRQEnabled:=True;
        SDHCI.Interrupts:=SDHCI.Interrupts or SDHCI_INT_CARD_INT;
       end
      else
       begin
        SDHCI.CardIRQEnabled:=False;
        SDHCI.Interrupts:=SDHCI.Interrupts and not(SDHCI_INT_CARD_INT);
       end;

      {Update Interrupts}
      SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
      SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);

      Result:=MMC_STATUS_SUCCESS;
     finally
      {Release Spin Lock}
      SDHCIHostUnlock(SDHCI);
     end;
    end;
  end;

 //See: sdhci_enable_sdio_irq in sdhci.c
 //     bcm2835_mmc_enable_sdio_irq in bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostCompleteCardIRQ(SDHCI:PSDHCIHost):LongWord;
{Default Card IRQ completion function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Complete Card IRQ');
 {$ENDIF}

 {Check Complete Card IRQ}
 if Assigned(SDHCI.HostCompleteCardIRQ) then
  begin
   {Host Complete Card IRQ Method}
   Result:=SDHCI.HostCompleteCardIRQ(SDHCI);
  end
 else
  begin
   {Default Complete Card IRQ Method}
   if SDHCI.CardIRQEnabled then
    begin
     Result:=SDHCIHostSetupCardIRQ(SDHCI,True);
    end
   else
    begin
     Result:=MMC_STATUS_SUCCESS;
    end;
  end;

 //See: sdhci_ack_sdio_irq in sdhci.c
 //     bcm2835_mmc_ack_sdio_irq in bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostTransferPIO(SDHCI:PSDHCIHost):LongWord;
{Default PIO transfer function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 Mask:LongWord;
 BlockChunk:LongWord;
 BlockBuffer:LongWord;
 BlockOffset:LongWord;
 BlockRemain:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Transfer PIO');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Transfer PIO when no current command');
   {$ENDIF}

   Exit;
  end;

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Transfer PIO when no current data');
   {$ENDIF}

   Exit;
  end;

 {Check Data Buffer}
 if SDHCI.Command.Data.Data = nil then Exit;

 {Check Blocks Remaining}
 if SDHCI.Command.Data.BlocksRemaining = 0 then Exit;

 {Check Direction}
 if (SDHCI.Command.Data.Flags and MMC_DATA_READ) <> 0 then
  begin
   Mask:=SDHCI_DATA_AVAILABLE;
  end
 else
  begin
   Mask:=SDHCI_SPACE_AVAILABLE;
  end;
 {Some controllers (JMicron JMB38x) mess up the buffer bits for transfers < 4 bytes. As long as it is just one block, we can ignore the bits}
 if ((SDHCI.Quirks and SDHCI_QUIRK_BROKEN_SMALL_PIO) <> 0) and (SDHCI.Command.Data.BlockCount = 1) then
  begin
   Mask:=LongWord(-1);
  end;

 {Check Data / Space Available}
 while (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and Mask) <> 0 do
  begin
   if (SDHCI.Quirks and SDHCI_QUIRK_PIO_NEEDS_DELAY) <> 0 then
    begin
     MicrosecondDelay(100);
    end;

   if (SDHCI.Command.Data.Flags and MMC_DATA_READ) <> 0 then
    begin
     {Read Block}
     BlockChunk:=0;
     BlockOffset:=0;
     BlockRemain:=SDHCI.Command.Data.BlockSize;
     while BlockRemain > 0 do
      begin
       {Read Chunk}
       if BlockChunk = 0 then
        begin
         BlockBuffer:=SDHCIHostReadLong(SDHCI,SDHCI_BUFFER);
         BlockChunk:=4;
        end;

       {Copy Chunk}
       PByte(PtrUInt(SDHCI.Command.Data.Data) + PtrUInt(SDHCI.Command.Data.BlockOffset) + PtrUInt(BlockOffset))^:=(BlockBuffer and $FF);

       {Update Block}
       Inc(BlockOffset);
       BlockBuffer:=BlockBuffer shr 8;
       Dec(BlockChunk);
       Dec(BlockRemain);
      end;
    end
   else
    begin
     {Write Block}
     BlockChunk:=0;
     BlockBuffer:=0;
     BlockOffset:=0;
     BlockRemain:=SDHCI.Command.Data.BlockSize;
     while BlockRemain > 0 do
      begin
       {Copy Chunk}
       BlockBuffer:=BlockBuffer or (PLongWord(PtrUInt(SDHCI.Command.Data.Data) + PtrUInt(SDHCI.Command.Data.BlockOffset) + PtrUInt(BlockOffset))^ shl (BlockChunk shl 3));

       {Update Block}
       Inc(BlockOffset);
       Inc(BlockChunk);
       Dec(BlockRemain);

       {Write Chunk}
       if (BlockChunk = 4) or (BlockRemain = 0) then
        begin
         SDHCIHostWriteLong(SDHCI,SDHCI_BUFFER,BlockBuffer);
         BlockChunk:=0;
         BlockBuffer:=0;
        end;
      end;
    end;

   Inc(SDHCI.Command.Data.BlockOffset,SDHCI.Command.Data.BlockSize);
   Dec(SDHCI.Command.Data.BlocksRemaining);
   Dec(SDHCI.Command.Data.BytesRemaining,SDHCI.Command.Data.BlockSize);
   if SDHCI.Command.Data.BlocksRemaining = 0 then Break;
  end;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Transfer PIO completed (BlocksRemaining=' + IntToStr(SDHCI.Command.Data.BlocksRemaining) + ')');
 {$ENDIF}

 Result:=MMC_STATUS_SUCCESS;

 //See: sdhci_transfer_pio in sdhci.c
 //     bcm2835_mmc_transfer_pio in bcm2835-mmc.c
 //     sdhci_read_block_pio / sdhci_write_block_pio in sdhci.c
end;

{==============================================================================}

function SDHCIHostTransferDMA(SDHCI:PSDHCIHost):LongWord;
{Default DMA transfer function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 StartAddress:PtrUInt;
 CurrentAddress:PtrUInt;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Transfer DMA');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Transfer DMA when no current command');
   {$ENDIF}

   Exit;
  end;

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Transfer DMA when no current data');
   {$ENDIF}

   Exit;
  end;

 {Update to the next DMA Block Boundary}
 StartAddress:=SDHCIHostGetSDMAAddress(SDHCI,SDHCI.Command);
 CurrentAddress:=StartAddress + SDHCI.Command.Data.BytesTransfered;

 {Advance DMA Block Boundary}
 CurrentAddress:=(CurrentAddress and not(SDHCI_DEFAULT_BOUNDARY_SIZE - 1)) + SDHCI_DEFAULT_BOUNDARY_SIZE;

 {Update Bytes Transfered}
 SDHCI.Command.Data.BytesTransfered:=CurrentAddress - StartAddress;

 {$IFDEF INTERRUPT_DEBUG}
 if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Next DMA Block Boundary = ' + AddrToHex(CurrentAddress));
 {$ENDIF}
 SDHCIHostSetSDMAAddress(SDHCI,CurrentAddress);

 Result:=MMC_STATUS_SUCCESS;

 //See: sdhci_data_irq in sdhci.c
end;

{==============================================================================}

function SDHCIHostFinishCommand(SDHCI:PSDHCIHost):LongWord;
{Default finish command function for SDHCI host controllers}
{Called by Interrupt Command handler when an SDHCI_INT_RESPONSE is received}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 Count:Integer;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Finish Command');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Finish Command when no current command');
   {$ENDIF}

   Exit;
  end;

 {Check for Response}
 if (SDHCI.Command.ResponseType and MMC_RSP_PRESENT) <> 0 then
  begin
   {Check for 136 bit Response}
   if (SDHCI.Command.ResponseType and MMC_RSP_136) <> 0 then
    begin
     //To Do //SDHCI_QUIRK2_RSP_136_HAS_CRC
     {CRC is stripped so we need to do some shifting}
     for Count:=0 to 3 do
      begin
       SDHCI.Command.Response[Count]:=SDHCIHostReadLong(SDHCI,SDHCI_RESPONSE + ((3 - Count) * 4)) shl 8;
       if Count <> 3 then
        begin
         SDHCI.Command.Response[Count]:=SDHCI.Command.Response[Count] or SDHCIHostReadByte(SDHCI,SDHCI_RESPONSE + (((3 - Count) * 4) - 1));
        end;
      end;
    end
   else
    begin
     SDHCI.Command.Response[0]:=SDHCIHostReadLong(SDHCI,SDHCI_RESPONSE);
    end;
  end;

 SDHCI.Command.CommandCompleted:=True;

 {Check for CMD23}
 {Not required, handled by MMCDeviceSendCommand and caller}

 {Finished CMD23, now send actual command}
 {Not required, handled by MMCDeviceSendCommand and caller}


 if SDHCI.Command.DataCompleted then
  begin
   SDHCIHostFinishData(SDHCI);
  end;

 if SDHCI.Command.Data = nil then
  begin
   SDHCI.Command.Status:=MMC_STATUS_SUCCESS;

   SDHCIHostSignal(SDHCI,SDHCI.Wait);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdhci_finish_command in \drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_finish_command in \drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostFinishData(SDHCI:PSDHCIHost):LongWord;
{Default finish data function for SDHCI host controllers}
{Called by Interrupt Data handler when data is received}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Finish Data');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Finish Data when no current command');
   {$ENDIF}

   Exit;
  end;

 {Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Finish Data when no current data');
   {$ENDIF}

   Exit;
  end;

 {Check for DMA request}
 {Not required, handled by MMCDeviceSendCommand and SDHCIHostStopDMA}

 {Check for Error}
 if SDHCI.Command.Status <> MMC_STATUS_NOT_PROCESSED then
  begin
   {The specification states that the block count register must be updated, but it does not specify at what point in the data flow.
    That makes the register entirely useless to read back so we have to assume that nothing made it to the card in the event of an error}
   SDHCI.Command.Data.BytesTransfered:=0;
  end
 else
  begin
   SDHCI.Command.Data.BytesTransfered:=(SDHCI.Command.Data.BlockSize * SDHCI.Command.Data.BlockCount);
  end;

 {Check for CMD12}
 {Need to send CMD12 if a) open-ended multiblock transfer (no CMD23) or b) error in multiblock transfer}
 if SDHCI.Command.Status <> MMC_STATUS_NOT_PROCESSED then
  begin
   {Not required, handled by MMCDeviceSendCommand and caller}
   Exit;
  end;

 SDHCI.Command.DataCompleted:=True;

 if SDHCI.Command.CommandCompleted then
  begin
   SDHCI.Command.Status:=MMC_STATUS_SUCCESS;

   SDHCIHostSignal(SDHCI,SDHCI.Wait);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdhci_finish_data in \drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_finish_data in \drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostCommandInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord;var ReturnMask:LongWord):LongWord;
{Default command interrupt processing function for SDHCI host controllers}
{Called by SDHCI controller interrupt handler when a command interrupt is received}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Command Interrupt');
 {$ENDIF}

 {Update Statistics}
 Inc(SDHCI.CommandInterruptCount);

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Command Interrupt when no current command (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}

   Exit;
  end;

 {Update Return Mask}
 {ReturnMask:=InterruptMask;} {Do not update from InterruptMask}

 {Check Timeout}
 if (InterruptMask and SDHCI_INT_TIMEOUT) <> 0 then
  begin
   SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;

   SDHCIHostSignal(SDHCI,SDHCI.Wait);
   Exit;
  end;

 {Check Invalid Sequence}
 if (InterruptMask and (SDHCI_INT_CRC or SDHCI_INT_END_BIT or SDHCI_INT_INDEX)) <> 0 then
  begin
   SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;

   SDHCIHostSignal(SDHCI,SDHCI.Wait);
   Exit;
  end;

 {The host can send an interrupt when the busy state has ended, allowing us to wait without wasting CPU cycles.
  Unfortunately this is overloaded on the "data complete" interrupt, so we need to take some care when handling it.
  Note: The 1.0 specification is a bit ambiguous about this feature so there might be some problems with older controllers}
 if (SDHCI.Command.ResponseType and MMC_RSP_BUSY) <> 0 then
  begin
   if SDHCI.Command.Data <> nil then
    begin
     {$IFDEF INTERRUPT_DEBUG}
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Command Interrupt cannot wait for busy end when data transfer current');
     {$ENDIF}
    end
   else if ((SDHCI.Quirks and SDHCI_QUIRK_NO_BUSY_IRQ) = 0) and (not(SDHCI.Command.BusyCompleted)) then
    begin
     {Mark that command completed before busy is ended}
     SDHCI.Command.BusyCompleted:=True;

     Exit;
    end;
   {The controller does not support the end-of-busy IRQ fall through and take the SDHCI_INT_RESPONSE}
  end
 else if ((SDHCI.Quirks2 and SDHCI_QUIRK2_STOP_WITH_TC) <> 0) and (SDHCI.Command.Command = MMC_CMD_STOP_TRANSMISSION) and (SDHCI.Command.Data = nil) then
  begin
   ReturnMask:=ReturnMask and not(SDHCI_INT_DATA_END);
  end;

 {Check Response}
 if (InterruptMask and SDHCI_INT_RESPONSE) <> 0 then
  begin
   SDHCIHostFinishCommand(SDHCI);
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdhci_cmd_irq in \drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_cmd_irq in \drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;
{Default data interrupt processing function for SDHCI host controllers}
{Called by SDHCI controller interrupt handler when a data interrupt is received}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 Command:Word;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Data Interrupt');
 {$ENDIF}

 {Update Statistics}
 Inc(SDHCI.DataInterruptCount);

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Data Interrupt when no current command (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}

   Exit;
  end;

 {CMD19 generates only Buffer Read Ready interrupt}
 if (InterruptMask and SDHCI_INT_DATA_AVAIL) <> 0 then
  begin
   Command:=SDHCIGetCommand(SDHCIHostReadWord(SDHCI,SDHCI_COMMAND));
   if (Command = MMC_CMD_SEND_TUNING_BLOCK) or (Command = MMC_CMD_SEND_TUNING_BLOCK_HS200) then
    begin
     SDHCI.Command.TuningCompleted:=True;

     //To Do
     Exit;
    end;
  end;

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {The "data complete" interrupt is also used to indicate that a busy state has ended. See comment above in SDHCIHostCommandInterrupt}
   if (SDHCI.Command.ResponseType and MMC_RSP_BUSY) <> 0 then
    begin
     {Check Timeout}
     if (InterruptMask and SDHCI_INT_DATA_TIMEOUT) <> 0 then
      begin
       SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;

       SDHCIHostSignal(SDHCI,SDHCI.Wait);
       Exit;
      end;

     {Check Data End}
     if (InterruptMask and SDHCI_INT_DATA_END) <> 0 then
      begin
       {Some cards handle busy-end interrupt before the command completed, so make sure we do things in the proper order}
       if SDHCI.Command.BusyCompleted then
        begin
         SDHCIHostFinishCommand(SDHCI);
        end
       else
        begin
         SDHCI.Command.BusyCompleted:=True;
        end;

       Exit;
      end;
    end;

   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Data Interrupt when no current data');
   {$ENDIF}

   Exit;
  end;

 {Check Timeout}
 if (InterruptMask and SDHCI_INT_DATA_TIMEOUT) <> 0 then
  begin
   SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;

   SDHCIHostFinishData(SDHCI);
   SDHCIHostSignal(SDHCI,SDHCI.Wait);
   Exit;
  end;

 {Check Invalid Sequence}
 if (InterruptMask and SDHCI_INT_DATA_END_BIT) <> 0 then
  begin
   SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;

   SDHCIHostFinishData(SDHCI);
   SDHCIHostSignal(SDHCI,SDHCI.Wait);
   Exit;
  end;
 if ((InterruptMask and SDHCI_INT_DATA_CRC) <> 0) and (SDHCIGetCommand(SDHCIHostReadWord(SDHCI,SDHCI_COMMAND)) <> MMC_CMD_BUS_TEST_R) then
  begin
   SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;

   SDHCIHostFinishData(SDHCI);
   SDHCIHostSignal(SDHCI,SDHCI.Wait);
   Exit;
  end;

 {Check Data Available / Space Available}
 if (InterruptMask and (SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL)) <> 0 then
  begin
   SDHCIHostTransferPIO(SDHCI);
  end;

 {Check for DMA End}
 if (InterruptMask and SDHCI_INT_DMA_END) <> 0 then
  begin
   SDHCIHostTransferDMA(SDHCI);
  end;

 {Check for Data End}
 if (InterruptMask and SDHCI_INT_DATA_END) <> 0 then
  begin
   if not(SDHCI.Command.CommandCompleted) then
    begin
     {Data finished before the command completed}
     SDHCI.Command.DataCompleted:=True;
    end
   else
    begin
     SDHCIHostFinishData(SDHCI);
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;

 //See: sdhci_data_irq in \drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_data_irq in \drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
{Default host start function for SDHCI host controllers}
{Called automatically to start each registered SDHCI controller when required}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 MMC:PMMCDevice;
 Status:LongWord;
 Capabilities:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host Start');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if SDHCI.SDHCIState = SDHCI_STATE_ENABLED then Exit;

 {Check Start/Stop}
 Result:=ERROR_INVALID_PARAMETER;
 if not(Assigned(SDHCI.HostStart)) then Exit;
 if not(Assigned(SDHCI.HostStop)) then Exit;

 {Check Host Flags}
 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_NON_STANDARD) <> 0 then
  begin
   {Call Host Start}
   Result:=SDHCI.HostStart(SDHCI);
  end
 else
  begin
   {Call Host Start}
   if SDHCI.HostStart(SDHCI) <> ERROR_SUCCESS then Exit;

   {Check Version}
   if SDHCIGetVersion(SDHCI) > SDHCI_SPEC_420 then
    begin
     if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Unknown host controller version');
    end;

   {Get Capabilities}
   Capabilities:=SDHCIHostReadLong(SDHCI,SDHCI_CAPABILITIES);
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Capabilities = ' + IntToHex(Capabilities,8));
   {$ENDIF}

   {Check SDMA Support}
   if (SDHCI.Quirks and SDHCI_QUIRK_FORCE_DMA) <> 0 then
    begin
     SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags or SDHCI_FLAG_SDMA;
    end
   else if (Capabilities and SDHCI_CAN_DO_SDMA) = 0 then
    begin
     if (SDHCI.Quirks and SDHCI_QUIRK_MISSING_CAPS) = 0 then
      begin
       if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Host does not support SDMA');
      end;
    end
   else
    begin
     SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags or SDHCI_FLAG_SDMA;
    end;

   if ((SDHCI.Quirks and SDHCI_QUIRK_BROKEN_DMA) <> 0) and ((SDHCI.Device.DeviceFlags and SDHCI_FLAG_SDMA) <> 0) then
    begin
     if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Disabling broken DMA support');

     SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags and not(SDHCI_FLAG_SDMA);
    end;

   {Check ADMA Support}
   if (SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_200) and ((Capabilities and SDHCI_CAN_DO_ADMA2) <> 0) then
    begin
     SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags or SDHCI_FLAG_ADMA;
    end;

   if ((SDHCI.Quirks and SDHCI_QUIRK_BROKEN_ADMA) <> 0) and ((SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0) then
    begin
     if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Disabling broken ADMA support');

     SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags and not(SDHCI_FLAG_ADMA);
    end;

   {Check 64-bit DMA support}
   if (SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_410) and (SDHCI.EnableV4Mode) then
    begin
     if (Capabilities and SDHCI_CAN_64BIT_V4) <> 0 then
      begin
       SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags or SDHCI_FLAG_64_BIT_DMA;
      end;
    end
   else
    begin
     if (Capabilities and SDHCI_CAN_64BIT) <> 0 then
      begin
       SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags or SDHCI_FLAG_64_BIT_DMA;
      end;
    end;

   {SDMA does not support 64-bit DMA if not in V4 mode}
   if ((SDHCI.Device.DeviceFlags and SDHCI_FLAG_64_BIT_DMA) <> 0) and not(SDHCI.EnableV4Mode) then
    begin
     SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags and not(SDHCI_FLAG_SDMA);
    end;

   {Check Clock Maximum}
   if SDHCI.ClockMaximum <> 0 then
    begin
     SDHCI.MaximumFrequency:=SDHCI.ClockMaximum;
    end
   else
    begin
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       SDHCI.MaximumFrequency:=((Capabilities and SDHCI_CLOCK_V3_BASE_MASK) shr SDHCI_CLOCK_BASE_SHIFT);
      end
     else
      begin
       SDHCI.MaximumFrequency:=((Capabilities and SDHCI_CLOCK_BASE_MASK) shr SDHCI_CLOCK_BASE_SHIFT);
      end;
     SDHCI.MaximumFrequency:=(SDHCI.MaximumFrequency * SDHCI_CLOCK_BASE_MULTIPLIER);
    end;
   if SDHCI.MaximumFrequency = 0 then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Host does not specify a maximum clock frequency');

     SDHCI.HostStop(SDHCI);
     Exit;
    end;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host maximum frequency = ' + IntToStr(SDHCI.MaximumFrequency));
   {$ENDIF}

   {Check Clock Minimum}
   if SDHCI.ClockMinimum <> 0 then
    begin
     SDHCI.MinimumFrequency:=SDHCI.ClockMinimum;
    end
   else
    begin
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       SDHCI.MinimumFrequency:=SDHCI.MaximumFrequency div SDHCI_MAX_CLOCK_DIV_SPEC_300;
      end
     else
      begin
       SDHCI.MinimumFrequency:=SDHCI.MaximumFrequency div SDHCI_MAX_CLOCK_DIV_SPEC_300;
      end;
    end;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host minimum frequency = ' + IntToStr(SDHCI.MinimumFrequency));
   {$ENDIF}

   {Determine Voltages}
   SDHCI.Voltages:=0;
   if (Capabilities and SDHCI_CAN_VDD_330) <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or MMC_VDD_32_33 or MMC_VDD_33_34;
    end;
   if (Capabilities and SDHCI_CAN_VDD_300) <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or MMC_VDD_29_30 or MMC_VDD_30_31;
    end;
   if (Capabilities and SDHCI_CAN_VDD_180) <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or MMC_VDD_165_195;
    end;
   {Check Presets}
   if SDHCI.PresetVoltages <> 0 then
    begin
     SDHCI.Voltages:=SDHCI.Voltages or SDHCI.PresetVoltages;
    end;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host voltages = ' + IntToHex(SDHCI.Voltages,8));
   {$ENDIF}

   {Determine Capabilities}
   SDHCI.Capabilities:=MMC_CAP_SD_HIGHSPEED or MMC_CAP_MMC_HIGHSPEED;
   if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
    begin
     if (Capabilities and SDHCI_CAN_DO_8BIT) <> 0 then
      begin
       {Don't report 8 bit data unless the host explicitly adds it to the presets}
       {Some host may be 8 bit capable but only have 4 data lines connected}
       {SDHCI.Capabilities:=SDHCI.Capabilities or MMC_CAP_8_BIT_DATA;}
      end;
    end;
   {Check Presets}
   if SDHCI.PresetCapabilities <> 0 then
    begin
     SDHCI.Capabilities:=SDHCI.Capabilities or SDHCI.PresetCapabilities;
    end;
   if (SDHCI.Quirks and SDHCI_QUIRK_FORCE_1_BIT_DATA) = 0 then
    begin
     SDHCI.Capabilities:=SDHCI.Capabilities or MMC_CAP_4_BIT_DATA;
    end;
   if (SDHCI.Quirks2 and SDHCI_QUIRK2_HOST_NO_CMD23) <> 0 then
    begin
     SDHCI.Capabilities:=SDHCI.Capabilities and not(MMC_CAP_CMD23);
    end;
   if SDHCI.PresetCapabilities2 <> 0 then
    begin
     SDHCI.Capabilities2:=SDHCI.Capabilities2 or SDHCI.PresetCapabilities2;
    end;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host capabilities = ' + IntToHex(SDHCI.Capabilities,8));
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host additional capabilities = ' + IntToHex(SDHCI.Capabilities2,8));
   {$ENDIF}

   {Set Maximum Request Size (Limited by SDMA boundary of 512KB)}
   SDHCI.MaximumRequestSize:=SIZE_512K;

   {Determine Maximum Block Size}
   if (SDHCI.Quirks and SDHCI_QUIRK_FORCE_BLK_SZ_2048) <> 0 then
    begin
     SDHCI.MaximumBlockSize:=2; {512 shl 2 = 2048}
    end
   else
    begin
     SDHCI.MaximumBlockSize:=((Capabilities and SDHCI_MAX_BLOCK_MASK) shr SDHCI_MAX_BLOCK_SHIFT);
     if SDHCI.MaximumBlockSize >= 3 then
      begin
       if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Invalid maximum block size, assuming 512 bytes');

       SDHCI.MaximumBlockSize:=0; {512 shl 0 = 512}
      end;
    end;
   SDHCI.MaximumBlockSize:=512 shl SDHCI.MaximumBlockSize;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host maximum block size = ' + IntToStr(SDHCI.MaximumBlockSize));
   {$ENDIF}

   {Determine Maximum Block Count}
   SDHCI.MaximumBlockCount:=MMC_MAX_BLOCK_COUNT;
   if (SDHCI.Quirks and SDHCI_QUIRK_NO_MULTIBLOCK) <> 0 then SDHCI.MaximumBlockCount:=1;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host maximum block count = ' + IntToStr(SDHCI.MaximumBlockCount));
   {$ENDIF}

   {Set Minimum DMA Size}
   SDHCI.MinimumDMASize:=MMC_MAX_BLOCK_LEN;

   {Set Maximum PIO Blocks}
   SDHCI.MaximumPIOBlocks:=0;

   {Set SDMA Buffer Boundary}
   SDHCI.SDMABoundary:=SDHCI_DEFAULT_BOUNDARY_ARG;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI SDMA buffer boundary = ' + IntToStr(1 shl (SDHCI.SDMABoundary + 12)));
   {$ENDIF}

   {Set ADMA Table Properties}
   SDHCI.ADMATableCount:=SDHCI_MAX_SEGS * 2 + 1;
   if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0 then
    begin
     {Get ADMA Descriptor Size}
     if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_64_BIT_DMA) = 0 then
      begin
       SDHCI.ADMADescriptorSize:=SDHCI_ADMA2_32_DESC_SIZE;
      end
     else
      begin
       if SDHCI.EnableV4Mode then SDHCI.ADMADescriptorSize:=SDHCI_ADMA2_64_DESC_V4_SIZE else SDHCI.ADMADescriptorSize:=SDHCI_ADMA2_64_DESC_SIZE;
      end;

     {Get ADMA Table Size}
     SDHCI.ADMATableSize:=SDHCI.ADMATableCount * SDHCI.ADMADescriptorSize;

     {Get ADMA Buffer Size}
     SDHCI.ADMABufferSize:=SDHCI_MAX_SEGS * SDHCI_ADMA2_ALIGN;

     {Allocate ADMA Table}
     SDHCI.ADMATable:=DMABufferAllocate(DMAHostGetDefault,SDHCI.ADMATableSize);
     if (SDHCI.ADMATable = nil) or (Align(SDHCI.ADMATable,SDHCI_ADMA2_ALIGN) <> SDHCI.ADMATable) then
      begin
       if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Failed to allocate memory for ADMA table, reverting to SDMA only');

       SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags and not(SDHCI_FLAG_ADMA);

       {Release ADMA Table}
       if SDHCI.ADMATable <> nil then
        begin
         DMABufferRelease(SDHCI.ADMATable);

         SDHCI.ADMATable:=nil;
        end;
      end
     else
      begin
       {Allocate ADMA Buffer}
       SDHCI.ADMABuffer:=DMABufferAllocate(DMAHostGetDefault,SDHCI.ADMABufferSize);
       if (SDHCI.ADMABuffer = nil) or (Align(SDHCI.ADMABuffer,SDHCI_ADMA2_DESC_ALIGN) <> SDHCI.ADMABuffer) then
        begin
         if MMC_LOG_ENABLED then MMCLogWarn(nil,'SDHCI Failed to allocate memory for ADMA buffer, reverting to SDMA only');

         SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags and not(SDHCI_FLAG_ADMA);

         {Release ADMA Table}
         if SDHCI.ADMATable <> nil then
          begin
           DMABufferRelease(SDHCI.ADMATable);

           SDHCI.ADMATable:=nil;
          end;

         {Release ADMA Buffer}
         if SDHCI.ADMABuffer <> nil then
          begin
           DMABufferRelease(SDHCI.ADMABuffer);

           SDHCI.ADMABuffer:=nil;
          end;
        end
       else
        begin
         {Check Cache}
         if not(DMA_CACHE_COHERENT) then
          begin
           {Clean Cache (Dest)}
           CleanDataCacheRange(PtrUInt(SDHCI.ADMATable),SDHCI.ADMATableSize);
           CleanDataCacheRange(PtrUInt(SDHCI.ADMABuffer),SDHCI.ADMABufferSize);
          end;
        end;
      end;
    end;

   {Update Auto Flags}
   if (SDHCI.Quirks and SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12) <> 0 then
    begin
     SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags or SDHCI_FLAG_AUTO_CMD12;
    end;
   if not(SDHCI.EnableV4Mode) and ((SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) = 0) then
    begin
     {In v3 mode, Auto-CMD23 only works for ADMA or PIO (or External DMA)}
     {In v4 mode, Auto-CMD23 works for SDMA as well}
     if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_SDMA) <> 0 then
      begin
       SDHCI.Device.DeviceFlags:=SDHCI.Device.DeviceFlags and not(SDHCI_FLAG_AUTO_CMD23);
      end;
    end;

   {Host reset done by host start}

   {Create MMC}
   MMC:=MMCDeviceCreate;
   if MMC = nil then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Failed to create new MMC device');

     SDHCI.HostStop(SDHCI);
     Exit;
    end;

   {Update MMC}
   {Device}
   MMC.Device.DeviceBus:=DEVICE_BUS_MMC;
   MMC.Device.DeviceType:=MMC_TYPE_MMC;
   MMC.Device.DeviceFlags:=MMC_FLAG_NONE;
   MMC.Device.DeviceData:=SDHCI;
   MMC.Device.DeviceDescription:=MMC_DEVICE_DESCRIPTION;
   {MMC}
   MMC.MMCState:=MMC_STATE_EJECTED;
   MMC.DeviceInitialize:=SDHCI.DeviceInitialize;
   MMC.DeviceDeinitialize:=SDHCI.DeviceDeinitialize;
   MMC.DeviceGetCardDetect:=SDHCI.DeviceGetCardDetect;
   MMC.DeviceGetWriteProtect:=SDHCI.DeviceGetWriteProtect;
   MMC.DeviceSendCommand:=SDHCI.DeviceSendCommand;
   MMC.DeviceSetIOS:=SDHCI.DeviceSetIOS;
   {Driver}
   MMC.Voltages:=SDHCI.Voltages;
   MMC.Capabilities:=SDHCI.Capabilities;
   MMC.Capabilities2:=SDHCI.Capabilities2;

   {Create Storage}
   MMC.Storage:=StorageDeviceCreate;
   if MMC.Storage = nil then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Failed to create new Storage device');

     MMCDeviceDestroy(MMC);
     SDHCI.HostStop(SDHCI);
     Exit;
    end;

   {Update Storage}
   {Device}
   MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
   MMC.Storage.Device.DeviceType:=STORAGE_TYPE_REMOVABLE;
   MMC.Storage.Device.DeviceFlags:=STORAGE_FLAG_REMOVABLE or STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA;
   MMC.Storage.Device.DeviceData:=MMC;
   MMC.Storage.Device.DeviceDescription:=MMC_STORAGE_DESCRIPTION;
   {Storage}
   MMC.Storage.StorageState:=STORAGE_STATE_EJECTED;
   MMC.Storage.DeviceRead:=MMCStorageDeviceRead;
   MMC.Storage.DeviceWrite:=MMCStorageDeviceWrite;
   MMC.Storage.DeviceErase:=MMCStorageDeviceErase;
   MMC.Storage.DeviceControl:=MMCStorageDeviceControl;

   {Initialize MMC}
   Status:=MMCDeviceInitialize(MMC);
   if (Status <> MMC_STATUS_SUCCESS) and (Status <> MMC_STATUS_NO_MEDIA) then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Failed to initialize new MMC device');

     StorageDeviceDestroy(MMC.Storage);
     MMCDeviceDestroy(MMC);
     SDHCI.HostStop(SDHCI);
     Exit;
    end;

   {Check MMC Type}
   if MMC.Device.DeviceType = MMC_TYPE_SDIO then
    begin
     {Destroy Storage}
     StorageDeviceDestroy(MMC.Storage);
     MMC.Storage:=nil;
    end;

   {Register MMC}
   if MMCDeviceRegister(MMC) <> MMC_STATUS_SUCCESS then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Failed to register new MMC device');

     StorageDeviceDestroy(MMC.Storage);
     MMCDeviceDestroy(MMC);
     SDHCI.HostStop(SDHCI);
     Exit;
    end;

   {Check Non Removeable}
   if StrToIntDef(EnvironmentGet(DeviceGetName(@MMC.Device) + '_NON_REMOVABLE'),0) = 1 then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_NON_REMOVABLE;

   {Enable Host}
   SDHCI.SDHCIState:=SDHCI_STATE_ENABLED;

   {Notify Enable}
   NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_ENABLE);

   {Notify Insert MMC}
   if MMC.MMCState = MMC_STATE_INSERTED then
    begin
     NotifierNotify(@MMC.Device,DEVICE_NOTIFICATION_INSERT);
    end;

   {Check Storage}
   if MMC.Storage <> nil then
    begin
     {Set Storage State to Inserted}
     if MMC.MMCState = MMC_STATE_INSERTED then
      begin
       StorageDeviceSetState(MMC.Storage,STORAGE_STATE_INSERTED);
      end;

     {Start Storage Status Checking}
     if StorageDeviceStartStatus(MMC.Storage,MMC_STATUS_TIMER_INTERVAL) <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Failed start status for new MMC device');
      end;
    end;

   {Check SDIO}
   if MMC.SDIOCount > 0 then
    begin
     {Bind Functions}
     SDIODeviceBindFunctions(MMC);
    end;

   Result:=ERROR_SUCCESS;
  end;

 //See: add_sdhci in sdhci.c
 //     sdhci_setup_host in sdhci.c
end;

{==============================================================================}

function SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
{Default host stop function for SDHCI host controllers}
{Called automatically to stop each registered SDHCI controller when required}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if SDHCI.SDHCIState <> SDHCI_STATE_ENABLED then Exit;

 {Check Stop}
 Result:=ERROR_INVALID_PARAMETER;
 if not(Assigned(SDHCI.HostStop)) then Exit;

 {Check Host Flags}
 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_NON_STANDARD) <> 0 then
  begin
   {Call Host Stop}
   Result:=SDHCI.HostStop(SDHCI);
  end
 else
  begin
   {Get MMC}
   MMC:=MMCDeviceFindByDevice(@SDHCI.Device);
   if MMC <> nil then
    begin
     {Check SDIO}
     if MMC.SDIOCount > 0 then
      begin
       {Unbind Functions}
       SDIODeviceUnbindFunctions(MMC,nil);
      end;

     {Check Storage}
     if MMC.Storage <> nil then
      begin
       {Stop Storage Status Checking}
       StorageDeviceStopStatus(MMC.Storage);

       {Set Storage State to Inserted}
       StorageDeviceSetState(MMC.Storage,STORAGE_STATE_EJECTED);
      end;

     {Eject MMC}
     MMC.MMCState:=MMC_STATE_EJECTED;

     {Notify Eject MMC}
     NotifierNotify(@MMC.Device,DEVICE_NOTIFICATION_EJECT);
    end;

   {Call Host Stop}
   if SDHCI.HostStop(SDHCI) <> ERROR_SUCCESS then Exit;

   {Disable Host}
   SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;

   {Notify Disable}
   NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_DISABLE);

   {Check MMC}
   if MMC <> nil then
    begin
     {Dergister MMC}
     MMCDeviceDeregister(MMC);

     {Check Storage}
     if MMC.Storage <> nil then
      begin
       {Destroy Storage}
       StorageDeviceDestroy(MMC.Storage);
      end;

     {Destroy MMC}
     MMCDeviceDestroy(MMC);
    end;

   {Free DMA Buffer}
   if SDHCI.DMABuffer <> nil then
    begin
     DMABufferRelease(SDHCI.DMABuffer);

     SDHCI.DMABuffer:=nil;
    end;

   {Free ADMA Table}
   if SDHCI.ADMATable <> nil then
    begin
     DMABufferRelease(SDHCI.ADMATable);

     SDHCI.ADMATable:=nil;
    end;

   {Free ADMA Buffer}
   if SDHCI.ADMABuffer <> nil then
    begin
     DMABufferRelease(SDHCI.ADMABuffer);

     SDHCI.ADMABuffer:=nil;
    end;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SDHCIHostLock(SDHCI:PSDHCIHost):LongWord; inline;
{Default host lock function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Host Lock}
 if Assigned(SDHCI.HostLock) then
  begin
   {Host Lock Method}
   Result:=SDHCI.HostLock(SDHCI);
  end
 else
  begin
   {Default Lock Method}
   Result:=SpinLockIRQ(SDHCI.Spin);
  end;
end;

{==============================================================================}

function SDHCIHostUnlock(SDHCI:PSDHCIHost):LongWord; inline;
{Default host unlock function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Host Unlock}
 if Assigned(SDHCI.HostUnlock) then
  begin
   {Host Unlock Method}
   Result:=SDHCI.HostUnlock(SDHCI);
  end
 else
  begin
   {Default Unlock Method}
   Result:=SpinUnlockIRQ(SDHCI.Spin);
  end;
end;

{==============================================================================}

function SDHCIHostSignal(SDHCI:PSDHCIHost;Semaphore:TSemaphoreHandle):LongWord; inline;
{Default host semaphore signal function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Host Signal}
 if Assigned(SDHCI.HostSignal) then
  begin
   {Host Signal Method}
   Result:=SDHCI.HostSignal(SDHCI,Semaphore);
  end
 else
  begin
   {Default Signal Method}
   Result:=SemaphoreSignal(Semaphore);
  end;
end;

{==============================================================================}

function SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; inline;
{Default read byte function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Read Byte}
 if Assigned(SDHCI.HostReadByte) then
  begin
   {Host Read Byte Method}
   Result:=SDHCI.HostReadByte(SDHCI,Reg);
  end
 else
  begin
   {Default Read Byte Method}
   Result:=PByte(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;
end;

{==============================================================================}

function SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; inline;
{Default read word function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Read Word}
 if Assigned(SDHCI.HostReadWord) then
  begin
   {Host Read Word Method}
   Result:=SDHCI.HostReadWord(SDHCI,Reg);
  end
 else
  begin
   {Default Read Word Method}
   Result:=PWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;
end;

{==============================================================================}

function SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; inline;
{Default read longword function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Read Long}
 if Assigned(SDHCI.HostReadLong) then
  begin
   {Host Read Long Method}
   Result:=SDHCI.HostReadLong(SDHCI,Reg);
  end
 else
  begin
   {Default Read Long Method}
   Result:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;
end;

{==============================================================================}

procedure SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); inline;
{Default write byte function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Write Byte}
 if Assigned(SDHCI.HostWriteByte) then
  begin
   {Host Write Byte Method}
   SDHCI.HostWriteByte(SDHCI,Reg,Value);
  end
 else
  begin
   {Default Write Byte Method}
   PByte(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;
end;

{==============================================================================}

procedure SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); inline;
{Default write word function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Write Word}
 if Assigned(SDHCI.HostWriteWord) then
  begin
   {Host Write Word Method}
   SDHCI.HostWriteWord(SDHCI,Reg,Value);
  end
 else
  begin
   {Default Write Word Method}
   PWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;
end;

{==============================================================================}

procedure SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); inline;
{Default write longword function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check Write Long}
 if Assigned(SDHCI.HostWriteLong) then
  begin
   {Host Write Long Method}
   SDHCI.HostWriteLong(SDHCI,Reg,Value);
  end
 else
  begin
   {Default Write Long Method}
   PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;
end;

{==============================================================================}

function SDHCIHostSetClockDivider(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;
{Default set clock divider function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Check Set Clock Divider}
 if Assigned(SDHCI.HostSetClockDivider) then
  begin
   {Host Set Clock Divider Method}
   Result:=SDHCI.HostSetClockDivider(SDHCI,Index,Divider);
  end
 else
  begin
   {Default Set Clock Divider Method}
   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_host->set_clock in sdhci.h
end;

{==============================================================================}

function SDHCIHostSetControlRegister(SDHCI:PSDHCIHost):LongWord;
{Default set control register function for SDHCI host controllers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Check Set Control Register}
 if Assigned(SDHCI.HostSetControlRegister) then
  begin
   {Host Set Control Register Method}
   Result:=SDHCI.HostSetControlRegister(SDHCI);
  end
 else
  begin
   {Default Set Control Register Method}
   Result:=MMC_STATUS_SUCCESS;
  end;

 //See: sdhci_host->set_control_reg in sdhci.h
end;

{==============================================================================}

function SDHCIHostGetADMAAddress(SDHCI:PSDHCIHost):PtrUInt;
{Get the DMA address of the ADMA table for the current request}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=0;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Check Bus Addresses}
 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_BUS_ADDRESSES) <> 0 then
  begin
   Result:=PhysicalToBusAddress(SDHCI.ADMATable);
  end
 else
  begin
   Result:=PtrUInt(SDHCI.ADMATable);
  end;
end;

{==============================================================================}

procedure SDHCIHostSetADMAAddress(SDHCI:PSDHCIHost;Address:PtrUInt);
{Set the address of the transfer data in the Advanced DMA (ADMA) registers}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Set ADMA Address (Address=' + AddrToHex(Address) + ')');
 {$ENDIF}

 {$IFDEF CPU32}
 {Write ADMA address}
 SDHCIHostWriteLong(SDHCI,SDHCI_ADMA_ADDRESS,Address);

 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_64_BIT_DMA) <> 0 then
  begin
   SDHCIHostWriteLong(SDHCI,SDHCI_ADMA_ADDRESS_HI,0);
  end;
 {$ENDIF CPU32}

 {$IFDEF CPU64}
 {Write ADMA address (Low 32 bits)}
 SDHCIHostWriteLong(SDHCI,SDHCI_ADMA_ADDRESS,Int64Rec(Address).Lo);

 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_64_BIT_DMA) <> 0 then
  begin
   {Write ADMA address (High 32 bits)}
   SDHCIHostWriteLong(SDHCI,SDHCI_ADMA_ADDRESS_HI,Int64Rec(Address).Hi);
  end;
 {$ENDIF CPU64}

 //See: sdhci_set_adma_addr in sdhci.c
end;

{==============================================================================}

function SDHCIHostGetSDMAAddress(SDHCI:PSDHCIHost;Command:PMMCCommand):PtrUInt;
{Get the DMA address of the transfer data for the current request}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 Result:=0;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Check Bounce Buffer}
 if SDHCI.DMABuffer <> nil then
  begin
   if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_BUS_ADDRESSES) <> 0 then
    begin
     Result:=PhysicalToBusAddress(SDHCI.DMABuffer);
    end
   else
    begin
     Result:=PtrUInt(SDHCI.DMABuffer);
    end;
  end
 else
  begin
   if (Command = nil) or (Command.Data = nil) then Exit;

   if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_BUS_ADDRESSES) <> 0 then
    begin
     Result:=PhysicalToBusAddress(Command.Data.Data);
    end
   else
    begin
     Result:=PtrUInt(Command.Data.Data);
    end;
  end;

 //See: sdhci_sdma_address in sdhci.c
end;

{==============================================================================}

procedure SDHCIHostSetSDMAAddress(SDHCI:PSDHCIHost;Address:PtrUInt);
{Set the address of the transfer data in the Simple DMA (SDMA) register}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Set SDMA Address (Address=' + AddrToHex(Address) + ')');
 {$ENDIF}

 if SDHCI.EnableV4Mode then
  begin
   {Write ADMA Address}
   SDHCIHostSetADMAAddress(SDHCI,Address);
  end
 else
  begin
   {Write SDMA Address}
   SDHCIHostWriteLong(SDHCI,SDHCI_DMA_ADDRESS,Address);
  end;

 //See: sdhci_set_sdma_addr in sdhci.c
end;

{==============================================================================}

procedure SDHCIHostWriteADMADescriptor(SDHCI:PSDHCIHost;var Descriptor:Pointer;Command,Len:Word;Address:PtrUInt);
{Write the properties to an ADMA descriptor}

{Note: Not intended to be called directly by applications, may be used by SDHCI drivers}
var
 ADMADescriptor:PSDHCIADMA2Descriptor64;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Write ADMA Descriptor (Descriptor=' + PtrToHex(Descriptor) + ' Command=' + IntToHex(Command,4) + ' Len=' + IntToStr(Len) + ' Address=' + AddrToHex(Address) + ')');
 {$ENDIF}

 {32-bit and 64-bit descriptors have these members in same position}
 ADMADescriptor:=PSDHCIADMA2Descriptor64(Descriptor);
 ADMADescriptor.Command:=WordNtoLE(Command);
 ADMADescriptor.Len:=WordNtoLE(Len);

 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_BUS_ADDRESSES) <> 0 then
  begin
   if Address <> 0 then Address:=PhysicalToBusAddress(Pointer(Address));
  end;

 {$IFDEF CPU32}
 ADMADescriptor.AddressLow:=LongWordNtoLE(Address);

 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_64_BIT_DMA) <> 0 then
  begin
   ADMADescriptor.AddressHigh:=0;
  end;
 {$ENDIF CPU32}

 {$IFDEF CPU64}
 ADMADescriptor.AddressLow:=LongWordNtoLE(Int64Rec(Address).Lo);

 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_64_BIT_DMA) <> 0 then
  begin
   ADMADescriptor.AddressHigh:=LongWordNtoLE(Int64Rec(Address).Hi);
  end;
 {$ENDIF CPU64}

 {Update Descriptor}
 Inc(Descriptor,SDHCI.ADMADescriptorSize);

 //See: __sdhci_adma_write_desc in sdhci.c
end;

{==============================================================================}

function SDHCIHostCreate:PSDHCIHost;
{Create a new SDHCI entry}
{Return: Pointer to new SDHCI entry or nil if SDHCI could not be created}
begin
 {}
 Result:=SDHCIHostCreateEx(SizeOf(TSDHCIHost));
end;

{==============================================================================}

function SDHCIHostCreateEx(Size:LongWord):PSDHCIHost;
{Create a new SDHCI entry}
{Size: Size in bytes to allocate for new SDHCI (Including the SDHCI entry)}
{Return: Pointer to new SDHCI entry or nil if SDHCI could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TSDHCIHost) then Exit;

 {Create SDHCI}
 Result:=PSDHCIHost(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=SDHCI_TYPE_NONE;
 Result.Device.DeviceFlags:=SDHCI_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update SDHCI}
 Result.SDHCIId:=DEVICE_ID_ANY;
 Result.SDHCIState:=SDHCI_STATE_DISABLED;
 Result.HostStart:=nil;
 Result.HostStop:=nil;
 Result.HostLock:=nil;
 Result.HostUnlock:=nil;
 Result.HostSignal:=nil;
 Result.HostReadByte:=nil;
 Result.HostReadWord:=nil;
 Result.HostReadLong:=nil;
 Result.HostWriteByte:=nil;
 Result.HostWriteWord:=nil;
 Result.HostWriteLong:=nil;
 Result.HostReset:=nil;
 Result.HostHardwareReset:=nil;
 Result.HostSetPower:=nil;
 Result.HostSetClock:=nil;
 Result.HostSetTiming:=nil;
 Result.HostSetBusWidth:=nil;
 Result.HostSetClockDivider:=nil;
 Result.HostSetControlRegister:=nil;
 Result.HostPrepareDMA:=nil;
 Result.HostStartDMA:=nil;
 Result.HostStopDMA:=nil;
 Result.HostSetupCardIRQ:=nil;
 Result.HostCompleteCardIRQ:=nil;
 Result.DeviceInitialize:=nil;
 Result.DeviceDeinitialize:=nil;
 Result.DeviceGetCardDetect:=nil;
 Result.DeviceGetWriteProtect:=nil;
 Result.DeviceSendCommand:=nil;
 Result.DeviceSetIOS:=nil;
 Result.Spin:=INVALID_HANDLE_VALUE;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Wait:=INVALID_HANDLE_VALUE;
 Result.DMAWait:=INVALID_HANDLE_VALUE;
 Result.CardIRQWorker:=INVALID_HANDLE_VALUE;

 {Create Spin Lock}
 Result.Spin:=SpinCreate;
 if Result.Spin = INVALID_HANDLE_VALUE then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to create spin lock for SDHCI host');

   SDHCIHostDestroy(Result);
   Result:=nil;
   Exit;
  end;

 {Create Mutex Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to create lock for SDHCI host');

   SDHCIHostDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SDHCIHostDestroy(SDHCI:PSDHCIHost):LongWord;
{Destroy an existing SDHCI entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SDHCI}
 Result:=ERROR_IN_USE;
 if SDHCIHostCheck(SDHCI) = SDHCI then Exit;

 {Check State}
 if SDHCI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Mutex Lock}
 if SDHCI.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(SDHCI.Lock);
  end;

 {Destroy Spin Lock}
 if SDHCI.Spin <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(SDHCI.Spin);
  end;

 {Destroy SDHCI}
 Result:=DeviceDestroy(@SDHCI.Device);
end;

{==============================================================================}

function SDHCIHostRegister(SDHCI:PSDHCIHost):LongWord;
{Register a new SDHCI in the SDHCI table}
var
 SDHCIId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.SDHCIId <> DEVICE_ID_ANY then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 if not(Assigned(SDHCI.HostStart)) then Exit;
 if not(Assigned(SDHCI.HostStop)) then Exit;

 {Check SDHCI}
 Result:=ERROR_ALREADY_EXISTS;
 if SDHCIHostCheck(SDHCI) = SDHCI then Exit;

 {Check State}
 if SDHCI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert SDHCI}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update SDHCI}
    SDHCIId:=0;
    while SDHCIHostFind(SDHCIId) <> nil do
     begin
      Inc(SDHCIId);
     end;
    SDHCI.SDHCIId:=SDHCIId;

    {Update Device}
    SDHCI.Device.DeviceName:=SDHCI_NAME_PREFIX + IntToStr(SDHCI.SDHCIId);
    SDHCI.Device.DeviceClass:=DEVICE_CLASS_SDHCI;

    {Register Device}
    Result:=DeviceRegister(@SDHCI.Device);
    if Result <> ERROR_SUCCESS then
     begin
      SDHCI.SDHCIId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link SDHCI}
    if SDHCIHostTable = nil then
     begin
      SDHCIHostTable:=SDHCI;
     end
    else
     begin
      SDHCI.Next:=SDHCIHostTable;
      SDHCIHostTable.Prev:=SDHCI;
      SDHCIHostTable:=SDHCI;
     end;

    {Increment Count}
    Inc(SDHCIHostTableCount);

    {Check Started}
    if MMCStarted then
     begin
      if not MMC_ASYNCSTART then
       begin
        {Start Host}
        SDHCIHostStart(SDHCI);

        {Return Result}
        Result:=ERROR_SUCCESS;
       end
      else
       begin
        {Schedule Worker}
        Result:=WorkerSchedule(0,TWorkerTask(MMCAsyncStart),SDHCI,nil)
       end;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_SUCCESS;
     end;
   finally
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDHCIHostDeregister(SDHCI:PSDHCIHost):LongWord;
{Deregister a SDHCI from the SDHCI table}
var
 Prev:PSDHCIHost;
 Next:PSDHCIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.SDHCIId = DEVICE_ID_ANY then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SDHCI}
 Result:=ERROR_NOT_FOUND;
 if SDHCIHostCheck(SDHCI) <> SDHCI then Exit;

 {Check State}
 if SDHCI.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove SDHCI}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    if MMCStarted then
     begin
      {Stop Host}
      Result:=SDHCIHostStop(SDHCI);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Deregister Device}
    Result:=DeviceDeregister(@SDHCI.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink SDHCI}
    Prev:=SDHCI.Prev;
    Next:=SDHCI.Next;
    if Prev = nil then
     begin
      SDHCIHostTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(SDHCIHostTableCount);

    {Update SDHCI}
    SDHCI.SDHCIId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDHCIHostFind(SDHCIId:LongWord):PSDHCIHost;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=nil;

 {Check Id}
 if SDHCIId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SDHCI}
    SDHCI:=SDHCIHostTable;
    while SDHCI <> nil do
     begin
      {Check State}
      if SDHCI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if SDHCI.SDHCIId = SDHCIId then
         begin
          Result:=SDHCI;
          Exit;
         end;
       end;

      {Get Next}
      SDHCI:=SDHCI.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end;
end;

{==============================================================================}

function SDHCIHostEnumerate(Callback:TSDHCIEnumerate;Data:Pointer):LongWord;
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    Result:=ERROR_OPERATION_FAILED;

    {Get SDHCI}
    SDHCI:=SDHCIHostTable;
    while SDHCI <> nil do
     begin
      {Check State}
      if SDHCI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(SDHCI,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      SDHCI:=SDHCI.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SDHCIHostNotification(SDHCI:PSDHCIHost;Callback:TSDHCINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_SDHCI,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check SDHCI}
   if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@SDHCI.Device,DEVICE_CLASS_SDHCI,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{MMC Helper Functions}
function MMCGetCount:LongWord;
{Get the current MMC count}
begin
 {}
 Result:=MMCDeviceTableCount;
end;

{==============================================================================}

function MMCDeviceCheck(MMC:PMMCDevice):PMMCDevice;
{Check if the supplied MMC is in the MMC table}
var
 Current:PMMCDevice;
begin
 {}
 Result:=nil;

 {Check MMC}
 if MMC = nil then Exit;
 if MMC.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(MMCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get MMC}
    Current:=MMCDeviceTable;
    while Current <> nil do
     begin
      {Check MMC}
      if Current = MMC then
       begin
        Result:=MMC;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(MMCDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function MMCIsSD(MMC:PMMCDevice):Boolean;
begin
 {}
 Result:=False;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=(MMC.Version and SD_VERSION_SD) <> 0;
end;

{==============================================================================}

function MMCIsSDIO(MMC:PMMCDevice):Boolean;
begin
 {}
 Result:=False;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=(MMC.Version and SDIO_VERSION_SDIO) <> 0;
end;

{==============================================================================}

function MMCGetSDHCI(MMC:PMMCDevice):PSDHCIHost; inline;
begin
 {}
 Result:=nil;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=PSDHCIHost(MMC.Device.DeviceData);
end;

{==============================================================================}

function MMCGetCIDValue(MMC:PMMCDevice;Version,Value:LongWord):LongWord;
{Extract a CID field value from the 128 bit Card Identification register}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Version} {The selection of the format here is based upon published specs from sandisk and other sources}
 case Version of
  MMC_CSD_SPEC_VER_0,MMC_CSD_SPEC_VER_1:begin
    {MMC v1.0 - v1.2 / MMC v1.4}
    case Value of
     MMC_CID_MID:Result:=MMCExtractBits(@MMC.CardIdentification,104,24);
     MMC_CID_PNM0:Result:=MMCExtractBits(@MMC.CardIdentification,96,8);
     MMC_CID_PNM1:Result:=MMCExtractBits(@MMC.CardIdentification,88,8);
     MMC_CID_PNM2:Result:=MMCExtractBits(@MMC.CardIdentification,80,8);
     MMC_CID_PNM3:Result:=MMCExtractBits(@MMC.CardIdentification,72,8);
     MMC_CID_PNM4:Result:=MMCExtractBits(@MMC.CardIdentification,64,8);
     MMC_CID_PNM5:Result:=MMCExtractBits(@MMC.CardIdentification,56,8);
     MMC_CID_PNM6:Result:=MMCExtractBits(@MMC.CardIdentification,48,8);
     MMC_CID_PRV:Result:=MMCExtractBits(@MMC.CardIdentification,40,8);
     MMC_CID_HRV:Result:=MMCExtractBits(@MMC.CardIdentification,44,4);
     MMC_CID_FRV:Result:=MMCExtractBits(@MMC.CardIdentification,40,4);
     MMC_CID_PSN:Result:=MMCExtractBits(@MMC.CardIdentification,16,24);
     MMC_CID_MDT_MONTH:Result:=MMCExtractBits(@MMC.CardIdentification,12,4);
     MMC_CID_MDT_YEAR:Result:=MMCExtractBits(@MMC.CardIdentification,8,4) + 1997; {MMC cards year offset}
     MMC_CID_CRC:Result:=MMCExtractBits(@MMC.CardIdentification,1,7);
    end;
   end;
  MMC_CSD_SPEC_VER_2,MMC_CSD_SPEC_VER_3,MMC_CSD_SPEC_VER_4:begin
    {MMC v2.0 - v2.2 / MMC v3.1 - v3.3 / MMC v4}
    case Value of
     MMC_CID_MID:Result:=MMCExtractBits(@MMC.CardIdentification,120,8);
     MMC_CID_OID:Result:=MMCExtractBits(@MMC.CardIdentification,104,16);
     MMC_CID_PNM0:Result:=MMCExtractBits(@MMC.CardIdentification,96,8);
     MMC_CID_PNM1:Result:=MMCExtractBits(@MMC.CardIdentification,88,8);
     MMC_CID_PNM2:Result:=MMCExtractBits(@MMC.CardIdentification,80,8);
     MMC_CID_PNM3:Result:=MMCExtractBits(@MMC.CardIdentification,72,8);
     MMC_CID_PNM4:Result:=MMCExtractBits(@MMC.CardIdentification,64,8);
     MMC_CID_PNM5:Result:=MMCExtractBits(@MMC.CardIdentification,56,8);
     MMC_CID_PRV:Result:=MMCExtractBits(@MMC.CardIdentification,48,8);
     MMC_CID_HRV:Result:=MMCExtractBits(@MMC.CardIdentification,52,4);
     MMC_CID_FRV:Result:=MMCExtractBits(@MMC.CardIdentification,48,4);
     MMC_CID_PSN:Result:=MMCExtractBits(@MMC.CardIdentification,16,32);
     MMC_CID_MDT_MONTH:Result:=MMCExtractBits(@MMC.CardIdentification,12,4);
     MMC_CID_MDT_YEAR:Result:=MMCExtractBits(@MMC.CardIdentification,8,4) + 1997; {MMC cards year offset}
     MMC_CID_CRC:Result:=MMCExtractBits(@MMC.CardIdentification,1,7);
    end;
   end;
 end;
end;

{==============================================================================}

function MMCGetCSDValue(MMC:PMMCDevice;Value:LongWord):LongWord;
{Extract a CSD field value from the 128 bit Card Specific register}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 if Value = MMC_CSD_STRUCTURE then
  begin
   {Get CSD Structure version}
   Result:=MMCExtractBits(@MMC.CardSpecific,126,2);
  end
 else
  begin
   case Value of
    MMC_CSD_STRUCTURE:Result:=MMCExtractBits(@MMC.CardSpecific,126,2);
    MMC_CSD_SPECVER:Result:=MMCExtractBits(@MMC.CardSpecific,122,4);
    MMC_CSD_TAAC_VALUE:Result:=MMCExtractBits(@MMC.CardSpecific,115,4);
    MMC_CSD_TAAC_UNIT:Result:=MMCExtractBits(@MMC.CardSpecific,112,3);
    MMC_CSD_NSAC:Result:=MMCExtractBits(@MMC.CardSpecific,104,8);
    MMC_CSD_TRAN_SPEED_VALUE:Result:=MMCExtractBits(@MMC.CardSpecific,99,4);
    MMC_CSD_TRAN_SPEED_UNIT:Result:=MMCExtractBits(@MMC.CardSpecific,96,3);
    MMC_CSD_CCC:Result:=MMCExtractBits(@MMC.CardSpecific,84,12);
    MMC_CSD_READ_BL_LEN:Result:=MMCExtractBits(@MMC.CardSpecific,80,4);
    MMC_CSD_READ_BL_PARTIAL:Result:=MMCExtractBits(@MMC.CardSpecific,79,1);
    MMC_CSD_WRITE_BLK_MISALIGN:Result:=MMCExtractBits(@MMC.CardSpecific,78,1);
    MMC_CSD_READ_BLK_MISALIGN:Result:=MMCExtractBits(@MMC.CardSpecific,77,1);
    MMC_CSD_DSR_IMP:Result:=MMCExtractBits(@MMC.CardSpecific,76,1);
    MMC_CSD_C_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,62,12);
    MMC_CSD_VDD_R_CURR_MIN:Result:=MMCExtractBits(@MMC.CardSpecific,59,3);
    MMC_CSD_VDD_R_CURR_MAX:Result:=MMCExtractBits(@MMC.CardSpecific,56,3);
    MMC_CSD_VDD_W_CURR_MIN:Result:=MMCExtractBits(@MMC.CardSpecific,53,3);
    MMC_CSD_VDD_W_CURR_MAX:Result:=MMCExtractBits(@MMC.CardSpecific,50,3);
    MMC_CSD_C_SIZE_MULT:Result:=MMCExtractBits(@MMC.CardSpecific,47,3);
    {MMC_CSD_SECTOR_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,,);}
    MMC_CSD_ERASE_GRP_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,42,5);
    MMC_CSD_ERASE_GRP_MULT:Result:=MMCExtractBits(@MMC.CardSpecific,37,5);
    MMC_CSD_WP_GRP_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,32,5);
    MMC_CSD_WP_GRP_ENABLE:Result:=MMCExtractBits(@MMC.CardSpecific,31,1);
    MMC_CSD_DEFAULT_ECC:Result:=MMCExtractBits(@MMC.CardSpecific,29,2);
    MMC_CSD_R2W_FACTOR:Result:=MMCExtractBits(@MMC.CardSpecific,26,3);
    MMC_CSD_WRITE_BL_LEN:Result:=MMCExtractBits(@MMC.CardSpecific,22,4);
    MMC_CSD_WRITE_BL_PARTIAL:Result:=MMCExtractBits(@MMC.CardSpecific,21,1);
    MMC_CSD_CONTENT_PROT_APP:Result:=MMCExtractBits(@MMC.CardSpecific,16,1);
    MMC_CSD_FILE_FORMAT_GRP:Result:=MMCExtractBits(@MMC.CardSpecific,15,1);
    MMC_CSD_COPY:Result:=MMCExtractBits(@MMC.CardSpecific,14,1);
    MMC_CSD_PERM_WRITE_PROTECT:Result:=MMCExtractBits(@MMC.CardSpecific,13,1);
    MMC_CSD_TMP_WRITE_PROTECT:Result:=MMCExtractBits(@MMC.CardSpecific,12,1);
    MMC_CSD_FILE_FORMAT:Result:=MMCExtractBits(@MMC.CardSpecific,10,2);
    MMC_CSD_ECC:Result:=MMCExtractBits(@MMC.CardSpecific,8,2);
    MMC_CSD_CRC:Result:=MMCExtractBits(@MMC.CardSpecific,1,7);
   end;
  end;
end;

{==============================================================================}

function MMCExtractBits(Buffer:Pointer;Start,Size:LongWord):LongWord;
{Start is the starting bit to extract, Size is the number of bits to extract}
{Start is the LSB so to extract 8 bits from 127 to 120 then Start would be 120 and Size would be 8}
begin
 {}
 Result:=MMCExtractBitsEx(Buffer,4,Start,Size);
end;

{==============================================================================}

function MMCExtractBitsEx(Buffer:Pointer;Length,Start,Size:LongWord):LongWord;
{Length is the size of the buffer in LongWords, Start is the starting bit to extract, Size is the number of bits to extract}
{Start is the LSB so to extract 8 bits from 127 to 120 then Start would be 120 and Size would be 8}
{For a 128 bit buffer (16 bytes) Length would be 4}
{For a 512 bit buffer (64 bytes) Length would be 16}
var
 Mask:LongWord;
 Shift:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Length < 1 then Exit;

 {Check Size}
 if Size > 32 then Exit;

 {Check Start}
 if Start > ((Length shl 5) - 1) then Exit;
 if (Start + Size) > (Length shl 5) then Exit;

 {Get Mask}
 if Size < 32 then
  begin
   Mask:=(1 shl Size) - 1;
  end
 else
  begin
   Mask:=LongWord(0 - 1);
  end;

 {Get Offset}
 Offset:=(Length - 1) - (Start div 32); //To Do //Should the divide be a shift ? //Yes

 {Get Shift}
 Shift:=(Start and 31);

 Result:=PLongWord(PtrUInt(Buffer) + PtrUInt(Offset shl 2))^ shr Shift;
 if (Size + Shift) > 32 then
  begin
   Result:=Result or (PLongWord(PtrUInt(Buffer) + PtrUInt((Offset - 1) shl 2))^ shl ((32 - Shift) mod 32));
  end;

 Result:=Result and Mask;
end;

{==============================================================================}

function MMCIsMultiCommand(Command:Word):Boolean; inline;
begin
 {}
 Result:=(Command = MMC_CMD_READ_MULTIPLE_BLOCK) or (Command = MMC_CMD_WRITE_MULTIPLE_BLOCK);
end;

{==============================================================================}

function MMCIsNonRemovable(MMC:PMMCDevice):Boolean; inline;
begin
 {}
 Result:=False;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=(MMC.Device.DeviceFlags and MMC_FLAG_NON_REMOVABLE) <> 0;
end;

{==============================================================================}

function MMCHasExtendedCSD(MMC:PMMCDevice):Boolean; inline;
begin
 {}
 Result:=False;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=MMC.CardSpecificData.SpecVersion > MMC_CSD_SPEC_VER_3;
end;

{==============================================================================}

function MMCHasSetBlockCount(MMC:PMMCDevice):Boolean; inline;
begin
 {}
 Result:=False;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=(MMC.Device.DeviceFlags and MMC_FLAG_SET_BLOCK_COUNT) <> 0;
end;

{==============================================================================}

function MMCHasAutoBlockCount(MMC:PMMCDevice):Boolean; inline;
begin
 {}
 Result:=False;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=(MMC.Device.DeviceFlags and MMC_FLAG_AUTO_BLOCK_COUNT) <> 0;
end;

{==============================================================================}

function MMCHasAutoCommandStop(MMC:PMMCDevice):Boolean; inline;
begin
 {}
 Result:=False;

 {Check MMC}
 if MMC = nil then Exit;

 Result:=(MMC.Device.DeviceFlags and MMC_FLAG_AUTO_COMMAND_STOP) <> 0;
end;

{==============================================================================}

function MMCStatusToString(Status:LongWord):String;
{Translates an MMC status code into a string describing it}
begin
 {}
 Result:='unknown error';

 case Status of
  MMC_STATUS_SUCCESS:Result:='MMC_STATUS_SUCCESS'; {success}
  MMC_STATUS_TIMEOUT:Result:='MMC_STATUS_TIMEOUT'; {request timed out}
  MMC_STATUS_NO_MEDIA:Result:='MMC_STATUS_NO_MEDIA'; {no media present}
  MMC_STATUS_HARDWARE_ERROR:Result:='MMC_STATUS_HARDWARE_ERROR'; {hardware error}
  MMC_STATUS_INVALID_DATA:Result:='MMC_STATUS_INVALID_DATA'; {invalid data}
  MMC_STATUS_INVALID_PARAMETER:Result:='MMC_STATUS_INVALID_PARAMETER'; {invalid parameter}
  MMC_STATUS_INVALID_SEQUENCE:Result:='MMC_STATUS_INVALID_SEQUENCE'; {invalid sequence}
  MMC_STATUS_OUT_OF_MEMORY:Result:='MMC_STATUS_OUT_OF_MEMORY'; {out of memory}
  MMC_STATUS_UNSUPPORTED_REQUEST:Result:='MMC_STATUS_UNSUPPORTED_REQUEST'; {unsupported request}
  MMC_STATUS_NOT_PROCESSED:Result:='MMC_STATUS_NOT_PROCESSED'; {request not processed yet}
  MMC_STATUS_OPERATION_FAILED:Result:='MMC_STATUS_OPERATION_FAILED'; {operation not completed}
  MMC_STATUS_DEVICE_DETACHED:Result:='MMC_STATUS_DEVICE_DETACHED'; {device was detached}
  MMC_STATUS_DEVICE_UNSUPPORTED:Result:='MMC_STATUS_DEVICE_UNSUPPORTED'; {device is unsupported by the driver}
  MMC_STATUS_NOT_BOUND:Result:='MMC_STATUS_NOT_BOUND'; {device is not bound to a driver}
  MMC_STATUS_ALREADY_BOUND:Result:='MMC_STATUS_ALREADY_BOUND'; {device is already bound to a driver}
  MMC_STATUS_NOT_READY:Result:='MMC_STATUS_NOT_READY'; {device is not ready yet, retry again later}
 end;
end;

{==============================================================================}

function MMCVersionToString(Version:LongWord):String;
{Translates an MMC version into a string}
begin
 {}
 Result:='MMC_VERSION_UNKNOWN';

 case Version of
  {SDIO Version}
  SDIO_VERSION_1_00:Result:='SDIO_VERSION_1_00';
  SDIO_VERSION_1_10:Result:='SDIO_VERSION_1_10';
  SDIO_VERSION_1_20:Result:='SDIO_VERSION_1_20';
  SDIO_VERSION_2_00:Result:='SDIO_VERSION_2_00';
  SDIO_VERSION_3_00:Result:='SDIO_VERSION_3_00';
  {SD Version}
  SD_VERSION_1_0:Result:='SD_VERSION_1_0';
  SD_VERSION_1_10:Result:='SD_VERSION_1_10';
  SD_VERSION_2:Result:='SD_VERSION_2';
  SD_VERSION_3:Result:='SD_VERSION_3';
  SD_VERSION_4:Result:='SD_VERSION_4';
  {MMC Version}
  MMC_VERSION_1_2:Result:='MMC_VERSION_1_2';
  MMC_VERSION_1_4:Result:='MMC_VERSION_1_4';
  MMC_VERSION_2_2:Result:='MMC_VERSION_2_2';
  MMC_VERSION_3:Result:='MMC_VERSION_3';
  MMC_VERSION_4:Result:='MMC_VERSION_4';
  MMC_VERSION_4_1:Result:='MMC_VERSION_4_1';
  MMC_VERSION_4_2:Result:='MMC_VERSION_4_2';
  MMC_VERSION_4_3:Result:='MMC_VERSION_4_3';
  MMC_VERSION_4_41:Result:='MMC_VERSION_4_41';
  MMC_VERSION_4_5:Result:='MMC_VERSION_4_5';
  MMC_VERSION_5_0:Result:='MMC_VERSION_5_0';
  MMC_VERSION_5_1:Result:='MMC_VERSION_5_1';
 end;
end;

{==============================================================================}

function MMCTimingToString(Timing:LongWord):String;
{Translates an MMC timing into a string}
begin
 {}
 Result:='MMC_TIMING_UNKNOWN';

 case Timing of
  MMC_TIMING_LEGACY:Result:='MMC_TIMING_LEGACY';
  MMC_TIMING_MMC_HS:Result:='MMC_TIMING_MMC_HS';
  MMC_TIMING_SD_HS:Result:='MMC_TIMING_SD_HS';
  MMC_TIMING_UHS_SDR12:Result:='MMC_TIMING_UHS_SDR12';
  MMC_TIMING_UHS_SDR25:Result:='MMC_TIMING_UHS_SDR25';
  MMC_TIMING_UHS_SDR50:Result:='MMC_TIMING_UHS_SDR50';
  MMC_TIMING_UHS_SDR104:Result:='MMC_TIMING_UHS_SDR104';
  MMC_TIMING_UHS_DDR50:Result:='MMC_TIMING_UHS_DDR50';
  MMC_TIMING_MMC_DDR52:Result:='MMC_TIMING_MMC_DDR52';
  MMC_TIMING_MMC_HS200:Result:='MMC_TIMING_MMC_HS200';
  MMC_TIMING_MMC_HS400:Result:='MMC_TIMING_MMC_HS400';
 end;
end;

{==============================================================================}

function MMCBusWidthToString(BusWidth:LongWord):String;
{Translates an MMC bus width into a string}
begin
 {}
 Result:='MMC_BUS_WIDTH_UNKNOWN';

 case BusWidth of
  MMC_BUS_WIDTH_1:Result:='MMC_BUS_WIDTH_1';
  MMC_BUS_WIDTH_4:Result:='MMC_BUS_WIDTH_4';
  MMC_BUS_WIDTH_8:Result:='MMC_BUS_WIDTH_8';
 end;
end;

{==============================================================================}

function MMCDriverTypeToString(DriverType:LongWord):String;
{Translates an MMC driver type into a string}
begin
 {}
 Result:='MMC_SET_DRIVER_TYPE_UNKNOWN';

 case DriverType of
  MMC_SET_DRIVER_TYPE_B:Result:='MMC_SET_DRIVER_TYPE_B';
  MMC_SET_DRIVER_TYPE_A:Result:='MMC_SET_DRIVER_TYPE_A';
  MMC_SET_DRIVER_TYPE_C:Result:='MMC_SET_DRIVER_TYPE_C';
  MMC_SET_DRIVER_TYPE_D:Result:='MMC_SET_DRIVER_TYPE_D';
 end;
end;

{==============================================================================}

function MMCSignalVoltageToString(SignalVoltage:LongWord):String;
{Translates an MMC signal voltage into a string}
begin
 {}
 Result:='MMC_SIGNAL_VOLTAGE_UNKNOWN';

 case SignalVoltage of
  MMC_SIGNAL_VOLTAGE_330:Result:='MMC_SIGNAL_VOLTAGE_330';
  MMC_SIGNAL_VOLTAGE_180:Result:='MMC_SIGNAL_VOLTAGE_180';
  MMC_SIGNAL_VOLTAGE_120:Result:='MMC_SIGNAL_VOLTAGE_120';
 end;
end;

{==============================================================================}

function MMCDeviceTypeToString(MMCType:LongWord):String;
begin
 {}
 Result:='MMC_TYPE_UNKNOWN';

 if MMCType <= MMC_TYPE_MAX then
  begin
   Result:=MMC_TYPE_NAMES[MMCType];
  end;
end;

{==============================================================================}

function MMCDeviceStateToString(MMCState:LongWord):String;
begin
 {}
 Result:='MMC_STATE_UNKNOWN';

 if MMCState <= MMC_STATE_MAX then
  begin
   Result:=MMC_STATE_NAMES[MMCState];
  end;
end;

{==============================================================================}

function MMCDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Device state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  MMC_STATE_EJECTED:Result:=DEVICE_NOTIFICATION_INSERT;
  MMC_STATE_INSERTED:Result:=DEVICE_NOTIFICATION_EJECT;
 end;
end;

{==============================================================================}

procedure MMCLog(Level:LongWord;MMC:PMMCDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < MMC_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = MMC_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = MMC_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = MMC_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'MMC: ';

 {Check MMC}
 if MMC <> nil then
  begin
   WorkBuffer:=WorkBuffer + MMC_NAME_PREFIX + IntToStr(MMC.MMCId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_MMC,LogLevelToLoggingSeverity(Level),'MMC',WorkBuffer + AText);
end;

{==============================================================================}

procedure MMCLogInfo(MMC:PMMCDevice;const AText:String); inline;
begin
 {}
 MMCLog(MMC_LOG_LEVEL_INFO,MMC,AText);
end;

{==============================================================================}

procedure MMCLogWarn(MMC:PMMCDevice;const AText:String); inline;
begin
 {}
 MMCLog(MMC_LOG_LEVEL_WARN,MMC,AText);
end;

{==============================================================================}

procedure MMCLogError(MMC:PMMCDevice;const AText:String); inline;
begin
 {}
 MMCLog(MMC_LOG_LEVEL_ERROR,MMC,AText);
end;

{==============================================================================}

procedure MMCLogDebug(MMC:PMMCDevice;const AText:String); inline;
begin
 {}
 MMCLog(MMC_LOG_LEVEL_DEBUG,MMC,AText);
end;

{==============================================================================}
{==============================================================================}
{SD Helper Functions}
function SDGetMaxClock(MMC:PMMCDevice):LongWord;
{Determine the Maximum Clock (DTR) for the current card}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 {Check Highspeed}
 if (MMC.SDSwitchData.Group1Support and SD_SWITCH_GROUP1_HS) <> 0 then
  begin
   Result:=SD_BUS_SPEED_HS;
  end
 else
  begin
   Result:=MMC.CardSpecificData.DataTransferRate;
  end;
end;

{==============================================================================}

function SDGetCIDValue(MMC:PMMCDevice;Value:LongWord):LongWord;
{Extract a CID field value from the 128 bit Card Identification register}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 {SD CID doesn't have a version field}
 case Value of
  MMC_CID_MID:Result:=MMCExtractBits(@MMC.CardIdentification,120,8);
  MMC_CID_OID:Result:=MMCExtractBits(@MMC.CardIdentification,104,16);
  MMC_CID_PNM0:Result:=MMCExtractBits(@MMC.CardIdentification,96,8);
  MMC_CID_PNM1:Result:=MMCExtractBits(@MMC.CardIdentification,88,8);
  MMC_CID_PNM2:Result:=MMCExtractBits(@MMC.CardIdentification,80,8);
  MMC_CID_PNM3:Result:=MMCExtractBits(@MMC.CardIdentification,72,8);
  MMC_CID_PNM4:Result:=MMCExtractBits(@MMC.CardIdentification,64,8);
  MMC_CID_PRV:Result:=MMCExtractBits(@MMC.CardIdentification,56,8);
  MMC_CID_HRV:Result:=MMCExtractBits(@MMC.CardIdentification,60,4);
  MMC_CID_FRV:Result:=MMCExtractBits(@MMC.CardIdentification,56,4);
  MMC_CID_PSN:Result:=MMCExtractBits(@MMC.CardIdentification,24,32);
  MMC_CID_MDT_YEAR:Result:=MMCExtractBits(@MMC.CardIdentification,12,8) + 2000; {SD cards year offset}
  MMC_CID_MDT_MONTH:Result:=MMCExtractBits(@MMC.CardIdentification,8,4);
  MMC_CID_CRC:Result:=MMCExtractBits(@MMC.CardIdentification,1,7);
 end;
end;

{==============================================================================}

function SDGetCSDValue(MMC:PMMCDevice;Version,Value:LongWord):LongWord;
{Extract a CSD field value from the 128 bit Card Specific register}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 if Value = MMC_CSD_STRUCTURE then
  begin
   {Get CSD Structure version}
   Result:=MMCExtractBits(@MMC.CardSpecific,126,2);
  end
 else
  begin
   {Check Version}
   case Version of
    SD_CSD_STRUCT_VER_1_0:begin
      {Standard Capacity}
      case Value of
       MMC_CSD_STRUCTURE:Result:=MMCExtractBits(@MMC.CardSpecific,126,2);
       MMC_CSD_TAAC_VALUE:Result:=MMCExtractBits(@MMC.CardSpecific,115,4);
       MMC_CSD_TAAC_UNIT:Result:=MMCExtractBits(@MMC.CardSpecific,112,3);
       MMC_CSD_NSAC:Result:=MMCExtractBits(@MMC.CardSpecific,104,8);
       MMC_CSD_TRAN_SPEED_VALUE:Result:=MMCExtractBits(@MMC.CardSpecific,99,4);
       MMC_CSD_TRAN_SPEED_UNIT:Result:=MMCExtractBits(@MMC.CardSpecific,96,3);
       MMC_CSD_CCC:Result:=MMCExtractBits(@MMC.CardSpecific,84,12);
       MMC_CSD_READ_BL_LEN:Result:=MMCExtractBits(@MMC.CardSpecific,80,4);
       MMC_CSD_READ_BL_PARTIAL:Result:=MMCExtractBits(@MMC.CardSpecific,79,1);
       MMC_CSD_WRITE_BLK_MISALIGN:Result:=MMCExtractBits(@MMC.CardSpecific,78,1);
       MMC_CSD_READ_BLK_MISALIGN:Result:=MMCExtractBits(@MMC.CardSpecific,77,1);
       MMC_CSD_DSR_IMP:Result:=MMCExtractBits(@MMC.CardSpecific,76,1);
       MMC_CSD_C_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,62,12);
       MMC_CSD_VDD_R_CURR_MIN:Result:=MMCExtractBits(@MMC.CardSpecific,59,3);
       MMC_CSD_VDD_R_CURR_MAX:Result:=MMCExtractBits(@MMC.CardSpecific,56,3);
       MMC_CSD_VDD_W_CURR_MIN:Result:=MMCExtractBits(@MMC.CardSpecific,53,3);
       MMC_CSD_VDD_W_CURR_MAX:Result:=MMCExtractBits(@MMC.CardSpecific,50,3);
       MMC_CSD_C_SIZE_MULT:Result:=MMCExtractBits(@MMC.CardSpecific,47,3);
       MMC_CSD_ERASE_BLK_EN:Result:=MMCExtractBits(@MMC.CardSpecific,46,1);
       MMC_CSD_SECTOR_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,39,7);
       MMC_CSD_WP_GRP_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,32,7);
       MMC_CSD_WP_GRP_ENABLE:Result:=MMCExtractBits(@MMC.CardSpecific,31,1);
       MMC_CSD_R2W_FACTOR:Result:=MMCExtractBits(@MMC.CardSpecific,26,3);
       MMC_CSD_WRITE_BL_LEN:Result:=MMCExtractBits(@MMC.CardSpecific,22,4);
       MMC_CSD_WRITE_BL_PARTIAL:Result:=MMCExtractBits(@MMC.CardSpecific,21,1);
       MMC_CSD_FILE_FORMAT_GRP:Result:=MMCExtractBits(@MMC.CardSpecific,15,1);
       MMC_CSD_COPY:Result:=MMCExtractBits(@MMC.CardSpecific,14,1);
       MMC_CSD_PERM_WRITE_PROTECT:Result:=MMCExtractBits(@MMC.CardSpecific,13,1);
       MMC_CSD_TMP_WRITE_PROTECT:Result:=MMCExtractBits(@MMC.CardSpecific,12,1);
       MMC_CSD_FILE_FORMAT:Result:=MMCExtractBits(@MMC.CardSpecific,10,2);
       MMC_CSD_CRC:Result:=MMCExtractBits(@MMC.CardSpecific,1,7);
      end;
     end;
    SD_CSD_STRUCT_VER_2_0:begin
      {High Capacity and Extended Capacity}
      case Value of
       MMC_CSD_STRUCTURE:Result:=MMCExtractBits(@MMC.CardSpecific,126,2);
       MMC_CSD_TAAC_VALUE:Result:=MMCExtractBits(@MMC.CardSpecific,115,4);
       MMC_CSD_TAAC_UNIT:Result:=MMCExtractBits(@MMC.CardSpecific,112,3);
       MMC_CSD_NSAC:Result:=MMCExtractBits(@MMC.CardSpecific,104,8);
       MMC_CSD_TRAN_SPEED_VALUE:Result:=MMCExtractBits(@MMC.CardSpecific,99,4);
       MMC_CSD_TRAN_SPEED_UNIT:Result:=MMCExtractBits(@MMC.CardSpecific,96,3);
       MMC_CSD_CCC:Result:=MMCExtractBits(@MMC.CardSpecific,84,12);
       MMC_CSD_READ_BL_LEN:Result:=MMCExtractBits(@MMC.CardSpecific,80,4);
       MMC_CSD_READ_BL_PARTIAL:Result:=MMCExtractBits(@MMC.CardSpecific,79,1);
       MMC_CSD_WRITE_BLK_MISALIGN:Result:=MMCExtractBits(@MMC.CardSpecific,78,1);
       MMC_CSD_READ_BLK_MISALIGN:Result:=MMCExtractBits(@MMC.CardSpecific,77,1);
       MMC_CSD_DSR_IMP:Result:=MMCExtractBits(@MMC.CardSpecific,76,1);
       MMC_CSD_C_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,48,22);
       MMC_CSD_ERASE_BLK_EN:Result:=MMCExtractBits(@MMC.CardSpecific,46,1);
       MMC_CSD_SECTOR_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,39,7);
       MMC_CSD_WP_GRP_SIZE:Result:=MMCExtractBits(@MMC.CardSpecific,32,7);
       MMC_CSD_WP_GRP_ENABLE:Result:=MMCExtractBits(@MMC.CardSpecific,31,1);
       MMC_CSD_R2W_FACTOR:Result:=MMCExtractBits(@MMC.CardSpecific,26,3);
       MMC_CSD_WRITE_BL_LEN:Result:=MMCExtractBits(@MMC.CardSpecific,22,4);
       MMC_CSD_WRITE_BL_PARTIAL:Result:=MMCExtractBits(@MMC.CardSpecific,21,1);
       MMC_CSD_FILE_FORMAT_GRP:Result:=MMCExtractBits(@MMC.CardSpecific,15,1);
       MMC_CSD_COPY:Result:=MMCExtractBits(@MMC.CardSpecific,14,1);
       MMC_CSD_PERM_WRITE_PROTECT:Result:=MMCExtractBits(@MMC.CardSpecific,13,1);
       MMC_CSD_TMP_WRITE_PROTECT:Result:=MMCExtractBits(@MMC.CardSpecific,12,1);
       MMC_CSD_FILE_FORMAT:Result:=MMCExtractBits(@MMC.CardSpecific,10,2);
       MMC_CSD_CRC:Result:=MMCExtractBits(@MMC.CardSpecific,1,7);
      end;
     end;
   end;
  end;
end;

 {==============================================================================}

function SDGetSCRValue(MMC:PMMCDevice;Value:LongWord):LongWord;
{Extract an SCR field value from the 64 bit SD Configuration register}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 if Value = SD_SCR_STRUCTURE then
  begin
   {Get SCR Structure version}
   Result:=MMCExtractBits(@MMC.SDConfiguration,60,4);
  end
 else
  begin
   case Value of
    SD_SCR_STRUCTURE:Result:=MMCExtractBits(@MMC.SDConfiguration,60,4);
    SD_SCR_SD_SPEC:Result:=MMCExtractBits(@MMC.SDConfiguration,56,4);
    SD_SCR_DATA_STAT_AFTER_ERASE:Result:=MMCExtractBits(@MMC.SDConfiguration,55,1);
    SD_SCR_SD_SECURITY:Result:=MMCExtractBits(@MMC.SDConfiguration,52,3);
    SD_SCR_SD_BUS_WIDTHS:Result:=MMCExtractBits(@MMC.SDConfiguration,48,4);
    SD_SCR_SD_SPEC3:Result:=MMCExtractBits(@MMC.SDConfiguration,47,1);
    SD_SCR_EX_SECURITY:Result:=MMCExtractBits(@MMC.SDConfiguration,43,4);
    SD_SCR_SD_SPEC4:Result:=MMCExtractBits(@MMC.SDConfiguration,42,1);
    SD_SCR_CMD_SUPPORT:Result:=MMCExtractBits(@MMC.SDConfiguration,32,4);
   end;
  end;
end;

{==============================================================================}

function SDGetSSRValue(MMC:PMMCDevice;Value:LongWord):LongWord;
{Extract an SCR field value from the 512 bit SD Status register}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 {SD SSR doesn't have a version field}
 case Value of
  SD_SSR_DAT_BUS_WIDTH:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,510,2);
  SD_SSR_SECURED_MODE:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,509,1);
  SD_SSR_SD_CARD_TYPE:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,480,16);
  SD_SSR_SIZE_OF_PROTECTED_AREA:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,448,32);
  SD_SSR_SPEED_CLASS:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,440,8);
  SD_SSR_PERFORMANCE_MOVE:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,432,8);
  SD_SSR_AU_SIZE:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,428,4);
  SD_SSR_ERASE_SIZE:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,408,16);
  SD_SSR_ERASE_TIMEOUT:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,402,6);
  SD_SSR_ERASE_OFFSET:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,400,2);
  SD_SSR_UHS_SPEED_GRADE:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,396,4);
  SD_SSR_UHS_AU_SIZE:Result:=MMCExtractBitsEx(@MMC.SDStatus,16,392,4);
 end;
end;

{==============================================================================}

function SDGetSwitchValue(MMC:PMMCDevice;Value:LongWord):LongWord;
{Extract a Switch field value from the 512 bit SD Switch status}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 if Value = SD_SWITCH_STRUCT_VERSION then
  begin
   {Get Switch Structure version}
   Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,368,8);
  end
 else
  begin
   case Value of
    SD_SWITCH_MAXIMUM_CURRENT:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,496,16);
    SD_SWITCH_GROUP6_SUPPORT:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,480,16);
    SD_SWITCH_GROUP5_SUPPORT:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,464,16);
    SD_SWITCH_GROUP4_SUPPORT:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,448,16);
    SD_SWITCH_GROUP3_SUPPORT:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,432,16);
    SD_SWITCH_GROUP2_SUPPORT:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,416,16);
    SD_SWITCH_GROUP1_SUPPORT:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,400,16);
    SD_SWITCH_GROUP6_SELECTION:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,396,4);
    SD_SWITCH_GROUP5_SELECTION:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,392,4);
    SD_SWITCH_GROUP4_SELECTION:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,388,4);
    SD_SWITCH_GROUP3_SELECTION:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,384,4);
    SD_SWITCH_GROUP2_SELECTION:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,380,4);
    SD_SWITCH_GROUP1_SELECTION:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,376,4);
    SD_SWITCH_STRUCT_VERSION:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,368,8);
    SD_SWITCH_GROUP6_BUSY_STATUS:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,352,16);
    SD_SWITCH_GROUP5_BUSY_STATUS:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,336,16);
    SD_SWITCH_GROUP4_BUSY_STATUS:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,320,16);
    SD_SWITCH_GROUP3_BUSY_STATUS:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,304,16);
    SD_SWITCH_GROUP2_BUSY_STATUS:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,288,16);
    SD_SWITCH_GROUP1_BUSY_STATUS:Result:=MMCExtractBitsEx(@MMC.SDSwitch,16,272,16);
   end;
  end;
end;

{==============================================================================}

function SDVersionToString(Version:LongWord):String;
{Translates an SD version into a string}
begin
 {}
 Result:='SD_VERSION_UNKNOWN';

 case Version of
  SD_VERSION_1_0:Result:='SD_VERSION_1_0';
  SD_VERSION_1_10:Result:='SD_VERSION_1_10';
  SD_VERSION_2:Result:='SD_VERSION_2';
  SD_VERSION_3:Result:='SD_VERSION_3';
  SD_VERSION_4:Result:='SD_VERSION_4';
 end;
end;

{==============================================================================}

function SDBusWidthToString(BusWidth:LongWord):String;
{Translates an SD bus width into a string}
begin
 {}
 Result:='SD_BUS_WIDTH_UNKNOWN';

 case BusWidth of
  SD_BUS_WIDTH_1:Result:='SD_BUS_WIDTH_1';
  SD_BUS_WIDTH_4:Result:='SD_BUS_WIDTH_4';
 end;
end;

{==============================================================================}
{==============================================================================}
{SDIO Helper Functions}
function SDIODriverGetCount:LongWord;
{Get the current SDIO driver count}
begin
 {}
 Result:=SDIODriverTableCount;
end;

{==============================================================================}

function SDIODriverCheck(Driver:PSDIODriver):PSDIODriver;
{Check if the supplied SDIO Driver is in the driver table}
var
 Current:PSDIODriver;
begin
 {}
 Result:=nil;

 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDIODriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Current:=SDIODriverTable;
    while Current <> nil do
     begin
      {Check Driver}
      if Current = Driver then
       begin
        Result:=Driver;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDIODriverTableLock);
   end;
  end;
end;

{==============================================================================}

function SDIODeviceGetMaxClock(MMC:PMMCDevice):LongWord;
{Determine the Maximum Clock (DTR) for the current SDIO device}
var
 MaxClock:LongWord;
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 if (MMC.Timing = MMC_TIMING_SD_HS) or (MMC.Timing = MMC_TIMING_MMC_HS) then
  begin
   MaxClock:=SD_BUS_SPEED_HS;
  end
 else
  begin
   if MMC.CIS <> nil then MaxClock:=MMC.CIS.MaxClock else MaxClock:=SD_BUS_SPEED_DEFAULT;
  end;

 if MMC.Device.DeviceType = MMC_TYPE_SD_COMBO then
  begin
   MaxClock:=Min(MaxClock,SDGetMaxClock(MMC));
  end;

 Result:=MaxClock;

 //See: mmc_sdio_get_max_clock in \drivers\mmc\core\sdio.c
end;

{==============================================================================}

function SDIOFunctionGetMMC(Func:PSDIOFunction):PMMCDevice;
begin
 {}
 Result:=nil;

 {Check Func}
 if Func = nil then Exit;

 Result:=Func.MMC;
end;

{==============================================================================}

function SDIOFunctionGetSDHCI(Func:PSDIOFunction):PSDHCIHost;
begin
 {}
 Result:=nil;

 {Check Func}
 if Func = nil then Exit;

 Result:=MMCGetSDHCI(Func.MMC);
end;

{==============================================================================}

function SDIOVersionToString(Version:LongWord):String;
{Translates an SDIO version into a string}
begin
 {}
 Result:='SDIO_VERSION_UNKNOWN';

 case Version of
  SDIO_VERSION_1_00:Result:='SDIO_VERSION_1_00';
  SDIO_VERSION_1_10:Result:='SDIO_VERSION_1_10';
  SDIO_VERSION_1_20:Result:='SDIO_VERSION_1_20';
  SDIO_VERSION_2_00:Result:='SDIO_VERSION_2_00';
  SDIO_VERSION_3_00:Result:='SDIO_VERSION_3_00';
 end;
end;

{==============================================================================}

function SDIOFunctionStateToString(SDIOState:LongWord):String;
begin
 {}
 Result:='SDIO_STATE_UNKNOWN';

 if SDIOState <= SDIO_STATE_MAX then
  begin
   Result:=SDIO_STATE_NAMES[SDIOState];
  end;
end;

{==============================================================================}

function SDIOFunctionStatusToString(SDIOStatus:LongWord):String;
begin
 {}
 Result:='SDIO_STATUS_UNKNOWN';

 if SDIOStatus <= SDIO_STATUS_MAX then
  begin
   Result:=SDIO_STATUS_NAMES[SDIOStatus];
  end;
end;

{==============================================================================}

function SDIOFunctionStateToNotification(State:LongWord):LongWord;
{Convert a Device state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  SDIO_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  SDIO_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  SDIO_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  SDIO_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}

function SDIOFunctionStatusToNotification(Status:LongWord):LongWord;
{Convert a Device status value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check Status}
 case Status of
  SDIO_STATUS_UNBOUND:Result:=DEVICE_NOTIFICATION_UNBIND;
  SDIO_STATUS_BOUND:Result:=DEVICE_NOTIFICATION_BIND;
 end;
end;

{==============================================================================}
{==============================================================================}
{SDHCI Helper Functions}
function SDHCIGetCount:LongWord;
{Get the current SDHCI count}
begin
 {}
 Result:=SDHCIHostTableCount;
end;

{==============================================================================}

function SDHCIHostCheck(SDHCI:PSDHCIHost):PSDHCIHost;
{Check if the supplied SDHCI is in the SDHCI table}
var
 Current:PSDHCIHost;
begin
 {}
 Result:=nil;

 {Check SDHCI}
 if SDHCI = nil then Exit;
 if SDHCI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SDHCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SDHCI}
    Current:=SDHCIHostTable;
    while Current <> nil do
     begin
      {Check SDHCI}
      if Current = SDHCI then
       begin
        Result:=SDHCI;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SDHCIHostTableLock);
   end;
  end;
end;

{==============================================================================}

function SDHCIIsSPI(SDHCI:PSDHCIHost):Boolean;
begin
 {}
 Result:=False;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 Result:=(SDHCI.Capabilities and MMC_CAP_SPI) <> 0;
end;

{==============================================================================}

function SDHCIHasDMA(SDHCI:PSDHCIHost):Boolean;
begin
 {}
 Result:=False;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Check SDMA}
 if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_SDMA) <> 0 then
  begin
   Result:=True;
  end
 {Check ADMA}
 else if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_ADMA) <> 0 then
  begin
   Result:=True;
  end
 {Check External DMA}
 else if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_EXTERNAL_DMA) <> 0 then
  begin
   Result:=DMAAvailable;
  end;
end;

{==============================================================================}

function SDHCIHasCMD23(SDHCI:PSDHCIHost):Boolean;
begin
 {}
 Result:=False;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 Result:=(SDHCI.Capabilities and MMC_CAP_CMD23) <> 0;
end;

{==============================================================================}

function SDHCIAutoCMD12(SDHCI:PSDHCIHost):Boolean;
begin
 {}
 Result:=False;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 Result:=(SDHCI.Device.DeviceFlags and SDHCI_FLAG_AUTO_CMD12) <> 0;
end;

{==============================================================================}

function SDHCIAutoCMD23(SDHCI:PSDHCIHost):Boolean;
begin
 {}
 Result:=False;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 Result:=(SDHCI.Device.DeviceFlags and SDHCI_FLAG_AUTO_CMD23) <> 0;
end;

{==============================================================================}

function SDHCIGetVersion(SDHCI:PSDHCIHost):Word;
begin
 {}
 Result:=SDHCI_SPEC_100; {Default to Version 1.0}

 {Check SDHCI}
 if SDHCI = nil then Exit;

 Result:=(SDHCI.Version and SDHCI_SPEC_VER_MASK);
end;

{==============================================================================}

function SDHCIGetCommand(Command:Word):Word;
begin
 {}
 Result:=((Command shr 8) and $3F);
end;

{==============================================================================}

function SDHCIMakeCommand(Command,Flags:Word):Word;
begin
 {}
 Result:=((Command and $FF) shl 8) or (Flags and $FF);
end;

{==============================================================================}

function SDHCIMakeBlockSize(DMA,BlockSize:Word):Word;
begin
 {}
 Result:=((DMA and $07) shl 12) or (BlockSize and $0FFF);
end;

{==============================================================================}

function SDHCIVersionToString(Version:LongWord):String;
{Translate an SDHCI version into a string}
begin
 {}
 Result:='SDHCI_SPEC_UNKNOWN';

 case Version of
  SDHCI_SPEC_100:Result:='SDHCI_SPEC_100';
  SDHCI_SPEC_200:Result:='SDHCI_SPEC_200';
  SDHCI_SPEC_300:Result:='SDHCI_SPEC_300';
  SDHCI_SPEC_400:Result:='SDHCI_SPEC_400';
  SDHCI_SPEC_410:Result:='SDHCI_SPEC_410';
  SDHCI_SPEC_420:Result:='SDHCI_SPEC_420';
 end;
end;

{==============================================================================}

function SDHCIPowerToString(Power:LongWord):String;
{Translate an SDHCI power value into a string}
begin
 {}
 Result:='SDHCI_POWER_UNKNOWN';

 case Power of
  SDHCI_POWER_180:Result:='SDHCI_POWER_180';
  SDHCI_POWER_300:Result:='SDHCI_POWER_300';
  SDHCI_POWER_330:Result:='SDHCI_POWER_330';
 end;
end;

{==============================================================================}

function SDHCIDeviceTypeToString(SDHCIType:LongWord):String; inline;
begin
 {}
 Result:=SDHCIHostTypeToString(SDHCIType);
end;

{==============================================================================}

function SDHCIHostTypeToString(SDHCIType:LongWord):String;
begin
 {}
 Result:='SDHCI_TYPE_UNKNOWN';

 if SDHCIType <= SDHCI_TYPE_MAX then
  begin
   Result:=SDHCI_TYPE_NAMES[SDHCIType];
  end;
end;

{==============================================================================}

function SDHCIDeviceStateToString(SDHCIState:LongWord):String; inline;
begin
 {}
 Result:=SDHCIHostStateToString(SDHCIState);
end;

{==============================================================================}

function SDHCIHostStateToString(SDHCIState:LongWord):String;
begin
 {}
 Result:='SDHCI_STATE_UNKNOWN';

 if SDHCIState <= SDHCI_STATE_MAX then
  begin
   Result:=SDHCI_STATE_NAMES[SDHCIState];
  end;
end;

{==============================================================================}

function SDHCIHostStateToNotification(State:LongWord):LongWord;
{Convert a Host state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  SDHCI_STATE_DISABLED:Result:=DEVICE_NOTIFICATION_DISABLE;
  SDHCI_STATE_ENABLED:Result:=DEVICE_NOTIFICATION_ENABLE;
 end;
end;

{==============================================================================}
{==============================================================================}
{MMC Storage Functions}
function MMCStorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
var
 Status:LongWord;
 BlockCount:Word;
 ReadOffset:PtrUInt;
 ReadRemain:Int64;
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMCStorageDeviceRead (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Count}
 Result:=ERROR_SUCCESS;
 if Count = 0 then Exit;

 {Check Buffer}
 Result:=ERROR_INVALID_PARAMETER;
 if Buffer = nil then Exit;

 {Get MMC}
 MMC:=PMMCDevice(Storage.Device.DeviceData);
 if MMC = nil then Exit;

 {Check Storage}
 if Storage.BlockSize = 0 then Exit;
 if Storage.BlockCount = 0 then Exit;

 {Check State}
 if Storage.StorageState <> STORAGE_STATE_INSERTED then Exit;

 {Acquire the Lock}
 if MutexLock(Storage.Lock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if MutexLock(MMC.Lock) = ERROR_SUCCESS then
     begin
      try
       {Set Block Length}
       if (MMC.Device.DeviceFlags and MMC_FLAG_BLOCK_ADDRESSED) = 0 then
        begin
         Status:=MMCDeviceSetBlockLength(MMC,Storage.BlockSize);
         if Status <> MMC_STATUS_SUCCESS then
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Set Block Length failure (Status=' + MMCStatusToString(Status) + ')');

           {Update Statistics}
           Inc(Storage.ReadErrors);

           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;

       {Start Read}
       ReadOffset:=0;
       ReadRemain:=Count;
       while ReadRemain > 0 do
        begin
         {Get Count}
         BlockCount:=ReadRemain;
         if ReadRemain > MMC_MAX_BLOCK_COUNT then BlockCount:=MMC_MAX_BLOCK_COUNT; //To do //This should use SDHCI.MaximumBlockCount ?

         {Set Block Count (If supported and not Auto Block Count)}
         if (BlockCount > 1) and MMCHasSetBlockCount(MMC) and not(MMCHasAutoBlockCount(MMC)) then
          begin
           Status:=MMCDeviceSetBlockCount(MMC,BlockCount,False);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Set Block Count failure (Status=' + MMCStatusToString(Status) + ')');

             {Update Statistics}
             Inc(Storage.ReadErrors);

             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end;

         {Read Blocks}
         Status:=MMCDeviceReadBlocks(MMC,(Start + ReadOffset),BlockCount,Pointer(PtrUInt(Buffer) + (ReadOffset shl Storage.BlockShift)));
         if Status <> MMC_STATUS_SUCCESS then
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Read Blocks failure (Status=' + MMCStatusToString(Status) + ')');

           {Stop Transmission}
           MMCDeviceStopTransmission(MMC);

           {Update Statistics}
           Inc(Storage.ReadErrors);

           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;

         {Stop Transmission (If no Set Block Count and not Auto Command Stop)}
         if (BlockCount > 1) and not(MMCHasSetBlockCount(MMC)) and not(MMCHasAutoCommandStop(MMC)) then
          begin
           Status:=MMCDeviceStopTransmission(MMC);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Stop Transmission failure (Status=' + MMCStatusToString(Status) + ')');

             {Update Statistics}
             Inc(Storage.ReadErrors);

             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end;

         Inc(ReadOffset,BlockCount);
         Dec(ReadRemain,BlockCount);
        end;

       {Update Statistics}
       Inc(Storage.ReadCount,Count shl Storage.BlockShift);

       Result:=ERROR_SUCCESS;
      finally
       {Release the Lock}
       MutexUnlock(MMC.Lock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Storage.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MMCStorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
var
 Status:LongWord;
 BlockCount:Word;
 WriteOffset:PtrUInt;
 WriteRemain:Int64;
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMCStorageDeviceWrite (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Count}
 Result:=ERROR_SUCCESS;
 if Count = 0 then Exit;

 {Check Buffer}
 Result:=ERROR_INVALID_PARAMETER;
 if Buffer = nil then Exit;

 {Get MMC}
 MMC:=PMMCDevice(Storage.Device.DeviceData);
 if MMC = nil then Exit;

 {Check Storage}
 if Storage.BlockSize = 0 then Exit;
 if Storage.BlockCount = 0 then Exit;

 {Check State}
 if Storage.StorageState <> STORAGE_STATE_INSERTED then Exit;

 {Acquire the Lock}
 if MutexLock(Storage.Lock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if MutexLock(MMC.Lock) = ERROR_SUCCESS then
     begin
      try
       {Set Block Length}
       if (MMC.Device.DeviceFlags and MMC_FLAG_BLOCK_ADDRESSED) = 0 then
        begin
         Status:=MMCDeviceSetBlockLength(MMC,Storage.BlockSize);
         if Status <> MMC_STATUS_SUCCESS then
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Set Block Length failure (Status=' + MMCStatusToString(Status) + ')');

           {Update Statistics}
           Inc(Storage.WriteErrors);

           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;

       {Start Write}
       WriteOffset:=0;
       WriteRemain:=Count;
       while WriteRemain > 0 do
        begin
         {Get Count}
         BlockCount:=WriteRemain;
         if WriteRemain > MMC_MAX_BLOCK_COUNT then BlockCount:=MMC_MAX_BLOCK_COUNT; //To Do //This should use SDHCI.MaximumBlockCount ?

         {Set Block Count (If supported and not Auto Block Count)}
         if (BlockCount > 1) and MMCHasSetBlockCount(MMC) and not(MMCHasAutoBlockCount(MMC)) then
          begin
           Status:=MMCDeviceSetBlockCount(MMC,BlockCount,False);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Set Block Count failure (Status=' + MMCStatusToString(Status) + ')');

             {Update Statistics}
             Inc(Storage.WriteErrors);

             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end;

         {Write Blocks}
         Status:=MMCDeviceWriteBlocks(MMC,(Start + WriteOffset),BlockCount,Pointer(PtrUInt(Buffer) + (WriteOffset shl Storage.BlockShift)));
         if Status <> MMC_STATUS_SUCCESS then
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Write Blocks failure (Status=' + MMCStatusToString(Status) + ')');

           {Stop Transmission}
           MMCDeviceStopTransmission(MMC);

           {Update Statistics}
           Inc(Storage.WriteErrors);

           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;

         {Stop Transmission (If no Set Block Count and not Auto Command Stop)}
         if (BlockCount > 1) and not(MMCHasSetBlockCount(MMC)) and not(MMCHasAutoCommandStop(MMC)) then
          begin
           Status:=MMCDeviceStopTransmission(MMC);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Storage Stop Transmission failure (Status=' + MMCStatusToString(Status) + ')');

             {Update Statistics}
             Inc(Storage.WriteErrors);

             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end;

         Inc(WriteOffset,BlockCount);
         Dec(WriteRemain,BlockCount);
        end;

       {Update Statistics}
       Inc(Storage.WriteCount,Count shl Storage.BlockShift);

       Result:=ERROR_SUCCESS;
      finally
       {Release the Lock}
       MutexUnlock(MMC.Lock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Storage.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MMCStorageDeviceErase(Storage:PStorageDevice;const Start,Count:Int64):LongWord;
var
 Status:LongWord;
 BlockCount:Word;
 EraseOffset:PtrUInt;
 EraseRemain:Int64;
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMCStorageDeviceErase (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Count}
 Result:=ERROR_SUCCESS;
 if Count = 0 then Exit;

 {Get MMC}
 Result:=ERROR_INVALID_PARAMETER;
 MMC:=PMMCDevice(Storage.Device.DeviceData);
 if MMC = nil then Exit;

 {Check Storage}
 if Storage.BlockSize = 0 then Exit;
 if Storage.BlockCount = 0 then Exit;

 {Check State}
 if Storage.StorageState <> STORAGE_STATE_INSERTED then Exit;

 {Acquire the Lock}
 if MutexLock(Storage.Lock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if MutexLock(MMC.Lock) = ERROR_SUCCESS then
     begin
      try

       //To Do //See USBStorageDeviceWrite and above

       //{Update Statistics}
       //Inc(Storage.EraseCount,Count);

       //See: mmc_berase in mmc_write.c
      finally
       {Release the Lock}
       MutexUnlock(MMC.Lock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Storage.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MMCStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
var
 MMC:PMMCDevice;
 SDHCI:PSDHCIHost;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMCStorageDeviceControl (Request=' + IntToStr(Request) + ' Argument1=' + IntToStr(Argument1) + ' Argument2=' + IntToStr(Argument2) + ')');
 {$ENDIF}

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get MMC}
 MMC:=PMMCDevice(Storage.Device.DeviceData);
 if MMC = nil then Exit;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Acquire the Lock}
 if MutexLock(Storage.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Request}
    case Request of
     STORAGE_CONTROL_TEST_READY,STORAGE_CONTROL_TEST_MEDIA:begin
       {Check Flags}
       if (Storage.Device.DeviceFlags and (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA)) = 0 then
        begin
         {Ready}
         Status:=MMCDeviceGetCardDetect(MMC);

         {Check Status and Flags}
         if (Status <> MMC_STATUS_SUCCESS) or ((MMC.Device.DeviceFlags and MMC_FLAG_CARD_PRESENT) = 0) then
          begin
           {Not Ready}
           {Reset Host}
           SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);

           {Deinitialize Device}
           Status:=MMCDeviceDeinitialize(MMC);
           if Status = MMC_STATUS_SUCCESS then
            begin
             {Notify Ejected}
             if MMC.MMCState = MMC_STATE_EJECTED then
              begin
               NotifierNotify(@MMC.Device,DEVICE_NOTIFICATION_EJECT);
              end;

             {Set Storage State to Ejected}
             if MMC.MMCState = MMC_STATE_EJECTED then
              begin
               StorageDeviceSetState(Storage,STORAGE_STATE_EJECTED);
              end;

             Result:=ERROR_NOT_READY;
             Exit;
            end;
          end;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end
       else
        begin
         {Not Ready}
         Status:=MMCDeviceGetCardDetect(MMC);

         {Check Status and Flags}
         if (Status = MMC_STATUS_SUCCESS) and ((MMC.Device.DeviceFlags and MMC_FLAG_CARD_PRESENT) <> 0) then
          begin
           {Ready}
           {Reset Host}
           SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);

           {Initialize Device}
           Status:=MMCDeviceInitialize(MMC);
           if Status = MMC_STATUS_SUCCESS then
            begin
             {Notify Inserted}
             if MMC.MMCState = MMC_STATE_INSERTED then
              begin
               NotifierNotify(@MMC.Device,DEVICE_NOTIFICATION_INSERT);
              end;

             {Set Storage State to Inserted}
             if MMC.MMCState = MMC_STATE_INSERTED then
              begin
               StorageDeviceSetState(Storage,STORAGE_STATE_INSERTED);
              end;

             Result:=ERROR_SUCCESS;
             Exit;
            end;
          end;

         {Return Result}
         Result:=ERROR_NOT_READY;
        end;
      end;
     STORAGE_CONTROL_RESET:begin
       {Not Supported}
       Result:=ERROR_NOT_SUPPORTED;
      end;
     STORAGE_CONTROL_LOCK:begin
       {Not Supported}
       Result:=ERROR_NOT_SUPPORTED;
      end;
     STORAGE_CONTROL_UNLOCK:begin
       {Not Supported}
       Result:=ERROR_NOT_SUPPORTED;
      end;
     STORAGE_CONTROL_EJECT:begin
       {Not Supported}
       Result:=ERROR_NOT_SUPPORTED;
      end;
     STORAGE_CONTROL_TEST_LOCKED:begin
       {Not Supported}
       Result:=ERROR_NOT_SUPPORTED;
      end;
     STORAGE_CONTROL_TEST_CHANGED:begin
       {Not Supported}
       Result:=ERROR_NOT_SUPPORTED;
      end;
     STORAGE_CONTROL_GET_VENDORID:begin
       {Get Vendor ID}
       Argument2:=0;
       //To Do

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     STORAGE_CONTROL_GET_PRODUCTID:begin
       {Get Product ID}
       Argument2:=0;
       //To Do

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     STORAGE_CONTROL_GET_SERIAL:begin
       {Get Serial No}
       Argument2:=0;
       //To Do

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     STORAGE_CONTROL_GET_REVISION:begin
       {Get Revision No}
       Argument2:=0;
       //To Do

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     STORAGE_CONTROL_GET_PRODUCT:begin
       {Get Product Name}
       Argument2:=0;
       //To Do

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     STORAGE_CONTROL_GET_MANUFACTURER:begin
       {Get Manufacturer Name}
       //To Do

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end;
   finally
    {Release the Lock}
    MutexUnlock(Storage.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{SDIO Macro Replacement Functions}
function SDIO_FBR_BASE(Number:LongWord):LongWord; inline;
begin
 {}
 Result:=(Number * $100);
end;

{==============================================================================}
{==============================================================================}

initialization
 MMCInit;
 if MMC_AUTOSTART then
  begin
   if not MMC_ASYNCSTART then
    begin
     {Start MMC}
     MMCStart;
    end
   else
    begin
     {Schedule Worker}
     WorkerSchedule(MMC_STARTDELAY,TWorkerTask(MMCAsyncStart),nil,nil);
    end;
  end;

{==============================================================================}

finalization
 MMCStop;

{==============================================================================}
{==============================================================================}

end.

