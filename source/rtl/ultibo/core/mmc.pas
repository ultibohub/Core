{
Ultibo MMC/SD interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

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
   
    \u-boot-HEAD-5745f8c\drivers\mmc\bcm2835_sdhci.c
    \u-boot-HEAD-5745f8c\drivers\mmc\sdhci.c
    \u-boot-HEAD-5745f8c\drivers\mmc\mmc.c
    \u-boot-HEAD-5745f8c\drivers\mmc\mmc_spi.c
    \u-boot-HEAD-5745f8c\drivers\mmc\mmc_write.c
    \u-boot-HEAD-5745f8c\include\mmc.h
    \u-boot-HEAD-5745f8c\include\sdhci.h
       
   General
   
    \linux-rpi-3.18.y\drivers\mmc\core\core.c
    \linux-rpi-3.18.y\drivers\mmc\core\host.c
    \linux-rpi-3.18.y\drivers\mmc\card\block.c
    
   
   SDHCI
   
    \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
    \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
    
   MMC
   
    \linux-rpi-3.18.y\drivers\mmc\core\mmc.c
    \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
    
   SD
   
    \linux-rpi-3.18.y\drivers\mmc\core\sd.c
    \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
    
   SDIO
   
    \linux-rpi-3.18.y\drivers\mmc\core\sdio.c
    \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
    
   Include

    \linux-rpi-3.18.y\include\linux\mmc
    
   
References
==========

 SD Host Controller Simplified Specification V3.0
 
   https://www.sdcard.org/downloads/pls/simplified_specs/partA2_300.pdf
  
 SDIO Simplified Specification V3.0
 
   https://www.sdcard.org/downloads/pls/simplified_specs/partE1_300.pdf

 SD Physical Layer Simplified Specification V4.10.pdf
 
   https://www.sdcard.org/downloads/pls/simplified_specs/part1_410.pdf
   
 Others
   
   http://elm-chan.org/docs/mmc/mmc_e.html
 

SD/MMC Devices
==============

This unit implements the standards based part of the SD/MMC specification including the standard SDHCI interfaces.

For each platform a SDHCI module needs to be provided that implements the platform specific parts of the SDHCI interface.

This is similar in model to USB and other interfaces in Ultibo, where the generic interface unit requires a platform specific
module to register with it in order to communicate with platform specific devices.

The SD/MMC interfaces are 2 tier (ie Host and Device) whereas the USB interface is 3 tier (Host, Device and Driver).

}
              
{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit MMC;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,DMA,Storage,SysUtils;
            
//To Do //For SPI based MMC/SDHCI see: \u-boot-HEAD-5745f8c\drivers\mmc\mmc_spi.c

              //For RPMB (Replay Protected Memory Block)
              //See: \u-boot-HEAD-5745f8c\drivers\mmc\rpmb.c
              
//To Do //See also: \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 
//To Do //Need to add DataMemoryBarriers in appropriate places here
              //See: mmiowb() in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
              
//To Do //Note: May need to expand this interface to allow SDIO support
              //See SDIO drivers (PSDIODriver) can register with this interface  and be called when an SDIO device (PSDIODevice) is
              //found to see if they support it. (eg BCM4330 WiFi / Bluetooth). Similar in concept to USB.
                       
 //To Do
              //Locks *****
              //SDMA/ADMA
              //MMC Detect/Initialize
              //SDIO Detect/Initialize
              //UHS-I and II Initialize  (Tuning etc)
              //Auto CMD23/Auto CMD12 handling
              //Host/Device Flags (Decode OCR/CID/CSD/SCR/SSR etc)
              //Host/Device Capabilities (specifically Device) (Decode OCR/CID/CSD/SCR/SSR etc)
              //Device Voltages (Decode OCR)
              //Statistics (CommandCount/CommandErrors etc)
              //
              
//To Do //Look for:

//Testing
            
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
 MMC_FLAG_CARD_PRESENT      = $00000001;
 MMC_FLAG_WRITE_PROTECT     = $00000002;
 MMC_FLAG_HIGH_CAPACITY     = $00000004;  {High Capacity (SDHC)}
 MMC_FLAG_EXT_CAPACITY      = $00000008;  {Extended Capacity (SDXC)}
 MMC_FLAG_UHS_I             = $00000010;  {Ultra High Speed (UHS-I)}
 MMC_FLAG_UHS_II            = $00000020;  {Ultra High Speed (UHS-II)}
 MMC_FLAG_BLOCK_ADDRESSED   = $00000040;  {Block Addressed (SDHC/SDXC and others)}
 MMC_FLAG_AUTO_BLOCK_COUNT  = $00000080;  {Auto CMD23 (Set Block Count)}
 MMC_FLAG_AUTO_COMMAND_STOP = $00000100;  {Auto CMD12 (Stop Transmission)}
 MMC_FLAG_DDR_MODE          = $00000200;  //To Do //Should this go in MMC.Capabilities ? //Maybe not
 //To Do //More

 {MMC/SD Status Codes} 
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
 
 {MMC/SD Versions}
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
 MMC_VERSION_UNKNOWN = (MMC_VERSION_MMC);
 
 {MMC/SD Modes} //To Do //These are really the capabilities flags for MMC/SDHCI.Capabilities/PresetCapabilities  MMC_CAP_
 MMC_MODE_HS		= (1 shl 0);
 MMC_MODE_HS_52MHz	= (1 shl 1);
 MMC_MODE_4BIT		= (1 shl 2);
 MMC_MODE_8BIT		= (1 shl 3);
 MMC_MODE_SPI		= (1 shl 4);
 MMC_MODE_HC		= (1 shl 5);
 MMC_MODE_DDR_52MHz	= (1 shl 6);
 //To Do //More
 
 {MMC/SD Capabilities (From: /include/linux/mmc/host.h)}
 //To Do //More
 MMC_CAP_CMD23		= (1 shl 30);	{CMD23 supported}
 
 {MMC/SD Directions} //To Do //??
 MMC_DATA_READ		= 1;
 MMC_DATA_WRITE		= 2;  
 
 {MMC/SD Bus Widths} 
 MMC_BUS_WIDTH_1	= 0;
 MMC_BUS_WIDTH_4    = 2;
 MMC_BUS_WIDTH_8	= 3;
 
 {MMC Bus Speeds (Hz)}
 MMC_BUS_SPEED_DEFAULT   = 0;
 MMC_BUS_SPEED_HS26      = 26000000;
 MMC_BUS_SPEED_HS52      = 52000000;
 MMC_BUS_SPEED_DDR       = 52000000;
 MMC_BUS_SPEED_HS200     = 200000000;
 
 {MMC/SD Timing (From: /include/linux/mmc/host.h)}
 MMC_TIMING_LEGACY	    = 0;
 MMC_TIMING_MMC_HS	    = 1;
 MMC_TIMING_SD_HS	    = 2;
 MMC_TIMING_UHS_SDR12	= 3;
 MMC_TIMING_UHS_SDR25	= 4;
 MMC_TIMING_UHS_SDR50	= 5;
 MMC_TIMING_UHS_SDR104	= 6;
 MMC_TIMING_UHS_DDR50	= 7;
 MMC_TIMING_MMC_DDR52	= 8;
 MMC_TIMING_MMC_HS200	= 9;
 MMC_TIMING_MMC_HS400	= 10; 

 {MMC Commands (From: /include/linux/mmc/mmc.h)}
 {Class 1}
 MMC_CMD_GO_IDLE_STATE		  = 0;
 MMC_CMD_SEND_OP_COND		  = 1;
 MMC_CMD_ALL_SEND_CID		  = 2;
 MMC_CMD_SET_RELATIVE_ADDR	  = 3;
 MMC_CMD_SET_DSR			  = 4;
 MMC_CMD_SLEEP_AWAKE		  = 5;
 MMC_CMD_SWITCH			      = 6;
 MMC_CMD_SELECT_CARD		  = 7;
 MMC_CMD_SEND_EXT_CSD		  = 8;
 MMC_CMD_SEND_CSD		      = 9;
 MMC_CMD_SEND_CID		      = 10;
 MMC_CMD_READ_DAT_UNTIL_STOP  = 11;
 MMC_CMD_STOP_TRANSMISSION	  = 12;
 MMC_CMD_SEND_STATUS		  = 13;
 MMC_CMD_BUS_TEST_R           = 14;
 MMC_CMD_GO_INACTIVE_STATE    = 15;
 MMC_CMD_BUS_TEST_W           = 19;
 MMC_CMD_SPI_READ_OCR		  = 58;
 MMC_CMD_SPI_CRC_ON_OFF		  = 59;
 
 {Class 2}
 MMC_CMD_SET_BLOCKLEN		  = 16;
 MMC_CMD_READ_SINGLE_BLOCK	  = 17;
 MMC_CMD_READ_MULTIPLE_BLOCK  = 18;
 MMC_CMD_SEND_TUNING_BLOCK    = 19;
 MMC_CMD_SEND_TUNING_BLOCK_HS200 = 21;
 
 {Class 3}
 MMC_CMD_WRITE_DAT_UNTIL_STOP = 20;
 
 {Class 4}
 MMC_CMD_SET_BLOCK_COUNT      = 23;
 MMC_CMD_WRITE_SINGLE_BLOCK   =	24;
 MMC_CMD_WRITE_MULTIPLE_BLOCK =	25;
 MMC_CMD_PROGRAM_CID          = 26;
 MMC_CMD_PROGRAM_CSD          = 27;

 {Class 6}
 MMC_CMD_SET_WRITE_PROT       = 28;
 MMC_CMD_CLR_WRITE_PROT       = 29;
 MMC_CMD_SEND_WRITE_PROT      = 30;
 
 {Class 5}
 MMC_CMD_ERASE_GROUP_START	  = 35;
 MMC_CMD_ERASE_GROUP_END	  = 36;
 MMC_CMD_ERASE			      = 38;
 
 {Class 9}
 MMC_CMD_FAST_IO              = 39;
 MMC_CMD_GO_IRQ_STATE         = 40;
 
 {Class 7}
 MMC_CMD_LOCK_UNLOCK          = 42;
 
 {Class 8}
 MMC_CMD_APP_CMD			  = 55;
 MMC_CMD_GEN_CMD              = 56;
 MMC_CMD_RES_MAN			  = 62;
 
 MMC_CMD62_ARG1			= $EFAC62EC;
 MMC_CMD62_ARG2			= $00CBAEA7;

 {MMC Response Types (From: /include/linux/mmc/mmc.h)}
 {Native}
 MMC_RSP_PRESENT = (1 shl 0);
 MMC_RSP_136	 = (1 shl 1); {136 bit response}
 MMC_RSP_CRC	 = (1 shl 2); {Expect valid crc}
 MMC_RSP_BUSY	 = (1 shl 3); {Card may send busy}
 MMC_RSP_OPCODE	 = (1 shl 4); {Response contains opcode}

 {These are the native response types, and correspond to valid bit patterns of the above flags. One additional valid pattern is all zeros, which means we don't expect a respons}
 MMC_RSP_NONE	= (0);
 MMC_RSP_R1	    = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);
 MMC_RSP_R1B    = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE or MMC_RSP_BUSY);
 MMC_RSP_R2	    = (MMC_RSP_PRESENT or MMC_RSP_136 or MMC_RSP_CRC);
 MMC_RSP_R3	    = (MMC_RSP_PRESENT);
 MMC_RSP_R4	    = (MMC_RSP_PRESENT);
 MMC_RSP_R5	    = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);
 MMC_RSP_R6	    = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);
 MMC_RSP_R7	    = (MMC_RSP_PRESENT or MMC_RSP_CRC or MMC_RSP_OPCODE);
 
 {SPI}
 MMC_RSP_SPI_S1	  = (1 shl 7);		{One status byte}
 MMC_RSP_SPI_S2	  = (1 shl 8);		{Second byte}
 MMC_RSP_SPI_B4	  = (1 shl 9);		{Four data bytes}
 MMC_RSP_SPI_BUSY = (1 shl 10);		{Card may send busy}

 {These are the SPI response types for MMC, SD, and SDIO cards. Commands return R1, with maybe more info. Zero is an error type, callers must always provide the appropriate MMC_RSP_SPI_Rx flags}
 MMC_RSP_SPI_R1	  = (MMC_RSP_SPI_S1);
 MMC_RSP_SPI_R1B  = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_BUSY);
 MMC_RSP_SPI_R2	  = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_S2);
 MMC_RSP_SPI_R3	  = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_B4);
 MMC_RSP_SPI_R4	  = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_B4);
 MMC_RSP_SPI_R5	  = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_S2);
 MMC_RSP_SPI_R7	  = (MMC_RSP_SPI_S1 or MMC_RSP_SPI_B4);
 
 {MMC Response Values}
 {R1 - MMC status in R1, for native mode (SPI bits are different)}
 MMC_RSP_R1_OUT_OF_RANGE		= (1 shl 31);
 MMC_RSP_R1_ADDRESS_ERROR	    = (1 shl 30);	
 MMC_RSP_R1_BLOCK_LEN_ERROR	    = (1 shl 29);	
 MMC_RSP_R1_ERASE_SEQ_ERROR     = (1 shl 28);	
 MMC_RSP_R1_ERASE_PARAM		    = (1 shl 27);	
 MMC_RSP_R1_WP_VIOLATION		= (1 shl 26);	
 MMC_RSP_R1_CARD_IS_LOCKED	    = (1 shl 25);	
 MMC_RSP_R1_LOCK_UNLOCK_FAILED	= (1 shl 24);	
 MMC_RSP_R1_COM_CRC_ERROR	    = (1 shl 23);	
 MMC_RSP_R1_ILLEGAL_COMMAND		= (1 shl 22);
 MMC_RSP_R1_CARD_ECC_FAILED	    = (1 shl 21);	
 MMC_RSP_R1_CC_ERROR		    = (1 shl 20);	
 MMC_RSP_R1_ERROR		        = (1 shl 19);	
 MMC_RSP_R1_UNDERRUN		    = (1 shl 18);	
 MMC_RSP_R1_OVERRUN		        = (1 shl 17);	
 MMC_RSP_R1_CID_CSD_OVERWRITE	= (1 shl 16);	
 MMC_RSP_R1_WP_ERASE_SKIP	    = (1 shl 15);	
 MMC_RSP_R1_CARD_ECC_DISABLED	= (1 shl 14);	
 MMC_RSP_R1_ERASE_RESET		    = (1 shl 13);	
 //MMC_RSP_R1_STATUS(x)            (x & 0xFFFFE000)
 //MMC_RSP_R1_CURRENT_STATE(x)	((x & 0x00001E00) >> 9)	
 MMC_RSP_R1_READY_FOR_DATA	    = (1 shl 8);
 MMC_RSP_R1_SWITCH_ERROR		= (1 shl 7);	
 MMC_RSP_R1_EXCEPTION_EVENT	    = (1 shl 6);
 MMC_RSP_R1_APP_CMD			    = (1 shl 5);
 MMC_RSP_R1_AKE_SEQ_ERROR       = (1 shl 3);
 
 {R1 SPI - MMC/SD in SPI mode reports R1 status always, and R2 for SEND_STATUS. R1 is the low order byte, R2 is the next highest byte, when present}
 MMC_RSP_R1_SPI_IDLE		    = (1 shl 0);
 MMC_RSP_R1_SPI_ERASE_RESET	    = (1 shl 1);
 MMC_RSP_R1_SPI_ILLEGAL_COMMAND	= (1 shl 2);
 MMC_RSP_R1_SPI_COM_CRC		    = (1 shl 3);
 MMC_RSP_R1_SPI_ERASE_SEQ	    = (1 shl 4);
 MMC_RSP_R1_SPI_ADDRESS		    = (1 shl 5);
 MMC_RSP_R1_SPI_PARAMETER	    = (1 shl 6);
 {R1 bit 7 is always zero}
 
 {R2 SPI - See above}
 MMC_RSP_R2_SPI_CARD_LOCKED	     = (1 shl 8);
 MMC_RSP_R2_SPI_WP_ERASE_SKIP	 = (1 shl 9);	{Or lock/unlock fail}
 MMC_RSP_R2_SPI_LOCK_UNLOCK_FAIL = MMC_RSP_R2_SPI_WP_ERASE_SKIP;
 MMC_RSP_R2_SPI_ERROR		     = (1 shl 10);
 MMC_RSP_R2_SPI_CC_ERROR		 = (1 shl 11);
 MMC_RSP_R2_SPI_CARD_ECC_ERROR	 = (1 shl 12);
 MMC_RSP_R2_SPI_WP_VIOLATION	 = (1 shl 13);
 MMC_RSP_R2_SPI_ERASE_PARAM    	 = (1 shl 14);
 MMC_RSP_R2_SPI_OUT_OF_RANGE	 = (1 shl 15);	{Or CSD overwrite}
 MMC_RSP_R2_SPI_CSD_OVERWRITE	 = MMC_RSP_R2_SPI_OUT_OF_RANGE;

 {MMC Operation Condition Register (OCR) values} {See: Section 5.1 of SD Physical Layer Simplified Specification V4.10}
 MMC_OCR_BUSY		   = $80000000; {Busy Status - 0 = Initializing / 1 = Initialization Complete}
 MMC_OCR_HCS		   = $40000000; {Card Capacity Status - 0 = SDSC / 1 = SDHC or SDXC}
 MMC_OCR_UHS_II        = $20000000; {UHS-II Card Status - 0 = Non UHS-II Card / 1 = UHS-II Card}
 MMC_OCR_S18A          = $01000000; {Switching to 1.8V Accepted - 0 = Continue current voltage signaling / 1 = Ready for switching signal voltage}
 MMC_OCR_VOLTAGE_MASK  = $007FFF80; 
 MMC_OCR_ACCESS_MODE   = $60000000; //To Do //??
 
 {MMC Card Status Register (CSR) values} {See: Section 4.10.1 of SD Physical Layer Simplified Specification Version 4.10}
 {Note: These map to the Native mode R1 response values}
 MMC_CARD_STATUS_MASK		    = not($0206BF7F);  //To Do //??
 MMC_CARD_STATUS_ERROR	        = (1 shl 19);
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
 MMC_CURRENT_STATE_PRG		 = (7 shl 9);
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
 MMC_CSD_TRAN_SPEED_VALUE     = 37; //To Do
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
 MMC_CCC_BASIC		    = (1 shl 0);	{(Class 0) Basic protocol functions (CMD0,1,2,3,4,7,9,10,12,13,15) (and for SPI, CMD58,59)}
 MMC_CCC_STREAM_READ	= (1 shl 1);	{(Class 1) Stream read commands (CMD11)}
 MMC_CCC_BLOCK_READ		= (1 shl 2);	{(Class 2) Block read commands (CMD16,17,18)}
 MMC_CCC_STREAM_WRITE	= (1 shl 3);	{(Class 3) Stream write commands (CMD20)}
 MMC_CCC_BLOCK_WRITE	= (1 shl 4);	{(Class 4) Block write commands (CMD16,24,25,26,27)}
 MMC_CCC_ERASE		    = (1 shl 5);	{(Class 5) Ability to erase blocks (CMD32,33,34,35,36,37,38,39)}
 MMC_CCC_WRITE_PROT		= (1 shl 6);	{(Class 6) Ability to write protect blocks (CMD28,29,30)}
 MMC_CCC_LOCK_CARD		= (1 shl 7);	{(Class 7) Ability to lock down card (CMD16,CMD42)}
 MMC_CCC_APP_SPEC		= (1 shl 8);	{(Class 8) Application specific (CMD55,56,57,ACMD*)}
 MMC_CCC_IO_MODE		= (1 shl 9);	{(Class 9) I/O mode (CMD5,39,40,52,53)}
 MMC_CCC_SWITCH		    = (1 shl 10);	{(Class 10) High speed switch (CMD6,34,35,36,37,50)}
 MMC_CCC_EXTENSION		= (1 shl 11);   {(Class 11) Extension (CMD?)}
 
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
 
 SECURE_ERASE	   = $80000000; //To Do //?? (Used by MMC_CMD_ERASE)
 
 {MMC Voltage Values}
 MMC_VDD_165_195	= $00000080; {VDD voltage 1.65 - 1.95}
 MMC_VDD_20_21		= $00000100; {VDD voltage 2.0 ~ 2.1}
 MMC_VDD_21_22		= $00000200; {VDD voltage 2.1 ~ 2.2}
 MMC_VDD_22_23		= $00000400; {VDD voltage 2.2 ~ 2.3}
 MMC_VDD_23_24		= $00000800; {VDD voltage 2.3 ~ 2.4}
 MMC_VDD_24_25		= $00001000; {VDD voltage 2.4 ~ 2.5}
 MMC_VDD_25_26		= $00002000; {VDD voltage 2.5 ~ 2.6}
 MMC_VDD_26_27		= $00004000; {VDD voltage 2.6 ~ 2.7}
 MMC_VDD_27_28		= $00008000; {VDD voltage 2.7 ~ 2.8}
 MMC_VDD_28_29		= $00010000; {VDD voltage 2.8 ~ 2.9}
 MMC_VDD_29_30		= $00020000; {VDD voltage 2.9 ~ 3.0}
 MMC_VDD_30_31		= $00040000; {VDD voltage 3.0 ~ 3.1}
 MMC_VDD_31_32		= $00080000; {VDD voltage 3.1 ~ 3.2}
 MMC_VDD_32_33		= $00100000; {VDD voltage 3.2 ~ 3.3}
 MMC_VDD_33_34		= $00200000; {VDD voltage 3.3 ~ 3.4}
 MMC_VDD_34_35		= $00400000; {VDD voltage 3.4 ~ 3.5}
 MMC_VDD_35_36		= $00800000; {VDD voltage 3.5 ~ 3.6}
 
 {MMC Switch Mode Values}
 MMC_SWITCH_MODE_CMD_SET	 = $00; {Change the command set}
 MMC_SWITCH_MODE_SET_BITS	 = $01; {Set bits in EXT_CSD byte addressed by index which are 1 in value field}
 MMC_SWITCH_MODE_CLEAR_BITS	 = $02; {Clear bits in EXT_CSD byte	addressed by index, which are 1 in value field}
 MMC_SWITCH_MODE_WRITE_BYTE	 = $03; {Set target byte to value}

 {MMC EXT_CSD fields} //To Do
 EXT_CSD_ENH_START_ADDR		  = 136; {R/W}
 EXT_CSD_ENH_SIZE_MULT		  = 140; {R/W}
 EXT_CSD_GP_SIZE_MULT		  = 143; {R/W}
 EXT_CSD_PARTITION_SETTING	  = 155; {R/W}
 EXT_CSD_PARTITIONS_ATTRIBUTE =	156; {R/W}
 EXT_CSD_MAX_ENH_SIZE_MULT	  = 157; {R}
 EXT_CSD_PARTITIONING_SUPPORT =	160; {RO}
 EXT_CSD_RST_N_FUNCTION		  = 162; {R/W}
 EXT_CSD_WR_REL_PARAM		  = 166; {R}
 EXT_CSD_WR_REL_SET		      = 167; {R/W}
 EXT_CSD_RPMB_MULT		      = 168; {RO}
 EXT_CSD_ERASE_GROUP_DEF	  = 175; {R/W}
 EXT_CSD_BOOT_BUS_WIDTH		  = 177;
 EXT_CSD_PART_CONF		      = 179; {R/W}
 EXT_CSD_BUS_WIDTH		      = 183; {R/W}
 EXT_CSD_HS_TIMING		      = 185; {R/W}
 EXT_CSD_REV			      = 192; {RO}
 EXT_CSD_CARD_TYPE		      = 196; {RO}
 EXT_CSD_SEC_CNT			  = 212; {RO, 4 bytes}
 EXT_CSD_HC_WP_GRP_SIZE		  = 221; {RO}
 EXT_CSD_HC_ERASE_GRP_SIZE	  = 224; {RO}
 EXT_CSD_BOOT_MULT		      = 226; {RO}
 
 {MMC EXT_CSD field definitions}
 EXT_CSD_CMD_SET_NORMAL		= (1 shl 0);
 EXT_CSD_CMD_SET_SECURE		= (1 shl 1);
 EXT_CSD_CMD_SET_CPSECURE	= (1 shl 2);

 EXT_CSD_CARD_TYPE_26	    = (1 shl 0); {Card can run at 26MHz}
 EXT_CSD_CARD_TYPE_52	    = (1 shl 1); {Card can run at 52MHz}
 EXT_CSD_CARD_TYPE_DDR_1_8V	= (1 shl 2);
 EXT_CSD_CARD_TYPE_DDR_1_2V	= (1 shl 3);
 EXT_CSD_CARD_TYPE_DDR_52	= (EXT_CSD_CARD_TYPE_DDR_1_8V or EXT_CSD_CARD_TYPE_DDR_1_2V);

 EXT_CSD_BUS_WIDTH_1	    = 0; {Card is in 1 bit mode}
 EXT_CSD_BUS_WIDTH_4	    = 1; {Card is in 4 bit mode}
 EXT_CSD_BUS_WIDTH_8	    = 2; {Card is in 8 bit mode}
 EXT_CSD_DDR_BUS_WIDTH_4	= 5; {Card is in 4 bit DDR mode}
 EXT_CSD_DDR_BUS_WIDTH_8	= 6; {Card is in 8 bit DDR mode}

 EXT_CSD_BOOT_ACK_ENABLE			= (1 shl 6);
 EXT_CSD_BOOT_PARTITION_ENABLE		= (1 shl 3);
 EXT_CSD_PARTITION_ACCESS_ENABLE	= (1 shl 0);
 EXT_CSD_PARTITION_ACCESS_DISABLE	= (0 shl 0);

 //EXT_CSD_BOOT_ACK(x)		    = (x shl 6);
 //EXT_CSD_BOOT_PART_NUM(x)	    = (x shl 3);
 //EXT_CSD_PARTITION_ACCESS(x)	= (x shl 0);

 //EXT_CSD_BOOT_BUS_WIDTH_MODE(x)  	= (x shl 3);
 //EXT_CSD_BOOT_BUS_WIDTH_RESET(x)	= (x shl 2);
 //EXT_CSD_BOOT_BUS_WIDTH_WIDTH(x)	= (x);

 EXT_CSD_PARTITION_SETTING_COMPLETED = (1 shl 0);

 EXT_CSD_ENH_USR	    = (1 shl 0); {user data area is enhanced}
 //EXT_CSD_ENH_GP(x)      = (1 shl ((x) + 1)); {GP part (x + 1) is enhanced}

 EXT_CSD_HS_CTRL_REL	= (1 shl 0); {host controlled WR_REL_SET}

 EXT_CSD_WR_DATA_REL_USR	= (1 shl 0); {user data area WR_REL}
 //EXT_CSD_WR_DATA_REL_GP(x)  = (1 shl ((x) + 1)); {GP part (x + 1) WR_REL}
 
 {MMC ?????}
 MMCPART_NOAVAILABLE = ($ff);
 PART_ACCESS_MASK	 = ($07);
 PART_SUPPORT		 = ($01);
 ENHNCD_SUPPORT		 = ($02);
 PART_ENH_ATTRIB	 = ($1f);
 
 {Maximum block size for MMC}
 MMC_MAX_BLOCK_LEN	= 512;

 {Maximum block count for MMC}
 MMC_MAX_BLOCK_COUNT = 65535;
 
 {The number of MMC physical partitions.  These consist of: boot partitions (2), general purpose partitions (4) in MMC v4.4.}
 MMC_NUM_BOOT_PARTITION	= 2;
 MMC_PART_RPMB          = 3; {RPMB partition number}
 
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
 SD_CMD_SEND_RELATIVE_ADDR	  = 3;
 SD_CMD_SEND_IF_COND		  = 8;
 SD_CMD_SWITCH_VOLTAGE        = 11;
 
 {Class 10}
 SD_CMD_SWITCH                = 6;  {See: 4.3.10 Switch Function Command}
 
 {Class 5}
 SD_CMD_ERASE_WR_BLK_START	  = 32;
 SD_CMD_ERASE_WR_BLK_END	  = 33;
 
 {Application commands}
 SD_CMD_APP_SET_BUS_WIDTH	  = 6;
 SD_CMD_APP_SD_STATUS         = 13;
 SD_CMD_APP_SEND_NUM_WR_BLKS  = 22;
 SD_CMD_APP_SEND_OP_COND	  = 41;
 SD_CMD_APP_SEND_SCR		  = 51;
 
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
 SD_SWITCH_MODE_CHECK	= 0;
 SD_SWITCH_MODE_SWITCH	= 1;
 
 {SD Switch Function Groups}
 SD_SWITCH_FUNCTION_GROUP_ACCESS	= 0; {Access Mode}
 SD_SWITCH_FUNCTION_GROUP_COMMAND	= 1; {Command System}
 SD_SWITCH_FUNCTION_GROUP_DRIVER	= 2; {Driver Strength}
 SD_SWITCH_FUNCTION_GROUP_POWER 	= 3; {Power Limit}
 
 {SD Switch Access Modes}
 SD_SWITCH_ACCESS_MODE_DEF	  = 0;  {Default SDR12}
 SD_SWITCH_ACCESS_MODE_HS	  = 1;  {High Speed SDR25}
 SD_SWITCH_ACCESS_MODE_SDR50  = 2;  {SDR50 (1.8V only)}
 SD_SWITCH_ACCESS_MODE_SDR104 = 3;  {SDR104 (1.8V only)}
 SD_SWITCH_ACCESS_MODE_DDR50  = 4;  {DDR50 (1.8V only)}
 
 {SD Switch Command System}
 SD_SWITCH_COMMAND_SYSTEM_DEF	  = 0;  {Default}
 SD_SWITCH_COMMAND_SYSTEM_EC	  = 1;  {For eC}
 SD_SWITCH_COMMAND_SYSTEM_OTP	  = 3;  {OTP}
 SD_SWITCH_COMMAND_SYSTEM_ASSD	  = 4;  {ASSD}
 
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
 SD_OCR_CCS		   = $40000000; {Card Capacity Status - 0 = SDSC / 1 = SDHC or SDXC}
 SD_OCR_UHS_II     = $20000000; {UHS-II Card Status - 0 = Non UHS-II Card / 1 = UHS-II Card}
 SD_OCR_XPC	       = $10000000;	{SDXC Power Control}
 SD_OCR_S18A	   = $01000000; {1.8V Switching Accepted}
 
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
 SD_SCR_SPEC_VER_0		= 0;	{Implements system specification 1.0 - 1.01}
 SD_SCR_SPEC_VER_1		= 1;	{Implements system specification 1.10}
 SD_SCR_SPEC_VER_2		= 2;	{Implements system specification 2.00-4.0X}
 
 {SD SCR Security values}
 SD_SCR_SECURITY_VER_0 = 0;     {No Security}
 SD_SCR_SECURITY_VER_2 = 2;     {SDSC Card (Security Version 1.01)}
 SD_SCR_SECURITY_VER_3 = 3;     {SDHC Card (Security Version 2.00)}
 SD_SCR_SECURITY_VER_4 = 4;     {SDXC Card (Security Version 3.xx)}
 
 {SD SCR Bus Width values}
 SD_SCR_BUS_WIDTH_1	   = (1 shl 0);     {1 bit (DAT0)}
 SD_SCR_BUS_WIDTH_4	   = (1 shl 2);     {4 bit (DAT0-3)}
 
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
 SDIO_RSP_R5_COM_CRC_ERROR	    = (1 shl 15);
 SDIO_RSP_R5_ILLEGAL_COMMAND	= (1 shl 14);	
 SDIO_RSP_R5_ERROR		        = (1 shl 11);	
 SDIO_RSP_R5_FUNCTION_NUMBER	= (1 shl 9);
 SDIO_RSP_R5_OUT_OF_RANGE		= (1 shl 8);
 //SDIO_RSP_R5_STATUS(x)		(x & 0xCB00)
 //SDIO_RSP_R5_IO_CURRENT_STATE(x)	((x & 0x3000) >> 12) /* s, b */
 
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
 SDIO_CCCR_CCCR		= $00;
 SDIO_CCCR_SD		= $01;
 SDIO_CCCR_IOEx		= $02;
 SDIO_CCCR_IORx		= $03;
 SDIO_CCCR_IENx		= $04;	{Function/Master Interrupt Enable}
 SDIO_CCCR_INTx		= $05;	{Function Interrupt Pending}
 SDIO_CCCR_ABORT	= $06;	{function abort/card reset}
 SDIO_CCCR_IF		= $07;	{bus interface controls}
 SDIO_CCCR_CAPS		= $08;
 SDIO_CCCR_CIS		= $09;	{common CIS pointer (3 bytes)}
 {Following 4 regs are valid only if SBS is set}
 SDIO_CCCR_SUSPEND	= $0c;
 SDIO_CCCR_SELx		= $0d;
 SDIO_CCCR_EXECx	= $0e;
 SDIO_CCCR_READYx	= $0f;
 SDIO_CCCR_BLKSIZE	= $10;
 SDIO_CCCR_POWER	= $12;
 SDIO_CCCR_SPEED	= $13;
 SDIO_CCCR_UHS		= $14;
 SDIO_CCCR_DRIVE_STRENGTH = $15;
 
 {SDIO CCCR CCCR Register values} 
 SDIO_CCCR_REV_1_00	= 0;	{CCCR/FBR Version 1.00}
 SDIO_CCCR_REV_1_10	= 1;	{CCCR/FBR Version 1.10}
 SDIO_CCCR_REV_1_20	= 2;	{CCCR/FBR Version 1.20}
 SDIO_CCCR_REV_3_00	= 3;	{CCCR/FBR Version 3.00}

 SDIO_SDIO_REV_1_00	= 0;	{SDIO Spec Version 1.00}
 SDIO_SDIO_REV_1_10	= 1;	{SDIO Spec Version 1.10}
 SDIO_SDIO_REV_1_20	= 2;	{SDIO Spec Version 1.20}
 SDIO_SDIO_REV_2_00	= 3;	{SDIO Spec Version 2.00}
 SDIO_SDIO_REV_3_00	= 4;	{SDIO Spec Version 3.00}
  
 {SDIO CCCR SD Register values} 
 SDIO_SD_REV_1_01	= 0;	{SD Physical Spec Version 1.01}
 SDIO_SD_REV_1_10	= 1;	{SD Physical Spec Version 1.10}
 SDIO_SD_REV_2_00	= 2;	{SD Physical Spec Version 2.00}
 SDIO_SD_REV_3_00	= 3;	{SD Physical Spev Version 3.00}
  
 {SDIO CCCR IF Register values} 
 SDIO_BUS_WIDTH_MASK	 = $03;	{data bus width setting}
 SDIO_BUS_WIDTH_1BIT	 = $00;
 SDIO_BUS_WIDTH_RESERVED = $01;
 SDIO_BUS_WIDTH_4BIT	 = $02;
 SDIO_BUS_ECSI		     = $20;	{Enable continuous SPI interrupt}
 SDIO_BUS_SCSI		     = $40;	{Support continuous SPI interrupt}

 SDIO_BUS_ASYNC_INT	     = $20;

 SDIO_BUS_CD_DISABLE     = $80;	{disable pull-up on DAT3 (pin 1)}
 
 {SDIO CCCR CAPS Register values} 
 SDIO_CCCR_CAP_SDC	= $01;	{can do CMD52 while data transfer}
 SDIO_CCCR_CAP_SMB	= $02;	{can do multi-block xfers (CMD53)}
 SDIO_CCCR_CAP_SRW	= $04;	{supports read-wait protocol}
 SDIO_CCCR_CAP_SBS	= $08;	{supports suspend/resume}
 SDIO_CCCR_CAP_S4MI	= $10;	{interrupt during 4-bit CMD53}
 SDIO_CCCR_CAP_E4MI	= $20;	{enable ints during 4-bit CMD53}
 SDIO_CCCR_CAP_LSC	= $40;	{low speed card}
 SDIO_CCCR_CAP_4BLS	= $80;	{4 bit low speed card}
 
 {SDIO CCCR POWER Register values} 
 SDIO_POWER_SMPC	= $01;	{Supports Master Power Control}
 SDIO_POWER_EMPC	= $02;	{Enable Master Power Control}
 
 {SDIO CCCR SPEED Register values} 
 SDIO_SPEED_SHS		= $01;	{Supports High-Speed mode}
 SDIO_SPEED_BSS_SHIFT	= 1;
 SDIO_SPEED_BSS_MASK	= (7 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR12	    = (0 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR25	    = (1 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR50	    = (2 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_SDR104	    = (3 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_DDR50	    = (4 shl SDIO_SPEED_BSS_SHIFT);
 SDIO_SPEED_EHS		= SDIO_SPEED_SDR25;	{Enable High-Speed}
 
 {SDIO CCCR UHS Register values} 
 SDIO_UHS_SDR50	= $01;
 SDIO_UHS_SDR104	= $02;
 SDIO_UHS_DDR50	= $04;
 
 {SDIO CCCR DRIVE STRENGTH Register values} 
 SDIO_SDTx_MASK		= $07;
 SDIO_DRIVE_SDTA	    = (1 shl 0);
 SDIO_DRIVE_SDTC	    = (1 shl 1);
 SDIO_DRIVE_SDTD	    = (1 shl 2);
 SDIO_DRIVE_DTSx_MASK	= $03;
 SDIO_DRIVE_DTSx_SHIFT	= 4;
 SDIO_DTSx_SET_TYPE_B	= (0 shl SDIO_DRIVE_DTSx_SHIFT);
 SDIO_DTSx_SET_TYPE_A	= (1 shl SDIO_DRIVE_DTSx_SHIFT);
 SDIO_DTSx_SET_TYPE_C	= (2 shl SDIO_DRIVE_DTSx_SHIFT);
 SDIO_DTSx_SET_TYPE_D	= (3 shl SDIO_DRIVE_DTSx_SHIFT);
 
 {SDIO Function Basic Registers (FBR)}
 //SDIO_FBR_BASE(f)	((f) * $100) {base of function f's FBRs}
 SDIO_FBR_STD_IF		= $00;
 SDIO_FBR_STD_IF_EXT	= $01;
 SDIO_FBR_POWER		    = $02;
 SDIO_FBR_CIS		    = $09;	{CIS pointer (3 bytes)}
 SDIO_FBR_CSA		    = $0C;	{CSA pointer (3 bytes)}
 SDIO_FBR_CSA_DATA	    = $0F;
 SDIO_FBR_BLKSIZE	    = $10;	{block size (2 bytes)}
 
 {SDIO FBR IF Register values}
 SDIO_FBR_SUPPORTS_CSA	= $40;	{supports Code Storage Area}
 SDIO_FBR_ENABLE_CSA	= $80;	{enable Code Storage Area}
 
 {SDIO FBR POWER Register values}
 SDIO_FBR_POWER_SPS	= $01;	{Supports Power Selection}
 SDIO_FBR_POWER_EPS	= $02;	{Enable (low) Power Selection}
 
 //To Do
 
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
 SDHCI_FLAG_SDMA          = $00000001;
 SDHCI_FLAG_ADMA          = $00000002;
 SDHCI_FLAG_SPI           = $00000004;
 SDHCI_FLAG_CRC_ENABLE    = $00000008;
 SDHCI_FLAG_NON_STANDARD  = $00000010; {Host Controller uses a non standard interface (not supporting SDHCI register layout)}
 SDHCI_FLAG_AUTO_CMD12    = $00000020; {Host Controller supports Auto CMD12 (Stop Transmission)}
 SDHCI_FLAG_AUTO_CMD23    = $00000040; {Host Controller supports Auto CMD23 (Set Block Count)}
 //To Do //More //DMA shared, DMA align etc
 
 {SDHCI Controller Registers}
 SDHCI_DMA_ADDRESS	     = $00;
 SDHCI_BLOCK_SIZE	     = $04;
 SDHCI_BLOCK_COUNT	     = $06;
 SDHCI_ARGUMENT		     = $08;
 SDHCI_TRANSFER_MODE	 = $0C;
 SDHCI_COMMAND		     = $0E;
 SDHCI_RESPONSE		     = $10;
 SDHCI_BUFFER		     = $20;
 SDHCI_PRESENT_STATE	 = $24;
 SDHCI_HOST_CONTROL	     = $28;
 SDHCI_POWER_CONTROL	 = $29;
 SDHCI_BLOCK_GAP_CONTROL = $2A;
 SDHCI_WAKE_UP_CONTROL	 = $2B;
 SDHCI_CLOCK_CONTROL	 = $2C;
 SDHCI_TIMEOUT_CONTROL	 = $2E;
 SDHCI_SOFTWARE_RESET	 = $2F;
 SDHCI_INT_STATUS	     = $30;
 SDHCI_INT_ENABLE	     = $34;
 SDHCI_SIGNAL_ENABLE	 = $38;
 SDHCI_ACMD12_ERR	     = $3C;
 {3E-3F reserved}
 SDHCI_CAPABILITIES      = $40;
 SDHCI_CAPABILITIES_1	 = $44;
 SDHCI_MAX_CURRENT	     = $48;
 {4C-4F reserved for more max current}
 SDHCI_SET_ACMD12_ERROR	 = $50;
 SDHCI_SET_INT_ERROR	 = $52;
 SDHCI_ADMA_ERROR	     = $54;
 {55-57 reserved}
 SDHCI_ADMA_ADDRESS	     = $58;
 {60-FB reserved}
 SDHCI_SLOT_INT_STATUS	 = $FC;
 SDHCI_HOST_VERSION	     = $FE;
 
 {SDHCI Transfer Modes}
 SDHCI_TRNS_DMA		    = $01;
 SDHCI_TRNS_BLK_CNT_EN	= $02;
 SDHCI_TRNS_AUTO_CMD12	= $04;  {SDHCI_TRNS_ACMD12}
 SDHCI_TRNS_AUTO_CMD23	= $08;
 SDHCI_TRNS_READ	    = $10;
 SDHCI_TRNS_MULTI	    = $20;
 
 {SDHCI Command Values}
 SDHCI_CMD_RESP_MASK = $03;
 SDHCI_CMD_CRC		 = $08;
 SDHCI_CMD_INDEX	 = $10;
 SDHCI_CMD_DATA		 = $20;
 SDHCI_CMD_ABORTCMD	 = $C0;
 
 {SDHCI Command Response Values}
 SDHCI_CMD_RESP_NONE	   = $00;
 SDHCI_CMD_RESP_LONG	   = $01;
 SDHCI_CMD_RESP_SHORT	   = $02;
 SDHCI_CMD_RESP_SHORT_BUSY = $03;
 
 {SDHCI Present State Values} 
 SDHCI_CMD_INHIBIT	         = $00000001;
 SDHCI_DATA_INHIBIT	         = $00000002;
 SDHCI_DOING_WRITE	         = $00000100;
 SDHCI_DOING_READ	         = $00000200;
 SDHCI_SPACE_AVAILABLE	     = $00000400;
 SDHCI_DATA_AVAILABLE	     = $00000800;
 SDHCI_CARD_PRESENT	         = $00010000;
 SDHCI_CARD_STATE_STABLE	 = $00020000;
 SDHCI_CARD_DETECT_PIN_LEVEL = $00040000;
 SDHCI_WRITE_PROTECT	     = $00080000; {Set if Write Enabled / Clear if Write Protected}
 
 {SDHCI Host Control Values}
 SDHCI_CTRL_LED		    = $01;
 SDHCI_CTRL_4BITBUS	    = $02;
 SDHCI_CTRL_HISPD	    = $04;
 SDHCI_CTRL_DMA_MASK    = $18;
 SDHCI_CTRL_SDMA	    = $00;
 SDHCI_CTRL_ADMA1	    = $08;
 SDHCI_CTRL_ADMA32	    = $10;
 SDHCI_CTRL_ADMA64	    = $18;
 SDHCI_CTRL_8BITBUS 	= $20;
 SDHCI_CTRL_CD_TEST_INS = $40;
 SDHCI_CTRL_CD_TEST	    = $80;

 {SDHCI Power Control Values}
 SDHCI_POWER_ON		= $01;
 SDHCI_POWER_180	= $0A;
 SDHCI_POWER_300	= $0C;
 SDHCI_POWER_330	= $0E;

 {SDHCI Wakeup Control Values}
 SDHCI_WAKE_ON_INT	  = $01;
 SDHCI_WAKE_ON_INSERT = $02;
 SDHCI_WAKE_ON_REMOVE = $04;
 
 {SDHCI Clock Control Values}
 SDHCI_DIVIDER_SHIFT	= 8;
 SDHCI_DIVIDER_HI_SHIFT	= 6;
 SDHCI_DIV_MASK	        = $FF;
 SDHCI_DIV_MASK_LEN	    = 8;
 SDHCI_DIV_HI_MASK	    = $0300;
 SDHCI_CLOCK_CARD_EN	= $0004;
 SDHCI_CLOCK_INT_STABLE = $0002;
 SDHCI_CLOCK_INT_EN	    = $0001;
 
 {SDHCI Software Reset Values}
 SDHCI_RESET_ALL	= $01;
 SDHCI_RESET_CMD	= $02;
 SDHCI_RESET_DATA	= $04;
 
 {SDHCI Interrupt Values}
 SDHCI_INT_RESPONSE	    = $00000001;
 SDHCI_INT_DATA_END	    = $00000002;
 SDHCI_INT_BLK_GAP      = $00000004;
 SDHCI_INT_DMA_END	    = $00000008;
 SDHCI_INT_SPACE_AVAIL	= $00000010;
 SDHCI_INT_DATA_AVAIL	= $00000020;
 SDHCI_INT_CARD_INSERT	= $00000040;
 SDHCI_INT_CARD_REMOVE	= $00000080;
 SDHCI_INT_CARD_INT	    = $00000100;
 SDHCI_INT_ERROR	    = $00008000;
 SDHCI_INT_TIMEOUT	    = $00010000;
 SDHCI_INT_CRC		    = $00020000;
 SDHCI_INT_END_BIT	    = $00040000;
 SDHCI_INT_INDEX	    = $00080000;
 SDHCI_INT_DATA_TIMEOUT = $00100000;
 SDHCI_INT_DATA_CRC	    = $00200000;
 SDHCI_INT_DATA_END_BIT = $00400000;
 SDHCI_INT_BUS_POWER	= $00800000;
 SDHCI_INT_ACMD12ERR	= $01000000;
 SDHCI_INT_ADMA_ERROR	= $02000000;

 SDHCI_INT_NORMAL_MASK	= $00007FFF;
 SDHCI_INT_ERROR_MASK	= $FFFF8000;

 SDHCI_INT_CMD_MASK	    = (SDHCI_INT_RESPONSE or SDHCI_INT_TIMEOUT or SDHCI_INT_CRC or SDHCI_INT_END_BIT or SDHCI_INT_INDEX);
 SDHCI_INT_DATA_MASK	= (SDHCI_INT_DATA_END or SDHCI_INT_DMA_END or SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_END_BIT or SDHCI_INT_ADMA_ERROR or SDHCI_INT_BLK_GAP);
 SDHCI_INT_ALL_MASK	    = (LongWord(-1));
 
 {SDHCI Capabilities Values}
 SDHCI_TIMEOUT_CLK_MASK	     = $0000003F;
 SDHCI_TIMEOUT_CLK_SHIFT     = 0;
 SDHCI_TIMEOUT_CLK_UNIT	     = $00000080;
 SDHCI_CLOCK_BASE_MASK	     = $00003F00;
 SDHCI_CLOCK_V3_BASE_MASK    = $0000FF00;
 SDHCI_CLOCK_BASE_SHIFT	     = 8;
 SDHCI_CLOCK_BASE_MULTIPLIER = 1000000;
 SDHCI_MAX_BLOCK_MASK	     = $00030000;
 SDHCI_MAX_BLOCK_SHIFT       = 16;
 SDHCI_CAN_DO_8BIT	         = $00040000;
 SDHCI_CAN_DO_ADMA2	         = $00080000;
 SDHCI_CAN_DO_ADMA1	         = $00100000;
 SDHCI_CAN_DO_HISPD	         = $00200000;
 SDHCI_CAN_DO_SDMA	         = $00400000;
 SDHCI_CAN_VDD_330	         = $01000000;
 SDHCI_CAN_VDD_300	         = $02000000;
 SDHCI_CAN_VDD_180	         = $04000000;
 SDHCI_CAN_64BIT	         = $10000000;
 
 {SDHCI Host Version Values}
 SDHCI_VENDOR_VER_MASK	= $FF00;
 SDHCI_VENDOR_VER_SHIFT	= 8;
 SDHCI_SPEC_VER_MASK	= $00FF;
 SDHCI_SPEC_VER_SHIFT	= 0;
 SDHCI_SPEC_100	        = 0;
 SDHCI_SPEC_200	        = 1;
 SDHCI_SPEC_300	        = 2;

 //SDHCI_GET_VERSION(x) (x->version and SDHCI_SPEC_VER_MASK);
 
 {SDHCI Clock Dividers}
 SDHCI_MAX_CLOCK_DIV_SPEC_200	 = 256;
 SDHCI_MAX_CLOCK_DIV_SPEC_300	 = 2046;
 
 {SDHCI Quirks/Bugs}
 {From: U-Boot sdhci.h}
 (*SDHCI_QUIRK_32BIT_DMA_ADDR	          = (1 shl 0); {See: SDHCI_QUIRK_32BIT_DMA_ADDR below}
 SDHCI_QUIRK_REG32_RW		          = (1 shl 1);
 SDHCI_QUIRK_BROKEN_R1B		          = (1 shl 2);
 SDHCI_QUIRK_NO_HISPD_BIT	          = (1 shl 3); {See: SDHCI_QUIRK_NO_HISPD_BIT below}
 SDHCI_QUIRK_BROKEN_VOLTAGE	          = (1 shl 4); {Use  SDHCI_QUIRK_MISSING_CAPS instead}
 SDHCI_QUIRK_NO_CD		              = (1 shl 5); {See: SDHCI_QUIRK_BROKEN_CARD_DETECTION below}
 SDHCI_QUIRK_WAIT_SEND_CMD	          = (1 shl 6);
 SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER  = (1 shl 7); {See: SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER below}
 SDHCI_QUIRK_USE_WIDE8		          = (1 shl 8);
 SDHCI_QUIRK_MISSING_CAPS             = (1 shl 9); {See: SDHCI_QUIRK_MISSING_CAPS below}*)
 
 {From Linux /include/linux/mmc/sdhci.h}
 SDHCI_QUIRK_CLOCK_BEFORE_RESET			= (1 shl 0); {Controller doesn't honor resets unless we touch the clock register}
 SDHCI_QUIRK_FORCE_DMA				    = (1 shl 1); {Controller has bad caps bits, but really supports DMA}
 SDHCI_QUIRK_NO_CARD_NO_RESET			= (1 shl 2); {Controller doesn't like to be reset when there is no card inserted.}
 SDHCI_QUIRK_SINGLE_POWER_WRITE			= (1 shl 3); {Controller doesn't like clearing the power reg before a change}
 SDHCI_QUIRK_RESET_CMD_DATA_ON_IOS		= (1 shl 4); {Controller has flaky internal state so reset it on each ios change}
 SDHCI_QUIRK_BROKEN_DMA				    = (1 shl 5); {Controller has an unusable DMA engine}
 SDHCI_QUIRK_BROKEN_ADMA				= (1 shl 6); {Controller has an unusable ADMA engine}
 SDHCI_QUIRK_32BIT_DMA_ADDR			    = (1 shl 7); {Controller can only DMA from 32-bit aligned addresses}
 SDHCI_QUIRK_32BIT_DMA_SIZE			    = (1 shl 8); {Controller can only DMA chunk sizes that are a multiple of 32 bits}
 SDHCI_QUIRK_32BIT_ADMA_SIZE			= (1 shl 9); {Controller can only ADMA chunks that are a multiple of 32 bits}
 SDHCI_QUIRK_RESET_AFTER_REQUEST		= (1 shl 10); {Controller needs to be reset after each request to stay stable}
 SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER	= (1 shl 11); {Controller needs voltage and power writes to happen separately}
 SDHCI_QUIRK_BROKEN_TIMEOUT_VAL			= (1 shl 12); {Controller provides an incorrect timeout value for transfers}
 SDHCI_QUIRK_BROKEN_SMALL_PIO			= (1 shl 13); {Controller has an issue with buffer bits for small transfers}
 SDHCI_QUIRK_NO_BUSY_IRQ				= (1 shl 14); {Controller does not provide transfer-complete interrupt when not busy}
 SDHCI_QUIRK_BROKEN_CARD_DETECTION		= (1 shl 15); {Controller has unreliable card detection}
 SDHCI_QUIRK_INVERTED_WRITE_PROTECT		= (1 shl 16); {Controller reports inverted write-protect state}
 SDHCI_QUIRK_PIO_NEEDS_DELAY			= (1 shl 18); {Controller does not like fast PIO transfers}
 SDHCI_QUIRK_FORCE_BLK_SZ_2048			= (1 shl 20); {Controller has to be forced to use block size of 2048 bytes}
 SDHCI_QUIRK_NO_MULTIBLOCK			    = (1 shl 21); {Controller cannot do multi-block transfers}
 SDHCI_QUIRK_FORCE_1_BIT_DATA			= (1 shl 22); {Controller can only handle 1-bit data transfers}
 SDHCI_QUIRK_DELAY_AFTER_POWER			= (1 shl 23); {Controller needs 10ms delay between applying power and clock}
 SDHCI_QUIRK_DATA_TIMEOUT_USES_SDCLK	= (1 shl 24); {Controller uses SDCLK instead of TMCLK for data timeouts}
 SDHCI_QUIRK_CAP_CLOCK_BASE_BROKEN		= (1 shl 25); {Controller reports wrong base clock capability}
 SDHCI_QUIRK_NO_ENDATTR_IN_NOPDESC		= (1 shl 26); {Controller cannot support End Attribute in NOP ADMA descriptor}
 SDHCI_QUIRK_MISSING_CAPS			    = (1 shl 27); {Controller is missing device caps. Use caps provided by host}
 SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12		= (1 shl 28); {Controller uses Auto CMD12 command to stop the transfer}
 SDHCI_QUIRK_NO_HISPD_BIT			    = (1 shl 29); {Controller doesn't have HISPD bit field in HI-SPEED SD card}
 SDHCI_QUIRK_BROKEN_ADMA_ZEROLEN_DESC	= (1 shl 30); {Controller treats ADMA descriptors with length 0000h incorrectly}
 SDHCI_QUIRK_UNSTABLE_RO_DETECT			= (1 shl 31); {The read-only detection via SDHCI_PRESENT_STATE register is unstable}
 
 {SDHCI More Quirks/Bugs} 
 {From Linux /include/linux/mmc/sdhci.h}
 SDHCI_QUIRK2_HOST_OFF_CARD_ON			= (1 shl 0);
 SDHCI_QUIRK2_HOST_NO_CMD23			    = (1 shl 1);
 SDHCI_QUIRK2_NO_1_8_V				    = (1 shl 2); {The system physically doesn't support 1.8v, even if the host does}
 SDHCI_QUIRK2_PRESET_VALUE_BROKEN		= (1 shl 3);
 SDHCI_QUIRK2_CARD_ON_NEEDS_BUS_ON		= (1 shl 4);
 SDHCI_QUIRK2_BROKEN_HOST_CONTROL		= (1 shl 5); {Controller has a non-standard host control register}
 SDHCI_QUIRK2_BROKEN_HS200			    = (1 shl 6); {Controller does not support HS200}
 SDHCI_QUIRK2_BROKEN_DDR50			    = (1 shl 7); {Controller does not support DDR50}
 SDHCI_QUIRK2_STOP_WITH_TC			    = (1 shl 8); {Stop command(CMD12) can set Transfer Complete when not using MMC_RSP_BUSY}
 
 {Additions from U-Boot}
 SDHCI_QUIRK2_REG32_RW                  = (1 shl 28); {Controller requires all register reads and writes as 32bit} //To Do //Not Required ?
 SDHCI_QUIRK2_BROKEN_R1B                = (1 shl 29); {Response type R1B is broken}                                //To Do //Not Required ?
 SDHCI_QUIRK2_WAIT_SEND_CMD             = (1 shl 30); {Controller requires a delay between each command write}     //To Do //Not Required ?
 SDHCI_QUIRK2_USE_WIDE8                 = (1 shl 31); {????????}
 
 {SDHCI Host SDMA buffer boundary (Valid values from 4K to 512K in powers of 2)}
 SDHCI_DEFAULT_BOUNDARY_SIZE  = (512 * 1024);
 SDHCI_DEFAULT_BOUNDARY_ARG	  = (7);

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
  DeviceSize:Word;                  {Device size}
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
  Voltages:LongWord;
  Capabilities:LongWord;
  {Register Properties}                            {See: Table 3-2: SD Memory Card Registers of SD Physical Layer Simplified Specification Version 4.10}
  InterfaceCondition:LongWord;                     {Interface Condition Result}
  OperationCondition:LongWord;                     {Operation Condition Register (OCR)} {See: Section 5.1 of SD Physical Layer Simplified Specification Version 4.10} 
  RelativeCardAddress:LongWord;                    {Relative Card Address (RCA) (Word)} {See: Section 5.4 of SD Physical Layer Simplified Specification Version 4.10}
  CardSpecific:array[0..3] of LongWord;            {Card Specific Data (CSD)}           {See: Section 5.3 of SD Physical Layer Simplified Specification Version 4.10}
  CardIdentification:array[0..3] of LongWord;      {Card Identification Data (CID)}     {See: Section 5.2 of SD Physical Layer Simplified Specification Version 4.10}
  CardStatus:LongWord;                             {Card Status Register (CSR)}         {See: Section 4.10.1 of SD Physical Layer Simplified Specification Version 4.10}
  DriverStage:LongWord;                            {Driver Stage Register (DSR) (Word)} {See: Section 5.5 of SD Physical Layer Simplified Specification Version 4.10}
  SDStatus:array[0..15] of LongWord;               {SD Status Register (SSR)}           {See: Section 4.10.2 of SD Physical Layer Simplified Specification Version 4.10}
  SDSwitch:array[0..15] of LongWord;               {SD Switch Status}                   {See: Section 4.3.10 of SD Physical Layer Simplified Specification Version 4.10}
  SDConfiguration:array[0..1] of LongWord;         {SD Configuration Register (SCR)}    {See: Section 5.6 of SD Physical Layer Simplified Specification Version 4.10}
  {Configuration Properties}
  CardSpecificData:TMMCCardSpecificData;
  CardIdentificationData:TMMCCardIdentificationData;
  SDStatusData:TSDStatusData;
  SDSwitchData:TSDSwitchData;
  SDConfigurationData:TSDConfigurationData;
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
 
{==============================================================================}
type
 {SDHCI specific types}
 {SDHCI Host}
 PSDHCIHost = ^TSDHCIHost;
 
 {SDHCI Enumeration Callback}
 TSDHCIEnumerate = function(SDHCI:PSDHCIHost;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {SDHCI Notification Callback}
 TSDHCINotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {SDHCI Host Methods}
 TSDHCIHostStart = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostStop = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadByte = function(SDHCI:PSDHCIHost;Reg:LongWord):Byte;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadWord = function(SDHCI:PSDHCIHost;Reg:LongWord):Word;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostReadLong = function(SDHCI:PSDHCIHost;Reg:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteByte = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteWord = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostWriteLong = procedure(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord);{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetClockDivider = function(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSDHCIHostSetControlRegister = function(SDHCI:PSDHCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TSDHCIHost = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this SDHCI}
  {SDHCI Properties}
  SDHCIId:LongWord;                    {Unique Id of this SDHCI in the SDHCI table}
  SDHCIState:LongWord;                 {SDHCI state (eg SDHCI_STATE_ENABLED)}
  HostStart:TSDHCIHostStart;           {A Host specific HostStart method implementing a standard SDHCI host interface}
  HostStop:TSDHCIHostStop;             {A Host specific HostStop method implementing a standard SDHCI host interface}
  HostReadByte:TSDHCIHostReadByte;     {A Host specific HostReadByte method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostReadWord:TSDHCIHostReadWord;     {A Host specific HostReadWord method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostReadLong:TSDHCIHostReadLong;     {A Host specific HostReadLong method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteByte:TSDHCIHostWriteByte;   {A Host specific HostWriteByte method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteWord:TSDHCIHostWriteWord;   {A Host specific HostWriteWord method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostWriteLong:TSDHCIHostWriteLong;   {A Host specific HostWriteLong method implementing a standard SDHCI host interface (Or nil if the default method is suitable)}
  HostSetClockDivider:TSDHCIHostSetClockDivider;
  HostSetControlRegister:TSDHCIHostSetControlRegister;
  DeviceInitialize:TMMCDeviceInitialize;           {A Device specific DeviceInitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceDeinitialize:TMMCDeviceDeinitialize;       {A Device specific DeviceDeinitialize method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetCardDetect:TMMCDeviceGetCardDetect;     {A Device specific DeviceGetCardDetect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceGetWriteProtect:TMMCDeviceGetWriteProtect; {A Device specific DeviceGetWriteProtect method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSendCommand:TMMCDeviceSendCommand;         {A Device specific DeviceSendCommand method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  DeviceSetIOS:TMMCDeviceSetIOS;                   {A Device specific DeviceSetIOS method implementing a standard MMC device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                   {Host lock}
  Address:Pointer;                     {Host register base address}
  Version:LongWord;                    {Host version information}
  Quirks:LongWord;                     {Host quirks/bugs flags}
  Quirks2:LongWord;                    {Host additional quirks/bugs flags}
  Clock:LongWord;                      {Host current clock}
  BusWidth:LongWord;                   {Host current bus width}
  Interrupts:LongWord;                 {Host interrupts to be handled}
  Voltages:LongWord;                   {Host configured voltage flags}
  Capabilities:LongWord;               {Host configured capabilities flags}
  MinimumFrequency:LongWord;           {Host configured minimum frequency}
  MaximumFrequency:LongWord;           {Host configured maximum frequency}
  MaximumBlockCount:LongWord;          {Host configured maximum block count}
  //To Do
  //PowerGPIO
  //CardDetectGPIO
  Command:PMMCCommand;                 {Currently processing command}
  Wait:TSemaphoreHandle;               {Command completed semaphore}
  {Configuration Properties}
  PresetVoltages:LongWord;             {Host predefined voltage flags}
  PresetCapabilities:LongWord;         {Host predefined capabilities flags}
  ClockMinimum:LongWord;               {Host predefined minimum clock frequency}
  ClockMaximum:LongWord;               {Host predefined maximum clock frequency}
  DriverStageRegister:LongWord;        {Host predefined driver stage register (DSR)}
  //To Do
  //PartitionType
  {Statistics Properties}                                        
  InterruptCount:LongWord;             {Number of interrupt requests received by the host}
  //To Do
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
function MMCDeviceSetBusWidth(MMC:PMMCDevice;Width:LongWord):LongWord;
function MMCDeviceSetBlockLength(MMC:PMMCDevice;Length:LongWord):LongWord;
function MMCDeviceSetBlockCount(MMC:PMMCDevice;Count:LongWord;Relative:Boolean):LongWord;
function MMCDeviceSetDriverStage(MMC:PMMCDevice;DriverStage:LongWord):LongWord;

function MMCDeviceStopTransmission(MMC:PMMCDevice):LongWord;

function MMCDeviceSelectCard(MMC:PMMCDevice):LongWord;
function MMCDeviceDeselectCard(MMC:PMMCDevice):LongWord;

//function MMCDeviceSetCapacity  //mmc_set_capacity (Block Device)
//function MMCDeviceChangeFrequency //mmc_change_freq

function MMCDeviceSwitch(MMC:PMMCDevice;Setting,Index,Value:Byte):LongWord;

function MMCDeviceSendCardStatus(MMC:PMMCDevice):LongWord;

function MMCDeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord;
//function MMCDeviceDecodeOperationCondition //To Do

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


//To Do //
              //mmc_select_hwpart / mmc_switch_part / *mmc_set_clock / *mmc_set_bus_width
              // 
              
              //mmc_startup / mmc_start_init / mmc_complete_init / mmc_init

//    Boot    //mmc_boot_partition_size_change / mmc_set_boot_bus_width / mmc_set_part_conf / mmc_set_rst_n_function
             
//    RPMB    //mmc_rpmb_set_key /  mmc_rpmb_get_counter / mmc_rpmb_read / mmc_rpmb_write
           

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

function SDDeviceSetBusSpeed(MMC:PMMCDevice;Speed:LongWord):LongWord;
function SDDeviceSetBusWidth(MMC:PMMCDevice;Width:LongWord):LongWord;

function SDDeviceSendInterfaceCondition(MMC:PMMCDevice):LongWord;
//function SDDeviceDecodeInterfaceCondition //To Do

function SDDeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord; 
//function SDDeviceDecodeOperationCondition //To Do

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
function SDIODeviceReset(MMC:PMMCDevice):LongWord;

function SDIODeviceSendOperationCondition(MMC:PMMCDevice;Probe:Boolean):LongWord; 

function SDIODeviceReadWriteDirect(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Input:Byte;Output:PByte):LongWord; 
function SDIODeviceReadWriteExtended(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Increment:Boolean;Buffer:Pointer;BlockCount,BlockSize:LongWord):LongWord; 

{==============================================================================}
{SDHCI Functions}
function SDHCIHostReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
function SDHCIHostSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
function SDHCIHostSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;

function SDHCIHostTransferPIO(SDHCI:PSDHCIHost):LongWord; 
function SDHCIHostTransferDMA(SDHCI:PSDHCIHost):LongWord; 

function SDHCIHostFinishCommand(SDHCI:PSDHCIHost):LongWord; 
function SDHCIHostFinishData(SDHCI:PSDHCIHost):LongWord; 

function SDHCIHostCommandInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord;var ReturnMask:LongWord):LongWord;
function SDHCIHostDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;

function SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
function SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

function SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; inline;
function SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; inline;
function SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; inline;
procedure SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); inline;
procedure SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); inline;
procedure SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); inline;

function SDHCIHostSetClockDivider(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;
function SDHCIHostSetControlRegister(SDHCI:PSDHCIHost):LongWord;

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
function MMCGetCount:LongWord; inline;

function MMCDeviceCheck(MMC:PMMCDevice):PMMCDevice;

function MMCIsSD(MMC:PMMCDevice):Boolean;

function MMCGetCIDValue(MMC:PMMCDevice;Version,Value:LongWord):LongWord;
function MMCGetCSDValue(MMC:PMMCDevice;Value:LongWord):LongWord;
function MMCGetExtendedCSDValue(MMC:PMMCDevice;Value:LongWord):LongWord;

function MMCExtractBits(Buffer:Pointer;Start,Size:LongWord):LongWord;
function MMCExtractBitsEx(Buffer:Pointer;Length,Start,Size:LongWord):LongWord;

function MMCIsMultiCommand(Command:Word):Boolean;

function MMCStatusToString(Status:LongWord):String;

function MMCDeviceTypeToString(MMCType:LongWord):String;
function MMCDeviceStateToString(MMCState:LongWord):String;

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

{==============================================================================}
{SDIO Helper Functions}

{==============================================================================}
{SDHCI Helper Functions}
function SDHCIGetCount:LongWord; inline;

function SDHCIHostCheck(SDHCI:PSDHCIHost):PSDHCIHost;

function SDHCIIsSPI(SDHCI:PSDHCIHost):Boolean;

function SDHCIGetVersion(SDHCI:PSDHCIHost):Word;

function SDHCIGetCommand(Command:Word):Word;
function SDHCIMakeCommand(Command,Flags:Word):Word;
function SDHCIMakeBlockSize(DMA,BlockSize:Word):Word;

function SDHCIDeviceTypeToString(SDHCIType:LongWord):String;
function SDHCIDeviceStateToString(SDHCIState:LongWord):String;

{==============================================================================}
{MMC Storage Functions}
function MMCStorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
function MMCStorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function MMCStorageDeviceErase(Storage:PStorageDevice;const Start,Count:Int64):LongWord;
function MMCStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
 
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
{var}
 {SDIO specific variables}
 
{==============================================================================}
{==============================================================================}
var
 {SDHCI specific variables}
 SDHCIHostTable:PSDHCIHost;
 SDHCIHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SDHCIHostTableCount:LongWord;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure MMCInit;
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
 
 //To Do //See also: \linux-rpi-3.18.y\drivers\mmc\card\block.c
 
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
 
 //To Do //See also: \linux-rpi-3.18.y\drivers\mmc\card\block.c
 
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
 
 //To Do
 
 //To Do //See also: \linux-rpi-3.18.y\drivers\mmc\card\block.c
 
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
 //     mmc_go_idle in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
 //See: 
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
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Bus Width (Width=' + IntToStr(Width) + ')');
 {$ENDIF}
 
 {Set Bus Width}
 MMC.BusWidth:=Width;
 
 {Set IOS}
 Result:=MMCDeviceSetIOS(MMC);
 
 //See: mmc_set_bus_width in U-Boot mmc.c 
 //See: 
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
 //See: mmc_set_blocklen in \linux-rpi-3.18.y\drivers\mmc\core\core.c
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
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set Block Count (Count=' + IntToStr(Count) + ' Relative=' + BoolToStr(Relative) + ')');
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
 
 //See: mmc_set_blockcount in \linux-rpi-3.18.y\drivers\mmc\core\core.c
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
 
 //See: mmc_set_dsr in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
 
 //See: _mmc_select_card in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
 
 //See: _mmc_select_card in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
end; 

{==============================================================================}

function MMCDeviceSwitch(MMC:PMMCDevice;Setting,Index,Value:Byte):LongWord; 
var
 Status:LongWord;
 Command:TMMCCommand;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;
 
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Switch');
 {$ENDIF}

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=MMC_CMD_SWITCH;
 Command.Argument:=(MMC_SWITCH_MODE_WRITE_BYTE shl 24) or (Index shl 16) or (Value shl 8) or Setting;
 Command.ResponseType:=MMC_RSP_SPI_R1B or MMC_RSP_R1B;
 Command.Data:=nil;
 
 {Send Command}
 Status:=MMCDeviceSendCommand(MMC,@Command);
 if Status <> MMC_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
 
 //To Do 
 
 //Result:=MMC_STATUS_SUCCESS;
 
 //See: mmc_switch in U-Boot mmc.c
 //See: __mmc_switch in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
 //See: __mmc_send_status in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
   if SDHCIIsSPI(SDHCI) then
    begin
     //To Do
    end
   else 
    begin   
     Command.Argument:=MMC.OperationCondition;
     
     //To Do
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
 //See: mmc_send_op_cond in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
   
   //See: mmc_send_cxd_data in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
  end;  
 
 //See: mmc_send_csd in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c 
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
  MMC_CSD_STRUCT_VER_1_1,MMC_CSD_STRUCT_VER_1_2:begin
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
     MMCLogDebug(nil,'  ReadBlockPartial = ' + BoolToStr(MMC.CardSpecificData.ReadBlockPartial));
     MMCLogDebug(nil,'  WriteBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.WriteBlockMisalign));
     MMCLogDebug(nil,'  ReadBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.ReadBlockMisalign));
     MMCLogDebug(nil,'  DSRImplemented = ' + BoolToStr(MMC.CardSpecificData.DSRImplemented));
     MMCLogDebug(nil,'  DeviceSize = ' + IntToStr(MMC.CardSpecificData.DeviceSize));
     MMCLogDebug(nil,'  VDDReadCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMin));
     MMCLogDebug(nil,'  VDDReadCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMax));
     MMCLogDebug(nil,'  VDDWriteCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMin));
     MMCLogDebug(nil,'  VDDWriteCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMax));
     MMCLogDebug(nil,'  DeviceSizeMultiplier = ' + IntToStr(MMC.CardSpecificData.DeviceSizeMultiplier));
     MMCLogDebug(nil,'  EraseGroupSize = ' + IntToStr(MMC.CardSpecificData.Erase.MMC31.EraseGroupSize));
     MMCLogDebug(nil,'  EraseGroupMultiplier = ' + IntToStr(MMC.CardSpecificData.Erase.MMC31.EraseGroupMultiplier));
     MMCLogDebug(nil,'  WriteProtectGroupSize = ' + IntToStr(MMC.CardSpecificData.WriteProtectGroupSize));
     MMCLogDebug(nil,'  WriteProtectGroupEnable = ' + BoolToStr(MMC.CardSpecificData.WriteProtectGroupEnable));
     MMCLogDebug(nil,'  DefaultECC = ' + IntToStr(MMC.CardSpecificData.DefaultECC));
     MMCLogDebug(nil,'  ReadToWriteFactor = ' + IntToStr(MMC.CardSpecificData.ReadToWriteFactor));
     MMCLogDebug(nil,'  WriteBlockLength = ' + IntToStr(MMC.CardSpecificData.WriteBlockLength));
     MMCLogDebug(nil,'  WriteBlockPartial = ' + BoolToStr(MMC.CardSpecificData.WriteBlockPartial));
     MMCLogDebug(nil,'  ContentProtectApplication = ' + BoolToStr(MMC.CardSpecificData.ContentProtectApplication));
     MMCLogDebug(nil,'  FileFormatGroup = ' + IntToStr(MMC.CardSpecificData.FileFormatGroup));
     MMCLogDebug(nil,'  CopyFlag = ' + BoolToStr(MMC.CardSpecificData.CopyFlag));
     MMCLogDebug(nil,'  PermanentWriteProtect = ' + BoolToStr(MMC.CardSpecificData.PermanentWriteProtect));
     MMCLogDebug(nil,'  TemporaryWriteProtect = ' + BoolToStr(MMC.CardSpecificData.TemporaryWriteProtect));
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
    end; 
  end; 
 {$ENDIF}
 
 //See: mmc_decode_csd in \linux-rpi-3.18.y\drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSendCardIdentification(MMC:PMMCDevice):LongWord;
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

   //To Do //This is a Data Command in SPI
   
   //See: mmc_send_cxd_data in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
  end;  
 
 //See: mmc_startup in U-Boot mmc.c
 //See: mmc_send_cid in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c 
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
 //See: mmc_all_send_cid in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c 
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
 
 //See: mmc_decode_cid in \linux-rpi-3.18.y\drivers\mmc\core\mmc.c
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

 //To Do
 
 Result:=MMC_STATUS_SUCCESS;
 
 //See: mmc_get_ext_csd in \linux-rpi-3.18.y\drivers\mmc\core\mmc.c
end;

{==============================================================================}

function MMCDeviceSendExtendedCardSpecific(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Extended Card Specific');
 {$ENDIF}

 //To Do
 
 Result:=MMC_STATUS_SUCCESS;
 
 //See: mmc_send_ext_csd in U-Boot mmc.c
 //See: mmc_send_ext_csd in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
end;

{==============================================================================}

function MMCDeviceDecodeExtendedCardSpecific(MMC:PMMCDevice):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Decode Extended Card Specific');
 {$ENDIF}

 //To Do
 
 //To Do //Set MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_BLOCK_ADDRESSED);
         //based on capacity > 2GB (Sectors > (2 * 1024 * 1024 * 1025) div 512
 
 Result:=MMC_STATUS_SUCCESS;
 
 //See: mmc_read_ext_csd in \linux-rpi-3.18.y\drivers\mmc\core\mmc.c
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
 
 //See: mmc_set_relative_addr in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c 
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
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC SPI Set CRC (Enable=' + BoolToStr(Enable) + ')');
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
 //See: mmc_spi_set_crc in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
 //See: mmc_spi_read_ocr in \linux-rpi-3.18.y\drivers\mmc\core\mmc_ops.c
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
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
   
   {Check Card Detect}
   if (MMC.Device.DeviceFlags and MMC_FLAG_CARD_PRESENT) = 0 then
    begin
     Result:=MMC_STATUS_NO_MEDIA;
     Exit;
    end; 
   
   {Update Device}
   if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_AUTO_CMD23) <> 0 then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_AUTO_BLOCK_COUNT;
   if (SDHCI.Device.DeviceFlags and SDHCI_FLAG_AUTO_CMD12) <> 0 then MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags or MMC_FLAG_AUTO_COMMAND_STOP;
   
   {Set Initial Power}
   Result:=SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI.Voltages) - 1);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;

   {Set Initial Bus Width}
   Result:=MMCDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_1);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
 
   {Set Initial Clock}
   Result:=MMCDeviceSetClock(MMC,MMC_BUS_SPEED_DEFAULT);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
 
   {Perform an SDIO Reset}
   SDIODeviceReset(MMC);
 
   {Set the Card to Idle State}
   Result:=MMCDeviceGoIdle(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
 
   {Get the Interface Condition}
   SDDeviceSendInterfaceCondition(MMC);
 
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
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     //To Do

     {Update Storage}
     MMC.Storage.Device.DeviceBus:=DEVICE_BUS_SD;
     //To Do
     
     Result:=MMC_STATUS_SUCCESS;
     Exit;
    end;
   
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
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end;

     {Get Card Identification}
     Result:=SDDeviceGetCardIdentification(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;      
      
     {Check for SPI}
     if not(SDHCIIsSPI(SDHCI)) then
      begin
       {Get Relative Address}
       Result:=SDDeviceSendRelativeAddress(MMC);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end;  
     
     {Get Card Specific}
     Result:=SDDeviceGetCardSpecific(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Decode Card Identification}
     Result:=SDDeviceDecodeCardIdentification(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
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
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end; 
     
     {Get SD Configuration}
     Result:=SDDeviceSendSDConfiguration(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Decode SD Configuration}
     Result:=SDDeviceDecodeSDConfiguration(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Get SD Status}
     Result:=SDDeviceSendSDStatus(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Decode SD Status}
     Result:=SDDeviceDecodeSDStatus(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Get Switch}
     Result:=SDDeviceSendSDSwitch(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
    
     {Decode Switch}
     Result:=SDDeviceDecodeSDSwitch(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Check for SPI}
     if SDHCIIsSPI(SDHCI) then
      begin
       {Enable SPI CRC} {Located AFTER the reading of the card registers because some SDHC cards are not able to provide valid CRCs for non-512-byte blocks}
       Result:=MMCDeviceSPISetCRC(MMC,True);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end;
     
     {Check Write Protect}
     Result:=MMCDeviceGetWriteProtect(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
  
     //To Do //Check for UHS-I and do UHS-I init
     
     {Switch to High Speed if supported}
     Result:=SDDeviceSwitchHighspeed(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Set Clock}
     Result:=MMCDeviceSetClock(MMC,SDGetMaxClock(MMC));
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Switch to 4 bit bus if supported}
     if ((SDHCI.Capabilities and MMC_MODE_4BIT) <> 0) and ((MMC.SDConfigurationData.BusWidths and SD_SCR_BUS_WIDTH_4) <> 0) then
      begin
       Result:=SDDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_4);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
       Result:=MMCDeviceSetBusWidth(MMC,MMC_BUS_WIDTH_4);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end;
    
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
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end;
     
     {Set the Card to Idle State}
     MMCDeviceGoIdle(MMC);
     
     {Set the High Capacity bit in the Operation Conditions}
     MMC.OperationCondition:=MMC.OperationCondition or MMC_OCR_HCS;
     
     {Get the Operation Condition}
     Result:=MMCDeviceSendOperationCondition(MMC,False);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Check for SPI}
     if SDHCIIsSPI(SDHCI) then
      begin
       {Enable SPI CRC}
       Result:=MMCDeviceSPISetCRC(MMC,True);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
        
       {Get Card Identification}
       Result:=MMCDeviceSendCardIdentification(MMC);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end
     else
      begin
       {Get Card Identification}
       Result:=MMCDeviceSendAllCardIdentification(MMC);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;

       {Set Relative Address}
       Result:=MMCDeviceSetRelativeAddress(MMC);
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end;      
     
     {Get Card Specific}
     Result:=MMCDeviceSendCardSpecific(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Decode Card Specific (Must be before CID)}
     Result:=MMCDeviceDecodeCardSpecific(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
     {Decode Card Identification}
     Result:=MMCDeviceDecodeCardIdentification(MMC);
     if Result <> MMC_STATUS_SUCCESS then
      begin
       Exit;
      end;
     
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
       if Result <> MMC_STATUS_SUCCESS then
        begin
         Exit;
        end;
      end; 
     
     {Get Extended Card Specific}
     Result:=MMCDeviceGetExtendedCardSpecific(MMC);
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
     
     //Setup Byte/Sector(Block) addressing
     
     //Setup Erase Size
     
     //To Do //See: mmc_init_card etc

     //SetClock/SetBusWidth etc
     
     {Update Storage}
     MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
     //To Do
     
     Result:=MMC_STATUS_SUCCESS;
     Exit;
    end;
    
   {Return Result}
   Result:=MMC_STATUS_NO_MEDIA;
  end; 
  
 //See: U-Boot mmc_start_init / mmc_complete_init / mmc_init / mmc_startup in mmc.c
 //     U-Boot sdhci_init in sdhci.c
 
 //See: mmc_init_card in \linux-rpi-3.18.y\drivers\mmc\core\mmc.c
 //     mmc_attach_mmc in \linux-rpi-3.18.y\drivers\mmc\core\mmc.c
 
 //     mmc_sd_init_card in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
 //     mmc_attach_sd in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
 //     mmc_sd_setup_card in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
 
 //     mmc_sdio_init_card in \linux-rpi-3.18.y\drivers\mmc\core\sdio.c
 //     mmc_attach_sdio in \linux-rpi-3.18.y\drivers\mmc\core\sdio.c
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
   MMC.BusWidth:=0;
   MMC.Voltages:=0;
   MMC.Capabilities:=0;
   {Register}
   MMC.InterfaceCondition:=0;
   MMC.OperationCondition:=0;
   MMC.RelativeCardAddress:=0;
   FillChar(MMC.CardSpecific,SizeOf(MMC.CardSpecific),0);
   FillChar(MMC.CardIdentification,SizeOf(MMC.CardIdentification),0);
   MMC.CardStatus:=0;
   MMC.DriverStage:=0;
   FillChar(MMC.SDStatus,SizeOf(MMC.SDStatus),0);
   FillChar(MMC.SDSwitch,SizeOf(MMC.SDSwitch),0);
   FillChar(MMC.SDConfiguration,SizeOf(MMC.SDConfiguration),0);
   {Configuration}
   FillChar(MMC.CardSpecificData,SizeOf(TMMCCardSpecificData),0);
   FillChar(MMC.CardIdentificationData,SizeOf(TMMCCardIdentificationData),0);
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
   
   //To Do //Check MMC_CAP_NONREMOVABLE and SDHCI_QUIRK_BROKEN_CARD_DETECTION //See: sdhci_do_get_cd in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c

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
 Address:LongWord;
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
      try
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
         {Setup Transfer Mode}
         Mode:=SDHCIHostReadWord(SDHCI,SDHCI_TRANSFER_MODE);
         
         {Clear Auto CMD settings for non data CMDs}
         Mode:=Mode and not(SDHCI_TRNS_AUTO_CMD12 or SDHCI_TRNS_AUTO_CMD23);
         
         {Clear Block Count, Multi, Read and DMA for non data CMDs}
         Mode:=Mode and not(SDHCI_TRNS_BLK_CNT_EN or SDHCI_TRNS_MULTI or SDHCI_TRNS_READ or SDHCI_TRNS_DMA); 
         
         {Write Argument}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command.Argument,8) + ')');
         {$ENDIF}
         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command.Argument);
         
         {Write Transfer Mode}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(Mode,8) + ')');
         {$ENDIF}
         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,Mode);
        end
       else 
        begin
         {Setup Data}
         Command.Data.BlockOffset:=0;
         Command.Data.BlocksRemaining:=Command.Data.BlockCount;
         Command.Data.BytesTransfered:=0;
         
         {Setup Transfer Mode}
         Mode:=SDHCI_TRNS_BLK_CNT_EN;
         if MMCIsMultiCommand(Command.Command) or (Command.Data.BlockCount > 1) then
          begin
           Mode:=Mode or SDHCI_TRNS_MULTI;
           
           Mode:=Mode or SDHCI_TRNS_AUTO_CMD12; //To Do //Testing (This works, need to sort out properly where it fits, plus SDHCI_TRNS_AUTO_CMD23)
           
           //To Do //SDHCI_TRNS_AUTO_CMD12 //SDHCI_TRNS_AUTO_CMD23 //SDHCI_ARGUMENT2 //See: sdhci_set_transfer_mode
                   //See 1.15 Block Count in the SD Host Controller Simplified Specifications
          end;
         if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
          begin
           Mode:=Mode or SDHCI_TRNS_READ;
          end;
         
         {Setup DMA Address}
         //mode |= SDHCI_TRNS_DMA;
         //Address:=
         //To Do
               
         {Setup Interrupts}
         SDHCI.Interrupts:=SDHCI.Interrupts or (SDHCI_INT_DATA_AVAIL or SDHCI_INT_SPACE_AVAIL);
         SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
         SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts); 
         //To Do //Different for DMA //Should we disable these again after the command ? //Yes, probably
         
         {Write DMA Address}
         //To Do
         //SDHCIHostWriteLong(SDHCI,SDHCI_DMA_ADDRESS,Address);
        
         {Write Block Size}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_BLOCK_SIZE (Value=' + IntToStr(Command.Data.BlockSize) + ')');
         {$ENDIF}
         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_SIZE,SDHCIMakeBlockSize(SDHCI_DEFAULT_BOUNDARY_ARG,Command.Data.BlockSize));
      
         {Write Block Count}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_BLOCK_COUNT (Value=' + IntToStr(Command.Data.BlockCount) + ')');
         {$ENDIF}
         SDHCIHostWriteWord(SDHCI,SDHCI_BLOCK_COUNT,Command.Data.BlockCount);
      
         {Write Argument}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_ARGUMENT (Value=' + IntToHex(Command.Argument,8) + ')');
         {$ENDIF}
         SDHCIHostWriteLong(SDHCI,SDHCI_ARGUMENT,Command.Argument);
      
         {Write Transfer Mode}
         {$IFDEF MMC_DEBUG}
         if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_TRANSFER_MODE (Value=' + IntToHex(Mode,8) + ')');
         {$ENDIF}
         SDHCIHostWriteWord(SDHCI,SDHCI_TRANSFER_MODE,Mode);
        end;     
      
       {Setup Command}
       SDHCI.Command:=Command;
       try
        {Write Command}
        {$IFDEF MMC_DEBUG}
        if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Send Command SDHCI_COMMAND (Value=' + IntToHex(SDHCIMakeCommand(Command.Command,Flags),8) + ')');
        {$ENDIF}
        SDHCIHostWriteWord(SDHCI,SDHCI_COMMAND,SDHCIMakeCommand(Command.Command,Flags));
      
        {Wait for Completion}
        if Command.Data = nil then
         begin
          {Wait for Signal with Timeout (100ms)}
          Status:=SemaphoreWaitEx(SDHCI.Wait,100);
          if Status <> ERROR_SUCCESS then
           begin
            if Status = ERROR_WAIT_TIMEOUT then
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Command Response Timeout');
              Command.Status:=MMC_STATUS_TIMEOUT;
              Exit;
             end
            else
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Command Response Failure');
              Command.Status:=MMC_STATUS_HARDWARE_ERROR;
              Exit;
             end;          
           end;
         end
        else
         begin
          {Wait for Signal with Timeout (5000ms)}
          Status:=SemaphoreWaitEx(SDHCI.Wait,5000);
          if Status <> ERROR_SUCCESS then
           begin
            if Status = ERROR_WAIT_TIMEOUT then
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Data Response Timeout');
              Command.Status:=MMC_STATUS_TIMEOUT;
              Exit;
             end
            else
             begin
              if MMC_LOG_ENABLED then MMCLogError(nil,'MMC Send Data Response Failure');
              Command.Status:=MMC_STATUS_HARDWARE_ERROR;
              Exit;
             end;          
           end;
         end;
       finally
        {Reset Command}
        SDHCI.Command:=nil; 
       end;
      finally
       {Check Status}
       if Command.Status <> MMC_STATUS_SUCCESS then //To Do //More see: sdhci_tasklet_finish //SDHCI_QUIRK_RESET_AFTER_REQUEST and SDHCI_QUIRK_CLOCK_BEFORE_RESET
        begin
         SDHCIHostReset(SDHCI,SDHCI_RESET_CMD);
         SDHCIHostReset(SDHCI,SDHCI_RESET_DATA);
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
 //See: bcm2835_mmc_send_command in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_send_command in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
end;

{==============================================================================}

function MMCDeviceSetIOS(MMC:PMMCDevice):LongWord;
var
 Value:Byte;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;
 
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC Set IOS');
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
   
   {Set Control Register}
   SDHCIHostSetControlRegister(SDHCI);
   
   {Check Clock}
   if MMC.Clock <> SDHCI.Clock then
    begin
     SDHCIHostSetClock(SDHCI,MMC.Clock);
     SDHCI.Clock:=MMC.Clock;
    end;
   
   {Set Power}   
   SDHCIHostSetPower(SDHCI,FirstBitSet(SDHCI.Voltages) - 1);
            
   {Set Bus Width}  
   Value:=SDHCIHostReadByte(SDHCI,SDHCI_HOST_CONTROL);
   if MMC.BusWidth = MMC_BUS_WIDTH_8 then
    begin
     Value:=Value and not(SDHCI_CTRL_4BITBUS);
     if (SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300) or ((SDHCI.Quirks2 and SDHCI_QUIRK2_USE_WIDE8) <> 0) then
      begin
       Value:=Value or SDHCI_CTRL_8BITBUS;
      end;
    end
   else
    begin
     if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
      begin
       Value:=Value and not(SDHCI_CTRL_8BITBUS);
      end;
     if MMC.BusWidth = MMC_BUS_WIDTH_4 then 
      begin
       Value:=Value or SDHCI_CTRL_4BITBUS;
      end
     else
      begin
       Value:=Value and not(SDHCI_CTRL_4BITBUS);
      end;      
    end;
    
   {Check Clock}
   if MMC.Clock > 26000000 then
    begin
     Value:=Value or SDHCI_CTRL_HISPD;
    end
   else
    begin
     Value:=Value and not(SDHCI_CTRL_HISPD);
    end;
   if (SDHCI.Quirks and SDHCI_QUIRK_NO_HISPD_BIT) <> 0 then
    begin
     Value:=Value and not(SDHCI_CTRL_HISPD);
    end;
   //To Do //More here (Reset SD Clock Enable / Re-enable SD Clock) //See: bcm2835_mmc_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
                 //Even more quirks                                       //See: sdhci_do_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
   SDHCIHostWriteByte(SDHCI,SDHCI_HOST_CONTROL,Value);
   
   Result:=MMC_STATUS_SUCCESS;
  end;  
  
 //See: mmc_set_ios in mmc.c 
 //     sdhci_set_ios in sdhci.c 
 //See: bcm2835_mmc_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_do_set_ios in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
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
 //See: mmc_sd_switch in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
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
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;
 
 {Check Command Class}
 if (MMC.CardSpecificData.CCC and MMC_CCC_SWITCH) = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SD Switch Highspeed card does not support Switch Class');
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;
 
 {Check Host Capability}
 if (SDHCI.Capabilities and MMC_MODE_HS) = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SD Switch Highspeed host does not support Highspeed');
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;
  
 {Check Card Support}
 if (MMC.SDSwitchData.Group1Support and SD_SWITCH_GROUP1_HS) = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'SD Switch Highspeed card does not support Highspeed');
   Result:=MMC_STATUS_SUCCESS;
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
 
 //See: mmc_sd_switch_hs in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
end;

{==============================================================================}

function SDDeviceSetBusSpeed(MMC:PMMCDevice;Speed:LongWord):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;
 
 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SD Set Bus Speed (Speed=' + IntToStr(Speed) + ')');
 {$ENDIF}
 
 //To Do //UHS-I and UHS-II Bus Speeds
 
 //See: sd_set_bus_speed_mode in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
 
 //See: mmc_app_set_bus_width in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
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
 //See: mmc_send_if_cond in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
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
   if SDHCIisSPI(SDHCI) then
    begin
     //To Do
     
     //What happens here, just the SD_OCR_CCS bit, is that all ?
     
    end
   else 
    begin
     Command.Argument:=MMC.OperationCondition;
     
     //To Do //Is this correct ? Do we need to limit voltages etc ?
                           //Do we need to use SDHCI.Voltages as a mask ?
                           
     
     {Set the Voltage bits in the OCR if not doing a probe operation}
     //Command.Argument:=(SDHCI.Voltages and SD_SEND_OP_COND_VOLTAGE_MASK);
     
     {Set the High Capacity flag in the OCR if version 2}
     //To Do //Should check for successful SEND IF to confirm //See:
     //if MMC.Version = SD_VERSION_2 then Command.Argument:=Command.Argument or SD_OCR_CCS;
     //No //Done by SDDeviceGetCardIdentification
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
 //See: mmc_send_app_op_cond in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
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
 
 //See: mmc_sd_get_csd in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
     MMCLogDebug(nil,'  ReadBlockPartial = ' + BoolToStr(MMC.CardSpecificData.ReadBlockPartial));
     MMCLogDebug(nil,'  WriteBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.WriteBlockMisalign));
     MMCLogDebug(nil,'  ReadBlockMisalign = ' + BoolToStr(MMC.CardSpecificData.ReadBlockMisalign));
     MMCLogDebug(nil,'  DSRImplemented = ' + BoolToStr(MMC.CardSpecificData.DSRImplemented));
     MMCLogDebug(nil,'  DeviceSize = ' + IntToStr(MMC.CardSpecificData.DeviceSize));
     MMCLogDebug(nil,'  VDDReadCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMin));
     MMCLogDebug(nil,'  VDDReadCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDReadCurrentMax));
     MMCLogDebug(nil,'  VDDWriteCurrentMin = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMin));
     MMCLogDebug(nil,'  VDDWriteCurrentMax = ' + IntToStr(MMC.CardSpecificData.VDDWriteCurrentMax));
     MMCLogDebug(nil,'  DeviceSizeMultiplier = ' + IntToStr(MMC.CardSpecificData.DeviceSizeMultiplier));
     MMCLogDebug(nil,'  EraseBlockEnable = ' + BoolToStr(MMC.CardSpecificData.Erase.SD.EraseBlockEnable));
     MMCLogDebug(nil,'  EraseSectorSize = ' + IntToStr(MMC.CardSpecificData.Erase.SD.SectorSize));
     MMCLogDebug(nil,'  WriteProtectGroupSize = ' + IntToStr(MMC.CardSpecificData.WriteProtectGroupSize));
     MMCLogDebug(nil,'  WriteProtectGroupEnable = ' + BoolToStr(MMC.CardSpecificData.WriteProtectGroupEnable));
     MMCLogDebug(nil,'  ReadToWriteFactor = ' + IntToStr(MMC.CardSpecificData.ReadToWriteFactor));
     MMCLogDebug(nil,'  WriteBlockLength = ' + IntToStr(MMC.CardSpecificData.WriteBlockLength));
     MMCLogDebug(nil,'  WriteBlockPartial = ' + BoolToStr(MMC.CardSpecificData.WriteBlockPartial));
     MMCLogDebug(nil,'  FileFormatGroup = ' + IntToStr(MMC.CardSpecificData.FileFormatGroup));
     MMCLogDebug(nil,'  CopyFlag = ' + BoolToStr(MMC.CardSpecificData.CopyFlag));
     MMCLogDebug(nil,'  PermanentWriteProtect = ' + BoolToStr(MMC.CardSpecificData.PermanentWriteProtect));
     MMCLogDebug(nil,'  TemporaryWriteProtect = ' + BoolToStr(MMC.CardSpecificData.TemporaryWriteProtect));
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
 
 //See: mmc_decode_csd in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
 //To Do
  {If the host supports one of UHS-I modes, request the card to switch to 1.8V signaling level. If the card has failed repeatedly to switch however, skip this}
  //MMC.OperationCondition:=MMC.OperationCondition or SD_OCR_S18A;
  
 {Check for Host Max Current}
 //To Do
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
     //To Do
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
 
 //See: mmc_sd_get_cid in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
 
 //See: mmc_decode_cid in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
 
 //See: mmc_read_ssr in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
   MMCLogDebug(nil,'  SecuredMode = ' + BoolToStr(MMC.SDStatusData.SecuredMode));
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
 
 //See: mmc_read_ssr in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
 
 //See: mmc_read_switch in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
 Data.Data:=@MMC.SDConfiguration;
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
 MMC.SDConfiguration[0]:=LongWordBEtoN(MMC.SDConfiguration[0]);
 MMC.SDConfiguration[1]:=LongWordBEtoN(MMC.SDConfiguration[1]);
 
 Result:=MMC_STATUS_SUCCESS;
 
 //See: mmc_app_send_scr in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
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
    MMC.SDConfigurationData.SpecVersion:=SDGetSCRValue(MMC,SD_SCR_SD_SPEC);  //To Do //In testing this shows as 5 and should be 0,1 or 2 only ?
    MMC.SDConfigurationData.DataAfterErase:=SDGetSCRValue(MMC,SD_SCR_DATA_STAT_AFTER_ERASE);
    MMC.SDConfigurationData.Security:=SDGetSCRValue(MMC,SD_SCR_SD_SECURITY);
    MMC.SDConfigurationData.BusWidths:=SDGetSCRValue(MMC,SD_SCR_SD_BUS_WIDTHS);
    MMC.SDConfigurationData.SpecVersion3:=(SDGetSCRValue(MMC,SD_SCR_SD_SPEC3) = 1);
    MMC.SDConfigurationData.ExtendedSecurity:=SDGetSCRValue(MMC,SD_SCR_EX_SECURITY);
    MMC.SDConfigurationData.SpecVersion4:=(SDGetSCRValue(MMC,SD_SCR_SD_SPEC4) = 1);
    MMC.SDConfigurationData.CommandSupport:=SDGetSCRValue(MMC,SD_SCR_CMD_SUPPORT);
 
    {Calculate Erase Byte}
    if MMC.SDConfigurationData.DataAfterErase = 1 then
     begin
      MMC.SDConfigurationData.ErasedByte:=$FF;
     end
    else
     begin
      MMC.SDConfigurationData.ErasedByte:=$00;
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
     MMCLogDebug(nil,'  SpecVersion3 = ' + BoolToStr(MMC.SDConfigurationData.SpecVersion3));
     MMCLogDebug(nil,'  ExtendedSecurity = ' + IntToHex(MMC.SDConfigurationData.ExtendedSecurity,8));
     MMCLogDebug(nil,'  SpecVersion4 = ' + BoolToStr(MMC.SDConfigurationData.SpecVersion4));
     MMCLogDebug(nil,'  CommandSupport = ' + IntToHex(MMC.SDConfigurationData.CommandSupport,8));
     {Calculated Values}
     MMCLogDebug(nil,'  ErasedByte = ' + IntToHex(MMC.SDConfigurationData.ErasedByte,2));
    end; 
  end; 
 {$ENDIF}
 
 //See: mmc_decode_scr in \linux-rpi-3.18.y\drivers\mmc\core\sd.c
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
 
 //See: mmc_send_relative_addr in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
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
 
 //See: mmc_wait_for_app_cmd in \linux-rpi-3.18.y\drivers\mmc\core\sd_ops.c
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
 
 //See: sdio_reset in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
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
   if SDHCIIsSPI(SDHCI) then
    begin
     //To Do
    end
   else 
    begin   
     Command.Argument:=MMC.OperationCondition;
     
     //To Do
    end; 
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
 
 //See: mmc_send_io_op_cond in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
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
        
 {Check Operation}
 if Operation > 7 then Exit;
 
 {Check Address}
 if (Address and not($0001FFFF)) <> 0 then Exit;

 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 Command.Command:=SDIO_CMD_RW_DIRECT;
 Command.Argument:=0;
 Command.ResponseType:=MMC_RSP_R5;
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
   if (Command.Response[0] and SDIO_RSP_R5_ERROR) <> 0 then Exit;
   if (Command.Response[0] and SDIO_RSP_R5_FUNCTION_NUMBER) <> 0 then Exit;
   if (Command.Response[0] and SDIO_RSP_R5_OUT_OF_RANGE) <> 0 then Exit;
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
 
 //See: mmc_io_rw_direct_host in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
end;

{==============================================================================}

function SDIODeviceReadWriteExtended(MMC:PMMCDevice;Write:Boolean;Operation,Address:LongWord;Increment:Boolean;Buffer:Pointer;BlockCount,BlockSize:LongWord):LongWord; 
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
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDIO Read Write Extended');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check Block Size}
 if  BlockSize = 0 then Exit;
 
 {Check Operation}
 if Operation > 7 then Exit;
 
 {Check Address}
 if (Address and not($0001FFFF)) <> 0 then Exit;
         
 {Setup Command}
 FillChar(Command,SizeOf(TMMCCommand),0);
 //To Do

 //Result:=MMC_STATUS_SUCCESS;
 
 //See: mmc_io_rw_extended in \linux-rpi-3.18.y\drivers\mmc\core\sdio_ops.c
 //
end;
 
{==============================================================================}
{==============================================================================}
{SDHCI Functions}
function SDHCIHostReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
{Reference: Section ?.? of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
var 
 Timeout:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IFDEF MMC_DEBUG}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Reset (Mask=' + IntToHex(Mask,2) + ')');
 {$ENDIF}
 
 {Setup Timeout (100ms)}
 Timeout:=100;
 
 {Send Reset}
 SDHCIHostWriteByte(SDHCI,SDHCI_SOFTWARE_RESET,Mask);

 {Reset Clock}
 if (Mask and SDHCI_RESET_ALL) <> 0 then
  begin
   SDHCI.Clock:=0;
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
 
 //See: sdhci_reset in sdhci.c
end;

{==============================================================================}

function SDHCIHostSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
{Reference: Section 3.3 of SD Host Controller Simplified Specification V3.0 partA2_300.pdf}
//To Do //Caller needs to pass result of fls (Find Last Set) as Power
        //FirstBitSet in Platform should provide the correct behaviour ? (- 1 to produce $FFFF on nothing ?)
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
 
 Value:=0;
 if Power <> $FFFF then
  begin
   case (1 shl Power) of
    MMC_VDD_165_195:Value:=SDHCI_POWER_180;
    MMC_VDD_29_30,MMC_VDD_30_31:Value:=SDHCI_POWER_300;
    MMC_VDD_32_33,MMC_VDD_33_34:Value:=SDHCI_POWER_330;
   end;
  end;
 
 if Value = 0 then
  begin
   {Power Off}
   SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,0);
  end
 else
  begin
   {Power On}
   if (SDHCI.Quirks and SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER) <> 0 then
    begin
     SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,Value);
    end;
   Value:=Value or SDHCI_POWER_ON;
   SDHCIHostWriteByte(SDHCI,SDHCI_POWER_CONTROL,Value);
   
   //To Do //More quirks, see: sdhci_set_power in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
  end;
 
 Result:=MMC_STATUS_SUCCESS;
 
 //See: sdhci_set_power in sdhci.c
end;

{==============================================================================}

function SDHCIHostSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;
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
 SDHCIHostSetClockDivider(SDHCI,0,Divider);  //To Do Index ? //What is this function for ?
 
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
 
 //See: sdhci_set_clock in sdhci.c
 //See also: \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostTransferPIO(SDHCI:PSDHCIHost):LongWord; 
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
       {Read Chunk} //To Do //Rework to read in LongWord blocks with LongWordBEToN etc ? (What about devices that read in Bytes ? SPI ? - Use MMCSPI instead)
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
       
       {Write Chunk} //To Do //Rework to write in LongWord blocks with LongWordNToBE etc ? (What about devices that write in Bytes ? SPI ? - Use MMCSPI instead)
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
   if SDHCI.Command.Data.BlocksRemaining = 0 then Break;
  end;
 
 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Transfer PIO completed (BlocksRemaining=' + IntToStr(SDHCI.Command.Data.BlocksRemaining) + ')'); 
 {$ENDIF}
 
 Result:=MMC_STATUS_SUCCESS; 
 
 //See: sdhci_transfer_pio in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_transfer_pio in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
 //     sdhci_read_block_pio / sdhci_write_block_pio
end;
 
{==============================================================================}

function SDHCIHostTransferDMA(SDHCI:PSDHCIHost):LongWord; 
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
 
 {Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Transfer DMA when no current data');
   {$ENDIF}
   Exit; 
  end; 
 
 //To Do
 
  Result:=MMC_STATUS_SUCCESS; 
  
 //See: bcm2835_mmc_transfer_dma in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostFinishCommand(SDHCI:PSDHCIHost):LongWord; 
{Called by Interrupt Command handler when an SDHCI_INT_RESPONSE is received}
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
 //To Do
 
 {Finished CMD23, now send actual command} 
 //To Do

 
 if SDHCI.Command.DataCompleted then
  begin
   SDHCIHostFinishData(SDHCI);
  end;
  
 if SDHCI.Command.Data = nil then
  begin
   SDHCI.Command.Status:=MMC_STATUS_SUCCESS;
   SemaphoreSignal(SDHCI.Wait);
  end; 
 
 Result:=MMC_STATUS_SUCCESS; 
 
 //See: sdhci_finish_command in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_finish_command in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostFinishData(SDHCI:PSDHCIHost):LongWord; 
{Called by Interrupt Data handler when data is received}
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
 //To Do
 
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
   //To Do //Check for No CMD23/Check for Stop needed/Check for Error
                         //If Error do reset (CMD and DATA) before SendCommand
                         
   Exit;                      
  end;                        
 
 SDHCI.Command.DataCompleted:=True;

 if SDHCI.Command.CommandCompleted then
  begin
   SDHCI.Command.Status:=MMC_STATUS_SUCCESS;
   SemaphoreSignal(SDHCI.Wait);       
  end; 
 
 Result:=MMC_STATUS_SUCCESS; 
 
 //See: sdhci_finish_data in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_finish_data in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostCommandInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord;var ReturnMask:LongWord):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(MMC_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Command Interrupt');
 {$ENDIF}
 
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
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;
 
 {Check Invalid Sequence}
 if (InterruptMask and (SDHCI_INT_CRC or SDHCI_INT_END_BIT or SDHCI_INT_INDEX)) <> 0 then 
  begin
   SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
   SemaphoreSignal(SDHCI.Wait);
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
 
 //See: sdhci_cmd_irq in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_cmd_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function SDHCIHostDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;
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
       SemaphoreSignal(SDHCI.Wait);
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
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;
  
 {Check Invalid Sequence} 
 if (InterruptMask and SDHCI_INT_DATA_END_BIT) <> 0 then
  begin
   SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
   SDHCIHostFinishData(SDHCI);
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end;
 if ((InterruptMask and SDHCI_INT_DATA_CRC) <> 0) and (SDHCIGetCommand(SDHCIHostReadWord(SDHCI,SDHCI_COMMAND)) <> MMC_CMD_BUS_TEST_R) then
  begin
   SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
   SDHCIHostFinishData(SDHCI);
   SemaphoreSignal(SDHCI.Wait);
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
   //SDHCIHostTransferDMA(SDHCI);
   //To Do //Setup next DMA boundary address
   //StartAddress/CurrentAddress/BytesTransfered
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
 
 //See: sdhci_data_irq in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
 //     bcm2835_mmc_data_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c 
end;

{==============================================================================}

function SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
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
  
   {Get Capabilities}
   Capabilities:=SDHCIHostReadLong(SDHCI,SDHCI_CAPABILITIES);
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Capabilities = ' + IntToHex(Capabilities,8));
   {$ENDIF}
                           
   {Check DMA Support}
   if ((Capabilities and SDHCI_CAN_DO_SDMA) = 0) and ((SDHCI.Quirks and SDHCI_QUIRK_MISSING_CAPS) = 0) then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI Host does not support SDMA');
     SDHCI.HostStop(SDHCI);
     Exit;
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
   SDHCI.Capabilities:=MMC_MODE_HS or MMC_MODE_HS_52MHz or MMC_MODE_4BIT;
   if SDHCIGetVersion(SDHCI) >= SDHCI_SPEC_300 then
    begin
     if (Capabilities and SDHCI_CAN_DO_8BIT) <> 0 then
      begin
       SDHCI.Capabilities:=SDHCI.Capabilities or MMC_MODE_8BIT;
      end;
    end;
   {Check Presets}
   if SDHCI.PresetCapabilities <> 0 then
    begin
     SDHCI.Capabilities:=SDHCI.Capabilities or SDHCI.PresetCapabilities;
    end;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host capabilities = ' + IntToHex(SDHCI.Capabilities,8));
   {$ENDIF}
   
   {Determine Maximum Blocks}
   SDHCI.MaximumBlockCount:=MMC_MAX_BLOCK_COUNT;
   {$IFDEF MMC_DEBUG}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI Host maximum blocks = ' + IntToStr(SDHCI.MaximumBlockCount));
   {$ENDIF}
   
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
   
   Result:=ERROR_SUCCESS;
  end;  
 
 //See: add_sdhci in sdhci.c
end;

{==============================================================================}

function SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
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
   //To Do //
   
   {Call Host Stop}
   if SDHCI.HostStop(SDHCI) <> ERROR_SUCCESS then Exit;
   
   {Disable Host}
   SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
  
   {Notify Disable}
   NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_DISABLE);
   
   //To Do //Eject/Deregister/Destroy (MMC/Storage) etc //See: Storage/Mouse etc
   
   Result:=ERROR_SUCCESS;
  end; 
end;

{==============================================================================}

function SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; inline;
begin
 {}
 if Assigned(SDHCI.HostReadByte) then
  begin
   Result:=SDHCI.HostReadByte(SDHCI,Reg);
  end
 else
  begin
   Result:=PByte(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;  
end;

{==============================================================================}

function SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; inline;
begin
 {}
 if Assigned(SDHCI.HostReadWord) then
  begin
   Result:=SDHCI.HostReadWord(SDHCI,Reg);
  end
 else
  begin
   Result:=PWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;  
end;

{==============================================================================}

function SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; inline;
begin
 {}
 if Assigned(SDHCI.HostReadLong) then
  begin
   Result:=SDHCI.HostReadLong(SDHCI,Reg);
  end
 else
  begin
   Result:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
  end;  
end;

{==============================================================================}

procedure SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); inline;
begin
 {}
 if Assigned(SDHCI.HostWriteByte) then
  begin
   SDHCI.HostWriteByte(SDHCI,Reg,Value);
  end
 else
  begin
   PByte(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;  
end;

{==============================================================================}

procedure SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); inline;
begin
 {}
 if Assigned(SDHCI.HostWriteWord) then
  begin
   SDHCI.HostWriteWord(SDHCI,Reg,Value);
  end
 else
  begin
   PWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;  
end;

{==============================================================================}

procedure SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); inline;
begin
 {}
 if Assigned(SDHCI.HostWriteLong) then
  begin
   SDHCI.HostWriteLong(SDHCI,Reg,Value);
  end
 else
  begin
   PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
  end;  
end;

{==============================================================================}

function SDHCIHostSetClockDivider(SDHCI:PSDHCIHost;Index:Integer;Divider:LongWord):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 if Assigned(SDHCI.HostSetClockDivider) then
  begin
   Result:=SDHCI.HostSetClockDivider(SDHCI,Index,Divider);
  end
 else
  begin
   Result:=MMC_STATUS_SUCCESS;
  end;  
  
 //See: sdhci_host->set_clock in sdhci.h
end;

{==============================================================================}

function SDHCIHostSetControlRegister(SDHCI:PSDHCIHost):LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 if Assigned(SDHCI.HostSetControlRegister) then
  begin
   Result:=SDHCI.HostSetControlRegister(SDHCI);
  end
 else
  begin
   Result:=MMC_STATUS_SUCCESS;
  end;  
  
 //See: sdhci_host->set_control_reg in sdhci.h
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
 Result.HostReadByte:=nil;
 Result.HostReadWord:=nil;
 Result.HostReadLong:=nil;
 Result.HostWriteByte:=nil;
 Result.HostWriteWord:=nil;
 Result.HostWriteLong:=nil;
 Result.HostSetClockDivider:=nil;
 Result.HostSetControlRegister:=nil;
 Result.DeviceInitialize:=nil;
 Result.DeviceDeinitialize:=nil;
 Result.DeviceGetCardDetect:=nil;
 Result.DeviceGetWriteProtect:=nil;
 Result.DeviceSendCommand:=nil;
 Result.DeviceSetIOS:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Wait:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
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
 
 {Destroy Lock}
 if SDHCI.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(SDHCI.Lock);
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
    //To Do //Stop host if started, see USB
    
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
function MMCGetCount:LongWord; inline;
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

function MMCGetExtendedCSDValue(MMC:PMMCDevice;Value:LongWord):LongWord;
{Extract an Extended CSD field value from the 512 byte Extended Card Specific register}
begin
 {}
 Result:=0;

 {Check MMC}
 if MMC = nil then Exit;

 //To Do 
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

 //Result:=PLongWord(PtrUInt(Buffer) + PtrUInt(Offset * SizeOf(LongWord)))^ shr Shift;  //To Do //Remove //Should the multiply be a shift ? //Yes
 Result:=PLongWord(PtrUInt(Buffer) + PtrUInt(Offset shl 2))^ shr Shift;
 if (Size + Shift) > 32 then
  begin
   //Result:=Result or (PLongWord(PtrUInt(Buffer) + PtrUInt((Offset - 1) * SizeOf(LongWord)))^ shl ((32 - Shift) mod (Length - 1)));  //To Do //Remove //Should the multiply be a shift ? //Yes
   Result:=Result or (PLongWord(PtrUInt(Buffer) + PtrUInt((Offset - 1) shl 2))^ shl ((32 - Shift) mod (Length - 1))); 
  end;

 Result:=Result and Mask;
end;

{==============================================================================}

function MMCIsMultiCommand(Command:Word):Boolean;
begin
 {}
 Result:=(Command = MMC_CMD_READ_MULTIPLE_BLOCK) or (Command = MMC_CMD_WRITE_MULTIPLE_BLOCK);
end;

{==============================================================================}

function MMCStatusToString(Status:LongWord):String;
{Translates an MMC status code into a string describing it}
begin
 {}
 Result:='unknown error';
 
 case Status of
  MMC_STATUS_SUCCESS:Result:='success';
  MMC_STATUS_TIMEOUT:Result:='request timed out';
  MMC_STATUS_NO_MEDIA:Result:='no media present';
  MMC_STATUS_HARDWARE_ERROR:Result:='hardware error';
  MMC_STATUS_INVALID_DATA:Result:='invalid data';
  MMC_STATUS_INVALID_PARAMETER:Result:='invalid parameter';
  MMC_STATUS_INVALID_SEQUENCE:Result:='invalid sequence';
  MMC_STATUS_OUT_OF_MEMORY:Result:='out of memory';
  MMC_STATUS_UNSUPPORTED_REQUEST:Result:='unsupported request';
  MMC_STATUS_NOT_PROCESSED:Result:='request not processed yet';
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
{==============================================================================}
{SDIO Helper Functions}
 
{==============================================================================}
{==============================================================================}
{SDHCI Helper Functions}
function SDHCIGetCount:LongWord; inline;
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

 Result:=(SDHCI.Capabilities and MMC_MODE_SPI) <> 0; 
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
 Result:=((DMA and $07) shl 12)  or (BlockSize and $0FFF);
end;

{==============================================================================}

function SDHCIDeviceTypeToString(SDHCIType:LongWord):String;
begin
 {}
 Result:='SDHCI_TYPE_UNKNOWN';
 
 if SDHCIType <= SDHCI_TYPE_MAX then
  begin
   Result:=SDHCI_TYPE_NAMES[SDHCIType];
  end;
end;

{==============================================================================}

function SDHCIDeviceStateToString(SDHCIState:LongWord):String;
 begin
 {}
 Result:='SDHCI_STATE_UNKNOWN';
 
 if SDHCIState <= SDHCI_STATE_MAX then
  begin
   Result:=SDHCI_STATE_NAMES[SDHCIState];
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
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;;  
       
       {Start Read}
       ReadOffset:=0;
       ReadRemain:=Count;
       while ReadRemain > 0 do
        begin
         {Get Count}
         BlockCount:=ReadRemain;
         if ReadRemain > MMC_MAX_BLOCK_COUNT then BlockCount:=MMC_MAX_BLOCK_COUNT; //To do //This should use SDHCI.MaximumBlockCount ?
         
         {Set Block Count}
         if (BlockCount > 1) and ((MMC.Device.DeviceFlags and MMC_FLAG_AUTO_BLOCK_COUNT) = 0) then 
          begin
           Status:=MMCDeviceSetBlockCount(MMC,BlockCount,False);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end; 
          
         {Read Blocks}
         Status:=MMCDeviceReadBlocks(MMC,(Start + ReadOffset),BlockCount,Pointer(PtrUInt(Buffer) + (ReadOffset shl Storage.BlockShift))); 
         if Status <> MMC_STATUS_SUCCESS then
          begin
           {Stop Transmission}
           MMCDeviceStopTransmission(MMC);
           
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
         
         {Stop Transmission}
         if (BlockCount > 1) and ((MMC.Device.DeviceFlags and MMC_FLAG_AUTO_COMMAND_STOP) = 0) then
          begin
           Status:=MMCDeviceStopTransmission(MMC);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end; 
         
         Inc(ReadOffset,BlockCount);
         Dec(ReadRemain,BlockCount);
        end;
            
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
              
         {Set Block Count}
         if (BlockCount > 1) and ((MMC.Device.DeviceFlags and MMC_FLAG_AUTO_BLOCK_COUNT) = 0) then 
          begin
           Status:=MMCDeviceSetBlockCount(MMC,BlockCount,False);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end; 
              
         {Write Blocks}
         Status:=MMCDeviceWriteBlocks(MMC,(Start + WriteOffset),BlockCount,Pointer(PtrUInt(Buffer) + (WriteOffset shl Storage.BlockShift))); 
         if Status <> MMC_STATUS_SUCCESS then
          begin
           {Stop Transmission}
           MMCDeviceStopTransmission(MMC);
           
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
           
         {Stop Transmission}
         if (BlockCount > 1) and ((MMC.Device.DeviceFlags and MMC_FLAG_AUTO_COMMAND_STOP) = 0) then
          begin
           Status:=MMCDeviceStopTransmission(MMC);
           if Status <> MMC_STATUS_SUCCESS then
            begin
             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
          end; 
           
         Inc(WriteOffset,BlockCount);
         Dec(WriteRemain,BlockCount);
        end;
            
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

function MMCStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
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
     WorkerSchedule(0,TWorkerTask(MMCAsyncStart),nil,nil);
    end;
  end; 
 
{==============================================================================}
 
finalization
 MMCStop;

{==============================================================================}
{==============================================================================}

end.
 