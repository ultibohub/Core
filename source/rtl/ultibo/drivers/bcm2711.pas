{
Ultibo BCM2711 interface unit.

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

   Linux - \drivers\char\hw_random\iproc-rng200.c
   
   Linux - \drivers\mmc\host\bcm2835-mmc.c (EMMC0) - Copyright 2014 Gellert Weisz
   Linux - \drivers\mmc\host\sdhci-iproc.c (EMMC0 and EMMC2) - Copyright (C) 2014 Broadcom Corporation
   
   Linux - \drivers\mmc\host\bcm2835-sdhost.c (EMMC1) - Copyright (C) 2015-2016 Raspberry Pi (Trading) Ltd.
   
References
==========

 BCM2711 ARM Peripherals
  
  https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2711/rpi_DATA_2711_1p0.pdf
 
BCM2711 Devices
===============
 
 This unit provides the BCM2711 specific implementations of the following devices:

  SPI0
  SPI1
  SPI2
  SPI3
  SPI4
  SPI5
  SPI6
  I2C0
  I2C1
  I2C2
  I2C3
  I2C4
  I2C5
  I2C6
  I2C7
  I2C Slave
  SPI Slave
  DMA
  PWM0
  PWM1
  PCM
  GPIO
  UART0
  UART1
  UART2
  UART3
  UART4
  UART5
  EMMC0
  EMMC1
  EMMC2
 
  Clock (System Timer)
  Clock (ARM Timer)
  Clock (Local Timer)
  ARM Timer
  Local Timer
  Random
  Mailbox
  Watchdog
  Framebuffer

BCM2711 SPI0/3/4/5/6 Device
===========================

 The BCM2711 has 5 master mode SPI controllers that support 3 wire, 2 wire and LoSSI modes of operation. It also has
 2 auxiliary SPI masters which do not support DMA mode (see SPI1/2 below).
 
 The main SPI0/3/4/5/6 controllers support polled, interrupt and DMA modes and include 3 chip selects although only CS0
 and CS1 are available on the 40 pin header. 

 The GPIO pins and functions for each device are shown below, not all of these combinations are accessible via the 40 pin header.

 Device     Line      Pin          Function             Notes
 -----------------------------------------------------------------------
 SPI0       CS1       GPIO_PIN_7   GPIO_FUNCTION_ALT0  
            CS0       GPIO_PIN_8   GPIO_FUNCTION_ALT0  
            MISO      GPIO_PIN_9   GPIO_FUNCTION_ALT0
            MOSI      GPIO_PIN_10  GPIO_FUNCTION_ALT0
            SCLK      GPIO_PIN_11  GPIO_FUNCTION_ALT0
                                   
            CS1       GPIO_PIN_35  GPIO_FUNCTION_ALT0   CM4 only
            CS0       GPIO_PIN_36  GPIO_FUNCTION_ALT0
            MISO      GPIO_PIN_37  GPIO_FUNCTION_ALT0
            MOSI      GPIO_PIN_38  GPIO_FUNCTION_ALT0
            SCLK      GPIO_PIN_39  GPIO_FUNCTION_ALT0
                                   
 SPI3       CS1       GPIO_PIN_24  GPIO_FUNCTION_ALT5
            CS0       GPIO_PIN_0   GPIO_FUNCTION_ALT3
            MISO      GPIO_PIN_1   GPIO_FUNCTION_ALT3
            MOSI      GPIO_PIN_2   GPIO_FUNCTION_ALT3
            SCLK      GPIO_PIN_3   GPIO_FUNCTION_ALT3
                                   
 SPI4       CS1       GPIO_PIN_25  GPIO_FUNCTION_ALT5
            CS0       GPIO_PIN_4   GPIO_FUNCTION_ALT3
            MISO      GPIO_PIN_5   GPIO_FUNCTION_ALT3
            MOSI      GPIO_PIN_6   GPIO_FUNCTION_ALT3
            SCLK      GPIO_PIN_7   GPIO_FUNCTION_ALT3
                                   
 SPI5       CS1       GPIO_PIN_26  GPIO_FUNCTION_ALT5
            CS0       GPIO_PIN_12  GPIO_FUNCTION_ALT3
            MISO      GPIO_PIN_13  GPIO_FUNCTION_ALT3
            MOSI      GPIO_PIN_14  GPIO_FUNCTION_ALT3
            SCLK      GPIO_PIN_15  GPIO_FUNCTION_ALT3
                                   
 SPI6       CS1       GPIO_PIN_27  GPIO_FUNCTION_ALT5
            CS0       GPIO_PIN_18  GPIO_FUNCTION_ALT3
            MISO      GPIO_PIN_19  GPIO_FUNCTION_ALT3
            MOSI      GPIO_PIN_20  GPIO_FUNCTION_ALT3
            SCLK      GPIO_PIN_21  GPIO_FUNCTION_ALT3


BCM2711 I2C0/1/2/3/4/5/6/7 Device
=================================

 The BCM2711 has 8 Broadcom Serial Controller (BSC) devices which are fast mode (400Kz) I2C masters numbered BSC0 to BSC7 (I2C0 to I2C7).
 
 Devices BSC2 and BSC7 are dedicated to the HDMI interfaces but can be accessed by the ARM processor for controlling some
 HDMI functionality. All BSC devices contain a 16 byte FIFO, support 7 bit and 10 bit addressing and have software
 configurable clock timing.
 
 The GPIO pins and functions for each device are shown below, not all of these combinations are accessible via the 40 pin header.
 
 Device     Line      Pin          Function             Notes
 -----------------------------------------------------------------------
 I2C0       SDA0      GPIO_PIN_0   GPIO_FUNCTION_ALT0
            SCL0      GPIO_PIN_1   GPIO_FUNCTION_ALT0
                      
            SDA0      GPIO_PIN_28  GPIO_FUNCTION_ALT0
            SCL0      GPIO_PIN_29  GPIO_FUNCTION_ALT0
                      
            SDA0      GPIO_PIN_44  GPIO_FUNCTION_ALT1
            SCL0      GPIO_PIN_45  GPIO_FUNCTION_ALT1
                      
 I2C1       SDA1      GPIO_PIN_2   GPIO_FUNCTION_ALT0
            SCL1      GPIO_PIN_3   GPIO_FUNCTION_ALT0
                      
            SDA1      GPIO_PIN_44  GPIO_FUNCTION_ALT2
            SCL1      GPIO_PIN_45  GPIO_FUNCTION_ALT2
            
 I2C2       SDA2                                       Not Applicable
            SCL2                                       Not Applicable
            
 I2C3       SDA3      GPIO_PIN_2   GPIO_FUNCTION_ALT5
            SCL3      GPIO_PIN_3   GPIO_FUNCTION_ALT5
                                   
            SDA3      GPIO_PIN_4   GPIO_FUNCTION_ALT5
            SCL3      GPIO_PIN_5   GPIO_FUNCTION_ALT5
                                   
 I2C4       SDA4      GPIO_PIN_6   GPIO_FUNCTION_ALT5
            SCL4      GPIO_PIN_7   GPIO_FUNCTION_ALT5
                                   
            SDA4      GPIO_PIN_8   GPIO_FUNCTION_ALT5
            SCL4      GPIO_PIN_9   GPIO_FUNCTION_ALT5
                      
 I2C5       SDA5      GPIO_PIN_10  GPIO_FUNCTION_ALT5
            SCL5      GPIO_PIN_11  GPIO_FUNCTION_ALT5
                      
            SDA5      GPIO_PIN_12  GPIO_FUNCTION_ALT5
            SCL5      GPIO_PIN_13  GPIO_FUNCTION_ALT5
                      
 I2C6       SDA6      GPIO_PIN_0   GPIO_FUNCTION_ALT5
            SCL6      GPIO_PIN_1   GPIO_FUNCTION_ALT5
                      
            SDA6      GPIO_PIN_22  GPIO_FUNCTION_ALT5
            SCL6      GPIO_PIN_23  GPIO_FUNCTION_ALT5
            
 I2C7       SDA7                                       Not Applicable
            SCL7                                       Not Applicable
 
 Note: On the Raspberry Pi 4B the ID EEPROM pins on the 40 pin header are actually connected to GPIO 0 and 1 (I2C0)
 

BCM2711 SPI1/2 Device
=====================

 The SPI1/2 devices are part of the AUX device which also includes the UART1 device.
 
 They are master SPI devices which are considered secondary low throughput interfaces
 as they have small FIFOs and no DMA support.
 
 The GPIO pins and functions for each device are shown below, not all of these combinations are accessible via the 40 pin header.
 
 Device     Line      Pin          Function             Notes
 -----------------------------------------------------------------------

 SPI1       CS2       GPIO_PIN_16  GPIO_FUNCTION_ALT4  
            CS1       GPIO_PIN_17  GPIO_FUNCTION_ALT4  
            CS0       GPIO_PIN_18  GPIO_FUNCTION_ALT4  
            MISO      GPIO_PIN_19  GPIO_FUNCTION_ALT4
            MOSI      GPIO_PIN_20  GPIO_FUNCTION_ALT4
            SCLK      GPIO_PIN_21  GPIO_FUNCTION_ALT4
 
 SPI2       CS2       GPIO_PIN_45  GPIO_FUNCTION_ALT4   CM4 only
            CS1       GPIO_PIN_44  GPIO_FUNCTION_ALT4
            CS0       GPIO_PIN_43  GPIO_FUNCTION_ALT4
            MISO      GPIO_PIN_40  GPIO_FUNCTION_ALT4
            MOSI      GPIO_PIN_41  GPIO_FUNCTION_ALT4
            SCLK      GPIO_PIN_42  GPIO_FUNCTION_ALT4
 
 Note: The GPIO function assignments section of the BCM2711 ARM Peripherals document incorrectly lists SPI2 as SPI0


BCM2711 SPI/I2C Slave Device
============================

 The slave device can be used as either an I2C or an SPI interface, the I2C slave supports 400KHz fast mode
 operation. Only 7 bit addressing is supported, DMA is not supported and neither is clock stretching.
 
 Unlike earlier models where the SPI slave device was apparently faulty on the Raspberry Pi 4 it is reported to work.

 The GPIO pins and functions for each device are shown below, not all of these combinations are accessible via the 40 pin header.
 
 Device     Line      Pin          Function             Notes
 -----------------------------------------------------------------------

 SPI Slave  CS        GPIO_PIN_8   GPIO_FUNCTION_ALT3
            MISO      GPIO_PIN_9   GPIO_FUNCTION_ALT3
            MOSI      GPIO_PIN_10  GPIO_FUNCTION_ALT3
            SCLK      GPIO_PIN_11  GPIO_FUNCTION_ALT3
 
 I2C Slave  SDA       GPIO_PIN_10  GPIO_FUNCTION_ALT3
            SCL       GPIO_PIN_11  GPIO_FUNCTION_ALT3
 
 Note: The BCM2711 ARM Peripherals document does not include documentation for the I2C/SPI slave
 
 
BCM2711 DMA Device
==================

 The DMA controller has 16 channels in total although not all are available for software to use as some are already used by the GPU.
 
 The firmware will pass the value dma.dmachans on the command line which will indicate which channels are available for our use.
 
 Channels 0 to 6 are normal channels which support 2D stride and transfers up to 1GB per control block
 
 Channels 7 to 10 are Lite channels which do not support stride and only allow transfers up to 64KB per control block
 
 Channels 11 to 14 are 40 bit channels which allow access to memory and peripherals beyond the 1GB boundary and have higher performance
  because they directly access the full 35 bit address map and can perform write bursts (DMA channel 11 is used to access the PCIe interface)

 Channel 15 is not mentioned in most documentation and is shown as not available in the mask passed in dma.dmachans
 
 Channel 0 and 15 are Bulk channels which have an additional FIFO for faster transfers (8 beat burst per read)


BCM2711 PWM0/1 Device
=====================

 The BCM2711 has two PWM controllers with 2 independent output bit streams with multiple algorithms for generating the output
 pulse. The PWM controllers support either a single data register (independent per channel) or a 16 x 32 FIFO which also supports
 DMA mode transmission.
 
 On the Raspberry Pi 4B PWM1_0 and PWM1_1 are also connected via GPIO pins 40 and 41 to the audio circuit and allow playback
 of digital audio signals via the 4 pole line jack.

 The GPIO pins and functions for each device are shown below, not all of these combinations are accessible via the 40 pin header.
 
 Device     Line      Pin          Function             Notes
 -----------------------------------------------------------------------

 PWM0       PWM0_0    GPIO_PIN_12  GPIO_FUNCTION_ALT0 
            PWM0_1    GPIO_PIN_13  GPIO_FUNCTION_ALT0 

            PWM0_0    GPIO_PIN_18  GPIO_FUNCTION_ALT5 
            PWM0_1    GPIO_PIN_19  GPIO_FUNCTION_ALT5 
            
            PWM0_1    GPIO_PIN_45  GPIO_FUNCTION_ALT0
 
 PWM1       PWM1_0    GPIO_PIN_40  GPIO_FUNCTION_ALT0 
            PWM1_1    GPIO_PIN_41  GPIO_FUNCTION_ALT0 

 On the Raspberry Pi 4B pins 12, 18 and 19 are exposed on the 40 pin header.


BCM2711 PCM Device
==================

 //To Do
 
BCM2711 GPIO Device
===================

 The GPIO has 54 pins available each with multiple alternate functions. All pins can be configured as input or output
 and all can have pull up or down applied.
 
 Not all pins are exposed on the 40 pin header of the Raspberry Pi, for details of which pins are available see:
 
  Raspberry Pi 4B - https://www.raspberrypi.org/documentation/usage/gpio/README.md
 
 Some of the 54 pins are used for peripheral communication (such as the SD card) and are not available for general use,
 take care when changing function selects on pins to avoid disabling certain system peripherals.
 
 Event detection can be enabled for both high and low levels as well as rising and falling edges, there is also an
 asynchronous rising or falling edge detection which can detect edges of very short duration.


BCM2711 UART0/2/3/4/5 Device
============================

 The UART0/2/3/4/5 device is an ARM PL011 UART which supports programmable baud rates, start, stop and parity bits and hardware
 flow control and many others. The UART0 is similar to the industry standard 16C650 but with a number of differences, the
 PL011 has some optional features such as IrDA, Serial InfraRed and DMA which are not supported by the Broadcom implementation.

 In the standard configuration the UART0 TX and RX lines are connected to GPIO pins 14 and 15 respectively (Alternate function
 0) but they can be remapped via GPIO function selects to a number of other locations. On the Raspberry Pi 4B none of these 
 alternate pin mappings are exposed via the 40 pin header and therefore cannot be used easily. This means that UART0 and UART1
 cannot be used via the 40 pin header at the same time.

 On the Raspberry Pi 4B the UART0 can be mapped to GPIO pins 32 and 33 (Alternate function 3) to communicate with the built in
 Bluetooth module.
 
 The GPIO pins and functions for each device are shown below, not all of these combinations are accessible via the 40 pin header.
 
 Device     Line      Pin          Function             Notes
 -----------------------------------------------------------------------
 
 UART0      TXD       GPIO_PIN_14  GPIO_FUNCTION_ALT0  
            RXD       GPIO_PIN_15  GPIO_FUNCTION_ALT0  
            CTS       GPIO_PIN_16  GPIO_FUNCTION_ALT3  
            RTS       GPIO_PIN_17  GPIO_FUNCTION_ALT3  
                                   
            TXD       GPIO_PIN_32  GPIO_FUNCTION_ALT3  
            RXD       GPIO_PIN_33  GPIO_FUNCTION_ALT3  
            CTS       GPIO_PIN_30  GPIO_FUNCTION_ALT3  
            RTS       GPIO_PIN_31  GPIO_FUNCTION_ALT3  
                                   
            TXD       GPIO_PIN_36  GPIO_FUNCTION_ALT2  
            RXD       GPIO_PIN_37  GPIO_FUNCTION_ALT2  
            CTS       GPIO_PIN_38  GPIO_FUNCTION_ALT2  
            RTS       GPIO_PIN_39  GPIO_FUNCTION_ALT2  
                                   
 UART2      TXD       GPIO_PIN_0   GPIO_FUNCTION_ALT4  
            RXD       GPIO_PIN_1   GPIO_FUNCTION_ALT4  
            CTS       GPIO_PIN_2   GPIO_FUNCTION_ALT4  
            RTS       GPIO_PIN_3   GPIO_FUNCTION_ALT4  
                                   
 UART3      TXD       GPIO_PIN_4   GPIO_FUNCTION_ALT4  
            RXD       GPIO_PIN_5   GPIO_FUNCTION_ALT4  
            CTS       GPIO_PIN_6   GPIO_FUNCTION_ALT4  
            RTS       GPIO_PIN_7   GPIO_FUNCTION_ALT4  
                                   
 UART4      TXD       GPIO_PIN_8   GPIO_FUNCTION_ALT4  
            RXD       GPIO_PIN_9   GPIO_FUNCTION_ALT4  
            CTS       GPIO_PIN_10  GPIO_FUNCTION_ALT4  
            RTS       GPIO_PIN_11  GPIO_FUNCTION_ALT4  
                                   
 UART5      TXD       GPIO_PIN_12  GPIO_FUNCTION_ALT4  
            RXD       GPIO_PIN_13  GPIO_FUNCTION_ALT4  
            CTS       GPIO_PIN_14  GPIO_FUNCTION_ALT4  
            RTS       GPIO_PIN_15  GPIO_FUNCTION_ALT4  


BCM2711 UART1 Device
====================

 The UART1 device is a Broadcom implementation that is part of the AUX device which also includes the SPI1 and SPI2 devices.
 This device is termed a Mini UART and has a smaller feature set than the PL011 UART but still supports a fairly standard
 communication protocol with programmable baud rate and hardware flow control.
 
 The Mini UART is similar to the standard 16550 device but is missing some of the features, the device also has no DMA support
 so high speed transfers will produce a higher CPU load.

 In the standard configuration the UART1 TX and RX lines are connected to GPIO pins 14 and 15 respectively (Alternate function
 5) but they can be remapped via GPIO function selects to a number of other locations. On the Raspberry Pi 4B none of these 
 alternate pin mappings are exposed via the 40 pin header and therefore cannot be used easily. This means that UART0 and UART1
 cannot be used via the 40 pin header at the same time.
 
 On the Raspberry Pi 4B the UART1 can be mapped to GPIO pins 32 and 33 (Alternate function 5) to communicate with the built in
 Bluetooth module.

 The GPIO pins and functions for the device are shown below, not all of these combinations are accessible via the 40 pin header.
 
 Device     Line      Pin          Function             Notes
 -----------------------------------------------------------------------

 UART1      TXD       GPIO_PIN_14  GPIO_FUNCTION_ALT5  
            RXD       GPIO_PIN_15  GPIO_FUNCTION_ALT5  
            CTS       GPIO_PIN_16  GPIO_FUNCTION_ALT5  
            RTS       GPIO_PIN_17  GPIO_FUNCTION_ALT5  
            
            TXD       GPIO_PIN_32  GPIO_FUNCTION_ALT5  
            RXD       GPIO_PIN_33  GPIO_FUNCTION_ALT5  
            CTS       GPIO_PIN_30  GPIO_FUNCTION_ALT5  
            RTS       GPIO_PIN_31  GPIO_FUNCTION_ALT5  

BCM2711 EMMC0 (SDHCI) Device
============================

 The SDHCI controller on the BCM2711 is an Arasan SD Host controller.

 The Card Detect pin is not connected.

 The Write Protect pin is not connected.
 
 The device can be routed to GPIO pins 22 to 27 (ALT3) or 48 to 53 (ALT3), it can also be
 routed to GPIO pins 34 to 39 (ALT3) to provide an SDIO controller for the on board WiFi.
 
 
BCM2711 EMMC1 (SDHOST) Device
=============================

 The SDHOST controller on the BCM2711 is a non SDHCI-compliant device which requires a specific
 driver. 
 
 It can be routed to GPIO pins 22 to 27 (ALT0) or 48 to 53 (ALT0) however only 22 to 27 are accessible
 to use.

 Note: The actual driver is implemented in the BCMSDHOST unit
 

BCM2711 EMMC2 (SDHCI) Device
============================

 The EMMC2 controller on the BCM2711 is an SDHCI-compliant device which does not appear on the GPIO
 pins and is internally connected to the SD card slot.
 
 The BCM2838_GPPINMUX register allows routing the SDHCI controller (EMMC0) to the SD card slot which
 then renders the EMMC2 unusable.
 

BCM2711 Clock (System Timer) Device
===================================

 The clock device in the BCM2711 is based on the System Timer which is a 64 bit free running counter that runs at 1MHz regardless
 of core or CPU clock speeds. The System Timer cannot be stopped and the counter cannot be set or reset.
 
 The System Timer includes 4 compare registers which can each generate an interrupt when the compare value is matched, however 2
 of the 4 are consumed by the GPU and on the Raspberry Pi A/B/A+/B+/Zero the other 2 are used for the scheduler and clock interrupts
 in Ultibo. 
 
 This device simply exposes the free running counter as a clock value and does not provide access to the timer compare functionality
 or to interrupt based events, for those see the timer devices below.
 

BCM2711 Clock (ARM Timer) Device
================================

 This device represents that free running counter from the ARM Timer device (below) as a clock device. The free running counter does
 not appear in the original SP804 timer. The counter is 32 bits wide and has its own divider that is 8 bits wide meaning that it can
 be set to clock rates of between 975KHz and 250MHz (or 1.5MHz to 400MHz on the Raspberry Pi 3B).
 
 The counter does not generate an interrupt and cannot be set or reset but it can be stopped and started.


BCM2711 ARM Timer Device
========================

 The ARM Timer device in the BCM2711 is based on the ARM SP804 timer with some modifications and additions. In the Raspberry Pi 
 it is connected to the core clock which by default is 250MHz but was increased to 400MHz on the Raspberry Pi 3B.
 
 The divider is 10 bits wide which means that the ARM Timer can be set to clock rates of between 250KHz and 250MHz (or 400KHz 
 to 400MHz on the Raspberry Pi 3B). Both the counter and the load/reload value are 32 bits wide by default giving a wide range of
 tick intervals.
  
 The ARM Timer features a free running counter which is not enabled or used by this driver and a down counter which operates in
 wrapping mode so that each time it reaches 0 it triggers an interrupt and reloads the value from a load or reload register to
 begin counting again.


BCM2711 Local Timer Device
==========================

 //To Do

BCM2711 Random Device
=====================

 //To Do

BCM2711 Mailbox Device
======================

 //To Do

BCM2711 Watchdog Device
=======================

 //To Do

BCM2711 Framebuffer Device
==========================
 
 //To Do
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit BCM2711;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE ..\core\GlobalDefines.inc}
{--$DEFINE BCM2711_SPI0_DMA_CS_DLEN} {Use DMA to load the CS and DLEN registers of SPI0 (See 10.6.3 DMA on Page 158 of BCM2835 ARM Peripherals)}
                                     {Not used by the Linux driver, works on RPi 2/3/4, fails randomly on RPi A/B/Zero}

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2838,Platform{$IFNDEF CONSOLE_EARLY_INIT},PlatformRPi4{$ENDIF},Threads,HeapManager,Devices,SPI,I2C,DMA,PWM,GPIO,UART,Serial,MMC,BCMSDHOST,Framebuffer,Audio,SysUtils; 

{==============================================================================}
const
 {BCM2711 specific constants}

 {BCM2711 SPI0/3/4/5/6 constants}
 BCM2711_SPI0_DESCRIPTION = 'BCM2838 Master SPI';
 
 BCM2711_SPI0_MAX_SIZE = $FFFF;

 BCM2711_SPI0_MIN_CLOCK = 7629;       {Default minimum based on the default settings from the firmware (Recalculated during open)}
 BCM2711_SPI0_MAX_CLOCK = 250000000;  {Default maximum based on the default settings from the firmware (Recalculated during open)}

 BCM2711_SPI0_MIN_DIVIDER = 2;        {Divider is always rounded down to an even number and a value of 0 sets the divider to 65536}
 BCM2711_SPI0_MAX_DIVIDER = $FFFE;    {Divider is always rounded down to an even number}

 BCM2711_SPI0_CORE_CLOCK = 500000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}
 
 BCM2711_SPI0_MODE_IRQ = 0;
 BCM2711_SPI0_MODE_DMA = 1;
 BCM2711_SPI0_MODE_PIO = 2;
 
 {BCM2711 I2C0/1/2/3/4/5/6/7 constants}
 BCM2711_I2C0_DESCRIPTION = 'BCM2838 Master I2C';

 BCM2711_I2C0_MAX_SIZE = $FFFF;
 
 BCM2711_I2C0_MIN_CLOCK = 7629;       {Default minimum based on the default settings from the firmware (Recalculated during open)}
 BCM2711_I2C0_MAX_CLOCK = 250000000;  {Default maximum based on the default settings from the firmware (Recalculated during open)}
 BCM2711_I2C0_DEFAULT_CLOCK = 100000; 
 
 BCM2711_I2C0_MIN_DIVIDER = 2;        {Divider is always rounded down to an even number and a value of 0 sets the divider to 32768}
 BCM2711_I2C0_MAX_DIVIDER = $FFFE;    {Divider is always rounded down to an even number}
 
 BCM2711_I2C0_CORE_CLOCK = 500000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}
 
 BCM2711_I2C0_MODE_WRITE = 0;
 BCM2711_I2C0_MODE_READ  = 1;
 
 {BCM2711 SPI1/2 (AUX) constants}
 BCM2711_SPI1_DESCRIPTION = 'BCM2838 AUX Master SPI1';
 BCM2711_SPI2_DESCRIPTION = 'BCM2838 AUX Master SPI2';

 //To Do
 
 {BCM2711 SPI/I2C Slave constants}
 BCM2711_I2CSLAVE_DESCRIPTION = 'BCM2838 I2C Slave';
 BCM2711_SPISLAVE_DESCRIPTION = 'BCM2838 SPI Slave';

 BCM2711_I2CSLAVE_TIMEOUT = 10;                  {Timeout (Milliseconds) for RX/TX wait data}
 BCM2711_I2CSLAVE_BUFFER_SIZE = 1024;            {Size in bytes of the RX/TX data buffer}
 BCM2711_I2CSLAVE_RX_POLL_LIMIT = 256;           {Number of times interrupt handler may poll the read FIFO}
 
 {BCM2711 DMA constants}
 BCM2711_DMA_DESCRIPTION = 'BCM2838 DMA';
 
 BCM2711_DMA_CHANNEL_COUNT = 16;                 {Total number of DMA channels (Not all are usable)}
 
 BCM2711_DMA_40_CHANNELS     = $7800;            {Mask of 40 bit channels (11 to 14)}
 BCM2711_DMA_LITE_CHANNELS   = $0780;            {Mask of Lite channels (7 to 10)}
 BCM2711_DMA_NORMAL_CHANNELS = $007E; {807F}     {Mask of normal channels (1 to 6)}
 BCM2711_DMA_BULK_CHANNELS   = $8001;            {Mask of Bulk channels (0 and 15)}
 
 BCM2711_DMA_SHARED_CHANNELS = $0780;            {Mask of channels with shared interrupt (7 to 10)}
 
 BCM2711_DMA_MAX_40_TRANSFER     = 1073741824;   {Maximum transfer length for a 40 bit channel}
 BCM2711_DMA_MAX_LITE_TRANSFER   = 65536;        {Maximum transfer length for a Lite channel}
 BCM2711_DMA_MAX_NORMAL_TRANSFER = 1073741824;   {Maximum transfer length for a normal channel}
 
 BCM2711_DMA_MAX_STRIDE   = $FFFF;               {Maximum stride value (Increment between rows) (Note this is a signed value (Min -32768 / Max 32767)}
 BCM2711_DMA_MAX_Y_COUNT  = $3FFF;               {Maximum number of X length transfers in 2D stride}
 BCM2711_DMA_MAX_X_LENGTH = $FFFF;               {Maximum X transfer length in 2D stride}
 
 BCM2711_DMA_CB_ALIGNMENT = 32;                  {Alignment required for DMA control blocks}
 
 BCM2711_DMA_40_BURST_LENGTH = 8;                {Burst length for 40 bit channels}
 BCM2711_DMA_LITE_BURST_LENGTH = 1;              {Burst length for DMA Lite channels}
 BCM2711_DMA_NORMAL_BURST_LENGTH = 2;            {Burst length for normal channels}
 BCM2711_DMA_BULK_BURST_LENGTH = 8;              {Burst length for DMA Bulk channels}
 
 BCM2711_DMA_REQUIRE_40_ADDRESS = SIZE_1G;       {DMA transfers to or from address above 1GB require a 40-bit channel}

 BCM2711_DMA_40_PERIPHERAL_OFFSET = 4;           {40 bit DMA channels use the "Full 35-bit address map" and must access peripherals at 0x4:7C000000 - 0x4:7FFFFFFF}
 BCM2711_DMA_40_PERIPHERAL_IO_BASE  = BCM2838_EXT_PERIPHERALS_BASE;
 BCM2711_DMA_40_PERIPHERAL_IO_ALIAS = $7C000000;
 BCM2711_DMA_40_PERIPHERAL_IO_MASK  = $03FFFFFF;

 {BCM2711 PWM0/1 constants}
 BCM2711_PWM0_DESCRIPTION = 'BCM2838 PWM';
 
 BCM2711_PWM0_MIN_PERIOD = 38;           {Default based on 54MHz PWM clock (Oscillator source)}
 BCM2711_PWM0_DEFAULT_CLOCK = 54000000;  {Default to the 54MHz oscillator clock} 
 
 {BCM2711 PCM constants}
 //To Do
 
 {BCM2711 GPIO constants}
 BCM2711_GPIO_DESCRIPTION = 'BCM2838 GPIO';
 
 BCM2711_GPIO_MIN_PIN = GPIO_PIN_0;
 BCM2711_GPIO_MAX_PIN = GPIO_PIN_57;
 
 BCM2711_GPIO_MAX_LEVEL = GPIO_LEVEL_HIGH;
 
 BCM2711_GPIO_MAX_PULL = GPIO_PULL_DOWN;
  
 BCM2711_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 BCM2711_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT5;

 BCM2711_GPIO_MIN_TRIGGER = GPIO_TRIGGER_LOW;
 BCM2711_GPIO_MAX_TRIGGER = GPIO_TRIGGER_ASYNC_FALLING;

 BCM2711_GPIO_PULL_MAP:array[GPIO_PULL_NONE..GPIO_PULL_DOWN] of LongWord = (
  {GPIO pull up/down to BCM2838 pull up/down}
  BCM2838_GPPUD_NONE,
  BCM2838_GPPUD_UP,
  BCM2838_GPPUD_DOWN);

 BCM2711_GPIO_PULL_UNMAP:array[BCM2838_GPPUD_NONE..BCM2838_GPPUD_DOWN] of LongWord = (
  {BCM2838 pull up/down to GPIO pull up/down}
  GPIO_PULL_NONE,
  GPIO_PULL_UP,
  GPIO_PULL_DOWN);

 BCM2711_GPIO_FUNCTION_MAP:array[BCM2711_GPIO_MIN_FUNCTION..BCM2711_GPIO_MAX_FUNCTION] of LongWord = (
  {GPIO functions to BCM2838 functions}
  BCM2838_GPFSEL_IN,
  BCM2838_GPFSEL_OUT,
  BCM2838_GPFSEL_ALT0,
  BCM2838_GPFSEL_ALT1,
  BCM2838_GPFSEL_ALT2,
  BCM2838_GPFSEL_ALT3,
  BCM2838_GPFSEL_ALT4,
  BCM2838_GPFSEL_ALT5);
 
 BCM2711_GPIO_FUNCTION_UNMAP:array[BCM2711_GPIO_MIN_FUNCTION..BCM2711_GPIO_MAX_FUNCTION] of LongWord = (
  {BCM2838 functions to GPIO functions}
  GPIO_FUNCTION_IN,
  GPIO_FUNCTION_OUT,
  GPIO_FUNCTION_ALT5,
  GPIO_FUNCTION_ALT4,
  GPIO_FUNCTION_ALT0,
  GPIO_FUNCTION_ALT1,
  GPIO_FUNCTION_ALT2,
  GPIO_FUNCTION_ALT3);
  
 BCM2711_GPIO_TRIGGER_MAP:array[BCM2711_GPIO_MIN_TRIGGER..BCM2711_GPIO_MAX_TRIGGER] of LongWord = (
  {GPIO triggers to BCM2838 event registers}
  BCM2838_GPLEN0,
  BCM2838_GPHEN0,
  BCM2838_GPREN0,
  BCM2838_GPFEN0,
  BCM2838_GPAREN0,
  BCM2838_GPAFEN0);

 {BCM2711 UART0/2/3/4/5 (PL011) constants}
 BCM2711_UART0_DESCRIPTION = 'BCM2838 PL011 UART';
 
 BCM2711_UART0_MIN_BAUD = 300;      {Default minimum of 300 baud}
 BCM2711_UART0_MAX_BAUD = 3000000;  {Default maximum based on the default settings from the firmware (Recalculated during open)}
 
 BCM2711_UART0_MIN_DATABITS = SERIAL_DATA_5BIT;
 BCM2711_UART0_MAX_DATABITS = SERIAL_DATA_8BIT;
 
 BCM2711_UART0_MIN_STOPBITS = SERIAL_STOP_1BIT;
 BCM2711_UART0_MAX_STOPBITS = SERIAL_STOP_2BIT;
 
 BCM2711_UART0_MAX_PARITY = SERIAL_PARITY_EVEN;
 
 BCM2711_UART0_MAX_FLOW = SERIAL_FLOW_RTS_CTS;
 
 BCM2711_UART0_CLOCK_RATE = 48000000; {3000000} {Default clock rate based on the default settings from the firmware (Requested from firmware during open)}
 {$IFDEF BCM2711_UART0_RX_BUFFER}
 BCM2711_UART0_RX_POLL_LIMIT = 256; {Number of times interrupt handler may poll the read FIFO}
 BCM2711_UART0_RX_BUFFER_SIZE = 1024;
 {$ENDIF}

 {BCM2711 UART1 (AUX) constants}
 BCM2711_UART1_DESCRIPTION = 'BCM2838 AUX UART1';
 
 {BCM2711 EMMC0 (SDHCI) constants}
 BCM2711_EMMC0_DESCRIPTION = 'BCM2838 Arasan SD Host';
 
 BCM2711_EMMC0_MIN_FREQ = 400000;    {Default minimum of 400KHz}
 BCM2711_EMMC0_MAX_FREQ = 250000000; {Default clock rate based on the default settings from the firmware (Requested from firmware during start)}

 {BCM2711 EMMC1 (SDHOST) constants}
 BCM2711_EMMC1_DESCRIPTION = 'BCM2838 SDHOST';
 
 BCM2711_EMMC1_MIN_FREQ = 400000;    {Default minimum of 400KHz}
 BCM2711_EMMC1_MAX_FREQ = 500000000; {Default clock rate based on the default settings from the firmware (Requested from firmware during start)}

 {See: BCMSDHOST for the driver implementation}

 {BCM2711 EMMC2 (SDHCI) constants}
 BCM2711_EMMC2_DESCRIPTION = 'BCM2838 SDHCI';
 
 BCM2711_EMMC2_MIN_FREQ = 400000;    {Default minimum of 400KHz}
 BCM2711_EMMC2_MAX_FREQ = 500000000; {Default clock rate based on the default settings from the firmware (Requested from firmware during start)}

 {BCM2711 Clock (System Timer) constants}
 BCM2711_SYS_CLOCK_DESCRIPTION = 'BCM2838 System Timer Clock';

 {BCM2711 Clock (ARM Timer) constants}
 BCM2711_ARM_CLOCK_DESCRIPTION = 'BCM2838 ARM Timer Clock';

 BCM2711_ARM_CLOCK_MIN_RATE = 1953125;     {Default minimum (Divider 255) based on the default settings from the firmware (Recalculated during start)}
 BCM2711_ARM_CLOCK_MAX_RATE = 500000000;   {Default maximum (Divider 0) based on the default settings from the firmware (Recalculated during start)}
 BCM2711_ARM_CLOCK_DEFAULT_RATE = 7936507; {Default rate (Divider 62) based on the default settings from the firmware (Recalculated during start)}
  
 BCM2711_ARM_CLOCK_MIN_DIVIDER = 0;
 BCM2711_ARM_CLOCK_MAX_DIVIDER = 255;
 BCM2711_ARM_CLOCK_DEFAULT_DIVIDER = 62;
 
 BCM2711_ARM_CLOCK_CORE_CLOCK = 500000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}
 
 {BCM2711 Clock (Local Timer) constants}
 BCM2711_LOCAL_CLOCK_DESCRIPTION = 'BCM2838 Local Timer Clock';
 
 {BCM2711 ARM Timer constants}
 BCM2711_ARM_TIMER_DESCRIPTION = 'BCM2838 ARM Timer';

 BCM2711_ARM_TIMER_MIN_RATE = 488281;      {Default minimum (Divider 1023) based on the default settings from the firmware (Recalculated during start)}
 BCM2711_ARM_TIMER_MAX_RATE = 500000000;   {Default maximum (Divider 0) based on the default settings from the firmware (Recalculated during start)}
 BCM2711_ARM_TIMER_DEFAULT_RATE = 2000000; {Default rate (Divider 249) based on the default settings from the firmware (Recalculated during start)}
 
 BCM2711_ARM_TIMER_MIN_INTERVAL = 1;
 BCM2711_ARM_TIMER_MAX_INTERVAL = $FFFFFFFF;
 
 BCM2711_ARM_TIMER_MIN_DIVIDER = 0;
 BCM2711_ARM_TIMER_MAX_DIVIDER = 1023;
 BCM2711_ARM_TIMER_DEFAULT_DIVIDER = 249;
 
 BCM2711_ARM_TIMER_CORE_CLOCK = 500000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}

 {BCM2711 Local Timer constants}
 BCM2711_LOCAL_TIMER_DESCRIPTION = 'BCM2838 Local Timer';
 
 {BCM2711 Random constants}
 BCM2711_RANDOM_DESCRIPTION = 'BCM2838 Random Number Generator (RNG200)';
 
 BCM2711_RANDOM_WARMUP_COUNT = $00040000; {The initial numbers generated are "less random" so will be discarded}

 {BCM2711 Mailbox constants}
 BCM2711_MAILBOX_DESCRIPTION = 'BCM2838 Mailbox';
 
 {BCM2711 Watchdog constants}
 BCM2711_WATCHDOG_DESCRIPTION = 'BCM2838 Watchdog Timer';

 {BCM2711 Framebuffer constants}
 BCM2711_FRAMEBUFFER_DESCRIPTION = 'BCM2838 Framebuffer';

{==============================================================================}
type
 {BCM2711 specific types}

 {BCM2711 SPI0/3/4/5/6 types}
 PBCM2711SPI0Device = ^TBCM2711SPI0Device;
 TBCM2711SPI0Device = record
  {SPI Properties}
  SPI:TSPIDevice;
  {BCM2711 Properties}
  Id:LongWord;                     {Id of this device (0/3/4/5/6)}
  IRQ:LongWord;                    {IRQ of this device}
  Address:Pointer;                 {Device register base address}
  CoreClock:LongWord;              {Core clock rate}
  SCLKPin:LongWord;                {GPIO pin for the SCLK line}
  MOSIPin:LongWord;                {GPIO pin for the MOSI line}
  MISOPin:LongWord;                {GPIO pin for the MISO line}
  CS0Pin:LongWord;                 {GPIO pin for the CS0 line}
  CS1Pin:LongWord;                 {GPIO pin for the CS1 line}
  CS2Pin:LongWord;                 {GPIO pin for the CS2 line}
  SCLKFunction:LongWord;           {GPIO function for the SCLK line}
  MOSIFunction:LongWord;           {GPIO function for the MOSI line}
  MISOFunction:LongWord;           {GPIO function for the MISO line}
  CS0Function:LongWord;            {GPIO function for the CS0 line}
  CS1Function:LongWord;            {GPIO function for the CS1 line}
  CS2Function:LongWord;            {GPIO function for the CS2 line}
  {Transfer Properties}
  Mode:LongWord;                   {Mode of current transfer (BCM2711_SPI0_MODE_IRQ / BCM2711_SPI0_MODE_DMA / BCM2711_SPI0_MODE_PIO)}
  Source:Pointer;                  {Pointer to the source for current transfer (nil if reading only)}
  Dest:Pointer;                    {Pointer to the destination for current transfer (nil if writing only)}
  Count:LongWord;                  {Count of bytes for current transfer}
  SourceRemain:LongWord;           {Source bytes remaining for current transfer}
  DestRemain:LongWord;             {Destination bytes remaining for current transfer}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end;

 {BCM2711 I2C0/1/2/3/4/5/6/7 types}
 PBCM2711I2C0Device = ^TBCM2711I2C0Device;
 TBCM2711I2C0Device = record
  {I2C Properties}
  I2C:TI2CDevice;
  {BCM2711 Properties}
  Id:LongWord;                     {Id of this device (0/1/3/4/5/6)}
  IRQ:LongWord;                    {IRQ of this device}
  Address:Pointer;                 {Device register base address}
  CoreClock:LongWord;              {Core clock rate}
  SDAPin:LongWord;                 {GPIO pin for the SDA line}
  SCLPin:LongWord;                 {GPIO pin for the SCL line}                 
  SDAFunction:LongWord;            {GPIO function for the SDA line}
  SCLFunction:LongWord;            {GPIO function for the SCL line}
  {Transfer Properties}
  Mode:LongWord;                   {Mode of current transfer (BCM2711_I2C0_MODE_WRITE / BCM2711_I2C0_MODE_READ)}
  Data:Pointer;                    {Pointer to the data for current transfer}
  Count:LongWord;                  {Count of bytes for current transfer}
  Remain:LongWord;                 {Bytes remaining for current transfer}
  Error:LongBool;                  {True if an error occurred during the transfer}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end;
 
 {BCM2711 SPI1/2 (AUX) types}
 PBCM2711SPI1Device = ^TBCM2711SPI1Device;
 TBCM2711SPI1Device = record
  {SPI Properties}
  SPI:TSPIDevice;
  {BCM2711 Properties}
  Address:Pointer;                 {Device register base address}
  CoreClock:LongWord;              {Core clock rate}
  {Transfer Properties}
  //To Do
 end;
 
 {BCM2711 SPI/I2C Slave types}
 PBCM2711I2CSlaveBuffer = ^TBCM2711I2CSlaveBuffer;
 TBCM2711I2CSlaveBuffer = record
  Wait:TSemaphoreHandle;           {Data ready semaphore}
  Start:LongWord;                  {Index of first available buffer entry}
  Count:LongWord;                  {Number of available entries in the buffer}
  Buffer:array[0..(BCM2711_I2CSLAVE_BUFFER_SIZE - 1)] of Byte; 
 end;

 PBCM2711I2CSlave = ^TBCM2711I2CSlave;
 TBCM2711I2CSlave = record
  {I2C Properties}
  I2C:TI2CDevice;
  {BCM2711 Properties}
  IRQ:LongWord;                    {IRQ of this device}
  Address:Pointer;                 {Device register base address}
  Lock:TSpinHandle;                {Device lock (Differs from lock in I2C device) (Spin lock due to use by interrupt handler)}
  SDAPin:LongWord;                 {GPIO pin for the SDA line}
  SCLPin:LongWord;                 {GPIO pin for the SCL line}                 
  SDAFunction:LongWord;            {GPIO function for the SDA line}
  SCLFunction:LongWord;            {GPIO function for the SCL line}
  {Transfer Properties}
  Receive:TBCM2711I2CSlaveBuffer;  {Receive Data Buffer}
  {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
  Transmit:TBCM2711I2CSlaveBuffer; {Transmit Data Buffer}
  {$ENDIF}
  {Statistics Properties}
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end;

 {BCM2711 DMA types}
 PBCM2711DMAHost = ^TBCM2711DMAHost;
 
 PBCM2711DMAChannel = ^TBCM2711DMAChannel;
 TBCM2711DMAChannel = record
  Host:PBCM2711DMAHost;                {DMA host this channel belongs to}
  Request:PDMARequest;                 {Current DMA request pending on this channel (or nil of no request is pending)} 
  Number:LongWord;                     {The channel number of this channel}
  Interrupt:LongWord;                  {The interrupt number of this channel}
  Registers:PBCM2838DMARegisters;      {The channel registers for configuration}
  Registers40:PBCM2838DMA40Registers;  {The 40 bit channel registers for configuration}
 end;
 
 TBCM2711DMAHost = record
  {DMA Properties}
  DMA:TDMAHost;
  {BCM2711 Properties}
  ChannelMask:LongWord;                                                   {Mask of available channels (Passed from GPU firmware)}
  ChannelFree:LongWord;                                                   {Bitmap of current free channels}
  ChannelLock:TMutexHandle;                                               {Lock for access to ChannelFree}
  ChannelWait:TSemaphoreHandle;                                           {Number of free normal channels in ChannelFree}
  Channel40:TSemaphoreHandle;                                             {Number of free 40 bit channels in ChannelFree}
  ChannelLite:TSemaphoreHandle;                                           {Number of free DMA Lite channels in ChannelFree}
  ChannelBulk:TSemaphoreHandle;                                           {Number of free DMA Bulk channels in ChannelFree}
  Channels:array[0..BCM2711_DMA_CHANNEL_COUNT - 1] of TBCM2711DMAChannel; {Channel information for each DMA channel on the host}
  EnableRegister:PLongWord;
  InterruptRegister:PLongWord;
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the host controller}
 end;
 
 {BCM2711 PWM0/1 types}
 PBCM2711PWM0Device = ^TBCM2711PWM0Device;
 PBCM2711PWM0Audio = ^TBCM2711PWM0Audio;
 
 TBCM2711PWM0Device = record
  {PWM Properties}
  PWM:TPWMDevice;
  {BCM2711 Properties}
  Id:LongWord;                     {Id of this device (0/1)}
  Address:Pointer;                 {Device register base address}
  Channel:LongWord;                {Channel for this device}
  Scaler:LongWord;                 {Scaler for Duty cycle and Period}
  Pair:PBCM2711PWM0Device;         {The paired PWM device for the other channel}
  {Audio Properties}
  Audio:PBCM2711PWM0Audio;         {The associated PWM Audio device}
 end; 

 TBCM2711PWM0Audio = record
  {Audio Properties}
  Audio:TAudioDevice;
  {BCM2711 Properties}
  //To Do 
  {PWM Properties}
  PWM0:PBCM2711PWM0Device;          {The PWM device for channel 0}
  PWM1:PBCM2711PWM0Device;          {The PWM device for channel 1}
 end;
 
 {BCM2711 PCM types}
 //To Do 
 
 {BCM2711 GPIO types}
 PBCM2711GPIODevice = ^TBCM2711GPIODevice;
 
 PBCM2711GPIOBank = ^TBCM2711GPIOBank;
 TBCM2711GPIOBank = record
  GPIO:PGPIODevice;
  Bank:LongWord;
  Address:PtrUInt;
  PinStart:LongWord;
 end;
 
 TBCM2711GPIODevice = record
  {GPIO Properties}
  GPIO:TGPIODevice;
  {BCM2711 Properties}
  Lock:TSpinHandle;                                                       {Device lock (Differs from lock in Device portion) (Spin lock due to use by interrupt handler)}
  Banks:array[0..BCM2838_GPIO_BANK_COUNT - 1] of TBCM2711GPIOBank;
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end;
 
 {BCM2711 UART0/2/3/4/5 types}
 PBCM2711UART0Device = ^TBCM2711UART0Device;
 TBCM2711UART0Device = record
  {UART Properties}
  UART:TUARTDevice;
  {BCM2711 Properties}
  Id:LongWord;                                                            {Id of this device (0/2/3/4/5)}
  IRQ:LongWord;                                                           {IRQ of this device}
  Lock:TSpinHandle;                                                       {Device lock (Differs from lock in UART device) (Spin lock due to use by interrupt handler)}
  Address:Pointer;                                                        {Device register base address}
  ClockRate:LongWord;                                                     {Device clock rate}
  TXDPin:LongWord;                                                        {GPIO pin for the TXD line}
  RXDPin:LongWord;                                                        {GPIO pin for the RXD line}
  CTSPin:LongWord;                                                        {GPIO pin for the CTS line}
  RTSPin:LongWord;                                                        {GPIO pin for the RTS line}
  TXDFunction:LongWord;                                                   {GPIO function for the TXD line}
  RXDFunction:LongWord;                                                   {GPIO function for the RXD line}
  CTSFunction:LongWord;                                                   {GPIO function for the CTS line}
  RTSFunction:LongWord;                                                   {GPIO function for the RTS line}
  {$IFDEF BCM2711_UART0_RX_BUFFER}
  Start:LongWord;                                                         {Index of first available buffer entry}
  Count:LongWord;                                                         {Number of available entries in the buffer}
  Buffer:array[0..(BCM2711_UART0_RX_BUFFER_SIZE - 1)] of Word;            {Buffer for received data (Includes data and status)}
  {$ENDIF}
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end;
 
 {BCM2711 UART1 types}
 PBCM2711UART1Device = ^TBCM2711UART1Device;
 TBCM2711UART1Device = record
  {UART Properties}
  UART:TUARTDevice;
  {BCM2711 Properties}
  //To Do 
 end; 
 
 {BCM2711 EMMC0/EMMC2 (SDHCI) types}
 PBCM2711EMMC0Host = ^TBCM2711EMMC0Host;
 TBCM2711EMMC0Host = record
  {SDHCI Properties}
  SDHCI:TSDHCIHost;
  {BCM2711 Properties}
  Id:LongWord;
  IRQ:LongWord;
  FIQ:LongBool;
  SDIO:LongBool;
  PowerId:LongWord;
  ClockId:LongWord;
  WriteDelay:LongWord;
  DelayClock:LongWord;
  ShadowRegister:LongWord;
 end;
 
 {BCM2711 EMMC1 (SDHOST) types}
 {See: BCMSDHOST for the driver implementation}

 {BCM2711 System Clock types}
 PBCM2711SystemClock = ^TBCM2711SystemClock;
 TBCM2711SystemClock = record
  {Clock Properties}
  Clock:TClockDevice;
  {BCM2711 Properties}
   {Nothing}
 end; 

 {BCM2711 ARM Clock types}
 PBCM2711ARMClock = ^TBCM2711ARMClock;
 TBCM2711ARMClock = record
  {Clock Properties}
  Clock:TClockDevice;
  {BCM2711 Properties}
  CoreClock:LongWord;              {Core clock rate}
 end; 
 
 {BCM2711 ARM Timer types}
 PBCM2711ARMTimer = ^TBCM2711ARMTimer;
 TBCM2711ARMTimer = record
  {Timer Properties}
  Timer:TTimerDevice;
  {BCM2711 Properties}
  CoreClock:LongWord;              {Core clock rate}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end; 
 
 {BCM2711 Local Timer types}
 PBCM2711LocalTimer = ^TBCM2711LocalTimer;
 TBCM2711LocalTimer = record
  {Timer Properties}
  Timer:TTimerDevice;
  {BCM2711 Properties}
  CoreClock:LongWord;              {Core clock rate}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end; 
 
 {BCM2711 Random types}
 PBCM2711Random = ^TBCM2711Random;
 TBCM2711Random = record
  {Random Properties}
  Random:TRandomDevice;
  {BCM2711 Properties}
   {Nothing}
 end; 

 {BCM2711 Mailbox types}
 PBCM2711Mailbox = ^TBCM2711Mailbox;
 TBCM2711Mailbox = record
  {Mailbox Properties}
  Mailbox:TMailboxDevice;
  {BCM2711 Properties}
   {Nothing}
 end; 
 
 {BCM2711 Watchdog types}
 PBCM2711Watchdog = ^TBCM2711Watchdog;
 TBCM2711Watchdog = record
  {Watchdog Properties}
  Watchdog:TWatchdogDevice;
  {BCM2711 Properties}
   {Nothing}
 end; 

 {BCM2711 Framebuffer types}
 PBCM2711Framebuffer = ^TBCM2711Framebuffer;
 TBCM2711Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {BCM2711 Properties}
  MultiDisplay:LongBool;
  DisplayNum:LongWord;
  DisplaySettings:TDisplaySettings;
 end; 

{==============================================================================}
{var}
 {BCM2711 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure BCM2711Init;
 
{==============================================================================}
{BCM2711 Functions}

{==============================================================================}
{BCM2711 SPI0/3/4/5/6 Functions}
function BCM2711SPI0Start(SPI:PSPIDevice;Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
function BCM2711SPI0Stop(SPI:PSPIDevice):LongWord;

function BCM2711SPI0WriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

function BCM2711SPI0SetMode(SPI:PSPIDevice;Mode:LongWord):LongWord;
function BCM2711SPI0SetClockRate(SPI:PSPIDevice;ChipSelect:Word;ClockRate:LongWord):LongWord;
function BCM2711SPI0SetClockPhase(SPI:PSPIDevice;ClockPhase:LongWord):LongWord;
function BCM2711SPI0SetClockPolarity(SPI:PSPIDevice;ClockPolarity:LongWord):LongWord;
function BCM2711SPI0SetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word;SelectPolarity:LongWord):LongWord;
 
procedure BCM2711SPI0ReadFIFO(SPI:PBCM2711SPI0Device);
procedure BCM2711SPI0WriteFIFO(SPI:PBCM2711SPI0Device);

function BCM2711SPI0SharedInterruptHandler(Number,CPUID,Flags:LongWord;SPI:PBCM2711SPI0Device):LongWord;
procedure BCM2711SPI0DMARequestCompleted(Request:PDMARequest); 

procedure BCM2711SPI0GetGPIOConfig(SPI:PBCM2711SPI0Device);

{==============================================================================}
{BCM2711 I2C0/1/2/3/4/5/6/7 Functions}
function BCM2711I2C0Start(I2C:PI2CDevice;Rate:LongWord):LongWord;
function BCM2711I2C0Stop(I2C:PI2CDevice):LongWord;
 
function BCM2711I2C0Read(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function BCM2711I2C0Write(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function BCM2711I2C0WriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function BCM2711I2C0WriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
 
function BCM2711I2C0SetRate(I2C:PI2CDevice;Rate:LongWord):LongWord;
 
function BCM2711I2C0SetAddress(I2C:PI2CDevice;Address:Word):LongWord;

procedure BCM2711I2C0FillFIFO(I2C:PBCM2711I2C0Device);
procedure BCM2711I2C0DrainFIFO(I2C:PBCM2711I2C0Device);

function BCM2711I2C0SharedInterruptHandler(Number,CPUID,Flags:LongWord;I2C:PBCM2711I2C0Device):LongWord;

procedure BCM2711I2C0GetGPIOConfig(I2C:PBCM2711I2C0Device);

{==============================================================================}
{BCM2711 SPI1/2 (AUX) Functions}
//To Do 

{==============================================================================}
{BCM2711 SPI/I2C Slave Functions}
function BCM2711I2CSlaveStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
function BCM2711I2CSlaveStop(I2C:PI2CDevice):LongWord;

function BCM2711I2CSlaveRead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function BCM2711I2CSlaveWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;

function BCM2711I2CSlaveSetAddress(I2C:PI2CDevice;Address:Word):LongWord;

{==============================================================================}
{BCM2711 DMA Functions}
function BCM2711DMAHostStart(DMA:PDMAHost):LongWord;
function BCM2711DMAHostStop(DMA:PDMAHost):LongWord;

function BCM2711DMAHostSubmit(DMA:PDMAHost;Request:PDMARequest):LongWord;
function BCM2711DMAHostCancel(DMA:PDMAHost;Request:PDMARequest):LongWord;

procedure BCM2711DMAInterruptHandler(Channel:PBCM2711DMAChannel);
procedure BCM2711DMA40InterruptHandler(Channel:PBCM2711DMAChannel);
procedure BCM2711DMASharedInterruptHandler(DMA:PBCM2711DMAHost);

procedure BCM2711DMARequestComplete(Channel:PBCM2711DMAChannel);

function BCM2711DMAPeripheralToDREQ(Peripheral:LongWord):LongWord;
procedure BCM2711DMADataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2838DMAControlBlock;Bulk,Lite:Boolean);
procedure BCM2711DMA40DataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2838DMA40ControlBlock);

{==============================================================================}
{BCM2711 PWM0/1 Functions}
function BCM2711PWM0Start(PWM:PPWMDevice):LongWord; 
function BCM2711PWM0Stop(PWM:PPWMDevice):LongWord; 

function BCM2711PWM0Write(PWM:PPWMDevice;Value:LongWord):LongWord; 
 
function BCM2711PWM0SetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
function BCM2711PWM0ResetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
function BCM2711PWM0SetMode(PWM:PPWMDevice;Mode:LongWord):LongWord;
function BCM2711PWM0SetRange(PWM:PPWMDevice;Range:LongWord):LongWord;
function BCM2711PWM0SetFrequency(PWM:PPWMDevice;Frequency:LongWord):LongWord;
function BCM2711PWM0SetPolarity(PWM:PPWMDevice;Polarity:LongWord):LongWord;

function BCM2711PWM0Configure(PWM:PPWMDevice;DutyNS,PeriodNS:LongWord):LongWord;

function BCM2711PWM0ClockStart(PWM:PPWMDevice;Frequency:LongWord):LongWord; 
function BCM2711PWM0ClockStop(PWM:PPWMDevice):LongWord; 
function BCM2711PWM0ClockEnabled(PWM:PPWMDevice):Boolean;

{==============================================================================}
{BCM2711 PCM Functions}
//To Do 

{==============================================================================}
{BCM2711 GPIO Functions}
function BCM2711GPIOStart(GPIO:PGPIODevice):LongWord; 
function BCM2711GPIOStop(GPIO:PGPIODevice):LongWord; 
 
function BCM2711GPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
procedure BCM2711GPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);

function BCM2711GPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function BCM2711GPIOInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
function BCM2711GPIOInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
function BCM2711GPIOInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function BCM2711GPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function BCM2711GPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function BCM2711GPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function BCM2711GPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function BCM2711GPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

procedure BCM2711GPIOInterruptHandler(Bank:PBCM2711GPIOBank);

procedure BCM2711GPIOEventTrigger(Pin:PGPIOPin);
procedure BCM2711GPIOEventTimeout(Event:PGPIOEvent);

{==============================================================================}
{BCM2711 UART0/2/3/4/5 Functions}
function BCM2711UART0Open(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
function BCM2711UART0Close(UART:PUARTDevice):LongWord;
 
function BCM2711UART0Read(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function BCM2711UART0Write(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
 
function BCM2711UART0GetStatus(UART:PUARTDevice):LongWord;
function BCM2711UART0SetStatus(UART:PUARTDevice;Status:LongWord):LongWord;

function BCM2711UART0SharedInterruptHandler(Number,CPUID,Flags:LongWord;UART:PUARTDevice):LongWord;

procedure BCM2711UART0Receive(UART:PUARTDevice);
procedure BCM2711UART0Transmit(UART:PUARTDevice);

procedure BCM2711UART0EnableInterrupt(UART:PBCM2711UART0Device;Interrupt:LongWord); 
procedure BCM2711UART0DisableInterrupt(UART:PBCM2711UART0Device;Interrupt:LongWord); 

procedure BCM2711UART0GetGPIOConfig(UART:PBCM2711UART0Device);

{==============================================================================}
{BCM2711 UART1 (AUX) Functions}
//To Do 

{==============================================================================}
{BCM2711 EMMC0 (SDHCI) Functions}
function BCM2711EMMC0HostStart(SDHCI:PSDHCIHost):LongWord;
function BCM2711EMMC0HostStop(SDHCI:PSDHCIHost):LongWord;

function BCM2711EMMC0HostLock(SDHCI:PSDHCIHost):LongWord;
function BCM2711EMMC0HostUnlock(SDHCI:PSDHCIHost):LongWord;

function BCM2711EMMC0HostSignal(SDHCI:PSDHCIHost;Semaphore:TSemaphoreHandle):LongWord;

function BCM2711EMMC0HostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
function BCM2711EMMC0HostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
function BCM2711EMMC0HostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
procedure BCM2711EMMC0HostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
procedure BCM2711EMMC0HostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
procedure BCM2711EMMC0HostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
 
function BCM2711EMMC0SharedInterruptHandler(Number,CPUID,Flags:LongWord;SDHCI:PSDHCIHost):LongWord;
function BCM2711EMMC0SetupInterrupts(SDHCI:PSDHCIHost):LongWord;

function BCM2711EMMC0DeviceGetCardDetect(MMC:PMMCDevice):LongWord;

{==============================================================================}
{BCM2711 EMMC1 (SDHOST) Functions}
{See: BCMSDHOST for the driver implementation}

{==============================================================================}
{BCM2711 EMMC2 (SDHCI) Functions}
{Handled by EMMC0 functions}

{==============================================================================}
{BCM2711 System Clock Functions}
function BCM2711SystemClockRead(Clock:PClockDevice):LongWord;
function BCM2711SystemClockRead64(Clock:PClockDevice):Int64;

{==============================================================================}
{BCM2711 ARM Clock Functions}
function BCM2711ARMClockStart(Clock:PClockDevice):LongWord;
function BCM2711ARMClockStop(Clock:PClockDevice):LongWord;

function BCM2711ARMClockRead(Clock:PClockDevice):LongWord;
function BCM2711ARMClockRead64(Clock:PClockDevice):Int64;

function BCM2711ARMClockSetRate(Clock:PClockDevice;Rate:LongWord):LongWord;

{==============================================================================}
{BCM2711 ARM Timer Functions}
function BCM2711ARMTimerStart(Timer:PTimerDevice):LongWord;
function BCM2711ARMTimerStop(Timer:PTimerDevice):LongWord;
function BCM2711ARMTimerRead64(Timer:PTimerDevice):Int64;
function BCM2711ARMTimerWait(Timer:PTimerDevice):LongWord;
function BCM2711ARMTimerEvent(Timer:PTimerDevice;Flags:LongWord;Callback:TTimerCallback;Data:Pointer):LongWord;
function BCM2711ARMTimerCancel(Timer:PTimerDevice):LongWord;
function BCM2711ARMTimerSetRate(Timer:PTimerDevice;Rate:LongWord):LongWord;
function BCM2711ARMTimerSetInterval(Timer:PTimerDevice;Interval:LongWord):LongWord;

procedure BCM2711ARMTimerInterruptHandler(Timer:PTimerDevice);

procedure BCM2711ARMTimerEventTrigger(Timer:PTimerDevice);

{==============================================================================}
{BCM2711 Local Timer Functions}
//To Do

{==============================================================================}
{BCM2711 Random Functions}
function BCM2711RandomStart(Random:PRandomDevice):LongWord;
function BCM2711RandomStop(Random:PRandomDevice):LongWord;

function BCM2711RandomReadLongWord(Random:PRandomDevice):LongWord;

{==============================================================================}
{BCM2711 Mailbox Functions}
//To Do

{==============================================================================}
{BCM2711 Watchdog Functions}
function BCM2711WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
function BCM2711WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
function BCM2711WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;

function BCM2711WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;

{==============================================================================}
{BCM2711 Framebuffer Functions}
function BCM2711FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function BCM2711FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function BCM2711FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function BCM2711FramebufferCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;

function BCM2711FramebufferWaitSync(Framebuffer:PFramebufferDevice):LongWord;
 
function BCM2711FramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;

function BCM2711FramebufferGetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
function BCM2711FramebufferSetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;

function BCM2711FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;

function BCM2711FramebufferSetCursor(Framebuffer:PFramebufferDevice;Width,Height,HotspotX,HotspotY:LongWord;Image:Pointer;Len:LongWord):LongWord;
function BCM2711FramebufferUpdateCursor(Framebuffer:PFramebufferDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;

{==============================================================================}
{BCM2711 Helper Functions}
function BCM2711SPIGetDescription(Id:LongWord):String;
function BCM2711I2CGetDescription(Id:LongWord):String;
function BCM2711I2CSlaveGetDescription(Id:LongWord):String;
function BCM2711PWMGetDescription(Id,Channel:LongWord):String;
function BCM2711UARTGetDescription(Id:LongWord):String;
  
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {BCM2711 specific variables}
 BCM2711Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
{$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
procedure BCM2711I2CSlaveFillFIFO(I2C:PBCM2711I2CSlave); forward;
{$ENDIF}
procedure BCM2711I2CSlaveDrainFIFO(I2C:PBCM2711I2CSlave); forward;

function BCM2711I2CSlaveInterruptHandler(Number,CPUID,Flags:LongWord;I2C:PBCM2711I2CSlave):LongWord;{$IFDEF i386} stdcall;{$ENDIF} forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BCM2711Init;

 procedure BCM2711RegisterSPI0(Id:LongWord);
 var
  Status:LongWord;
  BCM2711SPI0:PBCM2711SPI0Device;
 begin
  {Create SPI0}
  BCM2711SPI0:=PBCM2711SPI0Device(SPIDeviceCreateEx(SizeOf(TBCM2711SPI0Device)));
  if BCM2711SPI0 <> nil then
   begin
    {Update SPI0}
    {Device}
    BCM2711SPI0.SPI.Device.DeviceBus:=DEVICE_BUS_MMIO;
    BCM2711SPI0.SPI.Device.DeviceType:=SPI_TYPE_NONE;
    BCM2711SPI0.SPI.Device.DeviceFlags:=SPI_FLAG_4WIRE or SPI_FLAG_3WIRE or SPI_FLAG_LOSSI or SPI_FLAG_CPOL or SPI_FLAG_CPHA or SPI_FLAG_CSPOL or SPI_FLAG_NO_CS or SPI_FLAG_DMA;
    BCM2711SPI0.SPI.Device.DeviceData:=nil;
    BCM2711SPI0.SPI.Device.DeviceDescription:=BCM2711_SPI0_DESCRIPTION + IntToStr(Id);
    {SPI}
    BCM2711SPI0.SPI.SPIState:=SPI_STATE_DISABLED;
    BCM2711SPI0.SPI.SPIMode:=SPI_MODE_4WIRE;
    BCM2711SPI0.SPI.DeviceStart:=BCM2711SPI0Start;
    BCM2711SPI0.SPI.DeviceStop:=BCM2711SPI0Stop;
    BCM2711SPI0.SPI.DeviceWriteRead:=BCM2711SPI0WriteRead;
    BCM2711SPI0.SPI.DeviceSetMode:=BCM2711SPI0SetMode;
    BCM2711SPI0.SPI.DeviceSetClockRate:=BCM2711SPI0SetClockRate;
    BCM2711SPI0.SPI.DeviceSetClockPhase:=BCM2711SPI0SetClockPhase;
    BCM2711SPI0.SPI.DeviceSetClockPolarity:=BCM2711SPI0SetClockPolarity;
    BCM2711SPI0.SPI.DeviceSetSelectPolarity:=BCM2711SPI0SetSelectPolarity;
    {Driver}
    BCM2711SPI0.SPI.Properties.Flags:=BCM2711SPI0.SPI.Device.DeviceFlags;
    BCM2711SPI0.SPI.Properties.MaxSize:=BCM2711_SPI0_MAX_SIZE;
    BCM2711SPI0.SPI.Properties.MinClock:=BCM2711_SPI0_MIN_CLOCK;
    BCM2711SPI0.SPI.Properties.MaxClock:=BCM2711_SPI0_MAX_CLOCK;
    BCM2711SPI0.SPI.Properties.SelectCount:=3;
    BCM2711SPI0.SPI.Properties.Mode:=SPI_MODE_4WIRE;
    BCM2711SPI0.SPI.Properties.ClockRate:=0;
    BCM2711SPI0.SPI.Properties.ClockPhase:=SPI_CLOCK_PHASE_UNKNOWN;
    BCM2711SPI0.SPI.Properties.ClockPolarity:=SPI_CLOCK_POLARITY_UNKNOWN;
    BCM2711SPI0.SPI.Properties.SelectPolarity:=SPI_CS_POLARITY_UNKNOWN;
    {BCM2711}
    BCM2711SPI0.Id:=Id;
    BCM2711SPI0.IRQ:=BCM2838_SPI0_IRQS[Id];
    BCM2711SPI0.Address:=Pointer(BCM2838_SPI0_REGS_BASES[Id]);
    BCM2711SPI0.CoreClock:=BCM2711_SPI0_CORE_CLOCK;
    BCM2711SPI0GetGPIOConfig(BCM2711SPI0);
    
    {Register SPI0}
    Status:=SPIDeviceRegister(@BCM2711SPI0.SPI);
    if Status <> ERROR_SUCCESS then
     begin
      if SPI_LOG_ENABLED then SPILogError(nil,'BCM2711: Failed to register new SPI0 device: ' + ErrorToString(Status));

      {Destroy SPI0}
      SPIDeviceDestroy(@BCM2711SPI0.SPI);
     end;
   end
  else 
   begin
    if SPI_LOG_ENABLED then SPILogError(nil,'BCM2711: Failed to create new SPI0 device');
   end; 
 end;
 
 procedure BCM2711RegisterI2C0(Id:LongWord);
 var
  Status:LongWord;
  BCM2711I2C0:PBCM2711I2C0Device;
 begin
  {Create I2C0}
  BCM2711I2C0:=PBCM2711I2C0Device(I2CDeviceCreateEx(SizeOf(TBCM2711I2C0Device)));
  if BCM2711I2C0 <> nil then
   begin
    {Update I2C0}
    {Device}
    BCM2711I2C0.I2C.Device.DeviceBus:=DEVICE_BUS_MMIO;
    BCM2711I2C0.I2C.Device.DeviceType:=I2C_TYPE_MASTER;
    BCM2711I2C0.I2C.Device.DeviceFlags:=BCM2711I2C0.I2C.Device.DeviceFlags or I2C_FLAG_10BIT; {Don't override defaults}
    BCM2711I2C0.I2C.Device.DeviceData:=nil;
    BCM2711I2C0.I2C.Device.DeviceDescription:=BCM2711_I2C0_DESCRIPTION + IntToStr(Id);
    {I2C}
    BCM2711I2C0.I2C.I2CState:=I2C_STATE_DISABLED;
    BCM2711I2C0.I2C.DeviceStart:=BCM2711I2C0Start;
    BCM2711I2C0.I2C.DeviceStop:=BCM2711I2C0Stop;
    BCM2711I2C0.I2C.DeviceRead:=BCM2711I2C0Read;
    BCM2711I2C0.I2C.DeviceWrite:=BCM2711I2C0Write;
    BCM2711I2C0.I2C.DeviceWriteRead:=BCM2711I2C0WriteRead;
    BCM2711I2C0.I2C.DeviceWriteWrite:=BCM2711I2C0WriteWrite;
    BCM2711I2C0.I2C.DeviceSetRate:=BCM2711I2C0SetRate;
    BCM2711I2C0.I2C.DeviceSetAddress:=BCM2711I2C0SetAddress;
    {Driver}
    BCM2711I2C0.I2C.Properties.Flags:=BCM2711I2C0.I2C.Device.DeviceFlags;
    BCM2711I2C0.I2C.Properties.MaxSize:=BCM2711_I2C0_MAX_SIZE;
    BCM2711I2C0.I2C.Properties.MinClock:=BCM2711_I2C0_MIN_CLOCK;
    BCM2711I2C0.I2C.Properties.MaxClock:=BCM2711_I2C0_MAX_CLOCK;
    BCM2711I2C0.I2C.Properties.ClockRate:=0;
    BCM2711I2C0.I2C.Properties.SlaveAddress:=I2C_ADDRESS_INVALID;
    {BCM2711}
    BCM2711I2C0.Id:=Id;
    BCM2711I2C0.IRQ:=BCM2838_I2C0_IRQS[Id];
    BCM2711I2C0.Address:=Pointer(BCM2838_I2C0_REGS_BASES[Id]);
    BCM2711I2C0.CoreClock:=BCM2711_I2C0_CORE_CLOCK;
    BCM2711I2C0GetGPIOConfig(BCM2711I2C0);
    
    {Register I2C0}
    Status:=I2CDeviceRegister(@BCM2711I2C0.I2C);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2711: Failed to register new I2C0 device: ' + ErrorToString(Status));

      {Destroy I2C0}
      I2CDeviceDestroy(@BCM2711I2C0.I2C);
     end;
   end
  else 
   begin
    if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2711: Failed to create new I2C0 device');
   end; 
 end;
 
 procedure BCM2711RegisterPWM0(Id:LongWord);
 var
  Status:LongWord;
  BCM2711PWM0_0:PBCM2711PWM0Device;
  BCM2711PWM0_1:PBCM2711PWM0Device;
 begin
  {Create PWM0_0}
  BCM2711PWM0_0:=PBCM2711PWM0Device(PWMDeviceCreateEx(SizeOf(TBCM2711PWM0Device)));
  if BCM2711PWM0_0 <> nil then
   begin
    {Update PWM0_0}
    {Device}
    BCM2711PWM0_0.PWM.Device.DeviceBus:=DEVICE_BUS_MMIO;
    BCM2711PWM0_0.PWM.Device.DeviceType:=PWM_TYPE_NONE;
    BCM2711PWM0_0.PWM.Device.DeviceFlags:=PWM_FLAG_GPIO or PWM_FLAG_MODE or PWM_FLAG_RANGE or PWM_FLAG_FREQUENCY or PWM_FLAG_POLARITY;
    BCM2711PWM0_0.PWM.Device.DeviceData:=nil;
    BCM2711PWM0_0.PWM.Device.DeviceDescription:=BCM2711_PWM0_DESCRIPTION + IntToStr(Id) + '_0';
    {PWM}
    BCM2711PWM0_0.PWM.PWMState:=PWM_STATE_DISABLED;
    BCM2711PWM0_0.PWM.DeviceStart:=BCM2711PWM0Start;
    BCM2711PWM0_0.PWM.DeviceStop:=BCM2711PWM0Stop;
    BCM2711PWM0_0.PWM.DeviceWrite:=BCM2711PWM0Write;
    BCM2711PWM0_0.PWM.DeviceSetGPIO:=BCM2711PWM0SetGPIO;
    BCM2711PWM0_0.PWM.DeviceSetMode:=BCM2711PWM0SetMode;
    BCM2711PWM0_0.PWM.DeviceSetRange:=BCM2711PWM0SetRange;
    BCM2711PWM0_0.PWM.DeviceSetFrequency:=BCM2711PWM0SetFrequency;
    BCM2711PWM0_0.PWM.DeviceSetPolarity:=BCM2711PWM0SetPolarity;
    BCM2711PWM0_0.PWM.DeviceConfigure:=BCM2711PWM0Configure;
    {Driver}
    BCM2711PWM0_0.PWM.Properties.Flags:=BCM2711PWM0_0.PWM.Device.DeviceFlags;
    BCM2711PWM0_0.PWM.Properties.GPIO:=GPIO_PIN_UNKNOWN;
    BCM2711PWM0_0.PWM.Properties.Mode:=PWM_MODE_MARKSPACE;
    BCM2711PWM0_0.PWM.Properties.Range:=0;
    BCM2711PWM0_0.PWM.Properties.Frequency:=0;
    BCM2711PWM0_0.PWM.Properties.Polarity:=PWM_POLARITY_NORMAL;
    BCM2711PWM0_0.PWM.Properties.DutyNS:=0;
    BCM2711PWM0_0.PWM.Properties.PeriodNS:=0;
    BCM2711PWM0_0.PWM.Properties.MinPeriod:=BCM2711_PWM0_MIN_PERIOD;
    {BCM2711}
    BCM2711PWM0_0.Id:=Id;
    BCM2711PWM0_0.Address:=Pointer(BCM2838_PWM0_REGS_BASES[Id]);
    BCM2711PWM0_0.Channel:=0; {PWM Channel 1)}
    
    {Register PWM0_0}
    Status:=PWMDeviceRegister(@BCM2711PWM0_0.PWM);
    if Status <> ERROR_SUCCESS then
     begin
      if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2711: Failed to register new PWM0_0 device: ' + ErrorToString(Status));

      {Destroy PWM0_0}
      PWMDeviceDestroy(@BCM2711PWM0_0.PWM);
     end;
   end
  else 
   begin
    if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2711: Failed to create new PWM0_0 device');
   end; 

  {Create PWM0_1}
  BCM2711PWM0_1:=PBCM2711PWM0Device(PWMDeviceCreateEx(SizeOf(TBCM2711PWM0Device)));
  if BCM2711PWM0_1 <> nil then
   begin
    {Update PWM0_1}
    {Device}
    BCM2711PWM0_1.PWM.Device.DeviceBus:=DEVICE_BUS_MMIO;
    BCM2711PWM0_1.PWM.Device.DeviceType:=PWM_TYPE_NONE;
    BCM2711PWM0_1.PWM.Device.DeviceFlags:=PWM_FLAG_GPIO or PWM_FLAG_MODE or PWM_FLAG_RANGE or PWM_FLAG_FREQUENCY or PWM_FLAG_POLARITY;
    BCM2711PWM0_1.PWM.Device.DeviceData:=nil;
    BCM2711PWM0_1.PWM.Device.DeviceDescription:=BCM2711_PWM0_DESCRIPTION + IntToStr(Id) + '_1';
    {PWM}
    BCM2711PWM0_1.PWM.PWMState:=PWM_STATE_DISABLED;
    BCM2711PWM0_1.PWM.DeviceStart:=BCM2711PWM0Start;
    BCM2711PWM0_1.PWM.DeviceStop:=BCM2711PWM0Stop;
    BCM2711PWM0_1.PWM.DeviceWrite:=BCM2711PWM0Write;
    BCM2711PWM0_1.PWM.DeviceSetGPIO:=BCM2711PWM0SetGPIO;
    BCM2711PWM0_1.PWM.DeviceSetMode:=BCM2711PWM0SetMode;
    BCM2711PWM0_1.PWM.DeviceSetRange:=BCM2711PWM0SetRange;
    BCM2711PWM0_1.PWM.DeviceSetFrequency:=BCM2711PWM0SetFrequency;
    BCM2711PWM0_1.PWM.DeviceSetPolarity:=BCM2711PWM0SetPolarity;
    BCM2711PWM0_1.PWM.DeviceConfigure:=BCM2711PWM0Configure;
    {Driver}
    BCM2711PWM0_1.PWM.Properties.Flags:=BCM2711PWM0_1.PWM.Device.DeviceFlags;
    BCM2711PWM0_1.PWM.Properties.GPIO:=GPIO_PIN_UNKNOWN;
    BCM2711PWM0_1.PWM.Properties.Mode:=PWM_MODE_MARKSPACE;
    BCM2711PWM0_1.PWM.Properties.Range:=0;
    BCM2711PWM0_1.PWM.Properties.Frequency:=0;
    BCM2711PWM0_1.PWM.Properties.Polarity:=PWM_POLARITY_NORMAL;
    BCM2711PWM0_1.PWM.Properties.DutyNS:=0;
    BCM2711PWM0_1.PWM.Properties.PeriodNS:=0;
    BCM2711PWM0_1.PWM.Properties.MinPeriod:=BCM2711_PWM0_MIN_PERIOD;
    {BCM2711}
    BCM2711PWM0_1.Id:=Id;
    BCM2711PWM0_1.Address:=Pointer(BCM2838_PWM0_REGS_BASES[Id]);
    BCM2711PWM0_1.Channel:=1; {PWM Channel 2)}
    
    {Register PWM0_1}
    Status:=PWMDeviceRegister(@BCM2711PWM0_1.PWM);
    if Status <> ERROR_SUCCESS then
     begin
      if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2711: Failed to register new PWM0_1 device: ' + ErrorToString(Status));

      {Destroy PWM0_1}
      PWMDeviceDestroy(@BCM2711PWM0_1.PWM);
     end;
   end
  else 
   begin
    if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2711: Failed to create new PWM0_1 device');
   end; 
 
  {Create PWM Audio}
  if BCM2711_REGISTER_PWMAUDIO then
   begin
    //To Do //BCM2711PWM0_0.Audio:=
    //To Do //BCM2711PWM0_1.Audio:=
    //To Do //BCM2711PWMAudio.PWM0/PWM1:=
   end;
 end;

 procedure BCM2711RegisterUART0(Id:LongWord);
 var
  Status:LongWord;
  BCM2711UART0:PBCM2711UART0Device;
 begin
  {Create UART0}
  BCM2711UART0:=PBCM2711UART0Device(UARTDeviceCreateEx(SizeOf(TBCM2711UART0Device)));
  if BCM2711UART0 <> nil then
   begin
    {Update UART0}
    {Device}
    BCM2711UART0.UART.Device.DeviceBus:=DEVICE_BUS_MMIO; 
    BCM2711UART0.UART.Device.DeviceType:=UART_TYPE_16650;
    BCM2711UART0.UART.Device.DeviceFlags:=UART_FLAG_DATA_8BIT or UART_FLAG_DATA_7BIT or UART_FLAG_DATA_6BIT or UART_FLAG_DATA_5BIT or UART_FLAG_STOP_1BIT or UART_FLAG_STOP_2BIT or UART_FLAG_PARITY_ODD or UART_FLAG_PARITY_EVEN or UART_FLAG_FLOW_RTS_CTS or UART_FLAG_PUSH_RX;
    BCM2711UART0.UART.Device.DeviceData:=nil;
    BCM2711UART0.UART.Device.DeviceDescription:=BCM2711_UART0_DESCRIPTION + IntToStr(Id);
    {UART}
    BCM2711UART0.UART.UARTMode:=UART_MODE_NONE;
    BCM2711UART0.UART.UARTState:=UART_STATE_DISABLED;
    BCM2711UART0.UART.UARTStatus:=UART_STATUS_NONE;
    BCM2711UART0.UART.DeviceOpen:=BCM2711UART0Open;
    BCM2711UART0.UART.DeviceClose:=BCM2711UART0Close;
    BCM2711UART0.UART.DeviceRead:=BCM2711UART0Read;
    BCM2711UART0.UART.DeviceWrite:=BCM2711UART0Write;
    BCM2711UART0.UART.DeviceGetStatus:=BCM2711UART0GetStatus;
    BCM2711UART0.UART.DeviceSetStatus:=BCM2711UART0SetStatus;
    {Driver}
    BCM2711UART0.UART.Properties.Flags:=BCM2711UART0.UART.Device.DeviceFlags;
    BCM2711UART0.UART.Properties.MinRate:=BCM2711_UART0_MIN_BAUD;
    BCM2711UART0.UART.Properties.MaxRate:=BCM2711_UART0_MAX_BAUD;
    BCM2711UART0.UART.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
    BCM2711UART0.UART.Properties.DataBits:=SERIAL_DATA_8BIT;
    BCM2711UART0.UART.Properties.StopBits:=SERIAL_STOP_1BIT;
    BCM2711UART0.UART.Properties.Parity:=SERIAL_PARITY_NONE;
    BCM2711UART0.UART.Properties.FlowControl:=SERIAL_FLOW_NONE;
    {BCM2711}
    BCM2711UART0.Id:=Id;
    BCM2711UART0.IRQ:= BCM2838_UART0_IRQS[Id];
    BCM2711UART0.Lock:=INVALID_HANDLE_VALUE;
    BCM2711UART0.Address:=Pointer(BCM2838_UART0_REGS_BASES[Id]);
    BCM2711UART0.ClockRate:=BCM2711_UART0_CLOCK_RATE;
    BCM2711UART0GetGPIOConfig(BCM2711UART0);
    
    {Register UART0}
    Status:=UARTDeviceRegister(@BCM2711UART0.UART);
    if Status <> ERROR_SUCCESS then
     begin
      if UART_LOG_ENABLED then UARTLogError(nil,'BCM2711: Failed to register new UART0 device: ' + ErrorToString(Status));

      {Destroy UART0}
      UARTDeviceDestroy(@BCM2711UART0.UART);
     end;
   end
  else 
   begin
    if UART_LOG_ENABLED then UARTLogError(nil,'BCM2711: Failed to create new UART0 device');
   end;
 end;
 
var
 Status:LongWord;

 ClockMaximum:LongWord;

 DisplayId:LongWord;
 DisplayNum:LongWord;
 DisplayCount:LongWord;
 MultiDisplay:Boolean;
 
 BCM2711DMAHost:PBCM2711DMAHost;
 BCM2711GPIO:PBCM2711GPIODevice;
 BCM2711I2CSlave:PBCM2711I2CSlave;
 BCM2711EMMC0Host:PBCM2711EMMC0Host; 
 BCM2711EMMC2Host:PBCM2711EMMC0Host; 
 
 BCM2711SystemClock:PBCM2711SystemClock;
 BCM2711ARMClock:PBCM2711ARMClock;
 BCM2711ARMTimer:PBCM2711ARMTimer;
 BCM2711LocalTimer:PBCM2711LocalTimer;
 BCM2711Random:PBCM2711Random;
 BCM2711Mailbox:PBCM2711Mailbox;
 BCM2711Watchdog:PBCM2711Watchdog;
 BCM2711Framebuffer:PBCM2711Framebuffer;
begin
 {}
 {Check Initialized}
 if BCM2711Initialized then Exit;

 {Initialize BCM2711GPIO_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2711GPIO_FIQ_ENABLED:=False;
 
 {Initialize BCM2711EMMC0_FIQ_ENABLED/BCM2711EMMC1_FIQ_ENABLED/BCM2711EMMC2_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2711EMMC0_FIQ_ENABLED:=False;
 if not(FIQ_ENABLED) then BCM2711EMMC1_FIQ_ENABLED:=False;
 if not(FIQ_ENABLED) then BCM2711EMMC2_FIQ_ENABLED:=False;

 {Initialize BCM2711ARM_TIMER_FIQ_ENABLED/BCM2711LOCAL_TIMER_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2711ARM_TIMER_FIQ_ENABLED:=False;
 if not(FIQ_ENABLED) then BCM2711LOCAL_TIMER_FIQ_ENABLED:=False;
 
 {Initialize BCM2711EMMC0/2}
 if BCM2711_REGISTER_SDIO then BCM2711_REGISTER_EMMC0:=False;
 if BCM2711_REGISTER_EMMC0 then BCM2711_REGISTER_EMMC2:=False;
 if BCM2711EMMC0_FIQ_ENABLED then BCM2711EMMC2_FIQ_ENABLED:=True;
 if BCM2711EMMC2_FIQ_ENABLED then BCM2711EMMC0_FIQ_ENABLED:=True;

 {Register Platform Handlers}
 SPIGetDescriptionHandler:=BCM2711SPIGetDescription;
 I2CGetDescriptionHandler:=BCM2711I2CGetDescription;
 I2CSlaveGetDescriptionHandler:=BCM2711I2CSlaveGetDescription;
 PWMGetDescriptionHandler:=BCM2711PWMGetDescription;
 UARTGetDescriptionHandler:=BCM2711UARTGetDescription;

 {$IFNDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPi4GPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPi4GPUMemoryRelease;
 GPUMemoryLockHandler:=RPi4GPUMemoryLock;
 GPUMemoryUnlockHandler:=RPi4GPUMemoryUnlock;
 
 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPi4GPUExecuteCode;
 DispmanxHandleGetHandler:=RPi4DispmanxHandleGet;
 EDIDBlockGetHandler:=RPi4EDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPi4FramebufferAllocate;
 FramebufferReleaseHandler:=RPi4FramebufferRelease;
 FramebufferSetStateHandler:=RPi4FramebufferSetState;

 FramebufferGetDimensionsHandler:=RPi4FramebufferGetDimensions;
 
 FramebufferGetPhysicalHandler:=RPi4FramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPi4FramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPi4FramebufferTestPhysical;
 
 FramebufferGetVirtualHandler:=RPi4FramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPi4FramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPi4FramebufferTestVirtual;
 
 FramebufferGetDepthHandler:=RPi4FramebufferGetDepth;
 FramebufferSetDepthHandler:=RPi4FramebufferSetDepth;
 FramebufferTestDepthHandler:=RPi4FramebufferTestDepth;
 
 FramebufferGetPixelOrderHandler:=RPi4FramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPi4FramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPi4FramebufferTestPixelOrder;
 
 FramebufferGetAlphaModeHandler:=RPi4FramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPi4FramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPi4FramebufferTestAlphaMode;
 
 FramebufferGetPitchHandler:=RPi4FramebufferGetPitch;
 
 FramebufferGetOffsetHandler:=RPi4FramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPi4FramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPi4FramebufferTestOffset;
 
 FramebufferGetOverscanHandler:=RPi4FramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPi4FramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPi4FramebufferTestOverscan;
 
 FramebufferGetPaletteHandler:=RPi4FramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPi4FramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPi4FramebufferTestPalette;

 FramebufferTestVsyncHandler:=RPi4FramebufferTestVsync;
 FramebufferSetVsyncHandler:=RPi4FramebufferSetVsync;
 
 FramebufferSetBacklightHandler:=RPi4FramebufferSetBacklight;
 
 FramebufferGetNumDisplaysHandler:=RPi4FramebufferGetNumDisplays;
 FramebufferGetDisplayIdHandler:=RPi4FramebufferGetDisplayId;
 FramebufferSetDisplayNumHandler:=RPi4FramebufferSetDisplayNum;
 FramebufferGetDisplaySettingsHandler:=RPi4FramebufferGetDisplaySettings;
 FramebufferDisplayIdToNameHandler:=RPi4FramebufferDisplayIdToName;
 
 {Register Platform Touch Handlers}
 TouchGetBufferHandler:=RPi4TouchGetBuffer;
 TouchSetBufferHandler:=RPi4TouchSetBuffer;
 
 {Register Platform Cursor Handlers}
 CursorSetDefaultHandler:=RPi4CursorSetDefault;
 CursorSetInfoHandler:=RPi4CursorSetInfo;
 CursorSetStateHandler:=RPi4CursorSetState;
 {$ENDIF}
  
 {Create DMA}
 if BCM2711_REGISTER_DMA then
  begin
   BCM2711DMAHost:=PBCM2711DMAHost(DMAHostCreateEx(SizeOf(TBCM2711DMAHost)));
   if BCM2711DMAHost <> nil then
    begin
     {Update DMA}
     {Device}
     BCM2711DMAHost.DMA.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711DMAHost.DMA.Device.DeviceType:=DMA_TYPE_NONE;
     BCM2711DMAHost.DMA.Device.DeviceFlags:=DMA_FLAG_STRIDE or DMA_FLAG_DREQ or DMA_FLAG_NOINCREMENT or DMA_FLAG_NOREAD or DMA_FLAG_NOWRITE or DMA_FLAG_WIDE or DMA_FLAG_BULK or DMA_FLAG_LITE or DMA_FLAG_40BIT;
     BCM2711DMAHost.DMA.Device.DeviceData:=nil;
     BCM2711DMAHost.DMA.Device.DeviceDescription:=BCM2711_DMA_DESCRIPTION;
     if BCM2711DMA_SHARED_MEMORY then BCM2711DMAHost.DMA.Device.DeviceFlags:=BCM2711DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_SHARED;
     if BCM2711DMA_NOCACHE_MEMORY then BCM2711DMAHost.DMA.Device.DeviceFlags:=BCM2711DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_NOCACHE;
     if BCM2711DMA_CACHE_COHERENT then BCM2711DMAHost.DMA.Device.DeviceFlags:=BCM2711DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_COHERENT;
     {DMA}
     BCM2711DMAHost.DMA.DMAState:=DMA_STATE_DISABLED;
     BCM2711DMAHost.DMA.HostStart:=BCM2711DMAHostStart;
     BCM2711DMAHost.DMA.HostStop:=BCM2711DMAHostStop;
     BCM2711DMAHost.DMA.HostReset:=nil;
     BCM2711DMAHost.DMA.HostSubmit:=BCM2711DMAHostSubmit;
     BCM2711DMAHost.DMA.HostCancel:=BCM2711DMAHostCancel;
     BCM2711DMAHost.DMA.HostProperties:=nil;
     BCM2711DMAHost.DMA.Alignment:=BCM2711DMA_ALIGNMENT;
     BCM2711DMAHost.DMA.Multiplier:=BCM2711DMA_MULTIPLIER;
     BCM2711DMAHost.DMA.Properties.Flags:=BCM2711DMAHost.DMA.Device.DeviceFlags;
     BCM2711DMAHost.DMA.Properties.Alignment:=BCM2711DMAHost.DMA.Alignment;
     BCM2711DMAHost.DMA.Properties.Multiplier:=BCM2711DMAHost.DMA.Multiplier;
     BCM2711DMAHost.DMA.Properties.Channels:=BCM2711_DMA_CHANNEL_COUNT;
     BCM2711DMAHost.DMA.Properties.MaxSize:=BCM2711_DMA_MAX_NORMAL_TRANSFER;
     BCM2711DMAHost.DMA.Properties.MaxCount:=BCM2711_DMA_MAX_Y_COUNT;
     BCM2711DMAHost.DMA.Properties.MaxLength:=BCM2711_DMA_MAX_X_LENGTH;
     BCM2711DMAHost.DMA.Properties.MinStride:=-32768;
     BCM2711DMAHost.DMA.Properties.MaxStride:=32767;
     {BCM2711}
     BCM2711DMAHost.ChannelLock:=INVALID_HANDLE_VALUE;
     BCM2711DMAHost.ChannelWait:=INVALID_HANDLE_VALUE;
     BCM2711DMAHost.Channel40:=INVALID_HANDLE_VALUE;
     BCM2711DMAHost.ChannelLite:=INVALID_HANDLE_VALUE;
     BCM2711DMAHost.ChannelBulk:=INVALID_HANDLE_VALUE;
     
     {Register DMA}
     Status:=DMAHostRegister(@BCM2711DMAHost.DMA);
     if Status = ERROR_SUCCESS then
      begin
       {Start DMA}
       Status:=DMAHostStart(@BCM2711DMAHost.DMA);
       if Status <> ERROR_SUCCESS then
        begin
         if DMA_LOG_ENABLED then DMALogError(nil,'BCM2711: Failed to start new DMA host: ' + ErrorToString(Status));

         {Deregister DMA}
         DMAHostDeregister(@BCM2711DMAHost.DMA);

         {Destroy DMA}
         DMAHostDestroy(@BCM2711DMAHost.DMA);
        end;
      end
     else
      begin
       if DMA_LOG_ENABLED then DMALogError(nil,'BCM2711: Failed to register new DMA host: ' + ErrorToString(Status));

       {Destroy DMA}
       DMAHostDestroy(@BCM2711DMAHost.DMA);
      end;
    end
   else 
    begin
     if DMA_LOG_ENABLED then DMALogError(nil,'BCM2711: Failed to create new DMA host');
    end;
  end;
  
 {Create PCM}
 if BCM2711_REGISTER_PCM then
  begin
   //To Do
  end;
  
 {Create GPIO}
 if BCM2711_REGISTER_GPIO then
  begin
   BCM2711GPIO:=PBCM2711GPIODevice(GPIODeviceCreateEx(SizeOf(TBCM2711GPIODevice)));
   if BCM2711GPIO <> nil then
    begin
     {Update GPIO}
     {Device}
     BCM2711GPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711GPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
     BCM2711GPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP or GPIO_FLAG_PULL_DOWN or GPIO_FLAG_TRIGGER_LOW or GPIO_FLAG_TRIGGER_HIGH or GPIO_FLAG_TRIGGER_RISING or GPIO_FLAG_TRIGGER_FALLING or GPIO_FLAG_TRIGGER_ASYNC or GPIO_FLAG_TRIGGER_EDGE;
     BCM2711GPIO.GPIO.Device.DeviceData:=nil;
     BCM2711GPIO.GPIO.Device.DeviceDescription:=BCM2711_GPIO_DESCRIPTION;
     {GPIO}
     BCM2711GPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
     BCM2711GPIO.GPIO.DeviceStart:=BCM2711GPIOStart;
     BCM2711GPIO.GPIO.DeviceStop:=BCM2711GPIOStop;
     BCM2711GPIO.GPIO.DeviceRead:=BCM2711GPIORead;
     BCM2711GPIO.GPIO.DeviceWrite:=BCM2711GPIOWrite;
     BCM2711GPIO.GPIO.DeviceInputGet:=BCM2711GPIOInputGet;
     BCM2711GPIO.GPIO.DeviceInputWait:=BCM2711GPIOInputWait; 
     BCM2711GPIO.GPIO.DeviceInputEvent:=BCM2711GPIOInputEvent;
     BCM2711GPIO.GPIO.DeviceInputCancel:=BCM2711GPIOInputCancel;
     BCM2711GPIO.GPIO.DeviceOutputSet:=BCM2711GPIOOutputSet;
     BCM2711GPIO.GPIO.DevicePullGet:=BCM2711GPIOPullGet;  
     BCM2711GPIO.GPIO.DevicePullSelect:=BCM2711GPIOPullSelect;  
     BCM2711GPIO.GPIO.DeviceFunctionGet:=BCM2711GPIOFunctionGet;
     BCM2711GPIO.GPIO.DeviceFunctionSelect:=BCM2711GPIOFunctionSelect;    
     {Driver}
     BCM2711GPIO.GPIO.Address:=Pointer(BCM2838_GPIO_REGS_BASE);
     BCM2711GPIO.GPIO.Properties.Flags:=BCM2711GPIO.GPIO.Device.DeviceFlags;
     BCM2711GPIO.GPIO.Properties.PinMin:=BCM2711_GPIO_MIN_PIN;
     BCM2711GPIO.GPIO.Properties.PinMax:=BCM2711_GPIO_MAX_PIN;
     BCM2711GPIO.GPIO.Properties.PinCount:=BCM2838_GPIO_PIN_COUNT;
     BCM2711GPIO.GPIO.Properties.FunctionMin:=BCM2711_GPIO_MIN_FUNCTION;
     BCM2711GPIO.GPIO.Properties.FunctionMax:=BCM2711_GPIO_MAX_FUNCTION;
     BCM2711GPIO.GPIO.Properties.FunctionCount:=8;
     {BCM2711}
     BCM2711GPIO.Lock:=INVALID_HANDLE_VALUE;
     
     {Register GPIO}
     Status:=GPIODeviceRegister(@BCM2711GPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Start GPIO}
       Status:=GPIODeviceStart(@BCM2711GPIO.GPIO);
       if Status <> ERROR_SUCCESS then
        begin
         if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2711: Failed to start new GPIO device: ' + ErrorToString(Status));

         {Deregister GPIO}
         GPIODeviceDeregister(@BCM2711GPIO.GPIO);

         {Destroy GPIO}
         GPIODeviceDestroy(@BCM2711GPIO.GPIO);
        end;
      end
     else
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2711: Failed to register new GPIO device: ' + ErrorToString(Status));

       {Destroy GPIO}
       GPIODeviceDestroy(@BCM2711GPIO.GPIO);
      end;
    end
   else 
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2711: Failed to create new GPIO device');
    end;
  end;
 
 {Create SPI0/3/4/5/6}
 if BCM2711_REGISTER_SPI0 then
  begin
   BCM2711RegisterSPI0(0);
  end; 
 
 if BCM2711_REGISTER_SPI3 then
  begin
   BCM2711RegisterSPI0(3);
  end; 
 
 if BCM2711_REGISTER_SPI4 then
  begin
   BCM2711RegisterSPI0(4);
  end; 
 
 if BCM2711_REGISTER_SPI5 then
  begin
   BCM2711RegisterSPI0(5);
  end; 
 
 if BCM2711_REGISTER_SPI6 then
  begin
   BCM2711RegisterSPI0(6);
  end; 

 {Create I2C0/1/2/3/4/5/6/7}
 if BCM2711_REGISTER_I2C0 then
  begin
   BCM2711RegisterI2C0(0);
  end; 

 if BCM2711_REGISTER_I2C1 then
  begin
   BCM2711RegisterI2C0(1);
  end; 

 if BCM2711_REGISTER_I2C2 then
  begin
   BCM2711RegisterI2C0(2);
  end; 

 if BCM2711_REGISTER_I2C3 then
  begin
   BCM2711RegisterI2C0(3);
  end; 

 if BCM2711_REGISTER_I2C4 then
  begin
   BCM2711RegisterI2C0(4);
  end; 

 if BCM2711_REGISTER_I2C5 then
  begin
   BCM2711RegisterI2C0(5);
  end; 

 if BCM2711_REGISTER_I2C6 then
  begin
   BCM2711RegisterI2C0(6);
  end; 

 if BCM2711_REGISTER_I2C7 then
  begin
   BCM2711RegisterI2C0(7);
  end; 

 {Create PWM0/1}
 if BCM2711_REGISTER_PWM0 then
  begin
   BCM2711RegisterPWM0(0);
  end;
  
 if BCM2711_REGISTER_PWM1 then
  begin
   BCM2711RegisterPWM0(1);
  end;

 {Create UART0/2/3/4/5}
 if BCM2711_REGISTER_UART0 then
  begin
   BCM2711RegisterUART0(0);
  end; 
 
 if BCM2711_REGISTER_UART2 then
  begin
   BCM2711RegisterUART0(2);
  end; 

 if BCM2711_REGISTER_UART3 then
  begin
   BCM2711RegisterUART0(3);
  end; 

 if BCM2711_REGISTER_UART4 then
  begin
   BCM2711RegisterUART0(4);
  end; 
  
 if BCM2711_REGISTER_UART5 then
  begin
   BCM2711RegisterUART0(5);
  end; 
  
 {Create SPI1/2}
 if BCM2711_REGISTER_SPI1 then
  begin
   //To Do
  end;

 if BCM2711_REGISTER_SPI2 then
  begin
   //To Do
  end;
  
 {Create UART1}
 if BCM2711_REGISTER_UART1 then
  begin
   //To Do
  end;
  
 {Create I2C Slave}
 if BCM2708_REGISTER_I2CSLAVE then
  begin
   BCM2711I2CSlave:=PBCM2711I2CSlave(I2CSlaveCreateEx(SizeOf(TBCM2711I2CSlave)));
   if BCM2711I2CSlave <> nil then
    begin
     {Update I2C Slave}
     {Device}
     BCM2711I2CSlave.I2C.Device.DeviceBus:=DEVICE_BUS_MMIO;
     BCM2711I2CSlave.I2C.Device.DeviceType:=I2C_TYPE_SLAVE;
     BCM2711I2CSlave.I2C.Device.DeviceFlags:=BCM2711I2CSlave.I2C.Device.DeviceFlags; {Don't override defaults}
     BCM2711I2CSlave.I2C.Device.DeviceData:=nil;
     BCM2711I2CSlave.I2C.Device.DeviceDescription:=BCM2711_I2CSLAVE_DESCRIPTION;
     {I2C}
     BCM2711I2CSlave.I2C.I2CState:=I2C_STATE_DISABLED;
     BCM2711I2CSlave.I2C.DeviceStart:=BCM2711I2CSlaveStart;
     BCM2711I2CSlave.I2C.DeviceStop:=BCM2711I2CSlaveStop;
     BCM2711I2CSlave.I2C.DeviceRead:=BCM2711I2CSlaveRead;
     BCM2711I2CSlave.I2C.DeviceWrite:=BCM2711I2CSlaveWrite;
     BCM2711I2CSlave.I2C.DeviceSetAddress:=BCM2711I2CSlaveSetAddress;
     {Driver}
     BCM2711I2CSlave.I2C.Properties.Flags:=BCM2711I2CSlave.I2C.Device.DeviceFlags;
     BCM2711I2CSlave.I2C.Properties.SlaveAddress:=I2C_ADDRESS_INVALID;
     {BCM2711}
     BCM2711I2CSlave.IRQ:=BCM2838_IRQ_I2CSPI;
     BCM2711I2CSlave.Address:=Pointer(BCM2838_I2CSPI_REGS_BASE);
     BCM2711I2CSlave.Lock:=INVALID_HANDLE_VALUE;
     BCM2711I2CSlave.SDAPin:=GPIO_PIN_10;
     BCM2711I2CSlave.SCLPin:=GPIO_PIN_11;
     BCM2711I2CSlave.SDAFunction:=GPIO_FUNCTION_ALT3;
     BCM2711I2CSlave.SCLFunction:=GPIO_FUNCTION_ALT3;
     {Transfer}
     BCM2711I2CSlave.Receive.Wait:=INVALID_HANDLE_VALUE;
     {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
     BCM2711I2CSlave.Transmit.Wait:=INVALID_HANDLE_VALUE;
     {$ENDIF}

     {Register I2C Slave}
     Status:=I2CSlaveRegister(@BCM2711I2CSlave.I2C);
     if Status <> ERROR_SUCCESS then
      begin
       if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2711: Failed to register new I2C Slave device: ' + ErrorToString(Status));

       {Destroy I2C Slave}
       I2CSlaveDestroy(@BCM2711I2CSlave.I2C);
      end;
    end
   else 
    begin
     if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2711: Failed to create new I2C Slave device');
    end; 
  end;

 {Create EMMC0}
 if BCM2711_REGISTER_EMMC0 or BCM2711_REGISTER_SDIO then
  begin
   BCM2711EMMC0Host:=PBCM2711EMMC0Host(SDHCIHostCreateEx(SizeOf(TBCM2711EMMC0Host)));
   if BCM2711EMMC0Host <> nil then
    begin
     {Update EMMC0}
     {Device}
     BCM2711EMMC0Host.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711EMMC0Host.SDHCI.Device.DeviceType:=SDHCI_TYPE_SD;
     BCM2711EMMC0Host.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_AUTO_CMD12 or SDHCI_FLAG_AUTO_CMD23 or SDHCI_FLAG_EXTERNAL_DMA;
     BCM2711EMMC0Host.SDHCI.Device.DeviceData:=nil;
     BCM2711EMMC0Host.SDHCI.Device.DeviceDescription:=BCM2711_EMMC0_DESCRIPTION;
     {SDHCI}
     BCM2711EMMC0Host.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
     BCM2711EMMC0Host.SDHCI.HostStart:=BCM2711EMMC0HostStart;
     BCM2711EMMC0Host.SDHCI.HostStop:=BCM2711EMMC0HostStop;
     BCM2711EMMC0Host.SDHCI.HostLock:=BCM2711EMMC0HostLock;
     BCM2711EMMC0Host.SDHCI.HostUnlock:=BCM2711EMMC0HostUnlock;
     BCM2711EMMC0Host.SDHCI.HostSignal:=BCM2711EMMC0HostSignal;
     BCM2711EMMC0Host.SDHCI.HostReadByte:=BCM2711EMMC0HostReadByte;
     BCM2711EMMC0Host.SDHCI.HostReadWord:=BCM2711EMMC0HostReadWord;
     BCM2711EMMC0Host.SDHCI.HostReadLong:=BCM2711EMMC0HostReadLong;
     BCM2711EMMC0Host.SDHCI.HostWriteByte:=BCM2711EMMC0HostWriteByte;
     BCM2711EMMC0Host.SDHCI.HostWriteWord:=BCM2711EMMC0HostWriteWord;
     BCM2711EMMC0Host.SDHCI.HostWriteLong:=BCM2711EMMC0HostWriteLong;
     BCM2711EMMC0Host.SDHCI.HostReset:=nil;
     BCM2711EMMC0Host.SDHCI.HostHardwareReset:=nil;
     BCM2711EMMC0Host.SDHCI.HostSetPower:=nil;
     BCM2711EMMC0Host.SDHCI.HostSetClock:=nil;
     BCM2711EMMC0Host.SDHCI.HostSetTiming:=nil;
     BCM2711EMMC0Host.SDHCI.HostSetBusWidth:=nil;
     BCM2711EMMC0Host.SDHCI.HostSetClockDivider:=nil;
     BCM2711EMMC0Host.SDHCI.HostSetControlRegister:=nil;
     BCM2711EMMC0Host.SDHCI.HostPrepareDMA:=nil;
     BCM2711EMMC0Host.SDHCI.HostStartDMA:=nil;
     BCM2711EMMC0Host.SDHCI.HostStopDMA:=nil;
     BCM2711EMMC0Host.SDHCI.HostSetupCardIRQ:=nil;
     BCM2711EMMC0Host.SDHCI.HostCompleteCardIRQ:=nil;
     BCM2711EMMC0Host.SDHCI.DeviceInitialize:=nil;
     BCM2711EMMC0Host.SDHCI.DeviceDeinitialize:=nil;
     BCM2711EMMC0Host.SDHCI.DeviceGetCardDetect:=BCM2711EMMC0DeviceGetCardDetect;
     BCM2711EMMC0Host.SDHCI.DeviceGetWriteProtect:=nil;
     BCM2711EMMC0Host.SDHCI.DeviceSendCommand:=nil;
     BCM2711EMMC0Host.SDHCI.DeviceSetIOS:=nil;
     {Driver}
     BCM2711EMMC0Host.SDHCI.Address:=Pointer(BCM2838_EMMC0_REGS_BASE);
     BCM2711EMMC0Host.SDHCI.DMASlave:=DMA_DREQ_ID_EMMC0;
     {BCM2711}
     BCM2711EMMC0Host.Id:=0;
     BCM2711EMMC0Host.IRQ:=BCM2838_IRQ_EMMC0;
     BCM2711EMMC0Host.FIQ:=BCM2711EMMC0_FIQ_ENABLED;
     BCM2711EMMC0Host.SDIO:=BCM2711_REGISTER_SDIO;
     BCM2711EMMC0Host.PowerId:=POWER_ID_MMC0;
     BCM2711EMMC0Host.ClockId:=CLOCK_ID_MMC0;
   
     {Register SDHCI}
     Status:=SDHCIHostRegister(@BCM2711EMMC0Host.SDHCI);
     if Status <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2711: Failed to register SDHCI controller: ' + ErrorToString(Status));

       {Destroy SDHCI}
       SDHCIHostDestroy(@BCM2711EMMC0Host.SDHCI);
      end;
    end
   else 
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2711: Failed to create SDHCI controller');
    end;
  end;
  
 {Create EMMC1}
 if BCM2711_REGISTER_EMMC1 then
  begin
   {Set Parameters}
   ClockMaximum:=ClockGetRate(CLOCK_ID_MMC1);
   if ClockMaximum = 0 then ClockMaximum:=BCM2711_EMMC1_MAX_FREQ;
   
   {Create Device}
   BCMSDHOSTCreate(BCM2838_EMMC1_REGS_BASE,BCM2711_EMMC1_DESCRIPTION,BCM2838_IRQ_EMMC1,DMA_DREQ_ID_EMMC1,BCM2711_EMMC1_MIN_FREQ,ClockMaximum,GPIO_PIN_22,GPIO_PIN_27,GPIO_FUNCTION_ALT0,BCM2711EMMC1_FIQ_ENABLED);
  end;

 {Create EMMC2}
 if BCM2711_REGISTER_EMMC2 then
  begin
   BCM2711EMMC2Host:=PBCM2711EMMC0Host(SDHCIHostCreateEx(SizeOf(TBCM2711EMMC0Host)));
   if BCM2711EMMC2Host <> nil then
    begin
     {Update EMMC2}
     {Device}
     BCM2711EMMC2Host.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711EMMC2Host.SDHCI.Device.DeviceType:=SDHCI_TYPE_SD;
     BCM2711EMMC2Host.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_AUTO_CMD12 or SDHCI_FLAG_AUTO_CMD23; {EMMC2 controller supports SDMA/ADMA2}
     if ((ChipGetRevision and $FF) < $20) or not(DeviceTreeValid) then BCM2711EMMC2Host.SDHCI.Device.DeviceFlags:=BCM2711EMMC2Host.SDHCI.Device.DeviceFlags or SDHCI_FLAG_BUS_ADDRESSES;
     BCM2711EMMC2Host.SDHCI.Device.DeviceData:=nil;
     BCM2711EMMC2Host.SDHCI.Device.DeviceDescription:=BCM2711_EMMC2_DESCRIPTION;
     {SDHCI}
     BCM2711EMMC2Host.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
     BCM2711EMMC2Host.SDHCI.HostStart:=BCM2711EMMC0HostStart;
     BCM2711EMMC2Host.SDHCI.HostStop:=BCM2711EMMC0HostStop;
     BCM2711EMMC2Host.SDHCI.HostLock:=BCM2711EMMC0HostLock;
     BCM2711EMMC2Host.SDHCI.HostUnlock:=BCM2711EMMC0HostUnlock;
     BCM2711EMMC2Host.SDHCI.HostSignal:=BCM2711EMMC0HostSignal;
     BCM2711EMMC2Host.SDHCI.HostReadByte:=BCM2711EMMC0HostReadByte;
     BCM2711EMMC2Host.SDHCI.HostReadWord:=BCM2711EMMC0HostReadWord;
     BCM2711EMMC2Host.SDHCI.HostReadLong:=BCM2711EMMC0HostReadLong;
     BCM2711EMMC2Host.SDHCI.HostWriteByte:=BCM2711EMMC0HostWriteByte;
     BCM2711EMMC2Host.SDHCI.HostWriteWord:=BCM2711EMMC0HostWriteWord;
     BCM2711EMMC2Host.SDHCI.HostWriteLong:=BCM2711EMMC0HostWriteLong;
     BCM2711EMMC2Host.SDHCI.HostReset:=nil;
     BCM2711EMMC2Host.SDHCI.HostHardwareReset:=nil;
     BCM2711EMMC2Host.SDHCI.HostSetPower:=nil;
     BCM2711EMMC2Host.SDHCI.HostSetClock:=nil;
     BCM2711EMMC2Host.SDHCI.HostSetTiming:=nil;
     BCM2711EMMC2Host.SDHCI.HostSetBusWidth:=nil;
     BCM2711EMMC2Host.SDHCI.HostSetClockDivider:=nil;
     BCM2711EMMC2Host.SDHCI.HostSetControlRegister:=nil;
     BCM2711EMMC2Host.SDHCI.HostPrepareDMA:=nil;
     BCM2711EMMC2Host.SDHCI.HostStartDMA:=nil;
     BCM2711EMMC2Host.SDHCI.HostStopDMA:=nil;
     BCM2711EMMC2Host.SDHCI.HostSetupCardIRQ:=nil;
     BCM2711EMMC2Host.SDHCI.HostCompleteCardIRQ:=nil;
     BCM2711EMMC2Host.SDHCI.DeviceInitialize:=nil;
     BCM2711EMMC2Host.SDHCI.DeviceDeinitialize:=nil;
     BCM2711EMMC2Host.SDHCI.DeviceGetCardDetect:=BCM2711EMMC0DeviceGetCardDetect;
     BCM2711EMMC2Host.SDHCI.DeviceGetWriteProtect:=nil;
     BCM2711EMMC2Host.SDHCI.DeviceSendCommand:=nil;
     BCM2711EMMC2Host.SDHCI.DeviceSetIOS:=nil;
     {Driver}
     BCM2711EMMC2Host.SDHCI.Address:=Pointer(BCM2838_EMMC2_REGS_BASE);
     BCM2711EMMC2Host.SDHCI.DMASlave:=DMA_DREQ_ID_NONE; {EMMC2 controller supports ADMA2}
     {BCM2711}
     BCM2711EMMC2Host.Id:=2;
     BCM2711EMMC2Host.IRQ:=BCM2838_IRQ_EMMC2;
     BCM2711EMMC2Host.FIQ:=BCM2711EMMC2_FIQ_ENABLED;
     BCM2711EMMC2Host.SDIO:=False; {Always False}
     BCM2711EMMC2Host.PowerId:=POWER_ID_MMC2;
     BCM2711EMMC2Host.ClockId:=CLOCK_ID_MMC2;
   
     {Register SDHCI}
     Status:=SDHCIHostRegister(@BCM2711EMMC2Host.SDHCI);
     if Status <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2711: Failed to register SDHCI controller: ' + ErrorToString(Status));

       {Destroy SDHCI}
       SDHCIHostDestroy(@BCM2711EMMC2Host.SDHCI);
      end;
    end
   else 
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2711: Failed to create SDHCI controller');
    end;
  end;
 
 {Create System Clock}
 if BCM2711_REGISTER_SYS_CLOCK then
  begin
   BCM2711SystemClock:=PBCM2711SystemClock(ClockDeviceCreateEx(SizeOf(TBCM2711SystemClock)));
   if BCM2711SystemClock <> nil then
    begin
     {Update Clock}
     {Device}
     BCM2711SystemClock.Clock.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711SystemClock.Clock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     BCM2711SystemClock.Clock.Device.DeviceFlags:=CLOCK_FLAG_NONE;
     BCM2711SystemClock.Clock.Device.DeviceData:=nil;
     BCM2711SystemClock.Clock.Device.DeviceDescription:=BCM2711_SYS_CLOCK_DESCRIPTION;
     {Clock}
     BCM2711SystemClock.Clock.ClockState:=CLOCK_STATE_DISABLED;
     BCM2711SystemClock.Clock.DeviceRead:=BCM2711SystemClockRead;
     BCM2711SystemClock.Clock.DeviceRead64:=BCM2711SystemClockRead64;
     {Driver}
     BCM2711SystemClock.Clock.Address:=Pointer(BCM2838_SYSTEM_TIMER_REGS_BASE);
     BCM2711SystemClock.Clock.Rate:=BCM2838_SYSTEM_TIMER_FREQUENCY;
     BCM2711SystemClock.Clock.MinRate:=BCM2838_SYSTEM_TIMER_FREQUENCY;
     BCM2711SystemClock.Clock.MaxRate:=BCM2838_SYSTEM_TIMER_FREQUENCY;
    
     {Register Clock}
     Status:=ClockDeviceRegister(@BCM2711SystemClock.Clock);
     if Status = ERROR_SUCCESS then
      begin
       {Start Clock}
       Status:=ClockDeviceStart(@BCM2711SystemClock.Clock);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to start new clock device: ' + ErrorToString(Status));

         {Destroy Deregister}
         ClockDeviceDeregister(@BCM2711SystemClock.Clock);

         {Destroy Clock}
         ClockDeviceDestroy(@BCM2711SystemClock.Clock);
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to register new clock device: ' + ErrorToString(Status));

       {Destroy Clock}
       ClockDeviceDestroy(@BCM2711SystemClock.Clock);
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to create new clock device');
    end;
  end;
  
 {Create ARM Clock}
 if BCM2711_REGISTER_ARM_CLOCK then
  begin
   BCM2711ARMClock:=PBCM2711ARMClock(ClockDeviceCreateEx(SizeOf(TBCM2711ARMClock)));
   if BCM2711ARMClock <> nil then
    begin
     {Update ARM Clock}
     {Device}
     BCM2711ARMClock.Clock.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711ARMClock.Clock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     BCM2711ARMClock.Clock.Device.DeviceFlags:=CLOCK_FLAG_VARIABLE;
     BCM2711ARMClock.Clock.Device.DeviceData:=nil;
     BCM2711ARMClock.Clock.Device.DeviceDescription:=BCM2711_ARM_CLOCK_DESCRIPTION;
     {Clock}
     BCM2711ARMClock.Clock.ClockState:=CLOCK_STATE_DISABLED;
     BCM2711ARMClock.Clock.DeviceStart:=BCM2711ARMClockStart;
     BCM2711ARMClock.Clock.DeviceStop:=BCM2711ARMClockStop;
     BCM2711ARMClock.Clock.DeviceRead:=BCM2711ARMClockRead;
     BCM2711ARMClock.Clock.DeviceRead64:=BCM2711ARMClockRead64;
     BCM2711ARMClock.Clock.DeviceSetRate:=BCM2711ARMClockSetRate;
     {Driver}
     BCM2711ARMClock.Clock.Address:=Pointer(BCM2838_TIMER_REGS_BASE);
     BCM2711ARMClock.Clock.Rate:=BCM2711_ARM_CLOCK_DEFAULT_RATE;
     BCM2711ARMClock.Clock.MinRate:=BCM2711_ARM_CLOCK_MIN_RATE;
     BCM2711ARMClock.Clock.MaxRate:=BCM2711_ARM_CLOCK_MAX_RATE;
     {BCM2711}
     BCM2711ARMClock.CoreClock:=BCM2711_ARM_CLOCK_CORE_CLOCK;
     
     {Register Clock}
     Status:=ClockDeviceRegister(@BCM2711ARMClock.Clock);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to register new clock device: ' + ErrorToString(Status));

       {Destroy Clock}
       ClockDeviceDestroy(@BCM2711ARMClock.Clock);
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to create new clock device');
    end;
  end;
  
 {Create ARM Timer}
 if BCM2711_REGISTER_ARM_TIMER then
  begin
   BCM2711ARMTimer:=PBCM2711ARMTimer(TimerDeviceCreateEx(SizeOf(TBCM2711ARMTimer)));
   if BCM2711ARMTimer <> nil then
    begin
     {Update Timer}
     {Device}
     BCM2711ARMTimer.Timer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711ARMTimer.Timer.Device.DeviceType:=TIMER_TYPE_HARDWARE;
     BCM2711ARMTimer.Timer.Device.DeviceFlags:=TIMER_FLAG_WRAPPING or TIMER_FLAG_COUNTER or TIMER_FLAG_DOWN;
     BCM2711ARMTimer.Timer.Device.DeviceData:=nil;
     BCM2711ARMTimer.Timer.Device.DeviceDescription:=BCM2711_ARM_TIMER_DESCRIPTION;
     {Timer}
     BCM2711ARMTimer.Timer.TimerState:=TIMER_STATE_DISABLED;
     BCM2711ARMTimer.Timer.DeviceStart:=BCM2711ARMTimerStart;
     BCM2711ARMTimer.Timer.DeviceStop:=BCM2711ARMTimerStop;
     BCM2711ARMTimer.Timer.DeviceRead64:=BCM2711ARMTimerRead64;
     BCM2711ARMTimer.Timer.DeviceWait:=BCM2711ARMTimerWait;
     BCM2711ARMTimer.Timer.DeviceEvent:=BCM2711ARMTimerEvent;
     BCM2711ARMTimer.Timer.DeviceCancel:=BCM2711ARMTimerCancel;
     BCM2711ARMTimer.Timer.DeviceSetRate:=BCM2711ARMTimerSetRate;
     BCM2711ARMTimer.Timer.DeviceSetInterval:=BCM2711ARMTimerSetInterval;
     {Driver}
     BCM2711ARMTimer.Timer.Address:=Pointer(BCM2838_TIMER_REGS_BASE);
     BCM2711ARMTimer.Timer.Rate:=BCM2711_ARM_TIMER_DEFAULT_RATE;
     BCM2711ARMTimer.Timer.Interval:=0;
     BCM2711ARMTimer.Timer.Properties.Flags:=BCM2711ARMTimer.Timer.Device.DeviceFlags;
     BCM2711ARMTimer.Timer.Properties.Bits:=32;
     BCM2711ARMTimer.Timer.Properties.MinRate:=BCM2711_ARM_TIMER_MIN_RATE;
     BCM2711ARMTimer.Timer.Properties.MaxRate:=BCM2711_ARM_TIMER_MAX_RATE;
     BCM2711ARMTimer.Timer.Properties.MinInterval:=BCM2711_ARM_TIMER_MIN_INTERVAL;
     BCM2711ARMTimer.Timer.Properties.MaxInterval:=BCM2711_ARM_TIMER_MAX_INTERVAL;
     {BCM2711}
     BCM2711ARMTimer.CoreClock:=BCM2711_ARM_TIMER_CORE_CLOCK;
     
     {Register Timer}
     Status:=TimerDeviceRegister(@BCM2711ARMTimer.Timer);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to register new timer device: ' + ErrorToString(Status));

       {Destroy Timer}
       TimerDeviceDestroy(@BCM2711ARMTimer.Timer);
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to create new timer device');
    end;
  end; 

 {Create Local Timer}
 if BCM2711_REGISTER_LOCAL_TIMER then
  begin
   //To Do
  end; 
 
 {Create Random}
 if BCM2711_REGISTER_RANDOM then
  begin
   BCM2711Random:=PBCM2711Random(RandomDeviceCreateEx(SizeOf(TBCM2711Random)));
   if BCM2711Random <> nil then
    begin
     {Update Random}
     {Device}
     BCM2711Random.Random.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711Random.Random.Device.DeviceType:=RANDOM_TYPE_HARDWARE;
     BCM2711Random.Random.Device.DeviceFlags:=RANDOM_FLAG_NONE;
     BCM2711Random.Random.Device.DeviceData:=nil;
     BCM2711Random.Random.Device.DeviceDescription:=BCM2711_RANDOM_DESCRIPTION;
     {Random}
     BCM2711Random.Random.RandomState:=RANDOM_STATE_DISABLED;
     BCM2711Random.Random.DeviceStart:=BCM2711RandomStart;
     BCM2711Random.Random.DeviceStop:=BCM2711RandomStop;
     BCM2711Random.Random.DeviceReadLongWord:=BCM2711RandomReadLongWord;
     {Driver}
     BCM2711Random.Random.Address:=Pointer(BCM2838_RNG_REGS_BASE);
     
     {Register Random}
     Status:=RandomDeviceRegister(@BCM2711Random.Random);
     if Status = ERROR_SUCCESS then
      begin
       {Start Random}
       Status:=RandomDeviceStart(@BCM2711Random.Random);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to start new random device: ' + ErrorToString(Status));

         {Deregister Random}
         RandomDeviceDeregister(@BCM2711Random.Random);

         {Destroy Random}
         RandomDeviceDestroy(@BCM2711Random.Random);
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to register new random device: ' + ErrorToString(Status));

       {Destroy Random}
       RandomDeviceDestroy(@BCM2711Random.Random);
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to create new random device');
    end;
  end;
  
 {Create Mailbox}
 if BCM2711_REGISTER_MAILBOX then
  begin
   //To Do
  end; 
  
 {Create Watchdog}
 if BCM2711_REGISTER_WATCHDOG then
  begin
   BCM2711Watchdog:=PBCM2711Watchdog(WatchdogDeviceCreateEx(SizeOf(TBCM2711Watchdog)));
   if BCM2711Watchdog <> nil then
    begin
     {Device}
     BCM2711Watchdog.Watchdog.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2711Watchdog.Watchdog.Device.DeviceType:=WATCHDOG_TYPE_HARDWARE;
     BCM2711Watchdog.Watchdog.Device.DeviceFlags:=WATCHDOG_FLAG_NONE;
     BCM2711Watchdog.Watchdog.Device.DeviceData:=nil;
     BCM2711Watchdog.Watchdog.Device.DeviceDescription:=BCM2711_WATCHDOG_DESCRIPTION;
     {Watchdog}
     BCM2711Watchdog.Watchdog.WatchdogState:=WATCHDOG_STATE_DISABLED;
     BCM2711Watchdog.Watchdog.DeviceStart:=BCM2711WatchdogStart;
     BCM2711Watchdog.Watchdog.DeviceStop:=BCM2711WatchdogStop;
     BCM2711Watchdog.Watchdog.DeviceRefresh:=BCM2711WatchdogRefresh;
     BCM2711Watchdog.Watchdog.DeviceGetRemain:=BCM2711WatchdogGetRemain;
     {Driver}
     BCM2711Watchdog.Watchdog.Address:=Pointer(BCM2838_PM_REGS_BASE);
     
     {Register Watchdog}
     Status:=WatchdogDeviceRegister(@BCM2711Watchdog.Watchdog);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to register new watchdog device: ' + ErrorToString(Status));

       {Destroy Watchdog}
       WatchdogDeviceDestroy(@BCM2711Watchdog.Watchdog);
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to create new watchdog device');
    end;
  end;

 {Create Framebuffer}
 if BCM2711_REGISTER_FRAMEBUFFER then
  begin
   {Get Display Count and Check Multi-Display support}
   if FramebufferGetNumDisplays(DisplayCount) = ERROR_SUCCESS then
    begin
     MultiDisplay:=(DisplayCount > 0);
    end
   else
    begin
     MultiDisplay:=False;
     DisplayCount:=1;
    end;
   
   {Create Framebuffer for each Display}
   if DisplayCount > {$IFNDEF CONSOLE_EARLY_INIT}0{$ELSE}1{$ENDIF} then
    begin
     for DisplayNum:={$IFNDEF CONSOLE_EARLY_INIT}0{$ELSE}1{$ENDIF} to DisplayCount - 1 do
      begin
       {Get Display Id}
       DisplayId:=FramebufferGetDisplayId(DisplayNum);
       
       {Create Framebuffer}
       BCM2711Framebuffer:=PBCM2711Framebuffer(FramebufferDeviceCreateEx(SizeOf(TBCM2711Framebuffer)));
       if BCM2711Framebuffer <> nil then
        begin
         {Device}
         BCM2711Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
         BCM2711Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
         BCM2711Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_BACKLIGHT or FRAMEBUFFER_FLAG_VIRTUAL or FRAMEBUFFER_FLAG_OFFSETX or FRAMEBUFFER_FLAG_OFFSETY or FRAMEBUFFER_FLAG_SYNC or FRAMEBUFFER_FLAG_CURSOR;
         if EMULATOR_MODE then BCM2711Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2711Framebuffer.Framebuffer.Device.DeviceFlags and not(FRAMEBUFFER_FLAG_CURSOR); {QEMU does not support hardware cursor}
         BCM2711Framebuffer.Framebuffer.Device.DeviceData:=nil;
         BCM2711Framebuffer.Framebuffer.Device.DeviceDescription:=BCM2711_FRAMEBUFFER_DESCRIPTION + ' (' + FramebufferDisplayIdToName(DisplayId) + ')';
         {Framebuffer}
         BCM2711Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
         BCM2711Framebuffer.Framebuffer.DeviceAllocate:=BCM2711FramebufferAllocate;
         BCM2711Framebuffer.Framebuffer.DeviceRelease:=BCM2711FramebufferRelease;
         BCM2711Framebuffer.Framebuffer.DeviceBlank:=BCM2711FramebufferBlank;
         BCM2711Framebuffer.Framebuffer.DeviceCommit:=BCM2711FramebufferCommit;
         BCM2711Framebuffer.Framebuffer.DeviceWaitSync:=BCM2711FramebufferWaitSync;
         BCM2711Framebuffer.Framebuffer.DeviceSetOffset:=BCM2711FramebufferSetOffset;
         BCM2711Framebuffer.Framebuffer.DeviceGetPalette:=BCM2711FramebufferGetPalette;
         BCM2711Framebuffer.Framebuffer.DeviceSetPalette:=BCM2711FramebufferSetPalette;
         BCM2711Framebuffer.Framebuffer.DeviceSetBacklight:=BCM2711FramebufferSetBacklight;
         if not EMULATOR_MODE then BCM2711Framebuffer.Framebuffer.DeviceSetCursor:=BCM2711FramebufferSetCursor;
         if not EMULATOR_MODE then BCM2711Framebuffer.Framebuffer.DeviceUpdateCursor:=BCM2711FramebufferUpdateCursor;
         {Driver}
         BCM2711Framebuffer.MultiDisplay:=MultiDisplay;
         BCM2711Framebuffer.DisplayNum:=DisplayNum;
         FramebufferGetDisplaySettings(DisplayNum,BCM2711Framebuffer.DisplaySettings);
         
         {Setup Flags}
         if BCM2711FRAMEBUFFER_CACHED then BCM2711Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2711Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_COMMIT;
         if BCM2711FRAMEBUFFER_CACHED then BCM2711Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2711Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
         {if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then BCM2711Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2711Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;} {Handled by FramebufferAllocate}
         
         {Register Framebuffer}
         Status:=FramebufferDeviceRegister(@BCM2711Framebuffer.Framebuffer);
         if Status = ERROR_SUCCESS then
          begin
           {Allocate Framebuffer}
           Status:=FramebufferDeviceAllocate(@BCM2711Framebuffer.Framebuffer,nil);
           if Status <> ERROR_SUCCESS then
            begin
             if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to allocate new framebuffer device: ' + ErrorToString(Status));

             {Deregister Framebuffer}
             FramebufferDeviceDeregister(@BCM2711Framebuffer.Framebuffer);

             {Destroy Framebuffer}
             FramebufferDeviceDestroy(@BCM2711Framebuffer.Framebuffer);
            end;
          end
         else
          begin     
           if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to register new framebuffer device: ' + ErrorToString(Status));

           {Destroy Framebuffer}
           FramebufferDeviceDestroy(@BCM2711Framebuffer.Framebuffer);
          end;
        end
       else 
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: Failed to create new framebuffer device');
        end;
      end;
    end;  
  end;

 BCM2711Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{BCM2711 Functions}

{==============================================================================}
{==============================================================================}
{BCM2711 SPI0/3/4/5/6 Functions}
function BCM2711SPI0Start(SPI:PSPIDevice;Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
var
 Control:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Start (Mode=' + SPIModeToString(Mode) + ' ClockRate=' + IntToStr(ClockRate) + ' ClockPhase=' + SPIClockPhaseToString(ClockPhase) + ' ClockPolarity=' + SPIClockPolarityToString(ClockPolarity) + ')');
 {$ENDIF}
 
 {Update Core Clock}
 PBCM2711SPI0Device(SPI).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
 if PBCM2711SPI0Device(SPI).CoreClock = 0 then PBCM2711SPI0Device(SPI).CoreClock:=BCM2711_SPI0_CORE_CLOCK;

 {Update Properties}
 SPI.Properties.MinClock:=PBCM2711SPI0Device(SPI).CoreClock div BCM2711_SPI0_MAX_DIVIDER;
 SPI.Properties.MaxClock:=PBCM2711SPI0Device(SPI).CoreClock div BCM2711_SPI0_MIN_DIVIDER;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711:  CoreClock=' + IntToStr(PBCM2711SPI0Device(SPI).CoreClock) + ' MinClock=' + IntToStr(SPI.Properties.MinClock) + ' MaxClock=' + IntToStr(SPI.Properties.MaxClock));
 {$ENDIF}
 
 {Check Mode}
 if Mode > SPI_MODE_LOSSI then Exit;
 
 {Check Clock Rate}
 if (ClockRate < SPI.Properties.MinClock) or (ClockRate > SPI.Properties.MaxClock) then Exit;
 
 {Check Clock Phase}
 if ClockPhase > SPI_CLOCK_PHASE_HIGH then Exit;
 
 {Check Clock Polarity}
 if ClockPolarity > SPI_CLOCK_POLARITY_HIGH then Exit;
 
 {Enable GPIO Pins (CS1, CS0, MISO, MOSI, SCLK)}
 GPIOFunctionSelect(PBCM2711SPI0Device(SPI).CS1Pin,PBCM2711SPI0Device(SPI).CS1Function);
 GPIOFunctionSelect(PBCM2711SPI0Device(SPI).CS0Pin,PBCM2711SPI0Device(SPI).CS0Function);
 GPIOFunctionSelect(PBCM2711SPI0Device(SPI).MISOPin,PBCM2711SPI0Device(SPI).MISOFunction);
 GPIOFunctionSelect(PBCM2711SPI0Device(SPI).MOSIPin,PBCM2711SPI0Device(SPI).MOSIFunction);
 GPIOFunctionSelect(PBCM2711SPI0Device(SPI).SCLKPin,PBCM2711SPI0Device(SPI).SCLKFunction);
 
 {Get Divider}
 Divider:=PBCM2711SPI0Device(SPI).CoreClock div ClockRate;
 if (Divider and 1) <> 0 then Inc(Divider);

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711:  ClockRate=' + IntToStr(ClockRate) + ' Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control and Status} 
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX;

 {Setup Control}
 Control:=0;
 
 {Set Mode}
 case Mode of
  SPI_MODE_LOSSI:Control:=Control or BCM2838_SPI0_CS_LEN;
 end;
 
 {Set Clock Phase}
 if ClockPhase = SPI_CLOCK_PHASE_HIGH then Control:=Control or BCM2838_SPI0_CS_CPHA;
 
 {Set Clock Polarity}
 if ClockPolarity = SPI_CLOCK_POLARITY_HIGH then Control:=Control or BCM2838_SPI0_CS_CPOL;
 
 {Set Clock Divider}
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CLK:=(Divider and BCM2838_SPI0_CLK_CDIV);

 {Set Control and Status}
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Create Wait Semaphore}
 SPI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
 if SPI.Wait = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Request IRQ}
 RegisterInterrupt(PBCM2711SPI0Device(SPI).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711SPI0SharedInterruptHandler),SPI);
 
 {Update Properties}
 SPI.SPIMode:=Mode;
 SPI.Divider:=Divider;
 SPI.ClockRate:=ClockRate;
 SPI.ClockPhase:=ClockPhase;
 SPI.ClockPolarity:=ClockPolarity;
 SPI.SelectPolarity:=SPI_CS_POLARITY_LOW;
 SPI.Properties.Mode:=Mode;
 SPI.Properties.ClockRate:=ClockRate;
 SPI.Properties.ClockPhase:=ClockPhase;
 SPI.Properties.ClockPolarity:=ClockPolarity;
 SPI.Properties.SelectPolarity:=SPI_CS_POLARITY_LOW;
 SPI.ChipSelects[0].Pin:=GPIO_PIN_UNKNOWN;
 SPI.ChipSelects[0].SelectPolarity:=SPI_CS_POLARITY_LOW;
 SPI.ChipSelects[1].Pin:=GPIO_PIN_UNKNOWN;
 SPI.ChipSelects[1].SelectPolarity:=SPI_CS_POLARITY_LOW;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711SPI0Stop(SPI:PSPIDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Stop');
 {$ENDIF}
 
 {Release IRQ}
 DeregisterInterrupt(PBCM2711SPI0Device(SPI).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711SPI0SharedInterruptHandler),SPI);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control and Status} 
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Destroy Wait Semaphore}
 SemaphoreDestroy(SPI.Wait);
 
 {Reset Transfer}
 PBCM2711SPI0Device(SPI).Mode:=BCM2711_SPI0_MODE_IRQ;
 PBCM2711SPI0Device(SPI).Source:=nil;
 PBCM2711SPI0Device(SPI).Dest:=nil;
 PBCM2711SPI0Device(SPI).Count:=0;
 PBCM2711SPI0Device(SPI).SourceRemain:=0;
 PBCM2711SPI0Device(SPI).DestRemain:=0;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711SPI0WriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
var
 {$IFDEF BCM2711_SPI0_DMA_CS_DLEN}
 CSData:TDMAData;
 {$ENDIF}
 TXData:TDMAData;
 RXData:TDMAData;
 Control:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffers}
 if (Source = nil) and (Dest = nil) then Exit;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Write Read (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Size}
 if Size > BCM2711_SPI0_MAX_SIZE then Exit;
 
 {Check Chip Select}
 if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_2) then Exit;
 
 {Update Statistics}
 Inc(SPI.TransferCount);
 
 {Write from Source / Read to Dest}
 if Size > 0 then
  begin
   {Setup Data}
   PBCM2711SPI0Device(SPI).Source:=Source;
   PBCM2711SPI0Device(SPI).Dest:=Dest;
   PBCM2711SPI0Device(SPI).Count:=0;
   PBCM2711SPI0Device(SPI).SourceRemain:=Size;
   PBCM2711SPI0Device(SPI).DestRemain:=Size;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Get Control and Status}
   Control:=PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS and not(BCM2838_SPI0_CS_CS_MASK);
   
   {Set Mode}
   if (SPI.SPIMode = SPI_MODE_3WIRE) and (Dest <> nil) then
    begin
     Control:=Control or BCM2838_SPI0_CS_REN;
    end
   else  
    begin
     Control:=Control and not(BCM2838_SPI0_CS_REN);
    end;
   
   {Set Chip Select}
   if ChipSelect = SPI_CS_NONE then
    begin
     Control:=Control or (BCM2838_SPI0_CS_CS_MASK); {Select the reserved value}
    end
   else
    begin
     Control:=Control or (ChipSelect and BCM2838_SPI0_CS_CS_MASK);
    end;
   
   {Check Clock Rate}
   if (ChipSelect = SPI_CS_NONE) or (SPI.ChipSelects[ChipSelect].ClockRate = 0) then
    begin
     {Set Clock Divider}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CLK:=(SPI.Divider and BCM2838_SPI0_CLK_CDIV);
    end
   else 
    begin
     {Set Clock Divider}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CLK:=(SPI.ChipSelects[ChipSelect].Divider and BCM2838_SPI0_CLK_CDIV);
    end;
      
   {Check Flags}   
   if (Flags and SPI_TRANSFER_DMA) <> 0 then
    begin
     {Update Data}
     PBCM2711SPI0Device(SPI).Mode:=BCM2711_SPI0_MODE_DMA;
    
     {Check Cache}
     if not(DMA_CACHE_COHERENT) and (Dest <> nil) then
      begin
       {Clean Cache (Dest)}
       CleanDataCacheRange(PtrUInt(Dest),Size);
      end;
     
     {$IFDEF BCM2711_SPI0_DMA_CS_DLEN}
     {Setup Control Data (CS/DLEN)}
     FillChar(CSData,SizeOf(TDMAData),0);
     CSData.Source:=@Control;
     CSData.Dest:=@PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).FIFO;
     CSData.Flags:=DMA_DATA_FLAG_DEST_NOINCREMENT or DMA_DATA_FLAG_DEST_DREQ or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_NOINVALIDATE;
     CSData.StrideLength:=0;
     CSData.SourceStride:=0;
     CSData.DestStride:=0;
     CSData.Size:=SizeOf(LongWord);
     CSData.Next:=@TXData;
     {$ENDIF}
     
     {Setup TX Data}
     FillChar(TXData,SizeOf(TDMAData),0);
     TXData.Source:=Source;
     TXData.Dest:=@PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).FIFO;
     TXData.Flags:=DMA_DATA_FLAG_DEST_NOINCREMENT or DMA_DATA_FLAG_DEST_DREQ or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_NOINVALIDATE;
     if Source = nil then TXData.Flags:=TXData.Flags or DMA_DATA_FLAG_NOREAD or DMA_DATA_FLAG_NOCLEAN;
     TXData.StrideLength:=0;
     TXData.SourceStride:=0;
     TXData.DestStride:=0;
     TXData.Size:=Size;
     
     {Setup RX Data}
     FillChar(RXData,SizeOf(TDMAData),0);
     RXData.Source:=@PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).FIFO;
     RXData.Dest:=Dest;
     RXData.Flags:=DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_DREQ or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN;
     if Dest = nil then RXData.Flags:=RXData.Flags or DMA_DATA_FLAG_NOWRITE or DMA_DATA_FLAG_NOINVALIDATE;
     RXData.StrideLength:=0;
     RXData.SourceStride:=0;
     RXData.DestStride:=0;
     RXData.Size:=Size;
     
     {$IFDEF BCM2711_SPI0_DMA_CS_DLEN}
     {Set Control (Deassert/DMA/Clear)}
     Control:=Control or (BCM2838_SPI0_CS_ADCS or BCM2838_SPI0_CS_DMAEN or BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX);
     
     {Set Control and Status}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
     
     {Update Control (Active/Length)}
     Control:=(Size shl 16) or (Control and $FF) or BCM2838_SPI0_CS_TA;
     {$ELSE}
     {Set Length}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).DLEN:=Size;

     {Set Control (Deassert/DMA/Clear/Active)}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control or (BCM2838_SPI0_CS_ADCS or BCM2838_SPI0_CS_DMAEN or BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX or BCM2838_SPI0_CS_TA);
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
     {$ENDIF}
     
     {Enable RX Transfer}
     if DMATransferRequestEx(DMAHostGetDefault,@RXData,BCM2711SPI0DMARequestCompleted,SPI,DMA_DIR_DEV_TO_MEM,DMA_DREQ_ID_SPI_RX,DMA_REQUEST_FLAG_NONE) = ERROR_SUCCESS then
      begin
       {Perform TX Transfer}
       {$IFDEF BCM2711_SPI0_DMA_CS_DLEN}
       if DMATransferRequest(DMAHostGetDefault,@CSData,DMA_DIR_MEM_TO_DEV,DMA_DREQ_ID_SPI_TX,DMA_REQUEST_FLAG_NONE,INFINITE) = ERROR_SUCCESS then
       {$ELSE}
       if DMATransferRequest(DMAHostGetDefault,@TXData,DMA_DIR_MEM_TO_DEV,DMA_DREQ_ID_SPI_TX,DMA_REQUEST_FLAG_NONE,INFINITE) = ERROR_SUCCESS then
       {$ENDIF}
        begin
         {Wait for RX Completion}
         if SemaphoreWait(SPI.Wait) = ERROR_SUCCESS then
          begin
           {Update Count}
           Count:=Size;
          end
         else
          begin
           if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2711: Wait failure on DMA transfer'); 
           
           Result:=ERROR_OPERATION_FAILED;
          end;
        end
       else
        begin
         if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2711: Failure starting TX DMA transfer');
         
         Result:=ERROR_OPERATION_FAILED;
        end;
      end
     else
      begin
       if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2711: Failure starting RX DMA transfer');
       
       Result:=ERROR_OPERATION_FAILED;
      end;      
    end
   else if (Flags and SPI_TRANSFER_PIO) <> 0 then
    begin
     {Update Data}
     PBCM2711SPI0Device(SPI).Mode:=BCM2711_SPI0_MODE_PIO;
     
     {Set Data Length (See: https://www.raspberrypi.org/forums/viewtopic.php?f=44&t=181154)}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).DLEN:=Size;
     
     {Set Control (Active/Clear)}
     Control:=Control or (BCM2838_SPI0_CS_TA or BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX);
     
     {Set Control and Status}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
     
     {Loop until Completion}
     while PBCM2711SPI0Device(SPI).DestRemain > 0 do
      begin
       {Read FIFO}
       BCM2711SPI0ReadFIFO(PBCM2711SPI0Device(SPI));
  
       {Write FIFO}
       BCM2711SPI0WriteFIFO(PBCM2711SPI0Device(SPI));
  
       {Get Control and Status}
       Control:=PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS;
    
       {Check Done}
       if ((Control and BCM2838_SPI0_CS_DONE) <> 0) and (PBCM2711SPI0Device(SPI).SourceRemain = 0) then
        begin
         {Read remaining FIFO}
         BCM2711SPI0ReadFIFO(PBCM2711SPI0Device(SPI));
  
         {Reset Control (Active/Interrupt/Deassert/DMA/Clear)}
         Control:=Control and not(BCM2838_SPI0_CS_INTR or BCM2838_SPI0_CS_INTD or BCM2838_SPI0_CS_ADCS or BCM2838_SPI0_CS_DMAEN or BCM2838_SPI0_CS_TA);
         Control:=Control or (BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX);
  
         {Set Control and Status}
         PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
  
         {Set Data Length}
         PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).DLEN:=0;
        end;
      end;
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
     
     {Get Count}
     Count:=PBCM2711SPI0Device(SPI).Count;
     
     {Check Count}
     if Count < Size then
      begin
       if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2711: Write failure or timeout');
  
       Result:=ERROR_OPERATION_FAILED;
       
       {Update Statistics}
       Inc(SPI.TransferErrors);
      end;
    end
   else
    begin
     {Update Data}
     PBCM2711SPI0Device(SPI).Mode:=BCM2711_SPI0_MODE_IRQ;
      
     {Note: Cannot fill FIFO when TA bit is not set, interrupt handler will fill on first IRQ} 
     
     {Set Data Length (See: https://www.raspberrypi.org/forums/viewtopic.php?f=44&t=181154)}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).DLEN:=Size;
     
     {Set Control (Active/Interrupt/Clear)}
     Control:=Control or (BCM2838_SPI0_CS_INTR or BCM2838_SPI0_CS_INTD or BCM2838_SPI0_CS_TA or BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX);
     
     {Set Control and Status}
     PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
     
     {Wait for Completion}
     if SemaphoreWait(SPI.Wait) = ERROR_SUCCESS then
      begin
       {Get Count}
       Count:=PBCM2711SPI0Device(SPI).Count;
       
       {Check Count}
       if Count < Size then
        begin
         if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2711: Write failure or timeout'); 
         
         Result:=ERROR_OPERATION_FAILED;
         
         {Update Statistics}
         Inc(SPI.TransferErrors);
        end;
      end
     else
      begin
       if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2711: Wait failure on write'); 
       
       Result:=ERROR_OPERATION_FAILED;
      end;
    end;
    
   {Reset Data}
   PBCM2711SPI0Device(SPI).Source:=nil;
   PBCM2711SPI0Device(SPI).Dest:=nil;
   PBCM2711SPI0Device(SPI).Count:=0;
   PBCM2711SPI0Device(SPI).SourceRemain:=0;
   PBCM2711SPI0Device(SPI).DestRemain:=0;
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711SPI0SetMode(SPI:PSPIDevice;Mode:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Set Mode (Mode=' + SPIModeToString(Mode) + ')');
 {$ENDIF}

 {Check Mode}
 if Mode > SPI_MODE_LOSSI then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS;
 
 {Set Mode}
 case Mode of
  SPI_MODE_4WIRE:begin
    {Disable LEN/REN/LEN_LONG/DMA_LEN}
    Control:=Control and not(BCM2838_SPI0_CS_LEN or BCM2838_SPI0_CS_REN or BCM2838_SPI0_CS_LEN_LONG or BCM2838_SPI0_CS_DMA_LEN);
   end; 
  SPI_MODE_3WIRE:begin
    {Disable LEN/LEN_LONG/DMA_LEN}
    Control:=Control and not(BCM2838_SPI0_CS_LEN or BCM2838_SPI0_CS_LEN_LONG or BCM2838_SPI0_CS_DMA_LEN);
   end; 
  SPI_MODE_LOSSI:begin
    {Disable REN}
    Control:=Control and not(BCM2838_SPI0_CS_REN);
    
    {Enable LEN}
    Control:=Control or BCM2838_SPI0_CS_LEN;
   end; 
 end;
 
 {Set Control and Status}
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 SPI.SPIMode:=Mode;
 SPI.Properties.Mode:=Mode;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711SPI0SetClockRate(SPI:PSPIDevice;ChipSelect:Word;ClockRate:LongWord):LongWord;
var
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Set Clock Rate (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' ClockRate=' + IntToStr(ClockRate) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if SPI.SPIState <> SPI_STATE_ENABLED then
  begin
   {Update Core Clock}
   PBCM2711SPI0Device(SPI).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
   if PBCM2711SPI0Device(SPI).CoreClock = 0 then PBCM2711SPI0Device(SPI).CoreClock:=BCM2711_SPI0_CORE_CLOCK;
   
   {Update Properties}
   SPI.Properties.MinClock:=PBCM2711SPI0Device(SPI).CoreClock div BCM2711_SPI0_MAX_DIVIDER;
   SPI.Properties.MaxClock:=PBCM2711SPI0Device(SPI).CoreClock div BCM2711_SPI0_MIN_DIVIDER;
   
   {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
   if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711:  CoreClock=' + IntToStr(PBCM2711SPI0Device(SPI).CoreClock) + ' MinClock=' + IntToStr(SPI.Properties.MinClock) + ' MaxClock=' + IntToStr(SPI.Properties.MaxClock));
   {$ENDIF}
  end;
 
 {Check Chip Select}
 if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_2) then Exit;
 
 {Set Clock Rate}
 if ChipSelect = SPI_CS_NONE then
  begin
   {Check Clock Rate}
   if (ClockRate < SPI.Properties.MinClock) or (ClockRate > SPI.Properties.MaxClock) then Exit;
   
   {Get Divider}
   Divider:=PBCM2711SPI0Device(SPI).CoreClock div ClockRate;
   if (Divider and 1) <> 0 then Inc(Divider);
  
   {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
   if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711:  Divider=' + IntToStr(Divider));
   {$ENDIF}
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Set Clock Divider} 
   PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CLK:=(Divider and BCM2838_SPI0_CLK_CDIV);
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Update Properties}
   SPI.Divider:=Divider;
   SPI.ClockRate:=ClockRate;
   SPI.Properties.ClockRate:=ClockRate;
  end
 else
  begin
   {Check Clock Rate}
   if ClockRate <> 0 then
    begin
     {Check Clock Rate}
     if (ClockRate < SPI.Properties.MinClock) or (ClockRate > SPI.Properties.MaxClock) then Exit;

     {Get Divider}
     Divider:=PBCM2711SPI0Device(SPI).CoreClock div ClockRate;
     if (Divider and 1) <> 0 then Inc(Divider);
    end
   else
    begin
     {Reset Divider}
     Divider:=0;
    end;
   
   {Update Properties}
   SPI.ChipSelects[ChipSelect].Divider:=Divider;
   SPI.ChipSelects[ChipSelect].ClockRate:=ClockRate;
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711SPI0SetClockPhase(SPI:PSPIDevice;ClockPhase:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Set Clock Phase (ClockPhase=' + SPIClockPhaseToString(ClockPhase) + ')');
 {$ENDIF}

 {Check Clock Phase}
 if ClockPhase > SPI_CLOCK_PHASE_HIGH then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS;

 {Set Clock Phase}
 if ClockPhase = SPI_CLOCK_PHASE_HIGH then
  begin
   {Enable CPHA}
   Control:=Control or BCM2838_SPI0_CS_CPHA;
  end
 else
  begin
   {Disable CPHA}
   Control:=Control and not(BCM2838_SPI0_CS_CPHA);
  end;  
 
 {Set Control and Status}
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 SPI.ClockPhase:=ClockPhase;
 SPI.Properties.ClockPhase:=ClockPhase;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711SPI0SetClockPolarity(SPI:PSPIDevice;ClockPolarity:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Set Clock Polarity (ClockPolarity=' + SPIClockPolarityToString(ClockPolarity) + ')');
 {$ENDIF}

 {Check Clock Polarity}
 if ClockPolarity > SPI_CLOCK_POLARITY_HIGH then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS;
 
 {Set Clock Polarity}
 if ClockPolarity = SPI_CLOCK_POLARITY_HIGH then
  begin 
   {Enable CPOL}
   Control:=Control or BCM2838_SPI0_CS_CPOL;
  end
 else 
  begin
   {Disable CPOL}
   Control:=Control and not(BCM2838_SPI0_CS_CPOL);
  end;  
 
 {Set Control and Status}
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 SPI.ClockPolarity:=ClockPolarity;
 SPI.Properties.ClockPolarity:=ClockPolarity;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711SPI0SetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word;SelectPolarity:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2711: SPI0 Set Select Polarity (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' SelectPolarity=' + SPISelectPolarityToString(SelectPolarity) + ')');
 {$ENDIF}

 {Check Chip Select}
 if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_2) then Exit;
 
 {Check Select Polarity}
 if SelectPolarity > SPI_CS_POLARITY_HIGH then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS;
 
 {Set Select Polarity}
 if ChipSelect = SPI_CS_NONE then
  begin
   if SelectPolarity = SPI_CS_POLARITY_HIGH then
    begin
     {Enable CSPOL}
     Control:=Control or BCM2838_SPI0_CS_CSPOL;
    end
   else
    begin
     {Disable CSPOL}
     Control:=Control and not(BCM2838_SPI0_CS_CSPOL);
    end;
  end
 else
  begin 
   if SelectPolarity = SPI_CS_POLARITY_HIGH then
    begin
     {Enable CSPOL0/1/2}
     Control:=Control or (BCM2838_SPI0_CS_CSPOL0 shl ChipSelect);
    end
   else
    begin
     {Disable CSPOL0/1/2}
     Control:=Control and not(BCM2838_SPI0_CS_CSPOL0 shl ChipSelect);
    end;
  end;  
 
 {Set Control and Status}
 PBCM2838SPI0Registers(PBCM2711SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 if ChipSelect = SPI_CS_NONE then
  begin
   SPI.SelectPolarity:=SelectPolarity;
   SPI.Properties.SelectPolarity:=SelectPolarity;
  end
 else
  begin
   SPI.ChipSelects[ChipSelect].SelectPolarity:=SelectPolarity;
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

procedure BCM2711SPI0ReadFIFO(SPI:PBCM2711SPI0Device);
{Caller will hold the SPI device lock}
{Note: Called from within the interrupt handler}
var
 Data:LongWord;
begin
 {}
 {Check SPI}
 if SPI = nil then Exit;
 
 {Check Data}
 while (SPI.DestRemain > 0) and ((PBCM2838SPI0Registers(SPI.Address).CS and BCM2838_SPI0_CS_RXD) <> 0) do
  begin
   {Read Data}
   Data:=(PBCM2838SPI0Registers(SPI.Address).FIFO and BCM2838_SPI0_FIFO_IRQ_DATA);
   if SPI.Dest <> nil then
    begin
     PByte(SPI.Dest)^:=Data;
     
     {Update Dest}
     Inc(SPI.Dest);
    end; 
   
   {Update Remain}
   Inc(SPI.Count);
   Dec(SPI.DestRemain);
  end;
end;
 
{==============================================================================}

procedure BCM2711SPI0WriteFIFO(SPI:PBCM2711SPI0Device);
{Caller will hold the SPI device lock}
{Note: Called from within the interrupt handler}
var
 Data:LongWord;
begin
 {}
 {Check SPI}
 if SPI = nil then Exit;
 
 {Check Space}
 while (SPI.SourceRemain > 0) and ((PBCM2838SPI0Registers(SPI.Address).CS and BCM2838_SPI0_CS_TXD) <> 0) do
  begin
   {Write Data}
   if SPI.Source <> nil then
    begin
     Data:=PLongWord(SPI.Source)^;
     
     {Update Source}
     Inc(SPI.Source);
    end
   else
    begin
     Data:=0;
    end;    
   PBCM2838SPI0Registers(SPI.Address).FIFO:=(Data and BCM2838_SPI0_FIFO_IRQ_DATA);
 
   {Update Remain}
   Dec(SPI.SourceRemain);
  end;
end;

{==============================================================================}

function BCM2711SPI0SharedInterruptHandler(Number,CPUID,Flags:LongWord;SPI:PBCM2711SPI0Device):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;
 
 {Check SPI}
 if SPI = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Control and Status}
 Control:=PBCM2838SPI0Registers(SPI.Address).CS;
 if (Control and (BCM2838_SPI0_CS_RXR or BCM2838_SPI0_CS_DONE)) = 0 then Exit;

 {Update Statistics}
 Inc(SPI.InterruptCount);
 
 {Read FIFO}
 BCM2711SPI0ReadFIFO(SPI);

 {Write FIFO}
 BCM2711SPI0WriteFIFO(SPI);
 
 {Get Control and Status}
 Control:=PBCM2838SPI0Registers(SPI.Address).CS;
 
 {Check Done}
 if ((Control and BCM2838_SPI0_CS_DONE) <> 0) and (SPI.SourceRemain = 0) then
  begin
   {Read remaining FIFO}
   BCM2711SPI0ReadFIFO(SPI);
  
   {Reset Control (Active/Interrupt/Deassert/DMA/Clear)}
   Control:=Control and not(BCM2838_SPI0_CS_INTR or BCM2838_SPI0_CS_INTD or BCM2838_SPI0_CS_ADCS or BCM2838_SPI0_CS_DMAEN or BCM2838_SPI0_CS_TA);
   Control:=Control or (BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX);
   
   {Set Control and Status}
   PBCM2838SPI0Registers(SPI.Address).CS:=Control;
   
   {Set Data Length}
   PBCM2838SPI0Registers(SPI.Address).DLEN:=0;
  
   {Signal Semaphore}
   SemaphoreSignal(SPI.SPI.Wait);
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 Result:=INTERRUPT_RETURN_HANDLED;
end;

{==============================================================================}

procedure BCM2711SPI0DMARequestCompleted(Request:PDMARequest); 
{DMA Request completion callback for SPI0}
var
 Control:LongWord;
 SPI:PBCM2711SPI0Device;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get SPI}
 SPI:=PBCM2711SPI0Device(Request.DriverData);
 if SPI = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2838SPI0Registers(SPI.Address).CS;
 
 {Check Done}
 if (Control and BCM2838_SPI0_CS_DONE) <> 0 then
  begin
   {Reset Control (Active/Interrupt/Deassert/DMA/Clear)}
   Control:=Control and not(BCM2838_SPI0_CS_INTR or BCM2838_SPI0_CS_INTD or BCM2838_SPI0_CS_ADCS or BCM2838_SPI0_CS_DMAEN or BCM2838_SPI0_CS_TA);
   Control:=Control or (BCM2838_SPI0_CS_CLEAR_RX or BCM2838_SPI0_CS_CLEAR_TX);
   
   {Set Control and Status}
   PBCM2838SPI0Registers(SPI.Address).CS:=Control;
   
   {Set Data Length}
   PBCM2838SPI0Registers(SPI.Address).DLEN:=0;
  
   {Signal Semaphore}
   SemaphoreSignal(SPI.SPI.Wait);
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2711SPI0GetGPIOConfig(SPI:PBCM2711SPI0Device);
{Setup the GPIO config for the supplied SPI0 device}
begin
 {}
 if SPI = nil then Exit;
 
 {Check ID}
 case SPI.Id of
  0:begin
     {Setup Pins}
     SPI.SCLKPin:=GPIO_PIN_11;
     SPI.MOSIPin:=GPIO_PIN_10;
     SPI.MISOPin:=GPIO_PIN_9;
     SPI.CS0Pin:=GPIO_PIN_8;
     SPI.CS1Pin:=GPIO_PIN_7;
     SPI.CS2Pin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     SPI.SCLKFunction:=GPIO_FUNCTION_ALT0;
     SPI.MOSIFunction:=GPIO_FUNCTION_ALT0;
     SPI.MISOFunction:=GPIO_FUNCTION_ALT0;
     SPI.CS0Function:=GPIO_FUNCTION_ALT0;
     SPI.CS1Function:=GPIO_FUNCTION_ALT0;
     SPI.CS2Function:=GPIO_FUNCTION_UNKNOWN;
   end;
  3:begin
     {Setup Pins}
     SPI.SCLKPin:=GPIO_PIN_3;
     SPI.MOSIPin:=GPIO_PIN_2;
     SPI.MISOPin:=GPIO_PIN_1;
     SPI.CS0Pin:=GPIO_PIN_0;
     SPI.CS1Pin:=GPIO_PIN_24;
     SPI.CS2Pin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     SPI.SCLKFunction:=GPIO_FUNCTION_ALT3;
     SPI.MOSIFunction:=GPIO_FUNCTION_ALT3;
     SPI.MISOFunction:=GPIO_FUNCTION_ALT3;
     SPI.CS0Function:=GPIO_FUNCTION_ALT3;
     SPI.CS1Function:=GPIO_FUNCTION_ALT5;
     SPI.CS2Function:=GPIO_FUNCTION_UNKNOWN;
   end;
  4:begin
     {Setup Pins}
     SPI.SCLKPin:=GPIO_PIN_7;
     SPI.MOSIPin:=GPIO_PIN_6;
     SPI.MISOPin:=GPIO_PIN_5;
     SPI.CS0Pin:=GPIO_PIN_4;
     SPI.CS1Pin:=GPIO_PIN_25;
     SPI.CS2Pin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     SPI.SCLKFunction:=GPIO_FUNCTION_ALT3;
     SPI.MOSIFunction:=GPIO_FUNCTION_ALT3;
     SPI.MISOFunction:=GPIO_FUNCTION_ALT3;
     SPI.CS0Function:=GPIO_FUNCTION_ALT3;
     SPI.CS1Function:=GPIO_FUNCTION_ALT5;
     SPI.CS2Function:=GPIO_FUNCTION_UNKNOWN;
   end;
  5:begin
     {Setup Pins}
     SPI.SCLKPin:=GPIO_PIN_15;
     SPI.MOSIPin:=GPIO_PIN_14;
     SPI.MISOPin:=GPIO_PIN_13;
     SPI.CS0Pin:=GPIO_PIN_12;
     SPI.CS1Pin:=GPIO_PIN_26;
     SPI.CS2Pin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     SPI.SCLKFunction:=GPIO_FUNCTION_ALT3;
     SPI.MOSIFunction:=GPIO_FUNCTION_ALT3;
     SPI.MISOFunction:=GPIO_FUNCTION_ALT3;
     SPI.CS0Function:=GPIO_FUNCTION_ALT3;
     SPI.CS1Function:=GPIO_FUNCTION_ALT5;
     SPI.CS2Function:=GPIO_FUNCTION_UNKNOWN;
   end;
  6:begin
     {Setup Pins}
     SPI.SCLKPin:=GPIO_PIN_21;
     SPI.MOSIPin:=GPIO_PIN_20;
     SPI.MISOPin:=GPIO_PIN_19;
     SPI.CS0Pin:=GPIO_PIN_18;
     SPI.CS1Pin:=GPIO_PIN_27;
     SPI.CS2Pin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     SPI.SCLKFunction:=GPIO_FUNCTION_ALT3;
     SPI.MOSIFunction:=GPIO_FUNCTION_ALT3;
     SPI.MISOFunction:=GPIO_FUNCTION_ALT3;
     SPI.CS0Function:=GPIO_FUNCTION_ALT3;
     SPI.CS1Function:=GPIO_FUNCTION_ALT5;
     SPI.CS2Function:=GPIO_FUNCTION_UNKNOWN;
   end
  else
   begin
     {Setup Pins}
     SPI.SCLKPin:=GPIO_PIN_UNKNOWN;
     SPI.MOSIPin:=GPIO_PIN_UNKNOWN;
     SPI.MISOPin:=GPIO_PIN_UNKNOWN;
     SPI.CS0Pin:=GPIO_PIN_UNKNOWN;
     SPI.CS1Pin:=GPIO_PIN_UNKNOWN;
     SPI.CS2Pin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     SPI.SCLKFunction:=GPIO_FUNCTION_UNKNOWN;
     SPI.MOSIFunction:=GPIO_FUNCTION_UNKNOWN;
     SPI.MISOFunction:=GPIO_FUNCTION_UNKNOWN;
     SPI.CS0Function:=GPIO_FUNCTION_UNKNOWN;
     SPI.CS1Function:=GPIO_FUNCTION_UNKNOWN;
     SPI.CS2Function:=GPIO_FUNCTION_UNKNOWN;
   end;   
 end;
end;

{==============================================================================}
{==============================================================================}
{BCM2711 I2C0/1/2/3/4/5/6/7 Functions}
function BCM2711I2C0Start(I2C:PI2CDevice;Rate:LongWord):LongWord;
var
 Slave:LongWord;
 Divider:LongWord;
 Timeout:LongWord;
 RisingDelay:LongWord;
 FallingDelay:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Start (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Update Core Clock}
 PBCM2711I2C0Device(I2C).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
 if PBCM2711I2C0Device(I2C).CoreClock = 0 then PBCM2711I2C0Device(I2C).CoreClock:=BCM2711_I2C0_CORE_CLOCK; 
 
 {Update Properties}
 I2C.Properties.MinClock:=PBCM2711I2C0Device(I2C).CoreClock div BCM2711_I2C0_MAX_DIVIDER;
 I2C.Properties.MaxClock:=PBCM2711I2C0Device(I2C).CoreClock div BCM2711_I2C0_MIN_DIVIDER;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  CoreClock=' + IntToStr(PBCM2711I2C0Device(I2C).CoreClock) + ' MinClock=' + IntToStr(I2C.Properties.MinClock) + ' MaxClock=' + IntToStr(I2C.Properties.MaxClock));
 {$ENDIF}
 
 {Check Rate}
 if (Rate <> 0) and ((Rate < I2C.Properties.MinClock) or (Rate > I2C.Properties.MaxClock)) then Exit;
 
 {Enable GPIO Pins}
 GPIOFunctionSelect(PBCM2711I2C0Device(I2C).SDAPin,PBCM2711I2C0Device(I2C).SDAFunction);
 GPIOFunctionSelect(PBCM2711I2C0Device(I2C).SCLPin,PBCM2711I2C0Device(I2C).SCLFunction);

 {Get Divider}
 if Rate = 0 then Rate:=BCM2711_I2C0_DEFAULT_CLOCK;
 Divider:=PBCM2711I2C0Device(I2C).CoreClock div Rate;
 if (Divider and 1) <> 0 then Inc(Divider);

 {Get Timeout (35ms)}
 if Rate > ((BCM2838_BSC_CLKT_TOUT_MASK * 1000) div 35) then
  begin
   Timeout:=BCM2838_BSC_CLKT_TOUT_MASK;
  end
 else
  begin
   Timeout:=35 * (Rate div 1000);
  end;
 
 {Get Rising Edge Delay (REDL)}
 RisingDelay:=Max(Divider div 4,1);
 
 {Get Falling Edge Delay (FEDL)}
 FallingDelay:=Max(Divider div 16,1);

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Rate=' + IntToStr(Rate) + ' Divider=' + IntToStr(Divider) + ' Timeout=' + IntToStr(Timeout) + ' Falling=' + IntToStr(FallingDelay) + ' Rising=' + IntToStr(RisingDelay));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control (Disable I2C)} 
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=0;
 
 {Reset Status}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;
 
 {Set Divider}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).CDIV:=(Divider and BCM2838_BSC_CDIV_MASK);
 
 {Set Rising and Falling Delay}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).DEL:=(FallingDelay shl BCM2838_BSC_DEL_FEDL_SHIFT) or (RisingDelay shl BCM2838_BSC_DEL_REDL_SHIFT);
 
 {Set Timeout}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).CLKT:=(Timeout and BCM2838_BSC_CLKT_TOUT_MASK);

 {Get Slave}
 Slave:=(PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).A and BCM2838_BSC_A_MASK);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 {Create Wait Semaphore}
 I2C.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
 if I2C.Wait = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Register Interrupt}
 RegisterInterrupt(PBCM2711I2C0Device(I2C).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711I2C0SharedInterruptHandler),I2C);

 {Update Properties}
 I2C.ClockRate:=Rate;
 I2C.SlaveAddress:=Slave;
 I2C.Properties.ClockRate:=Rate;
 I2C.Properties.SlaveAddress:=Slave;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711I2C0Stop(I2C:PI2CDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Stop');
 {$ENDIF}
 
 {Deregister Interrupt}
 DeregisterInterrupt(PBCM2711I2C0Device(I2C).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711I2C0SharedInterruptHandler),I2C);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control (Disable I2C)} 
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=0;
 
 {Reset Status}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Destroy Wait Semaphore}
 SemaphoreDestroy(I2C.Wait);
 
 {Reset Transfer}
 PBCM2711I2C0Device(I2C).Mode:=BCM2711_I2C0_MODE_WRITE;
 PBCM2711I2C0Device(I2C).Data:=nil;
 PBCM2711I2C0Device(I2C).Count:=0;
 PBCM2711I2C0Device(I2C).Remain:=0;
 PBCM2711I2C0Device(I2C).Error:=False;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}
 
function BCM2711I2C0Read(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Read (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Size}
 if Size > BCM2711_I2C0_MAX_SIZE then Exit;
 
 {Update Statistics}
 Inc(I2C.ReadCount);
 
 {Read to Buffer}
 if Size > 0 then
  begin
   {Setup Data}
   PBCM2711I2C0Device(I2C).Mode:=BCM2711_I2C0_MODE_READ;
   PBCM2711I2C0Device(I2C).Data:=Buffer;
   PBCM2711I2C0Device(I2C).Count:=0;
   PBCM2711I2C0Device(I2C).Remain:=Size;
   PBCM2711I2C0Device(I2C).Error:=False;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Reset Status}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;

   {Setup Length}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).DLEN:=(Size and BCM2838_BSC_DLEN_MASK);
   
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
    
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).A:=(Address and BCM2838_BSC_A_MASK);
    end;    
   
   {Setup Control (Enable / Interrupt Receive / Interrupt Done / Start / Clear / Read)}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_I2CEN or BCM2838_BSC_C_INTR or BCM2838_BSC_C_INTD or BCM2838_BSC_C_ST or BCM2838_BSC_C_CLEAR or BCM2838_BSC_C_READ;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Wait for Completion}
   if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
    begin
     {Check Error}
     if not PBCM2711I2C0Device(I2C).Error then
      begin
       {Get Count}
       Count:=PBCM2711I2C0Device(I2C).Count;
      end;
     
     {Check Count}
     if Count < Size then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Read failure or timeout'); 
       
       Result:=ERROR_READ_FAULT;

       {Update Statistics}
       Inc(I2C.ReadErrors);
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Wait failure on read'); 
     
     Result:=ERROR_OPERATION_FAILED;
    end;
    
   {Reset Data}
   PBCM2711I2C0Device(I2C).Data:=nil;
   PBCM2711I2C0Device(I2C).Count:=0;
   PBCM2711I2C0Device(I2C).Remain:=0;
   PBCM2711I2C0Device(I2C).Error:=False;
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}

function BCM2711I2C0Write(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Write (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Size}
 if Size > BCM2711_I2C0_MAX_SIZE then Exit;
 
 {Update Statistics}
 Inc(I2C.WriteCount);
 
 {Write from Buffer}
 if Size > 0 then
  begin
   {Setup Data}
   PBCM2711I2C0Device(I2C).Mode:=BCM2711_I2C0_MODE_WRITE;
   PBCM2711I2C0Device(I2C).Data:=Buffer;
   PBCM2711I2C0Device(I2C).Count:=0;
   PBCM2711I2C0Device(I2C).Remain:=Size;
   PBCM2711I2C0Device(I2C).Error:=False;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Reset Status}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;

   {Setup Length}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).DLEN:=(Size and BCM2838_BSC_DLEN_MASK);
   
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then      
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
     
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).A:=(Address and BCM2838_BSC_A_MASK);
    end;    
   
   {Setup Control (Clear)}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_CLEAR;
   
   {Fill FIFO}
   BCM2711I2C0FillFIFO(PBCM2711I2C0Device(I2C));
   
   {Setup Control (Enable / Interrupt Transmit / Interrupt Done / Start)}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_I2CEN or BCM2838_BSC_C_INTT or BCM2838_BSC_C_INTD or BCM2838_BSC_C_ST;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Wait for Completion}
   if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
    begin
     {Check Error}
     if not PBCM2711I2C0Device(I2C).Error then
      begin
       {Get Count}
       Count:=PBCM2711I2C0Device(I2C).Count;
      end; 
     
     {Check Count}
     if Count < Size then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Write failure or timeout'); 
       
       Result:=ERROR_WRITE_FAULT;

       {Update Statistics}
       Inc(I2C.WriteErrors);
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Wait failure on write'); 
     
     Result:=ERROR_OPERATION_FAILED;
    end;
    
   {Reset Data}
   PBCM2711I2C0Device(I2C).Data:=nil;
   PBCM2711I2C0Device(I2C).Count:=0;
   PBCM2711I2C0Device(I2C).Remain:=0;
   PBCM2711I2C0Device(I2C).Error:=False;
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}

function BCM2711I2C0WriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
var
 Status:LongWord;
 Retries:LongWord;
 Written:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffers}
 if Initial = nil then Exit;
 if Data = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Write Read (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Sizes}
 if Len > BCM2711_I2C0_MAX_SIZE then Exit;
 if Size > BCM2711_I2C0_MAX_SIZE then Exit;

 {Check Len}
 if (Len > BCM2838_BSC_FIFO_SIZE) or not(BCM2711I2C_COMBINED_WRITEREAD) then
  begin
   Written:=0;
   
   {Write Initial}
   Result:=BCM2711I2C0Write(I2C,Address,Initial,Len,Written);
   if Result = ERROR_SUCCESS then
    begin
     {Read Data}
     Result:=BCM2711I2C0Read(I2C,Address,Data,Size,Count);
    end;
  end
 else
  begin
   {Write from Initial}
   if Len > 0 then
    begin
     {Setup Data}
     PBCM2711I2C0Device(I2C).Mode:=BCM2711_I2C0_MODE_WRITE;
     PBCM2711I2C0Device(I2C).Data:=Initial;
     PBCM2711I2C0Device(I2C).Count:=0;
     PBCM2711I2C0Device(I2C).Remain:=Len;
     PBCM2711I2C0Device(I2C).Error:=False;
     
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Reset Status}
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;
     
     {Setup Length}
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).DLEN:=(Len and BCM2838_BSC_DLEN_MASK);
     
     {Setup Address}
     if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then      
      begin
       {Update Properties}
       I2C.SlaveAddress:=Address;
       I2C.Properties.SlaveAddress:=Address;
       
       PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).A:=(Address and BCM2838_BSC_A_MASK);
      end;    
     
     {Setup Control (Clear)}
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_CLEAR;
     
     {Fill FIFO}
     BCM2711I2C0FillFIFO(PBCM2711I2C0Device(I2C));
     
     {Setup Control (Enable / Start) (No Interrupts)}
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_I2CEN or BCM2838_BSC_C_ST;
     
     {Poll Transfer Active}
     Retries:=200;
     Status:=PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S;
     while ((Status and (BCM2838_BSC_S_TA or BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE)) = 0) and (Retries > 0) do
      begin
       Status:=PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S;
       
       Dec(Retries);
      end; 

     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
      
     {Check Result}
     if (Status and (BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR) <> 0) or (Retries = 0) then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Write failure or timeout'); 
       
       Result:=ERROR_WRITE_FAULT;

       {Update Statistics}
       Inc(I2C.WriteErrors);
      end
     else if Size > 0 then
      begin
       {Read to Data}
       {Setup Data}
       PBCM2711I2C0Device(I2C).Mode:=BCM2711_I2C0_MODE_READ;
       PBCM2711I2C0Device(I2C).Data:=Data;
       PBCM2711I2C0Device(I2C).Count:=0;
       PBCM2711I2C0Device(I2C).Remain:=Size;
       PBCM2711I2C0Device(I2C).Error:=False;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Setup Length}
       PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).DLEN:=(Size and BCM2838_BSC_DLEN_MASK);
       
       {Setup Control (Enable / Interrupt Receive / Interrupt Done / Start / Read) (No Clear)}
       PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_I2CEN or BCM2838_BSC_C_INTR or BCM2838_BSC_C_INTD or BCM2838_BSC_C_ST or BCM2838_BSC_C_READ;
       
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Wait for Completion}
       if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
        begin
         {Check Error}
         if not PBCM2711I2C0Device(I2C).Error then
          begin
           {Get Count}
           Count:=PBCM2711I2C0Device(I2C).Count;
          end; 
         
         {Check Count}
         if Count < Size then
          begin
           if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Read failure or timeout'); 
           
           Result:=ERROR_READ_FAULT;

           {Update Statistics}
           Inc(I2C.ReadErrors);
          end;
        end
       else
        begin
         if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Wait failure on read'); 
         
         Result:=ERROR_OPERATION_FAILED;
        end;
      end;
      
     {Reset Data}
     PBCM2711I2C0Device(I2C).Data:=nil;
     PBCM2711I2C0Device(I2C).Count:=0;
     PBCM2711I2C0Device(I2C).Remain:=0;
     PBCM2711I2C0Device(I2C).Error:=False;
    end;

   {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
   if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Return Count=' + IntToStr(Count));
   {$ENDIF}
    
   {Return Result}
   if (Size = Count) then Result:=ERROR_SUCCESS; 
  end;  
end;

{==============================================================================}

function BCM2711I2C0WriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffers}
 if Initial = nil then Exit;
 if Data = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Write Write (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Sizes}
 if Len > BCM2838_BSC_FIFO_SIZE then Exit;
 if Size > BCM2711_I2C0_MAX_SIZE then Exit;

 {Write from Initial and Data}
 if (Len > 0) and (Size > 0) then
  begin
   {Setup Data}
   PBCM2711I2C0Device(I2C).Mode:=BCM2711_I2C0_MODE_WRITE;
   PBCM2711I2C0Device(I2C).Data:=Data;
   PBCM2711I2C0Device(I2C).Count:=0;
   PBCM2711I2C0Device(I2C).Remain:=Size;
   PBCM2711I2C0Device(I2C).Error:=False;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
     
   {Reset Status}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;
 
   {Setup Length}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).DLEN:=((Size + Len) and BCM2838_BSC_DLEN_MASK);
 
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then      
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
     
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).A:=(Address and BCM2838_BSC_A_MASK);
    end;    
   
   {Setup Control (Clear)}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_CLEAR;
 
   {Write Initial to FIFO}
   while (PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).S and BCM2838_BSC_S_TXD) <> 0 do
    begin
     {Write Initial}
     PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).FIFO:=(PLongWord(Initial)^ and BCM2838_BSC_FIFO_MASK);
   
     {Update Initial}
     Inc(Initial);
     Dec(Len);
     
     if Len = 0 then Break;
    end; 
   
   {Fill FIFO from Data}
   BCM2711I2C0FillFIFO(PBCM2711I2C0Device(I2C));
   
   {Setup Control (Enable / Interrupt Transmit / Interrupt Done / Start)}
   PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).C:=BCM2838_BSC_C_I2CEN or BCM2838_BSC_C_INTT or BCM2838_BSC_C_INTD or BCM2838_BSC_C_ST;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Wait for Completion}
   if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
    begin
     {Check Error}
     if not PBCM2711I2C0Device(I2C).Error then
      begin
       {Get Count}
       Count:=PBCM2711I2C0Device(I2C).Count;
      end; 
     
     {Check Count}
     if Count < Size then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Write failure or timeout'); 
       
       Result:=ERROR_WRITE_FAULT;

       {Update Statistics}
       Inc(I2C.WriteErrors);
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Wait failure on write'); 
     
     Result:=ERROR_OPERATION_FAILED;
    end;
    
   {Reset Data}
   PBCM2711I2C0Device(I2C).Data:=nil;
   PBCM2711I2C0Device(I2C).Count:=0;
   PBCM2711I2C0Device(I2C).Remain:=0;
   PBCM2711I2C0Device(I2C).Error:=False;
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end; 
   
{==============================================================================}
 
function BCM2711I2C0SetRate(I2C:PI2CDevice;Rate:LongWord):LongWord;
var
 Divider:LongWord;
 Timeout:LongWord;
 RisingDelay:LongWord;
 FallingDelay:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Set Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Check Rate}
 if (Rate < I2C.Properties.MinClock) or (Rate > I2C.Properties.MaxClock) then Exit;
 
 {Get Divider}
 Divider:=PBCM2711I2C0Device(I2C).CoreClock div Rate;
 if (Divider and 1) <> 0 then Inc(Divider);

 {Get Timeout (35ms)}
 if Rate > ((BCM2838_BSC_CLKT_TOUT_MASK * 1000) div 35) then
  begin
   Timeout:=BCM2838_BSC_CLKT_TOUT_MASK;
  end
 else
  begin
   Timeout:=35 * (Rate div 1000);
  end;
 
 {Get Rising Edge Delay (REDL)}
 RisingDelay:=Max(Divider div 4,1);
 
 {Get Falling Edge Delay (FEDL)}
 FallingDelay:=Max(Divider div 16,1);

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Divider=' + IntToStr(Divider) + ' Timeout=' + IntToStr(Timeout) + ' Falling=' + IntToStr(FallingDelay) + ' Rising=' + IntToStr(RisingDelay));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Divider}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).CDIV:=(Divider and BCM2838_BSC_CDIV_MASK);

 {Set Rising and Falling Delay}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).DEL:=(FallingDelay shl BCM2838_BSC_DEL_FEDL_SHIFT) or (RisingDelay shl BCM2838_BSC_DEL_REDL_SHIFT);
 
 {Set Timeout}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).CLKT:=(Timeout and BCM2838_BSC_CLKT_TOUT_MASK);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 I2C.ClockRate:=Rate;
 I2C.Properties.ClockRate:=Rate;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}
 
function BCM2711I2C0SetAddress(I2C:PI2CDevice;Address:Word):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C0 Set Address (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Address}
 PBCM2838BSCRegisters(PBCM2711I2C0Device(I2C).Address).A:=(Address and BCM2838_BSC_A_MASK);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 I2C.SlaveAddress:=Address;
 I2C.Properties.SlaveAddress:=Address;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}

procedure BCM2711I2C0FillFIFO(I2C:PBCM2711I2C0Device);
{Caller will hold the I2C device lock}
{Note: Called from within the interrupt handler}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Mode}
 if I2C.Mode = BCM2711_I2C0_MODE_READ then Exit;
 
 {Check Space}
 while (I2C.Remain > 0) and ((PBCM2838BSCRegisters(I2C.Address).S and BCM2838_BSC_S_TXD) <> 0) do
  begin
   {Write Data}
   PBCM2838BSCRegisters(I2C.Address).FIFO:=(PLongWord(I2C.Data)^ and BCM2838_BSC_FIFO_MASK);
   
   {Update Data}
   Inc(I2C.Data);
   Inc(I2C.Count);
   Dec(I2C.Remain);
  end;
end;

{==============================================================================}

procedure BCM2711I2C0DrainFIFO(I2C:PBCM2711I2C0Device);
{Caller will hold the I2C device lock}
{Note: Called from within the interrupt handler}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Mode}
 if I2C.Mode = BCM2711_I2C0_MODE_WRITE then Exit;
 
 {Check Data}
 while (I2C.Remain > 0) and ((PBCM2838BSCRegisters(I2C.Address).S and BCM2838_BSC_S_RXD) <> 0) do
  begin
   {Read Data}
   PByte(I2C.Data)^:=(PBCM2838BSCRegisters(I2C.Address).FIFO and BCM2838_BSC_FIFO_MASK);
   
   {Update Data}
   Inc(I2C.Data);
   Inc(I2C.Count);
   Dec(I2C.Remain);
  end;
end;

{==============================================================================}

function BCM2711I2C0SharedInterruptHandler(Number,CPUID,Flags:LongWord;I2C:PBCM2711I2C0Device):LongWord;
{Note: Thread submitting the current request will hold the I2C device lock}
var
 Status:LongWord;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 if I2C.Data <> nil then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Read Status}
   Status:=PBCM2838BSCRegisters(I2C.Address).S;
   
   {Check Status}
   if (Status and (BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR)) <> 0 then
    begin
     {Error}
     I2C.Error:=True;
     
     {Update Statistics}
     Inc(I2C.InterruptCount);
     
     {Reset Control (Disable I2C)} 
     PBCM2838BSCRegisters(I2C.Address).C:=0;
     
     {Reset Status}
     PBCM2838BSCRegisters(I2C.Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;
     
     {Signal Semaphore}
     SemaphoreSignal(I2C.I2C.Wait);
     
     Result:=INTERRUPT_RETURN_HANDLED;
    end
   else if (Status and BCM2838_BSC_S_DONE) <> 0 then
    begin
     {Completed}
     {Update Statistics}
     Inc(I2C.InterruptCount);

     {Check Mode}
     if I2C.Mode = BCM2711_I2C0_MODE_READ then
      begin
       {Drain FIFO}
       BCM2711I2C0DrainFIFO(I2C);
      end;
      
     {Reset Control (Disable I2C)} 
     PBCM2838BSCRegisters(I2C.Address).C:=0;
     
     {Reset Status}
     PBCM2838BSCRegisters(I2C.Address).S:=BCM2838_BSC_S_CLKT or BCM2838_BSC_S_ERR or BCM2838_BSC_S_DONE;
     
     {Signal Semaphore}
     SemaphoreSignal(I2C.I2C.Wait);
     
     Result:=INTERRUPT_RETURN_HANDLED;
    end
   else if (Status and BCM2838_BSC_S_RXR) <> 0 then 
    begin
     {Receive}
     {Update Statistics}
     Inc(I2C.InterruptCount);

     {Drain FIFO}
     BCM2711I2C0DrainFIFO(I2C);
     
     Result:=INTERRUPT_RETURN_HANDLED;
    end
   else if (Status and BCM2838_BSC_S_TXW) <> 0 then 
    begin
     {Transmit}
     {Update Statistics}
     Inc(I2C.InterruptCount);

     {Fill FIFO}
     BCM2711I2C0FillFIFO(I2C);
     
     Result:=INTERRUPT_RETURN_HANDLED;
    end;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;
end; 

{==============================================================================}

procedure BCM2711I2C0GetGPIOConfig(I2C:PBCM2711I2C0Device);
{Setup the GPIO config for the supplied I2C0 device}
begin
 {}
 if I2C = nil then Exit;
 
 {Check ID}
 case I2C.Id of
  0:begin
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_0;
     I2C.SCLPin:=GPIO_PIN_1;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_ALT0;
     I2C.SCLFunction:=GPIO_FUNCTION_ALT0;
   end;  
  1:begin
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_2;
     I2C.SCLPin:=GPIO_PIN_3;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_ALT0;
     I2C.SCLFunction:=GPIO_FUNCTION_ALT0;
   end;  
  2:begin
     {I2C2 is connected to HDMI0, no GPIO config}
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_UNKNOWN;
     I2C.SCLPin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_UNKNOWN;
     I2C.SCLFunction:=GPIO_FUNCTION_UNKNOWN;
   end;
  3:begin
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_4;
     I2C.SCLPin:=GPIO_PIN_5;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_ALT5;
     I2C.SCLFunction:=GPIO_FUNCTION_ALT5;
   end;  
  4:begin
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_6;
     I2C.SCLPin:=GPIO_PIN_7;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_ALT5;
     I2C.SCLFunction:=GPIO_FUNCTION_ALT5;
   end;  
  5:begin
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_12;
     I2C.SCLPin:=GPIO_PIN_13;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_ALT5;
     I2C.SCLFunction:=GPIO_FUNCTION_ALT5;
   end;  
  6:begin
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_22;
     I2C.SCLPin:=GPIO_PIN_23;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_ALT5;
     I2C.SCLFunction:=GPIO_FUNCTION_ALT5;
   end;  
  7:begin
     {I2C7 is connected to HDMI1, no GPIO config}
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_UNKNOWN;
     I2C.SCLPin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_UNKNOWN;
     I2C.SCLFunction:=GPIO_FUNCTION_UNKNOWN;
   end;
  else
   begin
     {Setup Pins}
     I2C.SDAPin:=GPIO_PIN_UNKNOWN;
     I2C.SCLPin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     I2C.SDAFunction:=GPIO_FUNCTION_UNKNOWN;
     I2C.SCLFunction:=GPIO_FUNCTION_UNKNOWN;
   end;   
 end; 
end;

{==============================================================================}
{==============================================================================}
{BCM2711 SPI1/2 (AUX) Functions}
//To Do 

{==============================================================================}
{==============================================================================}
{BCM2711 SPI/I2C Slave Functions}
function BCM2711I2CSlaveStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
{Implementation of I2CSlaveStart API for BCM2711 I2C slave}
{Note: Not intended to be called directly by applications, use I2CSlaveStart instead}

{Note: Rate is not applicable for I2C slave devices}
var
 Address:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C Slave Start');
 {$ENDIF}

 {Enable GPIO Pins}
 GPIOFunctionSelect(PBCM2711I2CSlave(I2C).SDAPin,PBCM2711I2CSlave(I2C).SDAFunction);
 GPIOFunctionSelect(PBCM2711I2CSlave(I2C).SCLPin,PBCM2711I2CSlave(I2C).SCLFunction);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Setup FIFO Interrupt Level (2 bytes RX / 2 bytes TX}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).IFLS:=BCM2838_I2CSPI_IFLS_RXIFLSEL1_8 or BCM2838_I2CSPI_IFLS_TXIFLSEL1_8;

 {Clear Interrupts}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).ICR:=$F;

 {Clear Errors}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).RSR:=0;

 {Clear FIFO}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).CR:=BCM2838_I2CSPI_CR_BRK;

 {Create Receive Semaphore}
 PBCM2711I2CSlave(I2C).Receive.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
 if PBCM2711I2CSlave(I2C).Receive.Wait = INVALID_HANDLE_VALUE then
  begin
   if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Failed to create receive semaphore');

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
 {Create Transmit Semaphore}
 PBCM2711I2CSlave(I2C).Transmit.Wait:=SemaphoreCreateEx(BCM2711_I2CSLAVE_BUFFER_SIZE,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
 if PBCM2711I2CSlave(I2C).Transmit.Wait = INVALID_HANDLE_VALUE then
  begin
   if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Failed to create transmit semaphore');

   SemaphoreDestroy(PBCM2711I2CSlave(I2C).Receive.Wait);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 {$ELSE}
 {Create Wait Semaphore}
 I2C.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
 if I2C.Wait = INVALID_HANDLE_VALUE then
  begin
   if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Failed to create wait semaphore');

   SemaphoreDestroy(PBCM2711I2CSlave(I2C).Receive.Wait);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 {$ENDIF}

 {Allocate Lock}
 PBCM2711I2CSlave(I2C).Lock:=SpinCreate;
 if PBCM2711I2CSlave(I2C).Lock = INVALID_HANDLE_VALUE then
  begin
   if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Failed to create device lock');

   SemaphoreDestroy(PBCM2711I2CSlave(I2C).Receive.Wait);
   {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
   SemaphoreDestroy(PBCM2711I2CSlave(I2C).Transmit.Wait);
   {$ELSE}
   SemaphoreDestroy(I2C.Wait);
   {$ENDIF}

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end; 

 {Reset Receive Buffer}
 PBCM2711I2CSlave(I2C).Receive.Start:=0;
 PBCM2711I2CSlave(I2C).Receive.Count:=0;

 {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
 {Reset Transmit Buffer}
 PBCM2711I2CSlave(I2C).Transmit.Start:=0;
 PBCM2711I2CSlave(I2C).Transmit.Count:=0;
 {$ENDIF}

 {Enable Interrupts}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).IMSC:=BCM2838_I2CSPI_IMSC_RXIM or BCM2838_I2CSPI_IMSC_TXIM;

 {Enable TX, I2C and device}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).CR:=BCM2838_I2CSPI_CR_EN or BCM2838_I2CSPI_CR_I2C or BCM2838_I2CSPI_CR_TXE or BCM2838_I2CSPI_CR_RXE;

 {Get Address}
 Address:=(PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).SLV and BCM2838_I2CSPI_SLV_ADDR_MASK);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Request IRQ}
 RegisterInterrupt(PBCM2711I2CSlave(I2C).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711I2CSlaveInterruptHandler),I2C);

 {Update Properties}
 I2C.SlaveAddress:=Address;
 I2C.Properties.SlaveAddress:=Address;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711I2CSlaveStop(I2C:PI2CDevice):LongWord;
{Implementation of I2CSlaveStop API for BCM2711 I2C slave}
{Note: Not intended to be called directly by applications, use I2CSlaveStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C Slave Stop');
 {$ENDIF}
 
 {Release IRQ}
 DeregisterInterrupt(PBCM2711I2CSlave(I2C).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711I2CSlaveInterruptHandler),I2C);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable Interrupts}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).IMSC:=0;

 {Disable I2C and device}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).CR:=0;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Destroy Lock}
 SpinDestroy(PBCM2711I2CSlave(I2C).Lock);
 PBCM2711I2CSlave(I2C).Lock:=INVALID_HANDLE_VALUE;

 {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
 {Destroy Transmit Semaphore}
 SemaphoreDestroy(PBCM2711I2CSlave(I2C).Transmit.Wait);
 {$ELSE}
 {Destroy Wait Semaphore}
 SemaphoreDestroy(I2C.Wait);
 {$ENDIF}

 {Destroy Receive Semaphore}
 SemaphoreDestroy(PBCM2711I2CSlave(I2C).Receive.Wait);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711I2CSlaveRead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Implementation of I2CSlaveRead API for BCM2711 I2C slave}
{Note: Not intended to be called directly by applications, use I2CSlaveRead instead}

{Note: Address is not applicable for I2C slave devices}

 function BCM2711I2CSlaveReceive(I2C:PBCM2711I2CSlave):LongWord;
 begin
  {}
  {Acquire the Lock}
  if SpinLockIRQ(I2C.Lock) = ERROR_SUCCESS then
   begin
    {Drain FIFO}
    BCM2711I2CSlaveDrainFIFO(I2C);

    {Release the Lock}
    SpinUnlockIRQ(I2C.Lock);

    Result:=ERROR_SUCCESS;
   end
  else
   begin
    Result:=ERROR_CAN_NOT_COMPLETE;
   end;
 end;

var
 Total:LongWord;
 Offset:LongWord;
 Status:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C Slave Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Update Statistics}
 Inc(I2C.ReadCount);

 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Receive Data}
   (*Status:=BCM2711I2CSlaveReceive(PBCM2711I2CSlave(I2C));
   if Status <> ERROR_SUCCESS then
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Receive failure on read');

     Result:=Status;
     Break;
    end;*)

   {Check Errors}
   Status:=PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).RSR;
   if (Status and BCM2838_I2CSPI_RSR_OE) <> 0 then
    begin
     {Clear Error}
     PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).RSR:=PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).RSR and not(BCM2838_I2CSPI_RSR_OE);

     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Overrun error on read');

     {Update Statistics}
     Inc(I2C.ReadErrors);

     Result:=ERROR_READ_FAULT;
     {Break;}
    end;

   {Release the Lock}
   MutexUnlock(I2C.Lock);

   {Wait for Data}
   Status:=SemaphoreWaitEx(PBCM2711I2CSlave(I2C).Receive.Wait,BCM2711_I2CSLAVE_TIMEOUT);

   {Acquire the Lock}
   if MutexLock(I2C.Lock) = ERROR_SUCCESS then
    begin
     if Status = ERROR_SUCCESS then
      begin
       while (PBCM2711I2CSlave(I2C).Receive.Count > 0) and (Size > 0) do
        begin
         {Acquire the Lock}
         if SpinLockIRQ(PBCM2711I2CSlave(I2C).Lock) = ERROR_SUCCESS then
          begin
           {Read Data}
           PByte(Buffer + Offset)^:=PBCM2711I2CSlave(I2C).Receive.Buffer[PBCM2711I2CSlave(I2C).Receive.Start];

           {Update Start}
           PBCM2711I2CSlave(I2C).Receive.Start:=(PBCM2711I2CSlave(I2C).Receive.Start + 1) mod BCM2711_I2CSLAVE_BUFFER_SIZE;

           {Update Count}
           Dec(PBCM2711I2CSlave(I2C).Receive.Count);

           {Release the Lock}
           SpinUnlockIRQ(PBCM2711I2CSlave(I2C).Lock);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;

         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Check Count}
         if (PBCM2711I2CSlave(I2C).Receive.Count = 0) or (Size = 0) then Break;
         
         {Decrement Wait}
         if SemaphoreWait(PBCM2711I2CSlave(I2C).Receive.Wait) <> ERROR_SUCCESS then Break;
        end;

       {Check Count}
       (*if PBCM2711I2CSlave(I2C).Receive.Count < BCM2711_I2CSLAVE_BUFFER_SIZE then
        begin
         {Acquire the Lock}
         if SpinLockIRQ(PBCM2711I2CSlave(I2C).Lock) = ERROR_SUCCESS then
          begin
           {Enable Interrupts for RX FIFO}
           PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).IMSC:=PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).IMSC or BCM2838_I2CSPI_IMSC_RXIM;

           {Release the Lock}
           SpinUnlockIRQ(PBCM2711I2CSlave(I2C).Lock);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;
        end;*)
      end
     else if Status = ERROR_WAIT_TIMEOUT then
      begin
       {Receive Data}
       Status:=BCM2711I2CSlaveReceive(PBCM2711I2CSlave(I2C));
       if Status <> ERROR_SUCCESS then
        begin
         if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Receive failure on read');

         Result:=Status;
         Break;
        end;
      end
     else
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Wait failure on read');

       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Failed to acquire lock');

     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;

   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
  end;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711I2CSlaveWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Implementation of I2CSlaveWrite API for BCM2711 I2C slave}
{Note: Not intended to be called directly by applications, use I2CSlaveWrite instead}

{Note: Address is not applicable for I2C slave devices}

 {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
 function BCM2711I2CSlaveTransmit(I2C:PBCM2711I2CSlave):LongWord;
 begin
  {}
  {Acquire the Lock}
  if SpinLockIRQ(I2C.Lock) = ERROR_SUCCESS then
   begin
    {Fill FIFO}
    BCM2711I2CSlaveFillFIFO(I2C);

    {Check Count}
    if I2C.Transmit.Count > 0 then
     begin
      {Enable Interrupts for TX FIFO}
      PBCM2838I2CSPIRegisters(I2C.Address).IMSC:=PBCM2838I2CSPIRegisters(I2C.Address).IMSC or BCM2838_I2CSPI_IMSC_TXIM;
     end;

    {Release the Lock}
    SpinUnlockIRQ(I2C.Lock);

    Result:=ERROR_SUCCESS;
   end
  else
   begin
    Result:=ERROR_CAN_NOT_COMPLETE;
   end;
 end;
 {$ENDIF}

var
 Total:LongWord;
 Offset:LongWord;
 Status:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C Slave Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Update Statistics}
 Inc(I2C.WriteCount);

 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
   {Transmit Data}
   Status:=BCM2711I2CSlaveTransmit(PBCM2711I2CSlave(I2C));
   if Status <> ERROR_SUCCESS then
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Transmit failure on write');

     Result:=Status;
     Break;
    end;
   {$ENDIF}

   {Check Errors}
   Status:=PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).RSR;
   if (Status and BCM2838_I2CSPI_RSR_UE) <> 0 then
    begin
     {Clear Error}
     PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).RSR:=PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).RSR and not(BCM2838_I2CSPI_RSR_UE);

     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Underrun error on write');

     {Update Statistics}
     Inc(I2C.WriteErrors);

     Result:=ERROR_WRITE_FAULT;
     {Break;}
    end;

   {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
   {Release the Lock}
   MutexUnlock(I2C.Lock);

   {Wait for Space}
   Status:=SemaphoreWaitEx(PBCM2711I2CSlave(I2C).Transmit.Wait,BCM2711_I2CSLAVE_TIMEOUT);

   {Acquire the Lock}
   if MutexLock(I2C.Lock) = ERROR_SUCCESS then
    begin
     if Status = ERROR_SUCCESS then
      begin
       while (PBCM2711I2CSlave(I2C).Transmit.Count < BCM2711_I2CSLAVE_BUFFER_SIZE) and (Size > 0) do
        begin
         {Acquire the Lock}
         if SpinLockIRQ(PBCM2711I2CSlave(I2C).Lock) = ERROR_SUCCESS then
          begin
           {Write Data}
           PBCM2711I2CSlave(I2C).Transmit.Buffer[(PBCM2711I2CSlave(I2C).Transmit.Start + PBCM2711I2CSlave(I2C).Transmit.Count) mod BCM2711_I2CSLAVE_BUFFER_SIZE]:=PByte(Buffer + Offset)^;

           {Update Count}
           Inc(PBCM2711I2CSlave(I2C).Transmit.Count);

           {Release the Lock}
           SpinUnlockIRQ(PBCM2711I2CSlave(I2C).Lock);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;

         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Check Count}
         if (PBCM2711I2CSlave(I2C).Transmit.Count = BCM2711_I2CSLAVE_BUFFER_SIZE) or (Size = 0) then Break;
         
         {Decrement Wait}
         SemaphoreWait(PBCM2711I2CSlave(I2C).Transmit.Wait);
        end;

       {Transmit Data}
       Status:=BCM2711I2CSlaveTransmit(PBCM2711I2CSlave(I2C));
       if Status <> ERROR_SUCCESS then
        begin
         if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Transmit failure on write');

         Result:=Status;
         Break;
        end;
      end
     else if Status = ERROR_WAIT_TIMEOUT then
      begin
       {Transmit Data}
       Status:=BCM2711I2CSlaveTransmit(PBCM2711I2CSlave(I2C));
       if Status <> ERROR_SUCCESS then
        begin
         if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Transmit failure on write');

         Result:=Status;
         Break;
        end;
      end
     else
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Wait failure on write');

       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2711: Failed to acquire lock');

     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;
   {$ELSE}
   {Setup Result}
   Result:=ERROR_CAN_NOT_COMPLETE;

   {Acquire the Lock}
   if SpinLockIRQ(PBCM2711I2CSlave(I2C).Lock) <> ERROR_SUCCESS then Exit;

   {Check Size}
   while Size > 0 do
    begin
     {Check Space}
     if (PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).FR and BCM2838_I2CSPI_FR_TXFF) = 0 then
      begin
       {Write Data}
       PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).DR:=(PLongWord(Buffer + Offset)^ and BCM2838_I2CSPI_DR_DATA_MASK);

       {Update Count}
       Inc(Count);
       
       {Update Size and Offset}
       Dec(Size);
       Inc(Offset);
      end
     else
      begin
       {Enable Interrupts for TX FIFO}
       PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).IMSC:=PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).IMSC or BCM2838_I2CSPI_IMSC_TXIM;

       {Release the Lock}
       SpinUnlockIRQ(PBCM2711I2CSlave(I2C).Lock);

       {Release the Lock}
       MutexUnlock(I2C.Lock);

       {Wait for Space}
       if SemaphoreWait(I2C.Wait) <> ERROR_SUCCESS then Exit;

       {Acquire the Lock}
       if MutexLock(I2C.Lock) <> ERROR_SUCCESS then Exit;

       {Acquire the Lock}
       if SpinLockIRQ(PBCM2711I2CSlave(I2C).Lock) <> ERROR_SUCCESS then Exit;
      end; 
    end;

   {Release the Lock}
   SpinUnlockIRQ(PBCM2711I2CSlave(I2C).Lock);
   {$ENDIF}

   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
  end;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711I2CSlaveSetAddress(I2C:PI2CDevice;Address:Word):LongWord;
{Implementation of I2CSlaveSetAddress API for BCM2711 I2C slave}
{Note: Not intended to be called directly by applications, use I2CSlaveSetAddress instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2711: I2C Slave Set Address (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set Address}
 PBCM2838I2CSPIRegisters(PBCM2711I2CSlave(I2C).Address).SLV:=(Address and BCM2838_I2CSPI_SLV_ADDR_MASK);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}

 {Update Properties}
 I2C.SlaveAddress:=Address;
 I2C.Properties.SlaveAddress:=Address;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
procedure BCM2711I2CSlaveFillFIFO(I2C:PBCM2711I2CSlave);
{Fill the transmit FIFO from the transmit buffer}
{Note: Caller must hold the I2C slave interrupt lock}
{Note: Called from within the interrupt handler}
var
 Value:LongWord;
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {Check Space}
 while (I2C.Transmit.Count > 0) and ((PBCM2838I2CSPIRegisters(I2C.Address).FR and BCM2838_I2CSPI_FR_TXFF) = 0) do
  begin
   {Get Data}
   Value:=I2C.Transmit.Buffer[I2C.Transmit.Start];

   {Write Data}
   PBCM2838I2CSPIRegisters(I2C.Address).DR:=(Value and BCM2838_I2CSPI_DR_DATA_MASK);

   {Update Start}
   I2C.Transmit.Start:=(I2C.Transmit.Start + 1) mod BCM2711_I2CSLAVE_BUFFER_SIZE;

   {Update Count}
   Dec(I2C.Transmit.Count);

   {Signal Semaphore}
   SemaphoreSignal(I2C.Transmit.Wait);
  end;
end;
{$ENDIF}
{==============================================================================}

procedure BCM2711I2CSlaveDrainFIFO(I2C:PBCM2711I2CSlave);
{Drain the receive FIFO to the receive buffer}
{Note: Caller must hold the I2C slave interrupt lock}
{Note: Called from within the interrupt handler}
var
 Limit:LongWord;
 Value:LongWord;
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {Check Data}
 Limit:=BCM2711_I2CSLAVE_RX_POLL_LIMIT;
 while (I2C.Receive.Count < BCM2711_I2CSLAVE_BUFFER_SIZE) and ((PBCM2838I2CSPIRegisters(I2C.Address).FR and BCM2838_I2CSPI_FR_RXFE) = 0) do
  begin
   {Read Data}
   Value:=(PBCM2838I2CSPIRegisters(I2C.Address).DR and BCM2838_I2CSPI_DR_DATA_MASK);

   {Put Data}
   I2C.Receive.Buffer[(I2C.Receive.Start + I2C.Receive.Count) mod BCM2711_I2CSLAVE_BUFFER_SIZE]:=Value;

   {Update Count}
   Inc(I2C.Receive.Count);

   {Signal Semaphore}
   SemaphoreSignal(I2C.Receive.Wait);

   {Update Limit}
   Dec(Limit);
   if Limit = 0 then Break;
  end;
end;

{==============================================================================}

function BCM2711I2CSlaveInterruptHandler(Number,CPUID,Flags:LongWord;I2C:PBCM2711I2CSlave):LongWord;
{Interrupt handler for the BCM2711 I2C slave}
{Note: Not intended to be called directly by applications}
var
 Status:LongWord;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;

 {Check I2C}
 if I2C = nil then Exit;

 {Acquire Lock}
 if SpinLockIRQ(I2C.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Read Status}
 Status:=PBCM2838I2CSPIRegisters(I2C.Address).MIS;

 {Check Status}
 if (Status and BCM2838_I2CSPI_MIS_RXMIS) <> 0 then 
  begin
   {Receive}
   {Update Statistics}
   Inc(I2C.InterruptCount);

   {Clear Interrupt}
   PBCM2838I2CSPIRegisters(I2C.Address).ICR:=BCM2838_I2CSPI_ICR_RXIC;

   {Drain FIFO}
   BCM2711I2CSlaveDrainFIFO(I2C);

   {Check Count}
   (*if I2C.Receive.Count = BCM2711_I2CSLAVE_BUFFER_SIZE then
    begin
     {Disable Interrupt}
     PBCM2838I2CSPIRegisters(I2C.Address).IMSC:=PBCM2838I2CSPIRegisters(I2C.Address).IMSC and not(BCM2838_I2CSPI_IMSC_RXIM);
    end;*)

   Result:=INTERRUPT_RETURN_HANDLED;
  end
 else if (Status and BCM2838_I2CSPI_MIS_TXMIS) <> 0 then 
  begin
   {Transmit}
   {Update Statistics}
   Inc(I2C.InterruptCount);

   {Clear Interrupt}
   PBCM2838I2CSPIRegisters(I2C.Address).ICR:=BCM2838_I2CSPI_ICR_TXIC;

   {$IFDEF BCM2711_I2CSLAVE_TX_BUFFER}
   {Fill FIFO}
   BCM2711I2CSlaveFillFIFO(I2C);

   {Check Count}
   if I2C.Transmit.Count = 0 then
    begin
     {Disable Interrupt}
     PBCM2838I2CSPIRegisters(I2C.Address).IMSC:=PBCM2838I2CSPIRegisters(I2C.Address).IMSC and not(BCM2838_I2CSPI_IMSC_TXIM);
    end;
   {$ELSE}
   {Disable Interrupt}
   PBCM2838I2CSPIRegisters(I2C.Address).IMSC:=PBCM2838I2CSPIRegisters(I2C.Address).IMSC and not(BCM2838_I2CSPI_IMSC_TXIM);

   {Signal Semaphore}
   SemaphoreSignal(I2C.I2C.Wait);
   {$ENDIF}

   Result:=INTERRUPT_RETURN_HANDLED;
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 {Release Lock}
 SpinUnlockIRQ(I2C.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2711 DMA Functions}
function BCM2711DMAHostStart(DMA:PDMAHost):LongWord;
var
 Mask:LongWord;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Get Channel Mask}
 PBCM2711DMAHost(DMA).ChannelMask:=DMAGetChannels;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2711: Channel mask = ' + IntToHex(PBCM2711DMAHost(DMA).ChannelMask,8));
 {$ENDIF}
 
 {Get Channel Free}
 PBCM2711DMAHost(DMA).ChannelFree:=PBCM2711DMAHost(DMA).ChannelMask;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2711: Channel free = ' + IntToHex(PBCM2711DMAHost(DMA).ChannelFree,8));
 {$ENDIF}
 
 {Create Channel Lock}
 PBCM2711DMAHost(DMA).ChannelLock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if PBCM2711DMAHost(DMA).ChannelLock = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Count Free Normal Channels}
 Mask:=(PBCM2711DMAHost(DMA).ChannelMask and BCM2711_DMA_NORMAL_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2711: Normal channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create Normal Channel Semaphore}
 PBCM2711DMAHost(DMA).ChannelWait:=SemaphoreCreate(Count);
 if PBCM2711DMAHost(DMA).ChannelWait = INVALID_HANDLE_VALUE then
  begin
   {Destroy Channel Lock}
   MutexDestroy(PBCM2711DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Count Free DMA 40 Channels}
 Mask:=(PBCM2711DMAHost(DMA).ChannelMask and BCM2711_DMA_40_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2711: DMA 40 channel free count = ' + IntToStr(Count));
 {$ENDIF}

 {Create DMA 40 Channel Semaphore}
 PBCM2711DMAHost(DMA).Channel40:=SemaphoreCreate(Count);
 if PBCM2711DMAHost(DMA).Channel40 = INVALID_HANDLE_VALUE then
  begin
   {Destroy Normal Channel Semaphore}
   SemaphoreDestroy(PBCM2711DMAHost(DMA).ChannelWait);
   
   {Destroy Channel Lock}
   MutexDestroy(PBCM2711DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Count Free DMA Lite Channels}
 Mask:=(PBCM2711DMAHost(DMA).ChannelMask and BCM2711_DMA_LITE_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2711: DMA Lite channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create DMA Lite Channel Semaphore}
 PBCM2711DMAHost(DMA).ChannelLite:=SemaphoreCreate(Count);
 if PBCM2711DMAHost(DMA).ChannelLite = INVALID_HANDLE_VALUE then
  begin
   {Destroy DMA 40 Channel Semaphore}
   SemaphoreDestroy(PBCM2711DMAHost(DMA).Channel40);
   
   {Destroy Normal Channel Semaphore}
   SemaphoreDestroy(PBCM2711DMAHost(DMA).ChannelWait);
   
   {Destroy Channel Lock}
   MutexDestroy(PBCM2711DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Count Free DMA Bulk Channels}
 Mask:=(PBCM2711DMAHost(DMA).ChannelMask and BCM2711_DMA_BULK_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2711: DMA Bulk channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create DMA Bulk Channel Semaphore}
 PBCM2711DMAHost(DMA).ChannelBulk:=SemaphoreCreate(Count);
 if PBCM2711DMAHost(DMA).ChannelBulk = INVALID_HANDLE_VALUE then
  begin
   {Destroy DMA Lite Channel Semaphore}
   SemaphoreDestroy(PBCM2711DMAHost(DMA).ChannelLite);

   {Destroy DMA 40 Channel Semaphore}
   SemaphoreDestroy(PBCM2711DMAHost(DMA).Channel40);
  
   {Destroy Normal Channel Semaphore}
   SemaphoreDestroy(PBCM2711DMAHost(DMA).ChannelWait);
   
   {Destroy Channel Lock}
   MutexDestroy(PBCM2711DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Setup Enable Register}
 PBCM2711DMAHost(DMA).EnableRegister:=PLongWord(BCM2838_DMA_ENABLE_BASE);
 
 {Setup Interrupt Register}
 PBCM2711DMAHost(DMA).InterruptRegister:=PLongWord(BCM2838_DMA_INT_STATUS_BASE);
 
 {Start Channels}
 for Count:=0 to BCM2711_DMA_CHANNEL_COUNT - 1 do
  begin
   {Host}
   PBCM2711DMAHost(DMA).Channels[Count].Host:=PBCM2711DMAHost(DMA);
   
   {Channel No}
   PBCM2711DMAHost(DMA).Channels[Count].Number:=Count;
   
   {Check Available}
   if (PBCM2711DMAHost(DMA).ChannelMask and (1 shl Count)) <> 0 then
    begin
     {Check Channel}
     case Count of
      {Channels 0 to 6}
      0..6:begin
        {Interrupt No}
        PBCM2711DMAHost(DMA).Channels[Count].Interrupt:=BCM2838_IRQ_DMA0 + Count;
      
        {Registers}
        PBCM2711DMAHost(DMA).Channels[Count].Registers:=PBCM2838DMARegisters(BCM2838_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2711DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2711DMAInterruptHandler),@PBCM2711DMAHost(DMA).Channels[Count]);
       end;
      {Channels 7 and 8 (Lite)}
      7..8:begin
        {Interrupt No}
        PBCM2711DMAHost(DMA).Channels[Count].Interrupt:=BCM2838_IRQ_DMA7_8;
      
        {Registers}
        PBCM2711DMAHost(DMA).Channels[Count].Registers:=PBCM2838DMARegisters(BCM2838_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2711DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2711DMASharedInterruptHandler),DMA);
       end; 
      {Channels 9 and 10 (Lite)}
      9..10:begin
        {Interrupt No}
        PBCM2711DMAHost(DMA).Channels[Count].Interrupt:=BCM2838_IRQ_DMA9_10;
      
        {Registers}
        PBCM2711DMAHost(DMA).Channels[Count].Registers:=PBCM2838DMARegisters(BCM2838_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2711DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2711DMASharedInterruptHandler),DMA);
       end; 
      {Channels 11 to 14 (40 bit)}
      11..14:begin
        {Interrupt No}
        PBCM2711DMAHost(DMA).Channels[Count].Interrupt:=BCM2838_IRQ_DMA11 + (Count - 11);
      
        {Registers40}
        PBCM2711DMAHost(DMA).Channels[Count].Registers40:=PBCM2838DMA40Registers(BCM2838_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2711DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2711DMA40InterruptHandler),@PBCM2711DMAHost(DMA).Channels[Count]);
       end;
      {Channel 15}
      15:begin
        {Interrupt No (Only available on the all channels interrupt)} 
        PBCM2711DMAHost(DMA).Channels[Count].Interrupt:=BCM2838_IRQ_DMA15;

        {Registers}
        PBCM2711DMAHost(DMA).Channels[Count].Registers:=PBCM2838DMARegisters(BCM2838_DMA15_REGS_BASE);
        
        {No Request IRQ}
       end;      
     end;
     
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Check the Channel}
     if (PBCM2711DMAHost(DMA).EnableRegister^ and (1 shl Count)) = 0 then
      begin
       {Enable the Channel}
       PBCM2711DMAHost(DMA).EnableRegister^:=PBCM2711DMAHost(DMA).EnableRegister^ or (1 shl Count);

       {Wait 1 millisecond}
       MicrosecondDelay(1000);
     
       {Reset the Channel}
       if PBCM2711DMAHost(DMA).Channels[Count].Registers <> nil then
        begin
         {Normal/Bulk/Lite}
         PBCM2711DMAHost(DMA).Channels[Count].Registers.CS:=BCM2838_DMA_CS_RESET;
        end
       else if PBCM2711DMAHost(DMA).Channels[Count].Registers40 <> nil then
        begin
         {40 bit}
         PBCM2711DMAHost(DMA).Channels[Count].Registers40.DEBUG:=BCM2838_DMA4_DEBUG_RESET;
        end;
      end; 
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
    end
   else
    begin
     {Interrupt No}
     PBCM2711DMAHost(DMA).Channels[Count].Interrupt:=LongWord(INVALID_HANDLE_VALUE);
     
     {Registers}
     PBCM2711DMAHost(DMA).Channels[Count].Registers:=nil;
     
     {Registers40}
     PBCM2711DMAHost(DMA).Channels[Count].Registers40:=nil;
    end;
  end;

 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2711DMAHostStop(DMA:PDMAHost):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Stop Channels}
 for Count:=0 to BCM2711_DMA_CHANNEL_COUNT - 1 do
  begin
   {Check Available}
   if (PBCM2711DMAHost(DMA).ChannelMask and (1 shl Count)) <> 0 then
    begin
     {Check Channel}
     case Count of
      {Channels 0 to 6}
      0..6:begin
        {Release IRQ}
        ReleaseIRQ(IRQ_ROUTING,PBCM2711DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2711DMAInterruptHandler),@PBCM2711DMAHost(DMA).Channels[Count]);
       end;
      {Channels 7 and 10 (Lite)}
      7..10:begin
        {Release IRQ}
        ReleaseIRQ(IRQ_ROUTING,PBCM2711DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2711DMASharedInterruptHandler),DMA);
       end;
      {Channels 11 to 14 (40 bit)}
      11..14:begin
        {Release IRQ}
        ReleaseIRQ(IRQ_ROUTING,PBCM2711DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2711DMA40InterruptHandler),@PBCM2711DMAHost(DMA).Channels[Count]);
       end;
      {Channel 15}
      15:begin
        {No Release IRQ}
       end;
     end;
    end;
  end; 

 {Destroy DMA Bulk Channel Semaphore}
 SemaphoreDestroy(PBCM2711DMAHost(DMA).ChannelBulk);
 PBCM2711DMAHost(DMA).ChannelBulk:=INVALID_HANDLE_VALUE;
  
 {Destroy DMA Lite Channel Semaphore}
 SemaphoreDestroy(PBCM2711DMAHost(DMA).ChannelLite);
 PBCM2711DMAHost(DMA).ChannelLite:=INVALID_HANDLE_VALUE;

 {Destroy DMA 40 Channel Semaphore}
 SemaphoreDestroy(PBCM2711DMAHost(DMA).Channel40);
 PBCM2711DMAHost(DMA).Channel40:=INVALID_HANDLE_VALUE;
  
 {Destroy Normal Channel Semaphore}
 SemaphoreDestroy(PBCM2711DMAHost(DMA).ChannelWait);
 PBCM2711DMAHost(DMA).ChannelWait:=INVALID_HANDLE_VALUE;
 
 {Destroy Channel Lock}
 MutexDestroy(PBCM2711DMAHost(DMA).ChannelLock);
 PBCM2711DMAHost(DMA).ChannelLock:=INVALID_HANDLE_VALUE;
 
 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2711DMAHostSubmit(DMA:PDMAHost;Request:PDMARequest):LongWord;

 function RequireDMA40(Data:PDMAData):Boolean;
 {Check if the source or destination addresses require DMA40}
 var
  Next:PDMAData;
 begin
  {}
  Result:=False;
  
  Next:=Data;
  while Next <> nil do
   begin
    {Check for 40 bit flag}
    if (Next.Flags and DMA_DATA_FLAG_40BIT) = 0 then
     begin
      {Check Source}
      if (Next.Flags and DMA_DATA_FLAG_SOURCE_DREQ) = 0 then
       begin
        {Check Address}
        if PtrUInt(Next.Source) >= BCM2711_DMA_REQUIRE_40_ADDRESS then Result:=True;
        
        {$IFDEF CPUARM}
        {Check Range}
        if Next.SourceRange > 0 then Result:=True;
        {$ENDIF CPUARM}
       end;

      {Check Destination}
      if (Next.Flags and DMA_DATA_FLAG_DEST_DREQ) = 0 then
       begin
        {Check Address}
        if PtrUInt(Next.Dest) >= BCM2711_DMA_REQUIRE_40_ADDRESS then Result:=True;
        
        {$IFDEF CPUARM}
        {Check Range}
        if Next.DestRange > 0 then Result:=True;
        {$ENDIF CPUARM}
       end;
       
      {Check Result} 
      if Result then Exit;
     end;
     
    Next:=Next.Next;
   end;
 end;
 
var
 Bulk:Boolean;
 Lite:Boolean;
 DMA40:Boolean;
 Flags:LongWord;
 Count:LongWord;
 Channel:LongWord;
 Maximum:LongWord;
 Data:PDMAData;
 BlockSize:LongWord;
 Block:PBCM2838DMAControlBlock;
 Block40:PBCM2838DMA40ControlBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.Host <> DMA then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Submitting request (Request=' + PtrToHex(Request) + ')');
 {$ENDIF}
 
 {Get Data Count}
 Count:=DMADataCount(Request.Data);
 if Count = 0 then Exit;

 {Get Data Flags}
 Flags:=DMADataFlags(Request.Data);
 
 {Get Data Maximum}
 Maximum:=DMADataMaximum(Request.Data);

 {Check Source and Dest Addresses} 
 if RequireDMA40(Request.Data) then
  begin
   {Force "40 bit" channel request}
   Flags:=Flags and not(DMA_DATA_FLAG_BULK or DMA_DATA_FLAG_LITE);
   Flags:=Flags or DMA_DATA_FLAG_40BIT;
   
   if DMA_LOG_ENABLED then DMALogWarn(DMA,'BCM2711: Forcing 40-bit DMA due to source or destination address (Request=' + PtrToHex(Request) + ')');
  end;
  
 Bulk:=False;
 Lite:=False;
 DMA40:=False;
 
 {Check for "Bulk" channel request}
 if (Flags and DMA_DATA_FLAG_BULK) <> 0 then
  begin
   Bulk:=True;

   {Check for Emulator (QEMU does not support Bulk channels)}
   if EMULATOR_MODE then Bulk:=False;
  end
 {Check for "Lite" channel request}
 else if (Flags and DMA_DATA_FLAG_LITE) <> 0 then
  begin 
   {Check for "Lite" suitable request (No Stride, No Ignore, Size less then 64K)}
   if (Flags and (DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_NOREAD or DMA_DATA_FLAG_NOWRITE) = 0) and (Maximum <= BCM2711_DMA_MAX_LITE_TRANSFER) then
    begin
     Lite:=True;

     {Check for Emulator (QEMU does not support Lite channels)}
     if EMULATOR_MODE then Lite:=False;
    end;
  end  
 {Check for "40 bit" channel request}
 else if (Flags and DMA_DATA_FLAG_40BIT) <> 0 then
  begin 
   DMA40:=True;
  end;
 
 {Get Maximum Size}
 Maximum:=BCM2711_DMA_MAX_NORMAL_TRANSFER;
 if Lite then Maximum:=BCM2711_DMA_MAX_LITE_TRANSFER;
 if DMA40 then Maximum:=BCM2711_DMA_MAX_40_TRANSFER;
 
 {Get Block Size}
 BlockSize:=SizeOf(TBCM2838DMAControlBlock);
 if DMA40 then BlockSize:=SizeOf(TBCM2838DMA40ControlBlock);
 
 Result:=ERROR_OPERATION_FAILED;
 
 {Create Control Blocks}
 if BCM2711DMA_SHARED_MEMORY then
  begin
   Request.ControlBlocks:=GetSharedAlignedMem(Count * BlockSize,BCM2711_DMA_CB_ALIGNMENT);
  end
 else if BCM2711DMA_NOCACHE_MEMORY then
  begin
   Request.ControlBlocks:=GetNoCacheAlignedMem(Count * BlockSize,BCM2711_DMA_CB_ALIGNMENT);
  end
 else 
  begin
   Request.ControlBlocks:=GetAlignedMem(Count * BlockSize,BCM2711_DMA_CB_ALIGNMENT);
  end;  
 if Request.ControlBlocks = nil then Exit;
 try
  {Update Control Blocks}
  Data:=Request.Data;
  if not DMA40 then
   begin
    {Bulk, Lite or Normal transfer}
    Block:=PBCM2838DMAControlBlock(Request.ControlBlocks);
    while Data <> nil do
     begin
      {Check Size}
      if Data.Size = 0 then Exit;
      if Data.Size > Maximum then Exit;
      if ((Data.Flags and DMA_DATA_FLAG_STRIDE) <> 0) and (Data.StrideLength = 0) then Exit;
      
      {Setup Control Block}
      BCM2711DMADataToControlBlock(Request,Data,Block,Bulk,Lite);
      
      {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
      if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Data block (Source=' + PtrToHex(Data.Source) + ' Dest=' + PtrToHex(Data.Dest) + ' Size=' + IntToStr(Data.Size) + ')');
      if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Control block (SourceAddress=' + IntToHex(Block.SourceAddress,8) + ' DestinationAddress=' + IntToHex(Block.DestinationAddress,8) + ' TransferLength=' + IntToHex(Block.TransferLength,8) + ')');
      {$ENDIF}
      
      {Get Next}
      Data:=Data.Next;
      if Data <> nil then
       begin
        {Get Next Block}
        Block:=PBCM2838DMAControlBlock(PtrUInt(Block) + BlockSize);
       end;
     end; 
   end
  else
   begin  
    {40 bit transfer}
    Block40:=PBCM2838DMA40ControlBlock(Request.ControlBlocks);
    while Data <> nil do
     begin
      {Check Size}
      if Data.Size = 0 then Exit;
      if Data.Size > Maximum then Exit;
      if ((Data.Flags and DMA_DATA_FLAG_STRIDE) <> 0) and (Data.StrideLength = 0) then Exit;
      
      {Setup Control Block}
      BCM2711DMA40DataToControlBlock(Request,Data,Block40);
      
      {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
      if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Data block (Source='{$IFDEF CPUARM} + IntToHex(Data.SourceRange,8) + ':'{$ENDIF CPUARM} + PtrToHex(Data.Source) + ' Dest='{$IFDEF CPUARM} + IntToHex(Data.DestRange,8) + ':'{$ENDIF CPUARM} + PtrToHex(Data.Dest) + ' Size=' + IntToStr(Data.Size) + ')');
      if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Control block 40 (SourceAddress=' + IntToHex(Block40.SourceAddress,8) + ' DestinationAddress=' + IntToHex(Block40.DestinationAddress,8) + ' TransferLength=' + IntToHex(Block40.TransferLength,8) + ')');
      if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711:                  (SourceInformation=' + IntToHex(Block40.SourceInformation,8) + ' DestinationInformation=' + IntToHex(Block40.DestinationInformation,8) + ' TransferInformation=' + IntToHex(Block40.TransferInformation,8) + ')');
      {$ENDIF}
      
      {Get Next}
      Data:=Data.Next;
      if Data <> nil then
       begin
        {Get Next Block}
        Block40:=PBCM2838DMA40ControlBlock(PtrUInt(Block40) + BlockSize);
       end;
     end; 
   end;
   
  {Flush Control Blocks}
  if not(BCM2711DMA_CACHE_COHERENT) then
   begin
    CleanDataCacheRange(PtrUInt(Request.ControlBlocks),Count * BlockSize);
   end;
  
  {Wait for Channel}
  if Bulk then
   begin
    if SemaphoreWait(PBCM2711DMAHost(DMA).ChannelBulk) <> ERROR_SUCCESS then Exit;
   end
  else if Lite then
   begin
    if SemaphoreWait(PBCM2711DMAHost(DMA).ChannelLite) <> ERROR_SUCCESS then Exit;
   end
  else if DMA40 then
   begin
    if SemaphoreWait(PBCM2711DMAHost(DMA).Channel40) <> ERROR_SUCCESS then Exit;
   end
  else
   begin  
    if SemaphoreWait(PBCM2711DMAHost(DMA).ChannelWait) <> ERROR_SUCCESS then Exit;
   end; 
  
  {Acquire the Lock}
  if MutexLock(PBCM2711DMAHost(DMA).ChannelLock) = ERROR_SUCCESS then
   begin
    try
     {Get Free Channel}
     if Bulk then
      begin
       Channel:=FirstBitSet(PBCM2711DMAHost(DMA).ChannelFree and BCM2711_DMA_BULK_CHANNELS);
      end
     else if Lite then
      begin
       Channel:=FirstBitSet(PBCM2711DMAHost(DMA).ChannelFree and BCM2711_DMA_LITE_CHANNELS);
      end
     else if DMA40 then
      begin
       Channel:=FirstBitSet(PBCM2711DMAHost(DMA).ChannelFree and BCM2711_DMA_40_CHANNELS);
      end
     else
      begin
       Channel:=FirstBitSet(PBCM2711DMAHost(DMA).ChannelFree and BCM2711_DMA_NORMAL_CHANNELS);
      end;
      
     {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
     if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Allocated channel (Channel=' + IntToStr(Channel) + ')');
     {$ENDIF}
      
     {Check Free Channel} 
     if Channel <> LongWord(INVALID_HANDLE_VALUE) then 
      begin
       {Update Channel Free}
       PBCM2711DMAHost(DMA).ChannelFree:=PBCM2711DMAHost(DMA).ChannelFree xor (1 shl Channel);
      
       {Update Channel}
       PBCM2711DMAHost(DMA).Channels[Channel].Request:=Request;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Check Type}
       if PBCM2711DMAHost(DMA).Channels[Channel].Registers <> nil then
        begin
         {Normal/Bulk/Lite}
         {Set Control Block}
         if BCM2711DMA_BUS_ADDRESSES then
          begin
           PBCM2711DMAHost(DMA).Channels[Channel].Registers.CONBLK_AD:=PhysicalToBusAddress(Request.ControlBlocks);
          end
         else
          begin
           PBCM2711DMAHost(DMA).Channels[Channel].Registers.CONBLK_AD:=PtrUInt(Request.ControlBlocks);
          end;

         {Note: Broadcom documentation states that BCM2838_DMA_CS_ERROR bit should be cleared by writing
                to the error bits in the debug register, this doesn't seem to be neccessary in practice}
                
         {Enable Channel}
         PBCM2711DMAHost(DMA).Channels[Channel].Registers.CS:=BCM2838_DMA_CS_ACTIVE;
         
         {Note: Broadcom documentation states that the BCM2838_DMA_CS_END bit will be set when a transfer
                is completed and should be cleared by writing 1 to it, this doesn't seem to be the case}
        end  
       else if PBCM2711DMAHost(DMA).Channels[Channel].Registers40 <> nil then
        begin
         {40 bit}
         {Set Control Block}
         PBCM2711DMAHost(DMA).Channels[Channel].Registers40.CB:=(PtrUInt(Request.ControlBlocks) and BCM2838_DMA4_CB_ADDR_MASK) shr BCM2838_DMA4_CB_ADDR_SHIFT;
         
         {Enable Channel}
         PBCM2711DMAHost(DMA).Channels[Channel].Registers40.CS:=BCM2838_DMA4_CS_QOS_DEFAULT or BCM2838_DMA4_CS_PANIC_QOS_DEFAULT or BCM2838_DMA4_CS_WAIT_FOR_OUTSTANDING_WRITES or BCM2838_DMA4_CS_ACTIVE;
         
         {Note: Broadcom documentation states that the BCM2838_DMA4_CS_END bit will be set when a transfer
                is completed and should be cleared by writing 1 to it, this doesn't seem to be the case}
        end;
                            
       {Update Status}
       Request.Status:=ERROR_NOT_COMPLETED;
      
       {Return Result}
       Result:=ERROR_SUCCESS;
      end
     else
      begin
       {Signal Semaphore}
       if Bulk then
        begin
         SemaphoreSignal(PBCM2711DMAHost(DMA).ChannelBulk);
        end
       else if Lite then
        begin
         SemaphoreSignal(PBCM2711DMAHost(DMA).ChannelLite);
        end
       else if DMA40 then
        begin
         SemaphoreSignal(PBCM2711DMAHost(DMA).Channel40);
        end
       else
        begin
         SemaphoreSignal(PBCM2711DMAHost(DMA).ChannelWait);
        end;
      end;     
    finally
     {Release the Lock}
     MutexUnlock(PBCM2711DMAHost(DMA).ChannelLock);
    end;   
   end;
 finally
  if Result <> ERROR_SUCCESS then
   begin
    FreeMem(Request.ControlBlocks);
   end;
 end; 
end; 

{==============================================================================}

function BCM2711DMAHostCancel(DMA:PDMAHost;Request:PDMARequest):LongWord;
var
 CS:LongWord;
 Count:LongWord;
 Channel:LongWord;
 Timeout:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.Host <> DMA then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Cancelling request (Request=' + PtrToHex(Request) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(PBCM2711DMAHost(DMA).ChannelLock) = ERROR_SUCCESS then
  begin
   try
    {Check Request}
    if Request.Status = ERROR_NOT_PROCESSED then
     begin
      {Update Request}
      Request.Status:=ERROR_CANCELLED;
     
      {Return Result}
      Result:=ERROR_SUCCESS;
     end
    else if Request.Status = ERROR_NOT_COMPLETED then
     begin
      {Update Request}
      Request.Status:=ERROR_CANCELLED;
     
      {Find Channel}
      Channel:=LongWord(INVALID_HANDLE_VALUE);
      for Count:=0 to BCM2711_DMA_CHANNEL_COUNT - 1 do
       begin
        if PBCM2711DMAHost(DMA).Channels[Channel].Request = Request then
         begin
          Channel:=Count;
          Break;
         end;
       end;
       
      {Check Channel}
      if Channel <> LongWord(INVALID_HANDLE_VALUE) then
       begin
        {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
        if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: Located channel (Channel=' + IntToStr(Channel) + ')');
        {$ENDIF}
      
        {Memory Barrier}
        DataMemoryBarrier; {Before the First Write}
        
        {Check Type}
        if PBCM2711DMAHost(DMA).Channels[Channel].Registers <> nil then
         begin
          {Normal/Bulk/Lite}
          {Get Status}
          CS:=PBCM2711DMAHost(DMA).Channels[Channel].Registers.CS;
        
          {Check Active}
          if (CS and BCM2838_DMA_CS_ACTIVE) <> 0 then
           begin
            {Pause the Channel}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers.CS:=CS and not(BCM2838_DMA_CS_ACTIVE);
            
            {Wait for Paused}
            Timeout:=10000;
            while ((CS and BCM2838_DMA_CS_PAUSED) = 0) and (Timeout > 0) do
             begin
              CS:=PBCM2711DMAHost(DMA).Channels[Channel].Registers.CS;
              
              Dec(Timeout);
             end;
            
            {Check Paused}
            if (CS and BCM2838_DMA_CS_PAUSED) = 0 then
             begin
              Result:=ERROR_TIMEOUT;
              Exit;
             end;
             
            {Clear the Next Control Block}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers.NEXTCONBK:=0;
            
            {Set the Interrupt Enable}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers.TI:=PBCM2711DMAHost(DMA).Channels[Channel].Registers.TI or BCM2838_DMA_TI_INTEN;
            
            {Enable and Abort the Channel}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers.CS:=PBCM2711DMAHost(DMA).Channels[Channel].Registers.CS or BCM2838_DMA_CS_ACTIVE or BCM2838_DMA_CS_ABORT;
           end;
         end
        else if PBCM2711DMAHost(DMA).Channels[Channel].Registers40 <> nil then
         begin
          {40 bit}
          {Get Status}
          CS:=PBCM2711DMAHost(DMA).Channels[Channel].Registers40.CS;
        
          {Check Active}
          if (CS and BCM2838_DMA4_CS_ACTIVE) <> 0 then
           begin
            {Pause the Channel}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers40.CS:=CS and not(BCM2838_DMA4_CS_ACTIVE);
            
            {Wait for Paused}
            Timeout:=10000;
            while ((CS and BCM2838_DMA4_CS_RD_PAUSED) = 0) and ((CS and BCM2838_DMA4_CS_WR_PAUSED) = 0) and (Timeout > 0) do
             begin
              CS:=PBCM2711DMAHost(DMA).Channels[Channel].Registers40.CS;
              
              Dec(Timeout);
             end;
            
            {Check Paused}
            if ((CS and BCM2838_DMA4_CS_RD_PAUSED) = 0) and ((CS and BCM2838_DMA4_CS_WR_PAUSED) = 0) then
             begin
              Result:=ERROR_TIMEOUT;
              Exit;
             end;
             
            {Clear the Next Control Block}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers40.NEXT_CB:=0;
            
            {Set the Interrupt Enable}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers40.TI:=PBCM2711DMAHost(DMA).Channels[Channel].Registers40.TI or BCM2838_DMA4_TI_INTEN;
            
            {Enable and Abort the Channel}
            PBCM2711DMAHost(DMA).Channels[Channel].Registers40.CS:=PBCM2711DMAHost(DMA).Channels[Channel].Registers40.CS or BCM2838_DMA4_CS_ACTIVE or BCM2838_DMA4_CS_ABORT;
           end;
         end;
         
        {Memory Barrier}
        DataMemoryBarrier; {After the Last Read} 
           
        {Interrupt handler will complete cancel}
       end
      else
       begin
        {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
        if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2711: No channel');
        {$ENDIF}
       
        {Interrupt handler will complete cancel}
       end;
              
      {Return Result}
      Result:=ERROR_SUCCESS;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_OPERATION_FAILED;
     end;     
   finally
    {Release the Lock}
    MutexUnlock(PBCM2711DMAHost(DMA).ChannelLock);
   end;   
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end; 

{==============================================================================}

procedure BCM2711DMAInterruptHandler(Channel:PBCM2711DMAChannel);
{DMA Channels 0 to 6 each have a dedicated interrupt, this handler simply
 clears the interrupt and sends a completion on the associated channel}
begin
 {}
 {Check Channel}
 if Channel = nil then Exit;
 if Channel.Registers = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Acknowledge Interrupt}
 Channel.Registers.CS:=BCM2838_DMA_CS_INT;
 
 {Send Completion}
 WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(BCM2711DMARequestComplete),Channel,nil);
end; 

{==============================================================================}

procedure BCM2711DMA40InterruptHandler(Channel:PBCM2711DMAChannel);
{DMA Channels 11 to 14 each have a dedicated interrupt, this handler simply
 clears the interrupt and sends a completion on the associated channel}
begin
 {}
 {Check Channel}
 if Channel = nil then Exit;
 if Channel.Registers40 = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Acknowledge Interrupt}
 Channel.Registers40.CS:=BCM2838_DMA4_CS_INT;
 
 {Send Completion}
 WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(BCM2711DMARequestComplete),Channel,nil);
end; 

{==============================================================================}

procedure BCM2711DMASharedInterruptHandler(DMA:PBCM2711DMAHost);
{DMA Channels 7 to 10 share a common interrupt, this alternate handler determines
 which one triggered the current interrupt and sends a completion on that channel}
var
 Channel:LongWord;
 Interrupts:LongWord; 
begin
 {}
 {Check DMA}
 if DMA = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Interrupts:=(DMA.InterruptRegister^ and BCM2711_DMA_SHARED_CHANNELS);
 while Interrupts <> 0 do
  begin
   {Get Channel}
   Channel:=FirstBitSet(Interrupts);
   
   {Check Channel}
   if DMA.Channels[Channel].Registers <> nil then
    begin
     {Acknowledge Interrupt}
     DMA.Channels[Channel].Registers.CS:=BCM2838_DMA_CS_INT;
     
     {Send Completion}
     WorkerScheduleIRQEx(CPU_AFFINITY_NONE,WORKER_FLAG_PRIORITY,TWorkerTask(BCM2711DMARequestComplete),@DMA.Channels[Channel],nil);
    end;
   
   {Clear the Interrupt}
   Interrupts:=Interrupts xor (1 shl Channel);
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2711DMARequestComplete(Channel:PBCM2711DMAChannel);
var
 CS:LongWord;
 Data:PDMAData;
 Error:Boolean;
 Offset:LongInt; {Allow for negative stride}
 DMA:PBCM2711DMAHost;
 Request:PDMARequest;
begin
 {}
 {Check Channel}
 if Channel = nil then Exit;
 if (Channel.Registers = nil) and (Channel.Registers40 = nil) then Exit;
 
 {Get Host}
 DMA:=Channel.Host;
 if DMA = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711: Request completed (Request=' + PtrToHex(Channel.Request) + ')');
 {$ENDIF}

 {Check Type}
 if Channel.Registers <> nil then
  begin
   {Normal/Bulk/Lite}
   {Get Status}
   CS:=Channel.Registers.CS;
   
   {Check Error}
   Error:=(CS and BCM2838_DMA_CS_ERROR) <> 0;
   
   {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.CS=' + IntToHex(Channel.Registers.CS,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.CONBLK_AD=' + IntToHex(Channel.Registers.CONBLK_AD,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.TI=' + IntToHex(Channel.Registers.TI,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.SOURCE_AD=' + IntToHex(Channel.Registers.SOURCE_AD,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.DEST_AD=' + IntToHex(Channel.Registers.DEST_AD,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.TXFR_LEN=' + IntToHex(Channel.Registers.TXFR_LEN,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.STRIDE=' + IntToHex(Channel.Registers.STRIDE,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.NEXTCONBK=' + IntToHex(Channel.Registers.NEXTCONBK,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers.DEBUG=' + IntToHex(Channel.Registers.DEBUG,8) + ')');
   {$ENDIF}
  end
 else if Channel.Registers40 <> nil then  
  begin
   {Get Status}
   CS:=Channel.Registers40.CS;

   {Check Error}
   Error:=(CS and BCM2838_DMA4_CS_ERROR) <> 0;

   {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.CS=' + IntToHex(Channel.Registers40.CS,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.CB=' + IntToHex(Channel.Registers40.CB,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.DEBUG=' + IntToHex(Channel.Registers40.DEBUG,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.TI=' + IntToHex(Channel.Registers40.TI,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.SRC=' + IntToHex(Channel.Registers40.SRC,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.SRCI=' + IntToHex(Channel.Registers40.SRCI,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.DEST=' + IntToHex(Channel.Registers40.DEST,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.DESTI=' + IntToHex(Channel.Registers40.DESTI,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.LEN=' + IntToHex(Channel.Registers40.LEN,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.NEXT_CB=' + IntToHex(Channel.Registers40.NEXT_CB,8) + ')');
   if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711:  (Registers40.DEBUG2=' + IntToHex(Channel.Registers40.DEBUG2,8) + ')');
   {$ENDIF}
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Get Request}
 Request:=Channel.Request;
 
 {Acquire the Lock}
 if MutexLock(DMA.ChannelLock) = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(DMA.InterruptCount);
    
    {Check Channel}
    if Channel.Number < BCM2711_DMA_CHANNEL_COUNT then
     begin
      {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DMA_DEBUG)}
      if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2711: Released channel (Channel=' + IntToStr(Channel.Number) + ')');
      {$ENDIF}
      
      {Update Channel}
      DMA.Channels[Channel.Number].Request:=nil;
      
      {Update Channel Free}
      DMA.ChannelFree:=DMA.ChannelFree or (1 shl Channel.Number);
      
      {Check Bulk}
      if ((1 shl Channel.Number) and BCM2711_DMA_BULK_CHANNELS) <> 0 then
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelBulk);
       end
      {Check Lite}
      else if ((1 shl Channel.Number) and BCM2711_DMA_LITE_CHANNELS) <> 0 then
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelLite);
       end
      {Check DMA40}
      else if ((1 shl Channel.Number) and BCM2711_DMA_40_CHANNELS) <> 0 then
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.Channel40);
       end
      else
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelWait);
       end;
     end; 
    
   finally
    {Release the Lock}
    MutexUnlock(DMA.ChannelLock);
   end;   
  end;
  
 {Check Request}
 if Request <> nil then
  begin
   {Check Status}
   if Error then
    begin
     Request.Status:=ERROR_OPERATION_FAILED;
    end
   else
    begin
     Request.Status:=ERROR_SUCCESS;
    end;       
   
   {Release Control Blocks}
   if Request.ControlBlocks <> nil then
    begin
     FreeMem(Request.ControlBlocks);
     Request.ControlBlocks:=nil;
    end; 
   
   {Flush Dest} 
   case Request.Direction of
    DMA_DIR_MEM_TO_MEM,DMA_DIR_DEV_TO_MEM:begin
      if not(BCM2711DMA_CACHE_COHERENT) or ((Request.Flags and DMA_REQUEST_FLAG_COMPATIBLE) = 0) then
       begin
        Data:=Request.Data;
        while Data <> nil do
         begin
          if (Data.Flags and DMA_DATA_FLAG_NOINVALIDATE) = 0 then
           begin
            if ((Data.Flags and DMA_DATA_FLAG_STRIDE) = 0) or (Data.DestStride = 0) then
             begin
              InvalidateDataCacheRange(PtrUInt(Data.Dest),Data.Size);
             end
            else
             begin
              Offset:=0;
              while Offset < Data.Size do
               begin
                InvalidateDataCacheRange(PtrUInt(Data.Dest + Offset),Data.StrideLength);
                
                Inc(Offset,Data.DestStride);
               end;
             end;
           end; 
          
          Data:=Data.Next;
         end; 
       end;
     end;
   end;
           
   {Complete the request}
   DMARequestComplete(Request);
  end;
end; 

{==============================================================================}

function BCM2711DMAPeripheralToDREQ(Peripheral:LongWord):LongWord;
begin
 {}
 Result:=BCM2838_DMA_DREQ_NONE;
 
 case Peripheral of
  DMA_DREQ_ID_UART0_TX:Result:=BCM2838_DMA_DREQ_UART0TX;
  DMA_DREQ_ID_UART0_RX:Result:=BCM2838_DMA_DREQ_UART0RX;
  DMA_DREQ_ID_UART2_TX:Result:=BCM2838_DMA_DREQ_UART2TX;
  DMA_DREQ_ID_UART2_RX:Result:=BCM2838_DMA_DREQ_UART2RX;
  DMA_DREQ_ID_UART3_TX:Result:=BCM2838_DMA_DREQ_UART3TX;
  DMA_DREQ_ID_UART3_RX:Result:=BCM2838_DMA_DREQ_UART3RX;
  DMA_DREQ_ID_UART4_TX:Result:=BCM2838_DMA_DREQ_UART4TX; 
  DMA_DREQ_ID_UART4_RX:Result:=BCM2838_DMA_DREQ_UART4RX;
  DMA_DREQ_ID_UART5_TX:Result:=BCM2838_DMA_DREQ_UART5TX;
  DMA_DREQ_ID_UART5_RX:Result:=BCM2838_DMA_DREQ_UART5RX;
  DMA_DREQ_ID_SPI0_TX:Result:=BCM2838_DMA_DREQ_SPI0TX;
  DMA_DREQ_ID_SPI0_RX:Result:=BCM2838_DMA_DREQ_SPI0RX;
  DMA_DREQ_ID_SPI1_TX:Result:=BCM2838_DMA_DREQ_SPI1TX;
  DMA_DREQ_ID_SPI1_RX:Result:=BCM2838_DMA_DREQ_SPI1RX;
  DMA_DREQ_ID_SPI4_TX:Result:=BCM2838_DMA_DREQ_SPI4TX;
  DMA_DREQ_ID_SPI4_RX:Result:=BCM2838_DMA_DREQ_SPI4RX;
  DMA_DREQ_ID_SPI5_TX:Result:=BCM2838_DMA_DREQ_SPI5TX;
  DMA_DREQ_ID_SPI5_RX:Result:=BCM2838_DMA_DREQ_SPI5RX;
  DMA_DREQ_ID_SPI6_TX:Result:=BCM2838_DMA_DREQ_SPI6TX;
  DMA_DREQ_ID_SPI6_RX:Result:=BCM2838_DMA_DREQ_SPI6RX;
  DMA_DREQ_ID_SPI_SLAVE0_TX:Result:=BCM2838_DMA_DREQ_BSCSPITX;
  DMA_DREQ_ID_SPI_SLAVE0_RX:Result:=BCM2838_DMA_DREQ_BSCSPIRX;
  DMA_DREQ_ID_PCM0_TX:Result:=BCM2838_DMA_DREQ_PCMTX;
  DMA_DREQ_ID_PCM0_RX:Result:=BCM2838_DMA_DREQ_PCMRX;
  DMA_DREQ_ID_PWM0:Result:=BCM2838_DMA_DREQ_PWM0;
  DMA_DREQ_ID_PWM1:Result:=BCM2838_DMA_DREQ_PWM1;
  DMA_DREQ_ID_EMMC0:Result:=BCM2838_DMA_DREQ_EMMC0;
  DMA_DREQ_ID_EMMC1:Result:=BCM2838_DMA_DREQ_EMMC1;
 end;
end;

{==============================================================================}

procedure BCM2711DMADataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2838DMAControlBlock;Bulk,Lite:Boolean);
var
 Count:LongWord;
 Offset:LongInt; {Allow for negative stride}
begin
 {}
 if Request = nil then Exit;
 if Data = nil then Exit;
 if Block = nil then Exit;
 
 {Clear Transfer Information}
 Block.TransferInformation:=0;
 
 {Setup Source and Destination}
 if BCM2711DMA_BUS_ADDRESSES then
  begin
   case Request.Direction of
    DMA_DIR_NONE:begin
      Block.SourceAddress:=PtrUInt(Data.Source);
      Block.DestinationAddress:=PtrUInt(Data.Dest);
     end;
    DMA_DIR_MEM_TO_MEM:begin
      Block.SourceAddress:=PhysicalToBusAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToBusAddress(Data.Dest);
     end;
    DMA_DIR_MEM_TO_DEV:begin
      Block.SourceAddress:=PhysicalToBusAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToIOAddress(Data.Dest);
     end;
    DMA_DIR_DEV_TO_MEM:begin
      Block.SourceAddress:=PhysicalToIOAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToBusAddress(Data.Dest);
     end;
    DMA_DIR_DEV_TO_DEV:begin
      Block.SourceAddress:=PhysicalToIOAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToIOAddress(Data.Dest);
     end;     
   end;
  end
 else
  begin
   Block.SourceAddress:=PtrUInt(Data.Source);
   Block.DestinationAddress:=PtrUInt(Data.Dest);
  end;  
   
 {Setup Transfer Length and Stride}
 if (Data.Flags and DMA_DATA_FLAG_STRIDE) = 0 then
  begin
   {Linear Mode}
   Block.TransferLength:=Data.Size;
   Block.ModeStride:=0;
  end
 else
  begin
   {Stride Mode}
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_2DMODE;
   
   {Get Count (minus 1)}
   Count:=(Data.Size div (Data.StrideLength and BCM2711_DMA_MAX_X_LENGTH)) - 1;
   
   {Set Length and Count}
   Block.TransferLength:=((Count and BCM2711_DMA_MAX_Y_COUNT) shl 16) or (Data.StrideLength and BCM2711_DMA_MAX_X_LENGTH);
   
   {Set Source and Dest Stride}
   Block.ModeStride:=((Data.DestStride and BCM2711_DMA_MAX_STRIDE) shl 16) or (Data.SourceStride and BCM2711_DMA_MAX_STRIDE);
  end;  
 
 {Setup Transfer Information}
 {Source Data Request}
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_WAIT_RESP or BCM2838_DMA_TI_SRC_DREQ;
  end;
 {Dest Data Request} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_WAIT_RESP or BCM2838_DMA_TI_DEST_DREQ;
  end;
 {Source Increment} 
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_NOINCREMENT) = 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_SRC_INC;
  end;
 {Dest Increment} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_NOINCREMENT) = 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_DEST_INC;
  end;
 {Source Width}
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_WIDE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_SRC_WIDTH;
  end;
 {Dest Width}
 if (Data.Flags and DMA_DATA_FLAG_DEST_WIDE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_DEST_WIDTH;
  end;
 {Source Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOREAD) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_SRC_IGNORE;
  end;
 {Dest Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOWRITE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_DEST_IGNORE;
  end;
 {Peripheral Map}
 if Request.Peripheral <> DMA_DREQ_ID_NONE then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2711DMAPeripheralToDREQ(Request.Peripheral) shl BCM2838_DMA_TI_PERMAP_SHIFT);
  end; 
 {Burst Length}
 if Bulk then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2711_DMA_BULK_BURST_LENGTH shl BCM2838_DMA_TI_BURST_LENGTH_SHIFT);
  end
 else if Lite then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2711_DMA_LITE_BURST_LENGTH shl BCM2838_DMA_TI_BURST_LENGTH_SHIFT);
  end
 else
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2711_DMA_NORMAL_BURST_LENGTH shl BCM2838_DMA_TI_BURST_LENGTH_SHIFT);
  end;  
 {Interrupt Enable}
 if Data.Next = nil then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA_TI_INTEN;
  end;
 
 {Setup Next Control Block}
 if Data.Next <> nil then
  begin
   {Set Next Block}
   if BCM2711DMA_BUS_ADDRESSES then
    begin
     Block.NextControlBlockAddress:=PhysicalToBusAddress(Pointer(PtrUInt(Block) + SizeOf(TBCM2838DMAControlBlock)));
    end
   else
    begin
     Block.NextControlBlockAddress:=PtrUInt(Block) + SizeOf(TBCM2838DMAControlBlock);
    end;
  end
 else
  begin
   Block.NextControlBlockAddress:=0;
  end;  
  
 {Setup Reserved} 
 Block.Reserved1:=0;
 Block.Reserved2:=0;
 
 {Flush Source} 
 case Request.Direction of
  DMA_DIR_MEM_TO_MEM,DMA_DIR_MEM_TO_DEV:begin
    if not(BCM2711DMA_CACHE_COHERENT) or ((Request.Flags and DMA_REQUEST_FLAG_COMPATIBLE) = 0) then
     begin
      if (Data.Flags and DMA_DATA_FLAG_NOCLEAN) = 0 then
       begin
        if ((Data.Flags and DMA_DATA_FLAG_STRIDE) = 0) or (Data.SourceStride = 0) then
         begin
          CleanDataCacheRange(PtrUInt(Data.Source),Data.Size);
         end
        else
         begin
          Offset:=0;
          while Offset < Data.Size do
           begin
            CleanDataCacheRange(PtrUInt(Data.Source + Offset),Data.StrideLength);
            
            Inc(Offset,Data.SourceStride);
           end; 
         end;
       end;
     end;
   end;
 end;  
end;

{==============================================================================}

procedure BCM2711DMA40DataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2838DMA40ControlBlock);

 function PeripheralToIOAddress(Address:Pointer):PtrUInt; inline;
 {Convert a DMA40 peripheral address to an IO address}
 begin
  {}
  Result:=(PtrUInt(Address) and BCM2711_DMA_40_PERIPHERAL_IO_MASK) + BCM2711_DMA_40_PERIPHERAL_IO_ALIAS
 end;
 
var
 Count:LongWord;
 Offset:LongInt; {Allow for negative stride}
 Peripheral:Boolean;
begin
 {}
 if Request = nil then Exit;
 if Data = nil then Exit;
 if Block = nil then Exit;
 
 {Clear Transfer/Source/Destination Information}
 Block.TransferInformation:=0;
 Block.SourceInformation:=0;
 Block.DestinationInformation:=0;
 
 {Setup Source and Destination}
 case Request.Direction of
  DMA_DIR_MEM_TO_DEV:begin
    {Memory to Device}
    Peripheral:=True;
    
    Block.SourceAddress:=PtrLow(Data.Source);
    Block.DestinationAddress:=PeripheralToIOAddress(Data.Dest);
    {$IFDEF CPUARM}
    Block.SourceInformation:=(Data.SourceRange and BCM2838_DMA4_SRCI_ADDR_MASK);
    {$ENDIF CPUARM}
    {$IFDEF CPUAARCH64}
    Block.SourceInformation:=(PtrHigh(Data.Source) and BCM2838_DMA4_SRCI_ADDR_MASK);
    {$ENDIF CPUAARCH64}
    Block.DestinationInformation:=BCM2711_DMA_40_PERIPHERAL_OFFSET;
   end;
  DMA_DIR_DEV_TO_MEM:begin
    {Device to Memory}
    Peripheral:=True;
    
    Block.SourceAddress:=PeripheralToIOAddress(Data.Source);
    Block.DestinationAddress:=PtrLow(Data.Dest);
    Block.SourceInformation:=BCM2711_DMA_40_PERIPHERAL_OFFSET;
    {$IFDEF CPUARM}
    Block.DestinationInformation:=(Data.DestRange and BCM2838_DMA4_DESTI_ADDR_MASK);
    {$ENDIF CPUARM}
    {$IFDEF CPUAARCH64}
    Block.DestinationInformation:=(PtrHigh(Data.Dest) and BCM2838_DMA4_DESTI_ADDR_MASK);
    {$ENDIF CPUAARCH64}
   end;
  DMA_DIR_DEV_TO_DEV:begin
    {Device to Device}
    Peripheral:=True;
    
    Block.SourceAddress:=PeripheralToIOAddress(Data.Source);
    Block.DestinationAddress:=PeripheralToIOAddress(Data.Dest);
    Block.SourceInformation:=BCM2711_DMA_40_PERIPHERAL_OFFSET;
    Block.DestinationInformation:=BCM2711_DMA_40_PERIPHERAL_OFFSET;
   end;     
  else
   begin
    {All others (Memory to Memory etc)}
    Peripheral:=False;
    
    Block.SourceAddress:=PtrLow(Data.Source);
    Block.DestinationAddress:=PtrLow(Data.Dest);
    {$IFDEF CPUARM}
    Block.SourceInformation:=(Data.SourceRange and BCM2838_DMA4_SRCI_ADDR_MASK);
    Block.DestinationInformation:=(Data.DestRange and BCM2838_DMA4_DESTI_ADDR_MASK);
    {$ENDIF CPUARM}
    {$IFDEF CPUAARCH64}
    Block.SourceInformation:=(PtrHigh(Data.Source) and BCM2838_DMA4_SRCI_ADDR_MASK);
    Block.DestinationInformation:=(PtrHigh(Data.Dest) and BCM2838_DMA4_DESTI_ADDR_MASK);
    {$ENDIF CPUAARCH64}
   end;
 end;

 {Setup Transfer Length and Stride}
 if (Data.Flags and DMA_DATA_FLAG_STRIDE) = 0 then
  begin
   {Linear Mode}
   Block.TransferLength:=Data.Size;
  end
 else
  begin
   {Stride Mode}
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA4_TI_TDMODE;

   {Get Count (minus 1)}
   Count:=(Data.Size div (Data.StrideLength and BCM2711_DMA_MAX_X_LENGTH)) - 1;
   
   {Set Length and Count}
   Block.TransferLength:=((Count and BCM2711_DMA_MAX_Y_COUNT) shl 16) or (Data.StrideLength and BCM2711_DMA_MAX_X_LENGTH);
   
   {Set Source Stride}
   Block.SourceInformation:=((Data.SourceStride and BCM2711_DMA_MAX_STRIDE) shl 16);
   
   {Set Dest Stride}
   Block.DestinationInformation:=((Data.DestStride and BCM2711_DMA_MAX_STRIDE) shl 16);
  end;  

 {Setup Transfer/Source/Destination Information}
 {Source Data Request}
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA4_TI_WAIT_RD_RESP or BCM2838_DMA4_TI_S_DREQ;
  end;
 {Dest Data Request} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA4_TI_WAIT_RESP or BCM2838_DMA4_TI_D_DREQ;
  end;
 {Source Increment} 
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_NOINCREMENT) = 0 then
  begin
   Block.SourceInformation:=Block.SourceInformation or BCM2838_DMA4_SRCI_INC;
  end;
 {Dest Increment} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_NOINCREMENT) = 0 then
  begin
   Block.DestinationInformation:=Block.DestinationInformation or BCM2838_DMA4_DESTI_INC;
  end;
 {Source Width}
 if ((Data.Flags and DMA_DATA_FLAG_SOURCE_WIDE) <> 0) and not(Peripheral) then
  begin
   Block.SourceInformation:=Block.SourceInformation or BCM2838_DMA4_SRCI_SIZE_128;
  end;
 {Dest Width}
 if ((Data.Flags and DMA_DATA_FLAG_DEST_WIDE) <> 0) and not(Peripheral) then
  begin
   Block.DestinationInformation:=Block.DestinationInformation or BCM2838_DMA4_DESTI_SIZE_128;
  end;
 {Source Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOREAD) <> 0 then
  begin
   Block.SourceInformation:=Block.SourceInformation or BCM2838_DMA4_SRCI_IGNORE;
  end;
 {Dest Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOWRITE) <> 0 then
  begin
   Block.DestinationInformation:=Block.DestinationInformation or BCM2838_DMA4_DESTI_IGNORE;
  end;
 {Peripheral Map}
 if Request.Peripheral <> DMA_DREQ_ID_NONE then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2711DMAPeripheralToDREQ(Request.Peripheral) shl BCM2838_DMA4_TI_PERMAP_SHIFT);
  end; 
 {Burst Length}
 if not(Peripheral) then
  begin
   Block.SourceInformation:=Block.SourceInformation or (BCM2711_DMA_40_BURST_LENGTH shl BCM2838_DMA4_SRCI_BURST_LENGTH_SHIFT);
   Block.DestinationInformation:=Block.DestinationInformation or ( BCM2711_DMA_40_BURST_LENGTH shl BCM2838_DMA4_DESTI_BURST_LENGTH_SHIFT);
  end; 
 
 {Interrupt Enable}
 if Data.Next = nil then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2838_DMA4_TI_INTEN;
  end;

 {Setup Next Control Block}
 if Data.Next <> nil then
  begin
   {Set Next Block}
   Block.NextControlBlockAddress:=((PtrUInt(Block) + SizeOf(TBCM2838DMA40ControlBlock)) and BCM2838_DMA4_NEXT_CB_ADDR_MASK) shr BCM2838_DMA4_NEXT_CB_ADDR_SHIFT;
  end
 else
  begin
   Block.NextControlBlockAddress:=0;
  end;  

 {Setup Reserved} 
 Block.Reserved1:=0;
 
 {Flush Source} 
 case Request.Direction of
  DMA_DIR_MEM_TO_MEM,DMA_DIR_MEM_TO_DEV:begin
    if not(BCM2711DMA_CACHE_COHERENT) or ((Request.Flags and DMA_REQUEST_FLAG_COMPATIBLE) = 0) then
     begin
      if (Data.Flags and DMA_DATA_FLAG_NOCLEAN) = 0 then
       begin
        if ((Data.Flags and DMA_DATA_FLAG_STRIDE) = 0) or (Data.SourceStride = 0) then
         begin
          CleanDataCacheRange(PtrUInt(Data.Source),Data.Size);
         end
        else
         begin
          Offset:=0;
          while Offset < Data.Size do
           begin
            CleanDataCacheRange(PtrUInt(Data.Source + Offset),Data.StrideLength);
            
            Inc(Offset,Data.SourceStride);
           end; 
         end;
       end;
     end;
   end;
 end;  
end;
 
{==============================================================================}
{==============================================================================}
{BCM2711 PWM0/1 Functions}
function BCM2711PWM0Start(PWM:PPWMDevice):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Start');
 {$ENDIF}
 
 {Check Settings}
 if PWM.Range = 0 then Exit;
 if PWM.Frequency = 0 then Exit;
 
 {Check GPIO}
 if PWM.GPIO = GPIO_PIN_UNKNOWN then
  begin
   {Check Id}
   case PBCM2711PWM0Device(PWM).Id of
    0:begin
      {PWM Device 0}
      {Check Channel}
      case PBCM2711PWM0Device(PWM).Channel of
       0:begin
         {PWM Channel 1}
         {Set GPIO 18}
         if BCM2711PWM0SetGPIO(PWM,GPIO_PIN_18) <> ERROR_SUCCESS then Exit;
        end; 
       1:begin
         {PWM Channel 2}
         {Set GPIO 19}
         if BCM2711PWM0SetGPIO(PWM,GPIO_PIN_19) <> ERROR_SUCCESS then Exit;
        end;
       else
        begin
         Exit;
        end;   
      end;   
     end;
    1:begin
      {PWM Device 1}
      {Check Channel}
      case PBCM2711PWM0Device(PWM).Channel of
       0:begin
         {PWM Channel 1}
         {Set GPIO 40}
         if BCM2711PWM0SetGPIO(PWM,GPIO_PIN_40) <> ERROR_SUCCESS then Exit;
        end; 
       1:begin
         {PWM Channel 2}
         {Set GPIO 41}
         if BCM2711PWM0SetGPIO(PWM,GPIO_PIN_41) <> ERROR_SUCCESS then Exit;
        end;
       else
        begin
         Exit;
        end;   
      end;   
     end;
   end;     
  end;
  
 {Start Clock}
 if BCM2711PWM0ClockStart(PWM,PWM.Frequency) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2711PWM0Device(PWM).Channel of
  0:begin
    {PWM Channel 1}
    {Enable PWEN}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL or BCM2838_PWM_CTL_PWEN1;
   end;
  1:begin
    {PWM Channel 2}
    {Enable PWEN}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL or BCM2838_PWM_CTL_PWEN2;
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  CTL=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  STA=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).STA,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  RNG1=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  DAT1=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  RNG2=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG2,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  DAT2=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT2,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0Stop(PWM:PPWMDevice):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Stop');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2711PWM0Device(PWM).Channel of
  0:begin
    {PWM Channel 1}
    {Disable PWEN}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_PWEN1);
   end;
  1:begin
    {PWM Channel 2}
    {Disable PWEN}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_PWEN2);
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {Stop Clock}
 if BCM2711PWM0ClockStop(PWM) <> ERROR_SUCCESS then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  CTL=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  STA=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).STA,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  RNG1=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  DAT1=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  RNG2=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG2,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  DAT2=' + IntToHex(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT2,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0Write(PWM:PPWMDevice;Value:LongWord):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Write (Value=' + IntToHex(Value,4) + ')');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2711PWM0Device(PWM).Channel of
  0:begin
    {PWM Channel 1}
    {Set Data}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT1:=Value;
   end;
  1:begin
    {PWM Channel 2}
    {Set Data}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT2:=Value;
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}
 
function BCM2711PWM0SetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Set GPIO (GPIO=' + IntToStr(GPIO) + ')');
 {$ENDIF}

 {Check Id}
 case PBCM2711PWM0Device(PWM).Id of
  0:begin
    {PWM Device 0}
    {Check Channel}
    case PBCM2711PWM0Device(PWM).Channel of
     0:begin
       {PWM Channel 1}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_12:begin
          {Function Select 0}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
         end;
        GPIO_PIN_18:begin
          {Function Select 5}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT5);
         end;
        else
         begin
          Exit;
         end;      
       end; 
       
       {Reset GPIO}
       if GPIO <> GPIO_PIN_12 then BCM2711PWM0ResetGPIO(PWM,GPIO_PIN_12);
       if GPIO <> GPIO_PIN_18 then BCM2711PWM0ResetGPIO(PWM,GPIO_PIN_18);
      end;
     1:begin
       {PWM Channel 2}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_13:begin
          {Function Select 0}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
         end;
        GPIO_PIN_19:begin
          {Function Select 5}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT5);
         end;
        GPIO_PIN_45:begin
          {Function Select 0}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
         end;
        else
         begin
          Exit;
         end;      
       end; 
       
       {Reset GPIO}
       if GPIO <> GPIO_PIN_13 then BCM2711PWM0ResetGPIO(PWM,GPIO_PIN_13);
       if GPIO <> GPIO_PIN_19 then BCM2711PWM0ResetGPIO(PWM,GPIO_PIN_19);
       if GPIO <> GPIO_PIN_45 then BCM2711PWM0ResetGPIO(PWM,GPIO_PIN_45);
      end;
     else
      begin
       Exit;
      end;   
    end;
   end;
  1:begin
    {PWM Device 1}
    {Check Channel}
    case PBCM2711PWM0Device(PWM).Channel of
     0:begin
       {PWM Channel 1}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_40:begin
          {Function Select 0}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
         end;
        else
         begin
          Exit;
         end;      
       end; 
       
       {Reset GPIO}
       if GPIO <> GPIO_PIN_40 then BCM2711PWM0ResetGPIO(PWM,GPIO_PIN_40);
      end;
     1:begin
       {PWM Channel 2}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_41:begin
          {Function Select 0}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
         end;
        else
         begin
          Exit;
         end;      
       end; 
       
       {Reset GPIO}
       if GPIO <> GPIO_PIN_41 then BCM2711PWM0ResetGPIO(PWM,GPIO_PIN_41);
      end;
     else
      begin
       Exit;
      end;   
    end;
   end;
 end;
 
 {Update Properties}
 PWM.GPIO:=GPIO;
 PWM.Properties.GPIO:=GPIO;

 {Delay} 
 MicrosecondDelay(110);
  
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0ResetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Reset GPIO (GPIO=' + IntToStr(GPIO) + ')');
 {$ENDIF}
 
 {Check Id}
 case PBCM2711PWM0Device(PWM).Id of
  0:begin
    {PWM Device 0}
    {Check Channel}
    case PBCM2711PWM0Device(PWM).Channel of
     0:begin
       {PWM Channel 1}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_12,GPIO_PIN_18:begin 
          {Function Select IN}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
         end;
        else
         begin
          Exit;
         end;      
       end; 
      end;
     1:begin
       {PWM Channel 2}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_13,GPIO_PIN_19,GPIO_PIN_45:begin
          {Function Select IN}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
         end;
        else
         begin
          Exit;
         end;      
       end; 
      end;
     else
      begin
       Exit;
      end;   
    end;
   end;
  1:begin
    {PWM Device 1}
    {Check Channel}
    case PBCM2711PWM0Device(PWM).Channel of
     0:begin
       {PWM Channel 1}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_40:begin 
          {Function Select IN}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
         end;
        else
         begin
          Exit;
         end;      
       end; 
      end;
     1:begin
       {PWM Channel 2}
       {Check GPIO}
       case GPIO of
        GPIO_PIN_41:begin
          {Function Select IN}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
         end;
        else
         begin
          Exit;
         end;      
       end; 
      end;
     else
      begin
       Exit;
      end;   
    end;
   end;
 end;
    
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 
    
{==============================================================================}

function BCM2711PWM0SetMode(PWM:PPWMDevice;Mode:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Set Mode (Mode=' + IntToStr(Mode) + ')');
 {$ENDIF}
 
 {Check Mode}
 if Mode > PWM_MODE_SERIALIZED then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2711PWM0Device(PWM).Channel of
  0:begin
    {PWM Channel 1}
    {Check Mode}
    case Mode of
     PWM_MODE_MARKSPACE:begin
       {Mark Space (Enable MSEN)}
       PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_MODE1)) or BCM2838_PWM_CTL_MSEN1;
      end;
     PWM_MODE_BALANCED:begin
       {Balanced (Disable MSEN / MODE)}
       PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_MODE1 or BCM2838_PWM_CTL_MSEN1));
      end;
     PWM_MODE_SERIALIZED:begin
       {Serialized (Enable MODE)}
       PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_MSEN1)) or BCM2838_PWM_CTL_MODE1;
      end;
    end;
   end;
  1:begin
    {PWM Channel 2}
    case Mode of
     PWM_MODE_MARKSPACE:begin
       {Mark Space (Enable MSEN)}
       PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_MODE2)) or BCM2838_PWM_CTL_MSEN2;
      end;
     PWM_MODE_BALANCED:begin
       {Balanced (Disable MSEN / MODE)}
       PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_MODE2 or BCM2838_PWM_CTL_MSEN2));
      end;
     PWM_MODE_SERIALIZED:begin
       {Serialized (Enable MODE)}
       PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=(PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_MSEN2)) or BCM2838_PWM_CTL_MODE2;
      end;
    end;
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 PWM.Mode:=Mode;
 PWM.Properties.Mode:=Mode;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0SetRange(PWM:PPWMDevice;Range:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Set Range (Range=' + IntToStr(Range) + ')');
 {$ENDIF}
 
 {Check Range}
 if Range = 0 then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2711PWM0Device(PWM).Channel of
  0:begin
    {PWM Channel 1}
    {Set Range}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG1:=Range;
   end;
  1:begin
    {PWM Channel 2}
    {Set Range}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG2:=Range;
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {Update Properties}
 PWM.Range:=Range;
 PWM.Properties.Range:=Range;
 
 {Delay}
 {MicrosecondDelay(10);}
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0SetFrequency(PWM:PPWMDevice;Frequency:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Set Frequency (Frequency=' + IntToStr(Frequency) + ')');
 {$ENDIF}
 
 {Check Frequency}
 if Frequency = 0 then Exit;
 
 {Check Pair}
 if PBCM2711PWM0Device(PWM).Pair <> nil then
  begin
   {Check Enabled}
   if PBCM2711PWM0Device(PWM).Pair.PWM.PWMState = PWM_STATE_ENABLED then Exit;
  end;
  
 {Stop Clock}
 if BCM2711PWM0ClockStop(PWM) <> ERROR_SUCCESS then Exit;
 
 {Check Enabled}
 if PWM.PWMState = PWM_STATE_ENABLED then
  begin
   {Start Clock}
   if BCM2711PWM0ClockStart(PWM,Frequency) <> ERROR_SUCCESS then Exit;
  end; 
 
 {Update Scaler}
 PBCM2711PWM0Device(PWM).Scaler:=NANOSECONDS_PER_SECOND div Frequency;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  Scaler=' + IntToStr(PBCM2711PWM0Device(PWM).Scaler));
 {$ENDIF}
 
 {Update Properties}
 PWM.Frequency:=Frequency;
 PWM.Properties.Frequency:=Frequency;
 
 {Check Pair}
 if PBCM2711PWM0Device(PWM).Pair <> nil then
  begin
   {Update Scaler}
   PBCM2711PWM0Device(PWM).Pair.Scaler:=NANOSECONDS_PER_SECOND div Frequency;
   
   {Update Properties}
   PBCM2711PWM0Device(PWM).Pair.PWM.Frequency:=Frequency;
   PBCM2711PWM0Device(PWM).Pair.PWM.Properties.Frequency:=Frequency;
  end;
  
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0SetPolarity(PWM:PPWMDevice;Polarity:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Set Polarity (Polarity=' + IntToStr(Polarity) + ')');
 {$ENDIF}
 
 {Check Polarity}
 if Polarity > PWM_POLARITY_INVERSE then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2711PWM0Device(PWM).Channel of
  0:begin
    {PWM Channel 1}
    {Check Polarity}
    if Polarity = PWM_POLARITY_INVERSE then
     begin
      {Inverse (Enable POLA)}
      PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL or BCM2838_PWM_CTL_POLA1;
     end
    else
     begin
      {Normal (Disable POLA)}
      PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_POLA1);
     end;
   end;
  1:begin
    {PWM Channel 2}
    {Check Polarity}
    if Polarity = PWM_POLARITY_INVERSE then
     begin
      {Inverse (Enable POLA)}
      PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL or BCM2838_PWM_CTL_POLA2;
     end
    else
     begin
      {Normal (Disable POLA)}
      PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL:=PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).CTL and not(BCM2838_PWM_CTL_POLA2);
     end;
   end;
  else
   begin
    Exit;
   end;   
 end;
    
 {Update Properties}
 PWM.Polarity:=Polarity;
 PWM.Properties.Polarity:=Polarity;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0Configure(PWM:PPWMDevice;DutyNS,PeriodNS:LongWord):LongWord;
var
 Data:LongWord;
 Range:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Configure (DutyNS=' + IntToStr(DutyNS) + ' PeriodNS=' + IntToStr(PeriodNS) + ')');
 {$ENDIF}
 
 {Check Period}
 if PeriodNS <= PWM.Properties.MinPeriod then Exit;
 
 {Check Scaler}
 if PBCM2711PWM0Device(PWM).Scaler = 0 then Exit;
 
 {Get Data}
 Data:=DutyNS div PBCM2711PWM0Device(PWM).Scaler;

 {Get Range}
 Range:=PeriodNS div PBCM2711PWM0Device(PWM).Scaler;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2711PWM0Device(PWM).Channel of
  0:begin
    {PWM Channel 1}
    {Set Data}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT1:=Data;

    {Set Range}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG1:=Range;
   end;
  1:begin
    {PWM Channel 2}
    {Set Data}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).DAT2:=Data;
    
    {Set Range}
    PBCM2838PWMRegisters(PBCM2711PWM0Device(PWM).Address).RNG2:=Range;
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {Update Properties}
 PWM.Range:=Range;
 PWM.DutyNS:=DutyNS;
 PWM.PeriodNS:=PeriodNS;
 PWM.Properties.Range:=Range;
 PWM.Properties.DutyNS:=DutyNS;
 PWM.Properties.PeriodNS:=PeriodNS;
 
 {Delay}
 {MicrosecondDelay(10);}
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2711PWM0ClockStart(PWM:PPWMDevice;Frequency:LongWord):LongWord; 
var
 DivisorI:LongWord;
 DivisorR:LongWord;
 DivisorF:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Clock Start');
 {$ENDIF}
 
 {Check Frequency} 
 if Frequency = 0 then Exit;

 {Check Enabled}
 if not BCM2711PWM0ClockEnabled(PWM) then
  begin
   {Get Divisors}
   DivisorI:=BCM2711_PWM0_DEFAULT_CLOCK div Frequency;
   DivisorR:=BCM2711_PWM0_DEFAULT_CLOCK mod Frequency;
   DivisorF:=Trunc((DivisorR * 4096) / BCM2711_PWM0_DEFAULT_CLOCK);
   
   if DivisorI > 4095 then DivisorI:=4095;
  
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Set Dividers}
   PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMDIV)^:=BCM2838_CM_PASSWORD or (DivisorI shl 12) or DivisorF;
   {Delay}
   MicrosecondDelay(10);
  
   {Set Source}   
   PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^:=BCM2838_CM_PASSWORD or BCM2838_CM_CTL_SRC_OSC;
   {Delay}
   MicrosecondDelay(10);
  
   {Start Clock}   
   PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^:=BCM2838_CM_PASSWORD or PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^ or BCM2838_CM_CTL_ENAB;
   {Delay}
   MicrosecondDelay(110);
   
   {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  DivisorI=' + IntToStr(DivisorI));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  DivisorF=' + IntToStr(DivisorF));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  PWMCTL=' + IntToHex(PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^,8));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  PWMDIV=' + IntToHex(PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMDIV)^,8));
   {$ENDIF}
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;  
end; 
 
{==============================================================================}

function BCM2711PWM0ClockStop(PWM:PPWMDevice):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Clock Stop');
 {$ENDIF}

 {Check Pair}
 if PBCM2711PWM0Device(PWM).Pair <> nil then
  begin
   {Check Enabled}
   if PBCM2711PWM0Device(PWM).Pair.PWM.PWMState = PWM_STATE_ENABLED then
    begin
     {Return Result}
     Result:=ERROR_SUCCESS;  
     Exit;
    end; 
  end;
 
 {Check Enabled}
 if BCM2711PWM0ClockEnabled(PWM) then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Stop the Clock}
   PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^:=BCM2838_CM_PASSWORD or (PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^ and not(BCM2838_CM_CTL_ENAB));
   {Delay}
   MicrosecondDelay(110);
   
   {Wait for not Busy}
   while (PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^ and BCM2838_CM_CTL_BUSY) <> 0 do
    begin
     {Delay}
     MicrosecondDelay(1);
    end;
    
   {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  PWMCTL=' + IntToHex(PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^,8));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  PWMDIV=' + IntToHex(PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMDIV)^,8));
   {$ENDIF}
    
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2711PWM0ClockEnabled(PWM:PPWMDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711: PWM Clock Enabled');
 {$ENDIF}
 
 {Check Clock}
 if (PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^ and BCM2838_CM_CTL_ENAB) <> 0 then
  begin
   {Return Result}
   Result:=True;
  end;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2711:  PWMCTL=' + IntToHex(PLongWord(BCM2838_CM_REGS_BASE + BCM2838_CM_PWMCTL)^,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end; 

{==============================================================================}
{==============================================================================}
{BCM2711 PCM Functions}


{==============================================================================}
{==============================================================================}
{BCM2711 GPIO Functions}
function BCM2711GPIOStart(GPIO:PGPIODevice):LongWord; 
var
 Pin:LongWord;
 Count:LongWord;
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Start');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Clear Registers}
 for Count:=0 to BCM2838_GPIO_BANK_COUNT - 1 do
  begin
   {Event Detect Registers}
   PLongWord(GPIO.Address + BCM2838_GPREN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2838_GPFEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2838_GPHEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2838_GPLEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2838_GPAREN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2838_GPAFEN0 + (Count * SizeOf(LongWord)))^:=0;
   
   {Event Detect Status}
   Value:=PLongWord(GPIO.Address + BCM2838_GPEDS0 + (Count * SizeOf(LongWord)))^;
   while Value <> 0 do
    begin
     {Get Pin}
     Pin:=FirstBitSet(Value);

     {Clear Status}
     PLongWord(GPIO.Address + BCM2838_GPEDS0 + (Count * SizeOf(LongWord)))^:=(BCM2838_GPEDS_MASK shl Pin);
     
     {Clear Pin}
     Value:=Value xor (1 shl Pin);
    end;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Create Lock}
 PBCM2711GPIODevice(GPIO).Lock:=SpinCreate;
 if PBCM2711GPIODevice(GPIO).Lock = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end; 
 
 {Setup Banks}
 for Count:=0 to BCM2838_GPIO_BANK_COUNT - 1 do
  begin
   PBCM2711GPIODevice(GPIO).Banks[Count].GPIO:=GPIO;
   PBCM2711GPIODevice(GPIO).Banks[Count].Bank:=Count;
   PBCM2711GPIODevice(GPIO).Banks[Count].Address:=PtrUInt(GPIO.Address) + BCM2838_GPEDS0 + (Count * SizeOf(LongWord));
   PBCM2711GPIODevice(GPIO).Banks[Count].PinStart:=Count * 32;
  end;
  
 {Create Pins}
 SetLength(GPIO.Pins,BCM2838_GPIO_PIN_COUNT);
 
 {Setup Pins}
 for Count:=0 to BCM2838_GPIO_PIN_COUNT - 1 do
  begin
   GPIO.Pins[Count].GPIO:=GPIO;
   GPIO.Pins[Count].Pin:=Count;
   GPIO.Pins[Count].Flags:=GPIO_EVENT_FLAG_NONE;
   GPIO.Pins[Count].Trigger:=GPIO_TRIGGER_NONE;
   GPIO.Pins[Count].Count:=0;
   GPIO.Pins[Count].Event:=INVALID_HANDLE_VALUE;
   GPIO.Pins[Count].Events:=nil;
  end;
  
 {Request IRQ/FIQ} //To Do // Redo this for new FIQ model with GIC
 if BCM2711GPIO_FIQ_ENABLED then
  begin
   {Bank0}
   if BCM2711GPIO_FIQ_BANK_NO = 0 then
    begin
     RequestFIQ(FIQ_ROUTING,BCM2838_IRQ_GPIO_0,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[0]);
    end
   else
    begin
     RequestIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_0,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[0]);
    end; 
   
   {Bank1}
   if BCM2711GPIO_FIQ_BANK_NO = 1 then
    begin
     RequestFIQ(FIQ_ROUTING,BCM2838_IRQ_GPIO_1,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[1]);
    end
   else
    begin
     RequestIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_1,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[1]);
    end; 
  end
 else
  begin 
   {Bank0}
   RequestIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_0,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[0]);
   {Bank1}
   RequestIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_1,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[1]);
  end; 

 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2711GPIOStop(GPIO:PGPIODevice):LongWord;
var
 Count:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Stop');
 {$ENDIF}
 
 {Release IRQ/FIQ} //To Do // Redo this for new FIQ model with GIC
 if BCM2711GPIO_FIQ_ENABLED then
  begin
   {Bank0}
   if BCM2711GPIO_FIQ_BANK_NO = 0 then
    begin
     ReleaseFIQ(FIQ_ROUTING,BCM2838_IRQ_GPIO_0,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[0]);
    end
   else
    begin
     ReleaseIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_0,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[0]);
    end; 
   
   {Bank1}
   if BCM2711GPIO_FIQ_BANK_NO = 1 then
    begin
     ReleaseFIQ(FIQ_ROUTING,BCM2838_IRQ_GPIO_1,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[1]);
    end
   else
    begin
     ReleaseIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_1,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[1]);
    end; 
  end
 else
  begin
   {Bank0}
   ReleaseIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_0,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[0]);
   {Bank1}
   ReleaseIRQ(IRQ_ROUTING,BCM2838_IRQ_GPIO_1,TInterruptHandler(BCM2711GPIOInterruptHandler),@PBCM2711GPIODevice(GPIO).Banks[1]);
  end; 
 
 {Release Pins}
 for Count:=0 to BCM2838_GPIO_PIN_COUNT - 1 do
  begin
   if GPIO.Pins[Count].Event <> INVALID_HANDLE_VALUE then
    begin
     EventDestroy(GPIO.Pins[Count].Event);
    end;
   
   if GPIO.Pins[Count].Events <> nil then
    begin
     Event:=GPIO.Pins[Count].Events;
     while Event <> nil do
      begin
       {Deregister Event}
       GPIODeviceDeregisterEvent(GPIO,@GPIO.Pins[Count],Event);
       
       {Destroy Event}
       GPIODeviceDestroyEvent(GPIO,Event);
       
       Event:=GPIO.Pins[Count].Events;
      end;
    end;
  end; 
 
 {Destroy Pins}
 SetLength(GPIO.Pins,0);
 
 {Destroy Lock}
 if PBCM2711GPIODevice(GPIO).Lock <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(PBCM2711GPIODevice(GPIO).Lock);
  end;
 
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2711GPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
begin
 {}
 Result:=0;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Read (Reg=' + IntToHex(Reg,8) + ')');
 {$ENDIF}
 
 {Read Register}
 Result:=PLongWord(GPIO.Address + Reg)^;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2711GPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
begin
 {}
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Write (Reg=' + IntToHex(Reg,8) + ' Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
   
 {Write Value}
 PLongWord(GPIO.Address + Reg)^:=Value;
end;

{==============================================================================}
 
function BCM2711GPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Input Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;

 {Update Statistics}
 Inc(GPIO.GetCount);
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 Reg:=BCM2838_GPLEV0 + ((Pin div 32) * SizeOf(LongWord));
 
 {Read Register}
 Result:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2838_GPLEV_MASK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function BCM2711GPIOInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO  Input Wait (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;
 
 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check Trigger}
 if ((Trigger < BCM2711_GPIO_MIN_TRIGGER) or (Trigger > BCM2711_GPIO_MAX_TRIGGER)) and (Trigger <> GPIO_TRIGGER_EDGE) then Exit;
 
 {Check Existing}
 if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_NONE then
  begin
   if GPIO.Pins[Pin].Trigger <> Trigger then Exit;
   if (GPIO.Pins[Pin].Flags and (GPIO_EVENT_FLAG_REPEAT or GPIO_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
  end;  
 
 {Check Lock}
 if (MutexOwner(GPIO.Lock) <> ThreadGetCurrent) or (MutexCount(GPIO.Lock) > 1) then Exit;
 
 {Check Event}
 if GPIO.Pins[Pin].Event = INVALID_HANDLE_VALUE then
  begin
   {Create Event (Manual Reset)}
   GPIO.Pins[Pin].Event:=EventCreate(True,False);
   
   {Check Event}
   if GPIO.Pins[Pin].Event = INVALID_HANDLE_VALUE then Exit;
  end;
  
 {Update Statistics}
 Inc(GPIO.WaitCount);

 {Check Trigger} 
 if GPIO.Pins[Pin].Trigger = GPIO_TRIGGER_NONE then
  begin
   {Get Shift}
   Shift:=Pin mod 32;
   
   {Set the Flags}
   GPIO.Pins[Pin].Flags:=GPIO_EVENT_FLAG_NONE;
   
   {Set the Trigger}
   GPIO.Pins[Pin].Trigger:=Trigger;
   
   {Acquire the Lock}
   if BCM2711GPIO_FIQ_ENABLED then
    begin
     if SpinLockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
    end
   else
    begin
     if SpinLockIRQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
    end; 
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Check Trigger} 
   if Trigger <> GPIO_TRIGGER_EDGE then
    begin
     {Get Register (Trigger)}
     Reg:=BCM2711_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Trigger)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end
   else
    begin 
     {Get Register (Rising)}
     Reg:=BCM2838_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Rising)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
   
     {Get Register (Falling)}
     Reg:=BCM2838_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Falling)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end; 
    
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Release the Lock}
   if BCM2711GPIO_FIQ_ENABLED then
    begin
     SpinUnlockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock);
    end
   else
    begin
     SpinUnlockIRQ(PBCM2711GPIODevice(GPIO).Lock);
    end; 
  end;
  
 {Increment Count}
 Inc(GPIO.Pins[Pin].Count);
 
 {Release the Lock}
 MutexUnlock(GPIO.Lock);
 
 {Wait for Event}
 if EventWaitEx(GPIO.Pins[Pin].Event,Timeout) = ERROR_SUCCESS then
  begin
   {Get Register (Level)}
   Reg:=BCM2838_GPLEV0 + ((Pin div 32) * SizeOf(LongWord));
   
   {Read Register}
   Result:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2838_GPLEV_MASK;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(GPIO.Lock) <> ERROR_SUCCESS then Exit;
   
   {Decrement Count}
   Dec(GPIO.Pins[Pin].Count);
   
   {Check Count}
   if GPIO.Pins[Pin].Count = 0 then
    begin
     {Check Trigger}
     if GPIO.Pins[Pin].Trigger = Trigger then
      begin
       {Get Shift}
       Shift:=Pin mod 32;
      
       {Acquire the Lock}
       if BCM2711GPIO_FIQ_ENABLED then
        begin
         if SpinLockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
        end
       else
        begin
         if SpinLockIRQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
        end; 
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Check Trigger} 
       if Trigger <> GPIO_TRIGGER_EDGE then
        begin
         {Get Register (Trigger)}
         Reg:=BCM2711_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Trigger)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end
       else
        begin 
         {Get Register (Rising)}
         Reg:=BCM2838_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Rising)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
       
         {Get Register (Falling)}
         Reg:=BCM2838_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Falling)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end; 
        
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Release the Lock}
       if BCM2711GPIO_FIQ_ENABLED then
        begin
         SpinUnlockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock);
        end
       else
        begin
         SpinUnlockIRQ(PBCM2711GPIODevice(GPIO).Lock);
        end; 
      
       {Reset the Flags}
       GPIO.Pins[Pin].Flags:=GPIO_EVENT_FLAG_NONE;
       
       {Reset the Trigger}
       GPIO.Pins[Pin].Trigger:=GPIO_TRIGGER_NONE;
      end; 
    end; 
  end;  
end;

{==============================================================================}

function BCM2711GPIOInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Input Event (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Flags=' + IntToHex(Flags,8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;
 
 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check Flags}
 if ((Flags and GPIO_EVENT_FLAG_REPEAT) <> 0) and ((Trigger = GPIO_TRIGGER_LOW) or (Trigger = GPIO_TRIGGER_HIGH)) then Exit;
 if ((Flags and GPIO_EVENT_FLAG_INTERRUPT) <> 0) and ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) then Exit;
 if ((Flags and GPIO_EVENT_FLAG_REPEAT) <> 0) and (Timeout <> INFINITE) then Exit;
                      
 {Check Trigger}
 if ((Trigger < BCM2711_GPIO_MIN_TRIGGER) or (Trigger > BCM2711_GPIO_MAX_TRIGGER)) and (Trigger <> GPIO_TRIGGER_EDGE) then Exit;
 
 {Check Existing}
 if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_NONE then
  begin
   Result:=ERROR_IN_USE;
   if GPIO.Pins[Pin].Trigger <> Trigger then Exit;
   if (Flags and (GPIO_EVENT_FLAG_REPEAT or GPIO_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
   if (GPIO.Pins[Pin].Flags and (GPIO_EVENT_FLAG_REPEAT or GPIO_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
  end; 

 {Create Event}
 Event:=GPIODeviceCreateEvent(GPIO,@GPIO.Pins[Pin],Callback,Data,Timeout);
 if Event = nil then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Register Event}
 if GPIODeviceRegisterEvent(GPIO,@GPIO.Pins[Pin],Event) <> ERROR_SUCCESS then
  begin
   GPIODeviceDestroyEvent(GPIO,Event);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Update Statistics}
 Inc(GPIO.EventCount);

 {Check Trigger} 
 if GPIO.Pins[Pin].Trigger = GPIO_TRIGGER_NONE then
  begin
   {Get Shift}
   Shift:=Pin mod 32;

   {Set the Flags}
   GPIO.Pins[Pin].Flags:=Flags;
   
   {Set the Trigger}
   GPIO.Pins[Pin].Trigger:=Trigger;

   {Acquire the Lock}
   if BCM2711GPIO_FIQ_ENABLED then
    begin
     if SpinLockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
    end
   else
    begin   
     if SpinLockIRQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
    end; 
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Check Trigger} 
   if Trigger <> GPIO_TRIGGER_EDGE then
    begin
     {Get Register (Trigger)}
     Reg:=BCM2711_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Trigger)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end
   else
    begin 
     {Get Register (Rising)}
     Reg:=BCM2838_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Rising)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
   
     {Get Register (Falling)}
     Reg:=BCM2838_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Falling)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end; 
  
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Release the Lock}
   if BCM2711GPIO_FIQ_ENABLED then
    begin
     SpinUnlockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock);
    end
   else
    begin
     SpinUnlockIRQ(PBCM2711GPIODevice(GPIO).Lock);
    end; 
  end; 
 
 {Increment Count}
 Inc(GPIO.Pins[Pin].Count);
 
 {Check Timeout}
 if Timeout <> INFINITE then
  begin
   {Schedule Worker}
   WorkerSchedule(Timeout,TWorkerTask(BCM2711GPIOEventTimeout),Event,nil);
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711GPIOInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Input Cancel (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;

 {Check Trigger}
 if GPIO.Pins[Pin].Trigger = GPIO_TRIGGER_NONE then
  begin
   Result:=ERROR_NOT_FOUND;
   Exit;
  end;
 
 {Check Flags}
 if (GPIO.Pins[Pin].Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
  begin
   Result:=ERROR_NOT_FOUND;
   Exit;
  end;
 
 {Get Event}
 Event:=GPIO.Pins[Pin].Events;
 if Event <> nil then
  begin
   {Deregister Event}
   GPIODeviceDeregisterEvent(GPIO,@GPIO.Pins[Pin],Event);
   
   {Check Timeout}
   if Event.Timeout = INFINITE then
    begin
     {Destroy Event}
     GPIODeviceDestroyEvent(GPIO,Event);
    end
   else
    begin
     {Set Timeout (Timeout will destroy event)}
     Event.Timeout:=INFINITE;
    end;    
    
   {Decrement Count}
   Dec(GPIO.Pins[Pin].Count);
   
   {Check Count}
   if GPIO.Pins[Pin].Count = 0 then
    begin
     {Check Trigger}
     if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_NONE then
      begin
       {Get Shift}
       Shift:=Pin mod 32;
      
       {Acquire the Lock}
       if BCM2711GPIO_FIQ_ENABLED then
        begin
         if SpinLockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
        end
       else
        begin
         if SpinLockIRQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
        end; 
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Check Trigger} 
       if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_EDGE then
        begin
         {Get Register (Trigger)}
         Reg:=BCM2711_GPIO_TRIGGER_MAP[GPIO.Pins[Pin].Trigger] + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Trigger)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end
       else
        begin 
         {Get Register (Rising)}
         Reg:=BCM2838_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Rising)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
       
         {Get Register (Falling)}
         Reg:=BCM2838_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Falling)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end; 
        
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Release the Lock}
       if BCM2711GPIO_FIQ_ENABLED then
        begin
         SpinUnlockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock);
        end
       else
        begin       
         SpinUnlockIRQ(PBCM2711GPIODevice(GPIO).Lock);
        end; 
      
       {Reset the Flags}
       GPIO.Pins[Pin].Flags:=GPIO_EVENT_FLAG_NONE;
       
       {Reset the Trigger}
       GPIO.Pins[Pin].Trigger:=GPIO_TRIGGER_NONE;
      end; 
    end; 
  end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711GPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;
 
 {Check Level}
 if Level > BCM2711_GPIO_MAX_LEVEL then Exit;
 
 {Update Statistics}
 Inc(GPIO.SetCount);
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 if Level = GPIO_LEVEL_HIGH then
  begin
   Reg:=BCM2838_GPSET0 + ((Pin div 32) * SizeOf(LongWord));
  end
 else
  begin
   Reg:=BCM2838_GPCLR0 + ((Pin div 32) * SizeOf(LongWord));
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Write Register}
 PLongWord(GPIO.Address + Reg)^:=(BCM2838_GPSET_MASK shl Shift);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711GPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Pull Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;

 {Get Shift}
 Shift:=(Pin mod 16) * 2;
 
 {Get Register}
 Reg:=BCM2838_GPPUD0 + ((Pin div 16) * SizeOf(LongWord));

 {Read Register}
 Current:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2838_GPPUD_MASK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=BCM2711_GPIO_PULL_UNMAP[Current];
end;

{==============================================================================}

function BCM2711GPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Value:LongWord;
 Select:LongWord;
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Pull Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOPullToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;
 
 {Check Mode}
 if Mode > BCM2711_GPIO_MAX_PULL then Exit;
 
 {Get Select}
 Select:=BCM2711_GPIO_PULL_MAP[Mode];
 
 {Get Shift}
 Shift:=(Pin mod 16) * 2;
 
 {Get Register}
 Reg:=BCM2838_GPPUD0 + ((Pin div 16) * SizeOf(LongWord));
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Read Value}
 Value:=PLongWord(GPIO.Address + Reg)^;
 
 {Get Current}
 Current:=(Value shr Shift) and BCM2838_GPPUD_MASK;
 
 {Check Current}
 if Select <> Current then
  begin
   {Select Mode}
   Value:=Value and not(BCM2838_GPPUD_MASK shl Shift);
   Value:=Value or (Select shl Shift);
   
   {Write Value}
   PLongWord(GPIO.Address + Reg)^:=Value;
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711GPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Current:LongWord;
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;
 
 {Get Shift}
 Shift:=(Pin mod 10) * 3;
 
 {Get Register}
 Reg:=BCM2838_GPFSEL0 + ((Pin div 10) * SizeOf(LongWord));
 
 {Read Register}
 Current:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2838_GPFSEL_MASK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=BCM2711_GPIO_FUNCTION_UNMAP[Current];
end;

{==============================================================================}

function BCM2711GPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Value:LongWord;
 Select:LongWord;
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2711_GPIO_MAX_PIN then Exit;
 
 {Check Mode}
 if Mode > BCM2711_GPIO_MAX_FUNCTION then Exit;
 
 {Get Select}
 Select:=BCM2711_GPIO_FUNCTION_MAP[Mode];
 
 {Get Shift}
 Shift:=(Pin mod 10) * 3;
 
 {Get Register}
 Reg:=BCM2838_GPFSEL0 + ((Pin div 10) * SizeOf(LongWord));
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Read Value}
 Value:=PLongWord(GPIO.Address + Reg)^;
 
 {Get Current}
 Current:=(Value shr Shift) and BCM2838_GPFSEL_MASK;
 
 {Check Current}
 if Select <> Current then
  begin
   {Check Mode}
   if (Select <> BCM2838_GPFSEL_IN) and (Current <> BCM2838_GPFSEL_IN) then
    begin
     {Select Input}
     Value:=Value and not(BCM2838_GPFSEL_MASK shl Shift);
     Value:=Value or (BCM2838_GPFSEL_IN shl Shift);
     
     {Write Value}
     PLongWord(GPIO.Address + Reg)^:=Value;
    end;
   
   {Select Mode}
   Value:=Value and not(BCM2838_GPFSEL_MASK shl Shift);
   Value:=Value or (Select shl Shift);
   
   {Write Value}
   PLongWord(GPIO.Address + Reg)^:=Value;
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
  
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

procedure BCM2711GPIOInterruptHandler(Bank:PBCM2711GPIOBank);
var
 Bit:LongWord;
 Pin:LongWord;
 Reg:LongWord;
 Shift:LongWord;
 Flags:LongWord;
 Status:LongWord;
 Trigger:LongWord;
 GPIO:PGPIODevice;
 Event:PGPIOEvent;
 ResultCode:LongWord; 
begin
 {}
 {Check Bank}
 if Bank = nil then Exit;

 {Get GPIO}
 GPIO:=Bank.GPIO;
 if GPIO = nil then Exit;
 
 {Acquire the Lock}
 if BCM2711GPIO_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(PBCM2711GPIODevice(GPIO).Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(PBCM2711GPIODevice(GPIO).InterruptCount);
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Get Status}
    Status:=PLongWord(Bank.Address)^;
    while Status <> 0 do
     begin
      {Get Bit}
      Bit:=FirstBitSet(Status);
      
      {Get Pin}
      Pin:=GPIO.Pins[Bank.PinStart + Bit].Pin;
      
      {Get Flags}
      Flags:=GPIO.Pins[Bank.PinStart + Bit].Flags;
      
      {Get Trigger}
      Trigger:=GPIO.Pins[Bank.PinStart + Bit].Trigger;
      
      {Check Trigger}
      if Trigger <> GPIO_TRIGGER_NONE then
       begin
        {Get Shift}
        Shift:=Pin mod 32;
        
        {Remove Triggers}
        if Trigger <> GPIO_TRIGGER_EDGE then
         begin
          {Check Flags}
          if ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) or (Trigger = GPIO_TRIGGER_LOW) or (Trigger = GPIO_TRIGGER_HIGH) then
           begin
            {Get Register (Trigger)}
            Reg:=BCM2711_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Trigger)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
         end
        else
         begin
          {Check Flags}
          if (Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
           begin
            {Get Register (Rising)}
            Reg:=BCM2838_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Rising)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
            
            {Get Register (Falling)}
            Reg:=BCM2838_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Falling)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
         end;     
       end; 
      
      {Clear Status}
      PLongWord(Bank.Address)^:=(BCM2838_GPEDS_MASK shl Bit);
      
      {Check Flags}
      if ((Flags and GPIO_EVENT_FLAG_INTERRUPT) = 0) or ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) then
       begin
        {Send Event}
        if BCM2711GPIO_FIQ_ENABLED then
         begin
          WorkerScheduleFIQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2711GPIOEventTrigger),@GPIO.Pins[Bank.PinStart + Bit],nil);
         end
        else
         begin
          WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2711GPIOEventTrigger),@GPIO.Pins[Bank.PinStart + Bit],nil);
         end; 
       end
      else
       begin
        {Call Event (Only for Repeating Interrupt events)}
        Event:=GPIO.Pins[Bank.PinStart + Bit].Events;
        if (Event <> nil) and Assigned(Event.Callback) then
         begin
          Event.Callback(Event.Data,Pin,Trigger);
         end;
       end;    
      
      {Clear Bit}
      Status:=Status xor (1 shl Bit);
     end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    {Release the Lock}
    if BCM2711GPIO_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock);
     end
    else
     begin    
      SpinUnlockIRQ(PBCM2711GPIODevice(GPIO).Lock);
     end; 
   end;   
  end; 
end;

{==============================================================================}

procedure BCM2711GPIOEventTrigger(Pin:PGPIOPin);
var
 Count:LongWord;
 Flags:LongWord;
 Trigger:LongWord;
 GPIO:PGPIODevice;
 Next:PGPIOEvent;
 Event:PGPIOEvent;
 Events:PGPIOEvent;
 Single:TGPIOEvent;
 Current:PGPIOEvent;
begin
 {}
 {Check Pin}
 if Pin = nil then Exit;

 {Get GPIO}
 GPIO:=Pin.GPIO;
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Event Trigger (Pin=' + GPIOPinToString(Pin.Pin) + ')');
 {$ENDIF}
 
 {Setup Count}
 Count:=0;
 
 {Setup Events}
 Events:=nil;

 {Setup Single}
 FillChar(Single,SizeOf(TGPIOEvent),0);
 
 {Get Flags}
 Flags:=Pin.Flags;
 
 {Get Trigger}
 Trigger:=Pin.Trigger;
 
 {Acquire the Lock}
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Flags}
    if (Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
     begin
      {Signal Event}
      if Pin.Event <> INVALID_HANDLE_VALUE then
       begin
        EventPulse(Pin.Event);
       end;
      
      {Count Events}
      Event:=Pin.Events;
      while Event <> nil do
       begin
        Inc(Count);
        {Get Next}
        Event:=Event.Next;
       end;
      
      {Check Count}
      if Count > 0 then
       begin
        if Count = 1 then
         begin
          {Get Single}
          Event:=Pin.Events;
          if Event <> nil then
           begin
            Single.Callback:=Event.Callback;
            Single.Data:=Event.Data;
            
            {Save Next}
            Next:=Event.Next;
            
            {Deregister Event}
            GPIODeviceDeregisterEvent(GPIO,Pin,Event);
            
            {Check Timeout}
            if Event.Timeout = INFINITE then
             begin
              {Destroy Event}
              GPIODeviceDestroyEvent(GPIO,Event);
             end
            else
             begin
              {Set Timeout (Timeout will destroy event)}
              Event.Timeout:=INFINITE;
             end;
            
            {Get Next}
            Event:=Next;
           end;
         end
        else
         begin        
          {Allocate Events}
          Events:=GetMem(Count * SizeOf(TGPIOEvent));
          Current:=Events;
          
          {Get Events}
          Event:=Pin.Events;
          while Event <> nil do
           begin
            Current.Callback:=Event.Callback;
            Current.Data:=Event.Data;
            Current.Next:=nil;
            if Event.Next <> nil then
             begin
              Current.Next:=PGPIOEvent(PtrUInt(Current) + SizeOf(TGPIOEvent));
              Current:=Current.Next;
             end;
            
            {Save Next}
            Next:=Event.Next;
            
            {Deregister Event}
            GPIODeviceDeregisterEvent(GPIO,Pin,Event);
            
            {Check Timeout}
            if Event.Timeout = INFINITE then
             begin
              {Destroy Event}
              GPIODeviceDestroyEvent(GPIO,Event);
             end
            else
             begin
              {Set Timeout (Timeout will destroy event)}
              Event.Timeout:=INFINITE;
             end;
             
            {Get Next}
            Event:=Next;
           end;
         end;  
       end;
       
      {Reset Flags}
      Pin.Flags:=GPIO_EVENT_FLAG_NONE;
      
      {Reset Trigger}
      Pin.Trigger:=GPIO_TRIGGER_NONE;
      
      {Reset Count}
      Pin.Count:=0;
     end
    else
     begin    
      {Get Single}
      Event:=Pin.Events;
      if Event <> nil then
       begin
        Single.Callback:=Event.Callback;
        Single.Data:=Event.Data;
       end; 
     end; 
   finally
    {Release the Lock}
    MutexUnlock(GPIO.Lock);
   end; 
  end; 

 {Check Flags}  
 if (Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
  begin
   if Count > 0 then
    begin
     if Count = 1 then
      begin
       {Call Event}
       if Assigned(Single.Callback) then
        begin
         Single.Callback(Single.Data,Pin.Pin,Trigger);
        end;
      end
     else
      begin  
       {Get Events}
       Event:=Events;
       while Event <> nil do
        begin
         {Call Event}
         if Assigned(Event.Callback) then
          begin
           Event.Callback(Event.Data,Pin.Pin,Trigger);
          end;
         {Get Next} 
         Event:=Event.Next;
        end;
       
       {Free Events}
       FreeMem(Events);
      end; 
    end; 
  end
 else
  begin
   {Call Event}
   if Assigned(Single.Callback) then
    begin
     Single.Callback(Single.Data,Pin.Pin,Trigger);
    end;
  end;  
end;

{==============================================================================}

procedure BCM2711GPIOEventTimeout(Event:PGPIOEvent);
var
 Reg:LongWord;
 Pin:PGPIOPin;
 Shift:LongWord;
 GPIO:PGPIODevice;
begin
 {}
 {Check Event}
 if Event = nil then Exit;

 {Get Pin}
 Pin:=Event.Pin;
 if Pin = nil then Exit;
 
 {Get GPIO}
 GPIO:=Pin.GPIO;
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2711: GPIO Event Timeout (Pin=' + GPIOPinToString(Pin.Pin) + ' Event=' + PtrToHex(Event) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Timeout}
    if Event.Timeout = INFINITE then
     begin
      {Event must have been handled by trigger}
      
      {Destroy Event}
      GPIODeviceDestroyEvent(GPIO,Event);
     end
    else
     begin
      {Deregister Event}
      GPIODeviceDeregisterEvent(GPIO,Pin,Event);
      
      {Destroy Event}
      GPIODeviceDestroyEvent(GPIO,Event);
      
      {Decrement Count}
      Dec(Pin.Count);
    
      {Check Count}
      if Pin.Count = 0 then 
       begin
        {Check Trigger}
        if Pin.Trigger <> GPIO_TRIGGER_NONE then
         begin
          {Get Shift}
          Shift:=Pin.Pin mod 32;
         
          {Acquire the Lock}
          if BCM2711GPIO_FIQ_ENABLED then
           begin
            if SpinLockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
           end
          else
           begin
            if SpinLockIRQ(PBCM2711GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
           end; 
          
          {Memory Barrier}
          DataMemoryBarrier; {Before the First Write}
          
          {Check Trigger} 
          if Pin.Trigger <> GPIO_TRIGGER_EDGE then
           begin
            {Get Register (Trigger)}
            Reg:=BCM2711_GPIO_TRIGGER_MAP[Pin.Trigger] + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Trigger)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end
          else
           begin 
            {Get Register (Rising)}
            Reg:=BCM2838_GPREN0 + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Rising)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
          
            {Get Register (Falling)}
            Reg:=BCM2838_GPFEN0 + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Falling)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
          
          {Memory Barrier}
          DataMemoryBarrier; {After the Last Read} 
          
          {Release the Lock}
          if BCM2711GPIO_FIQ_ENABLED then
           begin
            SpinUnlockIRQFIQ(PBCM2711GPIODevice(GPIO).Lock);
           end
          else
           begin
            SpinUnlockIRQ(PBCM2711GPIODevice(GPIO).Lock);
           end; 
         
          {Reset the Flags}
          Pin.Flags:=GPIO_EVENT_FLAG_NONE;
          
          {Reset the Trigger}
          Pin.Trigger:=GPIO_TRIGGER_NONE;
         end; 
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(GPIO.Lock);
   end; 
  end;    
end;

{==============================================================================}
{==============================================================================}
{BCM2711 UART0/2/3/4/5 Functions}
function BCM2711UART0Open(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
{Implementation of UARTDeviceOpen API for BCM2711 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceOpen instead}
var
 Control:LongWord;
 Divisor:LongWord;
 LineControl:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + IntToStr(DataBits) + ' StopBits=' + IntToStr(StopBits) + ' Parity=' + IntToStr(Parity) + ' FlowControl=' + IntToStr(FlowControl) + ')');
 {$ENDIF}
 
 {Update Clock Rate}
 PBCM2711UART0Device(UART).ClockRate:=ClockGetRate(CLOCK_ID_UART0);
 if PBCM2711UART0Device(UART).ClockRate = 0 then PBCM2711UART0Device(UART).ClockRate:=ClockGetMeasuredRate(CLOCK_ID_UART0);
 if PBCM2711UART0Device(UART).ClockRate = 0 then ClockSetRate(CLOCK_ID_UART0,BCM2711_UART0_CLOCK_RATE,True);
 if PBCM2711UART0Device(UART).ClockRate = 0 then PBCM2711UART0Device(UART).ClockRate:=BCM2711_UART0_CLOCK_RATE; 
 
 {Update Properties}
 UART.Properties.MaxRate:=PBCM2711UART0Device(UART).ClockRate div 16;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  ClockRate=' + IntToStr(PBCM2711UART0Device(UART).ClockRate) + ' MaxRate=' + IntToStr(UART.Properties.MaxRate));
 {$ENDIF}
 
 {Check Baud Rate}
 if ((BaudRate < BCM2711_UART0_MIN_BAUD) or (BaudRate > UART.Properties.MaxRate)) and (BaudRate <> SERIAL_BAUD_RATE_DEFAULT) then Exit;
 
 {Check Data Bits}
 if (DataBits < BCM2711_UART0_MIN_DATABITS) or (DataBits > BCM2711_UART0_MAX_DATABITS) then Exit;
 
 {Check Stop Bits}
 if (StopBits < BCM2711_UART0_MIN_STOPBITS) or (StopBits > BCM2711_UART0_MAX_STOPBITS) then Exit;
 
 {Check Parity}
 if Parity > BCM2711_UART0_MAX_PARITY then Exit;
 
 {Check Flow Control}
 if FlowControl > BCM2711_UART0_MAX_FLOW then Exit;
 
 {Adjust Baud Rate}
 if BaudRate = SERIAL_BAUD_RATE_DEFAULT then
  begin
   BaudRate:=SERIAL_BAUD_RATE_STANDARD;
   if (BaudRate > UART.Properties.MaxRate) then BaudRate:=SERIAL_BAUD_RATE_FALLBACK;
  end; 

 {Enable GPIO Pins}
 case BoardGetType of 
  BOARD_TYPE_RPI4B,BOARD_TYPE_RPI400,BOARD_TYPE_RPI_COMPUTE4:begin
    {On Raspberry Pi 4B/400/CM4 UART0 may be connected to the Bluetooth on pins 32 and 33}
    if PBCM2711UART0Device(UART).Id = 0 then
     begin
      GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_IN);
      GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_IN);
     end; 
   end;
 end;  
 GPIOPullSelect(PBCM2711UART0Device(UART).TXDPin,GPIO_PULL_NONE);
 GPIOFunctionSelect(PBCM2711UART0Device(UART).TXDPin,PBCM2711UART0Device(UART).TXDFunction);
 GPIOPullSelect(PBCM2711UART0Device(UART).RXDPin,GPIO_PULL_UP);
 GPIOFunctionSelect(PBCM2711UART0Device(UART).RXDPin,PBCM2711UART0Device(UART).RXDFunction);
 
 {Check Flow Conrol}
 if FlowControl > SERIAL_FLOW_NONE then
  begin 
   GPIOPullSelect(PBCM2711UART0Device(UART).CTSPin,GPIO_PULL_NONE);
   GPIOFunctionSelect(PBCM2711UART0Device(UART).CTSPin,PBCM2711UART0Device(UART).CTSFunction);
   GPIOPullSelect(PBCM2711UART0Device(UART).RTSPin,GPIO_PULL_NONE);
   GPIOFunctionSelect(PBCM2711UART0Device(UART).RTSPin,PBCM2711UART0Device(UART).RTSFunction);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Reset Control (Disable UART)}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR:=0;
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IMSC:=0;
 
 {Acknowledge Interrupts}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).ICR:=$7FF;
 
 {Reset Line Control (Flush FIFOs)}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).LCRH:=0;
 
 {Calculate Divisor}
 if BaudRate > (PBCM2711UART0Device(UART).ClockRate div 16) then
  begin
   Divisor:=DivRoundClosest(PBCM2711UART0Device(UART).ClockRate * 8,BaudRate);
  end
 else
  begin
   Divisor:=DivRoundClosest(PBCM2711UART0Device(UART).ClockRate * 4,BaudRate);
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  BaudRate=' + IntToStr(BaudRate) + ' Divisor=' + IntToStr(Divisor) + ' Divisor shr 6=' + IntToStr(Divisor shr 6) + ' Divisor and $3F=' + IntToStr(Divisor and $3f));
 {$ENDIF}

 {Set Baud Rate}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FBRD:=Divisor and $3f;
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IBRD:=Divisor shr 6;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Integer Divisor=' + IntToStr(PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IBRD));
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Fractional Divisor=' + IntToStr(PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FBRD));
 {$ENDIF}
  
 {Get Line Control}
 LineControl:=BCM2838_PL011_LCRH_FEN;
 {Data Bits}
 case DataBits of
  SERIAL_DATA_8BIT:LineControl:=LineControl or BCM2838_PL011_LCRH_WLEN8;
  SERIAL_DATA_7BIT:LineControl:=LineControl or BCM2838_PL011_LCRH_WLEN7;
  SERIAL_DATA_6BIT:LineControl:=LineControl or BCM2838_PL011_LCRH_WLEN6;
  SERIAL_DATA_5BIT:LineControl:=LineControl or BCM2838_PL011_LCRH_WLEN5;
 end;
 {Stop Bits}
 case StopBits of
  SERIAL_STOP_2BIT:LineControl:=LineControl or BCM2838_PL011_LCRH_STP2;
 end;
 {Parity}
 case Parity of
  SERIAL_PARITY_ODD:LineControl:=LineControl or BCM2838_PL011_LCRH_PEN;
  SERIAL_PARITY_EVEN:LineControl:=LineControl or BCM2838_PL011_LCRH_PEN or BCM2838_PL011_LCRH_EPS;
 end;
 
 {Set Line Control}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).LCRH:=LineControl;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Line Control=' + IntToHex(PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).LCRH,8));
 {$ENDIF}
 
 {Set Interrupt FIFO Level}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IFLS:=BCM2838_PL011_IFLS_RXIFLSEL1_8 or BCM2838_PL011_IFLS_TXIFLSEL1_8; {BCM2838_PL011_IFLS_RXIFLSEL1_2 / BCM2838_PL011_IFLS_TXIFLSEL1_2}

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Interrupt FIFO Level=' + IntToHex(PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IFLS,8));
 {$ENDIF}
 
 {Get Control} 
 Control:=BCM2838_PL011_CR_RXE or BCM2838_PL011_CR_TXE or BCM2838_PL011_CR_UARTEN;
 {Flow Control}
 case FlowControl of
  SERIAL_FLOW_RTS_CTS:Control:=Control or BCM2838_PL011_CR_CTSEN or BCM2838_PL011_CR_RTSEN;
 end;
 
 {Create Receive Event (Manual Reset)}
 UART.ReceiveWait:=EventCreate(True,False);
 if UART.ReceiveWait = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Create Transmit Event (Manual Reset / Intitial State)}
 UART.TransmitWait:=EventCreate(True,True);
 if UART.TransmitWait = INVALID_HANDLE_VALUE then
  begin
   EventDestroy(UART.ReceiveWait);
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Allocate Lock}
 PBCM2711UART0Device(UART).Lock:=SpinCreate;
 if PBCM2711UART0Device(UART).Lock = INVALID_HANDLE_VALUE then
  begin
   if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Failed to create device lock');

   EventDestroy(UART.TransmitWait);
   EventDestroy(UART.ReceiveWait);
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end; 
 
 {Set Control (Enable UART)}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR:=Control;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Control=' + IntToHex(PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR,8));
 {$ENDIF}
 
 {Request IRQ}
 RegisterInterrupt(PBCM2711UART0Device(UART).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711UART0SharedInterruptHandler),UART);
 
 {Set Interrupt Mask (Enable Interrupts)}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IMSC:=BCM2838_PL011_IMSC_TXIM or BCM2838_PL011_IMSC_RXIM;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Interrupt Mask=' + IntToHex(PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IMSC,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 UART.Properties.BaudRate:=BaudRate;
 UART.Properties.DataBits:=DataBits;
 UART.Properties.StopBits:=StopBits;
 UART.Properties.Parity:=Parity;
 UART.Properties.FlowControl:=FlowControl;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711UART0Close(UART:PUARTDevice):LongWord;
{Implementation of UARTDeviceClose API for BCM2711 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceClose instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Close');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IMSC:=0;
 
 {Acknowledge Interrupts}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).ICR:=$7FF;
 
 {Release IRQ}
 DeregisterInterrupt(PBCM2711UART0Device(UART).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711UART0SharedInterruptHandler),UART);
 
 {Reset Control (Disable UART)}
 PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR:=0;
 
 {Destroy Lock}
 SpinDestroy(PBCM2711UART0Device(UART).Lock);
 PBCM2711UART0Device(UART).Lock:=INVALID_HANDLE_VALUE;
 
 {Destroy Transmit Event}
 EventDestroy(UART.TransmitWait);
 UART.TransmitWait:=INVALID_HANDLE_VALUE;
 
 {Destroy Receive Event}
 EventDestroy(UART.ReceiveWait);
 UART.ReceiveWait:=INVALID_HANDLE_VALUE;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 UART.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 UART.Properties.DataBits:=SERIAL_DATA_8BIT;
 UART.Properties.StopBits:=SERIAL_STOP_1BIT;
 UART.Properties.Parity:=SERIAL_PARITY_NONE;
 UART.Properties.FlowControl:=SERIAL_FLOW_NONE;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
 
function BCM2711UART0Read(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of UARTDeviceRead API for BCM2711 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceRead instead}

 {$IFDEF BCM2711_UART0_RX_BUFFER}
 function BCM2711UART0PushRX(UART:PUARTDevice):LongWord;
 var
  Limit:LongWord;
  Status:LongWord;
 begin
  {}
  if SpinLockIRQ(PBCM2711UART0Device(UART).Lock) = ERROR_SUCCESS then
   begin
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Buffer Received Data}
    Limit:=BCM2711_UART0_RX_POLL_LIMIT;
    Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
    while ((Status and BCM2838_PL011_FR_RXFE) = 0) and (PBCM2711UART0Device(UART).Count < BCM2711_UART0_RX_BUFFER_SIZE) do
     begin
      {Read Data}
      PBCM2711UART0Device(UART).Buffer[(PBCM2711UART0Device(UART).Start + PBCM2711UART0Device(UART).Count) mod BCM2711_UART0_RX_BUFFER_SIZE]:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).DR;
      
      {Update Count}
      Inc(PBCM2711UART0Device(UART).Count);
      
      {Update Limit}
      Dec(Limit);
      if Limit = 0 then Break;
      
      {Get Status}
      Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
     end;
     
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    SpinUnlockIRQ(PBCM2711UART0Device(UART).Lock);
    
    {Set Event}
    EventSet(UART.ReceiveWait);
    
    Result:=ERROR_SUCCESS;
   end
  else
   begin
    Result:=ERROR_CAN_NOT_COMPLETE;
   end; 
 end;
 {$ENDIF}

var
 Value:LongWord;
 Total:LongWord;
 Offset:LongWord;
 Status:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if (EventState(UART.ReceiveWait) <> EVENT_STATE_SIGNALED) and ((PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR and BCM2838_PL011_FR_RXFE) = 0) then
    begin
     {$IFDEF BCM2711_UART0_RX_BUFFER}
     {Push Receive}
     if BCM2711UART0PushRX(UART) <> ERROR_SUCCESS then
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
     {$ELSE}
     {Push Receive (Set Event)}
     EventSet(UART.ReceiveWait);
     {$ENDIF}
    end;
  
   {Check Non Blocking}
   if ((Flags and UART_READ_NON_BLOCK) <> 0) and (EventState(UART.ReceiveWait) <> EVENT_STATE_SIGNALED) then
    begin
     Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;
 
   {Release the Lock}
   MutexUnlock(UART.Lock);
   
   {Wait for Data}
   Status:=EventWaitEx(UART.ReceiveWait,UART_PUSH_TIMEOUT);
   if Status = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(UART.Lock) = ERROR_SUCCESS then
      begin
       {$IFDEF BCM2711_UART0_RX_BUFFER}
       while (PBCM2711UART0Device(UART).Count > 0) and (Size > 0) do
        begin
         if SpinLockIRQ(PBCM2711UART0Device(UART).Lock) = ERROR_SUCCESS then
          begin
           {Read Data}
           Value:=PBCM2711UART0Device(UART).Buffer[PBCM2711UART0Device(UART).Start];

           {Update Start}
           PBCM2711UART0Device(UART).Start:=(PBCM2711UART0Device(UART).Start + 1) mod BCM2711_UART0_RX_BUFFER_SIZE;
         
           {Update Count}
           Dec(PBCM2711UART0Device(UART).Count);
          
           SpinUnlockIRQ(PBCM2711UART0Device(UART).Lock);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;
         
         {Check for Error}
         if (Value and BCM2838_PL011_DR_ERROR) <> 0 then
          begin
           {Check Error}
           if (Value and BCM2838_PL011_DR_OE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Overrun error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_OVERRUN_ERROR;
            end;
           if (Value and BCM2838_PL011_DR_BE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Break error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_BREAK_ERROR;
            end;
           if (Value and BCM2838_PL011_DR_PE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Parity error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_PARITY_ERROR;
            end;
           if (Value and BCM2838_PL011_DR_FE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Framing error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_FRAMING_ERROR;
            end;
           
           {Update Statistics}
           Inc(UART.ReceiveErrors);
          end;

         {Save Data}
         PByte(Buffer + Offset)^:=Value and BCM2838_PL011_DR_DATA;
         
         {Update Statistics}
         Inc(UART.ReceiveCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
        end;
        
       {Check Count} 
       if PBCM2711UART0Device(UART).Count = 0 then
        begin
         {Reset Event}
         EventReset(UART.ReceiveWait);
        end;
       {$ELSE}
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
 
       {Get Status}
       Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
       while ((Status and BCM2838_PL011_FR_RXFE) = 0) and (Size > 0) do
        begin
         {Read Data}
         Value:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).DR;
         
         {Check for Error}
         if (Value and BCM2838_PL011_DR_ERROR) <> 0 then
          begin
           {Check Error}
           if (Value and BCM2838_PL011_DR_OE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Overrun error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_OVERRUN_ERROR;
            end;
           if (Value and BCM2838_PL011_DR_BE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Break error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_BREAK_ERROR;
            end;
           if (Value and BCM2838_PL011_DR_PE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Parity error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_PARITY_ERROR;
            end;
           if (Value and BCM2838_PL011_DR_FE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2711: Framing error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_FRAMING_ERROR;
            end;
           
           {Update Statistics}
           Inc(UART.ReceiveErrors);
          end;
          
         {Save Data}
         PByte(Buffer + Offset)^:=Value and BCM2838_PL011_DR_DATA;
         
         {Update Statistics}
         Inc(UART.ReceiveCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
        end;
        
       {Check Status}
       if (Status and BCM2838_PL011_FR_RXFE) <> 0 then
        begin
         {Reset Event}
         EventReset(UART.ReceiveWait);
        end;        
 
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       {$ENDIF}
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end
   else if Status = ERROR_WAIT_TIMEOUT then
    begin
     {Acquire the Lock}
     if MutexLock(UART.Lock) = ERROR_SUCCESS then
      begin
       {$IFDEF BCM2711_UART0_RX_BUFFER}
       {Push Receive}
       if BCM2711UART0PushRX(UART) <> ERROR_SUCCESS then
        begin
         Result:=ERROR_CAN_NOT_COMPLETE;
         Exit;
        end;
       {$ELSE}
       {Push Receive (Set Event)}
       EventSet(UART.ReceiveWait);
       {$ENDIF}
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;    
  end;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711UART0Write(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of UARTDeviceWrite API for BCM2711 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceWrite instead}
var
 Total:LongWord;
 Offset:LongWord;
 Status:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if (EventState(UART.TransmitWait) <> EVENT_STATE_SIGNALED) and ((PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR and BCM2838_PL011_FR_TXFF) = 0) then
    begin
     {Set Event}
     EventSet(UART.TransmitWait);
    end;
  
   {Check Non Blocking}
   if ((Flags and UART_WRITE_NON_BLOCK) <> 0) and (EventState(UART.TransmitWait) <> EVENT_STATE_SIGNALED) then
    begin
     Result:=ERROR_INSUFFICIENT_BUFFER;
     Break;
    end;
   
   {Release the Lock}
   MutexUnlock(UART.Lock);
   
   {Wait for Space}
   if EventWait(UART.TransmitWait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(UART.Lock) = ERROR_SUCCESS then
      begin
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
      
       {Get Status}
       Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
       while ((Status and BCM2838_PL011_FR_TXFF) = 0) and (Size > 0) do
        begin
         {Write Data}
         PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).DR:=PByte(Buffer + Offset)^;
         
         {Update Statistics}
         Inc(UART.TransmitCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
        end;
        
       {Check Status}
       if (Status and BCM2838_PL011_FR_TXFF) <> 0 then
        begin
         {Enable Transmit}
         BCM2711UART0EnableInterrupt(PBCM2711UART0Device(UART),BCM2838_PL011_IMSC_TXIM);
         
         {Reset Event}
         EventReset(UART.TransmitWait);
        end;        
      
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;    
  end;
  
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
 
function BCM2711UART0GetStatus(UART:PUARTDevice):LongWord;
{Implementation of UARTDeviceGetStatus API for BCM2711 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceGetStatus instead}
var
 Flags:LongWord;
 Status:LongWord;
 Control:LongWord;
begin
 {}
 Result:=UART_STATUS_NONE;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Get Status');
 {$ENDIF}
 
 {Get Flags}
 Flags:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
 if (Flags and BCM2838_PL011_FR_CTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_CTS;
  end;
 if (Flags and BCM2838_PL011_FR_RXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_FULL;
  end;
 if (Flags and BCM2838_PL011_FR_RXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_EMPTY;
  end;
 if (Flags and BCM2838_PL011_FR_TXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_FULL;
  end;
 if (Flags and BCM2838_PL011_FR_TXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_EMPTY;
  end;
 if (Flags and BCM2838_PL011_FR_BUSY) <> 0 then
  begin
   Result:=Result or UART_STATUS_BUSY;
  end;
 
 {Get Status}
 Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).RSRECR;
 if Status <> 0 then
  begin
   if (Status and BCM2838_PL011_RSRECR_OE) <> 0 then
    begin
     Result:=Result or UART_STATUS_OVERRUN_ERROR;
    end;
   if (Status and BCM2838_PL011_RSRECR_BE) <> 0 then
    begin
     Result:=Result or UART_STATUS_BREAK_ERROR;
    end;
   if (Status and BCM2838_PL011_RSRECR_PE) <> 0 then
    begin
     Result:=Result or UART_STATUS_PARITY_ERROR;
    end;
   if (Status and BCM2838_PL011_RSRECR_FE) <> 0 then
    begin
     Result:=Result or UART_STATUS_FRAMING_ERROR;
    end;
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Clear Status} 
   PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).RSRECR:=0;  
  end;  

 {Get UART Status} 
 if UART.UARTStatus <> UART_STATUS_NONE then
  begin
   Result:=Result or UART.UARTStatus;
   {Clear UART Status}
   UART.UARTStatus:=UART_STATUS_NONE;
  end;

 {Get Control}
 Control:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR;
 if (Control and BCM2838_PL011_CR_RTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_RTS;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function BCM2711UART0SetStatus(UART:PUARTDevice;Status:LongWord):LongWord;
{Implementation of UARTDeviceSetStatus API for BCM2711 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceSetStatus instead}
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Set Status (Status=' + IntToHex(Status,8) + ')');
 {$ENDIF}
 
 {Get Control}
 Control:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read / Before the First Write} 
 
 {Check RTS}
 if (Status and UART_STATUS_RTS) <> 0 then
  begin
   {Enable}
   PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR:=Control or BCM2838_PL011_CR_RTS;
  end
 else
  begin
   {Disable}
   PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).CR:=Control and not(BCM2838_PL011_CR_RTS);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711UART0SharedInterruptHandler(Number,CPUID,Flags:LongWord;UART:PUARTDevice):LongWord;
{Interrupt handler for the BCM2711 UART0 device}
{Note: Not intended to be called directly by applications}
var
 {$IFDEF BCM2711_UART0_RX_BUFFER}
 Limit:LongWord;
 {$ENDIF}
 Status:LongWord;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;
 
 {Check UART}
 if UART = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).MIS;
 if Status = 0 then
  begin
   {No Interrupt}
   Exit;
  end
 else 
  begin
   {Acquire Lock}
   if SpinLockIRQ(PBCM2711UART0Device(UART).Lock) <> ERROR_SUCCESS then Exit;

   {Update Statistics}
   Inc(PBCM2711UART0Device(UART).InterruptCount);
   
   {Acknowledge Interrupts}
   PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).ICR:=Status and not(BCM2838_PL011_ICR_TXIC or BCM2838_PL011_ICR_RXIC);
   
   {Check Transmit}
   if (Status and BCM2838_PL011_MIS_TXMIS) <> 0 then
    begin
     {Acknowledge Transmit}
     PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).ICR:=BCM2838_PL011_ICR_TXIC;
     
     {Send Transmit}
     if WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2711UART0Transmit),UART,nil) = ERROR_SUCCESS then
      begin
       {Mask Transmit}
       PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IMSC:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).IMSC and not(BCM2838_PL011_IMSC_TXIM);
      end; 
    end;
    
   {Check Receive}
   if (Status and BCM2838_PL011_MIS_RXMIS) <> 0 then
    begin
     {Acknowledge Receive}
     PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).ICR:=BCM2838_PL011_ICR_RXIC;

     {$IFDEF BCM2711_UART0_RX_BUFFER}
     {Buffer Received Data}
     Limit:=BCM2711_UART0_RX_POLL_LIMIT;
     Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
     while ((Status and BCM2838_PL011_FR_RXFE) = 0) and (PBCM2711UART0Device(UART).Count < BCM2711_UART0_RX_BUFFER_SIZE) do
      begin
       {Read Data}
       PBCM2711UART0Device(UART).Buffer[(PBCM2711UART0Device(UART).Start + PBCM2711UART0Device(UART).Count) mod BCM2711_UART0_RX_BUFFER_SIZE]:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).DR;
       
       {Update Count}
       Inc(PBCM2711UART0Device(UART).Count);
       
       {Update Limit}
       Dec(Limit);
       if Limit = 0 then Break;
       
       {Get Status}
       Status:=PBCM2838PL011Registers(PBCM2711UART0Device(UART).Address).FR;
      end;
     {$ENDIF}
     
     {Send Receive}
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2711UART0Receive),UART,nil);
    end;
  
   {Release Lock}
   SpinUnlockIRQ(PBCM2711UART0Device(UART).Lock);
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 Result:=INTERRUPT_RETURN_HANDLED;
end;

{==============================================================================}

procedure BCM2711UART0Receive(UART:PUARTDevice);
{Receive handler for the BCM2711 UART0 device}
{Note: Not intended to be called directly by applications}
var
 Serial:PSerialDevice;
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Receive');
 {$ENDIF}
 
 {Check Mode}
 if UART.UARTMode = UART_MODE_SERIAL then
  begin
   {Get Serial}
   Serial:=UART.Serial;
   if Serial = nil then Exit;
   if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
   
   {Acquire the Lock}
   if MutexLock(Serial.Lock) = ERROR_SUCCESS then
    begin
     {Set Event}
     EventSet(UART.ReceiveWait);
     
     {Serial Receive}
     UARTSerialDeviceReceive(UART);
 
     {Release the Lock}
     MutexUnlock(Serial.Lock);
    end;
  end
 else if UART.UARTMode = UART_MODE_UART then
  begin
   {Acquire the Lock}
   if MutexLock(UART.Lock) = ERROR_SUCCESS then
    begin
     {Set Event}
     EventSet(UART.ReceiveWait);
     
     {Release the Lock}
     MutexUnlock(UART.Lock);
    end;
  end;
end;

{==============================================================================}

procedure BCM2711UART0Transmit(UART:PUARTDevice);
{Transmit handler for the BCM2711 UART0 device}
{Note: Not intended to be called directly by applications}
var
 Serial:PSerialDevice;
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2711: UART0 Transmit');
 {$ENDIF}
 
 {Check Mode}
 if UART.UARTMode = UART_MODE_SERIAL then
  begin
   {Get Serial}
   Serial:=UART.Serial;
   if Serial = nil then Exit;
   if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
   
   {Acquire the Lock}
   if MutexLock(Serial.Lock) = ERROR_SUCCESS then
    begin
     {Set Event}
     EventSet(UART.TransmitWait);
     
     {Serial Transmit}
     UARTSerialDeviceTransmit(UART);
 
     {Release the Lock}
     MutexUnlock(Serial.Lock);
    end;
  end
 else if UART.UARTMode = UART_MODE_UART then
  begin
   {Acquire the Lock}
   if MutexLock(UART.Lock) = ERROR_SUCCESS then
    begin
     {Set Event}
     EventSet(UART.TransmitWait);
     
     {Release the Lock}
     MutexUnlock(UART.Lock);
    end;
  end;
end;

{==============================================================================}

procedure BCM2711UART0EnableInterrupt(UART:PBCM2711UART0Device;Interrupt:LongWord); 
{Enable the specified interrupt in the interrupt mask register of a BCM2711 UART0 device}
{UART: The BCM2711 UART0 device to enable the interrupt for}
{Interrupt: The interrupt to enable}

{Note: Caller must hold the UART lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(UART.Lock) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Update Interrupt Mask} 
 PBCM2838PL011Registers(UART.Address).IMSC:=PBCM2838PL011Registers(UART.Address).IMSC or Interrupt;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 {Release Lock}
 SpinUnlockIRQ(UART.Lock);
end;

{==============================================================================}

procedure BCM2711UART0DisableInterrupt(UART:PBCM2711UART0Device;Interrupt:LongWord); 
{Disable the specified interrupt in the interrupt mask register of a BCM2711 UART0 device}
{UART: The BCM2711 UART0 device to disable the interrupt for}
{Interrupt: The interrupt to disable}

{Note: Caller must hold the UART lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(UART.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Update Interrupt Mask} 
 PBCM2838PL011Registers(UART.Address).IMSC:=PBCM2838PL011Registers(UART.Address).IMSC and not(Interrupt);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release Lock}
 SpinUnlockIRQ(UART.Lock);
end;

{==============================================================================}

procedure BCM2711UART0GetGPIOConfig(UART:PBCM2711UART0Device);
{Setup the GPIO config for the supplied UART0 device}
begin
 {}
 if UART = nil then Exit;

 {Check ID}
 case UART.Id of
  0:begin
     {Setup Pins}
     UART.TXDPin:=GPIO_PIN_14;
     UART.RXDPin:=GPIO_PIN_15;
     UART.CTSPin:=GPIO_PIN_16;
     UART.RTSPin:=GPIO_PIN_17;
     {Setup Functions}
     UART.TXDFunction:=GPIO_FUNCTION_ALT0;
     UART.RXDFunction:=GPIO_FUNCTION_ALT0;
     UART.CTSFunction:=GPIO_FUNCTION_ALT3;
     UART.RTSFunction:=GPIO_FUNCTION_ALT3;
   end;  
  2:begin
     {Setup Pins}
     UART.TXDPin:=GPIO_PIN_0;
     UART.RXDPin:=GPIO_PIN_1;
     UART.CTSPin:=GPIO_PIN_2;
     UART.RTSPin:=GPIO_PIN_3;
     {Setup Functions}
     UART.TXDFunction:=GPIO_FUNCTION_ALT4;
     UART.RXDFunction:=GPIO_FUNCTION_ALT4;
     UART.CTSFunction:=GPIO_FUNCTION_ALT4;
     UART.RTSFunction:=GPIO_FUNCTION_ALT4;
   end;  
  3:begin
     {Setup Pins}
     UART.TXDPin:=GPIO_PIN_4;
     UART.RXDPin:=GPIO_PIN_5;
     UART.CTSPin:=GPIO_PIN_6;
     UART.RTSPin:=GPIO_PIN_7;
     {Setup Functions}
     UART.TXDFunction:=GPIO_FUNCTION_ALT4;
     UART.RXDFunction:=GPIO_FUNCTION_ALT4;
     UART.CTSFunction:=GPIO_FUNCTION_ALT4;
     UART.RTSFunction:=GPIO_FUNCTION_ALT4;
   end;  
  4:begin
     {Setup Pins}
     UART.TXDPin:=GPIO_PIN_8;
     UART.RXDPin:=GPIO_PIN_9;
     UART.CTSPin:=GPIO_PIN_10;
     UART.RTSPin:=GPIO_PIN_11;
     {Setup Functions}
     UART.TXDFunction:=GPIO_FUNCTION_ALT4;
     UART.RXDFunction:=GPIO_FUNCTION_ALT4;
     UART.CTSFunction:=GPIO_FUNCTION_ALT4;
     UART.RTSFunction:=GPIO_FUNCTION_ALT4;
   end;  
  5:begin
     {Setup Pins}
     UART.TXDPin:=GPIO_PIN_12;
     UART.RXDPin:=GPIO_PIN_13;
     UART.CTSPin:=GPIO_PIN_14;
     UART.RTSPin:=GPIO_PIN_15;
     {Setup Functions}
     UART.TXDFunction:=GPIO_FUNCTION_ALT4;
     UART.RXDFunction:=GPIO_FUNCTION_ALT4;
     UART.CTSFunction:=GPIO_FUNCTION_ALT4;
     UART.RTSFunction:=GPIO_FUNCTION_ALT4;
   end;  
  else
   begin
     {Setup Pins}
     UART.TXDPin:=GPIO_PIN_UNKNOWN;
     UART.RXDPin:=GPIO_PIN_UNKNOWN;
     UART.CTSPin:=GPIO_PIN_UNKNOWN;
     UART.RTSPin:=GPIO_PIN_UNKNOWN;
     {Setup Functions}
     UART.TXDFunction:=GPIO_FUNCTION_UNKNOWN;
     UART.RXDFunction:=GPIO_FUNCTION_UNKNOWN;
     UART.CTSFunction:=GPIO_FUNCTION_UNKNOWN;
     UART.RTSFunction:=GPIO_FUNCTION_UNKNOWN;
   end;
 end; 
end;

{==============================================================================}
{==============================================================================}
{BCM2711 UART1 (AUX) Functions}

{==============================================================================}
{==============================================================================}
{BCM2711 EMMC0 (SDHCI) Functions}
function BCM2711EMMC0HostStart(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostStart API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostStart instead}
var
 Count:LongWord;
 Value:LongWord;
 Status:LongWord;
 ClockMax:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 SD voltage regulator state = ' + GPIOLevelToString(VirtualGPIOInputGet(VIRTUAL_GPIO_PIN_4)));
 {$ENDIF}
 
 {Reset supply to 3.3V}
 Status:=VirtualGPIOOutputSet(VIRTUAL_GPIO_PIN_4,GPIO_LEVEL_LOW);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI BCM2711 Failed to reset SD voltage regulator');
   
   Result:=Status;
   Exit;
  end;
 MicrosecondDelay(5000); 

 if MMC_LOG_ENABLED then MMCLogInfo(nil,'SDHCI BCM2711 Powering on SD host controller (' + SDHCI.Device.DeviceDescription + ')');
 
 {Power On SD}
 Status:=PowerOn(PBCM2711EMMC0Host(SDHCI).PowerId);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI BCM2711 Failed to power on SD host controller (' + SDHCI.Device.DeviceDescription + ')');
   
   Result:=Status;
   Exit;
  end;

 {Set SD Clock}
 ClockMax:=ClockGetMaxRate(PBCM2711EMMC0Host(SDHCI).ClockId);
 if ClockMax > 0 then
  begin
   {Set SD Clock}
   ClockSetRate(PBCM2711EMMC0Host(SDHCI).ClockId,ClockMax,True);
  end;
 
 {Setup GPIO}
 if PBCM2711EMMC0Host(SDHCI).Id = 0 then
  begin
   {EMMC0}
   if PBCM2711EMMC0Host(SDHCI).SDIO then
    begin
     {Setup SDIO}
     {Check EMMC1 (SDHOST) Enabled}
     if BCM2711_REGISTER_EMMC1 then
      begin
       {Connect GPIO 48 to 53 to SDHOST (ALT0)}     
       {for Count:=GPIO_PIN_48 to GPIO_PIN_53 do
        begin
         GPIOFunctionSelect(Count,GPIO_FUNCTION_ALT0);
        end;} {Note: GPIO 48 to 53 do not connect to the SD card on the Pi4}
      end
     else 
      begin
       {Disconnect GPIO 48 to 53 from SD/MMC (Input)}     
       {for Count:=GPIO_PIN_48 to GPIO_PIN_53 do
        begin
         GPIOFunctionSelect(Count,GPIO_FUNCTION_IN);
        end;} {Note: GPIO 48 to 53 do not connect to the SD card on the Pi4}
      end;

     {Connect GPIO 34 to 39 to SDIO (ALT3)}
     for Count:=GPIO_PIN_34 to GPIO_PIN_39 do
      begin
        GPIOFunctionSelect(Count,GPIO_FUNCTION_ALT3); 
        
        if Count = GPIO_PIN_34 then
         begin
          GPIOPullSelect(Count,GPIO_PULL_NONE);
         end
        else
         begin
          GPIOPullSelect(Count,GPIO_PULL_UP);
         end;
      end;
    end
   else
    begin   
     {Setup SD/MMC}
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Setup BCM2838_GPPINMUX}
     Value:=PLongWord(BCM2838_GPIO_REGS_BASE + BCM2838_GPPINMUX)^;
     Value:=Value or BCM2838_GPPINMUX_LEGACY_EMMC;
     PLongWord(BCM2838_GPIO_REGS_BASE + BCM2838_GPPINMUX)^:=Value;
   
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
   
     {Disconnect GPIO 34 to 39 from SDIO (Input)}
     for Count:=GPIO_PIN_34 to GPIO_PIN_39 do
      begin
       GPIOFunctionSelect(Count,GPIO_FUNCTION_IN); 
      end;
   
     {Connect GPIO 48 to 53 to SD/MMC (ALT3)}
     {for Count:=GPIO_PIN_48 to GPIO_PIN_53 do
      begin
       GPIOFunctionSelect(Count,GPIO_FUNCTION_ALT3); 
       
       if Count = GPIO_PIN_48 then
        begin
         GPIOPullSelect(Count,GPIO_PULL_NONE);
        end
       else
        begin
         GPIOPullSelect(Count,GPIO_PULL_UP);
        end;
      end;} {Note: GPIO 48 to 53 do not connect to the SD card on the Pi4}
    end;
  end
 else if PBCM2711EMMC0Host(SDHCI).Id = 2 then 
  begin
   {EMMC2}
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Setup BCM2838_GPPINMUX}
   Value:=PLongWord(BCM2838_GPIO_REGS_BASE + BCM2838_GPPINMUX)^;
   Value:=Value and not(BCM2838_GPPINMUX_LEGACY_EMMC);
   PLongWord(BCM2838_GPIO_REGS_BASE + BCM2838_GPPINMUX)^:=Value;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Does not appear on GPIO pins}
  end;

 {Update SDHCI}
 {Driver Properties}
 if PBCM2711EMMC0Host(SDHCI).FIQ then
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQFIQ);
  end
 else
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
  end;  
 SDHCI.Version:=SDHCIHostReadWord(SDHCI,SDHCI_HOST_VERSION);
 if PBCM2711EMMC0Host(SDHCI).Id = 0 then
  begin
   {EMMC0}
   SDHCI.DMAWait:=SemaphoreCreate(0);
   SDHCI.Quirks:=SDHCI_QUIRK_BROKEN_CARD_DETECTION or SDHCI_QUIRK_DATA_TIMEOUT_USES_SDCLK or SDHCI_QUIRK_MISSING_CAPS or SDHCI_QUIRK_NO_HISPD_BIT;
   SDHCI.Quirks2:=SDHCI_QUIRK2_PRESET_VALUE_BROKEN;
   {Configuration Properties}
   SDHCI.PresetVoltages:=MMC_VDD_32_33 or MMC_VDD_33_34 or MMC_VDD_165_195;
   SDHCI.PresetCapabilities:=MMC_CAP_CMD23 or MMC_CAP_NEEDS_POLL or MMC_CAP_SDIO_IRQ or MMC_CAP_SD_HIGHSPEED or MMC_CAP_MMC_HIGHSPEED;
  end
 else if PBCM2711EMMC0Host(SDHCI).Id = 2 then 
  begin
   {EMMC2}
   SDHCI.DMAWait:=INVALID_HANDLE_VALUE;
   SDHCI.Quirks:=SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12;
   SDHCI.Quirks2:=0;
   {Configuration Properties}
   SDHCI.PresetVoltages:=0;
   SDHCI.PresetCapabilities:=MMC_CAP_CMD23 or MMC_CAP_3_3V_DDR;
  end;
 SDHCI.ClockMinimum:=BCM2711_EMMC0_MIN_FREQ;
 SDHCI.ClockMaximum:=ClockGetRate(PBCM2711EMMC0Host(SDHCI).ClockId);
 if SDHCI.ClockMaximum = 0 then SDHCI.ClockMaximum:=ClockGetMeasuredRate(PBCM2711EMMC0Host(SDHCI).ClockId);
 if SDHCI.ClockMaximum = 0 then SDHCI.ClockMaximum:=BCM2711_EMMC0_MAX_FREQ;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'SDHCI BCM2711 Maximum clock rate = ' + IntToStr(SDHCI.ClockMaximum));
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 host version = ' + SDHCIVersionToString(SDHCIGetVersion(SDHCI)));
 {$ENDIF}
 
 {Update BCM2711}
 PBCM2711EMMC0Host(SDHCI).WriteDelay:=((2 * 1000000) div BCM2711_EMMC0_MIN_FREQ) + 1;
 PBCM2711EMMC0Host(SDHCI).DelayClock:=BCM2711_EMMC0_MIN_FREQ;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 host write delay = ' + IntToStr(PBCM2711EMMC0Host(SDHCI).WriteDelay));
 {$ENDIF}
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 host reset completed');
 {$ENDIF}
 
 {Setup Interrupts}
 Result:=BCM2711EMMC0SetupInterrupts(SDHCI);
 
 //See: bcm2835_sdhci_init in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2711EMMC0HostStop(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostStop API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostStop instead}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Release the IRQ/FIQ}
 if PBCM2711EMMC0Host(SDHCI).FIQ then
  begin
   DeregisterInterrupt(PBCM2711EMMC0Host(SDHCI).IRQ,CPUIDToMask(FIQ_ROUTING),INTERRUPT_PRIORITY_FIQ,INTERRUPT_FLAG_SHARED or INTERRUPT_FLAG_FIQ,TSharedInterruptHandler(BCM2711EMMC0SharedInterruptHandler),SDHCI);
  end
 else
  begin
   DeregisterInterrupt(PBCM2711EMMC0Host(SDHCI).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711EMMC0SharedInterruptHandler),SDHCI);
  end;  
 
 {Clear Interrupts}
 SDHCI.Interrupts:=0;
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 host reset completed');
 {$ENDIF}

 {Power Off Host}
 SDHCIHostSetPower(SDHCI,$FFFF);
 
 {Update SDHCI}
 {Driver Properties}
 if SDHCI.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(SDHCI.Wait);
   
   SDHCI.Wait:=INVALID_HANDLE_VALUE;
  end; 
 
 {Check for EMMC0}
 if PBCM2711EMMC0Host(SDHCI).Id = 0 then
  begin
   if SDHCI.DMAWait <> INVALID_HANDLE_VALUE then
    begin
     SemaphoreDestroy(SDHCI.DMAWait);
     
     SDHCI.DMAWait:=INVALID_HANDLE_VALUE;
    end; 
  end;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'SDHCI BCM2711 Powering off SD host controller (' + SDHCI.Device.DeviceDescription + ')');

 {Power Off SD}
 Status:=PowerOff(PBCM2711EMMC0Host(SDHCI).PowerId);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI BCM2711 Failed to power off SD host controller (' + SDHCI.Device.DeviceDescription + ')');
   
   Result:=Status;
   Exit;
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711EMMC0HostLock(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostLock API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostLock instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 if PBCM2711EMMC0Host(SDHCI).FIQ then
  begin
   Result:=SpinLockIRQFIQ(SDHCI.Spin);
  end
 else
  begin
   Result:=SpinLockIRQ(SDHCI.Spin);
  end;
end;

{==============================================================================}

function BCM2711EMMC0HostUnlock(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostUnlock API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostUnlock instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 if PBCM2711EMMC0Host(SDHCI).FIQ then
  begin
   Result:=SpinUnlockIRQFIQ(SDHCI.Spin);
  end
 else
  begin
   Result:=SpinUnlockIRQ(SDHCI.Spin);
  end;
end;

{==============================================================================}

function BCM2711EMMC0HostSignal(SDHCI:PSDHCIHost;Semaphore:TSemaphoreHandle):LongWord;
{Implementation of SDHCIHostSignal API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostSignal instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 if PBCM2711EMMC0Host(SDHCI).FIQ then
  begin
   Result:=TaskerSemaphoreSignal(Semaphore,1);
  end
 else
  begin
   Result:=SemaphoreSignal(Semaphore);
  end;
end;

{==============================================================================}

function BCM2711EMMC0HostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
{Implementation of SDHCIHostReadByte API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostReadByte instead}

{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word reads using LongWord reads.
}
var
 Value:LongWord;
 ByteNo:LongWord;
 ByteShift:LongWord;
begin
 {}
 {Read LongWord}
 Value:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Get Byte and Shift}
 ByteNo:=(Reg and 3);
 ByteShift:=(ByteNo shl 3);
 
 {Get Result}
 Result:=(Value shr ByteShift) and $FF;

 //See: bcm2835_sdhci_readb in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2711EMMC0HostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
{Implementation of SDHCIHostReadWord API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostReadWord instead}

{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word reads using LongWord reads.
}
var
 Value:LongWord;
 WordNo:LongWord;
 WordShift:LongWord;
begin
 {}
 {Read LongWord}
 Value:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Get Word and Shift}
 WordNo:=((Reg shr 1) and 1);
 WordShift:=(WordNo shl 4);
 
 {Get Result}
 Result:=(Value shr WordShift) and $FFFF;

 //See: bcm2835_sdhci_readw in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2711EMMC0HostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
{Implementation of SDHCIHostReadLong API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostReadLong instead}
begin
 {}
 {Read LongWord}
 Result:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 //See: bcm2835_sdhci_raw_readl in bcm2835_sdhci.c
 //     bcm2835_sdhci_readl in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2711EMMC0HostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
{Implementation of SDHCIHostWriteByte API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostWriteByte instead}

{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word writes using LongWord writes.
}
var
 Mask:LongWord;
 ByteNo:LongWord;
 ByteShift:LongWord;
 OldValue:LongWord;
 NewValue:LongWord;
begin
 {}
 {Read LongWord}
 OldValue:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Get Byte, Shift and Mask}
 ByteNo:=(Reg and 3);
 ByteShift:=(ByteNo shl 3);
 Mask:=($FF shl ByteShift);

 {Get Value}
 NewValue:=(OldValue and not(Mask)) or (Value shl ByteShift);
 
 {Write LongWord}
 BCM2711EMMC0HostWriteLong(SDHCI,Reg and not(3),NewValue);

 //See: bcm2835_sdhci_writeb in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2711EMMC0HostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
{Implementation of SDHCIHostWriteWord API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostWriteWord instead}

{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word writes using LongWord writes.
}
var
 Mask:LongWord;
 WordNo:LongWord;
 WordShift:LongWord;
 OldValue:LongWord;
 NewValue:LongWord;
begin
 {}
 {Check Register}
 if Reg = SDHCI_COMMAND then
  begin
   {Get LongWord}
   OldValue:=PBCM2711EMMC0Host(SDHCI).ShadowRegister;
  end
 else
  begin
   {Read LongWord}
   OldValue:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;

 {Get Word, Shift and Mask}
 WordNo:=((Reg shr 1) and 1);
 WordShift:=(WordNo shl 4); 
 Mask:=($FFFF shl WordShift);

 {Get Value}
 NewValue:=(OldValue and not(Mask)) or (Value shl WordShift);

 {Check Register}
 if Reg = SDHCI_TRANSFER_MODE then
  begin
   {Check for EMMC0}
   if PBCM2711EMMC0Host(SDHCI).Id = 0 then
    begin
     {Remove the SDHCI_TRNS_DMA flag as the controller fails to perform DMA requests when it is set}
     NewValue:=NewValue and not(SDHCI_TRNS_DMA);
    end; 

   {Save LongWord}
   PBCM2711EMMC0Host(SDHCI).ShadowRegister:=NewValue;
  end
 else
  begin
   {Write LongWord}
   BCM2711EMMC0HostWriteLong(SDHCI,Reg and not(3),NewValue);
  end;  
  
 //See: bcm2835_sdhci_writew in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2711EMMC0HostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
{Implementation of SDHCIHostWriteLong API for BCM2711 SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostWriteLong instead}

{Note: The source code of U-Boot and Linux kernel drivers have this comment

 The Arasan has a bugette whereby it may lose the content of
 successive writes to registers that are within two SD-card clock
 cycles of each other (a clock domain crossing problem).
 It seems, however, that the data register does not have this problem.
 (Which is just as well - otherwise we'd have to nobble the DMA engine too)
 
 For this reason this code must delay after each write to the registers.
}
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Write LongWord}
 PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
 
 {Check Clock}
 if SDHCI.Clock <> PBCM2711EMMC0Host(SDHCI).DelayClock then
  begin
   {Recalculate Delay}
   PBCM2711EMMC0Host(SDHCI).WriteDelay:=((2 * 1000000) div Max(SDHCI.Clock,BCM2711_EMMC0_MIN_FREQ)) + 1;
   PBCM2711EMMC0Host(SDHCI).DelayClock:=SDHCI.Clock;
  end;
 
 {Wait Delay}
 if Reg <> SDHCI_BUFFER then MicrosecondDelay(PBCM2711EMMC0Host(SDHCI).WriteDelay);
              
 //See: bcm2835_sdhci_raw_writel in bcm2835_sdhci.c
 //     bcm2835_sdhci_writel in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2711EMMC0SharedInterruptHandler(Number,CPUID,Flags:LongWord;SDHCI:PSDHCIHost):LongWord;
{Interrupt handler for the BCM2711 SDHCI host controller}
{Note: Not intended to be called directly by applications}
var
 Count:Integer;
 Present:Boolean;
 InterruptMask:LongWord;
 UnexpectedMask:LongWord;
 AcknowledgeMask:LongWord;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Interrupt Handler');
 {$ENDIF}
 
 {Get Interrupt Mask}
 InterruptMask:=SDHCIHostReadLong(SDHCI,SDHCI_INT_STATUS);

 {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Interrupt Handler (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
 {$ENDIF}
 
 {Check for No Interrupts}
 if (InterruptMask = 0) or (InterruptMask = $FFFFFFFF) then Exit;

 {Update Statistics}
 Inc(SDHCI.InterruptCount); 

 Count:=16;
 UnexpectedMask:=0;
 while InterruptMask <> 0 do
  begin
   {Clear selected interrupts}
   AcknowledgeMask:=(InterruptMask and (SDHCI_INT_CMD_MASK or SDHCI_INT_DATA_MASK or SDHCI_INT_BUS_POWER));
   SDHCIHostWriteLong(SDHCI,SDHCI_INT_STATUS,AcknowledgeMask);
   
   {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Interrupt Handler (AcknowledgeMask=' + IntToHex(AcknowledgeMask,8) + ')');
   {$ENDIF}
   
   {Check for insert / remove interrupts}
   if (InterruptMask and (SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE)) <> 0 then
    begin
     {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Insert / Remove Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     {There is a observation on i.mx esdhc. INSERT bit will be immediately set again when it gets cleared, if a card is inserted.
      We have to mask the irq to prevent interrupt storm which will freeze the system. And the REMOVE gets the same situation.
 
      More testing are needed here to ensure it works for other platforms though}
      
     {Get Card Present}
     Present:=(SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and SDHCI_CARD_PRESENT) <> 0;
     
     {Disable insert / remove interrupts}
     SDHCI.Interrupts:=SDHCI.Interrupts and not(SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE);
     
     {Enable insert / remove depending on presence}
     if Present then SDHCI.Interrupts:=SDHCI.Interrupts or SDHCI_INT_CARD_REMOVE;
     if not(Present) then SDHCI.Interrupts:=SDHCI.Interrupts or SDHCI_INT_CARD_INSERT;
     
     {Update interrupts}
     SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
     SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);
     
     {Acknowledge interrupts}
     SDHCIHostWriteLong(SDHCI,SDHCI_INT_STATUS,InterruptMask and (SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE));
                     
     {Signal insert or remove}
     //To Do //Needs an MMC Thread for Insert/Remove Handling and Polling Card Detect (Timer/Worker possibly ?)
    end;
    
   {Check for command iterrupts}
   if (InterruptMask and SDHCI_INT_CMD_MASK) <> 0 then
    begin
     {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Command Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostCommandInterrupt(SDHCI,InterruptMask and SDHCI_INT_CMD_MASK,InterruptMask);
    end;
    
   {Check for data interrupts} 
   if (InterruptMask and SDHCI_INT_DATA_MASK) <> 0 then
    begin
     {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Data Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostDataInterrupt(SDHCI,InterruptMask and SDHCI_INT_DATA_MASK);
    end;
   
   {Check for bus power interrupt}
   if (InterruptMask and SDHCI_INT_BUS_POWER) <> 0 then
    begin
     {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Bus Power Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
    end;
 
   {Check for card interrupt}
   if (InterruptMask and SDHCI_INT_CARD_INT) <> 0 then
    begin
     {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Card Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     {Disable Interrupt}
     SDHCI.Interrupts:=SDHCI.Interrupts and not(SDHCI_INT_CARD_INT);
     
     {Update Interrupts}
     SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
     SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);
     
     {Dispatch Interrupt}
     SDIOHostDispatchInterrupt(SDHCI,not(PBCM2711EMMC0Host(SDHCI).FIQ),PBCM2711EMMC0Host(SDHCI).FIQ);
    end;
   
   {Check for unexpected interrupts}
   InterruptMask:=InterruptMask and not(SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE or SDHCI_INT_CMD_MASK or SDHCI_INT_DATA_MASK or SDHCI_INT_ERROR or SDHCI_INT_BUS_POWER or SDHCI_INT_CARD_INT);
   if InterruptMask <> 0 then
    begin
     UnexpectedMask:=UnexpectedMask or InterruptMask;
     SDHCIHostWriteLong(SDHCI,SDHCI_INT_STATUS,InterruptMask);
    end;
    
   {Check Count}
   Dec(Count);
   if Count <= 0 then Break;
   
   {Get Interrupt Mask}
   InterruptMask:=SDHCIHostReadLong(SDHCI,SDHCI_INT_STATUS);
  end;

 if UnexpectedMask <> 0 then
  begin
   {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Unexpected Interrupt (UnexpectedMask=' + IntToHex(UnexpectedMask,8) + ')');
   {$ENDIF}
  end;
  
 {$IF (DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2711 Interrupt Handler completed');
 {$ENDIF}
  
 Result:=INTERRUPT_RETURN_HANDLED;
 
 //See: bcm2835_mmc_irq in \drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function BCM2711EMMC0SetupInterrupts(SDHCI:PSDHCIHost):LongWord;
{Configure and enable interrupt handling for the BCM2711 SDHCI}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Setup Interrupts}
 SDHCI.Interrupts:=SDHCI_INT_BUS_POWER or SDHCI_INT_DATA_END_BIT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_INDEX or SDHCI_INT_END_BIT or SDHCI_INT_CRC or SDHCI_INT_TIMEOUT or SDHCI_INT_DATA_END or SDHCI_INT_RESPONSE;
                   //SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE or //See sdhci_set_card_detection in \drivers\mmc\host\sdhci.c
                   //Note: SDHCI_INT_CARD_INSERT seems to hang everything, why? //Because the SDHCI_CARD_PRESENT bit is never updated !
                   
 {Enable Interrupts}
 SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
 SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);

 {Request the IRQ/FIQ} 
 if PBCM2711EMMC0Host(SDHCI).FIQ then
  begin
   RegisterInterrupt(PBCM2711EMMC0Host(SDHCI).IRQ,CPUIDToMask(FIQ_ROUTING),INTERRUPT_PRIORITY_FIQ,INTERRUPT_FLAG_SHARED or INTERRUPT_FLAG_FIQ,TSharedInterruptHandler(BCM2711EMMC0SharedInterruptHandler),SDHCI);
  end
 else
  begin
   RegisterInterrupt(PBCM2711EMMC0Host(SDHCI).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCM2711EMMC0SharedInterruptHandler),SDHCI);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
 
 //See: \drivers\mmc\host\bcm2835-mmc.c
end;
 
{==============================================================================}
 
function BCM2711EMMC0DeviceGetCardDetect(MMC:PMMCDevice):LongWord;
{Implementation of MMC GetCardDetect for the BCM2711 which does not update the
 bits in the SDHCI_PRESENT_STATE register to reflect card insertion or removal}
{Note: Not intended to be called directly by applications, use MMCDeviceGetCardDetect instead}
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2711 Get Card Detect');
 {$ENDIF}
 
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
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2711 Get Card Detect (SDHCI_PRESENT_STATE=' + IntToHex(SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE),8) + ')');
 {$ENDIF}
 
 {Check MMC State}
 if MMC.MMCState = MMC_STATE_INSERTED then
  begin
   {Get Card Status}
   if MMCDeviceSendCardStatus(MMC) <> MMC_STATUS_SUCCESS then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);
     
     {Reset Host}
     SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);

     {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2711 Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;
  end
 else
  begin
   {Get Card Present}
   if (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and SDHCI_CARD_PRESENT) <> 0 then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);
     
     {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2711 Get Card Detect (Flags=MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end
   else
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);
     
     {$IF DEFINED(BCM2711_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2711 Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;    
  end;

 Result:=MMC_STATUS_SUCCESS;  
end;

{==============================================================================}
{==============================================================================}
{BCM2711 System Clock Functions}
function BCM2711SystemClockRead(Clock:PClockDevice):LongWord;
{Implementation of ClockDeviceRead API for System Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead instead}
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Read Clock}
 Result:=PBCM2838SystemTimerRegisters(Clock.Address).CLO;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;
 
{==============================================================================}

function BCM2711SystemClockRead64(Clock:PClockDevice):Int64;
{Implementation of ClockDeviceRead64 API for System Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead64 instead}
var
 Check:LongWord;
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get High Value}
 Int64Rec(Result).Hi:=PBCM2838SystemTimerRegisters(Clock.Address).CHI;
 
 {Get Low Value}
 Int64Rec(Result).Lo:=PBCM2838SystemTimerRegisters(Clock.Address).CLO;
 
 {Check High Value}
 Check:=PBCM2838SystemTimerRegisters(Clock.Address).CHI;
 if Check <> Int64Rec(Result).Hi then
  begin
   {Rollover Occurred, Get Low Value Again}
   Int64Rec(Result).Hi:=Check;
   Int64Rec(Result).Lo:=PBCM2838SystemTimerRegisters(Clock.Address).CLO;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2711 ARM Clock Functions}
function BCM2711ARMClockStart(Clock:PClockDevice):LongWord;
{Implementation of ClockDeviceStart API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceStart instead}
var
 Control:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711: ARM Clock Start');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Update Core Clock}
  PBCM2711ARMClock(Clock).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
  if PBCM2711ARMClock(Clock).CoreClock = 0 then PBCM2711ARMClock(Clock).CoreClock:=BCM2711_ARM_CLOCK_CORE_CLOCK;
  
  {Update Min/Max}
  Clock.MinRate:=PBCM2711ARMClock(Clock).CoreClock div (BCM2711_ARM_CLOCK_MAX_DIVIDER + 1);
  Clock.MaxRate:=PBCM2711ARMClock(Clock).CoreClock div (BCM2711_ARM_CLOCK_MIN_DIVIDER + 1);
  
  {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711:  CoreClock=' + IntToStr(PBCM2711ARMClock(Clock).CoreClock) + ' MinRate=' + IntToStr(Clock.MinRate) + ' MaxRate=' + IntToStr(Clock.MaxRate));
  {$ENDIF}
  
  {Check Rate}
  if (Clock.Rate <> 0) and ((Clock.Rate < Clock.MinRate) or (Clock.Rate > Clock.MaxRate)) then Exit;
  if Clock.Rate = 0 then Clock.Rate:=BCM2711_ARM_CLOCK_DEFAULT_RATE;
  
  {Get Divider}
  Divider:=(PBCM2711ARMClock(Clock).CoreClock div Clock.Rate) - 1;
  
  {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711:  Divider=' + IntToStr(Divider));
  {$ENDIF}
  
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}
  
  {Get Control}
  Control:=PBCM2838ARMTimerRegisters(Clock.Address).Control;
  
  {Update Control (Counter Enable / Counter Prescale)}
  Control:=Control and not(BCM2838_ARM_TIMER_CONTROL_COUNTER_PRESCALE);
  Control:=Control or (Divider shl 16) or BCM2838_ARM_TIMER_CONTROL_COUNTER_ENABLED;
  
  {Set Control}
  PBCM2838ARMTimerRegisters(Clock.Address).Control:=Control;
  
  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read} 
  
  {Return Result}
  Result:=ERROR_SUCCESS;  
 finally
  MutexUnlock(Clock.Lock);
 end;
end;
 
{==============================================================================}

function BCM2711ARMClockStop(Clock:PClockDevice):LongWord;
{Implementation of ClockDeviceStop API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceStop instead}
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711: ARM Clock Stop');
 {$ENDIF}

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control}
 Control:=PBCM2838ARMTimerRegisters(Clock.Address).Control;
 
 {Update Control}
 Control:=Control and not(BCM2838_ARM_TIMER_CONTROL_COUNTER_ENABLED);
 
 {Set Control}
 PBCM2838ARMTimerRegisters(Clock.Address).Control:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 MutexUnlock(Clock.Lock);
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;
 
{==============================================================================}

function BCM2711ARMClockRead(Clock:PClockDevice):LongWord;
{Implementation of ClockDeviceRead API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead instead}
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711: ARM Clock Read');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 {Read Counter}
 Result:=PBCM2838ARMTimerRegisters(Clock.Address).Counter;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}

function BCM2711ARMClockRead64(Clock:PClockDevice):Int64;
{Implementation of ClockDeviceRead64 API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead64 instead}
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711: ARM Clock Read64');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 {Read Counter}
 Result:=PBCM2838ARMTimerRegisters(Clock.Address).Counter;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}

function BCM2711ARMClockSetRate(Clock:PClockDevice;Rate:LongWord):LongWord;
{Implementation of ClockDeviceSetRate API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceSetRate instead}
var
 Control:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711: ARM Clock Set Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Enabled}
  if Clock.ClockState <> CLOCK_STATE_ENABLED then
   begin
    {Update Core Clock}
    PBCM2711ARMClock(Clock).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
    if PBCM2711ARMClock(Clock).CoreClock = 0 then PBCM2711ARMClock(Clock).CoreClock:=BCM2711_ARM_CLOCK_CORE_CLOCK;
    
    {Update Min/Max}
    Clock.MinRate:=PBCM2711ARMClock(Clock).CoreClock div (BCM2711_ARM_CLOCK_MAX_DIVIDER + 1);
    Clock.MaxRate:=PBCM2711ARMClock(Clock).CoreClock div (BCM2711_ARM_CLOCK_MIN_DIVIDER + 1);
    
    {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711:  CoreClock=' + IntToStr(PBCM2711ARMClock(Clock).CoreClock) + ' MinRate=' + IntToStr(Clock.MinRate) + ' MaxRate=' + IntToStr(Clock.MaxRate));
    {$ENDIF}
   end;
   
  {Check Rate}
  if (Rate < Clock.MinRate) or (Rate > Clock.MaxRate) then Exit;
  
  {Get Divider}
  Divider:=(PBCM2711ARMClock(Clock).CoreClock div Rate) - 1;
  
  {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2711:  Divider=' + IntToStr(Divider));
  {$ENDIF}
  
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}
  
  {Get Control}
  Control:=PBCM2838ARMTimerRegisters(Clock.Address).Control;
  
  {Update Control}
  Control:=Control and not(BCM2838_ARM_TIMER_CONTROL_COUNTER_PRESCALE);
  Control:=Control or (Divider shl 16);
  
  {Set Control}
  PBCM2838ARMTimerRegisters(Clock.Address).Control:=Control;
  
  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read} 
  
  {Check Rate}
  if (PBCM2711ARMClock(Clock).CoreClock mod Rate) <> 0 then
   begin
    {Update Properties}
    Clock.Rate:=PBCM2711ARMClock(Clock).CoreClock div (Divider + 1);
   
    {Return Result}
    Result:=ERROR_NOT_EXACT;  
   end
  else
   begin
    {Update Properties}
    Clock.Rate:=Rate;
    
    {Return Result}
    Result:=ERROR_SUCCESS;  
   end;  
 finally
  MutexUnlock(Clock.Lock);
 end;
end;

{==============================================================================}
{==============================================================================}
{BCM2711 ARM Timer Functions}
function BCM2711ARMTimerStart(Timer:PTimerDevice):LongWord;
{Implementation of TimerDeviceStart API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceStart instead}
var
 Control:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Start');
 {$ENDIF}
 
 {Update Core Clock}
 PBCM2711ARMTimer(Timer).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
 if PBCM2711ARMTimer(Timer).CoreClock = 0 then PBCM2711ARMTimer(Timer).CoreClock:=BCM2711_ARM_TIMER_CORE_CLOCK;
 
 {Update Properties}
 Timer.Properties.MinRate:=PBCM2711ARMTimer(Timer).CoreClock div (BCM2711_ARM_TIMER_MAX_DIVIDER + 1);
 Timer.Properties.MaxRate:=PBCM2711ARMTimer(Timer).CoreClock div (BCM2711_ARM_TIMER_MIN_DIVIDER + 1);
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711:  CoreClock=' + IntToStr(PBCM2711ARMTimer(Timer).CoreClock) + ' MinRate=' + IntToStr(Timer.Properties.MinRate) + ' MaxRate=' + IntToStr(Timer.Properties.MaxRate));
 {$ENDIF}
 
 {Check Rate}
 if (Timer.Rate <> 0) and ((Timer.Rate < Timer.Properties.MinRate) or (Timer.Rate > Timer.Properties.MaxRate)) then Exit;
 if Timer.Rate = 0 then Timer.Rate:=BCM2711_ARM_TIMER_DEFAULT_RATE;
 
 {Check Interval}
 if (Timer.Interval <> 0) and ((Timer.Interval < Timer.Properties.MinInterval) or (Timer.Interval > Timer.Properties.MaxInterval)) then Exit;
 if Timer.Interval = 0 then Timer.Interval:=BCM2711_ARM_TIMER_MAX_INTERVAL;

 {Get Divider}
 Divider:=(PBCM2711ARMTimer(Timer).CoreClock div Timer.Rate) - 1;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711:  Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Predivider}
 PBCM2838ARMTimerRegisters(Timer.Address).Predivider:=Divider;
 
 {Set Interval}
 PBCM2838ARMTimerRegisters(Timer.Address).Load:=Timer.Interval;
 
 {Get Control}
 Control:=PBCM2838ARMTimerRegisters(Timer.Address).Control;
 
 {Update Control (Timer Enable / Interrupt Enable / 32 Bit Counter / Prescale None / Counter Disabled)}
 Control:=Control and not(BCM2838_ARM_TIMER_CONTROL_PRESCALE);
 Control:=Control or BCM2838_ARM_TIMER_CONTROL_TIMER_ENABLED or BCM2838_ARM_TIMER_CONTROL_INT_ENABLED or BCM2838_ARM_TIMER_CONTROL_32BIT;
 
 {Set Control}
 PBCM2838ARMTimerRegisters(Timer.Address).Control:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Create Event (Manual Reset)}
 Timer.Event:=EventCreate(True,False);

 {Request IRQ/FIQ}
 if BCM2711ARM_TIMER_FIQ_ENABLED then
  begin
   RequestFIQ(FIQ_ROUTING,BCM2838_IRQ_ARM_TIMER,TInterruptHandler(BCM2711ARMTimerInterruptHandler),Timer);
  end
 else
  begin 
   RequestIRQ(IRQ_ROUTING,BCM2838_IRQ_ARM_TIMER,TInterruptHandler(BCM2711ARMTimerInterruptHandler),Timer);
  end; 
 
 {Update Properties}
 {Nothing}
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2711ARMTimerStop(Timer:PTimerDevice):LongWord;
{Implementation of TimerDeviceStop API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceStop instead}
var
 Control:LongWord;
 Waiter:PTimerWaiter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Stop');
 {$ENDIF}
 
 {Release IRQ/FIQ}
 if BCM2711ARM_TIMER_FIQ_ENABLED then
  begin
   ReleaseFIQ(FIQ_ROUTING,BCM2838_IRQ_ARM_TIMER,TInterruptHandler(BCM2711ARMTimerInterruptHandler),Timer);
  end
 else
  begin 
   ReleaseIRQ(IRQ_ROUTING,BCM2838_IRQ_ARM_TIMER,TInterruptHandler(BCM2711ARMTimerInterruptHandler),Timer);
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control}
 Control:=PBCM2838ARMTimerRegisters(Timer.Address).Control;
 
 {Update Control}
 Control:=Control and not(BCM2838_ARM_TIMER_CONTROL_TIMER_ENABLED or BCM2838_ARM_TIMER_CONTROL_INT_ENABLED);
 
 {Set Control}
 PBCM2838ARMTimerRegisters(Timer.Address).Control:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 {Release Waiters}
 if Timer.Waiters <> nil then
  begin
   Waiter:=Timer.Waiters;
   while Waiter <> nil do
    begin
     {Deregister Waiter}
     TimerDeviceDeregisterWaiter(Timer,Waiter);
     
     {Destroy Waiter}
     TimerDeviceDestroyWaiter(Timer,Waiter);
     
     Waiter:=Timer.Waiters;
    end;
  end; 
 
 {Destroy Event}
 EventDestroy(Timer.Event);
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2711ARMTimerRead64(Timer:PTimerDevice):Int64;
{Implementation of TimerDeviceRead64 API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceRead64 instead}
begin
 {}
 Result:=0;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Read64');
 {$ENDIF}
 
 {Update Statistics}
 Inc(Timer.ReadCount);
 
 {Read Value}
 Result:=PBCM2838ARMTimerRegisters(Timer.Address).Value;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function BCM2711ARMTimerWait(Timer:PTimerDevice):LongWord;
{Implementation of TimerDeviceWait API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceWait instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Wait');
 {$ENDIF}
 
 {Check Existing (Wait not allowed with Repeating or Interrupt Event)}
 Result:=ERROR_IN_USE;
 if (Timer.Flags and (TIMER_EVENT_FLAG_REPEAT or TIMER_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
 
 {Check Lock}
 Result:=ERROR_OPERATION_FAILED;
 if (MutexOwner(Timer.Lock) <> ThreadGetCurrent) or (MutexCount(Timer.Lock) > 1) then Exit;
 
 {Check Event}
 if Timer.Event = INVALID_HANDLE_VALUE then
  begin
   {Create Event (Manual Reset)}
   Timer.Event:=EventCreate(True,False);
   
   Result:=ERROR_OPERATION_FAILED;
   if Timer.Event = INVALID_HANDLE_VALUE then Exit;
  end;
  
 {Update Statistics}
 Inc(Timer.WaitCount);
 
 {Increment Count}
 Inc(Timer.Count);
 
 {Release the Lock}
 MutexUnlock(Timer.Lock);
 
 {Wait for Event}
 if EventWait(Timer.Event) = ERROR_SUCCESS then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;  
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Timer.Lock) <> ERROR_SUCCESS then Exit;
   
   {Decrement Count}
   Dec(Timer.Count);
  
   {Return Result}
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function BCM2711ARMTimerEvent(Timer:PTimerDevice;Flags:LongWord;Callback:TTimerCallback;Data:Pointer):LongWord;
{Implementation of TimerDeviceEvent API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceEvent instead}
var
 Waiter:PTimerWaiter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Event (Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}
 
 {Check Flags (Interrupt not allowed without Repeat}
 if ((Flags and TIMER_EVENT_FLAG_INTERRUPT) <> 0) and ((Flags and TIMER_EVENT_FLAG_REPEAT) = 0) then Exit;
 
 {Check Existing (Only one Event allowed when Repeating or Interrupt}
 Result:=ERROR_IN_USE;
 if (Timer.Flags and (TIMER_EVENT_FLAG_REPEAT or TIMER_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
 
 {Create Waiter}
 Waiter:=TimerDeviceCreateWaiter(Timer,Callback,Data);
 if Waiter = nil then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Register Waiter}
 if TimerDeviceRegisterWaiter(Timer,Waiter) <> ERROR_SUCCESS then
  begin
   TimerDeviceDestroyWaiter(Timer,Waiter);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Update Statistics}
 Inc(Timer.EventCount);
 
 {Set the Flags}
 Timer.Flags:=Flags;
 
 {Increment Count}
 Inc(Timer.Count);
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711ARMTimerCancel(Timer:PTimerDevice):LongWord;
{Implementation of TimerDeviceCancel API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceCancel instead}
var
 Waiter:PTimerWaiter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Cancel');
 {$ENDIF}

 {Check Flags}
 if (Timer.Flags and TIMER_EVENT_FLAG_REPEAT) = 0 then
  begin
   Result:=ERROR_NOT_FOUND;
   Exit;
  end;
 
 {Get Waiter}
 Waiter:=Timer.Waiters;
 if Waiter <> nil then
  begin
   {Deregister Waiter}
   TimerDeviceDeregisterWaiter(Timer,Waiter);
 
   {Destroy Waiter}
   TimerDeviceDestroyWaiter(Timer,Waiter);
 
   {Decrement Count}
   Dec(Timer.Count);
   
   {Check Count}
   if Timer.Count = 0 then
    begin
     {Reset the Flags}
     Timer.Flags:=TIMER_EVENT_FLAG_NONE;
    end; 
  end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2711ARMTimerSetRate(Timer:PTimerDevice;Rate:LongWord):LongWord;
{Implementation of TimerDeviceSetRate API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceSetRate instead}
var
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Set Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Check Rate}
 if (Rate < Timer.Properties.MinRate) or (Rate > Timer.Properties.MaxRate) then Exit;
 
 {Get Divider}
 Divider:=(PBCM2711ARMTimer(Timer).CoreClock div Rate) - 1;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711:  Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Predivider}
 PBCM2838ARMTimerRegisters(Timer.Address).Predivider:=Divider;
 
 {Check Rate}
 if (PBCM2711ARMTimer(Timer).CoreClock mod Rate) <> 0 then
  begin
   {Update Properties}
   Timer.Rate:=PBCM2711ARMTimer(Timer).CoreClock div (Divider + 1);
  
   {Return Result}
   Result:=ERROR_NOT_EXACT;  
  end
 else
  begin
   {Update Properties}
   Timer.Rate:=Rate;
   
   {Return Result}
   Result:=ERROR_SUCCESS;  
  end;  
end;

{==============================================================================}

function BCM2711ARMTimerSetInterval(Timer:PTimerDevice;Interval:LongWord):LongWord;
{Implementation of TimerDeviceSetInterval API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceSetInterval instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Set Interval (Interval=' + IntToStr(Interval) + ')');
 {$ENDIF}
 
 {Check Interval}
 if (Interval < Timer.Properties.MinInterval) or (Interval > Timer.Properties.MaxInterval) then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Enabled}
 if (PBCM2838ARMTimerRegisters(Timer.Address).Control and BCM2838_ARM_TIMER_CONTROL_TIMER_ENABLED) = 0 then
  begin
   {Set Interval}
   PBCM2838ARMTimerRegisters(Timer.Address).Load:=Interval;
  end
 else
  begin 
   {Set Interval}
   PBCM2838ARMTimerRegisters(Timer.Address).Reload:=Interval;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 Timer.Interval:=Interval;
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

procedure BCM2711ARMTimerInterruptHandler(Timer:PTimerDevice);
{Interrupt handler for ARM Timer}
{Note: Not intended to be called directly by applications}
var
 Flags:LongWord;
 Waiter:PTimerWaiter;
begin
 {}
 {Check Timer}
 if Timer = nil then Exit;

 {Update Statistics}
 Inc(PBCM2711ARMTimer(Timer).InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Clear Interrupt}
 PBCM2838ARMTimerRegisters(Timer.Address).IRQClear:=1;
 
 {Get Flags}
 Flags:=Timer.Flags;
 
 {Check Flags}
 if ((Flags and TIMER_EVENT_FLAG_INTERRUPT) = 0) or ((Flags and TIMER_EVENT_FLAG_REPEAT) = 0) then
  begin
   {Send Event}
   if BCM2711ARM_TIMER_FIQ_ENABLED then
    begin
     WorkerScheduleFIQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2711ARMTimerEventTrigger),Timer,nil);
    end
   else
    begin
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2711ARMTimerEventTrigger),Timer,nil);
    end;
  end
 else
  begin
   {Call Waiter (Only for Repeating Interrupt events)}
   Waiter:=Timer.Waiters;
   if (Waiter <> nil) and Assigned(Waiter.Callback) then
    begin
     Waiter.Callback(Waiter.Data);
    end;
  end;    
end;

{==============================================================================}

procedure BCM2711ARMTimerEventTrigger(Timer:PTimerDevice);
{Event handler for ARM Timer}
{Note: Not intended to be called directly by applications}
var
 Count:LongWord;
 Flags:LongWord;
 Next:PTimerWaiter;
 Waiter:PTimerWaiter;
 Waiters:PTimerWaiter;
 Single:TTimerWaiter;
 Current:PTimerWaiter;
begin
 {}
 {Check Timer}
 if Timer = nil then Exit;

 {$IF DEFINED(BCM2711_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2711: ARM Timer Event Trigger');
 {$ENDIF}

 {Setup Count}
 Count:=0;

 {Setup Flags}
 Flags:=TIMER_EVENT_FLAG_NONE;
 
 {Setup Waiters}
 Waiters:=nil;

 {Setup Single}
 FillChar(Single,SizeOf(TTimerWaiter),0);

 {Acquire the Lock}
 if MutexLock(Timer.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Flags}
    Flags:=Timer.Flags;
 
    {Check Flags}
    if (Flags and TIMER_EVENT_FLAG_REPEAT) = 0 then
     begin
      {Signal Event}
      if Timer.Event <> INVALID_HANDLE_VALUE then
       begin
        EventPulse(Timer.Event);
       end;
 
      {Count Waiters}
      Waiter:=Timer.Waiters;
      while Waiter <> nil do
       begin
        Inc(Count);
        {Get Next}
        Waiter:=Waiter.Next;
       end;
 
      {Check Count}
      if Count > 0 then
       begin
        if Count = 1 then
         begin
          {Get Single}
          Waiter:=Timer.Waiters;
          if Waiter <> nil then
           begin
            Single.Callback:=Waiter.Callback;
            Single.Data:=Waiter.Data;
            
            {Save Next}
            Next:=Waiter.Next;
            
            {Deregister Waiter}
            TimerDeviceDeregisterWaiter(Timer,Waiter);
            
            {Destroy Waiter}
            TimerDeviceDestroyWaiter(Timer,Waiter);
            
            {Get Next}
            Waiter:=Next;
           end;
         end
        else
         begin        
          {Allocate Waiters}
          Waiters:=GetMem(Count * SizeOf(TTimerWaiter));
          Current:=Waiters;
          
          {Get Waiters}
          Waiter:=Timer.Waiters;
          while Waiter <> nil do
           begin
            Current.Callback:=Waiter.Callback;
            Current.Data:=Waiter.Data;
            Current.Next:=nil;
            if Waiter.Next <> nil then
             begin
              Current.Next:=PTimerWaiter(PtrUInt(Current) + SizeOf(TTimerWaiter));
              Current:=Current.Next;
             end;
            
            {Save Next}
            Next:=Waiter.Next;
            
            {Deregister Waiter}
            TimerDeviceDeregisterWaiter(Timer,Waiter);
            
            {Destroy Waiter}
            TimerDeviceDestroyWaiter(Timer,Waiter);
             
            {Get Next}
            Waiter:=Next;
           end;
         end;  
       end;
    
      {Reset Flags}
      Timer.Flags:=TIMER_EVENT_FLAG_NONE;
      
      {Reset Count}
      Timer.Count:=0;
     end
    else
     begin    
      {Get Single}
      Waiter:=Timer.Waiters;
      if Waiter <> nil then
       begin
        Single.Callback:=Waiter.Callback;
        Single.Data:=Waiter.Data;
       end; 
     end; 
   finally
    {Release the Lock}
    MutexUnlock(Timer.Lock);
   end; 
  end; 
  
 {Check Flags}  
 if (Flags and TIMER_EVENT_FLAG_REPEAT) = 0 then
  begin
   if Count > 0 then
    begin
     if Count = 1 then
      begin
       {Call Waiter}
       if Assigned(Single.Callback) then
        begin
         Single.Callback(Single.Data);
        end;
      end
     else
      begin  
       {Get Waiters}
       Waiter:=Waiters;
       while Waiter <> nil do
        begin
         {Call Waiter}
         if Assigned(Waiter.Callback) then
          begin
           Waiter.Callback(Waiter.Data);
          end;
         {Get Next} 
         Waiter:=Waiter.Next;
        end;
       
       {Free Waiters}
       FreeMem(Waiters);
      end; 
    end; 
  end
 else
  begin
   {Call Event}
   if Assigned(Single.Callback) then
    begin
     Single.Callback(Single.Data);
    end;
  end;  
end;

{==============================================================================}
{==============================================================================}
{BCM2711 Local Timer Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2711 Random Functions}
function BCM2711RandomStart(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;
 
 if MutexLock(Random.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
  
    {Check State}
    if (PBCM2838RNGRegisters(Random.Address).Control and BCM2838_RNG_CTRL_RNG_RBGEN_MASK) = 0 then
     begin
      {Initial numbers generated are "less random" so will be discarded}
      PBCM2838RNGRegisters(Random.Address).TBCThreshold:=BCM2711_RANDOM_WARMUP_COUNT;
    
      {Min FIFO count to generate full interrupt}
      PBCM2838RNGRegisters(Random.Address).FIFOCount:=2 shl BCM2838_RNG_FIFO_COUNT_RNG_FIFO_THRESHOLD_SHIFT;
    
      {Enable RNG (1Mhz sample rate)}
      PBCM2838RNGRegisters(Random.Address).Control:=(3 shl BCM2838_RNG_CTRL_RNG_DIV_CTRL_SHIFT) or BCM2838_RNG_CTRL_RNG_RBGEN_MASK
     end;
     
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
     
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Random.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2711RandomStop(Random:PRandomDevice):LongWord;
var
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;

 if MutexLock(Random.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Disable RNG}
    Value:=PBCM2838RNGRegisters(Random.Address).Control;
    Value:=Value and not(BCM2838_RNG_CTRL_RNG_RBGEN_MASK);
    Value:=Value or BCM2838_RNG_CTRL_RNG_RBGEN_DISABLE;
    PBCM2838RNGRegisters(Random.Address).Control:=Value;
   
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Random.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
 
{==============================================================================}

function BCM2711RandomReadLongWord(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;
 
 if MutexLock(Random.Lock) <> ERROR_SUCCESS then Exit;
 
 {Ensure warm up period has elapsed}
 while (PBCM2838RNGRegisters(Random.Address).TotalBitCount) <= 16 do
  begin
   ThreadSleep(0);
  end;
  
 {Ensure FIFO is not empty}
 while (PBCM2838RNGRegisters(Random.Address).FIFOCount and BCM2838_RNG_FIFO_COUNT_RNG_FIFO_COUNT_MASK) = 0 do
  begin
   ThreadSleep(0);
  end;
 
 {Read Data}
 Result:=PBCM2838RNGRegisters(Random.Address).FIFOData; 

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Statistics}
 Inc(Random.ReadCount);
 
 MutexUnlock(Random.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2711 Mailbox Functions}

{==============================================================================}
{==============================================================================}
{BCM2711 Watchdog Functions}
function BCM2711WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
var
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;
 
 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Timeout}
    Result:=ERROR_NOT_SUPPORTED;
    if Watchdog.Timeout = 0 then Exit;
 
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
 
    {Enable Watchdog}
    PBCM2838PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2838_PM_PASSWORD or ((Watchdog.Timeout * BCM2838_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2838_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2838PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2838PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2838_PM_PASSWORD or (Current and BCM2838_PM_RSTC_WRCFG_CLR) or BCM2838_PM_RSTC_WRCFG_FULL_RESET;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    {Update Statistics}
    Inc(Watchdog.StartCount);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2711WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;
 
 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
 
    {Disable Watchdog}
    PBCM2838PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2838_PM_PASSWORD or BCM2838_PM_RSTC_RESET;
    
    {Update Statistics}
    Inc(Watchdog.StopCount);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2711WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;
var
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;

 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Timeout}
    Result:=ERROR_NOT_SUPPORTED;
    if Watchdog.Timeout = 0 then Exit;

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Refresh Watchdog}
    PBCM2838PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2838_PM_PASSWORD or ((Watchdog.Timeout * BCM2838_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2838_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2838PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2838PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2838_PM_PASSWORD or (Current and BCM2838_PM_RSTC_WRCFG_CLR) or BCM2838_PM_RSTC_WRCFG_FULL_RESET;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
 
    {Update Statistics}
    Inc(Watchdog.RefreshCount);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2711WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;

 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Remain}
    Result:=(PBCM2838PMWatchdogRegisters(Watchdog.Address).WDOG and BCM2838_PM_WDOG_TIME_MASK) div BCM2838_PM_WDOG_TICKS_PER_MILLISECOND;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2711 Framebuffer Functions}
function BCM2711FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceAllocate API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceAllocate instead}
var
 Size:LongWord;
 Count:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Defaults:TFramebufferProperties;
 Palette:array[0..255] of LongWord;
 Tag:PBCM2838MailboxTagCreateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Properties}
    if Properties = nil then
     begin
      {Use Defaults}
      Defaults.Depth:=FRAMEBUFFER_DEFAULT_DEPTH;
      Defaults.Order:=FRAMEBUFFER_DEFAULT_ORDER;
      Defaults.Mode:=FRAMEBUFFER_DEFAULT_MODE;
      Defaults.PhysicalWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.PhysicalHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.VirtualWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.VirtualHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.OffsetX:=FRAMEBUFFER_DEFAULT_OFFSET_X;
      Defaults.OffsetY:=FRAMEBUFFER_DEFAULT_OFFSET_Y;
      Defaults.OverscanTop:=FRAMEBUFFER_DEFAULT_OVERSCAN_TOP;
      Defaults.OverscanBottom:=FRAMEBUFFER_DEFAULT_OVERSCAN_BOTTOM;
      Defaults.OverscanLeft:=FRAMEBUFFER_DEFAULT_OVERSCAN_LEFT;
      Defaults.OverscanRight:=FRAMEBUFFER_DEFAULT_OVERSCAN_RIGHT;
     end
    else
     begin
      {Use Properties}
      Defaults.Depth:=Properties.Depth;
      Defaults.Order:=Properties.Order;
      Defaults.Mode:=Properties.Mode;
      Defaults.PhysicalWidth:=Properties.PhysicalWidth;
      Defaults.PhysicalHeight:=Properties.PhysicalHeight;
      Defaults.VirtualWidth:=Properties.VirtualWidth;
      Defaults.VirtualHeight:=Properties.VirtualHeight;
      Defaults.OffsetX:=Properties.OffsetX;
      Defaults.OffsetY:=Properties.OffsetY;
      Defaults.OverscanTop:=Properties.OverscanTop;
      Defaults.OverscanBottom:=Properties.OverscanBottom;
      Defaults.OverscanLeft:=Properties.OverscanLeft;
      Defaults.OverscanRight:=Properties.OverscanRight;
     end;   

    {Check Defaults}
    if (Defaults.PhysicalWidth = 0) or (Defaults.PhysicalHeight = 0) then
     begin
      {Get Dimensions Width and Height}
      Result:=FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
      if Result <> ERROR_SUCCESS then
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: FramebufferAllocate - FramebufferGetDimensions failed: ' + ErrorToString(Result));
        {Exit;} {Do not fail}
        
        {Set Defaults}
        Defaults.PhysicalWidth:=640;
        Defaults.PhysicalHeight:=480;
       end;
      
      {Set Defaults}
      Defaults.VirtualWidth:=Defaults.PhysicalWidth;
      Defaults.VirtualHeight:=Defaults.PhysicalHeight;
     end;
    
    {Check Virtual Width}
    if Defaults.VirtualWidth < Defaults.PhysicalWidth then
     begin
      Defaults.VirtualWidth:=Defaults.PhysicalWidth;
     end;

    {Check Virtual Height}
    if Defaults.VirtualHeight < Defaults.PhysicalHeight then
     begin
      Defaults.VirtualHeight:=Defaults.PhysicalHeight;
     end;
     
    {Check Offset X} 
    if (Defaults.OffsetX > 0) and (Defaults.OffsetX > ((Defaults.VirtualWidth - Defaults.PhysicalWidth) - 1)) then
     begin
      Defaults.OffsetX:=0;
     end;

    {Check Offset Y} 
    if (Defaults.OffsetY > 0) and (Defaults.OffsetY > ((Defaults.VirtualHeight - Defaults.PhysicalHeight) - 1)) then
     begin
      Defaults.OffsetY:=0;
     end;
    
    {Calculate Size}
    Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagCreateBuffer) + SizeOf(TBCM2838MailboxFooter);
    
    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);
    
     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2838_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2838MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
     
     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2838_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2838MailboxTagSetPhysical) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;
     
     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2838_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2838MailboxTagSetVirtual) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2838_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2838MailboxTagSetDepth) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;
     
     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2838_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2838MailboxTagSetPixelOrder) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;
     
     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2838_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2838MailboxTagSetAlphaMode) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;
     
     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2838_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2838MailboxTagSetVirtualOffset) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;
     
     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2838_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2838MailboxTagSetOverscan) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;
     
     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2838_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2838MailboxTagAllocateBuffer) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2711FRAMEBUFFER_ALIGNMENT;
     
     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2838_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2838MailboxTagGetPitch) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);
     
     {Setup Footer}
     Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2838_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if PLATFORM_LOG_ENABLED then PlatformLogError('BCM2711: FramebufferAllocate - MailboxPropertyCall failed: ' + ErrorToString(Result));
       Exit;
      end; 
     
     {Update Framebuffer}
     Framebuffer.Address:=BusAddressToPhysical(Pointer(Tag.Allocate.Response.Address)); {Firmware may return address as a Bus address, writes must be to the Physical address}
     Framebuffer.Size:=Tag.Allocate.Response.Size;
     Framebuffer.Pitch:=Tag.Pitch.Response.Pitch;
     Framebuffer.Depth:=Tag.Depth.Response.Depth;
     Framebuffer.Order:=Tag.Order.Response.Order;
     Framebuffer.Mode:=Tag.Mode.Response.Mode;
     Framebuffer.PhysicalWidth:=Tag.Physical.Response.Width;
     Framebuffer.PhysicalHeight:=Tag.Physical.Response.Height;
     Framebuffer.VirtualWidth:=Tag.Vertual.Response.Width;
     Framebuffer.VirtualHeight:=Tag.Vertual.Response.Height;
     Framebuffer.OffsetX:=Tag.Offset.Response.X;
     Framebuffer.OffsetY:=Tag.Offset.Response.Y;
     Framebuffer.OverscanTop:=Tag.Overscan.Response.Top;
     Framebuffer.OverscanBottom:=Tag.Overscan.Response.Bottom;
     Framebuffer.OverscanLeft:=Tag.Overscan.Response.Left;
     Framebuffer.OverscanRight:=Tag.Overscan.Response.Right;
    
     {Check Depth}
     if Framebuffer.Depth = FRAMEBUFFER_DEPTH_8 then
      begin
       {Create Palette (Grayscale only)}
       FillChar(Palette,SizeOf(Palette),0);
       for Count:=0 to 255 do 
        begin
         Palette[Count]:=LongWord($FF000000 or ((Count and $FF) shl 16) or ((Count and $FF) shl 8) or (Count and $FF));
        end;
       
       {Set Palette}
       FramebufferSetPalette(0,256,@Palette,SizeOf(Palette));
      end;
    
     {Get Order}
     if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then
      begin
       Framebuffer.Order:=FRAMEBUFFER_ORDER_BGR;
      end
     else
      begin
       Framebuffer.Order:=FRAMEBUFFER_ORDER_RGB;
      end;      
      
     {Get Format}
     case Framebuffer.Depth of
      FRAMEBUFFER_DEPTH_8:begin
        {Order not relevant for indexed}
        Framebuffer.Format:=COLOR_FORMAT_INDEX8;
       end;
      FRAMEBUFFER_DEPTH_16:begin
        if Framebuffer.Order = FRAMEBUFFER_ORDER_RGB then
         begin
          Framebuffer.Format:=COLOR_FORMAT_RGB16;
         end
        else
         begin
          Framebuffer.Format:=COLOR_FORMAT_BGR16;
         end;
       end;
      FRAMEBUFFER_DEPTH_24:begin
        if Framebuffer.Order = FRAMEBUFFER_ORDER_RGB then
         begin
          Framebuffer.Format:=COLOR_FORMAT_RGB24;
         end
        else
         begin
          Framebuffer.Format:=COLOR_FORMAT_BGR24;
         end;
       end;
      FRAMEBUFFER_DEPTH_32:begin
        if Framebuffer.Order = FRAMEBUFFER_ORDER_RGB then
         begin
          Framebuffer.Format:=COLOR_FORMAT_ARGB32;
         end
        else
         begin
          Framebuffer.Format:=COLOR_FORMAT_ABGR32;
         end;
       end;
     end;  
     
     {Get Rotation}
     Framebuffer.Rotation:=FRAMEBUFFER_ROTATION_0;
    
     {Update Statistics}
     Inc(Framebuffer.AllocateCount);
    
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(Header);
    end;
    
   finally
    {Set Default Display}
    if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
    
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
   
{==============================================================================}

function BCM2711FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceRelease API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceRelease instead}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2838MailboxHeader;
 Footer:PBCM2838MailboxFooter;
 Tag:PBCM2838MailboxTagReleaseBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Calculate Size}
    Size:=SizeOf(TBCM2838MailboxHeader) + SizeOf(TBCM2838MailboxTagReleaseBuffer) + SizeOf(TBCM2838MailboxFooter);

    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);
    
     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2838_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2838MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2838MailboxHeader)));
     Tag.Header.Tag:=BCM2838_MBOX_TAG_RELEASE_BUFFER;
     Tag.Header.Size:=SizeOf(TBCM2838MailboxTagReleaseBuffer) - SizeOf(TBCM2838MailboxTagHeader);
     Tag.Header.Length:=SizeOf(Tag.Request);
    
     {Setup Footer}
     Footer:=PBCM2838MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2838MailboxTagReleaseBuffer)));
     Footer.Tag:=BCM2838_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2838_MAILBOX_0,BCM2838_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2711: FramebufferRelease: MailboxPropertyCall failed: ' + ErrorToString(Result));
       {Exit;} {Do not fail}
      end; 
     
     {Update Framebuffer}
     Framebuffer.Address:=0;
     Framebuffer.Size:=0;
     Framebuffer.Pitch:=0;
     Framebuffer.Depth:=FRAMEBUFFER_DEPTH_32;
     Framebuffer.Order:=FRAMEBUFFER_ORDER_RGB;
     Framebuffer.Mode:=FRAMEBUFFER_MODE_ENABLED;
     Framebuffer.Format:=COLOR_FORMAT_DEFAULT;
     Framebuffer.PhysicalWidth:=0;
     Framebuffer.PhysicalHeight:=0;
     Framebuffer.VirtualWidth:=0;
     Framebuffer.VirtualHeight:=0;
     Framebuffer.OffsetX:=0;
     Framebuffer.OffsetY:=0;
     Framebuffer.OverscanTop:=0;
     Framebuffer.OverscanBottom:=0;
     Framebuffer.OverscanLeft:=0;
     Framebuffer.OverscanRight:=0;
     Framebuffer.Rotation:=FRAMEBUFFER_ROTATION_0;
     
     {Update Statistics}
     Inc(Framebuffer.ReleaseCount);
     
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(Header);
    end;
   finally
    {Set Default Display}
    if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2711FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDeviceBlank API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceBlank instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Set Current Display}
 if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Check Blank}
  if Blank then
   begin
    Result:=FramebufferSetState(0);
   end
  else
   begin
    Result:=FramebufferSetState(1);
   end;
 finally
  {Set Default Display}
  if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end; 
end;

{==============================================================================}

function BCM2711FramebufferCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceCommit API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceCommit instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Flags}
 if (not(BCM2711DMA_CACHE_COHERENT) or ((Flags and FRAMEBUFFER_TRANSFER_DMA) = 0)) and BCM2711FRAMEBUFFER_CACHED then
  begin
   {Clean Cache}
   CleanAndInvalidateDataCacheRange(Address,Size);
  end;
 
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2711FramebufferWaitSync(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceWaitSync API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceWaitSync instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Set Current Display}
 if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
  end;
 try
  {Wait Sync}
  Result:=FramebufferSetVSync;
 finally
  {Set Default Display}
  if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end; 
end;
 
{==============================================================================}

function BCM2711FramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;
{Implementation of FramebufferDeviceSetOffset API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetOffset instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Offset}
    if X > (Framebuffer.VirtualWidth - Framebuffer.PhysicalWidth) then Exit;
    if Y > (Framebuffer.VirtualHeight - Framebuffer.PhysicalHeight) then Exit;
    
    {Set Offset}
    Result:=FramebufferSetOffset(X,Y);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Update Offset}
    if not(Pan) then
     begin
      Framebuffer.OffsetX:=X;
      Framebuffer.OffsetY:=Y;
     end; 
   finally
    {Set Default Display}
    if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2711FramebufferGetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
{Implementation of FramebufferDeviceGetPalette API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceGetPalette instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Palette}
    if Palette = nil then Exit;
    
    {Get Palette}
    Result:=FramebufferGetPalette(@Palette.Entries,SizeOf(Palette.Entries));
    if Result <> ERROR_SUCCESS then Exit;
    
    {Update Palette}
    Palette.Start:=0;
    Palette.Count:=256;
   finally
    {Set Default Display}
    if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
    
{==============================================================================}

function BCM2711FramebufferSetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
{Implementation of FramebufferDeviceSetPalette API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetPalette instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Palette}
    if Palette = nil then Exit;
    if Palette.Start > 255 then Exit;
    if Palette.Count > 256 then Exit;
    
    {Set Palette}
    Result:=FramebufferSetPalette(Palette.Start,Palette.Count,@Palette.Entries,SizeOf(Palette.Entries));
   finally
    {Set Default Display}
    if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2711FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{Implementation of FramebufferDeviceSetBacklight API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetBacklight instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Set Current Display}
 if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
  begin
   FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
  end;
 try 
  {Set Backlight}
  Result:=FramebufferSetBacklight(Brightness);
 finally
  {Set Default Display}
  if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
   begin
    FramebufferSetDisplayNum(0);
   end;
 end; 
end; 

{==============================================================================}
 
function BCM2711FramebufferSetCursor(Framebuffer:PFramebufferDevice;Width,Height,HotspotX,HotspotY:LongWord;Image:Pointer;Len:LongWord):LongWord;
{Implementation of FramebufferDeviceSetCursor API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetCursor instead}
var
 Cursor:Pointer;
 Address:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Image}
    if Image = nil then
     begin
      {Set Properties}
      Framebuffer.CursorWidth:=CURSOR_ARROW_DEFAULT_WIDTH;
      Framebuffer.CursorHeight:=CURSOR_ARROW_DEFAULT_HEIGHT;
      Framebuffer.CursorHotspotX:=0;
      Framebuffer.CursorHotspotY:=0;
      
      {Set Default}
      Result:=CursorSetDefault;
     end
    else
     begin
      {Check Width and Height}
      if (Width = 0) or (Height = 0) then Exit;
    
      {Check Len}
      if Len < (Width * Height * ColorFormatToBytes(COLOR_FORMAT_DEFAULT)) then Exit;
      
      {Set Properties}
      Framebuffer.CursorWidth:=Width;
      Framebuffer.CursorHeight:=Height;
      Framebuffer.CursorHotspotX:=HotspotX;
      Framebuffer.CursorHotspotY:=HotspotY;
      
      {Allocate the Cursor (No Cache)}
      Cursor:=AllocNoCacheMem(Len);
      if Cursor = nil then Exit;
      
      {Copy the Cursor}
      System.Move(Image^,Cursor^,Len);
      
      {Convert to Physical Address}
      Address:=PhysicalToBusAddress(Cursor);
      
      {Set Cursor}
      Result:=CursorSetInfo(Width,Height,HotspotX,HotspotY,Pointer(Address),Len);
      
      {Free the Cursor}
      FreeMem(Cursor);
     end; 
   finally
    {Set Default Display}
    if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end; 

{==============================================================================}

function BCM2711FramebufferUpdateCursor(Framebuffer:PFramebufferDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;
{Implementation of FramebufferDeviceUpdateCursor API for BCM2711 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceUpdateCursor instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   {Set Current Display}
   if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
    begin
     FramebufferSetDisplayNum(PBCM2711Framebuffer(Framebuffer).DisplayNum);
    end;
   try
    {Check Properties}
    (*if Enabled and ((Framebuffer.CursorWidth = 0) or (Framebuffer.CursorHeight = 0)) then
     begin
      {Set Properties}
      Framebuffer.CursorWidth:=CURSOR_ARROW_DEFAULT_WIDTH;
      Framebuffer.CursorHeight:=CURSOR_ARROW_DEFAULT_HEIGHT;
      Framebuffer.CursorHotspotX:=0;
      Framebuffer.CursorHotspotY:=0;
      
      {Set Default}
      Result:=CursorSetDefault;
     end;*)
    
    {Set Properties}
    if Enabled then Framebuffer.CursorState:=FRAMEBUFFER_CURSOR_ENABLED else Framebuffer.CursorState:=FRAMEBUFFER_CURSOR_DISABLED;
    if not Relative then
     begin
      Framebuffer.CursorX:=X;
      Framebuffer.CursorY:=Y;
     end
    else
     begin
      Framebuffer.CursorX:=Framebuffer.CursorX + X;
      Framebuffer.CursorY:=Framebuffer.CursorY + Y;
     end;
    
    {Update Cursor}
    Result:=CursorSetState(Enabled,X,Y,Relative);
   finally
    {Set Default Display}
    if PBCM2711Framebuffer(Framebuffer).MultiDisplay then
     begin
      FramebufferSetDisplayNum(0);
     end;
     
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2711 Helper Functions}
function BCM2711SPIGetDescription(Id:LongWord):String;
{Get the device description of an SPI device}
{Id: The Id number of the SPI device (0 to 6)}
{Return: The correct device description suitable for passing to SPIDeviceFindByDescription}

{Note: The Id number supplied to this function may differ from the Ultibo device id value}
begin
  {}
  case Id of
   0,3,4,5,6:Result:=BCM2711_SPI0_DESCRIPTION + IntToStr(Id);
   1:Result:=BCM2711_SPI1_DESCRIPTION;
   2:Result:=BCM2711_SPI2_DESCRIPTION;
  else
   Result:='';
  end;  
end;

{==============================================================================}

function BCM2711I2CGetDescription(Id:LongWord):String;
{Get the device description of an I2C device}
{Id: The Id number of the I2C device (0 to 7)}
{Return: The correct device description suitable for passing to I2CDeviceFindByDescription}

{Note: The Id number supplied to this function may differ from the Ultibo device id value}
begin
  {}
  case Id of
   0,1,2,3,4,5,6,7:Result:=BCM2711_I2C0_DESCRIPTION + IntToStr(Id);
  else
   Result:='';
  end;  
end;

{==============================================================================}

function BCM2711I2CSlaveGetDescription(Id:LongWord):String;
{Get the device description of an I2C slave device}
{Id: The Id number of the I2C slave device (Always 0)}
{Return: The correct device description suitable for passing to I2CSlaveFindByDescription}

{Note: The Id number supplied to this function may differ from the Ultibo device id value}
begin
  {}
  case Id of
   0:Result:=BCM2711_I2CSLAVE_DESCRIPTION;
  else
   Result:='';
  end;  
end;

{==============================================================================}

function BCM2711PWMGetDescription(Id,Channel:LongWord):String;
{Get the device description of a PWM device}
{Id: The Id number of the PWM device (0 or 1)}
{Channel: The channel number of the PWM device (0 or 1)}
{Return: The correct device description suitable for passing to PWMDeviceFindByDescription}

{Note: The Id number supplied to this function may differ from the Ultibo device id value}
begin
  {}
  case Id of
   0,1:begin
     case Channel of
      0,1:Result:=BCM2711_PWM0_DESCRIPTION + IntToStr(Id) + '_' + IntToStr(Channel);
     else
      Result:='';
     end; 
    end; 
  else
   Result:='';
  end;  
end;

{==============================================================================}

function BCM2711UARTGetDescription(Id:LongWord):String;
{Get the device description of a UART device}
{Id: The Id number of the UART device (0 to 5)}
{Return: The correct device description suitable for passing to UARTDeviceFindByDescription}

{Note: The Id number supplied to this function may differ from the Ultibo device id value}
begin
  {}
  case Id of
   0,2,3,4,5:Result:=BCM2711_UART0_DESCRIPTION + IntToStr(Id);
   1:Result:=BCM2711_UART1_DESCRIPTION;
  else
   Result:='';
  end;  
end;

{==============================================================================}
{==============================================================================}

initialization
 BCM2711Init;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 


