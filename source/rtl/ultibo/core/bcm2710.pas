{
Ultibo BCM2710 interface unit.

Copyright (C) 2018 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

   Linux MMC/SDHCI drivers
   U-Boot MMC/SDHCI drivers
 
   Linux - \drivers\video\bcm2708_fb.c
   U-Boot - \drivers\video\bcm2835.c 
 
   Linux - \drivers\dma\bcm2708-dmaengine.c - Copyright 2013-2014 Florian Meier and Gellert Weisz
   Linux - \drivers\dma\bcm2835-dma.c - Copyright 2013 Florian Meier
 
   Linux - \drivers\tty\serial\amba-pl011.c - Copyright (C) 2010 ST-Ericsson SA and others
   
   Linux - \drivers\i2c\busses\i2c-bcm2708.c - Copyright (C) 2012 Chris Boot & Frank Buss
   Linux - \drivers\i2c\busses\i2c-bcm2835.c
   
   Linux - \drivers\spi\spi-bcm2835.c - Copyright (C) 2015 Martin Sperl and others
   Linux - \drivers\spi\spi-bcm2835aux.c - Copyright (C) 2015 Martin Sperl
 
   Linux - \drivers\pwm\pwm-bcm2835.c - Copyright (C) 2014 Bart Tanghe
 
   Linux - \drivers\clk\bcm\clk-bcm2835.c - Copyright (C) 2010,2015 Broadcom
   Linux - \drivers\watchdog\bcm2835_wdt.c - Copyright (C) 2013 Lubomir Rintel
 
   Linux - \drivers\clocksource\timer-sp804.c - Copyright (C) 1999 - 2003 ARM Limited
 
References
==========

 BCM2835 ARM Peripherals

 Raspberry Pi Mailboxes
 
  https://github.com/raspberrypi/firmware/wiki/Mailboxes
 
 RPi Low-level peripherals
  
  http://elinux.org/RPi_Low-level_peripherals
  
 RPi SPI
 
  http://elinux.org/RPi_SPI
  https://www.raspberrypi.org/documentation/hardware/raspberrypi/spi/README.md
 
 I2C/SPI Slave
 
  https://github.com/hendric-git/bsc-slave  
  https://github.com/rLoopTeam/bsc-slave
 
BCM2710 Devices
===============
 
 This unit provides the BCM2710 specific implementations of the following devices:

  SPI0
  I2C0
  I2C1
  SPI1
  SPI2
  I2C Slave
  SPI Slave
  DMA
  PWM0
  PWM1
  PCM
  GPIO
  UART0
  UART1
  SDHCI (eMMC)
 
  Clock (System Timer)
  Clock (ARM Timer)
  Clock (Local Timer)
  ARM Timer
  Local Timer
  Random
  Mailbox
  Watchdog
  Framebuffer
  
  And MIPI CSI-2 (Camera Serial Interface) ?
  And DSI (Display Serial Interface) ?
 
BCM2710 SPI0 Device
===================

 The BCM2710 has a single master mode SPI controller that supports 3 wire, 2 wire and LoSSI modes of operation. It also has
 2 auxiliary SPI masters which do not support DMA mode (see SPI1/2 below).
 
 The main SPI0 controller supports polled, interrupt and DMA modes and includes 3 chip selects although only CS0 and 1 are
 available on the 26 or 40 pin header. 
 
 By default SPI0 can appear on GPIO pins 7 to 11 (CS1, CS0, MISO, MOSI, SCLK) using alternate function 0 or on GPIO pins 35
 to 39 (CS1, CS0, MISO, MOSI, SCLK) using alternate function 0, only pins 7 to 11 are available on the header.

BCM2710 I2C0/1 Device
=====================

 The BCM2710 has 3 Broadcom Serial Controller (BSC) devices which are fast mode (400Kz) masters numbered BSC0, BSC1 and BSC2.
 
 Device BSC2 is dedicated to the HDMI interface and is not availale for use by the ARM processor. All BSC devices contain a
 16 byte FIFO, support 7 bit and 10 bit addressing and have software configurable clock timing.
 
 By default BSC0 can appear on GPIO pins 0 and 1 (Alternate function 0), 28 and 29 (Alternate function 0) or 44 and 45 (Alternate
 function 1). Unfortunately on all except the Revision 1 models none of these pins are available on the 26 or 40 pin header.
 
 Note: On the Raspberry Pi A+/B+/Zero/2B/3B the ID EEPROM pins on the 40 pin header are actually connected to GPIO 0 and 1 (BSC0)
 
 Device BSC1 can appear on GPIO pins 2 and 3 (Alternate function 0) or 44 and 45 (Alternate function 2) but only pins 2 and 3 are
 exposed on the 26 or 40 pin header.
 
BCM2710 SPI1/2 Device
=====================

 The BCM2710 has 2 additional SPI universal masters available as part of the AUX device which support interrupt mode but not
 DMA and therefore only allow limited bandwidth transfers due to the CPU overhead required to sustain high data rates.
 
 Both devices support 3 chip selects, by default SPI1 is available on GPIO pins 16 to 21 (CS2, CS1, CS0, MISO, MOSI, SCLK)
 using alternate function 4 and SPI2 is available on GPIO pins 40 to 45 (MISO, MOSI, SCLK, CS0, CS1, CS2) using alternate
 function 4. Only pins 16 to 21 are available on the header and only on the 40 pin header of the Raspberry Pi A+/B+/Zero/2B/3B.

BCM2710 SPI/I2C Slave Device
============================

 
BCM2710 DMA Device
==================

 The DMA controller has 16 channels in total although not all are available for software to use as some are already used by the GPU.
 
 The firmware will pass the value dma.dmachans on the command line which will indicate which channels are available for our use.
 
 Channels 0 to 6 are normal channels which support 2D stride and transfers up to 1GB per control block
 
 Channels 7 to 14 are Lite channels which do not support stride and only allow transfers up to 64KB per control block

 Channel 15 is not mentioned in most documentation and is shown as not available in the mask passed in dma.dmachans
 
 Channel 0 and 15 are Bulk channels which have an additional FIFO for faster transfers (8 beat burst per read)


BCM2710 PWM0/1 Device
=====================

 The BCM2710 has a single PWM controller with 2 independent output bit streams with multiple algorithms for generating the output
 pulse. The PWM controller supports either a single data register (independent per channel) or a 16 x 32 FIFO which also supports
 DMA mode transmission.
 
 On the Raspberry Pi PWM0 and PWM1 are also connected via GPIO pins 40 and 45 (40 and 41 on the Raspberry Pi 3B) to the audio circuit
 and allow playback of digital audio signals via the 3 or 4 pole line jack (depending on model).

 PWM0 is available on GPIO pins 12 (function 0), 18 (function 5), 40 (function 0) and 52 (function 1).
 PWM1 is available on GPIO pins 13 (function 0), 19 (function 5), 41, 45 (function 0) and 53 (function 1).
 
 On the Raspberry Pi A and B only pin 18 is exposed on the 26 pin header.
 On the Raspberry Pi A+/B+/Zero/2B/3B pins 12, 18 and 19 are exposed on the 40 pin header.


BCM2710 PCM Device
==================

 
BCM2710 GPIO Device
===================

 The GPIO has 54 pins available each with multiple alternate functions. All pins can be configured as input or output
 and all can have pull up or down applied.
 
 Not all pins are exposed on the 26 or 40 pin header of the Raspberry Pi, for details of which pins are available see:
 
  Raspberry Pi A and B - https://www.raspberrypi.org/documentation/usage/gpio/README.md
  Raspberry Pi A+/B+/2B/3B/Zero - https://www.raspberrypi.org/documentation/usage/gpio-plus-and-raspi2/README.md
 
 Some of the 54 pins are used for peripheral communication (such as the SD card) and are not available for general use,
 take care when changing function selects on pins to avoid disabling certain system peripherals.
 
 Event detection can be enabled for both high and low levels as well as rising and falling edges, there is also an
 asynchronous rising or falling edge detection which can detect edges of very short duration.

 
BCM2710 UART0 Device
====================

 The UART0 device is an ARM PL011 UART which supports programmable baud rates, start, stop and parity bits and hardware
 flow control and many others. The UART0 is similar to the industry standard 16C650 but with a number of differences, the
 PL011 has a some optional features such as IrDA, Serial InfraRed and DMA which are not supported by the Broadcom implementation.

 In the standard configuration the UART0 TX and RX lines are connected to GPIO pins 14 and 15 respectively (Alternate function
 0) but they can be remapped via GPIO function selects to a number of other locations. On the Raspberry Pi (all models) none of
 these alternate pin mappings are exposed via the 26 or 40 pin header and therefore cannot be used easily. This means that UART0
 and UART1 cannot be used at the same time.
 
 On the Raspberry Pi 3B the UART0 can be mapped to GPIO pins 32 and 33 (Alternate function 3) to communicate with the built in
 Bluetooth module.

 
BCM2710 UART1 Device
====================

 The UART1 device is a Broadcom implementation that is part of the AUX device which also includes the SPI1 and SPI2 devices.
 This device is termed a Mini UART and has a smaller feature set than the PL011 UART but still supports a fairly standard
 communication protocol with programmable baud rate and hardware flow control.
 
 The Mini UART is similar to the standard 16550 device but is missing some of the features, the device also has no DMA support
 so high speed transfers will produce a higher CPU load.

 In the standard configuration the UART1 TX and RX lines are connected to GPIO pins 14 and 15 respectively (Alternate function
 5) but they can be remapped via GPIO function selects to a number of other locations. On the Raspberry Pi (all models) none of
 these alternate pin mappings are exposed via the 26 or 40 pin header and therefore cannot be used easily. This means that UART0
 and UART1 cannot be used at the same time.
 
 On the Raspberry Pi 3B the UART1 can be mapped to GPIO pins 32 and 33 (Alternate function 5) to communicate with the built in
 Bluetooth module.

 
BCM2710 SDHCI Device
====================

 The SDHCI controller on the BCM2710 is an Arasan SD Host controller.

 The Card Detect pin is not connected.

 The Write Protect pin is not connected.


BCM2710 Clock (System Timer) Device
===================================

 The clock device in the BCM2710 is based on the System Timer which is a 64 bit free running counter that runs at 1MHz regardless
 of core or CPU clock speeds. The System Timer cannot be stopped and the counter cannot be set or reset.
 
 The System Timer includes 4 compare registers which can each generate an interrupt when the compare value is matched, however 2
 of the 4 are consumed by the GPU and on the Raspberry Pi A/B/A+/B+/Zero the other 2 are used for the scheduler and clock interrupts
 in Ultibo. 
 
 This device simply exposes the free running counter as a clock value and does not provide access to the timer compare functionality
 or to interrupt based events, for those see the timer devices below.

 
BCM2710 Clock (ARM Timer) Device
================================

 This device represents that free running counter from the ARM Timer device (below) as a clock device. The free running counter does
 not appear in the original SP804 timer. The counter is 32 bits wide and has its own divider that is 8 bits wide meaning that it can
 be set to clock rates of between 975KHz and 250MHz (or 1.5MHz to 400MHz on the Raspberry Pi 3B).
 
 The counter does not generate an interrupt and cannot be set or reset but it can be stopped and started.
 
 
BCM2710 ARM Timer Device
========================

 The ARM Timer device in the BCM2710 is based on the ARM SP804 timer with some modifications and additions. In the Raspberry Pi 
 it is connected to the core clock which by default is 250MHz but was increased to 400MHz on the Raspberry Pi 3B.
 
 The divider is 10 bits wide which means that the ARM Timer can be set to clock rates of between 250KHz and 250MHz (or 400KHz 
 to 400MHz on the Raspberry Pi 3B). Both the counter and the load/reload value are 32 bits wide by default giving a wide range of
 tick intervals.
  
 The ARM Timer features a free running counter which is not enabled or used by this driver and a down counter which operates in
 wrapping mode so that each time it reaches 0 it triggers an interrupt and reloads the value from a load or reload register to
 begin counting again.

 
BCM2710 Local Timer Device
==========================


BCM2710 Random Device
=====================


BCM2710 Mailbox Device
======================


BCM2710 Watchdog Device
=======================


BCM2710 Framebuffer Device
==========================
 
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit BCM2710;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2837,Platform{$IFNDEF CONSOLE_EARLY_INIT},PlatformRPi3{$ENDIF},Threads,HeapManager,Devices,SPI,I2C,DMA,PWM,GPIO,UART,Serial,MMC,Framebuffer,Audio,SysUtils; 

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {BCM2710 specific constants}

 {BCM2710 SPI0 constants}
 BCM2710_SPI0_DESCRIPTION = 'BCM2837 SPI0 Master';
 
 BCM2710_SPI0_MAX_SIZE = $FFFF;

 BCM2710_SPI0_MIN_CLOCK = 3814;       {Default minimum based on the default settings from the firmware (Recalculated during open)}
 BCM2710_SPI0_MAX_CLOCK = 125000000;  {Default maximum based on the default settings from the firmware (Recalculated during open)}

 BCM2710_SPI0_MIN_DIVIDER = 2;        {Divider is always rounded down to an even number and a value of 0 sets the divider to 65536}
 BCM2710_SPI0_MAX_DIVIDER = $FFFE;    {Divider is always rounded down to an even number}

 BCM2710_SPI0_CORE_CLOCK = 250000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}
 
 BCM2710_SPI0_MODE_IRQ = 0;
 BCM2710_SPI0_MODE_DMA = 1;
 BCM2710_SPI0_MODE_PIO = 2;
 
 {BCM2710 BSCI2C (BSC0/1/2) constants}
 BCM2710_BSCI2C_MAX_SIZE = $FFFF;
 
 BCM2710_BSCI2C_MIN_CLOCK = 3814;       {Default minimum based on the default settings from the firmware (Recalculated during open)}
 BCM2710_BSCI2C_MAX_CLOCK = 125000000;  {Default maximum based on the default settings from the firmware (Recalculated during open)}
 BCM2710_BSCI2C_DEFAULT_CLOCK = 100000; 
 
 BCM2710_BSCI2C_MIN_DIVIDER = 2;        {Divider is always rounded down to an even number and a value of 0 sets the divider to 32768}
 BCM2710_BSCI2C_MAX_DIVIDER = $FFFE;    {Divider is always rounded down to an even number}
 
 BCM2710_BSCI2C_CORE_CLOCK = 250000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}
 
 BCM2710_BSCI2C_MODE_WRITE = 0;
 BCM2710_BSCI2C_MODE_READ  = 1;
 
 {BCM2710 I2C0 (BSC0) constants}
 BCM2710_I2C0_DESCRIPTION = 'BCM2837 BSC0 Master I2C';
 
 {BCM2710 I2C1 (BSC1) constants}
 BCM2710_I2C1_DESCRIPTION = 'BCM2837 BSC1 Master I2C';
 
 {BCM2710 SPI AUX (SPI1/2) constants}
 //To Do //Continuing
 
 {BCM2710 SPI1 constants}
 BCM2710_SPI1_DESCRIPTION = 'BCM2837 AUX SPI1 Master';

 {BCM2710 SPI2 constants}
 BCM2710_SPI2_DESCRIPTION = 'BCM2837 AUX SPI2 Master';
 
 {BCM2710 SPIBSC Slave constants}
 
 {BCM2710 I2C Slave constants}
 BCM2710_I2CSLAVE_DESCRIPTION = 'BCM2837 I2C Slave';

 {BCM2710 SPI Slave constants}
 BCM2710_SPISLAVE_DESCRIPTION = 'BCM2837 SPI Slave';
 
 {BCM2710 DMA constants}
 BCM2710_DMA_DESCRIPTION = 'BCM2837 DMA';
 
 BCM2710_DMA_CHANNEL_COUNT = 16;                 {Total number of DMA channels (Not all are usable)}
 
 BCM2710_DMA_LITE_CHANNELS   = $7F80;            {Mask of DMA Lite channels (7 to 14)}
 BCM2710_DMA_NORMAL_CHANNELS = $007E; {807F}     {Mask of normal channels (1 to 6)}
 BCM2710_DMA_BULK_CHANNELS   = $8001;            {Mask of DMA Bulk channels (0 and 15)}
 
 BCM2710_DMA_SHARED_CHANNELS = $7800;            {Mask of channels with shared interrupt (11 to 14)}
 
 BCM2710_DMA_MAX_LITE_TRANSFER   = 65536;        {Maximum transfer length for a DMA Lite channel}
 BCM2710_DMA_MAX_NORMAL_TRANSFER = 1073741824;   {Maximum transfer length for a normal channel}
 
 BCM2710_DMA_MAX_STRIDE   = $FFFF;               {Maximum stride value (Increment between rows) (Note this is a signed value (Min -32768 / Max 32767)}
 BCM2710_DMA_MAX_Y_COUNT  = $3FFF;               {Maximum number of X length transfers in 2D stride}
 BCM2710_DMA_MAX_X_LENGTH = $FFFF;               {Maximum X transfer length in 2D stride}
 
 BCM2710_DMA_CB_ALIGNMENT = 32;                  {Alignment required for DMA control blocks}
 
 BCM2710_DMA_LITE_BURST_LENGTH = 1;              {Burst length for DMA Lite channels}
 BCM2710_DMA_NORMAL_BURST_LENGTH = 2;            {Burst length for normal channels}
 BCM2710_DMA_BULK_BURST_LENGTH = 8;              {Burst length for DMA Bulk channels}
 
 {BCM2710 PWM constants}
 BCM2710_PWM_MIN_PERIOD = 108;          {Default based on 19.2MHz PWM clock (Oscillator source)}
 BCM2710_PWM_DEFAULT_CLOCK = 19200000;  {Default to the 19.2MHz oscillator clock}
 
 {BCM2710 PWM0 constants}
 BCM2710_PWM0_DESCRIPTION = 'BCM2837 PWM0';

 {BCM2710 PWM1 constants}
 BCM2710_PWM1_DESCRIPTION = 'BCM2837 PWM1';
 
 {BCM2710 PCM constants}
 
 {BCM2710 GPIO constants}
 BCM2710_GPIO_DESCRIPTION = 'BCM2837 GPIO';
 
 BCM2710_GPIO_MIN_PIN = GPIO_PIN_0;
 BCM2710_GPIO_MAX_PIN = GPIO_PIN_53;
 
 BCM2710_GPIO_MAX_LEVEL = GPIO_LEVEL_HIGH;
 
 BCM2710_GPIO_MAX_PULL = GPIO_PULL_DOWN;
  
 BCM2710_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 BCM2710_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT5;

 BCM2710_GPIO_MIN_TRIGGER = GPIO_TRIGGER_LOW;
 BCM2710_GPIO_MAX_TRIGGER = GPIO_TRIGGER_ASYNC_FALLING;

 BCM2710_GPIO_PULL_MAP:array[GPIO_PULL_NONE..GPIO_PULL_DOWN] of LongWord = (
  {GPIO pull up/down to BCM2837 pull up/down}
  BCM2837_GPPUD_NONE,
  BCM2837_GPPUD_UP,
  BCM2837_GPPUD_DOWN);
 
 BCM2710_GPIO_FUNCTION_MAP:array[BCM2710_GPIO_MIN_FUNCTION..BCM2710_GPIO_MAX_FUNCTION] of LongWord = (
  {GPIO functions to BCM2837 functions}
  BCM2837_GPFSEL_IN,
  BCM2837_GPFSEL_OUT,
  BCM2837_GPFSEL_ALT0,
  BCM2837_GPFSEL_ALT1,
  BCM2837_GPFSEL_ALT2,
  BCM2837_GPFSEL_ALT3,
  BCM2837_GPFSEL_ALT4,
  BCM2837_GPFSEL_ALT5);
 
 BCM2710_GPIO_FUNCTION_UNMAP:array[BCM2710_GPIO_MIN_FUNCTION..BCM2710_GPIO_MAX_FUNCTION] of LongWord = (
  {BCM2837 functions to GPIO functions}
  GPIO_FUNCTION_IN,
  GPIO_FUNCTION_OUT,
  GPIO_FUNCTION_ALT5,
  GPIO_FUNCTION_ALT4,
  GPIO_FUNCTION_ALT0,
  GPIO_FUNCTION_ALT1,
  GPIO_FUNCTION_ALT2,
  GPIO_FUNCTION_ALT3);
  
 BCM2710_GPIO_TRIGGER_MAP:array[BCM2710_GPIO_MIN_TRIGGER..BCM2710_GPIO_MAX_TRIGGER] of LongWord = (
  {GPIO triggers to BCM2837 event registers}
  BCM2837_GPLEN0,
  BCM2837_GPHEN0,
  BCM2837_GPREN0,
  BCM2837_GPFEN0,
  BCM2837_GPAREN0,
  BCM2837_GPAFEN0);
 
 {BCM2710 UART0 (PL011) constants}
 BCM2710_UART0_DESCRIPTION = 'BCM2837 PL011 UART';
 
 BCM2710_UART0_MIN_BAUD = 300;      {Default minimum of 300 baud}
 BCM2710_UART0_MAX_BAUD = 187500;   {Default maximum based on the default settings from the firmware (Recalculated during open)}
 
 BCM2710_UART0_MIN_DATABITS = SERIAL_DATA_5BIT;
 BCM2710_UART0_MAX_DATABITS = SERIAL_DATA_8BIT;
 
 BCM2710_UART0_MIN_STOPBITS = SERIAL_STOP_1BIT;
 BCM2710_UART0_MAX_STOPBITS = SERIAL_STOP_2BIT;
 
 BCM2710_UART0_MAX_PARITY = SERIAL_PARITY_EVEN;
 
 BCM2710_UART0_MAX_FLOW = SERIAL_FLOW_RTS_CTS;
 
 BCM2710_UART0_CLOCK_RATE = 3000000; {Default clock rate based on the default settings from the firmware (Requested from firmware during open)}
 {$IFDEF BCM2710_UART0_RX_BUFFER}
 BCM2710_UART0_RX_POLL_LIMIT = 256; {Number of times interrupt handler may poll the read FIFO}
 BCM2710_UART0_RX_BUFFER_SIZE = 1024;
 {$ENDIF}
 
 {BCM2710 UART1 (AUX) constants}
 BCM2710_UART1_DESCRIPTION = 'BCM2837 AUX (Mini) UART';
 
 {BCM2710 SDHCI constants}
 BCM2710_EMMC_DESCRIPTION = 'BCM2837 Arasan SD Host';
 
 BCM2710_EMMC_MIN_FREQ = 400000;    {Default minimum of 400KHz}
 BCM2710_EMMC_MAX_FREQ = 250000000; //To Do //Get the current frequency from the command line or mailbox instead ? //Peripheral init could get from Mailbox like SMSC95XX ?
 
 {BCM2710 Clock (System Timer) constants}
 BCM2710_SYS_CLOCK_DESCRIPTION = 'BCM2837 System Timer Clock';

 {BCM2710 Clock (ARM Timer) constants}
 BCM2710_ARM_CLOCK_DESCRIPTION = 'BCM2837 ARM Timer Clock';

 BCM2710_ARM_CLOCK_MIN_RATE = 976562;      {Default minimum (Divider 255) based on the default settings from the firmware (Recalculated during start)}
 BCM2710_ARM_CLOCK_MAX_RATE = 250000000;   {Default maximum (Divider 0) based on the default settings from the firmware (Recalculated during start)}
 BCM2710_ARM_CLOCK_DEFAULT_RATE = 3968253; {Default rate (Divider 62) based on the default settings from the firmware (Recalculated during start)}
  
 BCM2710_ARM_CLOCK_MIN_DIVIDER = 0;
 BCM2710_ARM_CLOCK_MAX_DIVIDER = 255;
 BCM2710_ARM_CLOCK_DEFAULT_DIVIDER = 62;
 
 BCM2710_ARM_CLOCK_CORE_CLOCK = 250000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}
 
 {BCM2710 Clock (Local Timer) constants}
 BCM2710_LOCAL_CLOCK_DESCRIPTION = 'BCM2837 Local Timer Clock';
 
 {BCM2710 ARM Timer constants}
 BCM2710_ARM_TIMER_DESCRIPTION = 'BCM2837 ARM Timer';

 BCM2710_ARM_TIMER_MIN_RATE = 244140;      {Default minimum (Divider 1023) based on the default settings from the firmware (Recalculated during start)}
 BCM2710_ARM_TIMER_MAX_RATE = 250000000;   {Default maximum (Divider 0) based on the default settings from the firmware (Recalculated during start)}
 BCM2710_ARM_TIMER_DEFAULT_RATE = 1000000; {Default rate (Divider 249) based on the default settings from the firmware (Recalculated during start)}
 
 BCM2710_ARM_TIMER_MIN_INTERVAL = 1;
 BCM2710_ARM_TIMER_MAX_INTERVAL = $FFFFFFFF;
 
 BCM2710_ARM_TIMER_MIN_DIVIDER = 0;
 BCM2710_ARM_TIMER_MAX_DIVIDER = 1023;
 BCM2710_ARM_TIMER_DEFAULT_DIVIDER = 249;
 
 BCM2710_ARM_TIMER_CORE_CLOCK = 250000000; {Default core clock based on the default settings from the firmware (Requested from firmware during start)}
 
 {BCM2710 Local Timer constants}
 BCM2710_LOCAL_TIMER_DESCRIPTION = 'BCM2837 Local Timer';
 
 {BCM2710 Random constants}
 BCM2710_RANDOM_DESCRIPTION = 'BCM2837 Random Number Generator';
 
 BCM2710_RANDOM_WARMUP_COUNT  = $00040000; {The initial numbers generated are "less random" so will be discarded}

 {BCM2710 Mailbox constants}
 BCM2710_MAILBOX_DESCRIPTION = 'BCM2837 Mailbox';
 
 {BCM2710 Watchdog constants}
 BCM2710_WATCHDOG_DESCRIPTION = 'BCM2837 Watchdog Timer';
 
 {BCM2710 Framebuffer constants}
 BCM2710_FRAMEBUFFER_DESCRIPTION = 'BCM2837 Framebuffer';
 
{==============================================================================}
type
 {BCM2710 specific types}
 
 {BCM2710 SPI0 types}
 PBCM2710SPI0Device = ^TBCM2710SPI0Device;
 TBCM2710SPI0Device = record
  {SPI Properties}
  SPI:TSPIDevice;
  {BCM2710 Properties}
  Address:Pointer;                 {Device register base address}
  CoreClock:LongWord;              {Core clock rate}
  {Transfer Properties}
  Mode:LongWord;                   {Mode of current transfer (BCM2710_SPI0_MODE_IRQ / BCM2710_SPI0_MODE_DMA / BCM2710_SPI0_MODE_PIO)}
  Source:Pointer;                  {Pointer to the source for current transfer (nil if reading only)}
  Dest:Pointer;                    {Pointer to the destination for current transfer (nil if writing only)}
  Count:LongWord;                  {Count of bytes for current transfer}
  SourceRemain:LongWord;           {Source bytes remaining for current transfer}
  DestRemain:LongWord;             {Destination bytes remaining for current transfer}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end;
 
 {BCM2710 BSCI2C (I2C0/1) types}
 PBCM2710BSCI2CDevice = ^TBCM2710BSCI2CDevice;
 TBCM2710BSCI2CDevice = record
  {I2C Properties}
  I2C:TI2CDevice;
  {BCM2710 Properties}
  Address:Pointer;                 {Device register base address}
  CoreClock:LongWord;              {Core clock rate}
  SDAPin:LongWord;                 {GPIO pin for the SDA line}
  SCLPin:LongWord;                 {GPIO pin for the SCL line}                 
  SDAFunction:LongWord;            {GPIO function for the SDA line}
  SCLFunction:LongWord;            {GPIO function for the SCL line}
  {Transfer Properties}
  Index:LongWord;                  {Index of this device in the IRQData array (Set during device initialization)}
  Mode:LongWord;                   {Mode of current transfer (BCM2710_BSCI2C_MODE_WRITE / BCM2710_BSCI2C_MODE_READ)}
  Data:Pointer;                    {Pointer to the data for current transfer}
  Count:LongWord;                  {Count of bytes for current transfer}
  Remain:LongWord;                 {Bytes remaining for current transfer}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end;
 
 PBCM2710BSCI2CIRQData = ^TBCM2710BSCI2CIRQData; {BSC I2C devices share a single interrupt}
 TBCM2710BSCI2CIRQData = record
  Count:LongWord;
  Lock:TSpinHandle;
  Devices:array[0..2] of PBCM2710BSCI2CDevice;
 end; 
 
 {BCM2710 SPI AUX (SPI1/2) types}
 PBCM2710SPIAUXDevice = ^TBCM2710SPIAUXDevice;
 TBCM2710SPIAUXDevice = record
  {SPI Properties}
  SPI:TSPIDevice;
  {BCM2710 Properties}
  Address:Pointer;                 {Device register base address}
  CoreClock:LongWord;              {Core clock rate}
  {Transfer Properties}
  //To Do //Continuing
 end;
 
 {BCM2710 SPI/I2C Slave types}
 
 {BCM2710 DMA types}
 PBCM2710DMAHost = ^TBCM2710DMAHost;
 
 PBCM2710DMAChannel = ^TBCM2710DMAChannel;
 TBCM2710DMAChannel = record
  Host:PBCM2710DMAHost;            {DMA host this channel belongs to}
  Request:PDMARequest;             {Current DMA request pending on this channel (or nil of no request is pending)} 
  Number:LongWord;                 {The channel number of this channel}
  Interrupt:LongWord;              {The interrupt number of this channel}
  Registers:PBCM2837DMARegisters;  {The channel registers for configuration}
 end;
 
 TBCM2710DMAHost = record
  {DMA Properties}
  DMA:TDMAHost;
  {BCM2710 Properties}
  ChannelMask:LongWord;                                                   {Mask of available channels (Passed from GPU firmware)}
  ChannelFree:LongWord;                                                   {Bitmap of current free channels}
  ChannelLock:TMutexHandle;                                               {Lock for access to ChannelFree}
  ChannelWait:TSemaphoreHandle;                                           {Number of free normal channels in ChannelFree}
  ChannelLite:TSemaphoreHandle;                                           {Number of free DMA Lite channels in ChannelFree}
  ChannelBulk:TSemaphoreHandle;                                           {Number of free DMA Bulk channels in ChannelFree}
  Channels:array[0..BCM2710_DMA_CHANNEL_COUNT - 1] of TBCM2710DMAChannel; {Channel information for each DMA channel on the host}
  EnableRegister:PLongWord;
  InterruptRegister:PLongWord;
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the host controller}
 end;
 
 {BCM2710 PWM types}
 PBCM2710PWMDevice = ^TBCM2710PWMDevice;
 PBCM2710PWMAudio = ^TBCM2710PWMAudio;
 
 TBCM2710PWMDevice = record
  {PWM Properties}
  PWM:TPWMDevice;
  {BCM2710 Properties}
  Address:Pointer;                 {Device register base address}
  Channel:LongWord;                {Channel for this device}
  Scaler:LongWord;                 {Scaler for Duty cycle and Period}
  Pair:PBCM2710PWMDevice;          {The paired PWM device for the other channel}
  {Audio Properties}
  Audio:PBCM2710PWMAudio;          {The associated PWM Audio device}
 end; 

 TBCM2710PWMAudio = record
  {Audio Properties}
  Audio:TAudioDevice;
  {BCM2710 Properties}
  //To Do 
  {PWM Properties}
  PWM0:PBCM2710PWMDevice;          {The PWM device for channel 0}
  PWM1:PBCM2710PWMDevice;          {The PWM device for channel 1}
 end;
 
 {BCM2710 PCM types}
 
 {BCM2710 GPIO types}
 PBCM2710GPIODevice = ^TBCM2710GPIODevice;
 
 PBCM2710GPIOBank = ^TBCM2710GPIOBank;
 TBCM2710GPIOBank = record
  GPIO:PGPIODevice;
  Bank:LongWord;
  Address:PtrUInt;
  PinStart:LongWord;
 end;
 
 TBCM2710GPIODevice = record
  {GPIO Properties}
  GPIO:TGPIODevice;
  {BCM2710 Properties}
  Lock:TSpinHandle;                                                       {Device lock (Differs from lock in Device portion) (Spin lock due to use by interrupt handler)}
  Banks:array[0..BCM2837_GPIO_BANK_COUNT - 1] of TBCM2710GPIOBank;
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end;
 
 {BCM2710 UART0 types}
 PBCM2710UART0Device = ^TBCM2710UART0Device;
 TBCM2710UART0Device = record
  {UART Properties}
  UART:TUARTDevice;
  {BCM2710 Properties}
  Lock:TSpinHandle;                                                       {Device lock (Differs from lock in UART device) (Spin lock due to use by interrupt handler)}
  Address:Pointer;                                                        {Device register base address}
  ClockRate:LongWord;                                                     {Device clock rate}
  {$IFDEF BCM2710_UART0_RX_BUFFER}
  Start:LongWord;                                                         {Index of first available buffer entry}
  Count:LongWord;                                                         {Number of available entries in the buffer}
  Buffer:array[0..(BCM2710_UART0_RX_BUFFER_SIZE - 1)] of Word;            {Buffer for received data (Includes data and status)}
  {$ENDIF}
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end;
 
 {BCM2710 UART1 types}
 
 {BCM2710 SDHCI types}
 PBCM2710SDHCIHost = ^TBCM2710SDHCIHost;
 TBCM2710SDHCIHost = record
  {SDHCI Properties}
  SDHCI:TSDHCIHost;
  {BCM2710 Properties}
  //Lock:TSpinHandle; //To Do //Not Needed ? //See: DWCOTG etc
  WriteDelay:LongWord;
  LastWrite:LongWord;
  ShadowRegister:LongWord;
 end;
 
 {BCM2710 System Clock types}
 PBCM2710SystemClock = ^TBCM2710SystemClock;
 TBCM2710SystemClock = record
  {Clock Properties}
  Clock:TClockDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 

 {BCM2710 ARM Clock types}
 PBCM2710ARMClock = ^TBCM2710ARMClock;
 TBCM2710ARMClock = record
  {Clock Properties}
  Clock:TClockDevice;
  {BCM2710 Properties}
  CoreClock:LongWord;              {Core clock rate}
 end; 
 
 {BCM2710 ARM Timer types}
 PBCM2710ARMTimer = ^TBCM2710ARMTimer;
 TBCM2710ARMTimer = record
  {Timer Properties}
  Timer:TTimerDevice;
  {BCM2710 Properties}
  CoreClock:LongWord;              {Core clock rate}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end; 
 
 {BCM2710 Local Timer types}
 PBCM2710LocalTimer = ^TBCM2710LocalTimer;
 TBCM2710LocalTimer = record
  {Timer Properties}
  Timer:TTimerDevice;
  {BCM2710 Properties}
  CoreClock:LongWord;              {Core clock rate}
  {Statistics Properties}          
  InterruptCount:LongWord;         {Number of interrupt requests received by the device}
 end; 
 
 {BCM2710 Random types}
 PBCM2710Random = ^TBCM2710Random;
 TBCM2710Random = record
  {Random Properties}
  Random:TRandomDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 

 {BCM2710 Mailbox types}
 PBCM2710Mailbox = ^TBCM2710Mailbox;
 TBCM2710Mailbox = record
  {Mailbox Properties}
  Mailbox:TMailboxDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 
 
 {BCM2710 Watchdog types}
 PBCM2710Watchdog = ^TBCM2710Watchdog;
 TBCM2710Watchdog = record
  {Watchdog Properties}
  Watchdog:TWatchdogDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 

 {BCM2710 Framebuffer types}
 PBCM2710Framebuffer = ^TBCM2710Framebuffer;
 TBCM2710Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 
 
{==============================================================================}
{var}
 {BCM2710 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure BCM2710Init;
 
{==============================================================================}
{BCM2710 Functions}

{==============================================================================}
{BCM2710 SPI0 Functions}
function BCM2710SPI0Start(SPI:PSPIDevice;Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
function BCM2710SPI0Stop(SPI:PSPIDevice):LongWord;

function BCM2710SPI0WriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

function BCM2710SPI0SetMode(SPI:PSPIDevice;Mode:LongWord):LongWord;
function BCM2710SPI0SetClockRate(SPI:PSPIDevice;ChipSelect:Word;ClockRate:LongWord):LongWord;
function BCM2710SPI0SetClockPhase(SPI:PSPIDevice;ClockPhase:LongWord):LongWord;
function BCM2710SPI0SetClockPolarity(SPI:PSPIDevice;ClockPolarity:LongWord):LongWord;
function BCM2710SPI0SetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word;SelectPolarity:LongWord):LongWord;
 
procedure BCM2710SPI0ReadFIFO(SPI:PBCM2710SPI0Device);
procedure BCM2710SPI0WriteFIFO(SPI:PBCM2710SPI0Device);

procedure BCM2710SPI0InterruptHandler(SPI:PBCM2710SPI0Device);
procedure BCM2710SPI0DMARequestCompleted(Request:PDMARequest); 

{==============================================================================}
{BCM2710 BSCI2C (I2C0/1) Functions}
function BCM2710BSCI2CStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
function BCM2710BSCI2CStop(I2C:PI2CDevice):LongWord;
 
function BCM2710BSCI2CRead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function BCM2710BSCI2CWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function BCM2710BSCI2CWriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function BCM2710BSCI2CWriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
 
function BCM2710BSCI2CSetRate(I2C:PI2CDevice;Rate:LongWord):LongWord;
 
function BCM2710BSCI2CSetAddress(I2C:PI2CDevice;Address:Word):LongWord;

procedure BCM2710BSCI2CFillFIFO(I2C:PBCM2710BSCI2CDevice);
procedure BCM2710BSCI2CDrainFIFO(I2C:PBCM2710BSCI2CDevice);

procedure BCM2710BSCI2CInterruptHandler(IRQData:PBCM2710BSCI2CIRQData);

{==============================================================================}
{BCM2710 SPI AUX (SPI1/2) Functions}
//To Do //Continuing

{==============================================================================}
{BCM2710 SPI/I2C Slave Functions}

{==============================================================================}
{BCM2710 DMA Functions}
function BCM2710DMAHostStart(DMA:PDMAHost):LongWord;
function BCM2710DMAHostStop(DMA:PDMAHost):LongWord;

function BCM2710DMAHostSubmit(DMA:PDMAHost;Request:PDMARequest):LongWord;
function BCM2710DMAHostCancel(DMA:PDMAHost;Request:PDMARequest):LongWord;

procedure BCM2710DMAInterruptHandler(Channel:PBCM2710DMAChannel);
procedure BCM2710DMASharedInterruptHandler(DMA:PBCM2710DMAHost);

procedure BCM2710DMARequestComplete(Channel:PBCM2710DMAChannel);

function BCM2710DMAPeripheralToDREQ(Peripheral:LongWord):LongWord;
procedure BCM2710DMADataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2837DMAControlBlock;Bulk,Lite:Boolean);

{==============================================================================}
{BCM2710 PWM0/1 Functions}
function BCM2710PWMStart(PWM:PPWMDevice):LongWord; 
function BCM2710PWMStop(PWM:PPWMDevice):LongWord; 

function BCM2710PWMWrite(PWM:PPWMDevice;Value:LongWord):LongWord; 
 
function BCM2710PWMSetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
function BCM2710PWMResetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
function BCM2710PWMSetMode(PWM:PPWMDevice;Mode:LongWord):LongWord;
function BCM2710PWMSetRange(PWM:PPWMDevice;Range:LongWord):LongWord;
function BCM2710PWMSetFrequency(PWM:PPWMDevice;Frequency:LongWord):LongWord;
function BCM2710PWMSetPolarity(PWM:PPWMDevice;Polarity:LongWord):LongWord;

function BCM2710PWMConfigure(PWM:PPWMDevice;DutyNS,PeriodNS:LongWord):LongWord;

function BCM2710PWMClockStart(PWM:PPWMDevice;Frequency:LongWord):LongWord; 
function BCM2710PWMClockStop(PWM:PPWMDevice):LongWord; 
function BCM2710PWMClockEnabled(PWM:PPWMDevice):Boolean;

{==============================================================================}
{BCM2710 PCM Functions}

{==============================================================================}
{BCM2710 GPIO Functions}
function BCM2710GPIOStart(GPIO:PGPIODevice):LongWord; 
function BCM2710GPIOStop(GPIO:PGPIODevice):LongWord; 
 
function BCM2710GPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
procedure BCM2710GPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);

function BCM2710GPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function BCM2710GPIOInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
function BCM2710GPIOInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
function BCM2710GPIOInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function BCM2710GPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function BCM2710GPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function BCM2710GPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function BCM2710GPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

procedure BCM2710GPIOInterruptHandler(Bank:PBCM2710GPIOBank);

procedure BCM2710GPIOEventTrigger(Pin:PGPIOPin);
procedure BCM2710GPIOEventTimeout(Event:PGPIOEvent);

{==============================================================================}
{BCM2710 UART0 Functions}
function BCM2710UART0Open(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
function BCM2710UART0Close(UART:PUARTDevice):LongWord;
 
function BCM2710UART0Read(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function BCM2710UART0Write(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
 
function BCM2710UART0GetStatus(UART:PUARTDevice):LongWord;
function BCM2710UART0SetStatus(UART:PUARTDevice;Status:LongWord):LongWord;

procedure BCM2710UART0InterruptHandler(UART:PUARTDevice);

procedure BCM2710UART0Receive(UART:PUARTDevice);
procedure BCM2710UART0Transmit(UART:PUARTDevice);

procedure BCM2710UART0EnableInterrupt(UART:PBCM2710UART0Device;Interrupt:LongWord); 
procedure BCM2710UART0DisableInterrupt(UART:PBCM2710UART0Device;Interrupt:LongWord); 

{==============================================================================}
{BCM2710 UART1 Functions}

{==============================================================================}
{BCM2710 SDHCI Functions}
function BCM2710SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
function BCM2710SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

function BCM2710SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
function BCM2710SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
function BCM2710SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
procedure BCM2710SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
procedure BCM2710SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
procedure BCM2710SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
 
procedure BCM2710SDHCIInterruptHandler(SDHCI:PSDHCIHost);
function BCM2710SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;

function BCM2710MMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
 
{==============================================================================}
{BCM2710 System Clock Functions}
function BCM2710SystemClockRead(Clock:PClockDevice):LongWord;
function BCM2710SystemClockRead64(Clock:PClockDevice):Int64;

{==============================================================================}
{BCM2710 ARM Clock Functions}
function BCM2710ARMClockStart(Clock:PClockDevice):LongWord;
function BCM2710ARMClockStop(Clock:PClockDevice):LongWord;

function BCM2710ARMClockRead(Clock:PClockDevice):LongWord;
function BCM2710ARMClockRead64(Clock:PClockDevice):Int64;

function BCM2710ARMClockSetRate(Clock:PClockDevice;Rate:LongWord):LongWord;

{==============================================================================}
{BCM2710 ARM Timer Functions}
function BCM2710ARMTimerStart(Timer:PTimerDevice):LongWord;
function BCM2710ARMTimerStop(Timer:PTimerDevice):LongWord;
function BCM2710ARMTimerRead64(Timer:PTimerDevice):Int64;
function BCM2710ARMTimerWait(Timer:PTimerDevice):LongWord;
function BCM2710ARMTimerEvent(Timer:PTimerDevice;Flags:LongWord;Callback:TTimerCallback;Data:Pointer):LongWord;
function BCM2710ARMTimerCancel(Timer:PTimerDevice):LongWord;
function BCM2710ARMTimerSetRate(Timer:PTimerDevice;Rate:LongWord):LongWord;
function BCM2710ARMTimerSetInterval(Timer:PTimerDevice;Interval:LongWord):LongWord;

procedure BCM2710ARMTimerInterruptHandler(Timer:PTimerDevice);

procedure BCM2710ARMTimerEventTrigger(Timer:PTimerDevice);

{==============================================================================}
{BCM2710 Local Timer Functions}
//To Do

{==============================================================================}
{BCM2710 Random Functions}
function BCM2710RandomStart(Random:PRandomDevice):LongWord;
function BCM2710RandomStop(Random:PRandomDevice):LongWord;

function BCM2710RandomReadLongWord(Random:PRandomDevice):LongWord;

{==============================================================================}
{BCM2710 Mailbox Functions}
//To Do

{==============================================================================}
{BCM2710 Watchdog Functions}
function BCM2710WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
function BCM2710WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
function BCM2710WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;

function BCM2710WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;

{==============================================================================}
{BCM2710 Framebuffer Functions}
function BCM2710FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function BCM2710FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function BCM2710FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function BCM2710FramebufferCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;

function BCM2710FramebufferWaitSync(Framebuffer:PFramebufferDevice):LongWord;
 
function BCM2710FramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;

function BCM2710FramebufferGetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
function BCM2710FramebufferSetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;

function BCM2710FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;

function BCM2710FramebufferSetCursor(Framebuffer:PFramebufferDevice;Width,Height,HotspotX,HotspotY:LongWord;Image:Pointer;Len:LongWord):LongWord;
function BCM2710FramebufferUpdateCursor(Framebuffer:PFramebufferDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;

function BCM2710FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

{==============================================================================}
{BCM2710 Helper Functions}
  
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {BCM2710 specific variables}
 BCM2710Initialized:Boolean;

 BCM2710BSCI2CIRQData:TBCM2710BSCI2CIRQData;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BCM2710Init;
var
 Status:LongWord;
 
 BCM2710DMAHost:PBCM2710DMAHost;
 BCM2710SDHCIHost:PBCM2710SDHCIHost; 

 BCM2710GPIO:PBCM2710GPIODevice;
 BCM2710SPI0:PBCM2710SPI0Device;
 BCM2710I2C0:PBCM2710BSCI2CDevice;
 BCM2710I2C1:PBCM2710BSCI2CDevice;
 BCM2710PWM0:PBCM2710PWMDevice;
 BCM2710PWM1:PBCM2710PWMDevice;
 BCM2710UART0:PBCM2710UART0Device;
 
 BCM2710SystemClock:PBCM2710SystemClock;
 BCM2710ARMClock:PBCM2710ARMClock;
 BCM2710ARMTimer:PBCM2710ARMTimer;
 BCM2710LocalTimer:PBCM2710LocalTimer;
 BCM2710Random:PBCM2710Random;
 BCM2710Mailbox:PBCM2710Mailbox;
 BCM2710Watchdog:PBCM2710Watchdog;
 BCM2710Framebuffer:PBCM2710Framebuffer;
begin
 {}
 {Check Initialized}
 if BCM2710Initialized then Exit;
 
 {Initialize BCM2710SDHCI_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2710SDHCI_FIQ_ENABLED:=False;

 {Initialize BCM2710ARM_TIMER_FIQ_ENABLED/BCM2710LOCAL_TIMER_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2710ARM_TIMER_FIQ_ENABLED:=False;
 if not(FIQ_ENABLED) then BCM2710LOCAL_TIMER_FIQ_ENABLED:=False;
 
 {Initialize IRQ Data}
 FillChar(BCM2710BSCI2CIRQData,SizeOf(TBCM2710BSCI2CIRQData),0);
 BCM2710BSCI2CIRQData.Lock:=SpinCreate;
 
 {$IFNDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPi3GPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPi3GPUMemoryRelease;
 GPUMemoryLockHandler:=RPi3GPUMemoryLock;
 GPUMemoryUnlockHandler:=RPi3GPUMemoryUnlock;
 
 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPi3GPUExecuteCode;
 DispmanxHandleGetHandler:=RPi3DispmanxHandleGet;
 EDIDBlockGetHandler:=RPi3EDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPi3FramebufferAllocate;
 FramebufferReleaseHandler:=RPi3FramebufferRelease;
 FramebufferSetStateHandler:=RPi3FramebufferSetState;

 FramebufferGetDimensionsHandler:=RPi3FramebufferGetDimensions;
 
 FramebufferGetPhysicalHandler:=RPi3FramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPi3FramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPi3FramebufferTestPhysical;
 
 FramebufferGetVirtualHandler:=RPi3FramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPi3FramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPi3FramebufferTestVirtual;
 
 FramebufferGetDepthHandler:=RPi3FramebufferGetDepth;
 FramebufferSetDepthHandler:=RPi3FramebufferSetDepth;
 FramebufferTestDepthHandler:=RPi3FramebufferTestDepth;
 
 FramebufferGetPixelOrderHandler:=RPi3FramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPi3FramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPi3FramebufferTestPixelOrder;
 
 FramebufferGetAlphaModeHandler:=RPi3FramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPi3FramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPi3FramebufferTestAlphaMode;
 
 FramebufferGetPitchHandler:=RPi3FramebufferGetPitch;
 
 FramebufferGetOffsetHandler:=RPi3FramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPi3FramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPi3FramebufferTestOffset;
 
 FramebufferGetOverscanHandler:=RPi3FramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPi3FramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPi3FramebufferTestOverscan;
 
 FramebufferGetPaletteHandler:=RPi3FramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPi3FramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPi3FramebufferTestPalette;

 FramebufferTestVsyncHandler:=RPi3FramebufferTestVsync;
 FramebufferSetVsyncHandler:=RPi3FramebufferSetVsync;
 
 FramebufferSetBacklightHandler:=RPi3FramebufferSetBacklight;
 
 {Register Platform Touch Handlers}
 TouchGetBufferHandler:=RPi3TouchGetBuffer;
 TouchSetBufferHandler:=RPi3TouchSetBuffer;
 
 {Register Platform Cursor Handlers}
 CursorSetDefaultHandler:=RPi3CursorSetDefault;
 CursorSetInfoHandler:=RPi3CursorSetInfo;
 CursorSetStateHandler:=RPi3CursorSetState;
 {$ENDIF}
  
 {Create DMA}
 if BCM2710_REGISTER_DMA then
  begin
   BCM2710DMAHost:=PBCM2710DMAHost(DMAHostCreateEx(SizeOf(TBCM2710DMAHost)));
   if BCM2710DMAHost <> nil then
    begin
     {Update DMA}
     {Device}
     BCM2710DMAHost.DMA.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710DMAHost.DMA.Device.DeviceType:=DMA_TYPE_NONE;
     BCM2710DMAHost.DMA.Device.DeviceFlags:=DMA_FLAG_STRIDE or DMA_FLAG_DREQ or DMA_FLAG_NOINCREMENT or DMA_FLAG_NOREAD or DMA_FLAG_NOWRITE or DMA_FLAG_WIDE;
     BCM2710DMAHost.DMA.Device.DeviceData:=nil;
     BCM2710DMAHost.DMA.Device.DeviceDescription:=BCM2710_DMA_DESCRIPTION;
     if BCM2710DMA_SHARED_MEMORY then BCM2710DMAHost.DMA.Device.DeviceFlags:=BCM2710DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_SHARED;
     if BCM2710DMA_NOCACHE_MEMORY then BCM2710DMAHost.DMA.Device.DeviceFlags:=BCM2710DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_NOCACHE;
     if BCM2710DMA_CACHE_COHERENT then BCM2710DMAHost.DMA.Device.DeviceFlags:=BCM2710DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_COHERENT;
     {DMA}
     BCM2710DMAHost.DMA.DMAState:=DMA_STATE_DISABLED;
     BCM2710DMAHost.DMA.HostStart:=BCM2710DMAHostStart;
     BCM2710DMAHost.DMA.HostStop:=BCM2710DMAHostStop;
     BCM2710DMAHost.DMA.HostReset:=nil;
     BCM2710DMAHost.DMA.HostSubmit:=BCM2710DMAHostSubmit;
     BCM2710DMAHost.DMA.HostCancel:=BCM2710DMAHostCancel;
     BCM2710DMAHost.DMA.HostProperties:=nil;
     BCM2710DMAHost.DMA.Alignment:=BCM2710DMA_ALIGNMENT;
     BCM2710DMAHost.DMA.Multiplier:=BCM2710DMA_MULTIPLIER;
     BCM2710DMAHost.DMA.Properties.Flags:=BCM2710DMAHost.DMA.Device.DeviceFlags;
     BCM2710DMAHost.DMA.Properties.Alignment:=BCM2710DMAHost.DMA.Alignment;
     BCM2710DMAHost.DMA.Properties.Multiplier:=BCM2710DMAHost.DMA.Multiplier;
     BCM2710DMAHost.DMA.Properties.Channels:=BCM2710_DMA_CHANNEL_COUNT;
     BCM2710DMAHost.DMA.Properties.MaxSize:=BCM2710_DMA_MAX_NORMAL_TRANSFER;
     BCM2710DMAHost.DMA.Properties.MaxCount:=BCM2710_DMA_MAX_Y_COUNT;
     BCM2710DMAHost.DMA.Properties.MaxLength:=BCM2710_DMA_MAX_X_LENGTH;
     BCM2710DMAHost.DMA.Properties.MinStride:=-32768;
     BCM2710DMAHost.DMA.Properties.MaxStride:=32767;
     {BCM2710}
     BCM2710DMAHost.ChannelLock:=INVALID_HANDLE_VALUE;
     BCM2710DMAHost.ChannelWait:=INVALID_HANDLE_VALUE;
     BCM2710DMAHost.ChannelLite:=INVALID_HANDLE_VALUE;
     BCM2710DMAHost.ChannelBulk:=INVALID_HANDLE_VALUE;
     
     {Register DMA}
     Status:=DMAHostRegister(@BCM2710DMAHost.DMA);
     if Status = ERROR_SUCCESS then
      begin
       {Start DMA}
       Status:=DMAHostStart(@BCM2710DMAHost.DMA);
       if Status <> ERROR_SUCCESS then
        begin
         if DMA_LOG_ENABLED then DMALogError(nil,'BCM2710: Failed to start new DMA host: ' + ErrorToString(Status));
        end;
      end
     else
      begin
       if DMA_LOG_ENABLED then DMALogError(nil,'BCM2710: Failed to register new DMA host: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DMA_LOG_ENABLED then DMALogError(nil,'BCM2710: Failed to create new DMA host');
    end;
  end;
  
 {Create PCM}
 if BCM2710_REGISTER_PCM then
  begin
   //To Do
  end;
  
 {Create GPIO}
 if BCM2710_REGISTER_GPIO then
  begin
   BCM2710GPIO:=PBCM2710GPIODevice(GPIODeviceCreateEx(SizeOf(TBCM2710GPIODevice)));
   if BCM2710GPIO <> nil then
    begin
     {Update GPIO}
     {Device}
     BCM2710GPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710GPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
     BCM2710GPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP or GPIO_FLAG_PULL_DOWN or GPIO_FLAG_TRIGGER_LOW or GPIO_FLAG_TRIGGER_HIGH or GPIO_FLAG_TRIGGER_RISING or GPIO_FLAG_TRIGGER_FALLING or GPIO_FLAG_TRIGGER_ASYNC or GPIO_FLAG_TRIGGER_EDGE;
     BCM2710GPIO.GPIO.Device.DeviceData:=nil;
     BCM2710GPIO.GPIO.Device.DeviceDescription:=BCM2710_GPIO_DESCRIPTION;
     {GPIO}
     BCM2710GPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
     BCM2710GPIO.GPIO.DeviceStart:=BCM2710GPIOStart;
     BCM2710GPIO.GPIO.DeviceStop:=BCM2710GPIOStop;
     BCM2710GPIO.GPIO.DeviceRead:=BCM2710GPIORead;
     BCM2710GPIO.GPIO.DeviceWrite:=BCM2710GPIOWrite;
     BCM2710GPIO.GPIO.DeviceInputGet:=BCM2710GPIOInputGet;
     BCM2710GPIO.GPIO.DeviceInputWait:=BCM2710GPIOInputWait; 
     BCM2710GPIO.GPIO.DeviceInputEvent:=BCM2710GPIOInputEvent;
     BCM2710GPIO.GPIO.DeviceInputCancel:=BCM2710GPIOInputCancel;
     BCM2710GPIO.GPIO.DeviceOutputSet:=BCM2710GPIOOutputSet;
     BCM2710GPIO.GPIO.DevicePullSelect:=BCM2710GPIOPullSelect;  
     BCM2710GPIO.GPIO.DeviceFunctionGet:=BCM2710GPIOFunctionGet;
     BCM2710GPIO.GPIO.DeviceFunctionSelect:=BCM2710GPIOFunctionSelect;    
     {Driver}
     BCM2710GPIO.GPIO.Address:=Pointer(BCM2837_GPIO_REGS_BASE);
     BCM2710GPIO.GPIO.Properties.Flags:=BCM2710GPIO.GPIO.Device.DeviceFlags;
     BCM2710GPIO.GPIO.Properties.PinMin:=BCM2710_GPIO_MIN_PIN;
     BCM2710GPIO.GPIO.Properties.PinMax:=BCM2710_GPIO_MAX_PIN;
     BCM2710GPIO.GPIO.Properties.PinCount:=BCM2837_GPIO_PIN_COUNT;
     BCM2710GPIO.GPIO.Properties.FunctionMin:=BCM2710_GPIO_MIN_FUNCTION;
     BCM2710GPIO.GPIO.Properties.FunctionMax:=BCM2710_GPIO_MAX_FUNCTION;
     BCM2710GPIO.GPIO.Properties.FunctionCount:=8;
     {BCM2710}
     BCM2710GPIO.Lock:=INVALID_HANDLE_VALUE;
     
     {Register GPIO}
     Status:=GPIODeviceRegister(@BCM2710GPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Start GPIO}
       Status:=GPIODeviceStart(@BCM2710GPIO.GPIO);
       if Status <> ERROR_SUCCESS then
        begin
         if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2710: Failed to start new GPIO device: ' + ErrorToString(Status));
        end;
      end
     else
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2710: Failed to register new GPIO device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2710: Failed to create new GPIO device');
    end;
  end;

 {Create SPI0}
 if BCM2710_REGISTER_SPI0 then
  begin
   BCM2710SPI0:=PBCM2710SPI0Device(SPIDeviceCreateEx(SizeOf(TBCM2710SPI0Device)));
   if BCM2710SPI0 <> nil then
    begin
     {Update SPI0}
     {Device}
     BCM2710SPI0.SPI.Device.DeviceBus:=DEVICE_BUS_MMIO;
     BCM2710SPI0.SPI.Device.DeviceType:=SPI_TYPE_NONE;
     BCM2710SPI0.SPI.Device.DeviceFlags:=SPI_FLAG_4WIRE or SPI_FLAG_3WIRE or SPI_FLAG_LOSSI or SPI_FLAG_CPOL or SPI_FLAG_CPHA or SPI_FLAG_CSPOL or SPI_FLAG_NO_CS or SPI_FLAG_DMA;
     BCM2710SPI0.SPI.Device.DeviceData:=nil;
     BCM2710SPI0.SPI.Device.DeviceDescription:=BCM2710_SPI0_DESCRIPTION;
     {SPI}
     BCM2710SPI0.SPI.SPIState:=SPI_STATE_DISABLED;
     BCM2710SPI0.SPI.SPIMode:=SPI_MODE_4WIRE;
     BCM2710SPI0.SPI.DeviceStart:=BCM2710SPI0Start;
     BCM2710SPI0.SPI.DeviceStop:=BCM2710SPI0Stop;
     BCM2710SPI0.SPI.DeviceWriteRead:=BCM2710SPI0WriteRead;
     BCM2710SPI0.SPI.DeviceSetMode:=BCM2710SPI0SetMode;
     BCM2710SPI0.SPI.DeviceSetClockRate:=BCM2710SPI0SetClockRate;
     BCM2710SPI0.SPI.DeviceSetClockPhase:=BCM2710SPI0SetClockPhase;
     BCM2710SPI0.SPI.DeviceSetClockPolarity:=BCM2710SPI0SetClockPolarity;
     BCM2710SPI0.SPI.DeviceSetSelectPolarity:=BCM2710SPI0SetSelectPolarity;
     {Driver}
     BCM2710SPI0.SPI.Properties.Flags:=BCM2710SPI0.SPI.Device.DeviceFlags;
     BCM2710SPI0.SPI.Properties.MaxSize:=BCM2710_SPI0_MAX_SIZE;
     BCM2710SPI0.SPI.Properties.MinClock:=BCM2710_SPI0_MIN_CLOCK;
     BCM2710SPI0.SPI.Properties.MaxClock:=BCM2710_SPI0_MAX_CLOCK;
     BCM2710SPI0.SPI.Properties.SelectCount:=3;
     BCM2710SPI0.SPI.Properties.Mode:=SPI_MODE_4WIRE;
     BCM2710SPI0.SPI.Properties.ClockRate:=0;
     BCM2710SPI0.SPI.Properties.ClockPhase:=SPI_CLOCK_PHASE_UNKNOWN;
     BCM2710SPI0.SPI.Properties.ClockPolarity:=SPI_CLOCK_POLARITY_UNKNOWN;
     BCM2710SPI0.SPI.Properties.SelectPolarity:=SPI_CS_POLARITY_UNKNOWN;
     {BCM2710}
     BCM2710SPI0.Address:=Pointer(BCM2837_SPI0_REGS_BASE);
     BCM2710SPI0.CoreClock:=BCM2710_SPI0_CORE_CLOCK;
     
     {Register SPI0}
     Status:=SPIDeviceRegister(@BCM2710SPI0.SPI);
     if Status <> ERROR_SUCCESS then
      begin
       if SPI_LOG_ENABLED then SPILogError(nil,'BCM2710: Failed to register new SPI0 device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if SPI_LOG_ENABLED then SPILogError(nil,'BCM2710: Failed to create new SPI0 device');
    end; 
  end; 
  
 {Create I2C0}
 if BCM2710_REGISTER_I2C0 then
  begin
   BCM2710I2C0:=PBCM2710BSCI2CDevice(I2CDeviceCreateEx(SizeOf(TBCM2710BSCI2CDevice)));
   if BCM2710I2C0 <> nil then
    begin
     {Update I2C0}
     {Device}
     BCM2710I2C0.I2C.Device.DeviceBus:=DEVICE_BUS_MMIO;
     BCM2710I2C0.I2C.Device.DeviceType:=I2C_TYPE_NONE;
     BCM2710I2C0.I2C.Device.DeviceFlags:=I2C_FLAG_10BIT;
     BCM2710I2C0.I2C.Device.DeviceData:=nil;
     BCM2710I2C0.I2C.Device.DeviceDescription:=BCM2710_I2C0_DESCRIPTION;
     {I2C}
     BCM2710I2C0.I2C.I2CState:=I2C_STATE_DISABLED;
     BCM2710I2C0.I2C.DeviceStart:=BCM2710BSCI2CStart;
     BCM2710I2C0.I2C.DeviceStop:=BCM2710BSCI2CStop;
     BCM2710I2C0.I2C.DeviceRead:=BCM2710BSCI2CRead;
     BCM2710I2C0.I2C.DeviceWrite:=BCM2710BSCI2CWrite;
     BCM2710I2C0.I2C.DeviceWriteRead:=BCM2710BSCI2CWriteRead;
     BCM2710I2C0.I2C.DeviceWriteWrite:=BCM2710BSCI2CWriteWrite;
     BCM2710I2C0.I2C.DeviceSetRate:=BCM2710BSCI2CSetRate;
     BCM2710I2C0.I2C.DeviceSetAddress:=BCM2710BSCI2CSetAddress;
     {Driver}
     BCM2710I2C0.I2C.Properties.Flags:=BCM2710I2C0.I2C.Device.DeviceFlags;
     BCM2710I2C0.I2C.Properties.MaxSize:=BCM2710_BSCI2C_MAX_SIZE;
     BCM2710I2C0.I2C.Properties.MinClock:=BCM2710_BSCI2C_MIN_CLOCK;
     BCM2710I2C0.I2C.Properties.MaxClock:=BCM2710_BSCI2C_MAX_CLOCK;
     BCM2710I2C0.I2C.Properties.ClockRate:=0;
     BCM2710I2C0.I2C.Properties.SlaveAddress:=I2C_ADDRESS_INVALID;
     {BCM2710}
     BCM2710I2C0.Address:=Pointer(BCM2837_BSC0_REGS_BASE);
     BCM2710I2C0.CoreClock:=BCM2710_BSCI2C_CORE_CLOCK;
     BCM2710I2C0.SDAPin:=GPIO_PIN_0;
     BCM2710I2C0.SCLPin:=GPIO_PIN_1;
     BCM2710I2C0.SDAFunction:=GPIO_FUNCTION_ALT0;
     BCM2710I2C0.SCLFunction:=GPIO_FUNCTION_ALT0;
     {Transfer}
     BCM2710I2C0.Index:=0; {BSC0}
     
     {Register I2C0}
     Status:=I2CDeviceRegister(@BCM2710I2C0.I2C);
     if Status <> ERROR_SUCCESS then
      begin
       if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2710: Failed to register new I2C0 device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2710: Failed to create new I2C0 device');
    end; 
  end;

 {Create I2C1}
 if BCM2710_REGISTER_I2C1 then
  begin
   BCM2710I2C1:=PBCM2710BSCI2CDevice(I2CDeviceCreateEx(SizeOf(TBCM2710BSCI2CDevice)));
   if BCM2710I2C1 <> nil then
    begin
     {Update I2C1}
     {Device}
     BCM2710I2C1.I2C.Device.DeviceBus:=DEVICE_BUS_MMIO;
     BCM2710I2C1.I2C.Device.DeviceType:=I2C_TYPE_NONE;
     BCM2710I2C1.I2C.Device.DeviceFlags:=I2C_FLAG_10BIT;
     BCM2710I2C1.I2C.Device.DeviceData:=nil;
     BCM2710I2C1.I2C.Device.DeviceDescription:=BCM2710_I2C1_DESCRIPTION;
     {I2C}
     BCM2710I2C1.I2C.I2CState:=I2C_STATE_DISABLED;
     BCM2710I2C1.I2C.DeviceStart:=BCM2710BSCI2CStart;
     BCM2710I2C1.I2C.DeviceStop:=BCM2710BSCI2CStop;
     BCM2710I2C1.I2C.DeviceRead:=BCM2710BSCI2CRead;
     BCM2710I2C1.I2C.DeviceWrite:=BCM2710BSCI2CWrite;
     BCM2710I2C1.I2C.DeviceWriteRead:=BCM2710BSCI2CWriteRead;
     BCM2710I2C1.I2C.DeviceWriteWrite:=BCM2710BSCI2CWriteWrite;
     BCM2710I2C1.I2C.DeviceSetRate:=BCM2710BSCI2CSetRate;
     BCM2710I2C1.I2C.DeviceSetAddress:=BCM2710BSCI2CSetAddress;
     {Driver}
     BCM2710I2C1.I2C.Properties.Flags:=BCM2710I2C1.I2C.Device.DeviceFlags;
     BCM2710I2C1.I2C.Properties.MaxSize:=BCM2710_BSCI2C_MAX_SIZE;
     BCM2710I2C1.I2C.Properties.MinClock:=BCM2710_BSCI2C_MIN_CLOCK;
     BCM2710I2C1.I2C.Properties.MaxClock:=BCM2710_BSCI2C_MAX_CLOCK;
     BCM2710I2C1.I2C.Properties.ClockRate:=0;
     BCM2710I2C1.I2C.Properties.SlaveAddress:=I2C_ADDRESS_INVALID;
     {BCM2710}
     BCM2710I2C1.Address:=Pointer(BCM2837_BSC1_REGS_BASE);
     BCM2710I2C1.CoreClock:=BCM2710_BSCI2C_CORE_CLOCK;
     BCM2710I2C1.SDAPin:=GPIO_PIN_2;
     BCM2710I2C1.SCLPin:=GPIO_PIN_3;
     BCM2710I2C1.SDAFunction:=GPIO_FUNCTION_ALT0;
     BCM2710I2C1.SCLFunction:=GPIO_FUNCTION_ALT0;
     {Transfer}
     BCM2710I2C1.Index:=1; {BSC1}
     
     {Register I2C1}
     Status:=I2CDeviceRegister(@BCM2710I2C1.I2C);
     if Status <> ERROR_SUCCESS then
      begin
       if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2710: Failed to register new I2C1 device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if I2C_LOG_ENABLED then I2CLogError(nil,'BCM2710: Failed to create new I2C1 device');
    end; 
  end;
  
 {Create PWM0}
 if BCM2710_REGISTER_PWM then
  begin
   BCM2710PWM0:=PBCM2710PWMDevice(PWMDeviceCreateEx(SizeOf(TBCM2710PWMDevice)));
   if BCM2710PWM0 <> nil then
    begin
     {Update PWM0}
     {Device}
     BCM2710PWM0.PWM.Device.DeviceBus:=DEVICE_BUS_MMIO;
     BCM2710PWM0.PWM.Device.DeviceType:=PWM_TYPE_NONE;
     BCM2710PWM0.PWM.Device.DeviceFlags:=PWM_FLAG_GPIO or PWM_FLAG_MODE or PWM_FLAG_RANGE or PWM_FLAG_FREQUENCY or PWM_FLAG_POLARITY;
     BCM2710PWM0.PWM.Device.DeviceData:=nil;
     BCM2710PWM0.PWM.Device.DeviceDescription:=BCM2710_PWM0_DESCRIPTION;
     {PWM}
     BCM2710PWM0.PWM.PWMState:=PWM_STATE_DISABLED;
     BCM2710PWM0.PWM.DeviceStart:=BCM2710PWMStart;
     BCM2710PWM0.PWM.DeviceStop:=BCM2710PWMStop;
     BCM2710PWM0.PWM.DeviceWrite:=BCM2710PWMWrite;
     BCM2710PWM0.PWM.DeviceSetGPIO:=BCM2710PWMSetGPIO;
     BCM2710PWM0.PWM.DeviceSetMode:=BCM2710PWMSetMode;
     BCM2710PWM0.PWM.DeviceSetRange:=BCM2710PWMSetRange;
     BCM2710PWM0.PWM.DeviceSetFrequency:=BCM2710PWMSetFrequency;
     BCM2710PWM0.PWM.DeviceSetPolarity:=BCM2710PWMSetPolarity;
     BCM2710PWM0.PWM.DeviceConfigure:=BCM2710PWMConfigure;
     {Driver}
     BCM2710PWM0.PWM.Properties.Flags:=BCM2710PWM0.PWM.Device.DeviceFlags;
     BCM2710PWM0.PWM.Properties.GPIO:=GPIO_PIN_UNKNOWN;
     BCM2710PWM0.PWM.Properties.Mode:=PWM_MODE_MARKSPACE;
     BCM2710PWM0.PWM.Properties.Range:=0;
     BCM2710PWM0.PWM.Properties.Frequency:=0;
     BCM2710PWM0.PWM.Properties.Polarity:=PWM_POLARITY_NORMAL;
     BCM2710PWM0.PWM.Properties.DutyNS:=0;
     BCM2710PWM0.PWM.Properties.PeriodNS:=0;
     BCM2710PWM0.PWM.Properties.MinPeriod:=BCM2710_PWM_MIN_PERIOD;
     {BCM2710}
     BCM2710PWM0.Address:=Pointer(BCM2837_PWM_REGS_BASE);
     BCM2710PWM0.Channel:=0; {PWM0 (PWM Channel 1)}
     
     {Register PWM0}
     Status:=PWMDeviceRegister(@BCM2710PWM0.PWM);
     if Status <> ERROR_SUCCESS then
      begin
       if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2710: Failed to register new PWM0 device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2710: Failed to create new PWM0 device');
    end; 
  end;

 {Create PWM1}
 if BCM2710_REGISTER_PWM then
  begin
   BCM2710PWM1:=PBCM2710PWMDevice(PWMDeviceCreateEx(SizeOf(TBCM2710PWMDevice)));
   if BCM2710PWM1 <> nil then
    begin
     {Update PWM1}
     {Device}
     BCM2710PWM1.PWM.Device.DeviceBus:=DEVICE_BUS_MMIO;
     BCM2710PWM1.PWM.Device.DeviceType:=PWM_TYPE_NONE;
     BCM2710PWM1.PWM.Device.DeviceFlags:=PWM_FLAG_GPIO or PWM_FLAG_MODE or PWM_FLAG_RANGE or PWM_FLAG_FREQUENCY or PWM_FLAG_POLARITY;
     BCM2710PWM1.PWM.Device.DeviceData:=nil;
     BCM2710PWM1.PWM.Device.DeviceDescription:=BCM2710_PWM1_DESCRIPTION;
     {PWM}
     BCM2710PWM1.PWM.PWMState:=PWM_STATE_DISABLED;
     BCM2710PWM1.PWM.DeviceStart:=BCM2710PWMStart;
     BCM2710PWM1.PWM.DeviceStop:=BCM2710PWMStop;
     BCM2710PWM1.PWM.DeviceWrite:=BCM2710PWMWrite;
     BCM2710PWM1.PWM.DeviceSetGPIO:=BCM2710PWMSetGPIO;
     BCM2710PWM1.PWM.DeviceSetMode:=BCM2710PWMSetMode;
     BCM2710PWM1.PWM.DeviceSetRange:=BCM2710PWMSetRange;
     BCM2710PWM1.PWM.DeviceSetFrequency:=BCM2710PWMSetFrequency;
     BCM2710PWM1.PWM.DeviceSetPolarity:=BCM2710PWMSetPolarity;
     BCM2710PWM1.PWM.DeviceConfigure:=BCM2710PWMConfigure;
     {Driver}
     BCM2710PWM1.PWM.Properties.Flags:=BCM2710PWM1.PWM.Device.DeviceFlags;
     BCM2710PWM1.PWM.Properties.GPIO:=GPIO_PIN_UNKNOWN;
     BCM2710PWM1.PWM.Properties.Mode:=PWM_MODE_MARKSPACE;
     BCM2710PWM1.PWM.Properties.Range:=0;
     BCM2710PWM1.PWM.Properties.Frequency:=0;
     BCM2710PWM1.PWM.Properties.Polarity:=PWM_POLARITY_NORMAL;
     BCM2710PWM1.PWM.Properties.DutyNS:=0;
     BCM2710PWM1.PWM.Properties.PeriodNS:=0;
     BCM2710PWM1.PWM.Properties.MinPeriod:=BCM2710_PWM_MIN_PERIOD;
     {BCM2710}
     BCM2710PWM1.Address:=Pointer(BCM2837_PWM_REGS_BASE);
     BCM2710PWM1.Channel:=1; {PWM1 (PWM Channel 2)}
     BCM2710PWM1.Pair:=BCM2710PWM0;
     
     {Update PWM0 (Pair)}
     if BCM2710PWM0 <> nil then
      begin
       BCM2710PWM0.Pair:=BCM2710PWM1;
      end;
      
     {Register PWM1}
     Status:=PWMDeviceRegister(@BCM2710PWM1.PWM);
     if Status <> ERROR_SUCCESS then
      begin
       if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2710: Failed to register new PWM1 device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if PWM_LOG_ENABLED then PWMLogError(nil,'BCM2710: Failed to create new PWM1 device');
    end; 
  end;
  
 {Create PWM Audio}
 if BCM2710_REGISTER_PWMAUDIO then
  begin
   //To Do
   
   //To Do //BCM2710PWM0.Audio:=
   //To Do //BCM2710PWM1.Audio:=
   //To Do //BCM2710PWMAudio.PWM0/PWM1:=
  end;
  
 {Create UART0}
 if BCM2710_REGISTER_UART0 then
  begin
   BCM2710UART0:=PBCM2710UART0Device(UARTDeviceCreateEx(SizeOf(TBCM2710UART0Device)));
   if BCM2710UART0 <> nil then
    begin
     {Update UART0}
     {Device}
     BCM2710UART0.UART.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710UART0.UART.Device.DeviceType:=UART_TYPE_16650;
     BCM2710UART0.UART.Device.DeviceFlags:=UART_FLAG_DATA_8BIT or UART_FLAG_DATA_7BIT or UART_FLAG_DATA_6BIT or UART_FLAG_DATA_5BIT or UART_FLAG_STOP_1BIT or UART_FLAG_STOP_2BIT or UART_FLAG_PARITY_ODD or UART_FLAG_PARITY_EVEN or UART_FLAG_FLOW_RTS_CTS or UART_FLAG_PUSH_RX;
     BCM2710UART0.UART.Device.DeviceData:=nil;
     BCM2710UART0.UART.Device.DeviceDescription:=BCM2710_UART0_DESCRIPTION;
     {UART}
     BCM2710UART0.UART.UARTMode:=UART_MODE_NONE;
     BCM2710UART0.UART.UARTState:=UART_STATE_DISABLED;
     BCM2710UART0.UART.UARTStatus:=UART_STATUS_NONE;
     BCM2710UART0.UART.DeviceOpen:=BCM2710UART0Open;
     BCM2710UART0.UART.DeviceClose:=BCM2710UART0Close;
     BCM2710UART0.UART.DeviceRead:=BCM2710UART0Read;
     BCM2710UART0.UART.DeviceWrite:=BCM2710UART0Write;
     BCM2710UART0.UART.DeviceGetStatus:=BCM2710UART0GetStatus;
     BCM2710UART0.UART.DeviceSetStatus:=BCM2710UART0SetStatus;
     {Driver}
     BCM2710UART0.UART.Properties.Flags:=BCM2710UART0.UART.Device.DeviceFlags;
     BCM2710UART0.UART.Properties.MinRate:=BCM2710_UART0_MIN_BAUD;
     BCM2710UART0.UART.Properties.MaxRate:=BCM2710_UART0_MAX_BAUD;
     BCM2710UART0.UART.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
     BCM2710UART0.UART.Properties.DataBits:=SERIAL_DATA_8BIT;
     BCM2710UART0.UART.Properties.StopBits:=SERIAL_STOP_1BIT;
     BCM2710UART0.UART.Properties.Parity:=SERIAL_PARITY_NONE;
     BCM2710UART0.UART.Properties.FlowControl:=SERIAL_FLOW_NONE;
     {BCM2710}
     BCM2710UART0.Lock:=INVALID_HANDLE_VALUE;
     BCM2710UART0.Address:=Pointer(BCM2837_PL011_REGS_BASE);
     BCM2710UART0.ClockRate:=BCM2710_UART0_CLOCK_RATE;
     
     {Register UART0}
     Status:=UARTDeviceRegister(@BCM2710UART0.UART);
     if Status <> ERROR_SUCCESS then
      begin
       if UART_LOG_ENABLED then UARTLogError(nil,'BCM2710: Failed to register new UART0 device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if UART_LOG_ENABLED then UARTLogError(nil,'BCM2710: Failed to create new UART0 device');
    end;
  end;

 {Create UART1}
 if BCM2710_REGISTER_UART1 then
  begin
   //To Do
  end;
  
 {Create SDHCI}
 if BCM2710_REGISTER_SDHCI then
  begin
   BCM2710SDHCIHost:=PBCM2710SDHCIHost(SDHCIHostCreateEx(SizeOf(TBCM2710SDHCIHost)));
   if BCM2710SDHCIHost <> nil then
    begin
     {Update SDHCI}
     {Device}
     BCM2710SDHCIHost.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710SDHCIHost.SDHCI.Device.DeviceType:=SDHCI_TYPE_SD;
     BCM2710SDHCIHost.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_AUTO_CMD12 or SDHCI_FLAG_AUTO_CMD23;
     BCM2710SDHCIHost.SDHCI.Device.DeviceData:=nil;
     BCM2710SDHCIHost.SDHCI.Device.DeviceDescription:=BCM2710_EMMC_DESCRIPTION;
     {SDHCI}
     BCM2710SDHCIHost.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
     BCM2710SDHCIHost.SDHCI.HostStart:=BCM2710SDHCIHostStart;
     BCM2710SDHCIHost.SDHCI.HostStop:=BCM2710SDHCIHostStop;
     BCM2710SDHCIHost.SDHCI.HostReadByte:=BCM2710SDHCIHostReadByte;
     BCM2710SDHCIHost.SDHCI.HostReadWord:=BCM2710SDHCIHostReadWord;
     BCM2710SDHCIHost.SDHCI.HostReadLong:=BCM2710SDHCIHostReadLong;
     BCM2710SDHCIHost.SDHCI.HostWriteByte:=BCM2710SDHCIHostWriteByte;
     BCM2710SDHCIHost.SDHCI.HostWriteWord:=BCM2710SDHCIHostWriteWord;
     BCM2710SDHCIHost.SDHCI.HostWriteLong:=BCM2710SDHCIHostWriteLong;
     BCM2710SDHCIHost.SDHCI.HostSetClockDivider:=nil;
     BCM2710SDHCIHost.SDHCI.HostSetControlRegister:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceInitialize:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceDeinitialize:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceGetCardDetect:=BCM2710MMCDeviceGetCardDetect;
     BCM2710SDHCIHost.SDHCI.DeviceGetWriteProtect:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceSendCommand:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceSetIOS:=nil;
     {Driver}
     BCM2710SDHCIHost.SDHCI.Address:=Pointer(BCM2837_SDHCI_REGS_BASE);
   
     {Register SDHCI}
     Status:=SDHCIHostRegister(@BCM2710SDHCIHost.SDHCI);
     if Status <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2710: Failed to register new SDHCI host: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2710: Failed to create new SDHCI host');
    end;
  end;

 {Create System Clock}
 if BCM2710_REGISTER_SYS_CLOCK then
  begin
   BCM2710SystemClock:=PBCM2710SystemClock(ClockDeviceCreateEx(SizeOf(TBCM2710SystemClock)));
   if BCM2710SystemClock <> nil then
    begin
     {Update Clock}
     {Device}
     BCM2710SystemClock.Clock.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710SystemClock.Clock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     BCM2710SystemClock.Clock.Device.DeviceFlags:=CLOCK_FLAG_NONE;
     BCM2710SystemClock.Clock.Device.DeviceData:=nil;
     BCM2710SystemClock.Clock.Device.DeviceDescription:=BCM2710_SYS_CLOCK_DESCRIPTION;
     {Clock}
     BCM2710SystemClock.Clock.ClockState:=CLOCK_STATE_DISABLED;
     BCM2710SystemClock.Clock.DeviceRead:=BCM2710SystemClockRead;
     BCM2710SystemClock.Clock.DeviceRead64:=BCM2710SystemClockRead64;
     {Driver}
     BCM2710SystemClock.Clock.Address:=Pointer(BCM2837_SYSTEM_TIMER_REGS_BASE);
     BCM2710SystemClock.Clock.Rate:=BCM2837_SYSTEM_TIMER_FREQUENCY;
     BCM2710SystemClock.Clock.MinRate:=BCM2837_SYSTEM_TIMER_FREQUENCY;
     BCM2710SystemClock.Clock.MaxRate:=BCM2837_SYSTEM_TIMER_FREQUENCY;
    
     {Register Clock}
     Status:=ClockDeviceRegister(@BCM2710SystemClock.Clock);
     if Status = ERROR_SUCCESS then
      begin
       {Start Clock}
       Status:=ClockDeviceStart(@BCM2710SystemClock.Clock);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to start new clock device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new clock device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new clock device');
    end;
  end;
  
 {Create ARM Clock}
 if BCM2710_REGISTER_ARM_CLOCK then
  begin
   BCM2710ARMClock:=PBCM2710ARMClock(ClockDeviceCreateEx(SizeOf(TBCM2710ARMClock)));
   if BCM2710ARMClock <> nil then
    begin
     {Update ARM Clock}
     {Device}
     BCM2710ARMClock.Clock.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710ARMClock.Clock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     BCM2710ARMClock.Clock.Device.DeviceFlags:=CLOCK_FLAG_VARIABLE;
     BCM2710ARMClock.Clock.Device.DeviceData:=nil;
     BCM2710ARMClock.Clock.Device.DeviceDescription:=BCM2710_ARM_CLOCK_DESCRIPTION;
     {Clock}
     BCM2710ARMClock.Clock.ClockState:=CLOCK_STATE_DISABLED;
     BCM2710ARMClock.Clock.DeviceStart:=BCM2710ARMClockStart;
     BCM2710ARMClock.Clock.DeviceStop:=BCM2710ARMClockStop;
     BCM2710ARMClock.Clock.DeviceRead:=BCM2710ARMClockRead;
     BCM2710ARMClock.Clock.DeviceRead64:=BCM2710ARMClockRead64;
     BCM2710ARMClock.Clock.DeviceSetRate:=BCM2710ARMClockSetRate;
     {Driver}
     BCM2710ARMClock.Clock.Address:=Pointer(BCM2837_TIMER_REGS_BASE);
     BCM2710ARMClock.Clock.Rate:=BCM2710_ARM_CLOCK_DEFAULT_RATE;
     BCM2710ARMClock.Clock.MinRate:=BCM2710_ARM_CLOCK_MIN_RATE;
     BCM2710ARMClock.Clock.MaxRate:=BCM2710_ARM_CLOCK_MAX_RATE;
     {BCM2710}
     BCM2710ARMClock.CoreClock:=BCM2710_ARM_CLOCK_CORE_CLOCK;
     
     {Register Clock}
     Status:=ClockDeviceRegister(@BCM2710ARMClock.Clock);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new clock device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new clock device');
    end;
  end;
  
 {Create ARM Timer}
 if BCM2710_REGISTER_ARM_TIMER then
  begin
   BCM2710ARMTimer:=PBCM2710ARMTimer(TimerDeviceCreateEx(SizeOf(TBCM2710ARMTimer)));
   if BCM2710ARMTimer <> nil then
    begin
     {Update Timer}
     {Device}
     BCM2710ARMTimer.Timer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710ARMTimer.Timer.Device.DeviceType:=TIMER_TYPE_HARDWARE;
     BCM2710ARMTimer.Timer.Device.DeviceFlags:=TIMER_FLAG_WRAPPING or TIMER_FLAG_COUNTER or TIMER_FLAG_DOWN;
     BCM2710ARMTimer.Timer.Device.DeviceData:=nil;
     BCM2710ARMTimer.Timer.Device.DeviceDescription:=BCM2710_ARM_TIMER_DESCRIPTION;
     {Timer}
     BCM2710ARMTimer.Timer.TimerState:=TIMER_STATE_DISABLED;
     BCM2710ARMTimer.Timer.DeviceStart:=BCM2710ARMTimerStart;
     BCM2710ARMTimer.Timer.DeviceStop:=BCM2710ARMTimerStop;
     BCM2710ARMTimer.Timer.DeviceRead64:=BCM2710ARMTimerRead64;
     BCM2710ARMTimer.Timer.DeviceWait:=BCM2710ARMTimerWait;
     BCM2710ARMTimer.Timer.DeviceEvent:=BCM2710ARMTimerEvent;
     BCM2710ARMTimer.Timer.DeviceCancel:=BCM2710ARMTimerCancel;
     BCM2710ARMTimer.Timer.DeviceSetRate:=BCM2710ARMTimerSetRate;
     BCM2710ARMTimer.Timer.DeviceSetInterval:=BCM2710ARMTimerSetInterval;
     {Driver}
     BCM2710ARMTimer.Timer.Address:=Pointer(BCM2837_TIMER_REGS_BASE);
     BCM2710ARMTimer.Timer.Rate:=BCM2710_ARM_TIMER_DEFAULT_RATE;
     BCM2710ARMTimer.Timer.Interval:=0;
     BCM2710ARMTimer.Timer.Properties.Flags:=BCM2710ARMTimer.Timer.Device.DeviceFlags;
     BCM2710ARMTimer.Timer.Properties.Bits:=32;
     BCM2710ARMTimer.Timer.Properties.MinRate:=BCM2710_ARM_TIMER_MIN_RATE;
     BCM2710ARMTimer.Timer.Properties.MaxRate:=BCM2710_ARM_TIMER_MAX_RATE;
     BCM2710ARMTimer.Timer.Properties.MinInterval:=BCM2710_ARM_TIMER_MIN_INTERVAL;
     BCM2710ARMTimer.Timer.Properties.MaxInterval:=BCM2710_ARM_TIMER_MAX_INTERVAL;
     {BCM2710}
     BCM2710ARMTimer.CoreClock:=BCM2710_ARM_TIMER_CORE_CLOCK;
     
     {Register Timer}
     Status:=TimerDeviceRegister(@BCM2710ARMTimer.Timer);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new timer device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new timer device');
    end;
  end; 

 {Create Local Timer}
 if BCM2710_REGISTER_LOCAL_TIMER then
  begin
   //To Do
  end; 
 
 {Create Random}
 if BCM2710_REGISTER_RANDOM then
  begin
   BCM2710Random:=PBCM2710Random(RandomDeviceCreateEx(SizeOf(TBCM2710Random)));
   if BCM2710Random <> nil then
    begin
     {Update Random}
     {Device}
     BCM2710Random.Random.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710Random.Random.Device.DeviceType:=RANDOM_TYPE_HARDWARE;
     BCM2710Random.Random.Device.DeviceFlags:=RANDOM_FLAG_NONE;
     BCM2710Random.Random.Device.DeviceData:=nil;
     BCM2710Random.Random.Device.DeviceDescription:=BCM2710_RANDOM_DESCRIPTION;
     {Random}
     BCM2710Random.Random.RandomState:=RANDOM_STATE_DISABLED;
     BCM2710Random.Random.DeviceStart:=BCM2710RandomStart;
     BCM2710Random.Random.DeviceStop:=BCM2710RandomStop;
     BCM2710Random.Random.DeviceReadLongWord:=BCM2710RandomReadLongWord;
     {Driver}
     BCM2710Random.Random.Address:=Pointer(BCM2837_RNG_REGS_BASE);
     
     {Register Random}
     Status:=RandomDeviceRegister(@BCM2710Random.Random);
     if Status = ERROR_SUCCESS then
      begin
       {Start Random}
       Status:=RandomDeviceStart(@BCM2710Random.Random);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to start new random device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new random device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new random device');
    end;
  end;
  
 {Create Mailbox}
 if BCM2710_REGISTER_MAILBOX then
  begin
   //To Do
  end; 
  
 {Create Watchdog}
 if BCM2710_REGISTER_WATCHDOG then
  begin
   BCM2710Watchdog:=PBCM2710Watchdog(WatchdogDeviceCreateEx(SizeOf(TBCM2710Watchdog)));
   if BCM2710Watchdog <> nil then
    begin
     {Device}
     BCM2710Watchdog.Watchdog.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710Watchdog.Watchdog.Device.DeviceType:=WATCHDOG_TYPE_HARDWARE;
     BCM2710Watchdog.Watchdog.Device.DeviceFlags:=WATCHDOG_FLAG_NONE;
     BCM2710Watchdog.Watchdog.Device.DeviceData:=nil;
     BCM2710Watchdog.Watchdog.Device.DeviceDescription:=BCM2710_WATCHDOG_DESCRIPTION;
     {Watchdog}
     BCM2710Watchdog.Watchdog.WatchdogState:=WATCHDOG_STATE_DISABLED;
     BCM2710Watchdog.Watchdog.DeviceStart:=BCM2710WatchdogStart;
     BCM2710Watchdog.Watchdog.DeviceStop:=BCM2710WatchdogStop;
     BCM2710Watchdog.Watchdog.DeviceRefresh:=BCM2710WatchdogRefresh;
     BCM2710Watchdog.Watchdog.DeviceGetRemain:=BCM2710WatchdogGetRemain;
     {Driver}
     BCM2710Watchdog.Watchdog.Address:=Pointer(BCM2837_PM_REGS_BASE);
     
     {Register Watchdog}
     Status:=WatchdogDeviceRegister(@BCM2710Watchdog.Watchdog);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new watchdog device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new watchdog device');
    end;
  end;
 
 {$IFNDEF CONSOLE_EARLY_INIT}
 {Create Framebuffer}
 if BCM2710_REGISTER_FRAMEBUFFER then
  begin
   BCM2710Framebuffer:=PBCM2710Framebuffer(FramebufferDeviceCreateEx(SizeOf(TBCM2710Framebuffer)));
   if BCM2710Framebuffer <> nil then
    begin
     {Device}
     BCM2710Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
     BCM2710Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_BACKLIGHT or FRAMEBUFFER_FLAG_VIRTUAL or FRAMEBUFFER_FLAG_OFFSETX or FRAMEBUFFER_FLAG_OFFSETY or FRAMEBUFFER_FLAG_SYNC or FRAMEBUFFER_FLAG_CURSOR;
     BCM2710Framebuffer.Framebuffer.Device.DeviceData:=nil;
     BCM2710Framebuffer.Framebuffer.Device.DeviceDescription:=BCM2710_FRAMEBUFFER_DESCRIPTION;
     {Framebuffer}
     BCM2710Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
     BCM2710Framebuffer.Framebuffer.DeviceAllocate:=BCM2710FramebufferAllocate;
     BCM2710Framebuffer.Framebuffer.DeviceRelease:=BCM2710FramebufferRelease;
     BCM2710Framebuffer.Framebuffer.DeviceBlank:=BCM2710FramebufferBlank;
     BCM2710Framebuffer.Framebuffer.DeviceCommit:=BCM2710FramebufferCommit;
     BCM2710Framebuffer.Framebuffer.DeviceWaitSync:=BCM2710FramebufferWaitSync;
     BCM2710Framebuffer.Framebuffer.DeviceSetOffset:=BCM2710FramebufferSetOffset;
     BCM2710Framebuffer.Framebuffer.DeviceGetPalette:=BCM2710FramebufferGetPalette;
     BCM2710Framebuffer.Framebuffer.DeviceSetPalette:=BCM2710FramebufferSetPalette;
     BCM2710Framebuffer.Framebuffer.DeviceSetBacklight:=BCM2710FramebufferSetBacklight;
     BCM2710Framebuffer.Framebuffer.DeviceSetCursor:=BCM2710FramebufferSetCursor;
     BCM2710Framebuffer.Framebuffer.DeviceUpdateCursor:=BCM2710FramebufferUpdateCursor;
     BCM2710Framebuffer.Framebuffer.DeviceSetProperties:=BCM2710FramebufferSetProperties;
     {Driver}
     
     {Setup Flags}
     if BCM2710FRAMEBUFFER_CACHED then BCM2710Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2710Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_COMMIT;
     if BCM2710FRAMEBUFFER_CACHED then BCM2710Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2710Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
     {if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then BCM2710Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2710Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;} {Handled by FramebufferAllocate}
     
     {Register Framebuffer}
     Status:=FramebufferDeviceRegister(@BCM2710Framebuffer.Framebuffer);
     if Status = ERROR_SUCCESS then
      begin
       {Allocate Framebuffer}
       Status:=FramebufferDeviceAllocate(@BCM2710Framebuffer.Framebuffer,nil);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
        end;
      end
     else
      begin     
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new framebuffer device');
    end;
  end;
 {$ENDIF}
 
 BCM2710Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{BCM2710 Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 SPI0 Functions}
function BCM2710SPI0Start(SPI:PSPIDevice;Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
var
 Control:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Start (Mode=' + SPIModeToString(Mode) + ' ClockRate=' + IntToStr(ClockRate) + ' ClockPhase=' + SPIClockPhaseToString(ClockPhase) + ' ClockPolarity=' + SPIClockPolarityToString(ClockPolarity) + ')');
 {$ENDIF}
 
 {Update Core Clock}
 PBCM2710SPI0Device(SPI).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
 if PBCM2710SPI0Device(SPI).CoreClock = 0 then PBCM2710SPI0Device(SPI).CoreClock:=BCM2710_SPI0_CORE_CLOCK;

 {Update Properties}
 SPI.Properties.MinClock:=PBCM2710SPI0Device(SPI).CoreClock div BCM2710_SPI0_MAX_DIVIDER;
 SPI.Properties.MaxClock:=PBCM2710SPI0Device(SPI).CoreClock div BCM2710_SPI0_MIN_DIVIDER;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710:  CoreClock=' + IntToStr(PBCM2710SPI0Device(SPI).CoreClock) + ' MinClock=' + IntToStr(SPI.Properties.MinClock) + ' MaxClock=' + IntToStr(SPI.Properties.MaxClock));
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
 GPIOFunctionSelect(GPIO_PIN_7,GPIO_FUNCTION_ALT0);
 GPIOFunctionSelect(GPIO_PIN_8,GPIO_FUNCTION_ALT0);
 GPIOFunctionSelect(GPIO_PIN_9,GPIO_FUNCTION_ALT0);
 GPIOFunctionSelect(GPIO_PIN_10,GPIO_FUNCTION_ALT0);
 GPIOFunctionSelect(GPIO_PIN_11,GPIO_FUNCTION_ALT0);
 
 {Get Divider}
 Divider:=PBCM2710SPI0Device(SPI).CoreClock div ClockRate;
 if (Divider and 1) <> 0 then Inc(Divider);

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710:  ClockRate=' + IntToStr(ClockRate) + ' Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control and Status} 
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX;

 {Setup Control}
 Control:=0;
 
 {Set Mode}
 case Mode of
  SPI_MODE_LOSSI:Control:=Control or BCM2837_SPI0_CS_LEN;
 end;
 
 {Set Clock Phase}
 if ClockPhase = SPI_CLOCK_PHASE_HIGH then Control:=Control or BCM2837_SPI0_CS_CPHA;
 
 {Set Clock Polarity}
 if ClockPolarity = SPI_CLOCK_POLARITY_HIGH then Control:=Control or BCM2837_SPI0_CS_CPOL;
 
 {Set Clock Divider}
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CLK:=(Divider and BCM2837_SPI0_CLK_CDIV);

 {Set Control and Status}
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
 
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
 RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_SPI,TInterruptHandler(BCM2710SPI0InterruptHandler),SPI);
 
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

function BCM2710SPI0Stop(SPI:PSPIDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Stop');
 {$ENDIF}
 
 {Release IRQ}
 ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_SPI,TInterruptHandler(BCM2710SPI0InterruptHandler),SPI);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control and Status} 
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Destroy Wait Semaphore}
 SemaphoreDestroy(SPI.Wait);
 
 {Reset Transfer}
 PBCM2710SPI0Device(SPI).Mode:=BCM2710_SPI0_MODE_IRQ;
 PBCM2710SPI0Device(SPI).Source:=nil;
 PBCM2710SPI0Device(SPI).Dest:=nil;
 PBCM2710SPI0Device(SPI).Count:=0;
 PBCM2710SPI0Device(SPI).SourceRemain:=0;
 PBCM2710SPI0Device(SPI).DestRemain:=0;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710SPI0WriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
var
 CSData:TDMAData;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Write Read (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Size}
 if Size > BCM2710_SPI0_MAX_SIZE then Exit;
 
 {Check Chip Select}
 if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_2) then Exit;
 
 {Update Statistics}
 Inc(SPI.TransferCount);
 
 {Write from Source / Read to Dest}
 if Size > 0 then
  begin
   {Setup Data}
   PBCM2710SPI0Device(SPI).Source:=Source;
   PBCM2710SPI0Device(SPI).Dest:=Dest;
   PBCM2710SPI0Device(SPI).Count:=0;
   PBCM2710SPI0Device(SPI).SourceRemain:=Size;
   PBCM2710SPI0Device(SPI).DestRemain:=Size;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Get Control and Status}
   Control:=PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS and not(BCM2837_SPI0_CS_CS_MASK);
   
   {Set Mode}
   if (SPI.SPIMode = SPI_MODE_3WIRE) and (Dest <> nil) then
    begin
     Control:=Control or BCM2837_SPI0_CS_REN;
    end
   else  
    begin
     Control:=Control and not(BCM2837_SPI0_CS_REN);
    end;
   
   {Set Chip Select}
   if ChipSelect = SPI_CS_NONE then
    begin
     Control:=Control or (BCM2837_SPI0_CS_CS_MASK); {Select the reserved value}
    end
   else
    begin
     Control:=Control or (ChipSelect and BCM2837_SPI0_CS_CS_MASK);
    end;
   
   {Check Clock Rate}
   if (ChipSelect = SPI_CS_NONE) or (SPI.ChipSelects[ChipSelect].ClockRate = 0) then
    begin
     {Set Clock Divider}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CLK:=(SPI.Divider and BCM2837_SPI0_CLK_CDIV);
    end
   else 
    begin
     {Set Clock Divider}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CLK:=(SPI.ChipSelects[ChipSelect].Divider and BCM2837_SPI0_CLK_CDIV);
    end;
      
   {Check Flags}   
   if (Flags and SPI_TRANSFER_DMA) <> 0 then
    begin
     {Update Data}
     PBCM2710SPI0Device(SPI).Mode:=BCM2710_SPI0_MODE_DMA;
    
     {Check Cache}
     if not(DMA_CACHE_COHERENT) and (Dest <> nil) then
      begin
       {Clean Cache (Dest)}
       CleanDataCacheRange(LongWord(Dest),Size);
      end;
     
     {Setup Control Data (CS/DLEN)}
     FillChar(CSData,SizeOf(TDMAData),0);
     CSData.Source:=@Control;
     CSData.Dest:=@PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).FIFO;
     CSData.Flags:=DMA_DATA_FLAG_DEST_NOINCREMENT or DMA_DATA_FLAG_DEST_DREQ or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_NOINVALIDATE;
     CSData.StrideLength:=0;
     CSData.SourceStride:=0;
     CSData.DestStride:=0;
     CSData.Size:=SizeOf(LongWord);
     CSData.Next:=@TXData;
     
     {Setup TX Data}
     FillChar(TXData,SizeOf(TDMAData),0);
     TXData.Source:=Source;
     TXData.Dest:=@PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).FIFO;
     TXData.Flags:=DMA_DATA_FLAG_DEST_NOINCREMENT or DMA_DATA_FLAG_DEST_DREQ or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_NOINVALIDATE;
     if Source = nil then TXData.Flags:=TXData.Flags or DMA_DATA_FLAG_NOREAD or DMA_DATA_FLAG_NOCLEAN;
     TXData.StrideLength:=0;
     TXData.SourceStride:=0;
     TXData.DestStride:=0;
     TXData.Size:=Size;
     
     {Setup RX Data}
     FillChar(RXData,SizeOf(TDMAData),0);
     RXData.Source:=@PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).FIFO;
     RXData.Dest:=Dest;
     RXData.Flags:=DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_DREQ or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN;
     if Dest = nil then RXData.Flags:=RXData.Flags or DMA_DATA_FLAG_NOWRITE or DMA_DATA_FLAG_NOINVALIDATE;
     RXData.StrideLength:=0;
     RXData.SourceStride:=0;
     RXData.DestStride:=0;
     RXData.Size:=Size;
     
     {Set Control (Deassert/DMA/Clear)}
     Control:=Control or (BCM2837_SPI0_CS_ADCS or BCM2837_SPI0_CS_DMAEN or BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);
     
     {Set Control and Status}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
     
     {Update Control (Active/Length)}
     Control:=(Size shl 16) or (Control and $FF) or BCM2837_SPI0_CS_TA;
     
     {Enable RX Transfer}
     if DMATransferRequestEx(DMAHostGetDefault,@RXData,BCM2710SPI0DMARequestCompleted,SPI,DMA_DIR_DEV_TO_MEM,DMA_DREQ_ID_SPI_RX,DMA_REQUEST_FLAG_NONE) = ERROR_SUCCESS then
      begin
       {Perform TX Transfer}
       if DMATransferRequest(DMAHostGetDefault,@CSData,DMA_DIR_MEM_TO_DEV,DMA_DREQ_ID_SPI_TX,DMA_REQUEST_FLAG_NONE,INFINITE) = ERROR_SUCCESS then
        begin
         {Wait for RX Completion}
         if SemaphoreWait(SPI.Wait) = ERROR_SUCCESS then
          begin
           {Update Count}
           Count:=Size;
          end
         else
          begin
           if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Wait failure on DMA transfer'); 
           
           Result:=ERROR_OPERATION_FAILED;
          end;
        end
       else
        begin
         if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Failure starting TX DMA transfer');
         
         Result:=ERROR_OPERATION_FAILED;
        end;
      end
     else
      begin
       if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Failure starting RX DMA transfer');
       
       Result:=ERROR_OPERATION_FAILED;
      end;      
    end
   else if (Flags and SPI_TRANSFER_PIO) <> 0 then
    begin
     {Update Data}
     PBCM2710SPI0Device(SPI).Mode:=BCM2710_SPI0_MODE_PIO;
     
     {Set Data Length (See: https://www.raspberrypi.org/forums/viewtopic.php?f=44&t=181154)}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).DLEN:=Size;
     
     {Set Control (Active/Clear)}
     Control:=Control or (BCM2837_SPI0_CS_TA or BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);
     
     {Set Control and Status}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
     
     {Loop until Completion}
     while PBCM2710SPI0Device(SPI).DestRemain > 0 do
      begin
       {Read FIFO}
       BCM2710SPI0ReadFIFO(PBCM2710SPI0Device(SPI));
  
       {Write FIFO}
       BCM2710SPI0WriteFIFO(PBCM2710SPI0Device(SPI));
  
       {Get Control and Status}
       Control:=PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS;
    
       {Check Done}
       if ((Control and BCM2837_SPI0_CS_DONE) <> 0) and (PBCM2710SPI0Device(SPI).SourceRemain = 0) then
        begin
         {Read remaining FIFO}
         BCM2710SPI0ReadFIFO(PBCM2710SPI0Device(SPI));
  
         {Reset Control (Active/Interrupt/Deassert/DMA/Clear)}
         Control:=Control and not(BCM2837_SPI0_CS_INTR or BCM2837_SPI0_CS_INTD or BCM2837_SPI0_CS_ADCS or BCM2837_SPI0_CS_DMAEN or BCM2837_SPI0_CS_TA);
         Control:=Control or (BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);
  
         {Set Control and Status}
         PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
  
         {Set Data Length}
         PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).DLEN:=0;
        end;
      end;
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
     
     {Get Count}
     Count:=PBCM2710SPI0Device(SPI).Count;
     
     {Check Count}
     if Count < Size then
      begin
       if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Write failure or timeout');
  
       Result:=ERROR_OPERATION_FAILED;
       
       {Update Statistics}
       Inc(SPI.TransferErrors);
      end;
    end
   else
    begin
     {Update Data}
     PBCM2710SPI0Device(SPI).Mode:=BCM2710_SPI0_MODE_IRQ;
      
     {Note: Cannot fill FIFO when TA bit is not set, interrupt handler will fill on first IRQ} 
     
     {Set Data Length (See: https://www.raspberrypi.org/forums/viewtopic.php?f=44&t=181154)}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).DLEN:=Size;
     
     {Set Control (Active/Interrupt/Clear)}
     Control:=Control or (BCM2837_SPI0_CS_INTR or BCM2837_SPI0_CS_INTD or BCM2837_SPI0_CS_TA or BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);
     
     {Set Control and Status}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
     
     {Wait for Completion}
     if SemaphoreWait(SPI.Wait) = ERROR_SUCCESS then
      begin
       {Get Count}
       Count:=PBCM2710SPI0Device(SPI).Count;
       
       {Check Count}
       if Count < Size then
        begin
         if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Write failure or timeout'); 
         
         Result:=ERROR_OPERATION_FAILED;
         
         {Update Statistics}
         Inc(SPI.TransferErrors);
        end;
      end
     else
      begin
       if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Wait failure on write'); 
       
       Result:=ERROR_OPERATION_FAILED;
      end;
    end;
    
   {Reset Data}
   PBCM2710SPI0Device(SPI).Source:=nil;
   PBCM2710SPI0Device(SPI).Dest:=nil;
   PBCM2710SPI0Device(SPI).Count:=0;
   PBCM2710SPI0Device(SPI).SourceRemain:=0;
   PBCM2710SPI0Device(SPI).DestRemain:=0;
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710SPI0SetMode(SPI:PSPIDevice;Mode:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Set Mode (Mode=' + SPIModeToString(Mode) + ')');
 {$ENDIF}

 {Check Mode}
 if Mode > SPI_MODE_LOSSI then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS;
 
 {Set Mode}
 case Mode of
  SPI_MODE_4WIRE:begin
    {Disable LEN/REN/LEN_LONG/DMA_LEN}
    Control:=Control and not(BCM2837_SPI0_CS_LEN or BCM2837_SPI0_CS_REN or BCM2837_SPI0_CS_LEN_LONG or BCM2837_SPI0_CS_DMA_LEN);
   end; 
  SPI_MODE_3WIRE:begin
    {Disable LEN/LEN_LONG/DMA_LEN}
    Control:=Control and not(BCM2837_SPI0_CS_LEN or BCM2837_SPI0_CS_LEN_LONG or BCM2837_SPI0_CS_DMA_LEN);
   end; 
  SPI_MODE_LOSSI:begin
    {Disable REN}
    Control:=Control and not(BCM2837_SPI0_CS_REN);
    
    {Enable LEN}
    Control:=Control or BCM2837_SPI0_CS_LEN;
   end; 
 end;
 
 {Set Control and Status}
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 SPI.SPIMode:=Mode;
 SPI.Properties.Mode:=Mode;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710SPI0SetClockRate(SPI:PSPIDevice;ChipSelect:Word;ClockRate:LongWord):LongWord;
var
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Set Clock Rate (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' ClockRate=' + IntToStr(ClockRate) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if SPI.SPIState <> SPI_STATE_ENABLED then
  begin
   {Update Core Clock}
   PBCM2710SPI0Device(SPI).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
   if PBCM2710SPI0Device(SPI).CoreClock = 0 then PBCM2710SPI0Device(SPI).CoreClock:=BCM2710_SPI0_CORE_CLOCK;
   
   {Update Properties}
   SPI.Properties.MinClock:=PBCM2710SPI0Device(SPI).CoreClock div BCM2710_SPI0_MAX_DIVIDER;
   SPI.Properties.MaxClock:=PBCM2710SPI0Device(SPI).CoreClock div BCM2710_SPI0_MIN_DIVIDER;
   
   {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
   if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710:  CoreClock=' + IntToStr(PBCM2710SPI0Device(SPI).CoreClock) + ' MinClock=' + IntToStr(SPI.Properties.MinClock) + ' MaxClock=' + IntToStr(SPI.Properties.MaxClock));
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
   Divider:=PBCM2710SPI0Device(SPI).CoreClock div ClockRate;
   if (Divider and 1) <> 0 then Inc(Divider);
  
   {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
   if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710:  Divider=' + IntToStr(Divider));
   {$ENDIF}
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Set Clock Divider} 
   PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CLK:=(Divider and BCM2837_SPI0_CLK_CDIV);
   
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
     Divider:=PBCM2710SPI0Device(SPI).CoreClock div ClockRate;
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

function BCM2710SPI0SetClockPhase(SPI:PSPIDevice;ClockPhase:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Set Clock Phase (ClockPhase=' + SPIClockPhaseToString(ClockPhase) + ')');
 {$ENDIF}

 {Check Clock Phase}
 if ClockPhase > SPI_CLOCK_PHASE_HIGH then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS;

 {Set Clock Phase}
 if ClockPhase = SPI_CLOCK_PHASE_HIGH then
  begin
   {Enable CPHA}
   Control:=Control or BCM2837_SPI0_CS_CPHA;
  end
 else
  begin
   {Disable CPHA}
   Control:=Control and not(BCM2837_SPI0_CS_CPHA);
  end;  
 
 {Set Control and Status}
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 SPI.ClockPhase:=ClockPhase;
 SPI.Properties.ClockPhase:=ClockPhase;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710SPI0SetClockPolarity(SPI:PSPIDevice;ClockPolarity:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Set Clock Polarity (ClockPolarity=' + SPIClockPolarityToString(ClockPolarity) + ')');
 {$ENDIF}

 {Check Clock Polarity}
 if ClockPolarity > SPI_CLOCK_POLARITY_HIGH then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS;
 
 {Set Clock Polarity}
 if ClockPolarity = SPI_CLOCK_POLARITY_HIGH then
  begin 
   {Enable CPOL}
   Control:=Control or BCM2837_SPI0_CS_CPOL;
  end
 else 
  begin
   {Disable CPOL}
   Control:=Control and not(BCM2837_SPI0_CS_CPOL);
  end;  
 
 {Set Control and Status}
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 SPI.ClockPolarity:=ClockPolarity;
 SPI.Properties.ClockPolarity:=ClockPolarity;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710SPI0SetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word;SelectPolarity:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Set Select Polarity (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' SelectPolarity=' + SPISelectPolarityToString(SelectPolarity) + ')');
 {$ENDIF}

 {Check Chip Select}
 if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_2) then Exit;
 
 {Check Select Polarity}
 if SelectPolarity > SPI_CS_POLARITY_HIGH then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS;
 
 {Set Select Polarity}
 if ChipSelect = SPI_CS_NONE then
  begin
   if SelectPolarity = SPI_CS_POLARITY_HIGH then
    begin
     {Enable CSPOL}
     Control:=Control or BCM2837_SPI0_CS_CSPOL;
    end
   else
    begin
     {Disable CSPOL}
     Control:=Control and not(BCM2837_SPI0_CS_CSPOL);
    end;
  end
 else
  begin 
   if SelectPolarity = SPI_CS_POLARITY_HIGH then
    begin
     {Enable CSPOL0/1/2}
     Control:=Control or (BCM2837_SPI0_CS_CSPOL0 shl ChipSelect);
    end
   else
    begin
     {Disable CSPOL0/1/2}
     Control:=Control and not(BCM2837_SPI0_CS_CSPOL0 shl ChipSelect);
    end;
  end;  
 
 {Set Control and Status}
 PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;
 
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

procedure BCM2710SPI0ReadFIFO(SPI:PBCM2710SPI0Device);
{Caller will hold the SPI device lock}
{Note: Called from within the interrupt handler}
var
 Data:LongWord;
begin
 {}
 {Check SPI}
 if SPI = nil then Exit;
 
 {Check Data}
 while (SPI.DestRemain > 0) and ((PBCM2837SPI0Registers(SPI.Address).CS and BCM2837_SPI0_CS_RXD) <> 0) do
  begin
   {Read Data}
   Data:=(PBCM2837SPI0Registers(SPI.Address).FIFO and BCM2837_SPI0_FIFO_IRQ_DATA);
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

procedure BCM2710SPI0WriteFIFO(SPI:PBCM2710SPI0Device);
{Caller will hold the SPI device lock}
{Note: Called from within the interrupt handler}
var
 Data:LongWord;
begin
 {}
 {Check SPI}
 if SPI = nil then Exit;
 
 {Check Space}
 while (SPI.SourceRemain > 0) and ((PBCM2837SPI0Registers(SPI.Address).CS and BCM2837_SPI0_CS_TXD) <> 0) do
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
   PBCM2837SPI0Registers(SPI.Address).FIFO:=(Data and BCM2837_SPI0_FIFO_IRQ_DATA);
 
   {Update Remain}
   Dec(SPI.SourceRemain);
  end;
end;
 
{==============================================================================}
 
procedure BCM2710SPI0InterruptHandler(SPI:PBCM2710SPI0Device);
var
 Control:LongWord;
begin
 {}
 {Check SPI}
 if SPI = nil then Exit;

 {Update Statistics}
 Inc(SPI.InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Read FIFO}
 BCM2710SPI0ReadFIFO(SPI);

 {Write FIFO}
 BCM2710SPI0WriteFIFO(SPI);
 
 {Get Control and Status}
 Control:=PBCM2837SPI0Registers(SPI.Address).CS;
 
 {Check Done}
 if ((Control and BCM2837_SPI0_CS_DONE) <> 0) and (SPI.SourceRemain = 0) then
  begin
   {Read remaining FIFO}
   BCM2710SPI0ReadFIFO(SPI);
  
   {Reset Control (Active/Interrupt/Deassert/DMA/Clear)}
   Control:=Control and not(BCM2837_SPI0_CS_INTR or BCM2837_SPI0_CS_INTD or BCM2837_SPI0_CS_ADCS or BCM2837_SPI0_CS_DMAEN or BCM2837_SPI0_CS_TA);
   Control:=Control or (BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);
   
   {Set Control and Status}
   PBCM2837SPI0Registers(SPI.Address).CS:=Control;
   
   {Set Data Length}
   PBCM2837SPI0Registers(SPI.Address).DLEN:=0;
  
   {Signal Semaphore}
   SemaphoreSignal(SPI.SPI.Wait);
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2710SPI0DMARequestCompleted(Request:PDMARequest); 
{DMA Request completion callback for SPI0}
var
 Control:LongWord;
 SPI:PBCM2710SPI0Device;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get SPI}
 SPI:=PBCM2710SPI0Device(Request.DriverData);
 if SPI = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control and Status}
 Control:=PBCM2837SPI0Registers(SPI.Address).CS;
 
 {Check Done}
 if (Control and BCM2837_SPI0_CS_DONE) <> 0 then
  begin
   {Reset Control (Active/Interrupt/Deassert/DMA/Clear)}
   Control:=Control and not(BCM2837_SPI0_CS_INTR or BCM2837_SPI0_CS_INTD or BCM2837_SPI0_CS_ADCS or BCM2837_SPI0_CS_DMAEN or BCM2837_SPI0_CS_TA);
   Control:=Control or (BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);
   
   {Set Control and Status}
   PBCM2837SPI0Registers(SPI.Address).CS:=Control;
   
   {Set Data Length}
   PBCM2837SPI0Registers(SPI.Address).DLEN:=0;
  
   {Signal Semaphore}
   SemaphoreSignal(SPI.SPI.Wait);
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}
{==============================================================================}
{BCM2710 BSCI2C (I2C0/1) Functions}
function BCM2710BSCI2CStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
var
 Slave:LongWord;
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Start (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Update Core Clock}
 PBCM2710BSCI2CDevice(I2C).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
 if PBCM2710BSCI2CDevice(I2C).CoreClock = 0 then PBCM2710BSCI2CDevice(I2C).CoreClock:=BCM2710_BSCI2C_CORE_CLOCK; 
 
 {Update Properties}
 I2C.Properties.MinClock:=PBCM2710BSCI2CDevice(I2C).CoreClock div BCM2710_BSCI2C_MAX_DIVIDER;
 I2C.Properties.MaxClock:=PBCM2710BSCI2CDevice(I2C).CoreClock div BCM2710_BSCI2C_MIN_DIVIDER;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710:  CoreClock=' + IntToStr(PBCM2710BSCI2CDevice(I2C).CoreClock) + ' MinClock=' + IntToStr(I2C.Properties.MinClock) + ' MaxClock=' + IntToStr(I2C.Properties.MaxClock));
 {$ENDIF}
 
 {Check Rate}
 if (Rate <> 0) and ((Rate < I2C.Properties.MinClock) or (Rate > I2C.Properties.MaxClock)) then Exit;
 
 {Enable GPIO Pins}
 GPIOFunctionSelect(PBCM2710BSCI2CDevice(I2C).SDAPin,PBCM2710BSCI2CDevice(I2C).SDAFunction);
 GPIOFunctionSelect(PBCM2710BSCI2CDevice(I2C).SCLPin,PBCM2710BSCI2CDevice(I2C).SCLFunction);

 {Get Divider}
 if Rate = 0 then Rate:=BCM2710_BSCI2C_DEFAULT_CLOCK;
 Divider:=PBCM2710BSCI2CDevice(I2C).CoreClock div Rate;
 if (Divider and 1) <> 0 then Inc(Divider);

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710:  Rate=' + IntToStr(Rate) + ' Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control (Disable I2C)} 
 PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=0;
 
 {Reset Status}
 PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;
 
 {Set Divider}
 PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).CDIV:=(Divider and BCM2837_BSC_CDIV_MASK);

 {Get Slave}
 Slave:=(PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).A and BCM2837_BSC_A_MASK);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 {Create Wait Semaphore}
 I2C.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
 if I2C.Wait = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Update IRQ Data}
 if SpinLock(BCM2710BSCI2CIRQData.Lock) = ERROR_SUCCESS then
  begin
   BCM2710BSCI2CIRQData.Devices[PBCM2710BSCI2CDevice(I2C).Index]:=PBCM2710BSCI2CDevice(I2C);
   Inc(BCM2710BSCI2CIRQData.Count);
   
   {Check Count}
   if BCM2710BSCI2CIRQData.Count = 1 then
    begin
     {Request IRQ}
     RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_I2C,TInterruptHandler(BCM2710BSCI2CInterruptHandler),@BCM2710BSCI2CIRQData);
    end;
    
   SpinUnlock(BCM2710BSCI2CIRQData.Lock);
  end;

 {Update Properties}
 I2C.ClockRate:=Rate;
 I2C.SlaveAddress:=Slave;
 I2C.Properties.ClockRate:=Rate;
 I2C.Properties.SlaveAddress:=Slave;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2710BSCI2CStop(I2C:PI2CDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Stop');
 {$ENDIF}
 
 {Update IRQ Data}
 if SpinLock(BCM2710BSCI2CIRQData.Lock) = ERROR_SUCCESS then
  begin
   BCM2710BSCI2CIRQData.Devices[PBCM2710BSCI2CDevice(I2C).Index]:=nil;
   Dec(BCM2710BSCI2CIRQData.Count);
   
   {Check Count}
   if BCM2710BSCI2CIRQData.Count = 0 then
    begin
     {Release IRQ}
     ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_I2C,TInterruptHandler(BCM2710BSCI2CInterruptHandler),@BCM2710BSCI2CIRQData);
    end;
    
   SpinUnlock(BCM2710BSCI2CIRQData.Lock);
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Control (Disable I2C)} 
 PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=0;
 
 {Reset Status}
 PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Destroy Wait Semaphore}
 SemaphoreDestroy(I2C.Wait);
 
 {Reset Transfer}
 PBCM2710BSCI2CDevice(I2C).Mode:=BCM2710_BSCI2C_MODE_WRITE;
 PBCM2710BSCI2CDevice(I2C).Data:=nil;
 PBCM2710BSCI2CDevice(I2C).Count:=0;
 PBCM2710BSCI2CDevice(I2C).Remain:=0;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}
 
function BCM2710BSCI2CRead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Read (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Size}
 if Size > BCM2710_BSCI2C_MAX_SIZE then Exit;
 
 {Update Statistics}
 Inc(I2C.ReadCount);
 
 {Read to Buffer}
 if Size > 0 then
  begin
   {Setup Data}
   PBCM2710BSCI2CDevice(I2C).Mode:=BCM2710_BSCI2C_MODE_READ;
   PBCM2710BSCI2CDevice(I2C).Data:=Buffer;
   PBCM2710BSCI2CDevice(I2C).Count:=0;
   PBCM2710BSCI2CDevice(I2C).Remain:=Size;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Reset Status}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;

   {Setup Length}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).DLEN:=(Size and BCM2837_BSC_DLEN_MASK);
   
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
    
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).A:=(Address and BCM2837_BSC_A_MASK);
    end;    
   
   {Setup Control (Enable / Interrupt Receive / Interrupt Done / Start / Clear / Read)}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_I2CEN or BCM2837_BSC_C_INTR or BCM2837_BSC_C_INTD or BCM2837_BSC_C_ST or BCM2837_BSC_C_CLEAR or BCM2837_BSC_C_READ;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Wait for Completion}
   if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
    begin
     {Get Count}
     Count:=PBCM2710BSCI2CDevice(I2C).Count;
     
     {Check Count}
     if Count < Size then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Read failure or timeout'); 
       
       {Update Statistics}
       Inc(I2C.ReadErrors);
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Wait failure on read'); 
     
     Result:=ERROR_OPERATION_FAILED;
    end;
    
   {Reset Data}
   PBCM2710BSCI2CDevice(I2C).Data:=nil;
   PBCM2710BSCI2CDevice(I2C).Count:=0;
   PBCM2710BSCI2CDevice(I2C).Remain:=0;
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}

function BCM2710BSCI2CWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Write (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Size}
 if Size > BCM2710_BSCI2C_MAX_SIZE then Exit;
 
 {Update Statistics}
 Inc(I2C.WriteCount);
 
 {Write from Buffer}
 if Size > 0 then
  begin
   {Setup Data}
   PBCM2710BSCI2CDevice(I2C).Mode:=BCM2710_BSCI2C_MODE_WRITE;
   PBCM2710BSCI2CDevice(I2C).Data:=Buffer;
   PBCM2710BSCI2CDevice(I2C).Count:=0;
   PBCM2710BSCI2CDevice(I2C).Remain:=Size;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Reset Status}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;

   {Setup Length}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).DLEN:=(Size and BCM2837_BSC_DLEN_MASK);
   
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then      
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
     
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).A:=(Address and BCM2837_BSC_A_MASK);
    end;    
   
   {Setup Control (Clear)}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_CLEAR;
   
   {Fill FIFO}
   BCM2710BSCI2CFillFIFO(PBCM2710BSCI2CDevice(I2C));
   
   {Setup Control (Enable / Interrupt Transmit / Interrupt Done / Start)}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_I2CEN or BCM2837_BSC_C_INTT or BCM2837_BSC_C_INTD or BCM2837_BSC_C_ST;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Wait for Completion}
   if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
    begin
     {Get Count}
     Count:=PBCM2710BSCI2CDevice(I2C).Count;
     
     {Check Count}
     if Count < Size then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Write failure or timeout'); 
       
       {Update Statistics}
       Inc(I2C.WriteErrors);
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Wait failure on write'); 
     
     Result:=ERROR_OPERATION_FAILED;
    end;
    
   {Reset Data}
   PBCM2710BSCI2CDevice(I2C).Data:=nil;
   PBCM2710BSCI2CDevice(I2C).Count:=0;
   PBCM2710BSCI2CDevice(I2C).Remain:=0;
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}

function BCM2710BSCI2CWriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Write Read (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Sizes}
 if Len > BCM2710_BSCI2C_MAX_SIZE then Exit;
 if Size > BCM2710_BSCI2C_MAX_SIZE then Exit;

 {Check Len}
 if (Len > BCM2837_BSC_FIFO_SIZE) or not(BCM2710I2C_COMBINED_WRITEREAD) then
  begin
   Written:=0;
   
   {Write Initial}
   Result:=BCM2710BSCI2CWrite(I2C,Address,Initial,Len,Written);
   if Result = ERROR_SUCCESS then
    begin
     {Read Data}
     Result:=BCM2710BSCI2CRead(I2C,Address,Data,Size,Count);
    end;
  end
 else
  begin
   {Write from Initial}
   if Len > 0 then
    begin
     {Setup Data}
     PBCM2710BSCI2CDevice(I2C).Mode:=BCM2710_BSCI2C_MODE_WRITE;
     PBCM2710BSCI2CDevice(I2C).Data:=Initial;
     PBCM2710BSCI2CDevice(I2C).Count:=0;
     PBCM2710BSCI2CDevice(I2C).Remain:=Len;
     
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Reset Status}
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;
     
     {Setup Length}
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).DLEN:=(Len and BCM2837_BSC_DLEN_MASK);
     
     {Setup Address}
     if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then      
      begin
       {Update Properties}
       I2C.SlaveAddress:=Address;
       I2C.Properties.SlaveAddress:=Address;
       
       PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).A:=(Address and BCM2837_BSC_A_MASK);
      end;    
     
     {Setup Control (Clear)}
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_CLEAR;
     
     {Fill FIFO}
     BCM2710BSCI2CFillFIFO(PBCM2710BSCI2CDevice(I2C));
     
     {Setup Control (Enable / Start) (No Interrupts)}
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_I2CEN or BCM2837_BSC_C_ST;
     
     {Poll Transfer Active}
     Retries:=200;
     Status:=PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S;
     while ((Status and (BCM2837_BSC_S_TA or BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE)) = 0) and (Retries > 0) do
      begin
       Status:=PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S;
       
       Dec(Retries);
      end; 

     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
      
     {Check Result}
     if (Status and (BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR) <> 0) or (Retries = 0) then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Write failure or timeout'); 
       
       {Update Statistics}
       Inc(I2C.WriteErrors);
       
       Result:=ERROR_OPERATION_FAILED;
      end
     else if Size > 0 then
      begin
       {Read to Data}
       {Setup Data}
       PBCM2710BSCI2CDevice(I2C).Mode:=BCM2710_BSCI2C_MODE_READ;
       PBCM2710BSCI2CDevice(I2C).Data:=Data;
       PBCM2710BSCI2CDevice(I2C).Count:=0;
       PBCM2710BSCI2CDevice(I2C).Remain:=Size;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Setup Length}
       PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).DLEN:=(Size and BCM2837_BSC_DLEN_MASK);
       
       {Setup Control (Enable / Interrupt Receive / Interrupt Done / Start / Read) (No Clear)}
       PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_I2CEN or BCM2837_BSC_C_INTR or BCM2837_BSC_C_INTD or BCM2837_BSC_C_ST or BCM2837_BSC_C_READ;
       
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Wait for Completion}
       if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
        begin
         {Get Count}
         Count:=PBCM2710BSCI2CDevice(I2C).Count;
         
         {Check Count}
         if Count < Size then
          begin
           if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Read failure or timeout'); 
           
           {Update Statistics}
           Inc(I2C.ReadErrors);
          end;
        end
       else
        begin
         if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Wait failure on read'); 
         
         Result:=ERROR_OPERATION_FAILED;
        end;
      end;
      
     {Reset Data}
     PBCM2710BSCI2CDevice(I2C).Data:=nil;
     PBCM2710BSCI2CDevice(I2C).Count:=0;
     PBCM2710BSCI2CDevice(I2C).Remain:=0;
    end;
  
  
   {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
   if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710:  Return Count=' + IntToStr(Count));
   {$ENDIF}
    
   {Return Result}
   if (Size = Count) then Result:=ERROR_SUCCESS; 
  end;  
end;

{==============================================================================}

function BCM2710BSCI2CWriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Write Write (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Sizes}
 if Len > BCM2837_BSC_FIFO_SIZE then Exit;
 if Size > BCM2710_BSCI2C_MAX_SIZE then Exit;

 {Write from Initial and Data}
 if (Len > 0) and (Size > 0) then
  begin
   {Setup Data}
   PBCM2710BSCI2CDevice(I2C).Mode:=BCM2710_BSCI2C_MODE_WRITE;
   PBCM2710BSCI2CDevice(I2C).Data:=Data;
   PBCM2710BSCI2CDevice(I2C).Count:=0;
   PBCM2710BSCI2CDevice(I2C).Remain:=Size;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
     
   {Reset Status}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;
 
   {Setup Length}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).DLEN:=((Size + Len) and BCM2837_BSC_DLEN_MASK);
 
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then      
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
     
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).A:=(Address and BCM2837_BSC_A_MASK);
    end;    
   
   {Setup Control (Clear)}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_CLEAR;
 
   {Write Initial to FIFO}
   while (PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).S and BCM2837_BSC_S_TXD) <> 0 do
    begin
     {Write Initial}
     PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).FIFO:=(PLongWord(Initial)^ and BCM2837_BSC_FIFO_MASK);
   
     {Update Initial}
     Inc(Initial);
     Dec(Len);
     
     if Len = 0 then Break;
    end; 
   
   {Fill FIFO from Data}
   BCM2710BSCI2CFillFIFO(PBCM2710BSCI2CDevice(I2C));
   
   {Setup Control (Enable / Interrupt Transmit / Interrupt Done / Start)}
   PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).C:=BCM2837_BSC_C_I2CEN or BCM2837_BSC_C_INTT or BCM2837_BSC_C_INTD or BCM2837_BSC_C_ST;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Wait for Completion}
   if SemaphoreWait(I2C.Wait) = ERROR_SUCCESS then
    begin
     {Get Count}
     Count:=PBCM2710BSCI2CDevice(I2C).Count;
     
     {Check Count}
     if Count < Size then
      begin
       if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Write failure or timeout'); 
       
       {Update Statistics}
       Inc(I2C.WriteErrors);
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(I2C,'BCM2710: Wait failure on write'); 
     
     Result:=ERROR_OPERATION_FAILED;
    end;
    
   {Reset Data}
   PBCM2710BSCI2CDevice(I2C).Data:=nil;
   PBCM2710BSCI2CDevice(I2C).Count:=0;
   PBCM2710BSCI2CDevice(I2C).Remain:=0;
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
  
 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS; 
end; 
   
{==============================================================================}
 
function BCM2710BSCI2CSetRate(I2C:PI2CDevice;Rate:LongWord):LongWord;
var
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Set Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Check Rate}
 if (Rate < I2C.Properties.MinClock) or (Rate > I2C.Properties.MaxClock) then Exit;
 
 {Get Divider}
 Divider:=PBCM2710BSCI2CDevice(I2C).CoreClock div Rate;
 if (Divider and 1) <> 0 then Inc(Divider);

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710:  Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Divider}
 PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).CDIV:=(Divider and BCM2837_BSC_CDIV_MASK);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 I2C.ClockRate:=Rate;
 I2C.Properties.ClockRate:=Rate;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}
 
function BCM2710BSCI2CSetAddress(I2C:PI2CDevice;Address:Word):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'BCM2710: BSCI2C Set Address (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Address}
 PBCM2837BSCRegisters(PBCM2710BSCI2CDevice(I2C).Address).A:=(Address and BCM2837_BSC_A_MASK);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 I2C.SlaveAddress:=Address;
 I2C.Properties.SlaveAddress:=Address;
 
 {Return Result}
 Result:=ERROR_SUCCESS; 
end; 

{==============================================================================}

procedure BCM2710BSCI2CFillFIFO(I2C:PBCM2710BSCI2CDevice);
{Caller will hold the I2C device lock}
{Note: Called from within the interrupt handler}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Mode}
 if I2C.Mode = BCM2710_BSCI2C_MODE_READ then Exit;
 
 {Check Space}
 while (I2C.Remain > 0) and ((PBCM2837BSCRegisters(I2C.Address).S and BCM2837_BSC_S_TXD) <> 0) do
  begin
   {Write Data}
   PBCM2837BSCRegisters(I2C.Address).FIFO:=(PLongWord(I2C.Data)^ and BCM2837_BSC_FIFO_MASK);
   
   {Update Data}
   Inc(I2C.Data);
   Inc(I2C.Count);
   Dec(I2C.Remain);
  end;
end;

{==============================================================================}

procedure BCM2710BSCI2CDrainFIFO(I2C:PBCM2710BSCI2CDevice);
{Caller will hold the I2C device lock}
{Note: Called from within the interrupt handler}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Mode}
 if I2C.Mode = BCM2710_BSCI2C_MODE_WRITE then Exit;
 
 {Check Data}
 while (I2C.Remain > 0) and ((PBCM2837BSCRegisters(I2C.Address).S and BCM2837_BSC_S_RXD) <> 0) do
  begin
   {Read Data}
   PByte(I2C.Data)^:=(PBCM2837BSCRegisters(I2C.Address).FIFO and BCM2837_BSC_FIFO_MASK);
   
   {Update Data}
   Inc(I2C.Data);
   Inc(I2C.Count);
   Dec(I2C.Remain);
  end;
end;

{==============================================================================}

procedure BCM2710BSCI2CInterruptHandler(IRQData:PBCM2710BSCI2CIRQData);
{Note: Thread submitting the current request will hold the I2C device lock}
var
 Count:LongWord;
 Status:LongWord;
 I2C:PBCM2710BSCI2CDevice;
begin
 {}
 {Check IRQ Data}
 if IRQData = nil then Exit;
 
 {Check Count}
 if IRQData.Count > 0 then
  begin
   for Count:=0 to 2 do
    begin
     {Check Device}
     I2C:=IRQData.Devices[Count];
     if (I2C <> nil) and (I2C.Data <> nil) then
      begin
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}

       {Read Status}
       Status:=PBCM2837BSCRegisters(I2C.Address).S;
       
       {Check Status}
       if (Status and (BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR)) <> 0 then
        begin
         {Error}
         {Update Statistics}
         Inc(I2C.InterruptCount);
         
         {Reset Control (Disable I2C)} 
         PBCM2837BSCRegisters(I2C.Address).C:=0;
         
         {Reset Status}
         PBCM2837BSCRegisters(I2C.Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;
         
         {Signal Semaphore}
         SemaphoreSignal(I2C.I2C.Wait);
        end
       else if (Status and BCM2837_BSC_S_DONE) <> 0 then
        begin
         {Completed}
         {Update Statistics}
         Inc(I2C.InterruptCount);

         {Check Mode}
         if I2C.Mode = BCM2710_BSCI2C_MODE_READ then
          begin
           {Drain FIFO}
           BCM2710BSCI2CDrainFIFO(I2C);
          end;
          
         {Reset Control (Disable I2C)} 
         PBCM2837BSCRegisters(I2C.Address).C:=0;
         
         {Reset Status}
         PBCM2837BSCRegisters(I2C.Address).S:=BCM2837_BSC_S_CLKT or BCM2837_BSC_S_ERR or BCM2837_BSC_S_DONE;
         
         {Signal Semaphore}
         SemaphoreSignal(I2C.I2C.Wait);
        end
       else if (Status and BCM2837_BSC_S_RXR) <> 0 then 
        begin
         {Receive}
         {Update Statistics}
         Inc(I2C.InterruptCount);

         {Drain FIFO}
         BCM2710BSCI2CDrainFIFO(I2C);
        end
       else if (Status and BCM2837_BSC_S_TXW) <> 0 then 
        begin
         {Transmit}
         {Update Statistics}
         Inc(I2C.InterruptCount);

         {Fill FIFO}
         BCM2710BSCI2CFillFIFO(I2C);
        end;
       
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
      end;
    end;
  end;
end; 

{==============================================================================}
{==============================================================================}
{BCM2710 SPI AUX (SPI1/2) Functions}
//To Do //Continuing

{==============================================================================}
{==============================================================================}
{BCM2710 SPI/I2C Slave Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 DMA Functions}
function BCM2710DMAHostStart(DMA:PDMAHost):LongWord;
var
 Mask:LongWord;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Get Channel Mask}
 PBCM2710DMAHost(DMA).ChannelMask:=DMAGetChannels;

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: Channel mask = ' + IntToHex(PBCM2710DMAHost(DMA).ChannelMask,8));
 {$ENDIF}
 
 {Get Channel Free}
 PBCM2710DMAHost(DMA).ChannelFree:=PBCM2710DMAHost(DMA).ChannelMask;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: Channel free = ' + IntToHex(PBCM2710DMAHost(DMA).ChannelFree,8));
 {$ENDIF}
 
 {Create Channel Lock}
 PBCM2710DMAHost(DMA).ChannelLock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if PBCM2710DMAHost(DMA).ChannelLock = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Count Free Normal Channels}
 Mask:=(PBCM2710DMAHost(DMA).ChannelMask and BCM2710_DMA_NORMAL_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: Normal channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create Normal Channel Semaphore}
 PBCM2710DMAHost(DMA).ChannelWait:=SemaphoreCreate(Count);
 if PBCM2710DMAHost(DMA).ChannelWait = INVALID_HANDLE_VALUE then
  begin
   {Destroy Channel Lock}
   MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Count Free DMA Lite Channels}
 Mask:=(PBCM2710DMAHost(DMA).ChannelMask and BCM2710_DMA_LITE_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: DMA Lite channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create DMA Lite Channel Semaphore}
 PBCM2710DMAHost(DMA).ChannelLite:=SemaphoreCreate(Count);
 if PBCM2710DMAHost(DMA).ChannelLite = INVALID_HANDLE_VALUE then
  begin
   {Destroy Normal Channel Semaphore}
   SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelWait);
   
   {Destroy Channel Lock}
   MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Count Free DMA Bulk Channels}
 Mask:=(PBCM2710DMAHost(DMA).ChannelMask and BCM2710_DMA_BULK_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: DMA Bulk channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create DMA Bulk Channel Semaphore}
 PBCM2710DMAHost(DMA).ChannelBulk:=SemaphoreCreate(Count);
 if PBCM2710DMAHost(DMA).ChannelBulk = INVALID_HANDLE_VALUE then
  begin
   {Destroy DMA Lite Channel Semaphore}
   SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelLite);
  
   {Destroy Normal Channel Semaphore}
   SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelWait);
   
   {Destroy Channel Lock}
   MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Setup Enable Register}
 PBCM2710DMAHost(DMA).EnableRegister:=PLongWord(BCM2837_DMA_ENABLE_BASE);
 
 {Setup Interrupt Register}
 PBCM2710DMAHost(DMA).InterruptRegister:=PLongWord(BCM2837_DMA_INT_STATUS_BASE);
 
 {Start Channels}
 for Count:=0 to BCM2710_DMA_CHANNEL_COUNT - 1 do
  begin
   {Host}
   PBCM2710DMAHost(DMA).Channels[Count].Host:=PBCM2710DMAHost(DMA);
   
   {Channel No}
   PBCM2710DMAHost(DMA).Channels[Count].Number:=Count;
   
   {Check Available}
   if (PBCM2710DMAHost(DMA).ChannelMask and (1 shl Count)) <> 0 then
    begin
     {Check Channel}
     case Count of
      {Channels 0 to 10}
      0..10:begin
        {Interrupt No}
        PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=BCM2837_IRQ_DMA0 + Count;
      
        {Registers}
        PBCM2710DMAHost(DMA).Channels[Count].Registers:=PBCM2837DMARegisters(BCM2837_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMAInterruptHandler),@PBCM2710DMAHost(DMA).Channels[Count]);
       end;
      {Channels 11 to 14}
      11..14:begin
        {Interrupt No}
        PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=BCM2837_IRQ_DMA11_14;
      
        {Registers}
        PBCM2710DMAHost(DMA).Channels[Count].Registers:=PBCM2837DMARegisters(BCM2837_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMASharedInterruptHandler),DMA);
       end; 
      {Channel 15}
      15:begin
        {Interrupt No (Only available on the all channels interrupt)} 
        PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=BCM2837_IRQ_DMA_ALL;

        {Registers}
        PBCM2710DMAHost(DMA).Channels[Count].Registers:=PBCM2837DMARegisters(BCM2837_DMA15_REGS_BASE);
        
        {No Request IRQ}
       end;      
     end;
     
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Check the Channel}
     if (PBCM2710DMAHost(DMA).EnableRegister^ and (1 shl Count)) = 0 then
      begin
       {Enable the Channel}
       PBCM2710DMAHost(DMA).EnableRegister^:=PBCM2710DMAHost(DMA).EnableRegister^ or (1 shl Count);
       MicrosecondDelay(1000);
     
       {Reset the Channel}
       PBCM2710DMAHost(DMA).Channels[Count].Registers.CS:=BCM2837_DMA_CS_RESET;
      end; 
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
    end
   else
    begin
     {Interrupt No}
     PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=LongWord(INVALID_HANDLE_VALUE);
     
     {Registers}
     PBCM2710DMAHost(DMA).Channels[Count].Registers:=nil;
    end;
  end;

 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2710DMAHostStop(DMA:PDMAHost):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Stop Channels}
 for Count:=0 to BCM2710_DMA_CHANNEL_COUNT - 1 do
  begin
   {Check Available}
   if (PBCM2710DMAHost(DMA).ChannelMask and (1 shl Count)) <> 0 then
    begin
     {Check Channel}
     case Count of
      {Channels 0 to 10}
      0..10:begin
        {Release IRQ}
        ReleaseIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMAInterruptHandler),@PBCM2710DMAHost(DMA).Channels[Count]);
       end;
      {Channels 11 to 14}
      11..14:begin
        {Release IRQ}
        ReleaseIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMASharedInterruptHandler),DMA);
       end;
      {Channel 15}
      15:begin
        {No Release IRQ}
       end;
     end;
    end;
  end; 

 {Destroy DMA Bulk Channel Semaphore}
 SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelBulk);
 PBCM2710DMAHost(DMA).ChannelBulk:=INVALID_HANDLE_VALUE;
  
 {Destroy DMA Lite Channel Semaphore}
 SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelLite);
 PBCM2710DMAHost(DMA).ChannelLite:=INVALID_HANDLE_VALUE;
  
 {Destroy Normal Channel Semaphore}
 SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelWait);
 PBCM2710DMAHost(DMA).ChannelWait:=INVALID_HANDLE_VALUE;
 
 {Destroy Channel Lock}
 MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
 PBCM2710DMAHost(DMA).ChannelLock:=INVALID_HANDLE_VALUE;
 
 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2710DMAHostSubmit(DMA:PDMAHost;Request:PDMARequest):LongWord;
var
 Bulk:Boolean;
 Lite:Boolean;
 Flags:LongWord;
 Count:LongWord;
 Channel:LongWord;
 Maximum:LongWord;
 Data:PDMAData;
 Block:PBCM2837DMAControlBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.Host <> DMA then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Submitting request (Request=' + IntToHex(LongWord(Request),8) + ')');
 {$ENDIF}
 
 {Get Data Count}
 Count:=DMADataCount(Request.Data);
 if Count = 0 then Exit;

 {Get Data Flags}
 Flags:=DMADataFlags(Request.Data);
 
 {Get Data Maximum}
 Maximum:=DMADataMaximum(Request.Data);
 
 Bulk:=False;
 Lite:=False;
 
 {Check for "Bulk" channel request}
 if (Flags and DMA_DATA_FLAG_BULK) <> 0 then
  begin
   Bulk:=True;
  end
 else
  begin 
   {Check for "Lite" suitable request (No Stride, No Ignore, Size less then 64K)}
   if (Flags and (DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_NOREAD or DMA_DATA_FLAG_NOWRITE) = 0) and (Maximum <= BCM2710_DMA_MAX_LITE_TRANSFER) then
    begin
     Lite:=True;
    end;
  end;  
 
 {Get Maximum Size}
 Maximum:=BCM2710_DMA_MAX_NORMAL_TRANSFER;
 if Lite then Maximum:=BCM2710_DMA_MAX_LITE_TRANSFER;
 
 Result:=ERROR_OPERATION_FAILED;
 
 {Create Control Blocks}
 if BCM2710DMA_SHARED_MEMORY then
  begin
   Request.ControlBlocks:=GetSharedAlignedMem(Count * SizeOf(TBCM2837DMAControlBlock),BCM2710_DMA_CB_ALIGNMENT);
  end
 else if BCM2710DMA_NOCACHE_MEMORY then
  begin
   Request.ControlBlocks:=GetNoCacheAlignedMem(Count * SizeOf(TBCM2837DMAControlBlock),BCM2710_DMA_CB_ALIGNMENT);
  end
 else 
  begin
   Request.ControlBlocks:=GetAlignedMem(Count * SizeOf(TBCM2837DMAControlBlock),BCM2710_DMA_CB_ALIGNMENT);
  end;  
 if Request.ControlBlocks = nil then Exit;
 try
  {Update Control Blocks}
  Data:=Request.Data;
  Block:=PBCM2837DMAControlBlock(Request.ControlBlocks);
  while Data <> nil do
   begin
    {Check Size}
    if Data.Size = 0 then Exit;
    if Data.Size > Maximum then Exit;
    if ((Data.Flags and DMA_DATA_FLAG_STRIDE) <> 0) and (Data.StrideLength = 0) then Exit;
    
    {Setup Control Block}
    BCM2710DMADataToControlBlock(Request,Data,Block,Bulk,Lite);
    
    {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
    if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Data block (Source=' + IntToHex(LongWord(Data.Source),8) + ' Dest=' + IntToHex(LongWord(Data.Dest),8) + ' Size=' + IntToStr(Data.Size) + ')');
    if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Control block (SourceAddress=' + IntToHex(Block.SourceAddress,8) + ' DestinationAddress=' + IntToHex(Block.DestinationAddress,8) + ' TransferLength=' + IntToHex(Block.TransferLength,8) + ')');
    {$ENDIF}
    
    {Get Next}
    Data:=Data.Next;
    if Data <> nil then
     begin
      {Get Next Block}
      Block:=PBCM2837DMAControlBlock(LongWord(Block) + SizeOf(TBCM2837DMAControlBlock));
     end;
   end; 
 
  {Flush Control Blocks}
  if not(BCM2710DMA_CACHE_COHERENT) then
   begin
    CleanDataCacheRange(LongWord(Request.ControlBlocks),Count * SizeOf(TBCM2837DMAControlBlock));
   end;
  
  {Wait for Channel}
  if Bulk then
   begin
    if SemaphoreWait(PBCM2710DMAHost(DMA).ChannelBulk) <> ERROR_SUCCESS then Exit;
   end
  else if Lite then
   begin
    if SemaphoreWait(PBCM2710DMAHost(DMA).ChannelLite) <> ERROR_SUCCESS then Exit;
   end
  else
   begin  
    if SemaphoreWait(PBCM2710DMAHost(DMA).ChannelWait) <> ERROR_SUCCESS then Exit;
   end; 
  
  {Acquire the Lock}
  if MutexLock(PBCM2710DMAHost(DMA).ChannelLock) = ERROR_SUCCESS then
   begin
    try
     {Get Free Channel}
     if Bulk then
      begin
       Channel:=FirstBitSet(PBCM2710DMAHost(DMA).ChannelFree and BCM2710_DMA_BULK_CHANNELS);
      end
     else if Lite then
      begin
       Channel:=FirstBitSet(PBCM2710DMAHost(DMA).ChannelFree and BCM2710_DMA_LITE_CHANNELS);
      end
     else
      begin
       Channel:=FirstBitSet(PBCM2710DMAHost(DMA).ChannelFree and BCM2710_DMA_NORMAL_CHANNELS);
      end;
      
     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
     if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Allocated channel (Channel=' + IntToStr(Channel) + ')');
     {$ENDIF}
      
     {Check Free Channel} 
     if Channel <> LongWord(INVALID_HANDLE_VALUE) then 
      begin
       {Update Channel Free}
       PBCM2710DMAHost(DMA).ChannelFree:=PBCM2710DMAHost(DMA).ChannelFree xor (1 shl Channel);
      
       {Update Channel}
       PBCM2710DMAHost(DMA).Channels[Channel].Request:=Request;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Set Control Block}
       if BCM2710DMA_BUS_ADDRESSES then
        begin
         PBCM2710DMAHost(DMA).Channels[Channel].Registers.CONBLK_AD:=PhysicalToBusAddress(Request.ControlBlocks);
        end
       else
        begin
         PBCM2710DMAHost(DMA).Channels[Channel].Registers.CONBLK_AD:=LongWord(Request.ControlBlocks);
        end; 
       
       {Note: Broadcom documentation states that BCM2837_DMA_CS_ERROR bit should be cleared by writing
              to the error bits in the debug register, this doesn't seem to be neccessary in practice}
              
       {Enable Channel}
       PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS:=BCM2837_DMA_CS_ACTIVE;
       
       {Note: Broadcom documentation states that the BCM2837_DMA_CS_END bit will be set when a transfer
              is completed and should be cleared by writing 1 to it, this doesn't seem to be the case}
                            
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
         SemaphoreSignal(PBCM2710DMAHost(DMA).ChannelBulk);
        end
       else if Lite then
        begin
         SemaphoreSignal(PBCM2710DMAHost(DMA).ChannelLite);
        end
       else
        begin
         SemaphoreSignal(PBCM2710DMAHost(DMA).ChannelWait);
        end;
      end;     
    finally
     {Release the Lock}
     MutexUnlock(PBCM2710DMAHost(DMA).ChannelLock);
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

function BCM2710DMAHostCancel(DMA:PDMAHost;Request:PDMARequest):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Cancelling request (Request=' + IntToHex(LongWord(Request),8) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(PBCM2710DMAHost(DMA).ChannelLock) = ERROR_SUCCESS then
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
      for Count:=0 to BCM2710_DMA_CHANNEL_COUNT - 1 do
       begin
        if PBCM2710DMAHost(DMA).Channels[Channel].Request = Request then
         begin
          Channel:=Count;
          Break;
         end;
       end;
       
      {Check Channel}
      if Channel <> LongWord(INVALID_HANDLE_VALUE) then
       begin
        {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
        if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Located channel (Channel=' + IntToStr(Channel) + ')');
        {$ENDIF}
      
        {Memory Barrier}
        DataMemoryBarrier; {Before the First Write}
      
        {Get Status}
        CS:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS;
      
        {Check Active}
        if (CS and BCM2837_DMA_CS_ACTIVE) <> 0 then
         begin
          {Pause the Channel}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS:=CS and not(BCM2837_DMA_CS_ACTIVE);
          
          {Wait for Paused}
          Timeout:=10000;
          while ((CS and BCM2837_DMA_CS_PAUSED) = 0) and (Timeout > 0) do
           begin
            CS:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS;
            
            Dec(Timeout);
           end;
          
          {Check Paused}
          if (CS and BCM2837_DMA_CS_PAUSED) = 0 then
           begin
            Result:=ERROR_TIMEOUT;
            Exit;
           end;
           
          {Clear the Next Control Block}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.NEXTCONBK:=0;
          
          {Set the Interrupt Enable}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.TI:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.TI or BCM2837_DMA_TI_INTEN;
          
          {Enable and Abort the Channel}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS or BCM2837_DMA_CS_ACTIVE or BCM2837_DMA_CS_ABORT;
         end;
         
        {Memory Barrier}
        DataMemoryBarrier; {After the Last Read} 
           
        {Interrupt handler will complete cancel}
       end
      else
       begin
        {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
        if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: No channel');
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
    MutexUnlock(PBCM2710DMAHost(DMA).ChannelLock);
   end;   
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end; 

{==============================================================================}

procedure BCM2710DMAInterruptHandler(Channel:PBCM2710DMAChannel);
{DMA Channels 0 to 10 each have a dedicated interrupt, this handler simply
 clears the interrupt and sends a completion on the associated channel}
begin
 {}
 {Check Channel}
 if Channel = nil then Exit;
 if Channel.Registers = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Acknowledge Interrupt}
 Channel.Registers.CS:=BCM2837_DMA_CS_INT;
 
 {Send Completion}
 WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710DMARequestComplete),Channel,nil);
end; 

{==============================================================================}

procedure BCM2710DMASharedInterruptHandler(DMA:PBCM2710DMAHost);
{DMA Channels 11 to 14 share a common interrupt, this alternate handler determines
 which ones triggered the current interrupt and sends a completion on that channel}
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
 Interrupts:=(DMA.InterruptRegister^ and BCM2710_DMA_SHARED_CHANNELS);
 while Interrupts <> 0 do
  begin
   {Get Channel}
   Channel:=FirstBitSet(Interrupts);
   
   {Check Channel}
   if DMA.Channels[Channel].Registers <> nil then
    begin
     {Acknowledge Interrupt}
     DMA.Channels[Channel].Registers.CS:=BCM2837_DMA_CS_INT;
     
     {Send Completion}
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710DMARequestComplete),@DMA.Channels[Channel],nil);
    end;
   
   {Clear the Interrupt}
   Interrupts:=Interrupts xor (1 shl Channel);
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2710DMARequestComplete(Channel:PBCM2710DMAChannel);
var
 CS:LongWord;
 Data:PDMAData;
 Offset:LongInt; {Allow for negative stride}
 DMA:PBCM2710DMAHost;
 Request:PDMARequest;
begin
 {}
 {Check Channel}
 if Channel = nil then Exit;
 if Channel.Registers = nil then Exit;
 
 {Get Host}
 DMA:=Channel.Host;
 if DMA = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710: Request completed (Request=' + IntToHex(LongWord(Channel.Request),8) + ')');
 {$ENDIF}

 {Get Status}
 CS:=Channel.Registers.CS;

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.CS=' + IntToHex(Channel.Registers.CS,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.CONBLK_AD=' + IntToHex(Channel.Registers.CONBLK_AD,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.TI=' + IntToHex(Channel.Registers.TI,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.SOURCE_AD=' + IntToHex(Channel.Registers.SOURCE_AD,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.DEST_AD=' + IntToHex(Channel.Registers.DEST_AD,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.TXFR_LEN=' + IntToHex(Channel.Registers.TXFR_LEN,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.STRIDE=' + IntToHex(Channel.Registers.STRIDE,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.NEXTCONBK=' + IntToHex(Channel.Registers.NEXTCONBK,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.DEBUG=' + IntToHex(Channel.Registers.DEBUG,8) + ')');
 {$ENDIF}
 
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
    if Channel.Number < BCM2710_DMA_CHANNEL_COUNT then
     begin
      {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
      if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710: Released channel (Channel=' + IntToStr(Channel.Number) + ')');
      {$ENDIF}
      
      {Update Channel}
      DMA.Channels[Channel.Number].Request:=nil;
      
      {Update Channel Free}
      DMA.ChannelFree:=DMA.ChannelFree or (1 shl Channel.Number);
      
      {Check Bulk}
      if ((1 shl Channel.Number) and BCM2710_DMA_BULK_CHANNELS) <> 0 then
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelBulk);
       end
      {Check Lite}
      else if ((1 shl Channel.Number) and BCM2710_DMA_LITE_CHANNELS) <> 0 then
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelLite);
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
   if (CS and BCM2837_DMA_CS_ERROR) <> 0 then
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
      if not(BCM2710DMA_CACHE_COHERENT) then
       begin
        Data:=Request.Data;
        while Data <> nil do
         begin
          if (Data.Flags and DMA_DATA_FLAG_NOINVALIDATE) = 0 then
           begin
            if ((Data.Flags and DMA_DATA_FLAG_STRIDE) = 0) or (Data.DestStride = 0) then
             begin
              InvalidateDataCacheRange(LongWord(Data.Dest),Data.Size);
             end
            else
             begin
              Offset:=0;
              while Offset < Data.Size do
               begin
                InvalidateDataCacheRange(LongWord(Data.Dest + Offset),Data.StrideLength);
                
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

function BCM2710DMAPeripheralToDREQ(Peripheral:LongWord):LongWord;
begin
 {}
 Result:=BCM2837_DMA_DREQ_NONE;
 
 case Peripheral of
  DMA_DREQ_ID_UART_TX:Result:=BCM2837_DMA_DREQ_UARTTX;
  DMA_DREQ_ID_UART_RX:Result:=BCM2837_DMA_DREQ_UARTRX;
  DMA_DREQ_ID_SPI_TX:Result:=BCM2837_DMA_DREQ_SPITX;
  DMA_DREQ_ID_SPI_RX:Result:=BCM2837_DMA_DREQ_SPIRX;
  DMA_DREQ_ID_SPI_SLAVE_TX:Result:=BCM2837_DMA_DREQ_BSCSPITX;
  DMA_DREQ_ID_SPI_SLAVE_RX:Result:=BCM2837_DMA_DREQ_BSCSPIRX;
  DMA_DREQ_ID_PCM_TX:Result:=BCM2837_DMA_DREQ_PCMTX;
  DMA_DREQ_ID_PCM_RX:Result:=BCM2837_DMA_DREQ_PCMRX;
  DMA_DREQ_ID_PWM:Result:=BCM2837_DMA_DREQ_PWM;
  DMA_DREQ_ID_MMC:Result:=BCM2837_DMA_DREQ_EMMC;
  DMA_DREQ_ID_SDHOST:Result:=BCM2837_DMA_DREQ_SDHOST;
 end;
end;

{==============================================================================}

procedure BCM2710DMADataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2837DMAControlBlock;Bulk,Lite:Boolean);
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
 if BCM2710DMA_BUS_ADDRESSES then
  begin
   case Request.Direction of
    DMA_DIR_NONE:begin
      Block.SourceAddress:=LongWord(Data.Source);
      Block.DestinationAddress:=LongWord(Data.Dest);
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
   Block.SourceAddress:=LongWord(Data.Source);
   Block.DestinationAddress:=LongWord(Data.Dest);
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
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_2DMODE;
   
   {Get Count (minus 1)}
   Count:=(Data.Size div (Data.StrideLength and BCM2710_DMA_MAX_X_LENGTH)) - 1;
   
   {Set Length and Count}
   Block.TransferLength:=((Count and BCM2710_DMA_MAX_Y_COUNT) shl 16) or (Data.StrideLength and BCM2710_DMA_MAX_X_LENGTH);
   
   {Set Source and Dest Stride}
   Block.ModeStride:=((Data.DestStride and BCM2710_DMA_MAX_STRIDE) shl 16) or (Data.SourceStride and BCM2710_DMA_MAX_STRIDE);
  end;  
 
 {Setup Transfer Information}
 {Source Data Request}
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_WAIT_RESP or BCM2837_DMA_TI_SRC_DREQ;
  end;
 {Dest Data Request} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_WAIT_RESP or BCM2837_DMA_TI_DEST_DREQ;
  end;
 {Source Increment} 
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_NOINCREMENT) = 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_SRC_INC;
  end;
 {Dest Increment} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_NOINCREMENT) = 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_DEST_INC;
  end;
 {Source Width}
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_WIDE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_SRC_WIDTH;
  end;
 {Dest Width}
 if (Data.Flags and DMA_DATA_FLAG_DEST_WIDE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_DEST_WIDTH;
  end;
 {Source Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOREAD) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_SRC_IGNORE;
  end;
 {Dest Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOWRITE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_DEST_IGNORE;
  end;
 {Peripheral Map}
 if Request.Peripheral <> DMA_DREQ_ID_NONE then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710DMAPeripheralToDREQ(Request.Peripheral) shl BCM2837_DMA_TI_PERMAP_SHIFT);
  end; 
 {Burst Length}
 if Bulk then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710_DMA_BULK_BURST_LENGTH shl BCM2837_DMA_TI_BURST_LENGTH_SHIFT);
  end
 else if Lite then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710_DMA_LITE_BURST_LENGTH shl BCM2837_DMA_TI_BURST_LENGTH_SHIFT);
  end
 else
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710_DMA_NORMAL_BURST_LENGTH shl BCM2837_DMA_TI_BURST_LENGTH_SHIFT);
  end;  
 {Interrupt Enable}
 if Data.Next = nil then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_INTEN;
  end;
 
 {Setup Next Control Block}
 if Data.Next <> nil then
  begin
   {Set Next Block}
   if BCM2710DMA_BUS_ADDRESSES then
    begin
     Block.NextControlBlockAddress:=PhysicalToBusAddress(Pointer(LongWord(Block) + SizeOf(TBCM2837DMAControlBlock)));
    end
   else
    begin
     Block.NextControlBlockAddress:=LongWord(Block) + SizeOf(TBCM2837DMAControlBlock);
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
    if not(BCM2710DMA_CACHE_COHERENT) and ((Data.Flags and DMA_DATA_FLAG_NOCLEAN) = 0) then
     begin
      if ((Data.Flags and DMA_DATA_FLAG_STRIDE) = 0) or (Data.SourceStride = 0) then
       begin
        CleanDataCacheRange(LongWord(Data.Source),Data.Size);
       end
      else
       begin
        Offset:=0;
        while Offset < Data.Size do
         begin
          CleanDataCacheRange(LongWord(Data.Source + Offset),Data.StrideLength);
          
          Inc(Offset,Data.SourceStride);
         end; 
       end;
     end;
   end;
 end;  
end;

{==============================================================================}
{==============================================================================}
{BCM2710 PWM0/1 Functions}
function BCM2710PWMStart(PWM:PPWMDevice):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Start');
 {$ENDIF}
 
 {Check Settings}
 if PWM.Range = 0 then Exit;
 if PWM.Frequency = 0 then Exit;
 
 {Check GPIO}
 if PWM.GPIO = GPIO_PIN_UNKNOWN then
  begin
   {Check Channel}
   case PBCM2710PWMDevice(PWM).Channel of
    0:begin
      {Set GPIO 18}
      if BCM2710PWMSetGPIO(PWM,GPIO_PIN_18) <> ERROR_SUCCESS then Exit;
     end; 
    1:begin
      {Set GPIO 19}
      if BCM2710PWMSetGPIO(PWM,GPIO_PIN_19) <> ERROR_SUCCESS then Exit;
     end;
    else
     begin
      Exit;
     end;   
   end;   
  end;
  
 {Start Clock}
 if BCM2710PWMClockStart(PWM,PWM.Frequency) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Enable PWEN}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL or BCM2837_PWM_CTL_PWEN1;
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Enable PWEN}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL or BCM2837_PWM_CTL_PWEN2;
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  CTL=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  STA=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).STA,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  RNG1=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  DAT1=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  RNG2=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG2,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  DAT2=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT2,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2710PWMStop(PWM:PPWMDevice):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Stop');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Disable PWEN}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_PWEN1);
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Disable PWEN}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_PWEN2);
   end;
  else
   begin
    Exit;
   end;   
 end;
 
 {Stop Clock}
 if BCM2710PWMClockStop(PWM) <> ERROR_SUCCESS then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  CTL=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  STA=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).STA,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  RNG1=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  DAT1=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT1,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  RNG2=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG2,8));
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  DAT2=' + IntToHex(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT2,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2710PWMWrite(PWM:PPWMDevice;Value:LongWord):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Write (Value=' + IntToHex(Value,4) + ')');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Set Data}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT1:=Value;
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Set Data}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT2:=Value;
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
 
function BCM2710PWMSetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
var
 BoardType:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Set GPIO (GPIO=' + IntToStr(GPIO) + ')');
 {$ENDIF}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Check GPIO}
    case GPIO of
     GPIO_PIN_12,GPIO_PIN_40:begin
       {Function Select 0}
       GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
      end;
     GPIO_PIN_18:begin
       {Function Select 5}
       GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT5);
      end;
     GPIO_PIN_52:begin
       {Do Not Set}
       Exit;
      end;
     else
      begin
       Exit;
      end;      
    end; 
    
    {Reset GPIO}
    if GPIO <> GPIO_PIN_12 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_12);
    if GPIO <> GPIO_PIN_40 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_40);
    if GPIO <> GPIO_PIN_18 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_18);
    {if GPIO <> GPIO_PIN_52 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_52);}
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Get Board Type}
    BoardType:=BoardGetType;
    
    {Check GPIO}
    case GPIO of
     GPIO_PIN_13:begin
       {Function Select 0}
       GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
      end;
     GPIO_PIN_41:begin
       {Check Board Type}
       case BoardType of
        BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS,BOARD_TYPE_RPI2B,BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
          {Do Not Set}
          Exit;
         end;
        else
         begin        
          {Function Select 0}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
         end; 
       end;  
      end;
     GPIO_PIN_45:begin
       {Check Board Type}
       case BoardType of
        BOARD_TYPE_RPI3B,BOARD_TYPE_RPI3B_PLUS,BOARD_TYPE_RPI3A_PLUS:begin
          {Do Not Set}
          Exit;
         end;
        else 
         begin
          {Function Select 0}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT0);
         end;
       end;
      end;
     GPIO_PIN_19:begin
       {Function Select 5}
       GPIOFunctionSelect(GPIO,GPIO_FUNCTION_ALT5);
      end;
     GPIO_PIN_53:begin
       {Do Not Set}
       Exit;
      end;
     else
      begin
       Exit;
      end;      
    end; 
    
    {Reset GPIO}
    if GPIO <> GPIO_PIN_13 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_13);
    if GPIO <> GPIO_PIN_41 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_41);
    if GPIO <> GPIO_PIN_45 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_45);
    if GPIO <> GPIO_PIN_19 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_19);
    {if GPIO <> GPIO_PIN_53 then BCM2710PWMResetGPIO(PWM,GPIO_PIN_53);}
   end;
  else
   begin
    Exit;
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

function BCM2710PWMResetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
var
 BoardType:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Reset GPIO (GPIO=' + IntToStr(GPIO) + ')');
 {$ENDIF}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Check GPIO}
    case GPIO of
     GPIO_PIN_12,GPIO_PIN_18:begin 
       {Function Select IN}
       GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
      end;
     GPIO_PIN_40:begin 
       {Function Select IN}
       GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
      end;
     GPIO_PIN_52:begin
       {Do Not Reset}
      end;
     else
      begin
       Exit;
      end;      
    end; 
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Get Board Type}
    BoardType:=BoardGetType;

    {Check GPIO}
    case GPIO of
     GPIO_PIN_13,GPIO_PIN_19:begin
       {Function Select IN}
       GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
      end;
     GPIO_PIN_41:begin
       {Check Board Type}
       case BoardType of
        BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS,BOARD_TYPE_RPI2B,BOARD_TYPE_RPI_ZERO,BOARD_TYPE_RPI_ZERO_W:begin
          {Do Not Reset}
         end;
        else
         begin        
          {Function Select IN}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
         end;
       end;
      end; 
     GPIO_PIN_45:begin
       {Check Board Type}
       case BoardType of
        BOARD_TYPE_RPI3B,BOARD_TYPE_RPI3B_PLUS,BOARD_TYPE_RPI3A_PLUS:begin
          {Do Not Reset}
         end;
        else 
         begin
          {Function Select IN}
          GPIOFunctionSelect(GPIO,GPIO_FUNCTION_IN);
         end;
       end;
      end;
     GPIO_PIN_53:begin
       {Do Not Reset}
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
    
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 
    
{==============================================================================}

function BCM2710PWMSetMode(PWM:PPWMDevice;Mode:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Set Mode (Mode=' + IntToStr(Mode) + ')');
 {$ENDIF}
 
 {Check Mode}
 if Mode > PWM_MODE_SERIALIZED then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Check Mode}
    case Mode of
     PWM_MODE_MARKSPACE:begin
       {Mark Space (Enable MSEN)}
       PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_MODE1)) or BCM2837_PWM_CTL_MSEN1;
      end;
     PWM_MODE_BALANCED:begin
       {Balanced (Disable MSEN / MODE)}
       PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_MODE1 or BCM2837_PWM_CTL_MSEN1));
      end;
     PWM_MODE_SERIALIZED:begin
       {Serialized (Enable MODE)}
       PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_MSEN1)) or BCM2837_PWM_CTL_MODE1;
      end;
    end;
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    case Mode of
     PWM_MODE_MARKSPACE:begin
       {Mark Space (Enable MSEN)}
       PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_MODE2)) or BCM2837_PWM_CTL_MSEN2;
      end;
     PWM_MODE_BALANCED:begin
       {Balanced (Disable MSEN / MODE)}
       PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_MODE2 or BCM2837_PWM_CTL_MSEN2));
      end;
     PWM_MODE_SERIALIZED:begin
       {Serialized (Enable MODE)}
       PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=(PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_MSEN2)) or BCM2837_PWM_CTL_MODE2;
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

function BCM2710PWMSetRange(PWM:PPWMDevice;Range:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Set Range (Range=' + IntToStr(Range) + ')');
 {$ENDIF}
 
 {Check Range}
 if Range = 0 then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Set Range}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG1:=Range;
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Set Range}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG2:=Range;
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

function BCM2710PWMSetFrequency(PWM:PPWMDevice;Frequency:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Set Frequency (Frequency=' + IntToStr(Frequency) + ')');
 {$ENDIF}
 
 {Check Frequency}
 if Frequency = 0 then Exit;
 
 {Check Pair}
 if PBCM2710PWMDevice(PWM).Pair <> nil then
  begin
   {Check Enabled}
   if PBCM2710PWMDevice(PWM).Pair.PWM.PWMState = PWM_STATE_ENABLED then Exit;
  end;
  
 {Stop Clock}
 if BCM2710PWMClockStop(PWM) <> ERROR_SUCCESS then Exit;
 
 {Check Enabled}
 if PWM.PWMState = PWM_STATE_ENABLED then
  begin
   {Start Clock}
   if BCM2710PWMClockStart(PWM,Frequency) <> ERROR_SUCCESS then Exit;
  end; 
 
 {Update Scaler}
 PBCM2710PWMDevice(PWM).Scaler:=NANOSECONDS_PER_SECOND div Frequency;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  Scaler=' + IntToStr(PBCM2710PWMDevice(PWM).Scaler));
 {$ENDIF}
 
 {Update Properties}
 PWM.Frequency:=Frequency;
 PWM.Properties.Frequency:=Frequency;
 
 {Check Pair}
 if PBCM2710PWMDevice(PWM).Pair <> nil then
  begin
   {Update Scaler}
   PBCM2710PWMDevice(PWM).Pair.Scaler:=NANOSECONDS_PER_SECOND div Frequency;
   
   {Update Properties}
   PBCM2710PWMDevice(PWM).Pair.PWM.Frequency:=Frequency;
   PBCM2710PWMDevice(PWM).Pair.PWM.Properties.Frequency:=Frequency;
  end;
  
 {Return Result}
 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}

function BCM2710PWMSetPolarity(PWM:PPWMDevice;Polarity:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Set Polarity (Polarity=' + IntToStr(Polarity) + ')');
 {$ENDIF}
 
 {Check Polarity}
 if Polarity > PWM_POLARITY_INVERSE then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Check Polarity}
    if Polarity = PWM_POLARITY_INVERSE then
     begin
      {Inverse (Enable POLA)}
      PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL or BCM2837_PWM_CTL_POLA1;
     end
    else
     begin
      {Normal (Disable POLA)}
      PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_POLA1);
     end;
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Check Polarity}
    if Polarity = PWM_POLARITY_INVERSE then
     begin
      {Inverse (Enable POLA)}
      PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL or BCM2837_PWM_CTL_POLA2;
     end
    else
     begin
      {Normal (Disable POLA)}
      PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL:=PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).CTL and not(BCM2837_PWM_CTL_POLA2);
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

function BCM2710PWMConfigure(PWM:PPWMDevice;DutyNS,PeriodNS:LongWord):LongWord;
var
 Data:LongWord;
 Range:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Configure (DutyNS=' + IntToStr(DutyNS) + ' PeriodNS=' + IntToStr(PeriodNS) + ')');
 {$ENDIF}
 
 {Check Period}
 if PeriodNS <= PWM.Properties.MinPeriod then Exit;
 
 {Check Scaler}
 if PBCM2710PWMDevice(PWM).Scaler = 0 then Exit;
 
 {Get Data}
 Data:=DutyNS div PBCM2710PWMDevice(PWM).Scaler;

 {Get Range}
 Range:=PeriodNS div PBCM2710PWMDevice(PWM).Scaler;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Channel}
 case PBCM2710PWMDevice(PWM).Channel of
  0:begin
    {PWM0 (PWM Channel 1)}
    {Set Data}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT1:=Data;

    {Set Range}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG1:=Range;
   end;
  1:begin
    {PWM1 (PWM Channel 2)}
    {Set Data}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).DAT2:=Data;
    
    {Set Range}
    PBCM2837PWMRegisters(PBCM2710PWMDevice(PWM).Address).RNG2:=Range;
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

function BCM2710PWMClockStart(PWM:PPWMDevice;Frequency:LongWord):LongWord; 
var
 DivisorI:LongWord;
 DivisorR:LongWord;
 DivisorF:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Clock Start');
 {$ENDIF}
 
 {Check Frequency} 
 if Frequency = 0 then Exit;

 {Check Enabled}
 if not BCM2710PWMClockEnabled(PWM) then
  begin
   {Get Divisors}
   DivisorI:=BCM2710_PWM_DEFAULT_CLOCK div Frequency;
   DivisorR:=BCM2710_PWM_DEFAULT_CLOCK mod Frequency;
   DivisorF:=Trunc((DivisorR * 4096) / BCM2710_PWM_DEFAULT_CLOCK);
   
   if DivisorI > 4095 then DivisorI:=4095;
  
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Set Dividers}
   PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMDIV)^:=BCM2837_CM_PASSWORD or (DivisorI shl 12) or DivisorF;
   {Delay}
   MicrosecondDelay(10);
  
   {Set Source}   
   PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^:=BCM2837_CM_PASSWORD or BCM2837_CM_CTL_SRC_OSC;
   {Delay}
   MicrosecondDelay(10);
  
   {Start Clock}   
   PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^:=BCM2837_CM_PASSWORD or PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^ or BCM2837_CM_CTL_ENAB;
   {Delay}
   MicrosecondDelay(110);
   
   {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  DivisorI=' + IntToStr(DivisorI));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  DivisorF=' + IntToStr(DivisorF));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  PWMCTL=' + IntToHex(PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^,8));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  PWMDIV=' + IntToHex(PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMDIV)^,8));
   {$ENDIF}
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;  
end; 
 
{==============================================================================}

function BCM2710PWMClockStop(PWM:PPWMDevice):LongWord; 
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Clock Stop');
 {$ENDIF}

 {Check Pair}
 if PBCM2710PWMDevice(PWM).Pair <> nil then
  begin
   {Check Enabled}
   if PBCM2710PWMDevice(PWM).Pair.PWM.PWMState = PWM_STATE_ENABLED then
    begin
     {Return Result}
     Result:=ERROR_SUCCESS;  
     Exit;
    end; 
  end;
 
 {Check Enabled}
 if BCM2710PWMClockEnabled(PWM) then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
  
   {Stop the Clock}
   PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^:=BCM2837_CM_PASSWORD or (PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^ and not(BCM2837_CM_CTL_ENAB));
   {Delay}
   MicrosecondDelay(110);
   
   {Wait for not Busy}
   while (PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^ and BCM2837_CM_CTL_BUSY) <> 0 do
    begin
     {Delay}
     MicrosecondDelay(1);
    end;
    
   {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  PWMCTL=' + IntToHex(PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^,8));
   if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  PWMDIV=' + IntToHex(PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMDIV)^,8));
   {$ENDIF}
    
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2710PWMClockEnabled(PWM:PPWMDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check PWM}
 if PWM = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710: PWM Clock Enabled');
 {$ENDIF}
 
 {Check Clock}
 if (PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^ and BCM2837_CM_CTL_ENAB) <> 0 then
  begin
   {Return Result}
   Result:=True;
  end;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(PWM_DEBUG)}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'BCM2710:  PWMCTL=' + IntToHex(PLongWord(BCM2837_CM_REGS_BASE + BCM2837_CM_PWMCTL)^,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end; 

{==============================================================================}
{==============================================================================}
{BCM2710 PCM Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 GPIO Functions}
function BCM2710GPIOStart(GPIO:PGPIODevice):LongWord; 
var
 Pin:LongWord;
 Count:LongWord;
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Start');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Clear Registers}
 for Count:=0 to BCM2837_GPIO_BANK_COUNT - 1 do
  begin
   {Event Detect Registers}
   PLongWord(GPIO.Address + BCM2837_GPREN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPFEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPHEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPLEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPAREN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPAFEN0 + (Count * SizeOf(LongWord)))^:=0;
   
   {Event Detect Status}
   Value:=PLongWord(GPIO.Address + BCM2837_GPEDS0 + (Count * SizeOf(LongWord)))^;
   while Value <> 0 do
    begin
     {Get Pin}
     Pin:=FirstBitSet(Value);

     {Clear Status}
     PLongWord(GPIO.Address + BCM2837_GPEDS0 + (Count * SizeOf(LongWord)))^:=(BCM2837_GPEDS_MASK shl Pin);
     
     {Clear Pin}
     Value:=Value xor (1 shl Pin);
    end;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Create Lock}
 PBCM2710GPIODevice(GPIO).Lock:=SpinCreate;
 if PBCM2710GPIODevice(GPIO).Lock = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end; 
 
 {Setup Banks}
 for Count:=0 to BCM2837_GPIO_BANK_COUNT - 1 do
  begin
   PBCM2710GPIODevice(GPIO).Banks[Count].GPIO:=GPIO;
   PBCM2710GPIODevice(GPIO).Banks[Count].Bank:=Count;
   PBCM2710GPIODevice(GPIO).Banks[Count].Address:=PtrUInt(GPIO.Address) + BCM2837_GPEDS0 + (Count * SizeOf(LongWord));
   PBCM2710GPIODevice(GPIO).Banks[Count].PinStart:=Count * 32;
  end;
  
 {Create Pins}
 SetLength(GPIO.Pins,BCM2837_GPIO_PIN_COUNT);
 
 {Setup Pins}
 for Count:=0 to BCM2837_GPIO_PIN_COUNT - 1 do
  begin
   GPIO.Pins[Count].GPIO:=GPIO;
   GPIO.Pins[Count].Pin:=Count;
   GPIO.Pins[Count].Flags:=GPIO_EVENT_FLAG_NONE;
   GPIO.Pins[Count].Trigger:=GPIO_TRIGGER_NONE;
   GPIO.Pins[Count].Count:=0;
   GPIO.Pins[Count].Event:=INVALID_HANDLE_VALUE;
   GPIO.Pins[Count].Events:=nil;
  end;
  
 {Request Bank0 IRQ}
 RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_0,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[0]);

 {Request Bank1 IRQ}
 RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_1,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[1]);

 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2710GPIOStop(GPIO:PGPIODevice):LongWord;
var
 Count:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Stop');
 {$ENDIF}
 
 {Release Bank0 IRQ}
 ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_0,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[0]);

 {Release Bank1 IRQ}
 ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_1,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[1]);
 
 {Release Pins}
 for Count:=0 to BCM2837_GPIO_PIN_COUNT - 1 do
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
 if PBCM2710GPIODevice(GPIO).Lock <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(PBCM2710GPIODevice(GPIO).Lock);
  end;
 
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2710GPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
begin
 {}
 Result:=0;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Read (Reg=' + IntToHex(Reg,8) + ')');
 {$ENDIF}
 
 {Read Register}
 Result:=PLongWord(GPIO.Address + Reg)^;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2710GPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
begin
 {}
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Write (Reg=' + IntToHex(Reg,8) + ' Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
   
 {Write Value}
 PLongWord(GPIO.Address + Reg)^:=Value;
end;

{==============================================================================}
 
function BCM2710GPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Input Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;

 {Update Statistics}
 Inc(GPIO.GetCount);
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 Reg:=BCM2837_GPLEV0 + ((Pin div 32) * SizeOf(LongWord));
 
 {Read Register}
 Result:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2837_GPLEV_MASK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function BCM2710GPIOInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO  Input Wait (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check Trigger}
 if ((Trigger < BCM2710_GPIO_MIN_TRIGGER) or (Trigger > BCM2710_GPIO_MAX_TRIGGER)) and (Trigger <> GPIO_TRIGGER_EDGE) then Exit;
 
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
   if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Check Trigger} 
   if Trigger <> GPIO_TRIGGER_EDGE then
    begin
     {Get Register (Trigger)}
     Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Trigger)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end
   else
    begin 
     {Get Register (Rising)}
     Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Rising)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
   
     {Get Register (Falling)}
     Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Falling)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end; 
    
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Release the Lock}
   SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
  end;
  
 {Increment Count}
 Inc(GPIO.Pins[Pin].Count);
 
 {Release the Lock}
 MutexUnlock(GPIO.Lock);
 
 {Wait for Event}
 if EventWaitEx(GPIO.Pins[Pin].Event,Timeout) = ERROR_SUCCESS then
  begin
   {Get Register (Level)}
   Reg:=BCM2837_GPLEV0 + ((Pin div 32) * SizeOf(LongWord));
   
   {Read Register}
   Result:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2837_GPLEV_MASK;
   
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
       if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Check Trigger} 
       if Trigger <> GPIO_TRIGGER_EDGE then
        begin
         {Get Register (Trigger)}
         Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Trigger)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end
       else
        begin 
         {Get Register (Rising)}
         Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Rising)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
       
         {Get Register (Falling)}
         Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Falling)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end; 
        
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Release the Lock}
       SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
      
       {Reset the Flags}
       GPIO.Pins[Pin].Flags:=GPIO_EVENT_FLAG_NONE;
       
       {Reset the Trigger}
       GPIO.Pins[Pin].Trigger:=GPIO_TRIGGER_NONE;
      end; 
    end; 
  end;  
end;

{==============================================================================}

function BCM2710GPIOInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Input Event (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Flags=' + IntToHex(Flags,8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check Flags}
 if ((Flags and GPIO_EVENT_FLAG_REPEAT) <> 0) and ((Trigger = GPIO_TRIGGER_LOW) or (Trigger = GPIO_TRIGGER_HIGH)) then Exit;
 if ((Flags and GPIO_EVENT_FLAG_INTERRUPT) <> 0) and ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) then Exit;
 if ((Flags and GPIO_EVENT_FLAG_REPEAT) <> 0) and (Timeout <> INFINITE) then Exit;
                      
 {Check Trigger}
 if ((Trigger < BCM2710_GPIO_MIN_TRIGGER) or (Trigger > BCM2710_GPIO_MAX_TRIGGER)) and (Trigger <> GPIO_TRIGGER_EDGE) then Exit;
 
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
   if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Check Trigger} 
   if Trigger <> GPIO_TRIGGER_EDGE then
    begin
     {Get Register (Trigger)}
     Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Trigger)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end
   else
    begin 
     {Get Register (Rising)}
     Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Rising)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
   
     {Get Register (Falling)}
     Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Falling)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end; 
  
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Release the Lock}
   SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
  end; 
 
 {Increment Count}
 Inc(GPIO.Pins[Pin].Count);
 
 {Check Timeout}
 if Timeout <> INFINITE then
  begin
   {Schedule Worker}
   WorkerSchedule(Timeout,TWorkerTask(BCM2710GPIOEventTimeout),Event,nil);
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710GPIOInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Input Cancel (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;

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
       if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Check Trigger} 
       if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_EDGE then
        begin
         {Get Register (Trigger)}
         Reg:=BCM2710_GPIO_TRIGGER_MAP[GPIO.Pins[Pin].Trigger] + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Trigger)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end
       else
        begin 
         {Get Register (Rising)}
         Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Rising)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
       
         {Get Register (Falling)}
         Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Falling)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end; 
        
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Release the Lock}
       SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
      
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

function BCM2710GPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Level}
 if Level > BCM2710_GPIO_MAX_LEVEL then Exit;
 
 {Update Statistics}
 Inc(GPIO.SetCount);
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 if Level = GPIO_LEVEL_HIGH then
  begin
   Reg:=BCM2837_GPSET0 + ((Pin div 32) * SizeOf(LongWord));
  end
 else
  begin
   Reg:=BCM2837_GPCLR0 + ((Pin div 32) * SizeOf(LongWord));
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Write Register}
 PLongWord(GPIO.Address + Reg)^:=(BCM2837_GPSET_MASK shl Shift);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710GPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Select:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Pull Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOPullToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Mode}
 if Mode > BCM2710_GPIO_MAX_PULL then Exit;
 
 {Get Select}
 Select:=BCM2710_GPIO_PULL_MAP[Mode];
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 Reg:=BCM2837_GPPUDCLK0 + ((Pin div 32) * SizeOf(LongWord));
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Write Mode}
 PLongWord(GPIO.Address + BCM2837_GPPUD)^:=Select;
 
 {Wait 150 microseconds (150 cycles)}
 MicrosecondDelay(150);
 
 {Write Clock}
 PLongWord(GPIO.Address + Reg)^:=(BCM2837_GPPUDCLK_MASK shl Shift);
 
 {Wait 150 microseconds (150 cycles)}
 MicrosecondDelay(150);
 
 {Reset Mode}
 PLongWord(GPIO.Address + BCM2837_GPPUD)^:=0;
 
 {Reset Clock}
 PLongWord(GPIO.Address + Reg)^:=0;
 
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710GPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Current:LongWord;
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Get Shift}
 Shift:=(Pin mod 10) * 3;
 
 {Get Register}
 Reg:=BCM2837_GPFSEL0 + ((Pin div 10) * SizeOf(LongWord));
 
 {Read Register}
 Current:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2837_GPFSEL_MASK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=BCM2710_GPIO_FUNCTION_UNMAP[Current];
end;

{==============================================================================}

function BCM2710GPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Mode}
 if Mode > BCM2710_GPIO_MAX_FUNCTION then Exit;
 
 {Get Select}
 Select:=BCM2710_GPIO_FUNCTION_MAP[Mode];
 
 {Get Shift}
 Shift:=(Pin mod 10) * 3;
 
 {Get Register}
 Reg:=BCM2837_GPFSEL0 + ((Pin div 10) * SizeOf(LongWord));
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Read Value}
 Value:=PLongWord(GPIO.Address + Reg)^;
 
 {Get Current}
 Current:=(Value shr Shift) and BCM2837_GPFSEL_MASK;
 
 {Check Current}
 if Select <> Current then
  begin
   {Check Mode}
   if (Select <> BCM2837_GPFSEL_IN) and (Current <> BCM2837_GPFSEL_IN) then
    begin
     {Select Input}
     Value:=Value and not(BCM2837_GPFSEL_MASK shl Shift);
     Value:=Value or (BCM2837_GPFSEL_IN shl Shift);
     
     {Write Value}
     PLongWord(GPIO.Address + Reg)^:=Value;
    end;
   
   {Select Mode}
   Value:=Value and not(BCM2837_GPFSEL_MASK shl Shift);
   Value:=Value or (Select shl Shift);
   
   {Write Value}
   PLongWord(GPIO.Address + Reg)^:=Value;
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
  
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

procedure BCM2710GPIOInterruptHandler(Bank:PBCM2710GPIOBank);
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
begin
 {}
 {Check Bank}
 if Bank = nil then Exit;

 {Get GPIO}
 GPIO:=Bank.GPIO;
 if GPIO = nil then Exit;
 
 {Acquire the Lock}
 if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(PBCM2710GPIODevice(GPIO).InterruptCount);
    
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
            Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
            
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
            Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Rising)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
            
            {Get Register (Falling)}
            Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Falling)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
         end;     
       end; 
      
      {Clear Status}
      PLongWord(Bank.Address)^:=(BCM2837_GPEDS_MASK shl Bit);
      
      {Check Flags}
      if ((Flags and GPIO_EVENT_FLAG_INTERRUPT) = 0) or ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) then
       begin
        {Send Event}
        WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710GPIOEventTrigger),@GPIO.Pins[Bank.PinStart + Bit],nil);
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
    SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
   end;   
  end; 
end;

{==============================================================================}

procedure BCM2710GPIOEventTrigger(Pin:PGPIOPin);
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Event Trigger (Pin=' + GPIOPinToString(Pin.Pin) + ')');
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

procedure BCM2710GPIOEventTimeout(Event:PGPIOEvent);
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Event Timeout (Pin=' + GPIOPinToString(Pin.Pin) + ' Event=' + IntToHex(LongWord(Event),8) + ')');
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
          if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
          
          {Memory Barrier}
          DataMemoryBarrier; {Before the First Write}
          
          {Check Trigger} 
          if Pin.Trigger <> GPIO_TRIGGER_EDGE then
           begin
            {Get Register (Trigger)}
            Reg:=BCM2710_GPIO_TRIGGER_MAP[Pin.Trigger] + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Trigger)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end
          else
           begin 
            {Get Register (Rising)}
            Reg:=BCM2837_GPREN0 + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Rising)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
          
            {Get Register (Falling)}
            Reg:=BCM2837_GPFEN0 + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Falling)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
          
          {Memory Barrier}
          DataMemoryBarrier; {After the Last Read} 
          
          {Release the Lock}
          SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
         
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
{BCM2710 UART0 Functions}
function BCM2710UART0Open(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
{Implementation of UARTDeviceOpen API for BCM2710 UART0}
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + IntToStr(DataBits) + ' StopBits=' + IntToStr(StopBits) + ' Parity=' + IntToStr(Parity) + ' FlowControl=' + IntToStr(FlowControl) + ')');
 {$ENDIF}
 
 {Update Clock Rate}
 PBCM2710UART0Device(UART).ClockRate:=ClockGetRate(CLOCK_ID_UART0);
 if PBCM2710UART0Device(UART).ClockRate = 0 then PBCM2710UART0Device(UART).ClockRate:=BCM2710_UART0_CLOCK_RATE; 
 
 {Update Properties}
 UART.Properties.MaxRate:=PBCM2710UART0Device(UART).ClockRate div 16;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  ClockRate=' + IntToStr(PBCM2710UART0Device(UART).ClockRate) + ' MaxRate=' + IntToStr(UART.Properties.MaxRate));
 {$ENDIF}
 
 {Check Baud Rate}
 if ((BaudRate < BCM2710_UART0_MIN_BAUD) or (BaudRate > UART.Properties.MaxRate)) and (BaudRate <> SERIAL_BAUD_RATE_DEFAULT) then Exit;
 
 {Check Data Bits}
 if (DataBits < BCM2710_UART0_MIN_DATABITS) or (DataBits > BCM2710_UART0_MAX_DATABITS) then Exit;
 
 {Check Stop Bits}
 if (StopBits < BCM2710_UART0_MIN_STOPBITS) or (StopBits > BCM2710_UART0_MAX_STOPBITS) then Exit;
 
 {Check Parity}
 if Parity > BCM2710_UART0_MAX_PARITY then Exit;
 
 {Check Flow Control}
 if FlowControl > BCM2710_UART0_MAX_FLOW then Exit;
 
 {Adjust Baud Rate}
 if BaudRate = SERIAL_BAUD_RATE_DEFAULT then
  begin
   BaudRate:=SERIAL_BAUD_RATE_STANDARD;
   if (BaudRate > UART.Properties.MaxRate) then BaudRate:=SERIAL_BAUD_RATE_FALLBACK;
  end; 

 {Enable GPIO Pins}
 case BoardGetType of 
  BOARD_TYPE_RPI3B,BOARD_TYPE_RPI3B_PLUS,BOARD_TYPE_RPI3A_PLUS:begin
    {On Raspberry Pi 3B/B+/A+ UART0 may be connected to the Bluetooth on pins 32 and 33}
    GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_IN);
    GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_IN);
   end;
 end;  
 GPIOPullSelect(GPIO_PIN_14,GPIO_PULL_NONE);
 GPIOFunctionSelect(GPIO_PIN_14,GPIO_FUNCTION_ALT0);
 GPIOPullSelect(GPIO_PIN_15,GPIO_PULL_UP);
 GPIOFunctionSelect(GPIO_PIN_15,GPIO_FUNCTION_ALT0);
 
 {Check Flow Conrol}
 if FlowControl > SERIAL_FLOW_NONE then
  begin 
   case BoardGetType of
    BOARD_TYPE_RPIA,BOARD_TYPE_RPIB:begin
      {On the Raspberry Pi A and B the RTS/CTS lines are available on header P5 (GPIO 30/31)}
      GPIOPullSelect(GPIO_PIN_30,GPIO_PULL_NONE);
      GPIOFunctionSelect(GPIO_PIN_30,GPIO_FUNCTION_ALT3);
      GPIOPullSelect(GPIO_PIN_31,GPIO_PULL_NONE);
      GPIOFunctionSelect(GPIO_PIN_31,GPIO_FUNCTION_ALT3);
     end;
    else
     begin
      {On all models with a 40 pin header the RTS/CTS lines are on GPIO 16/17}
      GPIOPullSelect(GPIO_PIN_16,GPIO_PULL_NONE);
      GPIOFunctionSelect(GPIO_PIN_16,GPIO_FUNCTION_ALT3);
      GPIOPullSelect(GPIO_PIN_17,GPIO_PULL_NONE);
      GPIOFunctionSelect(GPIO_PIN_17,GPIO_FUNCTION_ALT3);
     end;
   end;  
  end;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Reset Control (Disable UART)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=0;
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC:=0;
 
 {Acknowledge Interrupts}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=$7FF;
 
 {Reset Line Control (Flush FIFOs)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).LCRH:=0;
 
 {Calculate Divisor}
 if BaudRate > (PBCM2710UART0Device(UART).ClockRate div 16) then
  begin
   Divisor:=DivRoundClosest(PBCM2710UART0Device(UART).ClockRate * 8,BaudRate);
  end
 else
  begin
   Divisor:=DivRoundClosest(PBCM2710UART0Device(UART).ClockRate * 4,BaudRate);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  BaudRate=' + IntToStr(BaudRate) + ' Divisor=' + IntToStr(Divisor) + ' Divisor shr 6=' + IntToStr(Divisor shr 6) + ' Divisor and $3F=' + IntToStr(Divisor and $3f));
 {$ENDIF}

 {Set Baud Rate}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FBRD:=Divisor and $3f;
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IBRD:=Divisor shr 6;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Integer Divisor=' + IntToStr(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IBRD));
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Fractional Divisor=' + IntToStr(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FBRD));
 {$ENDIF}
  
 {Get Line Control}
 LineControl:=BCM2837_PL011_LCRH_FEN;
 {Data Bits}
 case DataBits of
  SERIAL_DATA_8BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN8;
  SERIAL_DATA_7BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN7;
  SERIAL_DATA_6BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN6;
  SERIAL_DATA_5BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN5;
 end;
 {Stop Bits}
 case StopBits of
  SERIAL_STOP_2BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_STP2;
 end;
 {Parity}
 case Parity of
  SERIAL_PARITY_ODD:LineControl:=LineControl or BCM2837_PL011_LCRH_PEN;
  SERIAL_PARITY_EVEN:LineControl:=LineControl or BCM2837_PL011_LCRH_PEN or BCM2837_PL011_LCRH_EPS;
 end;
 
 {Set Line Control}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).LCRH:=LineControl;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Line Control=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).LCRH,8));
 {$ENDIF}
 
 {Set Interrupt FIFO Level}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IFLS:=BCM2837_PL011_IFLS_RXIFLSEL1_8 or BCM2837_PL011_IFLS_TXIFLSEL1_8; {BCM2837_PL011_IFLS_RXIFLSEL1_2 / BCM2837_PL011_IFLS_TXIFLSEL1_2}

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Interrupt FIFO Level=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IFLS,8));
 {$ENDIF}
 
 {Get Control} 
 Control:=BCM2837_PL011_CR_RXE or BCM2837_PL011_CR_TXE or BCM2837_PL011_CR_UARTEN;
 {Flow Control}
 case FlowControl of
  SERIAL_FLOW_RTS_CTS:Control:=Control or BCM2837_PL011_CR_CTSEN or BCM2837_PL011_CR_RTSEN;
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
 PBCM2710UART0Device(UART).Lock:=SpinCreate;
 if PBCM2710UART0Device(UART).Lock = INVALID_HANDLE_VALUE then
  begin
   if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Failed to create device lock');

   EventDestroy(UART.TransmitWait);
   EventDestroy(UART.ReceiveWait);
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end; 
 
 {Set Control (Enable UART)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=Control;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Control=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR,8));
 {$ENDIF}
 
 {Request IRQ}
 RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_PL011,TInterruptHandler(BCM2710UART0InterruptHandler),UART);
 
 {Set Interrupt Mask (Enable Interrupts)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC:=BCM2837_PL011_IMSC_TXIM or BCM2837_PL011_IMSC_RXIM;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Interrupt Mask=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC,8));
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

function BCM2710UART0Close(UART:PUARTDevice):LongWord;
{Implementation of UARTDeviceClose API for BCM2710 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceClose instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Close');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC:=0;
 
 {Acknowledge Interrupts}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=$7FF;
 
 {Release IRQ}
 ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_PL011,TInterruptHandler(BCM2710UART0InterruptHandler),UART);
 
 {Reset Control (Disable UART)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=0;
 
 {Destroy Lock}
 SpinDestroy(PBCM2710UART0Device(UART).Lock);
 PBCM2710UART0Device(UART).Lock:=INVALID_HANDLE_VALUE;
 
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
 
function BCM2710UART0Read(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of UARTDeviceRead API for BCM2710 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceRead instead}

 {$IFDEF BCM2710_UART0_RX_BUFFER}
 function BCM2710UART0PushRX(UART:PUARTDevice):LongWord;
 var
  Limit:LongWord;
  Status:LongWord;
 begin
  {}
  if SpinLockIRQ(PBCM2710UART0Device(UART).Lock) = ERROR_SUCCESS then
   begin
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Buffer Received Data}
    Limit:=BCM2710_UART0_RX_POLL_LIMIT;
    Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
    while ((Status and BCM2837_PL011_FR_RXFE) = 0) and (PBCM2710UART0Device(UART).Count < BCM2710_UART0_RX_BUFFER_SIZE) do
     begin
      {Read Data}
      PBCM2710UART0Device(UART).Buffer[(PBCM2710UART0Device(UART).Start + PBCM2710UART0Device(UART).Count) mod BCM2710_UART0_RX_BUFFER_SIZE]:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).DR;
      
      {Update Count}
      Inc(PBCM2710UART0Device(UART).Count);
      
      {Update Limit}
      Dec(Limit);
      if Limit = 0 then Break;
      
      {Get Status}
      Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
     end;
     
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    SpinUnlockIRQ(PBCM2710UART0Device(UART).Lock);
    
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if (EventState(UART.ReceiveWait) <> EVENT_STATE_SIGNALED) and ((PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR and BCM2837_PL011_FR_RXFE) = 0) then
    begin
     {$IFDEF BCM2710_UART0_RX_BUFFER}
     {Push Receive}
     if BCM2710UART0PushRX(UART) <> ERROR_SUCCESS then
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
       {$IFDEF BCM2710_UART0_RX_BUFFER}
       while (PBCM2710UART0Device(UART).Count > 0) and (Size > 0) do
        begin
         if SpinLockIRQ(PBCM2710UART0Device(UART).Lock) = ERROR_SUCCESS then
          begin
           {Read Data}
           Value:=PBCM2710UART0Device(UART).Buffer[PBCM2710UART0Device(UART).Start];

           {Update Start}
           PBCM2710UART0Device(UART).Start:=(PBCM2710UART0Device(UART).Start + 1) mod BCM2710_UART0_RX_BUFFER_SIZE;
         
           {Update Count}
           Dec(PBCM2710UART0Device(UART).Count);
          
           SpinUnlockIRQ(PBCM2710UART0Device(UART).Lock);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;
         
         {Check for Error}
         if (Value and BCM2837_PL011_DR_ERROR) <> 0 then
          begin
           {Check Error}
           if (Value and BCM2837_PL011_DR_OE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Overrun error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_OVERRUN_ERROR;
            end;
           if (Value and BCM2837_PL011_DR_BE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Break error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_BREAK_ERROR;
            end;
           if (Value and BCM2837_PL011_DR_PE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Parity error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_PARITY_ERROR;
            end;
           if (Value and BCM2837_PL011_DR_FE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Framing error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_FRAMING_ERROR;
            end;
           
           {Update Statistics}
           Inc(UART.ReceiveErrors);
          end;

         {Save Data}
         PByte(Buffer + Offset)^:=Value and BCM2837_PL011_DR_DATA;
         
         {Update Statistics}
         Inc(UART.ReceiveCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
        end;
        
       {Check Count} 
       if PBCM2710UART0Device(UART).Count = 0 then
        begin
         {Reset Event}
         EventReset(UART.ReceiveWait);
        end;
       {$ELSE}
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
 
       {Get Status}
       Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
       while ((Status and BCM2837_PL011_FR_RXFE) = 0) and (Size > 0) do
        begin
         {Read Data}
         Value:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).DR;
         
         {Check for Error}
         if (Value and BCM2837_PL011_DR_ERROR) <> 0 then
          begin
           {Check Error}
           if (Value and BCM2837_PL011_DR_OE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Overrun error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_OVERRUN_ERROR;
            end;
           if (Value and BCM2837_PL011_DR_BE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Break error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_BREAK_ERROR;
            end;
           if (Value and BCM2837_PL011_DR_PE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Parity error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_PARITY_ERROR;
            end;
           if (Value and BCM2837_PL011_DR_FE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Framing error on receive character'); 
             
             UART.UARTStatus:=UART.UARTStatus or UART_STATUS_FRAMING_ERROR;
            end;
           
           {Update Statistics}
           Inc(UART.ReceiveErrors);
          end;
          
         {Save Data}
         PByte(Buffer + Offset)^:=Value and BCM2837_PL011_DR_DATA;
         
         {Update Statistics}
         Inc(UART.ReceiveCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
        end;
        
       {Check Status}
       if (Status and BCM2837_PL011_FR_RXFE) <> 0 then
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
       {$IFDEF BCM2710_UART0_RX_BUFFER}
       {Push Receive}
       if BCM2710UART0PushRX(UART) <> ERROR_SUCCESS then
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710UART0Write(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of UARTDeviceWrite API for BCM2710 UART0}
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if (EventState(UART.TransmitWait) <> EVENT_STATE_SIGNALED) and ((PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR and BCM2837_PL011_FR_TXFF) = 0) then
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
       Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
       while ((Status and BCM2837_PL011_FR_TXFF) = 0) and (Size > 0) do
        begin
         {Write Data}
         PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).DR:=PByte(Buffer + Offset)^;
         
         {Update Statistics}
         Inc(UART.TransmitCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
        end;
        
       {Check Status}
       if (Status and BCM2837_PL011_FR_TXFF) <> 0 then
        begin
         {Enable Transmit}
         BCM2710UART0EnableInterrupt(PBCM2710UART0Device(UART),BCM2837_PL011_IMSC_TXIM);
         
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
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
 
function BCM2710UART0GetStatus(UART:PUARTDevice):LongWord;
{Implementation of UARTDeviceGetStatus API for BCM2710 UART0}
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Get Status');
 {$ENDIF}
 
 {Get Flags}
 Flags:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
 if (Flags and BCM2837_PL011_FR_CTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_CTS;
  end;
 if (Flags and BCM2837_PL011_FR_RXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_FULL;
  end;
 if (Flags and BCM2837_PL011_FR_RXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_EMPTY;
  end;
 if (Flags and BCM2837_PL011_FR_TXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_FULL;
  end;
 if (Flags and BCM2837_PL011_FR_TXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_EMPTY;
  end;
 if (Flags and BCM2837_PL011_FR_BUSY) <> 0 then
  begin
   Result:=Result or UART_STATUS_BUSY;
  end;
 
 {Get Status}
 Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).RSRECR;
 if Status <> 0 then
  begin
   if (Status and BCM2837_PL011_RSRECR_OE) <> 0 then
    begin
     Result:=Result or UART_STATUS_OVERRUN_ERROR;
    end;
   if (Status and BCM2837_PL011_RSRECR_BE) <> 0 then
    begin
     Result:=Result or UART_STATUS_BREAK_ERROR;
    end;
   if (Status and BCM2837_PL011_RSRECR_PE) <> 0 then
    begin
     Result:=Result or UART_STATUS_PARITY_ERROR;
    end;
   if (Status and BCM2837_PL011_RSRECR_FE) <> 0 then
    begin
     Result:=Result or UART_STATUS_FRAMING_ERROR;
    end;
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Clear Status} 
   PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).RSRECR:=0;  
  end;  

 {Get UART Status} 
 if UART.UARTStatus <> UART_STATUS_NONE then
  begin
   Result:=Result or UART.UARTStatus;
   {Clear UART Status}
   UART.UARTStatus:=UART_STATUS_NONE;
  end;

 {Get Control}
 Control:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR;
 if (Control and BCM2837_PL011_CR_RTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_RTS;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function BCM2710UART0SetStatus(UART:PUARTDevice;Status:LongWord):LongWord;
{Implementation of UARTDeviceSetStatus API for BCM2710 UART0}
{Note: Not intended to be called directly by applications, use UARTDeviceSetStatus instead}
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Set Status (Status=' + IntToHex(Status,8) + ')');
 {$ENDIF}
 
 {Get Control}
 Control:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read / Before the First Write} 
 
 {Check RTS}
 if (Status and UART_STATUS_RTS) <> 0 then
  begin
   {Enable}
   PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=Control or BCM2837_PL011_CR_RTS;
  end
 else
  begin
   {Disable}
   PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=Control and not(BCM2837_PL011_CR_RTS);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

procedure BCM2710UART0InterruptHandler(UART:PUARTDevice);
{Interrupt handler for the BCM2710 UART0 device}
{Note: Not intended to be called directly by applications}
var
 {$IFDEF BCM2710_UART0_RX_BUFFER}
 Limit:LongWord;
 {$ENDIF}
 Status:LongWord;
begin
 {}
 {Check UART}
 if UART = nil then Exit;

 {Acquire Lock}
 if SpinLockIRQ(PBCM2710UART0Device(UART).Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(PBCM2710UART0Device(UART).InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).MIS;
 if Status <> 0 then
  begin
   {Acknowledge Interrupts}
   PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=Status and not(BCM2837_PL011_ICR_TXIC or BCM2837_PL011_ICR_RXIC);
   
   {Check Transmit}
   if (Status and BCM2837_PL011_MIS_TXMIS) <> 0 then
    begin
     {Acknowledge Transmit}
     PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=BCM2837_PL011_ICR_TXIC;
     
     {Send Transmit}
     if WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710UART0Transmit),UART,nil) = ERROR_SUCCESS then
      begin
       {Mask Transmit}
       PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC and not(BCM2837_PL011_IMSC_TXIM);
      end; 
    end;
    
   {Check Receive}
   if (Status and BCM2837_PL011_MIS_RXMIS) <> 0 then
    begin
     {Acknowledge Receive}
     PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=BCM2837_PL011_ICR_RXIC;

     {$IFDEF BCM2710_UART0_RX_BUFFER}
     {Buffer Received Data}
     Limit:=BCM2710_UART0_RX_POLL_LIMIT;
     Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
     while ((Status and BCM2837_PL011_FR_RXFE) = 0) and (PBCM2710UART0Device(UART).Count < BCM2710_UART0_RX_BUFFER_SIZE) do
      begin
       {Read Data}
       PBCM2710UART0Device(UART).Buffer[(PBCM2710UART0Device(UART).Start + PBCM2710UART0Device(UART).Count) mod BCM2710_UART0_RX_BUFFER_SIZE]:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).DR;
       
       {Update Count}
       Inc(PBCM2710UART0Device(UART).Count);
       
       {Update Limit}
       Dec(Limit);
       if Limit = 0 then Break;
       
       {Get Status}
       Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
      end;
     {$ENDIF}
     
     {Send Receive}
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710UART0Receive),UART,nil);
    end;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release Lock}
 SpinUnlockIRQ(PBCM2710UART0Device(UART).Lock);
end;

{==============================================================================}

procedure BCM2710UART0Receive(UART:PUARTDevice);
{Receive handler for the BCM2710 UART0 device}
{Note: Not intended to be called directly by applications}
var
 Serial:PSerialDevice;
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Receive');
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

procedure BCM2710UART0Transmit(UART:PUARTDevice);
{Transmit handler for the BCM2710 UART0 device}
{Note: Not intended to be called directly by applications}
var
 Serial:PSerialDevice;
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Transmit');
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

procedure BCM2710UART0EnableInterrupt(UART:PBCM2710UART0Device;Interrupt:LongWord); 
{Enable the specified interrupt in the interrupt mask register of a BCM2710 UART0 device}
{UART: The BCM2710 UART0 device to enable the interrupt for}
{Interrupt: The interrupt to enable}

{Note: Caller must hold the UART lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(UART.Lock) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Update Interrupt Mask} 
 PBCM2837PL011Registers(UART.Address).IMSC:=PBCM2837PL011Registers(UART.Address).IMSC or Interrupt;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 {Release Lock}
 SpinUnlockIRQ(UART.Lock);
end;

{==============================================================================}

procedure BCM2710UART0DisableInterrupt(UART:PBCM2710UART0Device;Interrupt:LongWord); 
{Disable the specified interrupt in the interrupt mask register of a BCM2710 UART0 device}
{UART: The BCM2710 UART0 device to disable the interrupt for}
{Interrupt: The interrupt to disable}

{Note: Caller must hold the UART lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(UART.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Update Interrupt Mask} 
 PBCM2837PL011Registers(UART.Address).IMSC:=PBCM2837PL011Registers(UART.Address).IMSC and not(Interrupt);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release Lock}
 SpinUnlockIRQ(UART.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2710 UART1 Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 SDHCI Functions}
function BCM2710SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'SDHCI BCM2710 Powering on Arasan SD Host Controller');

 {Power On SD}
 Status:=PowerOn(POWER_ID_MMC0);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI BCM2710 Failed to power on Arasan SD Host Controller');
   
   Result:=Status;
   Exit;
  end;
 
 {Update SDHCI}
 {Driver Properties}
 if BCM2710SDHCI_FIQ_ENABLED then
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQFIQ);
  end
 else
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
  end;  
 SDHCI.Version:=SDHCIHostReadWord(SDHCI,SDHCI_HOST_VERSION);
 SDHCI.Quirks:=SDHCI_QUIRK_NO_HISPD_BIT or SDHCI_QUIRK_MISSING_CAPS;  //To Do //More ?
 SDHCI.Quirks2:=SDHCI_QUIRK2_BROKEN_R1B or SDHCI_QUIRK2_WAIT_SEND_CMD;
 {Configuration Properties}
 SDHCI.PresetVoltages:=MMC_VDD_32_33 or MMC_VDD_33_34 or MMC_VDD_165_195;
 SDHCI.PresetCapabilities:=0;   //To Do //See: Linux ?
 SDHCI.ClockMinimum:=BCM2710_EMMC_MIN_FREQ;
 SDHCI.ClockMaximum:=BCM2710_EMMC_MAX_FREQ; //To Do //Get from somewhere //See above
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host version = ' + IntToHex(SDHCIGetVersion(SDHCI),4));
 {$ENDIF}
 
 {Update BCM2710}
 PBCM2710SDHCIHost(SDHCI).WriteDelay:=((2 * 1000000) div BCM2710_EMMC_MIN_FREQ) + 1;  //To Do //Get sdhci-BCM2710.emmc_clock_freq from command line (or get from Mailbox Properties ?) //No, probably command line is best. Platform startup can get from Mailbox and place in command line //see above
 PBCM2710SDHCIHost(SDHCI).LastWrite:=0;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host write delay =  ' + IntToStr(PBCM2710SDHCIHost(SDHCI).WriteDelay));
 {$ENDIF}
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host reset completed');
 {$ENDIF}
 
 {Setup Interrupts}
 Result:=BCM2710SDHCISetupInterrupts(SDHCI);
 
 //See: bcm2835_sdhci_init in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2710SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Release the IRQ/FIQ}
 if BCM2710SDHCI_FIQ_ENABLED then
  begin
   ReleaseFIQ(FIQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Clear Interrupts}
 SDHCI.Interrupts:=0;
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host reset completed');
 {$ENDIF}

 {Update SDHCI}
 {Driver Properties}
 SemaphoreDestroy(SDHCI.Wait);
 SDHCI.Wait:=INVALID_HANDLE_VALUE;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'SDHCI BCM2710 Powering off Arasan SD Host Controller');

 {Power Off SD}
 Status:=PowerOff(POWER_ID_MMC0);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI BCM2710 Failed to power off Arasan SD Host Controller');
   
   Result:=Status;
   Exit;
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
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

function BCM2710SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
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

function BCM2710SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
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

procedure BCM2710SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
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
 DataMemoryBarrier;{After the Last Read} 
 
 {Get Byte, Shift and Mask}
 ByteNo:=(Reg and 3);
 ByteShift:=(ByteNo shl 3);
 Mask:=($FF shl ByteShift);

 {Get Value}
 NewValue:=(OldValue and not(Mask)) or (Value shl ByteShift);
 
 {Write LongWord}
 BCM2710SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);

 //See: bcm2835_sdhci_writeb in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2710SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
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
   OldValue:=PBCM2710SDHCIHost(SDHCI).ShadowRegister;
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
   {Save LongWord}
   PBCM2710SDHCIHost(SDHCI).ShadowRegister:=NewValue;
  end
 else
  begin
   {Write LongWord}
   BCM2710SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);
  end;  
  
 //See: bcm2835_sdhci_writew in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2710SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
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
 
 {Wait Delay}
 MicrosecondDelay(PBCM2710SDHCIHost(SDHCI).WriteDelay);
 
 //To Do //Need GetTimerMicroseconds() in Platform (with a Since value as a Parameter ?) //Then use the LastWrite value in SDHCI
         //Also add GetTimerMilliseconds() in Platform as well
               
 //See: bcm2835_sdhci_raw_writel in bcm2835_sdhci.c
 //     bcm2835_sdhci_writel in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2710SDHCIInterruptHandler(SDHCI:PSDHCIHost);
var
 Count:Integer;
 Present:Boolean;
 InterruptMask:LongWord;
 UnexpectedMask:LongWord;
 AcknowledgeMask:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Update Statistics}
 Inc(SDHCI.InterruptCount); 
 
 {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler');
 {$ENDIF}
 
 {Get Interrupt Mask}
 InterruptMask:=SDHCIHostReadLong(SDHCI,SDHCI_INT_STATUS);

 {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
 {$ENDIF}
 
 {Check for No Interrupts}
 if (InterruptMask = 0) or (InterruptMask = $FFFFFFFF) then Exit;

 Count:=16;
 UnexpectedMask:=0;
 while InterruptMask <> 0 do
  begin
   {Clear selected interrupts}
   AcknowledgeMask:=(InterruptMask and (SDHCI_INT_CMD_MASK or SDHCI_INT_DATA_MASK or SDHCI_INT_BUS_POWER));
   SDHCIHostWriteLong(SDHCI,SDHCI_INT_STATUS,AcknowledgeMask);
   
   {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler (AcknowledgeMask=' + IntToHex(AcknowledgeMask,8) + ')');
   {$ENDIF}
   
   {Check for insert / remove interrupts}
   if (InterruptMask and (SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE)) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Insert / Remove Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
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
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Command Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostCommandInterrupt(SDHCI,InterruptMask and SDHCI_INT_CMD_MASK,InterruptMask);
    end;
    
   {Check for data interrupts} 
   if (InterruptMask and SDHCI_INT_DATA_MASK) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Data Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostDataInterrupt(SDHCI,InterruptMask and SDHCI_INT_DATA_MASK);
    end;
   
   {Check for bus power interrupt}
   if (InterruptMask and SDHCI_INT_BUS_POWER) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Bus Power Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     //To Do //Log Error
    end;
 
   {Check for card interrupt}
   if (InterruptMask and SDHCI_INT_CARD_INT) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Card Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     //To Do //Signal another thread ? //Is this only for SDIO ?
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
   {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Unexpected Interrupt (UnexpectedMask=' + IntToHex(UnexpectedMask,8) + ')');
   {$ENDIF}
   
   //To Do //Log Error
  end;
  
 {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler completed');
 {$ENDIF}
  
 //See: bcm2835_mmc_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function BCM2710SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Setup Interrupts}
 SDHCI.Interrupts:=SDHCI_INT_BUS_POWER or SDHCI_INT_DATA_END_BIT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_INDEX or SDHCI_INT_END_BIT or SDHCI_INT_CRC or SDHCI_INT_TIMEOUT or SDHCI_INT_DATA_END or SDHCI_INT_RESPONSE;
                   //SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE or //See sdhci_set_card_detection in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
                   //Note: SDHCI_INT_CARD_INSERT seems to hang everything, why? //Because the SDHCI_CARD_PRESENT bit is never updated !
                   
 {Enable Interrupts}
 SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
 SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);

 {Request the IRQ/FIQ} 
 if BCM2710SDHCI_FIQ_ENABLED then
  begin
   RequestFIQ(FIQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
 
 //See: \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;
 
{==============================================================================}
 
function BCM2710MMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
{Implementation of MMC GetCardDetect for the BCM2710 which does not update the
 bits in the SDHCI_PRESENT_STATE register to reflect card insertion or removal}
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect');
 {$ENDIF}
 
 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (SDHCI_PRESENT_STATE=' + IntToHex(SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE),8) + ')');
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

     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
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
     
     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (Flags=MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end
   else
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);
     
     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;    
  end;

 Result:=MMC_STATUS_SUCCESS;  
end;
 
{==============================================================================}
{==============================================================================}
{BCM2710 System Clock Functions}
function BCM2710SystemClockRead(Clock:PClockDevice):LongWord;
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
 Result:=PBCM2837SystemTimerRegisters(Clock.Address).CLO;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;
 
{==============================================================================}

function BCM2710SystemClockRead64(Clock:PClockDevice):Int64;
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
 Int64Rec(Result).Hi:=PBCM2837SystemTimerRegisters(Clock.Address).CHI;
 
 {Get Low Value}
 Int64Rec(Result).Lo:=PBCM2837SystemTimerRegisters(Clock.Address).CLO;
 
 {Check High Value}
 Check:=PBCM2837SystemTimerRegisters(Clock.Address).CHI;
 if Check <> Int64Rec(Result).Hi then
  begin
   {Rollover Occurred, Get Low Value Again}
   Int64Rec(Result).Hi:=Check;
   Int64Rec(Result).Lo:=PBCM2837SystemTimerRegisters(Clock.Address).CLO;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2710 ARM Clock Functions}
function BCM2710ARMClockStart(Clock:PClockDevice):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710: ARM Clock Start');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Update Core Clock}
  PBCM2710ARMClock(Clock).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
  if PBCM2710ARMClock(Clock).CoreClock = 0 then PBCM2710ARMClock(Clock).CoreClock:=BCM2710_ARM_CLOCK_CORE_CLOCK;
  
  {Update Min/Max}
  Clock.MinRate:=PBCM2710ARMClock(Clock).CoreClock div (BCM2710_ARM_CLOCK_MAX_DIVIDER + 1);
  Clock.MaxRate:=PBCM2710ARMClock(Clock).CoreClock div (BCM2710_ARM_CLOCK_MIN_DIVIDER + 1);
  
  {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710:  CoreClock=' + IntToStr(PBCM2710ARMClock(Clock).CoreClock) + ' MinRate=' + IntToStr(Clock.MinRate) + ' MaxRate=' + IntToStr(Clock.MaxRate));
  {$ENDIF}
  
  {Check Rate}
  if (Clock.Rate <> 0) and ((Clock.Rate < Clock.MinRate) or (Clock.Rate > Clock.MaxRate)) then Exit;
  if Clock.Rate = 0 then Clock.Rate:=BCM2710_ARM_CLOCK_DEFAULT_RATE;
  
  {Get Divider}
  Divider:=(PBCM2710ARMClock(Clock).CoreClock div Clock.Rate) - 1;
  
  {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710:  Divider=' + IntToStr(Divider));
  {$ENDIF}
  
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}
  
  {Get Control}
  Control:=PBCM2837ARMTimerRegisters(Clock.Address).Control;
  
  {Update Control (Counter Enable / Counter Prescale)}
  Control:=Control and not(BCM2837_ARM_TIMER_CONTROL_COUNTER_PRESCALE);
  Control:=Control or (Divider shl 16) or BCM2837_ARM_TIMER_CONTROL_COUNTER_ENABLED;
  
  {Set Control}
  PBCM2837ARMTimerRegisters(Clock.Address).Control:=Control;
  
  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read} 
  
  {Return Result}
  Result:=ERROR_SUCCESS;  
 finally
  MutexUnlock(Clock.Lock);
 end;
end;
 
{==============================================================================}

function BCM2710ARMClockStop(Clock:PClockDevice):LongWord;
{Implementation of ClockDeviceStop API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceStop instead}
var
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710: ARM Clock Stop');
 {$ENDIF}

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control}
 Control:=PBCM2837ARMTimerRegisters(Clock.Address).Control;
 
 {Update Control}
 Control:=Control and not(BCM2837_ARM_TIMER_CONTROL_COUNTER_ENABLED);
 
 {Set Control}
 PBCM2837ARMTimerRegisters(Clock.Address).Control:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 MutexUnlock(Clock.Lock);
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;
 
{==============================================================================}

function BCM2710ARMClockRead(Clock:PClockDevice):LongWord;
{Implementation of ClockDeviceRead API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead instead}
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710: ARM Clock Read');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 {Read Counter}
 Result:=PBCM2837ARMTimerRegisters(Clock.Address).Counter;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}

function BCM2710ARMClockRead64(Clock:PClockDevice):Int64;
{Implementation of ClockDeviceRead64 API for ARM Clock}
{Note: Not intended to be called directly by applications, use ClockDeviceRead64 instead}
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710: ARM Clock Read64');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 {Read Counter}
 Result:=PBCM2837ARMTimerRegisters(Clock.Address).Counter;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}

function BCM2710ARMClockSetRate(Clock:PClockDevice;Rate:LongWord):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710: ARM Clock Set Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Enabled}
  if Clock.ClockState <> CLOCK_STATE_ENABLED then
   begin
    {Update Core Clock}
    PBCM2710ARMClock(Clock).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
    if PBCM2710ARMClock(Clock).CoreClock = 0 then PBCM2710ARMClock(Clock).CoreClock:=BCM2710_ARM_CLOCK_CORE_CLOCK;
    
    {Update Min/Max}
    Clock.MinRate:=PBCM2710ARMClock(Clock).CoreClock div (BCM2710_ARM_CLOCK_MAX_DIVIDER + 1);
    Clock.MaxRate:=PBCM2710ARMClock(Clock).CoreClock div (BCM2710_ARM_CLOCK_MIN_DIVIDER + 1);
    
    {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710:  CoreClock=' + IntToStr(PBCM2710ARMClock(Clock).CoreClock) + ' MinRate=' + IntToStr(Clock.MinRate) + ' MaxRate=' + IntToStr(Clock.MaxRate));
    {$ENDIF}
   end;
   
  {Check Rate}
  if (Rate < Clock.MinRate) or (Rate > Clock.MaxRate) then Exit;
  
  {Get Divider}
  Divider:=(PBCM2710ARMClock(Clock).CoreClock div Rate) - 1;
  
  {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(@Clock.Device,'BCM2710:  Divider=' + IntToStr(Divider));
  {$ENDIF}
  
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}
  
  {Get Control}
  Control:=PBCM2837ARMTimerRegisters(Clock.Address).Control;
  
  {Update Control}
  Control:=Control and not(BCM2837_ARM_TIMER_CONTROL_COUNTER_PRESCALE);
  Control:=Control or (Divider shl 16);
  
  {Set Control}
  PBCM2837ARMTimerRegisters(Clock.Address).Control:=Control;
  
  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read} 
  
  {Check Rate}
  if (PBCM2710ARMClock(Clock).CoreClock mod Rate) <> 0 then
   begin
    {Update Properties}
    Clock.Rate:=PBCM2710ARMClock(Clock).CoreClock div (Divider + 1);
   
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
{BCM2710 ARM Timer Functions}
function BCM2710ARMTimerStart(Timer:PTimerDevice):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Start');
 {$ENDIF}
 
 {Update Core Clock}
 PBCM2710ARMTimer(Timer).CoreClock:=ClockGetRate(CLOCK_ID_CORE);
 if PBCM2710ARMTimer(Timer).CoreClock = 0 then PBCM2710ARMTimer(Timer).CoreClock:=BCM2710_ARM_TIMER_CORE_CLOCK;
 
 {Update Properties}
 Timer.Properties.MinRate:=PBCM2710ARMTimer(Timer).CoreClock div (BCM2710_ARM_TIMER_MAX_DIVIDER + 1);
 Timer.Properties.MaxRate:=PBCM2710ARMTimer(Timer).CoreClock div (BCM2710_ARM_TIMER_MIN_DIVIDER + 1);
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710:  CoreClock=' + IntToStr(PBCM2710ARMTimer(Timer).CoreClock) + ' MinRate=' + IntToStr(Timer.Properties.MinRate) + ' MaxRate=' + IntToStr(Timer.Properties.MaxRate));
 {$ENDIF}
 
 {Check Rate}
 if (Timer.Rate <> 0) and ((Timer.Rate < Timer.Properties.MinRate) or (Timer.Rate > Timer.Properties.MaxRate)) then Exit;
 if Timer.Rate = 0 then Timer.Rate:=BCM2710_ARM_TIMER_DEFAULT_RATE;
 
 {Check Interval}
 if (Timer.Interval <> 0) and ((Timer.Interval < Timer.Properties.MinInterval) or (Timer.Interval > Timer.Properties.MaxInterval)) then Exit;
 if Timer.Interval = 0 then Timer.Interval:=BCM2710_ARM_TIMER_MAX_INTERVAL;

 {Get Divider}
 Divider:=(PBCM2710ARMTimer(Timer).CoreClock div Timer.Rate) - 1;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710:  Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Predivider}
 PBCM2837ARMTimerRegisters(Timer.Address).Predivider:=Divider;
 
 {Set Interval}
 PBCM2837ARMTimerRegisters(Timer.Address).Load:=Timer.Interval;
 
 {Get Control}
 Control:=PBCM2837ARMTimerRegisters(Timer.Address).Control;
 
 {Update Control (Timer Enable / Interrupt Enable / 32 Bit Counter / Prescale None / Counter Disabled)}
 Control:=Control and not(BCM2837_ARM_TIMER_CONTROL_PRESCALE);
 Control:=Control or BCM2837_ARM_TIMER_CONTROL_TIMER_ENABLED or BCM2837_ARM_TIMER_CONTROL_INT_ENABLED or BCM2837_ARM_TIMER_CONTROL_32BIT;
 
 {Set Control}
 PBCM2837ARMTimerRegisters(Timer.Address).Control:=Control;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Create Event (Manual Reset)}
 Timer.Event:=EventCreate(True,False);

 {Request IRQ/FIQ}
 if BCM2710ARM_TIMER_FIQ_ENABLED then
  begin
   RequestFIQ(FIQ_ROUTING,BCM2837_IRQ_ARM_TIMER,TInterruptHandler(BCM2710ARMTimerInterruptHandler),Timer);
  end
 else
  begin 
   RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_ARM_TIMER,TInterruptHandler(BCM2710ARMTimerInterruptHandler),Timer);
  end; 
 
 {Update Properties}
 {Nothing}
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2710ARMTimerStop(Timer:PTimerDevice):LongWord;
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
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Stop');
 {$ENDIF}
 
 {Release IRQ/FIQ}
 if BCM2710ARM_TIMER_FIQ_ENABLED then
  begin
   ReleaseFIQ(FIQ_ROUTING,BCM2837_IRQ_ARM_TIMER,TInterruptHandler(BCM2710ARMTimerInterruptHandler),Timer);
  end
 else
  begin 
   ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_ARM_TIMER,TInterruptHandler(BCM2710ARMTimerInterruptHandler),Timer);
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Control}
 Control:=PBCM2837ARMTimerRegisters(Timer.Address).Control;
 
 {Update Control}
 Control:=Control and not(BCM2837_ARM_TIMER_CONTROL_TIMER_ENABLED or BCM2837_ARM_TIMER_CONTROL_INT_ENABLED);
 
 {Set Control}
 PBCM2837ARMTimerRegisters(Timer.Address).Control:=Control;
 
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

function BCM2710ARMTimerRead64(Timer:PTimerDevice):Int64;
{Implementation of TimerDeviceRead64 API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceRead64 instead}
begin
 {}
 Result:=0;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Read64');
 {$ENDIF}
 
 {Update Statistics}
 Inc(Timer.ReadCount);
 
 {Read Value}
 Result:=PBCM2837ARMTimerRegisters(Timer.Address).Value;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function BCM2710ARMTimerWait(Timer:PTimerDevice):LongWord;
{Implementation of TimerDeviceWait API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceWait instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Wait');
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

function BCM2710ARMTimerEvent(Timer:PTimerDevice;Flags:LongWord;Callback:TTimerCallback;Data:Pointer):LongWord;
{Implementation of TimerDeviceEvent API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceEvent instead}
var
 Waiter:PTimerWaiter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Event (Flags=' + IntToHex(Flags,8) + ')');
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

function BCM2710ARMTimerCancel(Timer:PTimerDevice):LongWord;
{Implementation of TimerDeviceCancel API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceCancel instead}
var
 Waiter:PTimerWaiter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Cancel');
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

function BCM2710ARMTimerSetRate(Timer:PTimerDevice;Rate:LongWord):LongWord;
{Implementation of TimerDeviceSetRate API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceSetRate instead}
var
 Divider:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Set Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Check Rate}
 if (Rate < Timer.Properties.MinRate) or (Rate > Timer.Properties.MaxRate) then Exit;
 
 {Get Divider}
 Divider:=(PBCM2710ARMTimer(Timer).CoreClock div Rate) - 1;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710:  Divider=' + IntToStr(Divider));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Set Predivider}
 PBCM2837ARMTimerRegisters(Timer.Address).Predivider:=Divider;
 
 {Check Rate}
 if (PBCM2710ARMTimer(Timer).CoreClock mod Rate) <> 0 then
  begin
   {Update Properties}
   Timer.Rate:=PBCM2710ARMTimer(Timer).CoreClock div (Divider + 1);
  
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

function BCM2710ARMTimerSetInterval(Timer:PTimerDevice;Interval:LongWord):LongWord;
{Implementation of TimerDeviceSetInterval API for ARM Timer}
{Note: Not intended to be called directly by applications, use TimerDeviceSetInterval instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Timer}
 if Timer = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Set Interval (Interval=' + IntToStr(Interval) + ')');
 {$ENDIF}
 
 {Check Interval}
 if (Interval < Timer.Properties.MinInterval) or (Interval > Timer.Properties.MaxInterval) then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Enabled}
 if (PBCM2837ARMTimerRegisters(Timer.Address).Control and BCM2837_ARM_TIMER_CONTROL_TIMER_ENABLED) = 0 then
  begin
   {Set Interval}
   PBCM2837ARMTimerRegisters(Timer.Address).Load:=Interval;
  end
 else
  begin 
   {Set Interval}
   PBCM2837ARMTimerRegisters(Timer.Address).Reload:=Interval;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 Timer.Interval:=Interval;
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

procedure BCM2710ARMTimerInterruptHandler(Timer:PTimerDevice);
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
 Inc(PBCM2710ARMTimer(Timer).InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Clear Interrupt}
 PBCM2837ARMTimerRegisters(Timer.Address).IRQClear:=1;
 
 {Get Flags}
 Flags:=Timer.Flags;
 
 {Check Flags}
 if ((Flags and TIMER_EVENT_FLAG_INTERRUPT) = 0) or ((Flags and TIMER_EVENT_FLAG_REPEAT) = 0) then
  begin
   {Send Event}
   if BCM2710ARM_TIMER_FIQ_ENABLED then
    begin
     WorkerScheduleFIQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710ARMTimerEventTrigger),Timer,nil);
    end
   else
    begin
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710ARMTimerEventTrigger),Timer,nil);
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

procedure BCM2710ARMTimerEventTrigger(Timer:PTimerDevice);
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

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(@Timer.Device,'BCM2710: ARM Timer Event Trigger');
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
{BCM2710 Local Timer Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2710 Random Functions}
function BCM2710RandomStart(Random:PRandomDevice):LongWord;
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
  
    {Enable Random}
    PBCM2837RNGRegisters(Random.Address).Status:=BCM2710_RANDOM_WARMUP_COUNT;
    PBCM2837RNGRegisters(Random.Address).Control:=BCM2837_RANDOM_ENABLE;
   
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

function BCM2710RandomStop(Random:PRandomDevice):LongWord;
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
   
    {Disable Random}
    PBCM2837RNGRegisters(Random.Address).Control:=BCM2837_RANDOM_DISABLE;
   
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

function BCM2710RandomReadLongWord(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;
 
 if MutexLock(Random.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check Status}
 while (PBCM2837RNGRegisters(Random.Address).Status shr 24) = 0 do
  begin
   ThreadSleep(0);
  end;
  
 {Read Random}
 Result:=PBCM2837RNGRegisters(Random.Address).Data; 

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Statistics}
 Inc(Random.ReadCount);
 
 MutexUnlock(Random.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2710 Mailbox Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 Watchdog Functions}
function BCM2710WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2837PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2837_PM_PASSWORD or ((Watchdog.Timeout * BCM2837_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2837_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2837_PM_PASSWORD or (Current and BCM2837_PM_RSTC_WRCFG_CLR) or BCM2837_PM_RSTC_WRCFG_FULL_RESET;

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

function BCM2710WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2837_PM_PASSWORD or BCM2837_PM_RSTC_RESET;
    
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

function BCM2710WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2837PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2837_PM_PASSWORD or ((Watchdog.Timeout * BCM2837_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2837_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2837_PM_PASSWORD or (Current and BCM2837_PM_RSTC_WRCFG_CLR) or BCM2837_PM_RSTC_WRCFG_FULL_RESET;

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

function BCM2710WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;
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
    Result:=(PBCM2837PMWatchdogRegisters(Watchdog.Address).WDOG and BCM2837_PM_WDOG_TIME_MASK) div BCM2837_PM_WDOG_TICKS_PER_MILLISECOND;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2710 Framebuffer Functions}
function BCM2710FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceAllocate API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceAllocate instead}
var
 Size:LongWord;
 Count:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Defaults:TFramebufferProperties;
 Palette:array[0..255] of LongWord;
 Tag:PBCM2837MailboxTagCreateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
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
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: FramebufferAllocate - FramebufferGetDimensions failed: ' + ErrorToString(Result));
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
    Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagCreateBuffer) + SizeOf(TBCM2837MailboxFooter);
    
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
     Header.Code:=BCM2837_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2837MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
     
     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2837_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2837MailboxTagSetPhysical) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;
     
     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtual) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2837_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2837MailboxTagSetDepth) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;
     
     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2837_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2837MailboxTagSetPixelOrder) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;
     
     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2837_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2837MailboxTagSetAlphaMode) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;
     
     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtualOffset) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;
     
     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2837_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2837MailboxTagSetOverscan) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;
     
     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2837_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2837MailboxTagAllocateBuffer) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2710FRAMEBUFFER_ALIGNMENT;
     
     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2837_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2837MailboxTagGetPitch) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);
     
     {Setup Footer}
     Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2837_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if PLATFORM_LOG_ENABLED then PlatformLogError('BCM2710: FramebufferAllocate - MailboxPropertyCall failed: ' + ErrorToString(Result));
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
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
   
{==============================================================================}

function BCM2710FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceRelease API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceRelease instead}
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagReleaseBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Calculate Size}
    Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagReleaseBuffer) + SizeOf(TBCM2837MailboxFooter);

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
     Header.Code:=BCM2837_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2837MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
     Tag.Header.Tag:=BCM2837_MBOX_TAG_RELEASE_BUFFER;
     Tag.Header.Size:=SizeOf(TBCM2837MailboxTagReleaseBuffer) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Header.Length:=SizeOf(Tag.Request);
    
     {Setup Footer}
     Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagReleaseBuffer)));
     Footer.Tag:=BCM2837_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: FramebufferRelease: MailboxPropertyCall failed: ' + ErrorToString(Result));
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
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2710FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDeviceBlank API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceBlank instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Blank}
 if Blank then
  begin
   Result:=FramebufferSetState(0);
  end
 else
  begin
   Result:=FramebufferSetState(1);
  end;
end;

{==============================================================================}

function BCM2710FramebufferCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceCommit API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceCommit instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Flags}
 if ((Flags and FRAMEBUFFER_TRANSFER_DMA) = 0) and BCM2710FRAMEBUFFER_CACHED then
  begin
   {Clean Cache}
   CleanAndInvalidateDataCacheRange(Address,Size);
  end;
 
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710FramebufferWaitSync(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceWaitSync API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceWaitSync instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Wait Sync}
 Result:=FramebufferSetVSync;
end;
 
{==============================================================================}

function BCM2710FramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;
{Implementation of FramebufferDeviceSetOffset API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetOffset instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
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
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2710FramebufferGetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
{Implementation of FramebufferDeviceGetPalette API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceGetPalette instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
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
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
    
{==============================================================================}

function BCM2710FramebufferSetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
{Implementation of FramebufferDeviceSetPalette API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetPalette instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Palette}
    if Palette = nil then Exit;
    if Palette.Start > 255 then Exit;
    if Palette.Count > 256 then Exit;
    
    {Set Palette}
    Result:=FramebufferSetPalette(Palette.Start,Palette.Count,@Palette.Entries,SizeOf(Palette.Entries));
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2710FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{Implementation of FramebufferDeviceSetBacklight API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetBacklight instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Set Backlight}
 Result:=FramebufferSetBacklight(Brightness);
end; 

{==============================================================================}
 
function BCM2710FramebufferSetCursor(Framebuffer:PFramebufferDevice;Width,Height,HotspotX,HotspotY:LongWord;Image:Pointer;Len:LongWord):LongWord;
{Implementation of FramebufferDeviceSetCursor API for BCM2710 Framebuffer}
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
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end; 

{==============================================================================}

function BCM2710FramebufferUpdateCursor(Framebuffer:PFramebufferDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;
{Implementation of FramebufferDeviceUpdateCursor API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceUpdateCursor instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
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
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2710FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceSetProperties API for BCM2710 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetProperties instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    
    //To Do //Check Properties against current, modify if possible, otherwise reallocate ? (and Notify Resize)
    
   finally
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
{BCM2710 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 BCM2710Init;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 